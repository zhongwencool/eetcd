-module(eetcd_watcher).

-include("eetcd.hrl").

%% API
-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([watch/2, watch/3, watch/4, unwatch/0, unwatch/1, unwatch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type watch_req() :: context().

-type watch() :: #{name := name(),
                   watch_req := watch_req(),
                   watch_conn := eetcd_watch:watch_conn(),
                   watch_timeout := timeout(),
                   receiver := pid(),
                   retry_times => non_neg_integer()
                  }.

-type state() :: #{watches := [watch()],
                   conn_names := [name()],
                   retry_watch_ms := pos_integer(),
                   retry_watch_max := infinity | pos_integer(),
                   retry_tref => reference(),
                   retry_watches := [watch()]}.

-type reason() :: {stream_error | conn_error | http2_down, any()} | timeout.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Before sending watch request to eetcd_watcher, `WatchRequest' should be constructed firstly.
-spec watch(name(), context()) -> ok | {error, reason()}.
watch(Name, WatchReq) ->
    watch(Name, WatchReq, 3000).

-spec watch(name(), context(), timeout()) -> ok.
watch(Name, WatchReq, WatchTimeout) ->
    watch(Name, WatchReq, WatchTimeout, 5000).

-spec watch(name(), context(), timeout(), timeout()) -> ok.
watch(Name, WatchReq, WatchTimeout, CallTimeout) ->
    gen_server:call(?SERVER, {watch, Name, WatchReq, WatchTimeout}, CallTimeout).

%% @doc Unwatch all keys
-spec unwatch() -> ok.
unwatch() ->
    unwatch(3000).

-spec unwatch(pos_integer()) -> ok.
unwatch(UnwatchTimeout) ->
    unwatch(UnwatchTimeout, 5000).

-spec unwatch(pos_integer(), timeout()) -> ok.
unwatch(UnwatchTimeout, CallTimeout) ->
    gen_server:call(?SERVER, {unwatch, UnwatchTimeout}, CallTimeout).

-spec start_link() -> gen:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    RetryWatchMs = application:get_env(eetcd, retry_watch_ms, 3000),
    RetryWatchMax = application:get_env(eetcd, retry_watch_max, infinity),
    {ok, ensure_retry_timer(#{retry_watch_ms => RetryWatchMs,
                              retry_watch_max => RetryWatchMax,
                              retry_watches => [],
                              watches => []})}.

-spec ensure_retry_timer(state()) -> state().
ensure_retry_timer(State = #{retry_watch_ms := RetryWatchMs}) ->
    TRef = erlang:start_timer(RetryWatchMs, self(), retry),
    State#{retry_tref => TRef}.


-spec handle_call(Request, {pid(), any()}, state()) ->
          {reply, ok | {error, reason()}, state()} when
      Request :: {watch, name(), watch_req(), timeout()}
               | {error, any()}.
handle_call({watch, Name, WatchReq = #{key := _Key}, WatchTimeout}, {From, _Any},
            State = #{watches := Watches}) ->
    case eetcd_watch:watch(Name, WatchReq, WatchTimeout) of
        {ok, WatchConn, _WatchId} ->
            Watch = #{name => Name,
                      watch_req => WatchReq,
                      watch_conn => WatchConn,
                      watch_timeout => WatchTimeout,
                      receiver => From},
            {reply, ok, State#{watches => [Watch | Watches]}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({unwatch, UnwatchTimeout}, _From, State = #{watches := Watches}) ->
    RemainWatches = lists:foldl(
                      fun(Watch = #{watch_conn := WatchConn}, Acc) ->
                              case eetcd_watch:unwatch(WatchConn, UnwatchTimeout) of
                                  ok -> Acc;
                                  {error, _Reason} ->
                                      [Watch | Acc]
                              end
                      end, [], Watches),
    {reply, ok, State#{watches => RemainWatches, retry_watches => []}};
handle_call(_UnexpecteReq, _From, State) ->
    {reply, {error, illegal_request}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({timeout, TRef, retry}, State = #{retry_watches := RetryWatches,
                                              watches := Watches,
                                              retry_watch_max := RetryWatchMax,
                                              retry_tref := TRef}) ->
    {NewWatches, NewRetryWatches} =
        lists:foldl(
          fun (#{retry_times := RetryTimes}, Acc)
                when RetryTimes >= RetryWatchMax  ->
                  %% Drop the retry watch when its retry times exceed `RetryWatchMax`
                  Acc;
              (RetryWatch = #{name := Name,
                              watch_req := WatchReq,
                              watch_timeout := WatchTimeout,
                              retry_times := RetryTimes},
               {AccWatches, AccRetryWatches}) ->
                  case eetcd_watch:watch(Name, WatchReq, WatchTimeout) of
                      {ok, WatchConn, _WatchId} ->
                          Watch = from_retry_watch(RetryWatch#{watch_conn => WatchConn}),
                          {[Watch | AccWatches], AccRetryWatches};
                      {error, _Reason} ->
                          {AccWatches, [RetryWatch#{retry_times => RetryTimes + 1} | AccRetryWatches]}
                  end
          end, {Watches, []}, RetryWatches),
    {noreply, ensure_retry_timer(
                State#{retry_watches => NewRetryWatches,
                       watches => NewWatches})};

%% handle data received from etcd
handle_info(Msg = {gun_data, CPid, SRef, nofin, _Data}, State = #{watches := Watches}) ->
    %% Find the watch connection via pid and stream reference
    case find_watch_conn(#{http2_pid => CPid, stream_ref => SRef}, Watches) of
        [] ->
            {stop, {shutdown, unexpected_data}, State};
        [Watch = #{watch_conn := WatchConn,
                   receiver := Receiver,
                   watch_req := WatchReq}] ->
            RestWatches = lists:delete(Watch, Watches),
            NewWatch = case eetcd_watch:watch_stream(WatchConn, Msg) of
                           {ok, NewWatchConn, #{events := Events,
                                                header := #{revision := Rev}}} ->
                               Receiver ! {ok, Events},
                               NewWatchReq = eetcd_watch:with_start_revision(WatchReq, Rev),
                               Watch#{watch_conn => NewWatchConn,
                                      watch_req => NewWatchReq};
                           {more, NewWatchConn} ->
                               Watch#{watch_conn => NewWatchConn}
                       end,
            {noreply, State#{watches => [NewWatch | RestWatches]}}
    end;

%% handle gRPC error
handle_info(Msg = {gun_trailers, CPid, SRef, [{<<"grpc-status">>, _Status},
                                              {<<"grpc-message">>, _Msg}]},
            State = #{watches := Watches, retry_watches := RetryWatches}) ->
    %% Find the watch connection via pid and stream reference
    case find_watch_conn(#{http2_pid => CPid, stream_ref => SRef}, Watches) of
        [] ->
            {stop, {shutdown, unexpected_data}, State};
        [Watch = #{watch_conn := WatchConn,
                   receiver := Receiver}] ->
            Receiver ! {error, grpc_error},
            eetcd_watch:watch_stream(WatchConn, Msg),
            {noreply, State#{watches => lists:delete(Watch, Watches),
                             retry_watches => [into_retry_watch(Watch) | RetryWatches]}}
    end;

%% handle stream error
handle_info(Msg = {gun_error, CPid, SRef, _Reason}, State = #{watches := Watches,
                                                              retry_watches := RetryWatches}) ->
    %% Find the watch connection via pid and stream reference
    case find_watch_conn(#{http2_pid => CPid, stream_ref => SRef}, Watches) of
        [] ->
            {stop, {shutdown, unexpected_data}, State};
        [Watch = #{watch_conn := WatchConn,
                   receiver := Receiver}] ->
            Receiver ! {error, stream_error},
            eetcd_watch:watch_stream(WatchConn, Msg),
            {noreply, State#{watches => lists:delete(Watch, Watches),
                             retry_watches => [into_retry_watch(Watch) | RetryWatches]}}
    end;

%% handle gun connection process state error
handle_info(Msg = {gun_error, Pid, _Reason}, State = #{retry_watches := RetryWatches,
                                                       watches := Watches}) ->
    %% Find the watch connection via pid
    case find_watch_conn(#{http2_pid => Pid}, Watches) of
        [] ->
            {stop, {shutdown, unexpected_data}, State};
        StaleWatches ->
            [ begin
                  Receiver ! {error, connection_state_error},
                  eetcd_watch:watch_stream(WatchConn, Msg)
              end || #{watch_conn := WatchConn, receiver := Receiver} <- StaleWatches],
            NewRetryWatches = [into_retry_watch(Watch) || Watch <- StaleWatches] ++ RetryWatches,
            NewWatches = Watches -- StaleWatches,
            {noreply, State#{watches => NewWatches, retry_watches => NewRetryWatches}}
    end;

%% handle gun connection down
handle_info(Msg = {'DOWN', MRef, process, Pid, _Reason},
            State = #{watches := Watches, retry_watches := RetryWatches}) ->
    %% Find the watch connection via pid and monitor reference
    case find_watch_conn(#{http2_pid => Pid, monitor_ref => MRef}, Watches) of
        [] ->
            {stop, {shutdown, unexpected_data}, State};
        [Watch = #{watch_conn := WatchConn, receiver := Receiver}] ->
            eetcd_watch:watch_stream(WatchConn, Msg),
            Receiver ! {error, connection_down},
            {noreply, State#{watches => lists:delete(Watch, Watches),
                             retry_watches => [into_retry_watch(Watch) | RetryWatches]}}
    end;

handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #{watches := Watches}) ->
    [eetcd_watch:unwatch(WatchConn, 5000) || #{watch_conn := WatchConn} <- Watches],
    ok.

-spec find_watch_conn(#{http2_pid => pid(),
                        stream_ref => reference(),
                        monitor_ref => reference()}, [watch()]) -> [watch()].
find_watch_conn(#{http2_pid := CPid, stream_ref := SRef}, Watches) ->
    [Watch || Watch = #{watch_conn := #{http2_pid := Http2Pid,
                                        stream_ref := StreamRef}} <- Watches,
              Http2Pid =:= CPid, StreamRef =:= SRef];
find_watch_conn(#{http2_pid := CPid, monitor_ref := MRef}, Watches) ->
    [Watch || Watch = #{watch_conn := #{http2_pid := Http2Pid,
                                        monitor_ref := MonitorRef}} <- Watches,
              Http2Pid =:= CPid, MonitorRef =:= MRef];
find_watch_conn(#{http2_pid := CPid}, Watches) ->
    [Watch || Watch = #{watch_conn := #{http2_pid := Http2Pid}} <- Watches,
              Http2Pid =:= CPid].

-spec into_retry_watch(watch()) -> watch().
into_retry_watch(Watch) ->
    Watch#{retry_times => 0}.

-spec from_retry_watch(watch()) -> watch().
from_retry_watch(Watch) ->
    maps:without([retry_times], Watch).

