-module(eetcd_watcher).

-include("eetcd.hrl").
-behaviour(gen_server).

%% API
-export([start/1, start/2, start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-export([watch/2, watch/3, unwatch/1]).

-define(DEFAULT_RETRY_INTERVAL, 3000).

-type watch_id() :: pos_integer().

-type watch_options() :: #{prefix => boolean(),
                           prev_kv => boolean(),
                           progress_notify => boolean(),
                           rev => non_neg_integer(),
                           watch_id => watch_id(),
                           fragment => boolean(),
                           filter_put => boolean(),
                           filter_delete => boolean(),
                           from_key => boolean(),
                           range_end => iodata(),
                           reuse_stream => boolean(),
                           retry_notify => boolean()
                          }.

-type watching_item_value() :: #{prefix => boolean(),
                                 prev_kv => boolean(),
                                 progress_notify => boolean(),
                                 rev => non_neg_integer(),
                                 key := key(),
                                 receiver := pid()
                                }.

-type watching_item() :: #{watch_id() => watching_item_value()}.

-type stream_ref() :: reference().

-type watching() :: #{stream_ref() => watching_item()}.

-type gun_pid() :: pid().

-type conns() :: #{gun_pid() => #{stream_ref() => eetcd_watch:watch_conn()}}.

-type state() :: #{name := name(),
                   conns := conns(),
                   watching := watching(),
                   retries := [{key(), watch_options(), pid()}],
                   retry_tref := undefined | reference(),
                   retry_interval_ms => pos_integer()
                  }.

%%%===================================================================
%%% API
%%%===================================================================

-spec watch(pid(), key()) -> ok.
watch(Watcher, Key) ->
    watch(Watcher, Key, #{}).

-spec watch(pid(), key(), watch_options()) -> ok.
watch(Watcher, Key, Options) ->
    gen_server:call(Watcher, {watch, Key, Options}).

-spec unwatch(pid()) -> ok.
unwatch(Watcher) ->
    gen_server:call(Watcher, unwatch, infinity).

-spec start(name()) -> gen:start_ret().
start(Name) ->
    start(Name, ?DEFAULT_RETRY_INTERVAL).

-spec start(name(), pos_integer()) -> gen:start_ret().
start(Name, RetryIntervalMs) ->
    case eetcd_watcher_sup:start_child(Name, RetryIntervalMs) of
        {ok, Pid} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason}
    end.

-spec start_link(name(), pos_integer()) -> gen:start_ret().
start_link(Name, RetryIntervalMs) ->
    gen_server:start_link(?MODULE, [Name, RetryIntervalMs], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([name() | pid() | pos_integer()]) -> {ok, state()}.
init([Name, RetryIntervalMs]) ->
    {ok, #{name => Name,
           conns => #{},
           watching => #{},
           retries => [],
           retry_tref => undefined,
           retry_interval_ms => RetryIntervalMs
          }}.


-spec handle_call({watch, key(), watch_options()}, {pid(), any()}, state()) ->
          {reply, ok, state()}.
handle_call(unwatch, _From, State = #{conns := WatchConns}) ->
    [begin
         [ eetcd_watch:unwatch(WatchConn, 5000) ||
             {_StreamRef, WatchConn} <- maps:to_list(Map) ]
     end || {_Pid, Map} <- maps:to_list(WatchConns)],
    {reply, ok, State#{watching => #{},
                       retries => [],
                       conns => #{}}};

handle_call({watch, Key, Options}, {Receiver, _Any}, State) ->
    case do_watch(Key, Options, Receiver, State) of
        {ok, NewState} ->
            {reply, ok, ensure_retry(NewState)};
        Err ->
            {reply, Err, ensure_retry(State)}
    end.

%% @doc random strategy.
-spec pick_watch_conn(conns()) -> {ok, eetcd_watch:watch_conn()} | {error, any()}.
pick_watch_conn(Conns) ->
    case maps:size(Conns) of
        0 ->
            {error, no_connection};
        Size when Size > 0 ->
            FilteredConns = maps:filter(
                              fun(_K, V) ->
                                      case maps:size(V) of
                                          0 -> false;
                                          _Else -> true
                                      end
                              end, Conns),
            case maps:size(FilteredConns) of
                0 -> {error, no_stream};
                _Else ->
                    ConnPids = maps:keys(FilteredConns),
                    FilteredConnsLength = maps:size(FilteredConns),
                    PickedConnPid = lists:nth(rand:uniform(FilteredConnsLength), ConnPids),
                    PickedConnection = maps:get(PickedConnPid, FilteredConns),
                    StreamRefsLength = maps:size(PickedConnection),
                    StreamRefs = maps:keys(PickedConnection),
                    PickedStreamRef = lists:nth(rand:uniform(StreamRefsLength), StreamRefs),
                    {ok, maps:get(PickedStreamRef, PickedConnection)}
            end
    end.

-spec handle_cast(Request :: term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Msg :: term(), state()) -> {noreply, state()}.
handle_info({timeout, TRef, retry}, State = #{retry_tref := TRef, retries := Retries}) ->
    NewState = lists:foldl(
                    fun(RetryItem = #{key := Key,
                                      receiver := Receiver}, StateAcc) ->
                            WatchOptions = maps:without([key, receiver], RetryItem),
                            case do_watch(Key, WatchOptions, Receiver, StateAcc) of
                                {ok, NewStateAcc = #{retries := Retries0}} ->
                                    case maps:get(retry_notify, WatchOptions, true) of
                                        true ->
                                            %% when retry succeed, notify the receiver
                                            Receiver ! {retried_watch, Key};
                                        false -> ok
                                    end,
                                    NewRetries = lists:delete(RetryItem, Retries0),
                                    NewStateAcc#{retries => NewRetries};
                                {error, Reason} ->
                                    ?LOG_ERROR("eetcd watcher watch failed for ~p", [Reason]),
                                    StateAcc
                            end
                    end, State, Retries),
    {noreply, ensure_retry(NewState#{retry_tref => undefined})};

handle_info(Msg, State = #{conns := Conns, watching := Watching, retries := Retries}) ->
    case classify_essential(Msg) of
        {receive_data, {SRef, Pid}} ->
            HandleReceivedData =
                fun(WatchConn, ConnItem) ->
                        case eetcd_watch:watch_stream(WatchConn, Msg) of
                            {ok, NewWatchConn, #{events := Events,
                                                 watch_id := WatchId,
                                                 header := #{revision := Revision}}} ->
                                NewConns = Conns#{Pid => ConnItem#{SRef => NewWatchConn}},
                                NewWatching = update_revision(SRef, WatchId, Revision, Watching),
                                #{SRef := #{WatchId := #{receiver := Receiver}}} = NewWatching,
                                notify(Receiver, Events),
                                {noreply, State#{conns => NewConns, watching => NewWatching}};
                            {more, NewWatchConn} ->
                                NewConns = Conns#{Pid => ConnItem#{SRef => NewWatchConn}},
                                {noreply, State#{conns => NewConns}}
                        end
                end,
            with_watch_conn(Pid, SRef, State, HandleReceivedData);
        {Type, {SRef, Pid}} when Type =:= grpc_error;
                                 Type =:= stream_error ->
            HandleErrorAboveStream =
                fun(WatchConn, ConnItem) ->
                        ?LOG_WARNING("Watch stream failed for ~p:~p",
                                     [eetcd_watch:watch_stream(WatchConn, Msg)]),
                        NewConns = Conns#{Pid => maps:remove(SRef, ConnItem)},
                        {NewWatching, NewRetries} = drop_stale_watching([SRef], Watching),
                        {noreply, State#{conns => NewConns,
                                         watching => NewWatching,
                                         retries => NewRetries ++ Retries}}
                end,
            with_watch_conn(Pid, SRef, State, HandleErrorAboveStream);
        {Type, Pid} when Type =:= conn_error;
                         Type =:= conn_down ->
            {NewConns, StreamRefs} = drop_stale_connection(Pid, Conns),
            {NewWatching, NewRetries} = drop_stale_watching(StreamRefs, Watching),
            {noreply, State#{conns => NewConns,
                             watching => NewWatching,
                             retries => NewRetries ++ Retries}};
        unknown ->
            {noreply, State}
    end.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec notify(pid(), [router_pb:'mvccpb.Event'()]) ->
          [router_pb:'mvccpb.Event'()].
notify(Pid, Events) ->
    Pid ! Events.

-spec update_revision(stream_ref(), watch_id(), integer(), watching()) -> watching().
update_revision(StreamRef, WatchId, Revision, Watching) ->
    maps:update_with(StreamRef,
                     fun(WatchingItem) ->
                             maps:update_with(WatchId,
                                              fun(V) ->
                                                      V#{rev => Revision}
                                              end,
                                              WatchingItem)
                     end,
                     Watching).

-spec with_options(Key, Value, context()) -> context() when
      Key :: prefix | prev_kv | progress_notify | rev
           | watch_id | fragment | filter_put | filter_delete
           | from_key | range_end,
      Value :: boolean() | non_neg_integer() | iodata().
with_options(prefix, true, Request) ->
    eetcd_watch:with_prefix(Request);
with_options(prev_kv, true, Request) ->
    eetcd_watch:with_prev_kv(Request);
with_options(progress_notify, true, Request) ->
    eetcd_watch:with_progress_notify(Request);
with_options(rev, Rev, Request) ->
    eetcd_watch:with_start_revision(Request, Rev);
with_options(watch_id, WatchId, Request) when is_integer(WatchId) ->
    eetcd_watch:with_watch_id(Request, WatchId);
with_options(fragment, true, Request) ->
    eetcd_watch:with_fragment(Request);
with_options(filter_put, true, Request) ->
    eetcd_watch:with_filter_put(Request);
with_options(filter_delete, true, Request) ->
    eetcd_watch:with_filter_delete(Request);
with_options(from_key, true, Request) ->
    eetcd_watch:with_from_key(Request);
with_options(range_end, End, Request) ->
    eetcd_watch:with_range_end(Request, End);
with_options(_K, _V, Request) ->
    Request.

-spec ensure_retry(state()) -> state().
ensure_retry(State = #{retry_tref := undefined,
                       retry_interval_ms := RetryIntervalMs}) ->
    TRef = erlang:start_timer(RetryIntervalMs, self(), retry),
    State#{retry_tref => TRef};
ensure_retry(State = #{retry_tref := RetryRef}) when is_reference(RetryRef) ->
    State.

-spec classify_essential(term()) -> Result when
      Result :: {receive_data | grpc_error | stream_error, {stream_ref(), pid()}}
              | {conn_error | conn_down, pid()}
              | unknown.
classify_essential({gun_data, Pid, SRef, nofin, _Data}) ->
    {receive_data, {SRef, Pid}};
classify_essential({gun_trailers, Pid, SRef, _GRPCMsg}) ->
    {grpc_error, {SRef, Pid}};
classify_essential({gun_error, Pid, SRef, _Reason}) ->
    {stream_error, {SRef, Pid}};
classify_essential({gun_error, Pid, _Reason}) ->
    {conn_error, Pid};
classify_essential({'DOWN', _MRef, process, Pid, _Reason}) ->
    {conn_down, Pid};
classify_essential(_UnKnown) ->
    unknown.

-spec with_watch_conn(gun_pid(), stream_ref(), state(), Fun) -> Result when
      Result :: {noreply, state()},
      Fun :: fun((eetcd_watch:watch_conn(),
                  #{stream_ref() := eetcd_watch:watch_conn()}) -> {noreply, state()}).
with_watch_conn(Pid, SRef, State = #{conns := Conns}, Fun) ->
    case maps:get(Pid, Conns, undefined) of
        undefined ->
            ?LOG_ERROR("Unexpected connection ~p, stream_ref: ~p",
                       [Pid, SRef]),
            {noreply, State};
        ConnItem ->
            case maps:get(SRef, ConnItem, undefined) of
                undefined ->
                    ?LOG_ERROR("Unexpected stream ~p", [SRef]),
                    {noreply, State};
                WatchConn ->
                    Fun(WatchConn, ConnItem)
            end
    end.

-spec yield_request(key(), watch_options()) -> context().
yield_request(Key, WatchOptions) ->
    ReqInit = eetcd_kv:new(),
    ReqKey = eetcd_watch:with_key(ReqInit, Key),
    Revision = maps:get(rev, WatchOptions, 0),
    ReqRev = eetcd_watch:with_start_revision(ReqKey, Revision),
    maps:fold(fun with_options/3, ReqRev, WatchOptions).

-spec add_watch_conn(eetcd_watch:watch_conn(), conns()) -> conns().
add_watch_conn(WatchConn = #{http2_pid := GunPid, stream_ref := StreamRef}, Conns) ->
    WatchConnMap = maps:get(GunPid, Conns, #{}),
    NewWatchConnMap = WatchConnMap#{StreamRef => WatchConn},
    Conns#{GunPid => NewWatchConnMap}.

-spec add_watching(stream_ref(), integer(), watching_item_value(), watching()) -> watching().
add_watching(StreamRef, WatchId, WatchingItemVal, Watching) ->
    WatchingItem = maps:get(StreamRef, Watching, #{}),
    NewWatchingItem = WatchingItem#{WatchId => WatchingItemVal},
    Watching#{StreamRef => NewWatchingItem}.

-spec drop_stale_connection(pid(), conns()) -> {conns(), [stream_ref()]}.
drop_stale_connection(Pid, Conns) ->
    case maps:get(Pid, Conns, undefined) of
        undefined -> {Conns, []};
        Value -> {maps:without([Pid], Conns), maps:keys(Value)}
    end.

-spec drop_stale_watching([stream_ref()], watching()) -> {watching(), [watching_item_value()]}.
drop_stale_watching(StreamRefs, Watching) ->
    NewRetries = lists:foldl(
                   fun(StreamRef, Acc) ->
                           case maps:get(StreamRef, Watching, undefined) of
                               undefined ->
                                   Acc;
                               WatchItems ->
                                   Acc ++ [ Value || {_Key, Value} <- maps:to_list(WatchItems)]
                           end
                   end, [], StreamRefs),
    NewWatching = maps:without(StreamRefs, Watching),
    {NewWatching, NewRetries}.

-spec do_watch(key(), watch_options(), pid(), state()) -> {ok, state()} | {error, any()}.
do_watch(Key, WatchOptions, Receiver, State = #{name := Name,
                                                conns := WatchConns,
                                                watching := Watching}) ->
    DoWatch = fun(TimeoutOrWatchConn) ->
                      Request = yield_request(Key, WatchOptions),
                      case eetcd_watch:watch(Name, Request, TimeoutOrWatchConn) of
                          {ok, WatchConn = #{stream_ref := StreamRef}, WatchId} ->
                              NewWatchConns = add_watch_conn(WatchConn, WatchConns),

                              WatchingItemVal = WatchOptions#{key => Key, receiver => Receiver},
                              NewWatching = add_watching(StreamRef, WatchId,
                                                         WatchingItemVal, Watching),
                              {ok, State#{watching => NewWatching, conns => NewWatchConns}};
                          Err = {error, _Reason} ->
                              Err
                      end
              end,
    case maps:get(reuse_stream, WatchOptions, false) of
        false ->
            DoWatch(5000);
        true ->
            case pick_watch_conn(WatchConns) of
                {ok, WatchConn} ->
                    DoWatch(WatchConn);
                Err = {error, _Reason} ->
                    {reply, Err, ensure_retry(State)}
            end
    end.
