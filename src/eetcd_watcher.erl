-module(eetcd_watcher).

-behaviour(gen_statem).

-include("eetcd.hrl").

%% API
-export([start_link/3, stop/1]).
-export([watch/2, watch/3, unwatch/1, unwatch/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, ready/3, watching/3, recovering/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-export_type([watcher/0, options/0, response/0, event/0, watch_req/0]).

%% Types
-type reg_name() :: atom().

-type watch_id() :: integer().

-type options() :: #{retry_watch_interval_ms => integer(),
                     retry_watch_max_times => non_neg_integer(),
                     reg_name => reg_name(), %% register name for watcher process
                     client => name() %% watcher name for eetcd watch process
                    }.

-type revision() :: integer().

-type watch_req() :: context().

-type watching_item() :: {watch_req(), revision()}.

-type watching() :: #{watch_id() := watching_item()}.

-type statedata() :: #{callback_mod := module(),
                       watching := watching(),
                       retry_watch_interval_ms := integer(),
                       retry_watch_max_times := non_neg_integer(),
                       retry_times := non_neg_integer(),
                       client := name(),
                       conn := undefined | eetcd_watch:watch_conn()
                      }.

-type retry_times() :: non_neg_integer().

-type retry_item() :: {watch_req(), revision(), retry_times()}.

-type response() :: router_pb:'Etcd.WatchResponse'().

-type event() :: router_pb:'mvccpb.Event'().

-type state() :: ready | watching | recovering.

-type watcher() :: gen_statem:server_ref().

-define(DEFAULT_RETRY_WATCH_INTERVAL_MS, 3000).
-define(DEFAULT_RETRY_WATCH_MAX, infinity).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_WATCH_TIMEOUT, 3000).
-define(DEFAULT_UNWATCH_TIMEOUT, 3000).

-callback handle_unwatch([response()], [response()]) -> ok.

-callback handle_watch_events([event()]) -> ok.

-optional_callbacks([ handle_unwatch/2 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc send the watch request to the watcher.
-spec watch(watcher(), watch_req()) -> ok | {error, any()}.
watch(Watcher, WatchReq) ->
    watch(Watcher, WatchReq, ?DEFAULT_WATCH_TIMEOUT).

-spec watch(watcher(), watch_req(), timeout()) -> ok | {error, any()}.
watch(Watcher, WatchReq, Timeout) ->
    gen_statem:call(Watcher, {watch, WatchReq, Timeout}, infinity).

%% @doc send the unwatch request to the watcher.
%% Notice: it would unwatch all requests in
-spec unwatch(watcher()) -> ok | {error, any()}.
unwatch(Watcher) ->
    unwatch(Watcher, ?DEFAULT_UNWATCH_TIMEOUT).

-spec unwatch(watcher(), timeout()) -> ok | {error, any()}.
unwatch(Watcher, Timeout) ->
    gen_statem:call(Watcher, {unwatch, Timeout}, infinity).

%% @doc stop the eetcd watcher.
-spec stop(watcher()) -> ok.
stop(Watcher) ->
    gen_statem:stop(Watcher).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Create a watcher process and start watching the keys in `WatchReq'.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(module(), name(), options()) -> gen_statem:start_ret().
start_link(Mod, Client, Options) ->
    RegName = maps:get(reg_name, Options, Mod),
    gen_statem:start_link({local, RegName}, ?MODULE, {Mod, Client, Options}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialize the watcher state.
%% @end
%%--------------------------------------------------------------------
-spec init({module(), name(), options()}) -> {ok, ready, statedata()}.
init({Mod, Client, Options}) ->
    process_flag(trap_exit, true),
    RetryWatchInterval = maps:get(retry_watch_interval_ms, Options, ?DEFAULT_RETRY_WATCH_INTERVAL_MS),
    RetryWatchMax = maps:get(retry_watch_max_times, Options, ?DEFAULT_RETRY_WATCH_MAX),
    StateData = #{callback_mod => Mod,
                  watching => #{},
                  retry_watch_interval_ms => RetryWatchInterval,
                  retry_watch_max_times => RetryWatchMax,
                  conn => undefined,
                  retry_times => 0,
                  client => Client
                 },
    {ok, ready, StateData}.

-spec ready({call, gen_statem:from()}, {watch, watch_req(), timeout()}, statedata()) ->
          keep_state_and_data | {next_state, watching}.
ready({call, Caller}, WatchReq = {watch, #{key := _Key}, _Timeout}, StateData) ->
    {next_state, watching, StateData, [{next_event, {call, Caller}, WatchReq}]};
ready(EventType, Msg, _StateData) ->
    ?LOG_ERROR("Receive unexpected msg ~p of event type: ~p in ready state", [Msg, EventType]),
    keep_state_and_data.

-spec watching({call, gen_statem:from()}, {watch, watch_req(), timeout()}, statedata()) ->
          keep_state_and_data | {next_state, watching}.
watching({call, Caller}, {watch, WatchReq, Timeout},
         StateData = #{watching := Watching, conn := WatchConn, client := Client}) ->
    case eetcd_watch:watch(Client, WatchReq, WatchConn, Timeout) of
        {ok, NewWatchConn = #{watch_ids := WatchIds}, WatchId} ->
            #{revision := Revision} = maps:get(WatchId, WatchIds),
            NewWatching = Watching#{WatchId => {WatchReq, Revision}},
            NewStateData = StateData#{conn => NewWatchConn, watching => NewWatching},
            {keep_state, NewStateData, {reply, Caller, ok}};
        Err = {error, _Reason} ->
            {keep_state_and_data, {reply, Caller, Err}}
    end;

watching({call, Caller}, {unwatch, Timeout}, StateData = #{callback_mod := Mod, conn := Conn}) ->
    {Reply, Responses, Events} = case eetcd_watch:unwatch(Conn, Timeout) of
                                     {ok, Responses0, OtherEvents} ->
                                         {ok, Responses0, OtherEvents};
                                     {error, Reason, Responses0, OtherEvents} ->
                                         {{error, Reason}, Responses0, OtherEvents}
                                 end,
    try Mod:handle_unwatch(Responses, Events)
    catch _Class:_Reason -> ok
    end,
    NewStateData = StateData#{conn => undefined, watching => #{}},
    {next_state, ready, NewStateData, {reply, Caller, Reply}};
watching(info, Msg, StateData = #{conn := Conn,
                                  watching := Watching,
                                  callback_mod := Mod}) ->
    case eetcd_watch:watch_stream(Conn, Msg) of
        {ok, NewConn, WatchEvent = #{ events := Events }} ->
            try Mod:handle_watch_events(Events) catch _C:_R -> ok end,
            NewWatching = update_revision(Watching, WatchEvent),
            {keep_state, StateData#{conn => NewConn, watching => NewWatching}};
        {more, NewConn} ->
            NewStateData = StateData#{conn => NewConn},
            {keep_state, NewStateData};
        Err ->
            ?LOG_ERROR("Watching stream failed for ~p", [Err]),
            %% Due to the watch strategy is reuse the same stream by default.
            %% If one `watch_stream' failed, all watch requests should be retried.
            RetryItems = maps:values(Watching),
            NewStateData = StateData#{conn => undefined, watching => #{}},
            {next_state, recovering, NewStateData, {state_timeout, 0, RetryItems}}
    end;
watching(EventType, EventContent, _StateData) ->
    ?LOG_ERROR("Receive unexpected msg ~p of event type ~p in watching state",
               [EventContent, EventType]),
    keep_state_and_data.


-spec recovering(state_timeout, [retry_item()], statedata()) ->
          keep_state_and_data |
          {next_state, ready, statedata()} |
          {next_state, watching, statedata()} |
          {keep_state, statedata(), {state_timeout, non_neg_integer(), non_neg_integer()}} |
          {keep_state_and_data, {reply, gen_statem:from(), {error, recovering}}}.
recovering(state_timeout, _RetryItems , StateData = #{retry_watch_max_times := RetryWatchMaxTimes,
                                                      retry_times := RetryTimes})
  when RetryTimes >= RetryWatchMaxTimes ->
    ?LOG_WARNING("Exceeded max retry times: ~p", [RetryWatchMaxTimes]),
    {next_state, ready, StateData};
recovering(state_timeout, RetryItems, StateData = #{retry_watch_interval_ms := RetryInterval,
                                                    retry_times := RetryTimes}) ->
    case retry_watch(RetryItems, StateData) of
        {ok, NewStateData} ->
            {next_state, watching, NewStateData};
        {error, Reason} ->
            ?LOG_ERROR("Retry watch error for ~p", [Reason]),
            NewStateData = StateData#{rety_times => RetryTimes + 1},
            {keep_state, NewStateData, {state_timeout, RetryInterval, RetryItems}}
    end;
recovering({call, Caller}, _Request, _StateData) ->
    {keep_state_and_data, {reply, Caller, {error, recovering}}};
recovering(EventType, EventContent, _StateData) ->
    ?LOG_ERROR("Receive unexpected msg ~p of event type ~p in recovering state",
               [EventType, EventContent]),
    keep_state_and_data.


-spec terminate(any(), state(), statedata()) ->
          any().
terminate(_Reason, _State, _Data) ->
    void.

-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: state(), Data :: statedata(), Extra :: term()) ->
          {ok, NewState :: term(), NewData :: term()} |
          (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc If one request failed, it must be `stream_error' or `conn_error' or `http2_down' and
%% all requests should be retried.
-spec retry_watch([retry_item()], statedata()) -> {ok, statedata()} | {error, any()}.
retry_watch([], StateData) ->
    {ok, StateData};
retry_watch([{WatchReq, Revision} | RestRetryItem], StateData = #{conn := Conn,
                                                                  client := Client,
                                                                  watching := Watching
                                                                 }) ->
    NewWatchReq = eetcd_watch:with_start_revision(WatchReq, Revision + 1),
    case eetcd_watch:watch(Client, NewWatchReq, Conn, ?DEFAULT_WATCH_TIMEOUT) of
        {ok, NewWatchConn = #{watch_ids := WatchIds}, WatchId} ->
            #{revision := NewRevision} = maps:get(WatchId, WatchIds),
            NewWatching = Watching#{WatchId => {NewWatchReq, NewRevision}},
            NewStateData = StateData#{conn => NewWatchConn, watching => NewWatching},
            retry_watch(RestRetryItem, NewStateData);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_revision(watching(), router_pb:'Etcd.WatchResponse'()) -> watching().
update_revision(Watching, #{header := #{revision := Rev}, watch_id := WatchId}) ->
    maps:update_with(WatchId, fun({Key, _}) -> {Key, Rev} end, Watching).

