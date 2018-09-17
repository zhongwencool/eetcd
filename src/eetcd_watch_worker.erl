-module(eetcd_watch_worker).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eetcd.hrl").

-record(state, {ref, watch_id, callback, ignore_create = true, ignore_cancel = true}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(router_pb:'Etcd.WatchRequest'(), Callback, list()) -> {ok, pid()} when
    Callback :: fun((router_pb:'Etcd.WatchResponse'()) -> term()).
start_link(Request, Callback, Options) ->
    gen_server:start_link(?MODULE, [Request, Callback, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Request, Callback, Options]) ->
    StreamRef = eetcd_watch:watch(Request),
    erlang:process_flag(trap_exit, true),
    {ok, #state{
        ref = StreamRef,
        callback = Callback,
        ignore_create = proplists:get_value(ignore_create, Options, true),
        ignore_cancel = proplists:get_value(ignore_cancel, Options, true)
    }}.

handle_call(unwatch, _From, State = #state{ref = Ref, watch_id = WatchId}) ->
    Request = #'Etcd.WatchRequest'{
        request_union = {cancel_request, #'Etcd.WatchCancelRequest'{
            watch_id = WatchId
        }}},
    eetcd_stream:data(Ref, Request, fin),
    {reply, ok, State};
handle_call(watch_id, _From, State = #state{watch_id = WatchId}) ->
    {reply, WatchId, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({gun_response, _Pid, Ref, nofin, 200, _Headers}, State = #state{ref = Ref}) ->
    {noreply, State};

handle_info({gun_data, _Pid, Ref, nofin, Data}, State = #state{ref = Ref}) ->
    handle_change_event(State, Data);

handle_info({gun_error, Pid, StreamRef, Reason}, State =
    #state{callback = Callback, watch_id = WatchId}) ->
    error_logger:warning_msg("Watcher({~p,~p}) need reconnect gun_error(~p) ~p~n state~p~n",
        [?MODULE, self(), Pid, {StreamRef, Reason}, State]),
    Reps = {gun_error, WatchId, Reason},
    run_callback(Callback, Reps),
    {stop, Reason, State};
handle_info({gun_error, Pid, Reason}, State =
    #state{callback = Callback, watch_id = WatchId}) ->
    error_logger:warning_msg("Watcher({~p,~p}) need reconnect gun_error(~p) ~p~n state~p~n",
        [?MODULE, self(), Pid, Reason, State]),
    Reps = {gun_error, WatchId, Reason},
    run_callback(Callback, Reps),
    {stop, Reason, State};

handle_info(Info, State) ->
    error_logger:error_msg("Watcher({~p,~p}) receive unknow msg ~p~n state~p~n",
        [?MODULE, self(), Info, State]),
    {noreply, State}.

terminate(_Reason, #state{ref = Ref}) ->
    Pid = eetcd_http2_keeper:get_http2_client_pid(),
    ok = gun:cancel(Pid, Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_change_event(State = #state{callback = Callback,
    ignore_create = IgnoreCreate, ignore_cancel = IgnoreCancel},
    Data) ->
    #'Etcd.WatchResponse'{created = Create,
        watch_id = WatchId, canceled = Cancel} = Resp
        = eetcd_grpc:decode(identity, Data, 'Etcd.WatchResponse'),
    if
        Create andalso IgnoreCreate ->
            {noreply, State#state{watch_id = WatchId}};
        Create ->
            run_callback(Callback, Resp),
            {noreply, State#state{watch_id = WatchId}};
        Cancel andalso IgnoreCancel ->
            {stop, normal, State};
        Cancel ->
            run_callback(Callback, Resp),
            {stop, normal, State};
        true ->
            run_callback(Callback, Resp),
            {noreply, State}
    end.

run_callback(Callback, Resp) ->
    try
        Callback(Resp)
    catch E:R ->
        error_logger:error_msg("Watcher(~p) run callback crash ~p~n Event~p~n",
            [self(), {E, R}, Resp])
    end.
