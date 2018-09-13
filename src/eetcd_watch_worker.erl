-module(eetcd_watch_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eetcd.hrl").

-record(state, {ref, watch_id, callback}).
%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(router_pb:'Etcd.WatchRequest'(), Callback) -> {ok, pid()} when
    Callback :: fun((router_pb:'Etcd.WatchResponse'()) -> term()).
start_link(Request, Callback) ->
    gen_server:start_link(?MODULE, [Request, Callback], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Request, Callback]) ->
    StreamRef = eetcd_watch:watch(Request),
    erlang:process_flag(trap_exit, true),
    {ok, #state{ref = StreamRef, callback = Callback}}.

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

handle_info(Info, State) ->
    error_logger:warning_msg("Watcher({~p,~p}) receive unknow msg ~p~n state~p~n",
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

handle_change_event(State = #state{callback = Callback}, Data) ->
    Resp = eetcd_grpc:decode(identity, Data, 'Etcd.WatchResponse'),
    try
        Callback(Resp)
    catch E:R ->
        error_logger:error_msg("Watcher(~p) run callback crash ~p~n Event~p~n",
            [self(), {E, R}, Resp])
    end,
    case Resp of
        #'Etcd.WatchResponse'{created = true, watch_id = WatchId} ->
            {noreply, State#state{watch_id = WatchId}};
        #'Etcd.WatchResponse'{canceled = true} ->
            {stop, normal, State};
        _ ->
            {noreply, State}
    end.
        