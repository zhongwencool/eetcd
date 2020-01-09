-module(eetcd_watch_worker).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eetcd.hrl").

-record(state, {stream_ref, client_ref, client_pid, watch_id, callback, data, ignore_create = true, ignore_cancel = true}).

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
    erlang:process_flag(trap_exit, true),
    {ok, Pid} = eetcd_http2_keeper:safe_get_http2_client_pid([240, 480, 960]),
    ClientRef = erlang:monitor(process, Pid),
    StreamRef = eetcd_watch:watch(Request),
    {ok, #state{
        stream_ref = StreamRef,
        client_ref = ClientRef,
        client_pid = Pid,
        callback = Callback,
        data = <<>>,
        ignore_create = proplists:get_value(ignore_create, Options, true),
        ignore_cancel = proplists:get_value(ignore_cancel, Options, true)
    }}.

handle_call(unwatch, _From, State = #state{stream_ref = Ref, watch_id = WatchId}) ->
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

handle_info({gun_response, _Pid, Ref, nofin, 200, _Headers}, State = #state{stream_ref = Ref}) ->
    {noreply, State};

handle_info({gun_data, _Pid, Ref, nofin, Data}, State = #state{stream_ref = Ref}) ->
    handle_gun_data(State, Data);

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
handle_info({'DOWN', Ref, process, Pid, Reason}, State = #state{client_ref = Ref, client_pid = Pid}) ->
    error_logger:warning_msg("~p ~p stop for gun(~p) process stop ~p~n", [?MODULE, self(), Pid, Reason]),
    {stop, eetcd_client_exit, State};

handle_info(Info, State) ->
    error_logger:error_msg("Watcher({~p,~p}) receive unknow msg ~p~n state~p~n",
        [?MODULE, self(), Info, State]),
    {noreply, State}.

terminate(normal, #state{stream_ref = Ref, client_pid = Pid}) ->
    ok = gun:cancel(Pid, Ref),
    ok;
terminate(Reason=eetcd_client_exit, State) ->
    info_watcher_owner_terminate(State, Reason),
    ok;
terminate(Reason, State = #state{stream_ref = Ref, client_pid = Pid}) ->
    info_watcher_owner_terminate(State, Reason),
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

info_watcher_owner_terminate(#state{callback = Callback}, Reason) ->
    run_callback(Callback, {eetcd_watcher_exit, self(), Reason}).

handle_gun_data(State = #state{data = OldData}, NewData) when byte_size(NewData) + byte_size(OldData) > 5 ->
    CurrentData = <<OldData/binary, NewData/binary>>,
    <<Compact:8, Length:32, Binary/binary>> = CurrentData,
    if
        byte_size(Binary) < Length ->
            %% current data not completion, wait for next gun_data msg
            {noreply, State#state{data = CurrentData}};
        true ->
            <<OnePacket:Length/binary, LeftBinary/binary>> = Binary,
            NewState = State#state{data = LeftBinary},
            case handle_change_event(NewState, <<Compact:8, Length:32, OnePacket/binary>>) of
                {stop, _, _} = Resp ->
                    Resp;
                {noreply, NewState1} ->
                    handle_gun_data(NewState1, <<>>)
            end
    end;
handle_gun_data(State = #state{data = OldData}, NewData) ->
    {noreply, State#state{data = <<OldData/binary, NewData/binary>>}}.
