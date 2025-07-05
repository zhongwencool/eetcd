-module(eetcd_lease).
-include("eetcd.hrl").
-behaviour(gen_server).

-export([time_to_live/3]).
-export([keep_alive/2, keep_alive_once/2]).
-export([gun_pid/1]).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BLOCK, 3).
-define(LeaseNotFound, <<"etcdserver: requested lease not found">>).
-define(Event(ID, Reason), #{event => 'KeepAliveHalted', lease_id => ID, reason => Reason}).
-define(TRY_RECONNECTING, try_reconnecting).

%% @doc TimeToLive retrieves the lease information of the given lease ID.
%% The 3rd argument is a option of `NeedAttachedKeys'.
-spec time_to_live(etcd_name(), pos_integer(), boolean()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()}|{error, eetcd_error()}.
time_to_live(EtcdName, LeaseID, WithKeys) when is_boolean(WithKeys) ->
    case eetcd_lease_gen:lease_time_to_live(EtcdName, #{'ID' => LeaseID, keys => WithKeys}) of
        {ok, #{'TTL' := TTL}} when TTL =< 0 ->
            {error, {grpc_error, #{'grpc-status' => ?GRPC_STATUS_NOT_FOUND, 'grpc-message' => ?LeaseNotFound}}};
        {ok, _Reps} = Status -> Status;
        {error, _Reason} = Err -> Err
    end.

%% @doc KeepAlive attempts to keep the given lease alive forever.
%%
%% If client keep alive processes halts with an unexpected error (e.g. "etcdserver: no leader") or canceled by others,
%% KeepAlive process send a KeepAliveHalted event(`#{event => 'KeepAliveHalted', lease_id => ID, reason => Reason}') to caller.
%%
%% KeepAlive makes best efforts to keep lease TTL, event the connection disconnect in 0 to ttl seconds.
%% todo more detail
-spec keep_alive(etcd_name(), pos_integer()) -> {ok, pid()} |{error, term()}.
keep_alive(EtcdName, LeaseID) ->
    case eetcd_lease_sup:start_child(EtcdName, LeaseID) of
        {ok, Pid} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason}
    end.

%% @doc KeepAliveOnce renews the lease once. The response corresponds to the
%% first message from calling KeepAlive. If the response has a recoverable
%% error, KeepAliveOnce will not retry the RPC with a new keep alive message.
%% In most of the cases, Keepalive should be used instead of KeepAliveOnce.
-spec keep_alive_once(etcd_name(), pos_integer()) ->
    {ok, router_pb:'Etcd.LeaseKeepAliveResponse'()}|{error, eetcd_error()}.
keep_alive_once(EtcdName, LeaseID) when is_atom(EtcdName) ->
    case eetcd_lease_gen:lease_keep_alive(EtcdName) of
        {ok, Gun, StreamRef, PbModule} ->
            MRef = erlang:monitor(process, Gun),
            Res = keep_alive_once(Gun, StreamRef, LeaseID, MRef, PbModule),
            gun:cancel(Gun, StreamRef),
            erlang:demonitor(MRef, [flush]),
            Res;
        Err -> Err
    end.

%% @private
gun_pid(LeaseKeepalivePid) ->
    gen_server:call(LeaseKeepalivePid, gun_pid).

-spec start_link(pid(), etcd_name(), integer()) -> Result
      when Result :: {ok, Pid} | ignore | {error, Error},
           Pid :: pid(),
           Error :: {already_started, Pid} | term().
%% The gen_server:start_ret() return type was introduced since OTP 25,
%% but for backward compatibility, we still use the old return type.
start_link(Caller, EtcdName, LeaseID) ->
    gen_server:start_link(?MODULE, [Caller, EtcdName, LeaseID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Caller, EtcdName, LeaseID]) ->
    process_flag(trap_exit, true),
    case first_keep_alive_once(EtcdName, LeaseID) of
        {ok, Gun, Ref, MRef, TTL, PbModule} ->
            %% eqwalizer:ignore
            After = erlang:max(round(TTL / ?BLOCK), 1) * 1000,
            TimeRef = schedule_next_keep_alive(After),
            {ok, #{name => EtcdName, gun => Gun, caller => Caller, pb_module => PbModule,
                ttl => TTL, lease_id => LeaseID,
                stream_ref => Ref, monitor_ref => MRef, next_ref => TimeRef,
                ongoing => 0, last_disconnect => 0}
            };
        {error, Err} -> {stop, {shutdown, Err}}
    end.

handle_call(gun_pid, _From, State = #{gun := Gun}) ->
    {reply, Gun, State, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State, State}.

handle_cast(_Request, State) ->
    {noreply, State, State}.

handle_info(?TRY_RECONNECTING, State) ->
    try_reconnecting(State);

handle_info({'DOWN', Ref, process, Gun, Reason}, #{gun := Gun, monitor_ref := Ref} = State) ->
    ?LOG_INFO("Lease KeepAlive: ~p find gun(~p) process stop ~p", [self(), Gun, Reason]),
    reconnect(State);

handle_info({gun_data, _Pid, Ref, nofin, Data},
    State = #{stream_ref := Ref, ongoing := Ongoing, pb_module := PbModule}) ->
    case eetcd_grpc:decode(identity, Data, 'Etcd.LeaseKeepAliveResponse', PbModule) of
        {ok, #{'ID' := _ID, 'TTL' := TTL}, <<>>} when TTL =< 0 ->
            {stop, {shutdown, lease_not_found}, State};
        {ok, #{'TTL' := _TTL}, <<>>} -> {noreply, State#{ongoing => Ongoing - 1}}
    end;

%% [{<<"grpc-status">>,<<"14">>},{<<"grpc-message">>,<<"etcdserver: no leader">>}]}
handle_info({gun_trailers, Gun, StreamRef, Header},
    State = #{name := EtcdName, stream_ref := StreamRef, gun := Gun}) ->
    check_leader(Header, EtcdName),
    reconnect(State);
%% it will receive another stream_ref gun_response to this process, notifying no leader event.
%% [{<<"grpc-status">>,<<"14">>},{<<"grpc-message">>,<<"etcdserver: no leader">>}]}
handle_info({gun_response, Gun, _StreamRef, _Fin, 200, Header},
    State = #{name := EtcdName, gun := Gun}) ->
    check_leader(Header, EtcdName),
    reconnect(State);
handle_info({keep_ttl, Next}, State) ->
    keep_ttl(Next, State);
handle_info({gun_error, Gun, _StreamRef, _Reason}, State = #{gun := Gun}) ->
    reconnect(State);
handle_info({gun_error, Gun, _Reason}, State = #{gun := Gun}) ->
    reconnect(State);

handle_info(Info, State) ->
    ?LOG_ERROR("Leaser({~p,~p}) receive unknown msg ~p~n state~p",
        [?MODULE, self(), Info, State]),
    {noreply, State}.

terminate(Reason, #{stream_ref := Ref, gun := Gun, lease_id := ID, caller := Caller}) ->
    case Reason of
        normal ->
            erlang:send(Caller, ?Event(ID, <<"stream closed manually">>));
        shutdown ->
            erlang:send(Caller, ?Event(ID, <<"stream closed manually">>));
        {shutdown, lease_not_found} ->
            erlang:send(Caller, ?Event(ID, ?LeaseNotFound));
        {shutdown, Reason} ->
            erlang:send(Caller, ?Event(ID, Reason));
        _ ->
            erlang:send(Caller,
                        ?Event(ID, iolist_to_binary([<<"stream closed unexpectedly: ">>,
                                                     io_lib:format("~p", [Reason])])))
    end,
    gun:cancel(Gun, Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec first_keep_alive_once(etcd_name(), pos_integer()) ->
    {ok, Gun :: pid(), StreamRef :: eetcd:stream_ref(), MRef :: reference(),
     TTL :: non_neg_integer(), PbModule :: module()} | {error, eetcd_error()}.
first_keep_alive_once(EtcdName, LeaseID) ->
    case eetcd_lease_gen:lease_keep_alive(EtcdName) of
        {ok, Gun, StreamRef, PbModule} ->
            MRef = erlang:monitor(process, Gun),
            case keep_alive_once(Gun, StreamRef, LeaseID, MRef, PbModule) of
                {ok, #{'TTL' := TTL}} -> {ok, Gun, StreamRef, MRef, TTL, PbModule};
                Err -> Err
            end;
        {error, _Reason} = Err -> Err
    end.

keep_alive_once(Gun, StreamRef, LeaseID, MRef, PbModule) ->
    eetcd_stream:data(Gun, StreamRef, #{'ID' => LeaseID}, 'Etcd.LeaseKeepAliveRequest', nofin, PbModule),
    case eetcd_stream:await(Gun, StreamRef, 5000, MRef) of
        {response, nofin, 200, _Headers} ->
            case eetcd_stream:await(Gun, StreamRef, 5000, MRef) of
                {data, nofin, ResBody} ->
                    case eetcd_grpc:decode(identity, ResBody, 'Etcd.LeaseKeepAliveResponse', PbModule) of
                        {ok, #{'TTL' := TTL}, <<>>} when TTL =< 0 ->
                            {error, #{'grpc-status' => ?GRPC_STATUS_NOT_FOUND, 'grpc-message' => ?LeaseNotFound}};
                        {ok, Resp, <<>>} -> {ok, Resp}
                    end;
                {error, _Reason} = Err1 -> Err1
            end;
        {response, fin, 200, RespHeaders} -> {error, eetcd_grpc:grpc_status(RespHeaders)};
        {error, _Reason} = Err2 -> Err2
    end.

keep_ttl(Next, State) ->
    #{stream_ref := Ref, gun := Gun, pb_module := PbModule,
      lease_id := ID, ongoing := Ongoing, name := EtcdName} = State,
    case Ongoing =< 2 * ?BLOCK of
        true ->
            eetcd_stream:data(Gun, Ref, #{'ID' => ID}, 'Etcd.LeaseKeepAliveRequest', nofin, PbModule),
            TimeRef = schedule_next_keep_alive(Next),
            {noreply, State#{ongoing => Ongoing + 1, next_ref => TimeRef}};
        false ->
            case time_to_live(EtcdName, ID, false) of
                {ok, _} -> reconnect(State);
                {error, Reason} ->
                    {stop, {shutdown, Reason}, State}
            end
    end.

schedule_next_keep_alive(After) ->
    erlang:send_after(After, self(), {keep_ttl, After}).

reconnect(State) ->
    #{next_ref := NextRef, stream_ref := Ref, gun := Gun} = State,
    erlang:cancel_timer(NextRef),
    gun:cancel(Gun, Ref),
    NewState = State#{ongoing => 0, last_disconnect => erlang:system_time(second)},
    try_reconnecting(NewState).

try_reconnecting(State) ->
    #{
        name := EtcdName, lease_id := LeaseID,
        last_disconnect := LastDisconnect, ttl := TTL,
        caller := Caller
    } = State,
    case erlang:system_time(second) - LastDisconnect > TTL of
        true ->
            case time_to_live(EtcdName, LeaseID, false) of
                {ok, _} -> reconnect(State);
                {error, Reason} ->
                    {stop, {shutdown, Reason}, State}
            end;
        false ->
            case init([Caller, EtcdName, LeaseID]) of
                {ok, NewState} -> {noreply, NewState};
                {stop, {shutdown, {grpc_error, #{'grpc-status' := ?GRPC_STATUS_NOT_FOUND} = Reason}}} ->
                    {stop, {shutdown, Reason}, State};
                {stop, _Reason} ->
                    erlang:send_after(1000, self(), ?TRY_RECONNECTING),
                    {noreply, State}
            end
    end.
check_leader(Header, EtcdName) ->
    case eetcd_grpc:grpc_status(Header) of
        #{'grpc-status' := 14} -> eetcd_conn:check_health(EtcdName);
        _ -> ok
    end.
