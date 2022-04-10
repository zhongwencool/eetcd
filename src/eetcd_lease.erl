-module(eetcd_lease).
-include("eetcd.hrl").
-behaviour(gen_server).

-export([new/1, with_timeout/2]).
-export([grant/2, revoke/2, time_to_live/3, leases/1]).
-export([keep_alive/2, keep_alive_once/2]).
-export([close/0]).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(BLOCK, 3).
-define(LeaseNotFound, <<"etcdserver: requested lease not found">>).
-define(Event(ID, Reason), #{event => 'KeepAliveHalted', lease_id => ID, reason => Reason}).
-define(TRY_RECONNECTING, try_reconnecting).

%%% @doc Create context for request.
-spec new(name()|context()) -> context().
new(Context) -> eetcd:new(Context).

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()|infinity) -> context().
with_timeout(Context, Timeout) -> eetcd:with_timeout(Context, Timeout).

%% @doc Grant creates a new lease with the provided TTL in seconds.
-spec grant(context(), pos_integer()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()}|{error, eetcd_error()}.
grant(Context, TTL) ->
    C1 = new(Context),
    C2 = maps:put('TTL', TTL, C1),
    eetcd_lease_gen:lease_grant(C2).

%% @doc Revoke revokes the given lease.
-spec revoke(name()|context(), pos_integer()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()}|{error, eetcd_error()}.
revoke(Context, LeaseID) ->
    C1 = new(Context),
    C2 = maps:put('ID', LeaseID, C1),
    eetcd_lease_gen:lease_revoke(C2).

%% @doc TimeToLive retrieves the lease information of the given lease ID.
%% The 3rd argument is a option of `NeedAttachedKeys'.
-spec time_to_live(context(), pos_integer(), boolean()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()}|{error, eetcd_error()}.
time_to_live(Context, LeaseID, WithKeys) when is_boolean(WithKeys) ->
    C1 = new(Context),
    C2 = maps:put('ID', LeaseID, C1),
    C3 = maps:put(keys, WithKeys, C2),
    case eetcd_lease_gen:lease_time_to_live(C3) of
        {ok, #{'TTL' := TTL}} when TTL =< 0 ->
            {error, #{'grpc-status' => ?GRPC_STATUS_NOT_FOUND, 'grpc-message' => ?LeaseNotFound}};
        {ok, _Reps} = Status -> Status;
        {error, _Reason} = Err -> Err
    end.

%% @doc Leases retrieves all leases.
-spec leases(context()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()}|{error, eetcd_error()}.
leases(ConnName) ->
    C1 = new(ConnName),
    eetcd_lease_gen:lease_leases(C1).

%% @doc KeepAlive attempts to keep the given lease alive forever.
%%
%% If client keep alive processes halts with an unexpected error (e.g. "etcdserver: no leader") or canceled by others,
%% KeepAlive process send a KeepAliveHalted event(`#{event => 'KeepAliveHalted', lease_id => ID, reason => Reason}') to caller.
%%
%% KeepAlive makes best efforts to keep lease TTL, event the connection disconnect in 0 to ttl seconds.
%% todo more detail
-spec keep_alive(name(), pos_integer()) -> {ok, pid()} |{error, term()}.
keep_alive(Name, LeaseID) ->
    case eetcd_lease_sup:start_child(Name, LeaseID) of
        {ok, Pid} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason}
    end.

%% @doc KeepAliveOnce renews the lease once. The response corresponds to the
%% first message from calling KeepAlive. If the response has a recoverable
%% error, KeepAliveOnce will not retry the RPC with a new keep alive message.
%% In most of the cases, Keepalive should be used instead of KeepAliveOnce.
-spec keep_alive_once(name(), pos_integer()) ->
    {ok, router_pb:'Etcd.LeaseKeepAliveResponse'()}|{error, eetcd_error()}.
keep_alive_once(Name, LeaseID) when is_atom(Name) orelse is_reference(Name) ->
    case eetcd_lease_gen:lease_keep_alive(Name) of
        {ok, Gun, StreamRef} ->
            MRef = erlang:monitor(process, Gun),
            Res = keep_alive_once(Gun, StreamRef, LeaseID, MRef),
            gun:cancel(Gun, StreamRef),
            erlang:demonitor(MRef, [flush]),
            Res;
        Err -> Err
    end.

%% @doc Close releases all resources Lease keeps for efficient communication with the etcd server.
%% Return the count of all close processes.
%% If you want to revokes the given lease, please use `revoke/2'.
-spec close() -> pos_integer().
close() ->
    lists:foldl(
        fun({_Id, Pid, _Type, _Modules}, Acc) ->
            gen_server:cast(Pid, close),
            Acc + 1
        end, 0,
        supervisor:which_children(eetcd_lease_sup)).

-spec start_link(pid(), name(), integer()) -> {ok, pid()}.
start_link(Caller, Name, LeaseID) ->
    gen_server:start_link(?MODULE, [Caller, Name, LeaseID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Caller, Name, LeaseID]) ->
    case first_keep_alive_once(Name, LeaseID) of
        {ok, Gun, Ref, MRef, TTL} ->
            After = erlang:max(round(TTL / ?BLOCK), 1) * 1000,
            TimeRef = schedule_next_keep_alive(After),
            {ok, #{name => Name, gun => Gun, caller => Caller,
                ttl => TTL, lease_id => LeaseID,
                stream_ref => Ref, monitor_ref => MRef, next_ref => TimeRef,
                ongoing => 0, last_disconnect => 0}
            };
        {error, Err} -> {stop, {shutdown, Err}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ignore, State, State}.

handle_cast(close, State = #{lease_id := ID, caller := Caller}) ->
    erlang:send(Caller, ?Event(ID, <<"stream closed manually">>)),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State, State}.

handle_info(?TRY_RECONNECTING, State) ->
    try_reconnecting(State);

handle_info({'DOWN', Ref, process, Gun, Reason}, #{gun := Gun, monitor_ref := Ref} = State) ->
    ?LOG_INFO("Lease KeepAlive: ~p find gun(~p) process stop ~p~n", [self(), Gun, Reason]),
    reconnect(State);

handle_info({gun_data, _Pid, Ref, nofin, Data},
    State = #{stream_ref := Ref, ongoing := Ongoing, caller := Caller}) ->
    case eetcd_grpc:decode(identity, Data, 'Etcd.LeaseKeepAliveResponse') of
        {ok, #{'ID' := ID, 'TTL' := TTL}, <<>>} when TTL =< 0 ->
            Event = ?Event(ID, ?LeaseNotFound),
            erlang:send(Caller, Event),
            {stop, {shutdown, Event}, State};
        {ok, #{'TTL' := _TTL}, <<>>} -> {noreply, State#{ongoing => Ongoing - 1}}
    end;

%% [{<<"grpc-status">>,<<"14">>},{<<"grpc-message">>,<<"etcdserver: no leader">>}]}
handle_info({gun_trailers, Gun, StreamRef, Header},
    State = #{name := Name, stream_ref := StreamRef, gun := Gun}) ->
    check_leader(Header, Name),
    reconnect(State);
%% it will receive another stream_ref gun_response to this process, notifying no leader event.
%% [{<<"grpc-status">>,<<"14">>},{<<"grpc-message">>,<<"etcdserver: no leader">>}]}
handle_info({gun_response, Gun, _StreamRef, _Fin, 200, Header},
    State = #{name := Name, gun := Gun}) ->
    check_leader(Header, Name),
    reconnect(State);
handle_info({keep_ttl, Next}, State) ->
    keep_ttl(Next, State);
handle_info({gun_error, Gun, _StreamRef, _Reason}, State = #{gun := Gun}) ->
    reconnect(State);
handle_info({gun_error, Gun, _Reason}, State = #{gun := Gun}) ->
    reconnect(State);

handle_info(Info, State) ->
    ?LOG_ERROR("Leaser({~p,~p}) receive unknown msg ~p~n state~p~n",
        [?MODULE, self(), Info, State]),
    {noreply, State}.

terminate(_Reason, #{stream_ref := Ref, gun := Gun}) ->
    gun:cancel(Gun, Ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
first_keep_alive_once(Name, LeaseID) ->
    case eetcd_lease_gen:lease_keep_alive(Name) of
        {ok, Gun, StreamRef} ->
            MRef = erlang:monitor(process, Gun),
            case keep_alive_once(Gun, StreamRef, LeaseID, MRef) of
                {ok, #{'TTL' := TTL}} -> {ok, Gun, StreamRef, MRef, TTL};
                Err -> Err
            end;
        {error, _Reason} = Err -> Err
    end.

keep_alive_once(Gun, StreamRef, LeaseID, MRef) ->
    eetcd_stream:data(Gun, StreamRef, #{'ID' => LeaseID}, 'Etcd.LeaseKeepAliveRequest', nofin),
    case eetcd_stream:await(Gun, StreamRef, 5000, MRef) of
        {response, nofin, 200, _Headers} ->
            case eetcd_stream:await(Gun, StreamRef, 5000, MRef) of
                {data, nofin, ResBody} ->
                    case eetcd_grpc:decode(identity, ResBody, 'Etcd.LeaseKeepAliveResponse') of
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
    #{stream_ref := Ref, gun := Gun,
        lease_id := ID, ongoing := Ongoing,
        name := Name, caller := Caller} = State,
    case Ongoing =< 2 * ?BLOCK of
        true ->
            eetcd_stream:data(Gun, Ref, #{'ID' => ID}, 'Etcd.LeaseKeepAliveRequest', nofin),
            TimeRef = schedule_next_keep_alive(Next),
            {noreply, State#{ongoing => Ongoing + 1, next_ref => TimeRef}};
        false ->
            case time_to_live(Name, ID, false) of
                {ok, _} -> reconnect(State);
                {error, Reason} ->
                    Event = ?Event(ID, Reason),
                    erlang:send(Caller, Event),
                    {stop, {shutdown, Event}, State}
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
        name := Name, lease_id := LeaseID,
        last_disconnect := LastDisconnect, ttl := TTL,
        caller := Caller
    } = State,
    case erlang:system_time(second) - LastDisconnect > TTL of
        true ->
            case time_to_live(Name, LeaseID, false) of
                {ok, _} -> reconnect(State);
                {error, Reason} ->
                    Event = ?Event(LeaseID, Reason),
                    erlang:send(Caller, Event),
                    {stop, {shutdown, Event}, State}
            end;
        false ->
            case init([Caller, Name, LeaseID]) of
                {ok, NewState} -> {noreply, NewState};
                {stop, {shutdown, #{'grpc-status' := ?GRPC_STATUS_NOT_FOUND} = Reason}} ->
                    Event = ?Event(LeaseID, Reason),
                    erlang:send(Caller, Event),
                    {stop, {shutdown, Event}, State};
                {stop, _Reason} ->
                    erlang:send_after(1000, self(), ?TRY_RECONNECTING),
                    {noreply, State}
            end
    end.
check_leader(Header, Name) ->
    case eetcd_grpc:grpc_status(Header) of
        #{'grpc-status' := 14} -> eetcd_conn:check_health(Name);
        _ -> ok
    end.
