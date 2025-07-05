%% @private
-module(eetcd_conn).
-include("eetcd.hrl").

-behaviour(gen_server).

%% API
-export([
    open/1, close/1,
    round_robin_select/1, pick_member/2,
    check_health/1, refresh_token/2,
    set_credentails/3, unset_credentails/1,
    update_member_list/2,
    member_id_hex/1
]).
-export([info/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

-define(check_health, check_health_msg).
-define(auto_sync, auto_sync).
-define(refresh_token, refresh_token_msg).
-define(reconnect, reconnect).

-type state() :: #{
    name := atom(),
    mode := random | connect_all,
    members := members(),
    credentials := undefined | #{name := string(), password := string()},
    auth_token := undefined | iodata(),
    auto_sync_interval_ms := non_neg_integer(),
    gun_opts := gun:opts(),
    options := eetcd:opts(),
    health_ref := undefined | reference(),
    reconn_ref := undefined | reference(),
    sync_ref := undefined | reference(),
    active_conns := [conn()],
    opening_conns := [conn()]
}.

-type conn() :: {member_id(), GunPid :: pid(), MonitorRef :: reference()}.
-type members() :: #{member_id() => {Host :: string(), inet:port_number(), tcp | tls}}.

-define(DEFAULT_REPLY_TIMEOUT, 10000).
-define(DEFAULT_REQ_OPTS, [{reply_timeout, ?DEFAULT_REPLY_TIMEOUT}]).

%%%===================================================================
%%% API
%%%===================================================================

open({EtcdName, _, _} = Args) ->
    case gen_server:start_link({local, EtcdName}, ?MODULE, Args, []) of
        {ok, Pid} -> {ok, Pid};
        {error, _} = Err -> Err
    end.

close(EtcdName) ->
    eetcd:close(EtcdName).

round_robin_select(EtcdName) ->
    case gen_server:call(EtcdName, round_robin_select) of
        {ok, GunPid, undefined} ->
            {ok, GunPid, ?HEADERS};
        {ok, GunPid, Token} ->
            {ok, GunPid, [{<<"token">>, Token} | ?HEADERS]};
        {error, _} = E -> E
    end.

pick_member(EtcdName, MemberId) ->
    case gen_server:call(EtcdName, {pick_member, MemberId}) of
        {ok, GunPid, undefined} ->
            {ok, GunPid, ?HEADERS};
        {ok, GunPid, Token} ->
            {ok, GunPid, [{<<"token">>, Token} | ?HEADERS]};
        {error, _} = E -> E
    end.

set_credentails(EtcdName, UserName, Password) ->
    gen_server:call(EtcdName, {set_credentials, {UserName, Password}}).

unset_credentails(EtcdName) ->
    gen_server:call(EtcdName, {set_credentials, undefined}).

update_member_list(_Name, Members) when map_size(Members) =:= 0 ->
    {error, empty_member_list};
update_member_list(EtcdName, Members) ->
    gen_server:cast(EtcdName, {update_member_list, Members}).

check_health(EtcdName) ->
    gen_server:cast(EtcdName, ?check_health).

refresh_token(EtcdName, Headers) ->
    case gen_server:call(EtcdName, ?refresh_token) of
        {ok, undefined} ->
            lists:keydelete(<<"token">>, 1, Headers);
        {ok, NewToken} ->
            lists:keyreplace(<<"token">>, 1, Headers, {<<"token">>, NewToken});
        {error, Reason} ->
            ?LOG_WARNING("Refresh token failed: ~p", [Reason]),
            Headers
    end.

%% @private
info(ServerRef) ->
    gen_server:call(ServerRef, info).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({EtcdName, Hosts, Options}) ->
    erlang:process_flag(trap_exit, true),
    GunOpts = #{
        protocols => [http2],
        connect_timeout => proplists:get_value(connect_timeout, Options, 1000),
        domain_lookup_timeout => proplists:get_value(domain_lookup_timeout, Options, 1000),
        tls_handshake_timeout => proplists:get_value(tls_handshake_timeout, Options, 3000),
        http2_opts => #{keepalive => infinity},
        retry_timeout => proplists:get_value(retry_timeout, Options, 5000),
        transport => proplists:get_value(transport, Options, tcp),
        tcp_opts => proplists:get_value(tcp_opts, Options, []),
        tls_opts => proplists:get_value(tls_opts, Options, [])
    },
    AutoSyncInterval = proplists:get_value(auto_sync_interval_ms, Options, 0),

    try
        Members =
            case ets:lookup(?ETCD_CLIENT_CACHE, EtcdName) of
                [#eetcd_client{members = CachedMembers}] ->
                    ?LOG_NOTICE("eetcd client ~p found cached members: ~p", [EtcdName, CachedMembers]),
                    CachedMembers;
                [] ->
                    NewMembers = get_member_list(EtcdName, shuffle(Hosts), GunOpts),
                    ?LOG_NOTICE("eetcd client ~p found members: ~p", [EtcdName, NewMembers]),
                    ets:insert(?ETCD_CLIENT_CACHE, #eetcd_client{name = EtcdName, members = NewMembers}),
                    NewMembers
            end,
        State0 = #{
            name => EtcdName,
            mode => proplists:get_value(mode, Options, connect_all),
            members => Members,
            credentials => undefined,
            auth_token => undefined,
            auto_sync_interval_ms => AutoSyncInterval,
            gun_opts => GunOpts,
            options => Options,
            health_ref => undefined,
            reconn_ref => undefined,
            sync_ref => undefined,
            active_conns => [],
            opening_conns => []
        },
        State1 = put_in_credentials(State0, Options),
        #{active_conns := [{_Id, GunPid, _MRef} | _]} = State2 = init_connect(Members, EtcdName, State1),
        case token_remote(GunPid, State2) of
            {ok, State3} ->
                {_, State4} = handle_continue(next_health_check, State3),
                {_, State5} = handle_continue(reconnect, State4),
                {_, State6} = handle_continue(auto_sync, State5),
                {ok, State6};
            {error, R} -> error(R)
        end
    catch
        error:Reason0 ->
            ?LOG_ERROR("~p failed to connect etcd endpoints: ~p ~p", [EtcdName, Hosts, Reason0]),
            {stop, Reason0}
    end.

handle_call(info, _From, State) ->
    {reply, maps:with([name, members, active_conns, opening_conns], State), State};

handle_call(round_robin_select, _From, #{active_conns := []} = State) ->
    {reply, {error, eetcd_conn_unavailable}, State};
handle_call(round_robin_select, _From, #{active_conns := [{_Host, GunPid, _MRef} = Conn | Rest],
                                         auth_token := Token} = State) ->
    {reply, {ok, GunPid, Token}, State#{active_conns => Rest ++ [Conn]}};

handle_call({set_credentails, undefined}, _From, State) ->
    {reply, ok, State#{credentials => undefined, auth_token => undefined}};
handle_call({set_credentails, {UserName, Password}}, _From, State) ->
    {reply, ok, State#{credentials => #{name => UserName, password => Password}}};

handle_call({pick_member, _MemberId}, _From, #{mode := random} = State) ->
    {reply, {error, random_mode}, State};
handle_call({pick_member, MemberId}, _From, #{mode := connect_all,
                                              members := Members,
                                              auth_token := Token,
                                              active_conns := Actives} = State)
  when is_map_key(MemberId, Members) ->
    case lists:keyfind(MemberId, 1, Actives) of
        {_MemberId, GunPid, _MRef} -> {reply, {ok, GunPid, Token}, State};
        false -> {reply, {error, eetcd_conn_unavailable}, State}
    end;
handle_call({pick_member, _MemberId}, _From, State) ->
    {reply, {error, eetcd_member_not_found}, State};

handle_call(?refresh_token, _From, #{active_conns := [{_Host, GunPid, _MRef} | _]} = State) ->
    case token_remote(GunPid, State) of
        {ok, #{auth_token := NewToken} = NewState} ->
            {reply, {ok, NewToken}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(?check_health, State) ->
    {noreply, do_check_health(State)};
handle_cast({update_member_list, NewMembers}, State) when map_size(NewMembers) =:= 0 ->
    {noreply, State};
handle_cast({update_member_list, NewMembers}, #{name := EtcdName,
                                                active_conns := Actives,
                                                members := OldMembers,
                                                opening_conns := Openings} = State) ->
    %% Close all connections to members that are not in the new list
    Removes = [R || {Id, _GunPid, _MRef} = R <- Actives ++ Openings, not is_map_key(Id, NewMembers)],
    lists:foreach(fun({Id, GunPid, MRef}) when is_reference(MRef) ->
        {Host, Port, _Transport} = maps:get(Id, OldMembers),
        ?LOG_NOTICE("~p removing member (~s) ~s:~p, conn_pid: ~p",
                    [EtcdName, member_id_hex(Id), Host, Port, GunPid]),
        erlang:demonitor(MRef, [flush]),
        ensure_close(GunPid)
    end, Removes),

    Added = maps:without(maps:keys(OldMembers), NewMembers),
    %% elp:ignore W0034
    [
        ?LOG_NOTICE("~p found new member (~s) ~s:~p", [EtcdName, member_id_hex(Id), Host, Port])
        || {Id, {Host, Port, _}} <- maps:to_list(Added)
    ],
    ets:insert(?ETCD_CLIENT_CACHE, #eetcd_client{name = EtcdName, members = NewMembers}),
    {noreply, State#{members => NewMembers}, {continue, reconnect_now}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({timeout, TRef, ?auto_sync},
            #{name := EtcdName, sync_ref := TRef, active_conns := []} = State) ->
    erlang:cancel_timer(TRef),
    ?LOG_WARNING("~p failed to sync member list: no active connections", [EtcdName]),
    {noreply, State#{sync_ref => undefined}, {continue, auto_sync}};
handle_info({timeout, TRef, ?auto_sync},
            #{name := EtcdName, sync_ref := TRef, active_conns := Actives} = State) ->
    erlang:cancel_timer(TRef),
    Self = self(),
    spawn(fun() -> check_member_list(Self, EtcdName, shuffle(Actives)) end),
    {noreply, State#{sync_ref => undefined}, {continue, auto_sync}};
handle_info({timeout, TRef, ?reconnect}, #{reconn_ref := TRef} = State) ->
    erlang:cancel_timer(TRef),
    {noreply, do_reconnect(State#{reconn_ref => undefined}), {continue, reconnect}};
handle_info({timeout, _TRef, ?reconnect}, State) ->
    {noreply, State};

handle_info({timeout, TRef, ?check_health}, #{health_ref := TRef} = State) ->
    erlang:cancel_timer(TRef),
    {noreply, do_check_health(State#{health_ref => undefined}), {continue, next_health_check}};

handle_info({gun_up, GunPid, http2}, State) ->
    handle_gun_up(GunPid, State),
    {noreply, State};
handle_info({await_check_ok, GunPid}, #{opening_conns := Openings,
                                        active_conns := Actives,
                                        members := Members} = State) ->
    case lists:keytake(GunPid, 2, Openings) of
        {value, {Id, GunPid, MRef}, Rest} ->
            #{Id := {Host, Port, _Transport}} = Members,
            ?LOG_NOTICE("Connection established to etcd member (~s) ~s:~p",
                        [member_id_hex(Id), Host, Port]),
            {noreply, State#{active_conns => Actives ++ [{Id, GunPid, MRef}], opening_conns => Rest}};
        false ->
            ?LOG_WARNING("Received unknown connection pid up: ~p", [GunPid]),
            {noreply, State}
    end;
handle_info({await_check_error, GunPid, Reason}, State) ->
    {noreply, handle_gun_down(GunPid, Reason, State)};
handle_info({gun_down, GunPid, http2, Reason, _Streams}, State) ->
    {noreply, handle_gun_down(GunPid, Reason, State)};
handle_info({'DOWN', MRef, process, GunPid, Reason}, State) ->
    erlang:demonitor(MRef, [flush]),
    {noreply, handle_gun_down(GunPid, Reason, State)};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_continue(next_health_check, #{options := Opts, health_ref := undefined} = State) ->
    After = case proplists:get_value(health_check_ms, Opts, 15000) of
                N when is_integer(N), N > 0 -> N;
                _ -> 15000
            end,
    TRef = erlang:start_timer(After, self(), ?check_health),
    {noreply, State#{health_ref => TRef}};
handle_continue(next_health_check, #{} = State) ->
    {noreply, State};

handle_continue(auto_sync, #{auto_sync_interval_ms := Interval,
                             sync_ref := undefined} = State) when Interval > 0 ->
    TRef = erlang:start_timer(Interval, self(), ?auto_sync),
    {noreply, State#{sync_ref => TRef}};
handle_continue(auto_sync, #{} = State) ->
    {noreply, State};
handle_continue(reconnect_now, #{reconn_ref := undefined} = State) ->
    {noreply, do_reconnect(State), {continue, reconnect}};
handle_continue(reconnect_now, #{reconn_ref := TRef} = State) ->
    erlang:cancel_timer(TRef),
    {noreply, do_reconnect(State#{reconn_ref => undefined}), {continue, reconnect}};
handle_continue(reconnect, #{reconn_ref := undefined} = State) ->
    TRef = erlang:start_timer(2000, self(), ?reconnect),
    {noreply, State#{reconn_ref => TRef}};
handle_continue(reconnect, State) ->
    {noreply, State}.

terminate(Reason, #{name := EtcdName, active_conns := Actives, opening_conns := Openings} = _State) ->
    ?LOG_NOTICE("eetcd client ~p terminating with reason: ~p", [EtcdName, Reason]),
    is_normal(Reason) andalso ets:delete(?ETCD_CLIENT_CACHE, EtcdName),
    [ensure_close(GunPid) || {_Host, GunPid, _Token} <- Actives ++ Openings],
    ok.

code_change(_OldVsntateName, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_normal(normal) -> true;
is_normal(shutdown) -> true;
is_normal({shutdown, _}) -> true;
is_normal(_) -> false.

ensure_close(undefined) -> ok;
ensure_close({GunPid, MRef}) when is_pid(GunPid), is_reference(MRef) ->
    erlang:demonitor(MRef, [flush]),
    ensure_close(GunPid);
ensure_close(GunPid) when is_pid(GunPid) ->
    try
        gun:close(GunPid)
    catch
        _:_ ->
            ok
    end;
ensure_close(_) ->
    ok.

-spec init_connect(members(), atom(), state()) -> state() | no_return().
init_connect(Members, EtcdName, #{mode := connect_all} = State) ->
    connect_all(Members, EtcdName, State);
init_connect(Members, EtcdName, #{mode := random} = State) ->
    connect_random(shuffle(maps:to_list(Members)), EtcdName, State);
init_connect(_, _, _) ->
    error(unsupported_mode).

connect_all(Members, EtcdName, #{gun_opts := GunOpts} = State) ->
    case fold_connect(maps:to_list(Members), EtcdName, GunOpts, [], []) of
        {Ok, []} ->
            State#{
                mode => connect_all,
                active_conns => Ok};
        {Ok, Failed} when length(Ok) > length(Failed) ->
            State#{
                mode => connect_all,
                active_conns => Ok};
        {Ok, Failed} ->
            [ensure_close({GunPid, MRef}) || {_Id, GunPid, MRef} <- Ok],
            error({shutdown, Failed})
    end.

connect_random([], _Name, _State) -> error(eetcd_conn_unavailable);
connect_random([{Id, {_Host, _Port, _Transport}} = Member | Rest], EtcdName,
               #{gun_opts := GunOpts} = State) ->
    case connect_and_await_up(EtcdName, Member, GunOpts) of
        {ok, {GunPid, MRef}} -> State#{active_conns => [{Id, GunPid, MRef}]};
        {error, _Reason} -> connect_random(Rest, EtcdName, State)
    end.

fold_connect([], _Name, _GunOpts, Ok, Fail) -> {Ok, Fail};
fold_connect([{Id, {_Host, _Port, _Transport}} = Member | Rest], EtcdName, GunOpts, Ok, Fail) ->
    case connect_and_await_up(EtcdName, Member, GunOpts) of
        {ok, {GunPid, MRef}} ->
            NewOk = [{Id, GunPid, MRef} | Ok],
            fold_connect(Rest, EtcdName, GunOpts, NewOk, Fail);
        {error, Reason} ->
            NewFail = [{Id, Reason} | Fail],
            fold_connect(Rest, EtcdName, GunOpts, Ok, NewFail)
    end.

-spec connect_and_await_up(atom(), {member_id(), {Host :: string(), inet:port_number(), tcp | tls}}, gun:opts()) ->
    {ok, {GunPid :: pid(), MRef :: reference()}} | {error, any()}.
connect_and_await_up(EtcdName, {Id, {Host, Port, Transport}}, GunOpts0) ->
    try
        GunOpts1 = GunOpts0#{retry => 0},
        GunOpts = case Transport of
                      tcp -> GunOpts1;
                      tls -> GunOpts1#{transport => tls}
                  end,
        GunPid =
            case gun:open(Host, Port, GunOpts) of
                {ok, Gun0} -> Gun0;
                {error, GunReason0} -> error({undefined, GunReason0})
            end,
        Retries = maps:get(retry, GunOpts, 0),
        AwaitTime = case {maps:get(connect_timeout, GunOpts), maps:get(retry_timeout, GunOpts)} of
                        {infinity, _} -> infinity;
                        {ConnectTimeout, RetryTimeout} when ConnectTimeout > 0, RetryTimeout > 0 ->
                            (Retries + 1) * ConnectTimeout + Retries * RetryTimeout
                    end,
        case gun:await_up(GunPid, AwaitTime) of
            {ok, http2} -> ok;
            {error, GunReason} -> error({GunPid, GunReason})
        end,
        try
            ok = await_check(EtcdName, GunPid),
            MRef = erlang:monitor(process, GunPid),
            ?LOG_NOTICE("Connection established to etcd member (~s) ~s:~p",
                        [member_id_hex(Id), Host, Port]),
            {ok, {GunPid, MRef}}
        catch
            error:Reason1 -> error({GunPid, Reason1})
        end
    catch
        error:{GunPid0, Reason} ->
            ensure_close(GunPid0),
            ?LOG_WARNING("~p failed to connect etcd member (~s) ~s:~p by ~p",
                         [EtcdName, member_id_hex(Id), Host, Port, Reason]),
            {error, Reason}
    end.

-spec await_check(atom(), pid()) -> ok | no_return().
await_check(EtcdName, GunPid) ->
    case check_health_remote(EtcdName, GunPid) of
        ok -> ok;
        {error, HealthReason} -> error(HealthReason)
    end,
    case check_leader_remote(EtcdName, GunPid) of
        ok -> ok;
        {error, LeaderReason} -> error(LeaderReason)
    end,
    ok.

do_reconnect(#{mode := random,
               members := Members,
               active_conns := [],
               opening_conns := [],
               gun_opts := GunOpts} = State) ->
    NewOpenings = open_members(shuffle(maps:to_list(Members)), GunOpts, [], one),
    State#{opening_conns => NewOpenings};
do_reconnect(#{mode := random} = State) ->
    State;

do_reconnect(#{mode := connect_all,
               members := Members,
               active_conns := Actives,
               opening_conns := Openings,
               gun_opts := GunOpts} = State) ->
    Retries0 = maps:keys(Members) -- [Id || {Id, _GunPid, _MRef} <- Actives],
    Retries = Retries0 -- [Id || {Id, _} <- Openings],
    RetriesMembers = maps:to_list(maps:with(Retries, Members)),
    NewOpenings = open_members(RetriesMembers, GunOpts, [], all),
    State#{opening_conns => Openings ++ NewOpenings}.

open_members([], _GunOpts, Result, _) -> Result;
open_members([{Id, {Host, Port, Transport}} | Rest], GunOpts0, Result, AllorOne) ->
    GunOpts1 = GunOpts0#{retry => 0},
    GunOpts = case Transport of
                  tcp -> GunOpts1;
                  tls -> GunOpts1#{transport => tls}
              end,
    case gun:open(Host, Port, GunOpts) of
        {ok, GunPid} ->
            MRef = erlang:monitor(process, GunPid),
            ?LOG_NOTICE("Opening connection to etcd member ~s:~p", [Host, Port]),
            case AllorOne of
                all -> open_members(Rest, GunOpts, [{Id, GunPid, MRef} | Result], AllorOne);
                one -> [{Id, GunPid, MRef}]
            end;
        {error, Reason} ->
            ?LOG_WARNING("Failed to open connection to etcd ~s:~p, reason: ~p",
                         [Host, Port, Reason]),
            open_members(Rest, GunOpts, Result, AllorOne)
    end.

handle_gun_up(GunPid, #{name := EtcdName, opening_conns := Openings} = _State) ->
    case lists:keytake(GunPid, 2, Openings) of
        {value, {_Id, GunPid, _MRef}, _Rest} ->
            Self = self(),
            spawn(fun() ->
                      try
                          ok = await_check(EtcdName, GunPid),
                          Self ! {await_check_ok, GunPid}
                      catch
                          error:Reason ->
                              Self ! {await_check_error, GunPid, Reason}
                      end
                  end);
        false ->
            ?LOG_WARNING("Received unknown connection pid up: ~p", [GunPid])
    end,
    ok.

handle_gun_down(GunPid, Reason,
                #{members := Members,
                  active_conns := Actives,
                  opening_conns := Openings} = State) ->
    case {lists:keyfind(GunPid, 2, Actives), lists:keyfind(GunPid, 2, Openings)} of
        {{Id, GunPid, MRef}, false} ->
            erlang:demonitor(MRef, [flush]),
            {Host, Port, _Transport} = maps:get(Id, Members),
            ?LOG_WARNING("etcd connection ~p to member ~s:~p down: ~p",
                         [GunPid, Host, Port, Reason]),
            State#{active_conns => lists:keydelete(GunPid, 2, Actives)};
        {false, {Id, GunPid, MRef}} ->
            erlang:demonitor(MRef, [flush]),
            {Host, Port, _Transport} = maps:get(Id, Members),
            ?LOG_WARNING("etcd connecting ~p to member ~s:~p failed: ~p",
                         [GunPid, Host, Port, Reason]),
            State#{opening_conns => lists:keydelete(GunPid, 2, Openings)};
        {false, false} ->
            ?LOG_WARNING("unknown etcd connection ~p down: ~p", [GunPid, Reason]),
            State
    end.

do_check_health(#{active_conns := Actives} = State) ->
    do_check_health(Actives, State).

do_check_health([], #{} = State) -> State;
do_check_health([{Id, GunPid, MRef} | Rest], #{name := EtcdName, active_conns := Actives} = State) ->
    try
        case check_health_remote(EtcdName, GunPid) of
            ok ->
                case check_leader_remote(EtcdName, GunPid) of
                    ok -> do_check_health(Rest, State);
                    {error, Reason2} -> error({leader, Reason2})
                end;
            {error, Reason1} ->
                error({health, Reason1})
        end
    catch
        error:{Type, Reason} ->
            ConnInfo = gun:info(GunPid),
            erlang:demonitor(MRef, [flush]),
            ensure_close(GunPid),
            ?LOG_ERROR("~p check member (~s) ~s:~p (~p) failed: ~p",
                       [EtcdName,
                        member_id_hex(Id),
                        inet:ntoa(maps:get(sock_ip, ConnInfo)),
                        maps:get(sock_port, ConnInfo),
                        Type,
                        Reason]),
            State1 = State#{active_conns => lists:keydelete(Id, 1, Actives)},
            do_check_health(Rest, State1)
    end.

%% UNKNOWN = 0;
%% SERVING = 1;
%% NOT_SERVING = 2;
%% SERVICE_UNKNOWN = 3;  // Used only by the Watch method.
-spec check_health_remote(atom(), pid()) -> ok | {error, any()}.
check_health_remote(EtcdName, GunPid) ->
    case eetcd_health_gen:check({EtcdName, {GunPid, ?HEADERS}}, #{}, ?DEFAULT_REQ_OPTS) of
        {ok, #{status := 'SERVING'}} -> ok;
        {ok, #{status := 1}} -> ok;
        {ok, #{status := 'UNKNOWN'}} -> ok;
        {ok, #{status := 0}} -> ok;
        %% etcd does not support health checks in early versions of v3 API
        {error, {grpc_error, #{'grpc-message' := <<"unknown service grpc.health.v1.Health">>,
                               'grpc-status'  := ?GRPC_STATUS_UNIMPLEMENTED}}} -> ok;
        {ok, #{status := Status}} -> {error, {unhealthy, Status}};
        {error, _Reason} = Err -> Err
    end.

check_leader_remote(EtcdName, GunPid) ->
    case eetcd_maintenance_gen:status({EtcdName, {GunPid, ?HEADERS}}, #{}, ?DEFAULT_REQ_OPTS) of
        {ok, #{leader := Leader}} when Leader > 0 -> ok;
        {ok, #{errors := Errors, leader := 0}} -> {error, {no_leader, Errors}};
        {error, _Reason} = Err -> Err
    end.

-spec token_remote(pid(), state()) -> {ok, state()} | {error, any()}.
token_remote(GunPid, #{name := EtcdName, credentials := #{name := UserName, password := Password}} = State) ->
    Request = #{name => UserName, password => Password},
    Opts = ?DEFAULT_REQ_OPTS,
    case eetcd_auth_gen:authenticate({EtcdName, {GunPid, ?HEADERS}}, Request, Opts) of
        {ok, #{token := Token}} -> {ok, State#{auth_token => Token}};
        {error, _Reason} = Err -> Err
    end;
token_remote(_GunPid, State) -> {ok, State#{auth_token => undefined}}.

-spec with_gun(atom(), Host :: string(), inet:port_number(), gun:opts(), Fun) -> Result when
    Fun ::  fun((atom(), pid()) -> T),
    Result :: T | {error, any()}.
with_gun(EtcdName, Host, Port, GunOpts, Fun) ->
    case gun:open(Host, Port, GunOpts) of
        {ok, GunPid} ->
            try
                case gun:await_up(GunPid, 5000) of
                    {ok, http2} -> Fun(EtcdName, GunPid);
                    {error, _Reason} = E -> E
                end
            after
                ensure_close(GunPid)
            end;
        Other -> Other
    end.

get_member_list(_Name, [], _GunOpts) -> error(no_available_members);
get_member_list(EtcdName, [{Host, Port}| Rest], GunOpts) ->
    case with_gun(EtcdName, Host, Port, GunOpts, fun member_list/2) of
        {ok, Members} -> Members;
        {error, Reason} ->
            ?LOG_WARNING("Failed to get member list from ~s:~p, reason: ~p",
                         [Host, Port, Reason]),
            get_member_list(EtcdName, Rest, GunOpts)
    end.

check_member_list(_SelfPid, EtcdName, []) ->
    ?LOG_WARNING("~p failed to sync member list: no active connections", [EtcdName]);
check_member_list(SelfPid, EtcdName, [{_Id, GunPid, _MRef} | _]) ->
    try member_list(EtcdName, GunPid) of
        {ok, Members} ->
            case update_member_list(SelfPid, Members) of
                ok -> ok;
                {error, Reason} ->
                    ?LOG_WARNING("~p failed to update member list: ~p", [EtcdName, Reason]),
                    ok
            end;
        {error, Reason} ->
            ?LOG_WARNING("~p failed to get member list: ~p", [EtcdName, Reason]),
            ok
    catch
        error:_Reason ->
            ok
    end.

-spec member_list(atom(), pid()) -> {ok, members()} | {error, any()}.
member_list(EtcdName, GunPid) ->
    Conn = {EtcdName, {GunPid, ?HEADERS}},
    Opts = [{reply_timeout, 10000}],
    case eetcd_cluster_gen:member_list(Conn, #{}, Opts) of
        {ok, #{members := Members0}} when is_list(Members0) ->
            {Members, _} = lists:partition(fun(#{isLearner := 1}) -> false;
                                              (#{isLearner := true}) -> false;
                                              (_) -> true
                                           end, Members0),
            {ok, parse_members(Members)};
        {error, _Reason} = Err -> Err
    end.

-spec parse_members([router_pb:'Etcd.Member'()]) -> members() | no_return().
parse_members(Members) ->
    Result = [{Id, parse_client_url(Url)} || #{'ID' := Id, clientURLs := [Url | _]} <- Members],
    maps:from_list(Result).

parse_client_url(Url) when is_binary(Url) ->
    parse_client_url(binary_to_list(Url));
parse_client_url(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := Host, port := Port, scheme := "http"} ->
            {Host, Port, tcp};
        #{host := Host, port := Port, scheme := "https"} ->
            {Host, Port, tls};
        _ ->
            ?LOG_WARNING("Invalid client URL: ~s", [Url]),
            error(invalid_url)
    end.

put_in_credentials(Data, Options) ->
    case {proplists:get_value(name, Options, undefined),
          proplists:get_value(password, Options, undefined)} of
        {undefined, _} -> Data;
        {_, undefined} -> Data;
        {UserName, Password} ->
            Data#{credentials => #{name => UserName, password => Password}}
    end.

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

member_id_hex(Id) when is_integer(Id) ->
    string:lowercase(binary:encode_hex(<<Id:64>>)).
