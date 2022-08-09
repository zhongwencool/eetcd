%% @private
-module(eetcd_conn).
-include("eetcd.hrl").

-behaviour(gen_statem).

%% API
-export([open/1, close/1,
    round_robin_select/1, lookup/1,
    sync/1, check_health/1, flush_token/2]).

-export([init/1, handle_event/4, terminate/3,
    code_change/4, callback_mode/0, format_status/2]).

-define(ready, ready).
-define(reconnect, reconnect).

-define(check_health, check_health_msg).
-define(auto_sync, auto_sync).
-define(flush_token, flush_token_msg).

%%% 200 400 800 1600 3200 6400 12800 25600
-define(MIN_RECONN, 200).
-define(MAX_RECONN, 25600).

%%%===================================================================
%%% API
%%%===================================================================

open(Args) ->
    case gen_statem:start_link(?MODULE, Args, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {shutdown, Err}} -> Err
    end.

close(Pid) ->
    gen_statem:stop(Pid).

round_robin_select(Name) ->
    %% ets:fun2ms(fun(#eetcd_conn{id = {_, N}, _ = '_', token = H, gun = G})when N =:= Name ->  {H, G} end),
    MS = [{#eetcd_conn{id = {'_', '$1'}, gun = '$2', token = '$3', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        [{{'$2', '$3'}}]}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [{Gun, undefined}] -> {ok, Gun, ?HEADERS};
        [{Gun, Token}] -> {ok, Gun, [{<<"authorization">>, Token} | ?HEADERS]};
        [Uniq] -> Uniq;
        Lists ->
            Length = erlang:length(Lists),
            Index = ets:update_counter(?ETCD_CONNS, Name, {1, 1, Length, 1}, {1, Name}),
            case lists:nth(Index, Lists) of
                {Gun, undefined} -> {ok, Gun, ?HEADERS};
                {Gun, Token} -> {ok, Gun, [{<<"authorization">>, Token} | ?HEADERS]}
            end
    end.

lookup(Name) when is_atom(Name) orelse is_reference(Name) ->
    %% ets:fun2ms(fun(#eetcd_conn{id = {_, N}, _ = '_', conn = G})when N =:= Name -> G end),
    MS = [{#eetcd_conn{id = {'_', '$1'}, conn = '$2', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        ['$2']}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [Pid | _] -> {ok, Pid}
    end.

sync(Name) ->
    case lookup(Name) of
        {ok, Pid} -> erlang:send(Pid, ?auto_sync);
        Err -> Err
    end.

check_health(Name) ->
    case lookup(Name) of
        {ok, Pid} -> erlang:send(Pid, ?check_health);
        Err -> Err
    end.

flush_token(Gun, Headers) ->
    Token = proplists:get_value(<<"authorization">>, Headers),
    %% ets:fun2ms(fun(#eetcd_conn{gun = G, _ = '_', conn = C})when G =:= Pid -> C end),
    MS = [{#eetcd_conn{id = '_', gun = '$1', conn = '$2', _ = '_'},
        [{'=:=', '$1', {const, Gun}}],
        ['$2']}],
    [Pid | _] = ets:select(?ETCD_CONNS, MS),
    NewToken = gen_statem:call(Pid, {?flush_token, Gun, Token}),
    lists:keyreplace(<<"authorization">>, 1, Headers, {<<"authorization">>, NewToken}).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init({Name, Hosts, Options, Transport, TransportOpts}) ->
    erlang:process_flag(trap_exit, true),
    GunOpts = #{protocols => [http2],
        connect_timeout => proplists:get_value(connect_timeout, Options, 1000),
        http2_opts => #{keepalive => 45000},
        retry => proplists:get_value(retry, Options, 0),
        retry_timeout => proplists:get_value(retry_timeout, Options, 5000),
        transport => Transport,
        transport_opts => TransportOpts
    },
    AutoSyncInterval = proplists:get_value(auto_sync_interval_ms, Options, 0),
    Data0 = #{
        name => Name,
        auto_sync_interval_ms => AutoSyncInterval,
        gun_opts => GunOpts,
        health_ref => undefined,
        reconn_ref => undefined,
        active_conns => [],
        freeze_conns => []
    },
    Data = put_in_authenticate(Data0, Options),
    case proplists:get_value(mode, Options, connect_all) of
        connect_all ->
            connect_all(Hosts, Name, Data);
        random ->
            AutoSyncInterval > 0 andalso
            ?LOG_WARNING("~s run under random mode, disabled auto_sync member list", [Name]),
            Length = erlang:length(Hosts),
            Data1 = Data#{endpoints => shuffle(Hosts), mode => random},
            Index = rand:uniform(Length),
            connect_one(Index, 2 * Length, Data1, Length)
    end.

callback_mode() -> [handle_event_function].

format_status(_Opt, [_PDict, StateName, Data]) ->
    #{'StateName' => StateName, 'StateData' => Data}.

handle_event({call, From}, {?flush_token, Gun, Token}, _StateName, Data) ->
    {NewToken, NewData} = do_flush_token(Gun, Token, Data),
    {keep_state, NewData, [{reply, From, NewToken}]};
handle_event(info, {'DOWN', _GunRef, process, Gun, _Reason}, _StateName, Data) ->
    handle_conn_down(Data, Gun);
handle_event(info, {gun_down, Gun, http2, _Error, _KilledStreams, _UnprocessedStreams}, _StateName, Data) ->
    handle_conn_down(Data, Gun);
handle_event(EventType, reconnecting, _StateName, Data)
    when EventType =:= internal orelse EventType =:= info ->
    reconnect_conns(Data);
handle_event(info, ?check_health, _StateName, Data) ->
    {keep_state, do_check_health(Data)};
handle_event(info, ?auto_sync, _StateName, Data) ->
    handle_do_sync(Data);
handle_event(internal, ?ready, ?ready, #{name := Name}) ->
    ?LOG_INFO("ETCD(~p, ~p)'s connections are ready.", [Name, self()]),
    keep_state_and_data;
handle_event(EventType, EventContent, StateName, Data) ->
    ?LOG_ERROR("~p: unknown event ~p ~p ~p ~n",
        [{?MODULE, self()}, {EventType, EventContent}, StateName, Data]),
    keep_state_and_data.

terminate(_Reason, _StateName, Data) ->
    #{name := Name, active_conns := Actives} = Data,
    %% ets:fun2ms(fun(#eetcd_conn{id = {_, N}, _ = '_'})->  N =:= Name end),
    MS = [{#eetcd_conn{id = {'_', '$1'}, _ = '_'}, [], [{'=:=', '$1', {const, Name}}]}],
    ets:select_delete(?ETCD_CONNS, MS),
    ets:match_delete(?ETCD_CONNS, {'_', Name}),
    [begin gun:close(Gun) end || {_Host, Gun, _Token} <- Actives],
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_all(Hosts, Name, #{gun_opts := GunOpts} = Data) ->
    Auth = maps:get(authenticate, Data, undefined),
    case fold_connect(Hosts, Name, GunOpts, Auth, [], []) of
        {Ok, []} ->
            {ok, ?ready, Data#{
                mode => connect_all,
                health_ref => next_check_health(),
                sync_ref => next_sync(Data),
                member_list => Hosts,
                active_conns => Ok,
                freeze_conns => []}};
        {Ok, Failed} when length(Ok) > length(Failed) ->
            Freezes = [{Host, ?MIN_RECONN} || {Host, _Reason} <- Failed],
            {ok, ?reconnect, Data#{
                mode => connect_all,
                health_ref => next_check_health(),
                sync_ref => next_sync(Data),
                member_list => Hosts,
                active_conns => Ok,
                freeze_conns => Freezes},
                {next_event, internal, reconnecting}};
        {Ok, Failed} ->
            [begin gun:close(G) end || {_Host, G, _Auth} <- Ok],
            {stop, {shutdown, Failed}}
    end.

connect_one(Index, Retry, Data, Len) ->
    #{endpoints := Hosts, name := Name, gun_opts := GunOpts} = Data,
    case lookup(Name) of
        {ok, _Pid} ->
            {stop, {shutdown, {error, already_started}}};
        {error, eetcd_conn_unavailable} ->
            Auth = maps:get(authenticate, Data, undefined),
            Host = lists:nth(Index, Hosts),
            case connect(Name, Host, GunOpts, Auth) of
                {ok, Gun, AuthToken} ->
                    {ok, ?ready, Data#{
                        index => Index,
                        health_ref => next_check_health(),
                        sync_ref => next_sync(Data),
                        active_conns => [{Host, Gun, AuthToken}]}};
                {error, Reason} when Retry =< 0 ->
                    {stop, {shutdown, Reason}};
                {error, Reason} ->
                    ?LOG_WARNING("~p failed to connect ETCD host: ~p ~p~n",
                        [Name, Host, Reason]),
                    NewIndex = (Index + 1) rem Len + 1,
                    connect_one(NewIndex, Retry - 1, Data, Len)
            end
    end.

fold_connect([], _Name, _GunOpts, _Auth, Ok, Fail) -> {Ok, Fail};
fold_connect([Host | Hosts], Name, GunOpts, Auth, Ok, Fail) ->
    case connect(Name, Host, GunOpts, Auth) of
        {ok, Gun, Token} ->
            NewOk = [{Host, Gun, Token} | Ok],
            fold_connect(Hosts, Name, GunOpts, Auth, NewOk, Fail);
        {error, Reason} ->
            ?LOG_ERROR("Failed to connect ETCD: ~p by ~p", [Host, Reason]),
            NewFail = [{Host, Reason} | Fail],
            fold_connect(Hosts, Name, GunOpts, Auth, Ok, NewFail)
    end.

connect(Name, {IP, Port}, GunOpts, Auth) ->
    {ok, Gun} = gun:open(IP, Port, GunOpts),
    Retries = maps:get(retry, GunOpts),
    ConnectTimeout = maps:get(connect_timeout, GunOpts),
    RetryTimeout = maps:get(retry_timeout, GunOpts),
    AwaitTime = (Retries + 1) * ConnectTimeout + Retries * RetryTimeout,
    case gun:await_up(Gun, AwaitTime) of
        {ok, http2} ->
            case check_health_remote(Gun) of
                ok ->
                    case check_leader_remote(Gun) of
                        ok ->
                            case token_remote(Gun, Auth) of
                                {ok, Token} ->
                                    try_update_conn(IP, Port, Name, Gun, Token);
                                {error, AuthReason} ->
                                    exit_conn("Authenticate", Gun, AuthReason, Name, IP, Port)
                            end;
                        {error, LeaderReason} ->
                            exit_conn("No Leader", Gun, LeaderReason, Name, IP, Port)
                    end;
                {error, HealthReason} ->
                    exit_conn("GRPC Unhealthy", Gun, HealthReason, Name, IP, Port)
            end;
        {error, GunReason} ->
            exit_conn("Gun Down", Gun, GunReason, Name, IP, Port)
    end.

try_update_conn(IP, Port, Name, Gun, Token) ->
    Conn = #eetcd_conn{id = {{IP, Port}, Name},
        gun = Gun, token = Token,
        conn = self()},
    case ets:insert_new(?ETCD_CONNS, Conn) of
        false ->
            gun:close(Gun),
            {error, already_started};
        true ->
            erlang:monitor(process, Gun),
            ?LOG_INFO("~p connect to ~p:~p gun(~p) successed~n",
                [Name, IP, Port, Gun]),
            {ok, Gun, Token}
    end.

exit_conn(Log, Gun, Reason, Name, IP, Port) ->
    gun:close(Gun),
    ?LOG_WARNING("~p failed to connect [~s:~p] by <~s> ~p~n",
        [Name, IP, Port, Log, Reason]),
    {error, Reason}.

handle_conn_down(#{mode := connect_all} = Data, Gun) -> freeze_conn(Data, Gun);
handle_conn_down(#{mode := random} = Data, Gun) -> clean_stale_conn(Data, Gun).

freeze_conn(Data, Gun) ->
    #{active_conns := Actives, freeze_conns := Freezes, name := Name} = Data,
    case lists:keytake(Gun, 2, Actives) of
        {value, {Endpoint, _, _}, NewActives} ->
            ets:delete(?ETCD_CONNS, {Endpoint, Name}),
            gun:close(Gun),
            NewData = Data#{active_conns => NewActives, freeze_conns => [{Endpoint, ?MIN_RECONN} | Freezes]},
            {next_state, ?reconnect, NewData, {next_event, internal, reconnecting}};
        false when Freezes =:= [] ->
            {next_state, ?ready, Data};
        false ->
            {next_state, ?reconnect, Data, {next_event, internal, reconnecting}}
    end.

clean_stale_conn(Data, Gun) ->
    #{active_conns := Actives, name := Name} = Data,
    case lists:keytake(Gun, 2, Actives) of
        {value, {Endpoint, _, _}, NewActives} ->
            ets:delete(?ETCD_CONNS, {Endpoint, Name}),
            gun:close(Gun),
            NewData = Data#{active_conns => NewActives},
            {next_state, ?reconnect, NewData, {next_event, internal, reconnecting}};
        false ->
            {next_state, ?reconnect, Data, {next_event, internal, reconnecting}}
    end.

do_flush_token(Gun, OldToken, Data) ->
    #{
        active_conns := Actives,
        name := Name,
        authenticate := Auth
    } = Data,
    case lists:keytake(Gun, 2, Actives) of
        {value, {Endpoint, Gun, OldToken}, NewActives} ->
            case token_remote(Gun, Auth) of
                {ok, NewToken} ->
                    Conn = #eetcd_conn{conn = self(),
                        gun = Gun, token = NewToken,
                        id = {Endpoint, Name}},
                    ets:insert(?ETCD_CONNS, Conn),
                    NewData = Data#{active_conns => [{Endpoint, Gun, NewToken} | NewActives]},
                    {NewToken, NewData};
                {error, _AuthReason} -> {OldToken, Data}
            end;
        {value, {_Endpoint, _Gun, NewToken}, _NewActives} ->
            {NewToken, Data};
        _ -> {OldToken, Data}
    end.

reconnect_conns(#{mode := connect_all} = Data) ->
    #{
        name := Name,
        freeze_conns := Freezes,
        active_conns := Actives,
        gun_opts := GunOpts,
        reconn_ref := ReConnRef
    } = Data,
    Auth = maps:get(authenticate, Data, undefined),
    is_reference(ReConnRef) andalso erlang:cancel_timer(ReConnRef),
    {NewActives, NewFreezes} =
        lists:foldl(fun({Host, Ms}, {Ok, Failed}) ->
            case connect(Name, Host, GunOpts, Auth) of
                {ok, Gun, Token} ->
                    {[{Host, Gun, Token} | Ok], Failed};
                {error, _Reason} ->
                    {Ok, [{Host, reconnect_time(Ms bsl 1)} | Failed]}
            end end,
            {[], []}, Freezes),
    NewData = Data#{active_conns => NewActives ++ Actives, freeze_conns => NewFreezes},
    case NewFreezes =:= [] of
        true -> {next_state, ?ready, NewData, {next_event, internal, ?ready}};
        false ->
            NextReconnMs = lists:min([T || {_, T} <- NewFreezes]),
            NewRef = erlang:send_after(NextReconnMs, self(), reconnecting),
            {next_state, ?reconnect, NewData#{reconn_ref => NewRef}}
    end;
reconnect_conns(#{mode := random, active_conns := []} = Data) ->
    #{endpoints := Hosts, index := Index, reconn_ref := ReConnRef} = Data,
    is_reference(ReConnRef) andalso erlang:cancel_timer(ReConnRef),
    Len = erlang:length(Hosts),
    case connect_one(Index, Len, Data, Len) of
        {ok, ?ready, NewData} ->
            {next_state, ?ready, NewData, {next_event, internal, ?ready}};
        {stop, {shutdown, _Reason}} ->
            NewRef = erlang:send_after(1000, self(), reconnecting),
            {next_state, ?reconnect, Data#{reconn_ref => NewRef}}
    end;
reconnect_conns(_Data) -> keep_state_and_data.

next_check_health() ->
    Ms = application:get_env(eetcd, health_check_ms, 15000),
    erlang:send_after(Ms, self(), ?check_health).

next_sync(#{auto_sync_interval_ms := Interval}) ->
    case application:get_env(eetcd, auto_sync_interval_ms, Interval) of
        Ms when is_number(Ms), Ms > 0 ->
            erlang:send_after(Ms, self(), ?auto_sync);
        _ -> undefined
    end;
next_sync(_Data) ->
    undefined.

do_check_health(Data = #{mode := Mode, health_ref := HealthRef}) ->
    erlang:cancel_timer(HealthRef),
    NewData = do_check_health(Mode, Data),
    NewHealthRef = next_check_health(),
    NewData#{health_ref => NewHealthRef}.

do_check_health(connect_all, Data) ->
    #{
        name := Name,
        active_conns := Actives,
        freeze_conns := Freezes,
        member_list := Members
    } = Data,
    {NewActives, NewFreezes} =
        lists:foldl(
          fun({Host, Gun, _Token} = Endpoint, {Health, Freeze}) ->
            case check_health_remote(Gun) of
                ok ->
                    case check_leader_remote(Gun) of
                        ok ->
                            {[Endpoint | Health], Freeze};
                        {error, Reason} ->
                            ets:delete(?ETCD_CONNS, {Host, Name}),
                            gun:close(Gun),
                            ?LOG_ERROR("~p check (~p) leader failed by ~p ", [Name, Host, Reason]),
                            {Health, [{Host, ?MIN_RECONN} | Freeze]}
                    end;
                {error, Reason1} ->
                    ets:delete(?ETCD_CONNS, {Host, Name}),
                    gun:close(Gun),
                    ?LOG_ERROR("~p check (~p) health failed by ~p ", [Name, Host, Reason1]),
                    {Health, [{Host, ?MIN_RECONN} | Freeze]}
            end
          end, {[], Freezes}, Actives),
        NewFreezes1 = [{{H1, P1}, RECONN} || {{H1, P1}, RECONN} <- NewFreezes,
                                             {H2, P2} <- Members,
                                             H1 =:= H2, P1 =:= P2],
    Data#{active_conns => NewActives, freeze_conns => NewFreezes1};
do_check_health(random, Data) ->
    #{name := Name, active_conns := ActiveConns} = Data,
    case ActiveConns of
        [{Host, Gun, _Token}] ->
            case check_health_remote(Gun) of
                ok ->
                    case check_leader_remote(Gun) of
                        ok -> Data;
                        {error, Reason} ->
                            ets:delete(?ETCD_CONNS, {Host, Name}),
                            gun:close(Gun),
                            ?LOG_ERROR("~p check (~p) leader failed by ~p ", [Name, Host, Reason]),
                            Data#{active_conns => []}
                    end;
                {error, Reason1} ->
                    ets:delete(?ETCD_CONNS, {Host, Name}),
                    gun:close(Gun),
                    ?LOG_ERROR("~p check (~p) health failed by ~p ", [Name, Host, Reason1]),
                    Data#{active_conns => []}
            end;
        [] -> Data
    end.

handle_do_sync(Data = #{sync_ref := SyncRef}) ->
    is_reference(SyncRef) andalso erlang:cancel_timer(SyncRef),
    #{freeze_conns := Freezes} = NewData = do_sync_memberlist(Data),
    NewSyncRef = next_sync(NewData),
    NewData1 = NewData#{sync_ref => NewSyncRef},
    case Freezes of
        [] ->
            {keep_state, NewData1};
        _ ->
            reconnect_conns(NewData1)
    end.

do_sync_memberlist(#{mode := random} = Data) ->
    Data;
do_sync_memberlist(#{active_conns := [], name := Name} = Data) ->
    ?LOG_WARNING("~s has no active connections to etcd cluster, cannot get member list", [Name]),
    Data;
do_sync_memberlist(#{name := Name,
                     gun_opts := #{transport := Transport},
                     active_conns := ActiveConns,
                     freeze_conns := Freezes} = Data) ->
    case eetcd_cluster:member_list(Name) of
        {ok, #{members := []}} ->
            %% TODO: remove all active connections? That's critical situation.
            Data;
        {ok, #{members := Members}} ->
            %% FIXME: take all clientURLs per member or take one per member?
            %% Here we take all available clientURLs for every not-a-learner member.
            ClientUrls0 = lists:filtermap(
                           fun(#{isLearner := true, 'ID' := MemberId, name := MemberName}) ->
                                   ?LOG_INFO(#{msg => "Member is a learner, skipped for now",
                                               member_id => MemberId,
                                               member_name => MemberName}),
                                   false;
                              (#{clientURLs := [], 'ID' := MemberId, name := MemberName}) ->
                                   ?LOG_INFO(#{msg => "Member not advertise client URLs",
                                               member_id => MemberId,
                                               member_name => MemberName}),
                                   false;
                              (#{clientURLs := Urls}) ->
                                   {true, Urls}
                           end, Members),
            ClientUrls1 = lists:flatten(ClientUrls0),

            ClientUrls2 = lists:filtermap(
                           fun(Url) ->
                               case uri_string:parse(Url) of
                                   #{host := Host, port := Port, scheme := Scheme}
                                     when erlang:bit_size(Host) > 0 ->
                                       case {Scheme, Transport} of
                                           {<<"http">>, tcp} ->
                                               {true, {erlang:binary_to_list(Host), Port}};
                                           {<<"https">>, tls} ->
                                               {true, {erlang:binary_to_list(Host), Port}};
                                           {<<"https">>, ssl} ->
                                               {true, {erlang:binary_to_list(Host), Port}};
                                           _ ->
                                               %% Note: because of the design of eetcd_conn, we need
                                               %% the member lists' URL use the same transport
                                               %% options to the active connections.
                                               ?LOG_WARNING("Not matched schemes from member list ~s",
                                                            [Url]),
                                               false
                                       end;
                                   _I ->
                                       ?LOG_WARNING("Url ~s from member list is not a valid etcd url",
                                                    [Url]),
                                       false
                               end
                           end, ClientUrls1),

            ClientUrls = lists:usort(ClientUrls2),

            A = [HostPort || {HostPort, _GunPid, _Token} <- ActiveConns],
            F = [HostPort || {HostPort, _Reconn} <- Freezes ],
            RemovedEndpoints = (A ++ F) -- ClientUrls,
            NewEndpoints = (ClientUrls -- A) -- F,

            %% Remove removed_endpoints from freeze_conns
            NewFreezes = [ {H, ?MIN_RECONN} || H <- (ClientUrls -- A)],

            case RemovedEndpoints of
                [] -> ok;
                _ ->
                    ?LOG_NOTICE(#{msg => "Got removed endpoints",
                                  removed_endpoints => RemovedEndpoints})
            end,

            case NewEndpoints of
                [] ->
                    Data#{member_list => ClientUrls};
                _ ->
                    ?LOG_NOTICE(#{msg => "Got new endpoints", new_endpoints => NewEndpoints}),
                    Data#{freeze_conns => NewFreezes, member_list => ClientUrls}
            end;
        {error, Reason} ->
            ?LOG_ERROR("~p get member_list failed by ~p ", [Name, Reason]),
            Data
    end.

%% UNKNOWN = 0;
%% SERVING = 1;
%% NOT_SERVING = 2;
%% SERVICE_UNKNOWN = 3;  // Used only by the Watch method.
check_health_remote(Gun) ->
    Path = <<"/grpc.health.v1.Health/Check">>,
    case eetcd_stream:unary(Gun, #{}, 'Etcd.HealthCheckRequest', Path, 'Etcd.HealthCheckResponse', ?HEADERS) of
        {ok, #{status := 'SERVING'}} -> ok;
        {ok, #{status := 'UNKNOWN'}} -> ok;
        %% etcd does not support health checks in early versions of v3 API
        {error, {grpc_error, #{'grpc-message' := <<"unknown service grpc.health.v1.Health">>,
                               'grpc-status'  := ?GRPC_STATUS_UNIMPLEMENTED}}} -> ok;
        {ok, #{status := Status}} -> {error, {unhealthy, Status}};
        {error, _Reason} = Err -> Err
    end.

check_leader_remote(Gun) ->
    Path = <<"/etcdserverpb.Maintenance/Status">>,
    Request = eetcd:with_timeout(#{}, 10000),
    case eetcd_stream:unary(Gun, Request, 'Etcd.StatusRequest', Path, 'Etcd.StatusResponse', ?HEADERS) of
        {ok, #{leader := Leader}} when Leader > 0 -> ok;
        {ok, #{errors := Errors, leader := 0}} -> {error, {no_leader, Errors}};
        {error, _Reason} = Err -> Err
    end.

token_remote(_Gun, undefined) -> {ok, undefined};
token_remote(Gun, #{name := Name, password := Passwd}) ->
    Path = <<"/etcdserverpb.Auth/Authenticate">>,
    Req1 = eetcd:with_timeout(#{}, 10000),
    Req2 = maps:put(name, Name, Req1),
    Req3 =
        case Passwd of
            undefined -> Req2;
            _ -> maps:put(password, Passwd, Req2)
        end,
    case eetcd_stream:unary(Gun, Req3, 'Etcd.AuthenticateRequest', Path, 'Etcd.AuthenticateResponse', ?HEADERS) of
        {ok, #{token := Token}} -> {ok, Token};
        {error, _Reason} = Err -> Err
    end.

reconnect_time(Ms) when Ms > ?MAX_RECONN -> ?MIN_RECONN;
reconnect_time(Ms) -> Ms.

put_in_authenticate(Data, Options) ->
    case proplists:get_value(name, Options, undefined) of
        undefined -> Data;
        UserName ->
            Password = proplists:get_value(password, Options),
            Auth = #{name => UserName, password => Password},
            Data#{authenticate => Auth}
    end.

shuffle(List) ->
    Disorders = [begin {rand:uniform(), K} end||K <-List],
    [begin K end||{_, K} <- lists:keysort(1, Disorders)].
