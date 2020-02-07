%% @private
-module(eetcd_conn).
-include("eetcd.hrl").

-behaviour(gen_statem).

%% API
-export([open/1, close/1,
    round_robin_select/1, select/1,
    check_health/1, update_token/2]).

-export([init/1, handle_event/4, terminate/3,
    code_change/4, callback_mode/0, format_status/2]).

-define(ready, ready).
-define(reconnect, reconnect).

-define(check_health, check_health).
-define(update_token, update_token).

%%% 200 400 800 1600 3200 6400 12800 25600 51200
-define(MIN_RECONN, 200).
-define(MAX_RECONN, 51200).

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
    %% ets:fun2ms(fun(#eetcd_conn{id = {_, N}, _ = '_', http_header = H, gun = G})when N =:= Name ->  {H, G} end),
    MS = [{#eetcd_conn{id = {'_', '$1'}, gun = '$2', http_header = '$3', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        [{{ok, '$2', '$3'}}]}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [Uniq] -> Uniq;
        Lists ->
            Length = erlang:length(Lists),
            Index = ets:update_counter(?ETCD_CONNS, Name, {1, 1, Length, 1}, {1, Name}),
            lists:nth(Index, Lists)
    end.

select(Name) when is_atom(Name) orelse is_reference(Name) ->
    %% ets:fun2ms(fun(#eetcd_conn{id = {_, N}, _ = '_', conn = G})when N =:= Name -> G end),
    MS = [{#eetcd_conn{id = {'_', '$1'}, conn = '$2', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        ['$2']}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [Pid | _] -> {ok, Pid}
    end.

check_health(Name) ->
    case select(Name) of
        {ok, Pid} -> erlang:send(Pid, ?check_health);
        Err -> Err
    end.

update_token(Gun, Headers) ->
    Auth = proplists:get_value(<<"authorization">>, Headers),
    %% ets:fun2ms(fun(#eetcd_conn{gun = G, _ = '_', conn = C})when G =:= Pid -> C end),
    MS = [{#eetcd_conn{id = '_', gun = '$1', conn = '$2', http_header = '_'},
        [{'=:=', '$1', {const, Gun}}],
        ['$2']}],
    [Pid | _] = ets:select(?ETCD_CONNS, MS),
    [NewAuth] = gen_statem:call(Pid, {?update_token, Gun, [{<<"authorization">>, Auth}]}),
    lists:keyreplace(<<"authorization">>, 1, Headers, NewAuth).
%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init({Name, Hosts, Transport, TransportOpts, Options}) ->
    erlang:process_flag(trap_exit, true),
    GunOpts = #{protocols => [http2],
        http2_opts => #{keepalive => 45000},
        retry => 0,
        transport => Transport,
        transport_opts => TransportOpts
    },
    Data0 = #{
        name => Name,
        gun_opts => GunOpts,
        health_ref => undefined,
        reconn_ref => undefined,
        active_conns => []
    },
    Data1 = put_in_authenticate(Data0, Options),
    case proplists:get_value(mode, Options, connect_all) of
        connect_all ->
            Data = Data1#{mode => connect_all, freeze_conns => []},
            connect_all(Hosts, Name, GunOpts, Data);
        random ->
            Length = erlang:length(Hosts),
            Data = Data1#{endpoints => Hosts, mode => random},
            Index = rand:uniform(Length),
            connect_one(Data, Index, Length, 2 * Length)
    end.

callback_mode() -> [handle_event_function].

format_status(_Opt, [_PDict, StateName, Data]) ->
    #{'StateName' => StateName, 'StateData' => Data}.

handle_event({call, From}, {?update_token, Gun, Auth}, _StateName, Data) ->
    {NewAuth, NewData} = do_update_token(Gun, Auth, Data),
    {keep_state, NewData, [{reply, From, NewAuth}]};
handle_event(info, {'DOWN', _GunRef, process, Gun, _Reason}, _StateName, Data) ->
    handle_conn_down(Data, Gun);
handle_event(info, {gun_down, Gun, http2, _Error, _KilledStreams, _UnprocessedStreams}, _StateName, Data) ->
    handle_conn_down(Data, Gun);
handle_event(EventType, reconnecting, _StateName, Data)
    when EventType =:= internal orelse EventType =:= info ->
    reconnect_conns(Data);
handle_event(info, ?check_health, _StateName, Data) ->
    NewData = do_check_health(Data),
    {keep_state, NewData};
handle_event(internal, ?ready, ?ready, #{name := Name}) ->
    ?LOG_INFO("ETCD(~p, ~p)'s connections are ready.", [Name, self()]),
    keep_state_and_data;
handle_event(EventType, EventContent, StateName, Data) ->
    ?LOG_ERROR("~p: unknow event ~p ~p ~p ~n",
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

connect_all(Hosts, Name, GunOpts, Data) ->
    Auth = maps:get(authenticate, Data, undefined),
    case fold_connect(Hosts, Name, GunOpts, Auth, [], []) of
        {Ok, []} ->
            {ok, ?ready, Data#{
                health_ref => next_check_health(),
                active_conns => Ok}};
        {Ok, Failed} when length(Ok) > length(Failed) ->
            ?LOG_WARNING("Failed to connect ETCD host: ~p ~n OK: ~p~n", [Failed, Ok]),
            FreezeConns = [begin {Host, ?MIN_RECONN} end || {Host, _R} <- Failed],
            {ok, ?reconnect, Data#{
                health_ref => next_check_health(),
                active_conns => Ok,
                freeze_conns => FreezeConns},
                {next_event, internal, reconnecting}};
        {Ok, Failed} ->
            [begin gun:close(G) end || {_Host, G, _Auth} <- Ok],
            {stop, {shutdown, Failed}}
    end.

connect_one(Data, Index, Len, Retry) ->
    #{endpoints := Hosts, name := Name, gun_opts := GunOpts} = Data,
    case select(Name) of
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
                        active_conns => [{Host, Gun, AuthToken}]}};
                {error, Reason} when Retry =< 0 ->
                    {stop, {shutdown, Reason}};
                {error, Reason} ->
                    ?LOG_WARNING("~p failed to connect ETCD host: ~p ~p~n",
                        [Name, Host, Reason]),
                    NewIndex = (Index + 1) rem Len + 1,
                    connect_one(Data, NewIndex, Len, Retry - 1)
            end
    end.

fold_connect([], _Name, _GunOpts, _Auth, Ok, Fail) -> {Ok, Fail};
fold_connect([Host | Hosts], Name, GunOpts, Auth, Ok, Fail) ->
    case connect(Name, Host, GunOpts, Auth) of
        {ok, Gun, Token} ->
            NewOk = [{Host, Gun, Token} | Ok],
            fold_connect(Hosts, Name, GunOpts, Auth, NewOk, Fail);
        {error, Reason} ->
            NewFail = [{Host, Reason} | Fail],
            fold_connect(Hosts, Name, GunOpts, Auth, Ok, NewFail)
    end.

connect(Name, {IP, Port}, GunOpts, Auth) ->
    {ok, Gun} = gun:open(IP, Port, GunOpts),
    case gun:await_up(Gun, 1000) of
        {ok, http2} ->
            case check_health_remote(Gun) of
                ok ->
                    case check_leader_remote(Gun) of
                        ok ->
                            case token_remote(Gun, Auth) of
                                {ok, AuthHeader} ->
                                    try_update_conn(IP, Port, Name, Gun, AuthHeader);
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

try_update_conn(IP, Port, Name, Gun, Auth) ->
    Conn = #eetcd_conn{id = {{IP, Port}, Name},
        gun = Gun, http_header = Auth,
        conn = self()},
    case ets:insert_new(?ETCD_CONNS, Conn) of
        false ->
            gun:close(Gun),
            {error, already_started};
        true ->
            erlang:monitor(process, Gun),
            ?LOG_INFO("~p connect to ~p:~p gun(~p) successed~n",
                [Name, IP, Port, Gun]),
            {ok, Gun, Auth}
    end.

exit_conn(Log, Gun, Reason, Name, IP, Port) ->
    gun:close(Gun),
    ?LOG_WARNING("~p failed to connect [~s:~p] by <~s> ~p~n",
        [Name, IP, Port, Log, Reason]),
    {error, Reason}.

handle_conn_down(#{mode := connect_all} = Data, Gun) -> freeze_conn(Data, Gun);
handle_conn_down(#{mode := random} = Data, Gun) -> try_next_conn(Data, Gun).

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

try_next_conn(Data, Gun) ->
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

do_update_token(Gun, Token, Data) ->
    #{
        active_conns := Actives,
        name := Name,
        authenticate := Auth
    } = Data,
    case lists:keytake(Gun, 2, Actives) of
        {value, {Endpoint, Gun, Token}, NewActives} ->
            case token_remote(Gun, Auth) of
                {ok, AuthHeader} ->
                    Conn = #eetcd_conn{id = {Endpoint, Name},
                        gun = Gun, http_header = AuthHeader,
                        conn = self()},
                    ets:insert(?ETCD_CONNS, Conn),
                    NewData = Data#{active_conns => [{Endpoint, Gun, AuthHeader} | NewActives]},
                    {AuthHeader, NewData};
                {error, _AuthReason} -> {Token, Data}
            end;
        {value, {_Endpoint, _Gun, NewToken}, _NewActives} ->
            {NewToken, Data};
        _ -> {Token, Data}
    end.

reconnect_conns(#{mode := connect_all} = Data) ->
    #{
        name := Name,
        freeze_conns := Freezes,
        active_conns := Actives,
        gun_opts := GunOpts,
        authenticate := Auth,
        reconn_ref := ReConnRef
    } = Data,
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
            NextReconnMs = next_reconnect_ms(NewFreezes),
            NewRef = erlang:send_after(NextReconnMs, self(), reconnecting),
            {next_state, ?reconnect, NewData#{reconn_ref => NewRef}}
    end;
reconnect_conns(#{mode := random, active_conns := []} = Data) ->
    #{endpoints := Hosts, index := Index, reconn_ref := ReConnRef} = Data,
    is_reference(ReConnRef) andalso erlang:cancel_timer(ReConnRef),
    Len = erlang:length(Hosts),
    case connect_one(Data, Index, Len, Len) of
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

do_check_health(Data = #{mode := Mode, health_ref := HealthRef}) ->
    erlang:cancel_timer(HealthRef),
    NewData = do_check_health(Mode, Data),
    NewHealthRef = next_check_health(),
    NewData#{health_ref => NewHealthRef}.

do_check_health(connect_all, Data) ->
    #{
        name := Name,
        active_conns := Actives,
        freeze_conns := Freezes
    } = Data,
    {NewActives, NewFreezes} =
        lists:foldl(fun({Host, Gun, _Token} = Endpoint, {Health, Freeze}) ->
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
    Data#{active_conns => NewActives, freeze_conns => NewFreezes};
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

%% UNKNOWN = 0;
%% SERVING = 1;
%% NOT_SERVING = 2;
%% SERVICE_UNKNOWN = 3;  // Used only by the Watch method.
check_health_remote(Gun) ->
    Path = <<"/grpc.health.v1.Health/Check">>,
    case eetcd_stream:unary(Gun, #{}, 'Etcd.HealthCheckRequest', Path, 'Etcd.HealthCheckResponse', ?HEADERS) of
        {ok, #{status := 'SERVING'}} -> ok;
        {ok, #{status := 'UNKNOWN'}} -> ok;
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

token_remote(_Gun, undefined) -> {ok, []};
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
        {ok, #{token := Token}} -> {ok, [{<<"authorization">>, Token}]};
        {error, _Reason} = Err -> Err
    end.

next_reconnect_ms(Freezes) ->
    Ms = lists:min([T || {_, T} <- Freezes]),
    reconnect_time(Ms).

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
