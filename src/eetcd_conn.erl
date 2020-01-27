-module(eetcd_conn).
-include("eetcd.hrl").

-behaviour(gen_statem).

%% API
-export([open/1, pick_by_round_robin/1, close/1, find_by_name/1, check_health/1]).
-export([init/1, format_status/2, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-define(ready, ready).
-define(reconnect, reconnect).
-define(check_health, check_health).

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

pick_by_round_robin(Name) ->
    %% ets:fun2ms(fun(#eetcd_conn{name = {_, N}, _ = '_', http_header = H, gun = G})when N =:= Name ->  {H, G} end),
    MS = [{#eetcd_conn{name = {'_', '$1'}, gun = '$2', http_header = '$3', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        [{{'$2', '$3'}}]}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [{Gun, HttpHeader}] -> {ok, Gun, HttpHeader};
        Lists ->
            Length = erlang:length(Lists),
            Index = ets:update_counter(?ETCD_CONNS, Name, {1, 1, Length, 1}, {1, Name}),
            {Gun, HttpHeader} = lists:nth(Index, Lists),
            {ok, Gun, HttpHeader}
    end.

close(Pid) ->
    gen_statem:stop(Pid).

find_by_name(Name) when is_atom(Name) orelse is_reference(Name) ->
    %% ets:fun2ms(fun(#eetcd_conn{name = {_, N}, _ = '_', conn = G})when N =:= Name -> G end),
    MS = [{#eetcd_conn{name = {'_', '$1'}, conn = '$2', _ = '_'},
        [{'=:=', '$1', {const, Name}}],
        ['$2']}],
    case ets:select(?ETCD_CONNS, MS) of
        [] -> {error, eetcd_conn_unavailable};
        [Pid | _] -> {ok, Pid}
    end.

check_health(Name) ->
    case find_by_name(Name) of
        {ok, Pid} -> erlang:send(Pid, ?check_health);
        Err -> Err
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
%% @private
init({Name, Hosts, Transport, TransportOpts}) ->
    erlang:process_flag(trap_exit, true),
    GunOpts = #{protocols => [http2],
        http2_opts => #{keepalive => 45000},
        retry => 0,
        transport => Transport,
        transport_opts => TransportOpts
    },
    Data = #{
        name => Name,
        gun_opts => GunOpts,
        http_header => [],
        health_ref => undefined,
        reconn_ref => undefined,
        active_conns => [],
        freeze_conns => []
    },
    {Ok, Failed} = multi_connect(Hosts, Name, GunOpts, [], [], []),
    if
        Failed =:= [] ->
            {ok, ?ready, Data#{
                health_ref => next_check_health(),
                active_conns => Ok}};
        length(Ok) > length(Failed) ->
            ?LOG_WARNING("Failed to connect ETCD host: ~p ~n OK: ~p~n", [Failed, Ok]),
            FreezeConns = [begin {Host, ?MIN_RECONN} end || {Host, _R} <- Failed],
            {ok, ?reconnect, Data#{
                health_ref => next_check_health(),
                active_conns => Ok,
                freeze_conns => FreezeConns},
                {next_event, internal, reconnecting}};
        true ->
            [begin gun:close(G) end || {_, G, _} <- Ok],
            {stop, {shutdown, Failed}}
    end.

%% @private
callback_mode() -> [handle_event_function].

%% @private
format_status(_Opt, [_PDict, StateName, Data]) ->
    #{'StateName' => StateName, 'StateData' => Data}.

%% @private
handle_event(info, {'DOWN', _GunRef, process, Gun, _Reason}, _StateName, Data) ->
    freeze_conns(Gun, Data);
handle_event(info, {gun_down, Gun, http2, _Error, _KilledStreams, _UnprocessedStreams}, _StateName, Data) ->
    freeze_conns(Gun, Data);
handle_event(internal, ?ready, ?ready, Data = #{name := Name}) ->
    ?LOG_INFO("~p:~p(~p)'s all connections is ok.~n", [?MODULE, Name, self()]),
    {keep_state, Data};
handle_event(EventType, reconnecting, _StateName, Data)
    when EventType =:= internal orelse EventType =:= info ->
    reconnect_conns(Data);
handle_event(info, ?check_health, _StateName, Data = #{health_ref := HealthRef}) ->
    erlang:cancel_timer(HealthRef),
    NewData = do_check_health(Data),
    NewHealthRef = next_check_health(),
    {keep_state, NewData#{health_ref => NewHealthRef}};
handle_event(EventType, EventContent, StateName, Data) ->
    ?LOG_ERROR("~p: unknow event ~p ~p ~p ~n",
        [{?MODULE, self()}, {EventType, EventContent}, StateName, Data]),
    {keep_state, Data}.

%% @private
terminate(_Reason, _StateName, Data) ->
    #{name := Name, active_conns := Actives} = Data,
    %% ets:fun2ms(fun(#eetcd_conn{name = {_, N}, _ = '_'})->  N =:= Name end),
    MS = [{#eetcd_conn{name = {'_', '$1'}, _ = '_'}, [], [{'=:=', '$1', {const, Name}}]}],
    ets:select_delete(?ETCD_CONNS, MS),
    ets:match_delete(?ETCD_CONNS, {'_', Name}),
    [begin gun:close(Gun) end || {_Host, Gun, _GunRef} <- Actives],
    ok.

%% @private
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
multi_connect([], _Name, _GunOpts, _Headers, Ok, Fail) -> {Ok, Fail};
multi_connect([{IP, Port} = Host | Hosts], Name, GunOpts, Headers, Ok, Fail) ->
    case connect(Name, IP, Port, GunOpts, Headers) of
        {ok, Gun, GunRef} ->
            NewOk = [{Host, Gun, GunRef} | Ok],
            multi_connect(Hosts, Name, GunOpts, Headers, NewOk, Fail);
        {error, Reason} ->
            NewFail = [{Name, Host, Reason} | Fail],
            multi_connect(Hosts, Name, GunOpts, Headers, Ok, NewFail)
    end.

connect(Name, IP, Port, GunOpts, Headers) ->
    {ok, Gun} = gun:open(IP, Port, GunOpts),
    case gun:await_up(Gun, 1000) of
        {ok, http2} ->
            case check_health(Gun, Headers) of
                ok ->
                    case check_leader_status(Gun, Headers) of
                        ok ->
                            try_update_connection(IP, Port, Name, Gun, Headers);
                        {error, LeaderReason} ->
                            exit_connection("No Leader", Gun, LeaderReason, Name, IP, Port)
                    end;
                {error, HealthReason} ->
                    exit_connection("GRPC Unhealthy", Gun, HealthReason, Name, IP, Port)
            end;
        {error, GunReason} ->
            exit_connection("Gun Connection", Gun, GunReason, Name, IP, Port)
    end.

try_update_connection(IP, Port, Name, Gun, Headers) ->
    Conn = #eetcd_conn{name = {{IP, Port}, Name},
        gun = Gun, http_header = Headers,
        conn = self()},
    case ets:insert_new(?ETCD_CONNS, Conn) of
        false ->
            gun:close(Gun),
            {error, already_started};
        true ->
            GunRef = erlang:monitor(process, Gun),
            ?LOG_INFO("~p connect to ~p:~p gun(~p) successed~n",
                [Name, IP, Port, Gun]),
            {ok, Gun, GunRef}
    end.

exit_connection(Log, Gun, Reason, Name, IP, Port) ->
    gun:close(Gun),
    ?LOG_WARNING("~p failed to connect [~s:~p] by <~s> ~p~n",
        [Name, IP, Port, Log, Reason]),
    {error, Reason}.

freeze_conns(Gun, Data) ->
    #{active_conns := Actives, freeze_conns := Freezes,
        name := Name} = Data,
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

reconnect_conns(Data) ->
    #{
        name := Name,
        freeze_conns := Freezes,
        active_conns := Actives,
        gun_opts := GunOpts,
        http_header := HttpHeaders,
        reconn_ref := ReConnRef
    } = Data,
    is_reference(ReConnRef) andalso erlang:cancel_timer(ReConnRef),
    {NewActives, NewFreezes} =
        lists:foldl(fun({{IP, Port} = Host, Ms}, {Ok, Failed}) ->
            case connect(Name, IP, Port, GunOpts, HttpHeaders) of
                {ok, Gun, GunRef} ->
                    {[{Host, Gun, GunRef} | Ok], Failed};
                {error, _Reason} ->
                    {Ok, [{Host, Ms bsl 1} | Failed]}
            end end,
            {[], []}, Freezes),
    NewData = Data#{active_conns => NewActives ++ Actives, freeze_conns => NewFreezes},
    case NewFreezes =:= [] of
        true -> {next_state, ?ready, NewData, {next_event, internal, ?ready}};
        false ->
            NextReconnMs = next_reconnect_ms(NewFreezes),
            NewRef = erlang:send_after(NextReconnMs, self(), reconnecting),
            {next_state, ?reconnect, NewData#{reconn_ref => NewRef}}
    end.

next_check_health() ->
    Ms = application:get_env(eetcd, health_check_ms, 10000),
    erlang:send_after(Ms, self(), ?check_health).

do_check_health(Data) ->
    #{
        name := Name,
        active_conns := Actives,
        freeze_conns := Freezes,
        http_header := Header
    } = Data,
    {NewActives, NewFreezes} =
        lists:foldl(fun({Host, Gun, _GunRef} = Endpoint, {Health, Freeze}) ->
            case check_health(Gun, Header) of
                ok ->
                    case check_leader_status(Gun, Header) of
                        ok ->
                            {[Endpoint | Health], Freeze};
                        {error, Reason} ->
                            ets:delete(?ETCD_CONNS, {Host, Name}),
                            gun:close(Gun),
                            ?LOG_ERROR("~p check (~p) leader_status failed by ~p ", [Name, Host, Reason]),
                            {Health, [{Host, ?MIN_RECONN} | Freeze]}
                    end;
                {error, Reason1} ->
                    ets:delete(?ETCD_CONNS, {Host, Name}),
                    gun:close(Gun),
                    ?LOG_ERROR("~p check (~p) health failed by ~p ", [Name, Host, Reason1]),
                    {Health, [{Host, ?MIN_RECONN} | Freeze]}
            end
                    end, {[], Freezes}, Actives),
    Data#{active_conns => NewActives, freeze_conns => NewFreezes}.

%% UNKNOWN = 0;
%% SERVING = 1;
%% NOT_SERVING = 2;
%% SERVICE_UNKNOWN = 3;  // Used only by the Watch method.
check_health(Gun, Headers) ->
    Path = <<"/grpc.health.v1.Health/Check">>,
    H = Headers ++ ?HEADERS,
    case eetcd_stream:unary(Gun, #{}, 'Etcd.HealthCheckRequest', Path, 'Etcd.HealthCheckResponse', H) of
        {ok, #{status := 'SERVING'}} -> ok;
        {ok, #{status := 'UNKNOWN'}} -> ok;
        {ok, #{status := Status}} -> {error, {unhealthy, Status}};
        {error, _Reason} = Err -> Err
    end.

check_leader_status(Gun, Header) ->
    Path = <<"/etcdserverpb.Maintenance/Status">>,
    H = Header ++ ?HEADERS,
    Request = eetcd:with_timeout(#{}, 10000),
    case eetcd_stream:unary(Gun, Request, 'Etcd.StatusRequest', Path, 'Etcd.StatusResponse', H) of
        {ok, #{leader := Leader}} when Leader > 0 -> ok;
        {ok, #{errors := Errors, leader := 0}} -> {error, {no_leader, Errors}};
        {error, _Reason} = Err -> Err
    end.

next_reconnect_ms(Freezes) ->
    Ms = lists:min([T || {_, T} <- Freezes]),
    case Ms > ?MAX_RECONN of
        true -> ?MIN_RECONN;
        false -> Ms
    end.
