-module(eetcd_conn).
-behaviour(gen_server).

-include("eetcd.hrl").

-export([open/4, whereis/1, close/1]).
-export([check_leader/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
open(Name, Cluster, Transport, TransportOpts) ->
    gen_server:start_link(?MODULE, [Name, Cluster, Transport, TransportOpts], []).

whereis(Name) ->
    case ets:lookup(?ETCD_CONNS, Name) of
        [#eetcd_conn{gun = Gun, http_header = HttpHeader}] -> {ok, Gun, HttpHeader};
        _ -> {error, eetcd_conn_unavailable}
    end.

close(Pid) ->
    gen_server:cast(Pid, close).

check_leader(Pid) when is_pid(Pid) -> gen_server:cast(Pid, check_leader);
check_leader(Name) when is_atom(Name) orelse is_reference(Name) ->
    case ?MODULE:whereis(Name) of
        {ok, Pid} -> gen_server:cast(Pid, check_leader);
        Err -> Err
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, Hosts, Transport, TransportOpts]) ->
    erlang:process_flag(trap_exit, true),
    InitState = #{
        name => Name,
        cluster => Hosts,
        transport => Transport,
        transport_opts => TransportOpts,
        reconnect_ms => 0,
        gun => undefined,
        index => undefined,
        gun_ref => undefined,
        http_header => []
    },
    case connect(InitState) of
        {ok, NewState} -> {ok, NewState};
        {error, Reason, _NewState} -> {stop, {shutdown, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(check_leader, State) ->
    case do_check_leader(State) of
        ignore -> {noreply, State};
        {ok, NewState} -> {noreply, NewState}
    end;
handle_cast(close, State) -> {stop, normal, State};
handle_cast(_Request, State) -> {noreply, State}.

handle_info({'DOWN', GunRef, process, Gun, Reason},
    State = #{name := Name, gun := Gun, gun_ref := GunRef, http_header := HttpHeader}) ->
    clean(Name, HttpHeader, Gun),
    ?LOG_WARNING("~p(~p) gun(~p) process 'DOWN' ~p~n", [Name, self(), Gun, Reason]),
    case connect(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, _Reason, NewState} -> {noreply, reconnect_after(NewState)}
    end;
handle_info({'DOWN', _Ref, process, OldGun, Reason},
    State = #{name := Name, gun := NewGun, http_header := HttpHeader}) ->
    clean(Name, HttpHeader, OldGun),
    ?LOG_INFO("~p(~p)'s gun(~p) process 'DOWN' ~p~n", [Name, self(), {OldGun, NewGun}, Reason]),
    {noreply, State};
handle_info({gun_down, Gun, http2, Error, KilledStreams, UnprocessedStreams},
    State = #{gun := Gun, name := Name, http_header := HttpHeader}) ->
    clean(Name, HttpHeader, Gun),
    ?LOG_WARNING("~p(~p) connection gun_down on ~p: ~p (Killed: ~p, Unprocessed: ~p)",
        [Name, self(), Gun, Error, KilledStreams, UnprocessedStreams]),
    case connect(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, _Reason, NewState} -> {noreply, reconnect_after(NewState)}
    end;
handle_info(reconnect, State = #{gun := undefined}) ->
    case connect(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, _Reason, NewState} -> {noreply, reconnect_after(NewState)}
    end;

handle_info(Info, State = #{name := Name}) ->
    ?LOG_WARNING("~p:~p(~p) Handle info unknown message ~p ~p ~n",
        [?MODULE, Name, self(), Info, State]),
    {noreply, State}.

terminate(_Reason, #{name := Name, http_header := HttpHeader, gun := Gun}) ->
    clean(Name, HttpHeader, Gun),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean(Name, HttpHeader, Gun) ->
    Rec = #eetcd_conn{name = Name, gun = Gun, http_header = HttpHeader, conn = self()},
    ets:delete_object(?ETCD_CONNS, Rec),
    gun:close(Gun),
    ok.

reconnect_after(State = #{reconnect_ms := Ms, name := Name}) ->
    erlang:send_after(Ms, self(), reconnect),
    ?LOG_INFO("~p reconnect after ~p~n", [{Name, self()}, Ms]),
    State#{reconnect_ms => erlang:min(Ms + 500, 5000)}.

connect(State) ->
    connect(State, 0, []).

connect(State = #{cluster := Cluster, name := Name}, RetryN, Errors) when RetryN >= 2 * length(Cluster) ->
    Reason = lists:usort(Errors),
    ?LOG_WARNING("~p connect error (~p) ~n", [{Name, self()}, Reason]),
    NewState = State#{gun => undefined, index => undefined, gun_ref => undefined},
    {error, Reason, NewState};
connect(State, RetryN, Errors) ->
    #{name := Name, cluster := Cluster, index := LastIndex} = State,
    CurIndex =
        case LastIndex of
            undefined -> %% random select first etcd server
                rand:uniform(length(Cluster));
            _ -> %% select next etcd server on reconnect or LastIndex not available
                (LastIndex + 1) rem length(Cluster) + 1
        end,
    {IP, Port} = lists:nth(CurIndex, Cluster),
    ?LOG_INFO("(~p) connecting to etcd_server (~p)~n", [{Name, self()}, {IP, Port}]),
    {ok, Pid} = gun:open(IP, Port, gun_opts(State)),
    case gun:await_up(Pid, 1000) of
        {ok, http2} ->
            Conn = #eetcd_conn{name = Name, gun = Pid, http_header = [], conn = self()},
            case ets:insert_new(?ETCD_CONNS, Conn) of
                false ->
                    gun:close(Pid),
                    {error, already_started, State};
                true ->
                    Ref = erlang:monitor(process, Pid),
                    ?LOG_INFO("(~p) connect to etcd_server (~p) successed~n",
                        [{Name, self()}, {IP, Port, CurIndex, Pid, Ref}]),
                    {ok, State#{gun => Pid, index => CurIndex, gun_ref => Ref}}
            end;
        {error, Error} ->
            gun:close(Pid),
            ?LOG_INFO("(~p) connect to etcd_server (~p) failed~n",
                [{Name, self()}, {IP, Port, CurIndex, Pid, Error}]),
            NewErrors = [{IP, Port, Error} | Errors],
            connect(State#{index => CurIndex}, RetryN + 1, NewErrors)
    end.

do_check_leader(_State) ->
    todo.
%%    case eetcd_maintenance:status(#'Etcd.StatusRequest'{}) of
%%        #'Etcd.StatusResponse'{leader = Leader} when Leader > 0 ->
%%            error_?LOG_WARNING_msg("Leader(~p) already exist but request timeout~n", [self(), Leader]),
%%            ignore;
%%        _ -> choose_ready_for_client(State, 1)
%%    end.
%%
%%choose_ready_for_client(#state{cluster = Cluster}, N) when length(Cluster) > N -> ignore;
%%choose_ready_for_client(State, N) ->
%%    #state{cluster = Cluster, index = Index} = State,
%%    case Index =/= N of
%%        true ->
%%            {IP, Port} = lists:nth(N, Cluster),
%%            {ok, Pid} = gun:open(IP, Port, get_default_gun_opts(State)),
%%            case gun:await_up(Pid, 1000) of
%%                {ok, http2} ->
%%                    Request = #'Etcd.StatusRequest'{},
%%                    Path = <<"/etcdserverpb.Maintenance/Status">>,
%%                    %% TODO status pid is wrong
%%                    case eetcd_stream:unary(Request, Path, 'Etcd.StatusResponse') of
%%                        #'Etcd.StatusResponse'{leader = Leader} when Leader > 0 ->
%%                            OldPid = erlang:whereis(?ETCD_HTTP2_CLIENT),
%%                            gun:close(OldPid),
%%                            true = register(?ETCD_HTTP2_CLIENT, Pid),
%%                            {ok, State#state{
%%                                pid = Pid,
%%                                cluster = Cluster,
%%                                index = N,
%%                                ref = erlang:monitor(process, Pid)
%%                            }};
%%                        _ ->
%%                            gun:close(Pid),
%%                            choose_ready_for_client(State, N + 1)
%%                    end;
%%                {error, _Reason} ->
%%                    choose_ready_for_client(State, N + 1)
%%            end;
%%        false ->
%%            choose_ready_for_client(State, N + 1)
%%    end.

gun_opts(#{transport := Transport, transport_opts := TransportOpts}) ->
    #{
        protocols => [http2],
        http2_opts => #{keepalive => 30000},
        retry => 0,
        transport => Transport,
        transport_opts => TransportOpts
    }.
