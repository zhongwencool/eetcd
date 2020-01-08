-module(eetcd_http2_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_http2_client_pid/0]).
-export([safe_get_http2_client_pid/1]).
-export([check_leader/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eetcd.hrl").
-record(state, {pid, ref, cluster = [], index = 0, transport = tcp, transport_opts = []}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_http2_client_pid() -> pid() | undefined.
get_http2_client_pid() ->
    erlang:whereis(?ETCD_HTTP2_CLIENT).

-spec safe_get_http2_client_pid(list()) -> {ok, pid()} | {error, retry_over_limit}.
safe_get_http2_client_pid([]) ->
    {error, retry_over_limit};
safe_get_http2_client_pid([H | Tail]) ->
    case erlang:whereis(?ETCD_HTTP2_CLIENT) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            timer:sleep(H),
            safe_get_http2_client_pid(Tail)
    end.

-spec check_leader() -> ok.
check_leader() ->
    gen_server:cast(?MODULE, check_leader).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Transport = application:get_env(eetcd, http2_transport, tcp),
    TransportOpts = application:get_env(eetcd, http2_transport_opts, []),
    Hosts = application:get_env(eetcd, etcd_cluster, ["127.0.0.1:2379"]),
    Cluster =
        [begin
             [IP, Port] = string:tokens(Host, ":"),
             {IP, list_to_integer(Port)}
         end|| Host <- Hosts],
    State = #state{pid = nil,
        cluster = Cluster,
        index = nil,
        ref = nil,
        transport = Transport,
        transport_opts = TransportOpts
    },
    case connect(State) of
        {ok, State1} -> {ok, State1};
        {error, Reason} -> {stop, Reason}
    end.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%handle_cast(check_leader, State) ->
%%    case check_leader(State) of
%%        ignore -> {noreply, State};
%%        {ok, NewState} -> {noreply, NewState}
%%    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State = #state{pid = Pid, ref = Ref}) ->
    error_logger:warning_msg("~p gun(~p) process stop ~p~n", [?MODULE, Pid, Reason]),
    case connect(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, Reason} -> {stop, Reason, State}
    end;
handle_info({'DOWN', _Ref, process, _OldPid, _Reason}, State) ->
    {noreply, State};

handle_info({gun_down, Pid, http2, {error, Reason}, KilledStreams, UnprocessedStreams},
    State = #state{pid = Pid}) ->
    error_logger:warning_msg(
        "~p connection down on ~p: ~p (Killed: ~p, Unprocessed: ~p)",
        [?MODULE, Pid, Reason, KilledStreams, UnprocessedStreams]),
    {noreply, State};
handle_info({gun_down, Pid, http2, normal, _KilledStreams, _UnprocessedStreams},
    State = #state{pid = Pid}) ->
    {noreply, State};
handle_info({gun_down, Pid, http2, closed, KilledStreams, UnprocessedStreams},
    State = #state{pid = Pid}) ->
    error_logger:warning_msg(
        "~p connection down on ~p: ~p (Killed: ~p, Unprocessed: ~p)",
        [?MODULE, Pid, closed, KilledStreams, UnprocessedStreams]),
    {noreply, State};

handle_info({gun_up, Pid, http2}, State = #state{pid = Pid}) ->
    {noreply, State};

handle_info(Info, State) ->
    error_logger:warning_msg("~p Handle info unknown message ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) ->
    connect(State, 0, []).

connect(#state{cluster = Cluster}, RetryN, Errors) when RetryN >= 2 * length(Cluster) ->
    error_logger:warning_msg("~p connect error (~p) ~n", [?MODULE, Errors]),
    {error, Errors};

connect(State = #state{cluster = Cluster}, RetryN, Errors) ->
    CurIndex =
        case State#state.index of
            LastIndex when is_integer(LastIndex) ->
                % select next etcd server on reconnect or LastIndex not available
                (LastIndex + 1) rem length(Cluster) + 1;
            _ ->
                % random select first etcd server
                rand:uniform(length(Cluster))
        end,
    {IP, Port} = lists:nth(CurIndex, Cluster),
    logger:info("(~p) connecting to etcd_server (~p)~n", [?MODULE, {IP, Port}]),
    {ok, Pid} = gun:open(IP, Port, get_default_gun_opts(State)),
    case gun:await_up(Pid, 1000) of
        {ok, http2} ->
            case whereis(?ETCD_HTTP2_CLIENT) of
                % sync close old conn if it exist
                Pid when is_pid(Pid) ->
                    gun:close(Pid);
                _ -> ok
            end,
            true = register(?ETCD_HTTP2_CLIENT, Pid),
            Ref = erlang:monitor(process, Pid),
            logger:info("(~p) connect to etcd_server (~p) successed~n",
                [?MODULE, {IP, Port, CurIndex, Pid, Ref}]),
            {ok, State#state{pid = Pid, index = CurIndex, ref = Ref}};
        {error, Error} ->
            gun:close(Pid),
            NewErrors = [{IP, Port, Error} | Errors],
            connect(State#state{index = CurIndex}, RetryN + 1, NewErrors)
    end.


%%check_leader(State) ->
%%    case eetcd_maintenance:status(#'Etcd.StatusRequest'{}) of
%%        #'Etcd.StatusResponse'{leader = Leader} when Leader > 0 ->
%%            error_logger:warning_msg("Leader(~p) already exist but request timeout~n", [?MODULE, Leader]),
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

get_default_gun_opts(#state{transport = Transport, transport_opts = TransportOpts}) ->
    #{
        protocols => [http2],
        http2_opts => #{keepalive => 45000},
        retry => 0,
        transport => Transport,
        transport_opts => TransportOpts
    }.
