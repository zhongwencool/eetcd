-module(eetcd_http2_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_http2_client_pid/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("eetcd.hrl").
-record(state, {pid, ref}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec get_http2_client_pid() -> pid() | undefined.
get_http2_client_pid() ->
    erlang:whereis(?ETCD_HTTP2_CLIENT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Host = application:get_env(eetcd, etcd_host, "127.0.0.1"),
    Port = application:get_env(eetcd, etcd_port, 2379),
    case connect(?ETCD_HTTP2_CLIENT, Host, Port) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, #state{pid = Pid, ref = Ref}};
        {error, Reason} -> {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, #state{pid = Pid, ref = Ref}) ->
    error_logger:warning_msg("gun(~p) process stop ~p~n", [Pid, Reason]),
    case reconnect(16, "") of
        {ok, State} -> {noreply, State};
        {error, Reason} -> {stop, Reason, #state{}}
    end;

handle_info({gun_down, Pid, http2, {error, Reason}, KilledStreams, UnprocessedStreams},
    State = #state{pid = Pid}) ->
    error_logger:warning_msg(
        "connection down on ~p: ~p (Killed: ~p, Unprocessed: ~p)",
        [Pid, Reason, KilledStreams, UnprocessedStreams]),
    {noreply, State};
handle_info({gun_down, Pid, http2, normal, _KilledStreams, _UnprocessedStreams},
    State = #state{pid = Pid}) ->
    {noreply, State};

handle_info({gun_up, Pid, http2}, State = #state{pid = Pid}) ->
    {noreply, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Handle info unknown message ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(Name, Host, Port) ->
    case gun:open(Host, Port,
        #{
            protocols => [http2],
            http2_opts => #{keepalive => 45000},
            retry => 10,
            retry_timeout => 2500,
            transport => application:get_env(eetcd, http2_transport, tcp),
            transport_opts => application:get_env(eetcd, http2_transport_opts, [])
        }) of
        {ok, Pid} ->
            case gun:await_up(Pid, 1000) of
                {ok, http2} ->
                    case register_name(Name, Pid) of
                        true -> {ok, Pid};
                        {false, NewPid} -> {error, {already_started, NewPid}}
                    end;
                %The only apparent timeout for gun:open is the connection timeout of the
                %underlying transport. So, a timeout message here comes from gun:await_up.
                {error, timeout} ->
                    {error, gun_open_timeout};
                %gun currently terminates with reason normal if gun:open fails to open
                %the requested connection. This bubbles up through gun:await_up.
                {error, normal} ->
                    {error, gun_open_failed}
            end
    end.

reconnect(0, Reason) -> {error, Reason};
reconnect(N, _OldReason) ->
    wait_http2_client_app_up(),
    case init([]) of
        {ok, State} -> {ok, State};
        {error, Reason} -> reconnect(N - 1, Reason)
    end.

register_name(Name, Pid) when is_atom(Name) ->
    try register(Name, Pid) of
        true -> true
    catch
        error:_ ->
            {false, whereis(Name)}
    end.

wait_http2_client_app_up() ->
    case whereis(gun_sup) of
        undefined ->
            timer:sleep(200),
            wait_http2_client_app_up();
        _ -> ok
    end.