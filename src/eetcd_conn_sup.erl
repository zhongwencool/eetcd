%% @private
-module(eetcd_conn_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0, start_child/3, stop_child/1, info/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, Hosts, Options) ->
    ChildSpec = child_spec(Name, Hosts, Options),
    supervisor:start_child(?MODULE, ChildSpec).

-spec stop_child(atom()) -> ok | {error, not_found}.
stop_child(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            ok = supervisor:delete_child(?MODULE, Name);
        {error, not_found} = E -> E
    end.

info() ->
    Children = supervisor:which_children(?MODULE),
    {Children1, _NotRunnings} = lists:partition(fun({_, Pid, _, _}) -> is_pid(Pid) end, Children),
    [child_info(Pid) || {_, Pid, _, _} <- Children1].

child_info(Pid) ->
    #{name := Name, members := Members, active_conns := Actives, opening_conns := Openings}
        = eetcd_conn:info(Pid),
    Freezes0 = maps:without([Id || {Id, _GunPid, _MRef} <- Actives ++ Openings], Members),
    %% elp:ignore W0034
    Freezes = [{Id, Host, Port, 0} || {Id, {Host, Port, _Transport}} <- maps:to_list(Freezes0)],
    {Name, #{etcd => Pid,
             active_conns => Actives,
             opening_conns => Openings,
             freeze_conns => Freezes,
             members => Members}}.

init([]) ->
    ets:new(?ETCD_CLIENT_CACHE, [named_table, public,
                                 {read_concurrency, true},
                                 {keypos, #eetcd_client.name}]),
    MaxRestarts = 300,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

child_spec(Name, Hosts, Options) ->
    #{
        id => Name,
        start => {eetcd_conn, open, [{Name, Hosts, Options}]},
        restart => transient,
        shutdown => 1000,
        type => worker,
        modules => [eetcd_conn]
     }.
