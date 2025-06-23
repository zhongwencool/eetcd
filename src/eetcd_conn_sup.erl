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
    lists:foldl(
        fun({_, Pid, _, _}, Acc) when is_pid(Pid), is_list(Acc) ->
            #{name := Name, members := Members} = State = sys:get_state(Pid),
            #{active_conns := Actives, opening_conns := Openings} = State,
            Freezes0 = maps:without([Id || {Id, _GunPid, _MRef} <- Actives ++ Openings], Members),
            Freezes = [{Id, Host, Port, 0} || {Id, {Host, Port, _Transport}} <- maps:to_list(Freezes0)],
            [{Name, #{etcd => Pid,
                      active_conns => Actives,
                      opening_conns => Openings,
                      freeze_conns => Freezes,
                      members => Members}} | Acc]
        end, [], supervisor:which_children(?MODULE)).

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
