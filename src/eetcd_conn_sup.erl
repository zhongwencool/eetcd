%% @private
-module(eetcd_conn_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0, start_child/1, info/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

info() ->
    lists:foldl(fun({_, Pid, _, _}, Acc) ->
        {_StateName,
            #{
                name := Name,
                active_conns := Actives
            } = Data} = sys:get_state(Pid),
        Freezes = maps:get(freeze_conns, Data, []),
        [{Name, #{etcd => Pid, active_conns => Actives, freeze_conns => Freezes}} | Acc]
                end, [], supervisor:which_children(?MODULE)).

init([]) ->
    ets:new(?ETCD_CONNS, [named_table, {read_concurrency, true}, public, {keypos, #eetcd_conn.id}]),
    MaxRestarts = 300,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    Worker = eetcd_conn,
    Child = #{
        id => Worker,
        start => {Worker, open, []},
        restart => transient,
        shutdown => 1000,
        type => worker,
        modules => [Worker]},
    {ok, {SupFlags, [Child]}}.
