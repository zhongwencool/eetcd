-module(eetcd_conn_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(?ETCD_CONNS, [named_table, {read_concurrency, true}, public]),
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1200,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    
    Worker = eetcd_conn,
    Child = #{id => Worker,
        start => {Worker, open, []},
        restart => transient,
        shutdown => 1000,
        type => worker,
        modules => [Worker]},
    
    {ok, {SupFlags, [Child]}}.
