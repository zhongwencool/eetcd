-module(eetcd_lease_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, LeaseID) ->
    supervisor:start_child(?MODULE, [self(), Name, LeaseID]).

init([]) ->
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    
    Worker = eetcd_lease,
    Child = #{id => Worker,
        start => {Worker, start_link, []},
        restart => temporary,
        shutdown => 1000,
        type => worker,
        modules => [Worker]},
    
    {ok, {SupFlags, [Child]}}.
