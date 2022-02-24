%% @private
-module(eetcd_watcher_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0, start_child/2]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(name(), pos_integer()) -> supervisor:startchild_ret().
start_child(Name, RetryIntervalMs) ->
    supervisor:start_child(?MODULE, [Name, RetryIntervalMs]).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    MaxRestarts = 300,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => MaxRestarts,
                 period => MaxSecondsBetweenRestarts},
    Worker = eetcd_watcher,
    Child  = #{id => Worker,
               start => {Worker, start_link, []},
               restart => transient,
               shutdown => 1000,
               type => worker,
               modules => [Worker]},
    {ok, {SupFlags, [Child]}}.
