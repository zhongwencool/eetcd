%% @private
-module(eetcd_lease_sup).

-behaviour(supervisor).
-include("eetcd.hrl").

%% API
-export([start_link/0, start_child/2, close/0, info/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, LeaseID) ->
    supervisor:start_child(?MODULE, [self(), Name, LeaseID]).

%% @doc Close releases all resources Lease keeps for efficient communication with the etcd server.
%% Return the count of all close processes.
%% If you want to revokes the given lease, please use `eetcd_lease_gen:lease_revoke/2'.
close() ->
    lists:foreach(fun({_, Pid, _, _}) ->
                      supervisor:terminate_child(?MODULE, Pid)
                  end,
                  supervisor:which_children(?MODULE)),
    ok.

info() ->
    lists:foldl(fun({_, Pid, _, _}, #{} = Acc) ->
                    Gun = eetcd_lease:gun_pid(Pid),
                    maps:update_with(Gun, fun(C) -> C + 1 end, 1, Acc)
                end, #{}, supervisor:which_children(?MODULE)).

init([]) ->
    MaxRestarts = 300,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    Worker = eetcd_lease,
    Child = #{
        id => Worker,
        start => {Worker, start_link, []},
        restart => temporary,
        shutdown => 1000,
        type => worker,
        modules => [Worker]},
    {ok, {SupFlags, [Child]}}.
