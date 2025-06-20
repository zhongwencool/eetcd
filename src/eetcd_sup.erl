%%%-------------------------------------------------------------------
%% @doc eetcd top level supervisor.
%% @private
%% @end
%%%-------------------------------------------------------------------

-module(eetcd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1000, period => 10},
    Http2Sup = eetcd_conn_sup,
    LeaserSup = eetcd_lease_sup,
    ChildSpecs = [
        #{id => Http2Sup,
            start => {Http2Sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [Http2Sup]},
        #{id => LeaserSup,
            start => {LeaserSup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [LeaserSup]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
