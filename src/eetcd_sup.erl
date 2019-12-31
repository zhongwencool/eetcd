%%%-------------------------------------------------------------------
%% @doc eetcd top level supervisor.
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

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1000, period => 10},
    Http2Work = eetcd_http2_keeper,
    WatcherSup = eetcd_watch_sup,
    LeaserWork = eetcd_lease_server,
    ChildSpecs = [
        #{id => Http2Work,
            start => {Http2Work, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [Http2Work]},
        #{id => WatcherSup,
            start => {WatcherSup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [WatcherSup]},
        #{id => LeaserWork,
            start => {LeaserWork, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [LeaserWork]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
