-module(eetcd_watch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([watch/2, unwatch/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec watch(router_pb:'Etcd.WatchRequest'(), Callback) -> {ok, pid()} when
    Callback :: fun((router_pb:'Etcd.WatchResponse'()) -> term()).
watch(Request, Callback) when is_function(Callback, 1) ->
    supervisor:start_child(?MODULE, [Request, Callback]).

-spec unwatch(pid()) -> ok.
unwatch(Pid) ->
    gen_server:call(Pid, unwatch).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 3},
    ChildSpecs = [
        #{id => eetcd_watch_worker,
            start => {eetcd_watch_worker, start_link, []},
            restart => temporary,
            shutdown => 2000,
            type => worker,
            modules => [eetcd_watch_worker]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

