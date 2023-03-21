-module(eetcd_maintenance_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    smoke_test/1
]).

-define(Endpoint, "127.0.0.1:2379").

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [
        smoke_test
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    Config.

end_per_suite(Config) ->
    application:stop(eetcd),
    Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

smoke_test(_Config) ->
    {ok, _} = eetcd_maintenance:defragment(?Endpoint, [{transport, tcp}]),
    {ok, _} = eetcd_maintenance:status(?Endpoint, [{transport, tcp}]),
    {ok, _} = eetcd_maintenance:hash_kv(?Endpoint, [{transport, tcp}], 0),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
