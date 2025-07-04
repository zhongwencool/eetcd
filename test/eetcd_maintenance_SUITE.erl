-module(eetcd_maintenance_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    smoke_test/1
]).

-define(Name, ?MODULE).

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
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    {ok, #{members := Members}} = eetcd_cluster_gen:member_list(?Name, #{}),
    [{members, Members} | Config].

end_per_suite(Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

smoke_test(Config) ->
    [#{'ID' := MemberId} | _] = ?config(members, Config),
    {ok, _} = eetcd_maintenance:defragment(?Name, MemberId),
    {ok, _} = eetcd_maintenance:status(?Name, MemberId),
    {ok, _} = eetcd_maintenance:hash_kv(?Name, MemberId, 0),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
