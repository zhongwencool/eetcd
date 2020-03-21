-module(eetcd_lease_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([lease_base/1, lease_timeout/1, lease_keepalive_once/1, lease_keepalive/1, put_with_lease/1]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [lease_base, lease_timeout, lease_keepalive_once, lease_keepalive, put_with_lease].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

init_per_testcase(_TestCase, Config) ->
    revoke_all_leases(?Name),
    Config.

end_per_testcase(_TestCase, Config) ->
    revoke_all_leases(?Name),
    Config.

end_per_suite(Config) ->
    revoke_all_leases(?Name),
    eetcd:close(?Name),
    application:stop(eetcd),
    Config.

lease_base(_Config) ->
    TTL = 10,
    {ok, #{'ID' := ID, 'TTL' := TTL}} = eetcd_lease:grant(?Name, TTL),
    timer:sleep(200),
    Leases1 = list_leases(?Name),
    ?assert(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases1)),
    {ok, #{}} = eetcd_lease:revoke(?Name, ID),
    Leases2 = list_leases(?Name),
    ?assertNot(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases2)),
    ok.

lease_timeout(_Config) ->
    TTL = 2,
    {ok, #{'ID' := ID, 'TTL' := NewTTL}} = eetcd_lease:grant(?Name, TTL),
    Leases1 = list_leases(?Name),
    ?assert(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases1)),
    timer:sleep(NewTTL * 1000 + 1000),
    Leases2 = list_leases(?Name),
    ?assertEqual([], Leases2),

    {error, {grpc_error, #{'grpc-status' := 5}}} = eetcd_lease:revoke(?Name, ID),
    ok.

lease_keepalive_once(_Config) ->
    TTL = 2,
    {ok, #{'ID' := ID, 'TTL' := NewTTL}} = eetcd_lease:grant(?Name, TTL),
    Leases1 = list_leases(?Name),
    [#{'ID' := ID} | _] = Leases1,
    timer:sleep(900),
    {ok, _Pid} = eetcd_lease:keep_alive_once(?Name, ID),
    timer:sleep(NewTTL * 1000 + 200),
    Leases2 = list_leases(?Name),
    ?assert(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases2)),
    timer:sleep(2100),
    {ok, #{leases := Leases3}} = eetcd_lease:leases(?Name),
    ?assertNot(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases3)),
    {error, {grpc_error, #{'grpc-status' := 5}}} = eetcd_lease:revoke(?Name, ID),
    ok.

lease_keepalive(_Config) ->
    TTL = 3,
    {ok, #{'ID' := ID, 'TTL' := TTL}}
        = eetcd_lease:grant(?Name, TTL),
    {ok, _Pid} = eetcd_lease:keep_alive(?Name, ID),

    Leases1 = list_leases(?Name),
    [#{'ID' := ID} | _] = Leases1,
    timer:sleep(10000),
    Leases2 = list_leases(?Name),
    [#{'ID' := ID} | _ ] = Leases2,

    eetcd_lease:revoke(?Name, ID),
    Leases3 = list_leases(?Name),
    ?assertNot(lists:any(fun(#{'ID' := Val}) -> Val =:= ID end, Leases3)),
    ok.

put_with_lease(_Config) ->
    TTL = 3,
    Key = <<"eetcd_key">>,
    Value = <<"eetcd_value">>,
    {ok, #{'ID' := ID, 'TTL' := TTL}}
        = eetcd_lease:grant(?Name, TTL),
    {ok, _Pid} = eetcd_lease:keep_alive(?Name, ID),

    Req = eetcd_kv:with_lease(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value), ID),
    eetcd_kv:put(Req),

    timer:sleep(TTL * 1000 * 2),

    {ok, #{kvs := [#{key := Key, value := Value, lease := ID}]}}
        = eetcd_kv:get(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),

    Leases = list_leases(?Name),
    [#{'ID' := ID} | _] = Leases,
    eetcd_lease:revoke(?Name, ID),

    {ok, #{kvs := [], more := false, count := 0}}
        = eetcd_kv:get(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),

    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

list_leases(?Name) ->
  list_leases(?Name, 10).

%% Lists leases with retries up to N times
list_leases(?Name, 0) ->
  [];
list_leases(?Name, RetriesLeft) ->
  case eetcd_lease:leases(?Name) of
    {ok, #{leases := []}} ->
      timer:sleep(100),
      list_leases(?Name, RetriesLeft - 1);
    {ok, #{leases := Leases}} when is_list(Leases) ->
      Leases
  end.

revoke_all_leases(?Name) ->
    {ok, #{leases := Leases}} = eetcd_lease:leases(?Name),
    lists:foreach(fun(#{'ID' := ID}) ->
      eetcd_lease:revoke(?Name, ID)
    end, Leases).
