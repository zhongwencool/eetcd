-module(eetcd_lease_SUITE).


-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
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

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

lease_base(_Config) ->
    TTL = 100,
    {ok, #{'ID' := ID, 'TTL' := TTL}} = eetcd_lease:grant(?Name, TTL),
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    {ok, #{}} = eetcd_lease:revoke(?Name, ID),
    {ok, #{leases := Leases}} = eetcd_lease:leases(?Name),
    false = lists:member(#{'ID' => ID}, Leases),
    ok.

lease_timeout(_Config) ->
    TTL = 2,
    {ok, #{'ID' := ID, 'TTL' := NewTTL}} = eetcd_lease:grant(?Name, TTL),
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    timer:sleep(NewTTL * 1000 + 1000),
    {ok, #{leases := []}} = eetcd_lease:leases(?Name),
    
    {error, {grpc_error, #{'grpc-status' := 5}}} = eetcd_lease:revoke(?Name, ID),
    ok.

lease_keepalive_once(_Config) ->
    TTL = 2,
    {ok, #{'ID' := ID, 'TTL' := NewTTL}} = eetcd_lease:grant(?Name, TTL),
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    timer:sleep(1000),
    {ok, _Pid} = eetcd_lease:keep_alive_once(?Name, ID),
    timer:sleep(NewTTL * 1000 + 200),
    {ok, #{leases := Leases}} = eetcd_lease:leases(?Name),
    true = lists:member(#{'ID' => ID}, Leases),
    timer:sleep(1500),
    {ok, #{leases := Leases1}} = eetcd_lease:leases(?Name),
    false = lists:member(#{'ID' => ID}, Leases1),
    {error, {grpc_error, #{'grpc-status' := 5}}} = eetcd_lease:revoke(?Name, ID),
    ok.

lease_keepalive(_Config) ->
    TTL = 3,
    {ok, #{'ID' := ID, 'TTL' := TTL}}
        = eetcd_lease:grant(?Name, TTL),
    {ok, _Pid} = eetcd_lease:keep_alive(?Name, ID),
    
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    timer:sleep(10000),
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    
    {ok, #{}} = eetcd_lease:revoke(?Name, ID),
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
    
    timer:sleep(10000),
    
    {ok, #{kvs := [#{key := Key, value := Value, lease := ID}]}}
        = eetcd_kv:get(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),
    {ok, #{leases := [#{'ID' := ID}]}} = eetcd_lease:leases(?Name),
    
    {ok, #{}} = eetcd_lease:revoke(?Name, ID),
    
    {ok, #{kvs := [], more := false, count := 0}}
        = eetcd_kv:get(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
