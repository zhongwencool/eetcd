
-module(eetcd_lease_SUITE).


-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([lease_base/1, lease_timeout/1, lease_keepalive/1, put_with_lease/1]).

-include("router_pb.hrl").

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [ lease_base, lease_timeout, lease_keepalive, put_with_lease ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    Config.

end_per_suite(_Config) ->
    application:stop(eetcd),
    ok.

lease_base(_Config) ->
    TTL = 100,
    {ok, #'Etcd.LeaseGrantResponse'{'ID' =ID, 'TTL' = TTL}}
        = eetcd_lease:lease_grant(#'Etcd.LeaseGrantRequest'{'TTL' = TTL}),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
        #'Etcd.LeaseStatus'{'ID' = ID}
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    {ok, #'Etcd.LeaseRevokeResponse'{}} =
        eetcd_lease:lease_revoke(#'Etcd.LeaseRevokeRequest'{'ID' = ID}),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    ok.

lease_timeout(_Config) ->
    TTL = 2,
    {ok, #'Etcd.LeaseGrantResponse'{'ID' =ID, 'TTL' = NewTTL}}
        = eetcd_lease:lease_grant(#'Etcd.LeaseGrantRequest'{'TTL' = TTL}),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
        #'Etcd.LeaseStatus'{'ID' = ID}
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    timer:sleep(NewTTL*1000 + 1000),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    
    {error, {grpc_error, 5, _}}
        = eetcd_lease:lease_revoke(#'Etcd.LeaseRevokeRequest'{'ID' = ID}),
    ok.

lease_keepalive(_Config) ->
    TTL = 3,
    {ok, #'Etcd.LeaseGrantResponse'{'ID' =ID, 'TTL' = TTL}}
        = eetcd_lease:lease_grant(#'Etcd.LeaseGrantRequest'{'TTL' = TTL}),
    ok = eetcd:lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{'ID' = ID}),
    
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
        #'Etcd.LeaseStatus'{'ID' = ID}
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    timer:sleep(10000),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
        #'Etcd.LeaseStatus'{'ID' = ID}
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    
    {ok, #'Etcd.LeaseRevokeResponse'{}} =
        eetcd_lease:lease_revoke(#'Etcd.LeaseRevokeRequest'{'ID' = ID}),
    ok.

put_with_lease(_Config) ->
    TTL = 3,
    Key = <<"eetcd_key">>,
    Value = <<"eetcd_value">>,
    {ok, #'Etcd.LeaseGrantResponse'{'ID' =ID, 'TTL' = TTL}}
        = eetcd_lease:lease_grant(#'Etcd.LeaseGrantRequest'{'TTL' = TTL}),
    ok = eetcd:lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{'ID' = ID}),
    
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value, lease = ID}),
    
    timer:sleep(10000),
    
    {ok, #'Etcd.RangeResponse'{kvs =
    [#'mvccpb.KeyValue'{key = Key, value = Value, lease = ID}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Key}),
    {ok, #'Etcd.LeaseLeasesResponse'{leases = [
        #'Etcd.LeaseStatus'{'ID' = ID}
    ]}}
        = eetcd_lease:lease_leases(#'Etcd.LeaseLeasesRequest'{}),
    
    {ok, #'Etcd.LeaseRevokeResponse'{}} =
        eetcd_lease:lease_revoke(#'Etcd.LeaseRevokeRequest'{'ID' = ID}),
    
    {ok, #'Etcd.RangeResponse'{kvs = [], more = false, count = 0}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Key}),
    
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
