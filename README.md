[![Build Status](https://travis-ci.org/zhongwencool/eetcd.svg?branch=master)](https://travis-ci.org/zhongwencool/eetcd)
[![GitHub tag](https://img.shields.io/github/tag/zhongwencool/eetcd.svg)](https://github.com/zhongwencool/eetcd)
[![Hex.pm Version](https://img.shields.io/hexpm/v/eetcd.svg)](https://hex.pm/packages/eetcd)

eetcd
=====

Erlang client for the [etcd](https://github.com/etcd-io/etcd) API v3.
`eetcd` aims to be a high-quality, production-ready client for the Protocol Buffer-based etcd v3 API.
All core features are supported.
It includes reconnection, transaction, software transactional memory, high-level query builders and lease management, watchers.

See [the full API documentation](https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md) for more.

1. Adding, Fetching and Deleting Keys;
2. Transaction;
3. Lease -- as well as a few convenience features like continuous keep alive;
4. Watch;
5. Maintenance -- User, Role, Authentication, Cluster, Alarms.


#### Retry
Create a new etcd client for a clustered etcd setup.
Client will connect to servers in sequence.
On failure it will try the next server.
When all servers have failed it will callback with error.
If it suspects the cluster is in leader election mode it will retry up to 4 times with exp backoff.

#### Automatic failover
1. If a request fails, client will try to get cluster configuration from all given seed URIs until first valid response.
The original request failed.
2. Watches are a special case, they will stop watching by run callback with argument `{gun_error, WatchId, Reason}` when the leader goes down.
After a failover reestablish your should manual watch again.
3. Due to the tcp connection is broken, All keep alive lease will lose after failover.


Quick Start
-----
#### 1. Setup
```erlang
## rebar.config
{deps, [eetcd]}.

```
Prepare configuration.

```erlang
## sys.config
[{eetcd,
    {etcd_cluster, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]},

    {http2_transport, tcp},  %% tcp | ssl
    %% ssl:connect_option() see all options in ssl_api.hrl such as [{certfile, Certfile}, {keyfile, Keyfile}] or [{cert, Cert}, {key, Key}]
    {http2_transport_opts, []}
 }
]
```
#### 2. Usage
All etcd3 API's are defined in [gRPC services](https://github.com/etcd-io/etcd/blob/master/etcdserver/etcdserverpb/rpc.proto), which categorize remote procedure calls (RPCs) understood by the etcd server.
A full listing of all etcd RPCs are documented in markdown in the [gRPC API listing](https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md).

##### KV - Creates, updates, fetches, and deletes key-value pairs.
```erlang
%% -include_lib("eetcd/include/eetcd.hrl").

%% creates
{ok, #'Etcd.PutResponse'{ header = #'Etcd.ResponseHeader'{}, prev_kv = undefined}}
    = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key">>, value = <<"value">>}).

%% updates
{ok, #'Etcd.PutResponse'{}}
    = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key_exist">>, value = <<"new_value">>, ignore_value = true}).
{error, {grpc_error, 3, <<"etcdserver: value is provided">>}}
    = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key_no_exist">>, value = <<"new_value">>, ignore_value = true}).

%% fetches
{ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 1,
        kvs = [#'mvccpb.KeyValue'{key = "key_exist", value = "value"}]
    }}
    = eetcd_kv:range(#'Etcd.RangeRequest'{key = "key_exist"}).
%% fetches all keys
{ok, #'Etcd.RangeResponse'{
        more = false,
        count = Count,
        kvs = Kvs
    }}
    = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
          sort_target = 'KEY', sort_order = 'ASCEND', keys_only = true}).

%% deletes
{ok, #'Etcd.DeleteRangeResponse'{
        deleted = 1,
        prev_kvs = [#'mvccpb.KeyValue'{key = "key", value = "value"}]
    }} = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = "key", prev_kv = true}).
%% batch deletes
{ok, #'Etcd.DeleteRangeResponse'{deleted = 2, prev_kvs = Kvs}}
    = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = "key", range_end = "\0", prev_kv = true}).

```

##### Txn - Transaction

```erlang
%% implement etcd v2 CompareAndSwap by Txn
    {ok, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv1, mod_revision = ModRevision}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1}),

    {ok,#'Etcd.TxnResponse'{
        succeeded = false,
        responses = []}
    } = eetcd_kv:txn(#'Etcd.TxnRequest'{
        compare = [#'Etcd.Compare'{result = 'EQUAL', target = 'MOD', key = Kv1, target_union = {mod_revision, ModRevision - 1}}],
        success = [#'Etcd.RequestOp'{request = {request_put, #'Etcd.PutRequest'{key = Kv1, value = Vv4, prev_kv = true}}}],
        failure = []
    }),
    {ok,#'Etcd.TxnResponse'{
        succeeded = true,
        responses = [#'Etcd.ResponseOp'{
            response = {response_put, #'Etcd.PutResponse'{prev_kv =  #'mvccpb.KeyValue'{key = Kv1, value = Vv1}}}
        }]}}
        = eetcd_kv:txn(#'Etcd.TxnRequest'{
        compare = [#'Etcd.Compare'{result = 'EQUAL', target = 'MOD', key = Kv1, target_union = {mod_revision, ModRevision}}],
        success = [#'Etcd.RequestOp'{request = {request_put, #'Etcd.PutRequest'{key = Kv1, value = Vv4, prev_kv = true}}}],
        failure = []
    }).
```

##### Watch - Monitors changes to keys.

```erlang
    Pid = self(),
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Callback),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),

    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value1}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value1}}]} = flush(),

    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]} = flush(),

    ok = eetcd:unwatch(WatchPid),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush().

flush() -> flush(1000).

flush(Time) ->
    receive Msg  -> Msg
    after Time -> {error, timeout}
    end.

```

##### Lease - Primitives for consuming client keep-alive messages.
```erlang
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
   eetcd_lease:lease_revoke(#'Etcd.LeaseRevokeRequest'{'ID' = ID}).

```
More detailed examples see [eetcd_kv_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_kv_SUITE.erl)  [eetcd_watch_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_watch_SUITE.erl)  [eetcd_lease_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_lease_SUITE.erl).

##### Authentication  
```erlang
AuthRequest = #'Etcd.AuthenticateRequest'{name = <<"youruser">>, password = <<"yourpassword">>},
{ok, #'Etcd.AuthenticateResponse'{token = Token}} = eetcd_auth:authenticate(AuthRequest),
{ok, PutResponse} = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key">>, value = <<"value">>}, Token]).
%% or 
{ok, PutResponse} = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key">>, value = <<"value">>}, [{<<"authorization">>, Token}]).
```
Test
-----

```erlang
rebar3 ct
```
Gen proto and client file
-----
```erlang
rebar3 etcd gen
``` 

Architecture
-----

<img src="https://user-images.githubusercontent.com/3116225/45798910-356e9000-bcde-11e8-876b-eca70894de94.jpg" width="95%" height = "85%" alt="Home"></img>

* `eetcd_http2_keeper` make sure http2 connection always work.
* `eetcd_lease_server` handle all lease keep alive event, and auto renew lease.
* `eetcd_watch_sup` start a `eetcd_watch_worker` child every `eetcd:watch/2-3`.


