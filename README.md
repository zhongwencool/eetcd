eetcd
=====

etcd v3 client for erlang.

Quick Start Example
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
    {etcd_host, "127.0.0.1"},
    {etcd_port, 2379},

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
    = eetcd_kv:put(#'Etcd.PutRequest'{key = <<"key">>, value = <<value>>}).

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
    #'Etcd.WatchResponse'{created = true, events = []} = flush(),
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
    #'Etcd.WatchResponse'{canceled = true, events = []} = flush(),
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


Test
-----

```erlang
rebar3 ct
```

Architecture
-----
TODO

Change Log
-----
