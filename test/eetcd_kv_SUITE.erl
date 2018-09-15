-module(eetcd_kv_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([put/1, range/1, delete_range/1, txn/1, compact/1]).

-include("router_pb.hrl").


-define(KEY(K), <<"eetcd_kev", (list_to_binary(K))/binary>>).

-define(VALUE(V), <<"eetcd_value", (list_to_binary(V))/binary>>).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        put, range, delete_range, txn, compact
    ].

groups() ->
    [].

init_per_suite(Config) ->
    Kvs = [
        {?KEY("v1"), ?VALUE("v1")},
        {?KEY("v2"), ?VALUE("v2")},
        {?KEY("v3"), ?VALUE("v3")},
        {?KEY("v4"), ?VALUE("v4")}
    ],
    application:ensure_all_started(eetcd),
    [{kvs, Kvs}|Config].

end_per_suite(_Config) ->
    application:stop(eetcd),
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, #'Etcd.DeleteRangeResponse'{header = #'Etcd.ResponseHeader'{}}} =
        eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = <<"\0">>, range_end = "\0"}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
    
put(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}|_] = get_kvs(Config),
    %% base
    {ok, #'Etcd.PutResponse'{
        header = #'Etcd.ResponseHeader'{},
        prev_kv = undefined}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    %% pre_kv
    %% If prev_kv is set, etcd gets the previous key-value pair before changing it.
    %% The previous key-value pair will be returned in the put response.
    {ok, #'Etcd.PutResponse'{
        header = #'Etcd.ResponseHeader'{},
        prev_kv = #'mvccpb.KeyValue'{key = Kv1, value = Vv1}}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv2, prev_kv = true}),
    %% ignore_value
    %% If ignore_value is set, etcd updates the key using its current value.
    %% Returns an error if the key does not exist.
    {error, {grpc_error, 3, _}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2, ignore_value = true}),
    %% ignore_lease
    %% If ignore_lease is set, etcd updates the key using its current lease.
    %% Returns an error if the key does not exist.
    {error, {grpc_error, 3, _}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1, ignore_lease = true, lease = 100}),
    ok.

range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    %% no key
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 0,
        kvs = []
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv3}),
    %% one key
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 1,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1}),
    %% prefix key
    %% range_end is the upper bound on the requested range [key, range_end).
    %% If range_end is '\0', the range is all keys >= key.
    %% If range_end is key plus one (e.g., "aa"+1 == "ab", "a\xff"+1 == "b"),
    %% then the range request gets all keys prefixed with key.
    %% If both key and range_end are '\0', then the range request returns all keys.
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 2,
        kvs = Kvs
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, range_end = Kv3}),
    [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}] = lists:usort(Kvs),
    
    %% limit prefix key
    %% limit is a limit on the number of keys returned for the request.
    %% When limit is set to 0, it is treated as no limit.
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = true,
        count = 2,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, mod_revision = Mod}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, range_end = Kv3,
        limit = 1, sort_order = 'ASCEND', sort_target = 'MOD'}),
    
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 2,
        kvs = [#'mvccpb.KeyValue'{key = Kv2}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, range_end = Kv3,
        limit = 1, sort_order = 'ASCEND', sort_target = 'MOD', min_mod_revision = Mod+1}),
    
    %% revision is the point-in-time of the key-value store to use for the range.
    %% If revision is less or equal to zero, the range is over the newest key-value store.
    %% If the revision has been compacted, ErrCompacted is returned as a response.
    {ok, #'Etcd.PutResponse'{
        prev_kv = #'mvccpb.KeyValue'{key = Kv1, value = Vv1, mod_revision = Revision}}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv2, prev_kv = true}),
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 1,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, revision = Revision}),
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 1,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv2}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, revision = 0}),
    
    %% sort_order is the order for returned sorted results.
    %% sort_target is the key-value field to use for sorting.
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 2,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv2}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        sort_target = 'KEY', sort_order = 'ASCEND'}),
    
    %% serializable sets the range request to use serializable member-local reads.
    %% Range requests are linearizable by default;
    %% linearizable requests have higher latency and lower throughput than serializable requests
    %% but reflect the current consensus of the cluster.
    %% For better performance, in exchange for possible stale reads, a serializable range request is served locally
    %% without needing to reach consensus with other nodes in the cluster.
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 2,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv2}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        sort_target = 'KEY', sort_order = 'ASCEND', serializable = true}),
    %% keys_only when set returns only the keys and not the values.
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 2,
        kvs = [#'mvccpb.KeyValue'{key = Kv1, value = <<>>}, #'mvccpb.KeyValue'{key = Kv2, value = <<>>}]
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        sort_target = 'KEY', sort_order = 'ASCEND', keys_only = true}),
    
    %% count_only when set returns only the count of the keys in the range.
    {ok, #'Etcd.RangeResponse'{
        more = false,
        count = 2,
        kvs = []
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        count_only = true}),
    
    %% min_mod_revision is the lower bound for returned key mod revisions;
    %% all keys with lesser mod revisions will be filtered away.
    %% max_mod_revision is the upper bound for returned key mod revisions;
    %% all keys with greater mod revisions will be filtered away.
    %% min_create_revision is the lower bound for returned key create revisions;
    %% all keys with lesser create revisions will be filtered away.
    %% max_create_revision is the upper bound for returned key create revisions;
    %% all keys with greater create revisions will be filtered away.
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv3, value = Vv3}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv4, value = Vv4}),
    
    {ok, #'Etcd.RangeResponse'{kvs = [_, K2, K3, _]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        sort_order = 'ASCEND', sort_target = 'CREATE'}),
    
    {ok, #'Etcd.RangeResponse'{kvs = [_, K22, K33, _]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        sort_order = 'ASCEND', sort_target = 'MOD'}),
    
    {ok, #'Etcd.RangeResponse'{count = 4, kvs = [K2, K3]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        min_create_revision = K2#'mvccpb.KeyValue'.create_revision,
        max_create_revision = K3#'mvccpb.KeyValue'.create_revision,
        sort_target = 'MOD', sort_order = 'ASCEND'}),
    {ok, #'Etcd.RangeResponse'{count = 4, kvs = [K22, K33]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = "\0", range_end = "\0",
        min_mod_revision = K22#'mvccpb.KeyValue'.mod_revision,
        max_mod_revision = K33#'mvccpb.KeyValue'.mod_revision,
        sort_target = 'MOD', sort_order = 'ASCEND'}),
    ok.

delete_range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, _Vv3}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    %% no key
    {ok, #'Etcd.DeleteRangeResponse'{deleted = 0, prev_kvs = []}}
        = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv3}),
    
    %% one key
    %% If prev_kv is set, etcd gets the previous key-value pairs before deleting it.
    %% The previous key-value pairs will be returned in the delete response.
    {ok, #'Etcd.DeleteRangeResponse'{
        deleted = 1,
        prev_kvs = [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}]
    }} = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv1, prev_kv = true}),
    
    %% prefix key with prev_kvs
    %% range_end is the key following the last key to delete for the range [key, range_end).
    %% If range_end is not given, the range is defined to contain only the key argument.
    %% If range_end is one bit larger than the given key,
    %% then the range is all the keys with the prefix (the given key).
    %% If range_end is '\0', the range is all keys greater than or equal to the key argument.
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    
    {ok, #'Etcd.DeleteRangeResponse'{deleted = 2, prev_kvs = Kvs}}
        = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv1, range_end = Kv3, prev_kv = true}),
    [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}] = lists:usort(Kvs),
    ok.

txn(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv3, value = Vv3}),
    %% From google paxosdb paper: Our implementation hinges around a powerful primitive which we call MultiOp.
    %% All other database operations except for iteration are implemented as a single call to MultiOp.
    %% A MultiOp is applied atomically and consists of three components: 1. A list of tests called guard.
    %% Each test in guard checks a single entry in the database. It may check for the absence or presence of a value, or compare with a given value.
    %% Two different tests in the guard may apply to the same or different entries in the database.
    %% All tests in the guard are applied and MultiOp returns the results. If all tests are true,
    %% MultiOp executes t op (see item 2 below), otherwise it executes f op (see item 3 below). 2. A list of database operations called t op.
    %% Each operation in the list is either an insert, delete, or lookup operation, and applies to a single database entry.
    %% Two different operations in the list may apply to the same or different entries in the database.
    %% These operations are executed if guard evaluates to true. 3. A list of database operations called f op.
    %% Like t op, but executed if guard evaluates to false.
    
    %% success success is a list of requests which will be applied when compare evaluates to true.
    %% succeeded succeeded is set to true if the compare evaluated to true or false otherwise.
    %% responses is a list of responses corresponding to the results from applying success if succeeded is true or failure if succeeded is false.
    {ok,#'Etcd.TxnResponse'{
        succeeded = true,
        responses = [#'Etcd.ResponseOp'{
            response = {response_put, #'Etcd.PutResponse'{prev_kv = undefined}}
        }]}}
        = eetcd_kv:txn(#'Etcd.TxnRequest'{
        compare = [#'Etcd.Compare'{result = 'NOT_EQUAL', target = 'VALUE', key = Kv1, range_end = Kv3, target_union = {value, "1"}}],
        success = [#'Etcd.RequestOp'{request = {request_put, #'Etcd.PutRequest'{key = Kv4, value = Vv4, prev_kv = true}}}],
        failure = [#'Etcd.RequestOp'{request = {request_range, #'Etcd.RangeRequest'{key = Kv4}}}]
    }),
    
    {ok,#'Etcd.TxnResponse'{
        succeeded = false,
        responses = [#'Etcd.ResponseOp'{
            response = {response_range, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{key = Kv4, value = Vv4}]}}
        }]}}
        = eetcd_kv:txn(#'Etcd.TxnRequest'{
        compare = [#'Etcd.Compare'{result = 'EQUAL', target = 'VALUE', key = Kv1, range_end = Kv3, target_union = {value, "1"}}],
        success = [#'Etcd.RequestOp'{request = {request_put, #'Etcd.PutRequest'{key = Kv4, value = Vv4, prev_kv = true}}}],
        failure = [#'Etcd.RequestOp'{request = {request_range, #'Etcd.RangeRequest'{key = Kv4}}}]
    }),
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
    }),
    ok.

compact(Config) ->
    %% Compact compacts the event history in the etcd key-value store.
    %% The key-value store should be periodically compacted or the event history will continue to grow indefinitely.
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv3, value = Vv3}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv4, value = Vv4}),
    %% revision is the key-value store revision for the compaction operation.
    %% physical is set so the RPC will wait until the compaction is physically applied to the local database such that
    %% compacted entries are totally removed from the backend database.
    {ok, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{mod_revision = Revision}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv2}),
    eetcd_kv:compact(#'Etcd.CompactionRequest'{revision = Revision, physical = true}),
    {error, {grpc_error, 11, <<"etcdserver: mvcc: required revision has been compacted">>}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, revision = Revision - 1}),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_kvs(Config) ->
    {kvs, Kvs} = lists:keyfind(kvs, 1, Config),
    Kvs.
