-module(eetcd_kv_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([put/1, range/1, delete_range/1, txn/1, compact/1]).

-include("router_pb.hrl").


-define(KEY(K), <<"eetcd", (list_to_binary(K))/binary>>).

-define(VALUE(V), <<"eetcd", (list_to_binary(V))/binary>>).

-define(DEL_ALL, <<"\0">>).

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
        eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = <<"eetcd">>, range_end = ?DEL_ALL}),
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
    {ok, #'Etcd.PutResponse'{
        header = #'Etcd.ResponseHeader'{},
        prev_kv = #'mvccpb.KeyValue'{key = Kv1, value = Vv1}}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv2, prev_kv = true}),
    %% ignore_value
    {error, {'grpc-error',<<"3">>, _}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2, ignore_value = true}),
    %% ignore_lease
    {error, {'grpc-error',<<"3">>, _}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1, ignore_lease = true, lease = 100}),
    ok.

range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, _Vv3}|_] = get_kvs(Config),
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
    {ok, #'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        more = false,
        count = 2,
        kvs = Kvs
    }}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, range_end = Kv3}),
    [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}] = lists:usort(Kvs),
    
    %% limit prefix key
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
    
    ok.

delete_range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, _Vv3}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    %% no key
    {ok, #'Etcd.DeleteRangeResponse'{deleted = 0, prev_kvs = []}}
        = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv3}),
    %% one key
    {ok, #'Etcd.DeleteRangeResponse'{deleted = 1, prev_kvs = []}}
        = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    %% prefix key with prev_kvs
    {ok, #'Etcd.DeleteRangeResponse'{deleted = 2, prev_kvs = Kvs}}
        = eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Kv1, range_end = Kv3, prev_kv = true}),
    [#'mvccpb.KeyValue'{key = Kv1, value = Vv1}, #'mvccpb.KeyValue'{key = Kv2, value = Vv2}] = lists:usort(Kvs),
    ok.

txn(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv3, value = Vv3}),
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
    ok.

compact(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4}|_] = get_kvs(Config),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv1, value = Vv1}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv2, value = Vv2}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv3, value = Vv3}),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Kv4, value = Vv4}),
    {ok, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{mod_revision = Revision}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv2}),
    eetcd_kv:compact(#'Etcd.CompactionRequest'{revision = Revision, physical = true}),
    {error, {'grpc-error',<<"11">>, <<"etcdserver: mvcc: required revision has been compacted">>}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Kv1, revision = Revision - 1}),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
get_kvs(Config) ->
    {kvs, Kvs} = lists:keyfind(kvs, 1, Config),
    Kvs.
