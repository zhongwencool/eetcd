-module(eetcd_kv_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    put/1,
    range/1,
    delete_range/1,
    txn/1,
    compact/1,
    prefix/1,
    prefix_range_end/1
]).

-define(KEY(K), <<"eetcd_key_", (list_to_binary(K))/binary>>).

-define(VALUE(V), <<"eetcd_value_", (list_to_binary(V))/binary>>).

-define(NAME(C), proplists:get_value(name, C)).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        put,
        range,
        delete_range,
        txn,
        compact,
        prefix,
        prefix_range_end
    ].

groups() ->
    [].

init_per_suite(_Config) ->
    Kvs = [
        {?KEY("a1"), ?VALUE("a1")},
        {?KEY("a2"), ?VALUE("a2")},
        {?KEY("a3"), ?VALUE("a3")},
        {?KEY("a4"), ?VALUE("a4")},

        {?KEY("v1"), ?VALUE("v1")},
        {?KEY("v2"), ?VALUE("v2")},
        {?KEY("v3"), ?VALUE("v3")},
        {?KEY("v4"), ?VALUE("v4")},

        {?KEY("z1"), ?VALUE("z1")},
        {?KEY("z2"), ?VALUE("z2")},
        {?KEY("z3"), ?VALUE("z3")}
    ],
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(eetcd_kv_conn, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    [{kvs, Kvs}, {name, eetcd_kv_conn}].

end_per_suite(Config) ->
    eetcd:close(?NAME(Config)),
    application:stop(eetcd),
    ok.

init_per_testcase(_TestCase, Config) ->
    Req = #{key => "\0", range_end => "\0"},
    {ok, #{header := #{}}} = eetcd_kv_gen:delete_range(?NAME(Config), Req),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

put(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2} | _] = get_kvs(Config),
    %% base
    {ok, #{header := #{}}} = eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1}),
    %% pre_kv
    %% If prev_kv is set, etcd gets the previous key-value pair before changing it.
    %% The previous key-value pair will be returned in the put response.
    {ok, #{header := #{}, prev_kv := #{key := Kv1, value := Vv1}}}
        = eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1, prev_kv => true}),

    %% ignore_value
    %% If ignore_value is set, etcd updates the key using its current value.
    %% Returns an error if the key does not exist.
    {error, {grpc_error, #{'grpc-status' := 3}}}
        = eetcd_kv_gen:put(?NAME(Config), #{key => Kv2, value => Vv2, ignore_value => true}),

    %% ignore_lease
    %% If ignore_lease is set, etcd updates the key using its current lease.
    %% Returns an error if the key does not exist.
    {error, {grpc_error, #{'grpc-status' := 3}}}
        = eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1,
                                            lease => 100, ignore_lease => true}),
    ok.

range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4} | _] = get_kvs(Config),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv2, value => Vv2}),
    %% no key
    {ok, #{header := #{}, more := false, count := 0, kvs := []}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv3}),
    %% one key
    {ok, #{
        header := #{}, more := false, count := 1, kvs := [#{key := Kv1, value := Vv1}]}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1}),
    %% prefix key
    %% range_end is the upper bound on the requested range [key, range_end).
    %% If range_end is '\0', the range is all keys >= key.
    %% If range_end is key plus one (e.g., "aa"+1 == "ab", "a\xff"+1 == "b"),
    %% then the range request gets all keys prefixed with key.
    %% If both key and range_end are '\0', then the range request returns all keys.
    {ok, #{header := #{}, more := false, count := 2, kvs := Kvs}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1, range_end => Kv3}),
    [#{key := Kv1, value := Vv1}, #{key := Kv2, value := Vv2}] = lists:usort(Kvs),

    %% limit prefix key
    %% limit is a limit on the number of keys returned for the request.
    %% When limit is set to 0, it is treated as no limit.
    % CLimit1 = eetcd_kv:with_range_end(eetcd_kv:with_key(Ctx, Kv1), Kv3),
    CLimit1 = #{key => Kv1, range_end => Kv3},
    CLimit2 = eetcd_kv:with_top(CLimit1, 'MOD', 'ASCEND'),
    {ok, #{header := #{}, more := false, count := 1, kvs := [#{key := Kv1, mod_revision := Mod}]}}
        = eetcd_kv_gen:range(?NAME(Config), CLimit2),

    {ok, #{header := #{}, more := false, count := 1, kvs := [#{key := Kv1}]}}
        = eetcd_kv_gen:range(?NAME(Config), CLimit2#{min_mod_revision => Mod}),

    %% revision is the point-in-time of the key-value store to use for the range.
    %% If revision is less or equal to zero, the range is over the newest key-value store.
    %% If the revision has been compacted, ErrCompacted is returned as a response.
    PrevKv = #{key => Kv1, value => Vv2, prev_kv => true},
    {ok, #{prev_kv := #{key := Kv1, value := Vv1, mod_revision := Revision}}} =
        eetcd_kv_gen:put(?NAME(Config), PrevKv),

    {ok, #{more := false, count := 1, kvs := [#{key := Kv1, value := Vv1}]}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1, revision => Revision}),

    {ok, #{more := false, count := 1, kvs := [#{key := Kv1, value := Vv2}]}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1, revision => 0}),

    %% sort_order is the order for returned sorted results.
    %% sort_target is the key-value field to use for sorting.
    WithSort = eetcd_kv:with_sort(#{key => "\0", range_end => "\0"}, 'KEY', 'ASCEND'),
    {ok, #{more := false, count := 2, kvs := [#{key := Kv1, value := Vv2}, #{key := Kv2, value := Vv2}]}}
        = eetcd_kv_gen:range(?NAME(Config), WithSort),

    %% serializable sets the range request to use serializable member-local reads.
    %% Range requests are linearizable by default;
    %% linearizable requests have higher latency and lower throughput than serializable requests
    %% but reflect the current consensus of the cluster.
    %% For better performance, in exchange for possible stale reads, a serializable range request is served locally
    %% without needing to reach consensus with other nodes in the cluster.
    {ok, #{more := false, count := 2, kvs := [#{key := Kv1, value := Vv2}, #{key := Kv2, value := Vv2}]}}
        = eetcd_kv_gen:range(?NAME(Config), WithSort#{serializable => true}),
    %% keys_only when set returns only the keys and not the values.
    {ok, #{more := false, count := 2, kvs := [#{key := Kv1, value := <<>>}, #{key := Kv2, value := <<>>}]}}
        = eetcd_kv_gen:range(?NAME(Config), WithSort#{keys_only => true}),

    %% count_only when set returns only the count of the keys in the range.
    {ok, #{more := false, count := 2, kvs := []}}
        = eetcd_kv_gen:range(?NAME(Config), WithSort#{count_only => true}),

    %% min_mod_revision is the lower bound for returned key mod revisions;
    %% all keys with lesser mod revisions will be filtered away.
    %% max_mod_revision is the upper bound for returned key mod revisions;
    %% all keys with greater mod revisions will be filtered away.
    %% min_create_revision is the lower bound for returned key create revisions;
    %% all keys with lesser create revisions will be filtered away.
    %% max_create_revision is the upper bound for returned key create revisions;
    %% all keys with greater create revisions will be filtered away.
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv3, value => Vv3}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv4, value => Vv4}),

    All = #{key => "\0", range_end => "\0"},
    WithSortCreate = eetcd_kv:with_sort(All, 'CREATE', 'ASCEND'),
    {ok, #{kvs := [_, #{create_revision := K2}, #{create_revision := K3}, _]}}
        = eetcd_kv_gen:range(?NAME(Config), WithSortCreate),

    WithSortMod = eetcd_kv:with_sort(All, 'MOD', 'ASCEND'),
    {ok, #{kvs := [_, #{mod_revision := K22}, #{mod_revision := K33}, _]}}
        = eetcd_kv_gen:range(?NAME(Config), WithSortMod),

    {ok, #{count := 4, kvs := [#{create_revision := K2}, #{create_revision := K3}]}}
        = eetcd_kv_gen:range(?NAME(Config), All#{max_create_revision => K3,
                                                 min_create_revision => K2}),
    {ok, #{count := 4, kvs := [#{mod_revision := K22}, #{mod_revision := K33}]}}
        = eetcd_kv_gen:range(?NAME(Config), All#{max_mod_revision => K33,
                                                 min_mod_revision => K22}),
    ok.

delete_range(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, _Vv3} | _] = get_kvs(Config),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv2, value => Vv2}),
    %% no key
    {ok, #{deleted := 0, prev_kvs := []}} = eetcd_kv_gen:delete_range(?NAME(Config), #{key => Kv3}),

    %% one key
    %% If prev_kv is set, etcd gets the previous key-value pairs before deleting it.
    %% The previous key-value pairs will be returned in the delete response.
    {ok, #{deleted := 1, prev_kvs := [#{key := Kv1, value := Vv1}]}}
        = eetcd_kv_gen:delete_range(?NAME(Config), #{key => Kv1, prev_kv => true}),

    %% prefix key with prev_kvs
    %% range_end is the key following the last key to delete for the range [key, range_end).
    %% If range_end is not given, the range is defined to contain only the key argument.
    %% If range_end is one bit larger than the given key,
    %% then the range is all the keys with the prefix (the given key).
    %% If range_end is '\0', the range is all keys greater than or equal to the key argument.
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1}),

    {ok, #{deleted := 2, prev_kvs := Kvs}}
        = eetcd_kv_gen:delete_range(?NAME(Config), #{key => Kv1,
                                                     range_end => Kv3,
                                                     prev_kv => true}),
    [Vv1] = [begin V end|| #{key := K, value := V} <- Kvs, K =:= Kv1],
    [Vv2] = [begin V end|| #{key := K, value := V} <- Kvs, K =:= Kv2],
    ok.

txn(Config) ->
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4} | _] = get_kvs(Config),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv1, value => Vv1}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv2, value => Vv2}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv3, value => Vv3}),
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
    Cmp = eetcd_compare:with_range_end(eetcd_compare:new(Kv1), Kv3),
    If = eetcd_compare:value(Cmp, "!=", "1"),
    Then = eetcd_op:put(#{key => Kv4, value => Vv4, prev_kv => true}),
    Else = eetcd_op:get(#{key => Kv4}),
    {ok, #{
        succeeded := true,
        responses := [#{response := {response_put, #{}}
        }]}}
        = eetcd_kv:txn(?NAME(Config), If, Then, Else),

    Cmp1 = eetcd_compare:with_range_end(eetcd_compare:new(Kv1), Kv3),
    If1 = eetcd_compare:value(Cmp1, "=", "1"),
    Then1 = eetcd_op:put(#{key => Kv4, value => Vv4, prev_kv => true}),
    Else1 = eetcd_op:get(#{key => Kv4}),
    {ok, #{
        succeeded := false,
        responses := [#{response := {response_range, #{kvs := [#{key := Kv4, value := Vv4}]}}}]}}
        = eetcd_kv:txn(?NAME(Config), If1, Then1, Else1),
    %% implement etcd v2 CompareAndSwap by Txn
    {ok, #{kvs := [#{key := Kv1, value := Vv1, mod_revision := ModRevision}]}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1}),

    Cmp2 = eetcd_compare:new(Kv1),
    If2 = eetcd_compare:mod_revision(Cmp2, "=", ModRevision - 1),
    Then2 = eetcd_op:put(#{key => Kv1, value => Vv4, prev_kv => true}),

    {ok, #{succeeded := false, responses := []}} = eetcd_kv:txn(?NAME(Config), If2, Then2, []),

    Cmp3 = eetcd_compare:new(Kv1),
    If3 = eetcd_compare:mod_revision(Cmp3, "=", ModRevision),
    Then3 = eetcd_op:put(#{key => Kv1, value => Vv4, prev_kv => true}),
    {ok, #{succeeded := true, responses := [#{response := {response_put, #{prev_kv := #{key := Kv1, value := Vv1}}}}]}}
    = eetcd_kv:txn(?NAME(Config), If3, Then3, []),
    ok.

compact(Config) ->
    %% Compact compacts the event history in the etcd key-value store.
    %% The key-value store should be periodically compacted or the event history will continue to grow indefinitely.
    [{Kv1, Vv1}, {Kv2, Vv2}, {Kv3, Vv3}, {Kv4, Vv4} | _] = get_kvs(Config),
    eetcd_kv_gen:put(?NAME(Config), Req1 = #{key => Kv1, value => Vv1}),
    eetcd_kv_gen:put(?NAME(Config), Req1),
    eetcd_kv_gen:put(?NAME(Config), Req1),
    eetcd_kv_gen:put(?NAME(Config), Req1),
    eetcd_kv_gen:put(?NAME(Config), Req2 = #{key => Kv2, value => Vv2}),
    eetcd_kv_gen:put(?NAME(Config), Req2),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv3, value => Vv3}),
    eetcd_kv_gen:put(?NAME(Config), #{key => Kv4, value => Vv4}),
    %% revision is the key-value store revision for the compaction operation.
    %% physical is set so the RPC will wait until the compaction is physically applied to the local database such that
    %% compacted entries are totally removed from the backend database.
    {ok, #{kvs := [#{mod_revision := Revision}]}} = eetcd_kv_gen:range(?NAME(Config), #{key => Kv2}),
    eetcd_kv_gen:compact(?NAME(Config), #{revision => Revision, physical => true}),
    {error, {grpc_error,
        #{'grpc-status' := 11, 'grpc-message' := <<"etcdserver: mvcc: required revision has been compacted">>}}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Kv1, revision => Revision - 1}),
    ok.

prefix(Config) ->
    KVs = get_kvs(Config),
    %% seed all keys
    lists:foreach(fun({Key, Val}) ->
                    eetcd_kv_gen:put(?NAME(Config), #{key => Key, value => Val})
                  end, KVs),
    %% find keys prefixed with an "a"
    Key1 = ?KEY("a"),
    {ok, #{header := #{}, more := false, count := 4, kvs := Results1}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Key1,
                                              range_end => eetcd:get_prefix_range_end(Key1)}),

    %% we expect results a1 through a4 but not v2 or z3
    ?assert(includes_key(?KEY("a1"), Results1)),
    ?assert(includes_key(?KEY("a2"), Results1)),
    ?assert(includes_key(?KEY("a3"), Results1)),
    ?assert(includes_key(?KEY("a4"), Results1)),
    ?assertNot(includes_key(?KEY("a5"), Results1)),
    ?assertNot(includes_key(?KEY("b1"), Results1)),
    ?assertNot(includes_key(?KEY("r8"), Results1)),
    ?assertNot(includes_key(?KEY("v2"), Results1)),
    ?assertNot(includes_key(?KEY("z3"), Results1)),

    Key2 = ?KEY("z"),
    {ok, #{header := #{}, more := false, count := 3, kvs := Results2}}
        = eetcd_kv_gen:range(?NAME(Config), #{key => Key2,
                                              range_end => eetcd:get_prefix_range_end(Key2)}),

    ?assertNot(includes_key(?KEY("a1"), Results2)),
    ?assertNot(includes_key(?KEY("a2"), Results2)),
    ?assertNot(includes_key(?KEY("a3"), Results2)),
    ?assertNot(includes_key(?KEY("a4"), Results2)),
    ?assertNot(includes_key(?KEY("a5"), Results2)),
    ?assertNot(includes_key(?KEY("b1"), Results2)),
    ?assertNot(includes_key(?KEY("r8"), Results2)),
    ?assertNot(includes_key(?KEY("v2"), Results2)),

    ?assert(includes_key(?KEY("z1"), Results2)),
    ?assert(includes_key(?KEY("z2"), Results2)),
    ?assert(includes_key(?KEY("z3"), Results2)),

    ok.

prefix_range_end(_) ->
    %% {Input, Output}
    Pairs = [
        {"a",    "b"},
        {"b1",   "b2"},
        {"etcd", "etce"},
        {"{",    "|"},
        {"xyz",  "xy{"},
        {"11",    "12"},
        {"19",    "1:"}
    ],
    [begin
         ?assertEqual(Output, eetcd:get_prefix_range_end(Input))
     end || {Input, Output} <- Pairs].

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_kvs(Config) ->
    {kvs, Kvs} = lists:keyfind(kvs, 1, Config),
    Kvs.

includes_key(KeyTarget, KVs) ->
    lists:any(fun ({Key, _Val})   -> Key =:= KeyTarget;
                  (#{key := Key}) -> Key =:= KeyTarget
              end, KVs).
