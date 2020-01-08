-module(eetcd).

%% API
-compile(export_all).
-export([with_key/2]).

test() ->
    logger:set_primary_config(level, info),
    {ok, _Pid} = eetcd_conn_sup:open(test, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"], tcp, []),
    R = eetcd_kv:put("test", "test_value"),
    io:format("~p~n", [R]),
    R1 = eetcd_kv:get("test"),
    io:format("~p~n", [R1]),
    eetcd_conn_sup:close(test),
    ok.

new() -> #{}.

with_timeout(Request, Timeout) when is_integer(Timeout) ->
    maps:put(timeout, Timeout, Request).

%%% @doc WithKeyBytes sets the byte slice for the Op's key.
with_key(Request, Key) ->
    maps:put(key, Key, Request).

%% WithValueBytes sets the byte slice for the Op's value.
with_value(Request, Key) ->
    maps:put(value, Key, Request).

%% WithPrefix enables 'Get', 'Delete', or 'Watch' requests to operate
%% on the keys with matching prefix. For example, 'Get(foo, WithPrefix())'
%% can return 'foo1', 'foo2', and so on.
with_prefix(Request) ->
    with_range_end(Request, "\0").

%%  WithFromKey specifies the range of 'Get', 'Delete', 'Watch' requests
%% to be equal or greater than the key in the argument.
with_from_key(Request) ->
    with_range_end(Request, "\x00").

%% Range

%% WithRangeBytes sets the byte slice for the Op's range end.
with_range_end(Request, End) ->
    maps:put(range_end, End, Request).

%% WithLimit limits the number of results to return from 'Get' request.
%% If WithLimit is given a 0 limit, it is treated as no limit.
with_limit(Request, End) ->
    maps:put(limit, End, Request).

%% WithRev specifies the store revision for 'Get' request.
%% Or the start revision of 'Watch' request.
with_revision(Request, Rev) ->
    maps:put(revision, Rev, Request).

%% WithSort specifies the ordering in 'Get' request. It requires
%% 'WithRange' and/or 'WithPrefix' to be specified too.
%% 'target' specifies the target to sort by: 'KEY', 'VERSION', 'VALUE', 'CREATE', 'MOD'.
%% 'order' can be either 'NONE', 'ASCEND', 'DESCEND'.
with_sort(Request, Target, Order) ->
    Targets = router_pb:find_enum_def('Etcd.RangeRequest.SortTarget'),
    Orders = router_pb:find_enum_def('Etcd.RangeRequest.SortOrder'),
    (not lists:keymember(Target, 1, Targets)) andalso throw({sort_target, Target}),
    (not lists:keymember(Order, 1, Orders)) andalso throw({sort_order, Order}),
    R1 = maps:put(sort_order, Order, Request),
    maps:put(sort_target, Target, R1).

%% WithSerializable makes 'Get' request serializable. By default,
%% it's linearizable. Serializable requests are better for lower latency
%% requirement.
with_serializable(Request) ->
    maps:put(serializable, true, Request).

%% WithKeysOnly makes the 'Get' request return only the keys and the corresponding
%% values will be omitted.
with_keys_only(Request) ->
    maps:put(keys_only, true, Request).

%% WithCountOnly makes the 'Get' request return only the count of keys.
with_count_only(Request) ->
    maps:put(count_only, true, Request).

%% WithMinModRev filters out keys for Get with modification revisions less than the given revision.
with_min_mod_revision(Request, Rev) ->
    maps:put(min_mod_revision, Rev, Request).

%% WithMaxModRev filters out keys for Get with modification revisions greater than the given revision.
with_max_mod_revision(Request, Rev) ->
    maps:put(max_mod_revision, Rev, Request).

with_min_create_revision(Request, Rev) ->
    maps:put(min_create_revision, Rev, Request).

with_max_create_revision(Request, Rev) ->
    maps:put(max_create_revision, Rev, Request).

%% WithFirstCreate gets the key with the oldest creation revision in the request range.
with_first_create(Request) ->
    with_top(Request, 'CREATE', 'ASCEND').

%% WithLastKey gets the lexically last key in the request range.
with_last_create(Request) ->
    with_top(Request, 'CREATE', 'DESCEND').

%% WithFirstRev gets the key with the oldest modification revision in the request range.
with_first_revision(Request) ->
    with_top(Request, 'MOD', 'ASCEND').

%%  WithLastRev gets the key with the latest modification revision in the request range.
with_last_revision(Request) ->
    with_top(Request, 'MOD', 'DESCEND').

%% WithFirstKey gets the lexically first key in the request range.
with_first_key(Request) ->
    with_top(Request, 'KEY', 'ASCEND').

%% WithLastKey gets the lexically last key in the request range.
with_last_key(Request) ->
    with_top(Request, 'KEY', 'DESCEND').

%% withTop gets the first key over the get's prefix given a sort order
with_top(Request, SortTarget, SortOrder) ->
    R1 = with_sort(Request, SortTarget, SortOrder),
    with_limit(R1, 1).

%% KV
with_prev_kv(Request, Bool) when is_boolean(Bool) ->
    maps:put(prev_kv, Bool, Request).

with_lease(Request, Bool) when is_boolean(Bool) ->
    maps:put(lease, Bool, Request).

with_ignore_value(Request, Bool) when is_boolean(Bool) ->
    maps:put(ignore_value, Bool, Request).

with_ignore_lease(Request, Bool) when is_boolean(Bool) ->
    maps:put(ignore_lease, Bool, Request).

%%

%% WithCompactPhysical makes Compact wait until all compacted entries are
%% removed from the etcd server's storage.
with_compact_physical(Request) ->
    maps:put(physical, true, Request).

get_prev_kv(Opts) ->
    take_with_default(withPreKV, Opts, undefined).

get_lease(Opts) ->
    take_with_default(withLease, Opts, undefined).

get_ignore_value(Opts) ->
    take_with_default(withIgnoreValue, Opts, undefined).

get_ignore_lease(Opts) ->
    take_with_default(withIgnoreLease, Opts, undefined).


take_with_default(Key, Opts, Default) ->
    case maps:take(Key, Opts) of
        error -> {Default, Opts};
        Res -> Res
    end.

%%isWithPrefix(Opts) ->
%%    Opts.'WithPrefix'
%%#{
%%t    => opType
%%key  =>
%%end []byte
%%
%%// for range
%%limit        int64
%%sort         *SortOption
%%serializable bool
%%keysOnly     bool
%%countOnly    bool
%%minModRev    int64
%%maxModRev    int64
%%minCreateRev int64
%%maxCreateRev int64
%%
%%// for range, watch
%%rev int64
%%
%%// for watch, put, delete
%%prevKV bool
%%
%%// for watch
%%// fragmentation should be disabled by default
%%// if true, split watch events when total exceeds
%%// "--max-request-bytes" flag value + 512-byte
%%fragment bool
%%
%%// for put
%%ignoreValue bool
%%ignoreLease bool
%%
%%// progressNotify is for progress updates.
%%progressNotify bool
%%// createdNotify is for created event
%%createdNotify bool
%%// filters for watchers
%%filterPut    bool
%%filterDelete bool
%%
%%// for put
%%val     []byte
%%leaseID LeaseID
%%
%%// txn
%%cmps    []Cmp
%%thenOps []Op
%%elseOps []Op
%%}