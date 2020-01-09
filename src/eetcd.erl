-module(eetcd).

%% API
-export([test/0]).

-export([
    new/0, with_timeout/2, with_key/2,
    with_value/2, with_prefix/1, with_range_end/2,
    with_from_key/1, with_limit/2, with_rev/2,
    with_serializable/1, with_keys_only/1, with_count_only/1,
    with_min_mod_rev/2, with_max_mod_rev/2,
    with_max_create_rev/2, with_min_create_rev/2,
    with_first_create/1, with_last_create/1,
    with_first_rev/1, with_last_rev/1, with_first_key/1,
    with_last_key/1, with_top/3, with_prev_kv/2,
    with_lease/2, with_ignore_value/2, with_ignore_lease/2,
    with_compact_physical/1
]).

test() ->
    logger:set_primary_config(level, info),
    {ok, _Pid} = eetcd_conn:open(test, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"], tcp, []),
    R = eetcd_kv:put("test", "test_value"),
    io:format("~p~n", [R]),
    R1 = eetcd_kv:get("test"),
    io:format("~p~n", [R1]),
    eetcd_conn:close(test),
    ok.

%%% @doc Create options for request.
new() -> #{}.

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
with_timeout(Request, Timeout) when is_integer(Timeout) ->
    maps:put(eetcd_reply_timeout, Timeout, Request).

%%% @doc Sets the byte slice for the Op's `key'.
with_key(Request, Key) ->
    maps:put(key, Key, Request).

%% @doc Sets the byte slice for the Op's `value'.
with_value(Request, Key) ->
    maps:put(value, Key, Request).

%% @doc Enables `get', `delete', or `watch' requests to operate
%% on the keys with matching prefix. For example, `get("foo", with_prefix())'
%% can return 'foo1', 'foo2', and so on.
with_prefix(Request) ->
    with_range_end(Request, "\0").

%%  @doc Specifies the range of `get', `delete', `watch' requests
%% to be equal or greater than the key in the argument.
with_from_key(Request) ->
    with_range_end(Request, "\x00").

%% @doc Sets the byte slice for the Op's `range_end'.
with_range_end(Request, End) ->
    maps:put(range_end, End, Request).

%% @doc Limit the number of results to return from `get' request.
%% If with_limit is given a 0 limit, it is treated as no limit.
with_limit(Request, End) ->
    maps:put(limit, End, Request).

%% @doc Specifies the store revision for `Get' request.
%% Or the start revision of `watch' request.
with_rev(Request, Rev) ->
    maps:put(revision, Rev, Request).

%% @doc Specifies the ordering in `get' request. It requires
%% `with_range' and/or `with_prefix' to be specified too.
%% `target' specifies the target to sort by: 'KEY', 'VERSION', 'VALUE', 'CREATE', 'MOD'.
%% `order' can be either 'NONE', 'ASCEND', 'DESCEND'.
with_sort(Request, Target, Order) ->
    Targets = router_pb:find_enum_def('Etcd.RangeRequest.SortTarget'),
    Orders = router_pb:find_enum_def('Etcd.RangeRequest.SortOrder'),
    (not lists:keymember(Target, 1, Targets)) andalso throw({sort_target, Target}),
    (not lists:keymember(Order, 1, Orders)) andalso throw({sort_order, Order}),
    R1 = maps:put(sort_order, Order, Request),
    maps:put(sort_target, Target, R1).

%% @doc Make `get' request serializable. By default,
%% it's linearizable. Serializable requests are better for lower latency
%% requirement.
with_serializable(Request) ->
    maps:put(serializable, true, Request).

%% @doc Make the `get' request return only the keys and the corresponding
%% values will be omitted.
with_keys_only(Request) ->
    maps:put(keys_only, true, Request).

%% @doc Make the `Get' request return only the count of keys.
with_count_only(Request) ->
    maps:put(count_only, true, Request).

%% @doc Filter out keys for Get with modification revisions less than the given revision.
with_min_mod_rev(Request, Rev) ->
    maps:put(min_mod_revision, Rev, Request).

%% @doc Filter out keys for Get with modification revisions greater than the given revision.
with_max_mod_rev(Request, Rev) ->
    maps:put(max_mod_revision, Rev, Request).

%% @doc Filter out keys for Get with creation revisions less than the given revision.
with_min_create_rev(Request, Rev) ->
    maps:put(min_create_revision, Rev, Request).

%% @doc Filter out keys for Get with creation revisions greater than the given revision.
with_max_create_rev(Request, Rev) ->
    maps:put(max_create_revision, Rev, Request).

%% @doc Get the key with the oldest creation revision in the request range.
with_first_create(Request) ->
    with_top(Request, 'CREATE', 'ASCEND').

%% @doc Get the lexically last key in the request range.
with_last_create(Request) ->
    with_top(Request, 'CREATE', 'DESCEND').

%% @doc Get the key with the oldest modification revision in the request range.
with_first_rev(Request) ->
    with_top(Request, 'MOD', 'ASCEND').

%% @doc Get the key with the latest modification revision in the request range.
with_last_rev(Request) ->
    with_top(Request, 'MOD', 'DESCEND').

%% @doc Get the lexically first key in the request range.
with_first_key(Request) ->
    with_top(Request, 'KEY', 'ASCEND').

%% @doc Get the lexically last key in the request range.
with_last_key(Request) ->
    with_top(Request, 'KEY', 'DESCEND').

%% @doc Get the first key over the get's prefix given a sort order
with_top(Request, SortTarget, SortOrder) ->
    R1 = with_sort(Request, SortTarget, SortOrder),
    with_limit(R1, 1).

%% @doc Get the previous key-value pair before the event happens.
%% If the previous KV is already compacted, nothing will be returned.
with_prev_kv(Request, Bool) when is_boolean(Bool) ->
    maps:put(prev_kv, Bool, Request).

%% @doc Attache a lease ID to a key in `put' request.
with_lease(Request, Bool) when is_boolean(Bool) ->
    maps:put(lease, Bool, Request).

%% @doc Update the key using its current value.
%% This option can not be combined with non-empty values.
%% Returns an error if the key does not exist.
with_ignore_value(Request, Bool) when is_boolean(Bool) ->
    maps:put(ignore_value, Bool, Request).

%% @doc Update the key using its current lease.
%% This option can not be combined with WithLease.
%% Returns an error if the key does not exist.
with_ignore_lease(Request, Bool) when is_boolean(Bool) ->
    maps:put(ignore_lease, Bool, Request).

%% @doc Make Compact wait until all compacted entries are
%% removed from the etcd server's storage.
with_compact_physical(Request) ->
    maps:put(physical, true, Request).

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