-module(eetcd).
-include("eetcd.hrl").
%% API
-export([test/0]).
-export([open/4, close/1]).
-export([
    new/1, with_timeout/2, with_key/2,
    with_value/2, with_prefix/1, with_range_end/2,
    with_from_key/1, with_limit/2, with_rev/2,
    with_serializable/1, with_keys_only/1, with_count_only/1,
    with_min_mod_rev/2, with_max_mod_rev/2,
    with_max_create_rev/2, with_min_create_rev/2,
    with_first_create/1, with_last_create/1,
    with_first_rev/1, with_last_rev/1, with_first_key/1,
    with_last_key/1, with_top/3, with_prev_kv/1,
    with_lease/2, with_ignore_value/1, with_ignore_lease/1,
    with_compact_physical/1,
    with_progress_notify/1, with_fragment/1, with_filter_put/1, with_filter_delete/1]).

test() ->
    logger:set_primary_config(level, info),
    {ok, _Pid} = eetcd:open(test, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"], tcp, []),
    R = eetcd_kv:put(test, "test", "test_value"),
    io:format("~p~n", [R]),
    R1 = eetcd_kv:get(test, "test"),
    io:format("~p~n", [R1]),
    eetcd:close(test),
    ok.

-spec open(name(),
    [string()],
    tcp | tls | ssl,
    [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok, pid()} | {error, any()}.
open(Name, Hosts, Transport, TransportOpts) ->
    Cluster = [begin [IP, Port] = string:tokens(Host, ":"), {IP, list_to_integer(Port)} end || Host <- Hosts],
    gen_server:start_link(eetcd_conn, [Name, Cluster, Transport, TransportOpts], []).

-spec close(name()) -> ok.
close(Name) ->
    case ets:lookup(?ETCD_CONNS, Name) of
        [#eetcd_conn{conn = Pid}] -> erlang:send(Pid, stop);
        _ -> ok
    end,
    ok.

%%% @doc Create options for request.
-spec new(atom()|reference()) -> context().
new(ConnName) when is_atom(ConnName) orelse is_reference(ConnName) -> #{eetcd_conn_name => ConnName};
new(Context) when is_map(Context) -> Context.

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()) -> context().
with_timeout(Context, Timeout) when is_integer(Timeout) ->
    maps:put(eetcd_reply_timeout, Timeout, Context).

%%% @doc Sets the byte slice for the Op's `key'.
-spec with_key(context(), key()) -> context().
with_key(Context, Key) ->
    maps:put(key, Key, Context).

%% @doc Sets the byte slice for the Op's `value'.
-spec with_value(context(), key()) -> context().
with_value(Context, Key) ->
    maps:put(value, Key, Context).

%% @doc Enables `get', `delete', or `watch' requests to operate
%% on the keys with matching prefix. For example, `get("foo", with_prefix())'
%% can return 'foo1', 'foo2', and so on.
-spec with_prefix(context()) -> context().
with_prefix(Context) ->
    with_range_end(Context, "\0").

%%  @doc Specifies the range of `get', `delete', `watch' requests
%% to be equal or greater than the key in the argument.
-spec with_from_key(context()) -> context().
with_from_key(Context) ->
    with_range_end(Context, "\x00").

%% @doc Sets the byte slice for the Op's `range_end'.
-spec with_range_end(context(), iodata()) -> context().
with_range_end(Context, End) ->
    maps:put(range_end, End, Context).

%% @doc Limit the number of results to return from `get' request.
%% If with_limit is given a 0 limit, it is treated as no limit.
-spec with_limit(context(), iodata()) -> context().
with_limit(Context, End) ->
    maps:put(limit, End, Context).

%% @doc Specifies the store revision for `Get' request.
%% Or the start revision of `watch' request.
-spec with_rev(context(), pos_integer()) -> context().
with_rev(Context, Rev) ->
    maps:put(revision, Rev, Context).

%% @doc Specifies the ordering in `get' request. It requires
%% `with_range' and/or `with_prefix' to be specified too.
%% `target' specifies the target to sort by: 'KEY', 'VERSION', 'VALUE', 'CREATE', 'MOD'.
%% `order' can be either 'NONE', 'ASCEND', 'DESCEND'.
-spec with_sort(context(),
    'KEY' | 'VERSION' | 'VALUE' | 'CREATE' |'MOD',
    'NONE' | 'ASCEND' | 'DESCEND') -> context().
with_sort(Context, Target, Order) ->
    Targets = router_pb:find_enum_def('Etcd.RangeRequest.SortTarget'),
    Orders = router_pb:find_enum_def('Etcd.RangeRequest.SortOrder'),
    (not lists:keymember(Target, 1, Targets)) andalso throw({sort_target, Target}),
    (not lists:keymember(Order, 1, Orders)) andalso throw({sort_order, Order}),
    R1 = maps:put(sort_order, Order, Context),
    maps:put(sort_target, Target, R1).

%% @doc Make `get' request serializable. By default,
%% it's linearizable. Serializable requests are better for lower latency
%% requirement.
-spec with_serializable(context()) -> context().
with_serializable(Context) ->
    maps:put(serializable, true, Context).

%% @doc Make the `get' request return only the keys and the corresponding
%% values will be omitted.
-spec with_keys_only(context()) -> context().
with_keys_only(Context) ->
    maps:put(keys_only, true, Context).

%% @doc Make the `Get' request return only the count of keys.
-spec with_count_only(context()) -> context().
with_count_only(Context) ->
    maps:put(count_only, true, Context).

%% @doc Filter out keys for Get with modification revisions less than the given revision.
-spec with_min_mod_rev(context(), pos_integer()) -> context().
with_min_mod_rev(Context, Rev) ->
    maps:put(min_mod_revision, Rev, Context).

%% @doc Filter out keys for Get with modification revisions greater than the given revision.
-spec with_max_mod_rev(context(), pos_integer()) -> context().
with_max_mod_rev(Context, Rev) ->
    maps:put(max_mod_revision, Rev, Context).

%% @doc Filter out keys for Get with creation revisions less than the given revision.
-spec with_min_create_rev(context(), pos_integer()) -> context().
with_min_create_rev(Context, Rev) ->
    maps:put(min_create_revision, Rev, Context).

%% @doc Filter out keys for Get with creation revisions greater than the given revision.
-spec with_max_create_rev(context(), pos_integer()) -> context().
with_max_create_rev(Context, Rev) ->
    maps:put(max_create_revision, Rev, Context).

%% @doc Get the key with the oldest creation revision in the request range.
-spec with_first_create(context()) -> context().
with_first_create(Context) ->
    with_top(Context, 'CREATE', 'ASCEND').

%% @doc Get the lexically last key in the request range.
-spec with_last_create(context()) -> context().
with_last_create(Context) ->
    with_top(Context, 'CREATE', 'DESCEND').

%% @doc Get the key with the oldest modification revision in the request range.
-spec with_first_rev(context()) -> context().
with_first_rev(Context) ->
    with_top(Context, 'MOD', 'ASCEND').

%% @doc Get the key with the latest modification revision in the request range.
-spec with_last_rev(context()) -> context().
with_last_rev(Context) ->
    with_top(Context, 'MOD', 'DESCEND').

%% @doc Get the lexically first key in the request range.
-spec with_first_key(context()) -> context().
with_first_key(Context) ->
    with_top(Context, 'KEY', 'ASCEND').

%% @doc Get the lexically last key in the request range.
-spec with_last_key(context()) -> context().
with_last_key(Context) ->
    with_top(Context, 'KEY', 'DESCEND').

%% @doc Get the first key over the get's prefix given a sort order
-spec with_top(context(),
    'KEY' | 'VERSION' | 'VALUE' | 'CREATE' |'MOD',
    'NONE' | 'ASCEND' | 'DESCEND') -> context().
with_top(Context, SortTarget, SortOrder) ->
    C1 = with_sort(Context, SortTarget, SortOrder),
    with_limit(C1, 1).

%% @doc Get the previous key-value pair before the event happens.
%% If the previous KV is already compacted, nothing will be returned.
-spec with_prev_kv(context()) -> context().
with_prev_kv(Context) ->
    maps:put(prev_kv, true, Context).

%% @doc WithFragment to receive raw watch response with fragmentation.
%%Fragmentation is disabled by default. If fragmentation is enabled,
%%etcd watch server will split watch response before sending to clients
%%when the total size of watch events exceed server-side request limit.
%%The default server-side request limit is 1.5 MiB, which can be configured
%%as "--max-request-bytes" flag value + gRPC-overhead 512 bytes.
%%See "watch.erl" for more details.
-spec with_fragment(context()) -> context().
with_fragment(Context) ->
    maps:put(fragment, true, Context).

%% @doc Attache a lease ID to a key in `put' request.
-spec with_lease(context(), integer()) -> context().
with_lease(Context, Id) when is_integer(Id) ->
    maps:put(lease, Id, Context).

%% @doc Update the key using its current value.
%% This option can not be combined with non-empty values.
%% Returns an error if the key does not exist.
-spec with_ignore_value(context()) -> context().
with_ignore_value(Context) ->
    maps:put(ignore_value, true, Context).

%% @doc Update the key using its current lease.
%% This option can not be combined with WithLease.
%% Returns an error if the key does not exist.
-spec with_ignore_lease(context()) -> context().
with_ignore_lease(Context) ->
    maps:put(ignore_lease, true, Context).

%% @doc Make Compact wait until all compacted entries are
%% removed from the etcd server's storage.
-spec with_compact_physical(context()) -> context().
with_compact_physical(Context) ->
    maps:put(physical, true, Context).

%% @doc Make watch server send periodic progress updates
%% every 10 minutes when there is no incoming events.
%% Progress updates have zero events in WatchResponse.
-spec with_progress_notify(context()) -> context().
with_progress_notify(Context) ->
    maps:put(progress_notify, true, Context).

%% @doc discards PUT events from the watcher.
-spec with_filter_put(context()) -> context().
with_filter_put(Context) ->
    maps:update_with(filters,
        fun(V) -> lists:usort(['NOPUT' | V]) end,
        ['NOPUT'],
        Context).

%% @doc discards DELETE events from the watcher.
-spec with_filter_delete(context()) -> context().
with_filter_delete(Context) ->
    maps:update_with(filters,
        fun(V) -> lists:usort(['NODELETE' | V]) end,
        ['NODELETE'],
        Context).