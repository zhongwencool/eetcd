-module(eetcd_watch).
-include("eetcd.hrl").

-export_type([watch_conn/0]).
-type watch_conn() :: #{http2_pid => pid(),
monitor_ref => reference(),
stream_ref => reference(),
unprocessed => binary(),
revision => integer(), %% Revision is the revision of the KV when the watchResponse is created. For a normal response, the revision should be the same as the last modified revision inside Events.
compact_revision => integer(), %% CompactRevision is set when the watcher is cancelled due to compaction.
watch_id => integer(),
response => router_pb:'Etcd.WatchResponse'()}.

%% API
-export([with_key/2,
    with_range_end/2, with_prefix/1, with_from_key/1,
    with_start_revision/2,
    with_filter_delete/1, with_filter_put/1,
    with_prev_kv/1,
    with_watch_id/2,
    with_fragment/1,
    with_progress_notify/1
]).
-export([watch/2, watch/3]).
-export([watch_stream/2]).
-export([unwatch/2]).
-export([rev/1]).
-export([test/0]).

test() ->
    application:ensure_all_started(eetcd),
    logger:set_primary_config(level, info),
    {ok, _Pid} = eetcd:open(test, ["127.0.0.1:2379", "127.0.0.1:2579", "127.0.0.1:2479"], tcp, []),
    %% {ok, _Pid} = eetcd:open(test, ["127.0.0.1:2379"], tcp, []),
    R1 = eetcd_kv:get(test, "test"),
    io:format("~p~n", [R1]),
    Request = with_key(#{}, "test"),
    {ok, Conn} = eetcd_watch:watch(test, Request, 5000),
    io:format("~p~n", [Conn]),
    eetcd_kv:put(test, "test", "goodv1"),
    receive X ->
        io:format("put:~p~n", [eetcd_watch:watch_stream(Conn, X)])
    end,
    eetcd_kv:delete(test, "test"),
    receive Y ->
        {ok, Conn1, Resp} = eetcd_watch:watch_stream(Conn, Y),
        io:format("delete:~p~n", [Resp]),
        T2 = eetcd_watch:unwatch(Conn1, 5000),
        io:format("unwatch:~p~n", [T2])
    end,
    ok.

%% @doc AutoWatchID is the watcher ID passed in WatchStream.Watch when no
%% user-provided ID is available, an ID will automatically be assigned.
with_watch_id(Context, WatchId) ->
    maps:put(watch_id, WatchId, Context).

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

with_start_revision(Context, Rev) ->
    maps:put(start_revision, Rev, Context).

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

%%% @doc Sets the byte slice for the Op's `key'.
-spec with_key(context(), key()) -> context().
with_key(Context, Key) ->
    maps:put(key, Key, Context).

%% @doc Enables `watch' requests to operate
%% on the keys with matching prefix. For example, `watch("foo", with_prefix())'
%% can return 'foo1', 'foo2', and so on.
-spec with_prefix(context()) -> context().
with_prefix(Context) ->
    with_range_end(Context, "\0").

%%  @doc Specifies the range of `get', `delete' requests
%% to be equal or greater than the key in the argument.
-spec with_from_key(context()) -> context().
with_from_key(Context) ->
    with_range_end(Context, "\x00").

%% @doc Sets the byte slice for the Op's `range_end'.
-spec with_range_end(context(), iodata()) -> context().
with_range_end(Context, End) ->
    maps:put(range_end, End, Context).

%% @doc {@equiv watch(name(), context(), 5000)}.
-spec watch(name(), context()) ->
    {ok, watch_conn()} | {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch(Name, CreateReq) ->
    watch(Name, CreateReq, 5000).

%% @doc Watch watches for events happening or that have happened. Both input and output are streams;
%% the input stream is for creating watchers and the output stream sends events.
%% One watch RPC can watch on multiple key ranges, streaming events for several watches at once.
%% The entire event history can be watched starting from the last compaction revision.
%%
%% Watch creates a watcher. The watcher watches the events happening or
%% happened on the given key or range [key, end) from the given startRev.
%%
%%	The whole event history can be watched unless compacted.
%%	If "startRev" <=0, watch observes events after currentRev.
%%
%%	The returned "id" is the ID of this watcher. It appears as WatchID
%%  in events that are sent to the created watcher through stream channel.
-spec watch(name(), context(), pos_integer()) ->
    {ok, watch_conn()} | {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch(Name, CreateReq, Timeout) ->
    Request = #{request_union => {create_request, CreateReq}},
    MRef = erlang:monitor(process, self()),
    {ok, Gun, StreamRef} = eetcd_watch_gen:watch(Name),
    eetcd_stream:data(Gun, StreamRef, Request, 'Etcd.WatchRequest', nofin),
    case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
        {response, nofin, 200, _Headers} ->
            case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
                {data, nofin, Body} ->
                    {ok,
                        #{
                            created := true,
                            canceled := false,
                            compact_revision := CompactRev,
                            header := #{revision := Rev},
                            watch_id := WatchId
                        }, <<>>}
                        = eetcd_grpc:decode(identity, Body, 'Etcd.WatchResponse'),
                    {ok,
                        #{
                            http2_pid => Gun,
                            monitor_ref => MRef,
                            stream_ref => StreamRef,
                            revision => Rev,
                            compact_revision => CompactRev,
                            watch_id => WatchId,
                            unprocessed => <<>>
                        }
                    };
                {error, _} = Err1 ->
                    erlang:demonitor(MRef, [flush]),
                    Err1
            end;
        {response, fin, 200, RespHeaders} ->
            erlang:demonitor(MRef, [flush]),
            {error, eetcd_grpc:grpc_status(RespHeaders)};
        {error, _} = Err2 ->
            erlang:demonitor(MRef, [flush]),
            Err2
    end.

%% @doc Streams the next batch of events from the given message.
%%This function processes a "message" which can be any term, but should be a message received by the process that owns the stream_ref.
%%Processing a message means that this function will parse it and check if it's a message that is directed to this connection,
%%that is, a gun_* message received on the gun connection.
%%If it is, then this function will parse the message, turn it into  watch responses, and possibly take action given the responses.
%%If there's no error, this function returns {ok, WatchConn, 'Etcd.WatchResponse'()}|{more, WatchConn}
%%If there's an error, {error, {stream_error | conn_error | http2_down, term()} | timeout} is returned.
%%If the given message is not from the gun connection, this function returns unknown.
-spec watch_stream(watch_conn(), Message) ->
    {ok, watch_conn(), router_pb:'Etcd.WatchResponse'()}
    | {more, watch_conn()}
    | unknown
    | {error, {stream_error | conn_error | http2_down, term()}} when
    Message :: term().

watch_stream(#{stream_ref := Ref, http2_pid := Pid, unprocessed := Unprocessed} = Conn,
    {gun_data, Pid, Ref, nofin, Data}) ->
    Bin = <<Unprocessed/binary, Data/binary>>,
    case eetcd_grpc:decode(identity, Bin, 'Etcd.WatchResponse') of
        {ok, Resp, NewUnprocessed} ->
            #{compact_revision := CompactRev,
                header := #{revision := Rev},
                watch_id := WatchId} = Resp,
            {ok,
                Conn#{
                    revision => Rev,
                    compact_revision => CompactRev,
                    watch_id => WatchId,
                    unprocessed => NewUnprocessed},
                Resp};
        more -> {more, Conn#{unprocessed => Bin}}
    end;
watch_stream(#{stream_ref := Ref, http2_pid := Pid}, {gun_error, Pid, Ref, Reason}) -> %% stream error
    {error, {stream_error, Reason}};
watch_stream(#{http2_pid := Pid}, {gun_error, Pid, Reason}) -> %% gun connection process state error
    {error, {conn_error, Reason}};
watch_stream(#{monitor_ref := MRef, http2_pid := Pid}, {'DOWN', MRef, process, Pid, Reason}) -> %% gun connection down
    {error, {http2_down, Reason}};
watch_stream(_Conn, _UnKnow) -> unknown.

%% @doc Rev returns the current revision of the KV the stream watches on.

rev(#{revision := Rev}) -> Rev.

%% @doc  Cancel watching so that no more events are transmitted.
%% This is a synchronous operation.
%% Other change events will be returned in OtherEvents when these events arrive between the request and the response.
-spec unwatch(watch_conn(), Timeout) ->
    {ok, router_pb:'Etcd.WatchResponse'(), OtherEvents}
    | {error, {stream_error | conn_error | http2_down, term()} | timeout, OtherEvents} when
    Timeout :: pos_integer(),
    OtherEvents :: [router_pb:'Etcd.WatchResponse'()].
unwatch(WatchConn, Timeout) ->
    #{
        http2_pid := Gun,
        monitor_ref := MRef,
        stream_ref := StreamRef,
        watch_id := WatchId,
        unprocessed := Unprocessed
    } = WatchConn,
    Request = #{request_union => {cancel_request, #{watch_id => WatchId}}},
    eetcd_stream:data(Gun, StreamRef, Request, 'Etcd.WatchRequest', fin),
    await_unwatch_resp(Gun, StreamRef, Unprocessed, WatchId, Timeout, MRef, []).

%%====================================================================
%% Internal functions
%%====================================================================

await_unwatch_resp(Gun, StreamRef, Unprocessed, WatchId, Timeout, MRef, Acc) ->
    case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
        {data, nofin, Data} ->
            Bin = <<Unprocessed/binary, Data/binary>>,
            case eetcd_grpc:decode(identity, Bin, 'Etcd.WatchResponse') of
                {ok, Resp, NewUnprocessed} ->
                    case Resp of
                        #{created := false, watch_id := WatchId, canceled := true} ->
                            gun:cancel(Gun, StreamRef),
                            erlang:demonitor(MRef, [flush]),
                            {ok, Resp, lists:reverse(Acc)};
                        OtherEvent ->
                            await_unwatch_resp(Gun, StreamRef, NewUnprocessed, WatchId, Timeout, MRef, [OtherEvent | Acc])
                    end;
                more ->
                    await_unwatch_resp(Gun, StreamRef, Bin, WatchId, Timeout, MRef, Acc)
            end;
        {error, Reason} -> {error, Reason, lists:reverse(Acc)}
    end.
