-module(eetcd_watch).
-include("eetcd.hrl").

-export_type([watch_conn/0]).
-type watch_conn() :: #{http2_pid => pid(),
                        monitor_ref => reference(),
                        stream_ref => reference(),

                        %% A buffer for incompleted response frame
                        unprocessed => binary(),

                        %% Revision is the revision of the KV when the watchResponse is created, aka
                        %% the [Start_Revision](https://etcd.io/docs/v3.5/learning/api/).
                        watch_ids => #{ integer() =>
                                        #{
                                          %% For a normal response, the revision should be the same as the last modified revision inside Events.
                                          revision => integer(),

                                          %% CompactRevision is set when the watcher is cancelled due to compaction.
                                          compact_revision => integer()
                                         }}
                       }.

%% API
-export([new/0, with_key/2,
    with_range_end/2, with_prefix/1, with_from_key/1,
    with_start_revision/2,
    with_filter_delete/1, with_filter_put/1,
    with_prev_kv/1,
    with_watch_id/2,
    with_fragment/1,
    with_progress_notify/1
]).
-export([watch/2, watch/3, watch/4]).
-export([watch_stream/2]).
-export([unwatch/2]).
-export([rev/1]).

%%% @doc init watch request
-spec new() -> context().
new() -> #{}.

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
-spec with_fragment(context()) -> context().
with_fragment(Context) ->
    maps:put(fragment, true, Context).

%% @doc Start revision, an optional revision for where to inclusively begin watching.
%% If not given, it will stream events following the revision of the watch creation
%% response header revision.
%% The entire available event history can be watched starting from the last compaction revision.
%%
%% Note that the start revision is inclusive, so for example, if the start revision is 100,
%% the first event returned will be at revision 100. So in practice, the start revision is better
%% set to the last **GET** revision + 1 to exclude the previous change before the watch.
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
with_prefix(#{key := Key} = Context) ->
    with_range_end(Context, eetcd:get_prefix_range_end(Key)).

%%  @doc Specifies the range of `get', `delete' requests
%% to be equal or greater than the key in the argument.
-spec with_from_key(context()) -> context().
with_from_key(Context) ->
    with_range_end(Context, "\x00").

%% @doc Sets the byte slice for the Op's `range_end'.
-spec with_range_end(context(), iodata()) -> context().
with_range_end(Context, End) ->
    maps:put(range_end, End, Context).

%% @doc @equiv watch(name(), context(), 5000).
-spec watch(name(), context()) ->
    {ok, watch_conn(), WatchId :: pos_integer()} |
    {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch(Name, CreateReq) ->
    watch(Name, CreateReq, undefined, 5000).

-spec watch(name(), context(), Timeout :: pos_integer() | watch_conn() | undefined) ->
    {ok, watch_conn(), WatchId :: pos_integer()} |
    {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch(Name, CreateReq, Timeout) when is_integer(Timeout) ->
    watch(Name, CreateReq, undefined, Timeout);
watch(Name, CreateReq, WatchConn) ->
    watch(Name, CreateReq, WatchConn, 5000).

%% @doc Watch watches for events happening or that have happened. Both input and output are streams;
%% the input stream is for creating watchers and the output stream sends events.
%% One watch RPC can watch on multiple key ranges, streaming events for several watches at once.
%% The entire event history can be watched starting from the last compaction revision.
%%
%% Watch creates a watcher. The watcher watches the events happening or
%% happened on the given key or range [key, end) from the given startRev.
%%
%%	The whole event history can be watched unless compacted.
%%	If `startRev <= 0', watch observes events after currentRev.
%%
%%	The returned "id" is the ID of this watcher. It appears as WatchID
%%  in events that are sent to the created watcher through stream channel.
-spec watch(name(), context(), watch_conn() | undefined, pos_integer()) ->
    {ok, watch_conn(), WatchId :: pos_integer()} |
    {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch(_Name, CreateReq, #{http2_pid := Gun,
                          stream_ref := StreamRef,
                          monitor_ref := MRef} = WatchConn, Timeout)
  when is_pid(Gun), is_reference(StreamRef), is_reference(MRef) ->
    watch_reuse_(CreateReq, WatchConn, Timeout);
watch(Name, CreateReq, undefined, Timeout) ->
    case eetcd_watch_gen:watch(Name) of
        {ok, Gun, StreamRef} -> watch_new_(CreateReq, Gun, StreamRef, Timeout);
        {error, _Reason} = E -> E
    end.

%% Do watch request with a new watch stream.
-spec watch_new_(context(), pid(), reference(), pos_integer()) ->
    {ok, watch_conn(), WatchId :: pos_integer()} |
    {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch_new_(CreateReq, Gun, StreamRef, Timeout) ->
    Request = #{request_union => {create_request, CreateReq}},
    MRef = erlang:monitor(process, Gun),
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
                            watch_ids => #{ WatchId => #{ revision => Rev,
                                                          compact_revision => CompactRev}},
                            unprocessed => <<>>
                        },
                        WatchId
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

%% Do watch request with the re-used watch stream.
-spec watch_reuse_(context(), watch_conn(), pos_integer()) ->
    {ok, watch_conn(), WatchId :: pos_integer()} |
    {error, {stream_error | conn_error | http2_down, term()} | timeout}.
watch_reuse_(CreateReq, #{http2_pid   := Gun,
                          stream_ref  := StreamRef,
                          monitor_ref := MRef,
                          watch_ids   := Ids} = WatchConn,
             Timeout) ->
    Request = #{request_union => {create_request, CreateReq}},
    eetcd_stream:data(Gun, StreamRef, Request, 'Etcd.WatchRequest', nofin),
    case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
        {response, fin, 200, RespHeaders} ->
            erlang:demonitor(MRef, [flush]),
            {error, eetcd_grpc:grpc_status(RespHeaders)};

        %% Response for the watch request with the existed/re-used watch stream.
        {data, nofin, Body} ->
            case eetcd_grpc:decode(identity, Body, 'Etcd.WatchResponse') of
                {ok,
                    #{
                        created := true,
                        canceled := false,
                        compact_revision := CompactRev,
                        header := #{revision := Rev},
                        watch_id := WatchId
                    }, <<>>} ->
                    {ok,
                        WatchConn#{
                            http2_pid => Gun,
                            monitor_ref => MRef,
                            stream_ref => StreamRef,
                            watch_ids => Ids#{ WatchId => #{ revision => Rev,
                                                             compact_revision => CompactRev}},
                            unprocessed => <<>>
                        },
                        WatchId
                    };

                {ok, #{created := false} = ReceivedMessage, _} ->
                    {error, {stream_error, ReceivedMessage}}
            end;

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
%%If there's an error, {error, {grpc_error, stream_error | conn_error | http2_down, term()} | timeout} is returned.
%%If the given message is not from the gun connection, this function returns unknown.
-spec watch_stream(watch_conn(), Message) ->
    {ok, watch_conn(), router_pb:'Etcd.WatchResponse'()}
    | {more, watch_conn()}
    | unknown
    | {error, {grpc_error, stream_error | conn_error | http2_down, term()}} when
    Message :: term().

watch_stream(#{stream_ref := Ref, http2_pid := Pid, unprocessed := Unprocessed, watch_ids := Ids} = Conn,
    {gun_data, Pid, Ref, nofin, Data}) ->
    Bin = <<Unprocessed/binary, Data/binary>>,
    case eetcd_grpc:decode(identity, Bin, 'Etcd.WatchResponse') of
        {ok, Resp, NewUnprocessed} ->
            #{compact_revision := CompactRev,
                header := #{revision := Rev},
                watch_id := WatchId} = Resp,
            {ok,
                Conn#{
                    watch_ids => Ids#{ WatchId => #{ revision => Rev,
                                                     compact_revision => CompactRev}},
                    unprocessed => NewUnprocessed},
                Resp};
        more -> {more, Conn#{unprocessed => Bin}}
    end;
watch_stream(#{stream_ref := SRef, http2_pid := Pid, monitor_ref := MRef},
    {gun_trailers, Pid, SRef, [{<<"grpc-status">>, Status}, {<<"grpc-message">>, Msg}]}) ->
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, {grpc_error, ?GRPC_ERROR(Status, Msg)}};
watch_stream(#{stream_ref := SRef, http2_pid := Pid, monitor_ref := MRef},
    {gun_error, Pid, SRef, Reason}) -> %% stream error
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, {stream_error, Reason}};
watch_stream(#{http2_pid := Pid, stream_ref := SRef, monitor_ref := MRef},
    {gun_error, Pid, Reason}) -> %% gun connection process state error
    erlang:demonitor(MRef, [flush]),
    gun:cancel(Pid, SRef),
    {error, {conn_error, Reason}};
watch_stream(#{http2_pid := Pid, monitor_ref := MRef},
    {'DOWN', MRef, process, Pid, Reason}) -> %% gun connection down
    erlang:demonitor(MRef, [flush]),
    {error, {http2_down, Reason}};
watch_stream(_Conn, _UnKnow) -> unknown.

%% @doc Rev returns the current revision of the KV the stream watches on.

rev(#{revision := Rev}) -> Rev.

%% @doc  Cancel watching so that no more events are transmitted.
%% This is a synchronous operation.
%% Other change events will be returned in OtherEvents when these events arrive between the request and the response.
%%
%% Notice that this function will cancel all the watches in the same stream.
-spec unwatch(watch_conn(), Timeout) ->
    {ok, Responses, OtherEvents}
    | {error, {stream_error | conn_error | http2_down, term()} | timeout, Responses, OtherEvents} when
    Timeout :: pos_integer(),
    Responses :: [router_pb:'Etcd.WatchResponse'()],
    OtherEvents :: [router_pb:'Etcd.WatchResponse'()].
unwatch(WatchConn, Timeout) ->
    unwatch_and_await_resp(WatchConn, Timeout, [], []).

%%====================================================================
%% Internal functions
%%====================================================================

unwatch_and_await_resp(#{http2_pid := Gun,
                         stream_ref := StreamRef,
                         monitor_ref := MRef,
                         watch_ids := WatchIds} = _WatchConn, _Timeout, RespAcc, Acc)
    when erlang:map_size(WatchIds) =:= 0 ->
    gun:cancel(Gun, StreamRef),
    erlang:demonitor(MRef, [flush]),
    {ok, lists:reverse(RespAcc), lists:reverse(Acc)};
unwatch_and_await_resp(#{http2_pid := Gun,
                         stream_ref := StreamRef,
                         watch_ids := WatchIds} = WatchConn, Timeout, RespAcc, Acc) ->
    [WatchId|_Rest] = maps:keys(WatchIds),
    IsFin = case maps:size(WatchIds) of
                1 -> fin;
                _ -> nofin
            end,
    Request = #{request_union => {cancel_request, #{watch_id => WatchId}}},
    eetcd_stream:data(Gun, StreamRef, Request, 'Etcd.WatchRequest', IsFin),
    await_unwatch_resp(WatchConn, Timeout, RespAcc, Acc).

await_unwatch_resp(#{http2_pid := Gun,
                     monitor_ref := MRef,
                     stream_ref := StreamRef,
                     watch_ids := WatchIds,
                     unprocessed := Unprocessed} = WatchConn, Timeout, RespAcc, Acc) ->
    case eetcd_stream:await(Gun, StreamRef, Timeout, MRef) of
        {data, nofin, Data} ->
            Bin = <<Unprocessed/binary, Data/binary>>,
            case eetcd_grpc:decode(identity, Bin, 'Etcd.WatchResponse') of
                {ok, Resp, NewUnprocessed} ->
                    case Resp of
                        #{created := false, watch_id := WatchId, canceled := true} ->
                            unwatch_and_await_resp(WatchConn#{unprocessed => NewUnprocessed,
                                                          watch_ids => maps:without([WatchId], WatchIds)
                                                         }, Timeout, [Resp|RespAcc], Acc);
                        OtherEvent ->
                            await_unwatch_resp(WatchConn#{unprocessed => NewUnprocessed}, Timeout, RespAcc, [OtherEvent | Acc])
                    end;
                more ->
                    await_unwatch_resp(WatchConn#{unprocessed => Bin}, Timeout, RespAcc, Acc)
            end;
        {error, Reason} -> {error, Reason, lists:reverse(RespAcc), lists:reverse(Acc)}
    end.
