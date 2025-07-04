-module(eetcd_watch_SUITE).
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([watch_one_key/1, watch_multi_keys/1,
    watch_keys_with_single_stream/1,
    watch_with_start_revision/1, watch_with_filters/1,
    watch_with_prev_kv/1, watch_with_watch_id/1,
    watch_with_huge_value/1]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        watch_one_key, watch_multi_keys, watch_with_start_revision, watch_with_filters,
        watch_keys_with_single_stream,
        watch_with_prev_kv, watch_with_watch_id, watch_with_huge_value
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

%% watch and unwatch one key
watch_one_key(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Timeout = 3000,
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, eetcd_watch:with_key(eetcd_watch:new(), Key), Timeout),
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    Message = flush(),
    {ok, Conn0, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message),

    eetcd_kv_gen:put(?Name, #{key => Key, value => Value1}),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value1}}]}}
        = eetcd_watch:watch_stream(Conn0, Message1),

    eetcd_kv_gen:delete_range(?Name, #{key => Key}),
    Message2 = flush(),
    {ok, Conn2, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn1, Message2),

    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn2, Timeout),
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value2}),
    {error, timeout} = flush(),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key}),

    ok.

%% watch multiple keys with one single stream and unwatch them
watch_keys_with_single_stream(_Config) ->
    Key1 = <<"etcd_key_1">>,
    Value1 = <<"etcd_value">>,
    Value1_1 = <<"etcd_value1_1">>,
    Value1_2 = <<"etcd_value1_2">>,

    Key2 = <<"etcd_key_2">>,
    Value2 = <<"etcd_value2">>,

    Timeout = 3000,
    {ok, WatchConn1, _WatchId1} = eetcd_watch:watch(?Name, eetcd_watch:with_key(eetcd_watch:new(), Key1), Timeout),
    %% Watch another key
    Req2 = eetcd_watch:with_key(eetcd_watch:new(), Key2),
    {ok, WatchConn, _WatchId2} = eetcd_watch:watch(?Name, Req2, WatchConn1, Timeout),

    %% Put key1
    eetcd_kv_gen:put(?Name, #{key => Key1, value => Value1}),
    Message = flush(),
    {ok, Conn0, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key1, value := Value1}}],
        watch_id := 0}}
        = eetcd_watch:watch_stream(WatchConn, Message),

    %% Change key1
    eetcd_kv_gen:put(?Name, #{key => Key1, value => Value1_1}),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key1, value := Value1_1}}],
        watch_id := 0}}
        = eetcd_watch:watch_stream(Conn0, Message1),

    %% Change key2
    eetcd_kv_gen:put(?Name, #{key => Key2, value => Value2}),
    Message2_1 = flush(),
    {ok, Conn2, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key2, value := Value2}}],
        watch_id := 1}}
        = eetcd_watch:watch_stream(Conn1, Message2_1),

    %% Delete key2
    eetcd_kv_gen:delete_range(?Name, #{key => Key2}),
    Message2 = flush(),
    {ok, Conn3, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key2, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn2, Message2),

    %% Delete key1
    eetcd_kv_gen:delete_range(?Name, #{key => Key1}),
    Message1_2 = flush(),
    {ok, Conn4, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key1, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn3, Message1_2),

    %% Unwatch all watch ids in this stream
    {ok, Resps, []} = eetcd_watch:unwatch(Conn4, Timeout),
    UnWatchedIds = [ UnWatchedId || #{created := false,
                                      canceled := true,
                                      events := [],
                                      watch_id := UnWatchedId } <- Resps],
    ?assertEqual([0, 1], lists:sort(UnWatchedIds)),

    eetcd_kv_gen:put(?Name, #{key => Key1, value => Value1_2}),
    {error, timeout} = flush(),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key1}),

    ok.

watch_multi_keys(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Timeout = 3000,
    WatchReq = eetcd_watch:with_range_end(eetcd_watch:with_key(eetcd_watch:new(), Key), "\0"),
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, WatchReq, Timeout),

    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),

    Key1 = <<Key/binary, "1">>,
    eetcd_kv_gen:put(?Name, #{key => Key1, value => Value1}),
    Message2 = flush(),
    {ok, Conn2, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key1, value := Value1}}]}}
        = eetcd_watch:watch_stream(Conn1, Message2),

    Key2 = <<"1", Key/binary>>,
    eetcd_kv_gen:put(?Name,  #{key => Key2, value => Value2}),
    {error, timeout} = flush(),

    eetcd_kv_gen:delete_range(?Name, #{key => Key1}),
    Message3 = flush(),
    {ok, Conn3, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key1, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn2, Message3),

    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn3, Timeout),
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value2}),
    {error, timeout} = flush(),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key}),
    eetcd_kv_gen:delete_range(?Name, #{key => Key2}),

    ok.

%% start_revision is an optional revision to watch from (inclusive). No start_revision is "now".
watch_with_start_revision(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    {ok, #{kvs := [#{mod_revision := Revision}]}} = eetcd_kv_gen:range(?Name, #{key => Key}),

    WatchReq = eetcd_watch:with_start_revision(eetcd_watch:with_key(eetcd_watch:new(), Key), Revision),
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, WatchReq, Timeout),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn1, Timeout),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key}),

    ok.

%% progress_notify is set so that the etcd server will periodically send a WatchResponse with no events
%% to the new watcher if there are no recent events.
%% It is useful when clients wish to recover a disconnected watcher starting from a recent known revision.
%% The etcd server may decide how often it will send notifications based on current load.
%% lead to very hard to test.
%%watch_with_progress_notify(_Config) ->
%%    Key = <<"etcd_key">>,
%%    Value = <<"etcd_value">>,
%%    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),

%%    Pid = self(),
%%    Callback = fun(Res) -> erlang:send(Pid, Res) end,
%%    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, progress_notify = true}, Callback),
%%    #'Etcd.WatchResponse'{created = false,
%%        events = [#'mvccpb.Event'{type = 'PUT',
%%            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(100000),
%%    ok = eetcd:unwatch(WatchPid),
%%    ok.

%% filters filter the events at server side before it sends back to the watcher.
watch_with_filters(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),

    WatchReq = eetcd_watch:with_filter_put(eetcd_watch:with_key(eetcd_watch:new(), Key)),
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, WatchReq, Timeout),

    eetcd_kv_gen:delete_range(?Name, #{key => Key}),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn1, Timeout),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key}),

    ok.

watch_with_prev_kv(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    timer:sleep(200),
    WatchReq = eetcd_watch:with_prev_kv(eetcd_watch:with_key(eetcd_watch:new(), Key)),
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, WatchReq, Timeout),

    eetcd_kv_gen:delete_range(?Name, #{key => Key}),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>},
            prev_kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn1, Timeout),
    ok.

watch_with_watch_id(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    timer:sleep(200),
    WatchReq1 = eetcd_watch:with_key(eetcd_watch:new(), Key),
    {ok, WatchConn1, _WatchId} = eetcd_watch:watch(?Name, WatchReq1, Timeout),

    #{watch_ids := WatchIds} = WatchConn1,
    [WatchId] = maps:keys(WatchIds),
    WatchReq2 = eetcd_watch:with_watch_id(eetcd_watch:with_key(eetcd_watch:new(), Key), WatchId),
    {ok, WatchConn2, WatchId2} = eetcd_watch:watch(?Name, WatchReq2, Timeout),
    ?assertEqual(WatchId, WatchId2),

    eetcd_kv_gen:delete_range(?Name, #{key => Key}),
    Message1 = flush(),
    Message2 = flush(),
    case eetcd_watch:watch_stream(WatchConn1, Message1) of
        unknown ->
            {ok, Conn1, #{created := false,
                events := [#{type := 'DELETE',
                    kv := #{key := Key, value := <<>>}}]}}
                = eetcd_watch:watch_stream(WatchConn1, Message2),
            {ok, Conn2, #{created := false,
                events := [#{type := 'DELETE',
                    kv := #{key := Key, value := <<>>}}]}}
                = eetcd_watch:watch_stream(WatchConn2, Message1),
            {ok, [#{created := false, canceled := true,
                events := []}], []} = eetcd_watch:unwatch(Conn1, Timeout),
            {ok, [#{created := false, canceled := true,
                events := []}], []} = eetcd_watch:unwatch(Conn2, Timeout);
        {ok, Conn1, #{created := false,
            events := [#{type := 'DELETE',
                kv := #{key := Key, value := <<>>}}]}} ->
            {ok, Conn2, #{created := false,
                events := [#{type := 'DELETE',
                    kv := #{key := Key, value := <<>>}}]}}
                = eetcd_watch:watch_stream(WatchConn2, Message2),
            {ok, [#{created := false, canceled := true,
                events := []}], []} = eetcd_watch:unwatch(Conn1, Timeout),
            {ok, [#{created := false, canceled := true,
                events := []}], []} = eetcd_watch:unwatch(Conn2, Timeout)
    end,
    ok.

watch_with_huge_value(_Config) ->
    Key = <<"etcd_huge_key">>,
    {ok, WatchConn, _WatchId} = eetcd_watch:watch(?Name, eetcd_watch:with_key(eetcd_watch:new(), Key)),
    List = [233333, 1, 13, 99, 122, 1222, 40000, 12345, 67890, 999999, 3, 4, 5, 33, 57, 157, 999, 99999, 2],
    {ok, Conn} = watch_loop(List, WatchConn, Key),
    {ok, [#{created := false, canceled := true,
        events := []}], []} = eetcd_watch:unwatch(Conn, 5000),

    %% Clear test keys
    eetcd_kv_gen:delete_range(?Name, #{key => Key}),

    ok.

watch_loop([], Conn, _) -> {ok, Conn};
watch_loop([Head | Tail], Conn, Key) ->
    Value = list_to_binary([100 || _ <- lists:seq(1, Head)]),
    eetcd_kv_gen:put(?Name, #{key => Key, value => Value}),
    Message = flush(),
    case eetcd_watch:watch_stream(Conn, Message) of
        {ok, Conn1, #{created := false,
            events := [#{type := 'PUT',
                kv := #{key := Key, value := Value}}]}} ->
            watch_loop(Tail, Conn1, Key);
        {more, Conn1} ->
            {ok, Conn2} = receive_fragment(Conn1, Key, Value),
            watch_loop(Tail, Conn2, Key)
    end.

receive_fragment(Conn, Key, Value) ->
    Message = flush(),
    case eetcd_watch:watch_stream(Conn, Message) of
        {ok, Conn1, #{created := false,
            events := [#{type := 'PUT',
                kv := #{key := Key, value := Value}}]}} ->
            {ok, Conn1};
        {more, Conn1} ->
            receive_fragment(Conn1, Key, Value)
    end.

%% fragment enables splitting large revisions into multiple watch responses.
%%watch_with_fragment(_Config) ->
%%    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flush() -> flush(1000).

flush(Time) ->
    receive Msg -> Msg
    after Time -> {error, timeout}
    end.
