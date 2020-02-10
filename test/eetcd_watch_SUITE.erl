-module(eetcd_watch_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([watch_one_key/1, watch_multi_keys/1,
    watch_with_start_revision/1, watch_with_filters/1,
    watch_with_prev_kv/1, watch_with_watch_id/1,
    watch_with_huge_value/1]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        watch_one_key, watch_multi_keys, watch_with_start_revision, watch_with_filters,
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
    {ok, WatchConn} = eetcd_watch:watch(?Name, eetcd_watch:with_key(eetcd_watch:new(), Key), Timeout),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value)),
    Message = flush(),
    {ok, Conn0, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message),
    
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value1)),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value1}}]}}
        = eetcd_watch:watch_stream(Conn0, Message1),
    
    eetcd_kv:delete(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),
    Message2 = flush(),
    {ok, Conn2, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn1, Message2),
    
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn2, Timeout),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value2)),
    {error, timeout} = flush(),
    
    ok.

watch_multi_keys(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Timeout = 3000,
    WatchReq = eetcd_watch:with_range_end(eetcd_watch:with_key(eetcd_watch:new(), Key), "\0"),
    {ok, WatchConn} = eetcd_watch:watch(?Name, WatchReq, Timeout),
    
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value)),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    
    Key1 = <<Key/binary, "1">>,
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key1), Value1)),
    Message2 = flush(),
    {ok, Conn2, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key1, value := Value1}}]}}
        = eetcd_watch:watch_stream(Conn1, Message2),
    
    Key2 = <<"1", Key/binary>>,
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key2), Value2)),
    {error, timeout} = flush(),
    
    eetcd_kv:delete(?Name, Key1),
    Message3 = flush(),
    {ok, Conn3, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key1, value := <<>>}}]}}
        = eetcd_watch:watch_stream(Conn2, Message3),
    
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn3, Timeout),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value2)),
    {error, timeout} = flush(),
    
    ok.

%% start_revision is an optional revision to watch from (inclusive). No start_revision is "now".
watch_with_start_revision(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    Ctx = eetcd_kv:new(?Name),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(Ctx, Key), Value)),
    {ok, #{kvs := [#{mod_revision := Revision}]}} = eetcd_kv:get(eetcd_kv:with_key(Ctx, Key)),
    
    WatchReq = eetcd_watch:with_start_revision(eetcd_watch:with_key(eetcd_watch:new(), Key), Revision),
    {ok, WatchConn} = eetcd_watch:watch(?Name, WatchReq, Timeout),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'PUT',
            kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn1, Timeout),
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
    Ctx = eetcd_kv:new(?Name),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(Ctx, Key), Value)),
    
    WatchReq = eetcd_watch:with_filter_put(eetcd_watch:with_key(eetcd_watch:new(), Key)),
    {ok, WatchConn} = eetcd_watch:watch(?Name, WatchReq, Timeout),
    
    eetcd_kv:delete(eetcd_kv:with_key(Ctx, Key)),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn1, Timeout),
    ok.

watch_with_prev_kv(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    Ctx = eetcd_kv:new(?Name),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(Ctx, Key), Value)),
    
    WatchReq = eetcd_watch:with_prev_kv(eetcd_watch:with_key(eetcd_watch:new(), Key)),
    {ok, WatchConn} = eetcd_watch:watch(?Name, WatchReq, Timeout),
    
    eetcd_kv:delete(eetcd_kv:with_key(Ctx, Key)),
    Message1 = flush(),
    {ok, Conn1, #{created := false,
        events := [#{type := 'DELETE',
            kv := #{key := Key, value := <<>>},
            prev_kv := #{key := Key, value := Value}}]}}
        = eetcd_watch:watch_stream(WatchConn, Message1),
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn1, Timeout),
    ok.

watch_with_watch_id(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    Ctx = eetcd_kv:new(?Name),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(Ctx, Key), Value)),
    timer:sleep(200),
    WatchReq1 = eetcd_watch:with_key(eetcd_watch:new(), Key),
    {ok, WatchConn1} = eetcd_watch:watch(?Name, WatchReq1, Timeout),
    
    #{watch_id := WatchId} = WatchConn1,
    WatchReq2 = eetcd_watch:with_watch_id(eetcd_watch:with_key(eetcd_watch:new(), Key), WatchId),
    {ok, WatchConn2} = eetcd_watch:watch(?Name, WatchReq2, Timeout),
    
    eetcd_kv:delete(?Name, Key),
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
            {ok, #{created := false, canceled := true,
                events := []}, []} = eetcd_watch:unwatch(Conn1, Timeout),
            {ok, #{created := false, canceled := true,
                events := []}, []} = eetcd_watch:unwatch(Conn2, Timeout);
        {ok, Conn1, #{created := false,
            events := [#{type := 'DELETE',
                kv := #{key := Key, value := <<>>}}]}} ->
            {ok, Conn2, #{created := false,
                events := [#{type := 'DELETE',
                    kv := #{key := Key, value := <<>>}}]}}
                = eetcd_watch:watch_stream(WatchConn2, Message2),
            {ok, #{created := false, canceled := true,
                events := []}, []} = eetcd_watch:unwatch(Conn1, Timeout),
            {ok, #{created := false, canceled := true,
                events := []}, []} = eetcd_watch:unwatch(Conn2, Timeout)
    end,
    ok.

watch_with_huge_value(_Config) ->
    Key = <<"etcd_huge_key">>,
    {ok, WatchConn} = eetcd_watch:watch(?Name, eetcd_watch:with_key(eetcd_watch:new(), Key)),
    List = [233333, 1, 13, 99, 122, 1222, 40000, 12345, 67890, 999999, 3, 4, 5, 33, 57, 157, 999, 99999, 2],
    {ok, Conn} = watch_loop(List, WatchConn, Key),
    {ok, #{created := false, canceled := true,
        events := []}, []} = eetcd_watch:unwatch(Conn, 5000),
    ok.

watch_loop([], Conn, _) -> {ok, Conn};
watch_loop([Head | Tail], Conn, Key) ->
    Value = list_to_binary([100 || _ <- lists:seq(1, Head)]),
    Ctx = eetcd_kv:new(?Name),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(Ctx, Key), Value)),
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