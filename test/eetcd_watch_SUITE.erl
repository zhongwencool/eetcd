
-module(eetcd_watch_SUITE).


-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([watch_one_key/1, watch_multi_keys/1,
    watch_with_start_revision/1, watch_with_filters/1, watch_with_create_cancel_event/1,
    watch_with_prev_kv/1, watch_with_watch_id/1, watch_with_huge_value/1]).

-include("router_pb.hrl").

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        watch_one_key, watch_multi_keys, watch_with_start_revision, watch_with_filters,
        watch_with_prev_kv, watch_with_watch_id, watch_with_create_cancel_event, watch_with_huge_value
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    Config.

end_per_suite(_Config) ->
    application:stop(eetcd),
    ok.

%% watch and unwatch one key
watch_one_key(_Config) ->
    Pid = self(),
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Callback),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value1}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value1}}]} = flush(),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]} = flush(),
    
    ok = eetcd:unwatch(WatchPid),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush(),
    
    ok.

watch_multi_keys(_Config) ->
    Pid = self(),
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, range_end = "\0"}, Callback),
    
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    
    Key1 = <<Key/binary, "1">>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key1, value = Value1}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key1, value = Value1}}]} = flush(),
    
    Key2 = <<"1", Key/binary>>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key2, value = Value2}),
    {error, timeout} = flush(),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key1}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key1, value = <<>>}}]} = flush(),
    
    ok = eetcd:unwatch(WatchPid),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush(),
    
    ok.

%% start_revision is an optional revision to watch from (inclusive). No start_revision is "now".
watch_with_start_revision(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    {ok, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{mod_revision = Revision}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Key}),
    
    Pid = self(),
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, start_revision = Revision}, Callback),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    ok = eetcd:unwatch(WatchPid),
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
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    Pid = self(),
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, filters = ['NOPUT']}, Callback),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]} = flush(),
    ok = eetcd:unwatch(WatchPid),
    ok.

watch_with_prev_kv(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    Pid = self(),
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, prev_kv = true}, Callback),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>},
            prev_kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    ok = eetcd:unwatch(WatchPid),
    ok.

watch_with_create_cancel_event(_Config) ->
    Pid = self(),
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value2 = <<"etcd_value2">>,
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} =
        eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Callback,
            [{ignore_create, false}, {ignore_cancel, false}]),
    #'Etcd.WatchResponse'{created = true, events = []} = flush(),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    
    ok = eetcd:unwatch(WatchPid),
    #'Etcd.WatchResponse'{canceled = true, events = []} = flush(),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush(),
    ok.

watch_with_watch_id(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    Pid = self(),
    Callback1 = fun(Res) -> erlang:send(Pid, {1, Res}) end,
    {ok, WatchPid1} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Callback1),
    
    WatchId = eetcd:get_watch_id(WatchPid1),
    Callback2 = fun(Res) -> erlang:send(Pid, {2, Res}) end,
    {ok, WatchPid2} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, watch_id = WatchId}, Callback2),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    {X1, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}} = flush(),
    {X2, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}} = flush(),
    [1, 2] = lists:usort([X1, X2]),
    ok = eetcd:unwatch(WatchPid1),
    ok = eetcd:unwatch(WatchPid2),
    ok.

%% watch large value data
watch_with_huge_value(_Config) ->
    Key = <<"etcd_key">>,
    Pid = self(),
    Callback = fun(Res) -> erlang:send(Pid, Res) end,
    {ok, WatchPid} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Callback),
    watch_loop([233333, 1, 13, 99, 122, 1222, 40000, 12345, 67890, 999999, 2, 3, 4, 5, 33, 57, 157, 999, 99999], Key),

    ok = eetcd:unwatch(WatchPid),
    ok.

watch_loop([], _) ->
    ok;
watch_loop([Head | Tail], Key) ->
    Value = list_to_binary([100 || _ <- lists:seq(1, Head)]),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    #'Etcd.WatchResponse'{events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]} = flush(),
    watch_loop(Tail, Key).

%% fragment enables splitting large revisions into multiple watch responses.
%%watch_with_fragment(_Config) ->
%%    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flush() -> flush(1000).

flush(Time) ->
    receive Msg  -> Msg
    after Time -> {error, timeout}
    end.