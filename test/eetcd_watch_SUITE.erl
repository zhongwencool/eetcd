-module(eetcd_watch_SUITE).


-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([watch_one_key/1, watch_multi_keys/1,
    watch_with_start_revision/1, watch_with_filters/1,
    watch_with_prev_kv/1, watch_with_watch_id/1]).

-include("router_pb.hrl").

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        watch_one_key, watch_multi_keys, watch_with_start_revision, watch_with_filters,
        watch_with_prev_kv, watch_with_watch_id
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
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Timeout = 3000,
    {ok, WatchConn} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Timeout),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    Message = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]}}
        = eetcd:watch_stream(WatchConn, Message),
    
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value1}),
    Message1 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value1}}]}}
        = eetcd:watch_stream(WatchConn, Message1),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    Message3 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}}
        = eetcd:watch_stream(WatchConn, Message3),
    
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn, Timeout),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush(),
    
    ok.

watch_multi_keys(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Timeout = 3000,
    {ok, WatchConn} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, range_end = "\0"}, Timeout),
    
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    Message1 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]}}
        = eetcd:watch_stream(WatchConn, Message1),
    
    Key1 = <<Key/binary, "1">>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key1, value = Value1}),
    Message2 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key1, value = Value1}}]}}
        = eetcd:watch_stream(WatchConn, Message2),
    
    Key2 = <<"1", Key/binary>>,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key2, value = Value2}),
    {error, timeout} = flush(),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key1}),
    Message3 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key1, value = <<>>}}]}}
        = eetcd:watch_stream(WatchConn, Message3),
    
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn, Timeout),
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value2}),
    {error, timeout} = flush(),
    
    ok.

%% start_revision is an optional revision to watch from (inclusive). No start_revision is "now".
watch_with_start_revision(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    {ok, #'Etcd.RangeResponse'{kvs = [#'mvccpb.KeyValue'{mod_revision = Revision}]}}
        = eetcd_kv:range(#'Etcd.RangeRequest'{key = Key}),
    
    {ok, WatchConn} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, start_revision = Revision}, Timeout),
    Message1 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'PUT',
            kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]}}
        = eetcd:watch_stream(WatchConn, Message1),
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn, Timeout),
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
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    {ok, WatchConn} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, filters = ['NOPUT']}, Timeout),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    Message1 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}}
        = eetcd:watch_stream(WatchConn, Message1),
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn, Timeout),
    ok.

watch_with_prev_kv(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    {ok, WatchConn} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, prev_kv = true}, Timeout),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    Message1 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>},
            prev_kv = #'mvccpb.KeyValue'{key = Key, value = Value}}]}}
        = eetcd:watch_stream(WatchConn, Message1),
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn, Timeout),
    ok.

watch_with_watch_id(_Config) ->
    Key = <<"etcd_key">>,
    Value = <<"etcd_value">>,
    Timeout = 3000,
    eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = Value}),
    
    {ok, WatchConn1} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key}, Timeout),
    
    #{response := #'Etcd.WatchResponse'{watch_id = WatchId}} = WatchConn1,
    {ok, WatchConn2} = eetcd:watch(#'Etcd.WatchCreateRequest'{key = Key, watch_id = WatchId}, Timeout),
    
    eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    Message1 = flush(),
    Message2 = flush(),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}}
        = eetcd:watch_stream(WatchConn1, Message1),
    {ok, #'Etcd.WatchResponse'{created = false,
        events = [#'mvccpb.Event'{type = 'DELETE',
            kv = #'mvccpb.KeyValue'{key = Key, value = <<>>}}]}}
        = eetcd:watch_stream(WatchConn2, Message2),
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn1, Timeout),
    {ok, #'Etcd.WatchResponse'{created = false, canceled = true,
        events = []}, []} = eetcd:unwatch(WatchConn2, Timeout),
    ok.

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