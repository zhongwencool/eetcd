-module(eetcd_watcher_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([watch_one_key/1, watch_multi_keys/1, watch_with_start_revision/1,
         watch_with_filter_put/1, watch_with_filter_delete/1,
         watch_keys_with_reused_stream/1,
         watch_with_client_disconnected/1]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [watch_one_key, watch_multi_keys, watch_with_start_revision,
     watch_with_filter_put, watch_with_filter_delete,
     watch_keys_with_reused_stream,
     watch_with_client_disconnected
    ].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

end_per_suite(_Config) ->
    %% eetcd:close(?Name),
    application:stop(eetcd).

watch_one_key(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),
    Key = <<"etcd_key">>,
    eetcd_watcher:watch(Watcher, Key),
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value2)),
    [[Event1], [Event2]] = receive_events(2, 50),
    ?assertMatch(#{kv := #{key := <<"etcd_key">>,
                           value := <<"etcd_value1">>},
                   type := 'PUT'}, Event1),
    ?assertMatch(#{kv := #{key := <<"etcd_key">>,
                           value := <<"etcd_value2">>},
                   type := 'PUT'}, Event2),
    eetcd_watcher:stop(Watcher).

watch_multi_keys(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),
    Key1 = <<"etcd_key1">>,
    Key2 = <<"etcd_key2">>,
    eetcd_watcher:watch(Watcher, Key1),
    eetcd_watcher:watch(Watcher, Key2),
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key1), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key2), Value2)),
    [[Event1], [Event2]] = receive_events(2, 50),
    ?assertMatch(#{kv := #{key := <<"etcd_key1">>,
                           value := <<"etcd_value1">>},
                   type := 'PUT'}, Event1),
    ?assertMatch(#{kv := #{key := <<"etcd_key2">>,
                           value := <<"etcd_value2">>},
                   type := 'PUT'}, Event2),
    eetcd_watcher:stop(Watcher).

watch_with_start_revision(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),
    Key = <<"etcd_key">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    eetcd_watcher:watch(Watcher, Key),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value2)),
    [[Event1], [Event2]] = receive_events(2, 50),
    #{kv := #{create_revision := CreateRevision1,
              mod_revision := ModRevision1,
              version := Version1,
              key := <<"etcd_key">>,
              value := EtcdValue1}} = Event1,
    #{kv := #{create_revision := CreateRevision2,
              mod_revision := ModRevision2,
              version := Version2,
              key := <<"etcd_key">>,
              value := EtcdValue2}} = Event2,
    ?assertEqual(CreateRevision1, CreateRevision2),
    ?assertEqual(1, Version2 - Version1),
    ?assertEqual(1, ModRevision2 - ModRevision1),
    ?assertEqual(<<"etcd_value1">>, EtcdValue1),
    ?assertEqual(<<"etcd_value2">>, EtcdValue2),
    eetcd_watcher:stop(Watcher).

watch_with_filter_put(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),
    Key = <<"etcd_key">>, Value = <<"etcd_value">>,
    eetcd_watcher:watch(Watcher, Key, #{filter_put => true}),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key), Value)),
    eetcd_kv:delete(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),
    [[Event]] = receive_events(2, 50),
    ?assertMatch(#{kv := #{key := <<"etcd_key">>},
                   type := 'DELETE'}, Event),
    eetcd_watcher:stop(Watcher).

watch_with_filter_delete(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),
    Key = <<"etcd_key">>, Value = <<"etcd_value">>,
    eetcd_watcher:watch(Watcher, Key, #{filter_delete => true}),
    eetcd_kv:put(
      eetcd_kv:with_value(
        eetcd_kv:with_key(
          eetcd_kv:new(?Name), Key), Value)),
    eetcd_kv:delete(eetcd_kv:with_key(eetcd_kv:new(?Name), Key)),
    [[Event]] = receive_events(2, 50),
    ?assertMatch(#{kv := #{key := <<"etcd_key">>, value := <<"etcd_value">>},
                   type := 'PUT'}, Event),
    eetcd_watcher:stop(Watcher).

watch_keys_with_reused_stream(_Config) ->
    {ok, Watcher} = eetcd_watcher:start(?Name),

    Key1 = <<"etcd_key1">>,
    Key2 = <<"etcd_key2">>,
    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,

    eetcd_watcher:watch(Watcher, Key1),
    eetcd_watcher:watch(Watcher, Key2, #{reuse_stream => true}),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key1), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key2), Value2)),

    [[Event1], [Event2]] = receive_events(2, 50),
    ?assertMatch(#{kv := #{key := <<"etcd_key1">>,
                           value := <<"etcd_value1">>},
                   type := 'PUT'}, Event1),
    ?assertMatch(#{kv := #{key := <<"etcd_key2">>,
                           value := <<"etcd_value2">>},
                   type := 'PUT'}, Event2),

    State = sys:get_state(Watcher),
    Conns = maps:get(conns, State),
    Watching = maps:get(watching, State),

    [{_Ref, WatchingItems}] = maps:to_list(Watching),
    [{_ConnPid, Streams}] = maps:to_list(Conns),
    #{0 := #{key := <<"etcd_key1">>, receiver := Pid1},
      1 := #{key := <<"etcd_key2">>, receiver := Pid2}} = WatchingItems,
    [{_StreamRef, WatchConnInfo}] = maps:to_list(Streams),

    WatchIds = maps:get(watch_ids, WatchConnInfo),

    ?assertMatch(#{0 := _, 1 := _}, WatchIds),
    ?assertEqual(Pid1, Pid2),
    ?assertEqual(Pid1, self()),

    eetcd_watcher:stop(Watcher).

watch_with_client_disconnected(_Config) ->
    {ok, _Pid} = eetcd:open(helper, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    {ok, Watcher} = eetcd_watcher:start(helper, 50),
    Key1 = <<"etcd_key1">>,
    Key2 = <<"etcd_key2">>,
    Key3 = <<"etcd_key3">>,

    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Value3 = <<"etcd_value3">>,

    eetcd_watcher:watch(Watcher, Key1),
    eetcd_watcher:watch(Watcher, Key2, #{reuse_stream => true}),
    eetcd_watcher:watch(Watcher, Key3),

    ok = eetcd:close(helper),

    {ok, _} = eetcd:open(helper, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),

    timer:sleep(100),

    ?assertEqual([{retried_watch,<<"etcd_key1">>},
                  {retried_watch,<<"etcd_key2">>},
                  {retried_watch,<<"etcd_key3">>}],
                 receive_events(3, 50)),

    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key1), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key2), Value2)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key3), Value3)),


    [[Event1], [Event2], [Event3]] = receive_events(3, 50),

    ?assertMatch(#{kv := #{key := <<"etcd_key1">>,
                           value := <<"etcd_value1">>},
                   type := 'PUT'}, Event1),

    ?assertMatch(#{kv := #{key := <<"etcd_key2">>,
                           value := <<"etcd_value2">>},
                   type := 'PUT'}, Event2),

    ?assertMatch(#{kv := #{key := <<"etcd_key3">>,
                           value := <<"etcd_value3">>},
                   type := 'PUT'}, Event3),

    eetcd_watcher:stop(Watcher).

receive_events(Times, Timeout) ->
    receive_events(Times, Timeout, []).

receive_events(0, _Timeout, Acc) ->
    lists:reverse(Acc);
receive_events(Times, Timeout, Acc) ->
    receive Msg ->
            receive_events(Times - 1, Timeout, [Msg | Acc])
    after Timeout ->
            lists:reverse(Acc)
    end.
