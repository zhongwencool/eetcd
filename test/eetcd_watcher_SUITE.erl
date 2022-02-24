-module(eetcd_watcher_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([watch/1,
         watch_with_client_disconnected/1]).

-define(Name, ?MODULE).

-define(Endpoints, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [watch, watch_with_client_disconnected].

init_per_suite(Config) ->
    application:set_env(eetcd, retry_watch_ms, 50),
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ?Endpoints),
    Config.

end_per_suite(_Config) ->
    application:stop(eetcd).

watch(_Config) ->
    WatchReq0 = eetcd_watch:new(),
    WatchEtcdKey = <<"etcd_key">>,
    EtcdKey1 = <<"etcd_key1">>,
    WatchReq1 = eetcd_watch:with_key(WatchReq0, WatchEtcdKey),
    WatchReq2 = eetcd_watch:with_prefix(WatchReq1),
    eetcd_watcher:watch(?Name, WatchReq2),
    Value1 = <<"etcd_value1">>,
    eetcd_kv:put(
      eetcd_kv:with_value(
        eetcd_kv:with_key(
          eetcd_kv:new(?Name), EtcdKey1), Value1)),
    eetcd_kv:delete(
      eetcd_kv:with_key(
        eetcd_kv:new(?Name), EtcdKey1)),

    ?assertMatch([{ok, [#{kv := #{key := <<"etcd_key1">>,
                                  value := <<"etcd_value1">>}, type := 'PUT'}]},
                  {ok, [#{kv := #{key := <<"etcd_key1">>}, type := 'DELETE'}]}],
                 receive_events(2, 3000)),

    eetcd_watcher:unwatch(),

    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), EtcdKey1), Value1)),

    ?assertEqual([], receive_events(1, 3000)).

watch_with_client_disconnected(_Config) ->

    {ok, _Pid} = eetcd:open(helper, ?Endpoints),
    Key1 = <<"etcd_key1">>,
    Key2 = <<"etcd_key2">>,
    Key3 = <<"etcd_key3">>,

    Watch1 = eetcd_watch:with_key(eetcd_watch:new(), Key1),
    Watch2 = eetcd_watch:with_key(eetcd_watch:new(), Key2),
    Watch3 = eetcd_watch:with_key(eetcd_watch:new(), Key3),

    Value1 = <<"etcd_value1">>,
    Value2 = <<"etcd_value2">>,
    Value3 = <<"etcd_value3">>,

    eetcd_watcher:watch(helper, Watch1),
    eetcd_watcher:watch(helper, Watch2),
    eetcd_watcher:watch(helper, Watch3),
    %% Simulate the situation that connection down
    ok = eetcd:close(helper),

    {ok, _} = eetcd:open(helper, ?Endpoints),

    timer:sleep(90),
    ?assertEqual([{error, connection_down},
                  {error, connection_down},
                  {error, connection_down}],
                 receive_events(3, 50)),

    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key1), Value1)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key2), Value2)),
    eetcd_kv:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(?Name), Key3), Value3)),

    ?assertMatch([{ok, [#{kv := #{key := <<"etcd_key1">>,
                                  value := <<"etcd_value1">>}}]},
                  {ok, [#{kv := #{key := <<"etcd_key2">>,
                                  value := <<"etcd_value2">>}}]},
                  {ok, [#{kv := #{key := <<"etcd_key3">>,
                                  value := <<"etcd_value3">>}}]}
                 ], receive_events(3, 50)),

    eetcd_watcher:unwatch(),
    ok = eetcd:close(helper).

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
