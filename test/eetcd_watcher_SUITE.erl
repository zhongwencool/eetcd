-module(eetcd_watcher_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).

-export([ watch/1, client_down/1 ]).

-define(Name, ?MODULE).

-define(Endpoints, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [watch, client_down].


init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ?Endpoints),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd).

watch(_Config) ->
    %% register the name to receive messages from watcher example
    register(?MODULE, self()),

    Key = <<"etcd_key">>, Value = <<"etcd_value">>,
    {ok, _Pid} = eetcd_watcher_example:start_link(?Name, #{}),
    WatchReq0 = eetcd_watch:new(),
    WatchReq = eetcd_watch:with_key(WatchReq0, Key),
    ok = eetcd_watcher_example:watch(WatchReq),

    eetcd_kv:put(?Name, Key, Value),
    eetcd_kv:delete(?Name, Key),

    ?assertMatch([#{kv := #{key := Key, value := Value},
                    type := 'PUT'},
                  #{kv := #{key := Key}, type := 'DELETE'}
                 ], receive_msg(2)),

    ok = eetcd_watcher_example:unwatch(),

    ?assertMatch([{[#{canceled := true}], []}], receive_msg(1, 200)),

    eetcd_kv:put(?Name, Key, Value),

    ?assertEqual([], receive_msg(1, 200)),

    eetcd_watcher_example:stop(),
    unregister(?MODULE).

client_down(_Config) ->
    register(?MODULE, self()),
    eetcd_watcher_example:start_link(?Name, #{retry_watch_interval_ms => 100,
                                         retry_watch_max => 10}),

    eetcd:open(helper, ?Endpoints),

    Key = <<"client_down">>,
    WatchReq0 = eetcd_watch:new(),
    WatchReq = eetcd_watch:with_key(WatchReq0, Key),
    ok = eetcd_watcher_example:watch(WatchReq),
    eetcd_kv:put(helper, Key, <<"ping1">>),

    eetcd:close(?Name),

    eetcd_kv:put(helper, Key, <<"ping2">>),
    eetcd_kv:delete(helper, Key),

    eetcd:open(?Name, ?Endpoints),
    timer:sleep(100),
    Results = receive_msg(2, 300),
    ct:pal("Results: ~p", [Results]),

    eetcd_watcher_example:stop(),

    unregister(?MODULE).

receive_msg(Count) ->
    receive_msg(Count, 100).

receive_msg(Count, Timeout) ->
    Results = receive_msg(Count, Timeout, []),
    lists:flatten(lists:reverse(Results)).

receive_msg(0, _Timeout, Acc) ->
    Acc;
receive_msg(Count, Timeout, Acc) ->
    receive Msg ->
            receive_msg(Count - 1, Timeout, [Msg | Acc])
    after Timeout ->
            Acc
    end.
