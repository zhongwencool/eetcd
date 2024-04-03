-module(eetcd_watcher_example).

-behaviour(eetcd_watcher).

-include("eetcd.hrl").

-export([ start_link/2, watch/1, unwatch/0, stop/0 ]).

-export([ handle_watch_events/1, handle_unwatch/2 ]).

-type options() :: eetcd_watcher:options().

-type response() :: eetcd_watcher:response().

-type event() :: eetcd_watcher:event().

-type watch_req() :: eetcd_watcher:watch_req().

-spec watch(watch_req()) -> ok.
watch(WatchReq) ->
    eetcd_watcher:watch(?MODULE, WatchReq).

-spec unwatch() -> ok.
unwatch() ->
    eetcd_watcher:unwatch(?MODULE).

-spec start_link(name(), options()) -> ok.
start_link(Client, Options) ->
    eetcd_watcher:start_link(?MODULE, Client, Options).

-spec stop() -> ok.
stop() ->
    eetcd_watcher:stop(?MODULE).

-spec handle_watch_events([event()]) -> ok.
handle_watch_events(Events) ->
    ct:pal("Receive events: ~p", [Events]),
    eetcd_watcher_SUITE ! Events,
    ok.

-spec handle_unwatch([response()], [event()]) -> ok.
handle_unwatch(Responses, Events) ->
    ct:pal("Unwatch Responses:"),
    ct:pal("Responses: ~p", [Responses]),
    ct:pal("Events: ~p", [Events]),
    eetcd_watcher_SUITE ! {Responses, Events},
    ok.
