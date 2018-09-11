-module(eetcd).

%% API
-export([watch/2, unwatch/1]).
-export([lease_keep_alive/1]).

-include("router_pb.hrl").

%% @doc Watch watches for events happening or that have happened. Both input and output are streams;
%% the input stream is for creating watchers and the output stream sends events.
%% One watch RPC can watch on multiple key ranges, streaming events for several watches at once.
%% The entire event history can be watched starting from the last compaction revision.
-spec watch(router_pb:'Etcd.WatchCreateRequest'(), Callback) -> {ok, pid()} when
    Callback :: fun((router_pb:'Etcd.WatchResponse'()) -> term()).
watch(CreateReq, Callback) when
    is_record(CreateReq, 'Etcd.WatchCreateRequest'),
    is_function(Callback, 1) ->
    Request = #'Etcd.WatchRequest'{request_union = {create_request, CreateReq}},
    eetcd_watch_sup:watch(Request, Callback).

%% @doc Cancel watches. No further events will be sent to the canceled watcher.
-spec unwatch(pid()) -> ok.
unwatch(Pid) when is_pid(Pid) ->
    eetcd_watch_sup:unwatch(Pid).

%% @doc Keeps the lease alive by streaming keep alive requests from the client to the server and
%% streaming keep alive responses from the server to the client.
-spec lease_keep_alive(router_pb:'Etcd.LeaseKeepAliveRequest'()|integer()) -> ok.
lease_keep_alive(Id) when is_integer(Id) ->
    lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{'ID' = Id});
lease_keep_alive(Request) when is_record(Request, 'Etcd.LeaseKeepAliveRequest') ->
    eetcd_lease_server:keep_alive(Request).
