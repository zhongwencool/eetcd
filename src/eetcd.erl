-module(eetcd).

%% API
-export([watch/2, watch/3, unwatch/1, get_watch_id/1]).
-export([lease_keep_alive/1]).
-export([member_list/0]).

-include("eetcd.hrl").

-type 'Grpc.Status'() ::
?GRPC_STATUS_OK | ?GRPC_STATUS_CANCELLED |
?GRPC_STATUS_UNKNOWN | ?GRPC_STATUS_INVALID_ARGUMENT |
?GRPC_STATUS_DEADLINE_EXCEEDED | ?GRPC_STATUS_NOT_FOUND |
?GRPC_STATUS_ALREADY_EXISTS | ?GRPC_STATUS_PERMISSION_DENIED |
?GRPC_STATUS_RESOURCE_EXHAUSTED | ?GRPC_STATUS_RESOURCE_EXHAUSTED |
?GRPC_STATUS_FAILED_PRECONDITION | ?GRPC_STATUS_ABORTED |
?GRPC_STATUS_OUT_OF_RANGE | ?GRPC_STATUS_UNIMPLEMENTED |
?GRPC_STATUS_INTERNAL | ?GRPC_STATUS_UNAVAILABLE |
?GRPC_STATUS_DATA_LOSS | ?GRPC_STATUS_UNAUTHENTICATED.

-export_type(['Grpc.Status'/0]).

%% @doc Watch watches for events happening or that have happened. Both input and output are streams;
%% the input stream is for creating watchers and the output stream sends events.
%% One watch RPC can watch on multiple key ranges, streaming events for several watches at once.
%% The entire event history can be watched starting from the last compaction revision.
%% {ignore_create, true} make callback function ignore create watch event.
%% {ignore_cancel, true} make callback function ignore cancel watch event.
-spec watch(#'Etcd.WatchCreateRequest'{}, Callback) -> {ok, pid()} when
    Callback :: fun((#'Etcd.WatchResponse'{}) -> term()).
watch(CreateReq, Callback) ->
    watch(CreateReq, Callback, [{ignore_create, true}, {ignore_cancel, true}]).

-spec watch(#'Etcd.WatchCreateRequest'{}, Callback, Options) -> {ok, pid()} when
    Callback :: fun((#'Etcd.WatchResponse'{}) -> term()),
    Options :: [{ignore_create | ignore_cancel, boolean()}].
watch(CreateReq, Callback, Options) when
    is_record(CreateReq, 'Etcd.WatchCreateRequest'),
    is_function(Callback, 1) ->
    Request = #'Etcd.WatchRequest'{request_union = {create_request, CreateReq}},
    eetcd_watch_sup:watch(Request, Callback, Options).

%% @doc Cancel watches. No further events will be sent to the canceled watcher.
-spec unwatch(pid()) -> ok.
unwatch(Pid) when is_pid(Pid) ->
    eetcd_watch_sup:unwatch(Pid).

%% @doc get watch id for other watchers
%% If watch_id is provided and non-zero, it will be assigned to this watcher.
%% Since creating a watcher in etcd is not a synchronous operation,
%% this can be used ensure that ordering is correct when creating multiple watchers on the same stream.
%% Creating a watcher with an ID already in use on the stream will cause an error to be returned.
-spec get_watch_id(pid()) -> non_neg_integer().
get_watch_id(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, watch_id).

%% @doc Keeps the lease alive by streaming keep alive requests from the client to the server and
%% streaming keep alive responses from the server to the client.
-spec lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{}|integer()) -> ok.
lease_keep_alive(Id) when is_integer(Id) ->
    lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{'ID' = Id});
lease_keep_alive(Request) when is_record(Request, 'Etcd.LeaseKeepAliveRequest') ->
    eetcd_lease_server:keep_alive(Request).

%% @doc members is a list of all members associated with the cluster.
-spec member_list() -> {ok, #'Etcd.MemberListResponse'{}} | {error, term()}.
member_list() ->
    eetcd_cluster:member_list(#'Etcd.MemberListRequest'{}).
