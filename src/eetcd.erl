-module(eetcd).

%% API
-export([watch/2, watch/3]).
-export([watch_stream/2]).
-export([unwatch/2]).
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

-type 'WatchConn'() :: #{http2_pid => pid(), monitor_ref => reference(), stream_ref => reference(), response => #'Etcd.WatchResponse'{}}.

-export_type(['Grpc.Status'/0, 'WatchConn'/0]).

%% @equiv watch(CreateReq, [], Timeout).
-spec watch(#'Etcd.WatchCreateRequest'{}, Timeout) -> {ok, WatchConn} when
    Timeout :: pos_integer(),
    WatchConn :: 'WatchConn'().
watch(CreateReq, Timeout) -> watch(CreateReq, [], Timeout).

%% @doc Watch watches for events happening or that have happened. Both input and output are streams;
%% the input stream is for creating watchers and the output stream sends events.
%% One watch RPC can watch on multiple key ranges, streaming events for several watches at once.
%% The entire event history can be watched starting from the last compaction revision.
-spec watch(#'Etcd.WatchCreateRequest'{}, Http2Header | Token, Timeout) -> {ok, WatchConn} | {error, term()} when
    Http2Header :: [{binary(), binary()}],
    Token :: binary(),
    Timeout :: pos_integer(),
    WatchConn :: 'WatchConn'().
watch(CreateReq, Http2HeaderOrToken, Timeout) when is_record(CreateReq, 'Etcd.WatchCreateRequest') ->
    Request = #'Etcd.WatchRequest'{request_union = {create_request, CreateReq}},
    MRef = erlang:monitor(process, self()),
    {Pid, StreamRef} = eetcd_watch:watch(Request, Http2HeaderOrToken),
    %% TODO: Requests the a watch stream progress status be sent in the watch response stream as soon as possible.
    %% PRequest = #'Etcd.WatchRequest'{request_union = {progress_request, #'Etcd.WatchProgressRequest'{}}},
    %% eetcd_stream:data(Pid, StreamRef, PRequest, fin),
    case gun:await(Pid, StreamRef, Timeout, MRef) of
        {response, nofin, 200, _Headers} ->
            case gun:await(Pid, StreamRef, Timeout, MRef) of
                {data, nofin, Body} ->
                    Response =
                        #'Etcd.WatchResponse'{created = true, canceled = false}
                        = eetcd_grpc:decode(identity, Body, 'Etcd.WatchResponse'),
                    {ok,
                        #{
                            http2_pid => Pid,
                            monitor_ref => MRef,
                            stream_ref => StreamRef,
                            response => Response
                        }
                    };
                {error, _} = Err1 -> Err1
            end;
        {response, fin, 200, RespHeaders} ->
            {GrpcStatus, GrpcMessage} = eetcd_grpc:grpc_status(RespHeaders),
            {error, ?GRPC_ERROR(GrpcStatus, GrpcMessage)};
        {error, _} = Err2 -> Err2
    end.

%% @doc  Cancel watching so that no more events are transmitted.
%% This is a synchronous operation.
%% Other change events will be returned in OtherEvents when these events arrive between the request and the response.
-spec unwatch('WatchConn'(), Timeout) ->
    {ok, #'Etcd.WatchResponse'{}, OtherEvents} | {error, Reason, OtherEvents} when
    Timeout :: pos_integer(),
    OtherEvents :: [#'Etcd.WatchResponse'{}],
    Reason :: term().
unwatch(WatchConn, Timeout) ->
    #{
        http2_pid := Pid,
        monitor_ref := MRef,
        stream_ref := StreamRef,
        response := #'Etcd.WatchResponse'{watch_id = WatchId}
    } = WatchConn,
    Request = #'Etcd.WatchRequest'{
        request_union = {cancel_request, #'Etcd.WatchCancelRequest'{
            watch_id = WatchId
        }}},
    eetcd_stream:data(Pid, StreamRef, Request, fin),
    await_unwatch_resp(Pid, StreamRef, WatchId, Timeout, MRef, []).

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

%%====================================================================
%% Internal functions
%%====================================================================

await_unwatch_resp(Pid, StreamRef, WatchId, Timeout, MRef, Acc) ->
    case gun:await(Pid, StreamRef, Timeout, MRef) of
        {data, nofin, Data} ->
            Reps = eetcd_grpc:decode(identity, Data, 'Etcd.WatchResponse'),
            case Reps of
                #'Etcd.WatchResponse'{created = false, watch_id = WatchId, canceled = true} ->
                    gun:cancel(Pid, StreamRef),
                    {ok, Reps, lists:reverse(Acc)};
                OtherEvent ->
                    await_unwatch_resp(Pid, StreamRef, WatchId, Timeout, MRef, [OtherEvent | Acc])
            end;
        {error, Reason} -> {error, Reason, lists:reverse(Acc)}
    end.

watch_stream(#{stream_ref := Ref, http2_pid := Pid}, {gun_data, Pid, Ref, nofin, Data}) ->
    {ok, eetcd_grpc:decode(identity, Data, 'Etcd.WatchResponse')};
watch_stream(_Conn, _UnKnow) -> unknown.
