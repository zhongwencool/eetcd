-module(eetcd_stream).

%% API
-export([unary/3, unary/4]).
-export([new/2, new/3]).
-export([data/4]).
-export([await/4]).

-define(TIMEOUT, 5000).
-include("eetcd.hrl").

-spec new(Path, Headers) -> {ok, GunPid, Http2Ref} when
    Path :: iodata(),
    Headers :: [{binary(), binary()}],
    GunPid :: pid(),
    Http2Ref :: reference().
new(Path, Headers) ->
    Pid = eetcd_http2_keeper:get_http2_client_pid(),
    {ok, Pid, gun:request(Pid, <<"POST">>, Path, Headers ++ ?HEADERS, <<>>)}.

-spec new(EtcdRequest, Http2Path, Http2Headers) -> Http2Ref when
    EtcdRequest :: tuple(),
    Http2Path :: iodata(),
    Http2Headers :: [{binary(), binary()}],
    Http2Ref :: reference().
new(Request, Path, Http2Headers) ->
    {ok, Pid, Ref} = new(Path, Http2Headers),
    data(Pid, Ref, Request, nofin),
    {Pid, Ref}.

-spec data(GunPid, Http2Ref, EtcdRequest, Http2Path) -> Http2Ref when
    GunPid :: pid(),
    Http2Ref :: reference(),
    EtcdRequest :: tuple(),
    Http2Path :: iodata().
data(Pid, Ref, Request, IsFin) ->
    EncodeBody = eetcd_grpc:encode(identity, Request),
    gun:data(Pid, Ref, IsFin, EncodeBody),
    Ref.

-spec unary(EtcdRequest, Http2Path, EtcdResponseType) -> EtcdResponse when
    EtcdRequest :: tuple(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    EtcdResponse :: tuple().
unary(Request, Path, ResponseType) ->
    Pid = eetcd_http2_keeper:get_http2_client_pid(),
    unary(Pid, Request, Path, ResponseType, ?HEADERS).
-spec unary(EtcdRequest, Http2Path, EtcdResponseType, Http2Headers) -> EtcdResponse when
    EtcdRequest :: tuple(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    Http2Headers :: list(),
    EtcdResponse :: tuple().
unary(Request, Path, ResponseType, Headers) ->
    Pid = eetcd_http2_keeper:get_http2_client_pid(),
    unary(Pid, Request, Path, ResponseType, Headers ++ ?HEADERS).

unary(Pid, Request, Path, ResponseType, Headers) ->
    EncodeBody = eetcd_grpc:encode(identity, Request),
    MRef = erlang:monitor(process, Pid),
    StreamRef = gun:request(Pid, <<"POST">>, Path, Headers, EncodeBody),
    Res =
        case await(Pid, StreamRef, ?TIMEOUT + 3000, MRef) of
            {response, nofin, 200, _Headers} ->
                case await_body(Pid, StreamRef, ?TIMEOUT, MRef, <<>>) of
                    {ok, ResBody, _Trailers} ->
                        {ok, eetcd_grpc:decode(identity, ResBody, ResponseType)};
                    {error, _} = Error1 -> Error1
                end;
            {response, fin, 200, RespHeaders} ->
                {GrpcStatus, GrpcMessage} = eetcd_grpc:grpc_status(RespHeaders),
                {error, ?GRPC_ERROR(GrpcStatus, GrpcMessage)};
            {error, _} = Error2 -> Error2
        end,
    erlang:demonitor(MRef, [flush]),
    Res.

%% `gun:await/2,3,4`, `gun:await_body/2,3,4` and `gun:await_up/1,2,3` don't distinguish the error types until v2.0.0.
%%  They can be a timeout, a connection error, a stream error or a down error (when the Gun process exited while waiting).
%% so we copy some code from gun v2.0.0 to replace `gun:await/4`
%% TODO remove this when upgrade gun to v2.0.0
await(ServerPid, StreamRef, Timeout, MRef) ->
    receive
        {gun_response, ServerPid, StreamRef, IsFin, Status, Headers} ->
            {response, IsFin, Status, Headers};
        {gun_data, ServerPid, StreamRef, IsFin, Data} ->
            {data, IsFin, Data};
        {gun_error, ServerPid, StreamRef, Reason} ->
            {error, {stream_error, Reason}};
        {gun_error, ServerPid, Reason} ->
            {error, {connection_error, Reason}};
        {'DOWN', MRef, process, ServerPid, Reason} ->
            {error, {down, Reason}}
    after Timeout ->
        {error, timeout}
    end.

await_body(ServerPid, StreamRef, Timeout, MRef, Acc) ->
    receive
        {gun_data, ServerPid, StreamRef, nofin, Data} ->
            await_body(ServerPid, StreamRef, Timeout, MRef,
                << Acc/binary, Data/binary >>);
        {gun_data, ServerPid, StreamRef, fin, Data} ->
            {ok, << Acc/binary, Data/binary >>};
    %% It's OK to return trailers here because the client specifically requested them
        {gun_trailers, ServerPid, StreamRef, Trailers} ->
            {ok, Acc, Trailers};
        {gun_error, ServerPid, StreamRef, Reason} ->
            {error, {stream_error, Reason}};
        {gun_error, ServerPid, Reason} ->
            {error, {connection_error, Reason}};
        {'DOWN', MRef, process, ServerPid, Reason} ->
            {error, {down, Reason}}
    after Timeout ->
        {error, timeout}
    end.
