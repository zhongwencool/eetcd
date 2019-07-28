-module(eetcd_stream).

%% API
-export([unary/3, unary/4]).
-export([new/2, new/3]).
-export([data/3, data/4]).

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
    Ref.

-spec data(Http2Ref, EtcdRequest, Http2Path) -> ok when
    Http2Ref :: reference(),
    EtcdRequest :: tuple(),
    Http2Path :: iodata().
data(Ref, Request, IsFin) ->
    Pid = eetcd_http2_keeper:get_http2_client_pid(),
    data(Pid, Ref, Request, IsFin).

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
    StreamRef = gun:request(Pid, <<"POST">>, Path, Headers, EncodeBody),
    MRef = erlang:monitor(process, Pid),
    Res =
        case gun:await(Pid, StreamRef, ?TIMEOUT + 10000, MRef) of
            {response, nofin, 200, _Headers} ->
                case gun:await_body(Pid, StreamRef, ?TIMEOUT, MRef) of
                    {ok, ResBody, _Trailers} ->
                        {ok, eetcd_grpc:decode(identity, ResBody, ResponseType)};
                    {error, _} = Error1 ->
                        Error1
                end;
            {response, fin, 200, RespHeaders} ->
                GrpcStatus = binary_to_integer(proplists:get_value(<<"grpc-status">>, RespHeaders, <<"0">>)),
                GrpcMessage = proplists:get_value(<<"grpc-message">>, RespHeaders, <<"">>),
                case GrpcStatus of
                    ?GRPC_STATUS_UNAVAILABLE -> %% {grpc_error, 14, <<"etcdserver: request timed out">>}}
                        eetcd_http2_keeper:check_leader();
                    _ -> ignore
                end,
                {error, {'grpc_error', GrpcStatus, GrpcMessage}};
            {error, _} = Error2 ->
                Error2
        end,
    erlang:demonitor(MRef, [flush]),
    Res.
