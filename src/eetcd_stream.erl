%% @private
-module(eetcd_stream).

%% API
-export([unary/4, unary/6]).
-export([new/2, new/4]).
-export([data/5]).
-export([await/4]).

-include("eetcd.hrl").

-spec new(Name, Path) -> {ok, GunPid, Http2Ref} | {error, eetcd:eetcd_error()} when
    Name :: name(),
    Path :: iodata(),
    GunPid :: pid(),
    Http2Ref :: reference().
new(Name, Path) ->
    case eetcd_conn:round_robin_select(Name) of
        {ok, Pid, Headers} ->
            Ref = gun:request(Pid, <<"POST">>, Path, Headers, <<>>),
            {ok, Pid, Ref};
        Err -> Err
    end.

-spec new(Name, EtcdMsg, EtcdMsgName, Http2Path) -> {ok, GunPid, Http2Ref} | {error, eetcd:eetcd_error()} when
    Name :: name(),
    EtcdMsg :: map(),
    EtcdMsgName :: atom(),
    Http2Path :: iodata(),
    GunPid :: pid(),
    Http2Ref :: reference().
new(Name, Msg, MsgName, Path) ->
    case new(Name, Path) of
        {ok, Pid, Ref} ->
            data(Pid, Ref, Msg, MsgName, nofin),
            {ok, Pid, Ref};
        Err -> Err
    end.

-spec data(GunPid, Http2Ref, EtcdMsg, EtcdMsgName, IsFin) -> Http2Ref when
    GunPid :: pid(),
    Http2Ref :: reference(),
    EtcdMsg :: map(),
    EtcdMsgName :: atom(),
    IsFin :: fin | nofin.
data(Pid, Ref, Msg, MsgName, IsFin) ->
    EncodeBody = eetcd_grpc:encode(identity, Msg, MsgName),
    gun:data(Pid, Ref, IsFin, EncodeBody),
    Ref.

-spec unary(EtcdRequest, EtcdRequestName, Http2Path, EtcdResponseType) -> EtcdResponse when
    EtcdRequest :: map(),
    EtcdRequestName :: atom(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    EtcdResponse :: tuple() | eetcd_error().
unary(Request, RequestName, Path, ResponseType) ->
    case maps:find(eetcd_conn_name, Request) of
        {ok, Name} ->
            case eetcd_conn:round_robin_select(Name) of
                {ok, Pid, Headers} ->
                    NewRequest = maps:remove(eetcd_conn_name, Request),
                    unary(Pid, NewRequest, RequestName, Path, ResponseType, Headers);
                Err -> Err
            end;
        error -> {error, eetcd_conn_unavailable}
    end.

-spec unary(Pid, EtcdRequest, EtcdRequestName, Http2Path, EtcdResponseType, Http2Headers) -> EtcdResponse when
    Pid :: pid(),
    EtcdRequest :: map(),
    EtcdRequestName :: atom(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    Http2Headers :: list(),
    EtcdResponse :: tuple().
unary(Pid, Request, RequestName, Path, ResponseType, Headers) when is_pid(Pid) ->
    Timeout = maps:get(eetcd_reply_timeout, Request, 9000),
    EncodeBody = eetcd_grpc:encode(identity, maps:remove(eetcd_reply_timeout, Request), RequestName),
    MRef = erlang:monitor(process, Pid),
    StreamRef = gun:request(Pid, <<"POST">>, Path, Headers, EncodeBody),
    Res =
        case await(Pid, StreamRef, Timeout, MRef) of
            {response, nofin, 200, _Headers} ->
                case await_body(Pid, StreamRef, Timeout, MRef, <<>>) of
                    {ok, ResBody, _Trailers} ->
                        {ok, Resp, <<>>} = eetcd_grpc:decode(identity, ResBody, ResponseType),
                        {ok, Resp};
                    {error, _} = Error1 -> Error1
                end;
            {response, fin, 200, RespHeaders} ->
                case eetcd_grpc:grpc_status(RespHeaders) of
                    #{'grpc-status' := ?GRPC_STATUS_UNAUTHENTICATED,
                        'grpc-message' := <<"etcdserver: invalid auth token">>} ->
                        NewHeaders = eetcd_conn:flush_token(Pid, Headers),
                        StreamRef1 = gun:request(Pid, <<"POST">>, Path, NewHeaders, EncodeBody),
                        case await(Pid, StreamRef1, Timeout, MRef) of
                            {response, nofin, 200, _Headers} ->
                                case await_body(Pid, StreamRef1, Timeout, MRef, <<>>) of
                                    {ok, ResBody, _Trailers} ->
                                        {ok, Resp, <<>>} = eetcd_grpc:decode(identity, ResBody, ResponseType),
                                        {ok, Resp};
                                    {error, _} = Error1 -> Error1
                                end;
                            {response, fin, 200, RespHeaders} ->
                                {error, {grpc_error, eetcd_grpc:grpc_status(RespHeaders)}}
                        end;
                    Error3 -> {error, {grpc_error, Error3}}
                end;
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
            {error, {gun_stream_error, Reason}};
        {gun_error, ServerPid, Reason} ->
            {error, {gun_conn_error, Reason}};
        {'DOWN', MRef, process, ServerPid, Reason} ->
            {error, {gun_down, Reason}}
    after Timeout ->
        {error, timeout}
    end.

await_body(ServerPid, StreamRef, Timeout, MRef, Acc) ->
    receive
        {gun_data, ServerPid, StreamRef, nofin, Data} ->
            await_body(ServerPid, StreamRef, Timeout, MRef,
                <<Acc/binary, Data/binary>>);
        {gun_data, ServerPid, StreamRef, fin, Data} ->
            {ok, <<Acc/binary, Data/binary>>};
    %% It's OK to return trailers here because the client specifically requested them
    %% Trailers are grpc_status and grpc_message headers
        {gun_trailers, ServerPid, StreamRef, Trailers} ->
            {ok, Acc, Trailers};
        {gun_error, ServerPid, StreamRef, Reason} ->
            {error, {gun_stream_error, Reason}};
        {gun_error, ServerPid, Reason} ->
            {error, {gun_conn_error, Reason}};
        {'DOWN', MRef, process, ServerPid, Reason} ->
            {error, {gun_down, Reason}}
    after Timeout ->
        {error, timeout}
    end.
