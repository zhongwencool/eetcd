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
            Ref = gun:headers(Pid, <<"POST">>, Path, Headers),
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
                case await_body(Pid, StreamRef, Timeout, MRef) of
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
                                case await_body(Pid, StreamRef1, Timeout, MRef) of
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

await(ServerPid, StreamRef, Timeout, MRef) ->
    case gun:await(ServerPid, StreamRef, Timeout, MRef) of
        {response, _, _, _}=Resp ->
            Resp;
        {data, _, _}=Resp ->
            Resp;
        {error, _} = Resp ->
            transfer_error(Resp);
        Other ->
            ?LOG_INFO("eetcd_await_resp_other ~p", [Other]),
            await(ServerPid, StreamRef, Timeout, MRef)
    end.

await_body(ServerPid, StreamRef, Timeout, MRef) ->
    transfer_error(gun:await_body(ServerPid, StreamRef, Timeout, MRef)).

transfer_error({error, {stream_error, Reason}}) ->
    {error, {gun_stream_error, Reason}};
transfer_error({error, {connection_error, Reason}}) ->
    {error, {gun_conn_error, Reason}};
transfer_error({error, {down, Reason}}) ->
    {error, {gun_down, Reason}};
transfer_error(Other) ->
    Other.
