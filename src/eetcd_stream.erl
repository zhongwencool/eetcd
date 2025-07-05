%% @private
-module(eetcd_stream).

%% API
-export([unary/7, bidi_streaming/4, server_streaming/6]).
-export([data/6, await/4]).

-include("eetcd.hrl").

-export_type([conn/0]).

-type conn() :: {GunPid :: pid(), Headers :: gun:req_headers()}.

-spec bidi_streaming(Client, Path, PbModule, Opts) -> Result when
    Result :: {ok, GunPid, Http2Ref, PbModule} | {error, eetcd:eetcd_error()},
    Client :: eetcd:client(),
    Path :: iodata(),
    PbModule :: module(),
    Opts :: eetcd:request_opts(),
    GunPid :: pid(),
    Http2Ref :: eetcd:stream_ref().
bidi_streaming(EtcdName, Path, PbModule, _Opts) when is_atom(EtcdName) ->
    case eetcd_conn:round_robin_select(EtcdName) of
        {ok, GunPid, Headers} ->
            bidi_streaming({EtcdName, {GunPid, Headers}}, Path, PbModule, _Opts);
        Err -> Err
    end;
bidi_streaming({EtcdName, {GunPid, Headers}}, Path, PbModule, _Opts) when is_atom(EtcdName) ->
    Ref = gun:headers(GunPid, <<"POST">>, Path, Headers),
    {ok, GunPid, Ref, PbModule}.

-spec server_streaming(Client, EtcdMsg, EtcdMsgName, Http2Path, PbModule, Opts) -> Result when
    Result :: {ok, GunPid, Http2Ref, PbModule} | {error, eetcd:eetcd_error()},
    Client :: eetcd:client(),
    EtcdMsg :: map(),
    EtcdMsgName :: atom(),
    Http2Path :: iodata(),
    PbModule :: module(),
    Opts :: eetcd:request_opts(),
    GunPid :: pid(),
    Http2Ref :: eetcd:stream_ref().
server_streaming(EtcdName, Msg, MsgName, Path, PbModule, Opts) ->
    case bidi_streaming(EtcdName, Path, PbModule, Opts) of
        {ok, Pid, Ref, _} ->
            data(Pid, Ref, Msg, MsgName, nofin, PbModule),
            {ok, Pid, Ref, PbModule};
        Err -> Err
    end.

-spec data(GunPid, Http2Ref, EtcdMsg, EtcdMsgName, IsFin, PbModule) -> Http2Ref when
    GunPid :: pid(),
    Http2Ref :: eetcd:stream_ref(),
    EtcdMsg :: map(),
    EtcdMsgName :: atom(),
    IsFin :: fin | nofin,
    PbModule :: module().
data(Pid, Ref, Msg, MsgName, IsFin, PbModule) ->
    EncodeBody = eetcd_grpc:encode(identity, Msg, MsgName, PbModule),
    gun:data(Pid, Ref, IsFin, EncodeBody),
    Ref.

-spec unary(EtcdName, EtcdRequest, EtcdRequestName, Http2Path, EtcdResponseType, PbModule, Opts) -> EtcdResponse when
    EtcdName :: etcd_name() | {etcd_name(), conn()},
    EtcdRequest :: map(),
    EtcdRequestName :: atom(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    PbModule :: module(),
    Opts :: eetcd:request_opts(),
    EtcdResponse :: {ok, ResponseMsg :: map()} | {error, eetcd_error()}.
unary(EtcdName, Request, RequestName, Path, ResponseType, PbModule, Opts) when is_atom(EtcdName) ->
    case eetcd_conn:round_robin_select(EtcdName) of
        {ok, GunPid, Headers} ->
            unary(EtcdName, GunPid, Request, RequestName, Path, ResponseType, Headers, PbModule,
                 Opts);
        Err -> Err
    end;
unary({EtcdName, {GunPid, Headers}}, Request, RequestName, Path, ResponseType, PbModule, Opts) ->
    unary(EtcdName, GunPid, Request, RequestName, Path, ResponseType, Headers, PbModule, Opts).

-spec unary(EtcdName, GunPid, EtcdRequest, EtcdRequestName, Http2Path, EtcdResponseType, Http2Headers,
            PbModule, Opts) -> EtcdResponse when
    EtcdName :: etcd_name(),
    GunPid :: pid(),
    EtcdRequest :: map(),
    EtcdRequestName :: atom(),
    Http2Path :: iodata(),
    EtcdResponseType :: atom(),
    Http2Headers :: gun:req_headers(),
    PbModule :: module(),
    Opts :: eetcd:request_opts(),
    EtcdResponse :: {ok, ResponseMsg :: map()} | {error, eetcd_error()}.
unary(EtcdName, GunPid, Request, RequestName, Path, ResponseType, Headers, PbModule, Opts) when is_pid(GunPid) ->
    Timeout = proplists:get_value(reply_timeout, Opts, 9000),
    EncodeBody = eetcd_grpc:encode(identity, Request, RequestName, PbModule),
    MRef = erlang:monitor(process, GunPid),
    StreamRef = gun:request(GunPid, <<"POST">>, Path, Headers, EncodeBody),
    Res =
        %% eqwalizer:ignore 
        case await(GunPid, StreamRef, Timeout, MRef) of
            {response, nofin, 200, _Headers} ->
                await_body(GunPid, StreamRef, Timeout, MRef, ResponseType, PbModule);
            {response, fin, 200, RespHeaders} ->
                case eetcd_grpc:grpc_status(RespHeaders) of
                    #{'grpc-status' := ?GRPC_STATUS_UNAUTHENTICATED,
                        'grpc-message' := <<"etcdserver: invalid auth token">>} ->
                        NewHeaders = eetcd_conn:refresh_token(EtcdName, Headers),
                        StreamRef1 = gun:request(GunPid, <<"POST">>, Path, NewHeaders, EncodeBody),
                        %% eqwalizer:ignore 
                        case await(GunPid, StreamRef1, Timeout, MRef) of
                            {response, nofin, 200, _Headers} ->
                                await_body(GunPid, StreamRef1, Timeout, MRef, ResponseType, PbModule);
                            {response, fin, 200, RespHeaders1} ->
                                {error, {grpc_error, eetcd_grpc:grpc_status(RespHeaders1)}}
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
-spec await(ServerPid, StreamRef, Timeout, MonitorRef) ->
    {response, IsFin, StatusCode, Headers} |
    {data, IsFin, Data} | {error, eetcd_error()}
      when
        ServerPid :: pid(),
        StreamRef :: gun:stream_ref(),
        Timeout :: timeout(),
        MonitorRef :: reference(),
        IsFin :: fin | nofin,
        StatusCode :: non_neg_integer(),
        Headers :: [{binary(), binary()}],
        Data :: binary().
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

await_body(ServerPid, StreamRef, Timeout, MRef, ResponseType, PbModule) ->
    case transfer_error(gun:await_body(ServerPid, StreamRef, Timeout, MRef)) of
        {ok, ResBody, _Trailers} ->
            {ok, Resp, <<>>} = eetcd_grpc:decode(identity, ResBody, ResponseType, PbModule),
            {ok, Resp};
        {error, _} = Error -> Error
    end.

transfer_error({error, {stream_error, Reason}}) ->
    {error, {gun_stream_error, Reason}};
transfer_error({error, {connection_error, Reason}}) ->
    {error, {gun_conn_error, Reason}};
transfer_error({error, {down, Reason}}) ->
    {error, {gun_down, Reason}};
transfer_error(Other) ->
    Other.
