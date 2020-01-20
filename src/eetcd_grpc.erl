-module(eetcd_grpc).
-include("eetcd.hrl").

-export([decode/3, encode/3]).
-export([grpc_status/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec encode(identity | gzip, map(), atom()) -> binary().
encode(GrpcType, Msg, MsgName) ->
    PbMsg = router_pb:encode_msg(Msg, MsgName, [{verify, true}]),
    encode_(GrpcType, PbMsg).

-spec decode(identity | gzip, binary(), atom()) -> grpc_status().
decode(Encoding, Frame, PbType) ->
    PbBin = decode_(Frame, Encoding),
    router_pb:decode_msg(PbBin, PbType).

grpc_status(RespHeaders) ->
    GrpcStatus = binary_to_integer(proplists:get_value(<<"grpc-status">>, RespHeaders, <<"0">>)),
    GrpcMessage = proplists:get_value(<<"grpc-message">>, RespHeaders, <<"">>),
    case GrpcStatus of
        ?GRPC_STATUS_UNAVAILABLE -> %% {grpc_error, 14, <<"etcdserver: request timed out">>}}
            eetcd_http2_keeper:check_leader();
        _ -> ignore
    end,
    #{'grpc-status' => GrpcStatus, 'grpc-message' => GrpcMessage}.

%%====================================================================
%% Internal functions
%%====================================================================

encode_(identity, Bin) ->
    Length = byte_size(Bin),
    <<0, Length:32, Bin/binary>>;
encode_(gzip, Bin) ->
    CompressedBin = zlib:gzip(Bin),
    Length = byte_size(CompressedBin),
    <<1, Length:32, CompressedBin/binary>>;
encode_(Encoding, _) ->
    throw({error, {unknown_encoding, Encoding}}).

decode_(<<0, Length:32, Encoded:Length/binary>>, _Encoding) -> Encoded;
decode_(<<1, Length:32, Compressed:Length/binary>>, gzip) ->
    try
        zlib:gunzip(Compressed)
    catch
        error:data_error ->
            throw(?GRPC_ERROR(?GRPC_STATUS_INTERNAL,
                <<"Could not decompress but compression algorithm gzip is supported">>))
    end;
decode_(<<_, Length:32, _Compressed:Length/binary, _Rest/binary>>, Encoding) ->
    throw(?GRPC_ERROR(?GRPC_STATUS_UNIMPLEMENTED,
        <<"Compression mechanism ", (atom_to_binary(Encoding, utf8))/binary,
            " used for received frame not supported">>)).
