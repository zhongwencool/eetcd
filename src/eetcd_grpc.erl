-module(eetcd_grpc).

-export([decode/3, encode/2]).

%%====================================================================
%% API functions
%%====================================================================
-define(GRPC_ERROR(Status, Message), {grpc_error, {Status, Message}}).
-define(GRPC_STATUS_UNIMPLEMENTED, <<"12">>).
-define(GRPC_STATUS_INTERNAL, <<"13">>).

-spec encode(identity | gzip, tuple()) -> binary().
encode(GrpcType, Msg) ->
    PbMsg = router_pb:encode_msg(Msg, [{verify, true}]),
    encode_(GrpcType, PbMsg).

-spec decode(identity | gzip, binary(), atom()) -> tuple().
decode(Encoding, Frame, PbType) ->
    PbBin = decode_(Frame, Encoding),
    router_pb:decode_msg(PbBin, PbType).

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
