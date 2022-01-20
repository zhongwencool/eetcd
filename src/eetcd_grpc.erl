%% @private
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

-spec decode(identity | gzip, binary(), atom()) -> {ok, map(), binary()} | more.
decode(Encoding, Frame, PbType) ->
    case decode_(Frame, Encoding) of
        {ok, PbBin, Fragment} ->
            {ok, router_pb:decode_msg(PbBin, PbType), Fragment};
        more -> more
    end.

grpc_status(RespHeaders) ->
    GrpcStatus = binary_to_integer(proplists:get_value(<<"grpc-status">>, RespHeaders, <<"0">>)),
    GrpcMessage = proplists:get_value(<<"grpc-message">>, RespHeaders, <<"">>),
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

decode_(<<0, Length:32, Encoded:Length/binary, Rest/binary>>, _Encoding) -> {ok, Encoded, Rest};
decode_(<<0, _Length:32, _Binary/binary>>, _Encoding) -> more;
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
