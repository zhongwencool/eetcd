%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:51:59+00:00 and should not be modified manually

-module(eetcd_kv).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([range/1, range/2]).
-export([put/1, put/2]).
-export([delete_range/1, delete_range/2]).
-export([txn/1, txn/2]).
-export([compact/1, compact/2]).

%% @doc Unary RPC 
-spec range(#'Etcd.RangeRequest'{}) ->
    {ok, #'Etcd.RangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
range(Request) when is_record(Request, 'Etcd.RangeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', []).
-spec range(#'Etcd.RangeRequest'{}, Http2Headers) ->
    {ok, #'Etcd.RangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
range(Request, Token) when is_record(Request, 'Etcd.RangeRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', [{<<"authorization">>, Token}]);
range(Request, Http2Headers) when is_record(Request, 'Etcd.RangeRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', Http2Headers).

%% @doc Unary RPC 
-spec put(#'Etcd.PutRequest'{}) ->
    {ok, #'Etcd.PutResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
put(Request) when is_record(Request, 'Etcd.PutRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', []).
-spec put(#'Etcd.PutRequest'{}, Http2Headers) ->
    {ok, #'Etcd.PutResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
put(Request, Token) when is_record(Request, 'Etcd.PutRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', [{<<"authorization">>, Token}]);
put(Request, Http2Headers) when is_record(Request, 'Etcd.PutRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', Http2Headers).

%% @doc Unary RPC 
-spec delete_range(#'Etcd.DeleteRangeRequest'{}) ->
    {ok, #'Etcd.DeleteRangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
delete_range(Request) when is_record(Request, 'Etcd.DeleteRangeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', []).
-spec delete_range(#'Etcd.DeleteRangeRequest'{}, Http2Headers) ->
    {ok, #'Etcd.DeleteRangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
delete_range(Request, Token) when is_record(Request, 'Etcd.DeleteRangeRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', [{<<"authorization">>, Token}]);
delete_range(Request, Http2Headers) when is_record(Request, 'Etcd.DeleteRangeRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', Http2Headers).

%% @doc Unary RPC 
-spec txn(#'Etcd.TxnRequest'{}) ->
    {ok, #'Etcd.TxnResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
txn(Request) when is_record(Request, 'Etcd.TxnRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', []).
-spec txn(#'Etcd.TxnRequest'{}, Http2Headers) ->
    {ok, #'Etcd.TxnResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
txn(Request, Token) when is_record(Request, 'Etcd.TxnRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', [{<<"authorization">>, Token}]);
txn(Request, Http2Headers) when is_record(Request, 'Etcd.TxnRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', Http2Headers).

%% @doc Unary RPC 
-spec compact(#'Etcd.CompactionRequest'{}) ->
    {ok, #'Etcd.CompactionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
compact(Request) when is_record(Request, 'Etcd.CompactionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', []).
-spec compact(#'Etcd.CompactionRequest'{}, Http2Headers) ->
    {ok, #'Etcd.CompactionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
compact(Request, Token) when is_record(Request, 'Etcd.CompactionRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', [{<<"authorization">>, Token}]);
compact(Request, Http2Headers) when is_record(Request, 'Etcd.CompactionRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', Http2Headers).

