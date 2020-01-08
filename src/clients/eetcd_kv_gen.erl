%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-06T08:17:27+00:00 and should not be modified manually

-module(eetcd_kv_gen).

-export([range/1, range/2]).
-export([put/1, put/2]).
-export([delete_range/1, delete_range/2]).
-export([txn/1, txn/2]).
-export([compact/1, compact/2]).

%% @doc Unary RPC 
-spec range(router_pb:'Etcd.RangeRequest'()) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', []).
-spec range(router_pb:'Etcd.RangeRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
range(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', [{<<"authorization">>, Token}]);
range(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', Http2Headers).

%% @doc Unary RPC 
-spec put(router_pb:'Etcd.PutRequest'()) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
put(Request) ->
    eetcd_stream:unary(Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', []).
-spec put(router_pb:'Etcd.PutRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
put(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', [{<<"authorization">>, Token}]);
put(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', Http2Headers).

%% @doc Unary RPC 
-spec delete_range(router_pb:'Etcd.DeleteRangeRequest'()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
delete_range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', []).
-spec delete_range(router_pb:'Etcd.DeleteRangeRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
delete_range(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', [{<<"authorization">>, Token}]);
delete_range(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', Http2Headers).

%% @doc Unary RPC 
-spec txn(router_pb:'Etcd.TxnRequest'()) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
txn(Request) ->
    eetcd_stream:unary(Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', []).
-spec txn(router_pb:'Etcd.TxnRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
txn(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', [{<<"authorization">>, Token}]);
txn(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', Http2Headers).

%% @doc Unary RPC 
-spec compact(router_pb:'Etcd.CompactionRequest'()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
compact(Request) ->
    eetcd_stream:unary(Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', []).
-spec compact(router_pb:'Etcd.CompactionRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
compact(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', [{<<"authorization">>, Token}]);
compact(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', Http2Headers).

