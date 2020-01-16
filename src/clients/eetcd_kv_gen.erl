%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-14T03:14:25+00:00 and should not be modified manually

-module(eetcd_kv_gen).

-export([range/1]).
-export([put/1]).
-export([delete_range/1]).
-export([txn/1]).
-export([compact/1]).

%% @doc Unary RPC 
-spec range(router_pb:'Etcd.RangeRequest'()) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse').

%% @doc Unary RPC 
-spec put(router_pb:'Etcd.PutRequest'()) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
put(Request) ->
    eetcd_stream:unary(Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse').

%% @doc Unary RPC 
-spec delete_range(router_pb:'Etcd.DeleteRangeRequest'()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
delete_range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse').

%% @doc Unary RPC 
-spec txn(router_pb:'Etcd.TxnRequest'()) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
txn(Request) ->
    eetcd_stream:unary(Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse').

%% @doc Unary RPC 
-spec compact(router_pb:'Etcd.CompactionRequest'()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
compact(Request) ->
    eetcd_stream:unary(Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse').

