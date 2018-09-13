%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_kv).

-include("router_pb.hrl").
-export([range/1]).
-export([put/1]).
-export([delete_range/1]).
-export([txn/1]).
-export([compact/1]).

%% @doc Unary RPC 
-spec range(#'Etcd.RangeRequest'{}) ->
    {ok, #'Etcd.RangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
range(Request) when is_record(Request, 'Etcd.RangeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse').

%% @doc Unary RPC 
-spec put(#'Etcd.PutRequest'{}) ->
    {ok, #'Etcd.PutResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
put(Request) when is_record(Request, 'Etcd.PutRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse').

%% @doc Unary RPC 
-spec delete_range(#'Etcd.DeleteRangeRequest'{}) ->
    {ok, #'Etcd.DeleteRangeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
delete_range(Request) when is_record(Request, 'Etcd.DeleteRangeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse').

%% @doc Unary RPC 
-spec txn(#'Etcd.TxnRequest'{}) ->
    {ok, #'Etcd.TxnResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
txn(Request) when is_record(Request, 'Etcd.TxnRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse').

%% @doc Unary RPC 
-spec compact(#'Etcd.CompactionRequest'{}) ->
    {ok, #'Etcd.CompactionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
compact(Request) when is_record(Request, 'Etcd.CompactionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse').

