%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.KV
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_kv_gen).

-export([range/1]).
-export([put/1]).
-export([delete_range/1]).
-export([txn/1]).
-export([compact/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Range"
-spec range(rpc_pb:'etcdserverpb.RangeRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.RangeResponse'()}|{error,eetcd:eetcd_error()}.
range(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'etcdserverpb.RangeResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Put"
-spec put(rpc_pb:'etcdserverpb.PutRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.PutResponse'()}|{error,eetcd:eetcd_error()}.
put(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.PutRequest', <<"/etcdserverpb.KV/Put">>, 'etcdserverpb.PutResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/DeleteRange"
-spec delete_range(rpc_pb:'etcdserverpb.DeleteRangeRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.DeleteRangeResponse'()}|{error,eetcd:eetcd_error()}.
delete_range(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'etcdserverpb.DeleteRangeResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Txn"
-spec txn(rpc_pb:'etcdserverpb.TxnRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.TxnResponse'()}|{error,eetcd:eetcd_error()}.
txn(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'etcdserverpb.TxnResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Compact"
-spec compact(rpc_pb:'etcdserverpb.CompactionRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.CompactionResponse'()}|{error,eetcd:eetcd_error()}.
compact(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'etcdserverpb.CompactionResponse').

