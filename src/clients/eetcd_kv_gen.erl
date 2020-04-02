%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-04-02T02:00:45+00:00 and should not be modified manually

-module(eetcd_kv_gen).

-export([range/1]).
-export([put/1]).
-export([delete_range/1]).
-export([txn/1]).
-export([compact/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Range" 
-spec range(router_pb:'Etcd.RangeRequest'()) ->
    {ok, router_pb:'Etcd.RangeResponse'()}|{error,eetcd:eetcd_error()}.
range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Put" 
-spec put(router_pb:'Etcd.PutRequest'()) ->
    {ok, router_pb:'Etcd.PutResponse'()}|{error,eetcd:eetcd_error()}.
put(Request) ->
    eetcd_stream:unary(Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/DeleteRange" 
-spec delete_range(router_pb:'Etcd.DeleteRangeRequest'()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()}|{error,eetcd:eetcd_error()}.
delete_range(Request) ->
    eetcd_stream:unary(Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Txn" 
-spec txn(router_pb:'Etcd.TxnRequest'()) ->
    {ok, router_pb:'Etcd.TxnResponse'()}|{error,eetcd:eetcd_error()}.
txn(Request) ->
    eetcd_stream:unary(Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.KV/Compact" 
-spec compact(router_pb:'Etcd.CompactionRequest'()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()}|{error,eetcd:eetcd_error()}.
compact(Request) ->
    eetcd_stream:unary(Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse').

