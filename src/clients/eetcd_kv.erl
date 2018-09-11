%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-11T07:54:56+00:00 and should not be modified manually

-module(eetcd_kv).

-export([range/1]).
-export([put/1]).
-export([delete_range/1]).
-export([txn/1]).
-export([compact/1]).

%% @doc Unary RPC 
-spec range(router_pb:'Etcd.RangeRequest'()) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, term()}.
range(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse').

%% @doc Unary RPC 
-spec put(router_pb:'Etcd.PutRequest'()) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, term()}.
put(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse').

%% @doc Unary RPC 
-spec delete_range(router_pb:'Etcd.DeleteRangeRequest'()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, term()}.
delete_range(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse').

%% @doc Unary RPC 
-spec txn(router_pb:'Etcd.TxnRequest'()) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, term()}.
txn(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse').

%% @doc Unary RPC 
-spec compact(router_pb:'Etcd.CompactionRequest'()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, term()}.
compact(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse').

