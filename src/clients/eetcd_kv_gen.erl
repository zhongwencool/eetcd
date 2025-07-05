%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.KV
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_kv_gen).

-export([range/2, range/3]).
-export([put/2, put/3]).
-export([delete_range/2, delete_range/3]).
-export([txn/2, txn/3]).
-export([compact/2, compact/3]).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Range'
-spec range(Client :: eetcd:client(), Request :: router_pb:'Etcd.RangeRequest'()) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, eetcd:eetcd_error()}.
range(Client, Request) ->
    range(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Range'
-spec range(Client :: eetcd:client(), Request :: router_pb:'Etcd.RangeRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.RangeResponse'()} | {error, eetcd:eetcd_error()}.
range(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.RangeRequest', <<"/etcdserverpb.KV/Range">>, 'Etcd.RangeResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Put'
-spec put(Client :: eetcd:client(), Request :: router_pb:'Etcd.PutRequest'()) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, eetcd:eetcd_error()}.
put(Client, Request) ->
    put(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Put'
-spec put(Client :: eetcd:client(), Request :: router_pb:'Etcd.PutRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.PutResponse'()} | {error, eetcd:eetcd_error()}.
put(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.PutRequest', <<"/etcdserverpb.KV/Put">>, 'Etcd.PutResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/DeleteRange'
-spec delete_range(Client :: eetcd:client(), Request :: router_pb:'Etcd.DeleteRangeRequest'()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, eetcd:eetcd_error()}.
delete_range(Client, Request) ->
    delete_range(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/DeleteRange'
-spec delete_range(Client :: eetcd:client(), Request :: router_pb:'Etcd.DeleteRangeRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.DeleteRangeResponse'()} | {error, eetcd:eetcd_error()}.
delete_range(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.DeleteRangeRequest', <<"/etcdserverpb.KV/DeleteRange">>, 'Etcd.DeleteRangeResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Txn'
-spec txn(Client :: eetcd:client(), Request :: router_pb:'Etcd.TxnRequest'()) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, eetcd:eetcd_error()}.
txn(Client, Request) ->
    txn(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Txn'
-spec txn(Client :: eetcd:client(), Request :: router_pb:'Etcd.TxnRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.TxnResponse'()} | {error, eetcd:eetcd_error()}.
txn(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.TxnRequest', <<"/etcdserverpb.KV/Txn">>, 'Etcd.TxnResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Compact'
-spec compact(Client :: eetcd:client(), Request :: router_pb:'Etcd.CompactionRequest'()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, eetcd:eetcd_error()}.
compact(Client, Request) ->
    compact(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.KV/Compact'
-spec compact(Client :: eetcd:client(), Request :: router_pb:'Etcd.CompactionRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.CompactionResponse'()} | {error, eetcd:eetcd_error()}.
compact(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.CompactionRequest', <<"/etcdserverpb.KV/Compact">>, 'Etcd.CompactionResponse', router_pb, Opts).

