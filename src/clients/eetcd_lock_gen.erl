%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lock
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_lock_gen).

-export([lock/2, lock/3]).
-export([unlock/2, unlock/3]).

%% @doc Unary RPC for service at path `/v3lockpb.Lock/Lock'
-spec lock(Client :: eetcd:client(), Request :: router_pb:'Etcd.LockRequest'()) ->
    {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd:eetcd_error()}.
lock(Client, Request) ->
    lock(Client, Request, []).

%% @doc Unary RPC for service at path `/v3lockpb.Lock/Lock'
-spec lock(Client :: eetcd:client(), Request :: router_pb:'Etcd.LockRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd:eetcd_error()}.
lock(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LockRequest', <<"/v3lockpb.Lock/Lock">>, 'Etcd.LockResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/v3lockpb.Lock/Unlock'
-spec unlock(Client :: eetcd:client(), Request :: router_pb:'Etcd.UnlockRequest'()) ->
    {ok, router_pb:'Etcd.UnlockResponse'()} | {error, eetcd:eetcd_error()}.
unlock(Client, Request) ->
    unlock(Client, Request, []).

%% @doc Unary RPC for service at path `/v3lockpb.Lock/Unlock'
-spec unlock(Client :: eetcd:client(), Request :: router_pb:'Etcd.UnlockRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.UnlockResponse'()} | {error, eetcd:eetcd_error()}.
unlock(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.UnlockRequest', <<"/v3lockpb.Lock/Unlock">>, 'Etcd.UnlockResponse', router_pb, Opts).

