%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lock
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-04-02T02:00:46+00:00 and should not be modified manually

-module(eetcd_lock_gen).

-export([lock/1]).
-export([unlock/1]).

%% @doc Unary RPC for service at path "/v3lockpb.Lock/Lock" 
-spec lock(router_pb:'Etcd.LockRequest'()) ->
    {ok, router_pb:'Etcd.LockResponse'()}|{error,eetcd:eetcd_error()}.
lock(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LockRequest', <<"/v3lockpb.Lock/Lock">>, 'Etcd.LockResponse').

%% @doc Unary RPC for service at path "/v3lockpb.Lock/Unlock" 
-spec unlock(router_pb:'Etcd.UnlockRequest'()) ->
    {ok, router_pb:'Etcd.UnlockResponse'()}|{error,eetcd:eetcd_error()}.
unlock(Request) ->
    eetcd_stream:unary(Request, 'Etcd.UnlockRequest', <<"/v3lockpb.Lock/Unlock">>, 'Etcd.UnlockResponse').

