%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Health
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-04-02T02:00:45+00:00 and should not be modified manually

-module(eetcd_health_gen).

-export([check/1]).
-export([watch/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Health/Check" 
-spec check(router_pb:'Etcd.HealthCheckRequest'()) ->
    {ok, router_pb:'Etcd.HealthCheckResponse'()}|{error,eetcd:eetcd_error()}.
check(Request) ->
    eetcd_stream:unary(Request, 'Etcd.HealthCheckRequest', <<"/etcdserverpb.Health/Check">>, 'Etcd.HealthCheckResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Health/Watch" 
-spec watch(router_pb:'Etcd.HealthCheckRequest'()) ->
    {ok, router_pb:'Etcd.HealthCheckResponse'()}|{error,eetcd:eetcd_error()}.
watch(Request) ->
    eetcd_stream:unary(Request, 'Etcd.HealthCheckRequest', <<"/etcdserverpb.Health/Watch">>, 'Etcd.HealthCheckResponse').

