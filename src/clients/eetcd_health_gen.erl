%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd grpc.health.v1.Health
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-06-21T09:30:19+00:00 and should not be modified manually

-module(eetcd_health_gen).

-export([check/1]).

%% @doc Unary RPC for service at path "/grpc.health.v1.Health/Check" 
-spec check(health_pb:'grpc.health.v1.HealthCheckRequest'()) ->
    {ok, health_pb:'grpc.health.v1.HealthCheckResponse'()}|{error,eetcd:eetcd_error()}.
check(Request) ->
    eetcd_stream:unary(Request, 'grpc.health.v1.HealthCheckRequest', <<"/grpc.health.v1.Health/Check">>, 'grpc.health.v1.HealthCheckResponse', health_pb).

