%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd grpc.health.v1.Health
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:57:53+00:00 and should not be modified manually

-module(eetcd_health_gen).

-export([check/2, check/3]).

%% @doc Unary RPC for service at path `/grpc.health.v1.Health/Check'
-spec check(Client :: eetcd:client(), Request :: health_pb:'grpc.health.v1.HealthCheckRequest'()) ->
    {ok, health_pb:'grpc.health.v1.HealthCheckResponse'()} | {error, eetcd:eetcd_error()}.
check(Client, Request) ->
    check(Client, Request, []).

%% @doc Unary RPC for service at path `/grpc.health.v1.Health/Check'
-spec check(Client :: eetcd:client(), Request :: health_pb:'grpc.health.v1.HealthCheckRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, health_pb:'grpc.health.v1.HealthCheckResponse'()} | {error, eetcd:eetcd_error()}.
check(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'grpc.health.v1.HealthCheckRequest', <<"/grpc.health.v1.Health/Check">>, 'grpc.health.v1.HealthCheckResponse', health_pb, Opts).

