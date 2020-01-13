%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-13T08:36:28+00:00 and should not be modified manually

-module(eetcd_lease_gen).

-export([lease_grant/1, lease_grant/2]).
-export([lease_revoke/1, lease_revoke/2]).
-export([lease_keep_alive/1, lease_keep_alive/2]).
-export([lease_time_to_live/1, lease_time_to_live/2]).
-export([lease_leases/1, lease_leases/2]).

%% @doc Unary RPC 
-spec lease_grant(router_pb:'Etcd.LeaseGrantRequest'()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_grant(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseGrantRequest', <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse').

%% @doc Unary RPC 
-spec lease_revoke(router_pb:'Etcd.LeaseRevokeRequest'()) ->
    {ok, router_pb:'Etcd.LeaseRevokeResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_revoke(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseRevokeRequest', <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse').

%% @doc Stream RPC 
-spec lease_keep_alive(router_pb:'Etcd.LeaseKeepAliveRequest'()) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_keep_alive(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>).

%% @doc Unary RPC 
-spec lease_time_to_live(router_pb:'Etcd.LeaseTimeToLiveRequest'()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_time_to_live(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseTimeToLiveRequest', <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse').

%% @doc Unary RPC 
-spec lease_leases(router_pb:'Etcd.LeaseLeasesRequest'()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_leases(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseLeasesRequest', <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse').

