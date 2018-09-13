%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_lease).

-include("router_pb.hrl").
-export([lease_grant/1]).
-export([lease_revoke/1]).
-export([lease_keep_alive/1]).
-export([lease_time_to_live/1]).
-export([lease_leases/1]).

%% @doc Unary RPC 
-spec lease_grant(#'Etcd.LeaseGrantRequest'{}) ->
    {ok, #'Etcd.LeaseGrantResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_grant(Request)when is_record(Request, 'Etcd.LeaseGrantRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse').

%% @doc Unary RPC 
-spec lease_revoke(#'Etcd.LeaseRevokeRequest'{}) ->
    {ok, #'Etcd.LeaseRevokeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_revoke(Request)when is_record(Request, 'Etcd.LeaseRevokeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse').

%% @doc Stream RPC 
-spec lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{}) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_keep_alive(Request)when is_record(Request, 'Etcd.LeaseKeepAliveRequest') ->
    eetcd_stream:data(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>).

%% @doc Unary RPC 
-spec lease_time_to_live(#'Etcd.LeaseTimeToLiveRequest'{}) ->
    {ok, #'Etcd.LeaseTimeToLiveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_time_to_live(Request)when is_record(Request, 'Etcd.LeaseTimeToLiveRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse').

%% @doc Unary RPC 
-spec lease_leases(#'Etcd.LeaseLeasesRequest'{}) ->
    {ok, #'Etcd.LeaseLeasesResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_leases(Request)when is_record(Request, 'Etcd.LeaseLeasesRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse').

