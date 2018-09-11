%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-11T07:54:56+00:00 and should not be modified manually

-module(eetcd_lease).

-export([lease_grant/1]).
-export([lease_revoke/1]).
-export([lease_keep_alive/1]).
-export([lease_time_to_live/1]).
-export([lease_leases/1]).

%% @doc Unary RPC 
-spec lease_grant(router_pb:'Etcd.LeaseGrantRequest'()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()} | {error, term()}.
lease_grant(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse').

%% @doc Unary RPC 
-spec lease_revoke(router_pb:'Etcd.LeaseRevokeRequest'()) ->
    {ok, router_pb:'Etcd.LeaseRevokeResponse'()} | {error, term()}.
lease_revoke(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse').

%% @doc Stream RPC 
-spec lease_keep_alive(router_pb:'Etcd.LeaseKeepAliveRequest'()) ->
    reference() | {error, term()}.
lease_keep_alive(Request) ->
    eetcd_stream:data(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>).

%% @doc Unary RPC 
-spec lease_time_to_live(router_pb:'Etcd.LeaseTimeToLiveRequest'()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()} | {error, term()}.
lease_time_to_live(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse').

%% @doc Unary RPC 
-spec lease_leases(router_pb:'Etcd.LeaseLeasesRequest'()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()} | {error, term()}.
lease_leases(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse').

