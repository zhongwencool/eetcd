%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2021-12-22T07:01:11+00:00 and should not be modified manually

-module(eetcd_lease_gen).

-export([lease_grant/1]).
-export([lease_revoke/1]).
-export([lease_keep_alive/1]).
-export([lease_time_to_live/1]).
-export([lease_leases/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseGrant" 
-spec lease_grant(router_pb:'Etcd.LeaseGrantRequest'()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()}|{error,eetcd:eetcd_error()}.
lease_grant(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseGrantRequest', <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseRevoke" 
-spec lease_revoke(router_pb:'Etcd.LeaseRevokeRequest'()) ->
    {ok, router_pb:'Etcd.LeaseRevokeResponse'()}|{error,eetcd:eetcd_error()}.
lease_revoke(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseRevokeRequest', <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse').

%% @doc Stream RPC 
-spec lease_keep_alive(atom()|reference()) ->
    {ok, GunPid :: pid(), Http2Ref:: reference()}|{error,eetcd:eetcd_error()}.
lease_keep_alive(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>).

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseTimeToLive" 
-spec lease_time_to_live(router_pb:'Etcd.LeaseTimeToLiveRequest'()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()}|{error,eetcd:eetcd_error()}.
lease_time_to_live(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseTimeToLiveRequest', <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseLeases" 
-spec lease_leases(router_pb:'Etcd.LeaseLeasesRequest'()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()}|{error,eetcd:eetcd_error()}.
lease_leases(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaseLeasesRequest', <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse').

