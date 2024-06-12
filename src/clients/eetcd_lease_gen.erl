%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.Lease
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_lease_gen).

-export([lease_grant/1]).
-export([lease_revoke/1]).
-export([lease_keep_alive/1]).
-export([lease_time_to_live/1]).
-export([lease_leases/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseGrant"
-spec lease_grant(rpc_pb:'etcdserverpb.LeaseGrantRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.LeaseGrantResponse'()}|{error,eetcd:eetcd_error()}.
lease_grant(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.LeaseGrantRequest', <<"/etcdserverpb.Lease/LeaseGrant">>, 'etcdserverpb.LeaseGrantResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseRevoke"
-spec lease_revoke(rpc_pb:'etcdserverpb.LeaseRevokeRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.LeaseRevokeResponse'()}|{error,eetcd:eetcd_error()}.
lease_revoke(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.LeaseRevokeRequest', <<"/etcdserverpb.Lease/LeaseRevoke">>, 'etcdserverpb.LeaseRevokeResponse').

%% @doc Stream RPC
-spec lease_keep_alive(atom()|reference()) ->
    {ok, GunPid :: pid(), Http2Ref:: reference()}|{error,eetcd:eetcd_error()}.
lease_keep_alive(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>).

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseTimeToLive"
-spec lease_time_to_live(rpc_pb:'etcdserverpb.LeaseTimeToLiveRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.LeaseTimeToLiveResponse'()}|{error,eetcd:eetcd_error()}.
lease_time_to_live(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.LeaseTimeToLiveRequest', <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'etcdserverpb.LeaseTimeToLiveResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Lease/LeaseLeases"
-spec lease_leases(rpc_pb:'etcdserverpb.LeaseLeasesRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.LeaseLeasesResponse'()}|{error,eetcd:eetcd_error()}.
lease_leases(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.LeaseLeasesRequest', <<"/etcdserverpb.Lease/LeaseLeases">>, 'etcdserverpb.LeaseLeasesResponse').

