%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_lease_gen).

-export([lease_grant/2, lease_grant/3]).
-export([lease_revoke/2, lease_revoke/3]).
-export([lease_keep_alive/1, lease_keep_alive/2]).
-export([lease_time_to_live/2, lease_time_to_live/3]).
-export([lease_leases/2, lease_leases/3]).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseGrant'
-spec lease_grant(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseGrantRequest'()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()} | {error, eetcd:eetcd_error()}.
lease_grant(Client, Request) ->
    lease_grant(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseGrant'
-spec lease_grant(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseGrantRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LeaseGrantResponse'()} | {error, eetcd:eetcd_error()}.
lease_grant(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LeaseGrantRequest', <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseRevoke'
-spec lease_revoke(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseRevokeRequest'()) ->
    {ok, router_pb:'Etcd.LeaseRevokeResponse'()} | {error, eetcd:eetcd_error()}.
lease_revoke(Client, Request) ->
    lease_revoke(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseRevoke'
-spec lease_revoke(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseRevokeRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LeaseRevokeResponse'()} | {error, eetcd:eetcd_error()}.
lease_revoke(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LeaseRevokeRequest', <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse', router_pb, Opts).

%% @doc Bidirectional streaming RPC for service at path `/etcdserverpb.Lease/LeaseKeepAlive'
-spec lease_keep_alive(Client :: eetcd:client()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
lease_keep_alive(Client) ->
    lease_keep_alive(Client, []).

%% @doc Bidirectional streaming RPC for service at path `/etcdserverpb.Lease/LeaseKeepAlive'
-spec lease_keep_alive(Client :: eetcd:client(), Opts :: eetcd:request_opts()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
lease_keep_alive(Client, Opts) ->
    eetcd_stream:bidi_streaming(Client, <<"/etcdserverpb.Lease/LeaseKeepAlive">>, router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseTimeToLive'
-spec lease_time_to_live(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseTimeToLiveRequest'()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()} | {error, eetcd:eetcd_error()}.
lease_time_to_live(Client, Request) ->
    lease_time_to_live(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseTimeToLive'
-spec lease_time_to_live(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseTimeToLiveRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LeaseTimeToLiveResponse'()} | {error, eetcd:eetcd_error()}.
lease_time_to_live(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LeaseTimeToLiveRequest', <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseLeases'
-spec lease_leases(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseLeasesRequest'()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()} | {error, eetcd:eetcd_error()}.
lease_leases(Client, Request) ->
    lease_leases(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Lease/LeaseLeases'
-spec lease_leases(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaseLeasesRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LeaseLeasesResponse'()} | {error, eetcd:eetcd_error()}.
lease_leases(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LeaseLeasesRequest', <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse', router_pb, Opts).

