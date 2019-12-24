%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Lease.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:52:00+00:00 and should not be modified manually

-module(eetcd_lease).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([lease_grant/1, lease_grant/2]).
-export([lease_revoke/1, lease_revoke/2]).
-export([lease_keep_alive/1, lease_keep_alive/2]).
-export([lease_time_to_live/1, lease_time_to_live/2]).
-export([lease_leases/1, lease_leases/2]).

%% @doc Unary RPC 
-spec lease_grant(#'Etcd.LeaseGrantRequest'{}) ->
    {ok, #'Etcd.LeaseGrantResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_grant(Request) when is_record(Request, 'Etcd.LeaseGrantRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse', []).
-spec lease_grant(#'Etcd.LeaseGrantRequest'{}, Http2Headers) ->
    {ok, #'Etcd.LeaseGrantResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
lease_grant(Request, Token) when is_record(Request, 'Etcd.LeaseGrantRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse', [{<<"authorization">>, Token}]);
lease_grant(Request, Http2Headers) when is_record(Request, 'Etcd.LeaseGrantRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseGrant">>, 'Etcd.LeaseGrantResponse', Http2Headers).

%% @doc Unary RPC 
-spec lease_revoke(#'Etcd.LeaseRevokeRequest'{}) ->
    {ok, #'Etcd.LeaseRevokeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_revoke(Request) when is_record(Request, 'Etcd.LeaseRevokeRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse', []).
-spec lease_revoke(#'Etcd.LeaseRevokeRequest'{}, Http2Headers) ->
    {ok, #'Etcd.LeaseRevokeResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
lease_revoke(Request, Token) when is_record(Request, 'Etcd.LeaseRevokeRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse', [{<<"authorization">>, Token}]);
lease_revoke(Request, Http2Headers) when is_record(Request, 'Etcd.LeaseRevokeRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseRevoke">>, 'Etcd.LeaseRevokeResponse', Http2Headers).

%% @doc Stream RPC 
-spec lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{}) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_keep_alive(Request) when is_record(Request, 'Etcd.LeaseKeepAliveRequest') ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>, []).
-spec lease_keep_alive(#'Etcd.LeaseKeepAliveRequest'{}, Http2Headers) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
lease_keep_alive(Request, Token) when is_record(Request, 'Etcd.LeaseKeepAliveRequest') andalso is_binary(Token) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>, [{<<"authorization">>, Token}]);
lease_keep_alive(Request, Http2Headers) when is_record(Request, 'Etcd.LeaseKeepAliveRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:new(Request, <<"/etcdserverpb.Lease/LeaseKeepAlive">>, Http2Headers).

%% @doc Unary RPC 
-spec lease_time_to_live(#'Etcd.LeaseTimeToLiveRequest'{}) ->
    {ok, #'Etcd.LeaseTimeToLiveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_time_to_live(Request) when is_record(Request, 'Etcd.LeaseTimeToLiveRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse', []).
-spec lease_time_to_live(#'Etcd.LeaseTimeToLiveRequest'{}, Http2Headers) ->
    {ok, #'Etcd.LeaseTimeToLiveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
lease_time_to_live(Request, Token) when is_record(Request, 'Etcd.LeaseTimeToLiveRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse', [{<<"authorization">>, Token}]);
lease_time_to_live(Request, Http2Headers) when is_record(Request, 'Etcd.LeaseTimeToLiveRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseTimeToLive">>, 'Etcd.LeaseTimeToLiveResponse', Http2Headers).

%% @doc Unary RPC 
-spec lease_leases(#'Etcd.LeaseLeasesRequest'{}) ->
    {ok, #'Etcd.LeaseLeasesResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
lease_leases(Request) when is_record(Request, 'Etcd.LeaseLeasesRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse', []).
-spec lease_leases(#'Etcd.LeaseLeasesRequest'{}, Http2Headers) ->
    {ok, #'Etcd.LeaseLeasesResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
lease_leases(Request, Token) when is_record(Request, 'Etcd.LeaseLeasesRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse', [{<<"authorization">>, Token}]);
lease_leases(Request, Http2Headers) when is_record(Request, 'Etcd.LeaseLeasesRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Lease/LeaseLeases">>, 'Etcd.LeaseLeasesResponse', Http2Headers).

