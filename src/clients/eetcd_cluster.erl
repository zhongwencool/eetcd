%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_cluster).

-include("router_pb.hrl").
-export([member_add/1]).
-export([member_remove/1]).
-export([member_update/1]).
-export([member_list/1]).

%% @doc Unary RPC 
-spec member_add(#'Etcd.MemberAddRequest'{}) ->
    {ok, #'Etcd.MemberAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_add(Request)when is_record(Request, 'Etcd.MemberAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse').

%% @doc Unary RPC 
-spec member_remove(#'Etcd.MemberRemoveRequest'{}) ->
    {ok, #'Etcd.MemberRemoveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_remove(Request)when is_record(Request, 'Etcd.MemberRemoveRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse').

%% @doc Unary RPC 
-spec member_update(#'Etcd.MemberUpdateRequest'{}) ->
    {ok, #'Etcd.MemberUpdateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_update(Request)when is_record(Request, 'Etcd.MemberUpdateRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse').

%% @doc Unary RPC 
-spec member_list(#'Etcd.MemberListRequest'{}) ->
    {ok, #'Etcd.MemberListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_list(Request)when is_record(Request, 'Etcd.MemberListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse').

