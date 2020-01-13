%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-13T08:36:29+00:00 and should not be modified manually

-module(eetcd_cluster_gen).

-export([member_add/1, member_add/2]).
-export([member_remove/1, member_remove/2]).
-export([member_update/1, member_update/2]).
-export([member_list/1, member_list/2]).

%% @doc Unary RPC 
-spec member_add(router_pb:'Etcd.MemberAddRequest'()) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse').

%% @doc Unary RPC 
-spec member_remove(router_pb:'Etcd.MemberRemoveRequest'()) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_remove(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse').

%% @doc Unary RPC 
-spec member_update(router_pb:'Etcd.MemberUpdateRequest'()) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_update(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse').

%% @doc Unary RPC 
-spec member_list(router_pb:'Etcd.MemberListRequest'()) ->
    {ok, router_pb:'Etcd.MemberListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse').

