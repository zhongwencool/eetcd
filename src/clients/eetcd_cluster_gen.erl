%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-04-02T02:00:45+00:00 and should not be modified manually

-module(eetcd_cluster_gen).

-export([member_add/1]).
-export([member_remove/1]).
-export([member_update/1]).
-export([member_list/1]).
-export([member_promote/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberAdd" 
-spec member_add(router_pb:'Etcd.MemberAddRequest'()) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()}|{error,eetcd:eetcd_error()}.
member_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberRemove" 
-spec member_remove(router_pb:'Etcd.MemberRemoveRequest'()) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()}|{error,eetcd:eetcd_error()}.
member_remove(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberUpdate" 
-spec member_update(router_pb:'Etcd.MemberUpdateRequest'()) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()}|{error,eetcd:eetcd_error()}.
member_update(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberList" 
-spec member_list(router_pb:'Etcd.MemberListRequest'()) ->
    {ok, router_pb:'Etcd.MemberListResponse'()}|{error,eetcd:eetcd_error()}.
member_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberPromote" 
-spec member_promote(router_pb:'Etcd.MemberPromoteRequest'()) ->
    {ok, router_pb:'Etcd.MemberPromoteResponse'()}|{error,eetcd:eetcd_error()}.
member_promote(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberPromoteRequest', <<"/etcdserverpb.Cluster/MemberPromote">>, 'Etcd.MemberPromoteResponse').

