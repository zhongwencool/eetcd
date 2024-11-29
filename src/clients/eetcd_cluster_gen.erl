%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.Cluster
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_cluster_gen).

-export([member_add/1]).
-export([member_remove/1]).
-export([member_update/1]).
-export([member_list/1]).
-export([member_promote/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberAdd"
-spec member_add(rpc_pb:'etcdserverpb.MemberAddRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MemberAddResponse'()}|{error,eetcd:eetcd_error()}.
member_add(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'etcdserverpb.MemberAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberRemove"
-spec member_remove(rpc_pb:'etcdserverpb.MemberRemoveRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MemberRemoveResponse'()}|{error,eetcd:eetcd_error()}.
member_remove(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'etcdserverpb.MemberRemoveResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberUpdate"
-spec member_update(rpc_pb:'etcdserverpb.MemberUpdateRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MemberUpdateResponse'()}|{error,eetcd:eetcd_error()}.
member_update(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'etcdserverpb.MemberUpdateResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberList"
-spec member_list(rpc_pb:'etcdserverpb.MemberListRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MemberListResponse'()}|{error,eetcd:eetcd_error()}.
member_list(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'etcdserverpb.MemberListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Cluster/MemberPromote"
-spec member_promote(rpc_pb:'etcdserverpb.MemberPromoteRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MemberPromoteResponse'()}|{error,eetcd:eetcd_error()}.
member_promote(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MemberPromoteRequest', <<"/etcdserverpb.Cluster/MemberPromote">>, 'etcdserverpb.MemberPromoteResponse').

