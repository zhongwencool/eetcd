%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_cluster_gen).

-export([member_add/2, member_add/3]).
-export([member_remove/2, member_remove/3]).
-export([member_update/2, member_update/3]).
-export([member_list/2, member_list/3]).
-export([member_promote/2, member_promote/3]).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberAdd'
-spec member_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberAddRequest'()) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()} | {error, eetcd:eetcd_error()}.
member_add(Client, Request) ->
    member_add(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberAdd'
-spec member_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberAddRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()} | {error, eetcd:eetcd_error()}.
member_add(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberRemove'
-spec member_remove(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberRemoveRequest'()) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()} | {error, eetcd:eetcd_error()}.
member_remove(Client, Request) ->
    member_remove(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberRemove'
-spec member_remove(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberRemoveRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()} | {error, eetcd:eetcd_error()}.
member_remove(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberUpdate'
-spec member_update(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberUpdateRequest'()) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()} | {error, eetcd:eetcd_error()}.
member_update(Client, Request) ->
    member_update(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberUpdate'
-spec member_update(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberUpdateRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()} | {error, eetcd:eetcd_error()}.
member_update(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberList'
-spec member_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberListRequest'()) ->
    {ok, router_pb:'Etcd.MemberListResponse'()} | {error, eetcd:eetcd_error()}.
member_list(Client, Request) ->
    member_list(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberList'
-spec member_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberListRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MemberListResponse'()} | {error, eetcd:eetcd_error()}.
member_list(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberPromote'
-spec member_promote(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberPromoteRequest'()) ->
    {ok, router_pb:'Etcd.MemberPromoteResponse'()} | {error, eetcd:eetcd_error()}.
member_promote(Client, Request) ->
    member_promote(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Cluster/MemberPromote'
-spec member_promote(Client :: eetcd:client(), Request :: router_pb:'Etcd.MemberPromoteRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MemberPromoteResponse'()} | {error, eetcd:eetcd_error()}.
member_promote(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MemberPromoteRequest', <<"/etcdserverpb.Cluster/MemberPromote">>, 'Etcd.MemberPromoteResponse', router_pb, Opts).

