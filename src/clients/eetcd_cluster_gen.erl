%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-06T08:17:27+00:00 and should not be modified manually

-module(eetcd_cluster_gen).

-export([member_add/1, member_add/2]).
-export([member_remove/1, member_remove/2]).
-export([member_update/1, member_update/2]).
-export([member_list/1, member_list/2]).

%% @doc Unary RPC 
-spec member_add(router_pb:'Etcd.MemberAddRequest'()) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', []).
-spec member_add(router_pb:'Etcd.MemberAddRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.MemberAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_add(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', [{<<"authorization">>, Token}]);
member_add(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.MemberAddRequest', <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_remove(router_pb:'Etcd.MemberRemoveRequest'()) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_remove(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', []).
-spec member_remove(router_pb:'Etcd.MemberRemoveRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.MemberRemoveResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_remove(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', [{<<"authorization">>, Token}]);
member_remove(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.MemberRemoveRequest', <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_update(router_pb:'Etcd.MemberUpdateRequest'()) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_update(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', []).
-spec member_update(router_pb:'Etcd.MemberUpdateRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.MemberUpdateResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_update(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', [{<<"authorization">>, Token}]);
member_update(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.MemberUpdateRequest', <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_list(router_pb:'Etcd.MemberListRequest'()) ->
    {ok, router_pb:'Etcd.MemberListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', []).
-spec member_list(router_pb:'Etcd.MemberListRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.MemberListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_list(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', [{<<"authorization">>, Token}]);
member_list(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.MemberListRequest', <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', Http2Headers).

