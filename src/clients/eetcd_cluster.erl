%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Cluster.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:52:00+00:00 and should not be modified manually

-module(eetcd_cluster).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([member_add/1, member_add/2]).
-export([member_remove/1, member_remove/2]).
-export([member_update/1, member_update/2]).
-export([member_list/1, member_list/2]).

%% @doc Unary RPC 
-spec member_add(#'Etcd.MemberAddRequest'{}) ->
    {ok, #'Etcd.MemberAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_add(Request) when is_record(Request, 'Etcd.MemberAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', []).
-spec member_add(#'Etcd.MemberAddRequest'{}, Http2Headers) ->
    {ok, #'Etcd.MemberAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_add(Request, Token) when is_record(Request, 'Etcd.MemberAddRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', [{<<"authorization">>, Token}]);
member_add(Request, Http2Headers) when is_record(Request, 'Etcd.MemberAddRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberAdd">>, 'Etcd.MemberAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_remove(#'Etcd.MemberRemoveRequest'{}) ->
    {ok, #'Etcd.MemberRemoveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_remove(Request) when is_record(Request, 'Etcd.MemberRemoveRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', []).
-spec member_remove(#'Etcd.MemberRemoveRequest'{}, Http2Headers) ->
    {ok, #'Etcd.MemberRemoveResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_remove(Request, Token) when is_record(Request, 'Etcd.MemberRemoveRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', [{<<"authorization">>, Token}]);
member_remove(Request, Http2Headers) when is_record(Request, 'Etcd.MemberRemoveRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberRemove">>, 'Etcd.MemberRemoveResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_update(#'Etcd.MemberUpdateRequest'{}) ->
    {ok, #'Etcd.MemberUpdateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_update(Request) when is_record(Request, 'Etcd.MemberUpdateRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', []).
-spec member_update(#'Etcd.MemberUpdateRequest'{}, Http2Headers) ->
    {ok, #'Etcd.MemberUpdateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_update(Request, Token) when is_record(Request, 'Etcd.MemberUpdateRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', [{<<"authorization">>, Token}]);
member_update(Request, Http2Headers) when is_record(Request, 'Etcd.MemberUpdateRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberUpdate">>, 'Etcd.MemberUpdateResponse', Http2Headers).

%% @doc Unary RPC 
-spec member_list(#'Etcd.MemberListRequest'{}) ->
    {ok, #'Etcd.MemberListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
member_list(Request) when is_record(Request, 'Etcd.MemberListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', []).
-spec member_list(#'Etcd.MemberListRequest'{}, Http2Headers) ->
    {ok, #'Etcd.MemberListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
member_list(Request, Token) when is_record(Request, 'Etcd.MemberListRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', [{<<"authorization">>, Token}]);
member_list(Request, Http2Headers) when is_record(Request, 'Etcd.MemberListRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Cluster/MemberList">>, 'Etcd.MemberListResponse', Http2Headers).

