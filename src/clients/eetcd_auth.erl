%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:52:00+00:00 and should not be modified manually

-module(eetcd_auth).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([auth_enable/1, auth_enable/2]).
-export([auth_disable/1, auth_disable/2]).
-export([authenticate/1, authenticate/2]).
-export([user_add/1, user_add/2]).
-export([user_get/1, user_get/2]).
-export([user_list/1, user_list/2]).
-export([user_delete/1, user_delete/2]).
-export([user_change_password/1, user_change_password/2]).
-export([user_grant_role/1, user_grant_role/2]).
-export([user_revoke_role/1, user_revoke_role/2]).
-export([role_add/1, role_add/2]).
-export([role_get/1, role_get/2]).
-export([role_list/1, role_list/2]).
-export([role_delete/1, role_delete/2]).
-export([role_grant_permission/1, role_grant_permission/2]).
-export([role_revoke_permission/1, role_revoke_permission/2]).

%% @doc Unary RPC 
-spec auth_enable(#'Etcd.AuthEnableRequest'{}) ->
    {ok, #'Etcd.AuthEnableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_enable(Request) when is_record(Request, 'Etcd.AuthEnableRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', []).
-spec auth_enable(#'Etcd.AuthEnableRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthEnableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
auth_enable(Request, Token) when is_record(Request, 'Etcd.AuthEnableRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', [{<<"authorization">>, Token}]);
auth_enable(Request, Http2Headers) when is_record(Request, 'Etcd.AuthEnableRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', Http2Headers).

%% @doc Unary RPC 
-spec auth_disable(#'Etcd.AuthDisableRequest'{}) ->
    {ok, #'Etcd.AuthDisableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_disable(Request) when is_record(Request, 'Etcd.AuthDisableRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', []).
-spec auth_disable(#'Etcd.AuthDisableRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthDisableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
auth_disable(Request, Token) when is_record(Request, 'Etcd.AuthDisableRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', [{<<"authorization">>, Token}]);
auth_disable(Request, Http2Headers) when is_record(Request, 'Etcd.AuthDisableRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', Http2Headers).

%% @doc Unary RPC 
-spec authenticate(#'Etcd.AuthenticateRequest'{}) ->
    {ok, #'Etcd.AuthenticateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
authenticate(Request) when is_record(Request, 'Etcd.AuthenticateRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', []).
-spec authenticate(#'Etcd.AuthenticateRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthenticateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
authenticate(Request, Token) when is_record(Request, 'Etcd.AuthenticateRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', [{<<"authorization">>, Token}]);
authenticate(Request, Http2Headers) when is_record(Request, 'Etcd.AuthenticateRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_add(#'Etcd.AuthUserAddRequest'{}) ->
    {ok, #'Etcd.AuthUserAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_add(Request) when is_record(Request, 'Etcd.AuthUserAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', []).
-spec user_add(#'Etcd.AuthUserAddRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_add(Request, Token) when is_record(Request, 'Etcd.AuthUserAddRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', [{<<"authorization">>, Token}]);
user_add(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserAddRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_get(#'Etcd.AuthUserGetRequest'{}) ->
    {ok, #'Etcd.AuthUserGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_get(Request) when is_record(Request, 'Etcd.AuthUserGetRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', []).
-spec user_get(#'Etcd.AuthUserGetRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_get(Request, Token) when is_record(Request, 'Etcd.AuthUserGetRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', [{<<"authorization">>, Token}]);
user_get(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserGetRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_list(#'Etcd.AuthUserListRequest'{}) ->
    {ok, #'Etcd.AuthUserListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_list(Request) when is_record(Request, 'Etcd.AuthUserListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', []).
-spec user_list(#'Etcd.AuthUserListRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_list(Request, Token) when is_record(Request, 'Etcd.AuthUserListRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', [{<<"authorization">>, Token}]);
user_list(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserListRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_delete(#'Etcd.AuthUserDeleteRequest'{}) ->
    {ok, #'Etcd.AuthUserDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_delete(Request) when is_record(Request, 'Etcd.AuthUserDeleteRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', []).
-spec user_delete(#'Etcd.AuthUserDeleteRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_delete(Request, Token) when is_record(Request, 'Etcd.AuthUserDeleteRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', [{<<"authorization">>, Token}]);
user_delete(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserDeleteRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_change_password(#'Etcd.AuthUserChangePasswordRequest'{}) ->
    {ok, #'Etcd.AuthUserChangePasswordResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_change_password(Request) when is_record(Request, 'Etcd.AuthUserChangePasswordRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', []).
-spec user_change_password(#'Etcd.AuthUserChangePasswordRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserChangePasswordResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_change_password(Request, Token) when is_record(Request, 'Etcd.AuthUserChangePasswordRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', [{<<"authorization">>, Token}]);
user_change_password(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserChangePasswordRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_grant_role(#'Etcd.AuthUserGrantRoleRequest'{}) ->
    {ok, #'Etcd.AuthUserGrantRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_grant_role(Request) when is_record(Request, 'Etcd.AuthUserGrantRoleRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', []).
-spec user_grant_role(#'Etcd.AuthUserGrantRoleRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserGrantRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_grant_role(Request, Token) when is_record(Request, 'Etcd.AuthUserGrantRoleRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', [{<<"authorization">>, Token}]);
user_grant_role(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserGrantRoleRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_revoke_role(#'Etcd.AuthUserRevokeRoleRequest'{}) ->
    {ok, #'Etcd.AuthUserRevokeRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_revoke_role(Request) when is_record(Request, 'Etcd.AuthUserRevokeRoleRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', []).
-spec user_revoke_role(#'Etcd.AuthUserRevokeRoleRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthUserRevokeRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_revoke_role(Request, Token) when is_record(Request, 'Etcd.AuthUserRevokeRoleRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', [{<<"authorization">>, Token}]);
user_revoke_role(Request, Http2Headers) when is_record(Request, 'Etcd.AuthUserRevokeRoleRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_add(#'Etcd.AuthRoleAddRequest'{}) ->
    {ok, #'Etcd.AuthRoleAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_add(Request) when is_record(Request, 'Etcd.AuthRoleAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', []).
-spec role_add(#'Etcd.AuthRoleAddRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_add(Request, Token) when is_record(Request, 'Etcd.AuthRoleAddRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', [{<<"authorization">>, Token}]);
role_add(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleAddRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_get(#'Etcd.AuthRoleGetRequest'{}) ->
    {ok, #'Etcd.AuthRoleGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_get(Request) when is_record(Request, 'Etcd.AuthRoleGetRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', []).
-spec role_get(#'Etcd.AuthRoleGetRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_get(Request, Token) when is_record(Request, 'Etcd.AuthRoleGetRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', [{<<"authorization">>, Token}]);
role_get(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleGetRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_list(#'Etcd.AuthRoleListRequest'{}) ->
    {ok, #'Etcd.AuthRoleListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_list(Request) when is_record(Request, 'Etcd.AuthRoleListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', []).
-spec role_list(#'Etcd.AuthRoleListRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_list(Request, Token) when is_record(Request, 'Etcd.AuthRoleListRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', [{<<"authorization">>, Token}]);
role_list(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleListRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_delete(#'Etcd.AuthRoleDeleteRequest'{}) ->
    {ok, #'Etcd.AuthRoleDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_delete(Request) when is_record(Request, 'Etcd.AuthRoleDeleteRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', []).
-spec role_delete(#'Etcd.AuthRoleDeleteRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_delete(Request, Token) when is_record(Request, 'Etcd.AuthRoleDeleteRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', [{<<"authorization">>, Token}]);
role_delete(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleDeleteRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_grant_permission(#'Etcd.AuthRoleGrantPermissionRequest'{}) ->
    {ok, #'Etcd.AuthRoleGrantPermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_grant_permission(Request) when is_record(Request, 'Etcd.AuthRoleGrantPermissionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', []).
-spec role_grant_permission(#'Etcd.AuthRoleGrantPermissionRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleGrantPermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_grant_permission(Request, Token) when is_record(Request, 'Etcd.AuthRoleGrantPermissionRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', [{<<"authorization">>, Token}]);
role_grant_permission(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleGrantPermissionRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_revoke_permission(#'Etcd.AuthRoleRevokePermissionRequest'{}) ->
    {ok, #'Etcd.AuthRoleRevokePermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_revoke_permission(Request) when is_record(Request, 'Etcd.AuthRoleRevokePermissionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', []).
-spec role_revoke_permission(#'Etcd.AuthRoleRevokePermissionRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AuthRoleRevokePermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_revoke_permission(Request, Token) when is_record(Request, 'Etcd.AuthRoleRevokePermissionRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', [{<<"authorization">>, Token}]);
role_revoke_permission(Request, Http2Headers) when is_record(Request, 'Etcd.AuthRoleRevokePermissionRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', Http2Headers).

