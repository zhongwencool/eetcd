%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-06T08:17:27+00:00 and should not be modified manually

-module(eetcd_auth_gen).

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
-spec auth_enable(router_pb:'Etcd.AuthEnableRequest'()) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_enable(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', []).
-spec auth_enable(router_pb:'Etcd.AuthEnableRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
auth_enable(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', [{<<"authorization">>, Token}]);
auth_enable(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', Http2Headers).

%% @doc Unary RPC 
-spec auth_disable(router_pb:'Etcd.AuthDisableRequest'()) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_disable(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', []).
-spec auth_disable(router_pb:'Etcd.AuthDisableRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
auth_disable(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', [{<<"authorization">>, Token}]);
auth_disable(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', Http2Headers).

%% @doc Unary RPC 
-spec authenticate(router_pb:'Etcd.AuthenticateRequest'()) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
authenticate(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', []).
-spec authenticate(router_pb:'Etcd.AuthenticateRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
authenticate(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', [{<<"authorization">>, Token}]);
authenticate(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_add(router_pb:'Etcd.AuthUserAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', []).
-spec user_add(router_pb:'Etcd.AuthUserAddRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_add(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', [{<<"authorization">>, Token}]);
user_add(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_get(router_pb:'Etcd.AuthUserGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_get(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', []).
-spec user_get(router_pb:'Etcd.AuthUserGetRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_get(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', [{<<"authorization">>, Token}]);
user_get(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_list(router_pb:'Etcd.AuthUserListRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', []).
-spec user_list(router_pb:'Etcd.AuthUserListRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_list(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', [{<<"authorization">>, Token}]);
user_list(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_delete(router_pb:'Etcd.AuthUserDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_delete(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', []).
-spec user_delete(router_pb:'Etcd.AuthUserDeleteRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_delete(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', [{<<"authorization">>, Token}]);
user_delete(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_change_password(router_pb:'Etcd.AuthUserChangePasswordRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_change_password(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', []).
-spec user_change_password(router_pb:'Etcd.AuthUserChangePasswordRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_change_password(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', [{<<"authorization">>, Token}]);
user_change_password(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_grant_role(router_pb:'Etcd.AuthUserGrantRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_grant_role(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', []).
-spec user_grant_role(router_pb:'Etcd.AuthUserGrantRoleRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_grant_role(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', [{<<"authorization">>, Token}]);
user_grant_role(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', Http2Headers).

%% @doc Unary RPC 
-spec user_revoke_role(router_pb:'Etcd.AuthUserRevokeRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_revoke_role(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', []).
-spec user_revoke_role(router_pb:'Etcd.AuthUserRevokeRoleRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
user_revoke_role(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', [{<<"authorization">>, Token}]);
user_revoke_role(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_add(router_pb:'Etcd.AuthRoleAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', []).
-spec role_add(router_pb:'Etcd.AuthRoleAddRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_add(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', [{<<"authorization">>, Token}]);
role_add(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_get(router_pb:'Etcd.AuthRoleGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_get(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', []).
-spec role_get(router_pb:'Etcd.AuthRoleGetRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_get(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', [{<<"authorization">>, Token}]);
role_get(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_list(router_pb:'Etcd.AuthRoleListRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', []).
-spec role_list(router_pb:'Etcd.AuthRoleListRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_list(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', [{<<"authorization">>, Token}]);
role_list(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_delete(router_pb:'Etcd.AuthRoleDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_delete(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', []).
-spec role_delete(router_pb:'Etcd.AuthRoleDeleteRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_delete(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', [{<<"authorization">>, Token}]);
role_delete(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_grant_permission(router_pb:'Etcd.AuthRoleGrantPermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_grant_permission(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', []).
-spec role_grant_permission(router_pb:'Etcd.AuthRoleGrantPermissionRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_grant_permission(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', [{<<"authorization">>, Token}]);
role_grant_permission(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', Http2Headers).

%% @doc Unary RPC 
-spec role_revoke_permission(router_pb:'Etcd.AuthRoleRevokePermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_revoke_permission(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', []).
-spec role_revoke_permission(router_pb:'Etcd.AuthRoleRevokePermissionRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
role_revoke_permission(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', [{<<"authorization">>, Token}]);
role_revoke_permission(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', Http2Headers).

