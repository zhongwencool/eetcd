%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.Auth
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_auth_gen).

-export([auth_enable/1]).
-export([auth_disable/1]).
-export([auth_status/1]).
-export([authenticate/1]).
-export([user_add/1]).
-export([user_get/1]).
-export([user_list/1]).
-export([user_delete/1]).
-export([user_change_password/1]).
-export([user_grant_role/1]).
-export([user_revoke_role/1]).
-export([role_add/1]).
-export([role_get/1]).
-export([role_list/1]).
-export([role_delete/1]).
-export([role_grant_permission/1]).
-export([role_revoke_permission/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/AuthEnable"
-spec auth_enable(rpc_pb:'etcdserverpb.AuthEnableRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthEnableResponse'()}|{error,eetcd:eetcd_error()}.
auth_enable(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'etcdserverpb.AuthEnableResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/AuthDisable"
-spec auth_disable(rpc_pb:'etcdserverpb.AuthDisableRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthDisableResponse'()}|{error,eetcd:eetcd_error()}.
auth_disable(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'etcdserverpb.AuthDisableResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/AuthStatus"
-spec auth_status(rpc_pb:'etcdserverpb.AuthStatusRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthStatusResponse'()}|{error,eetcd:eetcd_error()}.
auth_status(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthStatusRequest', <<"/etcdserverpb.Auth/AuthStatus">>, 'etcdserverpb.AuthStatusResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/Authenticate"
-spec authenticate(rpc_pb:'etcdserverpb.AuthenticateRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthenticateResponse'()}|{error,eetcd:eetcd_error()}.
authenticate(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'etcdserverpb.AuthenticateResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserAdd"
-spec user_add(rpc_pb:'etcdserverpb.AuthUserAddRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserAddResponse'()}|{error,eetcd:eetcd_error()}.
user_add(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'etcdserverpb.AuthUserAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserGet"
-spec user_get(rpc_pb:'etcdserverpb.AuthUserGetRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserGetResponse'()}|{error,eetcd:eetcd_error()}.
user_get(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'etcdserverpb.AuthUserGetResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserList"
-spec user_list(rpc_pb:'etcdserverpb.AuthUserListRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserListResponse'()}|{error,eetcd:eetcd_error()}.
user_list(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'etcdserverpb.AuthUserListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserDelete"
-spec user_delete(rpc_pb:'etcdserverpb.AuthUserDeleteRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserDeleteResponse'()}|{error,eetcd:eetcd_error()}.
user_delete(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'etcdserverpb.AuthUserDeleteResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserChangePassword"
-spec user_change_password(rpc_pb:'etcdserverpb.AuthUserChangePasswordRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserChangePasswordResponse'()}|{error,eetcd:eetcd_error()}.
user_change_password(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'etcdserverpb.AuthUserChangePasswordResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserGrantRole"
-spec user_grant_role(rpc_pb:'etcdserverpb.AuthUserGrantRoleRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserGrantRoleResponse'()}|{error,eetcd:eetcd_error()}.
user_grant_role(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'etcdserverpb.AuthUserGrantRoleResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserRevokeRole"
-spec user_revoke_role(rpc_pb:'etcdserverpb.AuthUserRevokeRoleRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthUserRevokeRoleResponse'()}|{error,eetcd:eetcd_error()}.
user_revoke_role(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'etcdserverpb.AuthUserRevokeRoleResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleAdd"
-spec role_add(rpc_pb:'etcdserverpb.AuthRoleAddRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleAddResponse'()}|{error,eetcd:eetcd_error()}.
role_add(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'etcdserverpb.AuthRoleAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleGet"
-spec role_get(rpc_pb:'etcdserverpb.AuthRoleGetRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleGetResponse'()}|{error,eetcd:eetcd_error()}.
role_get(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'etcdserverpb.AuthRoleGetResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleList"
-spec role_list(rpc_pb:'etcdserverpb.AuthRoleListRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleListResponse'()}|{error,eetcd:eetcd_error()}.
role_list(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'etcdserverpb.AuthRoleListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleDelete"
-spec role_delete(rpc_pb:'etcdserverpb.AuthRoleDeleteRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleDeleteResponse'()}|{error,eetcd:eetcd_error()}.
role_delete(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'etcdserverpb.AuthRoleDeleteResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleGrantPermission"
-spec role_grant_permission(rpc_pb:'etcdserverpb.AuthRoleGrantPermissionRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleGrantPermissionResponse'()}|{error,eetcd:eetcd_error()}.
role_grant_permission(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'etcdserverpb.AuthRoleGrantPermissionResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleRevokePermission"
-spec role_revoke_permission(rpc_pb:'etcdserverpb.AuthRoleRevokePermissionRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AuthRoleRevokePermissionResponse'()}|{error,eetcd:eetcd_error()}.
role_revoke_permission(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'etcdserverpb.AuthRoleRevokePermissionResponse').

