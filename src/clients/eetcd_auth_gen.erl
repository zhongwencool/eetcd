%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_auth_gen).

-export([auth_enable/2, auth_enable/3]).
-export([auth_disable/2, auth_disable/3]).
-export([authenticate/2, authenticate/3]).
-export([user_add/2, user_add/3]).
-export([user_get/2, user_get/3]).
-export([user_list/2, user_list/3]).
-export([user_delete/2, user_delete/3]).
-export([user_change_password/2, user_change_password/3]).
-export([user_grant_role/2, user_grant_role/3]).
-export([user_revoke_role/2, user_revoke_role/3]).
-export([role_add/2, role_add/3]).
-export([role_get/2, role_get/3]).
-export([role_list/2, role_list/3]).
-export([role_delete/2, role_delete/3]).
-export([role_grant_permission/2, role_grant_permission/3]).
-export([role_revoke_permission/2, role_revoke_permission/3]).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/AuthEnable'
-spec auth_enable(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthEnableRequest'()) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()} | {error, eetcd:eetcd_error()}.
auth_enable(Client, Request) ->
    auth_enable(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/AuthEnable'
-spec auth_enable(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthEnableRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()} | {error, eetcd:eetcd_error()}.
auth_enable(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/AuthDisable'
-spec auth_disable(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthDisableRequest'()) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()} | {error, eetcd:eetcd_error()}.
auth_disable(Client, Request) ->
    auth_disable(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/AuthDisable'
-spec auth_disable(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthDisableRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()} | {error, eetcd:eetcd_error()}.
auth_disable(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/Authenticate'
-spec authenticate(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthenticateRequest'()) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()} | {error, eetcd:eetcd_error()}.
authenticate(Client, Request) ->
    authenticate(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/Authenticate'
-spec authenticate(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthenticateRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()} | {error, eetcd:eetcd_error()}.
authenticate(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserAdd'
-spec user_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()} | {error, eetcd:eetcd_error()}.
user_add(Client, Request) ->
    user_add(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserAdd'
-spec user_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserAddRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()} | {error, eetcd:eetcd_error()}.
user_add(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserGet'
-spec user_get(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()} | {error, eetcd:eetcd_error()}.
user_get(Client, Request) ->
    user_get(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserGet'
-spec user_get(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserGetRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()} | {error, eetcd:eetcd_error()}.
user_get(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserList'
-spec user_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserListRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()} | {error, eetcd:eetcd_error()}.
user_list(Client, Request) ->
    user_list(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserList'
-spec user_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserListRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()} | {error, eetcd:eetcd_error()}.
user_list(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserDelete'
-spec user_delete(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()} | {error, eetcd:eetcd_error()}.
user_delete(Client, Request) ->
    user_delete(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserDelete'
-spec user_delete(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserDeleteRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()} | {error, eetcd:eetcd_error()}.
user_delete(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserChangePassword'
-spec user_change_password(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserChangePasswordRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()} | {error, eetcd:eetcd_error()}.
user_change_password(Client, Request) ->
    user_change_password(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserChangePassword'
-spec user_change_password(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserChangePasswordRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()} | {error, eetcd:eetcd_error()}.
user_change_password(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserGrantRole'
-spec user_grant_role(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserGrantRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()} | {error, eetcd:eetcd_error()}.
user_grant_role(Client, Request) ->
    user_grant_role(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserGrantRole'
-spec user_grant_role(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserGrantRoleRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()} | {error, eetcd:eetcd_error()}.
user_grant_role(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserRevokeRole'
-spec user_revoke_role(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserRevokeRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()} | {error, eetcd:eetcd_error()}.
user_revoke_role(Client, Request) ->
    user_revoke_role(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/UserRevokeRole'
-spec user_revoke_role(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthUserRevokeRoleRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()} | {error, eetcd:eetcd_error()}.
user_revoke_role(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleAdd'
-spec role_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()} | {error, eetcd:eetcd_error()}.
role_add(Client, Request) ->
    role_add(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleAdd'
-spec role_add(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleAddRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()} | {error, eetcd:eetcd_error()}.
role_add(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleGet'
-spec role_get(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()} | {error, eetcd:eetcd_error()}.
role_get(Client, Request) ->
    role_get(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleGet'
-spec role_get(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleGetRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()} | {error, eetcd:eetcd_error()}.
role_get(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleList'
-spec role_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleListRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()} | {error, eetcd:eetcd_error()}.
role_list(Client, Request) ->
    role_list(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleList'
-spec role_list(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleListRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()} | {error, eetcd:eetcd_error()}.
role_list(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleDelete'
-spec role_delete(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()} | {error, eetcd:eetcd_error()}.
role_delete(Client, Request) ->
    role_delete(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleDelete'
-spec role_delete(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleDeleteRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()} | {error, eetcd:eetcd_error()}.
role_delete(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleGrantPermission'
-spec role_grant_permission(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleGrantPermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()} | {error, eetcd:eetcd_error()}.
role_grant_permission(Client, Request) ->
    role_grant_permission(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleGrantPermission'
-spec role_grant_permission(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleGrantPermissionRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()} | {error, eetcd:eetcd_error()}.
role_grant_permission(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleRevokePermission'
-spec role_revoke_permission(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleRevokePermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()} | {error, eetcd:eetcd_error()}.
role_revoke_permission(Client, Request) ->
    role_revoke_permission(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Auth/RoleRevokePermission'
-spec role_revoke_permission(Client :: eetcd:client(), Request :: router_pb:'Etcd.AuthRoleRevokePermissionRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()} | {error, eetcd:eetcd_error()}.
role_revoke_permission(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse', router_pb, Opts).

