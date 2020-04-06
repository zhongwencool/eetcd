%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-04-02T02:00:45+00:00 and should not be modified manually

-module(eetcd_auth_gen).

-export([auth_enable/1]).
-export([auth_disable/1]).
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
-spec auth_enable(router_pb:'Etcd.AuthEnableRequest'()) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()}|{error,eetcd:eetcd_error()}.
auth_enable(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthEnableRequest', <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/AuthDisable" 
-spec auth_disable(router_pb:'Etcd.AuthDisableRequest'()) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()}|{error,eetcd:eetcd_error()}.
auth_disable(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthDisableRequest', <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/Authenticate" 
-spec authenticate(router_pb:'Etcd.AuthenticateRequest'()) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()}|{error,eetcd:eetcd_error()}.
authenticate(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthenticateRequest', <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserAdd" 
-spec user_add(router_pb:'Etcd.AuthUserAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()}|{error,eetcd:eetcd_error()}.
user_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserAddRequest', <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserGet" 
-spec user_get(router_pb:'Etcd.AuthUserGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()}|{error,eetcd:eetcd_error()}.
user_get(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGetRequest', <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserList" 
-spec user_list(router_pb:'Etcd.AuthUserListRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()}|{error,eetcd:eetcd_error()}.
user_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserListRequest', <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserDelete" 
-spec user_delete(router_pb:'Etcd.AuthUserDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()}|{error,eetcd:eetcd_error()}.
user_delete(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserDeleteRequest', <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserChangePassword" 
-spec user_change_password(router_pb:'Etcd.AuthUserChangePasswordRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()}|{error,eetcd:eetcd_error()}.
user_change_password(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserChangePasswordRequest', <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserGrantRole" 
-spec user_grant_role(router_pb:'Etcd.AuthUserGrantRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()}|{error,eetcd:eetcd_error()}.
user_grant_role(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserGrantRoleRequest', <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/UserRevokeRole" 
-spec user_revoke_role(router_pb:'Etcd.AuthUserRevokeRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()}|{error,eetcd:eetcd_error()}.
user_revoke_role(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthUserRevokeRoleRequest', <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleAdd" 
-spec role_add(router_pb:'Etcd.AuthRoleAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()}|{error,eetcd:eetcd_error()}.
role_add(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleAddRequest', <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleGet" 
-spec role_get(router_pb:'Etcd.AuthRoleGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()}|{error,eetcd:eetcd_error()}.
role_get(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGetRequest', <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleList" 
-spec role_list(router_pb:'Etcd.AuthRoleListRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()}|{error,eetcd:eetcd_error()}.
role_list(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleListRequest', <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleDelete" 
-spec role_delete(router_pb:'Etcd.AuthRoleDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()}|{error,eetcd:eetcd_error()}.
role_delete(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleDeleteRequest', <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleGrantPermission" 
-spec role_grant_permission(router_pb:'Etcd.AuthRoleGrantPermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()}|{error,eetcd:eetcd_error()}.
role_grant_permission(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleGrantPermissionRequest', <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Auth/RoleRevokePermission" 
-spec role_revoke_permission(router_pb:'Etcd.AuthRoleRevokePermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()}|{error,eetcd:eetcd_error()}.
role_revoke_permission(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AuthRoleRevokePermissionRequest', <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse').

