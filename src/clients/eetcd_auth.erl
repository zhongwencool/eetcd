%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-11T07:54:57+00:00 and should not be modified manually

-module(eetcd_auth).

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

%% @doc Unary RPC 
-spec auth_enable(router_pb:'Etcd.AuthEnableRequest'()) ->
    {ok, router_pb:'Etcd.AuthEnableResponse'()} | {error, term()}.
auth_enable(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse').

%% @doc Unary RPC 
-spec auth_disable(router_pb:'Etcd.AuthDisableRequest'()) ->
    {ok, router_pb:'Etcd.AuthDisableResponse'()} | {error, term()}.
auth_disable(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse').

%% @doc Unary RPC 
-spec authenticate(router_pb:'Etcd.AuthenticateRequest'()) ->
    {ok, router_pb:'Etcd.AuthenticateResponse'()} | {error, term()}.
authenticate(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse').

%% @doc Unary RPC 
-spec user_add(router_pb:'Etcd.AuthUserAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserAddResponse'()} | {error, term()}.
user_add(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse').

%% @doc Unary RPC 
-spec user_get(router_pb:'Etcd.AuthUserGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGetResponse'()} | {error, term()}.
user_get(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse').

%% @doc Unary RPC 
-spec user_list(router_pb:'Etcd.AuthUserListRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserListResponse'()} | {error, term()}.
user_list(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse').

%% @doc Unary RPC 
-spec user_delete(router_pb:'Etcd.AuthUserDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserDeleteResponse'()} | {error, term()}.
user_delete(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse').

%% @doc Unary RPC 
-spec user_change_password(router_pb:'Etcd.AuthUserChangePasswordRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserChangePasswordResponse'()} | {error, term()}.
user_change_password(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse').

%% @doc Unary RPC 
-spec user_grant_role(router_pb:'Etcd.AuthUserGrantRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserGrantRoleResponse'()} | {error, term()}.
user_grant_role(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse').

%% @doc Unary RPC 
-spec user_revoke_role(router_pb:'Etcd.AuthUserRevokeRoleRequest'()) ->
    {ok, router_pb:'Etcd.AuthUserRevokeRoleResponse'()} | {error, term()}.
user_revoke_role(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse').

%% @doc Unary RPC 
-spec role_add(router_pb:'Etcd.AuthRoleAddRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleAddResponse'()} | {error, term()}.
role_add(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse').

%% @doc Unary RPC 
-spec role_get(router_pb:'Etcd.AuthRoleGetRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGetResponse'()} | {error, term()}.
role_get(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse').

%% @doc Unary RPC 
-spec role_list(router_pb:'Etcd.AuthRoleListRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleListResponse'()} | {error, term()}.
role_list(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse').

%% @doc Unary RPC 
-spec role_delete(router_pb:'Etcd.AuthRoleDeleteRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleDeleteResponse'()} | {error, term()}.
role_delete(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse').

%% @doc Unary RPC 
-spec role_grant_permission(router_pb:'Etcd.AuthRoleGrantPermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleGrantPermissionResponse'()} | {error, term()}.
role_grant_permission(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse').

%% @doc Unary RPC 
-spec role_revoke_permission(router_pb:'Etcd.AuthRoleRevokePermissionRequest'()) ->
    {ok, router_pb:'Etcd.AuthRoleRevokePermissionResponse'()} | {error, term()}.
role_revoke_permission(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse').

