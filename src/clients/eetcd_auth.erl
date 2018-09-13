%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Auth.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_auth).

-include("router_pb.hrl").
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
-spec auth_enable(#'Etcd.AuthEnableRequest'{}) ->
    {ok, #'Etcd.AuthEnableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_enable(Request)when is_record(Request, 'Etcd.AuthEnableRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthEnable">>, 'Etcd.AuthEnableResponse').

%% @doc Unary RPC 
-spec auth_disable(#'Etcd.AuthDisableRequest'{}) ->
    {ok, #'Etcd.AuthDisableResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
auth_disable(Request)when is_record(Request, 'Etcd.AuthDisableRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/AuthDisable">>, 'Etcd.AuthDisableResponse').

%% @doc Unary RPC 
-spec authenticate(#'Etcd.AuthenticateRequest'{}) ->
    {ok, #'Etcd.AuthenticateResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
authenticate(Request)when is_record(Request, 'Etcd.AuthenticateRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/Authenticate">>, 'Etcd.AuthenticateResponse').

%% @doc Unary RPC 
-spec user_add(#'Etcd.AuthUserAddRequest'{}) ->
    {ok, #'Etcd.AuthUserAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_add(Request)when is_record(Request, 'Etcd.AuthUserAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserAdd">>, 'Etcd.AuthUserAddResponse').

%% @doc Unary RPC 
-spec user_get(#'Etcd.AuthUserGetRequest'{}) ->
    {ok, #'Etcd.AuthUserGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_get(Request)when is_record(Request, 'Etcd.AuthUserGetRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGet">>, 'Etcd.AuthUserGetResponse').

%% @doc Unary RPC 
-spec user_list(#'Etcd.AuthUserListRequest'{}) ->
    {ok, #'Etcd.AuthUserListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_list(Request)when is_record(Request, 'Etcd.AuthUserListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserList">>, 'Etcd.AuthUserListResponse').

%% @doc Unary RPC 
-spec user_delete(#'Etcd.AuthUserDeleteRequest'{}) ->
    {ok, #'Etcd.AuthUserDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_delete(Request)when is_record(Request, 'Etcd.AuthUserDeleteRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserDelete">>, 'Etcd.AuthUserDeleteResponse').

%% @doc Unary RPC 
-spec user_change_password(#'Etcd.AuthUserChangePasswordRequest'{}) ->
    {ok, #'Etcd.AuthUserChangePasswordResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_change_password(Request)when is_record(Request, 'Etcd.AuthUserChangePasswordRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserChangePassword">>, 'Etcd.AuthUserChangePasswordResponse').

%% @doc Unary RPC 
-spec user_grant_role(#'Etcd.AuthUserGrantRoleRequest'{}) ->
    {ok, #'Etcd.AuthUserGrantRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_grant_role(Request)when is_record(Request, 'Etcd.AuthUserGrantRoleRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserGrantRole">>, 'Etcd.AuthUserGrantRoleResponse').

%% @doc Unary RPC 
-spec user_revoke_role(#'Etcd.AuthUserRevokeRoleRequest'{}) ->
    {ok, #'Etcd.AuthUserRevokeRoleResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
user_revoke_role(Request)when is_record(Request, 'Etcd.AuthUserRevokeRoleRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/UserRevokeRole">>, 'Etcd.AuthUserRevokeRoleResponse').

%% @doc Unary RPC 
-spec role_add(#'Etcd.AuthRoleAddRequest'{}) ->
    {ok, #'Etcd.AuthRoleAddResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_add(Request)when is_record(Request, 'Etcd.AuthRoleAddRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleAdd">>, 'Etcd.AuthRoleAddResponse').

%% @doc Unary RPC 
-spec role_get(#'Etcd.AuthRoleGetRequest'{}) ->
    {ok, #'Etcd.AuthRoleGetResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_get(Request)when is_record(Request, 'Etcd.AuthRoleGetRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGet">>, 'Etcd.AuthRoleGetResponse').

%% @doc Unary RPC 
-spec role_list(#'Etcd.AuthRoleListRequest'{}) ->
    {ok, #'Etcd.AuthRoleListResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_list(Request)when is_record(Request, 'Etcd.AuthRoleListRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleList">>, 'Etcd.AuthRoleListResponse').

%% @doc Unary RPC 
-spec role_delete(#'Etcd.AuthRoleDeleteRequest'{}) ->
    {ok, #'Etcd.AuthRoleDeleteResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_delete(Request)when is_record(Request, 'Etcd.AuthRoleDeleteRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleDelete">>, 'Etcd.AuthRoleDeleteResponse').

%% @doc Unary RPC 
-spec role_grant_permission(#'Etcd.AuthRoleGrantPermissionRequest'{}) ->
    {ok, #'Etcd.AuthRoleGrantPermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_grant_permission(Request)when is_record(Request, 'Etcd.AuthRoleGrantPermissionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleGrantPermission">>, 'Etcd.AuthRoleGrantPermissionResponse').

%% @doc Unary RPC 
-spec role_revoke_permission(#'Etcd.AuthRoleRevokePermissionRequest'{}) ->
    {ok, #'Etcd.AuthRoleRevokePermissionResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
role_revoke_permission(Request)when is_record(Request, 'Etcd.AuthRoleRevokePermissionRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Auth/RoleRevokePermission">>, 'Etcd.AuthRoleRevokePermissionResponse').

