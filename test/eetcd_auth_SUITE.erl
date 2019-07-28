-module(eetcd_auth_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([auth/1, authenticate/1, user/1]).

-include("router_pb.hrl").

%% First create root user with root role
%% ETCDCTL_API=3 etcdctl user add root
%% ETCDCTL_API=3 etcdctl user grant-role root root

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    %% [auth, authenticate, user].
    [].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    Config.

end_per_suite(_Config) ->
    application:stop(eetcd),
    ok.

auth(_Config) ->
    {ok, #'Etcd.AuthEnableResponse'{}} = eetcd_auth:auth_enable(#'Etcd.AuthEnableRequest'{}),
    {ok, #'Etcd.AuthDisableResponse'{}} = eetcd_auth:auth_disable(#'Etcd.AuthDisableRequest'{}),
    ok.

authenticate(_Config) ->
    Request = #'Etcd.AuthenticateRequest'{name = <<"NoExist">>, password = <<"123">>},
    {error, {grpc_error, 9, _}} =
        eetcd_auth:authenticate(Request),
    {ok, #'Etcd.AuthEnableResponse'{}} = eetcd_auth:auth_enable(#'Etcd.AuthEnableRequest'{}),
    {ok, #'Etcd.AuthenticateResponse'{}} = eetcd_auth:authenticate(Request),
    {ok, #'Etcd.AuthDisableResponse'{}} = eetcd_auth:auth_disable(#'Etcd.AuthDisableRequest'{}),
    ok.

user(_Config) ->
    {ok, #'Etcd.AuthEnableResponse'{}} = eetcd_auth:auth_enable(#'Etcd.AuthEnableRequest'{}),
    %% list
    {ok, #'Etcd.AuthRoleListResponse'{roles = []}} = eetcd_auth:user_list(#'Etcd.AuthRoleListRequest'{}),
    %% add
    {ok, #'Etcd.AuthRoleAddResponse'{}} = eetcd_auth:user_add(#'Etcd.AuthUserAddRequest'{name = <<"name1">>, password = <<"12345">>}),
    %% get
    {ok, #'Etcd.AuthUserGetResponse'{roles = [<<"name1">>]}} = eetcd_auth:user_get(#'Etcd.AuthUserGetRequest'{name = <<"name1">>}),
    %% change password
    {ok, #'Etcd.AuthUserChangePasswordResponse'{}}
        = eetcd_auth:user_get(#'Etcd.AuthUserChangePasswordRequest'{name = <<"name1">>, password = <<"54321">>}),
    %% get
    {ok, #'Etcd.AuthUserGetResponse'{roles = [<<"name1">>]}} = eetcd_auth:user_get(#'Etcd.AuthUserGetRequest'{name = <<"name1">>}),
    %% delete
    {ok, #'Etcd.AuthUserDeleteResponse'{}} = eetcd_auth:user_delete(#'Etcd.AuthUserDeleteRequest'{name = <<"name1">>}),
    %% list
    {ok, #'Etcd.AuthRoleListResponse'{roles = []}} = eetcd_auth:user_list(#'Etcd.AuthRoleListRequest'{}),
    {ok, #'Etcd.AuthDisableResponse'{}} = eetcd_auth:auth_disable(#'Etcd.AuthDisableRequest'{}),
    ok.

