-module(eetcd_auth_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([auth/1, user/1]).

-define(Name, ?MODULE).

%% First create root user with root role
%% ETCDCTL_API=3 etcdctl user add root
%% ETCDCTL_API=3 etcdctl user grant-role root root

suite() ->
    [{timetrap, {minutes, 3}}].

%% all() -> [auth, user].
all() -> [].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

auth(Name) ->
    {ok, #{}} = eetcd_auth:auth_enable(Name),
    {ok, #{}} = eetcd_auth:auth_disable(Name),
    ok.

user(Name) ->
    {ok, #{}} = eetcd_auth:auth_enable(Name),
    %% list
    {ok, AuthRoleListResponse} = eetcd_auth:user_list(Name),
    %% add
    {ok, AuthRoleAddResponse} = eetcd_auth:user_add(Name, <<"name1">>, <<"12345">>),
    %% get
    {ok, AuthUserGetResponse} = eetcd_auth:user_get(Name, <<"name1">>),
    %% change password
    {ok, AuthUserChangePasswordResponse} = eetcd_auth:user_change_password(Name, <<"name1">>,<<"54321">>),
    %% get
    {ok, AuthUserGetResponse1} = eetcd_auth:user_get(Name, <<"name1">>),
    %% delete
    {ok, AuthUserDeleteResponse} = eetcd_auth:user_delete(Name, <<"name1">>),
    %% list
    {ok, AuthRoleListResponse2} = eetcd_auth:user_list(Name),
    {ok, AuthDisableResponse2} = eetcd_auth:auth_disable(Name),
    ok.

