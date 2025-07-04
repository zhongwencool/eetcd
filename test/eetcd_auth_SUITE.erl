-module(eetcd_auth_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([auth/1, user/1]).

-define(Name, ?MODULE).

%% First create root user with root role
%% ETCDCTL_API=3 etcdctl user add root
%% ETCDCTL_API=3 etcdctl user grant-role root root

suite() ->
    [{timetrap, {minutes, 3}}].

%% todo
%% all() -> [auth, user].
all() -> [].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    eetcd_auth_gen:user_add(?Name, #{name => "root", options => #{no_password => true}}),
    eetcd_auth_gen:user_grant_role(?Name, #{user => "root", role => "root"}),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

auth(_Config) ->
    {ok, #{}} = eetcd_auth_gen:auth_enable(?Name, #{}),
    {ok, #{}} = eetcd_auth_gen:auth_disable(?Name, #{}),
    ok.

user(EtcdName) ->
    {ok, #{}} = eetcd_auth_gen:auth_enable(EtcdName, #{}),
    %% list
    {ok, _AuthRoleListResponse} = eetcd_auth_gen:user_list(EtcdName, #{}),
    %% add
    {ok, _AuthRoleAddResponse} = eetcd_auth_gen:user_add(EtcdName, #{name => <<"name1">>,
                                                                     password => <<"12345">>}),
    %% get
    {ok, _AuthUserGetResponse} = eetcd_auth_gen:user_get(EtcdName, #{name => <<"name1">>}),
    %% change password
    {ok, _AuthUserChangePasswordResponse} =
        eetcd_auth_gen:user_change_password(EtcdName, #{name => <<"name1">>,
                                                        password => <<"54321">>}),
    %% get
    {ok, _AuthUserGetResponse1} = eetcd_auth_gen:user_get(EtcdName, #{name => <<"name1">>}),
    %% delete
    {ok, _AuthUserDeleteResponse} = eetcd_auth_gen:user_delete(EtcdName, #{name => <<"name1">>}),
    %% list
    {ok, _AuthRoleListResponse2} = eetcd_auth_gen:user_list(EtcdName, #{}),
    {ok, _AuthDisableResponse2} = eetcd_auth_gen:auth_disable(EtcdName, #{}),
    ok.

