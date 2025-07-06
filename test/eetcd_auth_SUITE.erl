-module(eetcd_auth_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([auth/1, user/1]).

-define(Name, ?MODULE).
-define(ENDPOINTS, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]).
-define(USERNAME, <<"root">>).
-define(PASSWORD, <<"password">>).

%% First create root user with root role
%% ETCDCTL_API=3 etcdctl user add root
%% ETCDCTL_API=3 etcdctl user grant-role root root

suite() ->
    [{timetrap, {minutes, 3}}].

%% todo
all() -> [auth, user].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ?ENDPOINTS),
    eetcd_auth_gen:user_add(?Name, #{name => ?USERNAME, password => ?PASSWORD}),
    eetcd_auth_gen:user_grant_role(?Name, #{user => "root", role => "root"}),
    eetcd:close(?Name),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

auth(_Config) ->
    {ok, _} = eetcd:open(?Name, ?ENDPOINTS),
    {ok, #{}} = eetcd_auth_gen:auth_enable(?Name, #{}),
    eetcd:close(?Name),

    {ok, _} = eetcd:open(?Name, ?ENDPOINTS, [{name, ?USERNAME}, {password, ?PASSWORD}]),
    {ok, #{}} = eetcd_auth_gen:auth_disable(?Name, #{}),
    eetcd:close(?Name),
    ok.

user(_Config) ->
    EtcdName = user_client, %% The etcd client name cannot be `user' because there is already
                            %% a system process registered with `user' name.
    {ok, _} = eetcd:open(EtcdName, ?ENDPOINTS),
    {ok, #{}} = eetcd_auth_gen:auth_enable(EtcdName, #{}),
    eetcd:close(EtcdName),

    {ok, _} = eetcd:open(EtcdName, ?ENDPOINTS, [{name, ?USERNAME}, {password, ?PASSWORD}]),
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
    eetcd:close(EtcdName),
    ok.
