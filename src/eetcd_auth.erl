-module(eetcd_auth).
-include("eetcd.hrl").

%% API
-export([auth_enable/1, auth_disable/1]).

%% -export([authenticate/1]).
-export([user_add/2, user_add/3, user_get/2,
    user_list/1, user_delete/2, user_change_password/3,
    user_grant_role/3, user_revoke_role/3
]).
-export([role_add/2, role_get/2, role_list/1, role_delete/2,
    role_grant_permission/5, role_revoke_permission/4
]).

%%% @doc AuthEnable enables auth of an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:auth_enable(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:auth_enable()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
auth_enable(Context) ->
    eetcd_auth_gen:auth_enable(eetcd:new(Context)).

%%% @doc AuthDisable disables auth of an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:auth_disable(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:auth_disable()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
auth_disable(Context) ->
    eetcd_auth_gen:auth_disable(eetcd:new(Context)).

%%% @doc UserAdd adds a new user with password to an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_add(ConnName, Name, Password)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_add(name, password)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_add(Context, Name, Password) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    C3 = maps:put(password, Password, C2),
    eetcd_auth_gen:user_add(C3).

%%% @doc UserAdd adds a new user without password to an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_add(ConnName, Name)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_add(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_add(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    C3 = maps:put(options, #{no_password => true}, C2),
    eetcd_auth_gen:user_add(C3).

%%% @doc UserDelete deletes a user from an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_delete(ConnName, Name)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_delete(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_delete(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    eetcd_auth_gen:user_add(C2).

%%% @doc UserChangePassword changes a password of a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_change_password(ConnName, Name, Password)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_change_password(name, password)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_change_password(Context, Name, Password) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    C3 = maps:put(password, Password, C2),
    eetcd_auth_gen:user_change_password(C3).

%%% @doc UserGrantRole grants a role to a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_grant_role(ConnName, User, Role)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_grant_role(user, role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_grant_role(Context, User, Role) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(user, User, C1),
    C3 = maps:put(role, Role, C2),
    eetcd_auth_gen:user_grant_role(C3).

%%% @doc UserGet gets a detailed information of a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_get(ConnName, Name)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_get(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_get(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    eetcd_auth_gen:user_get(C2).

%%% @doc UserList gets a list of all users.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_list(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_list()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_list(Context) ->
    eetcd_auth_gen:user_list(eetcd:new(Context)).

%%% @doc UserRevokeRole revokes a role of a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_revoke_role(ConnName, Name, Role)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_revoke_role(name, role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
user_revoke_role(Context, Name, Role) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    C3 = maps:put(role, Role, C2),
    eetcd_auth_gen:user_revoke_role(C3).

%%% @doc RoleAdd adds a new role to an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_add(ConnName, Name)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:role_add(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_add(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    eetcd_auth_gen:role_add(C2).

%%% @doc RoleGrantPermission grants a permission to a role.
%%% PermType: 'READ' | 'WRITE' | 'READWRITE' |
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_grant_permission(ConnName, Name, Key, RangeEnd, 'READ')'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:user_add(name, key, rangeEnd, :'WRITE')
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_grant_permission(Context, Name, Key, RangeEnd, PermType) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    Permission = #{key => Key, range_end => RangeEnd, permType => PermType},
    C3 = maps:put(perm, Permission, C2),
    eetcd_auth_gen:role_grant_permission(C3).

%%% @doc RoleGet gets a detailed information of a role.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_get(ConnName, Role)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:role_get(role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_get(Context, Role) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(role, Role, C1),
    eetcd_auth_gen:role_get(C2).

%%% @doc RoleList gets a list of all roles.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_list(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:role_list()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_list(Context) ->
    eetcd_auth_gen:role_list(eetcd:new(Context)).

%%% @doc RoleRevokePermission revokes a permission from a role.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_revoke_permission(ConnName, Role, Key, RangeEnd)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(ConnName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:role_revoke_permission(role, key, rangeEnd)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_revoke_permission(Context, Role, Key, RangeEnd) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(role, Role, C1),
    C3 = maps:put(key, Key, C2),
    C4 = maps:put(range_end, RangeEnd, C3),
    eetcd_auth_gen:role_revoke_permission(C4).

%%% @doc RoleDelete deletes a role.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_delete(ConnName, Role)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% eetcd:new(connName)
%%% |> eetcd:with_timeout(6000)
%%% |> eetcd_cluster:role_delete(role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd:with_timeout/2} {@link eetcd:new/1}
%%% @end
role_delete(Context, Role) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(role, Role, C1),
    eetcd_auth_gen:role_delete(C2).
