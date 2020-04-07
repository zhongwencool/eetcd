-module(eetcd_auth).
-include("eetcd.hrl").

%% API
-export([new/1, with_timeout/2]).
-export([auth_enable/1, auth_disable/1]).
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.auth_enable()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec auth_enable(name()|context()) ->
    {ok,router_pb:'Etcd.AuthEnableResponse'()}|{error,eetcd_error()}.
auth_enable(Context) ->
    eetcd_auth_gen:auth_enable(new(Context)).

%%% @doc AuthDisable disables auth of an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:auth_disable(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.auth_disable()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec auth_disable(name()|context()) ->
    {ok,router_pb:'Etcd.AuthDisableResponse'()}|{error,eetcd_error()}.
auth_disable(Context) ->
    eetcd_auth_gen:auth_disable(new(Context)).

%%% @doc UserAdd adds a new user with password to an etcd cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_add(ConnName, Name, Password)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_add(name, password)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_add(name()|context(), iodata(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserAddResponse'()}|{error,eetcd_error()}.
user_add(Context, Name, Password) ->
    C1 = new(Context),
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_add(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_add(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserAddResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_delete(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_delete(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserDeleteResponse'()}|{error,eetcd_error()}.
user_delete(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    eetcd_auth_gen:user_delete(C2).

%%% @doc UserChangePassword changes a password of a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_change_password(ConnName, Name, Password)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_change_password(name, password)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_change_password(name()|context(), iodata(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserChangePasswordResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_grant_role(user, role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_grant_role(name()|context(), iodata(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserGrantRoleResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_get(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_get(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserGetResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> eetcd_auth:user_list()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_list(name()|context()) ->
    {ok,router_pb:'Etcd.AuthUserListResponse'()}|{error,eetcd_error()}.
user_list(Context) ->
    eetcd_auth_gen:user_list(eetcd:new(Context)).

%%% @doc UserRevokeRole revokes a role of a user.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:user_revoke_role(ConnName, Name, Role)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_revoke_role(name, role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec user_revoke_role(name()|context(), iodata(), iodata()) ->
    {ok,router_pb:'Etcd.AuthUserRevokeRoleResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.role_add(name)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_add(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthRoleAddResponse'()}|{error,eetcd_error()}.
role_add(Context, Name) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(name, Name, C1),
    eetcd_auth_gen:role_add(C2).

%%% @doc RoleGrantPermission grants a permission to a role.
%%% PermType: 'READ' | 'WRITE' | 'READWRITE'
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_grant_permission(ConnName, Name, Key, RangeEnd, 'READ')'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.user_add(name, key, rangeEnd, :'WRITE')
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_grant_permission(name()|context(), iodata(), iodata(), iodata(), 'READ' | 'WRITE' | 'READWRITE') ->
    {ok,router_pb:'Etcd.AuthRoleGrantPermissionResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.role_get(role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_get(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthRoleGetResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.role_list()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_list(name()|context()) ->
    {ok,router_pb:'Etcd.AuthRoleListResponse'()}|{error,eetcd_error()}.
role_list(Context) ->
    eetcd_auth_gen:role_list(eetcd:new(Context)).

%%% @doc RoleRevokePermission revokes a permission from a role.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_auth:role_revoke_permission(ConnName, Role, Key, RangeEnd)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.role_revoke_permission(role, key, rangeEnd)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_revoke_permission(name()|context(), iodata(), iodata(), iodata()) ->
    {ok,router_pb:'Etcd.AuthRoleRevokePermissionResponse'()}|{error,eetcd_error()}.
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
%%% :eetcd_auth.new(connName)
%%% |> :eetcd_auth.with_timeout(6000)
%%% |> :eetcd_auth.role_delete(role)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_auth:with_timeout/2} {@link eetcd_auth:new/1}
%%% @end
-spec role_delete(name()|context(), iodata()) ->
    {ok,router_pb:'Etcd.AuthRoleDeleteResponse'()}|{error,eetcd_error()}.
role_delete(Context, Role) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(role, Role, C1),
    eetcd_auth_gen:role_delete(C2).

%%% @doc Create context for request.
-spec new(atom()|reference()) -> context().
new(Context) -> eetcd:new(Context).

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()|infinity) -> context().
with_timeout(Context, Timeout) -> eetcd:with_timeout(Context, Timeout).
