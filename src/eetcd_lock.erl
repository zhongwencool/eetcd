-module(eetcd_lock).
-include("eetcd.hrl").

-export([new/1, with_timeout/2, with_name/2, with_lease_id/2]).
-export([lock/1, lock/3, unlock/2]).


%%% @doc Creates a blank context for a request.
-spec new(atom()|reference()) -> context().
new(Context) -> eetcd:new(Context).

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()) -> context().
with_timeout(Context, Timeout) -> eetcd:with_timeout(Context, Timeout).

-spec with_name(context(), Name :: binary()) -> context().
with_name(Context, Name) ->
    maps:put(name, Name, Context).

-spec with_lease_id(context(), LeaseID :: pos_integer()) -> context().
with_lease_id(Context, LeaseID) ->
    maps:put(lease, LeaseID, Context).

-spec with_key(context(), Key :: binary()) -> context().
with_key(Context, Key) ->
    maps:put(key, Key, Context).



-spec lock(Ctx :: context()) -> {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd_error()}.
lock(Context) ->
    eetcd_lock_gen:lock(Context).

-spec lock(Ctx :: context(), Name :: binary(), LeaseID :: pos_integer()) -> {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd_error()}.
lock(Context0, Name, LeaseID) ->
    Context = with_lease_id(with_name(Context0, Name), LeaseID),
    eetcd_lock_gen:lock(Context).


-spec unlock(Ctx :: context(), Key :: binary()) -> {ok, router_pb:'Etcd.UnlockRequest'()} | {error, eetcd_error()}.
unlock(Context0, Key) ->
    Context = with_key(Context0, Key),
    eetcd_lock_gen:unlock(Context).
