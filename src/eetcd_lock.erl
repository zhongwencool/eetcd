-module(eetcd_lock).
-include("eetcd.hrl").

-export([new/1, with_timeout/2, with_name/2, with_lease/2, with_key/2]).
-export([lock/1, lock/3, unlock/2]).


%%% @doc Creates a blank context for a request.
-spec new(atom()|reference()) -> context().
new(Context) -> eetcd:new(Context).

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()|infinity) -> context().
with_timeout(Context, Timeout) -> eetcd:with_timeout(Context, Timeout).

%%% @doc name is the identifier for the distributed shared lock to be acquired.
-spec with_name(context(), Name :: binary()) -> context().
with_name(Context, Name) ->
    maps:put(name, Name, Context).

%%% @doc lease is the ID of the lease that will be attached to ownership of the
%%% lock. If the lease expires or is revoked and currently holds the lock,
%%% the lock is automatically released. Calls to Lock with the same lease will
%%% be treated as a single acquisition; locking twice with the same lease is a no-op.
-spec with_lease(context(), LeaseID :: pos_integer()) -> context().
with_lease(Context, LeaseID) ->
    maps:put(lease, LeaseID, Context).

%%% @doc key is a key that will exist on etcd for the duration that the Lock caller
%%  owns the lock. Users should not modify this key or the lock may exhibit undefined behavior.
-spec with_key(context(), Key :: binary()) -> context().
with_key(Context, Key) ->
    maps:put(key, Key, Context).

%%% @doc Lock acquires a distributed shared lock on a given named lock.
%%% On success, it will return a unique key that exists so long as the
%%% lock is held by the caller. This key can be used in conjunction with
%%% transactions to safely ensure updates to etcd only occur while holding
%%% lock ownership. The lock is held until Unlock is called on the key or the
%%% lease associate with the owner expires.
-spec lock(Ctx :: context()) -> {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd_error()}.
lock(Context) ->
    eetcd_lock_gen:lock(Context).

-spec lock(Ctx :: context()|name(), Name :: binary(), LeaseID :: pos_integer()) -> {ok, router_pb:'Etcd.LockResponse'()} | {error, eetcd_error()}.
lock(Context0, Name, LeaseID) ->
    Context1 = new(Context0),
    Context = with_lease(with_name(Context1, Name), LeaseID),
    eetcd_lock_gen:lock(Context).

%%% @doc Unlock takes a key returned by Lock and releases the hold on lock. The
%%% next Lock caller waiting for the lock will then be woken up and given
%%% ownership of the lock.
-spec unlock(Ctx :: context()|name(), Key :: binary()) -> {ok, router_pb:'Etcd.UnlockRequest'()} | {error, eetcd_error()}.
unlock(Context0, Key) ->
    Context1 = new(Context0),
    Context = with_key(Context1, Key),
    eetcd_lock_gen:unlock(Context).
