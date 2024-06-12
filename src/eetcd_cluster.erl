-module(eetcd_cluster).

%% API
-include("eetcd.hrl").
-export([new/1, with_timeout/2]).
-export([member_list/1]).
-export([member_add/2, member_add_as_learner/2]).
-export([member_remove/2]).
-export([member_update/3]).
-export([member_promote/2]).

%%% @doc MemberList lists the current cluster membership.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_list(ConnName)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_list()
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_list(new_context()) ->
    {ok,rpc_pb:'etcdserverpb.MemberListResponse'()}|{error,eetcd_error()}.
member_list(Context) -> eetcd_cluster_gen:member_list(new(Context)).

%% @doc MemberAdd adds a new member into the cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_add(ConnName, ["http://127.0.0.1:2380"])'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_add(["http://127.0.0.1:2380"])
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_add(new_context(), PeerURLs) ->
    {ok,rpc_pb:'etcdserverpb.MemberAddResponse'()}
    | {error, eetcd:eetcd_error()}
    when PeerURLs :: [iodata()].
member_add(Context, PeerAddrs) when is_list(PeerAddrs) ->
    C1 = new(Context),
    C2 = maps:put(peerURLs, PeerAddrs, C1),
    C3 = maps:put(isLearner, false, C2),
    eetcd_cluster_gen:member_add(C3).

%% @doc MemberAddAsLearner adds a new learner member into the cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_add_as_learner(ConnName, ["http://127.0.0.1:2380"])'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_add_as_learner(["http://127.0.0.1:2380"])
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_add_as_learner(new_context(), PeerURLs) ->
    {ok,rpc_pb:'etcdserverpb.MemberAddResponse'()}
    | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}
    when PeerURLs :: [iodata()].
member_add_as_learner(Context, PeerAddrs) when is_list(PeerAddrs) ->
    C1 = new(Context),
    C2 = maps:put(peerURLs, PeerAddrs, C1),
    C3 = maps:put(isLearner, true, C2),
    eetcd_cluster_gen:member_add(C3).

%% @doc MemberRemove removes an existing member from the cluster.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_remove(ConnName, Id)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_remove(id)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_remove(new_context(), pos_integer()) ->
    {ok,rpc_pb:'etcdserverpb.MemberRemoveResponse'()}|{error,eetcd_error()}.
member_remove(Context, Id) when is_integer(Id) ->
    C1 = new(Context),
    C2 = maps:put('ID', Id, C1),
    eetcd_cluster_gen:member_remove(C2).

%% @doc MemberUpdate updates the peer addresses of the member.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_update(ConnName, Id, PeerAddrs)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_remove(id, peerAddrs)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_update(new_context(), pos_integer(), [list()]) ->
    {ok,rpc_pb:'etcdserverpb.MemberUpdateResponse'()}|{error,eetcd_error()}.
member_update(Context, Id, PeerAddrs)
    when is_integer(Id) andalso is_list(PeerAddrs) ->
    C1 = new(Context),
    C2 = maps:put('ID', Id, C1),
    C3 = maps:put(peerURLs, PeerAddrs, C2),
    eetcd_cluster_gen:member_update(C3).

%% @doc MemberPromote promotes a member from raft learner (non-voting) to raft voting member.
%%% <dl>
%%% <dt> 1.base </dt>
%%% <dd> `eetcd_cluster:member_promote(ConnName, Id)'</dd>
%%% <dt> 2.elixir </dt>
%%% <dd>
%%% ```
%%% :eetcd_cluster.new(connName)
%%% |> :eetcd_cluster.with_timeout(6000)
%%% |> :eetcd_cluster.member_promote(id)
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_cluster:with_timeout/2} {@link eetcd_cluster:new/1}
%%% @end
-spec member_promote(new_context(), pos_integer()) ->
    {ok,rpc_pb:'etcdserverpb.MemberPromoteResponse'()}|{error,eetcd_error()}.
member_promote(Context, Id) when is_integer(Id) ->
    C1 = new(Context),
    C2 = maps:put('ID', Id, C1),
    eetcd_cluster_gen:member_promote(C2).

%%% @doc Create context for request.
-spec new(new_context()) -> context().
new(Context) -> eetcd:new(Context).

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()|infinity) -> context().
with_timeout(Context, Timeout) -> eetcd:with_timeout(Context, Timeout).
