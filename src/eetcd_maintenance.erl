-module(eetcd_maintenance).
-include("eetcd.hrl").
%% API
-export([alarm_list/1, alarm_disarm/3, alarm_disarm_all/1]).
-export([defragment/3, status/3, hash_kv/4, move_leader/2]).

%%% @doc AlarmList gets all active alarms.
-spec alarm_list(name()|context()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_list(ConnName) ->
    C1 = eetcd:new(ConnName),
    C2 = maps:put(action, 'GET', C1),
    C3 = maps:put(memberID, 0, C2),
    C4 = maps:put(alarm, 'NONE', C3),
    eetcd_maintenance_gen:alarm(C4).

%%% @doc AlarmDisarm disarms a given alarm.
-spec alarm_disarm(name()|context(), integer(), integer()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_disarm(Context, MemberId, Alarm) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(action, 'DEACTIVATE', C1),
    C3 = maps:put(memberID, MemberId, C2),
    C4 = maps:put(alarm, Alarm, C3),
    eetcd_maintenance_gen:alarm(C4).

%%% @doc AlarmDisarmAll disarms all alarm.
-spec alarm_disarm_all(name()|context()) ->
    router_pb:'Etcd.AlarmResponse'().
alarm_disarm_all(ConnName) ->
    {ok, Acc0 = #{alarms := List}} = alarm_list(ConnName),
    lists:foldl(
      fun(#{memberID := Id, alarm := Alarm}, Acc) ->
              #{alarms := Old} = Acc,
              case alarm_disarm(ConnName, Id, Alarm) of
                  {ok, #{alarms := L}} ->
                      Acc#{alarms => L ++ Old};
                  {error, Reason} ->
                      ?LOG_ERROR("~p disarm ~p failed by ~p ", [ConnName, Alarm, Reason]),
                      Acc
              end
      end, Acc0#{alarms => []}, List).

%%% @doc Defragment releases wasted space from internal fragmentation on a given etcd member.
%%% Defragment is only needed when deleting a large number of keys and want to reclaim the resources.
%%% Defragment is an expensive operation. User should avoid defragmenting multiple members at the same time.
%%% To defragment multiple members in the cluster, user need to call defragment multiple
%%% times with different endpoints.
-spec defragment(iodata(), tcp | tls | ssl, [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok,router_pb:'Etcd.DefragmentResponse'()}|{error,eetcd_error()}.
defragment(Endpoint, Transport, TransportOpts) ->
    Fun = fun(Conn) -> eetcd_maintenance_gen:defragment(eetcd:new(Conn)) end,
    dial(Endpoint, Transport, TransportOpts, Fun).

%%% @doc Status gets the status of the endpoint.
-spec status(iodata(), tcp | tls | ssl, [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok,router_pb:'Etcd.StatusResponse'()}|{error,eetcd_error()}.
status(Endpoint, Transport, TransportOpts) ->
    Fun = fun(Conn) -> eetcd_maintenance_gen:status(eetcd:new(Conn)) end,
    dial(Endpoint, Transport, TransportOpts, Fun).

%%% @doc HashKV returns a hash of the KV state at the time of the RPC.
%%% If revision is zero, the hash is computed on all keys. If the revision
%%% is non-zero, the hash is computed on all keys at or below the given revision.
-spec hash_kv(iodata(), tcp | tls | ssl, [gen_tcp:connect_option()] | [ssl:connect_option()], pos_integer()) ->
    {ok,router_pb:'Etcd.HashKVResponse'()}|{error,eetcd_error()}.
hash_kv(Endpoint, Transport, TransportOpts, Rev) ->
    Fun = fun(Conn) ->
        Context = maps:put(revision, Rev, eetcd:new(Conn)),
        eetcd_maintenance_gen:hash_kv(Context)
          end,
    dial(Endpoint, Transport, TransportOpts, Fun).

%%% Snapshot provides a reader for a point-in-time snapshot of etcd.
%%% If the context "ctx" is canceled or timed out, reading from returned
%%% "io.ReadCloser" would error out (e.g. context.Canceled, context.DeadlineExceeded).
%% snapshot(Context) (io.ReadCloser, error)
%% todo

%%% @doc MoveLeader requests current leader to transfer its leadership to the transferee.
%%% Request must be made to the leader.
-spec move_leader(name()|context(), pos_integer()) ->
    {ok,router_pb:'Etcd.MoveLeaderResponse'()}|{error,eetcd_error()}.
move_leader(Context, TargetID) ->
    C1 = eetcd:new(Context),
    C2 = maps:put(targetID, TargetID, C1),
    eetcd_maintenance_gen:move_leader(C2).

%%%===================================================================
%%% Internal functions
%%%===================================================================
dial(Endpoint, Transport, TransportOpts, Fun) ->
    Conn = make_ref(),
    try
        case eetcd:open(Conn, [Endpoint], Transport, TransportOpts) of
            {ok, _Pid} -> Fun(Conn);
            Err ->
                Err
        end
    catch E:R ->
        {error, E, R}
    after
        eetcd:close(Conn)
    end.
