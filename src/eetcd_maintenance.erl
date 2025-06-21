-module(eetcd_maintenance).
-include("eetcd.hrl").
%% API
-export([alarm_list/1, alarm_disarm/3, alarm_disarm_all/1]).
-export([defragment/2, status/2, hash_kv/2, hash_kv/3, move_leader/2]).

%%% @doc AlarmList gets all active alarms.
-spec alarm_list(new_context()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_list(ConnName) ->
    C1 = eetcd:new(ConnName),
    C2 = C1#{
        action => 'GET',
        memberID => 0,
        alarm => 'NONE'
    },
    eetcd_maintenance_gen:alarm(C2).

%%% @doc AlarmDisarm disarms a given alarm.
-spec alarm_disarm(new_context(), integer(), integer()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_disarm(Context, MemberId, Alarm) ->
    C1 = eetcd:new(Context),
    C2 = C1#{
        action => 'DEACTIVATE',
        memberID => MemberId,
        alarm => Alarm
    },
    eetcd_maintenance_gen:alarm(C2).

%%% @doc AlarmDisarmAll disarms all alarm.
-spec alarm_disarm_all(new_context()) ->
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
                      ?LOG_ERROR("~p disarm ~p failed by ~p", [ConnName, Alarm, Reason]),
                      Acc
              end
      end, Acc0#{alarms => []}, List).

%%% @doc Defragment releases wasted space from internal fragmentation on a given etcd member.
%%% Defragment is only needed when deleting a large number of keys and want to reclaim the resources.
%%% Defragment is an expensive operation. User should avoid defragmenting multiple members at the same time.
%%% To defragment multiple members in the cluster, user need to call defragment multiple
%%% times with different endpoints.
-spec defragment(name(), member_id()) ->
    {ok,router_pb:'Etcd.DefragmentResponse'()}|{error,eetcd_error()}.
defragment(Name, MemberId) ->
    case eetcd_conn:pick_member(Name, MemberId) of
        {ok, GunPid, Headers} ->
            Ctx = eetcd:new_with_conn(Name, GunPid, Headers),
            eetcd_maintenance_gen:defragment(Ctx);
        {error, _} = Err ->
            Err
    end.

%%% @doc Status gets the status of the endpoint.
-spec status(name(), member_id()) ->
    {ok,router_pb:'Etcd.StatusResponse'()}|{error,eetcd_error()}.
status(Name, MemberId) ->
    case eetcd_conn:pick_member(Name, MemberId) of
        {ok, GunPid, Headers} ->
            Ctx = eetcd:new_with_conn(Name, GunPid, Headers),
            eetcd_maintenance_gen:status(Ctx);
        {error, _} = Err ->
            Err
    end.

-spec hash_kv(name(), member_id()) ->
    {ok,router_pb:'Etcd.HashKVResponse'()}|{error,eetcd_error()}.
hash_kv(Name, MemberId) ->
    hash_kv(Name, MemberId, 0).

%%% @doc HashKV returns a hash of the KV state at the time of the RPC.
%%% If revision is zero, the hash is computed on all keys. If the revision
%%% is non-zero, the hash is computed on all keys at or below the given revision.
-spec hash_kv(name(), member_id(), non_neg_integer()) ->
    {ok,router_pb:'Etcd.HashKVResponse'()}|{error,eetcd_error()}.
hash_kv(Name, MemberId, Rev) ->
    case eetcd_conn:pick_member(Name, MemberId) of
        {ok, GunPid, Headers} ->
            Ctx1 = eetcd:new_with_conn(Name, GunPid, Headers),
            Ctx2 = Ctx1#{revision => Rev},
            eetcd_maintenance_gen:hash_kv(Ctx2);
        {error, _} = Err ->
            Err
    end.

%%% Snapshot provides a reader for a point-in-time snapshot of etcd.
%%% If the context "ctx" is canceled or timed out, reading from returned
%%% "io.ReadCloser" would error out (e.g. context.Canceled, context.DeadlineExceeded).
%% snapshot(Context) (io.ReadCloser, error)
%% todo

%%% @doc MoveLeader requests current leader to transfer its leadership to the transferee.
%%% Request must be made to the leader.
-spec move_leader(new_context(), pos_integer()) ->
    {ok,router_pb:'Etcd.MoveLeaderResponse'()}|{error,eetcd_error()}.
move_leader(Context, TargetID) ->
    C1 = eetcd:new(Context),
    C2 = C1#{targetID => TargetID},
    eetcd_maintenance_gen:move_leader(C2).
