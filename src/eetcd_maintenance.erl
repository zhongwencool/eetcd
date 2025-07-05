-module(eetcd_maintenance).
-include("eetcd.hrl").
%% API
-export([alarm_list/1, alarm_disarm/3, alarm_disarm_all/1]).
-export([defragment/2, status/2, hash_kv/2, hash_kv/3, move_leader/2]).

%%% @doc AlarmList gets all active alarms.
-spec alarm_list(etcd_name()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_list(EtcdName) ->
    Req = #{
        action => 'GET',
        memberID => 0,
        alarm => 'NONE'
    },
    eetcd_maintenance_gen:alarm(EtcdName, Req).

%%% @doc AlarmDisarm disarms a given alarm.
-spec alarm_disarm(etcd_name(), non_neg_integer(), integer()) ->
    {ok,router_pb:'Etcd.AlarmResponse'()}|{error,eetcd_error()}.
alarm_disarm(EtcdName, MemberId, Alarm) ->
    Req = #{
        action => 'DEACTIVATE',
        memberID => MemberId,
        alarm => Alarm
    },
    eetcd_maintenance_gen:alarm(EtcdName, Req).

%%% @doc AlarmDisarmAll disarms all alarm.
-spec alarm_disarm_all(etcd_name()) -> router_pb:'Etcd.AlarmResponse'().
alarm_disarm_all(EtcdName) ->
    {ok, Resp0 = #{alarms := List}} = alarm_list(EtcdName),
    Alarms = disarm_members(List, EtcdName, []),
    Resp0#{alarms => Alarms}.

disarm_members([], _EtcdName, Acc) -> Acc;
disarm_members([#{memberID := Id, alarm := Alarm} | Rest], EtcdName, Acc) ->
    case alarm_disarm(EtcdName, Id, Alarm) of
        {ok, #{alarms := L}} ->
            disarm_members(Rest, EtcdName, L ++ Acc);
        {error, Reason} ->
            ?LOG_ERROR("~p disarm ~p failed by ~p", [EtcdName, Alarm, Reason]),
            disarm_members(Rest, EtcdName, Acc)
    end.

%%% @doc Defragment releases wasted space from internal fragmentation on a given etcd member.
%%% Defragment is only needed when deleting a large number of keys and want to reclaim the resources.
%%% Defragment is an expensive operation. User should avoid defragmenting multiple members at the same time.
%%% To defragment multiple members in the cluster, user need to call defragment multiple
%%% times with different endpoints.
-spec defragment(etcd_name(), member_id()) ->
    {ok,router_pb:'Etcd.DefragmentResponse'()}|{error,eetcd_error()}.
defragment(EtcdName, MemberId) ->
    case eetcd_conn:pick_member(EtcdName, MemberId) of
        {ok, GunPid, Headers} ->
            eetcd_maintenance_gen:defragment({EtcdName, {GunPid, Headers}}, #{});
        {error, _} = Err ->
            Err
    end.

%%% @doc Status gets the status of the endpoint.
-spec status(etcd_name(), member_id()) ->
    {ok,router_pb:'Etcd.StatusResponse'()}|{error,eetcd_error()}.
status(EtcdName, MemberId) ->
    case eetcd_conn:pick_member(EtcdName, MemberId) of
        {ok, GunPid, Headers} ->
            eetcd_maintenance_gen:status({EtcdName, {GunPid, Headers}}, #{});
        {error, _} = Err ->
            Err
    end.

-spec hash_kv(etcd_name(), member_id()) ->
    {ok,router_pb:'Etcd.HashKVResponse'()}|{error,eetcd_error()}.
hash_kv(EtcdName, MemberId) ->
    hash_kv(EtcdName, MemberId, 0).

%%% @doc HashKV returns a hash of the KV state at the time of the RPC.
%%% If revision is zero, the hash is computed on all keys. If the revision
%%% is non-zero, the hash is computed on all keys at or below the given revision.
-spec hash_kv(etcd_name(), member_id(), non_neg_integer()) ->
    {ok,router_pb:'Etcd.HashKVResponse'()}|{error,eetcd_error()}.
hash_kv(EtcdName, MemberId, Rev) ->
    case eetcd_conn:pick_member(EtcdName, MemberId) of
        {ok, GunPid, Headers} ->
            eetcd_maintenance_gen:hash_kv({EtcdName, {GunPid, Headers}}, #{revision => Rev});
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
-spec move_leader(etcd_name(), pos_integer()) ->
    {ok,router_pb:'Etcd.MoveLeaderResponse'()}|{error,eetcd_error()}.
move_leader(EtcdName, TargetID) ->
    eetcd_maintenance_gen:move_leader(EtcdName, #{targetID => TargetID}).
