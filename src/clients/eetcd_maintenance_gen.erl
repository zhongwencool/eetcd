%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.Maintenance
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_maintenance_gen).

-export([alarm/1]).
-export([status/1]).
-export([defragment/1]).
-export([hash/1]).
-export([hash_kv/1]).
-export([snapshot/1]).
-export([move_leader/1]).
-export([downgrade/1]).

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Alarm"
-spec alarm(rpc_pb:'etcdserverpb.AlarmRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.AlarmResponse'()}|{error,eetcd:eetcd_error()}.
alarm(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.AlarmRequest', <<"/etcdserverpb.Maintenance/Alarm">>, 'etcdserverpb.AlarmResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Status"
-spec status(rpc_pb:'etcdserverpb.StatusRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.StatusResponse'()}|{error,eetcd:eetcd_error()}.
status(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.StatusRequest', <<"/etcdserverpb.Maintenance/Status">>, 'etcdserverpb.StatusResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Defragment"
-spec defragment(rpc_pb:'etcdserverpb.DefragmentRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.DefragmentResponse'()}|{error,eetcd:eetcd_error()}.
defragment(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.DefragmentRequest', <<"/etcdserverpb.Maintenance/Defragment">>, 'etcdserverpb.DefragmentResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Hash"
-spec hash(rpc_pb:'etcdserverpb.HashRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.HashResponse'()}|{error,eetcd:eetcd_error()}.
hash(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.HashRequest', <<"/etcdserverpb.Maintenance/Hash">>, 'etcdserverpb.HashResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/HashKV"
-spec hash_kv(rpc_pb:'etcdserverpb.HashKVRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.HashKVResponse'()}|{error,eetcd:eetcd_error()}.
hash_kv(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.HashKVRequest', <<"/etcdserverpb.Maintenance/HashKV">>, 'etcdserverpb.HashKVResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Snapshot"
-spec snapshot(rpc_pb:'etcdserverpb.SnapshotRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.SnapshotResponse'()}|{error,eetcd:eetcd_error()}.
snapshot(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.SnapshotRequest', <<"/etcdserverpb.Maintenance/Snapshot">>, 'etcdserverpb.SnapshotResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/MoveLeader"
-spec move_leader(rpc_pb:'etcdserverpb.MoveLeaderRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.MoveLeaderResponse'()}|{error,eetcd:eetcd_error()}.
move_leader(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.MoveLeaderRequest', <<"/etcdserverpb.Maintenance/MoveLeader">>, 'etcdserverpb.MoveLeaderResponse').

%% @doc Unary RPC for service at path "/etcdserverpb.Maintenance/Downgrade"
-spec downgrade(rpc_pb:'etcdserverpb.DowngradeRequest'()) ->
    {ok, rpc_pb:'etcdserverpb.DowngradeResponse'()}|{error,eetcd:eetcd_error()}.
downgrade(Request) ->
    eetcd_stream:unary(Request, 'etcdserverpb.DowngradeRequest', <<"/etcdserverpb.Maintenance/Downgrade">>, 'etcdserverpb.DowngradeResponse').

