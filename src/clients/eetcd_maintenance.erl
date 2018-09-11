%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Maintenance.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-11T07:54:56+00:00 and should not be modified manually

-module(eetcd_maintenance).

-export([alarm/1]).
-export([status/1]).
-export([defragment/1]).
-export([hash/1]).
-export([hash_kv/1]).
-export([snapshot/1]).
-export([move_leader/1]).

%% @doc Unary RPC 
-spec alarm(router_pb:'Etcd.AlarmRequest'()) ->
    {ok, router_pb:'Etcd.AlarmResponse'()} | {error, term()}.
alarm(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse').

%% @doc Unary RPC 
-spec status(router_pb:'Etcd.StatusRequest'()) ->
    {ok, router_pb:'Etcd.StatusResponse'()} | {error, term()}.
status(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse').

%% @doc Unary RPC 
-spec defragment(router_pb:'Etcd.DefragmentRequest'()) ->
    {ok, router_pb:'Etcd.DefragmentResponse'()} | {error, term()}.
defragment(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse').

%% @doc Unary RPC 
-spec hash(router_pb:'Etcd.HashRequest'()) ->
    {ok, router_pb:'Etcd.HashResponse'()} | {error, term()}.
hash(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse').

%% @doc Unary RPC 
-spec hash_kv(router_pb:'Etcd.HashKVRequest'()) ->
    {ok, router_pb:'Etcd.HashKVResponse'()} | {error, term()}.
hash_kv(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse').

%% @doc Unary RPC 
-spec snapshot(router_pb:'Etcd.SnapshotRequest'()) ->
    {ok, router_pb:'Etcd.SnapshotResponse'()} | {error, term()}.
snapshot(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse').

%% @doc Unary RPC 
-spec move_leader(router_pb:'Etcd.MoveLeaderRequest'()) ->
    {ok, router_pb:'Etcd.MoveLeaderResponse'()} | {error, term()}.
move_leader(Request) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse').

