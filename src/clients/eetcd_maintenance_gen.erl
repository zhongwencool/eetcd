%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Maintenance
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_maintenance_gen).

-export([alarm/2, alarm/3]).
-export([status/2, status/3]).
-export([defragment/2, defragment/3]).
-export([hash/2, hash/3]).
-export([hash_kv/2, hash_kv/3]).
-export([snapshot/2, snapshot/3]).
-export([move_leader/2, move_leader/3]).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Alarm'
-spec alarm(Client :: eetcd:client(), Request :: router_pb:'Etcd.AlarmRequest'()) ->
    {ok, router_pb:'Etcd.AlarmResponse'()} | {error, eetcd:eetcd_error()}.
alarm(Client, Request) ->
    alarm(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Alarm'
-spec alarm(Client :: eetcd:client(), Request :: router_pb:'Etcd.AlarmRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.AlarmResponse'()} | {error, eetcd:eetcd_error()}.
alarm(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.AlarmRequest', <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Status'
-spec status(Client :: eetcd:client(), Request :: router_pb:'Etcd.StatusRequest'()) ->
    {ok, router_pb:'Etcd.StatusResponse'()} | {error, eetcd:eetcd_error()}.
status(Client, Request) ->
    status(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Status'
-spec status(Client :: eetcd:client(), Request :: router_pb:'Etcd.StatusRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.StatusResponse'()} | {error, eetcd:eetcd_error()}.
status(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.StatusRequest', <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Defragment'
-spec defragment(Client :: eetcd:client(), Request :: router_pb:'Etcd.DefragmentRequest'()) ->
    {ok, router_pb:'Etcd.DefragmentResponse'()} | {error, eetcd:eetcd_error()}.
defragment(Client, Request) ->
    defragment(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Defragment'
-spec defragment(Client :: eetcd:client(), Request :: router_pb:'Etcd.DefragmentRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.DefragmentResponse'()} | {error, eetcd:eetcd_error()}.
defragment(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.DefragmentRequest', <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Hash'
-spec hash(Client :: eetcd:client(), Request :: router_pb:'Etcd.HashRequest'()) ->
    {ok, router_pb:'Etcd.HashResponse'()} | {error, eetcd:eetcd_error()}.
hash(Client, Request) ->
    hash(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/Hash'
-spec hash(Client :: eetcd:client(), Request :: router_pb:'Etcd.HashRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.HashResponse'()} | {error, eetcd:eetcd_error()}.
hash(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.HashRequest', <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/HashKV'
-spec hash_kv(Client :: eetcd:client(), Request :: router_pb:'Etcd.HashKVRequest'()) ->
    {ok, router_pb:'Etcd.HashKVResponse'()} | {error, eetcd:eetcd_error()}.
hash_kv(Client, Request) ->
    hash_kv(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/HashKV'
-spec hash_kv(Client :: eetcd:client(), Request :: router_pb:'Etcd.HashKVRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.HashKVResponse'()} | {error, eetcd:eetcd_error()}.
hash_kv(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.HashKVRequest', <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', router_pb, Opts).

%% @doc Server streaming RPC for service at path `/etcdserverpb.Maintenance/Snapshot'
-spec snapshot(Client :: eetcd:client(), Request :: router_pb:'Etcd.SnapshotRequest'()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
snapshot(Client, Request) ->
    snapshot(Client, Request, []).

%% @doc Server streaming RPC for service at path `/etcdserverpb.Maintenance/Snapshot'
-spec snapshot(Client :: eetcd:client(), Request :: router_pb:'Etcd.SnapshotRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
snapshot(Client, Request, Opts) ->
    eetcd_stream:server_streaming(Client, Request, 'Etcd.SnapshotRequest', <<"/etcdserverpb.Maintenance/Snapshot">>, router_pb, Opts).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/MoveLeader'
-spec move_leader(Client :: eetcd:client(), Request :: router_pb:'Etcd.MoveLeaderRequest'()) ->
    {ok, router_pb:'Etcd.MoveLeaderResponse'()} | {error, eetcd:eetcd_error()}.
move_leader(Client, Request) ->
    move_leader(Client, Request, []).

%% @doc Unary RPC for service at path `/etcdserverpb.Maintenance/MoveLeader'
-spec move_leader(Client :: eetcd:client(), Request :: router_pb:'Etcd.MoveLeaderRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.MoveLeaderResponse'()} | {error, eetcd:eetcd_error()}.
move_leader(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.MoveLeaderRequest', <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', router_pb, Opts).

