%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Maintenance.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-06T08:17:27+00:00 and should not be modified manually

-module(eetcd_maintenance_gen).

-export([alarm/1, alarm/2]).
-export([status/1, status/2]).
-export([defragment/1, defragment/2]).
-export([hash/1, hash/2]).
-export([hash_kv/1, hash_kv/2]).
-export([snapshot/1, snapshot/2]).
-export([move_leader/1, move_leader/2]).

%% @doc Unary RPC 
-spec alarm(router_pb:'Etcd.AlarmRequest'()) ->
    {ok, router_pb:'Etcd.AlarmResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
alarm(Request) ->
    eetcd_stream:unary(Request, 'Etcd.AlarmRequest', <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', []).
-spec alarm(router_pb:'Etcd.AlarmRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.AlarmResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
alarm(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.AlarmRequest', <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', [{<<"authorization">>, Token}]);
alarm(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.AlarmRequest', <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', Http2Headers).

%% @doc Unary RPC 
-spec status(router_pb:'Etcd.StatusRequest'()) ->
    {ok, router_pb:'Etcd.StatusResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
status(Request) ->
    eetcd_stream:unary(Request, 'Etcd.StatusRequest', <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', []).
-spec status(router_pb:'Etcd.StatusRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.StatusResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
status(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.StatusRequest', <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', [{<<"authorization">>, Token}]);
status(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.StatusRequest', <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', Http2Headers).

%% @doc Unary RPC 
-spec defragment(router_pb:'Etcd.DefragmentRequest'()) ->
    {ok, router_pb:'Etcd.DefragmentResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
defragment(Request) ->
    eetcd_stream:unary(Request, 'Etcd.DefragmentRequest', <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', []).
-spec defragment(router_pb:'Etcd.DefragmentRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.DefragmentResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
defragment(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.DefragmentRequest', <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', [{<<"authorization">>, Token}]);
defragment(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.DefragmentRequest', <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', Http2Headers).

%% @doc Unary RPC 
-spec hash(router_pb:'Etcd.HashRequest'()) ->
    {ok, router_pb:'Etcd.HashResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash(Request) ->
    eetcd_stream:unary(Request, 'Etcd.HashRequest', <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', []).
-spec hash(router_pb:'Etcd.HashRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.HashResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
hash(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.HashRequest', <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', [{<<"authorization">>, Token}]);
hash(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.HashRequest', <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', Http2Headers).

%% @doc Unary RPC 
-spec hash_kv(router_pb:'Etcd.HashKVRequest'()) ->
    {ok, router_pb:'Etcd.HashKVResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash_kv(Request) ->
    eetcd_stream:unary(Request, 'Etcd.HashKVRequest', <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', []).
-spec hash_kv(router_pb:'Etcd.HashKVRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.HashKVResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
hash_kv(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.HashKVRequest', <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', [{<<"authorization">>, Token}]);
hash_kv(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.HashKVRequest', <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', Http2Headers).

%% @doc Unary RPC 
-spec snapshot(router_pb:'Etcd.SnapshotRequest'()) ->
    {ok, router_pb:'Etcd.SnapshotResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
snapshot(Request) ->
    eetcd_stream:unary(Request, 'Etcd.SnapshotRequest', <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', []).
-spec snapshot(router_pb:'Etcd.SnapshotRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.SnapshotResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
snapshot(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.SnapshotRequest', <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', [{<<"authorization">>, Token}]);
snapshot(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.SnapshotRequest', <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', Http2Headers).

%% @doc Unary RPC 
-spec move_leader(router_pb:'Etcd.MoveLeaderRequest'()) ->
    {ok, router_pb:'Etcd.MoveLeaderResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
move_leader(Request) ->
    eetcd_stream:unary(Request, 'Etcd.MoveLeaderRequest', <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', []).
-spec move_leader(router_pb:'Etcd.MoveLeaderRequest'(), Http2Headers) ->
    {ok, router_pb:'Etcd.MoveLeaderResponse'()} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
move_leader(Request, Token)when is_binary(Token) ->
    eetcd_stream:unary(Request, 'Etcd.MoveLeaderRequest', <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', [{<<"authorization">>, Token}]);
move_leader(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:unary(Request, 'Etcd.MoveLeaderRequest', <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', Http2Headers).

