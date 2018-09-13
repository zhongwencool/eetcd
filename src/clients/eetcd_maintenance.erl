%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Maintenance.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_maintenance).

-include("router_pb.hrl").
-export([alarm/1]).
-export([status/1]).
-export([defragment/1]).
-export([hash/1]).
-export([hash_kv/1]).
-export([snapshot/1]).
-export([move_leader/1]).

%% @doc Unary RPC 
-spec alarm(#'Etcd.AlarmRequest'{}) ->
    {ok, #'Etcd.AlarmResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
alarm(Request)when is_record(Request, 'Etcd.AlarmRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse').

%% @doc Unary RPC 
-spec status(#'Etcd.StatusRequest'{}) ->
    {ok, #'Etcd.StatusResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
status(Request)when is_record(Request, 'Etcd.StatusRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse').

%% @doc Unary RPC 
-spec defragment(#'Etcd.DefragmentRequest'{}) ->
    {ok, #'Etcd.DefragmentResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
defragment(Request)when is_record(Request, 'Etcd.DefragmentRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse').

%% @doc Unary RPC 
-spec hash(#'Etcd.HashRequest'{}) ->
    {ok, #'Etcd.HashResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash(Request)when is_record(Request, 'Etcd.HashRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse').

%% @doc Unary RPC 
-spec hash_kv(#'Etcd.HashKVRequest'{}) ->
    {ok, #'Etcd.HashKVResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash_kv(Request)when is_record(Request, 'Etcd.HashKVRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse').

%% @doc Unary RPC 
-spec snapshot(#'Etcd.SnapshotRequest'{}) ->
    {ok, #'Etcd.SnapshotResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
snapshot(Request)when is_record(Request, 'Etcd.SnapshotRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse').

%% @doc Unary RPC 
-spec move_leader(#'Etcd.MoveLeaderRequest'{}) ->
    {ok, #'Etcd.MoveLeaderResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
move_leader(Request)when is_record(Request, 'Etcd.MoveLeaderRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse').

