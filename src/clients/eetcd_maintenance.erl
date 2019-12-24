%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Maintenance.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:52:00+00:00 and should not be modified manually

-module(eetcd_maintenance).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([alarm/1, alarm/2]).
-export([status/1, status/2]).
-export([defragment/1, defragment/2]).
-export([hash/1, hash/2]).
-export([hash_kv/1, hash_kv/2]).
-export([snapshot/1, snapshot/2]).
-export([move_leader/1, move_leader/2]).

%% @doc Unary RPC 
-spec alarm(#'Etcd.AlarmRequest'{}) ->
    {ok, #'Etcd.AlarmResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
alarm(Request) when is_record(Request, 'Etcd.AlarmRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', []).
-spec alarm(#'Etcd.AlarmRequest'{}, Http2Headers) ->
    {ok, #'Etcd.AlarmResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
alarm(Request, Token) when is_record(Request, 'Etcd.AlarmRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', [{<<"authorization">>, Token}]);
alarm(Request, Http2Headers) when is_record(Request, 'Etcd.AlarmRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Alarm">>, 'Etcd.AlarmResponse', Http2Headers).

%% @doc Unary RPC 
-spec status(#'Etcd.StatusRequest'{}) ->
    {ok, #'Etcd.StatusResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
status(Request) when is_record(Request, 'Etcd.StatusRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', []).
-spec status(#'Etcd.StatusRequest'{}, Http2Headers) ->
    {ok, #'Etcd.StatusResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
status(Request, Token) when is_record(Request, 'Etcd.StatusRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', [{<<"authorization">>, Token}]);
status(Request, Http2Headers) when is_record(Request, 'Etcd.StatusRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Status">>, 'Etcd.StatusResponse', Http2Headers).

%% @doc Unary RPC 
-spec defragment(#'Etcd.DefragmentRequest'{}) ->
    {ok, #'Etcd.DefragmentResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
defragment(Request) when is_record(Request, 'Etcd.DefragmentRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', []).
-spec defragment(#'Etcd.DefragmentRequest'{}, Http2Headers) ->
    {ok, #'Etcd.DefragmentResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
defragment(Request, Token) when is_record(Request, 'Etcd.DefragmentRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', [{<<"authorization">>, Token}]);
defragment(Request, Http2Headers) when is_record(Request, 'Etcd.DefragmentRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Defragment">>, 'Etcd.DefragmentResponse', Http2Headers).

%% @doc Unary RPC 
-spec hash(#'Etcd.HashRequest'{}) ->
    {ok, #'Etcd.HashResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash(Request) when is_record(Request, 'Etcd.HashRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', []).
-spec hash(#'Etcd.HashRequest'{}, Http2Headers) ->
    {ok, #'Etcd.HashResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
hash(Request, Token) when is_record(Request, 'Etcd.HashRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', [{<<"authorization">>, Token}]);
hash(Request, Http2Headers) when is_record(Request, 'Etcd.HashRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Hash">>, 'Etcd.HashResponse', Http2Headers).

%% @doc Unary RPC 
-spec hash_kv(#'Etcd.HashKVRequest'{}) ->
    {ok, #'Etcd.HashKVResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
hash_kv(Request) when is_record(Request, 'Etcd.HashKVRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', []).
-spec hash_kv(#'Etcd.HashKVRequest'{}, Http2Headers) ->
    {ok, #'Etcd.HashKVResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
hash_kv(Request, Token) when is_record(Request, 'Etcd.HashKVRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', [{<<"authorization">>, Token}]);
hash_kv(Request, Http2Headers) when is_record(Request, 'Etcd.HashKVRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/HashKV">>, 'Etcd.HashKVResponse', Http2Headers).

%% @doc Unary RPC 
-spec snapshot(#'Etcd.SnapshotRequest'{}) ->
    {ok, #'Etcd.SnapshotResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
snapshot(Request) when is_record(Request, 'Etcd.SnapshotRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', []).
-spec snapshot(#'Etcd.SnapshotRequest'{}, Http2Headers) ->
    {ok, #'Etcd.SnapshotResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
snapshot(Request, Token) when is_record(Request, 'Etcd.SnapshotRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', [{<<"authorization">>, Token}]);
snapshot(Request, Http2Headers) when is_record(Request, 'Etcd.SnapshotRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/Snapshot">>, 'Etcd.SnapshotResponse', Http2Headers).

%% @doc Unary RPC 
-spec move_leader(#'Etcd.MoveLeaderRequest'{}) ->
    {ok, #'Etcd.MoveLeaderResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
move_leader(Request) when is_record(Request, 'Etcd.MoveLeaderRequest') ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', []).
-spec move_leader(#'Etcd.MoveLeaderRequest'{}, Http2Headers) ->
    {ok, #'Etcd.MoveLeaderResponse'{}} | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
move_leader(Request, Token) when is_record(Request, 'Etcd.MoveLeaderRequest') andalso is_binary(Token) ->
    eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', [{<<"authorization">>, Token}]);
move_leader(Request, Http2Headers) when is_record(Request, 'Etcd.MoveLeaderRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:unary(Request, <<"/etcdserverpb.Maintenance/MoveLeader">>, 'Etcd.MoveLeaderResponse', Http2Headers).

