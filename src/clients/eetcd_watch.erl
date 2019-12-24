%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2019-12-24T00:52:00+00:00 and should not be modified manually

-module(eetcd_watch).

-include("router_pb.hrl").
-include("eetcd.hrl").
-export([watch/1, watch/2]).

%% @doc Stream RPC 
-spec watch(#'Etcd.WatchRequest'{}) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
watch(Request) when is_record(Request, 'Etcd.WatchRequest') ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, []).
-spec watch(#'Etcd.WatchRequest'{}, Http2Headers) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
watch(Request, Token) when is_record(Request, 'Etcd.WatchRequest') andalso is_binary(Token) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, [{<<"authorization">>, Token}]);
watch(Request, Http2Headers) when is_record(Request, 'Etcd.WatchRequest') andalso is_list(Http2Headers) ->
   eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, Http2Headers).

