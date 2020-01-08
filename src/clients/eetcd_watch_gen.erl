%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-06T08:17:27+00:00 and should not be modified manually

-module(eetcd_watch_gen).

-export([watch/1, watch/2]).

%% @doc Stream RPC 
-spec watch(router_pb:'Etcd.WatchRequest'()) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
watch(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, []).
-spec watch(router_pb:'Etcd.WatchRequest'(), Http2Headers) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()} when Http2Headers :: [{binary(), binary()}] | binary().
watch(Request, Token)when is_binary(Token) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, [{<<"authorization">>, Token}]);
watch(Request, Http2Headers) when is_list(Http2Headers) ->
   eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, Http2Headers).

