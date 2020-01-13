%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2020-01-13T08:36:28+00:00 and should not be modified manually

-module(eetcd_watch_gen).

-export([watch/1, watch/2]).

%% @doc Stream RPC 
-spec watch(router_pb:'Etcd.WatchRequest'()) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
watch(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>).

