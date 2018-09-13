%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-13T09:22:55+00:00 and should not be modified manually

-module(eetcd_watch).

-include("router_pb.hrl").
-export([watch/1]).

%% @doc Stream RPC 
-spec watch(#'Etcd.WatchRequest'{}) ->
    reference() | {error, {'grpc_error', non_neg_integer(), binary()}} | {error, term()}.
watch(Request)when is_record(Request, 'Etcd.WatchRequest') ->
    eetcd_stream:data(Request, <<"/etcdserverpb.Watch/Watch">>).

