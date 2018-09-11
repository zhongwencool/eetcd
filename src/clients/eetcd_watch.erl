%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch.
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2018-09-11T07:54:56+00:00 and should not be modified manually

-module(eetcd_watch).

-export([watch/1]).

%% @doc Stream RPC 
-spec watch(router_pb:'Etcd.WatchRequest'()) ->
    reference() | {error, term()}.
watch(Request) ->
    eetcd_stream:data(Request, <<"/etcdserverpb.Watch/Watch">>).

