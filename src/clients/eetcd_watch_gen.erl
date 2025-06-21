%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-06-21T09:30:26+00:00 and should not be modified manually

-module(eetcd_watch_gen).

-export([watch/1]).

%% @doc Stream RPC 
-spec watch(atom()|reference()) ->
    {ok, GunPid :: pid(), Http2Ref:: reference(), PbModule :: module()}|{error,eetcd:eetcd_error()}.
watch(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>, router_pb).

