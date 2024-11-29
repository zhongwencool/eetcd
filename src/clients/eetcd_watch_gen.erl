%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eetcd etcdserverpb.Watch
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2023-12-11T09:36:27+00:00 and should not be modified manually

-module(eetcd_watch_gen).

-export([watch/1]).

%% @doc Stream RPC 
-spec watch(atom()|reference()) ->
    {ok, GunPid :: pid(), Http2Ref:: reference()}|{error,eetcd:eetcd_error()}.
watch(Request) ->
    eetcd_stream:new(Request, <<"/etcdserverpb.Watch/Watch">>).

