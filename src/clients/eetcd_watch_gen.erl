%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Watch
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_watch_gen).

-export([watch/1, watch/2]).

%% @doc Bidirectional streaming RPC for service at path `/etcdserverpb.Watch/Watch'
-spec watch(Client :: eetcd:client()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
watch(Client) ->
    watch(Client, []).

%% @doc Bidirectional streaming RPC for service at path `/etcdserverpb.Watch/Watch'
-spec watch(Client :: eetcd:client(), Opts :: eetcd:request_opts()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
watch(Client, Opts) ->
    eetcd_stream:bidi_streaming(Client, <<"/etcdserverpb.Watch/Watch">>, router_pb, Opts).

