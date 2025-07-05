%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Election
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2025-07-05T16:58:01+00:00 and should not be modified manually

-module(eetcd_election_gen).

-export([campaign/2, campaign/3]).
-export([proclaim/2, proclaim/3]).
-export([leader/2, leader/3]).
-export([observe/1, observe/2]).
-export([resign/2, resign/3]).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Campaign'
-spec campaign(Client :: eetcd:client(), Request :: router_pb:'Etcd.CampaignRequest'()) ->
    {ok, router_pb:'Etcd.CampaignResponse'()} | {error, eetcd:eetcd_error()}.
campaign(Client, Request) ->
    campaign(Client, Request, []).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Campaign'
-spec campaign(Client :: eetcd:client(), Request :: router_pb:'Etcd.CampaignRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.CampaignResponse'()} | {error, eetcd:eetcd_error()}.
campaign(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.CampaignRequest', <<"/v3electionpb.Election/Campaign">>, 'Etcd.CampaignResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Proclaim'
-spec proclaim(Client :: eetcd:client(), Request :: router_pb:'Etcd.ProclaimRequest'()) ->
    {ok, router_pb:'Etcd.ProclaimResponse'()} | {error, eetcd:eetcd_error()}.
proclaim(Client, Request) ->
    proclaim(Client, Request, []).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Proclaim'
-spec proclaim(Client :: eetcd:client(), Request :: router_pb:'Etcd.ProclaimRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.ProclaimResponse'()} | {error, eetcd:eetcd_error()}.
proclaim(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.ProclaimRequest', <<"/v3electionpb.Election/Proclaim">>, 'Etcd.ProclaimResponse', router_pb, Opts).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Leader'
-spec leader(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaderRequest'()) ->
    {ok, router_pb:'Etcd.LeaderResponse'()} | {error, eetcd:eetcd_error()}.
leader(Client, Request) ->
    leader(Client, Request, []).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Leader'
-spec leader(Client :: eetcd:client(), Request :: router_pb:'Etcd.LeaderRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.LeaderResponse'()} | {error, eetcd:eetcd_error()}.
leader(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.LeaderRequest', <<"/v3electionpb.Election/Leader">>, 'Etcd.LeaderResponse', router_pb, Opts).

%% @doc Bidirectional streaming RPC for service at path `/v3electionpb.Election/Observe'
-spec observe(Client :: eetcd:client()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
observe(Client) ->
    observe(Client, []).

%% @doc Bidirectional streaming RPC for service at path `/v3electionpb.Election/Observe'
-spec observe(Client :: eetcd:client(), Opts :: eetcd:request_opts()) ->
    {ok, GunPid :: pid(), Http2Ref:: eetcd:stream_ref(), PbModule :: module()} | {error, eetcd:eetcd_error()}.
observe(Client, Opts) ->
    eetcd_stream:bidi_streaming(Client, <<"/v3electionpb.Election/Observe">>, router_pb, Opts).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Resign'
-spec resign(Client :: eetcd:client(), Request :: router_pb:'Etcd.ResignRequest'()) ->
    {ok, router_pb:'Etcd.ResignResponse'()} | {error, eetcd:eetcd_error()}.
resign(Client, Request) ->
    resign(Client, Request, []).

%% @doc Unary RPC for service at path `/v3electionpb.Election/Resign'
-spec resign(Client :: eetcd:client(), Request :: router_pb:'Etcd.ResignRequest'(), Opts :: eetcd:request_opts()) ->
    {ok, router_pb:'Etcd.ResignResponse'()} | {error, eetcd:eetcd_error()}.
resign(Client, Request, Opts) ->
    eetcd_stream:unary(Client, Request, 'Etcd.ResignRequest', <<"/v3electionpb.Election/Resign">>, 'Etcd.ResignResponse', router_pb, Opts).

