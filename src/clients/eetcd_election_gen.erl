%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for eectd Etcd.Election
%% @private
%%  All detail documents please visit https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md
%% @end
%%%-------------------------------------------------------------------

%% This module was generated on 2021-12-22T07:01:11+00:00 and should not be modified manually

-module(eetcd_election_gen).

-export([campaign/1]).
-export([proclaim/1]).
-export([leader/1]).
-export([observe/1]).
-export([resign/1]).

%% @doc Unary RPC for service at path "/v3electionpb.Election/Campaign" 
-spec campaign(router_pb:'Etcd.CampaignRequest'()) ->
    {ok, router_pb:'Etcd.CampaignResponse'()}|{error,eetcd:eetcd_error()}.
campaign(Request) ->
    eetcd_stream:unary(Request, 'Etcd.CampaignRequest', <<"/v3electionpb.Election/Campaign">>, 'Etcd.CampaignResponse').

%% @doc Unary RPC for service at path "/v3electionpb.Election/Proclaim" 
-spec proclaim(router_pb:'Etcd.ProclaimRequest'()) ->
    {ok, router_pb:'Etcd.ProclaimResponse'()}|{error,eetcd:eetcd_error()}.
proclaim(Request) ->
    eetcd_stream:unary(Request, 'Etcd.ProclaimRequest', <<"/v3electionpb.Election/Proclaim">>, 'Etcd.ProclaimResponse').

%% @doc Unary RPC for service at path "/v3electionpb.Election/Leader" 
-spec leader(router_pb:'Etcd.LeaderRequest'()) ->
    {ok, router_pb:'Etcd.LeaderResponse'()}|{error,eetcd:eetcd_error()}.
leader(Request) ->
    eetcd_stream:unary(Request, 'Etcd.LeaderRequest', <<"/v3electionpb.Election/Leader">>, 'Etcd.LeaderResponse').

%% @doc Stream RPC 
-spec observe(atom()|reference()) ->
    {ok, GunPid :: pid(), Http2Ref:: reference()}|{error,eetcd:eetcd_error()}.
observe(Request) ->
    eetcd_stream:new(Request, <<"/v3electionpb.Election/Observe">>).

%% @doc Unary RPC for service at path "/v3electionpb.Election/Resign" 
-spec resign(router_pb:'Etcd.ResignRequest'()) ->
    {ok, router_pb:'Etcd.ResignResponse'()}|{error,eetcd:eetcd_error()}.
resign(Request) ->
    eetcd_stream:unary(Request, 'Etcd.ResignRequest', <<"/v3electionpb.Election/Resign">>, 'Etcd.ResignResponse').

