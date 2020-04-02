-module(eetcd_election_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    campaign/1,
    proclaim/1,
    leader/1,
    resign/1

]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        campaign,
        proclaim,
        leader,
        resign
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

init_per_testcase(_TestCase, Config) ->
    revoke_all_leases(?Name),
    Config.

end_per_testcase(_TestCase, Config) ->
    revoke_all_leases(?Name),
    Config.

end_per_suite(Config) ->
    revoke_all_leases(?Name),
    eetcd:close(?Name),
    application:stop(eetcd),
    Config.

%%%===================================================================
%%% Test Cases
%%%===================================================================

campaign(_Config) ->
    LeaseID1 = new_lease(10),
    LeaseID2 = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    LeaderValue = <<"eetcd_leader_value">>,
    {ok, #{leader := Leader}} = eetcd_election:campaign(?Name, LeaderPfx, LeaseID1, LeaderValue),
    ?assertMatch(#{lease := LeaseID1, name := LeaderPfx}, Leader),
    ?assertMatch({error, timeout}, eetcd_election:campaign(?Name, LeaderPfx, LeaseID2, LeaderValue)),
    ok.

proclaim(_Config) ->
    LeaseID = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    InitLeaderValue = <<"eetcd_leader_init">>,
    ProclaimLeaderValue = <<"eetcd_leader_proclaim">>,
    {ok, #{leader := Leader}} = eetcd_election:campaign(?Name, LeaderPfx, LeaseID, InitLeaderValue),
    {ok, _} = eetcd_election:proclaim(?Name, Leader, ProclaimLeaderValue),
    ?assertMatch({ok, #{kv := #{value := ProclaimLeaderValue}}}, eetcd_election:leader(?Name, LeaderPfx)),
    ok.

leader(_Config) ->
    LeaderPfx = <<"eetcd_leader_pfx">>,
    ?assertMatch({error, {grpc_error,
        #{'grpc-message' := <<"election: no leader">>,
            'grpc-status' := 2}}}, eetcd_election:leader(?Name, LeaderPfx)),
    LeaseID = new_lease(10),
    LeaderValue = <<"eetcd_leader_init">>,
    {ok, #{leader := Leader}} = eetcd_election:campaign(?Name, LeaderPfx, LeaseID, LeaderValue),
    ?assertMatch(#{lease := LeaseID, name := LeaderPfx}, Leader),
    ?assertMatch({ok, #{kv := #{lease := LeaseID, value := LeaderValue}}}, eetcd_election:leader(?Name, LeaderPfx)),
    ok.

resign(_Config) ->
    LeaseID = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    LeaderValue = <<"eetcd_leader_init">>,
    {ok, #{leader := Leader}} = eetcd_election:campaign(?Name, LeaderPfx, LeaseID, LeaderValue),
    ?assertMatch({ok, #{kv := #{lease := LeaseID, value := LeaderValue}}}, eetcd_election:leader(?Name, LeaderPfx)),
    ?assertMatch({ok, _}, eetcd_election:resign(?Name, Leader)),
    ?assertMatch({error, {grpc_error,
        #{'grpc-message' := <<"election: no leader">>,
            'grpc-status' := 2}}}, eetcd_election:leader(?Name, LeaderPfx)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

revoke_all_leases(?Name) ->
    {ok, #{leases := Leases}} = eetcd_lease:leases(?Name),
    lists:foreach(fun(#{'ID' := ID}) ->
      eetcd_lease:revoke(?Name, ID)
    end, Leases).

new_lease(Sec) ->
    {ok, #{'ID' := Id}} = eetcd_lease:grant(?Name, Sec),
    {ok, _Pid} = eetcd_lease:keep_alive(?Name, Id),
    Id.
    