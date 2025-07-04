-module(eetcd_election_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    campaign/1,
    campaign_async/1,
    proclaim/1,
    leader/1,
    resign/1,
    resign_no_leader/1,
    observe_with_leader/1

]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        campaign,
        campaign_async,
        proclaim,
        leader,
        resign,
        resign_no_leader,
        observe_with_leader
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
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                     lease => LeaseID1,
                                                                     value => LeaderValue}),
    ?assertMatch(#{lease := LeaseID1, name := LeaderPfx}, Leader),
    ?assertMatch({error, timeout}, eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                        lease => LeaseID2,
                                                                        value => LeaderValue})),
    ok.

proclaim(_Config) ->
    LeaseID = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    InitLeaderValue = <<"eetcd_leader_init">>,
    ProclaimLeaderValue = <<"eetcd_leader_proclaim">>,
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                     lease => LeaseID,
                                                                     value => InitLeaderValue}),
    {ok, _} = eetcd_election_gen:proclaim(?Name, #{leader => Leader,
                                                   value => ProclaimLeaderValue}),
    ?assertMatch({ok, #{kv := #{value := ProclaimLeaderValue}}},
                 eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ok.

leader(_Config) ->
    LeaderPfx = <<"eetcd_leader_pfx">>,
    ?assertMatch({error, {grpc_error,
        #{'grpc-message' := <<"election: no leader">>,
            'grpc-status' := 2}}}, eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    LeaseID = new_lease(10),
    LeaderValue = <<"eetcd_leader_init">>,
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                     lease => LeaseID,
                                                                     value => LeaderValue}),
    ?assertMatch(#{lease := LeaseID, name := LeaderPfx}, Leader),
    ?assertMatch({ok, #{kv := #{lease := LeaseID, value := LeaderValue}}},
                 eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ok.

resign_no_leader(_Config) ->
    LeaseID = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    LeaderValue = <<"eetcd_leader_init">>,
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                     lease => LeaseID,
                                                                     value => LeaderValue}),
    ?assertMatch({ok, #{kv := #{lease := LeaseID, value := LeaderValue}}},
                 eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ?assertMatch({ok, _}, eetcd_election_gen:resign(?Name, #{leader => Leader})),
    ?assertMatch({error, {grpc_error,
        #{'grpc-message' := <<"election: no leader">>,
            'grpc-status' := 2}}}, eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ok.

resign(_Config) ->
    LeaseID1 = new_lease(10),
    LeaseID2 = new_lease(10),
    LeaderPfx = <<"eetcd_leader_pfx">>,
    LeaderValue = <<"eetcd_leader_init">>,
    LeaderResign = <<"eetcd_leader_resign">>,
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                                     lease => LeaseID1,
                                                                     value => LeaderValue}),
    {error, timeout} = eetcd_election_gen:campaign(?Name, #{name => LeaderPfx,
                                                            lease => LeaseID2,
                                                            value => LeaderResign}),
    ?assertMatch({ok, #{kv := #{lease := LeaseID1, value := LeaderValue}}},
                 eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ?assertMatch({ok, _}, eetcd_election_gen:resign(?Name, #{leader => Leader})),
    ?assertMatch({ok, #{kv := #{value := LeaderResign}}},
                 eetcd_election_gen:leader(?Name, #{name => LeaderPfx})),
    ok.

observe_with_leader(_Config) ->
    LeaseID = new_lease(10),
    LeaderKey = <<"LeaderKey">>,
    ?assertMatch({error, {grpc_error,
        #{'grpc-message' := <<"election: no leader">>,
            'grpc-status' := 2}}}, eetcd_election_gen:leader(?Name, #{name => LeaderKey})),
    {ok, #{leader := Leader}} = eetcd_election_gen:campaign(?Name, #{name => LeaderKey,
                                                                     lease => LeaseID,
                                                                     value => <<"Leader-V1">>}),
    ?assertMatch(#{lease := LeaseID, name := LeaderKey}, Leader),
    {ok, OCtx} = eetcd_election:observe(?Name, LeaderKey, 3000),
    ?assertMatch(#{leader := #{lease := LeaseID, value := <<"Leader-V1">>}}, OCtx),
    {ok, #{header := #{revision := _Revision}}} =
        eetcd_election_gen:proclaim(?Name, #{leader => Leader, value => <<"Leader-V2">>}),
    receive Msg1 ->
        {ok, OCtx1} = eetcd_election:observe_stream(OCtx, Msg1),
        ?assertMatch(#{leader := #{lease := LeaseID, value := <<"Leader-V2">>}}, OCtx1),
        {ok, _} = eetcd_election_gen:proclaim(?Name, #{leader => Leader, value => <<"Leader-V3">>}),
        receive Msg2 ->
            {ok, OCtx2} = eetcd_election:observe_stream(OCtx1, Msg2),
            ?assertMatch(#{leader := #{lease := LeaseID, value := <<"Leader-V3">>}}, OCtx2),
            ok
        after 1000 -> throw({error, proclaim2_not_working})
        end
    after 1000 -> throw({error, proclaim1_not_working})
    end,
    ok.

campaign_async(_Config) ->
    LeaderKey = <<"CampaignTest">>,
    {ok, Pid1} = eetcd_election_leader_example:start_link(?Name, LeaderKey, "V1"),
    {ok, Pid2} = eetcd_election_leader_example:start_link(?Name, LeaderKey, "V2"),
    {ok, Leader1} = eetcd_election_leader_example:get_leader(Pid1),
    {ok, Leader2} = eetcd_election_leader_example:get_leader(Pid2),
    ?assertMatch(#{value := <<"V1">>}, Leader1),
    ?assertMatch(Leader1, Leader2),
    {ok, Campaign1} = eetcd_election_leader_example:get_campaign(Pid1),
    {ok, Campaign2} = eetcd_election_leader_example:get_campaign(Pid2),
    ?assertMatch(#{leader := #{name := LeaderKey}}, Campaign1),
    ?assertMatch(undefined, Campaign2),

    ok = eetcd_election_leader_example:resign(Pid1),
    timer:sleep(500),
    {ok, Campaign3} = eetcd_election_leader_example:get_campaign(Pid2),
    ?assertMatch(#{leader := #{name := LeaderKey}}, Campaign3),

    eetcd_election_leader_example:stop(Pid1),
    eetcd_election_leader_example:stop(Pid2),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

revoke_all_leases(?Name) ->
    {ok, #{leases := Leases}} = eetcd_lease_gen:lease_leases(?Name, #{}),
    lists:foreach(fun(#{'ID' := ID}) ->
        eetcd_lease_gen:lease_revoke(?Name, #{'ID' => ID})
                  end, Leases).

new_lease(Sec) ->
    {ok, #{'ID' := Id}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => Sec}),
    {ok, _Pid} = eetcd_lease:keep_alive(?Name, Id),
    Id.

