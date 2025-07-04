-module(eetcd_lock_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    acquire_lock_and_release_it/1,
    acquire_lock_and_reacquire_after_expiration/1,
    acquire_lock_and_attempt_to_reacquire_that_times_out/1
]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        acquire_lock_and_release_it,
        acquire_lock_and_reacquire_after_expiration,
        acquire_lock_and_attempt_to_reacquire_that_times_out
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"],
                            [{mode, random}, {transport, tcp}]),
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

acquire_lock_and_release_it(_Config) ->
    TTL = 3,
    {ok, #{'ID' := LeaseID}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => TTL}),
    Key = <<"eetcd_key1">>,
    {ok, #{key := GeneratedKey}} = eetcd_lock_gen:lock(?Name, #{name => Key, lease => LeaseID}),
    ?assertMatch({ok, _}, eetcd_lock_gen:unlock(?Name, #{key => GeneratedKey})),
    ok.

acquire_lock_and_reacquire_after_expiration(_Config) ->
    TTL = 1,

    {ok, #{'ID' := LeaseID1}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => TTL}),
    Key = <<"eetcd_key2">>,
    {ok, _} = eetcd_lock_gen:lock(?Name, #{name => Key, lease => LeaseID1}),

    %% wait till the lease expires
    timer:sleep(1500),
    {ok, #{'ID' := LeaseID2}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => TTL}),
    %% the user provided key remains the same
    {ok, #{key := GeneratedKey}} = eetcd_lock_gen:lock(?Name, #{name => Key, lease => LeaseID2}),

    ?assertMatch({ok, _}, eetcd_lock_gen:unlock(?Name, #{key => GeneratedKey})),
    ok.

acquire_lock_and_attempt_to_reacquire_that_times_out(_Config) ->
    TTL = 4,
    Key = <<"eetcd_key3">>,

    {ok, #{'ID' := LeaseID1}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => TTL}),
    {ok, #{key := GeneratedKey}} = eetcd_lock_gen:lock(?Name, #{name => Key, lease => LeaseID1}),

    %% this with a different session call will block and time out
    {ok, #{'ID' := LeaseID2}} = eetcd_lease_gen:lease_grant(?Name, #{'TTL' => TTL}),
    ?assertEqual({error, timeout},
                 eetcd_lock_gen:lock(?Name, #{name => Key, lease => LeaseID2},
                                     [{reply_timeout, 2000}])),
    ?assertMatch({ok, _}, eetcd_lock_gen:unlock(?Name, #{key => GeneratedKey})),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

revoke_all_leases(?Name) ->
    {ok, #{leases := Leases}} = eetcd_lease_gen:lease_leases(?Name, #{}),
    lists:foreach(fun(#{'ID' := ID}) ->
      eetcd_lease_gen:lease_revoke(?Name, #{'ID' => ID})
    end, Leases).
