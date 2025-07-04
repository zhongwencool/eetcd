-module(eetcd_cluster_SUITE).

%% API

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([member/1]).

-define(Name, ?MODULE).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [
        member
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    {ok, _Pid} = eetcd:open(?Name, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"]),
    Config.

end_per_suite(_Config) ->
    eetcd:close(?Name),
    application:stop(eetcd),
    ok.

member(_Config) ->
    %% TODO HOW TO
    %% add
    %% peerURLs is the list of URLs the added member will use to communicate with the cluster.
    %% member is the member information for the added member.
    %% members is a list of all members after adding the new member.
    %% FakePeerURLs = [<<"http://127.0.0.1:2480">>],
    %% {ok,#'Etcd.MemberAddResponse'{
    %%    header =#'Etcd.ResponseHeader'{},
    %%    member = #'Etcd.Member'{peerURLs = FakePeerURLs, clientURLs = []},
    %%    members = [_, _]}}
    %%    = eetcd_cluster:member_add(#'Etcd.MemberAddRequest'{'peerURLs' = FakePeerURLs}),

    %% list
    %% members is a list of all members associated with the cluster.
    {ok, #{members := Members}} = eetcd_cluster_gen:member_list(?Name, #{}),
    true = (length(Members) >= 1),
    %% #'Etcd.Member'{'ID' = ID} = lists:keyfind(FakePeerURLs, #'Etcd.Member'.peerURLs, Members),

    %% update
    %% ID is the member ID of the member to update.
    %% peerURLs is the new list of URLs the member will use to communicate with the cluster.
    %% members is a list of all members after updating the member.
    %% TODO NOT WORK
    %% {ok, #'Etcd.MemberUpdateResponse'{members = Members2}}
    %%    = eetcd_cluster:member_update(#'Etcd.MemberUpdateRequest'{'ID' = ID, peerURLs = [<<"http://127.0.0.1:2480">>]}),
    %% #'Etcd.Member'{'ID' = ID} = lists:keyfind([<<"http://127.0.0.1:2480">>], #'Etcd.Member'.peerURLs, Members2),

    %% remove
    %% members is a list of all members after updating the member.
    %% {ok, #'Etcd.MemberRemoveResponse'{members = [_]}}
    %%   = eetcd_cluster:member_remove(#'Etcd.MemberRemoveRequest'{'ID' = ID}),
    ok.
