-module(eetcd_election_leader_example).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1]).
-export([get_leader/1, get_campaign/1, resign/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, LeaderKey, Value) ->
    gen_server:start_link(?MODULE, [Name, LeaderKey, Value], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

get_leader(Pid) ->
    gen_server:call(Pid, get_leader).

get_campaign(Pid) ->
    gen_server:call(Pid, get_campaign).

resign(Pid) ->
    gen_server:call(Pid, resign).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Etcd, LeaderKey, Value]) ->
    logger:set_primary_config(#{level => info}),
    erlang:process_flag(trap_exit, true),
    {ok, #{'ID' := LeaseID}} = eetcd_lease:grant(Etcd, 8),
    {ok, _} = eetcd_lease:keep_alive(Etcd, LeaseID),
    {ok, Campaign} = eetcd_election:campaign_request(Etcd, LeaderKey, LeaseID, Value),
    {ok, Observe} = eetcd_election:observe(Etcd, LeaderKey, 2500),
    {ok, #{etcd => Etcd, lease => LeaseID, campaign => Campaign, observe => Observe}}.

handle_call(get_leader, _From, State = #{observe := Observe}) ->
    #{leader := Leader} = Observe,
    {reply, {ok, Leader}, State};

handle_call(get_campaign, _From, State = #{campaign := Campaign}) ->
    {reply, {ok, Campaign}, State};

handle_call(resign, _From, State) ->
    #{etcd := Etcd, campaign := #{campaign := Leader}} = State,
    case is_map(Leader) of
        true -> eetcd_election:resign(Etcd, Leader);
        false -> ignore
    end,
    {reply, is_map(Leader), State};
handle_call(stop, _From, State = #{}) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State = #{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #{}) ->
    {noreply, State}.

handle_info(Msg, State) ->
    #{campaign := Campaign, observe := Observe} = State,
    case eetcd_election:campaign_response(Campaign, Msg) of
        {ok, NewCampaign = #{campaign := Leader}} ->
            %% Only get this response when you win campaign by yourself.
            %% You are leader!
            win_campaign_event(Leader),
            {noreply, State#{campaign => NewCampaign}};
        {error, Reason} -> %% you can just let it crash and restart process or recampaign !!!
            campaign_unexpected_error(Reason),
            {noreply, State};
        unknown ->
            case eetcd_election:observe_stream(Observe, Msg) of
                {ok, NewObserve = #{leader := Leader}} ->
                    leader_change_event(Leader),
                    {noreply, State#{observe => NewObserve}};
                {error, Reason} -> %% you can just let it crash and restart process
                    observe_unexpected_error(Reason),
                    {noreply, State};
                unknown ->
                    handle_info_your_own_msg(Msg, State),
                    {noreply, State}
            end
    end.

terminate(_Reason, _State = #{etcd := Etcd, lease := LeaseID}) ->
    eetcd_lease:revoke(Etcd, LeaseID),
    ok.

code_change(_OldVsn, State = #{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
win_campaign_event(Leader) ->
    logger:info("win campaign event:~p", [Leader]),
    "Todo".

campaign_unexpected_error(Reason) ->
    logger:info("campaign unexpected error:~p", [Reason]),
    "Todo: try to recampaign".

leader_change_event(Leader) ->
    logger:info("leader change event:~p", [Leader]),
    "Todo".

observe_unexpected_error(Reason) ->
    logger:info("observe unexpect error:~p", [Reason]),
    "Todo: try to reobserve after some sleep.".

handle_info_your_own_msg(Msg, State) ->
    logger:info("hanle info your own msg:~p ~p", [Msg, State]),
    "Todo".