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
    {ok, #{'ID' := LeaseID}} = eetcd_lease_gen:lease_grant(Etcd, #{'TTL' => 8}),
    {ok, _} = eetcd_lease:keep_alive(Etcd, LeaseID),
    Self = self(),
    Pid = spawn_link(fun() ->
        Res = eetcd_election_gen:campaign(Etcd, #{name => LeaderKey,
                                                  lease => LeaseID,
                                                  value => Value}),
        Self ! {campaign_response, self(), Res}
    end),
    {ok, Observe} = eetcd_election:observe(Etcd, LeaderKey, 2500),
    {ok, #{etcd => Etcd, lease => LeaseID, observe => Observe, campaign_pid => Pid}}.

handle_call(get_leader, _From, State = #{observe := Observe}) ->
    #{leader := Leader} = Observe,
    {reply, {ok, Leader}, State};

handle_call(get_campaign, _From, State) ->
    {reply, {ok, maps:get(campaign, State, undefined)}, State};

handle_call(resign, _From, #{etcd := Etcd} = State) ->
    Result = 
        case maps:get(campaign, State, undefined) of
            undefined -> ignored; %% no campaign, nothing to resign
            #{leader := Leader} ->
                case eetcd_election_gen:resign(Etcd, #{leader => Leader}) of
                    {ok, _} -> ok;
                    {error, _Reason} = E -> E
                end
        end,
    {reply, Result, State};
handle_call(stop, _From, State = #{}) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State = #{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #{}) ->
    {noreply, State}.

handle_info({campaign_response, Pid, Resp}, #{campaign_pid := Pid} = State) ->
    case Resp of
        {ok, #{leader := Leader} = NewCampaign} ->
            %% Only get this response when you win campaign by yourself.
            %% You are leader!
            win_campaign_event(Leader),
            {noreply, State#{campaign => NewCampaign}};
        {error, Reason} -> %% you can just let it crash and restart process or recampaign !!!
            campaign_unexpected_error(Reason),
            {noreply, State}
    end;

handle_info(Msg, #{observe := Observe} = State) ->
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
    end.

terminate(_Reason, _State = #{etcd := Etcd, lease := LeaseID}) ->
    eetcd_lease_gen:lease_revoke(Etcd, #{'ID' => LeaseID}),
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
    logger:info("observe unexpected error:~p", [Reason]),
    "Todo: try to reobserve after some sleep.".

handle_info_your_own_msg(Msg, State) ->
    logger:info("handle info your own msg:~p ~p", [Msg, State]),
    "Todo".
