%%%-------------------------------------------------------------------
%%% @author zhongwen
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 1月 2020 11:13
%%%-------------------------------------------------------------------
-module(test).
-author("zhongwen").

-behaviour(gen_statem).

%% API
-export([start_link/0, send/1]).

%% gen_statem callbacks
-export([init/1, format_status/2, state_name/3, handle_event/4, terminate/3,
    code_change/4, callback_mode/0]).

-define(SERVER, ?MODULE).

send(Msg) -> gen_statem:cast(?MODULE, Msg).
-record(test_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
    {ok, state_name, #test_state{}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    handle_event_function.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
state_name(_EventType, _EventContent, State = #test_state{}) ->
    NextStateName = next_state,
    {next_state, NextStateName, State, [postpone]}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(internal, good1, _StateName, State = #test_state{}) ->
    io:format("3~p~n", [{_StateName, State}]),
    {keep_state, State};
handle_event(internal, EventContent, _StateName, State = #test_state{}) ->
    io:format("2~p~n", [{_StateName, State}]),
    {next_state, EventContent, State, [{next_event, internal, good1}]};
handle_event(_EventType, EventContent, _StateName, State = #test_state{}) ->
    io:format("1~p~n", [{_EventType, EventContent, _StateName, State}]),
    NextStateName = the_next_state_name,
    {next_state, NextStateName, State, [{next_event, internal, erlang:timestamp()}]}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #test_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #test_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
