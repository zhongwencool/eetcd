-module(eetcd_watch_example).

-behaviour(gen_server).
-define(NAME, watch_example_conn).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(KEY1, <<"heartbeat:">>).
-define(KEY2, <<"heartbeat2:">>).
-define(CHECK_RETRY_MS, 3000).

start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    Registers = ["127.0.0.1:2379"],
    {ok, _Pid} = eetcd:open(?NAME, Registers),

    ets:new(?MODULE, [named_table, {read_concurrency, true}, public]),

    {ok, Services, Rev1} = get_exist_services(?KEY1),
    ets:insert(?MODULE, Services),
    {ok, Services2, Rev2} = get_exist_services(?KEY2),
    ets:insert(?MODULE, Services2),

    {ok, Conn1, WatchId1} = watch_services_event_(?KEY1, Rev1 + 1),
    {ok, Conn2, WatchId2} = watch_services_event_(?KEY2, Rev2 + 1, Conn1),
    Mapping = #{WatchId1 => {?KEY1, Rev1}, WatchId2 => {?KEY2, Rev2}},

    {ok, ensure_retry(#{conn      => Conn2,
                        mapping   => Mapping,
                        retries   => [],
                        retry_ref => undefined})}.

get_exist_services(KeyPrefix) ->
    Req = #{
        key => KeyPrefix,
        range_end => eetcd:get_prefix_range_end(KeyPrefix),
        keys_only => true
    },
    {ok, #{header := #{revision := Revision}, kvs := Services}} = eetcd_kv_gen:range(?NAME, Req),
    Services1 =
        [begin
             [_, Type, IP, Port] = binary:split(Key, [<<"|">>], [global]),
             {{IP, Port}, Type}
         end || #{key := Key} <- Services],
    {ok, Services1, Revision}.

watch_services_event_(Key, Revision) ->
    watch_services_event_(Key, Revision, undefined).

watch_services_event_(Key, Revision, Conn) ->
    ReqInit = eetcd_watch:new(),
    ReqKey = eetcd_watch:with_key(ReqInit, Key),
    ReqPrefix = eetcd_watch:with_prefix(ReqKey),
    Req = eetcd_watch:with_start_revision(ReqPrefix, Revision),
    eetcd_watch:watch(?NAME, Req, Conn).

handle_info(retry, #{retries := []} = State) ->
    {noreply, ensure_retry(State#{retry_ref => undefined})};
handle_info(retry, State) ->
    NewState = retry(State),
    {noreply, ensure_retry(NewState#{retry_ref => undefined})};

handle_info(Msg, #{conn := Conn, mapping := Mapping} = State) ->
    case eetcd_watch:watch_stream(Conn, Msg) of
        {ok, NewConn, WatchEvent} ->
            io:format("Received changes: ~p~n", [WatchEvent]),
            update_services(WatchEvent),
            io:format("ets: ~p~n", [ets:tab2list(?MODULE)]),
            {noreply, State#{conn => NewConn, mapping => update_revision(Mapping, WatchEvent)}};
        {more, NewConn} ->
            {noreply, State#{conn => NewConn}};
        {error, Reason} ->
            %% TODO handle error like stream error and/or connection error
            io:format("watch_stream error for ~p, state: ~p~n", [Reason, State]),
            {noreply, State#{conn => undefined, retries => maps:values(Mapping)}};
        unknown ->
            {noreply, State}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    eetcd:close(?NAME),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_services(#{events := Events}) ->
    [begin
         X = binary:split(Key, [<<"|">>], [global]),
         [_, Type, IP, Port] = X,
         case EventType of
             'PUT' -> ets:insert(?MODULE, {{IP, Port}, Type});
             'DELETE' -> ets:delete(?MODULE, {IP, Port})
         end
     end || #{kv := #{key := Key}, type := EventType} <- Events],
    ok.

update_revision(Mapping, #{header := #{revision := Rev}, watch_id := WatchId}) ->
    maps:update_with(WatchId, fun({Key, _}) -> {Key, Rev} end, Mapping).

ensure_retry(#{retry_ref := undefined} = State) ->
    Ref = erlang:send_after(?CHECK_RETRY_MS, self(), retry),
    State#{retry_ref := Ref};
ensure_retry(State) ->
    State.

retry(#{mapping := _Mapping, retries := [], conn := _Conn} = State) -> State;
retry(#{mapping := Mapping, retries := Retries, conn := Conn} = State) ->
    {NewConn, Watches, NewRetries} =
    lists:foldl(fun({Key, Rev}, {Conn0, Acc, Retries0}) ->
                      case watch_services_event_(Key, Rev + 1, Conn0) of
                          {ok, NewConn0, NewWatchId} ->
                              {NewConn0, [{NewWatchId, {Key, Rev}}|Acc], Retries0};
                          {error, Reason} ->
                              io:format("Watch key ~p error: ~p~n", [Key, Reason]),
                              {Conn0, Acc, [{Key, Rev}|Retries0]}
                      end
              end, {Conn, [], []}, Retries),
    State#{conn => NewConn,
           mapping => maps:merge(Mapping, maps:from_list(Watches)),
           retries => NewRetries}.
