-module(eetcd_watch_example).

-behaviour(gen_server).
-define(NAME, watch_example_conn).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(KEY1, <<"heartbeat:">>).
-define(KEY2, <<"heartbeat2:">>).

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

    {ok, {Conn2, Mapping}}.

get_exist_services(KeyPrefix) ->
    Ctx = eetcd_kv:new(?NAME),
    Ctx1 = eetcd_kv:with_key(Ctx, KeyPrefix),
    Ctx2 = eetcd_kv:with_prefix(Ctx1),
    Ctx3 = eetcd_kv:with_keys_only(Ctx2),
    {ok, #{header := #{revision := Revision}, kvs := Services}} = eetcd_kv:get(Ctx3),
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

handle_info(Msg, {#{watch_ids := _WatchIds} = Conn, Mapping} = State) ->
    case eetcd_watch:watch_stream(Conn, Msg) of
        {ok, NewConn, WatchEvent} ->
            io:format("Received changes: ~p~n", [WatchEvent]),
            update_services(WatchEvent),
            io:format("ets: ~p~n", [ets:tab2list(?MODULE)]),
            {noreply, {NewConn, update_revision(Mapping, WatchEvent)}};
        {more, NewConn} ->
            {noreply, {NewConn, Mapping}};
        {error, _Reason} ->
            {NewConn, Watches} =
            maps:fold(fun(_WatchId, {Key, Rev}, {Conn0, Acc}) ->
                          {ok, NewConn0, NewWatchId} = watch_services_event_(Key, Rev, Conn0),
                          {NewConn0, [{NewWatchId, {Key, Rev}}|Acc]}
                      end, {undefined, []}, Mapping),
            {noreply, {NewConn, maps:from_list(Watches)}};
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
