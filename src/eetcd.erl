-module(eetcd).
-include("eetcd.hrl").
%% API
-export([test/0]).
-export([open/2, open/4, open/5, close/1]).
-export([new/1, with_timeout/2]).
-export([info/0]).

test() ->
    application:ensure_all_started(eetcd),
    logger:set_primary_config(level, info),
    {ok, _Pid} = eetcd:open(test, ["127.0.0.1:2379", "127.0.0.1:2479", "127.0.0.1:2579"], tcp, []),
    R = eetcd_kv:put(test, "test", <<"2">>),
    io:format("~p~n", [R]),
    R1 = eetcd_kv:get(test, "test"),
    io:format("~p~n", [R1]),
    Key = "test",
    Cmp = eetcd_compare:new(Key),
    If = eetcd_compare:value(Cmp, "=", <<"2">>),
    Then = eetcd_op:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(), Key), <<"1000">>)),
    Else = eetcd_op:delete(eetcd_kv:with_key(eetcd_kv:new(), Key)),
    eetcd_kv:txn(test, [If], [Then], [Else]).
    %% eetcd:close(test),
    %%ok.

%% @doc Connects to a etcd server on TCP port
%% Port on the host with IP address Address, such as:
%% `open(test,["127.0.0.1:2379","127.0.0.1:2479","127.0.0.1:2579"]).'
-spec open(name(), [string()]) -> {ok, pid()} | {error, any()}.
open(Name, Hosts) ->
    open(Name, Hosts, tcp, [], []).

%% @doc Connects to a etcd server.
%%
-spec open(name(),
    [string()],
    tcp | tls | ssl,
    [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok, pid()} | {error, any()}.
open(Name, Hosts, Transport, TransportOpts) ->
    open(Name, Hosts, Transport, TransportOpts, []).

%% @doc Connects to a etcd server.
%% ssl:connect_option() see all options in ssl_api.hrl
%% such as [{certfile, Certfile}, {keyfile, Keyfile}] or [{cert, Cert}, {key, Key}].
%% Default mode is `connect_all', it creates multiple sub-connections (one sub-connection per each endpoint).
%% `{mode, random}' creates only one connection to a random endpoint.
-spec open(name(),
    [string()],
    tcp | tls | ssl,
    [gen_tcp:connect_option()] | [ssl:connect_option()],
    [{mode, connect_all|random}]) ->
    {ok, pid()} | {error, any()}.
open(Name, Hosts, Transport, TransportOpts, Options) ->
    Cluster = [begin [IP, Port] = string:tokens(Host, ":"), {IP, list_to_integer(Port)} end || Host <- Hosts],
    eetcd_conn_sup:start_child([{Name, Cluster, Transport, TransportOpts, Options}]).

-spec close(name()) -> ok | {error, eetcd_conn_unavailable}.
close(Name) ->
    case eetcd_conn:find_by_name(Name) of
        {ok, Pid} -> eetcd_conn:close(Pid);
        Err -> Err
    end.

%%% @doc etcd's overview.
-spec info() -> any().
info() ->
    Leases = eetcd_lease_sup:info(),
    Conns = eetcd_conn_sup:info(),
    [begin
         {Name, #{active_conns := Actives, freeze_conns := Freezes}} = Conn,
         case Actives =/= [] of
             true ->
                 io:format("|\e[4m\e[48;2;80;80;80m Name           | Status |   IP:Port    | Gun      |LeaseNum\e[0m|~n");
             false -> ignore
         end,
         [begin
              io:format("| ~-15.15s| Active |~s:~w|~p |~7.7w | ~n", [Name, IP, Port, Gun, maps:get(Gun, Leases, 0)])
          end || {{IP, Port}, Gun, _GRef} <- Actives],
         case Freezes =/= [] of
             true ->
                 io:format("|\e[4m\e[48;2;184;0;0m Name           | Status |   IP:Port    | ReconnectSecond   \e[49m\e[0m|~n");
             false -> ignore
         end,
         [begin
              io:format("| ~-15.15s| Freeze |~s:~w|   ~-15.15w | ~n", [Name, IP, Port, Ms / 1000])
          end || {{IP, Port}, Ms} <- Freezes]
     end || Conn <- Conns],
    ok.

%%% @doc Create context for request.
-spec new(atom()|reference()) -> context().
new(ConnName) when is_atom(ConnName) orelse is_reference(ConnName) -> #{eetcd_conn_name => ConnName};
new(Context) when is_map(Context) -> Context.

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely. Default value is 5000.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()) -> context().
with_timeout(Context, Timeout) when is_integer(Timeout) ->
    maps:put(eetcd_reply_timeout, Timeout, Context).