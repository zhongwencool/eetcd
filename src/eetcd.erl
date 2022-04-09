-module(eetcd).
-include("eetcd.hrl").
%% API
-export([open/2, open/4, open/5, close/1]).
-export([info/0]).
-export([new/1, with_timeout/2]).
-export([get_prefix_range_end/1]).

%% @doc Connects to a etcd server on TCP port
%% Port on the host with IP address Address, such as:
%% `open(test,["127.0.0.1:2379","127.0.0.1:2479","127.0.0.1:2579"]).'
-spec open(name(), [string()]) -> {ok, pid()} | {error, any()}.
open(Name, Hosts) ->
    open(Name, Hosts, [], tcp, []).

%% @doc Connects to a etcd server.
-spec open(name(),
    [string()],
    tcp | tls | ssl,
    [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok, pid()} | {error, any()}.
open(Name, Hosts, Transport, TransportOpts) ->
    open(Name, Hosts, [], Transport, TransportOpts).

%% @doc Connects to a etcd server.
%% ssl:connect_option() see all options in ssl_api.hrl
%% such as [{certfile, Certfile}, {keyfile, Keyfile}] or [{cert, Cert}, {key, Key}].
%%
%% Default mode is `connect_all', it creates multiple sub-connections (one sub-connection per each endpoint).
%% The balancing policy is round robin.
%% For instance, in 5-node cluster, `connect_all' would require 5 TCP connections,
%% This may consume more resources but provide more flexible load balance with better failover performance.
%% `eetcd_conn' will do his best to keep all connections normal, and try to reconnect when connection is broken.
%% The reconnect millisecond is 200 400 800 1600 3200 6400 12800 25600, and keep recycling this reconnection time until normal.
%%
%% `{mode, random}' creates only one connection to a random endpoint,
%% it would pick one address and use it to send all client requests.
%% The pinned address is maintained until the client connection is closed.
%% When the client receives an error, it randomly picks another normal endpoint.
%%
%% `{connect_timeout, Interval}' is the connection timeout. Defaults to one second (1000).
%% `{retry, Attempts}' is the number of times it will try to reconnect on failure before giving up. Defaults to zero (disabled).
%% `{retry_timeout, Interval}' is the time between retries in milliseconds.
%%
%% `{auto_sync_interval_ms, Interval}' sets the default `Interval' in milliseconds of auto-sync.
%% Default is 0, which means no auto-sync. If enabled auto-sync, you can set `auto_sync_interval_ms'
%% in application env to change the interval. If disabled, the `auto_sync_interval_ms' in application
%% env will be ignored. With auto-sync enabled, eetcd will automatically sync the cluster member
%% list via the MemberList API of etcd, and will try to connect any new endpoints if in `connect_all'
%% mode.
%%
%% `[{name, string()},{password, string()}]' generates an authentication token based on a given user name and password.
%%
%% You can use `eetcd:info/0' to see the internal connection status.
-spec open(name(),
    [string()],
    [
      {mode, connect_all | random}
      | {name, string()}
      | {password, string()}
      | {retry, non_neg_integer()}
      | {retry_timeout, pos_integer()}
      | {connect_timeout, timeout()}
    ],
    tcp | tls | ssl,
    [gen_tcp:connect_option()] | [ssl:connect_option()]) ->
    {ok, pid()} | {error, any()}.
open(Name, Hosts, Options, Transport, TransportOpts) ->
    Cluster = [begin [IP, Port] = string:tokens(Host, ":"), {IP, list_to_integer(Port)} end || Host <- Hosts],
    eetcd_conn_sup:start_child([{Name, Cluster, Options, Transport, TransportOpts}]).

%% @doc close connections with etcd server.
-spec close(name()) -> ok | {error, eetcd_conn_unavailable}.
close(Name) ->
    case eetcd_conn:lookup(Name) of
        {ok, Pid} -> eetcd_conn:close(Pid);
        Err -> Err
    end.

%%% @doc etcd's overview.
-spec info() -> any().
info() ->
    Leases = eetcd_lease_sup:info(),
    Conns = eetcd_conn_sup:info(),
    io:format("|\e[4m\e[48;2;80;80;80m Name           | Status |   IP:Port    | Conn     | Gun      |LeaseNum\e[0m|~n"),
    [begin
         {Name, #{etcd := Etcd, active_conns := Actives}} = Conn,
         [begin
              io:format("| ~-15.15s| Active |~s:~w|~p |~p |~7.7w | ~n", [Name, IP, Port, Etcd, Gun, maps:get(Gun, Leases, 0)])
          end || {{IP, Port}, Gun, _Token} <- Actives]
     end || Conn <- Conns],
    io:format("|\e[4m\e[48;2;184;0;0m Name           | Status |   IP:Port    | Conn     | ReconnectSecond   \e[49m\e[0m|~n"),
    [begin
         {Name, #{etcd := Etcd, freeze_conns := Freezes}} = Conn,
         [begin
              io:format("| ~-15.15s| Freeze |~s:~w|~p |   ~-15.15w | ~n", [Name, IP, Port, Etcd, Ms / 1000])
          end || {{IP, Port}, Ms} <- Freezes]
     end || Conn <- Conns],
    ok.

%%% @doc Create context for request.
-spec new(atom()|reference()|context()) -> context().
new(ConnName) when is_atom(ConnName) orelse is_reference(ConnName) -> #{eetcd_conn_name => ConnName};
new(Context) when is_map(Context) -> Context.

%% @doc Timeout is an integer greater than zero which specifies how many milliseconds to wait for a reply,
%% or the atom infinity to wait indefinitely.
%% If no reply is received within the specified time, the function call fails with `{error, timeout}'.
-spec with_timeout(context(), pos_integer()|infinity) -> context().
with_timeout(Context, Timeout) when is_integer(Timeout); Timeout == infinity ->
    case Timeout < 100 of
        true ->
            Reason =
                lists:flatten(
                    io_lib:format("Setting timeout to ~wms(less than 100ms) is not allowed", [Timeout])),
            throw({error, Reason});
        false -> ok
    end,
    maps:put(eetcd_reply_timeout, Timeout, Context).

-define(UNBOUND_RANGE_END, "\0").

get_prefix_range_end(Key) ->
    case eetcd_data_coercion:to_list(Key) of
        [] -> ?UNBOUND_RANGE_END;
        List0 when is_list(List0) ->
            %% find last character < 0xff (255)
            {Prefix, Suffix} = lists:splitwith(fun(Ch) -> Ch < 255 end, List0),
            case Prefix of
                %% keys where all characters >= 0xff are returned as is
                []     -> Key;
                Prefix ->
                    %% advance the last character
                    Ord = lists:last(Prefix),
                    lists:droplast(Prefix) ++ [Ord + 1] ++ Suffix
            end
    end.
