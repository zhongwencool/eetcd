-module(eetcd).
-include("eetcd.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([open/2, open/3, close/1]).
-export([info/0]).
-export([get_prefix_range_end/1]).
-export_type([opts/0, request_opts/0, stream_ref/0]).

-type opts() :: [ {mode, connect_all | random} |
                  {transport, tcp | tls | ssl} |
                  {name, string()} |
                  {password, string()} |
                  {auto_sync_interval_ms, non_neg_integer()} |
                  {retry, non_neg_integer()} |
                  {retry_timeout, pos_integer()} |
                  {connect_timeout, timeout()} |
                  {domain_lookup_timeout, timeout()} |
                  {tls_handshake_timeout, timeout()} |
                  {tcp_opts, [gen_tcp:connect_option()]} |
                  {tls_opts, [ssl:tls_client_option()]}
                ].

-type request_opts() :: [ {reply_timeout, pos_integer()} ].
-type stream_ref() :: gun:stream_ref().

%% @doc Connects to a etcd server on TCP port
%% Port on the host with IP address Address, such as:
%% `open(test,["127.0.0.1:2379","127.0.0.1:2479","127.0.0.1:2579"]).'
-spec open(etcd_name(), [string()]) -> {ok, pid()} | {error, any()}.
open(EtcdName, Hosts) ->
    open(EtcdName, Hosts, [{transport, tcp}]).

%% @doc Connects to a etcd server.
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
%% `{domain_lookup_timeout, Interval}' is the domain_lookup_timeout timeout. Defaults to one second (1000).
%% `{tls_handshake_timeout, Interval}' is the tls_handshake_timeout timeout. Defaults to three second (3000).
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
%% `[{name, string()}, {password, string()}]' generates an authentication token based on a given user name and password.
%%
%% `{tcp_opts, [gen_tcp:connect_option()]}' and `{tls_opts, [ssl:tls_client_option()]}' are the
%% options for gun:open/3 in Gun 2.0.
%%
%% See all TCP options in {@link gen_tcp} module.
%%
%% See all TLS client options in {@link ssl} module,
%% such as `[{certfile, Certfile}, {keyfile, Keyfile}] or [{cert, Cert}, {key, Key}]'.
%%
%% Read more details of gun options in the
%% [https://ninenines.eu/docs/en/gun/2.0/manual/gun/ Gun 2.0 manual].
%%
%% You can use `eetcd:info/0' to see the internal connection status.
-spec open(etcd_name(), [string()], opts()) -> {ok, pid()} | {error, any()}.
open(EtcdName, Hosts, Options) ->
    Hosts1 = [begin [IP, Port] = string:tokens(Host, ":"), {IP, list_to_integer(Port)} end || Host <- Hosts],
    case eetcd_conn_sup:start_child(EtcdName, Hosts1, Options) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, _}} ->
            {error, already_started};
        {error, {E, _Spec}} ->
            {error, E}
    end.

%% @doc close connections with etcd server.
-spec close(etcd_name()) -> ok | {error, not_found}.
close(EtcdName) ->
    eetcd_conn_sup:stop_child(EtcdName).

%%% @doc etcd's overview.
-spec info() -> any().
info() ->
    Leases = eetcd_lease_sup:info(),
    Conns = eetcd_conn_sup:info(),
    io:format("|\e[4m\e[48;2;80;80;80m Name            |   Status |     MemberID     |         Host:Port     | Conn      | Gun       | LeaseNum \e[0m|~n"),
    [begin
         {Name, #{etcd := Etcd, active_conns := Actives, opening_conns := Openings, members := Members}} = Conn,
         Availables = [{X, "Active"}|| X <- Actives] ++ [{Y, "Opening"} || Y <- Openings],
         [begin
              {Host, Port, _Transport} = maps:get(Id, Members),
              io:format("| ~-15.15s | ~8s | ~16s | ~15s:~-5w | ~p | ~p | ~8.7w |~n",
                        [Name, Status, eetcd_conn:member_id_hex(Id), Host, Port, Etcd, Gun, maps:get(Gun, Leases, 0)])
          end || {{Id, Gun, _MRef}, Status} <- Availables]
     end || Conn <- Conns],

    io:format("|\e[4m\e[48;2;184;0;0m Name            |   Status |     MemberID     |         Host:Port     | Conn      | ReconnectSecond      \e[49m\e[0m|~n"),
    [begin
         {Name, #{etcd := Etcd, freeze_conns := Freezes}} = Conn,
         [begin
              io:format("| ~-15.15s |   Freeze | ~16s | ~15s:~-5w | ~p |   ~-18.15w |~n",
                        [Name, eetcd_conn:member_id_hex(Id), Host, Port, Etcd, Ms / 1000])
          end || {Id, Host, Port, Ms} <- Freezes]
     end || Conn <- Conns],
    ok.

-define(UNBOUND_RANGE_END, "\0").

get_prefix_range_end(Key) ->
    RangeEndRev = lists:reverse(eetcd_data_coercion:to_list(Key)),
    lists:reverse(find_prefix_rev(RangeEndRev)).

find_prefix_rev([]) -> ?UNBOUND_RANGE_END;
find_prefix_rev([H | T]) when H < 255 -> [H + 1 | T];
find_prefix_rev([_ | T]) -> find_prefix_rev(T).

-ifdef(TEST).
get_prefix_range_end_test() ->
    ?assertEqual(?UNBOUND_RANGE_END, get_prefix_range_end([])),
    ?assertEqual("b", get_prefix_range_end("a")),
    ?assertEqual("a\x01", get_prefix_range_end("a\x00")),
    ?assertEqual("a\x02", get_prefix_range_end("a\x01")),
    ?assertEqual("b", get_prefix_range_end("a\xff")),
    ?assertEqual("c", get_prefix_range_end("b")),
    ?assertEqual("ab", get_prefix_range_end("aa")),
    ok.
-endif.

