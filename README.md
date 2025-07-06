[![Build Status](https://github.com/zhongwencool/eetcd/actions/workflows/ci.yml/badge.svg)](https://github.com/zhongwencool/eetcd/actions)
[![GitHub tag](https://img.shields.io/github/tag/zhongwencool/eetcd.svg)](https://github.com/zhongwencool/eetcd)
[![Hex.pm Version](https://img.shields.io/hexpm/v/eetcd.svg)](https://hex.pm/packages/eetcd)

# eetcd

Erlang client for the [etcd](https://github.com/etcd-io/etcd) API v3.
`eetcd` aims to be a high-quality, production-ready client for the Protocol Buffer-based etcd v3 API.
All core features are supported.
It includes reconnection, transaction, software transactional memory, high-level query builders and lease management, watchers.

See [the full v3 API documentation](https://etcd.io/docs/) for more:

1. Adding, Fetching and Deleting Keys;
2. Transaction;
3. Lease -- as well as a few convenience features like continuous keep alive;
4. Watch;
5. Maintenance -- User, Role, Authentication, Cluster, Alarms;
6. Lock;
7. Election.
8. `grpc.health.v1.Health`.

## Quick Start

### 1. Setup
```erlang
## rebar.config
{deps, [eetcd]}.

```
zero configuration.

### 2. Usage
All etcd3 API's are defined in [gRPC services](https://github.com/etcd-io/etcd/blob/master/etcdserver/etcdserverpb/rpc.proto), which categorize remote procedure calls (RPCs) understood by the etcd server.
A full listing of all etcd RPCs are documented in markdown in the [gRPC API listing](https://github.com/etcd-io/etcd/blob/master/Documentation/dev-guide/api_reference_v3.md).

Firstly, open eetcd when your application starts.

```erlang
-module(my_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(NAME, etcd_example_conn).

start(_StartType, _StartArgs) ->
    Endpoints = ["127.0.0.1:2379", "127.0.0.1:2579", "127.0.0.1:2479"],
    {ok, _Pid} = eetcd:open(?NAME, Endpoints),
    my_sup:start_link().

stop(_State) ->
    eetcd:close(?NAME),
    ok.
```

#### KV - Creates, updates, fetches, and deletes key-value pairs.
```erlang

%% creates
{ok,#{header :=
          #{cluster_id := 11360555963653019356,
            member_id := 13803658152347727308,raft_term := 5,
            revision := 6}}}
    = eetcd_kv_gen:put(?NAME, #{key => <<"key">>, value => <<"value">>}).

%% updates
Request1 = #{key => <<"KeyExist">>, value => <<"NewValue">>, ignore_vale => true},
{ok,#{header :=
          #{cluster_id := 11360555963653019356,
            member_id := 16409577466894847729,raft_term := 5,
            revision := 7}}}
     = eetcd_kv_gen:put(?NAME, Request1}).

Request2 = #{key => <<"KeyNoExist">>, value => <<"NewValue">>, ignore_vale => true},

{error,{grpc_error,#{'grpc-message' :=
                         <<"etcdserver: value is provided">>,
                     'grpc-status' := 3}}}
    = eetcd_kv:put(?NAME, Request2).

%% fetches
{ok,#{count := 1,
      header :=
          #{cluster_id := 11360555963653019356,
            member_id := 16409577466894847729,raft_term := 5,
            revision := 7},
      kvs :=
          [#{create_revision := 7,key := <<"KeyExist">>,lease := 0,
             mod_revision := 7,value := <<"NewValue">>,version := 1}],
      more := false}}
            = eetcd_kv:get(?NAME, <<"KeyExist">>).
%% fetches all keys
Request3 = eetcd_kv:with_sort(#{key => "\0", range_end => "\0"}, 'KEY', 'ASCEND'),
{ok,#{count := 2,
      header :=
          #{cluster_id := 11360555963653019356,
            member_id := 13803658152347727308,raft_term := 5,
            revision := 7},
      kvs :=
          [#{create_revision := 7,key := <<"KeyExist">>,lease := 0,
             mod_revision := 7,value := <<"NewValue">>,version := 1}
           %% ....
          ], more := false}}
    = eetcd_kv_gen:range(?NAME, Request3).

%% deletes
{ok,#{deleted := 1,
      header :=
          #{cluster_id := 11360555963653019356,
            member_id := 11020526813496739906,raft_term := 5,
            revision := 7},
      prev_kvs := []}}
   = eetcd_kv_gen:delete_range(?NAME, #{key => "KeyExist"}).

%% batch deletes
%% With prefix
Request4 = #{key => "K", range_end => eetcd:get_prefix_range_end("K")},
{ok,#{deleted := 100,
      header :=
          #{cluster_id := 11360555963653019356,
            member_id := 13803658152347727308,raft_term := 5,
            revision := 9},
      prev_kvs := []}}
   = eetcd_kv_gen:delete_range(?NAME, Request4).

```

#### Txn - Transaction

```erlang
%% implement etcd v2 CompareAndSwap by Txn
{ok,#{count := 1,
      header := #{revision := Revision},
      kvs :=
          [#{ mod_revision := ModRev,value := Value}],
      more := false}}
  = eetcd_kv:get(?NAME, Kv1),

Cmp = eetcd_compare:new(Kv1),
If = eetcd_compare:mod_revision(Cmp, "=", ModRev),
Then = eetcd_op:put(#{key => Key, value => <<"Change", Value/binary>>}),
Else = [],
eetcd_kv:txn(EtcdConnName, If, Then, Else).

```

#### Lease - Primitives for consuming client keep-alive messages.
```erlang
 1> eetcd_lease_gen:lease_grant(Name, TTL),
{ok,#{'ID' => 1076765125482045706,'TTL' => 100,error => <<>>,
      header =>
          #{cluster_id => 11360555963653019356,
            member_id => 16409577466894847729,raft_term => 5,
            revision => 9}}}
2> eetcd_lease:keep_alive(Name, 1076765125482045706).
{ok,<0.456.0>}

3> eetcd_lease_gen:lease_leases(Name, #{}).
{ok,#{header =>
          #{cluster_id => 11360555963653019356,
            member_id => 11020526813496739906,raft_term => 5,
            revision => 9},
      leases => [#{'ID' => 1076765125482045706}]}}

```
More detailed examples see [eetcd_kv_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_kv_SUITE.erl)  [eetcd_watch_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_watch_SUITE.erl)  [eetcd_lease_SUITE.erl](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_lease_SUITE.erl).

#### Watch - Monitors changes to keys.
```erlang
-module(watch_example).

-behaviour(gen_server).
-define(NAME, watch_example_conn).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    Registers = ["127.0.0.1:2379", "127.0.0.1:2579", "127.0.0.1:2479"],
    {ok, _Pid} = eetcd:open(?NAME, Registers),
    ets:new(?MODULE, [named_table, {read_concurrency, true}, public]),
    {ok, Services, Revision} = get_exist_services(),
    ets:insert(?MODULE, Services),
    {ok, Conn} = watch_services_event(Revision),
    {ok, Conn}.

get_exist_services() ->
    Key = <<"heartbeat:">>,
    Request = #{key => Key, range_end => eetcd:get_prefix_range_end(Key), keys_only => true},
    {ok, #{header := #{revision := Revision}, kvs := Services}} = eetcd_kv_gen:range(?NAME, Request),
    Services1 =
        [begin
             [_, Type, IP, Port] = binary:split(Key, [<<"|">>], [global]),
             {{IP, Port}, Type}
         end || #{key := Key} <- Services],
    {ok, Services1, Revision}.

watch_services_event(Revision) ->
    Key = <<"heartbeat:">>,
    Req = #{key => Key,
            range_end => eetcd:get_prefix_range_end(Key),
            start_revision => Revision + 1},
    eetcd_watch:watch(?NAME, Req).

handle_info(Msg, Conn) ->
    case eetcd_watch:watch_stream(Conn, Msg) of
        {ok, NewConn, WatchEvent} ->
            update_services(WatchEvent),
            {noreply, NewConn};
        {more, NewConn} ->
            {noreply, NewConn};
        {error, _Reason} ->
            #{watch_ids := Ids} = Conn,
            %% We expect there is only one watch in the Conn in this example
            %%
            %% TODO
            %% If there are more than one watch (aka multiplexing watch stream),
            %% this watcher process should keep the corresponding key/prefix to the watch id,
            %% to retrieve the correct revision of it.
            [#{revision := Revision}] = maps:values(Ids),
            {ok, NewConn} = watch_services_event(Revision),
            {noreply, NewConn};
        unknown ->
            {noreply, Conn}
    end.

handle_call(_Request, _From, Conn) ->
    {reply, ok, Conn}.

handle_cast(_Request, Conn) ->
    {noreply, Conn}.

terminate(_Reason, _Conn) ->
    eetcd:close(?NAME),
    ok.

code_change(_OldVsn, Conn, _Extra) ->
    {ok, Conn}.

update_services(#{events := Events}) ->
    [begin
         [_, Type, IP, Port] = binary:split(Key, [<<"|">>], [global]),
         case EventType of
             'PUT' -> ets:insert(?MODULE, {{IP, Port}, Type});
             'DELETE' -> ets:delete(?MODULE, {IP, Port})
         end
     end || #{kv := #{key := Key}, type := EventType} <- Events],
    ok.
```

We can use a single stream for multiplex watches, see [example](/test/eetcd_watch_example.erl).

#### Election Example
[Election Example](https://github.com/zhongwencool/eetcd/blob/master/test/eetcd_election_leader_example.erl)

#### Debug information
```erlang
1>eetcd:info().
| Name           | Status |   IP:Port    | Conn     | Gun      |LeaseNum|
| test           | Active |127.0.0.1:2379|<0.535.0> |<0.536.0> |      1 |
| test           | Active |127.0.0.1:2579|<0.535.0> |<0.539.0> |      2 |
| Name           | Status |   IP:Port    | Conn     | ReconnectSecond   |
| test           | Freeze |127.0.0.1:2479|<0.535.0> |   1.6             |
```

- `Active` is normal connection.
- `Freeze` is a broken connection who try to reconnect after `ReconnectSecond`.

## Test

```erlang
rebar3 ct
```

## Gen proto and client file

```erlang
rebar3 protobuf compile
rebar3 etcd gen
```

## Migration from eetcd 0.3.x to 0.4.x

eetcd 0.4.x now dependents on Gun 2.0, which introduced some breaking changes,
and propagate to eetcd.

The prior transport options are split into `tcp_opts` and `tls_opts` and moved
inside the new `eetcd:opts()` parameter. As a result, the functions `eetcd:open/4,5`
have been replaced with `eetcd:open/2,3`.

Likewise, the transport options for `eetcd_maintenance` APIs are split into
`tcp_opts` and `tls_opts` as well.

- The function `eetcd:open/4,5` has been replaced with `eetcd:open/3`.
- The function `eetcd_maintenance:defragment/3` has been replaced with `eetcd_maintenance:defragment/2`.
- The function `eetcd_maintenance:status/3` has been replaced with `eetcd_maintenance:status/2`.
- The function `eetcd_maintenance:has_kv/4` has been replaced with `eetcd_maintenance:has_kv/3`.

New options `{domain_lookup_timeout, Interval}` and `{tls_handshake_timeout, Interval}`
have been added for `eetcd:open/3`. Alone with the prior `{connect_timeout, Interval}`,
it allows the underlining Gun library to get separate events when connecting,
the domain lookup, connection and TLS handshakes.
- `tls_opts` Passed to Gun.

Read more details of Gun options in the [Gun 2.0 manual](https://ninenines.eu/docs/en/gun/2.0/manual/gun/).
