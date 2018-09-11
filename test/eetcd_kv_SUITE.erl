
-module(eetcd_kv_SUITE).


-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1]).

-export([base_operation/1]).

-include("router_pb.hrl").

suite() ->
    [{timetrap,{minutes,2}}].

all() ->
    [
        base_operation
    ].

groups() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(eetcd),
    Config.

end_per_suite(_Config) ->
    application:stop(eetcd),
    ok.


base_operation(_Config) ->
    Key = "china",
    {ok,#'Etcd.DeleteRangeResponse'{header = #'Etcd.ResponseHeader'{}}} =
        eetcd_kv:delete_range(#'Etcd.DeleteRangeRequest'{key = Key}),
    Response = eetcd_kv:range(#'Etcd.RangeRequest'{key = Key}),
    {ok,#'Etcd.RangeResponse'{
        header = #'Etcd.ResponseHeader'{},
        kvs = [],more = false,count = 0}} = Response,
    {ok,#'Etcd.PutResponse'{
        header = #'Etcd.ResponseHeader'{},
        prev_kv = undefined}} =
        eetcd_kv:put(#'Etcd.PutRequest'{key = Key, value = "glorious"}),
    ok.
