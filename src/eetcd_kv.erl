-module(eetcd_kv).
-include("eetcd.hrl").

-export([txn/4]).
-export([
    from_key/1,
    with_first_create/1,
    with_last_create/1,
    with_first_rev/1,
    with_last_rev/1,
    with_first_key/1,
    with_last_key/1,
    with_sort/3,
    with_top/3
]).

-export_type([
    range_request/0,
    sort_target/0,
    sort_order/0
]).

-type range_request() :: router_pb:'Etcd.RangeRequest'().
-type sort_target() :: 'KEY' | 'VERSION' | 'VALUE' | 'CREATE' | 'MOD'.
-type sort_order() :: 'NONE' | 'ASCEND' | 'DESCEND'.

-spec from_key(Key :: iodata()) -> map().
from_key(Key) ->
    #{key => Key, range_end => "\x00"}.

%% @doc Specifies the ordering in `get' request. It requires
%% `with_range' and/or `with_prefix' to be specified too.
%% `target' specifies the target to sort by: 'KEY', 'VERSION', 'VALUE', 'CREATE', 'MOD'.
%% `order' can be either 'NONE', 'ASCEND', 'DESCEND'.
-spec with_sort(range_request(), sort_target(), sort_order()) -> range_request().
with_sort(Request, SortTarget, SortOrder) ->
    Targets = router_pb:find_enum_def('Etcd.RangeRequest.SortTarget'),
    Orders = router_pb:find_enum_def('Etcd.RangeRequest.SortOrder'),
    (not lists:keymember(SortTarget, 1, Targets)) andalso throw({sort_target, SortTarget}),
    (not lists:keymember(SortOrder, 1, Orders)) andalso throw({sort_order, SortOrder}),
    Request#{
        sort_target => SortTarget,
        sort_order => SortOrder
    }.

%% @doc Get the key with the oldest creation revision in the request range.
-spec with_first_create(range_request()) -> range_request().
with_first_create(Context) ->
    with_top(Context, 'CREATE', 'ASCEND').

%% @doc Get the lexically last key in the request range.
-spec with_last_create(range_request()) -> range_request().
with_last_create(Context) ->
    with_top(Context, 'CREATE', 'DESCEND').

%% @doc Get the key with the oldest modification revision in the request range.
-spec with_first_rev(range_request()) -> range_request().
with_first_rev(Context) ->
    with_top(Context, 'MOD', 'ASCEND').

%% @doc Get the key with the latest modification revision in the request range.
-spec with_last_rev(range_request()) -> range_request().
with_last_rev(Context) ->
    with_top(Context, 'MOD', 'DESCEND').

%% @doc Get the lexically first key in the request range.
-spec with_first_key(range_request()) -> range_request().
with_first_key(Context) ->
    with_top(Context, 'KEY', 'ASCEND').

%% @doc Get the lexically last key in the request range.
-spec with_last_key(range_request()) -> range_request().
with_last_key(RangeRequest) ->
    with_top(RangeRequest, 'KEY', 'DESCEND').

%% @doc Get the first key over the get's prefix given a sort order
-spec with_top(range_request(), sort_target(), sort_order()) -> range_request().
with_top(#{key := Key} = RangeRequest0, SortTarget, SortOrder) ->
    RangeEnd = eetcd:get_prefix_range_end(Key),
    Request = with_sort(RangeRequest0, SortTarget, SortOrder),
    Request#{range_end => RangeEnd, limit => 1}.

%%% @doc Txn creates a transaction.
%% <dd>If takes a list of comparison. If all comparisons passed in succeed,</dd>
%% <dd>the operations passed into Then() will be executed.</dd>
%% <dd>Or the operations passed into Else() will be executed.</dd>
%% <dd>Then takes a list of operations. The Ops list will be executed, if the comparisons passed in If() succeed.</dd>
%% <dd> Else takes a list of operations. The Ops list will be executed, if the comparisons passed in If() fail.</dd>
%%% <dl>
%%% <dd>
%%% ```
%%% Cmp = eetcd_compare:new(Key),
%%% If = eetcd_compare:value(Cmp, ">", Value),
%%% Then = eetcd_op:put(eetcd_kv:with_value(eetcd_kv:with_key(eetcd_kv:new(), Key), NewValue)),
%%% Else = eetcd_op:delete(eetcd_kv:with_key(eetcd_kv:new(), Key)),
%%% eetcd_kv:txn(EtcdConnName, If, Then, Else).
%%% '''
%%% </dd> </dl>
%%% {@link eetcd_compare:new/1} {@link eetcd_compare:with_range/2}
%%% {@link eetcd_compare:value/3} {@link eetcd_compare:version/3}
%%% {@link eetcd_compare:mod_revision/3} {@link eetcd_compare:create_revision/3}
%%% {@link eetcd_compare:lease/3}
%%% {@link eetcd_op:put/1} {@link eetcd_op:get/1}
%%% {@link eetcd_op:delete/1} {@link eetcd_op:txn/1}
%%% @end
-spec txn(etcd_name(), [router_pb:'Etcd.Compare'()], [router_pb:'Etcd.RequestOp'()], [router_pb:'Etcd.RequestOp'()]) ->
    {ok, router_pb:'Etcd.TxnResponse'()}|{error, eetcd_error()}.
txn(EtcdName, If, Then, Else) ->
    Compare = case is_list(If) of true -> If; false -> [If] end,
    Success = case is_list(Then) of true -> Then; false -> [Then] end,
    Failure = case is_list(Else) of true -> Else; false -> [Else] end,
    Txn = #{compare => Compare, success => Success, failure => Failure},
    eetcd_kv_gen:txn(EtcdName, Txn).
