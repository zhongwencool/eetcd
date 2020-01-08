-module(eetcd_op).

-compile(export_all).
%% API
-export([]).

put(PutRequest) ->
    {request_put, PutRequest}.

range(RangeRequest) ->
    {request_range, RangeRequest}.

delete_range(DeleteRangeRequest) ->
    {request_delete_range, DeleteRangeRequest}.

txn(TxnRequest) ->
    {request_txn, TxnRequest}.

