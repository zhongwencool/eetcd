-module(eetcd_op).
-export([put/1, get/1, delete/1, txn/1]).

put(PutRequest) ->
    #{request => {request_put, PutRequest}}.

get(RangeRequest) ->
    #{request => {request_range, RangeRequest}}.

delete(DeleteRangeRequest) ->
    #{request => {request_delete_range, DeleteRangeRequest}}.

txn(TxnRequest) ->
    #{request => {request_txn, TxnRequest}}.
