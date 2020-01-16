-ifndef(etcd_hrl).
-define(etcd_hrl, true).

-define(HEADERS, [{<<"grpc-encoding">>, <<"identity">>}, {<<"content-type">>, <<"application/grpc+proto">>}]).
-define(ETCD_HTTP2_CLIENT, etcd_http2_client).
-define(GRPC_ERROR(Status, Message), {'grpc_error', Status, Message}).

-export_type([key/0, value/0, context/0, name/0]).
-type key() :: iodata().
-type value() :: iodata().
-type context() :: map().
-type name() :: atom() | reference().

%% Grpc status code
-define(GRPC_STATUS_OK, 0).
-define(GRPC_STATUS_CANCELLED, 1).
-define(GRPC_STATUS_UNKNOWN, 2).
-define(GRPC_STATUS_INVALID_ARGUMENT, 3).
-define(GRPC_STATUS_DEADLINE_EXCEEDED, 4).
-define(GRPC_STATUS_NOT_FOUND, 5).
-define(GRPC_STATUS_ALREADY_EXISTS, 6).
-define(GRPC_STATUS_PERMISSION_DENIED, 7).
-define(GRPC_STATUS_RESOURCE_EXHAUSTED, 8).
-define(GRPC_STATUS_FAILED_PRECONDITION, 9).
-define(GRPC_STATUS_ABORTED, 10).
-define(GRPC_STATUS_OUT_OF_RANGE, 11).
-define(GRPC_STATUS_UNIMPLEMENTED, 12).
-define(GRPC_STATUS_INTERNAL, 13).
-define(GRPC_STATUS_UNAVAILABLE, 14).
-define(GRPC_STATUS_DATA_LOSS, 15).
-define(GRPC_STATUS_UNAUTHENTICATED, 16).

%% TODO
-define(ALL, "\0").

-define(ETCD_CONNS, eetcd_conns).
-record(eetcd_conn, {name, gun, http_header = [], conn}).
-endif.
