-module(eetcd_data_coercion).

%% API
-export([to_list/1, to_binary/1]).

-spec to_list(Val :: integer() | list() | binary() | atom() | map()) -> list().
to_list(Val) when is_list(Val)    -> Val;
to_list(Val) when is_map(Val)     -> maps:to_list(Val);
to_list(Val) when is_atom(Val)    -> atom_to_list(Val);
to_list(Val) when is_binary(Val)  -> binary_to_list(Val);
to_list(Val) when is_integer(Val) -> integer_to_list(Val).

-spec to_binary(Val :: binary() | list() | atom() | integer()) -> binary().
to_binary(Val) when is_list(Val)    -> list_to_binary(Val);
to_binary(Val) when is_atom(Val)    -> atom_to_binary(Val, utf8);
to_binary(Val) when is_integer(Val) -> integer_to_binary(Val);
to_binary(Val)                      -> Val.
