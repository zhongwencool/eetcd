-module(eetcd_compare).

-export([new/1, with_range_end/2]).
-export([value/3, version/3,
    mod_revision/3, create_revision/3,
    lease/3]).

new(Key) -> #{key => Key}.

%% WithRange sets the comparison to scan the range [key, end).
with_range_end(Cmp, End) -> Cmp#{range_end => End}.

value(Cmp, Result, Value) ->
    maps:merge(Cmp,
        #{
            result => to_result(Result),
            target => 'VALUE',
            target_union => {value, Value}
        }).

version(Cmp, Result, Version) ->
    maps:merge(Cmp,
        #{
            result => to_result(Result),
            target => 'VERSION',
            target_union => {version, Version}
        }).

mod_revision(Cmp, Result, Version) ->
    maps:merge(Cmp,
        #{
            result => to_result(Result),
            target => 'MOD',
            target_union => {mod_revision, Version}
        }).

create_revision(Cmp, Result, Version) ->
    maps:merge(Cmp,
        #{
            result => to_result(Result),
            target => 'CREATE',
            target_union => {create_revision, Version}
        }).

lease(Cmp, Result, Version) ->
    maps:merge(Cmp,
        #{
            result => to_result(Result),
            target => 'LEASE',
            target_union => {lease, Version}
        }).


to_result("=") -> 'EQUAL';
to_result("==") -> 'EQUAL';
to_result("!=") -> 'NOT_EQUAL';
to_result("=/=") -> 'NOT_EQUAL';
to_result(">") -> 'GREATER';
to_result("<") -> 'LESS'.

