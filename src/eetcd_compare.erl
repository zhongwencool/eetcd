-module(eetcd_compare).
%% API
-compile(export_all).

-export([]).

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
to_result("!=") -> 'NOT_EQUAL';
to_result(">") -> 'GREATER';
to_result("<") -> 'LESS'.
