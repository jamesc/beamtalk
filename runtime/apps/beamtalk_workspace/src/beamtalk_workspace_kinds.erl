%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_kinds).

%%% **DDD Context:** Workspace Context

-moduledoc """
The one ChangeLog entry-kind descriptor table (BT-4894).

Each `kind()` carries its flush tier (ADR 0113), its wire name (the string
persisted in the ChangeLog JSON and consumed by the Rust LSP), and whether it
is a rename (ADR 0114). `beamtalk_workspace_changelog` (decode, known-kind
list) and `beamtalk_workspace_flush` (`flushKinds:` classification, tiering,
rename partitioning) both read this table; adding a kind means adding one row.

`unknown` is the open-enum fallback for a value written by a newer build; it is
a member of the domain but is never persisted, never named by `flushKinds:`.
""".

-export([
    all/0,
    persisted/0,
    decode/1,
    wire_name/1,
    tier/1,
    rename/1,
    is_rename/1,
    filter_symbol/1
]).

-export_type([kind/0, tier/0, descriptor/0]).

-type kind() ::
    instance
    | class
    | 'new-class'
    | 'class-def'
    | 'remove-method'
    | 'remove-class'
    | 'rename-class'
    | 'rename-method'
    | unknown.
-type tier() :: tier1 | tier2.
-type descriptor() :: #{
    kind := kind(),
    wire := binary(),
    tier := tier(),
    rename := class | method | none
}.

-spec table() -> [descriptor()].
table() ->
    [
        d(instance, <<"instance">>, tier1, none),
        d(class, <<"class">>, tier1, none),
        d('new-class', <<"new-class">>, tier1, none),
        d('class-def', <<"class-def">>, tier1, none),
        d('remove-method', <<"remove-method">>, tier1, none),
        d('remove-class', <<"remove-class">>, tier2, none),
        d('rename-class', <<"rename-class">>, tier2, class),
        d('rename-method', <<"rename-method">>, tier2, method),
        d(unknown, <<"unknown">>, tier1, none)
    ].

-spec d(kind(), binary(), tier(), class | method | none) -> descriptor().
d(Kind, Wire, Tier, Rename) ->
    #{kind => Kind, wire => Wire, tier => Tier, rename => Rename}.

-doc "Every kind `kind()` admits, including `unknown`, in declaration order.".
-spec all() -> [kind()].
all() ->
    [maps:get(kind, D) || D <- table()].

-doc "Every kind that is persisted and nameable in `flushKinds:` (all but `unknown`).".
-spec persisted() -> [kind()].
persisted() ->
    [K || K <- all(), K =/= unknown].

-doc "Decode a persisted wire name; `error` for anything else (including `<<\"unknown\">>`).".
-spec decode(binary()) -> {ok, kind()} | error.
decode(Wire) ->
    case [K || #{kind := K, wire := W} <- table(), W =:= Wire, K =/= unknown] of
        [Kind] -> {ok, Kind};
        [] -> error
    end.

-spec wire_name(kind()) -> binary().
wire_name(Kind) ->
    maps:get(wire, descriptor(Kind)).

-doc "Flush tier. A kind outside the table (defensive) is `tier1`, like `unknown`.".
-spec tier(term()) -> tier().
tier(Kind) ->
    case lists:search(fun(#{kind := K}) -> K =:= Kind end, table()) of
        {value, #{tier := T}} -> T;
        false -> tier1
    end.

-spec rename(term()) -> class | method | none.
rename(Kind) ->
    case lists:search(fun(#{kind := K}) -> K =:= Kind end, table()) of
        {value, #{rename := R}} -> R;
        false -> none
    end.

-spec is_rename(term()) -> boolean().
is_rename(Kind) ->
    rename(Kind) =/= none.

-doc "How a persisted kind is written as a Beamtalk symbol literal: `#instance`, `#'new-class'`.".
-spec filter_symbol(kind()) -> binary().
filter_symbol(Kind) ->
    Wire = wire_name(Kind),
    case binary:match(Wire, <<"-">>) of
        nomatch -> <<"#", Wire/binary>>;
        _ -> <<"#'", Wire/binary, "'">>
    end.

-spec descriptor(kind()) -> descriptor().
descriptor(Kind) ->
    {value, D} = lists:search(fun(#{kind := K}) -> K =:= Kind end, table()),
    D.
