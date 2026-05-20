%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_extensions).

%%% **DDD Context:** Object System Context

-moduledoc """
Extension method registry for Beamtalk primitives.

This module provides a Pharo-style extension registry that allows libraries
to add methods to existing classes (especially primitives like Integer, String).
Extensions use a global namespace with conflict tracking.

## Design Philosophy

Following Pharo's proven approach:
- Global namespace (no import declarations needed)
- Last-writer-wins conflict resolution
- Conflicts are logged and visible via tooling
- Provenance tracking (which library registered each method)

## ETS Table Structure

Table: `beamtalk_extensions`
- Type: set (unique keys)
- Key: `{Class, Selector}`
- Value: `{{Class, Selector}, Fun, Owner}`

Sources table: `beamtalk_extension_sources` (BT-2196)
- Type: set (unique keys)
- Key: `{Class, Selector}`
- Value: `{{Class, Selector}, Source}` where Source is the method body text
- Populated only by `register/5`. Enables source-text navigation queries
  (`SystemNavigation sendersOf:`, `referencesTo:`, `methodsMatching:`) to
  scan extension methods.

Conflicts table: `beamtalk_extension_conflicts`
- Type: bag (multiple values per key)
- Key: `{Class, Selector}`
- Value: `{{Class, Selector}, Owner, Timestamp}`

## Usage Examples

```erlang
%% Register an extension (actor classes — state-threading signature)
Fun = fun(Args, Self, State) -> {Result, NewState} end,
beamtalk_extensions:register('Counter', 'doubled', Fun, mylib).

%% Register an extension (value types — no state)
Fun = fun(Args, Self) -> Result end,
beamtalk_extensions:register('String', 'json', Fun, mylib).

%% Lookup an extension
case beamtalk_extensions:lookup('String', 'json') of
    {ok, Fun, Owner} -> Fun([], <<"hello">>);
    not_found -> error(does_not_understand)
end.

%% List all extensions on a class
beamtalk_extensions:list('String').
% => [{json, mylib}, {trim, stdlib}, ...]

%% Show conflicting registrations
beamtalk_extensions:conflicts().
% => [{'String', json, [mylib, otherlib]}]
```

See: docs/internal/design-self-as-object.md Section "Extension Registry Design"
""".
-export([
    init/0,
    register/4,
    register/5,
    lookup/2,
    list/1,
    extenders_of/1,
    extensions_by/1,
    listAllWithSource/0,
    getSource/2,
    conflicts/0,
    has/2
]).

-include_lib("kernel/include/logger.hrl").

-define(EXTENSIONS_TABLE, beamtalk_extensions).
-define(SOURCES_TABLE, beamtalk_extension_sources).
-define(CONFLICTS_TABLE, beamtalk_extension_conflicts).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Initialize the extension registry ETS tables.

Creates three tables:
- beamtalk_extensions: Current method registrations
- beamtalk_extension_sources: Method body source text (BT-2196) — populated
  only by `register/5` so source-text navigation queries can scan extensions.
- beamtalk_extension_conflicts: History of conflicting registrations

This should be called once during application startup.
""".
-spec init() -> ok.
init() ->
    %% Create main registry table
    case ets:info(?EXTENSIONS_TABLE) of
        undefined ->
            ets:new(?EXTENSIONS_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _ ->
            % Already exists
            ok
    end,

    %% Create sources table (BT-2196)
    case ets:info(?SOURCES_TABLE) of
        undefined ->
            ets:new(?SOURCES_TABLE, [set, public, named_table, {read_concurrency, true}]);
        _ ->
            % Already exists
            ok
    end,

    %% Create conflicts tracking table
    case ets:info(?CONFLICTS_TABLE) of
        undefined ->
            ets:new(?CONFLICTS_TABLE, [bag, public, named_table, {read_concurrency, true}]);
        _ ->
            % Already exists
            ok
    end,

    ok.

-doc """
Register an extension method on a class.

Tracks the owner (library/package name) for provenance.
If a method is already registered by a different owner, logs a warning
and overwrites (last-writer-wins policy).

Parameters:
- Class: The class atom (e.g., 'Integer', 'String')
- Selector: The method selector atom (e.g., 'json', 'trim')
- Fun: The method function (actor: fun(Args, Self, State) -> {Result, NewState};
        value type: fun(Args, Self) -> Result)
- Owner: The registering library/package atom (e.g., 'mylib', 'stdlib')

Examples:
```
JsonFun = fun([], Str) -> iolist_to_binary(json:encode(Str)) end,
register('String', 'json', JsonFun, mylib).
```
""".
-spec register(atom(), atom(), function(), atom()) -> ok.
register(Class, Selector, Fun, Owner) ->
    register(Class, Selector, Fun, Owner, undefined).

-doc """
Register an extension method on a class, with the method body source text.

Same as `register/4` but also stores `Source` (a binary holding the method's
body text) in the extension sources table. Powers source-text navigation
queries (`SystemNavigation sendersOf:`, `referencesTo:`, `methodsMatching:`)
that need to scan extension methods (BT-2196).

`Source` may be `undefined` to register without source (equivalent to
`register/4`); pass a binary to enable scanning.

Parameters:
- Class, Selector, Fun, Owner — as for `register/4`.
- Source — `binary()` method body, or `undefined` to skip source tracking.
""".
-spec register(atom(), atom(), function(), atom(), binary() | undefined) -> ok.
register(Class, Selector, Fun, Owner, Source) when
    is_atom(Class),
    is_atom(Selector),
    is_function(Fun),
    is_atom(Owner),
    (Source =:= undefined orelse is_binary(Source))
->
    Key = {Class, Selector},

    %% Check for existing registration
    case ets:lookup(?EXTENSIONS_TABLE, Key) of
        [] ->
            %% New registration
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            maybe_store_source(Key, Source),
            ok;
        [{Key, _OldFun, OldOwner}] when OldOwner =:= Owner ->
            %% Same owner updating - no conflict
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            maybe_store_source(Key, Source),
            ok;
        [{Key, _OldFun, OldOwner}] ->
            %% Conflict: different owner
            ?LOG_WARNING(
                "Extension conflict: '~p' on '~p' (from '~p') overwritten by '~p'",
                [Selector, Class, OldOwner, Owner],
                #{domain => [beamtalk, runtime]}
            ),

            %% Record conflict for tooling - record BOTH owners
            Timestamp = erlang:system_time(millisecond),
            ets:insert(?CONFLICTS_TABLE, {Key, OldOwner, Timestamp}),
            ets:insert(?CONFLICTS_TABLE, {Key, Owner, Timestamp + 1}),

            %% Overwrite (last-writer-wins)
            ets:insert(?EXTENSIONS_TABLE, {Key, Fun, Owner}),
            maybe_store_source(Key, Source),
            ok
    end.

-doc """
Lookup an extension method.

Returns the currently registered function and its owner, or not_found.

Examples:
```
case lookup('String', 'json') of
    {ok, Fun, mylib} -> Fun([], <<"hello">>);
    not_found -> error(does_not_understand)
end.
```
""".
-spec lookup(atom(), atom()) -> {ok, function(), atom()} | not_found.
lookup(Class, Selector) when is_atom(Class), is_atom(Selector) ->
    Key = {Class, Selector},
    case ets:lookup(?EXTENSIONS_TABLE, Key) of
        [{Key, Fun, Owner}] -> {ok, Fun, Owner};
        [] -> not_found
    end.

-doc """
List all extensions registered on a class.

Returns a list of {Selector, Owner} tuples for all methods
registered on the given class.

Examples:
```
list('String').
% => [{json, mylib}, {trim, stdlib}, {camelize, mylib}]
```
""".
-spec list(atom()) -> [{atom(), atom()}].
list(Class) when is_atom(Class) ->
    MatchPattern = {{Class, '$1'}, '_', '$2'},
    ets:select(?EXTENSIONS_TABLE, [{MatchPattern, [], [{{'$1', '$2'}}]}]).

-doc """
Return the unique set of owner classes that contribute any extension to the given target class.

Scans the ETS extension table (`O(table size)`) for all entries whose key
`{ClassName, _}` matches the given `TargetClass`, then collects the unique
`Owner` atoms. Returns `[]` when no class extends `TargetClass`.

This backs the `SystemNavigation extendersOf:` query: "which classes add
extension methods to class X?"

Examples:
```
extenders_of('Integer').
% => [mylib, stdlib]   -- all owners that extend Integer
extenders_of('NotExtended').
% => []
```
""".
-spec extenders_of(atom()) -> [atom()].
extenders_of(TargetClass) when is_atom(TargetClass) ->
    MatchPattern = {{TargetClass, '_'}, '_', '$1'},
    Owners = ets:select(?EXTENSIONS_TABLE, [{MatchPattern, [], ['$1']}]),
    lists:usort(Owners).

-doc """
Return all extensions contributed by the given owner class.

Scans the ETS extension table (`O(table size)`) for all entries whose
`Owner` field matches `OwnerClass`, then returns `{TargetClass, Selector}`
tuples for every matching entry. Returns `[]` when `OwnerClass` contributes
no extensions.

This backs the `SystemNavigation extensionsBy:` query: "which extension
methods does class Y contribute, and to which target classes?"

Examples:
```
extensions_by(mylib).
% => [{'Integer', double}, {'String', json}]
extensions_by(unknown_owner).
% => []
```
""".
-spec extensions_by(atom()) -> [{atom(), atom()}].
extensions_by(OwnerClass) when is_atom(OwnerClass) ->
    ets:foldl(
        fun
            ({{TargetClass, Selector}, _Fun, Owner}, Acc) when Owner =:= OwnerClass ->
                [{TargetClass, Selector} | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        ?EXTENSIONS_TABLE
    ).

-doc """
Show all methods that have been registered by multiple owners.

Returns a list of {Class, Selector, [Owner1, Owner2, ...]} tuples
for methods that have conflicts in the history.

Examples:
```
conflicts().
% => [{'String', json, [mylib, otherlib]}, {'Integer', parse, [lib1, lib2]}]
```
""".
-spec conflicts() -> [{atom(), atom(), [atom()]}].
conflicts() ->
    %% Get all conflict records
    AllConflicts = ets:tab2list(?CONFLICTS_TABLE),

    %% Group by {Class, Selector}
    Grouped = group_conflicts(AllConflicts),

    %% Convert to result format
    [{Class, Selector, Owners} || {{Class, Selector}, Owners} <- Grouped, length(Owners) > 1].

-doc """
Check if an extension exists for a class and selector.

Examples:
```
has('String', 'json').    % => true
has('String', 'unknown'). % => false
```
""".
-spec has(atom(), atom()) -> boolean().
has(Class, Selector) when is_atom(Class), is_atom(Selector) ->
    Key = {Class, Selector},
    ets:member(?EXTENSIONS_TABLE, Key).

-doc """
Return the registered source body for an extension, if `register/5`
was used to register it (BT-2196).

Returns `{ok, Source}` when a source was stored, or `not_found` when no
source is recorded (either the extension does not exist, or it was
registered via `register/4` without source).
""".
-spec getSource(atom(), atom()) -> {ok, binary()} | not_found.
getSource(Class, Selector) when is_atom(Class), is_atom(Selector) ->
    Key = {Class, Selector},
    try
        case ets:lookup(?SOURCES_TABLE, Key) of
            [{Key, Source}] -> {ok, Source};
            [] -> not_found
        end
    catch
        error:badarg ->
            %% Sources table not initialised yet (early bootstrap).
            %% Mirrors `listAllWithSource/0` — preserve the `not_found`
            %% contract rather than letting the missing-table error escape.
            not_found
    end.

-doc """
Return all extension entries that have an associated source body (BT-2196).

Powers source-text navigation queries on `SystemNavigation` that need to
scan extension methods. Extensions registered via `register/4` (without
source) are deliberately excluded — there is nothing to scan, and
returning them would force every caller to filter.

Each tuple is `{Class, Selector, Source}` where Source is the binary
method body text. Returns `[]` when no extensions have stored source.
""".
-spec listAllWithSource() -> [{atom(), atom(), binary()}].
listAllWithSource() ->
    try
        ets:foldl(
            fun({{Class, Selector}, Source}, Acc) ->
                [{Class, Selector, Source} | Acc]
            end,
            [],
            ?SOURCES_TABLE
        )
    catch
        error:badarg ->
            %% Sources table not initialised yet (early bootstrap).
            []
    end.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc "Group conflict records by class and selector.".
-spec group_conflicts(list()) -> [{term(), [atom()]}].
group_conflicts(Conflicts) ->
    Dict = lists:foldl(
        fun({{Class, Selector}, Owner, _Timestamp}, Acc) ->
            Key = {Class, Selector},
            maps:update_with(Key, fun(Owners) -> lists:usort([Owner | Owners]) end, [Owner], Acc)
        end,
        #{},
        Conflicts
    ),
    maps:to_list(Dict).

-doc """
Store source body in the sources table iff a source binary was supplied.

`undefined` means the caller used the legacy `register/4` path (or
explicitly passed `undefined` to `register/5`) and has no source body
to share. In that case we **delete** any previously stored source for
the same `{Class, Selector}` rather than no-op'ing: an extension that
was re-registered without source must not keep its old body around in
the sources table, or `listAllWithSource/0` would surface stale text
attributed to a now-sourceless registration.

Deletion is a silent no-op if no row exists (ETS semantics), so the
common "first registration via `register/4`" path stays cheap.
""".
-spec maybe_store_source({atom(), atom()}, binary() | undefined) -> ok.
maybe_store_source(Key, undefined) ->
    %% Drop any stale source body from a prior register/5 — keep the
    %% sources table in sync with the (now sourceless) dispatch entry.
    ets:delete(?SOURCES_TABLE, Key),
    ok;
maybe_store_source(Key, Source) when is_binary(Source) ->
    ets:insert(?SOURCES_TABLE, {Key, Source}),
    ok.
