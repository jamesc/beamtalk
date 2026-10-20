%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection_selectors).

%%% **DDD Context:** Object System Context

-moduledoc """
The messages a class can override that the stdlib's own collection methods run
through (ADR 0133, BT-3916, BT-3951).

The compiler's `state_threading_selectors` module holds the lists. This module
is their runtime copy, read when the receiver guard of an inlined list-op fold
looks for a runtime extension method (which the compile-time
`overrides_collection_selector` and `overrides_species_construction` flags
cannot see). The copy is enforced, not trusted: the Rust test
`runtime_overridable_selector_list_matches_the_compiler_list` fails when a list
between its markers differs from the compiler's.
""".

-export([
    overridable/0,
    species_dependent/0,
    species_construction/0,
    species_construction_class_side/0,
    extended_on/1,
    species_construction_extended_on/1
]).

-doc "The overridable collection selectors, in the compiler's order.".
-spec overridable() -> [atom()].
overridable() ->
    %% BEGIN SELECTORS
    [
        'collect:',
        'select:',
        'reject:',
        'detect:',
        'detect:ifNone:',
        'inject:into:',
        'count:',
        'anySatisfy:',
        'allSatisfy:',
        'flatMap:',
        'takeWhile:',
        'dropWhile:',
        'groupBy:',
        'partition:',
        'sort:',
        'eachWithIndex:',
        'do:separatedBy:',
        'doWithKey:',
        'keysAndValuesDo:'
    ].
%% END SELECTORS

-doc """
The selectors whose `Collection` provision ends in `self species withAll:`.
""".
-spec species_dependent() -> [atom()].
species_dependent() ->
    %% BEGIN SPECIES DEPENDENT
    [
        'collect:',
        'select:',
        'reject:'
    ].
%% END SPECIES DEPENDENT

-doc "The instance-side messages that change what `species_dependent/0` answer.".
-spec species_construction() -> [atom()].
species_construction() ->
    %% BEGIN SPECIES INSTANCE
    [
        'species'
    ].
%% END SPECIES INSTANCE

-doc "The class-side messages that change what `species_dependent/0` answer.".
-spec species_construction_class_side() -> [atom()].
species_construction_class_side() ->
    %% BEGIN SPECIES CLASS
    [
        'withAll:'
    ].
%% END SPECIES CLASS

-doc """
Whether a runtime extension method registered on `Class` has one of the
overridable selectors.

One O(1) `beamtalk_extensions:lookup/2` per selector rather than a table scan
per class. `beamtalk_class_shadow_flags` is not usable here: it records
extensions on class-object tags only, never the instance-side ones.
""".
-spec extended_on(atom()) -> boolean().
extended_on(Class) ->
    guarded(fun() -> has_any(Class, overridable()) end).

-doc """
Whether a runtime extension on `Class` is a species-construction message:
instance-side (keyed under the class name) or class-side (keyed under the
class-object tag).
""".
-spec species_construction_extended_on(atom()) -> boolean().
species_construction_extended_on(Class) ->
    guarded(fun() ->
        has_any(Class, species_construction()) orelse
            has_any(
                beamtalk_class_registry:class_object_tag(Class),
                species_construction_class_side()
            )
    end).

%% Registry table not initialised yet (early bootstrap): nothing is extended.
-spec guarded(fun(() -> boolean())) -> boolean().
guarded(Check) ->
    try
        Check()
    catch
        error:badarg -> false
    end.

-spec has_any(atom(), [atom()]) -> boolean().
has_any(Key, Selectors) ->
    lists:any(
        fun(Selector) -> beamtalk_extensions:lookup(Key, Selector) =/= not_found end, Selectors
    ).
