%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection_selectors).

%%% **DDD Context:** Object System Context

-moduledoc """
The messages a class can override that the stdlib's own collection methods run
through (ADR 0133, BT-3916, BT-3951).

The compiler's `state_threading_selectors::overridable_collection_selectors`
is the one list. This module is its runtime copy, read when the receiver guard
of an inlined list-op fold looks for a runtime extension method (which the
compile-time `overrides_collection_selector` flag cannot see). The copy is
enforced, not trusted: the Rust test
`runtime_overridable_selector_list_matches_the_compiler_list` fails when the
list between the markers differs from the compiler's.
""".

-export([overridable/0, extended_on/1]).

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
        'keysAndValuesDo:',
        'species'
    ].
%% END SELECTORS

-doc """
Whether a runtime extension method registered on `Class` has one of the
overridable selectors.
""".
-spec extended_on(atom()) -> boolean().
extended_on(Class) ->
    Overridable = overridable(),
    try beamtalk_extensions:list(Class) of
        Extensions ->
            lists:any(
                fun({Selector, _Owner}) -> lists:member(Selector, Overridable) end,
                Extensions
            )
    catch
        %% Registry table not initialised yet (early bootstrap): nothing is extended.
        error:badarg -> false
    end.
