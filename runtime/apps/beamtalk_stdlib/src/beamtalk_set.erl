%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_set).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper operations for Set (a term-order-sorted list in a tagged map).

BT-73: Complex Set operations that cannot be inlined as direct BIF
calls in generated Core Erlang. Called from compiled stdlib module
bt@stdlib@set.

Sets are represented as tagged maps:
  #{'$beamtalk_class' => 'Set', elements => SortedUniqueList}

## Element identity is `=:=` (BT-2997)

This used `ordsets`, which decides membership by Erlang term *order* — `==`
semantics. That made `Set` disagree with everything else in the language:
`Set new add: 1; add: 1.0` held one element, while a `Dictionary` keyed on `1`
and `1.0` (Erlang maps, hence `=:=`) held two, and `#(1) includes: 1.0`
answered false. ADR 0002 and BT-1562 make `=:=` the language's element
identity, so `Set` was the outlier.

The representation is unchanged — a term-order-sorted list, which
`beamtalk_inspector`, `beamtalk_primitive` and `beamtalk_stream` all read
directly, and which keeps `asList` sorted. Only identity moved to `=:=`, via
the `beamtalk_list:unique/1` / `strict_member_sorted/2` helpers. `==` and `=:=`
disagree only for numbers, so mutually-`==` elements form a short contiguous
run that those helpers scan strictly.
""".

-export([
    new/0,
    from_list/1,
    size/1,
    is_empty/1,
    includes/2,
    add/2,
    remove/2,
    union/2,
    intersection/2,
    difference/2,
    is_subset_of/2,
    as_list/1,
    do/2
]).

%%% ============================================================================
%%% Set Representation
%%% ============================================================================

-doc "Create a new empty Set.".
-spec new() -> map().
new() ->
    #{'$beamtalk_class' => 'Set', elements => []}.

-doc "Create a Set from a list of elements.".
-spec from_list(list()) -> map().
from_list(List) when is_list(List) ->
    #{'$beamtalk_class' => 'Set', elements => beamtalk_list:unique(List)};
from_list(_NonList) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, 'fromList:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Expected a List argument">>)).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

-doc "Return the number of elements in the Set.".
-spec size(map()) -> non_neg_integer().
size(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    length(Elements).

-doc "Return true if the Set has no elements.".
-spec is_empty(map()) -> boolean().
is_empty(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    Elements =:= [].

-doc "Return true if the Set contains the given element.".
-spec includes(map(), term()) -> boolean().
includes(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    beamtalk_list:strict_member_sorted(Element, Elements).

%%% ============================================================================
%%% Modification (returns new Set)
%%% ============================================================================

-doc "Add an element to the Set, returning a new Set.".
-spec add(map(), term()) -> map().
add(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    #{'$beamtalk_class' => 'Set', elements => strict_add(Element, Elements)}.

-doc "Remove an element from the Set, returning a new Set.".
-spec remove(map(), term()) -> map().
remove(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    #{'$beamtalk_class' => 'Set', elements => strict_del(Element, Elements)}.

%%% ============================================================================
%%% Set Operations (returns new Set)
%%% ============================================================================

-doc "Return the union of two Sets.".
-spec union(map(), map()) -> map().
union(
    #{'$beamtalk_class' := 'Set', elements := E1},
    #{'$beamtalk_class' := 'Set', elements := E2}
) ->
    #{'$beamtalk_class' => 'Set', elements => beamtalk_list:unique(E1 ++ E2)};
union(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('union:');
union(_, _) ->
    set_type_error('union:').

-doc "Return the intersection of two Sets.".
-spec intersection(map(), map()) -> map().
intersection(
    #{'$beamtalk_class' := 'Set', elements := E1},
    #{'$beamtalk_class' := 'Set', elements := E2}
) ->
    #{'$beamtalk_class' => 'Set', elements => strict_intersection(E1, E2)};
intersection(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('intersection:');
intersection(_, _) ->
    set_type_error('intersection:').

-doc "Return the difference of two Sets (elements in self but not other).".
-spec difference(map(), map()) -> map().
difference(
    #{'$beamtalk_class' := 'Set', elements := E1},
    #{'$beamtalk_class' := 'Set', elements := E2}
) ->
    #{'$beamtalk_class' => 'Set', elements => strict_difference(E1, E2)};
difference(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('difference:');
difference(_, _) ->
    set_type_error('difference:').

%%% ============================================================================
%%% Predicates
%%% ============================================================================

-doc "Return true if self is a subset of other.".
-spec is_subset_of(map(), map()) -> boolean().
is_subset_of(
    #{'$beamtalk_class' := 'Set', elements := E1},
    #{'$beamtalk_class' := 'Set', elements := E2}
) ->
    strict_is_subset(E1, E2);
is_subset_of(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('isSubsetOf:');
is_subset_of(_, _) ->
    set_type_error('isSubsetOf:').

%%% ============================================================================
%%% Conversion
%%% ============================================================================

-doc "Return the elements as a plain list (already a sorted list).".
-spec as_list(map()) -> list().
as_list(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    Elements.

%%% ============================================================================
%%% Iteration
%%% ============================================================================

-doc "Apply a block to each element of the Set.".
-spec do(map(), fun((term()) -> term())) -> 'nil'.
do(#{'$beamtalk_class' := 'Set', elements := Elements}, Block) when is_function(Block, 1) ->
    lists:foreach(Block, Elements),
    nil;
do(#{'$beamtalk_class' := 'Set'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, 'do:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% Both operands are term-order-sorted and `=:=`-unique, so these walk the two
%% lists in step — linear, as `ordsets:intersection/2` and `ordsets:subtract/2`
%% were. A `[X || X <- E1, strict_member_sorted(X, E2)]` comprehension would be
%% O(|E1|*|E2|) in the worst case.
%%
%% `==`-equal-but-not-`=:=` terms (an integer and a float of equal value) sit in
%% the same order position, so neither list can be advanced on an order tie
%% alone — those cases fall back to a bounded `strict_member_sorted/2` probe of
%% the run rather than risk skipping a strict match.

-spec strict_intersection(list(), list()) -> list().
strict_intersection([], _E2) ->
    [];
strict_intersection(_E1, []) ->
    [];
strict_intersection([H1 | T1] = E1, [H2 | T2] = E2) ->
    if
        H1 =:= H2 -> [H1 | strict_intersection(T1, T2)];
        H1 == H2 -> tie_intersection(E1, E2);
        H1 < H2 -> strict_intersection(T1, E2);
        true -> strict_intersection(E1, T2)
    end.

%% Order tie without strict equality: probe H1 against E2's tied run, then
%% advance E1 only. Runs are bounded (an integer and a float), so this stays
%% linear overall.
-spec tie_intersection(list(), list()) -> list().
tie_intersection([H1 | T1], E2) ->
    case beamtalk_list:strict_member_sorted(H1, E2) of
        true -> [H1 | strict_intersection(T1, E2)];
        false -> strict_intersection(T1, E2)
    end.

-spec strict_difference(list(), list()) -> list().
strict_difference([], _E2) ->
    [];
strict_difference(E1, []) ->
    E1;
strict_difference([H1 | T1] = E1, [H2 | T2] = E2) ->
    if
        H1 =:= H2 -> strict_difference(T1, T2);
        H1 == H2 -> tie_difference(E1, E2);
        H1 < H2 -> [H1 | strict_difference(T1, E2)];
        true -> strict_difference(E1, T2)
    end.

-spec tie_difference(list(), list()) -> list().
tie_difference([H1 | T1], E2) ->
    case beamtalk_list:strict_member_sorted(H1, E2) of
        true -> strict_difference(T1, E2);
        false -> [H1 | strict_difference(T1, E2)]
    end.

-spec strict_is_subset(list(), list()) -> boolean().
strict_is_subset([], _E2) ->
    true;
strict_is_subset(_E1, []) ->
    false;
strict_is_subset([H1 | T1] = E1, [H2 | T2] = E2) ->
    if
        H1 =:= H2 -> strict_is_subset(T1, T2);
        H1 == H2 -> beamtalk_list:strict_member_sorted(H1, E2) andalso strict_is_subset(T1, E2);
        H1 < H2 -> false;
        true -> strict_is_subset(E1, T2)
    end.

-doc """
Insert `Elem` into a term-order-sorted list, keeping it sorted and `=:=`-unique.

An element that merely compares `==` to one already present (`1` vs `1.0`) is a
distinct element and is inserted after it, keeping the run contiguous.
""".
-spec strict_add(term(), list()) -> list().
strict_add(Elem, Sorted) ->
    case beamtalk_list:strict_member_sorted(Elem, Sorted) of
        true -> Sorted;
        false -> strict_insert(Elem, Sorted)
    end.

-spec strict_insert(term(), list()) -> list().
strict_insert(Elem, []) ->
    [Elem];
strict_insert(Elem, [H | T]) when Elem < H ->
    [Elem, H | T];
strict_insert(Elem, [H | T]) ->
    [H | strict_insert(Elem, T)].

-doc """
Remove the `=:=` match for `Elem` from a term-order-sorted list.

Elements that only compare `==` are left in place.
""".
-spec strict_del(term(), list()) -> list().
strict_del(_Elem, []) ->
    [];
strict_del(Elem, [H | T]) when H =:= Elem ->
    T;
strict_del(Elem, [H | T]) when H == Elem; H < Elem ->
    [H | strict_del(Elem, T)];
strict_del(_Elem, PastIt) ->
    %% Sorted ascending, so Elem cannot appear further along.
    PastIt.

-doc "Raise a type_error for binary set operations receiving a non-Set argument.".
-spec set_type_error(atom()) -> no_return().
set_type_error(Selector) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Argument must be a Set">>)).
