%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper operations for Set (Erlang ordsets in tagged maps).
%%
%% BT-73: Complex Set operations that cannot be inlined as direct BIF
%% calls in generated Core Erlang. Called from compiled stdlib module
%% beamtalk_set.
%%
%% Sets are represented as tagged maps:
%%   #{'$beamtalk_class' => 'Set', elements => OrdsetData}
%%
%% where OrdsetData is a sorted list maintained by the ordsets module.
%%
%% **DDD Context:** Runtime Context — Domain Service
-module(beamtalk_set_ops).

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

-include("beamtalk.hrl").

%%% ============================================================================
%%% Set Representation
%%% ============================================================================

%% @doc Create a new empty Set.
-spec new() -> map().
new() ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:new()}.

%% @doc Create a Set from a list of elements.
-spec from_list(list()) -> map().
from_list(List) when is_list(List) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:from_list(List)};
from_list(_NonList) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, 'fromList:'),
    error(beamtalk_error:with_hint(Error1, <<"Expected a List argument">>)).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

%% @doc Return the number of elements in the Set.
-spec size(map()) -> non_neg_integer().
size(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    length(Elements).

%% @doc Return true if the Set has no elements.
-spec is_empty(map()) -> boolean().
is_empty(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    Elements =:= [].

%% @doc Return true if the Set contains the given element.
-spec includes(map(), term()) -> boolean().
includes(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    ordsets:is_element(Element, Elements).

%%% ============================================================================
%%% Modification (returns new Set)
%%% ============================================================================

%% @doc Add an element to the Set, returning a new Set.
-spec add(map(), term()) -> map().
add(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:add_element(Element, Elements)}.

%% @doc Remove an element from the Set, returning a new Set.
-spec remove(map(), term()) -> map().
remove(#{'$beamtalk_class' := 'Set', elements := Elements}, Element) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:del_element(Element, Elements)}.

%%% ============================================================================
%%% Set Operations (returns new Set)
%%% ============================================================================

%% @doc Return the union of two Sets.
-spec union(map(), map()) -> map().
union(#{'$beamtalk_class' := 'Set', elements := E1},
      #{'$beamtalk_class' := 'Set', elements := E2}) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:union(E1, E2)};
union(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('union:');
union(_, _) ->
    set_type_error('union:').

%% @doc Return the intersection of two Sets.
-spec intersection(map(), map()) -> map().
intersection(#{'$beamtalk_class' := 'Set', elements := E1},
             #{'$beamtalk_class' := 'Set', elements := E2}) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:intersection(E1, E2)};
intersection(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('intersection:');
intersection(_, _) ->
    set_type_error('intersection:').

%% @doc Return the difference of two Sets (elements in self but not other).
-spec difference(map(), map()) -> map().
difference(#{'$beamtalk_class' := 'Set', elements := E1},
           #{'$beamtalk_class' := 'Set', elements := E2}) ->
    #{'$beamtalk_class' => 'Set', elements => ordsets:subtract(E1, E2)};
difference(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('difference:');
difference(_, _) ->
    set_type_error('difference:').

%%% ============================================================================
%%% Predicates
%%% ============================================================================

%% @doc Return true if self is a subset of other.
-spec is_subset_of(map(), map()) -> boolean().
is_subset_of(#{'$beamtalk_class' := 'Set', elements := E1},
             #{'$beamtalk_class' := 'Set', elements := E2}) ->
    ordsets:is_subset(E1, E2);
is_subset_of(#{'$beamtalk_class' := 'Set'}, _Other) ->
    set_type_error('isSubsetOf:');
is_subset_of(_, _) ->
    set_type_error('isSubsetOf:').

%%% ============================================================================
%%% Conversion
%%% ============================================================================

%% @doc Return the elements as a plain list (ordsets are already lists).
-spec as_list(map()) -> list().
as_list(#{'$beamtalk_class' := 'Set', elements := Elements}) ->
    Elements.

%%% ============================================================================
%%% Iteration
%%% ============================================================================

%% @doc Apply a block to each element of the Set.
-spec do(map(), fun((term()) -> term())) -> nil.
do(#{'$beamtalk_class' := 'Set', elements := Elements}, Block) when is_function(Block, 1) ->
    lists:foreach(Block, Elements),
    nil;
do(#{'$beamtalk_class' := 'Set'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, 'do:'),
    error(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private Raise a type_error for binary set operations receiving a non-Set argument.
-spec set_type_error(atom()) -> no_return().
set_type_error(Selector) ->
    Error0 = beamtalk_error:new(type_error, 'Set'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    error(beamtalk_error:with_hint(Error1, <<"Argument must be a Set">>)).
