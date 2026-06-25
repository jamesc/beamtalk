%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_array).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper operations for Array (canonical map-backed representation).

BT-822 introduced Array backed by Erlang's `array` module for O(log n) random
access. ADR 0090 (BT-2680) replaced that backing store with a canonical
index→value map after the `array` module's copy-on-write cache slot was found to
break `=:=`/`phash2` consistency (BT-2362): `array:set/3` leaves a stale cache
node, so an array updated via `at:put:` did not compare equal to a literal with
the same elements, and the interim O(n) re-canonicalisation made repeated
`at:put:` O(n²).

Representation:
  #{'$beamtalk_class' => 'Array', 'data' => #{0 => V0, 1 => V1, ..., N-1 => Vn-1}}

The `'data'` payload is a map whose keys are the contiguous 0-based indices
`0..N-1`. This is **canonical by construction**: two Erlang maps are `=:=` iff
they hold the same associations, independent of insertion order or internal HAMT
layout, and `erlang:phash2` hashes them by content. So any array holding a given
element sequence is `=:=` and `phash2`-equal to any other array holding the same
sequence, regardless of edit history — which makes Array correct as a
Dictionary/Set key for free and keeps ADR 0002's strict, inlined `=:=` intact.
`at:put:` is O(log n) (a single key replacement) with no cache slot to leak.

Arrays are immutable — operations returning modified arrays return new tagged
maps.
""".

-export([
    from_list/1,
    size/1,
    is_empty/1,
    do/2,
    at/2,
    at_put/3,
    collect/2,
    select/2,
    inject_into/3,
    includes/2,
    print_string/1,
    slice_from/2
]).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Wrap a canonical index→value data map in the tagged Array map.
-spec new(map()) -> map().
new(Data) when is_map(Data) ->
    #{'$beamtalk_class' => 'Array', 'data' => Data}.

%% Build the canonical index→value map from an ordered list of elements.
-spec data_from_list(list()) -> map().
data_from_list(List) ->
    {_, Data} = lists:foldl(
        fun(Elem, {Index, Acc}) -> {Index + 1, Acc#{Index => Elem}} end,
        {0, #{}},
        List
    ),
    Data.

%% Materialise the elements of a data map as an ordered list (index 0..N-1).
-spec data_to_list(map()) -> list().
data_to_list(Data) ->
    [maps:get(Index, Data) || Index <- lists:seq(0, maps:size(Data) - 1)].

%%% ============================================================================
%%% Construction
%%% ============================================================================

-doc "Create an Array from a list of elements.".
-spec from_list(list()) -> map().
from_list(List) when is_list(List) ->
    new(data_from_list(List));
from_list(_NonList) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'withAll:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Expected a List argument">>)).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

-doc "Return the number of elements in the Array.".
-spec size(map()) -> non_neg_integer().
size(#{'$beamtalk_class' := 'Array', 'data' := Data}) ->
    maps:size(Data).

-doc "Return true if the Array has no elements.".
-spec is_empty(map()) -> boolean().
is_empty(#{'$beamtalk_class' := 'Array', 'data' := Data}) ->
    maps:size(Data) =:= 0.

-doc """
Return the element at the given 1-based index.

Raises index_out_of_bounds if the index is out of range.
""".
-spec at(map(), integer()) -> term().
at(#{'$beamtalk_class' := 'Array', 'data' := Data}, Index) when is_integer(Index), Index >= 1 ->
    N = maps:size(Data),
    if
        Index > N ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Index is beyond array size">>),
            beamtalk_error:raise(Error2);
        true ->
            maps:get(Index - 1, Data)
    end;
at(#{'$beamtalk_class' := 'Array'}, Index) when is_integer(Index) ->
    Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be >= 1">>));
at(#{'$beamtalk_class' := 'Array'}, _Index) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be an Integer">>)).

-doc """
Return a new Array with the element at `Index` (1-based) replaced by `Value`.

O(log n): replaces a single key in the canonical index→value map. The result is
`=:=` and `phash2`-equal to an array literal with the same elements, because the
map representation is canonical by construction (two maps are `=:=` iff they hold
the same associations, regardless of edit history). No copy-on-write cache slot,
so repeated `at:put:` in a loop is O(n log n), not O(n²) — see ADR 0090.
""".
-spec at_put(map(), integer(), term()) -> map().
at_put(#{'$beamtalk_class' := 'Array', 'data' := Data}, Index, Value) when
    is_integer(Index), Index >= 1
->
    N = maps:size(Data),
    if
        Index > N ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Index is beyond array size">>),
            beamtalk_error:raise(Error2);
        true ->
            %% Key Index-1 is guaranteed to exist (0..N-1), so this replaces in
            %% place and never inserts a new key — keeping the map canonical.
            new(Data#{Index - 1 => Value})
    end;
at_put(#{'$beamtalk_class' := 'Array'}, Index, _Value) when is_integer(Index) ->
    Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be >= 1">>));
at_put(#{'$beamtalk_class' := 'Array'}, _Index, _Value) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be an Integer">>)).

%%% ============================================================================
%%% Iteration
%%% ============================================================================

-doc "Apply a block to each element of the Array in order. Returns nil.".
-spec do(map(), fun((term()) -> term())) -> 'nil'.
do(#{'$beamtalk_class' := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    lists:foreach(fun(Elem) -> Block(Elem) end, data_to_list(Data)),
    nil;
do(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'do:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

-doc "Return true if the Array contains the given element.".
-spec includes(map(), term()) -> boolean().
includes(#{'$beamtalk_class' := 'Array', 'data' := Data}, Element) ->
    lists:member(Element, maps:values(Data)).

%%% ============================================================================
%%% Functional
%%% ============================================================================

-doc "Map a block over the Array, returning a new Array.".
-spec collect(map(), fun((term()) -> term())) -> map().
collect(#{'$beamtalk_class' := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    %% maps:map preserves keys (0..N-1), so the result stays canonical.
    new(maps:map(fun(_Index, Elem) -> Block(Elem) end, Data));
collect(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'collect:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

-doc "Select elements for which block returns true, returning a new Array.".
-spec select(map(), fun((term()) -> boolean())) -> map().
select(#{'$beamtalk_class' := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    %% Re-index the kept elements to 0..M-1 so the result is canonical.
    Kept = [Elem || Elem <- data_to_list(Data), Block(Elem) =:= true],
    new(data_from_list(Kept));
select(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'select:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

-doc """
Fold the Array with an accumulator.

Calls Block(Acc, Elem) for each element — accumulator first, element second —
matching the Beamtalk `block value: acc value: each` convention.
""".
-spec inject_into(map(), term(), fun((term(), term()) -> term())) -> term().
inject_into(#{'$beamtalk_class' := 'Array', 'data' := Data}, Initial, Block) when
    is_function(Block, 2)
->
    lists:foldl(fun(Elem, Acc) -> Block(Acc, Elem) end, Initial, data_to_list(Data));
inject_into(#{'$beamtalk_class' := 'Array'}, _Initial, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'inject:into:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a binary function">>)).

%%% ============================================================================
%%% Slicing
%%% ============================================================================

-doc """
Return a new Array containing elements from 1-based index `From` to the end.

Used by codegen for rest patterns in array destructuring:
`#[a, b, ...rest] := arr` generates `slice_from(Arr, 3)` for the rest binding.
""".
-spec slice_from(map(), integer()) -> map().
slice_from(#{'$beamtalk_class' := 'Array', 'data' := Data}, From) when
    is_integer(From), From >= 1
->
    case From > maps:size(Data) of
        true ->
            from_list([]);
        false ->
            Rest = lists:nthtail(From - 1, data_to_list(Data)),
            new(data_from_list(Rest))
    end;
slice_from(#{'$beamtalk_class' := 'Array'}, From) when is_integer(From) ->
    Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'sliceFrom:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be >= 1">>));
slice_from(#{'$beamtalk_class' := 'Array'}, _From) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'sliceFrom:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be an Integer">>)).

%%% ============================================================================
%%% String Representation
%%% ============================================================================

-doc """
Return a developer-readable string representation of the Array.

Format: `#[elem1, elem2, ...]`
""".
-spec print_string(map()) -> binary().
print_string(#{'$beamtalk_class' := 'Array', 'data' := Data}) ->
    Parts = [beamtalk_primitive:print_string(E) || E <- data_to_list(Data)],
    Joined = lists:join(<<", ">>, Parts),
    iolist_to_binary(["#[", Joined, "]"]).
