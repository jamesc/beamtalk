%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_array).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper operations for Array (canonical map-backed representation).

Backed by a canonical index→value map rather than Erlang's `array` module:
`array`'s copy-on-write cache slot breaks `=:=`/`phash2` consistency —
`array:set/3` leaves a stale cache node, so an array updated via `at:put:`
would not compare equal to a literal with the same elements. See ADR 0090
for the full rationale.

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

-include_lib("beamtalk_runtime/include/beamtalk_sentinel_tags.hrl").

-export([
    from_list/1,
    size/1,
    is_empty/1,
    do/2,
    at/2,
    at_put/3,
    first/1,
    last/1,
    collect/2,
    select/2,
    inject_into/3,
    includes/2,
    print_string/1,
    slice_from/2,
    reversed/1,
    'indexOf'/2,
    take/2,
    drop/2,
    sort/1,
    sort/2
]).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Wrap a canonical index→value data map in the tagged Array map.
-spec new(map()) -> map().
new(Data) when is_map(Data) ->
    #{?BT_CLASS_TAG_KEY => 'Array', 'data' => Data}.

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
    beamtalk_error:raise_type_error('Array', 'withAll:', <<"Expected a List argument">>).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

-doc "Return the number of elements in the Array.".
-spec size(map()) -> non_neg_integer().
size(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    maps:size(Data).

-doc "Return true if the Array has no elements.".
-spec is_empty(map()) -> boolean().
is_empty(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    maps:size(Data) =:= 0.

-doc """
Return the element at the given 1-based index.

Raises index_out_of_bounds if the index is out of range.

Indexing an *empty* Array raises `empty_collection` instead, matching
`beamtalk_list:at/2` and `beamtalk_string:at/2`. An index below 1 is malformed
whether or not the Array is empty, so it stays `index_out_of_bounds`.
""".
-spec at(map(), integer()) -> term().
at(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Index) when is_integer(Index), Index >= 1 ->
    N = maps:size(Data),
    if
        N =:= 0 ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    empty_collection,
                    'Array',
                    'at:',
                    <<"Array is empty; guard with `isEmpty` before indexing">>
                )
            );
        Index > N ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    index_out_of_bounds, 'Array', 'at:', <<"Index is beyond array size">>
                )
            );
        true ->
            maps:get(Index - 1, Data)
    end;
at(#{?BT_CLASS_TAG_KEY := 'Array'}, Index) when is_integer(Index) ->
    beamtalk_error:raise(
        beamtalk_error:new(index_out_of_bounds, 'Array', 'at:', <<"Index must be >= 1">>)
    );
at(#{?BT_CLASS_TAG_KEY := 'Array'}, _Index) ->
    beamtalk_error:raise_type_error('Array', 'at:', <<"Index must be an Integer">>).

-doc """
Return the first element of the Array.

Mirrors `beamtalk_string:first/1` and `beamtalk_list:first/1` —
raises `empty_collection` on an empty Array so `on:do:` can handle all three
element accessors with the same clause.
""".
-spec first(map()) -> term().
first(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    case maps:size(Data) of
        0 ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    empty_collection,
                    'Array',
                    'first',
                    <<"Array is empty; guard with `isEmpty` before calling `first`">>
                )
            );
        _ ->
            maps:get(0, Data)
    end.

-doc """
Return the last element of the Array.

Raises `empty_collection` if the Array is empty (see `first/1`).
""".
-spec last(map()) -> term().
last(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    N = maps:size(Data),
    case N of
        0 ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    empty_collection,
                    'Array',
                    'last',
                    <<"Array is empty; guard with `isEmpty` before calling `last`">>
                )
            );
        _ ->
            maps:get(N - 1, Data)
    end.

-doc """
Return a new Array with the element at `Index` (1-based) replaced by `Value`.

O(log n): replaces a single key in the canonical index→value map. The result is
`=:=` and `phash2`-equal to an array literal with the same elements, because the
map representation is canonical by construction (two maps are `=:=` iff they hold
the same associations, regardless of edit history). No copy-on-write cache slot,
so repeated `at:put:` in a loop is O(n log n), not O(n²) — see ADR 0090.
""".
-spec at_put(map(), integer(), term()) -> map().
at_put(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Index, Value) when
    is_integer(Index), Index >= 1
->
    N = maps:size(Data),
    if
        Index > N ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    index_out_of_bounds, 'Array', 'at:put:', <<"Index is beyond array size">>
                )
            );
        true ->
            %% Key Index-1 is guaranteed to exist (0..N-1), so this replaces in
            %% place and never inserts a new key — keeping the map canonical.
            new(Data#{Index - 1 => Value})
    end;
at_put(#{?BT_CLASS_TAG_KEY := 'Array'}, Index, _Value) when is_integer(Index) ->
    beamtalk_error:raise(
        beamtalk_error:new(index_out_of_bounds, 'Array', 'at:put:', <<"Index must be >= 1">>)
    );
at_put(#{?BT_CLASS_TAG_KEY := 'Array'}, _Index, _Value) ->
    beamtalk_error:raise_type_error('Array', 'at:put:', <<"Index must be an Integer">>).

%%% ============================================================================
%%% Iteration
%%% ============================================================================

-doc "Apply a block to each element of the Array in order. Returns nil.".
-spec do(map(), fun((term()) -> term())) -> 'nil'.
do(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    lists:foreach(Block, data_to_list(Data)),
    nil;
do(#{?BT_CLASS_TAG_KEY := 'Array'}, _Block) ->
    beamtalk_error:raise_type_error('Array', 'do:', <<"Block must be a unary function">>).

-doc "Return true if the Array contains the given element.".
-spec includes(map(), term()) -> boolean().
includes(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Element) ->
    %% Fold directly over the map (no intermediate maps:values/1 list) for the
    %% raw pass, matching the strict-equality membership the old array:foldl
    %% scan used.
    RawHit = maps:fold(
        fun(_Index, Value, Found) -> Found orelse Value =:= Element end, false, Data
    ),
    %% A raw miss may still be an `equals:` hit on an element class
    %% that overrides it, so fall back to the dispatching scan — and only then
    %% pay for materialising the values. `Array` is a linear sequence like
    %% `List`, so the two must agree about membership.
    RawHit orelse beamtalk_equality:scan(maps:values(Data), Element).

%%% ============================================================================
%%% Functional
%%% ============================================================================

-doc "Map a block over the Array, returning a new Array.".
-spec collect(map(), fun((term()) -> term())) -> map().
collect(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    %% maps:map preserves keys (0..N-1), so the result stays canonical.
    new(maps:map(fun(_Index, Elem) -> Block(Elem) end, Data));
collect(#{?BT_CLASS_TAG_KEY := 'Array'}, _Block) ->
    beamtalk_error:raise_type_error('Array', 'collect:', <<"Block must be a unary function">>).

-doc "Select elements for which block returns true, returning a new Array.".
-spec select(map(), fun((term()) -> boolean())) -> map().
select(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Block) when is_function(Block, 1) ->
    %% Re-index the kept elements to 0..M-1 so the result is canonical.
    Kept = [Elem || Elem <- data_to_list(Data), Block(Elem) =:= true],
    new(data_from_list(Kept));
select(#{?BT_CLASS_TAG_KEY := 'Array'}, _Block) ->
    beamtalk_error:raise_type_error('Array', 'select:', <<"Block must be a unary function">>).

-doc """
Fold the Array with an accumulator.

Calls Block(Acc, Elem) for each element — accumulator first, element second —
matching the Beamtalk `block value: acc value: each` convention.
""".
-spec inject_into(map(), term(), fun((term(), term()) -> term())) -> term().
inject_into(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Initial, Block) when
    is_function(Block, 2)
->
    lists:foldl(fun(Elem, Acc) -> Block(Acc, Elem) end, Initial, data_to_list(Data));
inject_into(#{?BT_CLASS_TAG_KEY := 'Array'}, _Initial, _Block) ->
    beamtalk_error:raise_type_error('Array', 'inject:into:', <<"Block must be a binary function">>).

%%% ============================================================================
%%% Order-dependent reads (BT-5090)
%%%
%%% Array is an indexed peer of List for reading, not for growing. These are the
%%% non-growing, order-dependent selectors List has. Each returns a new Array
%%% (or an index) and mirrors the matching `beamtalk_list` behaviour and error
%%% shapes, so the two classes agree. The specs use `term()` (not `map()`) because
%%% `Array` is a tagged map that the FFI type mapping would read as a Dictionary.
%%% ============================================================================

-doc "Return a new Array with the elements in reverse order.".
-spec reversed(term()) -> term().
reversed(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    new(data_from_list(lists:reverse(data_to_list(Data)))).

-doc """
Return the 1-based index of the first element equal to `Item`, or `nil`.

Equality is `beamtalk_equality:eq/2`, as `List>>indexOf:` uses, so an element
class's `equals:` override is honoured.
""".
-spec 'indexOf'(term(), term()) -> pos_integer() | 'nil'.
'indexOf'(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Item) ->
    index_of(data_to_list(Data), Item, 1).

-spec index_of(list(), term(), pos_integer()) -> pos_integer() | 'nil'.
index_of([], _Item, _N) ->
    nil;
index_of([Elem | Rest], Item, N) ->
    case beamtalk_equality:eq(Elem, Item) of
        true -> N;
        _ -> index_of(Rest, Item, N + 1)
    end.

-doc "Return a new Array of the first `N` elements (fewer if the Array is shorter).".
-spec take(term(), term()) -> term().
take(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, N) when is_integer(N), N >= 0 ->
    new(data_from_list(lists:sublist(data_to_list(Data), N)));
take(#{?BT_CLASS_TAG_KEY := 'Array'}, N) ->
    raise_bad_count('take:', N).

-doc "Return a new Array without the first `N` elements.".
-spec drop(term(), term()) -> term().
drop(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, N) when is_integer(N), N >= 0 ->
    case N >= maps:size(Data) of
        true -> new(#{});
        false -> new(data_from_list(lists:nthtail(N, data_to_list(Data))))
    end;
drop(#{?BT_CLASS_TAG_KEY := 'Array'}, N) ->
    raise_bad_count('drop:', N).

-spec raise_bad_count(atom(), term()) -> no_return().
raise_bad_count(Selector, N) ->
    Hint = beamtalk_error:chars_to_binary(
        io_lib:format("Argument must be a non-negative integer, got: ~tp", [N])
    ),
    beamtalk_error:raise_type_error('Array', Selector, Hint).

-doc "Return a new Array sorted by Erlang term order.".
-spec sort(term()) -> term().
sort(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    new(data_from_list(lists:sort(data_to_list(Data)))).

-doc "Return a new Array sorted by the binary comparator block.".
-spec sort(term(), term()) -> term().
sort(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, Block) when is_function(Block, 2) ->
    new(data_from_list(lists:sort(Block, data_to_list(Data))));
sort(#{?BT_CLASS_TAG_KEY := 'Array'}, _Block) ->
    beamtalk_error:raise_type_error('Array', 'sort:', <<"Block must be a binary function">>).

%%% ============================================================================
%%% Slicing
%%% ============================================================================

-doc """
Return a new Array containing elements from 1-based index `From` to the end.

Used by codegen for rest patterns in array destructuring:
`#[a, b, ...rest] := arr` generates `slice_from(Arr, 3)` for the rest binding.
""".
-spec slice_from(map(), integer()) -> map().
slice_from(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}, From) when
    is_integer(From), From >= 1
->
    case From > maps:size(Data) of
        true ->
            from_list([]);
        false ->
            Rest = lists:nthtail(From - 1, data_to_list(Data)),
            new(data_from_list(Rest))
    end;
slice_from(#{?BT_CLASS_TAG_KEY := 'Array'}, From) when is_integer(From) ->
    beamtalk_error:raise(
        beamtalk_error:new(index_out_of_bounds, 'Array', 'sliceFrom:', <<"Index must be >= 1">>)
    );
slice_from(#{?BT_CLASS_TAG_KEY := 'Array'}, _From) ->
    beamtalk_error:raise_type_error('Array', 'sliceFrom:', <<"Index must be an Integer">>).

%%% ============================================================================
%%% String Representation
%%% ============================================================================

-doc """
Return a developer-readable string representation of the Array.

Format: `#[elem1, elem2, ...]`
""".
-spec print_string(map()) -> binary().
print_string(#{?BT_CLASS_TAG_KEY := 'Array', 'data' := Data}) ->
    Parts = [beamtalk_primitive:print_string(E) || E <- data_to_list(Data)],
    Joined = lists:join(<<", ">>, Parts),
    iolist_to_binary(["#[", Joined, "]"]).
