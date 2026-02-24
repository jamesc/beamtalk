%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper operations for Array (Erlang array in tagged map).
%%
%% BT-822: Array type backed by Erlang's `array` module, giving O(log n)
%% random access. Arrays are immutable — operations returning modified
%% arrays return new tagged maps.
%%
%% Representation:
%%   #{'$beamtalk_class' => 'Array', 'data' => ErlangArray}
%%
%% where ErlangArray is an opaque Erlang array record created by the
%% `array` module.
%%
%% **DDD Context:** Runtime Context — Domain Service
-module(beamtalk_array_ops).

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
    print_string/1
]).

-include("beamtalk.hrl").

%%% ============================================================================
%%% Construction
%%% ============================================================================

%% @doc Create an Array from a list of elements.
-spec from_list(list()) -> map().
from_list(List) when is_list(List) ->
    #{'$beamtalk_class' => 'Array', 'data' => array:from_list(List)};
from_list(_NonList) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'withAll:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Expected a List argument">>)).

%%% ============================================================================
%%% Accessors
%%% ============================================================================

%% @doc Return the number of elements in the Array.
-spec size(map()) -> non_neg_integer().
size(#{'$beamtalk_class' := 'Array', 'data' := Arr}) ->
    array:size(Arr).

%% @doc Return true if the Array has no elements.
-spec is_empty(map()) -> boolean().
is_empty(#{'$beamtalk_class' := 'Array', 'data' := Arr}) ->
    array:size(Arr) =:= 0.

%% @doc Return the element at the given 1-based index.
%%
%% Raises index_out_of_bounds if the index is out of range.
-spec at(map(), integer()) -> term().
at(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Index) when is_integer(Index), Index >= 1 ->
    N = array:size(Arr),
    if
        Index > N ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Index is beyond array size">>),
            beamtalk_error:raise(Error2);
        true ->
            array:get(Index - 1, Arr)
    end;
at(#{'$beamtalk_class' := 'Array'}, Index) when is_integer(Index) ->
    Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be >= 1">>));
at(#{'$beamtalk_class' := 'Array'}, _Index) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'at:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Index must be an Integer">>)).

%% @doc Return a new Array with the element at `Index` (1-based) replaced by `Value`.
-spec at_put(map(), integer(), term()) -> map().
at_put(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Index, Value) when
    is_integer(Index), Index >= 1
->
    N = array:size(Arr),
    if
        Index > N ->
            Error0 = beamtalk_error:new(index_out_of_bounds, 'Array'),
            Error1 = beamtalk_error:with_selector(Error0, 'at:put:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Index is beyond array size">>),
            beamtalk_error:raise(Error2);
        true ->
            #{'$beamtalk_class' => 'Array', 'data' => array:set(Index - 1, Value, Arr)}
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

%% @doc Apply a block to each element of the Array. Returns nil.
-spec do(map(), fun((term()) -> term())) -> nil.
do(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Block) when is_function(Block, 1) ->
    array:foldl(fun(_I, Elem, _Acc) -> Block(Elem) end, nil, Arr),
    nil;
do(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'do:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

%% @doc Return true if the Array contains the given element.
-spec includes(map(), term()) -> boolean().
includes(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Element) ->
    array:foldl(
        fun(_I, Elem, Found) ->
            Found orelse (Elem =:= Element)
        end,
        false,
        Arr
    ).

%%% ============================================================================
%%% Functional
%%% ============================================================================

%% @doc Map a block over the Array, returning a new Array.
-spec collect(map(), fun((term()) -> term())) -> map().
collect(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Block) when is_function(Block, 1) ->
    Mapped = array:map(fun(_I, Elem) -> Block(Elem) end, Arr),
    #{'$beamtalk_class' => 'Array', 'data' => Mapped};
collect(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'collect:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

%% @doc Select elements for which block returns true, returning a new Array.
-spec select(map(), fun((term()) -> boolean())) -> map().
select(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Block) when is_function(Block, 1) ->
    Selected = array:foldl(
        fun(_I, Elem, Acc) ->
            case Block(Elem) of
                true -> [Elem | Acc];
                _ -> Acc
            end
        end,
        [],
        Arr
    ),
    #{'$beamtalk_class' => 'Array', 'data' => array:from_list(lists:reverse(Selected))};
select(#{'$beamtalk_class' := 'Array'}, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'select:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a unary function">>)).

%% @doc Fold the Array with an accumulator.
%%
%% Calls Block(Acc, Elem) for each element — accumulator first, element second —
%% matching the Beamtalk `block value: acc value: each` convention.
-spec inject_into(map(), term(), fun((term(), term()) -> term())) -> term().
inject_into(#{'$beamtalk_class' := 'Array', 'data' := Arr}, Initial, Block) when
    is_function(Block, 2)
->
    array:foldl(
        fun(_I, Elem, Acc) -> Block(Acc, Elem) end,
        Initial,
        Arr
    );
inject_into(#{'$beamtalk_class' := 'Array'}, _Initial, _Block) ->
    Error0 = beamtalk_error:new(type_error, 'Array'),
    Error1 = beamtalk_error:with_selector(Error0, 'inject:into:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, <<"Block must be a binary function">>)).

%%% ============================================================================
%%% String Representation
%%% ============================================================================

%% @doc Return a developer-readable string representation of the Array.
%%
%% Format: `#[elem1, elem2, ...]`
-spec print_string(map()) -> binary().
print_string(#{'$beamtalk_class' := 'Array', 'data' := Arr}) ->
    Elements = array:to_list(Arr),
    Parts = lists:map(fun(E) -> beamtalk_primitive:print_string(E) end, Elements),
    Joined = lists:join(<<", ">>, Parts),
    iolist_to_binary(["#[", Joined, "]"]).
