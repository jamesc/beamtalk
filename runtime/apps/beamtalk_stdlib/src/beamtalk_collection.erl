%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime infrastructure for Collection iteration.

BT-505: Provides `to_list/1` — a helper used by compiler-generated
Core Erlang (list_ops.rs) to convert non-list collection receivers
to Erlang lists before passing them to `lists:foldl`.

BT-815: Provides `inject_into/3` — called by the `@primitive "inject:into:"`
body on the abstract Collection class.  Most other collection methods
(collect:, select:, reject:, includes:, detect:, anySatisfy:, allSatisfy:)
are now self-hosted as pure Beamtalk on Collection.bt and no longer need
Erlang helpers.
""".

-export([
    average/1,
    from_list_like/2,
    inject_into/3,
    maximum/1,
    minimum/1,
    sum/1,
    to_list/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Fold a block over the collection with an accumulator.

Calls Block(Acc, Elem) for each element — accumulator first, element
second — matching the Beamtalk `block value: acc value: each` convention
used by Collection.bt's `collect:`, `select:`, and `reject:`.

Note: Erlang's `lists:foldl/3` calls Fun(Elem, Acc), so we wrap the
block to swap the argument order.
""".
-spec inject_into(term(), term(), function()) -> term().
inject_into(Self, Initial, Block) ->
    List = to_list(Self),
    lists:foldl(
        fun(Elem, Acc) -> Block(Acc, Elem) end,
        Initial,
        List
    ).

-doc """
Sum all elements, returning 0 for an empty collection.

Backs `@primitive "sum"` on Collection. Elements must support `+`.
""".
-spec sum(term()) -> number().
sum(Self) ->
    lists:foldl(fun(Elem, Acc) -> Acc + Elem end, 0, to_list(Self)).

-doc """
Return the largest element, comparing with `>`.

Backs `@primitive "max"` on Collection. Raises a user_error on an empty
collection — there is no sensible maximum of nothing.
""".
-spec maximum(term()) -> term().
maximum(Self) ->
    case to_list(Self) of
        [] ->
            raise_empty('max');
        [H | T] ->
            lists:foldl(
                fun(E, Acc) ->
                    case E > Acc of
                        true -> E;
                        false -> Acc
                    end
                end,
                H,
                T
            )
    end.

-doc """
Return the smallest element, comparing with `<`.

Backs `@primitive "min"` on Collection. Raises a user_error on an empty
collection.
""".
-spec minimum(term()) -> term().
minimum(Self) ->
    case to_list(Self) of
        [] ->
            raise_empty('min');
        [H | T] ->
            lists:foldl(
                fun(E, Acc) ->
                    case E < Acc of
                        true -> E;
                        false -> Acc
                    end
                end,
                H,
                T
            )
    end.

-doc """
Return the mean of the elements as a float.

Backs `@primitive "average"` on Collection. Raises a user_error on an empty
collection.
""".
-spec average(term()) -> float().
average(Self) ->
    case to_list(Self) of
        [] -> raise_empty('average');
        List -> lists:sum(List) / length(List)
    end.

-spec raise_empty(atom()) -> no_return().
raise_empty(Selector) ->
    Error0 = beamtalk_error:new(user_error, 'Collection'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(
        beamtalk_error:with_hint(Error1, <<"Collection is empty">>)
    ).

-doc """
Reconstruct a `collect:`/`select:`/`reject:` result so its type matches the
original receiver, mirroring the pure (non-mutating) list-op path.

The stateful (mutation-threading) list-op codegen folds over `to_list/1` of the
receiver and produces a raw Erlang list. This helper wraps that list back into
the receiver's representation:

- String receiver (binary) → binary (`iolist_to_binary/1`)
- Array receiver → `Array` (`beamtalk_array:from_list/1`)
- anything else (already an Erlang list, or another collection) → the list as-is

Called from compiler-generated Core Erlang (`list_ops`) — see BT-2342.
""".
-spec from_list_like(term(), list()) -> term().
from_list_like(Recv, List) when is_binary(Recv) ->
    erlang:iolist_to_binary(List);
from_list_like(#{'$beamtalk_class' := 'Array'}, List) ->
    beamtalk_array:from_list(List);
from_list_like(_Recv, List) ->
    List.

-doc """
Convert any collection to a list by iterating with do:.

Called from compiler-generated Core Erlang for `do:`, `collect:`,
`select:`, `reject:`, and `inject:into:` when the receiver is not
already an Erlang list.
""".
-spec to_list(term()) -> list().
to_list(Self) when is_list(Self) ->
    Self;
to_list(Self) ->
    Ref = make_ref(),
    put(Ref, []),
    try
        Block = fun(Each) ->
            put(Ref, [Each | get(Ref)])
        end,
        beamtalk_primitive:send(Self, 'do:', [Block]),
        lists:reverse(get(Ref))
    after
        erase(Ref)
    end.
