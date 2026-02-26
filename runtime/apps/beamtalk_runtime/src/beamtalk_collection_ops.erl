%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime infrastructure for Collection iteration.
%%
%% BT-505: Provides `to_list/1` — a helper used by compiler-generated
%% Core Erlang (list_ops.rs) to convert non-list collection receivers
%% to Erlang lists before passing them to `lists:foldl`.
%%
%% BT-815: Provides `inject_into/3` — called by the `@primitive "inject:into:"`
%% body on the abstract Collection class.  Most other collection methods
%% (collect:, select:, reject:, includes:, detect:, anySatisfy:, allSatisfy:)
%% are now self-hosted as pure Beamtalk on Collection.bt and no longer need
%% Erlang helpers.
%%
%%% **DDD Context:** Runtime Context — Domain Service
-module(beamtalk_collection_ops).

-export([
    inject_into/3,
    to_list/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Fold a block over the collection with an accumulator.
%%
%% Calls Block(Acc, Elem) for each element — accumulator first, element
%% second — matching the Beamtalk `block value: acc value: each` convention
%% used by Collection.bt's `collect:`, `select:`, and `reject:`.
%%
%% Note: Erlang's `lists:foldl/3` calls Fun(Elem, Acc), so we wrap the
%% block to swap the argument order.
-spec inject_into(term(), term(), function()) -> term().
inject_into(Self, Initial, Block) ->
    List = to_list(Self),
    lists:foldl(
        fun(Elem, Acc) -> Block(Acc, Elem) end,
        Initial,
        List
    ).

%% @doc Convert any collection to a list by iterating with do:.
%%
%% Called from compiler-generated Core Erlang for `do:`, `collect:`,
%% `select:`, `reject:`, and `inject:into:` when the receiver is not
%% already an Erlang list.
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
