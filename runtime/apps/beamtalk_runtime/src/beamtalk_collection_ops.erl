%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Runtime helper operations for Collection abstract superclass.
%%
%% BT-505: Provides default implementations of collection iteration
%% methods that work for any collection supporting `do:` and `size`.
%%
%% These operations dispatch `do:` on the actual Self value, which
%% routes to the concrete subclass implementation (Set, Dictionary, Tuple).
%%
%% **DDD Context:** Runtime Context — Domain Service
-module(beamtalk_collection_ops).

-export([
    includes/2,
    inject_into/3,
    collect/2,
    select/2,
    reject/2,
    detect/2,
    detect_if_none/3,
    any_satisfy/2,
    all_satisfy/2,
    to_list/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Test if the collection contains the given element.
-spec includes(term(), term()) -> boolean().
includes(Self, Element) ->
    lists:member(Element, to_list(Self)).

%% @doc Reduce the collection with an accumulator using a binary block.
-spec inject_into(term(), term(), fun((term(), term()) -> term())) -> term().
inject_into(Self, Initial, Block) when is_function(Block, 2) ->
    lists:foldl(Block, Initial, to_list(Self)).

%% @doc Collect results of evaluating block on each element.
-spec collect(term(), fun((term()) -> term())) -> list().
collect(Self, Block) when is_function(Block, 1) ->
    lists:map(Block, to_list(Self)).

%% @doc Select elements for which block returns true.
-spec select(term(), fun((term()) -> boolean())) -> list().
select(Self, Block) when is_function(Block, 1) ->
    lists:filter(Block, to_list(Self)).

%% @doc Reject elements for which block returns true.
-spec reject(term(), fun((term()) -> boolean())) -> list().
reject(Self, Block) when is_function(Block, 1) ->
    lists:filter(fun(X) -> not Block(X) end, to_list(Self)).

%% @doc Find the first element matching block, or nil if none.
-spec detect(term(), fun((term()) -> boolean())) -> term().
detect(Self, Block) ->
    detect_if_none(Self, Block, fun() -> nil end).

%% @doc Find the first element matching block, or evaluate noneBlock.
-spec detect_if_none(term(), fun((term()) -> boolean()), fun(() -> term())) -> term().
detect_if_none(Self, Block, NoneBlock) when is_function(Block, 1), is_function(NoneBlock, 0) ->
    find_first(Block, NoneBlock, to_list(Self)).

%% @doc Test if any element satisfies block.
-spec any_satisfy(term(), fun((term()) -> boolean())) -> boolean().
any_satisfy(Self, Block) when is_function(Block, 1) ->
    lists:any(Block, to_list(Self)).

%% @doc Test if all elements satisfy block.
-spec all_satisfy(term(), fun((term()) -> boolean())) -> boolean().
all_satisfy(Self, Block) when is_function(Block, 1) ->
    lists:all(Block, to_list(Self)).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @doc Convert any collection to a list by iterating with do:.
%% Also called from compiler-generated Core Erlang for mutation-threading
%% paths that need lists:foldl but receive non-list collections.
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

%% @private Find first element matching predicate.
-spec find_first(fun((term()) -> boolean()), fun(() -> term()), list()) -> term().
find_first(_Block, NoneBlock, []) ->
    NoneBlock();
find_first(Block, NoneBlock, [H | T]) ->
    case Block(H) of
        true -> H;
        _ -> find_first(Block, NoneBlock, T)
    end.
