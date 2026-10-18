%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_tier2_test_block).

%%% **DDD Context:** Object System Context

-moduledoc """
Hand-written Tier 2 blocks for the ADR 0132 EUnit tests (BT-3885).

`beamtalk_dispatch:call_threaded/3` takes a fun of one argument more than it is
called with as a Tier 2 block only when it carries the Tier 2 mark
(`beamtalk_dispatch:is_tier2_block/1`), which codegen gives every Tier 2 block
it compiles. A test that hands the gate an Erlang `fun(Args..., Acc) -> {R,
Acc1} end` mints it here, under the same generated name
(`?BT_TIER2_BLOCK_VAR`, `beamtalk_threading_abi.hrl`), so the tests exercise
exactly the mark the gate reads and drift between the two fails them.
""".

-export([mark/1]).

-include("beamtalk_threading_abi.hrl").

-doc """
`F` (a `fun(Args..., Acc) -> {Result, Acc1}` of arity 1 to 4) as a marked Tier 2
block of the same arity.
""".
-spec mark(function()) -> function().
mark(F) when is_function(F, 1) ->
    fun ?BT_TIER2_BLOCK_VAR(Acc) ->
        _ = ?BT_TIER2_BLOCK_VAR,
        F(Acc)
    end;
mark(F) when is_function(F, 2) ->
    fun ?BT_TIER2_BLOCK_VAR(A, Acc) ->
        _ = ?BT_TIER2_BLOCK_VAR,
        F(A, Acc)
    end;
mark(F) when is_function(F, 3) ->
    fun ?BT_TIER2_BLOCK_VAR(A, B, Acc) ->
        _ = ?BT_TIER2_BLOCK_VAR,
        F(A, B, Acc)
    end;
mark(F) when is_function(F, 4) ->
    fun ?BT_TIER2_BLOCK_VAR(A, B, C, Acc) ->
        _ = ?BT_TIER2_BLOCK_VAR,
        F(A, B, C, Acc)
    end.
