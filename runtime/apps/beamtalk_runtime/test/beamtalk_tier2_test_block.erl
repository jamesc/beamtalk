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
Acc1} end` mints it here, as a named fun called `?BT_TIER2_BLOCK_NAME`
(`beamtalk_threading_abi.hrl`). Erlang cannot take a fun name from a macro in
a form `erlfmt` parses, so the name is spelled below;
`beamtalk_dispatch_tests:tier2_mark_test_/0` fails if it ever differs from the
generated one, and every gate test that uses `mark/1` fails with it.
""".

-export([mark/1]).

-doc """
`F` (a `fun(Args..., Acc) -> {Result, Acc1}` of arity 1 to 4) as a marked Tier 2
block of the same arity. The fun's name is used in its body, or the compiler
would drop it.
""".
-spec mark(function()) -> function().
mark(F) when is_function(F, 1) ->
    fun BtTier2Block(Acc) ->
        _ = BtTier2Block,
        F(Acc)
    end;
mark(F) when is_function(F, 2) ->
    fun BtTier2Block(A, Acc) ->
        _ = BtTier2Block,
        F(A, Acc)
    end;
mark(F) when is_function(F, 3) ->
    fun BtTier2Block(A, B, Acc) ->
        _ = BtTier2Block,
        F(A, B, Acc)
    end;
mark(F) when is_function(F, 4) ->
    fun BtTier2Block(A, B, C, Acc) ->
        _ = BtTier2Block,
        F(A, B, C, Acc)
    end.
