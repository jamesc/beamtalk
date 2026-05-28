#!/usr/bin/env escript
%%% Copyright 2026 James Casey
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% Regenerate the ADR 0088 Phase 0b reference ETF bytes (BT-2315).
%%%
%%% Emits two payloads in the same format used by the byte-equivalence
%%% tests in:
%%%
%%%   * `crates/beamtalk-core/src/codegen/core_erlang/cerl.rs`
%%%     (`EMPTY_BT_NAPKIN_ETF`, `MINIMUM_BT_NAPKIN_ETF`).
%%%
%%% Run after upgrading OTP to verify the cerl record shape has not changed
%%% (extremely unlikely — stable since OTP 18). Diff the output against the
%%% hard-coded constants in `cerl.rs` if you need to refresh them.

-mode(compile).

main(_) ->
    dump("EMPTY (no defs)", empty_module()),
    dump("MINIMUM (module_info/0,1)", minimum_module()),
    ok.

empty_module() ->
    cerl:c_module(cerl:c_atom(bt_napkin), [], [], []).

minimum_module() ->
    ModName = bt_napkin,
    Mi0Var = cerl:c_var({module_info, 0}),
    Mi1Var = cerl:c_var({module_info, 1}),
    Mi0Fun = cerl:c_fun(
        [],
        cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(get_module_info),
                    [cerl:c_atom(ModName)])
    ),
    X = cerl:c_var('X'),
    Mi1Fun = cerl:c_fun(
        [X],
        cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(get_module_info),
                    [cerl:c_atom(ModName), X])
    ),
    Defs = [{Mi0Var, Mi0Fun}, {Mi1Var, Mi1Fun}],
    cerl:c_module(cerl:c_atom(ModName), [Mi0Var, Mi1Var], [], Defs).

dump(Label, Term) ->
    Bytes = term_to_binary(Term),
    io:format("~n=== ~s — ~p bytes ===~n", [Label, byte_size(Bytes)]),
    io:format("Term: ~p~n", [Term]),
    io:format("ETF:  ~w~n", [Bytes]).
