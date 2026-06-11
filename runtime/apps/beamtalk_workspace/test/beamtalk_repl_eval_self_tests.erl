%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_eval_self_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for the stateless `evaluate:` path (`beamtalk_repl_eval:eval_with_self/2`,
BT-2503) — the Inspector's value `evaluate:`.

Focus: the per-process transient-module lifecycle. A hot `evaluate:` loop must not
leak a never-reclaimed atom per call, and each evaluation's compiled module must be
fully unloaded afterwards (the `code:delete`-then-`code:purge` order, not the
no-op reverse).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% A bare `self` expression evaluates to the bound value, with no runtime
%% primitives required.
eval_with_self_returns_bound_self_test() ->
    ?assertEqual({ok, 42}, beamtalk_repl_eval:eval_with_self(42, <<"self">>)).

%% Repeated `evaluate:` calls in one process reuse a single module-name atom
%% (cached in the process dictionary), so a tight loop cannot exhaust the atom
%% table, and each transient module is fully unloaded after its call. EUnit runs
%% each test in its own process, so the cached name starts empty here.
eval_with_self_reuses_module_and_unloads_test() ->
    ?assertEqual(undefined, get('$beamtalk_inspector_eval_module')),
    ?assertEqual({ok, 1}, beamtalk_repl_eval:eval_with_self(1, <<"self">>)),
    Module = get('$beamtalk_inspector_eval_module'),
    ?assert(is_atom(Module)),
    %% The transient module is fully unloaded after the call.
    ?assertEqual(false, code:is_loaded(Module)),
    %% A second call recycles the same atom — no new module name is minted.
    ?assertEqual({ok, 2}, beamtalk_repl_eval:eval_with_self(2, <<"self">>)),
    ?assertEqual(Module, get('$beamtalk_inspector_eval_module')),
    ?assertEqual(false, code:is_loaded(Module)).
