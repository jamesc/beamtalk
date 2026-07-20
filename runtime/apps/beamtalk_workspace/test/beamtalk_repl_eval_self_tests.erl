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

%% BT-2956: `eval_with_self/2` always rejects a class-definition-shaped
%% source via `eval_not_an_expression_error/0` — it must not register (or,
%% worse, clobber) a `beamtalk_alias_xref` edge along the way, since nothing
%% ever reads the ClassInfo that would drive that registration. A pre-existing
%% edge for the same class name (as a prior real `:load` would have left
%% behind) must survive the rejected `evaluate:` call completely untouched —
%% before this fix, `compile_class_definition_result/2`'s unconditional
%% `register_class(ClassNameBin, ReferencedAliases)` (whole-set replacement,
%% not a delta) would have stomped it to `[]`.
eval_with_self_class_definition_does_not_touch_alias_xref_test_() ->
    {setup, fun alias_xref_setup/0, fun alias_xref_teardown/1, fun() ->
        ok = beamtalk_alias_xref:register_class(<<"EvalSelfXrefClass">>, [
            <<"EvalSelfXrefAlias">>
        ]),
        Source = <<"Object subclass: EvalSelfXrefClass\n\n  bar => 1\n">>,
        ?assertMatch({error, _}, beamtalk_repl_eval:eval_with_self(nil, Source)),
        ?assertEqual(
            [<<"EvalSelfXrefClass">>],
            beamtalk_alias_xref:dependents_of(<<"EvalSelfXrefAlias">>)
        )
    end}.

alias_xref_setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    case whereis(beamtalk_alias_xref) of
        undefined -> {ok, _} = beamtalk_alias_xref:start_link();
        _Pid -> ok
    end,
    ok = beamtalk_alias_xref:clear().

alias_xref_teardown(_) ->
    ok = beamtalk_alias_xref:clear().

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
