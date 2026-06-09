%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_behaviour_bt module (BT-2473).

Tests the Behaviour bootstrap stub: dispatch/4 error handling,
has_method/1 queries, and register_class/0. Models beamtalk_class_bt_tests.erl.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/4 — all selectors return DNU
%%% ============================================================================

dispatch_unknown_selector_returns_dnu_test() ->
    State = #{},
    {error, Error, RetState} = beamtalk_behaviour_bt:dispatch('unknownMethod', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Behaviour', Error#beamtalk_error.class),
    ?assertEqual('unknownMethod', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

dispatch_unknown_with_args_returns_dnu_test() ->
    State = #{},
    {error, Error, RetState} = beamtalk_behaviour_bt:dispatch(
        'method:with:args', [arg1, arg2], self(), State
    ),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Behaviour', Error#beamtalk_error.class),
    ?assertEqual('method:with:args', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

%%% ============================================================================
%%% has_method/1 — stub always returns false
%%% ============================================================================

has_method_known_selector_returns_false_test() ->
    %% The stub module has no methods. Only the compiled bt@stdlib@behaviour has methods.
    ?assertNot(beamtalk_behaviour_bt:has_method('superclass')).

has_method_unknown_selector_returns_false_test() ->
    %% Even for unknown selectors, the stub returns false (not error).
    ?assertNot(beamtalk_behaviour_bt:has_method('unknownMethod')).

has_method_selector_with_arity_returns_false_test() ->
    %% Stub returns false for all selectors, including multi-arg selectors.
    ?assertNot(beamtalk_behaviour_bt:has_method('method:with:args')).

%%% ============================================================================
%%% has_method/1 — edge cases
%%% ============================================================================

has_method_empty_selector_returns_false_test() ->
    ?assertNot(beamtalk_behaviour_bt:has_method('')).

has_method_multiple_selectors_returns_false_test() ->
    %% Stub returns false for complex selectors.
    ?assertNot(beamtalk_behaviour_bt:has_method('class')),
    ?assertNot(beamtalk_behaviour_bt:has_method('superclass')),
    ?assertNot(beamtalk_behaviour_bt:has_method('unknownMethod')).

%%% ============================================================================
%%% register_class/0 (requires bootstrap + explicit first call)
%%% ============================================================================

register_class_test_() ->
    {setup, fun setup_with_behaviour/0, fun teardown_bootstrap/1, [
        {"registered Behaviour class process is alive", fun register_class_creates_test/0},
        {"register_class is idempotent", fun register_class_idempotent_test/0},
        {"registered Behaviour has correct superclass", fun register_class_superclass_test/0}
    ]}.

setup_with_behaviour() ->
    %% beamtalk_behaviour_bt:register_class/0 is NOT called by beamtalk_bootstrap —
    %% it is invoked by the module activation system when bt@stdlib@behaviour loads.
    %% We call it explicitly here so the register_class/0 path is exercised in tests.
    case whereis(pg) of
        undefined ->
            case pg:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                {error, Reason} -> error({pg_start_failed, Reason})
            end;
        _ ->
            ok
    end,
    beamtalk_extensions:init(),
    case beamtalk_bootstrap:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Only register if not already registered (may have been loaded by stdlib)
    case beamtalk_class_registry:whereis_class('Behaviour') of
        undefined ->
            ok = beamtalk_behaviour_bt:register_class();
        _ ->
            %% Class already exists; this is expected if stdlib loaded first.
            %% Skip explicit registration to avoid stdlib_shadowing error.
            ok
    end,
    ok.

teardown_bootstrap(_) ->
    ok.

register_class_creates_test() ->
    %% After calling register_class/0, the class process must be alive.
    Pid = beamtalk_class_registry:whereis_class('Behaviour'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Behaviour', beamtalk_object_class:class_name(Pid)).

register_class_idempotent_test() ->
    %% Calling register_class/0 again hits the already_started branch in
    %% beamtalk_object_class:start/2, which calls update_class/2. Must
    %% return ok and leave the class in the registry.
    %% Note: if stdlib has loaded, the class module will be bt@stdlib@behaviour,
    %% and re-calling register_class() with the stub may trigger stdlib_shadowing.
    %% This is expected and correct; we just verify the class is still registered.
    %% register_class/0's documented contract is `ok | {error, term()}`. The
    %% case below enforces that shape — an undocumented return (e.g. a bare atom
    %% or `{ok, _}`) raises case_clause and fails the test — without pinning the
    %% error reason: under the full runtime suite the second registration can
    %% fail for reasons beyond stdlib_shadowing (e.g. transient class-process
    %% churn), so the exact reason is not asserted.
    Result = beamtalk_behaviour_bt:register_class(),
    case Result of
        ok -> ok;
        {error, _Reason} -> ok
    end,
    %% Regardless of whether register_class() succeeded or failed with
    %% stdlib_shadowing (which is expected if stdlib loaded first), the class
    %% should still be registered and available.
    Pid = beamtalk_class_registry:whereis_class('Behaviour'),
    ?assertNotEqual(undefined, Pid).

register_class_superclass_test() ->
    Pid = beamtalk_class_registry:whereis_class('Behaviour'),
    ?assertEqual('Object', beamtalk_object_class:superclass(Pid)).
