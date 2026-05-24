%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_builder_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_builder_bt module (BT-2262).

Tests ClassBuilder bootstrap stub dispatch error handling (always DNU),
has_method/1 queries (always false), and class registration.
Models beamtalk_metaclass_bt_tests.erl and beamtalk_class_bt_tests.erl.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/4 — always returns does_not_understand (Phase 1 stub)
%%% ============================================================================

dispatch_any_selector_returns_dnu_test() ->
    State = #{},
    {error, Error, RetState} = beamtalk_class_builder_bt:dispatch('anyMethod', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('ClassBuilder', Error#beamtalk_error.class),
    ?assertEqual('anyMethod', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

dispatch_name_colon_returns_dnu_test() ->
    %% name: is a Phase 2 ClassBuilder protocol method; Phase 1 stub returns DNU
    State = #{},
    {error, Error, RetState} = beamtalk_class_builder_bt:dispatch(
        'name:', [<<"MyClass">>], self(), State
    ),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('ClassBuilder', Error#beamtalk_error.class),
    ?assertEqual('name:', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

dispatch_superclass_colon_returns_dnu_test() ->
    %% superclass: is a Phase 2 ClassBuilder protocol method; Phase 1 stub returns DNU
    State = #{},
    {error, Error, _} = beamtalk_class_builder_bt:dispatch(
        'superclass:', [self()], self(), State
    ),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('ClassBuilder', Error#beamtalk_error.class).

dispatch_register_returns_dnu_test() ->
    %% register is a Phase 2 ClassBuilder protocol method
    State = #{},
    {error, Error, _} = beamtalk_class_builder_bt:dispatch('register', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind).

dispatch_preserves_nonempty_state_test() ->
    %% State map must pass through unchanged for every selector
    State = #{value => 100, tag => <<"cb">>},
    {error, _, RetState} = beamtalk_class_builder_bt:dispatch('anything', [], self(), State),
    ?assertEqual(State, RetState).

%%% ============================================================================
%%% has_method/1 — always false in Phase 1 stub
%%% ============================================================================

has_method_returns_false_for_any_selector_test() ->
    ?assertNot(beamtalk_class_builder_bt:has_method('name:')),
    ?assertNot(beamtalk_class_builder_bt:has_method('superclass:')),
    ?assertNot(beamtalk_class_builder_bt:has_method('fields:')),
    ?assertNot(beamtalk_class_builder_bt:has_method('methods:')),
    ?assertNot(beamtalk_class_builder_bt:has_method('register')),
    ?assertNot(beamtalk_class_builder_bt:has_method('new')),
    ?assertNot(beamtalk_class_builder_bt:has_method('unknownSelector')).

%%% ============================================================================
%%% register_class/0 (requires bootstrap)
%%% ============================================================================

register_class_test_() ->
    {setup, fun setup_bootstrap/0, fun teardown_bootstrap/1, [
        {"register_class creates ClassBuilder", fun register_class_creates_test/0},
        {"register_class is idempotent", fun register_class_idempotent_test/0},
        {"registered ClassBuilder has correct superclass", fun register_class_superclass_test/0}
    ]}.

setup_bootstrap() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    case beamtalk_bootstrap:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    ok.

teardown_bootstrap(_) ->
    ok.

register_class_creates_test() ->
    %% Bootstrap already registered ClassBuilder; verify it is present
    Pid = beamtalk_class_registry:whereis_class('ClassBuilder'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('ClassBuilder', beamtalk_object_class:class_name(Pid)).

register_class_idempotent_test() ->
    %% Calling register_class again must not crash and ClassBuilder must still be there
    ok = beamtalk_class_builder_bt:register_class(),
    Pid = beamtalk_class_registry:whereis_class('ClassBuilder'),
    ?assertNotEqual(undefined, Pid).

register_class_superclass_test() ->
    Pid = beamtalk_class_registry:whereis_class('ClassBuilder'),
    ?assertEqual('Actor', beamtalk_object_class:superclass(Pid)).
