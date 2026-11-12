%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_builder_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_builder_bt module.

Tests ClassBuilder bootstrap stub dispatch error handling (always DNU),
has_method/1 queries (always false). Registration: see beamtalk_class_bt_registration_tests.
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
