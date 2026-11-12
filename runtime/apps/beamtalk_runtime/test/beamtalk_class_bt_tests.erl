%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_bt module.

Tests Class bootstrap stub dispatch error handling, has_method/1 queries
(registration is covered by beamtalk_class_bt_registration_tests).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/4 — classBuilder selector (ADR 0038 Phase 1)
%%% ============================================================================

dispatch_classbuilder_returns_dnu_test() ->
    %% ADR 0038 Phase 1: dispatch still returns DNU even though has_method/1 returns true.
    %% has_method/1 returning true makes Class respondsTo: #classBuilder answer true, but
    %% the actual implementation is deferred to Phase 2 (class_builder.bt stdlib module).
    State = #{},
    {error, Error, RetState} = beamtalk_class_bt:dispatch('classBuilder', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Class', Error#beamtalk_error.class),
    ?assertEqual('classBuilder', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

dispatch_classbuilder_preserves_state_test() ->
    State = #{value => 99, name => <<"test">>},
    {error, _, RetState} = beamtalk_class_bt:dispatch('classBuilder', [], self(), State),
    ?assertEqual(State, RetState).

%%% ============================================================================
%%% dispatch/4 — generic unknown selectors
%%% ============================================================================

dispatch_unknown_selector_test() ->
    State = #{},
    {error, Error, RetState} = beamtalk_class_bt:dispatch('unknownMethod', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Class', Error#beamtalk_error.class),
    ?assertEqual('unknownMethod', Error#beamtalk_error.selector),
    ?assertEqual(State, RetState).

dispatch_unknown_with_args_test() ->
    State = #{},
    {error, Error, _} = beamtalk_class_bt:dispatch('foo:', [42], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Class', Error#beamtalk_error.class).

dispatch_name_selector_returns_dnu_test() ->
    %% 'name' is not handled in Phase 1; Phase 2 (class.bt) provides it
    State = #{},
    {error, Error, _} = beamtalk_class_bt:dispatch('name', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind).

dispatch_superclass_selector_returns_dnu_test() ->
    %% 'superclass' is a class method, not an instance method in this stub
    State = #{},
    {error, Error, _} = beamtalk_class_bt:dispatch('superclass', [], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind).

dispatch_preserves_nonempty_state_test() ->
    %% State map should pass through unchanged for any selector
    State = #{foo => bar, count => 42},
    {error, _, RetState} = beamtalk_class_bt:dispatch('anySelector', [], self(), State),
    ?assertEqual(State, RetState).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_classbuilder_true_test() ->
    %% ADR 0038 Phase 1: classBuilder must return true so Class respondsTo: #classBuilder
    ?assert(beamtalk_class_bt:has_method('classBuilder')).

has_method_unknown_selectors_test() ->
    ?assertNot(beamtalk_class_bt:has_method('unknownMethod')),
    ?assertNot(beamtalk_class_bt:has_method('new')),
    ?assertNot(beamtalk_class_bt:has_method('name')),
    ?assertNot(beamtalk_class_bt:has_method('superclass')),
    ?assertNot(beamtalk_class_bt:has_method('printString')).
