%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_metaclass_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_metaclass_bt module.

Tests metaclass identity predicates, dispatch error handling,
has_method/1 queries. Registration: see beamtalk_bt_stub_registration_tests.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/4 — identity predicates
%%% ============================================================================

dispatch_is_meta_test() ->
    State = #{},
    ?assertEqual({reply, true, State}, beamtalk_metaclass_bt:dispatch('isMeta', [], self(), State)).

dispatch_is_class_test() ->
    State = #{},
    ?assertEqual(
        {reply, false, State}, beamtalk_metaclass_bt:dispatch('isClass', [], self(), State)
    ).

dispatch_is_metaclass_test() ->
    State = #{},
    ?assertEqual(
        {reply, true, State}, beamtalk_metaclass_bt:dispatch('isMetaclass', [], self(), State)
    ).

dispatch_preserves_state_test() ->
    %% State map should pass through unchanged
    State = #{foo => bar, count => 42},
    {reply, true, RetState} = beamtalk_metaclass_bt:dispatch('isMeta', [], self(), State),
    ?assertEqual(State, RetState).

%%% ============================================================================
%%% dispatch/4 — unknown selectors
%%% ============================================================================

dispatch_unknown_selector_test() ->
    State = #{},
    {error, Error, RetState} = beamtalk_metaclass_bt:dispatch(
        'unknownMethod', [], self(), State
    ),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind),
    ?assertEqual('Metaclass', Error#beamtalk_error.class),
    ?assertEqual(State, RetState).

dispatch_unknown_with_args_test() ->
    State = #{},
    {error, Error, _} = beamtalk_metaclass_bt:dispatch('foo:', [42], self(), State),
    ?assertEqual(does_not_understand, Error#beamtalk_error.kind).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_known_selectors_test() ->
    ?assert(beamtalk_metaclass_bt:has_method('isMeta')),
    ?assert(beamtalk_metaclass_bt:has_method('isClass')),
    ?assert(beamtalk_metaclass_bt:has_method('isMetaclass')).

has_method_unknown_selectors_test() ->
    ?assertNot(beamtalk_metaclass_bt:has_method('unknownMethod')),
    ?assertNot(beamtalk_metaclass_bt:has_method('new')),
    ?assertNot(beamtalk_metaclass_bt:has_method('name')),
    ?assertNot(beamtalk_metaclass_bt:has_method('superclass')).
