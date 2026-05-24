%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_bt_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_bt module (BT-2262).

Tests Class bootstrap stub dispatch error handling, has_method/1 queries,
and class registration. Models beamtalk_metaclass_bt_tests.erl.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% dispatch/4 — classBuilder selector (ADR 0038 Phase 1)
%%% ============================================================================

dispatch_classbuilder_returns_dnu_test() ->
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
    %% 'name' is not handled in Phase 1; Phase 2 (Class.bt) provides it
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

%%% ============================================================================
%%% register_class/0 (requires bootstrap)
%%% ============================================================================

register_class_test_() ->
    {setup, fun setup_bootstrap/0, fun teardown_bootstrap/1, [
        {"register_class creates Class", fun register_class_creates_test/0},
        {"register_class is idempotent", fun register_class_idempotent_test/0},
        {"registered Class has correct superclass", fun register_class_superclass_test/0}
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
    %% Bootstrap already registered Class; verify it's there
    Pid = beamtalk_class_registry:whereis_class('Class'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Class', beamtalk_object_class:class_name(Pid)).

register_class_idempotent_test() ->
    %% Calling register_class again must not crash and Class must still be there
    ok = beamtalk_class_bt:register_class(),
    Pid = beamtalk_class_registry:whereis_class('Class'),
    ?assertNotEqual(undefined, Pid).

register_class_superclass_test() ->
    Pid = beamtalk_class_registry:whereis_class('Class'),
    ?assertEqual('Object', beamtalk_object_class:superclass(Pid)).
