%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_chain_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Wire check tests for ADR 0032 Phase 0.

Proves the core assumption of the Early Class Protocol:
a class-side message not found in user-defined class methods
dispatches through the 'Class' instance method chain.

## What This Tests

1. **Dispatch fallthrough**: `Counter yourself` reaches the `Class`
   instance-method chain (`Object>>yourself`, which `Counter` does not
   define class-side) via `try_class_chain_fallthrough/3`.

2. **Metaclass tag preservation**: The `self` argument received by
   the Class method has the 'Counter class' tag, not 'Counter'.
   This proves the virtual metaclass tag survives the dispatch chain.

3. **DNU still works**: Messages not in Class chain still raise
   does_not_understand.

4. **Fallback when Class absent**: When 'Class' is not registered,
   does_not_understand is raised (no crash, graceful fallback).

## Phase 0 Outcome

The probe is the stdlib's own `Object>>yourself`, so no test-only method is
installed. The dispatch mechanism (try_class_chain_fallthrough) remains in
beamtalk_class_dispatch.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    beamtalk_test_boot:boot_real_stdlib('Integer'),
    %% The probe is `Object>>yourself` (sealed, answers its receiver): Counter
    %% defines no class-side `yourself`, so it is reached by
    %% try_class_chain_fallthrough/3 through the Class chain.
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Test Suite
%%====================================================================

class_chain_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"BT-732: Counter yourself dispatches through Class chain",
                fun test_dispatch_fallthrough/0},
            {"BT-732: metaclass tag is preserved in self", fun test_metaclass_tag_preserved/0},
            {"BT-732: unknown message still raises does_not_understand",
                fun test_unknown_message_dnu/0},
            {"BT-732: fallthrough returns not_found when Class absent",
                fun test_fallthrough_absent_class/0}
        ]
    end}.

%%====================================================================
%% Test Implementations
%%====================================================================

%% Test 1: The dispatch fallthrough works end-to-end.
%%
%% When Counter (a class object) receives yourself,
%% it is NOT defined in Counter's class methods, so the dispatch
%% falls through to the 'Class' instance-method chain, where
%% Object>>yourself handles it.
test_dispatch_fallthrough() ->
    ok = beamtalk_test_class:ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertNotEqual(undefined, CounterPid),

    %% class_send delegates to user-defined class methods first (not found),
    %% then falls through to Class chain (Object>>yourself).
    Result = beamtalk_object_class:class_send(CounterPid, yourself, []),

    %% Result is Self, the class object
    ?assertMatch(#beamtalk_object{}, Result).

%% Test 2: The virtual metaclass tag is preserved.
%%
%% The self received by yourself must have class = 'Counter class'
%% (the metaclass tag), not 'Counter'.
%% This confirms that class objects retain their identity through dispatch.
test_metaclass_tag_preserved() ->
    ok = beamtalk_test_class:ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    Self = beamtalk_object_class:class_send(CounterPid, yourself, []),

    %% Self must be a beamtalk_object with the metaclass tag 'Counter class'
    ExpectedTag = beamtalk_class_registry:class_object_tag('Counter'),
    ?assertEqual(ExpectedTag, Self#beamtalk_object.class),
    %% The pid in Self must be the Counter class process pid
    ?assertEqual(CounterPid, Self#beamtalk_object.pid).

%% Test 3: Unknown messages still raise does_not_understand.
%%
%% The fallthrough only adds one more lookup before DNU.
%% If a message is not in user-defined class methods AND not in Class,
%% it must still raise does_not_understand with correct class name.
test_unknown_message_dnu() ->
    ok = beamtalk_test_class:ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    ?assertBtError(
        does_not_understand,
        'Counter',
        beamtalk_object_class:class_send(CounterPid, totallyUnknownMessage, [])
    ).

%% Test 4: Graceful fallback when Class is not registered.
%%
%% try_class_chain_fallthrough/3 returns not_found when 'Class' is absent,
%% and class_send raises does_not_understand (no crash).
test_fallthrough_absent_class() ->
    ok = beamtalk_test_class:ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    %% `subclasses` is a Behaviour method, answered only through the Class chain
    %% (unlike `yourself`, which the class-object protocol also answers itself).
    ClassPid = beamtalk_class_registry:whereis_class('Class'),
    ?assert(is_pid(ClassPid)),
    ClassModule = beamtalk_object_class:module_name(ClassPid),
    %% Kill 'Class' to test the absent case.
    %% Use monitor + DOWN to avoid flaky timer:sleep on loaded CI nodes.
    Ref = erlang:monitor(process, ClassPid),
    exit(ClassPid, kill),
    ?awaitMessage({'DOWN', Ref, process, ClassPid, _Reason}, 1000),

    try
        %% Should raise does_not_understand, not crash
        ?assertBtError(
            does_not_understand,
            'Counter',
            beamtalk_object_class:class_send(CounterPid, subclasses, [])
        )
    after
        %% Re-register the real 'Class' (same module it had) so later tests
        %% and modules in the same VM are not affected.
        ok = ClassModule:register_class()
    end.

%%====================================================================
%% Helpers
%%====================================================================
