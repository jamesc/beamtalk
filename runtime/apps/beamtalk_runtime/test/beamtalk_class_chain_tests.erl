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

1. **Dispatch fallthrough**: `Counter testClassProtocol` reaches
   the `Class` instance-method chain (in this suite, a `testClassProtocol`
   extension registered on `Class`) via `try_class_chain_fallthrough/3`.

2. **Metaclass tag preservation**: The `self` argument received by
   the Class method has the 'Counter class' tag, not 'Counter'.
   This proves the virtual metaclass tag survives the dispatch chain.

3. **DNU still works**: Messages not in Class chain still raise
   does_not_understand.

4. **Fallback when Class absent**: When 'Class' is not registered,
   does_not_understand is raised (no crash, graceful fallback).

## Phase 0 Outcome

The testClassProtocol probe is a test-registered extension on `Class`
(removed in teardown) to keep production code clean. The dispatch mechanism
(try_class_chain_fallthrough) remains in beamtalk_class_dispatch.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    %% Provide the testClassProtocol probe as an instance-side extension on
    %% 'Class' (the supported open-class path) instead of re-pointing the
    %% registered 'Class' at a test module. Redefining 'Class' from a non-stdlib
    %% module is refused by the stdlib-shadowing gate once the compiled stdlib
    %% 'Class' is registered (any fresh VM that ran beamtalk_stdlib:init/0).
    register_probe(),
    ok.

teardown(_) ->
    unregister_probe(),
    ok.

%% Install / remove the testClassProtocol probe on 'Class'. dispatch/5 checks
%% the extension registry before the class's own methods, so the probe is
%% reached by try_class_chain_fallthrough/3 without touching 'Class' itself.
register_probe() ->
    ProbeFun = fun([], Self) -> {class_protocol_ok, Self} end,
    ok = beamtalk_extensions:register('Class', testClassProtocol, ProbeFun, class_chain_tests).

unregister_probe() ->
    ok = beamtalk_extensions:unregister('Class', testClassProtocol).

%%====================================================================
%% Test Suite
%%====================================================================

class_chain_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"BT-732: Counter testClassProtocol dispatches through Class chain",
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
%% When Counter (a class object) receives testClassProtocol,
%% it is NOT defined in Counter's class methods, so the dispatch
%% falls through to 'Class' instance methods, where
%% the testClassProtocol extension probe handles it.
test_dispatch_fallthrough() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertNotEqual(undefined, CounterPid),

    %% class_send delegates to user-defined class methods first (not found),
    %% then falls through to Class chain (testClassProtocol found in test helper).
    Result = beamtalk_object_class:class_send(CounterPid, testClassProtocol, []),

    %% Result is {class_protocol_ok, Self} where Self is the class object
    ?assertMatch({class_protocol_ok, #beamtalk_object{}}, Result).

%% Test 2: The virtual metaclass tag is preserved.
%%
%% The self received by testClassProtocol must have class = 'Counter class'
%% (the metaclass tag), not 'Counter'.
%% This confirms that class objects retain their identity through dispatch.
test_metaclass_tag_preserved() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    {class_protocol_ok, Self} =
        beamtalk_object_class:class_send(CounterPid, testClassProtocol, []),

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
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    ?assertError(
        #{error := #beamtalk_error{kind = does_not_understand, class = 'Counter'}},
        beamtalk_object_class:class_send(CounterPid, totallyUnknownMessage, [])
    ).

%% Test 4: Graceful fallback when Class is not registered.
%%
%% try_class_chain_fallthrough/3 returns not_found when 'Class' is absent,
%% and class_send raises does_not_understand (no crash).
test_fallthrough_absent_class() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    %% The probe extension is checked before the Class process is consulted,
    %% so remove it for the absent-Class case and restore it afterwards.
    unregister_probe(),
    ClassPid = beamtalk_class_registry:whereis_class('Class'),
    ?assert(is_pid(ClassPid)),
    ClassModule = beamtalk_object_class:module_name(ClassPid),
    %% Kill 'Class' to test the absent case.
    %% Use monitor + DOWN to avoid flaky timer:sleep on loaded CI nodes.
    Ref = erlang:monitor(process, ClassPid),
    exit(ClassPid, kill),
    receive
        {'DOWN', Ref, process, ClassPid, _Reason} -> ok
    after 1000 ->
        ?assert(false)
    end,

    try
        %% Should raise does_not_understand, not crash
        ?assertError(
            #{error := #beamtalk_error{kind = does_not_understand, class = 'Counter'}},
            beamtalk_object_class:class_send(CounterPid, testClassProtocol, [])
        )
    after
        %% Re-register the real 'Class' (same module it had) and the probe so
        %% later tests and modules in the same VM are not affected.
        ok = ClassModule:register_class(),
        register_probe()
    end.

%%====================================================================
%% Helpers
%%====================================================================

ensure_counter_loaded() ->
    case erlang:function_exported('bt@counter', register_class, 0) of
        true ->
            'bt@counter':register_class(),
            ok;
        false ->
            case code:load_file('bt@counter') of
                {module, 'bt@counter'} ->
                    case erlang:function_exported('bt@counter', register_class, 0) of
                        true ->
                            'bt@counter':register_class(),
                            ok;
                        false ->
                            {error, no_register_class}
                    end;
                {error, nofile} ->
                    {error, counter_not_built}
            end
    end.
