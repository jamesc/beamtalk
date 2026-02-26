%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Wire check tests for ADR 0032 Phase 0 (BT-732).
%%%
%%% **DDD Context:** Object System
%%%
%%% Proves the core assumption of the Early Class Protocol:
%%% a class-side message not found in user-defined class methods
%%% dispatches through the 'Class' instance method chain.
%%%
%%% ## What This Tests
%%%
%%% 1. **Dispatch fallthrough**: `Counter testClassProtocol` reaches
%%%    the currently registered Class implementation module's
%%%    `dispatch/4` (in this suite, `beamtalk_class_chain_test_helper:dispatch/4`)
%%%    via `try_class_chain_fallthrough/3`.
%%%
%%% 2. **Metaclass tag preservation**: The `self` argument received by
%%%    the Class method has the 'Counter class' tag, not 'Counter'.
%%%    This proves the virtual metaclass tag survives the dispatch chain.
%%%
%%% 3. **DNU still works**: Messages not in Class chain still raise
%%%    does_not_understand.
%%%
%%% 4. **Fallback when Class absent**: When 'Class' is not registered,
%%%    does_not_understand is raised (no crash, graceful fallback).
%%%
%%% ## Phase 0 Outcome
%%%
%%% The testClassProtocol probe was provided by beamtalk_class_chain_test_helper
%%% (test-only module) to keep production code clean. The dispatch mechanism
%%% (try_class_chain_fallthrough) remains in beamtalk_class_dispatch.

-module(beamtalk_class_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    %% Register 'Class' with the test helper module so testClassProtocol is available.
    %% In production, beamtalk_class_bt has no probe methods; the test helper
    %% provides testClassProtocol only for this wire check test suite.
    register_class_with_test_helper(),
    ok.

teardown(_) ->
    %% Restore 'Class' to the production stub so other test modules in the
    %% same VM don't see the test helper module as Class's implementation.
    ClassInfo = #{
        name => 'Class',
        superclass => 'Object',
        module => beamtalk_class_bt,
        instance_variables => [],
        class_methods => #{},
        instance_methods => #{}
    },
    case beamtalk_class_registry:whereis_class('Class') of
        undefined -> ok;
        _Pid -> beamtalk_object_class:update_class('Class', ClassInfo)
    end,
    ok.

%% Register or update 'Class' to use the test helper module.
register_class_with_test_helper() ->
    ClassInfo = #{
        name => 'Class',
        superclass => 'Object',
        module => beamtalk_class_chain_test_helper,
        instance_variables => [],
        class_methods => #{
            'superclass' => #{arity => 0}
        },
        instance_methods => #{
            testClassProtocol => #{arity => 0}
        }
    },
    Result =
        case beamtalk_class_registry:whereis_class('Class') of
            undefined ->
                beamtalk_object_class:start('Class', ClassInfo);
            _Pid ->
                beamtalk_object_class:update_class('Class', ClassInfo)
        end,
    {ok, _} = Result,
    ok.

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
%% beamtalk_class_chain_test_helper:dispatch/4 handles it.
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

    %% Kill 'Class' process if it's running to test the absent case.
    %% Use monitor + DOWN to avoid flaky timer:sleep on loaded CI nodes.
    case beamtalk_class_registry:whereis_class('Class') of
        undefined ->
            ok;
        ClassPid ->
            Ref = erlang:monitor(process, ClassPid),
            exit(ClassPid, kill),
            receive
                {'DOWN', Ref, process, ClassPid, _Reason} -> ok
            after 1000 ->
                ?assert(false)
            end
    end,

    try
        %% Should raise does_not_understand, not crash
        ?assertError(
            #{error := #beamtalk_error{kind = does_not_understand, class = 'Counter'}},
            beamtalk_object_class:class_send(CounterPid, testClassProtocol, [])
        )
    after
        %% Restore Class so subsequent tests (if order changes) are not affected
        register_class_with_test_helper()
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
