%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dispatch_tests).

-moduledoc """
Unit tests for beamtalk_dispatch module.

Tests the method dispatch domain service including:
- Hierarchy walking (lookup/5)
- Super send dispatch (super/5)
- Extension method checking
- Error handling (structured #beamtalk_error{})

## Test Strategy

Uses the real compiled Counter fixture (tests/repl-protocol/fixtures/counter.bt)
and beamtalk_object_class registry for realistic testing.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Test Setup/Teardown
%%% ============================================================================

%% Setup function that starts minimal required services
setup() ->
    %% Just start the application to ensure ETS tables and registries are ready
    %% The beamtalk_runtime app should handle starting necessary supervisors
    application:ensure_all_started(beamtalk_runtime),

    %% BT-446: Ensure stdlib classes are registered (may have been killed by
    %% earlier test teardowns since class processes are unlinked)
    beamtalk_stdlib:init(),

    %% Initialize extensions registry
    beamtalk_extensions:init(),

    ok.

%% Teardown function
teardown(_) ->
    %% Minimal cleanup
    ok.

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Run all tests with setup/teardown
dispatch_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"lookup finds local method", fun test_lookup_local_method/0},
            {"lookup finds inherited method", fun test_lookup_inherited_method/0},
            {"lookup returns DNU error for missing method", fun test_lookup_missing_method/0},
            {"super skips current class", fun test_super_skips_current/0},
            {"super returns error at root", fun test_super_at_root/0},
            {"lookup checks extensions before class methods", fun test_extension_priority/0},
            {"dispatch errors are always 2-tuples", fun test_error_tuple_shape/0},
            {"super finds inherited reflection method", fun test_super_finds_inherited/0},
            {"responds_to existing method", fun test_responds_to_existing_method/0},
            {"responds_to missing method", fun test_responds_to_missing_method/0},
            {"responds_to nonexistent class", fun test_responds_to_nonexistent_class/0},
            {"responds_to inherited method", fun test_responds_to_inherited_method/0},
            {"extension error propagation", fun test_extension_error_propagation/0},
            {"responds_to extension method", fun test_responds_to_extension_method/0},
            {"BT-283: dispatch lookup performance", fun test_dispatch_lookup_performance/0},
            {"BT-283: dynamic method found after put_method",
                fun test_dynamic_method_responds_to/0},
            {"BT-283: inherited dynamic method found via chain walk",
                fun test_inherited_responds_to_dynamic_method/0},
            {"BT-387: out-of-order registration dispatches correctly",
                fun test_out_of_order_registration/0},
            {"BT-429: super finds extension on superclass",
                fun test_super_finds_extension_on_superclass/0},
            %% BT-623: Additional coverage tests
            {"lookup on non-existent class returns class_not_found",
                fun test_lookup_nonexistent_class/0},
            %% BT-1086: Uncovered path tests
            {"actor instance displayString bypasses module dispatch",
                fun test_actor_instance_displaystring/0},
            {"actor instance inspect bypasses module dispatch", fun test_actor_instance_inspect/0},
            {"depth limit exceeded returns DNU error", fun test_depth_limit_exceeded/0},
            {"module without dispatch/4 falls through to superclass",
                fun test_module_without_dispatch/0},
            {"error in dispatch/4 is wrapped as beamtalk_error",
                fun test_dispatch_throws_wrapped/0},
            {"actor instance displayString bypasses throwing module",
                fun test_actor_instance_bypass_throwing_module/0},
            {"super on non-existent class returns error", fun test_super_nonexistent_class/0},
            {"lookup returns class_not_found when parent class is gone",
                fun test_stale_parent_class/0},
            {"continue_to_superclass at root returns DNU", fun test_continue_to_superclass_root/0},
            {"lookup returns error for dead class process", fun test_dead_class_process/0},
            {"super at Object for unknown method", fun test_super_object_unknown/0},
            {"lookup with class_not_found in superclass chain",
                fun test_lookup_missing_superclass_in_chain/0},
            %% BT-1512: Extension state threading tests (previously unwired)
            {"arity-3 extension threads state correctly", fun test_extension_state_threading/0},
            {"arity-2 value-type extension via runtime dispatch",
                fun test_value_type_extension_via_runtime_dispatch/0},
            %% BT-1970: Additional coverage tests
            {"extension with bad arity raises error", fun test_extension_bad_arity/0},
            {"non-actor Self uses normal dispatch for displayString",
                fun test_non_actor_self_displaystring/0},
            {"lookup zero-arity method succeeds", fun test_lookup_zero_arity_method/0},
            {"responds_to walks full hierarchy depth", fun test_responds_to_deep_chain/0},
            {"lookup succeeds with extensions table present",
                fun test_lookup_with_extensions_table/0},
            {"super from class with no superclass returns DNU", fun test_super_no_superclass/0},
            %% BT-1981: additional dispatch-pipeline coverage
            {"continue_to_superclass when superclass is not in registry",
                fun test_continue_to_superclass_missing_registry/0},
            {"extension method that raises is re-raised", fun test_extension_raises_reraised/0},
            {"super finds and invokes extension on superclass (arity-3)",
                fun test_super_extension_invoke/0},
            {"compiled dispatch returning 3-tuple error is normalized to 2-tuple",
                fun test_compiled_dispatch_returns_error_tuple/0},
            {"invoke_method with module=undefined continues to superclass",
                fun test_invoke_method_module_undefined_continues/0}
        ]
    end}.

%%% ============================================================================
%%% Test Cases
%%% ============================================================================

%% Test that lookup/5 finds a method defined in the current class
test_lookup_local_method() ->
    %% Use the Counter class from compiled fixture
    %% Counter defines increment/0 locally

    %% First, ensure Counter is loaded and registered
    ok = ensure_counter_loaded(),

    %% Create a Counter instance state
    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    % Placeholder self
    Self = make_ref(),

    %% Dispatch increment on Counter
    Result = beamtalk_dispatch:lookup(increment, [], Self, State, 'Counter'),

    %% Should succeed (exact result depends on Counter implementation)
    ?assertMatch({reply, _Result, _NewState}, Result).

%% Test that lookup/5 walks hierarchy to find inherited method
test_lookup_inherited_method() ->
    %% Counter has 'class' method inlined in its dispatch
    %% This test actually tests that we can find a method in the current class
    %% (Real hierarchy walking is tested with LoggingCounter -> Counter)

    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% Dispatch 'class' on Counter (defined locally in Counter's dispatch)
    Result = beamtalk_dispatch:lookup(class, [], Self, State, 'Counter'),

    %% Should succeed and return 'Counter'
    ?assertMatch({reply, 'Counter', _State}, Result).

%% Test that lookup/5 returns structured error for missing method
test_lookup_missing_method() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% Dispatch a method that doesn't exist
    Result = beamtalk_dispatch:lookup(unknownMethod, [], Self, State, 'Counter'),

    %% Should return structured error
    ?assertMatch(
        {error, #beamtalk_error{
            kind = does_not_understand,
            class = 'Counter',
            selector = unknownMethod
        }},
        Result
    ).

%% Test that super/5 skips the current class and starts at superclass
test_super_skips_current() ->
    ok = ensure_counter_loaded(),

    %% Counter inherits from Actor. Call super on Counter for 'increment'.
    %% Counter defines 'increment', but super should NOT find it in Counter.
    %% It should look in Actor (no increment there), then Object, etc.
    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% Super from Counter for 'increment' should NOT find it
    %% (Actor/Object don't define increment)
    Result = beamtalk_dispatch:super(increment, [], Self, State, 'Counter'),

    %% Should return does_not_understand since increment is only in Counter
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result).

%% Test that super/5 returns error when at root with no superclass
test_super_at_root() ->
    %% Object has no superclass (or ProtoObject is root)
    %% Calling super from Object should return error

    %% We need to know the root class name
    %% For now, assume Object is close to root

    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Object'
    },

    Self = make_ref(),

    %% Call super on Object for a method that doesn't exist in superclass chain
    Result = beamtalk_dispatch:super(unknownMethod, [], Self, State, 'Object'),

    %% Should return error (no superclass or method not found)
    ?assertMatch({error, #beamtalk_error{}}, Result).

%% Test that extensions are checked before class methods
test_extension_priority() ->
    %% Register an extension method on Counter
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    TestFun = fun(_Args, _Self, State) ->
        %% BT-1512: Return {Result, NewState}
        {extension_called, State}
    end,

    ok = beamtalk_extensions:register('Counter', testExtension, TestFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% Call the extension method
    Result = beamtalk_dispatch:lookup(testExtension, [], Self, State, 'Counter'),

    %% Clean up the extension to avoid leaking between tests
    (try
        ets:delete(beamtalk_extensions, {'Counter', testExtension})
    catch
        _:_ -> ok
    end),

    %% Should invoke the extension
    ?assertMatch({reply, extension_called, _}, Result).

%% Test that all dispatch error returns are proper 2-tuples {error, #beamtalk_error{}}
%% This validates the normalization in invoke_method that converts
%% 3-tuple {error, Error, State} from dispatch/4 to 2-tuple {error, Error}
test_error_tuple_shape() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% All error paths should return exactly {error, #beamtalk_error{}} (2-tuple)
    %% Never {error, Error, State} (3-tuple)
    LookupResult = beamtalk_dispatch:lookup(noSuchMethod, [], Self, State, 'Counter'),
    ?assertMatch({error, #beamtalk_error{}}, LookupResult),
    ?assertEqual(2, tuple_size(LookupResult)),

    SuperResult = beamtalk_dispatch:super(noSuchMethod, [], Self, State, 'Counter'),
    ?assertMatch({error, #beamtalk_error{}}, SuperResult),
    ?assertEqual(2, tuple_size(SuperResult)).

%% Test that super/5 can find a method defined in a parent class
%% Counter inherits 'class' which is inlined in all compiled modules.
%% We test 'isNil' which is registered in Object's metadata but has no
%% compiled module — demonstrating the hierarchy walk terminates correctly.
test_super_finds_inherited() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% ADR 0006 Phase 1b: 'class' is no longer inlined in Counter's dispatch.
    %% Super walks from Counter → Actor → Object, finds 'class' in beamtalk_object.erl.
    Result = beamtalk_dispatch:super(class, [], Self, State, 'Counter'),

    %% Object's dispatch/4 handles 'class' by reading $beamtalk_class from State
    ?assertMatch({reply, 'Counter', _}, Result).

%%% ============================================================================
%%% Helper Functions
%%% ============================================================================

%% Ensure the Counter class is loaded and registered
ensure_counter_loaded() ->
    %% Check if Counter class is already registered
    case beamtalk_class_registry:whereis_class('Counter') of
        undefined ->
            %% Counter not registered - register it
            %% The counter module is compiled from test fixture (ADR 0016: bt@ prefix)
            case code:ensure_loaded('bt@counter') of
                {module, 'bt@counter'} ->
                    %% Call the module's register_class/0 if it exists
                    case erlang:function_exported('bt@counter', register_class, 0) of
                        true ->
                            'bt@counter':register_class(),
                            ok;
                        false ->
                            error(counter_no_register_function)
                    end;
                {error, Reason} ->
                    error({counter_module_not_found, Reason})
            end;
        _Pid ->
            %% Already registered
            ok
    end.

%%% ============================================================================
%%% BT-344: Additional Test Cases
%%% ============================================================================

%% Test responds_to for a method that exists
test_responds_to_existing_method() ->
    ok = ensure_counter_loaded(),
    ?assert(beamtalk_dispatch:responds_to(increment, 'Counter')).

%% Test responds_to for a method that doesn't exist
test_responds_to_missing_method() ->
    ok = ensure_counter_loaded(),
    ?assertNot(beamtalk_dispatch:responds_to(totallyFakeMethod, 'Counter')).

%% Test responds_to for nonexistent class
test_responds_to_nonexistent_class() ->
    ?assertNot(beamtalk_dispatch:responds_to(anything, 'ClassThatDoesNotExist')).

%% Test responds_to walks hierarchy (Counter inherits from Actor which inherits from Object)
test_responds_to_inherited_method() ->
    ok = ensure_counter_loaded(),
    %% 'class' is defined in Object (inherited through Actor -> Counter)
    ?assert(beamtalk_dispatch:responds_to(class, 'Counter')).

%% Test extension method error propagation
test_extension_error_propagation() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    %% Register an extension that throws
    CrashFun = fun(_Args, _Self, _State) ->
        error(extension_test_crash)
    end,

    ok = beamtalk_extensions:register('Counter', crashExt, CrashFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },
    Self = make_ref(),

    try
        %% Extension error should propagate
        ?assertError(
            extension_test_crash,
            beamtalk_dispatch:lookup(crashExt, [], Self, State, 'Counter')
        )
    after
        (try
            ets:delete(beamtalk_extensions, {'Counter', crashExt})
        catch
            _:_ -> ok
        end)
    end.

%% Test responds_to finds extension methods
test_responds_to_extension_method() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    TestFun = fun(_Args, _Self, State0) -> {ok, State0} end,
    ok = beamtalk_extensions:register('Counter', extTestMethod, TestFun, test_owner),

    try
        ?assert(beamtalk_dispatch:responds_to(extTestMethod, 'Counter'))
    after
        (try
            ets:delete(beamtalk_extensions, {'Counter', extTestMethod})
        catch
            _:_ -> ok
        end)
    end.

%%% ============================================================================
%%% BT-283: Performance and Correctness Tests (ADR 0032 Phase 1)
%%% ============================================================================

%% Benchmark: chain walk dispatch for inherited methods
%% ADR 0032 Phase 1: flattened table removed; direct hierarchy walk used instead.
test_dispatch_lookup_performance() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },
    Self = make_ref(),

    %% Warm up
    _ = beamtalk_dispatch:lookup(increment, [], Self, State, 'Counter'),

    %% Benchmark chain walk for an inherited method.
    %% 'class' is defined in Object, so Counter -> Actor -> Object (depth 3).
    N = 10000,

    Start = erlang:monotonic_time(microsecond),
    lists:foreach(
        fun(_) ->
            _ = beamtalk_dispatch:lookup(class, [], Self, State, 'Counter')
        end,
        lists:seq(1, N)
    ),
    End = erlang:monotonic_time(microsecond),

    AvgTime = (End - Start) / N,
    ?LOG_NOTICE("Chain walk: ~p lookups in ~p μs (avg ~.2f μs/call)", [N, End - Start, AvgTime]),

    %% Chain walk at depth 3 should complete well within 500 μs/call on any CI machine
    ?assert(AvgTime < 500.0).

%% Test that a dynamically added method is immediately discoverable via responds_to.
%% ADR 0032 Phase 1: no flattened table invalidation needed — chain walk is always current.
test_dynamic_method_responds_to() ->
    ok = ensure_counter_loaded(),

    CounterPid = beamtalk_class_registry:whereis_class('Counter'),

    %% Add a new dynamic method
    TestMethod = fun(_Self, [], State) -> {reply, test_result, State} end,
    ok = beamtalk_object_class:put_method(CounterPid, testNewDynMethod, TestMethod),

    %% Method is immediately discoverable — chain walk reads current instance_methods
    ?assert(beamtalk_dispatch:responds_to(testNewDynMethod, 'Counter')).

%% Test that a method added to a parent class is immediately visible in subclass responds_to.
%% ADR 0032 Phase 1: chain walk traverses live gen_server state; no broadcast needed.
test_inherited_responds_to_dynamic_method() ->
    ok = ensure_counter_loaded(),

    ActorPid = beamtalk_class_registry:whereis_class('Actor'),
    ?assertNotEqual(undefined, ActorPid),

    %% Counter shouldn't have this method initially
    ?assertNot(beamtalk_dispatch:responds_to(inheritedDynTestMethod, 'Counter')),

    %% Add to Actor (parent of Counter)
    ParentMethod = fun(_Self, [], State) -> {reply, parent_result, State} end,
    ok = beamtalk_object_class:put_method(ActorPid, inheritedDynTestMethod, ParentMethod),

    %% Chain walk finds it immediately in Actor — no async rebuild required
    ?assert(beamtalk_dispatch:responds_to(inheritedDynTestMethod, 'Counter')).

%% Test that registering a parent class AFTER a child class works correctly.
%% ADR 0032 Phase 1: chain walk reads the registry at dispatch time, so
%% out-of-order registration is resolved automatically without rebuild broadcasts.
test_out_of_order_registration() ->
    %% Register child class with non-existent parent
    ChildMethod = fun(_Self, [], State) -> {reply, child_result, State} end,
    {ok, ChildPid} = beamtalk_object_class:start_link('TestChild', #{
        superclass => 'TestParent',
        instance_methods => #{childMethod => #{block => ChildMethod, arity => 0}},
        instance_variables => []
    }),

    %% Before parent is registered — only local method visible
    ?assert(beamtalk_dispatch:responds_to(childMethod, 'TestChild')),
    ?assertNot(beamtalk_dispatch:responds_to(parentMethod, 'TestChild')),

    %% Register parent class — chain walk will find it immediately
    ParentMethod = fun(_Self, [], State) -> {reply, parent_result, State} end,
    {ok, ParentPid} = beamtalk_object_class:start_link('TestParent', #{
        superclass => none,
        instance_methods => #{parentMethod => #{block => ParentMethod, arity => 0}},
        instance_variables => []
    }),

    %% No barrier or sleep needed — chain walk reads registry at call time
    ?assert(beamtalk_dispatch:responds_to(childMethod, 'TestChild')),
    ?assert(beamtalk_dispatch:responds_to(parentMethod, 'TestChild')),

    %% Clean up
    gen_server:stop(ChildPid),
    gen_server:stop(ParentPid).

%% BT-429: Test that super/5 finds extension methods registered on a superclass.
%% Register extension on Actor class, verify it's found via super from Counter.
test_super_finds_extension_on_superclass() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    %% Register an extension method on Actor (Counter's superclass)
    TestFun = fun(_Args, _Self, State0) ->
        %% BT-1512: Return {Result, NewState}
        {super_extension_called, State0}
    end,

    ok = beamtalk_extensions:register('Actor', superExtTest, TestFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Self = make_ref(),

    %% Call super from Counter — should find the extension on Actor
    try
        Result = beamtalk_dispatch:super(superExtTest, [], Self, State, 'Counter'),

        %% Should invoke the extension registered on Actor
        ?assertMatch({reply, super_extension_called, _}, Result)
    after
        %% Clean up extension (ignore any error if it's already gone)
        (try
            ets:delete(beamtalk_extensions, {'Actor', superExtTest})
        catch
            _:_ -> ok
        end)
    end.

%%% ============================================================================
%%% BT-1512: Extension method state threading tests
%%% ============================================================================

%% Test: arity-3 (actor) extension threads state correctly via runtime dispatch
test_extension_state_threading() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    %% Register an arity-3 extension that mutates state
    TestFun = fun(_Args, _Self, State) ->
        OldVal = maps:get('value', State, 0),
        {OldVal + 100, maps:put('value', OldVal + 100, State)}
    end,

    ok = beamtalk_extensions:register('Counter', addHundred, TestFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 42
    },
    Self = make_ref(),

    try
        Result = beamtalk_dispatch:lookup(addHundred, [], Self, State, 'Counter'),
        %% State should be threaded: value 42 + 100 = 142
        ?assertMatch({reply, 142, #{'value' := 142}}, Result)
    after
        (try
            ets:delete(beamtalk_extensions, {'Counter', addHundred})
        catch
            _:_ -> ok
        end)
    end.

%% Test: arity-2 (value-type) extension works via runtime dispatch path
test_value_type_extension_via_runtime_dispatch() ->
    ok = beamtalk_extensions:init(),

    %% Register an arity-2 extension (value-type style)
    TestFun = fun(_Args, Self) ->
        %% Self is the receiver; just return a derived value
        {value_ext_called, Self}
    end,

    ok = beamtalk_extensions:register('Integer', testValExt, TestFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Integer'
    },
    Self = 42,

    try
        Result = beamtalk_dispatch:lookup(testValExt, [], Self, State, 'Integer'),
        %% Arity-2 path: original State is preserved
        ?assertMatch({reply, {value_ext_called, 42}, _}, Result)
    after
        (try
            ets:delete(beamtalk_extensions, {'Integer', testValExt})
        catch
            _:_ -> ok
        end)
    end.

%%% ============================================================================
%%% BT-623: Additional Coverage Tests
%%% ============================================================================

%% Test lookup on a completely non-existent class
test_lookup_nonexistent_class() ->
    State = #{'$beamtalk_class' => 'NoSuchClass'},
    Self = make_ref(),
    Result = beamtalk_dispatch:lookup(anyMethod, [], Self, State, 'NoSuchClass'),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found, class = 'NoSuchClass'}}, Result).

%% Test super on a non-existent class
test_super_nonexistent_class() ->
    State = #{'$beamtalk_class' => 'NoSuchClass'},
    Self = make_ref(),
    Result = beamtalk_dispatch:super(anyMethod, [], Self, State, 'NoSuchClass'),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result).

%% Test that when the parent class process is killed, lookup returns class_not_found.
%% Chain walk tries to reach the parent via registry; when it's gone, class_not_found.
test_stale_parent_class() ->
    ParentMethod = fun(_Self, [], State) -> {reply, parent_ok, State} end,
    {ok, ParentPid} = beamtalk_object_class:start_link('StaleTestParent', #{
        superclass => none,
        instance_methods => #{staleMethod => #{block => ParentMethod, arity => 0}},
        instance_variables => []
    }),

    ChildMethod = fun(_Self, [], State) -> {reply, child_ok, State} end,
    {ok, ChildPid} = beamtalk_object_class:start_link('StaleTestChild', #{
        superclass => 'StaleTestParent',
        instance_methods => #{childOnly => #{block => ChildMethod, arity => 0}},
        instance_variables => []
    }),

    %% Verify parent method is reachable before killing parent
    ?assert(beamtalk_dispatch:responds_to(staleMethod, 'StaleTestChild')),

    %% Kill the parent class process (simulates hot reload edge case)
    gen_server:stop(ParentPid),

    %% Chain walk tries to reach StaleTestParent, gets class_not_found
    State = #{'$beamtalk_class' => 'StaleTestChild'},
    Self = make_ref(),
    Result = beamtalk_dispatch:lookup(staleMethod, [], Self, State, 'StaleTestChild'),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result),

    gen_server:stop(ChildPid).

%% Test continue_to_superclass when class has no module and no superclass (root)
test_continue_to_superclass_root() ->
    %% Create a root class with no module and no superclass
    {ok, RootPid} = beamtalk_object_class:start_link('RootOnlyClass', #{
        superclass => none,
        instance_methods => #{},
        instance_variables => []
    }),

    State = #{'$beamtalk_class' => 'RootOnlyClass'},
    Self = make_ref(),
    Result = beamtalk_dispatch:lookup(anyMethod, [], Self, State, 'RootOnlyClass'),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),

    gen_server:stop(RootPid).

%% Test that lookup on a dead class process returns class_not_found (not a crash).
test_dead_class_process() ->
    ok = ensure_counter_loaded(),

    %% Create a temporary class, get its pid, then kill it
    TmpMethod = fun(_Self, [], State) -> {reply, ok, State} end,
    {ok, TmpPid} = beamtalk_object_class:start_link('TmpDeadClass', #{
        superclass => none,
        instance_methods => #{tmpMethod => #{block => TmpMethod, arity => 0}},
        instance_variables => []
    }),
    gen_server:stop(TmpPid),

    %% Lookup on dead class should not crash — returns class_not_found error
    State = #{'$beamtalk_class' => 'TmpDeadClass'},
    Self = make_ref(),
    Result = beamtalk_dispatch:lookup(tmpMethod, [], Self, State, 'TmpDeadClass'),
    ?assertMatch({error, #beamtalk_error{}}, Result).

%% Test super from Object for method that doesn't exist anywhere
test_super_object_unknown() ->
    ok = ensure_counter_loaded(),
    State = #{'$beamtalk_class' => 'Object'},
    Self = make_ref(),
    Result = beamtalk_dispatch:super(completelyUnknownMethod, [], Self, State, 'Object'),
    ?assertMatch({error, #beamtalk_error{}}, Result).

%% Test lookup when superclass in chain is not registered
test_lookup_missing_superclass_in_chain() ->
    %% Create a class whose superclass doesn't exist
    ChildMethod = fun(_Self, [], State) -> {reply, ok, State} end,
    {ok, ChildPid} = beamtalk_object_class:start_link('OrphanChild', #{
        superclass => 'GhostParent',
        instance_methods => #{localMethod => #{block => ChildMethod, arity => 0}},
        instance_variables => []
    }),

    State = #{'$beamtalk_class' => 'OrphanChild'},
    Self = make_ref(),
    %% Looking up a method not in OrphanChild should fail with class_not_found
    %% when it tries to walk to the non-existent superclass
    Result = beamtalk_dispatch:lookup(nonExistentMethod, [], Self, State, 'OrphanChild'),
    ?assertMatch({error, #beamtalk_error{}}, Result),

    gen_server:stop(ChildPid).

%%% ============================================================================
%%% BT-1086: Uncovered path tests
%%% ============================================================================

%% Test that displayString sent to an actor instance bypasses the compiled
%% module dispatch and uses beamtalk_object_ops instead (avoids gen_server deadlock).
test_actor_instance_displaystring() ->
    ok = ensure_counter_loaded(),

    %% Create a fake actor instance: a #beamtalk_object{} with a real pid.
    %% The pid is just self() — displayString via beamtalk_object_ops doesn't
    %% send any message to the pid, so it won't deadlock or crash.
    ActorSelf = #beamtalk_object{
        class = 'Counter',
        class_mod = 'bt@counter',
        pid = self()
    },
    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 42
    },

    %% displayString on an actor instance goes through beamtalk_object_ops (not Counter's dispatch)
    Result = beamtalk_dispatch:lookup('displayString', [], ActorSelf, State, 'Counter'),

    ?assertMatch({reply, _, _}, Result).

%% Test that inspect sent to an actor instance similarly bypasses module dispatch.
test_actor_instance_inspect() ->
    ok = ensure_counter_loaded(),

    ActorSelf = #beamtalk_object{
        class = 'Counter',
        class_mod = 'bt@counter',
        pid = self()
    },
    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },

    Result = beamtalk_dispatch:lookup(inspect, [], ActorSelf, State, 'Counter'),

    ?assertMatch({reply, _, _}, Result).

%% Test that exceeding MAX_HIERARCHY_DEPTH (20) returns a DNU error with a message
%% about depth limit rather than crashing the process.
test_depth_limit_exceeded() ->
    %% Build a chain of 22 classes (deeper than MAX_HIERARCHY_DEPTH = 20).
    %% Each class has no methods and points to the next as superclass.
    NumClasses = 22,
    % elp:fixme W0023 intentional atom creation
    ClassNames = [
        list_to_atom("DepthTestClass" ++ integer_to_list(I))
     || I <- lists:seq(1, NumClasses)
    ],

    %% Start each class; class N+1 is the superclass of class N
    Pids = lists:foldl(
        fun(I, AccPids) ->
            ClassName = lists:nth(I, ClassNames),
            Super =
                if
                    I =:= NumClasses -> none;
                    true -> lists:nth(I + 1, ClassNames)
                end,
            {ok, Pid} = beamtalk_object_class:start_link(ClassName, #{
                superclass => Super,
                instance_methods => #{},
                instance_variables => []
            }),
            AccPids ++ [Pid]
        end,
        [],
        lists:seq(1, NumClasses)
    ),

    try
        %% Lookup a method that doesn't exist in any class — should hit depth limit
        LeafClass = hd(ClassNames),
        State = #{'$beamtalk_class' => LeafClass},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(noSuchMethod, [], Self, State, LeafClass),

        %% Depth exceeded → does_not_understand (depth guard fires before "not found at root")
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result)
    after
        lists:foreach(fun gen_server:stop/1, Pids)
    end.

%% Test that when a class's module exists but does not export dispatch/4,
%% the lookup falls through to the superclass chain (continue_to_superclass path).
test_module_without_dispatch() ->
    %% Compile a stub module that does NOT export dispatch/4
    Forms = [
        {attribute, 1, module, bt_test_no_dispatch_stub},
        {attribute, 2, export, [{hello, 0}]},
        {function, 3, hello, 0, [{clause, 3, [], [], [{atom, 3, world}]}]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_no_dispatch_stub.beam", Bin),

    %% Register a class with this stub module.
    %% The class has stubMethod in instance_methods (has_method returns true),
    %% so invoke_method is called. The stub module lacks dispatch/4 →
    %% continue_to_superclass → no superclass → DNU.
    {ok, Pid} = beamtalk_object_class:start_link('NoDispatchStubClass', #{
        module => bt_test_no_dispatch_stub,
        superclass => none,
        instance_methods => #{
            stubMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),

    try
        State = #{'$beamtalk_class' => 'NoDispatchStubClass'},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(stubMethod, [], Self, State, 'NoDispatchStubClass'),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result)
    after
        gen_server:stop(Pid),
        code:purge(bt_test_no_dispatch_stub),
        code:delete(bt_test_no_dispatch_stub)
    end.

%% Test that an Erlang exception thrown inside dispatch/4 is caught and
%% wrapped as a structured #beamtalk_error{} (not an unhandled crash).
test_dispatch_throws_wrapped() ->
    %% Compile a module whose dispatch/4 always throws
    Forms = [
        {attribute, 1, module, bt_test_dispatch_throws_stub},
        {attribute, 2, export, [{dispatch, 4}]},
        {function, 3, dispatch, 4, [
            {clause, 3,
                [{var, 3, '_Sel'}, {var, 3, '_Args'}, {var, 3, '_Self'}, {var, 3, '_State'}], [], [
                    {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                        {atom, 3, test_dispatch_crash}
                    ]}
                ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_dispatch_throws_stub.beam", Bin),

    %% Register a class with the crashing module
    {ok, Pid} = beamtalk_object_class:start_link('ThrowingDispatchClass', #{
        module => bt_test_dispatch_throws_stub,
        superclass => none,
        instance_methods => #{
            throwMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),

    try
        State = #{'$beamtalk_class' => 'ThrowingDispatchClass'},
        Self = make_ref(),

        %% dispatch/4 throws → caught → wrapped as #beamtalk_error{}
        Result = beamtalk_dispatch:lookup(throwMethod, [], Self, State, 'ThrowingDispatchClass'),
        ?assertMatch({error, #beamtalk_error{}}, Result)
    after
        gen_server:stop(Pid),
        code:purge(bt_test_dispatch_throws_stub),
        code:delete(bt_test_dispatch_throws_stub)
    end.

%% Test that the actor instance bypass correctly intercepts displayString before
%% reaching the module's dispatch/4. The stub module throws, but the actor bypass
%% routes to beamtalk_object_ops instead, so the result is {reply, _, _}.
test_actor_instance_bypass_throwing_module() ->
    ok = ensure_counter_loaded(),

    %% Compile a stub dispatch module whose dispatch/4 always throws.
    %% The actor bypass should prevent this from ever being called.
    Forms = [
        {attribute, 1, module, bt_test_actor_dispatch_throws_stub},
        {attribute, 2, export, [{dispatch, 4}]},
        {function, 3, dispatch, 4, [
            {clause, 3,
                [{var, 3, '_Sel'}, {var, 3, '_Args'}, {var, 3, '_Self'}, {var, 3, '_State'}], [], [
                    {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                        {atom, 3, actor_dispatch_crash}
                    ]}
                ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_actor_dispatch_throws_stub.beam", Bin),

    {ok, Pid} = beamtalk_object_class:start_link('ActorDispatchThrowsClass', #{
        module => bt_test_actor_dispatch_throws_stub,
        superclass => none,
        instance_methods => #{
            'displayString' => #{
                block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0
            }
        },
        instance_variables => []
    }),

    try
        %% Actor instance triggers the beamtalk_object_ops path, not the stub module.
        ActorSelf = #beamtalk_object{
            class = 'ActorDispatchThrowsClass',
            class_mod = bt_test_actor_dispatch_throws_stub,
            pid = self()
        },
        State = #{'$beamtalk_class' => 'ActorDispatchThrowsClass'},

        %% displayString is intercepted before calling the module's dispatch/4,
        %% so beamtalk_object_ops handles it safely and returns {reply, _, _}
        Result = beamtalk_dispatch:lookup(
            'displayString', [], ActorSelf, State, 'ActorDispatchThrowsClass'
        ),
        ?assertMatch({reply, _, _}, Result)
    after
        gen_server:stop(Pid),
        code:purge(bt_test_actor_dispatch_throws_stub),
        code:delete(bt_test_actor_dispatch_throws_stub)
    end.

%%% ============================================================================
%%% BT-1970: Additional coverage tests
%%% ============================================================================

%% Test: extension with bad arity raises {bad_extension_arity, N}
test_extension_bad_arity() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    %% Register an extension with arity 1 (neither 2 nor 3)
    BadFun = fun(_OnlyOneArg) -> bad end,
    ok = beamtalk_extensions:register('Counter', badArityExt, BadFun, test_owner),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },
    Self = make_ref(),

    try
        ?assertError(
            {bad_extension_arity, 1},
            beamtalk_dispatch:lookup(badArityExt, [], Self, State, 'Counter')
        )
    after
        (try
            ets:delete(beamtalk_extensions, {'Counter', badArityExt})
        catch
            _:_ -> ok
        end)
    end.

%% Test: is_actor_instance returns false for non-actor values.
%% When Self is not a #beamtalk_object{}, displayString goes through normal dispatch.
test_non_actor_self_displaystring() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },
    Self = make_ref(),

    %% Non-actor Self: displayString goes through compiled Counter dispatch,
    %% not the beamtalk_object_ops bypass path.
    Result = beamtalk_dispatch:lookup('displayString', [], Self, State, 'Counter'),
    ?assertMatch({reply, _, _}, Result).

%% Test: lookup with zero-arity method succeeds
test_lookup_zero_arity_method() ->
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 5
    },
    Self = make_ref(),

    Result = beamtalk_dispatch:lookup('getValue', [], Self, State, 'Counter'),
    ?assertMatch({reply, _, _}, Result).

%% Test: responds_to walks full hierarchy depth correctly
test_responds_to_deep_chain() ->
    ok = ensure_counter_loaded(),

    %% Counter -> Actor -> Object. 'class' is in Object.
    ?assert(beamtalk_dispatch:responds_to(class, 'Counter')),

    %% Counter defines increment locally
    ?assert(beamtalk_dispatch:responds_to(increment, 'Counter')),

    %% A method that doesn't exist anywhere
    ?assertNot(beamtalk_dispatch:responds_to(totallyFakeDeepMethod, 'Counter')).

%% Test: lookup succeeds when extensions ETS table is present but has no match.
%% Exercises the check_extension -> not_found -> lookup_in_class_chain path.
test_lookup_with_extensions_table() ->
    ok = beamtalk_extensions:init(),
    ok = ensure_counter_loaded(),

    State = #{
        '$beamtalk_class' => 'Counter',
        'value' => 0
    },
    Self = make_ref(),

    %% increment is a local Counter method, should be found regardless of extensions
    Result = beamtalk_dispatch:lookup(increment, [], Self, State, 'Counter'),
    ?assertMatch({reply, _, _}, Result).

%% BT-1981: continue_to_superclass path when the superclass is registered
%% in metadata but its class process is not in the registry. The walk
%% should surface class_not_found rather than crash.
test_continue_to_superclass_missing_registry() ->
    %% Leaf class has a module without dispatch/4, superclass points to a
    %% name that is NOT registered. invoke_method → continue_to_superclass
    %% → whereis_class/1 returns undefined → class_not_found error.
    Forms = [
        {attribute, 1, module, bt_test_nodisp_leaf},
        {attribute, 2, export, [{hello, 0}]},
        {function, 3, hello, 0, [{clause, 3, [], [], [{atom, 3, world}]}]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_nodisp_leaf.beam", Bin),

    {ok, Pid} = beamtalk_object_class:start_link('NoDispLeafClass', #{
        module => bt_test_nodisp_leaf,
        superclass => 'UnregisteredParentClass',
        instance_methods => #{
            stubMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),
    try
        State = #{'$beamtalk_class' => 'NoDispLeafClass'},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(stubMethod, [], Self, State, 'NoDispLeafClass'),
        ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result)
    after
        gen_server:stop(Pid),
        code:purge(bt_test_nodisp_leaf),
        code:delete(bt_test_nodisp_leaf)
    end.

%% BT-1981: extension method that raises is re-raised after logging.
test_extension_raises_reraised() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    ThrowingFun = fun(_Args, _Self, _State) ->
        error(extension_crash)
    end,
    ok = beamtalk_extensions:register('Counter', throwingExt, ThrowingFun, test_owner),

    State = #{'$beamtalk_class' => 'Counter', 'value' => 0},
    Self = make_ref(),
    try
        ?assertError(
            extension_crash,
            beamtalk_dispatch:lookup(throwingExt, [], Self, State, 'Counter')
        )
    after
        (try
            ets:delete(beamtalk_extensions, {'Counter', throwingExt})
        catch
            _:_ -> ok
        end)
    end.

%% BT-1981: invoke_method with module_name=undefined continues to superclass.
%% Covers the 'undefined' branch in invoke_method which routes to
%% continue_to_superclass (line 320), and its recursive lookup (line 418)
%% when the superclass has the method.
test_invoke_method_module_undefined_continues() ->
    %% Parent class has a real module with dispatch/4 for the selector.
    %% Child class explicitly has module=undefined AND the method in its
    %% metadata → invoke_method → module_name=undefined → continue_to_superclass
    %% → find parent → invoke parent dispatch.
    ParentModule = bt_test_parent_dispatch,
    Forms = [
        {attribute, 1, module, ParentModule},
        {attribute, 2, export, [{dispatch, 4}, {has_method, 1}]},
        {function, 3, dispatch, 4, [
            {clause, 3, [{var, 3, '_Sel'}, {var, 3, '_Args'}, {var, 3, '_Self'}, {var, 3, 'State'}],
                [], [
                    {tuple, 3, [{atom, 3, reply}, {atom, 3, parent_result}, {var, 3, 'State'}]}
                ]}
        ]},
        {function, 4, has_method, 1, [
            {clause, 4, [{var, 4, '_'}], [], [{atom, 4, true}]}
        ]}
    ],
    {ok, ParentModule, Bin} = compile:forms(Forms),
    {module, ParentModule} = code:load_binary(ParentModule, "bt_test_parent_dispatch.beam", Bin),

    ParentName = 'BT1981ModUndefParent',
    ChildName = 'BT1981ModUndefChild',

    {ok, ParentPid} = beamtalk_object_class:start_link(ParentName, #{
        module => ParentModule,
        superclass => none,
        instance_methods => #{
            sharedMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),
    {ok, ChildPid} = beamtalk_object_class:start_link(ChildName, #{
        module => undefined,
        superclass => ParentName,
        instance_methods => #{
            sharedMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),
    try
        State = #{'$beamtalk_class' => ChildName},
        Self = make_ref(),
        %% Lookup: child has sharedMethod → invoke_method → module_name=undefined
        %%         → continue_to_superclass → find Parent → invoke parent's dispatch.
        Result = beamtalk_dispatch:lookup(sharedMethod, [], Self, State, ChildName),
        ?assertMatch({reply, parent_result, _}, Result)
    after
        (try
            gen_server:stop(ChildPid)
        catch
            _:_ -> ok
        end),
        (try
            gen_server:stop(ParentPid)
        catch
            _:_ -> ok
        end),
        code:purge(ParentModule),
        code:delete(ParentModule)
    end.

%% BT-1981: compiled dispatch/4 returning a 3-tuple error is normalized to
%% a 2-tuple {error, Error} via the 'Other/normalize' path of invoke_method.
test_compiled_dispatch_returns_error_tuple() ->
    Forms = [
        {attribute, 1, module, bt_test_dispatch_err_stub},
        {attribute, 2, export, [{dispatch, 4}]},
        {function, 3, dispatch, 4, [
            {clause, 3, [{var, 3, '_Sel'}, {var, 3, '_Args'}, {var, 3, '_Self'}, {var, 3, 'State'}],
                [], [
                    %% Return {error, {beamtalk_error, does_not_understand, fake, sel, <<>>, <<>>, #{}}, State}
                    {tuple, 3, [
                        {atom, 3, error},
                        {tuple, 3, [
                            {atom, 3, beamtalk_error},
                            {atom, 3, does_not_understand},
                            {atom, 3, fake_class},
                            {atom, 3, fake_sel},
                            {bin, 3, []},
                            {bin, 3, []},
                            {map, 3, []}
                        ]},
                        {var, 3, 'State'}
                    ]}
                ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, "bt_test_dispatch_err_stub.beam", Bin),

    {ok, Pid} = beamtalk_object_class:start_link('DispatchErrTupleClass', #{
        module => bt_test_dispatch_err_stub,
        superclass => none,
        instance_methods => #{
            errMethod => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),
    try
        State = #{'$beamtalk_class' => 'DispatchErrTupleClass'},
        Self = make_ref(),
        Result = beamtalk_dispatch:lookup(errMethod, [], Self, State, 'DispatchErrTupleClass'),
        %% Normalized to 2-tuple
        ?assertMatch({error, #beamtalk_error{}}, Result),
        ?assertEqual(2, tuple_size(Result))
    after
        gen_server:stop(Pid),
        code:purge(bt_test_dispatch_err_stub),
        code:delete(bt_test_dispatch_err_stub)
    end.

%% BT-1981: super finds extension on superclass (covers super -> extension -> invoke).
test_super_extension_invoke() ->
    ok = ensure_counter_loaded(),
    ok = beamtalk_extensions:init(),

    TestFun = fun(_Args, _Self, State) -> {from_super_ext, State} end,
    %% Register on Actor (Counter's superclass).
    ok = beamtalk_extensions:register('Actor', superExtMethod, TestFun, test_owner),

    State = #{'$beamtalk_class' => 'Counter', 'value' => 0},
    Self = make_ref(),
    try
        Result = beamtalk_dispatch:super(superExtMethod, [], Self, State, 'Counter'),
        ?assertMatch({reply, from_super_ext, _}, Result)
    after
        (try
            ets:delete(beamtalk_extensions, {'Actor', superExtMethod})
        catch
            _:_ -> ok
        end)
    end.

%% Test: super from a class with no superclass returns DNU (not crash)
test_super_no_superclass() ->
    {ok, Pid} = beamtalk_object_class:start_link('RootTestClass', #{
        superclass => none,
        instance_methods => #{
            localOnly => #{block => fun(_S, [], St) -> {reply, ok, St} end, arity => 0}
        },
        instance_variables => []
    }),

    State = #{'$beamtalk_class' => 'RootTestClass'},
    Self = make_ref(),

    try
        Result = beamtalk_dispatch:super(localOnly, [], Self, State, 'RootTestClass'),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result)
    after
        gen_server:stop(Pid)
    end.
