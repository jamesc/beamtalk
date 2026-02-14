%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_actor module
%%%
%%% Tests all actor behaviors:
%%% - Basic message dispatch (sync and async)
%%% - doesNotUnderstand fallback
%%% - Multiple actors communicating
%%% - Code hot reload (code_change)
%%% - Actor spawn/initialization
%%% - Error cases

-module(beamtalk_actor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% Tests

%%% Initialization tests

init_with_valid_state_test() ->
    State = #{
        '$beamtalk_class' => 'TestActor',
        '__methods__' => #{test => fun(_Args, S) -> {reply, ok, S} end},
        data => 123
    },
    ?assertEqual({ok, State}, beamtalk_actor:init(State)).

init_without_class_test() ->
    State = #{
        '__methods__' => #{test => fun(_Args, S) -> {reply, ok, S} end}
    },
    ?assertEqual({stop, {missing_key, '$beamtalk_class'}}, beamtalk_actor:init(State)).

init_with_non_atom_class_test() ->
    State = #{
        '$beamtalk_class' => "StringNotAtom",
        '__methods__' => #{test => fun(_Args, S) -> {reply, ok, S} end}
    },
    ?assertEqual({stop, {invalid_value, '$beamtalk_class'}}, beamtalk_actor:init(State)).

init_without_methods_test() ->
    State = #{
        '$beamtalk_class' => 'TestActor'
    },
    ?assertEqual({stop, {missing_key, '__methods__'}}, beamtalk_actor:init(State)).

init_with_non_map_test() ->
    ?assertEqual({stop, {invalid_state, not_a_map}}, beamtalk_actor:init("not a map")),
    ?assertEqual({stop, {invalid_state, not_a_map}}, beamtalk_actor:init([1, 2, 3])),
    ?assertEqual({stop, {invalid_state, not_a_map}}, beamtalk_actor:init(123)).

%%% Sync message dispatch tests

sync_message_with_reply_test() ->
    {ok, Counter} = test_counter:start_link(10),
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(10, Result),
    gen_server:stop(Counter).

sync_message_modify_state_test() ->
    {ok, Counter} = test_counter:start_link(5),
    ok = gen_server:call(Counter, {'setValue:', [20]}),
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(20, Result),
    gen_server:stop(Counter).

sync_message_unknown_selector_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Result = gen_server:call(Counter, {unknownMethod, []}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand, selector = unknownMethod}}, Result),
    gen_server:stop(Counter).

malformed_call_message_test() ->
    {ok, Counter} = test_counter:start_link(0),
    %% Send a call message that doesn't match the expected {Selector, Args} format
    Result = gen_server:call(Counter, malformed_message),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
    gen_server:stop(Counter).

%%% Async message dispatch tests

async_message_with_future_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    %% Send async increment message
    Future = beamtalk_future:new(),
    gen_server:cast(Counter, {increment, [], Future}),
    
    %% Wait for future to resolve
    ?assertEqual(nil, beamtalk_future:await(Future)),
    
    %% Check that state was updated
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(1, Result),
    
    gen_server:stop(Counter).

async_multiple_messages_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    %% Send 5 increment messages
    Futures = [begin
        F = beamtalk_future:new(),
        gen_server:cast(Counter, {increment, [], F}),
        F
    end || _ <- lists:seq(1, 5)],
    
    %% Wait for all to complete
    [beamtalk_future:await(F) || F <- Futures],
    
    %% Check final value
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(5, Result),
    
    gen_server:stop(Counter).

async_message_with_reply_test() ->
    {ok, Counter} = test_counter:start_link(42),
    
    %% Async call to getValue should resolve with the value
    Future = beamtalk_future:new(),
    gen_server:cast(Counter, {getValue, [], Future}),
    
    Result = beamtalk_future:await(Future),
    ?assertEqual(42, Result),
    
    gen_server:stop(Counter).

malformed_cast_message_test() ->
    {ok, Counter} = test_counter:start_link(0),
    %% Send a cast message that doesn't match the expected format
    %% This should be silently ignored (logged but not crash)
    gen_server:cast(Counter, malformed_cast),
    
    %% Verify actor still works
    timer:sleep(10),  % Give it time to process the malformed message
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(0, Result),
    
    gen_server:stop(Counter).

%%% doesNotUnderstand tests

dnu_forward_to_target_test() ->
    {ok, Counter} = test_counter:start_link(10),
    {ok, Proxy} = test_proxy:start_link(Counter),
    
    %% Call getValue on proxy - should forward to counter
    Result = gen_server:call(Proxy, {getValue, []}),
    ?assertEqual(10, Result),
    
    gen_server:stop(Proxy),
    gen_server:stop(Counter).

dnu_no_target_test() ->
    {ok, Proxy} = test_proxy:start_link(nil),
    
    %% Call unknown method with no target
    Result = gen_server:call(Proxy, {someMethod, []}),
    ?assertMatch({error, no_target}, Result),
    
    gen_server:stop(Proxy).

dnu_change_target_test() ->
    {ok, Counter1} = test_counter:start_link(10),
    {ok, Counter2} = test_counter:start_link(20),
    {ok, Proxy} = test_proxy:start_link(Counter1),
    
    %% First call should return 10
    Result1 = gen_server:call(Proxy, {getValue, []}),
    ?assertEqual(10, Result1),
    
    %% Change target
    ok = gen_server:call(Proxy, {setTarget, [Counter2]}),
    
    %% Second call should return 20
    Result2 = gen_server:call(Proxy, {getValue, []}),
    ?assertEqual(20, Result2),
    
    gen_server:stop(Proxy),
    gen_server:stop(Counter1),
    gen_server:stop(Counter2).

dnu_handler_throws_test() ->
    {ok, Actor} = test_throwing_dnu_actor:start_link(),
    
    %% Call unknown method - DNU handler will throw
    Result = gen_server:call(Actor, {unknownMethod, [arg1]}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = 'doesNotUnderstand:args:'}}, Result),
    
    gen_server:stop(Actor).

%%% Multiple actors communicating

multiple_actors_test() ->
    {ok, C1} = test_counter:start_link(1),
    {ok, C2} = test_counter:start_link(2),
    {ok, C3} = test_counter:start_link(3),
    
    %% Get values from all counters
    V1 = gen_server:call(C1, {getValue, []}),
    V2 = gen_server:call(C2, {getValue, []}),
    V3 = gen_server:call(C3, {getValue, []}),
    
    ?assertEqual(1, V1),
    ?assertEqual(2, V2),
    ?assertEqual(3, V3),
    
    %% Increment all
    [gen_server:call(C, {increment, []}) || C <- [C1, C2, C3]],
    
    %% Verify
    ?assertEqual(2, gen_server:call(C1, {getValue, []})),
    ?assertEqual(3, gen_server:call(C2, {getValue, []})),
    ?assertEqual(4, gen_server:call(C3, {getValue, []})),
    
    [gen_server:stop(C) || C <- [C1, C2, C3]].

%%% Code change (hot reload) tests

code_change_preserves_state_test() ->
    {ok, Counter} = test_counter:start_link(100),
    
    %% Simulate code change
    State = sys:get_state(Counter),
    {ok, NewState} = beamtalk_actor:code_change(old_version, State, extra),
    
    %% State should be unchanged
    ?assertEqual(State, NewState),
    
    gen_server:stop(Counter).

%%% Callback tests

handle_info_ignores_unknown_messages_test() ->
    {ok, Counter} = test_counter:start_link(5),
    
    %% Send an info message (not cast or call)
    Counter ! {some, random, message},
    Counter ! another_random_message,
    
    %% Give it time to process
    timer:sleep(10),
    
    %% Verify actor still works normally
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(5, Result),
    
    gen_server:stop(Counter).

terminate_callback_test() ->
    {ok, Counter} = test_counter:start_link(10),
    
    %% Stop the actor normally
    ok = gen_server:stop(Counter, normal, 1000),
    
    %% Verify it's stopped
    timer:sleep(10),
    ?assertNot(is_process_alive(Counter)).

%%% Spawn helper tests

spawn_actor_test() ->
    {ok, Pid} = test_counter:start_link(15),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    
    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(15, Result),
    
    gen_server:stop(Pid).

start_link_test() ->
    {ok, Pid} = beamtalk_actor:start_link(test_counter, 25),
    ?assert(is_pid(Pid)),
    
    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(25, Result),
    
    gen_server:stop(Pid).

%%% Error handling tests
%% Comprehensive tests for exception handling in methods and dispatch

method_throws_exception_sync_test() ->
    {ok, Actor} = test_throwing_actor:start_link(),
    
    %% Call method that throws - should return error tuple
    Result = gen_server:call(Actor, {throwError, []}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = throwError}}, Result),
    
    %% Verify actor still works after exception
    NormalResult = gen_server:call(Actor, {normalMethod, []}),
    ?assertEqual(ok, NormalResult),
    
    gen_server:stop(Actor).

method_throws_beamtalk_error_preserves_kind_test() ->
    {ok, Actor} = test_bt_error_actor:start_link(),
    
    %% Call method that throws a #beamtalk_error{} - should preserve original error
    Result = gen_server:call(Actor, {throwBtError, []}),
    ?assertMatch({error, #beamtalk_error{kind = instantiation_error, class = 'TestClass'}}, Result),
    
    %% Verify actor still works after exception
    NormalResult = gen_server:call(Actor, {normalMethod, []}),
    ?assertEqual(ok, NormalResult),
    
    gen_server:stop(Actor).

method_throws_exception_async_test() ->
    {ok, Actor} = test_throwing_actor:start_link(),
    
    %% Send async message to method that throws
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {throwError, [], Future}),
    
    %% Future should be rejected with the error
    ?assertThrow(
        {future_rejected, #beamtalk_error{kind = type_error, selector = throwError}},
        beamtalk_future:await(Future, 1000)
    ),
    
    %% Verify actor still works after exception
    NormalFuture = beamtalk_future:new(),
    gen_server:cast(Actor, {normalMethod, [], NormalFuture}),
    NormalResult = beamtalk_future:await(NormalFuture, 1000),
    ?assertEqual(ok, NormalResult),
    
    gen_server:stop(Actor).

invalid_method_not_function_sync_test() ->
    {ok, Actor} = test_invalid_method_actor:start_link(),
    
    %% Try to call a method that's not a function
    Result = gen_server:call(Actor, {notAFunction, []}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = notAFunction}}, Result),
    
    %% Verify actor still works with valid methods
    ValidResult = gen_server:call(Actor, {validMethod, []}),
    ?assertEqual(ok, ValidResult),
    
    gen_server:stop(Actor).

invalid_method_not_function_async_test() ->
    {ok, Actor} = test_invalid_method_actor:start_link(),
    
    %% Send async message to method that's not a function
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {notAFunction, [], Future}),
    
    %% Future should be rejected
    ?assertThrow(
        {future_rejected, #beamtalk_error{kind = type_error, selector = notAFunction}},
        beamtalk_future:await(Future, 1000)
    ),
    
    gen_server:stop(Actor).

%%% Stress tests

stress_many_actors_test() ->
    %% Create 50 actors and verify they all work independently
    NumActors = 50,
    Actors = [element(2, test_counter:start_link(N)) || N <- lists:seq(1, NumActors)],
    
    %% Verify initial values
    Values = [gen_server:call(A, {getValue, []}) || A <- Actors],
    ?assertEqual(lists:seq(1, NumActors), Values),
    
    %% Increment all
    [gen_server:call(A, {increment, []}) || A <- Actors],
    
    %% Verify incremented values
    NewValues = [gen_server:call(A, {getValue, []}) || A <- Actors],
    ?assertEqual(lists:seq(2, NumActors + 1), NewValues),
    
    %% Clean up
    [gen_server:stop(A) || A <- Actors].

stress_many_messages_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    %% Send 100 increment messages
    NumMessages = 100,
    [gen_server:call(Counter, {increment, []}) || _ <- lists:seq(1, NumMessages)],
    
    %% Verify final value
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(NumMessages, Result),
    
    gen_server:stop(Counter).

%%% Edge case tests - concurrent behavior

concurrent_message_sends_test() ->
    %% Test multiple processes sending messages to same actor concurrently
    {ok, Counter} = test_counter:start_link(0),
    Parent = self(),
    NumProcesses = 10,
    IncrementsPerProcess = 10,
    
    %% Spawn multiple processes that increment concurrently
    Pids = [spawn(fun() ->
        [gen_server:call(Counter, {increment, []}) || _ <- lists:seq(1, IncrementsPerProcess)],
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, NumProcesses)],
    
    %% Wait for all processes to complete
    [receive {done, Pid} -> ok after 5000 -> ?assert(false) end || Pid <- Pids],
    
    %% Verify final value is correct
    Expected = NumProcesses * IncrementsPerProcess,
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(Expected, Result),
    
    gen_server:stop(Counter).

actor_crash_during_processing_test() ->
    %% Test that throwing actor handles errors without crashing the process
    {ok, Actor} = test_throwing_actor:start_link(),
    
    %% Actor should handle exception without crashing
    Result = gen_server:call(Actor, {throwError, []}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = throwError}}, Result),
    
    %% Verify actor is still alive
    ?assert(is_process_alive(Actor)),
    
    %% Verify it still processes messages normally
    NormalResult = gen_server:call(Actor, {normalMethod, []}),
    ?assertEqual(ok, NormalResult),
    
    gen_server:stop(Actor).

large_state_handling_test() ->
    %% Test actor with large state (simulating memory pressure)
    LargeData = lists:duplicate(10000, {data, lists:seq(1, 100)}),
    
    {ok, Pid} = beamtalk_actor:start_link(test_counter, 0),
    
    %% Set large state via sys:replace_state
    sys:replace_state(Pid, fun(State) ->
        State#{large_data => LargeData}
    end),
    
    %% Verify actor still works with large state
    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(0, Result),
    
    %% Verify can still modify state
    gen_server:call(Pid, {'setValue:', [42]}),
    ?assertEqual(42, gen_server:call(Pid, {getValue, []})),
    
    gen_server:stop(Pid).

message_queue_overflow_test() ->
    %% Test actor behavior when message queue grows large
    {ok, Counter} = test_counter:start_link(0),
    
    %% Send many async messages rapidly
    Futures = [begin
        F = beamtalk_future:new(),
        gen_server:cast(Counter, {increment, [], F}),
        F
    end || _ <- lists:seq(1, 1000)],
    
    %% Wait for all to complete
    [beamtalk_future:await(F, 5000) || F <- Futures],
    
    %% Verify all messages were processed
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(1000, Result),
    
    %% Verify actor is still responsive
    ?assert(is_process_alive(Counter)),
    
    gen_server:stop(Counter).

dnu_with_invalid_signatures_test() ->
    %% Test doesNotUnderstand with various invalid method signatures
    {ok, Target} = test_counter:start_link(100),
    {ok, Proxy} = test_proxy:start_link(Target),
    
    %% Test with empty selector - should be unknown
    Result1 = gen_server:call(Proxy, {'', []}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand, selector = ''}}, Result1),
    
    %% Test with atom selector containing special characters
    Result2 = gen_server:call(Proxy, {'method:with:colons:', [1, 2, 3]}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand, selector = 'method:with:colons:'}}, Result2),
    
    %% Test with very long selector name (will be unknown)
    LongSelector = list_to_atom(lists:duplicate(100, $a)),
    Result3 = gen_server:call(Proxy, {LongSelector, []}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand, selector = LongSelector}}, Result3),
    
    %% Test that proxy still works after invalid selectors
    ValidResult = gen_server:call(Proxy, {getValue, []}),
    ?assertEqual(100, ValidResult),
    
    gen_server:stop(Proxy),
    gen_server:stop(Target).

state_rollback_on_error_test() ->
    %% Verify state is not modified if method throws exception
    {ok, Counter} = test_counter:start_link(10),
    
    %% Get initial state
    InitialValue = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(10, InitialValue),
    
    %% Try to call a non-existent method (will fail)
    _ErrorResult = gen_server:call(Counter, {nonExistent, []}),
    
    %% Verify state is unchanged
    FinalValue = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(10, FinalValue),
    
    gen_server:stop(Counter).

rapid_message_sending_performance_test() ->
    %% Performance test: send many messages rapidly and measure time
    {ok, Counter} = test_counter:start_link(0),
    NumMessages = 1000,
    
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Send messages synchronously for accuracy
    [gen_server:call(Counter, {increment, []}) || _ <- lists:seq(1, NumMessages)],
    
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedUs = EndTime - StartTime,
    
    %% Verify all messages processed
    Result = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(NumMessages, Result),
    
    %% Sanity check: should complete in under 5 seconds
    ?assert(ElapsedUs < 5000000),
    
    gen_server:stop(Counter).

gen_server_callback_edge_cases_test() ->
    %% Test edge cases in gen_server callbacks
    {ok, Counter} = test_counter:start_link(5),
    
    %% Test handle_call with timeout
    Result = gen_server:call(Counter, {getValue, []}, 5000),
    ?assertEqual(5, Result),
    
    %% Test sys messages (get_state, replace_state)
    State = sys:get_state(Counter),
    ?assertMatch(#{value := 5}, State),
    
    NewState = sys:replace_state(Counter, fun(S) -> S#{value => 99} end),
    ?assertMatch(#{value := 99}, NewState),
    
    UpdatedValue = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(99, UpdatedValue),
    
    gen_server:stop(Counter).

memory_cleanup_after_termination_test() ->
    %% Test that actor memory is cleaned up after termination
    InitialMemory = erlang:memory(processes),
    
    %% Create and destroy multiple actors
    Actors = [element(2, test_counter:start_link(N)) || N <- lists:seq(1, 100)],
    
    %% Stop all actors
    [gen_server:stop(A) || A <- Actors],
    
    %% Give GC time to clean up
    timer:sleep(100),
    erlang:garbage_collect(),
    timer:sleep(100),
    
    FinalMemory = erlang:memory(processes),
    
    %% Memory should not have grown significantly (allow 10% margin)
    MemoryGrowth = FinalMemory - InitialMemory,
    ?assert(MemoryGrowth < InitialMemory * 0.1).

concurrent_doesNotUnderstand_test() ->
    %% Test concurrent calls to doesNotUnderstand
    {ok, Target} = test_counter:start_link(42),
    {ok, Proxy} = test_proxy:start_link(Target),
    Parent = self(),
    NumProcesses = 10,
    
    %% Spawn processes that call unknown methods concurrently
    Pids = [spawn(fun() ->
        %% Call unknown method which will trigger doesNotUnderstand
        Result = gen_server:call(Proxy, {list_to_atom("unknown" ++ integer_to_list(N)), [N]}),
        Parent ! {result, self(), Result}
    end) || N <- lists:seq(1, NumProcesses)],
    
    %% Collect results
    Results = [receive {result, Pid, R} -> R after 5000 -> timeout end || Pid <- Pids],
    
    %% All should have returned errors (target doesn't have these methods)
    ?assertEqual(NumProcesses, length([R || {error, _} = R <- Results])),
    
    gen_server:stop(Proxy),
    gen_server:stop(Target).

%%% BT-159: Self-as-object tests

make_self_test() ->
    %% Test that make_self/1 constructs a proper #beamtalk_object{} record
    %% Start an actor to get a valid pid context
    {ok, Pid} = test_counter:start_link(0),
    
    %% Call make_self in the actor's context via a helper function
    Self = gen_server:call(Pid, {test_make_self, []}),
    
    %% Verify the record structure
    ?assertMatch({beamtalk_object, 'Counter', 'counter', _}, Self),
    ?assertEqual('Counter', element(2, Self)),
    ?assertEqual('counter', element(3, Self)),
    ?assertEqual(Pid, element(4, Self)),
    
    gen_server:stop(Pid).

dispatch4_with_self_parameter_test() ->
    %% Test dispatch/4 with Self parameter using new-style method
    {ok, Actor} = test_self_aware_actor:start_link(100),
    
    %% Call method that returns Self
    Result = gen_server:call(Actor, {getSelf, []}),
    
    %% Should return #beamtalk_object{} record
    ?assertMatch({beamtalk_object, 'SelfAwareActor', 'test_self_aware_actor', _}, Result),
    ?assertEqual('SelfAwareActor', element(2, Result)),
    ?assertEqual('test_self_aware_actor', element(3, Result)),
    
    gen_server:stop(Actor).

dispatch4_access_self_class_test() ->
    %% Test that methods can access Self.class
    {ok, Actor} = test_self_aware_actor:start_link(0),
    
    %% Call method that returns self's class
    Result = gen_server:call(Actor, {getClassName, []}),
    
    ?assertEqual('SelfAwareActor', Result),
    
    gen_server:stop(Actor).

dispatch3_backward_compatibility_test() ->
    %% Test that old dispatch/3 still works with existing actors
    {ok, Counter} = test_counter:start_link(10),
    
    %% Old-style methods (Fun/2) should still work
    Result1 = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(10, Result1),
    
    %% Modify state
    ok = gen_server:call(Counter, {'setValue:', [25]}),
    Result2 = gen_server:call(Counter, {getValue, []}),
    ?assertEqual(25, Result2),
    
    gen_server:stop(Counter).

dispatch4_mixed_methods_test() ->
    %% Test actor with both old-style (Fun/2) and new-style (Fun/4) methods
    {ok, Actor} = test_mixed_actor:start_link(50),
    
    %% Call old-style method
    Old = gen_server:call(Actor, {getValueOldStyle, []}),
    ?assertEqual(50, Old),
    
    %% Call new-style method
    New = gen_server:call(Actor, {getValueNewStyle, []}),
    ?assertEqual(50, New),
    
    %% Call method that returns Self (new-style)
    Self = gen_server:call(Actor, {getSelf, []}),
    ?assertMatch({beamtalk_object, 'MixedActor', _, _}, Self),
    
    gen_server:stop(Actor).

dispatch4_dnu_with_self_test() ->
    %% Test new-style DNU handler (Fun/3) with Self parameter
    {ok, Actor} = test_self_aware_proxy:start_link(),
    
    %% Call unknown method - should trigger DNU handler which returns Self
    Result = gen_server:call(Actor, {unknownMethod, [arg1, arg2]}),
    
    %% DNU handler should return Self
    ?assertMatch({beamtalk_object, 'SelfAwareProxy', 'test_self_aware_proxy', _}, Result),
    
    gen_server:stop(Actor).

dispatch4_async_with_self_test() ->
    %% Test async messages work with new-style methods (Fun/4)
    {ok, Actor} = test_self_aware_actor:start_link(0),
    
    %% Send async message to getSelf method
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {getSelf, [], Future}),
    
    %% Wait for result
    Result = beamtalk_future:await(Future),
    
    %% Should return Self
    ?assertMatch({beamtalk_object, 'SelfAwareActor', 'test_self_aware_actor', _}, Result),
    
    gen_server:stop(Actor).

%%% BT-177: Object reflection API tests
%%% BT-427: Reflection methods now delegate to hierarchy walk, which requires bootstrap

respondsTo_existing_method_test() ->
    %% Test respondsTo: with an existing method
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    %% Check for existing methods (use keyword selector with colon)
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [increment]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [getValue]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', ['setValue:']})),
    
    gen_server:stop(Counter).

respondsTo_nonexistent_method_test() ->
    %% Test respondsTo: with a nonexistent method
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    %% Check for nonexistent methods
    ?assertEqual(false, gen_server:call(Counter, {'respondsTo:', [unknownMethod]})),
    ?assertEqual(false, gen_server:call(Counter, {'respondsTo:', [noSuchThing]})),
    
    gen_server:stop(Counter).

instVarNames_test() ->
    %% Test instVarNames returns instance variable names
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),
    
    %% Get instance variable names (should exclude $beamtalk_class, __class_mod__, __methods__)
    Result = gen_server:call(Counter, {instVarNames, []}),
    
    %% Should return [value] - the only user-defined instance variable
    ?assertEqual([value], Result),
    
    gen_server:stop(Counter).

instVarAt_existing_variable_test() ->
    %% Test instVarAt: with an existing instance variable
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),
    
    %% Read the value field
    Result = gen_server:call(Counter, {'instVarAt:', [value]}),
    ?assertEqual(42, Result),
    
    %% Modify state and read again
    gen_server:call(Counter, {'setValue:', [99]}),
    Result2 = gen_server:call(Counter, {'instVarAt:', [value]}),
    ?assertEqual(99, Result2),
    
    gen_server:stop(Counter).

instVarAt_nonexistent_variable_test() ->
    %% Test instVarAt: with a nonexistent instance variable (should return nil)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    %% Read nonexistent field
    Result = gen_server:call(Counter, {'instVarAt:', [nonExistent]}),
    ?assertEqual(nil, Result),
    
    gen_server:stop(Counter).

instVarAt_put_test() ->
    %% Test instVarAt:put: to write instance variable (BT-164)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),
    
    %% Read initial value
    InitialValue = gen_server:call(Counter, {'instVarAt:', [value]}),
    ?assertEqual(42, InitialValue),
    
    %% Write new value using instVarAt:put: (returns value, Smalltalk-80 semantics)
    Result = gen_server:call(Counter, {'instVarAt:put:', [value, 99]}),
    ?assertEqual(99, Result),
    
    %% Verify the value was updated
    NewValue = gen_server:call(Counter, {'instVarAt:', [value]}),
    ?assertEqual(99, NewValue),
    
    %% Write a new instance variable that didn't exist
    Result2 = gen_server:call(Counter, {'instVarAt:put:', [newField, <<"hello">>]}),
    ?assertEqual(<<"hello">>, Result2),
    
    %% Verify the new field exists
    NewFieldValue = gen_server:call(Counter, {'instVarAt:', [newField]}),
    ?assertEqual(<<"hello">>, NewFieldValue),
    
    %% Verify it appears in instVarNames
    VarNames = gen_server:call(Counter, {instVarNames, []}),
    ?assert(lists:member(newField, VarNames)),
    
    gen_server:stop(Counter).

reflection_combined_test() ->
    %% Combined test: use reflection to discover and access instance variables
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(123),
    
    %% Discover instance variables
    VarNames = gen_server:call(Counter, {instVarNames, []}),
    ?assertEqual([value], VarNames),
    
    %% Read each discovered variable
    [VarName] = VarNames,
    Value = gen_server:call(Counter, {'instVarAt:', [VarName]}),
    ?assertEqual(123, Value),
    
    %% Check that we respond to the method we're about to call
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [increment]})),
    
    %% Call the method
    gen_server:call(Counter, {increment, []}),
    
    %% Read the updated value via reflection
    NewValue = gen_server:call(Counter, {'instVarAt:', [value]}),
    ?assertEqual(124, NewValue),
    
    gen_server:stop(Counter).

%%% BT-165: perform: dynamic message send tests

perform_unary_message_test() ->
    %% Test perform: with no arguments (unary message)
    {ok, Counter} = test_counter:start_link(10),
    
    %% obj perform: #increment  => obj increment
    %% increment returns noreply, so the result is nil
    nil = gen_server:call(Counter, {'perform:', [increment]}),
    ?assertEqual(11, gen_server:call(Counter, {getValue, []})),
    
    %% Another increment via perform:
    nil = gen_server:call(Counter, {'perform:', [increment]}),
    ?assertEqual(12, gen_server:call(Counter, {getValue, []})),
    
    gen_server:stop(Counter).

perform_keyword_message_test() ->
    %% Test perform:withArguments: with keyword message
    {ok, Counter} = test_counter:start_link(5),
    
    %% obj perform: #'setValue:' withArgs: [100]  => obj setValue: 100
    ok = gen_server:call(Counter, {'perform:withArguments:', ['setValue:', [100]]}),
    ?assertEqual(100, gen_server:call(Counter, {getValue, []})),
    
    gen_server:stop(Counter).

perform_with_result_test() ->
    %% Test perform: on a method that returns a value
    {ok, Counter} = test_counter:start_link(42),
    
    %% obj perform: #getValue  => obj getValue (returns value)
    Result = gen_server:call(Counter, {'perform:', [getValue]}),
    ?assertEqual(42, Result),
    
    gen_server:stop(Counter).

perform_withArgs_multiple_arguments_test() ->
    %% Test perform:withArguments: with multiple arguments
    {ok, Actor} = test_multi_arg_actor:start_link(),
    
    %% obj perform: #'compute:plus:' withArgs: [10, 20]  => obj compute: 10 plus: 20
    Result = gen_server:call(Actor, {'perform:withArguments:', ['compute:plus:', [10, 20]]}),
    ?assertEqual(30, Result),
    
    gen_server:stop(Actor).

perform_unknown_selector_test() ->
    %% Test perform: with unknown selector (should trigger doesNotUnderstand)
    {ok, Counter} = test_counter:start_link(0),
    
    %% obj perform: #unknownMethod  => doesNotUnderstand
    Result = gen_server:call(Counter, {'perform:', [unknownMethod]}),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand, selector = unknownMethod}}, Result),
    
    gen_server:stop(Counter).

perform_invalid_selector_type_test() ->
    %% Test perform: with non-atom selector (should return type_error)
    {ok, Counter} = test_counter:start_link(0),
    
    %% obj perform: "notAnAtom"  => type error
    Result = gen_server:call(Counter, {'perform:', ["notAnAtom"]}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = 'perform:'}}, Result),
    
    gen_server:stop(Counter).

perform_withArgs_invalid_args_type_test() ->
    %% Test perform:withArguments: with non-list args (should error)
    {ok, Counter} = test_counter:start_link(0),
    
    %% obj perform: #increment withArgs: 42  => type error (42 is not a list)
    Result = gen_server:call(Counter, {'perform:withArguments:', [increment, 42]}),
    ?assertMatch({error, #beamtalk_error{kind = type_error, selector = 'perform:withArguments:'}}, Result),
    
    gen_server:stop(Counter).

perform_async_with_future_test() ->
    %% Test perform: with async message (cast with future)
    {ok, Counter} = test_counter:start_link(0),
    
    %% Send async perform: message
    Future = beamtalk_future:new(),
    gen_server:cast(Counter, {'perform:', [increment], Future}),
    
    %% Wait for future to resolve
    ?assertEqual(nil, beamtalk_future:await(Future)),
    
    %% Verify state was updated
    ?assertEqual(1, gen_server:call(Counter, {getValue, []})),
    
    gen_server:stop(Counter).

perform_recursive_dispatch_test() ->
    %% Test that perform: correctly dispatches through the same dispatch/4 logic
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(50),
    
    %% Use perform: to call respondsTo: (a built-in reflection method)
    Result1 = gen_server:call(Counter, {'perform:withArguments:', ['respondsTo:', [increment]]}),
    ?assertEqual(true, Result1),
    
    %% Use perform: to call instVarAt: (another built-in)
    Result2 = gen_server:call(Counter, {'perform:withArguments:', ['instVarAt:', [value]]}),
    ?assertEqual(50, Result2),
    
    gen_server:stop(Counter).

%%% BT-427: Object method delegation tests
%%% Actors inherit Object base methods via hierarchy walk

object_describe_test() ->
    %% Test describe on actor (inherited from Object via hierarchy walk)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),
    
    Result = gen_server:call(Counter, {describe, []}),
    ?assert(is_binary(Result)),
    
    gen_server:stop(Counter).

object_inspect_test() ->
    %% Test inspect on actor (inherited from Object via hierarchy walk)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),
    
    Result = gen_server:call(Counter, {inspect, []}),
    ?assert(is_binary(Result)),
    
    gen_server:stop(Counter).

object_isNil_test() ->
    %% Test isNil on actor (inherited from Object)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    ?assertEqual(false, gen_server:call(Counter, {isNil, []})),
    
    gen_server:stop(Counter).

object_notNil_test() ->
    %% Test notNil on actor (inherited from Object)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    ?assertEqual(true, gen_server:call(Counter, {notNil, []})),
    
    gen_server:stop(Counter).

object_hash_test() ->
    %% Test hash on actor (inherited from Object)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    Result = gen_server:call(Counter, {hash, []}),
    ?assert(is_integer(Result)),
    
    gen_server:stop(Counter).

object_yourself_test() ->
    %% Test yourself on actor (inherited from Object, returns Self)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    Result = gen_server:call(Counter, {yourself, []}),
    ?assertMatch({beamtalk_object, 'Counter', counter, _}, Result),
    
    gen_server:stop(Counter).

object_class_test() ->
    %% Test class on actor (inherited from Object)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    Result = gen_server:call(Counter, {class, []}),
    ?assertEqual('Counter', Result),
    
    gen_server:stop(Counter).

respondsTo_inherited_methods_test() ->
    %% Test respondsTo: reports inherited Object methods
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    
    %% Inherited methods from Object should be reported
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [describe]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [inspect]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [isNil]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [hash]})),
    
    %% Actor-specific built-in should also be reported
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [isAlive]})),
    
    gen_server:stop(Counter).

%%% Actor lifecycle tests (BT-170)

isAlive_returns_true_for_running_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    %% isAlive via async_send should resolve to true
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, isAlive, [], Future),
    ?assertEqual(true, beamtalk_future:await(Future)),
    
    gen_server:stop(Counter).

isAlive_returns_false_after_actor_stops_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),
    
    %% isAlive via async_send should resolve to false
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, isAlive, [], Future),
    ?assertEqual(false, beamtalk_future:await(Future)).

isAlive_sync_returns_true_for_running_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    ?assertEqual(true, beamtalk_actor:sync_send(Counter, isAlive, [])),
    
    gen_server:stop(Counter).

isAlive_sync_returns_false_after_actor_stops_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),
    
    ?assertEqual(false, beamtalk_actor:sync_send(Counter, isAlive, [])).

isAlive_via_dispatch_returns_true_test() ->
    %% When called via gen_server:call (dispatch path), always true
    {ok, Counter} = test_counter:start_link(0),
    
    ?assertEqual(true, gen_server:call(Counter, {isAlive, []})),
    
    gen_server:stop(Counter).

monitor_returns_reference_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, monitor, [], Future),
    Ref = beamtalk_future:await(Future),
    ?assert(is_reference(Ref)),
    
    %% Clean up the monitor
    erlang:demonitor(Ref, [flush]),
    gen_server:stop(Counter).

monitor_sync_returns_reference_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    Ref = beamtalk_actor:sync_send(Counter, monitor, []),
    ?assert(is_reference(Ref)),
    
    erlang:demonitor(Ref, [flush]),
    gen_server:stop(Counter).

monitor_delivers_down_on_actor_death_test() ->
    {ok, Counter} = test_counter:start_link(0),
    
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, monitor, [], Future),
    Ref = beamtalk_future:await(Future),
    ?assert(is_reference(Ref)),
    
    %% Kill the actor and verify we get a DOWN message
    gen_server:stop(Counter, normal, 1000),
    receive
        {'DOWN', Ref, process, Counter, normal} -> ok
    after 1000 ->
        ?assert(false)
    end.

async_message_to_dead_actor_rejects_future_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),
    
    %% Sending to dead actor should reject the future
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, increment, [], Future),
    
    ?assertThrow(
        {future_rejected, #beamtalk_error{kind = actor_dead, selector = increment}},
        beamtalk_future:await(Future, 1000)
    ).

sync_message_to_dead_actor_returns_error_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),
    
    Result = beamtalk_actor:sync_send(Counter, getValue, []),
    ?assertMatch({error, #beamtalk_error{kind = actor_dead, selector = getValue}}, Result).

%%% ===========================================================================
%%% register_spawned/4 Callback Integration Tests
%%% ===========================================================================

register_spawned_invokes_callback_when_env_set_test() ->
    %% Register test process so test_spawn_callback can find us
    register(spawn_callback_test, self()),
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback),

    RegistryPid = self(),
    ActorPid = spawn(fun() -> receive stop -> ok end end),
    ok = beamtalk_actor:register_spawned(RegistryPid, ActorPid, 'Counter', test_counter),

    %% Verify callback was invoked with correct args
    receive
        {callback_invoked, RegPid, APid, Class, Mod} ->
            ?assertEqual(RegistryPid, RegPid),
            ?assertEqual(ActorPid, APid),
            ?assertEqual('Counter', Class),
            ?assertEqual(test_counter, Mod)
    after 1000 ->
        error("Callback was not invoked")
    end,

    %% Cleanup
    exit(ActorPid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    unregister(spawn_callback_test).

register_spawned_noop_when_env_unset_test() ->
    %% Ensure no callback env is set
    application:unset_env(beamtalk_runtime, actor_spawn_callback),

    ActorPid = spawn(fun() -> receive stop -> ok end end),
    %% Should return ok without error
    ?assertEqual(ok, beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)),

    exit(ActorPid, kill).

register_spawned_survives_undef_callback_test() ->
    %% Set callback to a module that doesn't export on_actor_spawned/4
    %% This module exists (it's the test module itself) but doesn't have the function
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_actor_tests),

    ActorPid = spawn(fun() -> receive stop -> ok end end),
    %% Should return ok despite undef error
    ?assertEqual(ok, beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)),

    %% Cleanup
    exit(ActorPid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).

register_spawned_survives_beamtalk_error_test() ->
    %% Set callback to a module that raises a #beamtalk_error{}
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback_bt_error),

    ActorPid = spawn(fun() -> receive stop -> ok end end),
    %% Should return ok despite beamtalk error
    ?assertEqual(ok, beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)),

    %% Cleanup
    exit(ActorPid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).

register_spawned_survives_generic_error_test() ->
    %% Set callback to a module that throws a generic error
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback_generic_error),

    ActorPid = spawn(fun() -> receive stop -> ok end end),
    %% Should return ok despite generic throw
    ?assertEqual(ok, beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)),

    %% Cleanup
    exit(ActorPid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).
