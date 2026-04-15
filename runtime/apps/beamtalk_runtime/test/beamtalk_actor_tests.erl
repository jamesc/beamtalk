%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
EUnit tests for beamtalk_actor module

Tests all actor behaviors:
- Basic message dispatch (sync and async)
- doesNotUnderstand fallback
- Multiple actors communicating
- Code hot reload (code_change)
- Actor spawn/initialization
- Error cases
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Logger handler callback for BT-1822 stacktrace tests
-export([log/2]).

log(LogEvent, #{config := #{parent := Parent}}) ->
    Parent ! {log_event, LogEvent},
    ok.

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
    ?assertMatch(
        {error, #beamtalk_error{kind = does_not_understand, selector = unknownMethod}}, Result
    ),
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
    Futures = [
        begin
            F = beamtalk_future:new(),
            gen_server:cast(Counter, {increment, [], F}),
            F
        end
     || _ <- lists:seq(1, 5)
    ],

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

    % Give it time to process the malformed message
    timer:sleep(10),
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
    ?assertMatch(
        {error, #beamtalk_error{kind = runtime_error, selector = 'doesNotUnderstand:args:'}}, Result
    ),

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
    ?assertMatch({error, #beamtalk_error{kind = runtime_error, selector = throwError}}, Result),

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
        {future_rejected, #beamtalk_error{kind = runtime_error, selector = throwError}},
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

%%% Backward-compat error path tests (BT-1889)
%% Verify that the {error, {ErlType, ErrorValue}} 2-tuple path (pre-BT-1822
%% compiled actors without stacktrace) preserves the exception class.

compat_sync_send_exit_preserves_class_test() ->
    %% An exit error through the backward-compat path should produce ExitError,
    %% not RuntimeError (which would happen if ErlType were discarded).
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        beamtalk_actor:sync_send(Actor, triggerExitError, []),
        ?assert(false, "Expected error to be raised")
    catch
        error:#{error := Inner, '$beamtalk_class' := Class} ->
            ?assertEqual('ExitError', Class),
            ?assertEqual(erlang_exit, Inner#beamtalk_error.kind)
    after
        gen_server:stop(Actor)
    end.

compat_sync_send_throw_preserves_class_test() ->
    %% A throw error through the backward-compat path should produce ThrowError.
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        beamtalk_actor:sync_send(Actor, triggerThrowError, []),
        ?assert(false, "Expected error to be raised")
    catch
        error:#{error := Inner, '$beamtalk_class' := Class} ->
            ?assertEqual('ThrowError', Class),
            ?assertEqual(erlang_throw, Inner#beamtalk_error.kind)
    after
        gen_server:stop(Actor)
    end.

compat_sync_send_error_preserves_class_test() ->
    %% A plain error through the backward-compat path should produce RuntimeError.
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        beamtalk_actor:sync_send(Actor, triggerPlainError, []),
        ?assert(false, "Expected error to be raised")
    catch
        error:#{error := Inner, '$beamtalk_class' := Class} ->
            ?assertEqual('RuntimeError', Class),
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind)
    after
        gen_server:stop(Actor)
    end.

compat_self_dispatch_exit_preserves_class_test() ->
    %% Test the self_dispatch path (unwrap_dispatch_result) with backward-compat
    %% 2-tuple errors. selfSendExitError triggers a self-send inside the actor,
    %% which goes through self_dispatch -> safe_dispatch/3 -> unwrap_dispatch_result.
    %% The safe_dispatch/3 for triggerExitError returns {error, {exit, ...}, State}.
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        %% The self-send error bubbles up through dispatch/4's try/catch as an
        %% error exception, which wrap_method_error catches and wraps.
        Result = gen_server:call(Actor, {selfSendExitError, []}),
        ?assertMatch({error, _}, Result),
        {error, Inner} = Result,
        %% The inner error should be a #beamtalk_error{} from wrap_method_error,
        %% with the original_reason containing the ExitError wrapped exception.
        ?assertMatch(#beamtalk_error{kind = runtime_error}, Inner),
        Details = Inner#beamtalk_error.details,
        OriginalReason = maps:get(original_reason, Details),
        ?assertMatch(#{'$beamtalk_class' := 'ExitError'}, OriginalReason)
    after
        gen_server:stop(Actor)
    end.

compat_self_dispatch_throw_preserves_class_test() ->
    %% Same as above but for throw errors through the self_dispatch path.
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        Result = gen_server:call(Actor, {selfSendThrowError, []}),
        ?assertMatch({error, _}, Result),
        {error, Inner} = Result,
        ?assertMatch(#beamtalk_error{kind = runtime_error}, Inner),
        Details = Inner#beamtalk_error.details,
        OriginalReason = maps:get(original_reason, Details),
        ?assertMatch(#{'$beamtalk_class' := 'ThrowError'}, OriginalReason)
    after
        gen_server:stop(Actor)
    end.

compat_self_dispatch_error_preserves_class_test() ->
    %% Same as above but for plain errors through the self_dispatch path.
    {ok, Actor} = test_compat_error_actor:start_link(),
    try
        Result = gen_server:call(Actor, {selfSendPlainError, []}),
        ?assertMatch({error, _}, Result),
        {error, Inner} = Result,
        ?assertMatch(#beamtalk_error{kind = runtime_error}, Inner),
        Details = Inner#beamtalk_error.details,
        OriginalReason = maps:get(original_reason, Details),
        ?assertMatch(#{'$beamtalk_class' := 'RuntimeError'}, OriginalReason)
    after
        gen_server:stop(Actor)
    end.

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
    Pids = [
        spawn(fun() ->
            [gen_server:call(Counter, {increment, []}) || _ <- lists:seq(1, IncrementsPerProcess)],
            Parent ! {done, self()}
        end)
     || _ <- lists:seq(1, NumProcesses)
    ],

    %% Wait for all processes to complete
    [
        receive
            {done, Pid} -> ok
        after 5000 -> ?assert(false)
        end
     || Pid <- Pids
    ],

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
    ?assertMatch({error, #beamtalk_error{kind = runtime_error, selector = throwError}}, Result),

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
    Futures = [
        begin
            F = beamtalk_future:new(),
            gen_server:cast(Counter, {increment, [], F}),
            F
        end
     || _ <- lists:seq(1, 1000)
    ],

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
    ?assertMatch(
        {error, #beamtalk_error{kind = does_not_understand, selector = 'method:with:colons:'}},
        Result2
    ),

    %% Test with very long selector name (will be unknown)
    LongSelector = list_to_atom(lists:duplicate(100, $a)),
    Result3 = gen_server:call(Proxy, {LongSelector, []}),
    ?assertMatch(
        {error, #beamtalk_error{kind = does_not_understand, selector = LongSelector}}, Result3
    ),

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
    Pids = [
        spawn(fun() ->
            %% Call unknown method which will trigger doesNotUnderstand
            Result = gen_server:call(Proxy, {list_to_atom("unknown" ++ integer_to_list(N)), [N]}),
            Parent ! {result, self(), Result}
        end)
     || N <- lists:seq(1, NumProcesses)
    ],

    %% Collect results
    Results = [
        receive
            {result, Pid, R} -> R
        after 5000 -> timeout
        end
     || Pid <- Pids
    ],

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

fieldNames_test() ->
    %% Test fieldNames returns instance variable names
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),

    %% Get instance variable names (should exclude $beamtalk_class, __class_mod__, __methods__)
    Result = gen_server:call(Counter, {fieldNames, []}),

    %% Should return [value] - the only user-defined instance variable
    ?assertEqual([value], Result),

    gen_server:stop(Counter).

fieldAt_existing_variable_test() ->
    %% Test fieldAt: with an existing instance variable
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),

    %% Read the value field
    Result = gen_server:call(Counter, {'fieldAt:', [value]}),
    ?assertEqual(42, Result),

    %% Modify state and read again
    gen_server:call(Counter, {'setValue:', [99]}),
    Result2 = gen_server:call(Counter, {'fieldAt:', [value]}),
    ?assertEqual(99, Result2),

    gen_server:stop(Counter).

fieldAt_nonexistent_variable_test() ->
    %% Test fieldAt: with a nonexistent instance variable (should return nil)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),

    %% Read nonexistent field
    Result = gen_server:call(Counter, {'fieldAt:', [nonExistent]}),
    ?assertEqual(nil, Result),

    gen_server:stop(Counter).

fieldAt_put_test() ->
    %% Test fieldAt:put: to write instance variable (BT-164)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),

    %% Read initial value
    InitialValue = gen_server:call(Counter, {'fieldAt:', [value]}),
    ?assertEqual(42, InitialValue),

    %% Write new value using fieldAt:put: (returns value, Smalltalk-80 semantics)
    Result = gen_server:call(Counter, {'fieldAt:put:', [value, 99]}),
    ?assertEqual(99, Result),

    %% Verify the value was updated
    NewValue = gen_server:call(Counter, {'fieldAt:', [value]}),
    ?assertEqual(99, NewValue),

    %% Write a new instance variable that didn't exist
    Result2 = gen_server:call(Counter, {'fieldAt:put:', [newField, <<"hello">>]}),
    ?assertEqual(<<"hello">>, Result2),

    %% Verify the new field exists
    NewFieldValue = gen_server:call(Counter, {'fieldAt:', [newField]}),
    ?assertEqual(<<"hello">>, NewFieldValue),

    %% Verify it appears in fieldNames
    VarNames = gen_server:call(Counter, {fieldNames, []}),
    ?assert(lists:member(newField, VarNames)),

    gen_server:stop(Counter).

reflection_combined_test() ->
    %% Combined test: use reflection to discover and access instance variables
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(123),

    %% Discover instance variables
    VarNames = gen_server:call(Counter, {fieldNames, []}),
    ?assertEqual([value], VarNames),

    %% Read each discovered variable
    [VarName] = VarNames,
    Value = gen_server:call(Counter, {'fieldAt:', [VarName]}),
    ?assertEqual(123, Value),

    %% Check that we respond to the method we're about to call
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [increment]})),

    %% Call the method
    gen_server:call(Counter, {increment, []}),

    %% Read the updated value via reflection
    NewValue = gen_server:call(Counter, {'fieldAt:', [value]}),
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
    ?assertMatch(
        {error, #beamtalk_error{kind = does_not_understand, selector = unknownMethod}}, Result
    ),

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
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error, selector = 'perform:withArguments:'}}, Result
    ),

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

    %% Use perform: to call fieldAt: (another built-in)
    Result2 = gen_server:call(Counter, {'perform:withArguments:', ['fieldAt:', [value]]}),
    ?assertEqual(50, Result2),

    gen_server:stop(Counter).

%%% BT-427: Object method delegation tests
%%% Actors inherit Object base methods via hierarchy walk

object_printstring_test() ->
    %% Test printString on actor (inherited from Object via hierarchy walk)
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(42),

    Result = gen_server:call(Counter, {'printString', []}),
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
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', ['printString']})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [inspect]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [isNil]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [hash]})),

    %% Actor-specific built-in should also be reported
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [isAlive]})),
    %% BT-1442: pid, monitor, onExit: should be reported
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [pid]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [monitor]})),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', ['onExit:']})),

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

%%% BT-1442: pid tests

pid_async_returns_pid_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, pid, [], Future),
    Result = beamtalk_future:await(Future),
    ?assert(is_pid(Result)),
    ?assertEqual(Counter, Result),
    gen_server:stop(Counter).

pid_sync_returns_pid_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Result = beamtalk_actor:sync_send(Counter, pid, []),
    ?assert(is_pid(Result)),
    ?assertEqual(Counter, Result),
    gen_server:stop(Counter).

%%% BT-1442: onExit: tests

on_exit_sync_calls_block_on_death_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Self = self(),
    Block = fun(Reason) -> Self ! {exit_reason, Reason} end,
    beamtalk_actor:sync_send(Counter, 'onExit:', [Block]),
    gen_server:stop(Counter, normal, 1000),
    %% Give the watcher process time to be scheduled and call the block
    timer:sleep(50),
    receive
        {exit_reason, normal} -> ok
    after 2000 ->
        ?assert(false)
    end.

on_exit_async_calls_block_on_death_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Self = self(),
    Block = fun(Reason) -> Self ! {exit_reason_async, Reason} end,
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, 'onExit:', [Block], Future),
    ?assertEqual(ok, beamtalk_future:await(Future)),
    gen_server:stop(Counter, normal, 1000),
    %% Give the watcher process time to be scheduled and call the block
    timer:sleep(50),
    receive
        {exit_reason_async, normal} -> ok
    after 2000 ->
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
    %% BT-918: sync_send to a dead actor now raises an actor_dead exception (not returns error tuple).
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),

    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead, selector = getValue}},
        beamtalk_actor:sync_send(Counter, getValue, [])
    ).

%%% ===========================================================================
%%% register_spawned callback tests (BT-391)
%%% ===========================================================================

register_spawned_returns_ok_when_no_callback_configured_test() ->
    %% Clear any existing callback config
    application:unset_env(beamtalk_runtime, actor_spawn_callback),

    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_actor:register_spawned(FakePid, FakePid, 'Test', test_mod),
    ?assertEqual(ok, Result),

    exit(FakePid, kill).

register_spawned_returns_error_when_callback_undef_test() ->
    %% Clear first in case a previous test leaked (assertion failure before cleanup)
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    %% Set callback to a module that doesn't implement on_actor_spawned/4
    application:set_env(beamtalk_runtime, actor_spawn_callback, nonexistent_callback_mod),

    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_actor:register_spawned(FakePid, FakePid, 'Test', test_mod),
    ?assertMatch({error, {callback_undef, nonexistent_callback_mod}}, Result),

    exit(FakePid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).

register_spawned_returns_error_when_callback_crashes_test() ->
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    application:set_env(
        beamtalk_runtime, actor_spawn_callback, beamtalk_actor_tests_crash_callback
    ),

    %% Ensure crash callback module exists
    ok = ensure_crash_callback_module(),

    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_actor:register_spawned(FakePid, FakePid, 'Test', test_mod),
    ?assertMatch({error, {error, deliberate_crash}}, Result),

    exit(FakePid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).

register_spawned_surfaces_callback_error_result_test() ->
    %% Test that {error, _} from on_actor_spawned is returned, not swallowed
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    application:set_env(
        beamtalk_runtime, actor_spawn_callback, beamtalk_actor_tests_error_callback
    ),

    ok = ensure_error_callback_module(),

    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_actor:register_spawned(FakePid, FakePid, 'Test', test_mod),
    ?assertEqual({error, {registry_failed, registry_not_running}}, Result),

    exit(FakePid, kill),
    application:unset_env(beamtalk_runtime, actor_spawn_callback).

%%% ===========================================================================
%%% register_spawned/4 Callback Integration Tests (from main)
%%% ===========================================================================

register_spawned_invokes_callback_when_env_set_test() ->
    %% Register test process so test_spawn_callback can find us
    register(spawn_callback_test, self()),
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback),

    RegistryPid = self(),
    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
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
        end
    after
        exit(ActorPid, kill),
        application:unset_env(beamtalk_runtime, actor_spawn_callback),
        try
            unregister(spawn_callback_test)
        catch
            _:_ -> ok
        end
    end.

register_spawned_noop_when_env_unset_test() ->
    %% Ensure no callback env is set
    application:unset_env(beamtalk_runtime, actor_spawn_callback),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        %% Should return ok without error
        ?assertEqual(ok, beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter))
    after
        exit(ActorPid, kill)
    end.

register_spawned_returns_error_on_undef_callback_test() ->
    %% Set callback to a module that doesn't export on_actor_spawned/4
    %% beamtalk_actor_tests exists but doesn't have the function
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_actor_tests),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        %% Returns {error, _} since BT-391 surfaces callback failures
        ?assertMatch(
            {error, {callback_undef, beamtalk_actor_tests}},
            beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)
        )
    after
        exit(ActorPid, kill),
        application:unset_env(beamtalk_runtime, actor_spawn_callback)
    end.

register_spawned_returns_error_on_beamtalk_error_test() ->
    %% Set callback to a module that raises a #beamtalk_error{}
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback_bt_error),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        %% Returns {error, _} since BT-391 surfaces callback failures
        ?assertMatch(
            {error, {beamtalk_error, _}},
            beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)
        )
    after
        exit(ActorPid, kill),
        application:unset_env(beamtalk_runtime, actor_spawn_callback)
    end.

register_spawned_returns_error_on_generic_error_test() ->
    %% Set callback to a module that throws a generic error
    application:set_env(beamtalk_runtime, actor_spawn_callback, test_spawn_callback_generic_error),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        %% Returns {error, _} since BT-391 surfaces callback failures
        ?assertMatch(
            {error, _},
            beamtalk_actor:register_spawned(self(), ActorPid, 'Counter', test_counter)
        )
    after
        exit(ActorPid, kill),
        application:unset_env(beamtalk_runtime, actor_spawn_callback)
    end.

%%% ===========================================================================
%%% Test helpers for BT-391
%%% ===========================================================================

-doc "Create a callback module that crashes".
ensure_crash_callback_module() ->
    Forms = [
        {attribute, 1, module, beamtalk_actor_tests_crash_callback},
        {attribute, 2, export, [{on_actor_spawned, 4}]},
        {function, 3, on_actor_spawned, 4, [
            {clause, 3, [{var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}], [], [
                {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [
                    {atom, 3, deliberate_crash}
                ]}
            ]}
        ]}
    ],
    {ok, _, Bin} = compile:forms(Forms),
    {module, _} = code:load_binary(beamtalk_actor_tests_crash_callback, "test", Bin),
    ok.

-doc "Create a callback module that returns an error tuple".
ensure_error_callback_module() ->
    Forms = [
        {attribute, 1, module, beamtalk_actor_tests_error_callback},
        {attribute, 2, export, [{on_actor_spawned, 4}]},
        {function, 3, on_actor_spawned, 4, [
            {clause, 3, [{var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}, {var, 3, '_'}], [], [
                {tuple, 3, [
                    {atom, 3, error},
                    {tuple, 3, [{atom, 3, registry_failed}, {atom, 3, registry_not_running}]}
                ]}
            ]}
        ]}
    ],
    {ok, _, Bin} = compile:forms(Forms),
    {module, _} = code:load_binary(beamtalk_actor_tests_error_callback, "test", Bin),
    ok.

%%% ============================================================================
%%% Actor Lifecycle Tests (async_send/sync_send wrappers)
%%% ============================================================================

async_send_is_alive_on_live_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, isAlive, [], Future),
    Result = beamtalk_future:await(Future),
    ?assertEqual(true, Result),
    gen_server:stop(Counter).

async_send_is_alive_on_dead_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, isAlive, [], Future),
    Result = beamtalk_future:await(Future),
    ?assertEqual(false, Result).

async_send_stop_live_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, stop, [], Future),
    Result = beamtalk_future:await(Future),
    ?assertEqual(ok, Result),
    timer:sleep(10),
    ?assertNot(is_process_alive(Counter)).

async_send_stop_dead_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, stop, [], Future),
    Result = beamtalk_future:await(Future),
    ?assertEqual(ok, Result).

async_send_to_dead_actor_rejects_future_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, increment, [], Future),
    %% Future should be rejected with actor_dead error
    ?assertThrow({future_rejected, _}, beamtalk_future:await(Future, 500)).

%%% ============================================================================
%%% terminate/2 Tests
%%% ============================================================================

terminate_returns_ok_test() ->
    State = #{
        '$beamtalk_class' => 'TestActor',
        '__methods__' => #{}
    },
    ?assertEqual(ok, beamtalk_actor:terminate(normal, State)),
    ?assertEqual(ok, beamtalk_actor:terminate(shutdown, State)),
    ?assertEqual(ok, beamtalk_actor:terminate({error, reason}, State)).

%%% ============================================================================
%%% make_self/1 Tests
%%% ============================================================================

make_self_creates_object_ref_test() ->
    State = #{
        '$beamtalk_class' => 'Counter',
        '__methods__' => #{}
    },
    Self = beamtalk_actor:make_self(State),
    ?assertMatch(#beamtalk_object{class = 'Counter'}, Self).

%%% ============================================================================
%%% sync_send Tests
%%% ============================================================================

sync_send_is_alive_test() ->
    {ok, Counter} = test_counter:start_link(0),
    ?assertEqual(true, beamtalk_actor:sync_send(Counter, isAlive, [])),
    gen_server:stop(Counter),
    timer:sleep(10),
    ?assertEqual(false, beamtalk_actor:sync_send(Counter, isAlive, [])).

sync_send_stop_test() ->
    {ok, Counter} = test_counter:start_link(0),
    ?assertEqual(ok, beamtalk_actor:sync_send(Counter, stop, [])),
    timer:sleep(10),
    ?assertNot(is_process_alive(Counter)).

sync_send_stop_dead_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    ?assertEqual(ok, beamtalk_actor:sync_send(Counter, stop, [])).

sync_send_monitor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Ref = beamtalk_actor:sync_send(Counter, monitor, []),
    ?assert(is_reference(Ref)),
    gen_server:stop(Counter).

sync_send_to_dead_actor_test() ->
    %% BT-918: sync_send to a dead actor raises an actor_dead exception (not returns error tuple).
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_actor:sync_send(Counter, getValue, [])
    ).

%%% ============================================================================
%%% BT-1190: sync_send/4 (explicit timeout) tests
%%% ============================================================================

sync_send_4_succeeds_with_sufficient_timeout_test() ->
    {ok, Counter} = test_counter:start_link(42),
    %% slowGet: sleeps 50ms then returns value; 2000ms timeout is plenty
    Result = beamtalk_actor:sync_send(Counter, 'slowGet:', [50], 2000),
    ?assertEqual(42, Result),
    gen_server:stop(Counter).

sync_send_4_times_out_with_short_timeout_test() ->
    {ok, Counter} = test_counter:start_link(42),
    %% slowGet: sleeps 500ms; 50ms timeout should trigger timeout
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = timeout}},
        beamtalk_actor:sync_send(Counter, 'slowGet:', [500], 50)
    ),
    gen_server:stop(Counter).

sync_send_4_infinity_timeout_test() ->
    {ok, Counter} = test_counter:start_link(42),
    %% infinity timeout should never time out
    Result = beamtalk_actor:sync_send(Counter, 'slowGet:', [50], infinity),
    ?assertEqual(42, Result),
    gen_server:stop(Counter).

sync_send_4_dead_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_actor:sync_send(Counter, getValue, [], 5000)
    ).

%%% ============================================================================
%%% BT-917: cast_send/3 and fire-and-forget handle_cast wire format tests
%%% ============================================================================

cast_send_to_live_actor_returns_ok_test() ->
    {ok, Counter} = test_counter:start_link(0),
    %% cast_send always returns ok
    Result = beamtalk_actor:cast_send(Counter, increment, []),
    ?assertEqual(ok, Result),
    %% Give actor time to process the cast
    timer:sleep(20),
    %% Verify state was updated
    ?assertEqual(1, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

cast_send_to_dead_actor_returns_ok_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    %% Fire-and-forget: silently drops message if actor is dead
    Result = beamtalk_actor:cast_send(Counter, increment, []),
    ?assertEqual(ok, Result).

cast_send_state_mutation_test() ->
    {ok, Counter} = test_counter:start_link(10),
    %% Send multiple fire-and-forget increments
    ok = beamtalk_actor:cast_send(Counter, increment, []),
    ok = beamtalk_actor:cast_send(Counter, increment, []),
    ok = beamtalk_actor:cast_send(Counter, increment, []),
    timer:sleep(30),
    ?assertEqual(13, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

handle_cast_fire_and_forget_dispatches_test() ->
    %% Directly test the {cast, Selector, Args} wire format via gen_server:cast
    {ok, Counter} = test_counter:start_link(5),
    gen_server:cast(Counter, {cast, increment, []}),
    timer:sleep(20),
    ?assertEqual(6, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

handle_cast_fire_and_forget_result_discarded_test() ->
    %% {cast, Selector, Args} with a method that returns a value — result is discarded
    {ok, Counter} = test_counter:start_link(42),
    gen_server:cast(Counter, {cast, getValue, []}),
    timer:sleep(20),
    %% Actor still alive and state unchanged
    ?assertEqual(42, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

handle_cast_backward_compat_with_future_test() ->
    %% Old {Selector, Args, FuturePid} format still works
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    gen_server:cast(Counter, {increment, [], Future}),
    ?assertEqual(nil, beamtalk_future:await(Future)),
    ?assertEqual(1, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

handle_cast_fire_and_forget_unknown_selector_logs_and_continues_test() ->
    %% Unknown selector in fire-and-forget: logs warning, actor continues
    {ok, Counter} = test_counter:start_link(7),
    gen_server:cast(Counter, {cast, unknownMethod, []}),
    timer:sleep(20),
    %% Actor still alive and state unchanged
    ?assertEqual(7, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

%%% Regression tests for kill semantics (race-condition fix)
%%% These guard against the bug where exit(Pid, kill) + immediate ok
%%% meant is_process_alive could still return true on the next call.

async_send_kill_live_actor_resolves_ok_and_actor_is_dead_test() ->
    %% Use start/1 (not start_link) so kill does not propagate to the test process.
    {ok, Counter} = test_counter:start(0),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, kill, [], Future),
    Result = beamtalk_future:await(Future),
    %% kill must return ok only once the process is confirmed dead
    ?assertEqual(ok, Result),
    ?assertNot(is_process_alive(Counter)).

async_send_kill_dead_actor_resolves_ok_test() ->
    {ok, Counter} = test_counter:start(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, kill, [], Future),
    Result = beamtalk_future:await(Future),
    %% Killing an already-dead process is idempotent
    ?assertEqual(ok, Result).

sync_send_kill_live_actor_returns_ok_and_actor_is_dead_test() ->
    {ok, Counter} = test_counter:start(0),
    Result = beamtalk_actor:sync_send(Counter, kill, []),
    %% kill must return ok only once the process is confirmed dead
    ?assertEqual(ok, Result),
    ?assertNot(is_process_alive(Counter)).

sync_send_kill_dead_actor_returns_ok_test() ->
    {ok, Counter} = test_counter:start(0),
    gen_server:stop(Counter),
    timer:sleep(10),
    Result = beamtalk_actor:sync_send(Counter, kill, []),
    %% Killing an already-dead process is idempotent
    ?assertEqual(ok, Result).

async_send_kill_isAlive_false_immediately_after_test() ->
    %% This is the exact sequence that was flaky before the fix.
    %% kill must guarantee is_process_alive is false by the time it resolves.
    {ok, Counter} = test_counter:start(0),
    KillFuture = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, kill, [], KillFuture),
    ok = beamtalk_future:await(KillFuture),
    %% No sleep — process must be dead synchronously after kill resolves
    AliveFuture = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, isAlive, [], AliveFuture),
    ?assertEqual(false, beamtalk_future:await(AliveFuture)).

%%% BT-1541: await_initialize / safe_spawn tests

await_initialize_alive_process_test() ->
    %% await_initialize returns ok for a healthy gen_server
    {ok, Pid} = test_counter:start(0),
    ?assertEqual(ok, beamtalk_actor:await_initialize(Pid)),
    gen_server:stop(Pid).

await_initialize_dead_process_test() ->
    %% await_initialize returns {error, _} for a dead process
    {ok, Pid} = test_counter:start(0),
    gen_server:stop(Pid),
    timer:sleep(10),
    ?assertMatch({error, _}, beamtalk_actor:await_initialize(Pid)).

safe_spawn_success_test() ->
    %% safe_spawn returns {ok, Pid} for a healthy module
    {ok, Pid} = beamtalk_actor:safe_spawn(test_counter, #{init_count => 0}),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).

await_initialize_preserves_full_stop_reason_test() ->
    %% When handle_continue returns {stop, {error, function_clause}, State},
    %% await_initialize must return {error, {error, function_clause}} — not
    %% {error, error} (the bug before the pattern-match fix).
    %% Use gen_server:start (not start_link) to avoid EXIT signal killing the test.
    {ok, Pid} = gen_server:start(test_crashing_init_actor, #{}, []),
    Result = beamtalk_actor:await_initialize(Pid),
    ?assertMatch({error, {error, function_clause}}, Result).

safe_spawn_restores_trap_exit_test() ->
    %% safe_spawn restores the original trap_exit flag
    OldTrap = process_flag(trap_exit, false),
    try
        {ok, Pid} = beamtalk_actor:safe_spawn(test_counter, #{init_count => 0}),
        try
            ?assertEqual(false, process_flag(trap_exit, false))
        after
            catch gen_server:stop(Pid)
        end
    after
        process_flag(trap_exit, OldTrap)
    end.

%%% ============================================================================
%%% BT-1822: Stacktrace preservation tests
%%% ============================================================================

spawn_callback_crash_log_includes_stacktrace_test() ->
    %% Install a capturing logger handler to verify stacktrace is logged
    Parent = self(),
    HandlerId = bt_1822_actor_test_handler,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => Parent},
        level => all
    }),
    try
        application:set_env(
            beamtalk_runtime, actor_spawn_callback, beamtalk_actor_tests_crash_callback
        ),
        ok = ensure_crash_callback_module(),

        FakePid = spawn(fun() ->
            receive
                _ -> ok
            end
        end),
        _Result = beamtalk_actor:register_spawned(FakePid, FakePid, 'Test', test_mod),
        exit(FakePid, kill),

        %% Collect log events and find the specific callback failure with stacktrace
        Found = collect_log_with_stacktrace("Actor spawn callback failed", 500),
        ?assertMatch({ok, _}, Found),
        {ok, ST} = Found,
        ?assert(is_list(ST)),
        ?assert(length(ST) > 0)
    after
        application:unset_env(beamtalk_runtime, actor_spawn_callback),
        logger:remove_handler(HandlerId)
    end.

-doc """
Collect log events until we find one matching the expected message with stacktrace
""".
collect_log_with_stacktrace(ExpectedMsg, Timeout) ->
    collect_log_with_stacktrace(ExpectedMsg, Timeout, erlang:monotonic_time(millisecond)).

collect_log_with_stacktrace(ExpectedMsg, Timeout, Start) ->
    Remaining = Timeout - (erlang:monotonic_time(millisecond) - Start),
    case Remaining > 0 of
        false ->
            not_found;
        true ->
            receive
                {log_event, #{msg := {string, Msg}, meta := #{stacktrace := ST}}} when
                    Msg =:= ExpectedMsg
                ->
                    {ok, ST};
                {log_event, _} ->
                    collect_log_with_stacktrace(ExpectedMsg, Timeout, Start)
            after Remaining ->
                not_found
            end
    end.

%%% ============================================================================
%%% BT-1958: Trace context propagation and restoration tests
%%% ============================================================================

set_and_get_trace_context_test() ->
    %% set_trace_context stores a map in the process dictionary
    beamtalk_actor:clear_trace_context(),
    beamtalk_actor:set_trace_context(#{workflowId => <<"wf-1">>}),
    Ctx = beamtalk_actor:get_trace_context(),
    ?assertEqual(#{workflowId => <<"wf-1">>}, Ctx),
    beamtalk_actor:clear_trace_context().

get_trace_context_empty_when_unset_test() ->
    %% get_trace_context returns #{} when no context is set
    erase('$beamtalk_trace_ctx'),
    ?assertEqual(#{}, beamtalk_actor:get_trace_context()).

get_trace_context_ignores_non_map_test() ->
    %% get_trace_context returns #{} when pdict has a non-map value
    put('$beamtalk_trace_ctx', not_a_map),
    ?assertEqual(#{}, beamtalk_actor:get_trace_context()),
    erase('$beamtalk_trace_ctx').

clear_trace_context_removes_keys_test() ->
    %% clear_trace_context removes the context and logger metadata keys
    beamtalk_actor:set_trace_context(#{myKey => <<"val">>}),
    ?assertEqual(#{myKey => <<"val">>}, beamtalk_actor:get_trace_context()),
    beamtalk_actor:clear_trace_context(),
    ?assertEqual(#{}, beamtalk_actor:get_trace_context()).

set_trace_context_merges_test() ->
    %% set_trace_context merges with existing context
    beamtalk_actor:clear_trace_context(),
    beamtalk_actor:set_trace_context(#{a => 1}),
    beamtalk_actor:set_trace_context(#{b => 2}),
    ?assertEqual(#{a => 1, b => 2}, beamtalk_actor:get_trace_context()),
    beamtalk_actor:clear_trace_context().

%%% ============================================================================
%%% BT-1958: Causal trace context tests
%%% ============================================================================

get_causal_ctx_empty_when_unset_test() ->
    erase('$beamtalk_trace_id'),
    erase('$beamtalk_span_id'),
    erase('$beamtalk_parent_span_id'),
    ?assertEqual(#{}, beamtalk_actor:get_causal_ctx()).

get_causal_ctx_with_trace_id_only_test() ->
    put('$beamtalk_trace_id', 42),
    erase('$beamtalk_span_id'),
    erase('$beamtalk_parent_span_id'),
    ?assertEqual(#{trace_id => 42}, beamtalk_actor:get_causal_ctx()),
    erase('$beamtalk_trace_id').

get_causal_ctx_with_all_ids_test() ->
    put('$beamtalk_trace_id', 100),
    put('$beamtalk_span_id', 200),
    put('$beamtalk_parent_span_id', 50),
    Expected = #{trace_id => 100, span_id => 200, parent_span_id => 50},
    ?assertEqual(Expected, beamtalk_actor:get_causal_ctx()),
    erase('$beamtalk_trace_id'),
    erase('$beamtalk_span_id'),
    erase('$beamtalk_parent_span_id').

%%% ============================================================================
%%% BT-1958: Propagated context tests
%%% ============================================================================

get_propagated_ctx_includes_trace_context_test() ->
    beamtalk_actor:clear_trace_context(),
    erase('$beamtalk_trace_id'),
    beamtalk_actor:set_trace_context(#{reqId => <<"r-1">>}),
    Ctx = beamtalk_actor:get_propagated_ctx(),
    ?assertEqual(#{reqId => <<"r-1">>}, maps:get(trace_ctx, Ctx)),
    beamtalk_actor:clear_trace_context().

get_propagated_ctx_includes_causal_when_set_test() ->
    beamtalk_actor:clear_trace_context(),
    put('$beamtalk_trace_id', 42),
    put('$beamtalk_span_id', 99),
    Ctx = beamtalk_actor:get_propagated_ctx(),
    ?assertEqual(#{trace_id => 42, span_id => 99}, maps:get(causal, Ctx)),
    erase('$beamtalk_trace_id'),
    erase('$beamtalk_span_id').

get_propagated_ctx_no_causal_when_unset_test() ->
    erase('$beamtalk_trace_id'),
    Ctx = beamtalk_actor:get_propagated_ctx(),
    ?assertEqual(false, maps:is_key(causal, Ctx)).

%%% ============================================================================
%%% BT-1958: restore_propagated_ctx tests
%%% ============================================================================

restore_propagated_ctx_restores_trace_context_test() ->
    beamtalk_actor:clear_trace_context(),
    PropCtx = #{
        otel => undefined,
        trace_ctx => #{workflowId => <<"wf-restore">>}
    },
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual(#{workflowId => <<"wf-restore">>}, beamtalk_actor:get_trace_context()),
    beamtalk_actor:clear_trace_context().

restore_propagated_ctx_clears_on_empty_trace_ctx_test() ->
    %% When trace_ctx is #{} in propagated context, existing context is cleared
    beamtalk_actor:set_trace_context(#{stale => <<"should-be-cleared">>}),
    PropCtx = #{otel => undefined, trace_ctx => #{}},
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual(#{}, beamtalk_actor:get_trace_context()).

restore_propagated_ctx_restores_causal_context_test() ->
    PropCtx = #{
        otel => undefined,
        trace_ctx => #{},
        causal => #{trace_id => 10, span_id => 20}
    },
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual(10, get('$beamtalk_trace_id')),
    ?assertEqual(20, get('$beamtalk_parent_span_id')),
    erase('$beamtalk_trace_id'),
    erase('$beamtalk_parent_span_id').

restore_propagated_ctx_clears_stale_causal_context_test() ->
    %% When no causal key in PropCtx, stale IDs are erased
    put('$beamtalk_trace_id', 999),
    put('$beamtalk_span_id', 888),
    PropCtx = #{otel => undefined, trace_ctx => #{}},
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual(undefined, get('$beamtalk_trace_id')),
    ?assertEqual(undefined, get('$beamtalk_span_id')).

restore_propagated_ctx_restores_call_stack_test() ->
    FakePid = list_to_pid("<0.999.0>"),
    PropCtx = #{
        otel => undefined,
        trace_ctx => #{},
        call_stack => [FakePid]
    },
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual([FakePid], get('$bt_call_stack')),
    erase('$bt_call_stack').

restore_propagated_ctx_erases_call_stack_when_absent_test() ->
    put('$bt_call_stack', [list_to_pid("<0.111.0>")]),
    PropCtx = #{otel => undefined, trace_ctx => #{}},
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    ?assertEqual(undefined, get('$bt_call_stack')).

restore_propagated_ctx_non_map_is_noop_test() ->
    %% Non-map argument is silently ignored
    beamtalk_actor:restore_propagated_ctx(not_a_map),
    ok.

%%% ============================================================================
%%% BT-1958: sync_send/4 invalid timeout test
%%% ============================================================================

sync_send_4_invalid_timeout_raises_type_error_test() ->
    {ok, Counter} = test_counter:start_link(0),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
            beamtalk_actor:sync_send(Counter, getValue, [], negative_atom)
        ),
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
            beamtalk_actor:sync_send(Counter, getValue, [], -1)
        )
    after
        gen_server:stop(Counter)
    end.

%%% ============================================================================
%%% BT-1958: sync_send delegate raises signal error
%%% ============================================================================

sync_send_delegate_raises_signal_error_test() ->
    {ok, Counter} = test_counter:start_link(0),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = signal}},
            beamtalk_actor:sync_send(Counter, delegate, [])
        )
    after
        gen_server:stop(Counter)
    end.

%%% ============================================================================
%%% BT-1958: async_send delegate rejects future
%%% ============================================================================

async_send_delegate_rejects_future_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, delegate, [], Future),
    ?assertThrow(
        {future_rejected, #beamtalk_error{kind = signal}},
        beamtalk_future:await(Future, 1000)
    ),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: start_link/3 named registration test
%%% ============================================================================

start_link_3_named_registration_test() ->
    Name = {local, test_named_actor_bt1958},
    {ok, Pid} = beamtalk_actor:start_link(Name, test_counter, 77),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(test_named_actor_bt1958)),
    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(77, Result),
    gen_server:stop(Pid).

%%% ============================================================================
%%% BT-1958: start_link_supervised/3 test
%%% ============================================================================

start_link_supervised_test() ->
    {ok, Pid} = beamtalk_actor:start_link_supervised(test_counter, start_link, [33]),
    ?assert(is_pid(Pid)),
    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(33, Result),
    gen_server:stop(Pid).

%%% ============================================================================
%%% BT-1958: dispatch/4 delegate returns signal error
%%% ============================================================================

dispatch_delegate_returns_signal_error_test() ->
    State = #{
        '$beamtalk_class' => 'TestActor',
        '__methods__' => #{}
    },
    Self = beamtalk_actor:make_self(State),
    Result = beamtalk_actor:dispatch(delegate, [], Self, State),
    ?assertMatch({error, #beamtalk_error{kind = signal}, _}, Result).

%%% ============================================================================
%%% BT-1958: dispatch/4 perform:withArguments:timeout: tests
%%% ============================================================================

dispatch_perform_with_timeout_valid_test() ->
    {ok, Counter} = test_counter:start_link(99),
    %% perform:withArguments:timeout: re-dispatches through dispatch/4
    Result = gen_server:call(Counter, {'perform:withArguments:timeout:', [getValue, [], 5000]}),
    ?assertEqual(99, Result),
    gen_server:stop(Counter).

dispatch_perform_with_timeout_invalid_args_test() ->
    {ok, Counter} = test_counter:start_link(0),
    %% Non-atom selector
    Result1 = gen_server:call(Counter, {'perform:withArguments:timeout:', ["badSel", [], 5000]}),
    ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result1),
    %% Negative timeout
    Result2 = gen_server:call(Counter, {'perform:withArguments:timeout:', [getValue, [], -1]}),
    ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result2),
    %% Non-list args
    Result3 = gen_server:call(
        Counter, {'perform:withArguments:timeout:', [getValue, notAList, 5000]}
    ),
    ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result3),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: handle_cast fire-and-forget error path
%%% ============================================================================

handle_cast_fire_and_forget_error_does_not_crash_actor_test() ->
    %% fire-and-forget cast with a throwing method logs error, actor continues
    {ok, Actor} = test_throwing_actor:start_link(),
    gen_server:cast(Actor, {cast, throwError, []}),
    timer:sleep(30),
    %% Actor should still be alive
    ?assert(is_process_alive(Actor)),
    %% And still responsive
    NormalResult = gen_server:call(Actor, {normalMethod, []}),
    ?assertEqual(ok, NormalResult),
    gen_server:stop(Actor).

%%% ============================================================================
%%% BT-1958: handle_call/handle_cast with propagated context wire format
%%% ============================================================================

handle_call_with_propagated_context_test() ->
    %% Test the {Selector, Args, PropCtx} wire format for sync calls
    {ok, Counter} = test_counter:start_link(42),
    PropCtx = #{otel => undefined, trace_ctx => #{testKey => <<"test">>}},
    Result = gen_server:call(Counter, {getValue, [], PropCtx}),
    %% The call should succeed normally (PropCtx is consumed by restore_propagated_ctx)
    ?assertEqual(42, Result),
    gen_server:stop(Counter).

handle_cast_with_propagated_context_future_test() ->
    %% Test the {Selector, Args, FuturePid, PropCtx} wire format for async casts
    {ok, Counter} = test_counter:start_link(0),
    Future = beamtalk_future:new(),
    PropCtx = #{otel => undefined, trace_ctx => #{}},
    gen_server:cast(Counter, {increment, [], Future, PropCtx}),
    ?assertEqual(nil, beamtalk_future:await(Future)),
    ?assertEqual(1, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

handle_cast_fire_and_forget_with_propagated_context_test() ->
    %% Test the {cast, Selector, Args, PropCtx} wire format
    {ok, Counter} = test_counter:start_link(0),
    PropCtx = #{otel => undefined, trace_ctx => #{}},
    gen_server:cast(Counter, {cast, increment, [], PropCtx}),
    timer:sleep(20),
    ?assertEqual(1, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: safe_spawn error and ignore paths
%%% ============================================================================

safe_spawn_error_from_start_link_test() ->
    %% safe_spawn returns {error, Reason} when gen_server:start_link fails
    %% Use a module that doesn't exist to trigger an error
    Result = beamtalk_actor:safe_spawn(nonexistent_module_bt1958, #{}),
    ?assertMatch({error, _}, Result).

safe_spawn_restores_trap_exit_on_error_test() ->
    %% safe_spawn restores the original trap_exit flag even on error
    OldTrap = process_flag(trap_exit, false),
    try
        _Result = beamtalk_actor:safe_spawn(nonexistent_module_bt1958, #{}),
        ?assertEqual(false, process_flag(trap_exit, false))
    after
        process_flag(trap_exit, OldTrap)
    end.

%%% ============================================================================
%%% BT-1958: make_self class_mod handling
%%% ============================================================================

make_self_with_class_mod_test() ->
    State = #{
        '$beamtalk_class' => 'MyClass',
        '__class_mod__' => my_class_mod,
        '__methods__' => #{}
    },
    Self = beamtalk_actor:make_self(State),
    ?assertEqual('MyClass', Self#beamtalk_object.class),
    ?assertEqual(my_class_mod, Self#beamtalk_object.class_mod).

make_self_without_class_mod_test() ->
    State = #{
        '$beamtalk_class' => 'MyClass',
        '__methods__' => #{}
    },
    Self = beamtalk_actor:make_self(State),
    ?assertEqual('MyClass', Self#beamtalk_object.class),
    ?assertEqual(undefined, Self#beamtalk_object.class_mod).

%%% ============================================================================
%%% BT-1958: format_method_error_message coverage (via dispatch)
%%% ============================================================================

method_badarg_error_test() ->
    %% Trigger a badarg error in a method to cover format_method_error_message badarg path
    {ok, Actor} = test_badarg_actor:start_link(),
    Result = gen_server:call(Actor, {triggerBadarg, []}),
    ?assertMatch({error, #beamtalk_error{kind = runtime_error, selector = triggerBadarg}}, Result),
    %% Message should contain "Bad argument"
    {error, Err} = Result,
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Bad argument">>)),
    gen_server:stop(Actor).

method_badarith_error_test() ->
    %% Trigger a badarith error to cover format_method_error_message badarith path
    {ok, Actor} = test_badarg_actor:start_link(),
    Result = gen_server:call(Actor, {triggerBadarith, []}),
    ?assertMatch(
        {error, #beamtalk_error{kind = runtime_error, selector = triggerBadarith}}, Result
    ),
    {error, Err} = Result,
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Arithmetic error">>)),
    gen_server:stop(Actor).

method_undef_error_test() ->
    %% Trigger an undef error to cover format_method_error_message undef path
    {ok, Actor} = test_badarg_actor:start_link(),
    Result = gen_server:call(Actor, {triggerUndef, []}),
    ?assertMatch({error, #beamtalk_error{kind = runtime_error, selector = triggerUndef}}, Result),
    {error, Err} = Result,
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"Undefined function">>)),
    gen_server:stop(Actor).

method_function_clause_error_test() ->
    %% Trigger a function_clause error to cover that path
    {ok, Actor} = test_badarg_actor:start_link(),
    Result = gen_server:call(Actor, {triggerFunctionClause, []}),
    ?assertMatch(
        {error, #beamtalk_error{kind = runtime_error, selector = triggerFunctionClause}}, Result
    ),
    {error, Err} = Result,
    ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"No matching clause">>)),
    gen_server:stop(Actor).

%%% ============================================================================
%%% BT-1958: respondsTo: for delegate selector
%%% ============================================================================

respondsTo_delegate_test() ->
    application:ensure_all_started(beamtalk_runtime),
    {ok, Counter} = test_counter:start_link(0),
    ?assertEqual(true, gen_server:call(Counter, {'respondsTo:', [delegate]})),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: Concurrent async error + future watcher tests
%%% ============================================================================

async_error_in_method_rejects_future_test() ->
    %% Async cast to a method that throws should reject the future
    {ok, Actor} = test_throwing_actor:start_link(),
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {throwError, [], Future}),
    ?assertThrow(
        {future_rejected, #beamtalk_error{kind = runtime_error}},
        beamtalk_future:await(Future, 1000)
    ),
    %% Actor should survive after the error
    ?assert(is_process_alive(Actor)),
    gen_server:stop(Actor).

%%% ============================================================================
%%% BT-1958: handle_call with unknown message format
%%% ============================================================================

handle_call_unknown_format_returns_dnu_error_test() ->
    %% Sending a non-tuple message via gen_server:call triggers the catch-all clause
    {ok, Counter} = test_counter:start_link(0),
    Result = gen_server:call(Counter, just_an_atom),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
    %% Actor should still work
    ?assertEqual(0, gen_server:call(Counter, {getValue, []})),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: terminate/2 with various reasons
%%% ============================================================================

terminate_with_different_reasons_test() ->
    %% Test terminate with various stop reasons
    State = #{
        '$beamtalk_class' => 'TestActor',
        '__methods__' => #{}
    },
    ?assertEqual(ok, beamtalk_actor:terminate(normal, State)),
    ?assertEqual(ok, beamtalk_actor:terminate(shutdown, State)),
    ?assertEqual(ok, beamtalk_actor:terminate({shutdown, timeout}, State)),
    ?assertEqual(ok, beamtalk_actor:terminate(killed, State)).

%%% ============================================================================
%%% BT-1958: code_change/3 delegates to hot_reload
%%% ============================================================================

code_change_preserves_state_unit_test() ->
    State = #{
        '$beamtalk_class' => 'TestActor',
        '__methods__' => #{},
        value => 42
    },
    {ok, NewState} = beamtalk_actor:code_change("1.0.0", State, []),
    ?assertEqual(State, NewState).

%%% ============================================================================
%%% BT-1958: sync_send/3 exit catch-all paths
%%% ============================================================================

sync_send_to_shutdown_actor_raises_actor_dead_test() ->
    %% When an actor stops with shutdown reason during a call, we get actor_dead
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, shutdown, 1000),
    timer:sleep(10),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = actor_dead}},
        beamtalk_actor:sync_send(Counter, getValue, [])
    ).

%%% ============================================================================
%%% BT-1958: lookup_class returns unknown for non-actor pids
%%% ============================================================================

lookup_class_unknown_for_non_actor_test() ->
    %% A regular process (not an actor) should return 'unknown' from lookup_class
    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    try
        %% We can't call lookup_class directly (it's not exported as a test helper)
        %% but we can test it indirectly via cast_send which uses it
        ok = beamtalk_actor:cast_send(FakePid, someMsg, [])
    after
        exit(FakePid, kill)
    end.

%%% ============================================================================
%%% BT-1958: sync_send pid returns the raw PID
%%% ============================================================================

sync_send_pid_returns_raw_pid_test() ->
    {ok, Counter} = test_counter:start_link(0),
    Result = beamtalk_actor:sync_send(Counter, pid, []),
    ?assertEqual(Counter, Result),
    gen_server:stop(Counter).

%%% ============================================================================
%%% BT-1958: async_send onExit: with actor that's already dead
%%% ============================================================================

async_send_onExit_already_dead_actor_test() ->
    {ok, Counter} = test_counter:start_link(0),
    gen_server:stop(Counter, normal, 1000),
    timer:sleep(10),
    Self = self(),
    Block = fun(Reason) -> Self ! {exit_reason_already_dead, Reason} end,
    Future = beamtalk_future:new(),
    beamtalk_actor:async_send(Counter, 'onExit:', [Block], Future),
    %% The watcher should get a DOWN immediately and call the block
    ?assertEqual(ok, beamtalk_future:await(Future, 2000)),
    receive
        {exit_reason_already_dead, _Reason} -> ok
    after 2000 ->
        ?assert(false)
    end.

%%% ============================================================================
%%% BT-1958: sync_send onExit: fires block on death
%%% ============================================================================

sync_send_onExit_with_kill_reason_test() ->
    {ok, Counter} = test_counter:start(0),
    Self = self(),
    Block = fun(Reason) -> Self ! {kill_exit_reason, Reason} end,
    beamtalk_actor:sync_send(Counter, 'onExit:', [Block]),
    exit(Counter, kill),
    receive
        {kill_exit_reason, killed} -> ok
    after 2000 ->
        ?assert(false)
    end.
