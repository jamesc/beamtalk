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

%%% Tests

%%% Initialization tests

init_with_valid_state_test() ->
    State = #{
        '__class__' => 'TestActor',
        '__methods__' => #{test => fun(_Args, S) -> {reply, ok, S} end},
        data => 123
    },
    ?assertEqual({ok, State}, beamtalk_actor:init(State)).

init_without_class_test() ->
    State = #{
        '__methods__' => #{test => fun(_Args, S) -> {reply, ok, S} end}
    },
    ?assertEqual({stop, {missing_key, '__class__'}}, beamtalk_actor:init(State)).

init_without_methods_test() ->
    State = #{
        '__class__' => 'TestActor'
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
    ?assertMatch({error, {unknown_message, unknownMethod}}, Result),
    gen_server:stop(Counter).

malformed_call_message_test() ->
    {ok, Counter} = test_counter:start_link(0),
    %% Send a call message that doesn't match the expected {Selector, Args} format
    Result = gen_server:call(Counter, malformed_message),
    ?assertMatch({error, {unknown_call_format, malformed_message}}, Result),
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
    ?assertMatch({error, {dnu_handler_error, _}}, Result),
    
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
    Pid = beamtalk_actor:spawn_actor(test_counter, 15),
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
    ?assertMatch({error, {method_error, throwError, intentional_error}}, Result),
    
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
    Result = beamtalk_future:await(Future, 1000),
    ?assertMatch({error, {method_error, throwError, intentional_error}}, Result),
    
    %% Verify actor still works after exception
    NormalFuture = beamtalk_future:new(),
    gen_server:cast(Actor, {normalMethod, [], NormalFuture}),
    NormalResult = beamtalk_future:await(NormalFuture, 1000),
    ?assertEqual({ok, ok}, NormalResult),
    
    gen_server:stop(Actor).

invalid_method_not_function_sync_test() ->
    {ok, Actor} = test_invalid_method_actor:start_link(),
    
    %% Try to call a method that's not a function
    Result = gen_server:call(Actor, {notAFunction, []}),
    ?assertMatch({error, {invalid_method, notAFunction}}, Result),
    
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
    Result = beamtalk_future:await(Future, 1000),
    ?assertMatch({error, {invalid_method, notAFunction}}, Result),
    
    gen_server:stop(Actor).

%%% Stress tests

stress_many_actors_test() ->
    %% Create 50 actors and verify they all work independently
    NumActors = 50,
    Actors = [beamtalk_actor:spawn_actor(test_counter, N) || N <- lists:seq(1, NumActors)],
    
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
