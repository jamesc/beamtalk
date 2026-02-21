%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_future module
%%%
%%% Tests all future behaviors:
%%% - Basic resolve/await
%%% - Basic reject/await
%%% - Multiple waiters
%%% - Callbacks
%%% - Timeout handling
%%% - Edge cases

-module(beamtalk_future_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% Basic resolve/await tests

resolve_then_await_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, 42),
    Result = beamtalk_future:await(Future),
    ?assertEqual(42, Result).

await_then_resolve_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),

    %% Spawn a process that waits on the future
    spawn(fun() ->
        Result = beamtalk_future:await(Future),
        Parent ! {result, Result}
    end),

    %% Give it a moment to register as a waiter
    timer:sleep(10),

    %% Now resolve the future
    beamtalk_future:resolve(Future, hello),

    %% Check that the waiter got the result
    receive
        {result, Value} -> ?assertEqual(hello, Value)
    after 1000 ->
        % Timeout - test failed
        ?assert(false)
    end.

%%% Basic reject/await tests

reject_then_await_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, test_error),

    ?assertThrow({future_rejected, test_error}, beamtalk_future:await(Future)).

await_then_reject_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),

    %% Spawn a process that waits on the future
    spawn(fun() ->
        try
            beamtalk_future:await(Future)
        catch
            throw:{future_rejected, Reason} ->
                Parent ! {rejected, Reason}
        end
    end),

    %% Give it a moment to register as a waiter
    timer:sleep(10),

    %% Now reject the future
    beamtalk_future:reject(Future, oops),

    %% Check that the waiter got the rejection
    receive
        {rejected, Reason} -> ?assertEqual(oops, Reason)
    after 1000 ->
        % Timeout - test failed
        ?assert(false)
    end.

%%% Multiple waiters tests

multiple_waiters_resolved_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),

    %% Spawn 5 waiters
    NumWaiters = 5,
    lists:foreach(
        fun(N) ->
            spawn(fun() ->
                Result = beamtalk_future:await(Future),
                Parent ! {waiter, N, Result}
            end)
        end,
        lists:seq(1, NumWaiters)
    ),

    %% Give them time to register
    timer:sleep(50),

    %% Resolve the future
    beamtalk_future:resolve(Future, shared_value),

    %% Collect results from all waiters
    Results = lists:map(
        fun(_) ->
            receive
                {waiter, _, Value} -> Value
            after 1000 ->
                timeout
            end
        end,
        lists:seq(1, NumWaiters)
    ),

    %% All waiters should have received the value
    ?assertEqual([shared_value, shared_value, shared_value, shared_value, shared_value], Results).

multiple_waiters_rejected_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),

    %% Spawn 3 waiters
    NumWaiters = 3,
    lists:foreach(
        fun(N) ->
            spawn(fun() ->
                try
                    beamtalk_future:await(Future)
                catch
                    throw:{future_rejected, Reason} ->
                        Parent ! {waiter, N, {rejected, Reason}}
                end
            end)
        end,
        lists:seq(1, NumWaiters)
    ),

    %% Give them time to register
    timer:sleep(50),

    %% Reject the future
    beamtalk_future:reject(Future, shared_error),

    %% Collect results from all waiters
    Results = lists:map(
        fun(_) ->
            receive
                {waiter, _, Result} -> Result
            after 1000 ->
                timeout
            end
        end,
        lists:seq(1, NumWaiters)
    ),

    %% All waiters should have received the rejection
    Expected = [{rejected, shared_error}, {rejected, shared_error}, {rejected, shared_error}],
    ?assertEqual(Expected, Results).

%%% Callback tests

callback_after_resolve_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, 100),

    Parent = self(),
    beamtalk_future:when_resolved(Future, fun(Value) ->
        Parent ! {callback_called, Value}
    end),

    %% Callback should execute immediately since future is already resolved
    receive
        {callback_called, Value} -> ?assertEqual(100, Value)
    after 1000 ->
        % Timeout - callback wasn't called
        ?assert(false)
    end.

callback_before_resolve_test() ->
    Future = beamtalk_future:new(),

    Parent = self(),
    beamtalk_future:when_resolved(Future, fun(Value) ->
        Parent ! {callback_called, Value}
    end),

    %% Resolve the future
    beamtalk_future:resolve(Future, 200),

    %% Callback should execute now
    receive
        {callback_called, Value} -> ?assertEqual(200, Value)
    after 1000 ->
        % Timeout - callback wasn't called
        ?assert(false)
    end.

callback_rejected_test() ->
    Future = beamtalk_future:new(),

    Parent = self(),
    beamtalk_future:when_rejected(Future, fun(Reason) ->
        Parent ! {error_callback_called, Reason}
    end),

    %% Reject the future
    beamtalk_future:reject(Future, error_reason),

    %% Error callback should execute
    receive
        {error_callback_called, Reason} -> ?assertEqual(error_reason, Reason)
    after 1000 ->
        % Timeout - callback wasn't called
        ?assert(false)
    end.

callback_wrong_state_test() ->
    Future = beamtalk_future:new(),

    Parent = self(),
    %% Register a rejected callback
    beamtalk_future:when_rejected(Future, fun(Reason) ->
        Parent ! {error_callback_called, Reason}
    end),

    %% But resolve instead of rejecting
    beamtalk_future:resolve(Future, success),

    %% The rejected callback should NOT be called
    receive
        % Should not happen
        {error_callback_called, _} -> ?assert(false)
    after 100 ->
        % Expected - callback wasn't called
        ok
    end.

%%% Timeout tests

await_with_timeout_resolved_test() ->
    Future = beamtalk_future:new(),

    %% Spawn a task that resolves the future after 50ms
    spawn(fun() ->
        timer:sleep(50),
        beamtalk_future:resolve(Future, quick)
    end),

    %% Await with a 500ms timeout (should succeed)
    Result = beamtalk_future:await(Future, 500),
    ?assertEqual(quick, Result).

await_with_timeout_expired_test() ->
    Future = beamtalk_future:new(),

    %% Don't resolve the future
    %% Await with a 50ms timeout (should timeout with structured error)
    ?assertThrow(
        #beamtalk_error{
            kind = timeout,
            class = 'Future',
            message = <<"Await timed out">>
        },
        beamtalk_future:await(Future, 50)
    ).

await_with_timeout_rejected_test() ->
    Future = beamtalk_future:new(),

    %% Spawn a task that rejects the future after 50ms
    spawn(fun() ->
        timer:sleep(50),
        beamtalk_future:reject(Future, failed)
    end),

    %% Await with a 500ms timeout (should get rejection, not timeout)
    ?assertThrow({future_rejected, failed}, beamtalk_future:await(Future, 500)).

await_default_timeout_test() ->
    Future = beamtalk_future:new(),

    %% Spawn a task that resolves the future after 100ms
    spawn(fun() ->
        timer:sleep(100),
        beamtalk_future:resolve(Future, within_default_timeout)
    end),

    %% Await without explicit timeout (should use 30s default and succeed)
    Result = beamtalk_future:await(Future),
    ?assertEqual(within_default_timeout, Result).

await_forever_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),

    %% Spawn a task that resolves after 200ms
    spawn(fun() ->
        timer:sleep(200),
        beamtalk_future:resolve(Future, late_value)
    end),

    %% Spawn awaiter with await_forever (should wait indefinitely)
    spawn(fun() ->
        Result = beamtalk_future:await_forever(Future),
        Parent ! {result, Result}
    end),

    %% Should receive the result even though it takes a while
    receive
        {result, Value} -> ?assertEqual(late_value, Value)
    after 500 ->
        ?assert(false andalso "Test failed - should have received result")
    end.

%%% Edge cases

double_resolve_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, first),
    % Should be ignored
    beamtalk_future:resolve(Future, second),

    Result = beamtalk_future:await(Future),
    % Should get the first value
    ?assertEqual(first, Result).

resolve_after_reject_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, error),
    % Should be ignored
    beamtalk_future:resolve(Future, value),

    ?assertThrow({future_rejected, error}, beamtalk_future:await(Future)).

reject_after_resolve_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, value),
    % Should be ignored
    beamtalk_future:reject(Future, error),

    Result = beamtalk_future:await(Future),
    ?assertEqual(value, Result).

%%% Garbage collection test

future_gc_test() ->
    %% Create a future and let it go out of scope
    Future = beamtalk_future:new(),
    ?assert(is_process_alive(Future)),

    %% Resolve it
    beamtalk_future:resolve(Future, done),

    %% The process should still be alive (in resolved state)
    ?assert(is_process_alive(Future)),

    %% When all references are dropped, the process will be GC'd by BEAM
    %% We can't directly test this, but we can verify the process exists
    %% and behaves correctly

    %% Await should still work
    Result = beamtalk_future:await(Future),
    ?assertEqual(done, Result),
    ?assert(is_process_alive(Future)).

%%% Stress test

stress_test_many_futures_test() ->
    %% Create many futures and verify they all work
    NumFutures = 100,
    Futures = [beamtalk_future:new() || _ <- lists:seq(1, NumFutures)],

    %% Resolve all of them with their index
    lists:foreach(
        fun({Future, Index}) ->
            beamtalk_future:resolve(Future, Index)
        end,
        lists:zip(Futures, lists:seq(1, NumFutures))
    ),

    %% Await all of them
    Results = [beamtalk_future:await(F) || F <- Futures],

    %% Verify results
    ?assertEqual(lists:seq(1, NumFutures), Results).

%%% Additional edge case tests

multiple_awaiters_on_same_future_test() ->
    %% Test multiple processes awaiting the same future
    Future = beamtalk_future:new(),
    Parent = self(),
    NumWaiters = 10,

    %% Spawn multiple processes that wait on the same future
    Pids = [
        spawn(fun() ->
            Result = beamtalk_future:await(Future),
            Parent ! {waiter_done, self(), Result}
        end)
     || _ <- lists:seq(1, NumWaiters)
    ],

    %% Give them time to register as waiters
    timer:sleep(50),

    %% Now resolve the future
    beamtalk_future:resolve(Future, shared_value),

    %% All waiters should receive the same value
    Results = [
        receive
            {waiter_done, Pid, R} -> R
        after 2000 -> timeout
        end
     || Pid <- Pids
    ],

    ?assertEqual(lists:duplicate(NumWaiters, shared_value), Results).

concurrent_resolve_await_race_test() ->
    %% Test race condition: resolve and await happening concurrently
    NumIterations = 50,

    Results = [
        begin
            Future = beamtalk_future:new(),
            Parent = self(),

            %% Spawn resolver
            spawn(fun() -> beamtalk_future:resolve(Future, iteration_value) end),

            %% Spawn awaiter
            spawn(fun() ->
                Result = beamtalk_future:await(Future, 1000),
                Parent ! {result, Result}
            end),

            %% Collect result
            receive
                {result, R} -> R
            after 2000 ->
                timeout
            end
        end
     || _ <- lists:seq(1, NumIterations)
    ],

    %% All should have resolved successfully (no timeouts)
    ?assertEqual(lists:duplicate(NumIterations, iteration_value), Results).

future_chaining_with_callbacks_test() ->
    %% Test chaining futures via callbacks
    Future1 = beamtalk_future:new(),
    Future2 = beamtalk_future:new(),

    %% Chain: when Future1 resolves, resolve Future2 with transformed value
    beamtalk_future:when_resolved(Future1, fun(Value) ->
        beamtalk_future:resolve(Future2, Value * 2)
    end),

    %% Resolve first future
    beamtalk_future:resolve(Future1, 21),

    %% Wait for chained future
    timer:sleep(50),
    Result = beamtalk_future:await(Future2),
    ?assertEqual(42, Result).

error_propagation_through_callbacks_test() ->
    %% Test error propagation from one future to another
    Future1 = beamtalk_future:new(),
    Future2 = beamtalk_future:new(),

    %% Chain errors: when Future1 rejects, reject Future2
    beamtalk_future:when_rejected(Future1, fun(Reason) ->
        beamtalk_future:reject(Future2, {propagated, Reason})
    end),

    %% Reject first future
    beamtalk_future:reject(Future1, original_error),

    %% Wait for chained future to be rejected
    timer:sleep(50),
    ?assertThrow(
        {future_rejected, {propagated, original_error}}, beamtalk_future:await(Future2, 1000)
    ).

process_cleanup_when_future_abandoned_test() ->
    %% Test that future process stays alive even if creator crashes
    Parent = self(),

    %% Create future in a separate process that will die
    _CreatorPid = spawn(fun() ->
        F = beamtalk_future:new(),
        Parent ! {future, F},
        %% Process exits, but future should remain
        exit(normal)
    end),

    %% Receive the future reference
    FutureRef =
        receive
            {future, F} -> F
        after 1000 -> error(timeout)
        end,

    %% Give creator time to exit
    timer:sleep(50),

    %% Future process should still be alive
    ?assert(is_process_alive(FutureRef)),

    %% Should still be usable
    beamtalk_future:resolve(FutureRef, still_alive),
    Result = beamtalk_future:await(FutureRef),
    ?assertEqual(still_alive, Result).

%%% Additional state transition edge cases

resolve_after_reject_in_rejected_state_test() ->
    %% Resolve after reject should be silently ignored
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, first_error),
    beamtalk_future:resolve(Future, should_be_ignored),

    %% Should still get the rejection
    ?assertThrow({future_rejected, first_error}, beamtalk_future:await(Future)).

stray_timeout_in_resolved_state_test() ->
    %% Timeout messages in resolved state should be ignored
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, 42),

    %% Send a stray timeout message directly to the future process
    Future ! {timeout, self()},

    %% Future should still work normally
    timer:sleep(10),
    ?assertEqual(42, beamtalk_future:await(Future)).

stray_timeout_in_rejected_state_test() ->
    %% Timeout messages in rejected state should be ignored
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, error),

    %% Send a stray timeout message
    Future ! {timeout, self()},

    timer:sleep(10),
    ?assertThrow({future_rejected, error}, beamtalk_future:await(Future)).

rejected_callback_on_resolved_future_test() ->
    %% Register a rejected callback on a resolved future — should be ignored
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, 42),

    Parent = self(),
    beamtalk_future:when_rejected(Future, fun(Reason) ->
        Parent ! {unexpected_callback, Reason}
    end),

    %% Give time for processing
    timer:sleep(50),

    %% Callback should NOT fire
    receive
        {unexpected_callback, _} -> ?assert(false)
    after 100 ->
        % Expected — callback was ignored
        ok
    end.

resolved_callback_on_rejected_future_test() ->
    %% Register a resolved callback on a rejected future — should be ignored
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, error),

    Parent = self(),
    beamtalk_future:when_resolved(Future, fun(Value) ->
        Parent ! {unexpected_callback, Value}
    end),

    %% Give time for processing
    timer:sleep(50),

    %% Callback should NOT fire
    receive
        {unexpected_callback, _} -> ?assert(false)
    after 100 ->
        % Expected — callback was ignored
        ok
    end.

callback_exception_does_not_crash_future_test() ->
    %% A callback that throws should not crash the future process
    Future = beamtalk_future:new(),

    %% Register a callback that will crash
    beamtalk_future:when_resolved(Future, fun(_Value) ->
        error(callback_crash)
    end),

    %% Also register a well-behaved callback to verify the future still works
    Parent = self(),
    beamtalk_future:when_resolved(Future, fun(Value) ->
        Parent ! {good_callback, Value}
    end),

    %% Resolve — crashing callback runs in separate process
    beamtalk_future:resolve(Future, 42),

    %% Good callback should still fire
    receive
        {good_callback, Value} -> ?assertEqual(42, Value)
    after 1000 ->
        ?assert(false)
    end,

    %% Future process should still be alive
    timer:sleep(50),
    ?assert(is_process_alive(Future)),

    %% Subsequent await should work
    ?assertEqual(42, beamtalk_future:await(Future)).

await_infinity_resolves_test() ->
    %% await with infinity timeout should not trigger the after clause
    Future = beamtalk_future:new(),
    spawn(fun() ->
        timer:sleep(100),
        beamtalk_future:resolve(Future, done)
    end),
    Result = beamtalk_future:await(Future, infinity),
    ?assertEqual(done, Result).

await_on_already_resolved_future_multiple_times_test() ->
    %% Test that already resolved future can be awaited multiple times
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, persistent_value),

    %% Await multiple times
    ?assertEqual(persistent_value, beamtalk_future:await(Future)),
    ?assertEqual(persistent_value, beamtalk_future:await(Future)),
    ?assertEqual(persistent_value, beamtalk_future:await(Future)),

    %% With timeout too
    ?assertEqual(persistent_value, beamtalk_future:await(Future, 100)).

future_with_large_value_test() ->
    %% Test future with large data value
    LargeValue = lists:duplicate(10000, {complex, data, structure, with, many, elements}),

    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, LargeValue),

    Result = beamtalk_future:await(Future),
    ?assertEqual(LargeValue, Result).

concurrent_callback_registration_test() ->
    %% Test concurrent registration of callbacks
    Future = beamtalk_future:new(),
    Parent = self(),
    NumCallbacks = 10,

    %% Register multiple callbacks concurrently
    [
        spawn(fun() ->
            beamtalk_future:when_resolved(Future, fun(Value) ->
                Parent ! {callback_fired, Value}
            end)
        end)
     || _ <- lists:seq(1, NumCallbacks)
    ],

    %% Give time for callbacks to register
    timer:sleep(50),

    %% Resolve the future
    beamtalk_future:resolve(Future, callback_value),

    %% All callbacks should fire
    Notifications = [
        receive
            {callback_fired, V} -> V
        after 1000 -> timeout
        end
     || _ <- lists:seq(1, NumCallbacks)
    ],

    ?assertEqual(lists:duplicate(NumCallbacks, callback_value), Notifications).

reject_without_await_no_crash_test() ->
    %% Test that rejecting a future without anyone waiting doesn't crash
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, unhandled_error),

    %% Future process should still be alive
    ?assert(is_process_alive(Future)),

    %% Later await should get the rejection
    ?assertThrow({future_rejected, unhandled_error}, beamtalk_future:await(Future, 100)).
