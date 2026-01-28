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
        ?assert(false)  % Timeout - test failed
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
        ?assert(false)  % Timeout - test failed
    end.

%%% Multiple waiters tests

multiple_waiters_resolved_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),
    
    %% Spawn 5 waiters
    NumWaiters = 5,
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Result = beamtalk_future:await(Future),
            Parent ! {waiter, N, Result}
        end)
    end, lists:seq(1, NumWaiters)),
    
    %% Give them time to register
    timer:sleep(50),
    
    %% Resolve the future
    beamtalk_future:resolve(Future, shared_value),
    
    %% Collect results from all waiters
    Results = lists:map(fun(_) ->
        receive
            {waiter, _, Value} -> Value
        after 1000 ->
            timeout
        end
    end, lists:seq(1, NumWaiters)),
    
    %% All waiters should have received the value
    ?assertEqual([shared_value, shared_value, shared_value, shared_value, shared_value], Results).

multiple_waiters_rejected_test() ->
    Future = beamtalk_future:new(),
    Parent = self(),
    
    %% Spawn 3 waiters
    NumWaiters = 3,
    lists:foreach(fun(N) ->
        spawn(fun() ->
            try
                beamtalk_future:await(Future)
            catch
                throw:{future_rejected, Reason} ->
                    Parent ! {waiter, N, {rejected, Reason}}
            end
        end)
    end, lists:seq(1, NumWaiters)),
    
    %% Give them time to register
    timer:sleep(50),
    
    %% Reject the future
    beamtalk_future:reject(Future, shared_error),
    
    %% Collect results from all waiters
    Results = lists:map(fun(_) ->
        receive
            {waiter, _, Result} -> Result
        after 1000 ->
            timeout
        end
    end, lists:seq(1, NumWaiters)),
    
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
        ?assert(false)  % Timeout - callback wasn't called
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
        ?assert(false)  % Timeout - callback wasn't called
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
        ?assert(false)  % Timeout - callback wasn't called
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
        {error_callback_called, _} -> ?assert(false)  % Should not happen
    after 100 ->
        ok  % Expected - callback wasn't called
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
    ?assertEqual({ok, quick}, Result).

await_with_timeout_expired_test() ->
    Future = beamtalk_future:new(),
    
    %% Don't resolve the future
    %% Await with a 50ms timeout (should timeout)
    Result = beamtalk_future:await(Future, 50),
    ?assertEqual({error, timeout}, Result).

await_with_timeout_rejected_test() ->
    Future = beamtalk_future:new(),
    
    %% Spawn a task that rejects the future after 50ms
    spawn(fun() ->
        timer:sleep(50),
        beamtalk_future:reject(Future, failed)
    end),
    
    %% Await with a 500ms timeout (should get rejection, not timeout)
    Result = beamtalk_future:await(Future, 500),
    ?assertEqual({error, failed}, Result).

%%% Edge cases

double_resolve_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, first),
    beamtalk_future:resolve(Future, second),  % Should be ignored
    
    Result = beamtalk_future:await(Future),
    ?assertEqual(first, Result).  % Should get the first value

resolve_after_reject_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:reject(Future, error),
    beamtalk_future:resolve(Future, value),  % Should be ignored
    
    ?assertThrow({future_rejected, error}, beamtalk_future:await(Future)).

reject_after_resolve_test() ->
    Future = beamtalk_future:new(),
    beamtalk_future:resolve(Future, value),
    beamtalk_future:reject(Future, error),  % Should be ignored
    
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
    lists:foreach(fun({Future, Index}) ->
        beamtalk_future:resolve(Future, Index)
    end, lists:zip(Futures, lists:seq(1, NumFutures))),
    
    %% Await all of them
    Results = [beamtalk_future:await(F) || F <- Futures],
    
    %% Verify results
    ?assertEqual(lists:seq(1, NumFutures), Results).
