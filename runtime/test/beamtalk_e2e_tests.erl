%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc End-to-end tests for Beamtalk compiler output.
%%%
%%% These tests verify that compiled Beamtalk modules work correctly
%%% on the BEAM VM. Each test manually creates module structures that
%%% mirror what the compiler generates, allowing us to test the runtime
%%% behavior of compiled actors.
%%%
%%% Test categories:
%%% - spawn/0 tests (Counter spawn)
%%% - spawn/1 tests (Counter spawnWith: #{...})
%%% - State merging behavior (InitArgs override defaults)
%%% - Async message protocol (BT-79) - futures, awaits, errors, concurrency
%%%
%%% @see beamtalk_actor for the runtime implementation

-module(beamtalk_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Test Fixtures - Simulated compiled Beamtalk modules
%%% ===========================================================================

%% @doc Creates a Counter module structure as the compiler would generate.
%% This mirrors the output of compiling:
%%
%% ```beamtalk
%% value := 0.
%% increment := [self.value := self.value + 1. ^self.value].
%% getValue := [^self.value].
%% divide: n := [^self.value / n].
%% ```
-spec counter_module_state(InitArgs :: map()) -> map().
counter_module_state(InitArgs) ->
    DefaultState = #{
        '__class__' => 'Counter',
        '__methods__' => #{
            increment => fun counter_increment/2,
            getValue => fun counter_getValue/2,
            'divide:' => fun counter_divide/2
        },
        value => 0
    },
    %% Merge InitArgs - this is what init/1 does
    maps:merge(DefaultState, InitArgs).

%% Method: increment
counter_increment([], State) ->
    Value = maps:get(value, State),
    NewValue = Value + 1,
    NewState = maps:put(value, NewValue, State),
    {reply, NewValue, NewState}.

%% Method: getValue
counter_getValue([], State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.

%% Method: divide: (can error on division by zero)
counter_divide([N], State) ->
    Value = maps:get(value, State),
    case N of
        0 -> {error, division_by_zero, State};
        _ -> {reply, Value / N, State}
    end.

%%% ===========================================================================
%%% spawn/0 tests (Counter spawn)
%%% ===========================================================================

spawn_zero_uses_default_state_test() ->
    %% spawn/0 passes empty map to init, which merges with defaults
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    
    %% Start gen_server with this state
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Verify default value is 0
    Value = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(0, Value),
    
    gen_server:stop(Pid).

spawn_zero_methods_work_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Increment several times
    ?assertEqual(1, gen_server:call(Pid, {increment, []})),
    ?assertEqual(2, gen_server:call(Pid, {increment, []})),
    ?assertEqual(3, gen_server:call(Pid, {increment, []})),
    
    %% Verify final value
    ?assertEqual(3, gen_server:call(Pid, {getValue, []})),
    
    gen_server:stop(Pid).

%%% ===========================================================================
%%% spawn/1 tests (Counter spawnWith: #{...})
%%% ===========================================================================

spawn_with_overrides_default_value_test() ->
    %% spawnWith: #{value => 42} passes InitArgs to init
    InitArgs = #{value => 42},
    State = counter_module_state(InitArgs),
    
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Verify initial value was overridden
    Value = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(42, Value),
    
    gen_server:stop(Pid).

spawn_with_methods_still_work_test() ->
    %% spawnWith: #{value => 100}
    InitArgs = #{value => 100},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Increment from 100
    ?assertEqual(101, gen_server:call(Pid, {increment, []})),
    ?assertEqual(102, gen_server:call(Pid, {increment, []})),
    
    gen_server:stop(Pid).

spawn_with_preserves_unspecified_defaults_test() ->
    %% spawnWith: #{extra => foo} - should preserve value default
    InitArgs = #{extra => foo},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Value should still be default (0)
    ?assertEqual(0, gen_server:call(Pid, {getValue, []})),
    
    %% Extra field should be in state
    ActorState = sys:get_state(Pid),
    ?assertEqual(foo, maps:get(extra, ActorState)),
    
    gen_server:stop(Pid).

spawn_with_multiple_overrides_test() ->
    %% spawnWith: #{value => 10, extra => bar}
    InitArgs = #{value => 10, extra => bar},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Value was overridden
    ?assertEqual(10, gen_server:call(Pid, {getValue, []})),
    
    %% Extra field present
    ActorState = sys:get_state(Pid),
    ?assertEqual(bar, maps:get(extra, ActorState)),
    
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Async message protocol E2E tests (BT-79)
%%% ===========================================================================

%%% Basic async message send and future resolution

async_message_creates_future_test() ->
    %% Test: Async message send creates a future
    State = counter_module_state(#{}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Simulate: future := actor increment
    Future = beamtalk_future:new(),
    ?assert(is_pid(Future)),
    
    %% Clean up: resolve the future so we don't leave a pending future process around
    beamtalk_future:resolve(Future, ok),
    
    gen_server:stop(Actor).

async_message_uses_cast_tuple_test() ->
    %% Test: gen_server:cast with {Selector, Args, FuturePid} tuple
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    Future = beamtalk_future:new(),
    %% This is the exact protocol the compiler generates
    ok = gen_server:cast(Actor, {increment, [], Future}),
    
    %% Verify future resolves
    {ok, Result} = beamtalk_future:await(Future, 1000),
    ?assertEqual(1, Result),
    
    gen_server:stop(Actor).

async_message_resolves_after_method_completes_test() ->
    %% Test: Future resolution after method completes
    State = counter_module_state(#{value => 10}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send async increment
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future}),
    
    %% Wait for resolution
    {ok, Result} = beamtalk_future:await(Future, 1000),
    ?assertEqual(11, Result),
    
    %% Verify actor state was updated
    FinalValue = gen_server:call(Actor, {getValue, []}),
    ?assertEqual(11, FinalValue),
    
    gen_server:stop(Actor).

%%% await on pending vs resolved futures

async_await_on_pending_future_blocks_test() ->
    %% Test: await on a pending future blocks until resolved
    State = counter_module_state(#{value => 5}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    Future = beamtalk_future:new(),
    Parent = self(),
    
    %% Spawn a process that will await the future
    spawn(fun() ->
        Parent ! {ready, self()},  % Signal that we're about to await
        {ok, Result} = beamtalk_future:await(Future, 2000),
        Parent ! {awaited, Result}
    end),
    
    %% Wait for the awaiter to signal it's ready
    receive
        {ready, _} -> ok
    after 1000 ->
        ?assert(false)  % Timeout - spawned process didn't start
    end,
    
    %% Give a moment for the await to register with the future
    timer:sleep(100),
    
    %% Now send the async message to resolve the future
    gen_server:cast(Actor, {increment, [], Future}),
    
    %% Verify the awaiter got the result
    receive
        {awaited, Value} -> ?assertEqual(6, Value)
    after 3000 ->
        ?assert(false)  % Timeout - test failed
    end,
    
    gen_server:stop(Actor).

async_await_on_resolved_future_returns_immediately_test() ->
    %% Test: await on an already-resolved future returns immediately
    State = counter_module_state(#{value => 20}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send async message and await to ensure it's resolved
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future}),
    
    %% First await to ensure future is resolved
    {ok, Result1} = beamtalk_future:await(Future, 1000),
    ?assertEqual(21, Result1),
    
    %% Now await the already-resolved future again with a very small timeout.
    %% This verifies it is already resolved without relying on wall-clock timing.
    SmallTimeout = 10,
    {ok, Result2} = beamtalk_future:await(Future, SmallTimeout),
    
    ?assertEqual(21, Result2),
    
    gen_server:stop(Actor).

%%% Future rejection on errors

async_future_rejection_on_method_error_test() ->
    %% Test: Future rejection when method returns error
    State = counter_module_state(#{value => 10}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send async message that will error (divide by zero)
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {'divide:', [0], Future}),
    
    %% Await should return error
    Result = beamtalk_future:await(Future, 1000),
    ?assertEqual({error, division_by_zero}, Result),
    
    gen_server:stop(Actor).

%%% Multiple concurrent async messages

async_multiple_concurrent_messages_test() ->
    %% Test: Multiple concurrent async messages to same actor
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send 5 concurrent increment messages
    Futures = [begin
        F = beamtalk_future:new(),
        gen_server:cast(Actor, {increment, [], F}),
        F
    end || _ <- lists:seq(1, 5)],
    
    %% Await all results
    Results = [begin
        {ok, R} = beamtalk_future:await(F, 1000),
        R
    end || F <- Futures],
    
    %% All increments should have completed
    ?assertEqual([1, 2, 3, 4, 5], Results),
    
    %% Final value should be 5
    FinalValue = gen_server:call(Actor, {getValue, []}),
    ?assertEqual(5, FinalValue),
    
    gen_server:stop(Actor).

async_concurrent_messages_order_preserved_test() ->
    %% Test: Message order is preserved even with async
    State = counter_module_state(#{value => 100}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send multiple async messages rapidly
    F1 = beamtalk_future:new(),
    F2 = beamtalk_future:new(),
    F3 = beamtalk_future:new(),
    
    gen_server:cast(Actor, {increment, [], F1}),
    gen_server:cast(Actor, {increment, [], F2}),
    gen_server:cast(Actor, {increment, [], F3}),
    
    %% Results should be in order
    {ok, R1} = beamtalk_future:await(F1, 1000),
    {ok, R2} = beamtalk_future:await(F2, 1000),
    {ok, R3} = beamtalk_future:await(F3, 1000),
    
    ?assertEqual(101, R1),
    ?assertEqual(102, R2),
    ?assertEqual(103, R3),
    
    gen_server:stop(Actor).

%%% Chained async operations

async_chained_operations_test() ->
    %% Test: Chained async operations (nested futures)
    %% Simulates: result1 := actor increment await. result2 := actor increment await.
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% First async increment
    Future1 = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future1}),
    {ok, Result1} = beamtalk_future:await(Future1, 1000),
    ?assertEqual(1, Result1),
    
    %% Second async increment (chained after first)
    Future2 = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future2}),
    {ok, Result2} = beamtalk_future:await(Future2, 1000),
    ?assertEqual(2, Result2),
    
    gen_server:stop(Actor).

async_nested_futures_different_actors_test() ->
    %% Test: Nested futures with different actors
    %% Simulates: actor1 increment await + actor2 increment await
    State1 = counter_module_state(#{value => 10}),
    State2 = counter_module_state(#{value => 20}),
    {ok, Actor1} = gen_server:start_link(beamtalk_actor, State1, []),
    {ok, Actor2} = gen_server:start_link(beamtalk_actor, State2, []),
    
    %% Send async messages to both actors
    F1 = beamtalk_future:new(),
    F2 = beamtalk_future:new(),
    gen_server:cast(Actor1, {increment, [], F1}),
    gen_server:cast(Actor2, {increment, [], F2}),
    
    %% Await both results
    {ok, R1} = beamtalk_future:await(F1, 1000),
    {ok, R2} = beamtalk_future:await(F2, 1000),
    
    ?assertEqual(11, R1),
    ?assertEqual(21, R2),
    
    gen_server:stop(Actor1),
    gen_server:stop(Actor2).

%%% Mixed sync and async messages

async_mixed_with_sync_messages_test() ->
    %% Test: Mixing synchronous and asynchronous messages
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Sync increment
    ?assertEqual(1, gen_server:call(Actor, {increment, []})),
    
    %% Async increment
    Future = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future}),
    {ok, Result} = beamtalk_future:await(Future, 1000),
    ?assertEqual(2, Result),
    
    %% Sync getValue
    ?assertEqual(2, gen_server:call(Actor, {getValue, []})),
    
    gen_server:stop(Actor).

%%% Edge cases with futures

async_future_timeout_behavior_test() ->
    %% Test: Future await with timeout works correctly
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Create a future but DON'T send the message
    Future = beamtalk_future:new(),
    
    %% Await with short timeout should fail
    Result = beamtalk_future:await(Future, 100),
    ?assertEqual({error, timeout}, Result),
    
    %% Optional cleanup: resolve the future; the future process remains
    %% alive until its inactivity timeout elapses
    beamtalk_future:resolve(Future, ok),
    
    gen_server:stop(Actor).

async_multiple_awaits_same_future_test() ->
    %% Test: Multiple processes can await the same future
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),
    
    Future = beamtalk_future:new(),
    Parent = self(),
    
    %% Spawn 3 processes awaiting the same future
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Parent ! {ready, N},  % Signal ready to await
            {ok, Result} = beamtalk_future:await(Future, 2000),
            Parent ! {waiter, N, Result}
        end)
    end, [1, 2, 3]),
    
    %% Wait for all waiters to signal they're ready
    lists:foreach(fun(_) ->
        receive
            {ready, _} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, [1, 2, 3]),
    
    %% Give a moment for all awaits to register
    timer:sleep(100),
    
    %% Send the async message
    gen_server:cast(Actor, {increment, [], Future}),
    
    %% All waiters should get the result
    Results = lists:sort([receive
        {waiter, N, R} -> {N, R}
    after 3000 ->
        ?assert(false)
    end || _ <- [1, 2, 3]]),
    
    ?assertEqual([{1, 1}, {2, 1}, {3, 1}], Results),
    
    gen_server:stop(Actor).

%%% ===========================================================================
%%% Edge cases
%%% ===========================================================================

spawn_with_empty_map_same_as_spawn_zero_test() ->
    %% Both should produce identical state
    State1 = counter_module_state(#{}),
    State2 = counter_module_state(#{}),
    
    %% Verify they're equal (functionally)
    ?assertEqual(maps:get(value, State1), maps:get(value, State2)),
    ?assertEqual(0, maps:get(value, State1)).

spawn_with_nil_values_override_test() ->
    %% spawnWith: #{value => nil} should set value to nil
    InitArgs = #{value => nil},
    State = counter_module_state(InitArgs),
    
    ?assertEqual(nil, maps:get(value, State)).

spawn_preserves_class_and_methods_test() ->
    %% Verify __class__ and __methods__ are preserved after merge
    InitArgs = #{value => 999},
    State = counter_module_state(InitArgs),
    
    ?assertEqual('Counter', maps:get('__class__', State)),
    ?assert(is_map(maps:get('__methods__', State))),
    
    %% Methods should still be functions
    Methods = maps:get('__methods__', State),
    ?assert(is_function(maps:get(increment, Methods), 2)),
    ?assert(is_function(maps:get(getValue, Methods), 2)).

%%% ===========================================================================
%%% Boolean control flow tests
%%% ===========================================================================
%%%
%%% These tests verify Boolean control flow message implementations work
%%% correctly on BEAM. The helper functions below implement the exact semantics
%%% that compiled Beamtalk Boolean methods must produce.
%%%
%%% Boolean messages are core language semantics - True and False respond
%%% to control flow messages like ifTrue:ifFalse:, and:, or:, not.
%%%
%%% Tests pass block funs through the helper implementations to verify:
%%% - Correct block is evaluated for conditional messages
%%% - Short-circuit behavior prevents evaluation of unused blocks
%%% - Return values match Beamtalk semantics
%%%
%%% @see lib/True.bt for True control flow API
%%% @see lib/False.bt for False control flow API

%%% ---------------------------------------------------------------------------
%%% Boolean method implementations (simulating compiled Beamtalk methods)
%%% ---------------------------------------------------------------------------

%% True>>ifTrue:ifFalse: - evaluates trueBlock, ignores falseBlock
beamtalk_if_true_if_false(true, TrueBlock, _FalseBlock) when is_function(TrueBlock, 0) ->
    TrueBlock();
%% False>>ifTrue:ifFalse: - evaluates falseBlock, ignores trueBlock
beamtalk_if_true_if_false(false, _TrueBlock, FalseBlock) when is_function(FalseBlock, 0) ->
    FalseBlock().

%% True>>ifTrue: - evaluates the block
beamtalk_if_true(true, Block) when is_function(Block, 0) ->
    Block();
%% False>>ifTrue: - returns self without evaluating
beamtalk_if_true(false, _Block) ->
    false.

%% True>>ifFalse: - returns self without evaluating
beamtalk_if_false(true, _Block) ->
    true;
%% False>>ifFalse: - evaluates the block
beamtalk_if_false(false, Block) when is_function(Block, 0) ->
    Block().

%% True>>and: - evaluates the block (short-circuit: true needs to check)
beamtalk_and(true, Block) when is_function(Block, 0) ->
    Block();
%% False>>and: - returns false without evaluating (short-circuit)
beamtalk_and(false, _Block) ->
    false.

%% True>>or: - returns true without evaluating (short-circuit)
beamtalk_or(true, _Block) ->
    true;
%% False>>or: - evaluates the block (short-circuit: false needs to check)
beamtalk_or(false, Block) when is_function(Block, 0) ->
    Block().

%% True>>not - returns false
beamtalk_not(true) ->
    false;
%% False>>not - returns true
beamtalk_not(false) ->
    true.

%%% ---------------------------------------------------------------------------
%%% ifTrue:ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] ifFalse: [0] => 42
true_if_true_if_false_evaluates_true_block_test() ->
    TrueBlock = fun() -> 42 end,
    FalseBlock = fun() -> error(should_not_evaluate) end,
    Result = beamtalk_if_true_if_false(true, TrueBlock, FalseBlock),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] ifFalse: [0] => 0
false_if_true_if_false_evaluates_false_block_test() ->
    TrueBlock = fun() -> error(should_not_evaluate) end,
    FalseBlock = fun() -> 0 end,
    Result = beamtalk_if_true_if_false(false, TrueBlock, FalseBlock),
    ?assertEqual(0, Result).

%%% ---------------------------------------------------------------------------
%%% ifTrue: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] => 42
true_if_true_evaluates_block_test() ->
    Block = fun() -> 42 end,
    Result = beamtalk_if_true(true, Block),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] => false
false_if_true_returns_self_test() ->
    Block = fun() -> error(should_not_evaluate) end,
    Result = beamtalk_if_true(false, Block),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifFalse: [42] => true
true_if_false_returns_self_test() ->
    Block = fun() -> error(should_not_evaluate) end,
    Result = beamtalk_if_false(true, Block),
    ?assertEqual(true, Result).

%% Test: false ifFalse: [42] => 42
false_if_false_evaluates_block_test() ->
    Block = fun() -> 42 end,
    Result = beamtalk_if_false(false, Block),
    ?assertEqual(42, Result).

%%% ---------------------------------------------------------------------------
%%% and: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true and: [true] => true
true_and_evaluates_block_returns_true_test() ->
    Block = fun() -> true end,
    Result = beamtalk_and(true, Block),
    ?assertEqual(true, Result).

%% Test: true and: [false] => false
true_and_evaluates_block_returns_false_test() ->
    Block = fun() -> false end,
    Result = beamtalk_and(true, Block),
    ?assertEqual(false, Result).

%% Test: false and: [error(should_not_evaluate)] => false
false_and_short_circuits_test() ->
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    Result = beamtalk_and(false, ShouldNotRun),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% or: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true or: [error(should_not_evaluate)] => true
true_or_short_circuits_test() ->
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    Result = beamtalk_or(true, ShouldNotRun),
    ?assertEqual(true, Result).

%% Test: false or: [true] => true
false_or_evaluates_block_returns_true_test() ->
    Block = fun() -> true end,
    Result = beamtalk_or(false, Block),
    ?assertEqual(true, Result).

%% Test: false or: [false] => false
false_or_evaluates_block_returns_false_test() ->
    Block = fun() -> false end,
    Result = beamtalk_or(false, Block),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% not tests
%%% ---------------------------------------------------------------------------

%% Test: true not => false
true_not_returns_false_test() ->
    Result = beamtalk_not(true),
    ?assertEqual(false, Result).

%% Test: false not => true
false_not_returns_true_test() ->
    Result = beamtalk_not(false),
    ?assertEqual(true, Result).

%%% ---------------------------------------------------------------------------
%%% Complex control flow tests
%%% ---------------------------------------------------------------------------

%% Test nested conditionals: true ifTrue: [false ifTrue: [1] ifFalse: [2]] => 2
nested_if_true_if_false_test() ->
    OuterBlock = fun() ->
        InnerTrueBlock = fun() -> error(should_not_evaluate) end,
        InnerFalseBlock = fun() -> 2 end,
        beamtalk_if_true_if_false(false, InnerTrueBlock, InnerFalseBlock)
    end,
    Result = beamtalk_if_true(true, OuterBlock),
    ?assertEqual(2, Result).

%% Test chained logical operations: true and: [true] and: [false] => false
chained_and_operations_test() ->
    FirstBlock = fun() -> true end,
    SecondBlock = fun() -> false end,
    FirstResult = beamtalk_and(true, FirstBlock),
    ?assertEqual(true, FirstResult),
    Result = beamtalk_and(FirstResult, SecondBlock),
    ?assertEqual(false, Result).

%% Test chained logical operations: false or: [false] or: [true] => true
chained_or_operations_test() ->
    FirstBlock = fun() -> false end,
    SecondBlock = fun() -> true end,
    FirstResult = beamtalk_or(false, FirstBlock),
    ?assertEqual(false, FirstResult),
    Result = beamtalk_or(FirstResult, SecondBlock),
    ?assertEqual(true, Result).
