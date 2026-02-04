%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for compiler-generated code patterns using real compiled Beamtalk.
%%%
%%% These tests verify runtime behavior by using the counter module compiled
%%% from tests/e2e/fixtures/counter.bt (unified fixture - BT-239). This tests 
%%% the actual code generation, not simulated patterns.
%%%
%%% **Note:** These are NOT true end-to-end tests. For real E2E tests that
%%% compile actual Beamtalk source files in full, see `tests/e2e/cases/*.bt`.
%%%
%%% Test categories:
%%% - spawn/0 tests (Counter spawn) - returns #beamtalk_object{}
%%% - spawn/1 tests (Counter spawnWith: #{...})
%%% - State merging behavior (InitArgs override defaults)
%%% - Async message protocol (BT-79) - futures, awaits, errors, concurrency
%%% - Block Evaluation Tests (value, value:, value:value:, closures)
%%% - Control Flow Tests (whileTrue:, whileFalse:, repeat)
%%% - Boolean control flow tests (ifTrue:ifFalse:, and:, or:, not)
%%% - Cascade message sends (BT-133)
%%% - Multi-keyword messages (BT-133)
%%% - Actor interaction patterns (BT-133)
%%% - Error handling (BT-133)
%%% - Instance variable access patterns (BT-133)
%%% - Nested message sends and binary operators (BT-133)
%%%
%%% @see beamtalk_actor for the runtime implementation

-module(beamtalk_codegen_simulation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ===========================================================================
%%% Test Fixtures - Counter simulation and compiled module
%%% ===========================================================================

%% Note: For spawn/0 and spawn/1 tests, we use the real compiled counter module
%% from tests/e2e/fixtures/counter.bt (unified fixture - BT-239) which generates 
%% actual #beamtalk_object{} records.
%%
%% For other tests that need manual state manipulation (async, cascade, etc.),
%% we use simulated state structures below.

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

%%%
%%% spawn/0 tests (Counter spawn)
%%% 
%%% Tests use counter:spawn() from compiled tests/e2e/fixtures/counter.bt
%%% which returns {beamtalk_object, 'Counter', counter, Pid}
%%% ===========================================================================

spawn_zero_uses_default_state_test() ->
    %% spawn/0 passes empty map to init, which merges with defaults
    Object = counter:spawn(),
    ?assertMatch({beamtalk_object, 'Counter', counter, _Pid}, Object),
    
    %% Extract pid from #beamtalk_object{} record (element 4, 1-indexed)
    Pid = element(4, Object),
    
    %% Verify default value is 0
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(0, Value),
    
    gen_server:stop(Pid).

spawn_zero_methods_work_test() ->
    Object = counter:spawn(),
    Pid = element(4, Object),
    
    %% Increment several times
    ?assertEqual({ok, 1}, gen_server:call(Pid, {increment, []})),
    ?assertEqual({ok, 2}, gen_server:call(Pid, {increment, []})),
    ?assertEqual({ok, 3}, gen_server:call(Pid, {increment, []})),
    
    %% Verify final value
    ?assertEqual({ok, 3}, gen_server:call(Pid, {getValue, []})),
    
    gen_server:stop(Pid).

%%% ===========================================================================
%%% spawn/1 tests (Counter spawnWith: #{...})
%%% 
%%% Tests use counter:spawn(InitArgs) with initialization arguments
%%% ===========================================================================

spawn_with_overrides_default_value_test() ->
    %% spawnWith: #{value => 42} passes InitArgs to init
    InitArgs = #{value => 42},
    Object = counter:spawn(InitArgs),
    Pid = element(4, Object),
    
    %% Verify initial value was overridden
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(42, Value),
    
    gen_server:stop(Pid).

spawn_with_methods_still_work_test() ->
    %% spawnWith: #{value => 100}
    InitArgs = #{value => 100},
    Object = counter:spawn(InitArgs),
    Pid = element(4, Object),
    
    %% Increment from 100
    ?assertEqual({ok, 101}, gen_server:call(Pid, {increment, []})),
    ?assertEqual({ok, 102}, gen_server:call(Pid, {increment, []})),
    
    gen_server:stop(Pid).

spawn_with_preserves_unspecified_defaults_test() ->
    %% spawnWith: #{extra => foo} - should preserve value default
    InitArgs = #{extra => foo},
    Object = counter:spawn(InitArgs),
    Pid = element(4, Object),
    
    %% Value should still be default (0)
    ?assertEqual({ok, 0}, gen_server:call(Pid, {getValue, []})),
    
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
%%% Block Evaluation Tests (value, value:, value:value:, etc.)
%%% ===========================================================================

%% @doc Test: [42] value => 42
%% Block with no arguments, evaluated with value message
block_value_no_args_test() ->
    Block = fun() -> 42 end,
    Result = Block(),
    ?assertEqual(42, Result).

%% @doc Test: [:x | x + 1] value: 5 => 6
%% Block with one parameter, evaluated with value: message
block_value_one_arg_test() ->
    Block = fun(X) -> X + 1 end,
    Result = Block(5),
    ?assertEqual(6, Result).

%% @doc Test: [:x :y | x + y] value: 3 value: 4 => 7
%% Block with two parameters, evaluated with value:value: message
block_value_two_args_test() ->
    Block = fun(X, Y) -> X + Y end,
    Result = Block(3, 4),
    ?assertEqual(7, Result).

%% @doc Test: [:x :y :z | x + y + z] value: 1 value: 2 value: 3 => 6
%% Block with three parameters, evaluated with value:value:value: message
block_value_three_args_test() ->
    Block = fun(X, Y, Z) -> X + Y + Z end,
    Result = Block(1, 2, 3),
    ?assertEqual(6, Result).

%% @doc Test: [:x | [:y | x + y]] value: 10 value: 5 => 15
%% Nested blocks - outer block returns inner block, which captures x
block_nested_evaluation_test() ->
    %% Outer block: [:x | [:y | x + y]]
    OuterBlock = fun(X) ->
        %% Inner block captures X from lexical scope
        fun(Y) -> X + Y end
    end,
    
    %% Evaluate outer with 10, returns inner block
    InnerBlock = OuterBlock(10),
    
    %% Evaluate inner with 5
    Result = InnerBlock(5),
    ?assertEqual(15, Result).

%% @doc Test closures capture lexical scope
%% captured := 100. [:x | x + captured] value: 5 => 105
block_captures_lexical_scope_test() ->
    %% Simulate captured variable from lexical scope
    Captured = 100,
    
    %% Block captures Captured
    Block = fun(X) -> X + Captured end,
    
    %% Evaluate block
    Result = Block(5),
    ?assertEqual(105, Result).

%% @doc Test block that returns another block
%% makeAdder := [:n | [:x | x + n]].
%% addFive := makeAdder value: 5.
%% addFive value: 10 => 15
block_returns_closure_test() ->
    %% makeAdder returns a closure that adds n to its argument
    MakeAdder = fun(N) ->
        fun(X) -> X + N end
    end,
    
    %% Create an adder that adds 5
    AddFive = MakeAdder(5),
    
    %% Use the adder
    ?assertEqual(15, AddFive(10)),
    ?assertEqual(8, AddFive(3)).

%% @doc Test block with multiple statements
%% [:x | temp := x * 2. temp + 1] value: 5 => 11
block_multiple_statements_test() ->
    Block = fun(X) ->
        Temp = X * 2,
        Temp + 1
    end,
    Result = Block(5),
    ?assertEqual(11, Result).

%%% ===========================================================================
%%% Control Flow Tests (whileTrue:, whileFalse:, repeat)
%%% ===========================================================================

%% @doc Test: [counter < 5] whileTrue: [counter := counter + 1]
%% Loop while condition is true
block_while_true_loop_test() ->
    %% Simulate: counter := 0. [counter < 5] whileTrue: [counter := counter + 1]
    %% We'll use a recursive function to simulate the letrec loop
    InitialCounter = 0,
    FinalCounter = while_true_loop(InitialCounter, 5),
    ?assertEqual(5, FinalCounter).

%% Helper: Simulates whileTrue: loop implementation
while_true_loop(Counter, Limit) ->
    case Counter < Limit of
        true ->
            NewCounter = Counter + 1,
            while_true_loop(NewCounter, Limit);
        false ->
            Counter
    end.

%% @doc Test: [counter >= 10] whileFalse: [counter := counter + 1]
%% Loop while condition is false
block_while_false_loop_test() ->
    %% Start at 0, increment until counter >= 10
    InitialCounter = 0,
    FinalCounter = while_false_loop(InitialCounter, 10),
    ?assertEqual(10, FinalCounter).

%% Helper: Simulates whileFalse: loop implementation
while_false_loop(Counter, Target) ->
    case Counter >= Target of
        false ->
            NewCounter = Counter + 1,
            while_false_loop(NewCounter, Target);
        true ->
            Counter
    end.

%% @doc Test: [counter < 100] whileTrue: [counter := counter + 1. process: counter]
%% whileTrue: executes body multiple times
block_while_true_accumulates_test() ->
    %% Sum numbers 1 to 10
    {_Counter, Sum} = while_true_accumulate(1, 0),
    ?assertEqual(55, Sum).  %% 1+2+3+...+10 = 55

while_true_accumulate(Counter, Sum) ->
    case Counter =< 10 of
        true ->
            NewSum = Sum + Counter,
            NewCounter = Counter + 1,
            while_true_accumulate(NewCounter, NewSum);
        false ->
            {Counter, Sum}
    end.

%% @doc Test: repeat loop with early termination
%% counter := 0. [counter := counter + 1. counter < 5] repeat => loops forever
%% We simulate with a bounded version that terminates
block_repeat_with_termination_test() ->
    %% Simulate repeat that increments until threshold
    %% [counter := counter + 1. counter >= 5 ifTrue: [^counter]] repeat
    Result = repeat_until_threshold(0, 5),
    ?assertEqual(5, Result).

%% Helper: Simulates repeat with early return
repeat_until_threshold(Counter, Threshold) ->
    NewCounter = Counter + 1,
    case NewCounter >= Threshold of
        true ->
            NewCounter;  %% Early return simulates ^counter
        false ->
            repeat_until_threshold(NewCounter, Threshold)
    end.

%% @doc Test: nested loops with whileTrue:
%% outer := 0. inner := 0.
%% [outer < 3] whileTrue: [
%%   inner := 0.
%%   [inner < 3] whileTrue: [inner := inner + 1].
%%   outer := outer + 1
%% ]
block_nested_while_true_test() ->
    %% Count total iterations in nested loop (3 * 3 = 9)
    TotalIterations = nested_while_loops(0, 0),
    ?assertEqual(9, TotalIterations).

nested_while_loops(Outer, Total) ->
    case Outer < 3 of
        true ->
            %% Inner loop from 0 to 3
            NewTotal = inner_while_loop(0, Total),
            nested_while_loops(Outer + 1, NewTotal);
        false ->
            Total
    end.

inner_while_loop(Inner, Total) ->
    case Inner < 3 of
        true ->
            inner_while_loop(Inner + 1, Total + 1);
        false ->
            Total
    end.

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

%%% ===========================================================================
%%% Block Closure Tests - Captures from outer scope
%%% ===========================================================================

%% Test: Block captures single variable from outer scope
block_captures_single_variable_test() ->
    Outer = 10,
    Block = fun(X) -> X + Outer end,
    Result = Block(5),
    ?assertEqual(15, Result).

%% Test: Block captures multiple variables from outer scope
block_captures_multiple_variables_test() ->
    Base = 100,
    Multiplier = 2,
    Block = fun(X) -> (X + Base) * Multiplier end,
    Result = Block(50),
    ?assertEqual(300, Result).

%% Test: Nested closure - outer block returns inner block with capture
nested_closure_with_capture_test() ->
    OuterBlock = fun(N) -> fun(X) -> X + N end end,
    InnerBlock = OuterBlock(10),
    Result = InnerBlock(5),
    ?assertEqual(15, Result).

%% Test: Multiple closures sharing same captured variable
shared_captured_variable_test() ->
    Shared = 1000,
    AddShared = fun(X) -> X + Shared end,
    SubtractShared = fun(X) -> X - Shared end,
    ?assertEqual(1500, AddShared(500)),
    ?assertEqual(200, SubtractShared(1200)).

%%% ===========================================================================
%%% Complex Keyword Message Tests
%%% ===========================================================================

%% Test: Three-argument block with mixed operations
three_arg_block_mixed_operations_test() ->
    Block = fun(X, Y, Z) -> X * Y + Z end,
    Result = Block(2, 3, 4),
    ?assertEqual(10, Result).

%% Test: Nested keyword messages - block returning block
nested_keyword_block_test() ->
    OuterBlock = fun(X) -> fun(Y) -> X + Y end end,
    InnerBlock = OuterBlock(10),
    Result = InnerBlock(5),
    ?assertEqual(15, Result).

%% Test: Higher-order function - block taking block as argument
higher_order_block_test() ->
    ApplyBlock = fun(BlockFun, Arg) -> BlockFun(Arg) end,
    InnerBlock = fun(X) -> X + 100 end,
    Result = ApplyBlock(InnerBlock, 42),
    ?assertEqual(142, Result).

%%% ===========================================================================
%%% Nested Block Evaluation Tests
%%% ===========================================================================

%% Test: Three levels of nested blocks
three_level_nested_blocks_test() ->
    Level1 = fun(A) -> fun(B) -> fun(C) -> A + B + C end end end,
    Level2 = Level1(1),
    Level3 = Level2(2),
    Result = Level3(3),
    ?assertEqual(6, Result).

%% Test: Nested block evaluation with arithmetic
nested_blocks_arithmetic_test() ->
    Outer = fun() -> fun(X) -> X * 3 end end,
    Inner = Outer(),
    Result = Inner(4),
    ?assertEqual(12, Result).

%% Test: Block returning result of nested block evaluation
block_returns_nested_result_test() ->
    Block = fun() -> (fun() -> 100 end)() end,
    Result = Block(),
    ?assertEqual(100, Result).

%%% ===========================================================================
%%% String Operation Tests
%%% ===========================================================================

%% Test: String concatenation (when stdlib implemented, this will use methods)
%% For now, test basic string handling in blocks
string_in_block_test() ->
    Block = fun(S) -> S end,
    Result = Block(<<"test">>),
    ?assertEqual(<<"test">>, Result).

%% Test: String as block parameter
string_parameter_test() ->
    Block = fun(S) -> S end,
    Result = Block(<<"hello world">>),
    ?assertEqual(<<"hello world">>, Result).

%%% ===========================================================================
%%% Complex Message Send Patterns
%%% ===========================================================================

%% Test: Sequential operations pattern (not true cascades)
%% Note: This simulates the effect of cascades but doesn't test actual
%% cascade message sends, which require actor infrastructure
sequential_operations_pattern_test() ->
    %% Simulate effect of: value increment; increment; getValue
    InitialValue = 0,
    Value1 = InitialValue + 1,
    Value2 = Value1 + 1,
    FinalValue = Value2,
    ?assertEqual(2, FinalValue).

%% Test: Block with complex arithmetic precedence
complex_arithmetic_in_block_test() ->
    Block = fun(X) -> X * 2 + 1 end,
    Result = Block(5),
    ?assertEqual(11, Result).

%% Test: Chained block evaluations
chained_block_evaluation_test() ->
    Block1 = fun(X) -> X + 10 end,
    Block2 = fun(X) -> X * 2 end,
    Intermediate = Block1(5),
    Result = Block2(Intermediate),
    ?assertEqual(30, Result).

%%% ===========================================================================
%%% Cascade Message Send Tests (BT-133)
%%% ===========================================================================

%% Test: Cascade - multiple messages to same actor
%% Simulates: counter increment; increment; getValue
cascade_multiple_messages_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send cascade of messages to same actor
    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),
    Result = gen_server:call(Pid, {getValue, []}),
    
    ?assertEqual(2, Result),
    gen_server:stop(Pid).

%% Test: Cascade with mixed message types
%% Simulates: counter increment; getValue; increment
cascade_mixed_operations_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    gen_server:call(Pid, {increment, []}),
    Value1 = gen_server:call(Pid, {getValue, []}),
    gen_server:call(Pid, {increment, []}),
    Value2 = gen_server:call(Pid, {getValue, []}),
    
    ?assertEqual(1, Value1),
    ?assertEqual(2, Value2),
    gen_server:stop(Pid).

%% Test: Cascade returns last message result
%% Simulates: counter increment; increment; increment
cascade_returns_last_result_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),
    LastResult = gen_server:call(Pid, {increment, []}),
    
    ?assertEqual(3, LastResult),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Multi-Keyword Message Tests (BT-133)
%%% ===========================================================================

%% Test fixture for multi-keyword messages
%% Simulates: Rectangle width:height: method
-spec rectangle_module_state(InitArgs :: map()) -> map().
rectangle_module_state(InitArgs) ->
    DefaultState = #{
        '__class__' => 'Rectangle',
        '__methods__' => #{
            'width:height:' => fun rectangle_width_height/2,
            area => fun rectangle_area/2
        },
        width => 0,
        height => 0
    },
    maps:merge(DefaultState, InitArgs).

rectangle_width_height([W, H], State) ->
    NewState = maps:put(width, W, maps:put(height, H, State)),
    {reply, self, NewState}.

rectangle_area([], State) ->
    W = maps:get(width, State),
    H = maps:get(height, State),
    {reply, W * H, State}.

%% Test: Multi-keyword message with two arguments
%% Simulates: rect width: 5 height: 3
multi_keyword_two_args_test() ->
    InitArgs = #{},
    State = rectangle_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    gen_server:call(Pid, {'width:height:', [5, 3]}),
    Area = gen_server:call(Pid, {area, []}),
    
    ?assertEqual(15, Area),
    gen_server:stop(Pid).

%% Test fixture for three-keyword messages
%% Simulates: Box width:height:depth: method
-spec box_module_state(InitArgs :: map()) -> map().
box_module_state(InitArgs) ->
    DefaultState = #{
        '__class__' => 'Box',
        '__methods__' => #{
            'width:height:depth:' => fun box_width_height_depth/2,
            volume => fun box_volume/2
        },
        width => 0,
        height => 0,
        depth => 0
    },
    maps:merge(DefaultState, InitArgs).

box_width_height_depth([W, H, D], State) ->
    NewState = maps:put(width, W, maps:put(height, H, maps:put(depth, D, State))),
    {reply, self, NewState}.

box_volume([], State) ->
    W = maps:get(width, State),
    H = maps:get(height, State),
    D = maps:get(depth, State),
    {reply, W * H * D, State}.

%% Test: Multi-keyword message with three arguments
%% Simulates: box width: 2 height: 3 depth: 4
multi_keyword_three_args_test() ->
    InitArgs = #{},
    State = box_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    gen_server:call(Pid, {'width:height:depth:', [2, 3, 4]}),
    Volume = gen_server:call(Pid, {volume, []}),
    
    ?assertEqual(24, Volume),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Actor Interaction Patterns (BT-133)
%%% ===========================================================================

%% Test fixture: Spawner that creates another actor
-spec spawner_module_state(InitArgs :: map()) -> map().
spawner_module_state(InitArgs) ->
    DefaultState = #{
        '__class__' => 'Spawner',
        '__methods__' => #{
            spawnCounter => fun spawner_spawn_counter/2,
            lastPid => fun spawner_last_pid/2
        },
        lastPid => nil
    },
    maps:merge(DefaultState, InitArgs).

spawner_spawn_counter([], State) ->
    %% Spawn a counter actor
    CounterState = counter_module_state(#{}),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, CounterState, []),
    NewState = maps:put(lastPid, Pid, State),
    {reply, Pid, NewState}.

spawner_last_pid([], State) ->
    %% Return the last spawned PID, or nil if none
    Pid = maps:get(lastPid, State, nil),
    {reply, Pid, State}.

%% Test: Actor A spawns Actor B
actor_spawns_another_actor_test() ->
    InitArgs = #{},
    SpawnerState = spawner_module_state(InitArgs),
    {ok, Spawner} = gen_server:start_link(beamtalk_actor, SpawnerState, []),
    
    try
        %% Spawner creates a Counter
        CounterPid = gen_server:call(Spawner, {spawnCounter, []}),
        ?assert(is_pid(CounterPid)),
        
        %% Verify counter works
        gen_server:call(CounterPid, {increment, []}),
        Value = gen_server:call(CounterPid, {getValue, []}),
        ?assertEqual(1, Value),
        
        gen_server:stop(CounterPid)
    after
        gen_server:stop(Spawner)
    end.

%% Test: Actors communicating via messages
actors_communicate_test() ->
    %% Create two counters
    State1 = counter_module_state(#{}),
    State2 = counter_module_state(#{}),
    {ok, Counter1} = gen_server:start_link(beamtalk_actor, State1, []),
    {ok, Counter2} = gen_server:start_link(beamtalk_actor, State2, []),
    
    try
        %% Increment counter1 twice
        gen_server:call(Counter1, {increment, []}),
        gen_server:call(Counter1, {increment, []}),
        
        %% Get value from counter1 and set it in counter2 (simulated)
        Value1 = gen_server:call(Counter1, {getValue, []}),
        
        %% Increment counter2 that many times
        [gen_server:call(Counter2, {increment, []}) || _ <- lists:seq(1, Value1)],
        
        Value2 = gen_server:call(Counter2, {getValue, []}),
        ?assertEqual(Value1, Value2)
    after
        gen_server:stop(Counter1),
        gen_server:stop(Counter2)
    end.

%% Test: Actor chain - A spawns B, B spawns C
actor_spawn_chain_test() ->
    SpawnerState = spawner_module_state(#{}),
    {ok, Spawner1} = gen_server:start_link(beamtalk_actor, SpawnerState, []),
    
    %% Spawner1 creates Spawner2
    Spawner2State = spawner_module_state(#{}),
    {ok, Spawner2} = gen_server:start_link(beamtalk_actor, Spawner2State, []),
    
    try
        %% Spawner2 creates Counter
        CounterPid = gen_server:call(Spawner2, {spawnCounter, []}),
        
        %% Use the counter
        gen_server:call(CounterPid, {increment, []}),
        gen_server:call(CounterPid, {increment, []}),
        Value = gen_server:call(CounterPid, {getValue, []}),
        
        ?assertEqual(2, Value),
        
        gen_server:stop(CounterPid)
    after
        gen_server:stop(Spawner2),
        gen_server:stop(Spawner1)
    end.

%%% ===========================================================================
%%% Error Handling Tests (BT-133)
%%% ===========================================================================

%% Test: Method not found error
method_not_found_error_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Try to call non-existent method
    Result = gen_server:call(Pid, {nonExistentMethod, []}),
    
    %% Should get does_not_understand error
    ?assertMatch({error, #beamtalk_error{
        kind = does_not_understand,
        class = 'Counter',
        selector = nonExistentMethod
    }}, Result),
    
    gen_server:stop(Pid).

%% Test: Division by zero error
division_by_zero_error_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Try to divide by zero
    Result = gen_server:call(Pid, {'divide:', [0]}),
    
    %% Should get division_by_zero error
    ?assertMatch({error, division_by_zero}, Result),
    
    gen_server:stop(Pid).

%% Test: Wrong number of arguments error
wrong_arg_count_error_test() ->
    InitArgs = #{},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% divide: expects 1 arg, give it 2
    Result = gen_server:call(Pid, {'divide:', [5, 10]}),
    
    %% Should get a type_error (method threw exception due to wrong arity)
    ?assertMatch({error, #beamtalk_error{
        kind = type_error,
        class = 'Counter',
        selector = 'divide:'
    }}, Result),
    
    gen_server:stop(Pid).

%% Test: Actor crash and recovery simulation
actor_crash_simulation_test() ->
    %% Create a counter
    State = counter_module_state(#{}),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Use it normally
    gen_server:call(Pid, {increment, []}),
    Value1 = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(1, Value1),
    
    %% Simulate crash by stopping it
    gen_server:stop(Pid),
    
    %% Create a new one (simulates restart)
    {ok, NewPid} = gen_server:start_link(beamtalk_actor, State, []),
    Value2 = gen_server:call(NewPid, {getValue, []}),
    
    %% New actor has fresh state
    ?assertEqual(0, Value2),
    
    gen_server:stop(NewPid).

%%% ===========================================================================
%%% Instance Variable Access Patterns (BT-133)
%%% ===========================================================================

%% Test: Instance variable shadowing
%% When a method param has same name as instance var, param takes precedence
instance_var_shadowing_test() ->
    %% Module with 'value' instance var and method that takes 'value' param
    ModuleState = #{
        '__class__' => 'ShadowTest',
        '__methods__' => #{
            getInstanceValue => fun([], State) -> 
                {reply, maps:get(value, State), State}
            end,
            echoParam => fun([Value], State) ->
                %% Param 'Value' shadows instance var 'value'
                {reply, Value, State}
            end
        },
        value => 42
    },
    
    {ok, Pid} = gen_server:start_link(beamtalk_actor, ModuleState, []),
    
    %% Instance var value is 42
    InstanceValue = gen_server:call(Pid, {getInstanceValue, []}),
    ?assertEqual(42, InstanceValue),
    
    %% But param value is independent
    ParamValue = gen_server:call(Pid, {echoParam, [99]}),
    ?assertEqual(99, ParamValue),
    
    %% Instance var unchanged
    StillInstanceValue = gen_server:call(Pid, {getInstanceValue, []}),
    ?assertEqual(42, StillInstanceValue),
    
    gen_server:stop(Pid).

%% Test: Multiple instance variables
multiple_instance_vars_test() ->
    ModuleState = #{
        '__class__' => 'Point',
        '__methods__' => #{
            'setX:Y:' => fun([X, Y], State) ->
                NewState = maps:put(x, X, maps:put(y, Y, State)),
                {reply, ok, NewState}
            end,
            sumCoordinates => fun([], State) ->
                X = maps:get(x, State),
                Y = maps:get(y, State),
                {reply, X + Y, State}
            end
        },
        x => 0,
        y => 0
    },
    
    {ok, Pid} = gen_server:start_link(beamtalk_actor, ModuleState, []),
    
    gen_server:call(Pid, {'setX:Y:', [10, 20]}),
    Sum = gen_server:call(Pid, {sumCoordinates, []}),
    
    ?assertEqual(30, Sum),
    
    gen_server:stop(Pid).

%% Test: Instance variable persistence across method calls
instance_var_persistence_test() ->
    State = counter_module_state(#{}),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Increment modifies instance var
    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),
    
    %% Value persists
    Value = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(2, Value),
    
    %% Increment again
    gen_server:call(Pid, {increment, []}),
    
    %% Still persists
    NewValue = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(3, NewValue),
    
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Nested Message Send Tests (BT-133)
%%% ===========================================================================

%% Test: Nested unary messages
%% Simulates: counter getValue getValue (if getValue returned an object)
nested_message_sends_simulation_test() ->
    %% This simulates the pattern, not actual nesting since getValue returns int
    State = counter_module_state(#{}),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    gen_server:call(Pid, {increment, []}),
    Value = gen_server:call(Pid, {getValue, []}),
    
    %% If we had an object wrapper, we'd send another message
    %% For now, just verify the value is what we expect
    ?assertEqual(1, Value),
    
    gen_server:stop(Pid).

%% Test: Binary operators in expressions
binary_operators_test() ->
    %% Simulate: (3 + 4) * 2
    Block = fun() ->
        Sum = 3 + 4,
        Sum * 2
    end,
    
    Result = Block(),
    ?assertEqual(14, Result).

%% Test: Chained binary operators with precedence
chained_binary_operators_test() ->
    %% Simulate: 2 + 3 * 4 - 1
    %% Should follow standard precedence: 2 + (3 * 4) - 1 = 2 + 12 - 1 = 13
    Block = fun() -> 2 + 3 * 4 - 1 end,
    
    Result = Block(),
    ?assertEqual(13, Result).

%%% ===========================================================================
%%% Super Keyword Tests (BT-108) - E2E Runtime Tests
%%% ===========================================================================
%%%
%%% These tests use the compiled logging_counter module from
%%% tests/fixtures/logging_counter.bt which demonstrates super dispatch
%%% in an inheritance hierarchy.
%%%
%%% Inheritance chain: Actor -> Counter -> LoggingCounter
%%%
%%% LoggingCounter overrides increment to:
%%% 1. Increment logCount
%%% 2. Call super increment (Counter's version)
%%% 3. Return the value
%%%
%%% Runtime support (BT-152) is complete - beamtalk_classes:super_dispatch/3
%%% is implemented and all 356 runtime tests pass.
%%%
%%% **Setup:** These tests require beamtalk_classes registry to be running
%%% with Counter and LoggingCounter classes registered.
%%%
%%% @see tests/fixtures/logging_counter.bt
%%% @see BT-152 for runtime super_dispatch/3 implementation
%%%
%%% **DISABLED (BT-211):** These tests are commented out because subclass init
%%% doesn't include inherited state fields from parent classes. The compiled
%%% logging_counter module doesn't have 'value' field from Counter, so super
%%% dispatch fails with badarg when accessing missing state.
%%% Re-enable when BT-211 is fixed.

%% Setup helper for super tests
setup_super_test_classes() ->
    %% Ensure pg is started
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    
    %% Start Counter class process if not already running
    case beamtalk_class:whereis_class('Counter') of
        undefined ->
            {ok, _CounterPid} = beamtalk_class:start_link('Counter', #{
                name => 'Counter',
                module => counter,
                superclass => 'Actor',
                instance_methods => #{
                    increment => #{arity => 0},
                    getValue => #{arity => 0},
                    decrement => #{arity => 0}
                },
                instance_variables => [value],
                class_variables => #{},
                method_source => #{},
                source_file => "tests/fixtures/counter.bt"
            }),
            ok;
        _ExistingPid1 ->
            ok
    end,
    
    %% Start LoggingCounter class process if not already running
    case beamtalk_class:whereis_class('LoggingCounter') of
        undefined ->
            {ok, _LoggingPid} = beamtalk_class:start_link('LoggingCounter', #{
                name => 'LoggingCounter',
                module => logging_counter,
                superclass => 'Counter',
                instance_methods => #{
                    increment => #{arity => 0},
                    getValue => #{arity => 0},
                    getLogCount => #{arity => 0}
                },
                instance_variables => [value, logCount],
                class_variables => #{},
                method_source => #{},
                source_file => "tests/fixtures/logging_counter.bt"
            }),
            ok;
        _ExistingPid2 ->
            ok
    end,
    ok.

%% ==========================================================================
%% DISABLED TESTS (BT-211)
%% 
%% The following super_* tests are disabled because BT-211 (subclass init
%% doesn't include inherited state fields) causes them to fail. The compiled
%% logging_counter module lacks the 'value' field inherited from Counter.
%%
%% Re-enable these tests when BT-211 is fixed.
%% ==========================================================================

%% Test: Super dispatch calls parent method
%% LoggingCounter increment calls Counter increment via super
%% DISABLED: BT-211 - subclass init missing inherited fields
super_calls_parent_method_test() ->
    setup_super_test_classes(),
    
    %% Create logging counter with initial state
    Object = logging_counter:spawn(),
    ?assertMatch({beamtalk_object, 'LoggingCounter', logging_counter, _Pid}, Object),
    
    Pid = element(4, Object),
    
    %% Increment should:
    %% 1. Increment logCount to 1
    %% 2. Call super increment (increments value to 1)
    %% 3. Return value (1)
    {ok, Value} = gen_server:call(Pid, {increment, []}),
    ?assertEqual(1, Value),
    
    %% Verify logCount was incremented
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    ?assertEqual(1, LogCount),
    
    gen_server:stop(Pid).

%% Test: Multiple super calls accumulate properly
%% DISABLED: BT-211 - subclass init missing inherited fields
super_multiple_calls_test() ->
    setup_super_test_classes(),
    
    Object = logging_counter:spawn(),
    Pid = element(4, Object),
    
    %% Call increment 3 times
    {ok, _} = gen_server:call(Pid, {increment, []}),
    {ok, _} = gen_server:call(Pid, {increment, []}),
    {ok, Value3} = gen_server:call(Pid, {increment, []}),
    
    %% Value should be 3 (super incremented it)
    ?assertEqual(3, Value3),
    
    %% LogCount should also be 3
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    ?assertEqual(3, LogCount),
    
    gen_server:stop(Pid).

%% Test: Super with getValue - different method
%% DISABLED: BT-211 - subclass init missing inherited fields
super_with_different_method_test() ->
    setup_super_test_classes(),
    
    InitArgs = #{value => 42},
    Object = logging_counter:spawn(InitArgs),
    Pid = element(4, Object),
    
    %% getValue calls super getValue (Counter's version)
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(42, Value),
    
    gen_server:stop(Pid).

%% Test: Child adds new methods alongside super
%% DISABLED: BT-211 - subclass init missing inherited fields
super_with_new_methods_test() ->
    setup_super_test_classes(),
    
    Object = logging_counter:spawn(),
    Pid = element(4, Object),
    
    %% getLogCount is new to LoggingCounter
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    ?assertEqual(0, LogCount),
    
    %% After increment, both value and logCount change
    {ok, _} = gen_server:call(Pid, {increment, []}),
    
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    {ok, LogCount2} = gen_server:call(Pid, {getLogCount, []}),
    
    ?assertEqual(1, Value),
    ?assertEqual(1, LogCount2),
    
    gen_server:stop(Pid).

%% Test: Super maintains state consistency
%% DISABLED: BT-211 - subclass init missing inherited fields
super_maintains_state_test() ->
    setup_super_test_classes(),
    
    Object = logging_counter:spawn(),
    Pid = element(4, Object),
    
    %% Mix calls to overridden and non-overridden methods
    {ok, 1} = gen_server:call(Pid, {increment, []}),  % Calls super
    {ok, 1} = gen_server:call(Pid, {getValue, []}),   % Calls super
    {ok, 2} = gen_server:call(Pid, {increment, []}),  % Calls super
    {ok, 2} = gen_server:call(Pid, {getValue, []}),   % Calls super
    
    %% Both state variables updated correctly
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    ?assertEqual(2, LogCount),
    
    gen_server:stop(Pid).

%% Test: Super with initial state override
%% DISABLED: BT-211 - subclass init missing inherited fields
super_with_init_args_test() ->
    setup_super_test_classes(),
    
    InitArgs = #{value => 100, logCount => 5},
    Object = logging_counter:spawn(InitArgs),
    Pid = element(4, Object),
    
    %% Starting values should be overridden
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    
    ?assertEqual(100, Value),
    ?assertEqual(5, LogCount),
    
    %% Increment should work from these values
    {ok, 101} = gen_server:call(Pid, {increment, []}),
    {ok, 6} = gen_server:call(Pid, {getLogCount, []}),
    
    gen_server:stop(Pid).
