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
%% ```
-spec counter_module_state(InitArgs :: map()) -> map().
counter_module_state(InitArgs) ->
    DefaultState = #{
        '__class__' => 'Counter',
        '__methods__' => #{
            increment => fun counter_increment/2,
            getValue => fun counter_getValue/2
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
%%% Async message tests with spawn/1
%%% ===========================================================================

spawn_with_async_messages_test() ->
    %% Start with custom initial value
    InitArgs = #{value => 50},
    State = counter_module_state(InitArgs),
    {ok, Pid} = gen_server:start_link(beamtalk_actor, State, []),
    
    %% Send async increment via future
    Future = beamtalk_future:new(),
    gen_server:cast(Pid, {increment, [], Future}),
    
    %% Await result
    {ok, Result} = beamtalk_future:await(Future, 1000),
    ?assertEqual(51, Result),
    
    gen_server:stop(Pid).

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
