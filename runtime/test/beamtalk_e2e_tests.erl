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
%%% Boolean control flow tests
%%% ===========================================================================
%%%
%%% These tests simulate the Boolean control flow message semantics used
%%% by compiled Beamtalk code. The helper functions below implement the
%%% expected behavior of Boolean methods as they would be generated by
%%% the compiler and executed on the BEAM VM.
%%%
%%% Boolean messages are core language semantics: True and False respond
%%% to control flow messages like ifTrue:ifFalse:, and:, or:, not.
%%%
%%% @see lib/True.bt for True control flow API
%%% @see lib/False.bt for False control flow API

%%% ---------------------------------------------------------------------------
%%% Helper functions - simulate Boolean message dispatch
%%% ---------------------------------------------------------------------------

%% Simulate Boolean>>ifTrue:ifFalse: message dispatch
beamtalk_if_true_if_false(true, TrueBlock, _FalseBlock) when is_function(TrueBlock, 0) ->
    TrueBlock();
beamtalk_if_true_if_false(false, _TrueBlock, FalseBlock) when is_function(FalseBlock, 0) ->
    FalseBlock().

%% Simulate Boolean>>ifTrue: message dispatch
beamtalk_if_true(true, TrueBlock) when is_function(TrueBlock, 0) ->
    TrueBlock();
beamtalk_if_true(false, _TrueBlock) ->
    false.

%% Simulate Boolean>>ifFalse: message dispatch
beamtalk_if_false(true, _FalseBlock) ->
    true;
beamtalk_if_false(false, FalseBlock) when is_function(FalseBlock, 0) ->
    FalseBlock().

%% Simulate Boolean>>and: message dispatch
beamtalk_and(true, Block) when is_function(Block, 0) ->
    Block();
beamtalk_and(false, _Block) ->
    false.

%% Simulate Boolean>>or: message dispatch
beamtalk_or(true, _Block) ->
    true;
beamtalk_or(false, Block) when is_function(Block, 0) ->
    Block().

%% Simulate Boolean>>not message dispatch
beamtalk_not(true) ->
    false;
beamtalk_not(false) ->
    true.

%%% ---------------------------------------------------------------------------
%%% ifTrue:ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] ifFalse: [0] => 42
true_if_true_if_false_evaluates_true_block_test() ->
    TrueBlock = fun() -> 42 end,
    FalseBlock = fun() -> 0 end,
    %% True>>ifTrue:ifFalse: evaluates trueBlock
    Result = beamtalk_if_true_if_false(true, TrueBlock, FalseBlock),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] ifFalse: [0] => 0
false_if_true_if_false_evaluates_false_block_test() ->
    TrueBlock = fun() -> 42 end,
    FalseBlock = fun() -> 0 end,
    %% False>>ifTrue:ifFalse: evaluates falseBlock
    Result = beamtalk_if_true_if_false(false, TrueBlock, FalseBlock),
    ?assertEqual(0, Result).

%%% ---------------------------------------------------------------------------
%%% ifTrue: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] => 42
true_if_true_evaluates_block_test() ->
    TrueBlock = fun() -> 42 end,
    %% True>>ifTrue: evaluates the block
    Result = beamtalk_if_true(true, TrueBlock),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] => false
false_if_true_returns_self_test() ->
    _TrueBlock = fun() -> 42 end,
    %% False>>ifTrue: returns self without evaluating
    Result = beamtalk_if_true(false, _TrueBlock),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifFalse: [42] => true
true_if_false_returns_self_test() ->
    _FalseBlock = fun() -> 42 end,
    %% True>>ifFalse: returns self without evaluating
    Result = beamtalk_if_false(true, _FalseBlock),
    ?assertEqual(true, Result).

%% Test: false ifFalse: [42] => 42
false_if_false_evaluates_block_test() ->
    FalseBlock = fun() -> 42 end,
    %% False>>ifFalse: evaluates the block
    Result = beamtalk_if_false(false, FalseBlock),
    ?assertEqual(42, Result).

%%% ---------------------------------------------------------------------------
%%% and: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true and: [true] => true
true_and_evaluates_block_returns_true_test() ->
    AndBlock = fun() -> true end,
    %% True>>and: evaluates the block
    Result = beamtalk_and(true, AndBlock),
    ?assertEqual(true, Result).

%% Test: true and: [false] => false
true_and_evaluates_block_returns_false_test() ->
    AndBlock = fun() -> false end,
    %% True>>and: evaluates the block (can return false)
    Result = beamtalk_and(true, AndBlock),
    ?assertEqual(false, Result).

%% Test: false and: [error(should_not_evaluate)] => false
false_and_short_circuits_test() ->
    %% Block should NOT be evaluated - this verifies short-circuit
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    %% False>>and: returns false without evaluating block
    %% If the implementation incorrectly evaluates ShouldNotRun, this test
    %% will fail with should_not_evaluate error
    Result = beamtalk_and(false, ShouldNotRun),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% or: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true or: [error(should_not_evaluate)] => true
true_or_short_circuits_test() ->
    %% Block should NOT be evaluated - this verifies short-circuit
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    %% True>>or: returns true without evaluating block
    %% If the implementation incorrectly evaluates ShouldNotRun, this test
    %% will fail with should_not_evaluate error
    Result = beamtalk_or(true, ShouldNotRun),
    ?assertEqual(true, Result).

%% Test: false or: [true] => true
false_or_evaluates_block_returns_true_test() ->
    OrBlock = fun() -> true end,
    %% False>>or: evaluates the block
    Result = beamtalk_or(false, OrBlock),
    ?assertEqual(true, Result).

%% Test: false or: [false] => false
false_or_evaluates_block_returns_false_test() ->
    OrBlock = fun() -> false end,
    %% False>>or: evaluates the block (can return false)
    Result = beamtalk_or(false, OrBlock),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% not tests
%%% ---------------------------------------------------------------------------

%% Test: true not => false
true_not_returns_false_test() ->
    %% True>>not returns false
    Result = beamtalk_not(true),
    ?assertEqual(false, Result).

%% Test: false not => true
false_not_returns_true_test() ->
    %% False>>not returns true
    Result = beamtalk_not(false),
    ?assertEqual(true, Result).

%%% ---------------------------------------------------------------------------
%%% Complex control flow tests
%%% ---------------------------------------------------------------------------

%% Test nested conditionals: true ifTrue: [false ifTrue: [1] ifFalse: [2]] => 2
nested_if_true_if_false_test() ->
    InnerTrueBlock = fun() -> 1 end,
    InnerFalseBlock = fun() -> 2 end,
    InnerBlock = fun() ->
        %% false ifTrue:ifFalse: evaluates false block
        beamtalk_if_true_if_false(false, InnerTrueBlock, InnerFalseBlock)
    end,
    %% true ifTrue: evaluates the outer block
    Result = beamtalk_if_true(true, InnerBlock),
    ?assertEqual(2, Result).

%% Test chained logical operations: true and: [true] and: [false] => false
chained_and_operations_test() ->
    FirstAnd = fun() -> true end,
    SecondAnd = fun() -> false end,
    %% true and: [true] => true, then true and: [false] => false
    FirstResult = beamtalk_and(true, FirstAnd),
    ?assertEqual(true, FirstResult),
    %% Since FirstResult is true, evaluate second and:
    Result = beamtalk_and(FirstResult, SecondAnd),
    ?assertEqual(false, Result).

%% Test chained logical operations: false or: [false] or: [true] => true
chained_or_operations_test() ->
    FirstOr = fun() -> false end,
    SecondOr = fun() -> true end,
    %% false or: [false] => false, then false or: [true] => true
    FirstResult = beamtalk_or(false, FirstOr),
    ?assertEqual(false, FirstResult),
    %% Since FirstResult is false, evaluate second or:
    Result = beamtalk_or(FirstResult, SecondOr),
    ?assertEqual(true, Result).
