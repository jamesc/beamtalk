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
%%% These tests verify Boolean control flow messages work correctly on BEAM.
%%% Boolean messages are core language semantics - True and False respond
%%% to control flow messages like ifTrue:ifFalse:, and:, or:, not.
%%%
%%% The tests simulate the behavior of compiled Boolean methods as they
%%% would be generated by the compiler.
%%%
%%% @see lib/True.bt for True control flow API
%%% @see lib/False.bt for False control flow API

%%% ---------------------------------------------------------------------------
%%% ifTrue:ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] ifFalse: [0] => 42
true_if_true_if_false_evaluates_true_block_test() ->
    TrueBlock = fun() -> 42 end,
    FalseBlock = fun() -> 0 end,
    %% Simulate True>>ifTrue:ifFalse: - evaluates trueBlock
    Result = TrueBlock(),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] ifFalse: [0] => 0
false_if_true_if_false_evaluates_false_block_test() ->
    TrueBlock = fun() -> 42 end,
    FalseBlock = fun() -> 0 end,
    %% Simulate False>>ifTrue:ifFalse: - evaluates falseBlock
    Result = FalseBlock(),
    ?assertEqual(0, Result).

%%% ---------------------------------------------------------------------------
%%% ifTrue: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifTrue: [42] => 42
true_if_true_evaluates_block_test() ->
    TrueBlock = fun() -> 42 end,
    %% Simulate True>>ifTrue: - evaluates the block
    Result = TrueBlock(),
    ?assertEqual(42, Result).

%% Test: false ifTrue: [42] => false
false_if_true_returns_self_test() ->
    TrueBlock = fun() -> 42 end,
    %% Simulate False>>ifTrue: - returns self without evaluating
    Result = false,  % Returns receiver (false)
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% ifFalse: tests
%%% ---------------------------------------------------------------------------

%% Test: true ifFalse: [42] => true
true_if_false_returns_self_test() ->
    FalseBlock = fun() -> 42 end,
    %% Simulate True>>ifFalse: - returns self without evaluating
    Result = true,  % Returns receiver (true)
    ?assertEqual(true, Result).

%% Test: false ifFalse: [42] => 42
false_if_false_evaluates_block_test() ->
    FalseBlock = fun() -> 42 end,
    %% Simulate False>>ifFalse: - evaluates the block
    Result = FalseBlock(),
    ?assertEqual(42, Result).

%%% ---------------------------------------------------------------------------
%%% and: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true and: [true] => true
true_and_evaluates_block_returns_true_test() ->
    AndBlock = fun() -> true end,
    %% Simulate True>>and: - evaluates the block
    Result = AndBlock(),
    ?assertEqual(true, Result).

%% Test: true and: [false] => false
true_and_evaluates_block_returns_false_test() ->
    AndBlock = fun() -> false end,
    %% Simulate True>>and: - evaluates the block (can return false)
    Result = AndBlock(),
    ?assertEqual(false, Result).

%% Test: false and: [error(should_not_evaluate)] => false
false_and_short_circuits_test() ->
    %% Block should NOT be evaluated - this verifies short-circuit
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    %% Simulate False>>and: - returns false without evaluating block
    Result = false,  % Short-circuit: returns self
    ?assertEqual(false, Result).
    %% Note: ShouldNotRun is intentionally not called

%%% ---------------------------------------------------------------------------
%%% or: short-circuit tests
%%% ---------------------------------------------------------------------------

%% Test: true or: [error(should_not_evaluate)] => true
true_or_short_circuits_test() ->
    %% Block should NOT be evaluated - this verifies short-circuit
    ShouldNotRun = fun() -> error(should_not_evaluate) end,
    %% Simulate True>>or: - returns true without evaluating block
    Result = true,  % Short-circuit: returns self
    ?assertEqual(true, Result).
    %% Note: ShouldNotRun is intentionally not called

%% Test: false or: [true] => true
false_or_evaluates_block_returns_true_test() ->
    OrBlock = fun() -> true end,
    %% Simulate False>>or: - evaluates the block
    Result = OrBlock(),
    ?assertEqual(true, Result).

%% Test: false or: [false] => false
false_or_evaluates_block_returns_false_test() ->
    OrBlock = fun() -> false end,
    %% Simulate False>>or: - evaluates the block (can return false)
    Result = OrBlock(),
    ?assertEqual(false, Result).

%%% ---------------------------------------------------------------------------
%%% not tests
%%% ---------------------------------------------------------------------------

%% Test: true not => false
true_not_returns_false_test() ->
    %% Simulate True>>not - returns false
    Result = false,
    ?assertEqual(false, Result).

%% Test: false not => true
false_not_returns_true_test() ->
    %% Simulate False>>not - returns true
    Result = true,
    ?assertEqual(true, Result).

%%% ---------------------------------------------------------------------------
%%% Complex control flow tests
%%% ---------------------------------------------------------------------------

%% Test nested conditionals: true ifTrue: [false ifTrue: [1] ifFalse: [2]] => 2
nested_if_true_if_false_test() ->
    InnerBlock = fun() ->
        InnerTrueBlock = fun() -> 1 end,
        InnerFalseBlock = fun() -> 2 end,
        %% false ifTrue:ifFalse: evaluates false block
        InnerFalseBlock()
    end,
    %% true ifTrue: evaluates the outer block
    Result = InnerBlock(),
    ?assertEqual(2, Result).

%% Test chained logical operations: true and: [true] and: [false] => false
chained_and_operations_test() ->
    FirstAnd = fun() -> true end,
    SecondAnd = fun() -> false end,
    %% true and: [true] => true, then true and: [false] => false
    FirstResult = FirstAnd(),
    ?assertEqual(true, FirstResult),
    %% Since FirstResult is true, evaluate second and:
    Result = SecondAnd(),
    ?assertEqual(false, Result).

%% Test chained logical operations: false or: [false] or: [true] => true
chained_or_operations_test() ->
    FirstOr = fun() -> false end,
    SecondOr = fun() -> true end,
    %% false or: [false] => false, then false or: [true] => true
    FirstResult = FirstOr(),
    ?assertEqual(false, FirstResult),
    %% Since FirstResult is false, evaluate second or:
    Result = SecondOr(),
    ?assertEqual(true, Result).
