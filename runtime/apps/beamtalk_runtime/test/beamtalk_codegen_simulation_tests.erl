%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_codegen_simulation_tests).

-moduledoc """
Tests for compiler-generated code patterns using real compiled Beamtalk.

These tests verify runtime behavior by using the counter module compiled
from tests/repl-protocol/fixtures/counter.bt (unified fixture - BT-239). This tests
the actual code generation, not simulated patterns.

**Note:** These are NOT true end-to-end tests. For real E2E tests that
compile actual Beamtalk source files in full, see `tests/repl-protocol/cases/*.bt`.

Test categories:
- spawn/0 tests (Counter spawn) - returns #beamtalk_object{}
- spawn/1 tests (Counter spawnWith: #{...})
- State merging behavior (InitArgs override defaults)
- Async future-cast protocol (BT-79) - futures, awaits, errors, concurrency
  (runtime-only path — see the section header below for why it cannot
  migrate onto compiled `.bt` output)
- Block Evaluation Tests (value, value:, value:value:, closures)
- Control Flow Tests (whileTrue:, whileFalse:, repeat)
- Boolean control flow tests (ifTrue:ifFalse:, and:, or:, not) - migrated
  (BT-3093) onto stdlib/bootstrap-test/booleans.btscript and
  stdlib/test/boolean_short_circuit_test.bt; see the section header below
- Cascade message sends (BT-133)
- Multi-keyword messages (BT-133)
- Actor interaction patterns (BT-133)
- Error handling (BT-133)
- Instance variable access patterns (BT-133)
- Nested message sends and binary operators (BT-133)

BT-3093: Migrated the hand-simulated Counter/Rectangle/Box/Spawner/
instance-var fixtures that stood in for "as the compiler would generate"
output onto real `.bt` fixtures compiled by
runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript
(arithmetic_actor.bt, rectangle_actor.bt, box_actor.bt, spawner_actor.bt,
shadow_actor.bt, coordinate_actor.bt), following the BT-239 precedent.
The async future-cast protocol section is documented in place as staying
simulated: it exercises a `beamtalk_actor:handle_cast/2` code path
(`{Selector, Args, FuturePid}`) that generated per-class `handle_cast/2`
callbacks do not implement, so no compiled source construct reaches it.

See also: beamtalk_actor for the runtime implementation
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ===========================================================================
%%% Test Fixtures - Counter simulation and compiled module
%%% ===========================================================================

%% Note: For spawn/0 and spawn/1 tests, we use the real compiled counter module
%% from tests/repl-protocol/fixtures/counter.bt (unified fixture - BT-239) which generates
%% actual #beamtalk_object{} records.
%%
%% BT-3093: counter_module_state/1 and friends below previously claimed to
%% mirror "the output of compiling" a Counter class — that claim was false.
%% Real compiled classes get their own generated dispatch/4 case-statement
%% module (crates/beamtalk-core/src/codegen/core_erlang/gen_server/*); they
%% never store a `'__methods__'` map of funs in actor state. That funs-table
%% shape is a *runtime* representation: it is what `beamtalk_actor:dispatch/4`
%% and `beamtalk_actor:handle_cast/2`'s async future-cast branch
%% (`{Selector, Args, FuturePid}`) operate on directly, and it is the same
%% shape produced by Beamtalk's dynamic class-building API
%% (`Object classBuilder ... classMethods: #{...}; register`).
%%
%% Everything that can be expressed as compiled `.bt` source and reached via
%% the ordinary `{Selector, Args}` synchronous call protocol has been
%% migrated (BT-3093) onto the fixtures in
%% runtime/apps/beamtalk_runtime/test_fixtures/ (arithmetic_actor.bt,
%% rectangle_actor.bt, box_actor.bt, spawner_actor.bt, shadow_actor.bt,
%% coordinate_actor.bt), following the BT-239 precedent — see the section
%% headers below for what moved where.
%%
%% What is kept here, and why: the async future-cast protocol
%% (`{Selector, Args, FuturePid}` sent via `gen_server:cast/2`) is only
%% implemented by `beamtalk_actor:handle_cast/2`'s generic fallback clause.
%% Confirmed empirically (BT-3093): a real compiled actor's generated
%% `handle_cast/2` (crates/.../gen_server/callbacks.rs `generate_handle_cast`)
%% only matches the `{cast, Selector, Args}` fire-and-forget tag; a bare
%% `{Selector, Args, FuturePid}` cast falls through to its `<_> -> {noreply,
%% State}` catch-all and is silently dropped — verified by spawning
%% 'bt@arithmetic_actor' and observing the future time out. There is no
%% compiled `.bt` source construct that reaches this branch, so per the
%% Consistency-Test Disposition Rule's documented exception ("a runtime-only
%% failure path that has no corresponding source construct"), the async
%% section below stays on this hand-built state+methods fixture, which is a
%% legitimate direct input to `beamtalk_actor:dispatch/4` rather than a
%% "simulated compiler output" fixture.
-spec counter_module_state(InitArgs :: map()) -> map().
counter_module_state(InitArgs) ->
    DefaultState = #{
        '$beamtalk_class' => 'Counter',
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
%%% Tests use 'bt@counter':spawn() from compiled tests/repl-protocol/fixtures/counter.bt
%%% which returns {beamtalk_object, 'Counter', 'bt@counter', Pid}
%%% ===========================================================================

spawn_zero_uses_default_state_test() ->
    %% spawn/0 passes empty map to init, which merges with defaults
    Object = 'bt@counter':spawn(),
    ?assertMatch({beamtalk_object, 'Counter', 'bt@counter', _Pid}, Object),

    %% Extract pid from #beamtalk_object{} record (element 4, 1-indexed)
    Pid = element(4, Object),

    %% Verify default value is 0
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(0, Value),

    gen_server:stop(Pid).

spawn_zero_methods_work_test() ->
    Object = 'bt@counter':spawn(),
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
%%% Tests use 'bt@counter':spawn(InitArgs) with initialization arguments
%%% ===========================================================================

spawn_with_overrides_default_value_test() ->
    %% spawnWith: #{value => 42} passes InitArgs to init
    InitArgs = #{value => 42},
    Object = 'bt@counter':spawn(InitArgs),
    Pid = element(4, Object),

    %% Verify initial value was overridden
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(42, Value),

    gen_server:stop(Pid).

spawn_with_methods_still_work_test() ->
    %% spawnWith: #{value => 100}
    InitArgs = #{value => 100},
    Object = 'bt@counter':spawn(InitArgs),
    Pid = element(4, Object),

    %% Increment from 100
    ?assertEqual({ok, 101}, gen_server:call(Pid, {increment, []})),
    ?assertEqual({ok, 102}, gen_server:call(Pid, {increment, []})),

    gen_server:stop(Pid).

spawn_with_preserves_unspecified_defaults_test() ->
    %% spawnWith: #{extra => foo} - should preserve value default
    InitArgs = #{extra => foo},
    Object = 'bt@counter':spawn(InitArgs),
    Pid = element(4, Object),

    %% Value should still be default (0)
    ?assertEqual({ok, 0}, gen_server:call(Pid, {getValue, []})),

    %% Extra field should be in state
    ActorState = sys:get_state(Pid),
    ?assertEqual(foo, maps:get(extra, ActorState)),

    gen_server:stop(Pid).

%% BT-3093: migrated off counter_module_state/raw start_link onto the real
%% compiled 'bt@counter' module — spawn/1 already merges InitArgs the same
%% way (proven identically by spawn_with_preserves_unspecified_defaults_test
%% above), so this now exercises real codegen instead of a hand-simulated
%% stand-in.
spawn_with_multiple_overrides_test() ->
    %% spawnWith: #{value => 10, extra => bar}
    InitArgs = #{value => 10, extra => bar},
    Object = 'bt@counter':spawn(InitArgs),
    Pid = element(4, Object),

    %% Value was overridden
    ?assertEqual({ok, 10}, gen_server:call(Pid, {getValue, []})),

    %% Extra field present
    ActorState = sys:get_state(Pid),
    ?assertEqual(bar, maps:get(extra, ActorState)),

    gen_server:stop(Pid).

%%% ===========================================================================
%%% Async future-cast protocol tests (BT-79) — DOCUMENTED AS STAYING SIMULATED
%%% ===========================================================================
%%%
%%% BT-3093: kept on counter_module_state/1 deliberately. This section
%%% exercises `beamtalk_actor:handle_cast/2`'s `{Selector, Args, FuturePid}`
%%% branch, which is only reachable through the generic actor callback
%%% module — real compiled classes' generated `handle_cast/2` does not
%%% implement this cast shape (verified empirically; see the
%%% counter_module_state/1 doc comment above for how). There is no `.bt`
%%% source construct that reaches this code path, so per the
%%% Consistency-Test Disposition Rule's documented exception this stays a
%%% direct unit test of beamtalk_actor.erl's own dispatch, not a simulated-
%%% compiler-output fixture.

%%% Basic async message send and future resolution

async_message_creates_future_test() ->
    %% Test: Async message send creates a future
    State = counter_module_state(#{}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),

    %% Simulate: future := actor increment
    Future = beamtalk_future:new(),
    ?assert(beamtalk_future:is_future(Future)),

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
    Result = beamtalk_future:await(Future, 1000),
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
    Result = beamtalk_future:await(Future, 1000),
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
        % Signal that we're about to await
        Parent ! {ready, self()},
        Result = beamtalk_future:await(Future, 2000),
        Parent ! {awaited, Result}
    end),

    %% Wait for the awaiter to signal it's ready
    receive
        {ready, _} -> ok
    after 1000 ->
        % Timeout - spawned process didn't start
        ?assert(false)
    end,

    %% Give a moment for the await to register with the future
    timer:sleep(100),

    %% Now send the async message to resolve the future
    gen_server:cast(Actor, {increment, [], Future}),

    %% Verify the awaiter got the result
    receive
        {awaited, Value} -> ?assertEqual(6, Value)
    after 3000 ->
        % Timeout - test failed
        ?assert(false)
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
    Result1 = beamtalk_future:await(Future, 1000),
    ?assertEqual(21, Result1),

    %% Now await the already-resolved future again with a very small timeout.
    %% This verifies it is already resolved without relying on wall-clock timing.
    SmallTimeout = 10,
    Result2 = beamtalk_future:await(Future, SmallTimeout),

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

    %% Await should throw future_rejected
    ?assertThrow({future_rejected, division_by_zero}, beamtalk_future:await(Future, 1000)),

    gen_server:stop(Actor).

%%% Multiple concurrent async messages

async_multiple_concurrent_messages_test() ->
    %% Test: Multiple concurrent async messages to same actor
    State = counter_module_state(#{value => 0}),
    {ok, Actor} = gen_server:start_link(beamtalk_actor, State, []),

    %% Send 5 concurrent increment messages
    Futures = [
        begin
            F = beamtalk_future:new(),
            gen_server:cast(Actor, {increment, [], F}),
            F
        end
     || _ <- lists:seq(1, 5)
    ],

    %% Await all results
    Results = [beamtalk_future:await(F, 1000) || F <- Futures],

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
    R1 = beamtalk_future:await(F1, 1000),
    R2 = beamtalk_future:await(F2, 1000),
    R3 = beamtalk_future:await(F3, 1000),

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
    Result1 = beamtalk_future:await(Future1, 1000),
    ?assertEqual(1, Result1),

    %% Second async increment (chained after first)
    Future2 = beamtalk_future:new(),
    gen_server:cast(Actor, {increment, [], Future2}),
    Result2 = beamtalk_future:await(Future2, 1000),
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
    R1 = beamtalk_future:await(F1, 1000),
    R2 = beamtalk_future:await(F2, 1000),

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
    Result = beamtalk_future:await(Future, 1000),
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

    %% Await with short timeout should throw structured error
    ?assertThrow(
        #beamtalk_error{
            kind = timeout,
            class = 'Future',
            message = <<"Await timed out">>
        },
        beamtalk_future:await(Future, 100)
    ),

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
    lists:foreach(
        fun(N) ->
            spawn(fun() ->
                % Signal ready to await
                Parent ! {ready, N},
                Result = beamtalk_future:await(Future, 2000),
                Parent ! {waiter, N, Result}
            end)
        end,
        [1, 2, 3]
    ),

    %% Wait for all waiters to signal they're ready
    lists:foreach(
        fun(_) ->
            receive
                {ready, _} -> ok
            after 1000 ->
                ?assert(false)
            end
        end,
        [1, 2, 3]
    ),

    %% Give a moment for all awaits to register
    timer:sleep(100),

    %% Send the async message
    gen_server:cast(Actor, {increment, [], Future}),

    %% All waiters should get the result
    Results = lists:sort([
        receive
            {waiter, N, R} -> {N, R}
        after 3000 ->
            ?assert(false)
        end
     || _ <- [1, 2, 3]
    ]),

    ?assertEqual([{1, 1}, {2, 1}, {3, 1}], Results),

    gen_server:stop(Actor).

%%% ===========================================================================
%%% Edge cases — MIGRATED / TRIMMED (BT-3093)
%%% ===========================================================================
%%%
%%% spawn_with_empty_map_same_as_spawn_zero_test was removed: it only
%%% asserted a property of counter_module_state/1's own `maps:merge/2` call
%%% (that spawn() and spawn(#{}) merge identically) — plain Erlang map
%%% semantics, not Beamtalk codegen behavior, and already implied by the
%%% real-dispatch coverage in the spawn/0 and spawn/1 sections above
%%% (spawn_zero_uses_default_state_test,
%%% spawn_with_preserves_unspecified_defaults_test, etc., all against the
%%% real compiled 'bt@counter' module). No scenario coverage was lost.
%%%
%%% spawn_preserves_class_and_methods_test is kept below (not removed) —
%%% see its own doc comment for why.
%%%
%%% spawn_with_nil_values_override_test migrated onto real 'bt@counter'
%%% spawn/1, since that scenario (spawnWith: #{value => nil}) was not
%%% otherwise covered against real compiled output.

spawn_with_nil_values_override_test() ->
    %% spawnWith: #{value => nil} should set value to nil
    Object = 'bt@counter':spawn(#{value => nil}),
    Pid = element(4, Object),

    Result = gen_server:call(Pid, {getValue, []}),
    ?assertEqual({ok, nil}, Result),

    gen_server:stop(Pid).

%% BT-3093: kept as a direct unit test of the counter_module_state/1 helper
%% itself, since the async future-cast section above still depends on that
%% helper producing a `'__methods__'` funs table (see its doc comment).
spawn_preserves_class_and_methods_test() ->
    %% Verify $beamtalk_class and __methods__ are preserved after merge
    InitArgs = #{value => 999},
    State = counter_module_state(InitArgs),

    ?assertEqual('Counter', maps:get('$beamtalk_class', State)),
    ?assert(is_map(maps:get('__methods__', State))),

    %% Methods should still be functions
    Methods = maps:get('__methods__', State),
    ?assert(is_function(maps:get(increment, Methods), 2)),
    ?assert(is_function(maps:get(getValue, Methods), 2)).

%%% ===========================================================================
%%% Block Evaluation Tests (value, value:, value:value:, etc.)
%%% ===========================================================================
%%%
%%% BT-3093 disposition note: the tests from here through the "Complex
%%% Message Send Patterns" section below use plain Erlang `fun`s to probe
%%% closure/control-flow mechanics on BEAM directly (capture, nesting,
%%% arity, whileTrue:/whileFalse:/repeat, binary-operator precedence).
%%% Unlike the Counter/Rectangle/Box/Boolean fixtures elsewhere in this
%%% file, none of these carry an "as the compiler would generate" claim
%%% about a specific compiled structure, so they are not the fixture the
%%% Consistency-Test Disposition Rule (architecture-principles.md § 7)
%%% targets — there is no compiled shape here that can drift out of sync.
%%% Equivalent scenarios are additionally proven end-to-end against real
%%% compiled Beamtalk in stdlib/bootstrap-test/blocks.btscript,
%%% stdlib/test/block_evaluation_test.bt, stdlib/test/blocks_test.bt, and
%%% stdlib/test/while_value_test.bt, so no coverage gap exists either way.
%%% Left as-is.

-doc """
Test: [42] value => 42
Block with no arguments, evaluated with value message
""".
block_value_no_args_test() ->
    Block = fun() -> 42 end,
    Result = Block(),
    ?assertEqual(42, Result).

-doc """
Test: [:x | x + 1] value: 5 => 6
Block with one parameter, evaluated with value: message
""".
block_value_one_arg_test() ->
    Block = fun(X) -> X + 1 end,
    Result = Block(5),
    ?assertEqual(6, Result).

-doc """
Test: [:x :y | x + y] value: 3 value: 4 => 7
Block with two parameters, evaluated with value:value: message
""".
block_value_two_args_test() ->
    Block = fun(X, Y) -> X + Y end,
    Result = Block(3, 4),
    ?assertEqual(7, Result).

-doc """
Test: [:x :y :z | x + y + z] value: 1 value: 2 value: 3 => 6
Block with three parameters, evaluated with value:value:value: message
""".
block_value_three_args_test() ->
    Block = fun(X, Y, Z) -> X + Y + Z end,
    Result = Block(1, 2, 3),
    ?assertEqual(6, Result).

-doc """
Test: [:x | [:y | x + y]] value: 10 value: 5 => 15
Nested blocks - outer block returns inner block, which captures x
""".
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

-doc """
Test closures capture lexical scope
captured := 100. [:x | x + captured] value: 5 => 105
""".
block_captures_lexical_scope_test() ->
    %% Simulate captured variable from lexical scope
    Captured = 100,

    %% Block captures Captured
    Block = fun(X) -> X + Captured end,

    %% Evaluate block
    Result = Block(5),
    ?assertEqual(105, Result).

-doc """
Test block that returns another block
makeAdder := [:n | [:x | x + n]].
addFive := makeAdder value: 5.
addFive value: 10 => 15
""".
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

-doc """
Test block with multiple statements
[:x | temp := x * 2. temp + 1] value: 5 => 11
""".
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

-doc """
Test: [counter < 5] whileTrue: [counter := counter + 1]
Loop while condition is true
""".
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

-doc """
Test: [counter >= 10] whileFalse: [counter := counter + 1]
Loop while condition is false
""".
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

-doc """
Test: [counter < 100] whileTrue: [counter := counter + 1. process: counter]
whileTrue: executes body multiple times
""".
block_while_true_accumulates_test() ->
    %% Sum numbers 1 to 10
    {_Counter, Sum} = while_true_accumulate(1, 0),
    %% 1+2+3+...+10 = 55
    ?assertEqual(55, Sum).

while_true_accumulate(Counter, Sum) ->
    case Counter =< 10 of
        true ->
            NewSum = Sum + Counter,
            NewCounter = Counter + 1,
            while_true_accumulate(NewCounter, NewSum);
        false ->
            {Counter, Sum}
    end.

-doc """
Test: repeat loop with early termination
counter := 0. [counter := counter + 1. counter < 5] repeat => loops forever
We simulate with a bounded version that terminates
""".
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
            %% Early return simulates ^counter
            NewCounter;
        false ->
            repeat_until_threshold(NewCounter, Threshold)
    end.

-doc """
Test: nested loops with whileTrue:
outer := 0. inner := 0.
[outer < 3] whileTrue: [
  inner := 0.
  [inner < 3] whileTrue: [inner := inner + 1].
  outer := outer + 1
]
""".
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
%%% Boolean control flow tests — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% This section used to hand-roll `beamtalk_if_true_if_false/3`,
%%% `beamtalk_if_true/2`, `beamtalk_if_false/2`, `beamtalk_and/2`,
%%% `beamtalk_or/2`, `beamtalk_not/1` as Erlang stand-ins for "the exact
%%% semantics that compiled Beamtalk Boolean methods must produce" — a
%%% textbook case of the hand-written "simulated compiler output" fixture
%%% the Consistency-Test Disposition Rule
%%% (docs/development/architecture-principles.md § 7) says to migrate onto
%%% real compiled/generated output, per the BT-239 precedent.
%%%
%%% Real compiled coverage already existed and needed no new fixture:
%%% - Value-correctness for ifTrue:ifFalse:, ifTrue:, ifFalse:, and:, or:,
%%%   not (and xor:) is exercised end-to-end against the real compiled
%%%   True/False/Boolean stdlib classes (stdlib/src/true.bt, false.bt,
%%%   boolean.bt) by stdlib/bootstrap-test/booleans.btscript.
%%% - Short-circuit behavior (the unused block must never run, not just
%%%   "the result happens to be right") is proven against the same real
%%%   compiled classes by stdlib/test/boolean_short_circuit_test.bt, added
%%%   as part of this migration using the AtomicCounter side-effect idiom
%%%   already established in stdlib/test/blocks_test.bt.
%%%
%%% Deleting the Erlang re-implementation here removes the drift risk the
%%% disposition rule flags: the hand-rolled clauses could silently diverge
%%% from true.bt/false.bt's actual dispatch without either test file
%%% failing.

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
%%% Cascade Message Send Tests (BT-133) — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% Migrated off counter_module_state/raw start_link onto the real compiled
%%% 'bt@counter' module (tests/repl-protocol/fixtures/counter.bt).

%% Test: Cascade - multiple messages to same actor
%% Simulates: counter increment; increment; getValue
cascade_multiple_messages_test() ->
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),

    %% Send cascade of messages to same actor
    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),
    Result = gen_server:call(Pid, {getValue, []}),

    ?assertEqual({ok, 2}, Result),
    gen_server:stop(Pid).

%% Test: Cascade with mixed message types
%% Simulates: counter increment; getValue; increment
cascade_mixed_operations_test() ->
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {increment, []}),
    Value1 = gen_server:call(Pid, {getValue, []}),
    gen_server:call(Pid, {increment, []}),
    Value2 = gen_server:call(Pid, {getValue, []}),

    ?assertEqual({ok, 1}, Value1),
    ?assertEqual({ok, 2}, Value2),
    gen_server:stop(Pid).

%% Test: Cascade returns last message result
%% Simulates: counter increment; increment; increment
cascade_returns_last_result_test() ->
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),
    LastResult = gen_server:call(Pid, {increment, []}),

    ?assertEqual({ok, 3}, LastResult),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Multi-Keyword Message Tests (BT-133) — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% Migrated the hand-simulated rectangle_module_state/box_module_state
%%% fixtures onto real compiled dispatch: rectangle_actor.bt exercises a
%%% two-keyword message (width:height:), box_actor.bt a three-keyword
%%% message (width:height:depth:) — both compiled by
%%% runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript.

%% Test: Multi-keyword message with two arguments
%% Simulates: rect width: 5 height: 3
multi_keyword_two_args_test() ->
    Object = 'bt@rectangle_actor':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {'width:height:', [5, 3]}),
    Area = gen_server:call(Pid, {area, []}),

    ?assertEqual({ok, 15}, Area),
    gen_server:stop(Pid).

%% Test: Multi-keyword message with three arguments
%% Simulates: box width: 2 height: 3 depth: 4
multi_keyword_three_args_test() ->
    Object = 'bt@box_actor':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {'width:height:depth:', [2, 3, 4]}),
    Volume = gen_server:call(Pid, {volume, []}),

    ?assertEqual({ok, 24}, Volume),
    gen_server:stop(Pid).

%%% ===========================================================================
%%% Actor Interaction Patterns (BT-133) — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% Migrated the hand-simulated spawner_module_state fixture (and its raw
%%% counter_module_state siblings) onto real compiled dispatch:
%%% spawner_actor.bt's spawnChild method spawns an ArithmeticActor
%%% (arithmetic_actor.bt) from within a compiled method body — proving
%%% actor-to-actor `ClassName spawn` against real codegen.

%% Test: Actor A spawns Actor B
actor_spawns_another_actor_test() ->
    Object = 'bt@spawner_actor':spawn(),
    Spawner = element(4, Object),

    try
        %% Spawner creates an ArithmeticActor; spawnChild returns the
        %% #beamtalk_object{} spawn() produces, same as any other spawn call.
        {ok, ChildObject} = gen_server:call(Spawner, {spawnChild, []}),
        ?assertMatch({beamtalk_object, 'ArithmeticActor', 'bt@arithmetic_actor', _}, ChildObject),
        ChildPid = element(4, ChildObject),
        ?assert(is_pid(ChildPid)),

        %% Verify the child actor works
        gen_server:call(ChildPid, {increment, []}),
        Value = gen_server:call(ChildPid, {getValue, []}),
        ?assertEqual({ok, 1}, Value),

        gen_server:stop(ChildPid)
    after
        gen_server:stop(Spawner)
    end.

%% Test: Actors communicating via messages
actors_communicate_test() ->
    %% Create two actors
    Object1 = 'bt@arithmetic_actor':spawn(),
    Object2 = 'bt@arithmetic_actor':spawn(),
    Actor1 = element(4, Object1),
    Actor2 = element(4, Object2),

    try
        %% Increment actor1 twice
        gen_server:call(Actor1, {increment, []}),
        gen_server:call(Actor1, {increment, []}),

        %% Get value from actor1 and set it in actor2 (simulated)
        {ok, Value1} = gen_server:call(Actor1, {getValue, []}),

        %% Increment actor2 that many times
        [gen_server:call(Actor2, {increment, []}) || _ <- lists:seq(1, Value1)],

        {ok, Value2} = gen_server:call(Actor2, {getValue, []}),
        ?assertEqual(Value1, Value2)
    after
        gen_server:stop(Actor1),
        gen_server:stop(Actor2)
    end.

%% Test: Actor chain - A spawns B, B spawns C
actor_spawn_chain_test() ->
    Object1 = 'bt@spawner_actor':spawn(),
    Spawner1 = element(4, Object1),

    %% Spawner1's sibling Spawner2 creates the child actor
    Object2 = 'bt@spawner_actor':spawn(),
    Spawner2 = element(4, Object2),

    try
        %% Spawner2 creates a child ArithmeticActor
        {ok, ChildObject} = gen_server:call(Spawner2, {spawnChild, []}),
        ChildPid = element(4, ChildObject),

        %% Use the child actor
        gen_server:call(ChildPid, {increment, []}),
        gen_server:call(ChildPid, {increment, []}),
        Value = gen_server:call(ChildPid, {getValue, []}),

        ?assertEqual({ok, 2}, Value),

        gen_server:stop(ChildPid)
    after
        gen_server:stop(Spawner2),
        gen_server:stop(Spawner1)
    end.

%%% ===========================================================================
%%% Error Handling Tests (BT-133) — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% Migrated off counter_module_state onto real compiled dispatch
%%% ('bt@counter' / 'bt@arithmetic_actor'). Doing so surfaced real drift in
%%% the hand-simulated protocol this section used to assert: the fictional
%%% `counter_divide/2` returned bare `{error, division_by_zero}` /
%%% `#beamtalk_error{kind = type_error}` shapes that don't match what real
%%% codegen produces. The corrected assertions below were captured by
%%% spawning 'bt@arithmetic_actor' directly and observing
%%% `beamtalk_actor:safe_dispatch/3`'s actual catch shape (matches the
%%% already-migrated extension_error_propagation_test's
%%% `{error, {error, Reason, Stacktrace}}` pattern) and the generated
%%% dispatch/4 arity-mismatch clause (`{'reply', {'error', 'bad_arity'},
%%% State}`, see crates/beamtalk-core/src/codegen/core_erlang/gen_server/
%%% dispatch.rs and methods.rs).

%% Test: Method not found error
%%
%% BT-3093: the DNU fallback for a real compiled actor walks the class
%% hierarchy (beamtalk_dispatch:super/5) up to 'Actor', which is registered
%% asynchronously by beamtalk_stdlib's background class loader at
%% application startup. Earlier tests in this suite never happen to need
%% that walk (they only call methods the compiled module handles directly),
%% so this is the first point 'Actor' must actually be present —
%% wait_for_class/2 makes that a deterministic precondition instead of an
%% incidental side effect of how many slower tests ran first.
method_not_found_error_test() ->
    wait_for_class('Actor', 5000),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),

    %% Try to call non-existent method
    Result = gen_server:call(Pid, {nonExistentMethod, []}),

    %% Should get does_not_understand error
    ?assertMatch(
        {error, #beamtalk_error{
            kind = does_not_understand,
            class = 'Counter',
            selector = nonExistentMethod
        }},
        Result
    ),

    gen_server:stop(Pid).

%% Test: Division by zero error
%% BT-3093: real division by zero raises a raw `badarith` inside the
%% compiled method body, caught by safe_dispatch/3's generic try/catch
%% (crates/beamtalk-core/src/codegen/core_erlang/gen_server/dispatch.rs
%% generate_safe_dispatch: `catch <Type, Error, Stacktrace> -> {'error',
%% {Type, Error, Stacktrace}, State}`) — not the bare `division_by_zero`
%% atom the old hand-simulated counter_divide/2 fabricated.
division_by_zero_error_test() ->
    Object = 'bt@arithmetic_actor':spawn(),
    Pid = element(4, Object),

    %% Try to divide by zero
    Result = gen_server:call(Pid, {'divide:', [0]}),

    ?assertMatch({error, {error, badarith, _Stacktrace}}, Result),

    gen_server:stop(Pid).

%% Test: Wrong number of arguments error
%% BT-3093: real generated dispatch matches a known selector against a
%% fixed-arity Args pattern and falls through to an explicit bad_arity
%% reply clause on a mismatch — it's a normal (successful) call reply
%% carrying an error value, not a raised #beamtalk_error{} exception as the
%% old hand-simulated version (which relied on Erlang function-clause
%% matching, an implementation detail the real compiler doesn't share).
wrong_arg_count_error_test() ->
    Object = 'bt@arithmetic_actor':spawn(),
    Pid = element(4, Object),

    %% divide: expects 1 arg, give it 2
    Result = gen_server:call(Pid, {'divide:', [5, 10]}),

    ?assertEqual({ok, {error, bad_arity}}, Result),

    gen_server:stop(Pid).

%% Test: Actor crash and recovery simulation
actor_crash_simulation_test() ->
    %% Create an actor
    Object = 'bt@arithmetic_actor':spawn(),
    Pid = element(4, Object),

    %% Use it normally
    gen_server:call(Pid, {increment, []}),
    Value1 = gen_server:call(Pid, {getValue, []}),
    ?assertEqual({ok, 1}, Value1),

    %% Simulate crash by stopping it
    gen_server:stop(Pid),

    %% Create a new one (simulates restart) with fresh default state
    NewObject = 'bt@arithmetic_actor':spawn(),
    NewPid = element(4, NewObject),
    Value2 = gen_server:call(NewPid, {getValue, []}),

    %% New actor has fresh state
    ?assertEqual({ok, 0}, Value2),

    gen_server:stop(NewPid).

%%% ===========================================================================
%%% Instance Variable Access Patterns (BT-133) — MIGRATED (BT-3093)
%%% ===========================================================================
%%%
%%% Migrated the hand-simulated ModuleState maps onto real compiled
%%% dispatch: shadow_actor.bt (parameter name shadows an instance variable)
%%% and coordinate_actor.bt (multiple instance variables mutated by one
%%% keyword message), both compiled by
%%% runtime/apps/beamtalk_runtime/test_fixtures/compile_fixtures.escript.

%% Test: Instance variable shadowing
%% When a method param has same name as instance var, param takes precedence
instance_var_shadowing_test() ->
    Object = 'bt@shadow_actor':spawn(),
    Pid = element(4, Object),

    %% Instance var value is 42
    InstanceValue = gen_server:call(Pid, {getInstanceValue, []}),
    ?assertEqual({ok, 42}, InstanceValue),

    %% But param value is independent
    ParamValue = gen_server:call(Pid, {'echoParam:', [99]}),
    ?assertEqual({ok, 99}, ParamValue),

    %% Instance var unchanged
    StillInstanceValue = gen_server:call(Pid, {getInstanceValue, []}),
    ?assertEqual({ok, 42}, StillInstanceValue),

    gen_server:stop(Pid).

%% Test: Multiple instance variables
multiple_instance_vars_test() ->
    Object = 'bt@coordinate_actor':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {'setX:y:', [10, 20]}),
    Sum = gen_server:call(Pid, {sumCoordinates, []}),

    ?assertEqual({ok, 30}, Sum),

    gen_server:stop(Pid).

%% Test: Instance variable persistence across method calls
instance_var_persistence_test() ->
    Object = 'bt@arithmetic_actor':spawn(),
    Pid = element(4, Object),

    %% Increment modifies instance var
    gen_server:call(Pid, {increment, []}),
    gen_server:call(Pid, {increment, []}),

    %% Value persists
    Value = gen_server:call(Pid, {getValue, []}),
    ?assertEqual({ok, 2}, Value),

    %% Increment again
    gen_server:call(Pid, {increment, []}),

    %% Still persists
    NewValue = gen_server:call(Pid, {getValue, []}),
    ?assertEqual({ok, 3}, NewValue),

    gen_server:stop(Pid).

%%% ===========================================================================
%%% Nested Message Send Tests (BT-133)
%%% ===========================================================================

%% Test: Nested unary messages
%% Simulates: counter getValue getValue (if getValue returned an object)
%% BT-3093: migrated off counter_module_state onto the real compiled
%% 'bt@counter' module.
nested_message_sends_simulation_test() ->
    %% This simulates the pattern, not actual nesting since getValue returns int
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),

    gen_server:call(Pid, {increment, []}),
    Value = gen_server:call(Pid, {getValue, []}),

    %% If we had an object wrapper, we'd send another message
    %% For now, just verify the value is what we expect
    ?assertEqual({ok, 1}, Value),

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
%%% See also: tests/fixtures/logging_counter.bt
%%% See also: BT-152 for runtime super_dispatch/3 implementation
%%%
%%% **DISABLED (BT-211):** These tests are commented out because subclass init
%%% doesn't include inherited state fields from parent classes. The compiled
%%% logging_counter module doesn't have 'value' field from Counter, so super
%%% dispatch fails with badarg when accessing missing state.
%%% Re-enable when BT-211 is fixed.

%% Setup helper for super tests
%% Both counter and logging_counter have register_class/0 from class syntax.
%% We call register_class() directly (not via on_load) because on_load uses
%% start_link which links to the module loader — the process dies when
%% on_load returns. Direct calls keep processes alive in the test process.
setup_super_test_classes() ->
    %% Ensure test fixtures directory is in code path
    code:add_path("_build/test/lib/beamtalk_runtime/test"),

    %% Ensure application is started (bootstrap classes, pg, etc.)
    application:ensure_all_started(beamtalk_runtime),

    %% Load modules and register classes explicitly
    {module, 'bt@counter'} = code:ensure_loaded('bt@counter'),
    case beamtalk_class_registry:whereis_class('Counter') of
        undefined -> 'bt@counter':register_class();
        _ -> ok
    end,

    {module, 'bt@logging_counter'} = code:ensure_loaded('bt@logging_counter'),
    case beamtalk_class_registry:whereis_class('LoggingCounter') of
        undefined -> 'bt@logging_counter':register_class();
        _ -> ok
    end,
    ok.

%% BT-3093: block until ClassName is registered in the class registry, or
%% raise if TimeoutMs elapses first. beamtalk_stdlib registers stdlib
%% classes (including 'Actor') asynchronously in the background at
%% application startup, so tests whose real-codegen DNU path needs a
%% hierarchy walk up to 'Actor' can otherwise run before it's ready.
-spec wait_for_class(atom(), non_neg_integer()) -> ok.
wait_for_class(ClassName, TimeoutMs) ->
    application:ensure_all_started(beamtalk_runtime),
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_class_loop(ClassName, Deadline).

wait_for_class_loop(ClassName, Deadline) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    error({class_not_registered, ClassName});
                false ->
                    timer:sleep(10),
                    wait_for_class_loop(ClassName, Deadline)
            end;
        _Pid ->
            ok
    end.

%% ==========================================================================
%% Super Keyword Tests (BT-108, BT-211)
%%
%% These tests use the compiled logging_counter module and verify super
%% dispatch in an inheritance hierarchy: Actor -> Counter -> LoggingCounter
%%
%% Previously disabled due to BT-211 (subclass init didn't merge InitArgs).
%% Fixed: codegen now does parent defaults → child defaults → user InitArgs.
%% ==========================================================================

%% Test: Super dispatch calls parent method
%% LoggingCounter increment calls Counter increment via super
super_calls_parent_method_test() ->
    setup_super_test_classes(),

    %% Create logging counter with initial state
    Object = 'bt@logging_counter':spawn(),
    ?assertMatch({beamtalk_object, 'LoggingCounter', 'bt@logging_counter', _Pid}, Object),

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
super_multiple_calls_test() ->
    setup_super_test_classes(),

    Object = 'bt@logging_counter':spawn(),
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
super_with_different_method_test() ->
    setup_super_test_classes(),

    InitArgs = #{value => 42},
    Object = 'bt@logging_counter':spawn(InitArgs),
    Pid = element(4, Object),

    %% getValue calls super getValue (Counter's version)
    {ok, Value} = gen_server:call(Pid, {getValue, []}),
    ?assertEqual(42, Value),

    gen_server:stop(Pid).

%% Test: Child adds new methods alongside super
super_with_new_methods_test() ->
    setup_super_test_classes(),

    Object = 'bt@logging_counter':spawn(),
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
super_maintains_state_test() ->
    setup_super_test_classes(),

    Object = 'bt@logging_counter':spawn(),
    Pid = element(4, Object),

    %% Mix calls to overridden and non-overridden methods

    % Calls super
    {ok, 1} = gen_server:call(Pid, {increment, []}),
    % Calls super
    {ok, 1} = gen_server:call(Pid, {getValue, []}),
    % Calls super
    {ok, 2} = gen_server:call(Pid, {increment, []}),
    % Calls super
    {ok, 2} = gen_server:call(Pid, {getValue, []}),

    %% Both state variables updated correctly
    {ok, LogCount} = gen_server:call(Pid, {getLogCount, []}),
    ?assertEqual(2, LogCount),

    gen_server:stop(Pid).

%% Test: Super with initial state override
super_with_init_args_test() ->
    setup_super_test_classes(),

    InitArgs = #{value => 100, logCount => 5},
    Object = 'bt@logging_counter':spawn(InitArgs),
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

%%% ===========================================================================
%%% Extension method tests (BT-229)
%%%
%%% Tests that extension methods work on compiled Actor classes.
%%% Uses 'bt@counter':spawn() from compiled tests/repl-protocol/fixtures/counter.bt.
%%% ===========================================================================

%% Test: Extension method dispatches on compiled Actor
extension_method_on_compiled_actor_test() ->
    beamtalk_extensions:init(),
    %% BT-1512: Extension funs take (Args, Self, State) and return {Result, NewState}
    ExtFun = fun([], _Self, State) -> {42, State} end,
    beamtalk_extensions:register('Counter', testExtension, ExtFun, test_owner),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        {ok, Result} = gen_server:call(Pid, {testExtension, []}),
        ?assertEqual(42, Result)
    after
        gen_server:stop(Pid),
        ets:delete(beamtalk_extensions, {'Counter', testExtension})
    end.

%% Test: Extension method receives Args correctly
extension_method_with_args_test() ->
    beamtalk_extensions:init(),
    ExtFun = fun([N], _Self, State) -> {N * 10, State} end,
    beamtalk_extensions:register('Counter', 'scaleBy:', ExtFun, test_owner),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        {ok, Result} = gen_server:call(Pid, {'scaleBy:', [5]}),
        ?assertEqual(50, Result)
    after
        gen_server:stop(Pid),
        ets:delete(beamtalk_extensions, {'Counter', 'scaleBy:'})
    end.

%% Test: Extension doesn't interfere with regular methods
extension_coexists_with_regular_methods_test() ->
    beamtalk_extensions:init(),
    ExtFun = fun([], _Self, State) -> {extended, State} end,
    beamtalk_extensions:register('Counter', myExtension, ExtFun, test_owner),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        {ok, 0} = gen_server:call(Pid, {getValue, []}),
        {ok, 1} = gen_server:call(Pid, {increment, []}),
        {ok, extended} = gen_server:call(Pid, {myExtension, []})
    after
        gen_server:stop(Pid),
        ets:delete(beamtalk_extensions, {'Counter', myExtension})
    end.

%% Test: Unregistered extension falls through to DNU error
%% BT-3093: see method_not_found_error_test's comment — the DNU fallback
%% walks the class hierarchy up to 'Actor', so wait for it deterministically
%% rather than relying on enough earlier tests having run first.
unregistered_extension_gives_dnu_test() ->
    wait_for_class('Actor', 5000),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        Result = gen_server:call(Pid, {nonExistentMethod, []}),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result)
    after
        gen_server:stop(Pid)
    end.

%% Test: BT-1512 — Extension method correctly threads state mutations
extension_method_state_threading_test() ->
    beamtalk_extensions:init(),
    %% Extension that mutates the 'value' field in State
    ExtFun = fun([], _Self, State) ->
        OldValue = maps:get('value', State, 0),
        NewState = maps:put('value', OldValue + 100, State),
        {OldValue + 100, NewState}
    end,
    beamtalk_extensions:register('Counter', 'addHundred', ExtFun, test_owner),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        %% Initial value is 0
        {ok, 0} = gen_server:call(Pid, {getValue, []}),
        %% Extension mutates state: value becomes 100
        {ok, 100} = gen_server:call(Pid, {'addHundred', []}),
        %% State mutation persisted: getValue should return 100
        {ok, 100} = gen_server:call(Pid, {getValue, []}),
        %% Call extension again: value becomes 200
        {ok, 200} = gen_server:call(Pid, {'addHundred', []}),
        {ok, 200} = gen_server:call(Pid, {getValue, []})
    after
        gen_server:stop(Pid),
        ets:delete(beamtalk_extensions, {'Counter', 'addHundred'})
    end.

%% Test: Extension that throws propagates error through safe_dispatch
extension_error_propagation_test() ->
    beamtalk_extensions:init(),
    CrashFun = fun([], _Self, _State) -> error(extension_crash) end,
    beamtalk_extensions:register('Counter', crashMethod, CrashFun, test_owner),
    Object = 'bt@counter':spawn(),
    Pid = element(4, Object),
    try
        Result = gen_server:call(Pid, {crashMethod, []}),
        %% BT-1822: safe_dispatch now includes stacktrace in error tuple
        ?assertMatch({error, {error, extension_crash, _Stacktrace}}, Result),
        {ok, 0} = gen_server:call(Pid, {getValue, []})
    after
        gen_server:stop(Pid),
        ets:delete(beamtalk_extensions, {'Counter', crashMethod})
    end.
