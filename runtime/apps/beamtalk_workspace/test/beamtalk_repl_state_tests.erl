%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_state module
%%%
%%% Tests REPL state management: bindings, counters, loaded modules, and configuration.

-module(beamtalk_repl_state_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% State creation tests

new_default_test() ->
    State = beamtalk_repl_state:new(undefined, 1234),
    ?assertEqual(undefined, beamtalk_repl_state:get_listen_socket(State)),
    ?assertEqual(1234, beamtalk_repl_state:get_port(State)),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)),
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

new_with_socket_test() ->
    %% Create a dummy socket (self() will work for testing)
    MockSocket = self(),
    State = beamtalk_repl_state:new(MockSocket, 5678),
    ?assertEqual(MockSocket, beamtalk_repl_state:get_listen_socket(State)),
    ?assertEqual(5678, beamtalk_repl_state:get_port(State)).

%%% Bindings tests

get_set_bindings_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),

    Bindings = #{x => 1, y => 2},
    State2 = beamtalk_repl_state:set_bindings(Bindings, State),
    ?assertEqual(Bindings, beamtalk_repl_state:get_bindings(State2)).

clear_bindings_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1, y => 2}, State),
    ?assertNotEqual(#{}, beamtalk_repl_state:get_bindings(State2)),

    State3 = beamtalk_repl_state:clear_bindings(State2),
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State3)).

bindings_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1}, State),

    %% Original state should be unchanged
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State)),
    ?assertEqual(#{x => 1}, beamtalk_repl_state:get_bindings(State2)).

%%% Eval counter tests

eval_counter_starts_at_zero_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)).

increment_eval_counter_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:increment_eval_counter(State),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(State2)),

    State3 = beamtalk_repl_state:increment_eval_counter(State2),
    ?assertEqual(2, beamtalk_repl_state:get_eval_counter(State3)).

eval_counter_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    _State2 = beamtalk_repl_state:increment_eval_counter(State),

    %% Original state should be unchanged
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State)).

eval_counter_many_increments_test() ->
    State = lists:foldl(
        fun(_, S) -> beamtalk_repl_state:increment_eval_counter(S) end,
        beamtalk_repl_state:new(undefined, 0),
        lists:seq(1, 100)
    ),
    ?assertEqual(100, beamtalk_repl_state:get_eval_counter(State)).

%%% Loaded modules tests

loaded_modules_starts_empty_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

add_loaded_module_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(counter, State),
    ?assertEqual([counter], beamtalk_repl_state:get_loaded_modules(State2)).

add_multiple_loaded_modules_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(counter, State),
    State3 = beamtalk_repl_state:add_loaded_module(point, State2),
    State4 = beamtalk_repl_state:add_loaded_module(actor, State3),

    Modules = beamtalk_repl_state:get_loaded_modules(State4),
    ?assertEqual(3, length(Modules)),
    ?assert(lists:member(counter, Modules)),
    ?assert(lists:member(point, Modules)),
    ?assert(lists:member(actor, Modules)).

loaded_modules_immutability_test() ->
    State = beamtalk_repl_state:new(undefined, 0),
    _State2 = beamtalk_repl_state:add_loaded_module(counter, State),

    %% Original state should be unchanged
    ?assertEqual([], beamtalk_repl_state:get_loaded_modules(State)).

loaded_modules_order_test() ->
    %% Modules are added to the front of the list
    State = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:add_loaded_module(mod1, State),
    State3 = beamtalk_repl_state:add_loaded_module(mod2, State2),
    State4 = beamtalk_repl_state:add_loaded_module(mod3, State3),

    ?assertEqual([mod3, mod2, mod1], beamtalk_repl_state:get_loaded_modules(State4)).

%%% Configuration tests

listen_socket_test() ->
    MockSocket = self(),
    State = beamtalk_repl_state:new(MockSocket, 1234),
    ?assertEqual(MockSocket, beamtalk_repl_state:get_listen_socket(State)).

port_test() ->
    State = beamtalk_repl_state:new(undefined, 9876),
    ?assertEqual(9876, beamtalk_repl_state:get_port(State)).

%%% State composition tests

complex_state_test() ->
    %% Test a state with all fields set
    State = beamtalk_repl_state:new(self(), 1234),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1, y => 2}, State),
    State3 = beamtalk_repl_state:increment_eval_counter(State2),
    State4 = beamtalk_repl_state:increment_eval_counter(State3),
    State5 = beamtalk_repl_state:add_loaded_module(counter, State4),
    State6 = beamtalk_repl_state:add_loaded_module(point, State5),

    %% Verify all fields
    ?assertEqual(self(), beamtalk_repl_state:get_listen_socket(State6)),
    ?assertEqual(1234, beamtalk_repl_state:get_port(State6)),
    ?assertEqual(#{x => 1, y => 2}, beamtalk_repl_state:get_bindings(State6)),
    ?assertEqual(2, beamtalk_repl_state:get_eval_counter(State6)),
    ?assertEqual([point, counter], beamtalk_repl_state:get_loaded_modules(State6)).

state_independence_test() ->
    %% Test that operations on one state don't affect other states
    State1 = beamtalk_repl_state:new(undefined, 0),
    State2 = beamtalk_repl_state:set_bindings(#{x => 1}, State1),
    State3 = beamtalk_repl_state:increment_eval_counter(State1),

    %% All three states should be independent
    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State1)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State1)),

    ?assertEqual(#{x => 1}, beamtalk_repl_state:get_bindings(State2)),
    ?assertEqual(0, beamtalk_repl_state:get_eval_counter(State2)),

    ?assertEqual(#{}, beamtalk_repl_state:get_bindings(State3)),
    ?assertEqual(1, beamtalk_repl_state:get_eval_counter(State3)).
