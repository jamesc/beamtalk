%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for beamtalk_repl with compiler daemon.
%%%
%%% These tests require the compiler daemon to be running.
%%% Run with: make integration-test (which starts/stops daemon)
%%%
%%% Or manually:
%%% 1. Start daemon: beamtalk daemon start --foreground
%%% 2. Run tests: erl -noshell -pa ebin -eval 'eunit:test(beamtalk_repl_integration_tests, [verbose]), halt().'

-module(beamtalk_repl_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture that wraps all tests with daemon start/stop
repl_integration_test_() ->
    {setup,
     fun setup_daemon/0,
     fun cleanup_daemon/1,
     fun(_) ->
         [
          {"eval integer literal", fun eval_integer_literal/0},
          {"eval negative integer", fun eval_negative_integer/0},
          {"eval float literal", fun eval_float_literal/0},
          {"eval string literal", fun eval_string_literal/0},
          {"eval symbol literal", fun eval_symbol_literal/0},
          {"eval true literal", fun eval_true_literal/0},
          {"eval false literal", fun eval_false_literal/0},
          {"eval nil literal", fun eval_nil_literal/0},
          {"eval assignment", fun eval_assignment/0},
          {"eval variable reference", fun eval_variable_reference/0},
          {"eval multiple assignments", fun eval_multiple_assignments/0},
          {"eval reassignment", fun eval_reassignment/0},
          {"clear bindings", fun clear_bindings/0}
         ]
     end}.

%% Setup: Start the compiler daemon
setup_daemon() ->
    %% Try to start daemon via os:cmd
    %% Assumes beamtalk binary is in PATH
    DaemonCmd = "beamtalk daemon start --foreground &",
    os:cmd(DaemonCmd),
    %% Give daemon time to start up
    timer:sleep(1000),
    ok.

%% Cleanup: Stop the daemon
cleanup_daemon(_) ->
    os:cmd("beamtalk daemon stop"),
    timer:sleep(500),
    ok.

%%% Integration Tests

eval_integer_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "42"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 42}, Result).

eval_negative_integer() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "-123"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, -123}, Result).

eval_float_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, Result} = beamtalk_repl:eval(Pid, "3.14"),
    beamtalk_repl:stop(Pid),
    ?assert(abs(Result - 3.14) < 0.001).

eval_string_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "'hello'"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, <<"hello">>}, Result).

eval_symbol_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "#ok"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, ok}, Result).

eval_true_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "true"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, true}, Result).

eval_false_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "false"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, false}, Result).

eval_nil_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "nil"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, nil}, Result).

eval_assignment() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "x := 42"),
    Bindings = beamtalk_repl:get_bindings(Pid),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 42}, Result),
    ?assertEqual(42, maps:get(x, Bindings)).

eval_variable_reference() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 100} = beamtalk_repl:eval(Pid, "myVar := 100"),
    Result = beamtalk_repl:eval(Pid, "myVar"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 100}, Result).

eval_multiple_assignments() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 1} = beamtalk_repl:eval(Pid, "a := 1"),
    {ok, 2} = beamtalk_repl:eval(Pid, "b := 2"),
    {ok, 3} = beamtalk_repl:eval(Pid, "c := 3"),
    Bindings = beamtalk_repl:get_bindings(Pid),
    beamtalk_repl:stop(Pid),
    ?assertEqual(3, maps:size(Bindings)),
    ?assertEqual(1, maps:get(a, Bindings)),
    ?assertEqual(2, maps:get(b, Bindings)),
    ?assertEqual(3, maps:get(c, Bindings)).

eval_reassignment() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 10} = beamtalk_repl:eval(Pid, "x := 10"),
    {ok, 20} = beamtalk_repl:eval(Pid, "x := 20"),
    Result = beamtalk_repl:eval(Pid, "x"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 20}, Result).

clear_bindings() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 42} = beamtalk_repl:eval(Pid, "x := 42"),
    ?assertEqual(1, maps:size(beamtalk_repl:get_bindings(Pid))),
    ok = beamtalk_repl:clear_bindings(Pid),
    ?assertEqual(0, maps:size(beamtalk_repl:get_bindings(Pid))),
    beamtalk_repl:stop(Pid).
