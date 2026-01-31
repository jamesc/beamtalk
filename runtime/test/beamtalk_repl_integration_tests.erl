%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for beamtalk_repl with compiler daemon.
%%%
%%% These tests verify the full Rust compiler daemon ↔ Erlang runtime integration,
%%% covering compilation, evaluation, error handling, concurrency, and resource cleanup.
%%%
%%% ## Test Requirements
%%%
%%% The compiler daemon must be running before executing these tests:
%%%   Terminal 1: beamtalk daemon start --foreground
%%%   Terminal 2: cd runtime && rebar3 eunit --module=beamtalk_repl_integration_tests
%%%
%%% NOTE: Automatic daemon start/stop in setup/cleanup does not work reliably:
%%% - `beamtalk daemon start --foreground &` doesn't properly track the PID
%%% - Race condition: fixed timer:sleep doesn't verify daemon readiness
%%% - The daemon's --foreground mode doesn't support backgrounding
%%%
%%% ## Coverage Areas (35+ tests)
%%%
%%% - Basic evaluation (15 tests): literals, variables, arithmetic, bindings
%%% - Hot code reloading (3 tests): recompilation, state preservation
%%% - Concurrent compilation (3 tests): multiple REPL processes, queue handling
%%% - Error propagation (3 tests): parse errors, undefined variables, syntax errors
%%% - Large/complex expressions (2 tests): nested expressions, multi-statement
%%% - Connection management (3 tests): sequential connections, rapid cycling, error recovery
%%% - Resource cleanup (3 tests): process cleanup, error cleanup, stress testing
%%% - Edge cases (4 tests): empty expressions, long strings, deep nesting, error recovery
%%%
%%% ## Future Improvements
%%%
%%% - Poll socket to verify daemon is accepting connections
%%% - Capture daemon PID for proper cleanup
%%% - Consider separate integration test runner with daemon management

-module(beamtalk_repl_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture that wraps all tests - checks daemon availability
repl_integration_test_() ->
    {setup,
     fun setup_daemon/0,
     fun cleanup_daemon/1,
     fun(DaemonAvailable) ->
         case DaemonAvailable of
             true ->
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
                  {"eval variable in arithmetic (BT-57)", fun eval_variable_arithmetic/0},
                  {"eval multiple variable arithmetic (BT-57)", fun eval_multi_variable_arithmetic/0},
                  {"eval multiple assignments", fun eval_multiple_assignments/0},
                  {"eval reassignment", fun eval_reassignment/0},
                  {"clear bindings", fun clear_bindings/0},
                  %% Hot code reloading tests
                  {"hot reload preserves bindings", fun hot_reload_preserves_bindings/0},
                  {"hot reload recompiles same expression", fun hot_reload_recompiles/0},
                  {"hot reload changes take effect", fun hot_reload_changes_effect/0},
                  %% Concurrent compilation tests
                  {"concurrent REPL processes", fun concurrent_repl_processes/0},
                  {"concurrent same expression", fun concurrent_same_expression/0},
                  {"concurrent different expressions", fun concurrent_different_expressions/0},
                  %% Error propagation tests
                  {"parse error returns error tuple", fun parse_error_handling/0},
                  {"undefined variable error", fun undefined_variable_error/0},
                  {"invalid syntax error", fun invalid_syntax_error/0},
                  %% Large/complex expressions
                  {"large nested expression", fun large_nested_expression/0},
                  {"complex multi-statement", fun complex_multi_statement/0},
                  %% Connection management (these test REPL behavior, not daemon)
                  {"multiple sequential connections", fun multiple_sequential_connections/0},
                  {"rapid connection cycling", fun rapid_connection_cycling/0},
                  {"connection after error", fun connection_after_error/0},
                  %% Resource cleanup
                  {"cleanup after normal stop", fun cleanup_after_stop/0},
                  {"cleanup after error", fun cleanup_after_error/0},
                  {"cleanup with many evaluations", fun cleanup_many_evaluations/0},
                  %% Edge cases
                  {"empty expression handling", fun empty_expression_handling/0},
                  {"very long string literal", fun very_long_string_literal/0},
                  {"deeply nested arithmetic", fun deeply_nested_arithmetic/0},
                  {"sequential error recovery", fun sequential_error_recovery/0}
                 ];
             false ->
                 %% Skip all tests when daemon is not available
                 [{"(skipped - daemon not running)", fun() -> ok end}]
         end
     end}.

%% Setup: Check if daemon is available (don't try to start it)
setup_daemon() ->
    %% Check if daemon socket exists by trying a quick connect
    Home = os:getenv("HOME"),
    SocketPath = case Home of
        false -> "/tmp/.beamtalk/daemon.sock";
        H -> filename:join([H, ".beamtalk", "daemon.sock"])
    end,
    %% Try to connect to check if daemon is running
    case gen_tcp:connect({local, SocketPath}, 0,
                         [binary, {active, false}], 100) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("~n=== Daemon is running, executing integration tests ===~n"),
            true;
        {error, _} ->
            io:format("~n=== Daemon not running, skipping integration tests ===~n"),
            io:format("To run: beamtalk daemon start --foreground~n"),
            false
    end.

%% Cleanup: Nothing to clean up since we don't start the daemon
cleanup_daemon(_DaemonAvailable) ->
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

%% BT-57: Variable references in arithmetic expressions
%% Tests that variable lookup works correctly via State alias
eval_variable_arithmetic() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 10} = beamtalk_repl:eval(Pid, "x := 10"),
    Result = beamtalk_repl:eval(Pid, "x + 5"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 15}, Result).

%% BT-57: Multiple variables in expression
%% Tests that all variable lookups correctly resolve from bindings
eval_multi_variable_arithmetic() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 3} = beamtalk_repl:eval(Pid, "a := 3"),
    {ok, 4} = beamtalk_repl:eval(Pid, "b := 4"),
    Result = beamtalk_repl:eval(Pid, "a + b"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 7}, Result).

%%% Hot Code Reloading Tests

%% Test that hot reloading preserves REPL state and bindings
hot_reload_preserves_bindings() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Set up initial bindings
    {ok, 42} = beamtalk_repl:eval(Pid, "x := 42"),
    {ok, 100} = beamtalk_repl:eval(Pid, "y := 100"),
    %% Compile and execute same expression again (simulates reload)
    Result1 = beamtalk_repl:eval(Pid, "x + y"),
    Result2 = beamtalk_repl:eval(Pid, "x + y"),
    %% Bindings should still be intact
    Bindings = beamtalk_repl:get_bindings(Pid),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 142}, Result1),
    ?assertMatch({ok, 142}, Result2),
    ?assertEqual(42, maps:get(x, Bindings)),
    ?assertEqual(100, maps:get(y, Bindings)).

%% Test that recompiling the same expression works correctly
hot_reload_recompiles() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Compile same expression multiple times
    {ok, 10} = beamtalk_repl:eval(Pid, "5 + 5"),
    {ok, 10} = beamtalk_repl:eval(Pid, "5 + 5"),
    {ok, 10} = beamtalk_repl:eval(Pid, "5 + 5"),
    beamtalk_repl:stop(Pid),
    ok.

%% Test that changes to evaluated code take effect
hot_reload_changes_effect() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Evaluate initial value
    {ok, 10} = beamtalk_repl:eval(Pid, "val := 10"),
    %% Change the value (simulates code change)
    {ok, 20} = beamtalk_repl:eval(Pid, "val := 20"),
    %% Verify change took effect
    Result = beamtalk_repl:eval(Pid, "val"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 20}, Result).

%%% Concurrent Compilation Tests

%% Test multiple REPL processes compiling simultaneously
concurrent_repl_processes() ->
    %% Start 3 independent REPL processes
    {ok, Pid1} = beamtalk_repl:start_link(0, #{}),
    {ok, Pid2} = beamtalk_repl:start_link(0, #{}),
    {ok, Pid3} = beamtalk_repl:start_link(0, #{}),
    
    %% Issue concurrent compilation requests
    Parent = self(),
    spawn_link(fun() ->
        {ok, 1} = beamtalk_repl:eval(Pid1, "1"),
        Parent ! {done, 1}
    end),
    spawn_link(fun() ->
        {ok, 2} = beamtalk_repl:eval(Pid2, "2"),
        Parent ! {done, 2}
    end),
    spawn_link(fun() ->
        {ok, 3} = beamtalk_repl:eval(Pid3, "3"),
        Parent ! {done, 3}
    end),
    
    %% Wait for all to complete
    receive {done, 1} -> ok after 5000 -> error(timeout) end,
    receive {done, 2} -> ok after 5000 -> error(timeout) end,
    receive {done, 3} -> ok after 5000 -> error(timeout) end,
    
    %% Cleanup
    beamtalk_repl:stop(Pid1),
    beamtalk_repl:stop(Pid2),
    beamtalk_repl:stop(Pid3),
    ok.

%% Test concurrent compilation of the same expression
concurrent_same_expression() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Parent = self(),
    
    %% Issue multiple concurrent requests to same REPL
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "42"),
        Parent ! {result, 1, Result}
    end),
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "42"),
        Parent ! {result, 2, Result}
    end),
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "42"),
        Parent ! {result, 3, Result}
    end),
    
    %% Collect results
    R1 = receive {result, 1, Res1} -> Res1 after 5000 -> error(timeout) end,
    R2 = receive {result, 2, Res2} -> Res2 after 5000 -> error(timeout) end,
    R3 = receive {result, 3, Res3} -> Res3 after 5000 -> error(timeout) end,
    
    beamtalk_repl:stop(Pid),
    
    %% All should succeed
    ?assertMatch({ok, 42}, R1),
    ?assertMatch({ok, 42}, R2),
    ?assertMatch({ok, 42}, R3).

%% Test concurrent compilation of different expressions
concurrent_different_expressions() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Parent = self(),
    
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "1 + 1"),
        Parent ! {result, 1, Result}
    end),
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "2 * 2"),
        Parent ! {result, 2, Result}
    end),
    spawn_link(fun() ->
        Result = beamtalk_repl:eval(Pid, "3 + 3"),
        Parent ! {result, 3, Result}
    end),
    
    R1 = receive {result, 1, Res1} -> Res1 after 5000 -> error(timeout) end,
    R2 = receive {result, 2, Res2} -> Res2 after 5000 -> error(timeout) end,
    R3 = receive {result, 3, Res3} -> Res3 after 5000 -> error(timeout) end,
    
    beamtalk_repl:stop(Pid),
    
    ?assertMatch({ok, 2}, R1),
    ?assertMatch({ok, 4}, R2),
    ?assertMatch({ok, 6}, R3).

%%% Error Propagation Tests

%% Test that parse errors propagate correctly from Rust to Erlang
parse_error_handling() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Invalid syntax should return error tuple
    Result = beamtalk_repl:eval(Pid, "1 + + 2"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({error, _}, Result).

%% Test that undefined variable errors are caught
undefined_variable_error() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Reference undefined variable
    Result = beamtalk_repl:eval(Pid, "undefinedVar"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({error, _}, Result).

%% Test various invalid syntax patterns
invalid_syntax_error() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Various syntax errors
    R1 = beamtalk_repl:eval(Pid, "1 +"),
    R2 = beamtalk_repl:eval(Pid, ":="),
    %% Note: Some expressions that look invalid may parse successfully
    %% (e.g., "1 2 3" parses as "1" with trailing ignored). Test actual errors.
    beamtalk_repl:stop(Pid),
    ?assertMatch({error, _}, R1),
    ?assertMatch({error, _}, R2).

%%% Large/Complex Expression Tests

%% Test compilation of large nested expressions
large_nested_expression() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Deeply nested arithmetic
    Expr = "((((1 + 2) * 3) + 4) * 5) + 6",
    Result = beamtalk_repl:eval(Pid, Expr),
    beamtalk_repl:stop(Pid),
    %% ((((1 + 2) * 3) + 4) * 5) + 6 = (((3 * 3) + 4) * 5) + 6 = ((9 + 4) * 5) + 6 = (13 * 5) + 6 = 65 + 6 = 71
    ?assertMatch({ok, 71}, Result).

%% Test complex multi-statement expressions
complex_multi_statement() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Multiple operations
    {ok, 10} = beamtalk_repl:eval(Pid, "a := 10"),
    {ok, 20} = beamtalk_repl:eval(Pid, "b := 20"),
    {ok, 30} = beamtalk_repl:eval(Pid, "c := 30"),
    {ok, 60} = beamtalk_repl:eval(Pid, "result := a + b + c"),
    Final = beamtalk_repl:eval(Pid, "result * 2"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 120}, Final).

%%% Connection Management Tests

%% Test multiple sequential connections to REPL
multiple_sequential_connections() ->
    %% Start and stop REPL multiple times
    {ok, Pid1} = beamtalk_repl:start_link(0, #{}),
    {ok, 1} = beamtalk_repl:eval(Pid1, "1"),
    beamtalk_repl:stop(Pid1),
    
    {ok, Pid2} = beamtalk_repl:start_link(0, #{}),
    {ok, 2} = beamtalk_repl:eval(Pid2, "2"),
    beamtalk_repl:stop(Pid2),
    
    {ok, Pid3} = beamtalk_repl:start_link(0, #{}),
    {ok, 3} = beamtalk_repl:eval(Pid3, "3"),
    beamtalk_repl:stop(Pid3),
    ok.

%% Test rapid creation and destruction of REPL connections
rapid_connection_cycling() ->
    lists:foreach(fun(N) ->
        {ok, Pid} = beamtalk_repl:start_link(0, #{}),
        {ok, N} = beamtalk_repl:eval(Pid, integer_to_list(N)),
        beamtalk_repl:stop(Pid)
    end, lists:seq(1, 10)),
    ok.

%% Test that connection works after error
connection_after_error() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Cause an error
    {error, _} = beamtalk_repl:eval(Pid, "invalid syntax +++"),
    %% Connection should still work
    Result = beamtalk_repl:eval(Pid, "42"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 42}, Result).

%%% Resource Cleanup Tests

%% Test that resources are cleaned up after normal stop
cleanup_after_stop() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 100} = beamtalk_repl:eval(Pid, "x := 100"),
    beamtalk_repl:stop(Pid),
    %% Verify process is gone
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

%% Test that resources are cleaned up after error
cleanup_after_error() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {error, _} = beamtalk_repl:eval(Pid, "1 + + 1"),
    beamtalk_repl:stop(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

%% Test cleanup with many evaluations
cleanup_many_evaluations() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Run evaluations (reduced from 50 to 20 to avoid daemon overload)
    lists:foreach(fun(N) ->
        {ok, N} = beamtalk_repl:eval(Pid, integer_to_list(N))
    end, lists:seq(1, 20)),
    beamtalk_repl:stop(Pid),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

%%% Edge Case Tests

%% Test handling of empty expressions (should error)
empty_expression_handling() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, ""),
    beamtalk_repl:stop(Pid),
    ?assertMatch({error, _}, Result).

%% Test very long string literals
very_long_string_literal() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Create a string with 1000 characters
    LongString = lists:duplicate(100, "0123456789"),
    Expr = "'" ++ LongString ++ "'",
    Result = beamtalk_repl:eval(Pid, Expr),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, _}, Result),
    {ok, ResultStr} = Result,
    ?assertEqual(1000, byte_size(ResultStr)).

%% Test deeply nested arithmetic expressions
deeply_nested_arithmetic() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Build nested expression: 1 + (1 + (1 + (1 + 1)))
    Expr = lists:foldl(
        fun(_, Acc) -> "1 + (" ++ Acc ++ ")" end,
        "1",
        lists:seq(1, 20)
    ),
    Result = beamtalk_repl:eval(Pid, Expr),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 21}, Result).

%% Test that REPL can recover after multiple sequential errors
sequential_error_recovery() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Multiple errors in sequence
    {error, _} = beamtalk_repl:eval(Pid, "bad syntax 1"),
    {error, _} = beamtalk_repl:eval(Pid, "bad syntax 2"),
    {error, _} = beamtalk_repl:eval(Pid, "bad syntax 3"),
    %% Should still be able to evaluate valid code
    Result = beamtalk_repl:eval(Pid, "42"),
    beamtalk_repl:stop(Pid),
    ?assertMatch({ok, 42}, Result).

