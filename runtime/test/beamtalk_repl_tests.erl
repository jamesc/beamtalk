%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_repl module
%%%
%%% Tests the REPL backend functionality:
%%% - Expression parsing and evaluation
%%% - Variable binding management
%%% - JSON protocol parsing and formatting
%%% - Error handling
%%% - TCP server behavior

-module(beamtalk_repl_tests).
-include_lib("eunit/include/eunit.hrl").

%%% JSON Protocol Tests

parse_eval_request_test() ->
    Request = <<"{\"type\":\"eval\",\"expression\":\"42\"}">>,
    ?assertEqual({eval, "42"}, beamtalk_repl:parse_request(Request)).

parse_eval_request_with_spaces_test() ->
    Request = <<"{ \"type\" : \"eval\" , \"expression\" : \"1 + 2\" }">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl:parse_request(Request)).

parse_clear_request_test() ->
    Request = <<"{\"type\":\"clear\"}">>,
    ?assertEqual({clear_bindings}, beamtalk_repl:parse_request(Request)).

parse_bindings_request_test() ->
    Request = <<"{\"type\":\"bindings\"}">>,
    ?assertEqual({get_bindings}, beamtalk_repl:parse_request(Request)).

parse_raw_expression_test() ->
    %% Non-JSON input is treated as a raw expression
    Request = <<"counter getValue">>,
    ?assertEqual({eval, "counter getValue"}, beamtalk_repl:parse_request(Request)).

parse_empty_expression_test() ->
    Request = <<"">>,
    ?assertEqual({error, empty_expression}, beamtalk_repl:parse_request(Request)).

parse_whitespace_only_test() ->
    Request = <<"   ">>,
    ?assertEqual({error, empty_expression}, beamtalk_repl:parse_request(Request)).

%%% Response Formatting Tests

format_integer_response_test() ->
    Response = beamtalk_repl:format_response(42),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":42}">>, Response).

format_float_response_test() ->
    Response = beamtalk_repl:format_response(3.14),
    %% Float formatting may vary slightly
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, Response).

format_atom_response_test() ->
    Response = beamtalk_repl:format_response(ok),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":\"ok\"}">>, Response).

format_string_response_test() ->
    Response = beamtalk_repl:format_response(<<"hello">>),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":\"hello\"}">>, Response).

format_string_with_quotes_test() ->
    Response = beamtalk_repl:format_response(<<"say \"hi\"">>),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":\"say \\\"hi\\\"\"}">>, Response).

format_nil_response_test() ->
    Response = beamtalk_repl:format_response(nil),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":\"nil\"}">>, Response).

format_list_response_test() ->
    Response = beamtalk_repl:format_response([1, 2, 3]),
    ?assertEqual(<<"{\"type\":\"result\",\"value\":[1,2,3]}">>, Response).

format_pid_response_test() ->
    Response = beamtalk_repl:format_response(self()),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":\"#<pid ", _/binary>>, Response).

%%% Error Formatting Tests

format_undefined_variable_error_test() ->
    Response = beamtalk_repl:format_error({undefined_variable, "foo"}),
    ?assertEqual(<<"{\"type\":\"error\",\"message\":\"Undefined variable: foo\"}">>, Response).

format_empty_expression_error_test() ->
    Response = beamtalk_repl:format_error(empty_expression),
    ?assertEqual(<<"{\"type\":\"error\",\"message\":\"Empty expression\"}">>, Response).

format_timeout_error_test() ->
    Response = beamtalk_repl:format_error(timeout),
    ?assertEqual(<<"{\"type\":\"error\",\"message\":\"Request timed out\"}">>, Response).

%%% Direct Evaluation Tests (using eval/2)

eval_integer_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, 42}, beamtalk_repl:eval(Pid, "42")),
    beamtalk_repl:stop(Pid).

eval_negative_integer_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, -123}, beamtalk_repl:eval(Pid, "-123")),
    beamtalk_repl:stop(Pid).

eval_float_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, Result} = beamtalk_repl:eval(Pid, "3.14"),
    ?assert(abs(Result - 3.14) < 0.001),
    beamtalk_repl:stop(Pid).

eval_string_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, <<"hello">>}, beamtalk_repl:eval(Pid, "'hello'")),
    beamtalk_repl:stop(Pid).

eval_double_quoted_string_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, <<"world">>}, beamtalk_repl:eval(Pid, "\"world\"")),
    beamtalk_repl:stop(Pid).

eval_symbol_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, ok}, beamtalk_repl:eval(Pid, "#ok")),
    beamtalk_repl:stop(Pid).

eval_true_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, true}, beamtalk_repl:eval(Pid, "true")),
    beamtalk_repl:stop(Pid).

eval_false_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, false}, beamtalk_repl:eval(Pid, "false")),
    beamtalk_repl:stop(Pid).

eval_nil_literal_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, nil}, beamtalk_repl:eval(Pid, "nil")),
    beamtalk_repl:stop(Pid).

%%% Variable Binding Tests

eval_assignment_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertMatch({ok, 42}, beamtalk_repl:eval(Pid, "x := 42")),
    %% Verify binding was stored
    Bindings = beamtalk_repl:get_bindings(Pid),
    ?assertEqual(42, maps:get(x, Bindings)),
    beamtalk_repl:stop(Pid).

eval_variable_reference_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% First assign
    {ok, 100} = beamtalk_repl:eval(Pid, "myVar := 100"),
    %% Then reference
    ?assertMatch({ok, 100}, beamtalk_repl:eval(Pid, "myVar")),
    beamtalk_repl:stop(Pid).

eval_multiple_assignments_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 1} = beamtalk_repl:eval(Pid, "a := 1"),
    {ok, 2} = beamtalk_repl:eval(Pid, "b := 2"),
    {ok, 3} = beamtalk_repl:eval(Pid, "c := 3"),
    Bindings = beamtalk_repl:get_bindings(Pid),
    ?assertEqual(3, maps:size(Bindings)),
    ?assertEqual(1, maps:get(a, Bindings)),
    ?assertEqual(2, maps:get(b, Bindings)),
    ?assertEqual(3, maps:get(c, Bindings)),
    beamtalk_repl:stop(Pid).

eval_reassignment_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 10} = beamtalk_repl:eval(Pid, "x := 10"),
    {ok, 20} = beamtalk_repl:eval(Pid, "x := 20"),
    ?assertMatch({ok, 20}, beamtalk_repl:eval(Pid, "x")),
    beamtalk_repl:stop(Pid).

eval_undefined_variable_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "undefinedVar"),
    ?assertMatch({error, {compile_error, _}}, Result),
    beamtalk_repl:stop(Pid).

clear_bindings_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 42} = beamtalk_repl:eval(Pid, "x := 42"),
    ?assertEqual(1, maps:size(beamtalk_repl:get_bindings(Pid))),
    ok = beamtalk_repl:clear_bindings(Pid),
    ?assertEqual(0, maps:size(beamtalk_repl:get_bindings(Pid))),
    beamtalk_repl:stop(Pid).

%%% Error Handling Tests

eval_empty_expression_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, ""),
    ?assertMatch({error, {compile_error, _}}, Result),
    beamtalk_repl:stop(Pid).

eval_whitespace_expression_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "   "),
    ?assertMatch({error, {compile_error, _}}, Result),
    beamtalk_repl:stop(Pid).

eval_syntax_error_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = beamtalk_repl:eval(Pid, "this is not valid syntax @#$%"),
    ?assertMatch({error, _}, Result),
    beamtalk_repl:stop(Pid).

%%% Actor Integration Tests (requires test_counter module)

actor_spawn_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Spawn a counter actor using the test_counter module
    {ok, ActorPid} = beamtalk_repl:eval(Pid, "counter := Test_counter spawn"),
    ?assert(is_pid(ActorPid)),
    ?assert(is_process_alive(ActorPid)),
    %% Clean up
    gen_server:stop(ActorPid),
    beamtalk_repl:stop(Pid).

%% Test removed - requires full test_counter module to be loadable
%% actor_message_send_test() -> ...

%%% Server Lifecycle Tests

start_stop_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assert(is_process_alive(Pid)),
    beamtalk_repl:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

multiple_servers_test() ->
    %% Can start multiple REPL servers on different ports
    {ok, Pid1} = beamtalk_repl:start_link(0, #{}),
    {ok, Pid2} = beamtalk_repl:start_link(0, #{}),
    ?assert(is_process_alive(Pid1)),
    ?assert(is_process_alive(Pid2)),
    ?assertNot(Pid1 =:= Pid2),
    beamtalk_repl:stop(Pid1),
    beamtalk_repl:stop(Pid2).

%%% Binding Isolation Tests

bindings_isolated_between_servers_test() ->
    {ok, Pid1} = beamtalk_repl:start_link(0, #{}),
    {ok, Pid2} = beamtalk_repl:start_link(0, #{}),
    %% Set binding in first server
    {ok, 1} = beamtalk_repl:eval(Pid1, "x := 1"),
    %% Second server should not have this binding
    ?assertEqual(0, maps:size(beamtalk_repl:get_bindings(Pid2))),
    %% Set different binding in second server
    {ok, 2} = beamtalk_repl:eval(Pid2, "y := 2"),
    %% First server should still have only x
    ?assertEqual(1, maps:size(beamtalk_repl:get_bindings(Pid1))),
    ?assertEqual(1, maps:get(x, beamtalk_repl:get_bindings(Pid1))),
    beamtalk_repl:stop(Pid1),
    beamtalk_repl:stop(Pid2).

%%% Edge Case Tests

eval_underscore_identifier_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 5} = beamtalk_repl:eval(Pid, "_private := 5"),
    ?assertMatch({ok, 5}, beamtalk_repl:eval(Pid, "_private")),
    beamtalk_repl:stop(Pid).

eval_camelcase_identifier_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 10} = beamtalk_repl:eval(Pid, "myVariable := 10"),
    ?assertMatch({ok, 10}, beamtalk_repl:eval(Pid, "myVariable")),
    beamtalk_repl:stop(Pid).

eval_identifier_with_numbers_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    {ok, 99} = beamtalk_repl:eval(Pid, "var123 := 99"),
    ?assertMatch({ok, 99}, beamtalk_repl:eval(Pid, "var123")),
    beamtalk_repl:stop(Pid).

%%% End-to-End TCP Tests

tcp_eval_integer_test() ->
    %% Start REPL server on random port
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    %% Connect via TCP
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% Send JSON eval request
    Request = <<"{\"type\":\"eval\",\"expression\":\"42\"}\n">>,
    ok = gen_tcp:send(Socket, Request),
    
    %% Receive response
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    
    %% Verify response contains result
    ?assert(binary:match(Response, <<"\"type\":\"result\"">>) =/= nomatch),
    ?assert(binary:match(Response, <<"42">>) =/= nomatch),
    
    gen_tcp:close(Socket),
    beamtalk_repl:stop(Pid).

tcp_eval_string_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% Send string expression
    Request = <<"{\"type\":\"eval\",\"expression\":\"'hello world'\"}\n">>,
    ok = gen_tcp:send(Socket, Request),
    
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    ?assert(binary:match(Response, <<"\"type\":\"result\"">>) =/= nomatch),
    ?assert(binary:match(Response, <<"hello world">>) =/= nomatch),
    
    gen_tcp:close(Socket),
    beamtalk_repl:stop(Pid).

tcp_eval_assignment_and_reference_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% First: assign variable
    ok = gen_tcp:send(Socket, <<"{\"type\":\"eval\",\"expression\":\"x := 100\"}\n">>),
    {ok, Response1} = gen_tcp:recv(Socket, 0, 5000),
    ?assert(binary:match(Response1, <<"100">>) =/= nomatch),
    
    %% Second: reference variable
    ok = gen_tcp:send(Socket, <<"{\"type\":\"eval\",\"expression\":\"x\"}\n">>),
    {ok, Response2} = gen_tcp:recv(Socket, 0, 5000),
    ?assert(binary:match(Response2, <<"100">>) =/= nomatch),
    
    gen_tcp:close(Socket),
    beamtalk_repl:stop(Pid).

tcp_eval_error_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% Request undefined variable
    ok = gen_tcp:send(Socket, <<"{\"type\":\"eval\",\"expression\":\"undefinedVar\"}\n">>),
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    
    %% Should get error response
    ?assert(binary:match(Response, <<"\"type\":\"error\"">>) =/= nomatch),
    
    gen_tcp:close(Socket),
    beamtalk_repl:stop(Pid).

tcp_raw_expression_test() ->
    %% Test sending raw expression (not JSON) - backwards compatibility
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% Send raw expression (not JSON)
    ok = gen_tcp:send(Socket, <<"123\n">>),
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    
    ?assert(binary:match(Response, <<"123">>) =/= nomatch),
    
    gen_tcp:close(Socket),
    beamtalk_repl:stop(Pid).

tcp_multiple_clients_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    
    %% Connect two clients
    {ok, Socket1} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    {ok, Socket2} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, line}]),
    
    %% Both can evaluate
    ok = gen_tcp:send(Socket1, <<"1\n">>),
    ok = gen_tcp:send(Socket2, <<"2\n">>),
    
    {ok, Response1} = gen_tcp:recv(Socket1, 0, 5000),
    {ok, Response2} = gen_tcp:recv(Socket2, 0, 5000),
    
    ?assert(binary:match(Response1, <<"1">>) =/= nomatch),
    ?assert(binary:match(Response2, <<"2">>) =/= nomatch),
    
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    beamtalk_repl:stop(Pid).
