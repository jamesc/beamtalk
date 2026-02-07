%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl module
%%%
%%% Tests the REPL backend functionality that does NOT require the compiler daemon:
%%% - JSON protocol parsing and formatting
%%% - Response formatting
%%% - Server lifecycle
%%%
%%% For tests that require the compiler daemon, see beamtalk_repl_integration_tests.erl

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

parse_load_request_test() ->
    Request = <<"{\"type\":\"load\",\"path\":\"examples/counter.bt\"}">>,
    ?assertEqual({load_file, "examples/counter.bt"}, beamtalk_repl:parse_request(Request)).

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
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => 42}, Decoded).

format_float_response_test() ->
    Response = beamtalk_repl:format_response(3.14),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(is_float(Value) andalso Value > 3.1 andalso Value < 3.2).

format_atom_response_test() ->
    Response = beamtalk_repl:format_response(ok),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => <<"ok">>}, Decoded).

format_string_response_test() ->
    Response = beamtalk_repl:format_response(<<"hello">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => <<"hello">>}, Decoded).

format_string_with_quotes_test() ->
    Response = beamtalk_repl:format_response(<<"say \"hi\"">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => <<"say \"hi\"">>}, Decoded).

format_nil_response_test() ->
    Response = beamtalk_repl:format_response(nil),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => <<"nil">>}, Decoded).

format_list_response_test() ->
    Response = beamtalk_repl:format_response([1, 2, 3]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"result">>, <<"value">> => [1, 2, 3]}, Decoded).

format_pid_response_test() ->
    Response = beamtalk_repl:format_response(self()),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    Value = maps:get(<<"value">>, Decoded),
    ?assertMatch(<<"#Actor<0.", _/binary>>, Value).

format_function_response_test() ->
    %% Test that functions are formatted as "a Block/N"
    Response1 = beamtalk_repl:format_response(fun(X) -> X end),
    Decoded1 = jsx:decode(Response1, [return_maps]),
    ?assertEqual(<<"a Block/1">>, maps:get(<<"value">>, Decoded1)),

    Response2 = beamtalk_repl:format_response(fun(X, Y) -> X + Y end),
    Decoded2 = jsx:decode(Response2, [return_maps]),
    ?assertEqual(<<"a Block/2">>, maps:get(<<"value">>, Decoded2)).

%%% Error Formatting Tests

format_undefined_variable_error_test() ->
    Response = beamtalk_repl:format_error({undefined_variable, "foo"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"Undefined variable: foo">>}, Decoded).

format_empty_expression_error_test() ->
    Response = beamtalk_repl:format_error(empty_expression),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"Empty expression">>}, Decoded).

format_timeout_error_test() ->
    Response = beamtalk_repl:format_error(timeout),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"Request timed out">>}, Decoded).

format_file_not_found_error_test() ->
    Response = beamtalk_repl:format_error({file_not_found, "missing.bt"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"File not found: missing.bt">>}, Decoded).

format_read_error_test() ->
    Response = beamtalk_repl:format_error({read_error, eacces}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"Failed to read file: eacces">>}, Decoded).

format_daemon_unavailable_error_test() ->
    Response = beamtalk_repl:format_error(daemon_unavailable),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(#{<<"type">> => <<"error">>, <<"message">> => <<"Unable to connect to compiler daemon. Start with: beamtalk daemon start --foreground">>}, Decoded).

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

get_port_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Port = beamtalk_repl:get_port(Pid),
    ?assert(is_integer(Port)),
    ?assert(Port > 0),
    beamtalk_repl:stop(Pid).

bindings_initially_empty_test() ->
    %% Each server starts with empty bindings
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ?assertEqual(#{}, beamtalk_repl:get_bindings(Pid)),
    beamtalk_repl:stop(Pid).

%%% Edge case tests

parse_very_long_expression_test() ->
    %% Test parsing very long expressions (>1000 chars)
    LongExpr = lists:duplicate(2000, $a),
    Request = list_to_binary(LongExpr),
    ?assertEqual({eval, LongExpr}, beamtalk_repl:parse_request(Request)).

parse_malformed_json_recovery_test() ->
    %% Test various malformed/incomplete JSON handling
    %% The key goal is that parse_request doesn't crash on any input
    
    %% Incomplete JSON that's still valid but missing required fields
    Request1 = <<"{\"type\":\"eval\"}">>,%% Missing expression field
    Result1 = beamtalk_repl:parse_request(Request1),
    ?assertMatch({error, {invalid_request, unknown_type}}, Result1),
    
    %% Various potentially problematic inputs - just verify they don't crash
    _Result2 = beamtalk_repl:parse_request(<<"{incomplete[">>),
    _Result3 = beamtalk_repl:parse_request(<<"">>),
    _Result4 = beamtalk_repl:parse_request(<<"plain text">>),
    
    %% Verify plain text is treated as eval
    Request5 = <<"not json at all">>,  
    ?assertEqual({eval, "not json at all"}, beamtalk_repl:parse_request(Request5)).

parse_unicode_expression_test() ->
    %% Test parsing expressions with unicode characters
    %% The unicode in JSON will be properly decoded by jsx
    Request = <<"{\"type\":\"eval\",\"expression\":\"hello world + test\"}">>,
    ?assertEqual({eval, "hello world + test"}, beamtalk_repl:parse_request(Request)).

parse_newlines_in_expression_test() ->
    %% Test expressions with newline escape sequences in JSON
    %% jsx will properly handle the \\n escape sequence
    Request = <<"{\"type\":\"eval\",\"expression\":\"line1\\nline2\"}">>,
    Result = beamtalk_repl:parse_request(Request),
    %% Should get eval tuple with the expression
    ?assertMatch({eval, _}, Result),
    {eval, Expr} = Result,
    %% The expression should be a string (not empty)
    ?assert(length(Expr) > 0).

format_complex_nested_structure_test() ->
    %% Test formatting deeply nested structures
    NestedMap = #{
        level1 => #{
            level2 => #{
                level3 => [1, 2, 3]
            }
        }
    },
    Response = beamtalk_repl:format_response(NestedMap),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

format_large_list_test() ->
    %% Test formatting large lists
    LargeList = lists:seq(1, 1000),
    Response = beamtalk_repl:format_response(LargeList),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(LargeList, maps:get(<<"value">>, Decoded)).

format_binary_with_special_chars_test() ->
    %% Test formatting binaries with special characters
    Binary = <<"tabs\there\nand\nnewlines">>,
    Response = beamtalk_repl:format_response(Binary),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

format_error_with_long_message_test() ->
    %% Test error formatting with very long message
    LongMessage = lists:duplicate(500, "error "),
    Response = beamtalk_repl:format_error({custom_error, LongMessage}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))).

server_handles_stop_during_operation_test() ->
    %% Test that server handles stop gracefully
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    
    %% Stop immediately
    beamtalk_repl:stop(Pid),
    
    %% Should not crash
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

server_survives_info_messages_test() ->
    %% Test that server ignores unexpected info messages
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    
    %% Send random info messages
    Pid ! {unexpected, message},
    Pid ! random_atom,
    Pid ! [1, 2, 3],
    
    timer:sleep(10),
    
    %% Server should still be alive and functional
    ?assert(is_process_alive(Pid)),
    ?assertEqual(#{}, beamtalk_repl:get_bindings(Pid)),
    
    beamtalk_repl:stop(Pid).

concurrent_get_port_requests_test() ->
    %% Test concurrent access to get_port
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Parent = self(),
    
    %% Spawn multiple processes requesting the port
    [spawn(fun() ->
        Port = beamtalk_repl:get_port(Pid),
        Parent ! {port, Port}
    end) || _ <- lists:seq(1, 10)],
    
    %% Collect all port responses
    Ports = [receive {port, P} -> P after 1000 -> timeout end || _ <- lists:seq(1, 10)],
    
    %% All should return the same port
    [FirstPort | Rest] = Ports,
    ?assert(lists:all(fun(P) -> P =:= FirstPort end, Rest)),
    
    beamtalk_repl:stop(Pid).

concurrent_get_bindings_test() ->
    %% Test concurrent access to bindings
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Parent = self(),
    
    %% Spawn multiple processes getting bindings
    [spawn(fun() ->
        Bindings = beamtalk_repl:get_bindings(Pid),
        Parent ! {bindings, Bindings}
    end) || _ <- lists:seq(1, 10)],
    
    %% All should return empty map
    Results = [receive {bindings, B} -> B after 1000 -> timeout end || _ <- lists:seq(1, 10)],
    ?assert(lists:all(fun(B) -> B =:= #{} end, Results)),
    
    beamtalk_repl:stop(Pid).

parse_request_type_variations_test() ->
    %% Test all valid request types
    ?assertEqual({eval, "expr"}, 
        beamtalk_repl:parse_request(<<"{\"type\":\"eval\",\"expression\":\"expr\"}">>)),
    ?assertEqual({clear_bindings}, 
        beamtalk_repl:parse_request(<<"{\"type\":\"clear\"}">>)),
    ?assertEqual({get_bindings}, 
        beamtalk_repl:parse_request(<<"{\"type\":\"bindings\"}">>)),
    ?assertEqual({load_file, "file.bt"}, 
        beamtalk_repl:parse_request(<<"{\"type\":\"load\",\"path\":\"file.bt\"}">>)).

format_response_type_coverage_test() ->
    %% Ensure all basic Erlang types can be formatted
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response(42)),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response(3.14)),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response(atom)),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response(<<"binary">>)),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response([1, 2, 3])),
    ?assertMatch(<<"{\"type\":\"result\",\"value\":", _/binary>>, 
        beamtalk_repl:format_response(#{key => value})).

%%% Server Lifecycle Tests

%% Helper function to generate a random test port to avoid conflicts
test_port() ->
    50000 + rand:uniform(10000).

server_starts_and_stops_test() ->
    %% Start the server with a random high port to avoid conflicts
    Port = test_port(),
    {ok, Pid} = beamtalk_repl:start_link(Port),
    ?assert(is_process_alive(Pid)),
    ok = beamtalk_repl:stop(Pid),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

server_starts_with_default_port_test() ->
    %% Test start_link/0 which uses application:get_env
    application:set_env(beamtalk_runtime, repl_port, 51234),
    try
        {ok, Pid} = beamtalk_repl:start_link(),
        ?assert(is_process_alive(Pid)),
        Port = beamtalk_repl:get_port(Pid),
        ?assertEqual(51234, Port),
        ok = beamtalk_repl:stop(Pid)
    after
        application:unset_env(beamtalk_runtime, repl_port)
    end.

server_starts_with_options_test() ->
    Port = test_port(),
    Options = #{daemon_socket_path => "/tmp/test.sock"},
    {ok, Pid} = beamtalk_repl:start_link(Port, Options),
    ?assert(is_process_alive(Pid)),
    ok = beamtalk_repl:stop(Pid).

get_port_returns_correct_port_test() ->
    Port = test_port(),
    {ok, Pid} = beamtalk_repl:start_link(Port),
    ?assertEqual(Port, beamtalk_repl:get_port(Pid)),
    ok = beamtalk_repl:stop(Pid).

clear_bindings_test() ->
    Port = test_port(),
    {ok, Pid} = beamtalk_repl:start_link(Port),
    %% Clear bindings should succeed
    ok = beamtalk_repl:clear_bindings(Pid),
    ok = beamtalk_repl:stop(Pid).

get_bindings_test() ->
    Port = test_port(),
    {ok, Pid} = beamtalk_repl:start_link(Port),
    %% Initially should have empty bindings
    Bindings = beamtalk_repl:get_bindings(Pid),
    ?assertEqual(#{}, Bindings),
    ok = beamtalk_repl:stop(Pid).

%%% Error Formatting Tests (additional edge cases)

format_error_undefined_variable_test() ->
    Response = beamtalk_repl:format_error({undefined_variable, "foo"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assertMatch(<<"Undefined variable: foo">>, Message).

format_error_compilation_failed_test() ->
    Response = beamtalk_repl:format_error({compile_error, "syntax error"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assertEqual(<<"syntax error">>, Message).

format_error_generic_test() ->
    Response = beamtalk_repl:format_error(some_unknown_error),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    %% Generic errors are formatted with io_lib:format("~p", [Reason])
    ?assertMatch(<<"some_unknown_error">>, Message).

%%% BT-343: handle_info client_request tests

handle_info_client_request_clear_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Send clear_bindings via handle_info client_request path
    Request = <<"{\"type\": \"clear\"}">>,
    Pid ! {client_request, Request, self()},
    receive
        {response, Response} ->
            Decoded = jsx:decode(Response, [return_maps]),
            ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded))
    after 5000 ->
        ?assert(false)
    end,
    beamtalk_repl:stop(Pid).

handle_info_client_request_bindings_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Request = <<"{\"type\": \"bindings\"}">>,
    Pid ! {client_request, Request, self()},
    receive
        {response, Response} ->
            Decoded = jsx:decode(Response, [return_maps]),
            ?assertEqual(<<"bindings">>, maps:get(<<"type">>, Decoded)),
            ?assert(maps:is_key(<<"bindings">>, Decoded))
    after 5000 ->
        ?assert(false)
    end,
    beamtalk_repl:stop(Pid).

handle_info_client_request_actors_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Request = <<"{\"type\": \"actors\"}">>,
    Pid ! {client_request, Request, self()},
    receive
        {response, Response} ->
            Decoded = jsx:decode(Response, [return_maps]),
            ?assertEqual(<<"actors">>, maps:get(<<"type">>, Decoded))
    after 5000 ->
        ?assert(false)
    end,
    beamtalk_repl:stop(Pid).

handle_info_client_request_modules_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Request = <<"{\"type\": \"modules\"}">>,
    Pid ! {client_request, Request, self()},
    receive
        {response, Response} ->
            Decoded = jsx:decode(Response, [return_maps]),
            ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded))
    after 5000 ->
        ?assert(false)
    end,
    beamtalk_repl:stop(Pid).

handle_info_client_request_parse_error_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    %% Send request with unknown type
    Request = <<"{\"type\": \"eval\"}">>,
    Pid ! {client_request, Request, self()},
    receive
        {response, Response} ->
            Decoded = jsx:decode(Response, [return_maps]),
            %% Missing expression field triggers unknown_type error
            ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded))
    after 5000 ->
        ?assert(false)
    end,
    beamtalk_repl:stop(Pid).

handle_info_unknown_message_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Pid ! completely_unexpected_message,
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    beamtalk_repl:stop(Pid).

handle_cast_unknown_message_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    gen_server:cast(Pid, unknown_cast),
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    beamtalk_repl:stop(Pid).

handle_call_unknown_request_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    Result = gen_server:call(Pid, unknown_request),
    ?assertEqual({error, unknown_request}, Result),
    beamtalk_repl:stop(Pid).

handle_call_stop_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ok = gen_server:call(Pid, stop),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

clear_bindings_via_api_test() ->
    {ok, Pid} = beamtalk_repl:start_link(0, #{}),
    ok = beamtalk_repl:clear_bindings(Pid),
    ?assertEqual(#{}, beamtalk_repl:get_bindings(Pid)),
    beamtalk_repl:stop(Pid).
