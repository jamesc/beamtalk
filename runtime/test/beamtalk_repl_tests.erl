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
