%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_server module
%%%
%%% Tests TCP client handling, JSON protocol parsing, and response formatting.

-module(beamtalk_repl_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Request parsing tests

parse_request_eval_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_clear_test() ->
    Request = <<"{\"type\": \"clear\"}">>,
    ?assertEqual({clear_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_bindings_test() ->
    Request = <<"{\"type\": \"bindings\"}">>,
    ?assertEqual({get_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_load_test() ->
    Request = <<"{\"type\": \"load\", \"path\": \"examples/counter.bt\"}">>,
    ?assertEqual({load_file, "examples/counter.bt"}, beamtalk_repl_server:parse_request(Request)).

parse_request_with_newline_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 2\"}\n">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_raw_expression_test() ->
    %% Backwards compatibility: non-JSON is treated as eval
    Request = <<"1 + 2">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_empty_test() ->
    Request = <<"">>,
    ?assertMatch({error, empty_expression}, beamtalk_repl_server:parse_request(Request)).

parse_request_unknown_type_test() ->
    Request = <<"{\"type\": \"unknown\"}">>,
    ?assertMatch({error, {invalid_request, unknown_type}}, beamtalk_repl_server:parse_request(Request)).

parse_request_missing_expression_test() ->
    Request = <<"{\"type\": \"eval\"}">>,
    %% Missing expression field - parser can't extract the expression value
    %% so it returns an invalid_request error
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {invalid_request, _}}, Result).

%%% Response formatting tests

format_response_integer_test() ->
    Response = beamtalk_repl_server:format_response(42),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)).

format_response_string_test() ->
    Response = beamtalk_repl_server:format_response("hello"),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"hello">>, maps:get(<<"value">>, Decoded)).

format_response_atom_test() ->
    Response = beamtalk_repl_server:format_response(ok),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"ok">>, maps:get(<<"value">>, Decoded)).

format_response_list_test() ->
    Response = beamtalk_repl_server:format_response([1, 2, 3]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual([1, 2, 3], maps:get(<<"value">>, Decoded)).

format_response_map_test() ->
    Response = beamtalk_repl_server:format_response(#{x => 1, y => 2}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assertEqual(1, maps:get(<<"x">>, Value)),
    ?assertEqual(2, maps:get(<<"y">>, Value)).

format_response_pid_test() ->
    Pid = self(),
    Response = beamtalk_repl_server:format_response(Pid),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should be formatted as #Actor<...>
    ?assert(binary:match(Value, <<"#Actor<">>) =/= nomatch).

format_response_function_test() ->
    Fun = fun(X) -> X + 1 end,
    Response = beamtalk_repl_server:format_response(Fun),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should be formatted as "a Block/N"
    ?assert(binary:match(Value, <<"a Block/">>) =/= nomatch).

format_response_tuple_test() ->
    Response = beamtalk_repl_server:format_response({ok, value}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Tuples are wrapped with __tuple__ marker
    ?assertMatch(#{<<"__tuple__">> := _}, Value).

%%% Error formatting tests

format_error_empty_expression_test() ->
    Response = beamtalk_repl_server:format_error(empty_expression),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Empty expression">>, maps:get(<<"message">>, Decoded)).

format_error_timeout_test() ->
    Response = beamtalk_repl_server:format_error(timeout),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)).

format_error_compile_error_test() ->
    Response = beamtalk_repl_server:format_error({compile_error, <<"Syntax error">>}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"Syntax error">>, maps:get(<<"message">>, Decoded)).

format_error_undefined_variable_test() ->
    Response = beamtalk_repl_server:format_error({undefined_variable, x}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Undefined variable">>) =/= nomatch).

format_error_eval_error_test() ->
    Response = beamtalk_repl_server:format_error({eval_error, error, badarg}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Evaluation error">>) =/= nomatch).

format_error_daemon_unavailable_test() ->
    Response = beamtalk_repl_server:format_error(daemon_unavailable),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"daemon">>) =/= nomatch).

%%% Bindings formatting tests

format_bindings_empty_test() ->
    Response = beamtalk_repl_server:format_bindings(#{}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"bindings">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(#{}, maps:get(<<"bindings">>, Decoded)).

format_bindings_single_test() ->
    Response = beamtalk_repl_server:format_bindings(#{x => 42}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"x">>, Bindings)).

format_bindings_multiple_test() ->
    Response = beamtalk_repl_server:format_bindings(#{x => 1, y => 2, z => 3}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(1, maps:get(<<"x">>, Bindings)),
    ?assertEqual(2, maps:get(<<"y">>, Bindings)),
    ?assertEqual(3, maps:get(<<"z">>, Bindings)).

format_bindings_complex_values_test() ->
    Response = beamtalk_repl_server:format_bindings(#{
        atom => ok,
        str => "hello",
        list => [1, 2, 3]
    }),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(<<"ok">>, maps:get(<<"atom">>, Bindings)),
    ?assertEqual(<<"hello">>, maps:get(<<"str">>, Bindings)),
    ?assertEqual([1, 2, 3], maps:get(<<"list">>, Bindings)).

%%% Loaded classes formatting tests

format_loaded_empty_test() ->
    Response = beamtalk_repl_server:format_loaded([]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"loaded">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"classes">>, Decoded)).

format_loaded_single_test() ->
    Response = beamtalk_repl_server:format_loaded(["Counter"]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual([<<"Counter">>], maps:get(<<"classes">>, Decoded)).

format_loaded_multiple_test() ->
    Response = beamtalk_repl_server:format_loaded(["Counter", "Point", "Actor"]),
    Decoded = jsx:decode(Response, [return_maps]),
    Classes = maps:get(<<"classes">>, Decoded),
    ?assertEqual(3, length(Classes)),
    ?assert(lists:member(<<"Counter">>, Classes)),
    ?assert(lists:member(<<"Point">>, Classes)),
    ?assert(lists:member(<<"Actor">>, Classes)).

%%% JSON parsing tests (internal parser)

parse_json_not_json_test() ->
    %% Non-JSON should be treated as raw expression
    NotJson = <<"not json at all">>,
    ?assertEqual({eval, "not json at all"}, beamtalk_repl_server:parse_request(NotJson)).
