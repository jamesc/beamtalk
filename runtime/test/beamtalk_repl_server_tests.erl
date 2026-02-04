%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_server module
%%%
%%% Tests TCP client handling, JSON protocol parsing, and response formatting.

-module(beamtalk_repl_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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

format_response_future_rejected_test() ->
    %% Regression test: ensure {future_rejected, Reason} can be formatted without crashing
    %% (even though these are now returned as errors, not results, in normal REPL flow)
    %% This test guards against crashes when term_to_json tries to embed maps in iolists
    Error = {unknown_message, increment, 'Counter'},
    Response = beamtalk_repl_server:format_response({future_rejected, Error}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should be formatted as #Future<rejected: ...> with user-friendly message
    ?assert(binary:match(Value, <<"#Future<rejected: ">>) =/= nomatch),
    %% The unknown_message is formatted as "ClassName does not understand 'Selector'"
    ?assert(binary:match(Value, <<"Counter">>) =/= nomatch),
    ?assert(binary:match(Value, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Value, <<"increment">>) =/= nomatch).

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
    Response = beamtalk_repl_server:format_loaded([#{name => "Counter", superclass => "Actor"}]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual([<<"Counter">>], maps:get(<<"classes">>, Decoded)).

format_loaded_multiple_test() ->
    Response = beamtalk_repl_server:format_loaded([
        #{name => "Counter", superclass => "Actor"},
        #{name => "Point", superclass => "Object"},
        #{name => "Actor", superclass => "Object"}
    ]),
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

%%% term_to_json is internal - tested indirectly via format_response

%%% Response formatting edge cases

format_response_nil_test() ->
    Response = beamtalk_repl_server:format_response(nil),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"nil">>, maps:get(<<"value">>, Decoded)).

format_response_large_number_test() ->
    Response = beamtalk_repl_server:format_response(999999999999999),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(999999999999999, maps:get(<<"value">>, Decoded)).

format_response_complex_pid_test() ->
    %% Test that PID formatting includes #Actor< prefix
    Parent = self(),
    Pid = spawn(fun() -> 
        receive stop -> Parent ! stopped
        after 100 -> ok
        end
    end),
    Response = beamtalk_repl_server:format_response(Pid),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"#Actor<">>) =/= nomatch),
    ?assert(binary:match(Value, <<">">>) =/= nomatch),
    %% Clean up spawned process
    Pid ! stop,
    receive stopped -> ok after 200 -> ok end.

format_response_multi_arity_function_test() ->
    %% Test function with different arity
    Fun0 = fun() -> ok end,
    Fun3 = fun(A, B, C) -> {A, B, C} end,
    
    Response0 = beamtalk_repl_server:format_response(Fun0),
    Response3 = beamtalk_repl_server:format_response(Fun3),
    
    Decoded0 = jsx:decode(Response0, [return_maps]),
    Decoded3 = jsx:decode(Response3, [return_maps]),
    
    Value0 = maps:get(<<"value">>, Decoded0),
    Value3 = maps:get(<<"value">>, Decoded3),
    
    ?assert(binary:match(Value0, <<"a Block/0">>) =/= nomatch),
    ?assert(binary:match(Value3, <<"a Block/3">>) =/= nomatch).

%%% Error formatting edge cases

format_error_load_error_test() ->
    Response = beamtalk_repl_server:format_error({load_error, badfile}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to load bytecode">>) =/= nomatch).

format_error_file_not_found_test() ->
    Response = beamtalk_repl_server:format_error({file_not_found, "/path/to/file.bt"}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"File not found">>) =/= nomatch),
    ?assert(binary:match(Message, <<"/path/to/file.bt">>) =/= nomatch).

format_error_read_error_test() ->
    Response = beamtalk_repl_server:format_error({read_error, eacces}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to read file">>) =/= nomatch).

format_error_generic_tuple_test() ->
    Response = beamtalk_repl_server:format_error({unknown_error_type, "details"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(is_binary(Message)).

format_error_generic_atom_test() ->
    Response = beamtalk_repl_server:format_error(some_unknown_error),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    %% Generic errors are formatted with ~p, so should contain the atom name
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0).

%%% Bindings and loaded formatting edge cases

format_bindings_with_special_characters_test() ->
    %% Test bindings with special characters in keys
    Response = beamtalk_repl_server:format_bindings(#{'_privateVar' => 42, 'CamelCase' => "test"}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"_privateVar">>, Bindings)),
    ?assertEqual(<<"test">>, maps:get(<<"CamelCase">>, Bindings)).

format_loaded_preserves_order_test() ->
    %% Verify that loaded classes are returned in the same order
    Classes = [
        #{name => "Zebra", superclass => "Object"},
        #{name => "Alpha", superclass => "Object"},
        #{name => "Middle", superclass => "Object"}
    ],
    Response = beamtalk_repl_server:format_loaded(Classes),
    Decoded = jsx:decode(Response, [return_maps]),
    Result = maps:get(<<"classes">>, Decoded),
    ?assertEqual([<<"Zebra">>, <<"Alpha">>, <<"Middle">>], Result).

%%% Request parsing edge cases

parse_request_whitespace_only_test() ->
    Request = <<"   \n\t  ">>,
    Result = beamtalk_repl_server:parse_request(Request),
    %% Whitespace-only is treated as empty after stripping
    ?assertMatch({error, empty_expression}, Result).

parse_request_json_with_extra_fields_test() ->
    %% JSON with extra fields should be ignored
    Request = <<"{\"type\": \"eval\", \"expression\": \"1 + 1\", \"extra\": \"ignored\"}">>,
    ?assertEqual({eval, "1 + 1"}, beamtalk_repl_server:parse_request(Request)).

parse_request_load_with_spaces_in_path_test() ->
    Request = <<"{\"type\": \"load\", \"path\": \"/path/with spaces/file.bt\"}">>,
    ?assertEqual({load_file, "/path/with spaces/file.bt"}, beamtalk_repl_server:parse_request(Request)).

parse_request_string_literal_with_escaped_quotes_test() ->
    %% Test parsing string literal with escaped quotes (BT-227 regression test)
    %% The expression "\"hello\"" contains escaped quotes that must be parsed correctly
    Request = <<"{\"type\": \"eval\", \"expression\": \"\\\"hello\\\"\"}">>,
    ?assertEqual({eval, "\"hello\""}, beamtalk_repl_server:parse_request(Request)).

parse_request_malformed_json_test() ->
    %% Malformed JSON should fall back to raw expression (backwards compatibility)
    Request = <<"{\"type\": incomplete">>,
    ?assertEqual({eval, "{\"type\": incomplete"}, beamtalk_repl_server:parse_request(Request)).

parse_request_unicode_test() ->
    %% Test with unicode characters
    Request = <<"{\"type\": \"eval\", \"expression\": \"greeting := \\\"你好\\\"\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({eval, _}, Result).

%%% BT-238: New error formatting tests

%% Test format_error_message with #beamtalk_error{} record
format_error_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ErrorWithSelector = beamtalk_error:with_selector(Error, super),
    Response = beamtalk_repl_server:format_error(ErrorWithSelector),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Counter">>) =/= nomatch),
    ?assert(binary:match(Message, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Message, <<"'super'">>) =/= nomatch).

%% Test format_error_message with #beamtalk_error{} with hint
format_error_beamtalk_error_with_hint_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, foo),
    Error = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
    Response = beamtalk_repl_server:format_error(Error),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Integer">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Hint:">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Check spelling">>) =/= nomatch).

%% Test term_to_json with #beamtalk_error{} record
format_response_beamtalk_error_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'String'),
    Error = beamtalk_error:with_selector(Error0, typo),
    Response = beamtalk_repl_server:format_response(Error),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"String">>) =/= nomatch),
    ?assert(binary:match(Value, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Value, <<"'typo'">>) =/= nomatch).

%% Test format_response with {future_rejected, #beamtalk_error{}}
format_response_future_rejected_beamtalk_error_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Actor'),
    Error = beamtalk_error:with_selector(Error0, badMethod),
    Response = beamtalk_repl_server:format_response({future_rejected, Error}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"#Future<rejected:">>) =/= nomatch),
    ?assert(binary:match(Value, <<"Actor">>) =/= nomatch),
    ?assert(binary:match(Value, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Value, <<"'badMethod'">>) =/= nomatch).

%% Test format_rejection_reason fallback with arbitrary term
format_rejection_reason_fallback_test() ->
    %% Test that the fallback can handle arbitrary terms without crashing
    Result = beamtalk_repl_server:format_response({future_rejected, {custom_error, some, data}}),
    Decoded = jsx:decode(Result, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should format the tuple with ~p
    ?assert(binary:match(Value, <<"#Future<rejected:">>) =/= nomatch),
    ?assert(is_binary(Value)).
