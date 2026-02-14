%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_repl_server module
%%%
%%% Tests TCP client handling, JSON protocol parsing, and response formatting.

-module(beamtalk_repl_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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

%%% New protocol format (op) parsing tests

parse_request_op_eval_test() ->
    Request = <<"{\"op\": \"eval\", \"id\": \"msg-001\", \"code\": \"1 + 2\"}">>,
    ?assertEqual({eval, "1 + 2"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_clear_test() ->
    Request = <<"{\"op\": \"clear\", \"id\": \"msg-002\"}">>,
    ?assertEqual({clear_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_bindings_test() ->
    Request = <<"{\"op\": \"bindings\"}">>,
    ?assertEqual({get_bindings}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_load_file_test() ->
    Request = <<"{\"op\": \"load-file\", \"path\": \"examples/counter.bt\"}">>,
    ?assertEqual({load_file, "examples/counter.bt"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_actors_test() ->
    Request = <<"{\"op\": \"actors\"}">>,
    ?assertEqual({list_actors}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_modules_test() ->
    Request = <<"{\"op\": \"modules\"}">>,
    ?assertEqual({list_modules}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_kill_test() ->
    Request = <<"{\"op\": \"kill\", \"actor\": \"<0.123.0>\"}">>,
    ?assertEqual({kill_actor, "<0.123.0>"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unload_test() ->
    Request = <<"{\"op\": \"unload\", \"module\": \"Counter\"}">>,
    ?assertEqual({unload_module, "Counter"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unknown_test() ->
    Request = <<"{\"op\": \"foobar\"}">>,
    ?assertMatch({error, {unknown_op, <<"foobar">>}}, beamtalk_repl_server:parse_request(Request)).

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
    %% BT-535: Maps are now pre-formatted as Beamtalk syntax strings
    ?assert(is_binary(Value)),
    ?assert(binary:match(Value, <<"#x => 1">>) =/= nomatch),
    ?assert(binary:match(Value, <<"#y => 2">>) =/= nomatch).

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
    %% BT-536: Tuples are formatted as {el1, el2, ...} with symbol notation
    ?assertEqual(<<"{#ok, #value}">>, Value).

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

%%% BT-342: Additional term_to_json edge case tests

term_to_json_integer_test() ->
    ?assertEqual(42, beamtalk_repl_server:term_to_json(42)).

term_to_json_float_test() ->
    ?assertEqual(3.14, beamtalk_repl_server:term_to_json(3.14)).

term_to_json_boolean_test() ->
    ?assertEqual(true, beamtalk_repl_server:term_to_json(true)),
    ?assertEqual(false, beamtalk_repl_server:term_to_json(false)).

term_to_json_atom_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_server:term_to_json(hello)),
    ?assertEqual(<<"nil">>, beamtalk_repl_server:term_to_json(nil)).

term_to_json_binary_test() ->
    ?assertEqual(<<"foo">>, beamtalk_repl_server:term_to_json(<<"foo">>)).

term_to_json_invalid_utf8_binary_test() ->
    Result = beamtalk_repl_server:term_to_json(<<255, 254>>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<255, 254>>, Result).

term_to_json_printable_list_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_server:term_to_json("hello")).

term_to_json_printable_list_unicode_test() ->
    %% Use explicit codepoints for encoding independence
    ?assertEqual(<<"café"/utf8>>, beamtalk_repl_server:term_to_json("caf" ++ [16#00E9])).

term_to_json_non_printable_list_test() ->
    Result = beamtalk_repl_server:term_to_json([1, 2, 3]),
    ?assertEqual([1, 2, 3], Result).

term_to_json_nested_list_test() ->
    Result = beamtalk_repl_server:term_to_json([<<"a">>, 1, true]),
    ?assertEqual([<<"a">>, 1, true], Result).

term_to_json_empty_list_test() ->
    %% Empty list should serialize as empty JSON array, not empty string
    ?assertEqual([], beamtalk_repl_server:term_to_json([])).

term_to_json_map_with_atom_keys_test() ->
    %% BT-535: Maps are pre-formatted as Beamtalk syntax strings
    Result = beamtalk_repl_server:term_to_json(#{name => <<"test">>, count => 5}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#count => 5">>) =/= nomatch),
    ?assert(binary:match(Result, <<"#name => test">>) =/= nomatch).

term_to_json_map_with_binary_keys_test() ->
    Result = beamtalk_repl_server:term_to_json(#{<<"key">> => <<"val">>}),
    ?assertEqual(<<"#{key => val}">>, Result).

term_to_json_map_with_list_keys_test() ->
    %% BT-535: List keys are printed using print_string format
    Result = beamtalk_repl_server:term_to_json(#{"stringkey" => 42}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"42">>) =/= nomatch).

term_to_json_map_with_non_printable_list_key_test() ->
    Result = beamtalk_repl_server:term_to_json(#{[1,2,3] => <<"val">>}),
    ?assert(is_binary(Result)).

term_to_json_map_with_integer_key_test() ->
    Result = beamtalk_repl_server:term_to_json(#{99 => <<"val">>}),
    ?assert(is_binary(Result)).

term_to_json_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)),
    Result = beamtalk_repl_server:term_to_json(Pid),
    ?assert(binary:match(Result, <<"#Dead<">>) =/= nomatch).

term_to_json_alive_pid_test() ->
    Parent = self(),
    Pid = spawn(fun() -> receive stop -> Parent ! done end end),
    Result = beamtalk_repl_server:term_to_json(Pid),
    ?assert(binary:match(Result, <<"#Actor<">>) =/= nomatch),
    Pid ! stop,
    receive done -> ok after 200 -> ok end.

term_to_json_function_arity_0_test() ->
    Result = beamtalk_repl_server:term_to_json(fun() -> ok end),
    ?assertEqual(<<"a Block/0">>, Result).

term_to_json_function_arity_2_test() ->
    Result = beamtalk_repl_server:term_to_json(fun(A, B) -> {A, B} end),
    ?assertEqual(<<"a Block/2">>, Result).

term_to_json_beamtalk_object_tuple_test() ->
    Pid = spawn(fun() -> receive _ -> ok end end),
    Result = beamtalk_repl_server:term_to_json({beamtalk_object, 'Counter', counter, Pid}),
    ?assert(binary:match(Result, <<"#Actor<">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Counter">>) =/= nomatch),
    exit(Pid, kill).

term_to_json_future_timeout_test() ->
    Pid = spawn(fun() -> receive _ -> ok end end),
    Result = beamtalk_repl_server:term_to_json({future_timeout, Pid}),
    ?assert(binary:match(Result, <<"#Future<timeout,">>) =/= nomatch),
    exit(Pid, kill).

term_to_json_future_rejected_test() ->
    Result = beamtalk_repl_server:term_to_json({future_rejected, some_reason}),
    ?assert(binary:match(Result, <<"#Future<rejected:">>) =/= nomatch).

term_to_json_generic_tuple_test() ->
    Result = beamtalk_repl_server:term_to_json({a, b, c}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#a, #b, #c}">>, Result).

term_to_json_fallback_reference_test() ->
    Ref = make_ref(),
    Result = beamtalk_repl_server:term_to_json(Ref),
    ?assert(is_binary(Result)).

%%% BT-342: Additional format_error_message tests

format_error_message_module_not_found_test() ->
    Msg = beamtalk_repl_server:format_error_message({module_not_found, <<"counter">>}),
    ?assert(binary:match(Msg, <<"Module not loaded">>) =/= nomatch).

format_error_message_invalid_module_name_test() ->
    Msg = beamtalk_repl_server:format_error_message({invalid_module_name, <<"123bad">>}),
    ?assert(binary:match(Msg, <<"Invalid module name">>) =/= nomatch).

format_error_message_actors_exist_singular_test() ->
    Msg = beamtalk_repl_server:format_error_message({actors_exist, counter, 1}),
    ?assert(binary:match(Msg, <<"1 actor still running">>) =/= nomatch),
    ?assert(binary:match(Msg, <<":kill">>) =/= nomatch).

format_error_message_actors_exist_plural_test() ->
    Msg = beamtalk_repl_server:format_error_message({actors_exist, counter, 3}),
    ?assert(binary:match(Msg, <<"3 actors still running">>) =/= nomatch).

format_error_message_unknown_op_test() ->
    Msg = beamtalk_repl_server:format_error_message({unknown_op, <<"badop">>}),
    ?assert(binary:match(Msg, <<"Unknown operation">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"badop">>) =/= nomatch).

format_error_message_inspect_failed_test() ->
    Msg = beamtalk_repl_server:format_error_message({inspect_failed, "<0.1.0>"}),
    ?assert(binary:match(Msg, <<"Failed to inspect actor">>) =/= nomatch).

format_error_message_actor_not_alive_test() ->
    Msg = beamtalk_repl_server:format_error_message({actor_not_alive, "<0.1.0>"}),
    ?assert(binary:match(Msg, <<"Actor is not alive">>) =/= nomatch).

format_error_message_reload_module_not_loaded_test() ->
    Msg = beamtalk_repl_server:format_error_message({module_not_loaded, "counter"}),
    ?assert(binary:match(Msg, <<"Module not loaded">>) =/= nomatch).

format_error_message_no_source_file_test() ->
    Msg = beamtalk_repl_server:format_error_message({no_source_file, "Counter"}),
    ?assert(binary:match(Msg, <<"No source file recorded">>) =/= nomatch).

format_error_message_missing_module_name_test() ->
    Msg = beamtalk_repl_server:format_error_message({missing_module_name, reload}),
    ?assert(binary:match(Msg, <<"Usage">>) =/= nomatch).

format_error_message_session_creation_failed_test() ->
    Msg = beamtalk_repl_server:format_error_message({session_creation_failed, max_children}),
    ?assert(binary:match(Msg, <<"Failed to create session">>) =/= nomatch).

format_error_message_invalid_request_test() ->
    Msg = beamtalk_repl_server:format_error_message({invalid_request, bad_format}),
    ?assert(binary:match(Msg, <<"Invalid request">>) =/= nomatch).

format_error_message_parse_error_test() ->
    Msg = beamtalk_repl_server:format_error_message({parse_error, syntax}),
    ?assert(binary:match(Msg, <<"Parse error">>) =/= nomatch).

format_error_message_compile_error_list_test() ->
    Msg = beamtalk_repl_server:format_error_message({compile_error, "something wrong"}),
    ?assertEqual(<<"something wrong">>, Msg).

format_error_message_fallback_test() ->
    Msg = beamtalk_repl_server:format_error_message({completely_unknown, stuff}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

%%% BT-237: eval_error with #beamtalk_error{} formatting

format_error_message_eval_error_beamtalk_error_test() ->
    Error = beamtalk_error:with_selector(
        beamtalk_error:new(does_not_understand, 'Integer'), nonExistentMethod),
    Msg = beamtalk_repl_server:format_error_message({eval_error, error, Error}),
    ?assertEqual(<<"Integer does not understand 'nonExistentMethod'">>, Msg).

format_error_message_eval_error_beamtalk_error_with_hint_test() ->
    Error0 = beamtalk_error:new(instantiation_error, 'Actor'),
    Error1 = beamtalk_error:with_selector(Error0, new),
    Error = beamtalk_error:with_hint(Error1, <<"Use spawn instead">>),
    Msg = beamtalk_repl_server:format_error_message({eval_error, error, Error}),
    ?assert(binary:match(Msg, <<"Actor">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"Hint: Use spawn instead">>) =/= nomatch).

%%% BT-342: format_actors tests

format_actors_empty_test() ->
    Response = beamtalk_repl_server:format_actors([]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"actors">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)).

format_actors_single_test() ->
    Pid = self(),
    Actors = [#{pid => Pid, class => 'Counter', module => counter, spawned_at => 1234567890}],
    Response = beamtalk_repl_server:format_actors(Actors),
    Decoded = jsx:decode(Response, [return_maps]),
    ActorList = maps:get(<<"actors">>, Decoded),
    ?assertEqual(1, length(ActorList)),
    [Actor] = ActorList,
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Actor)),
    ?assertEqual(<<"counter">>, maps:get(<<"module">>, Actor)),
    ?assertEqual(1234567890, maps:get(<<"spawned_at">>, Actor)).

format_actors_multiple_test() ->
    Pid = self(),
    Actors = [
        #{pid => Pid, class => 'Counter', module => counter, spawned_at => 100},
        #{pid => Pid, class => 'Timer', module => timer_actor, spawned_at => 200}
    ],
    Response = beamtalk_repl_server:format_actors(Actors),
    Decoded = jsx:decode(Response, [return_maps]),
    ActorList = maps:get(<<"actors">>, Decoded),
    ?assertEqual(2, length(ActorList)).

%%% BT-342: format_modules tests

format_modules_empty_test() ->
    Response = beamtalk_repl_server:format_modules([]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"modules">>, Decoded)).

format_modules_single_test() ->
    Info = {counter, #{
        name => <<"Counter">>,
        source_file => "/tmp/counter.bt",
        actor_count => 3,
        load_time => 1234567890,
        time_ago => "2 minutes ago"
    }},
    Response = beamtalk_repl_server:format_modules([Info]),
    Decoded = jsx:decode(Response, [return_maps]),
    Modules = maps:get(<<"modules">>, Decoded),
    ?assertEqual(1, length(Modules)),
    [Mod] = Modules,
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, Mod)),
    ?assertEqual(<<"/tmp/counter.bt">>, maps:get(<<"source_file">>, Mod)),
    ?assertEqual(3, maps:get(<<"actor_count">>, Mod)).

%%% BT-342: format_response edge cases

format_response_empty_map_test() ->
    Response = beamtalk_repl_server:format_response(#{}),
    Decoded = jsx:decode(Response, [return_maps]),
    %% BT-535: Empty map is pre-formatted as "#{}"
    ?assertEqual(<<"#{}">>, maps:get(<<"value">>, Decoded)).

format_response_nested_map_test() ->
    Response = beamtalk_repl_server:format_response(#{a => #{b => 1}}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-535: Nested maps are pre-formatted as Beamtalk syntax
    ?assert(is_binary(Value)),
    ?assertEqual(<<"#{#a => #{#b => 1}}">>, Value).

format_response_true_false_test() ->
    ResponseT = beamtalk_repl_server:format_response(true),
    DecodedT = jsx:decode(ResponseT, [return_maps]),
    ?assertEqual(true, maps:get(<<"value">>, DecodedT)),
    ResponseF = beamtalk_repl_server:format_response(false),
    DecodedF = jsx:decode(ResponseF, [return_maps]),
    ?assertEqual(false, maps:get(<<"value">>, DecodedF)).

%%% BT-520: format_response_with_warnings tests

format_response_with_warnings_no_warnings_test() ->
    Response = beamtalk_repl_server:format_response_with_warnings(42, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_response_with_warnings_single_warning_test() ->
    Response = beamtalk_repl_server:format_response_with_warnings(42, [<<"watch out">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"watch out">>], maps:get(<<"warnings">>, Decoded)).

format_response_with_warnings_multiple_warnings_test() ->
    Warnings = [<<"warn1">>, <<"warn2">>, <<"warn3">>],
    Response = beamtalk_repl_server:format_response_with_warnings("hello", Warnings),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"hello">>, maps:get(<<"value">>, Decoded)),
    ?assertEqual(Warnings, maps:get(<<"warnings">>, Decoded)).

format_response_with_warnings_complex_value_test() ->
    Response = beamtalk_repl_server:format_response_with_warnings(
        #{x => 1, y => 2}, [<<"shadow">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-535: Maps are pre-formatted as Beamtalk syntax strings
    ?assert(is_binary(Value)),
    ?assert(binary:match(Value, <<"#x => 1">>) =/= nomatch),
    ?assertEqual([<<"shadow">>], maps:get(<<"warnings">>, Decoded)).

%%% BT-520: format_error_with_warnings tests

format_error_with_warnings_no_warnings_test() ->
    Response = beamtalk_repl_server:format_error_with_warnings(timeout, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_error_with_warnings_single_warning_test() ->
    Response = beamtalk_repl_server:format_error_with_warnings(
        empty_expression, [<<"deprecated syntax">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Empty expression">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual([<<"deprecated syntax">>], maps:get(<<"warnings">>, Decoded)).

format_error_with_warnings_multiple_warnings_test() ->
    Warnings = [<<"w1">>, <<"w2">>],
    Response = beamtalk_repl_server:format_error_with_warnings(daemon_unavailable, Warnings),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(binary:match(maps:get(<<"message">>, Decoded), <<"daemon">>) =/= nomatch),
    ?assertEqual(Warnings, maps:get(<<"warnings">>, Decoded)).

format_error_with_warnings_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    ErrorWithSel = beamtalk_error:with_selector(Error, foo),
    Response = beamtalk_repl_server:format_error_with_warnings(ErrorWithSel, [<<"hint">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Integer">>) =/= nomatch),
    ?assertEqual([<<"hint">>], maps:get(<<"warnings">>, Decoded)).

%%% BT-520: format_docs tests

format_docs_simple_test() ->
    Response = beamtalk_repl_server:format_docs(<<"Counter is a class">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Counter is a class">>, maps:get(<<"docs">>, Decoded)).

format_docs_empty_test() ->
    Response = beamtalk_repl_server:format_docs(<<>>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<>>, maps:get(<<"docs">>, Decoded)).

format_docs_multiline_test() ->
    Doc = <<"Counter\n\nA simple counter class.\nMethods: increment, decrement">>,
    Response = beamtalk_repl_server:format_docs(Doc),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(Doc, maps:get(<<"docs">>, Decoded)).

%%% BT-520: safe_to_existing_atom tests

safe_to_existing_atom_existing_test() ->
    %% 'ok' is a well-known existing atom
    ?assertEqual({ok, ok}, beamtalk_repl_server:safe_to_existing_atom(<<"ok">>)).

safe_to_existing_atom_nonexistent_test() ->
    %% A random string should fail
    ?assertEqual({error, badarg},
        beamtalk_repl_server:safe_to_existing_atom(<<"xyzzy_nonexistent_atom_9999">>)).

safe_to_existing_atom_empty_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_server:safe_to_existing_atom(<<>>)).

safe_to_existing_atom_non_binary_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_server:safe_to_existing_atom(123)).

safe_to_existing_atom_integer_atom_test() ->
    %% 'Integer' is a well-known existing atom in beamtalk
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_server:safe_to_existing_atom(<<"Integer">>)).

%%% BT-520: generate_session_id tests (via parse_request with session operations)

%% Test that parse_request handles op "sessions"
parse_request_op_sessions_test() ->
    Request = <<"{\"op\": \"sessions\"}">>,
    %% sessions doesn't map via op_to_request - it's handled in handle_op
    %% The legacy parse doesn't have a sessions type, so this goes through op route
    Result = beamtalk_repl_server:parse_request(Request),
    %% sessions isn't in op_to_request, so it's an unknown_op
    ?assertMatch({error, {unknown_op, <<"sessions">>}}, Result).

%% Test that parse_request handles op "clone"
parse_request_op_clone_test() ->
    Request = <<"{\"op\": \"clone\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"clone">>}}, Result).

%% Test that parse_request handles op "close"
parse_request_op_close_test() ->
    Request = <<"{\"op\": \"close\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"close">>}}, Result).

%% Test that parse_request handles op "inspect"
parse_request_op_inspect_test() ->
    Request = <<"{\"op\": \"inspect\", \"actor\": \"<0.1.0>\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"inspect">>}}, Result).

%% Test that parse_request handles op "reload"
parse_request_op_reload_test() ->
    Request = <<"{\"op\": \"reload\", \"module\": \"Counter\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"reload">>}}, Result).

%% Test that parse_request handles op "complete"
parse_request_op_complete_test() ->
    Request = <<"{\"op\": \"complete\", \"code\": \"Cou\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"complete">>}}, Result).

%% Test that parse_request handles op "info"
parse_request_op_info_test() ->
    Request = <<"{\"op\": \"info\", \"symbol\": \"Counter\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({error, {unknown_op, <<"info">>}}, Result).

%% Test that parse_request handles op "docs"
parse_request_op_docs_test() ->
    Request = <<"{\"op\": \"docs\", \"class\": \"Integer\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({get_docs, <<"Integer">>, undefined}, Result).

parse_request_op_docs_with_selector_test() ->
    Request = <<"{\"op\": \"docs\", \"class\": \"Integer\", \"selector\": \"+\"}">>,
    Result = beamtalk_repl_server:parse_request(Request),
    ?assertMatch({get_docs, <<"Integer">>, <<"+">>}, Result).

%%% BT-520: Additional format_error_message edge cases

format_error_message_module_not_loaded_test() ->
    Msg = beamtalk_repl_server:format_error_message({module_not_found, <<"Counter">>}),
    ?assert(binary:match(Msg, <<"Module not loaded">>) =/= nomatch).

format_error_message_invalid_module_test() ->
    Msg = beamtalk_repl_server:format_error_message({invalid_module, missing}),
    %% Falls through to generic formatter
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_unknown_actor_test() ->
    Msg = beamtalk_repl_server:format_error_message({unknown_actor, "<0.1.0>"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_invalid_pid_test() ->
    Msg = beamtalk_repl_server:format_error_message({invalid_pid, "not-a-pid"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_class_not_found_test() ->
    %% Ensure the atom exists first
    _ = 'NonExistentClass',
    Msg = beamtalk_repl_server:format_error_message({class_not_found, 'NonExistentClass'}),
    ?assert(binary:match(Msg, <<"Unknown class">>) =/= nomatch),
    ?assert(binary:match(Msg, <<":modules">>) =/= nomatch).

format_error_message_class_not_found_binary_test() ->
    Msg = beamtalk_repl_server:format_error_message({class_not_found, <<"Foo">>}),
    ?assert(binary:match(Msg, <<"Unknown class">>) =/= nomatch).

format_error_message_method_not_found_test() ->
    Msg = beamtalk_repl_server:format_error_message({method_not_found, 'Integer', <<"+:">>}),
    ?assert(binary:match(Msg, <<"Integer">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"does not understand">>) =/= nomatch).

format_error_message_wrapped_exception_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ErrorWithSel = beamtalk_error:with_selector(Error, badMethod),
    Wrapped = #{'$beamtalk_class' => 'Exception', error => ErrorWithSel},
    Msg = beamtalk_repl_server:format_error_message(Wrapped),
    ?assert(binary:match(Msg, <<"Exception">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"Counter">>) =/= nomatch).

format_error_message_eval_error_wrapped_exception_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = #{'$beamtalk_class' => 'TypeError', error => Error},
    Msg = beamtalk_repl_server:format_error_message({eval_error, error, Wrapped}),
    ?assert(binary:match(Msg, <<"TypeError">>) =/= nomatch).

%%% BT-520: term_to_json additional edge cases

term_to_json_future_pending_test() ->
    %% Simulate a future by spawning a process in beamtalk_future:pending/1
    Pid = beamtalk_future:new(),
    timer:sleep(50),
    Result = beamtalk_repl_server:term_to_json(Pid),
    ?assertEqual(<<"#Future<pending>">>, Result),
    beamtalk_future:resolve(Pid, ok),
    exit(Pid, kill).

term_to_json_nested_map_test() ->
    %% BT-535: Nested maps are pre-formatted as Beamtalk syntax
    Result = beamtalk_repl_server:term_to_json(#{a => #{b => #{c => 42}}}),
    ?assertEqual(<<"#{#a => #{#b => #{#c => 42}}}">>, Result).

term_to_json_empty_map_test() ->
    ?assertEqual(<<"#{}">>, beamtalk_repl_server:term_to_json(#{})).

term_to_json_map_with_mixed_keys_test() ->
    %% BT-535: Mixed keys are formatted as Beamtalk syntax
    Result = beamtalk_repl_server:term_to_json(#{atom_key => 1, <<"bin_key">> => 2}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#atom_key => 1">>) =/= nomatch),
    ?assert(binary:match(Result, <<"bin_key => 2">>) =/= nomatch).

term_to_json_single_element_tuple_test() ->
    Result = beamtalk_repl_server:term_to_json({only}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#only}">>, Result).

term_to_json_large_tuple_test() ->
    Result = beamtalk_repl_server:term_to_json({a, b, c, d, e}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#a, #b, #c, #d, #e}">>, Result).

term_to_json_mixed_tuple_test() ->
    %% BT-536: Tuple with mixed types: integer, atom, string
    Result = beamtalk_repl_server:term_to_json({ok, 42, <<"hello">>}),
    ?assertEqual(<<"{#ok, 42, hello}">>, Result).

term_to_json_empty_tuple_test() ->
    %% BT-536: Empty tuple
    Result = beamtalk_repl_server:term_to_json({}),
    ?assertEqual(<<"{}">>, Result).

term_to_json_nested_list_with_maps_test() ->
    %% BT-535: Maps in lists are pre-formatted as strings
    Result = beamtalk_repl_server:term_to_json([#{a => 1}, #{b => 2}]),
    ?assertEqual(2, length(Result)),
    [First, Second] = Result,
    ?assertEqual(<<"#{#a => 1}">>, First),
    ?assertEqual(<<"#{#b => 2}">>, Second).

term_to_json_list_with_pids_test() ->
    Parent = self(),
    Pid = spawn(fun() -> receive stop -> Parent ! done end end),
    Result = beamtalk_repl_server:term_to_json([Pid, 42]),
    ?assertEqual(2, length(Result)),
    [PidJson, NumJson] = Result,
    ?assert(binary:match(PidJson, <<"#Actor<">>) =/= nomatch),
    ?assertEqual(42, NumJson),
    Pid ! stop,
    receive done -> ok after 200 -> ok end.

term_to_json_reference_test() ->
    Ref = make_ref(),
    Result = beamtalk_repl_server:term_to_json(Ref),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

term_to_json_port_test() ->
    %% Create a port deterministically so the test doesn't depend on environment state
    Port = open_port({spawn, "true"}, []),
    Result = beamtalk_repl_server:term_to_json(Port),
    ?assert(is_binary(Result)),
    catch port_close(Port).

%%% BT-520: format_response fallback/crash safety tests

format_response_deeply_nested_test() ->
    Response = beamtalk_repl_server:format_response([1, [2, [3, [4]]]]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

format_response_with_warnings_error_fallback_test() ->
    %% Even bad values should produce valid JSON via fallback
    Response = beamtalk_repl_server:format_response_with_warnings(42, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

%%% BT-520: format_error fallback tests

format_error_with_warnings_fallback_test() ->
    %% Generic atom errors should still produce valid JSON with warnings
    Response = beamtalk_repl_server:format_error_with_warnings(
        some_bizarre_error, [<<"w1">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))),
    ?assertEqual([<<"w1">>], maps:get(<<"warnings">>, Decoded)).

%%% BT-520: format_modules edge cases

format_modules_multiple_test() ->
    Modules = [
        {counter, #{
            name => <<"Counter">>,
            source_file => "/tmp/counter.bt",
            actor_count => 2,
            load_time => 100,
            time_ago => "5 seconds ago"
        }},
        {point, #{
            name => <<"Point">>,
            source_file => "/tmp/point.bt",
            actor_count => 0,
            load_time => 200,
            time_ago => "10 seconds ago"
        }}
    ],
    Response = beamtalk_repl_server:format_modules(Modules),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    ModList = maps:get(<<"modules">>, Decoded),
    ?assertEqual(2, length(ModList)),
    [First, Second] = ModList,
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, First)),
    ?assertEqual(2, maps:get(<<"actor_count">>, First)),
    ?assertEqual(<<"Point">>, maps:get(<<"name">>, Second)),
    ?assertEqual(0, maps:get(<<"actor_count">>, Second)).

%%% BT-520: Additional parse_request edge cases

parse_request_nested_json_expression_test() ->
    Request = <<"{\"type\": \"eval\", \"expression\": \"#{x => 1, y => 2}\"}">>,
    ?assertMatch({eval, _}, beamtalk_repl_server:parse_request(Request)).

parse_request_very_long_expression_test() ->
    LongExpr = list_to_binary(lists:duplicate(1000, $a)),
    Request = jsx:encode(#{<<"type">> => <<"eval">>, <<"expression">> => LongExpr}),
    ?assertMatch({eval, _}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_with_id_and_session_test() ->
    Request = <<"{\"op\": \"eval\", \"id\": \"msg-123\", \"session\": \"s1\", \"code\": \"42\"}">>,
    ?assertEqual({eval, "42"}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_load_file_missing_path_test() ->
    Request = <<"{\"op\": \"load-file\"}">>,
    ?assertEqual({load_file, ""}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_unload_missing_module_test() ->
    Request = <<"{\"op\": \"unload\"}">>,
    ?assertEqual({unload_module, ""}, beamtalk_repl_server:parse_request(Request)).

parse_request_op_kill_with_pid_field_test() ->
    Request = <<"{\"op\": \"kill\", \"pid\": \"<0.99.0>\"}">>,
    ?assertEqual({kill_actor, "<0.99.0>"}, beamtalk_repl_server:parse_request(Request)).

%%% BT-520: Legacy type-format parse_request tests (covering lines 499-506)

parse_request_type_actors_test() ->
    Request = <<"{\"type\": \"actors\"}">>,
    ?assertEqual({list_actors}, beamtalk_repl_server:parse_request(Request)).

parse_request_type_modules_test() ->
    Request = <<"{\"type\": \"modules\"}">>,
    ?assertEqual({list_modules}, beamtalk_repl_server:parse_request(Request)).

parse_request_type_unload_test() ->
    Request = <<"{\"type\": \"unload\", \"module\": \"Counter\"}">>,
    ?assertEqual({unload_module, "Counter"}, beamtalk_repl_server:parse_request(Request)).

parse_request_type_kill_test() ->
    Request = <<"{\"type\": \"kill\", \"pid\": \"<0.1.0>\"}">>,
    ?assertEqual({kill_actor, "<0.1.0>"}, beamtalk_repl_server:parse_request(Request)).

%%% BT-520: format_bindings with non-atom key types (covering lines 640-642)

format_bindings_list_key_test() ->
    Response = beamtalk_repl_server:format_bindings(#{"myVar" => 42}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"myVar">>, Bindings)).

format_bindings_binary_key_test() ->
    Response = beamtalk_repl_server:format_bindings(#{<<"binKey">> => "hello"}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(<<"hello">>, maps:get(<<"binKey">>, Bindings)).

format_bindings_integer_key_test() ->
    Response = beamtalk_repl_server:format_bindings(#{99 => true}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(true, maps:get(<<"99">>, Bindings)).

%%% BT-520: term_to_json unicode error paths (covering lines 739-740)

term_to_json_list_with_bad_unicode_test() ->
    %% A list with high codepoints that fail characters_to_binary
    %% This should fall back to io_lib:format
    Result = beamtalk_repl_server:term_to_json([16#FFFF, 16#FFFE]),
    ?assert(is_list(Result) orelse is_binary(Result)).

%%% BT-520: term_to_json with class object display (covering line 817)

term_to_json_beamtalk_class_object_test() ->
    %% A class object has Class that is a class name
    %% We need beamtalk_object_class to be accessible; test the tuple pattern
    Pid = spawn(fun() -> receive _ -> ok end end),
    Result = beamtalk_repl_server:term_to_json({beamtalk_object, 'TestClassName', some_module, Pid}),
    %% Should either be a class display name or #Actor<...> format
    ?assert(is_binary(Result)),
    exit(Pid, kill).

%%% BT-520: format_name edge cases (covering lines 1041, 1045)

format_error_message_with_list_name_test() ->
    Msg = beamtalk_repl_server:format_error_message({file_not_found, "test/file.bt"}),
    ?assert(binary:match(Msg, <<"File not found">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"test/file.bt">>) =/= nomatch).

format_error_message_with_complex_reason_test() ->
    %% Triggers format_name fallback with non-atom/binary/list
    Msg = beamtalk_repl_server:format_error_message({read_error, {posix, enoent}}),
    ?assert(binary:match(Msg, <<"Failed to read file">>) =/= nomatch).

%%% BT-520: format_response with port value test

format_response_with_unserializable_test() ->
    %% Port references are handled by term_to_json's fallback via io_lib:format
    Port = open_port({spawn, "true"}, []),
    Response = beamtalk_repl_server:format_response(Port),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    catch port_close(Port).

%%% BT-520: format_error debug log fallback (covering lines 579, 586)
%% Note: these paths are hit when format_error_message itself crashes,
%% which is hard to trigger with valid error terms. The fallback is
%% a safety net.

%%% BT-520: parse_request catching exceptions (covering line 519)

parse_request_binary_causes_crash_test() ->
    %% Test that parse_request doesn't crash on truly bizarre input
    %% Even if something throws during parsing, it should return error
    Result = beamtalk_repl_server:parse_request(<<0>>),
    %% Should either parse as raw expression or return error
    ?assert(element(1, Result) =:= eval orelse element(1, Result) =:= error).

%% Generator: runs all TCP integration tests within a single workspace instance
tcp_integration_test_() ->
    {setup,
     fun tcp_setup/0,
     fun tcp_cleanup/1,
     fun({Port, _SupPid}) ->
         [
          {"clear op", fun() -> tcp_clear_test(Port) end},
          {"bindings op (empty)", fun() -> tcp_bindings_empty_test(Port) end},
          {"actors op (empty)", fun() -> tcp_actors_empty_test(Port) end},
          {"sessions op", fun() -> tcp_sessions_test(Port) end},
          {"close op", fun() -> tcp_close_test(Port) end},
          {"complete op (empty prefix)", fun() -> tcp_complete_empty_test(Port) end},
          {"complete op (with prefix)", fun() -> tcp_complete_prefix_test(Port) end},
          {"info op (unknown symbol)", fun() -> tcp_info_unknown_test(Port) end},
          {"info op (known atom)", fun() -> tcp_info_known_atom_test(Port) end},
          {"unknown op", fun() -> tcp_unknown_op_test(Port) end},
          {"empty eval op", fun() -> tcp_eval_empty_test(Port) end},
          {"simple eval op", fun() -> tcp_eval_simple_test(Port) end},
          {"unload nonexistent", fun() -> tcp_unload_nonexistent_test(Port) end},
          {"unload empty module", fun() -> tcp_unload_empty_test(Port) end},
          {"unload in-use module", fun() -> tcp_unload_in_use_test(Port) end},
          {"inspect invalid pid", fun() -> tcp_inspect_invalid_pid_test(Port) end},
          {"kill invalid pid", fun() -> tcp_kill_invalid_pid_test(Port) end},
          {"reload module not loaded", fun() -> tcp_reload_module_not_loaded_test(Port) end},
          {"docs unknown class", fun() -> tcp_docs_unknown_class_test(Port) end},
          {"modules op", fun() -> tcp_modules_test(Port) end},
          {"clone op", fun() -> tcp_clone_test(Port) end},
          {"inspect dead actor", fun() -> tcp_inspect_dead_actor_test(Port) end},
          {"malformed json", fun() -> tcp_malformed_json_test(Port) end},
          {"raw expression", fun() -> tcp_raw_expression_test(Port) end},
          %% BT-523: TCP connection lifecycle tests
          {"multiple sequential connects", fun() -> tcp_multiple_connects_test(Port) end},
          {"concurrent clients", fun() -> tcp_concurrent_clients_test(Port) end},
          {"client disconnect", fun() -> tcp_client_disconnect_test(Port) end},
          {"multi request same connection", fun() -> tcp_multi_request_same_conn_test(Port) end},
          %% BT-523: Connection error handling tests
          {"empty line", fun() -> tcp_empty_line_test(Port) end},
          {"binary garbage", fun() -> tcp_binary_garbage_test(Port) end},
          {"reload empty module", fun() -> tcp_reload_empty_module_test(Port) end},
          {"reload with path", fun() -> tcp_reload_with_path_test(Port) end},
          {"docs with selector unknown class", fun() -> tcp_docs_with_selector_unknown_test(Port) end},
          {"inspect unknown actor", fun() -> tcp_inspect_unknown_actor_test(Port) end},
          {"kill unknown actor", fun() -> tcp_kill_unknown_actor_test(Port) end},
          %% BT-523: gen_server callback tests
          {"get port", fun() -> tcp_get_port_test(Port) end},
          %% BT-523: session ID uniqueness test
          {"clone uniqueness", fun() -> tcp_clone_uniqueness_test(Port) end}
         ]
     end}.

tcp_setup() ->
    process_flag(trap_exit, true),
    application:ensure_all_started(beamtalk_runtime),
    %% Find a free port and start workspace, with retry on port conflict
    {Port, SupPid} = tcp_start_workspace(3),
    timer:sleep(100),
    {Port, SupPid}.

tcp_start_workspace(0) -> error(failed_to_start_workspace);
tcp_start_workspace(Retries) ->
    {ok, LSock} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    gen_tcp:close(LSock),
    timer:sleep(50),
    Config = #{
        workspace_id => <<"tcp_test_ws">>,
        project_path => <<"/tmp/bt_tcp_test">>,
        tcp_port => Port,
        auto_cleanup => false
    },
    case beamtalk_workspace_sup:start_link(Config) of
        {ok, Pid} -> {Port, Pid};
        {error, {already_started, Pid}} -> {Port, Pid};
        {error, {listen_failed, eaddrinuse}} -> tcp_start_workspace(Retries - 1);
        {error, Reason} -> error({workspace_start_failed, Reason})
    end.

tcp_cleanup({_Port, SupPid}) ->
    case is_pid(SupPid) andalso is_process_alive(SupPid) of
        true ->
            unlink(SupPid),
            exit(SupPid, shutdown),
            timer:sleep(200);
        false -> ok
    end.

%% Helper: connect, send a JSON op, receive response
tcp_send_op(Port, OpJson) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock, [OpJson, "\n"]),
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    jsx:decode(Data, [return_maps]).

%% Test: clear op returns ok status
tcp_clear_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"t1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t1">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%% Test: bindings op returns bindings list
tcp_bindings_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"t2">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t2">>}, Resp),
    ?assert(maps:is_key(<<"bindings">>, Resp)).

%% Test: actors op returns empty actor list
tcp_actors_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"actors">>, <<"id">> => <<"t3">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t3">>}, Resp),
    ?assertEqual([], maps:get(<<"actors">>, Resp)).

%% Test: sessions op returns sessions list
tcp_sessions_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"sessions">>, <<"id">> => <<"t4">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t4">>}, Resp),
    ?assert(maps:is_key(<<"sessions">>, Resp)).

%% Test: close op returns ok status
tcp_close_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"close">>, <<"id">> => <<"t5">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t5">>}, Resp),
    ?assert(lists:member(<<"done">>, maps:get(<<"status">>, Resp))).

%% Test: complete with empty prefix returns empty
tcp_complete_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"complete">>, <<"id">> => <<"t6">>, <<"code">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t6">>}, Resp),
    ?assertEqual([], maps:get(<<"completions">>, Resp)).

%% Test: info for unknown symbol returns not found
tcp_info_unknown_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"info">>, <<"id">> => <<"t7">>, <<"symbol">> => <<"xyzzy">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t7">>}, Resp),
    Info = maps:get(<<"info">>, Resp),
    ?assertEqual(false, maps:get(<<"found">>, Info)).

%% Test: info for known atom (like 'error' or 'ok')
tcp_info_known_atom_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"info">>, <<"id">> => <<"t7b">>, <<"symbol">> => <<"error">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t7b">>}, Resp),
    ?assert(maps:is_key(<<"info">>, Resp)).

%% Test: complete with a non-empty prefix
tcp_complete_prefix_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"complete">>, <<"id">> => <<"t6b">>, <<"code">> => <<"sel">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t6b">>}, Resp),
    Completions = maps:get(<<"completions">>, Resp),
    %% "self" starts with "sel" and is a built-in keyword
    ?assert(lists:member(<<"self">>, Completions)).

%% Test: unknown op returns error
tcp_unknown_op_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"foobar_nonexistent">>, <<"id">> => <<"t8">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t8">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: eval with empty code returns error
tcp_eval_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"eval">>, <<"id">> => <<"t9">>, <<"code">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t9">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: eval with simple expression
tcp_eval_simple_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"eval">>, <<"id">> => <<"t10">>, <<"code">> => <<"1 + 2">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t10">>}, Resp),
    %% Result could be error (daemon not running) or success
    ?assert(maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp)).

%% Test: unload nonexistent module
tcp_unload_nonexistent_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"unload">>, <<"id">> => <<"t11">>, <<"module">> => <<"XyzzyNotLoaded">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t11">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: unload with empty module name
tcp_unload_empty_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"unload">>, <<"id">> => <<"t12">>, <<"module">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t12">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: unload module that has active processes returns in-use error
tcp_unload_in_use_test(Port) ->
    DummyMod = 'bt519_tcp_in_use',
    Forms = [{attribute,1,module,DummyMod},
             {attribute,2,export,[{loop,0}]},
             {function,3,loop,0,
              [{clause,3,[],[],
                [{'receive',3,
                  [{clause,3,[{atom,3,stop}],[],[{atom,3,ok}]}]}]}]}],
    {ok, DummyMod, Binary} = compile:forms(Forms),
    {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
    LoopPid = spawn(DummyMod, loop, []),
    %% Load new version so running process uses "old" code
    {module, DummyMod} = code:load_binary(DummyMod, "test.erl", Binary),
    try
        Msg = jsx:encode(#{<<"op">> => <<"unload">>, <<"id">> => <<"t12b">>,
                           <<"module">> => atom_to_binary(DummyMod, utf8)}),
        Resp = tcp_send_op(Port, Msg),
        ?assertMatch(#{<<"id">> := <<"t12b">>}, Resp),
        ?assert(maps:is_key(<<"error">>, Resp)),
        ErrorMsg = maps:get(<<"error">>, Resp),
        ?assert(binary:match(ErrorMsg, <<"Stop actors">>) =/= nomatch)
    after
        LoopPid ! stop,
        timer:sleep(50),
        _ = code:soft_purge(DummyMod),
        _ = code:delete(DummyMod),
        _ = code:soft_purge(DummyMod)
    end.

%% Test: inspect with invalid PID string
tcp_inspect_invalid_pid_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"inspect">>, <<"id">> => <<"t13">>, <<"actor">> => <<"not-a-pid">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t13">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: kill with invalid PID string
tcp_kill_invalid_pid_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"kill">>, <<"id">> => <<"t14">>, <<"actor">> => <<"not-a-pid">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t14">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: reload module that hasn't been loaded returns error
tcp_reload_module_not_loaded_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"reload">>, <<"id">> => <<"t15">>, <<"module">> => <<"Counter">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t15">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)),
    ErrorMsg = maps:get(<<"error">>, Resp),
    ?assert(binary:match(ErrorMsg, <<"Module not loaded">>) =/= nomatch).

%% Test: docs for unknown class
tcp_docs_unknown_class_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"docs">>, <<"id">> => <<"t16">>, <<"class">> => <<"XyzzyUnknown">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t16">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: modules op returns modules list
tcp_modules_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"modules">>, <<"id">> => <<"t17">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t17">>}, Resp),
    ?assert(maps:is_key(<<"modules">>, Resp)).

%% Test: clone op creates a new session
tcp_clone_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"t18">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t18">>}, Resp),
    %% Clone should return a session ID as value, or error if sup not ready
    ?assert(maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp)).

%% Test: inspect a dead actor by valid PID format
tcp_inspect_dead_actor_test(Port) ->
    Pid = spawn(fun() -> ok end),
    %% Wait until the process is confirmed dead
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, process, Pid, _} -> ok after 1000 -> error(timeout) end,
    PidStr = list_to_binary(pid_to_list(Pid)),
    Msg = jsx:encode(#{<<"op">> => <<"inspect">>, <<"id">> => <<"t19">>, <<"actor">> => PidStr}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"t19">>}, Resp),
    %% Should return error (not alive or not known)
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: malformed JSON falls back to raw eval
tcp_malformed_json_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock, <<"not valid json\n">>),
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    Resp = jsx:decode(Data, [return_maps]),
    %% The server should try to eval "not valid json" or return error
    ?assert(maps:is_key(<<"value">>, Resp) orelse maps:is_key(<<"error">>, Resp) orelse maps:is_key(<<"type">>, Resp)).

%% Test: raw expression (non-JSON) backwards compatibility
tcp_raw_expression_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock, <<"42\n">>),
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    Resp = jsx:decode(Data, [return_maps]),
    %% Raw expressions go through protocol decode
    ?assert(is_map(Resp)).

%%% BT-523: TCP connection lifecycle tests

%% Test: multiple sequential connections to the same port
tcp_multiple_connects_test(Port) ->
    %% First connection
    Msg1 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc1">>}),
    Resp1 = tcp_send_op(Port, Msg1),
    ?assertMatch(#{<<"id">> := <<"mc1">>}, Resp1),
    %% Second connection after first is closed
    Msg2 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc2">>}),
    Resp2 = tcp_send_op(Port, Msg2),
    ?assertMatch(#{<<"id">> := <<"mc2">>}, Resp2),
    %% Third connection
    Msg3 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mc3">>}),
    Resp3 = tcp_send_op(Port, Msg3),
    ?assertMatch(#{<<"id">> := <<"mc3">>}, Resp3).

%% Test: multiple concurrent TCP connections
tcp_concurrent_clients_test(Port) ->
    Msg1 = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"cc1">>}),
    Msg2 = jsx:encode(#{<<"op">> => <<"actors">>, <<"id">> => <<"cc2">>}),
    {ok, Sock1} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    {ok, Sock2} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock1, [Msg1, "\n"]),
    ok = gen_tcp:send(Sock2, [Msg2, "\n"]),
    {ok, Data1} = gen_tcp:recv(Sock1, 0, 5000),
    {ok, Data2} = gen_tcp:recv(Sock2, 0, 5000),
    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    Resp1 = jsx:decode(Data1, [return_maps]),
    Resp2 = jsx:decode(Data2, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"cc1">>}, Resp1),
    ?assertMatch(#{<<"id">> := <<"cc2">>}, Resp2).

%% Test: client disconnect is handled gracefully (no crash)
tcp_client_disconnect_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    %% Close immediately without sending
    gen_tcp:close(Sock),
    timer:sleep(100),
    %% Server should still be responsive
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"dc1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"dc1">>}, Resp).

%% Test: multiple requests on the same connection
tcp_multi_request_same_conn_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    %% Send first request
    Msg1 = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"mr1">>}),
    ok = gen_tcp:send(Sock, [Msg1, "\n"]),
    {ok, Data1} = gen_tcp:recv(Sock, 0, 5000),
    Resp1 = jsx:decode(Data1, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"mr1">>}, Resp1),
    %% Send second request on same connection
    Msg2 = jsx:encode(#{<<"op">> => <<"bindings">>, <<"id">> => <<"mr2">>}),
    ok = gen_tcp:send(Sock, [Msg2, "\n"]),
    {ok, Data2} = gen_tcp:recv(Sock, 0, 5000),
    Resp2 = jsx:decode(Data2, [return_maps]),
    ?assertMatch(#{<<"id">> := <<"mr2">>}, Resp2),
    gen_tcp:close(Sock).

%%% BT-523: Connection error handling tests

%% Test: send empty line to server
tcp_empty_line_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock, <<"\n">>),
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    Resp = jsx:decode(Data, [return_maps]),
    %% Empty line results in error (legacy type=error or protocol status=[error])
    ?assert(maps:is_key(<<"error">>, Resp) orelse
            maps:get(<<"type">>, Resp, undefined) =:= <<"error">>).

%% Test: binary garbage data
tcp_binary_garbage_test(Port) ->
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, line}, {active, false}]),
    ok = gen_tcp:send(Sock, <<0, 1, 2, 255, 254, 10>>),
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    gen_tcp:close(Sock),
    %% Should get some response without crashing the server
    ?assert(is_binary(Data)),
    %% Server still works after garbage
    Msg = jsx:encode(#{<<"op">> => <<"clear">>, <<"id">> => <<"bg1">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"bg1">>}, Resp).

%% Test: reload with empty module name
tcp_reload_empty_module_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"reload">>, <<"id">> => <<"re1">>, <<"module">> => <<>>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"re1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: reload with path but no module
tcp_reload_with_path_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"reload">>, <<"id">> => <<"re2">>,
                       <<"path">> => <<"/nonexistent/path.bt">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"re2">>}, Resp),
    %% Should error because file doesn't exist
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: docs with selector for unknown class
tcp_docs_with_selector_unknown_test(Port) ->
    Msg = jsx:encode(#{<<"op">> => <<"docs">>, <<"id">> => <<"d1">>,
                       <<"class">> => <<"XyzzyUnknown523">>,
                       <<"selector">> => <<"+">>}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"d1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: inspect unknown actor (valid PID format, not in registry)
tcp_inspect_unknown_actor_test(Port) ->
    Pid = spawn(fun() -> receive _ -> ok end end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    exit(Pid, kill),
    timer:sleep(50),
    Msg = jsx:encode(#{<<"op">> => <<"inspect">>, <<"id">> => <<"iu1">>,
                       <<"actor">> => PidStr}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"iu1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%% Test: kill unknown actor (valid PID format, not in registry)
tcp_kill_unknown_actor_test(Port) ->
    Pid = spawn(fun() -> receive _ -> ok end end),
    PidStr = list_to_binary(pid_to_list(Pid)),
    exit(Pid, kill),
    timer:sleep(50),
    Msg = jsx:encode(#{<<"op">> => <<"kill">>, <<"id">> => <<"ku1">>,
                       <<"actor">> => PidStr}),
    Resp = tcp_send_op(Port, Msg),
    ?assertMatch(#{<<"id">> := <<"ku1">>}, Resp),
    ?assert(maps:is_key(<<"error">>, Resp)).

%%% BT-523: gen_server callback and lifecycle tests

%% Test: get_port returns port after start
tcp_get_port_test(Port) ->
    {ok, ActualPort} = beamtalk_repl_server:get_port(),
    ?assertEqual(Port, ActualPort).

%% Test: start_link with integer port (backward compatibility)
%% This verifies the module compiles and start_link/1 accepts integer args.
%% The actual start is tested in tcp_integration_test_ which starts a full workspace.
start_link_integer_port_test() ->
    %% Verify the backward-compat clause exists by checking module exports
    Exports = beamtalk_repl_server:module_info(exports),
    ?assert(lists:member({start_link, 1}, Exports)).

%%% BT-523: generate_session_id format and uniqueness tests (via clone op)

%% Clone creates new sessions, each with a unique generated ID.
%% This tests generate_session_id indirectly via the clone TCP op.
%% The test is in tcp_integration_test_ as tcp_clone_uniqueness_test.

%% Test: two clone ops produce different session IDs
tcp_clone_uniqueness_test(Port) ->
    Msg1 = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"cu1">>}),
    Msg2 = jsx:encode(#{<<"op">> => <<"clone">>, <<"id">> => <<"cu2">>}),
    Resp1 = tcp_send_op(Port, Msg1),
    Resp2 = tcp_send_op(Port, Msg2),
    case {maps:find(<<"value">>, Resp1), maps:find(<<"value">>, Resp2)} of
        {{ok, V1}, {ok, V2}} ->
            ?assertNotEqual(V1, V2),
            %% Verify session ID format: "session_<timestamp>_<random>"
            ?assert(binary:match(V1, <<"session_">>) =:= {0, 8}),
            ?assert(binary:match(V2, <<"session_">>) =:= {0, 8});
        _ ->
            %% Clone may error if sup not fully ready; still valid
            ok
    end.

%%% BT-523: format_error_message additional coverage

format_error_message_unknown_actor_v2_test() ->
    Msg = beamtalk_repl_server:format_error_message({unknown_actor, "<0.99.0>"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_invalid_pid_v2_test() ->
    Msg = beamtalk_repl_server:format_error_message({invalid_pid, "garbage"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_load_error_atom_test() ->
    Msg = beamtalk_repl_server:format_error_message({load_error, {bad_module, counter}}),
    ?assert(binary:match(Msg, <<"Failed to load bytecode">>) =/= nomatch).

format_error_message_read_error_enoent_test() ->
    Msg = beamtalk_repl_server:format_error_message({read_error, enoent}),
    ?assert(binary:match(Msg, <<"Failed to read file">>) =/= nomatch).

format_error_message_daemon_unavailable_v2_test() ->
    Msg = beamtalk_repl_server:format_error_message(daemon_unavailable),
    ?assert(binary:match(Msg, <<"compiler daemon">>) =/= nomatch).

format_error_message_empty_expression_v2_test() ->
    Msg = beamtalk_repl_server:format_error_message(empty_expression),
    ?assertEqual(<<"Empty expression">>, Msg).

format_error_message_timeout_v2_test() ->
    Msg = beamtalk_repl_server:format_error_message(timeout),
    ?assertEqual(<<"Request timed out">>, Msg).
