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

term_to_json_printable_list_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_server:term_to_json("hello")).

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
    Result = beamtalk_repl_server:term_to_json(#{name => <<"test">>, count => 5}),
    ?assertEqual(<<"test">>, maps:get(<<"name">>, Result)),
    ?assertEqual(5, maps:get(<<"count">>, Result)).

term_to_json_map_with_binary_keys_test() ->
    Result = beamtalk_repl_server:term_to_json(#{<<"key">> => <<"val">>}),
    ?assertEqual(<<"val">>, maps:get(<<"key">>, Result)).

term_to_json_map_with_list_keys_test() ->
    Result = beamtalk_repl_server:term_to_json(#{"stringkey" => 42}),
    ?assertEqual(42, maps:get(<<"stringkey">>, Result)).

term_to_json_map_with_non_printable_list_key_test() ->
    Result = beamtalk_repl_server:term_to_json(#{[1,2,3] => <<"val">>}),
    ?assert(is_map(Result)).

term_to_json_map_with_integer_key_test() ->
    Result = beamtalk_repl_server:term_to_json(#{99 => <<"val">>}),
    ?assert(is_map(Result)).

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
    ?assert(is_map(Result)),
    ?assert(maps:is_key(<<"__tuple__">>, Result)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], maps:get(<<"__tuple__">>, Result)).

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

format_error_message_not_implemented_reload_test() ->
    Msg = beamtalk_repl_server:format_error_message({not_implemented, {reload, "counter"}}),
    ?assert(binary:match(Msg, <<"Reload not yet implemented">>) =/= nomatch).

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
    ?assertEqual(#{}, maps:get(<<"value">>, Decoded)).

format_response_nested_map_test() ->
    Response = beamtalk_repl_server:format_response(#{a => #{b => 1}}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    Inner = maps:get(<<"a">>, Value),
    ?assertEqual(1, maps:get(<<"b">>, Inner)).

format_response_true_false_test() ->
    ResponseT = beamtalk_repl_server:format_response(true),
    DecodedT = jsx:decode(ResponseT, [return_maps]),
    ?assertEqual(true, maps:get(<<"value">>, DecodedT)),
    ResponseF = beamtalk_repl_server:format_response(false),
    DecodedF = jsx:decode(ResponseF, [return_maps]),
    ?assertEqual(false, maps:get(<<"value">>, DecodedF)).
