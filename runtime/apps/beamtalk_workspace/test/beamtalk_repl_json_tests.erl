%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** REPL

%%% @doc EUnit tests for beamtalk_repl_json (BT-708).
%%%
%%% Tests JSON formatting for REPL protocol responses: format_response,
%%% format_error, format_bindings, format_actors, format_modules,
%%% format_docs, term_to_json, format_error_message, parse_json.

-module(beamtalk_repl_json_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% parse_json tests
%%% ============================================================================

parse_json_valid_test() ->
    {ok, Map} = beamtalk_repl_json:parse_json(<<"{\"key\":\"value\"}">>),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Map)).

parse_json_invalid_test() ->
    ?assertEqual({error, not_json}, beamtalk_repl_json:parse_json(<<"not json">>)).

parse_json_empty_object_test() ->
    {ok, Map} = beamtalk_repl_json:parse_json(<<"{}">>),
    ?assertEqual(#{}, Map).

%%% ============================================================================
%%% format_response tests
%%% ============================================================================

format_response_integer_test() ->
    Result = beamtalk_repl_json:format_response(42),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)).

format_response_binary_test() ->
    Result = beamtalk_repl_json:format_response(<<"hello">>),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"hello">>, maps:get(<<"value">>, Decoded)).

format_response_atom_test() ->
    Result = beamtalk_repl_json:format_response(nil),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"nil">>, maps:get(<<"value">>, Decoded)).

format_response_boolean_test() ->
    Result = beamtalk_repl_json:format_response(true),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(true, maps:get(<<"value">>, Decoded)).

format_response_float_test() ->
    Result = beamtalk_repl_json:format_response(3.14),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(3.14, maps:get(<<"value">>, Decoded)).

format_response_list_test() ->
    Result = beamtalk_repl_json:format_response([1, 2, 3]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([1, 2, 3], maps:get(<<"value">>, Decoded)).

format_response_empty_list_test() ->
    Result = beamtalk_repl_json:format_response([]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([], maps:get(<<"value">>, Decoded)).

%%% ============================================================================
%%% format_error tests
%%% ============================================================================

format_error_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Result = beamtalk_repl_json:format_error(Error),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))).

format_error_empty_expression_test() ->
    Result = beamtalk_repl_json:format_error(empty_expression),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"Empty expression">>, maps:get(<<"message">>, Decoded)).

format_error_timeout_test() ->
    Result = beamtalk_repl_json:format_error(timeout),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)).

format_error_compile_error_binary_test() ->
    Result = beamtalk_repl_json:format_error({compile_error, <<"syntax error">>}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"syntax error">>, maps:get(<<"message">>, Decoded)).

format_error_compile_error_list_test() ->
    Result = beamtalk_repl_json:format_error({compile_error, "syntax error"}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"syntax error">>, maps:get(<<"message">>, Decoded)).

format_error_undefined_variable_test() ->
    Result = beamtalk_repl_json:format_error({undefined_variable, <<"x">>}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertMatch(<<"Undefined variable: ", _/binary>>, maps:get(<<"message">>, Decoded)).

format_error_file_not_found_test() ->
    Result = beamtalk_repl_json:format_error({file_not_found, <<"/foo/bar.bt">>}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertMatch(<<"File not found: ", _/binary>>, maps:get(<<"message">>, Decoded)).

format_error_unknown_op_test() ->
    Result = beamtalk_repl_json:format_error({unknown_op, <<"badop">>}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertMatch(<<"Unknown operation: ", _/binary>>, maps:get(<<"message">>, Decoded)).

format_error_fallback_test() ->
    Result = beamtalk_repl_json:format_error(some_unknown_thing),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))).

%%% ============================================================================
%%% format_response_with_warnings tests
%%% ============================================================================

format_response_with_no_warnings_test() ->
    Result = beamtalk_repl_json:format_response_with_warnings(42, []),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_response_with_warnings_test() ->
    Result = beamtalk_repl_json:format_response_with_warnings(42, [<<"warn1">>]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"warn1">>], maps:get(<<"warnings">>, Decoded)).

%%% ============================================================================
%%% format_error_with_warnings tests
%%% ============================================================================

format_error_with_no_warnings_test() ->
    Result = beamtalk_repl_json:format_error_with_warnings(timeout, []),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_error_with_warnings_test() ->
    Result = beamtalk_repl_json:format_error_with_warnings(timeout, [<<"w1">>]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual([<<"w1">>], maps:get(<<"warnings">>, Decoded)).

%%% ============================================================================
%%% format_bindings tests
%%% ============================================================================

format_bindings_empty_test() ->
    Result = beamtalk_repl_json:format_bindings(#{}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"bindings">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(#{}, maps:get(<<"bindings">>, Decoded)).

format_bindings_atom_key_test() ->
    Result = beamtalk_repl_json:format_bindings(#{x => 42}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"x">>, Bindings)).

format_bindings_binary_key_test() ->
    Result = beamtalk_repl_json:format_bindings(#{<<"y">> => <<"hello">>}),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(<<"hello">>, maps:get(<<"y">>, Bindings)).

%%% ============================================================================
%%% format_docs tests
%%% ============================================================================

format_docs_test() ->
    Result = beamtalk_repl_json:format_docs(<<"Integer supports + - * /">>),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Integer supports + - * /">>, maps:get(<<"docs">>, Decoded)).

%%% ============================================================================
%%% format_loaded tests
%%% ============================================================================

format_loaded_test() ->
    Classes = [
        #{name => "Counter", superclass => "Actor"},
        #{name => "Point", superclass => "Object"}
    ],
    Result = beamtalk_repl_json:format_loaded(Classes),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"loaded">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([<<"Counter">>, <<"Point">>], maps:get(<<"classes">>, Decoded)).

format_loaded_empty_test() ->
    Result = beamtalk_repl_json:format_loaded([]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([], maps:get(<<"classes">>, Decoded)).

%%% ============================================================================
%%% format_actors tests
%%% ============================================================================

format_actors_test() ->
    Pid = self(),
    Actors = [#{pid => Pid, class => 'Counter', module => counter, spawned_at => 1000}],
    Result = beamtalk_repl_json:format_actors(Actors),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"actors">>, maps:get(<<"type">>, Decoded)),
    [Actor] = maps:get(<<"actors">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Actor)),
    ?assertEqual(<<"counter">>, maps:get(<<"module">>, Actor)),
    ?assertEqual(1000, maps:get(<<"spawned_at">>, Actor)).

format_actors_empty_test() ->
    Result = beamtalk_repl_json:format_actors([]),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([], maps:get(<<"actors">>, Decoded)).

%%% ============================================================================
%%% format_modules tests
%%% ============================================================================

format_modules_test() ->
    Modules = [
        {counter, #{
            name => <<"Counter">>,
            source_file => "/tmp/counter.bt",
            actor_count => 2,
            load_time => 1000,
            time_ago => "5 seconds ago"
        }}
    ],
    Result = beamtalk_repl_json:format_modules(Modules),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    [Mod] = maps:get(<<"modules">>, Decoded),
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, Mod)),
    ?assertEqual(<<"/tmp/counter.bt">>, maps:get(<<"source_file">>, Mod)),
    ?assertEqual(2, maps:get(<<"actor_count">>, Mod)).

%%% ============================================================================
%%% term_to_json tests
%%% ============================================================================

term_to_json_integer_test() ->
    ?assertEqual(42, beamtalk_repl_json:term_to_json(42)).

term_to_json_float_test() ->
    ?assertEqual(3.14, beamtalk_repl_json:term_to_json(3.14)).

term_to_json_boolean_test() ->
    ?assertEqual(true, beamtalk_repl_json:term_to_json(true)),
    ?assertEqual(false, beamtalk_repl_json:term_to_json(false)).

term_to_json_atom_test() ->
    ?assertEqual(<<"nil">>, beamtalk_repl_json:term_to_json(nil)),
    ?assertEqual(<<"ok">>, beamtalk_repl_json:term_to_json(ok)).

term_to_json_binary_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_json:term_to_json(<<"hello">>)).

term_to_json_empty_list_test() ->
    ?assertEqual([], beamtalk_repl_json:term_to_json([])).

term_to_json_number_list_test() ->
    ?assertEqual([1, 2, 3], beamtalk_repl_json:term_to_json([1, 2, 3])).

term_to_json_string_test() ->
    %% Printable lists are converted to binaries
    Result = beamtalk_repl_json:term_to_json("hello"),
    ?assertEqual(<<"hello">>, Result).

term_to_json_pid_alive_test() ->
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json(Pid),
    ?assert(is_binary(Result)),
    Pid ! stop.

term_to_json_pid_dead_test() ->
    Pid = spawn(fun() -> ok end),
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end,
    Result = beamtalk_repl_json:term_to_json(Pid),
    ?assertMatch(<<"#Dead<", _/binary>>, Result).

term_to_json_function_test() ->
    F = fun(X) -> X end,
    Result = beamtalk_repl_json:term_to_json(F),
    ?assertEqual(<<"a Block/1">>, Result).

term_to_json_function_arity0_test() ->
    F = fun() -> ok end,
    Result = beamtalk_repl_json:term_to_json(F),
    ?assertEqual(<<"a Block/0">>, Result).

term_to_json_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Result = beamtalk_repl_json:term_to_json(Error),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% format_error_message tests
%%% ============================================================================

format_error_message_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Result = beamtalk_repl_json:format_error_message(Error),
    ?assert(is_binary(Result)).

format_error_message_runtime_error_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    Result = beamtalk_repl_json:format_error_message(Wrapped),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"RuntimeError: ", _/binary>>, Result).

format_error_message_empty_expression_test() ->
    ?assertEqual(<<"Empty expression">>, beamtalk_repl_json:format_error_message(empty_expression)).

format_error_message_timeout_test() ->
    ?assertEqual(<<"Request timed out">>, beamtalk_repl_json:format_error_message(timeout)).

format_error_message_undefined_variable_test() ->
    Result = beamtalk_repl_json:format_error_message({undefined_variable, x}),
    ?assertEqual(<<"Undefined variable: x">>, Result).

format_error_message_invalid_request_test() ->
    Result = beamtalk_repl_json:format_error_message({invalid_request, <<"bad">>}),
    ?assertEqual(<<"Invalid request: bad">>, Result).

format_error_message_parse_error_test() ->
    Result = beamtalk_repl_json:format_error_message({parse_error, <<"unexpected token">>}),
    ?assertEqual(<<"Parse error: unexpected token">>, Result).

format_error_message_eval_error_test() ->
    Result = beamtalk_repl_json:format_error_message({eval_error, error, badarg}),
    ?assertMatch(<<"Evaluation error: ", _/binary>>, Result).

format_error_message_eval_error_with_beamtalk_error_test() ->
    Error = beamtalk_error:new(type_error, 'Integer'),
    Result = beamtalk_repl_json:format_error_message({eval_error, error, Error}),
    ?assert(is_binary(Result)).

format_error_message_eval_error_with_runtime_error_test() ->
    Error = beamtalk_error:new(type_error, 'Integer'),
    Wrapped = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    Result = beamtalk_repl_json:format_error_message({eval_error, error, Wrapped}),
    ?assertMatch(<<"RuntimeError: ", _/binary>>, Result).

format_error_message_load_error_test() ->
    Result = beamtalk_repl_json:format_error_message({load_error, badfile}),
    ?assertMatch(<<"Failed to load bytecode: ", _/binary>>, Result).

format_error_message_read_error_test() ->
    Result = beamtalk_repl_json:format_error_message({read_error, enoent}),
    ?assertMatch(<<"Failed to read file: ", _/binary>>, Result).

format_error_message_module_not_found_test() ->
    Result = beamtalk_repl_json:format_error_message({module_not_found, <<"counter">>}),
    ?assertEqual(<<"Module not loaded: counter">>, Result).

format_error_message_actors_exist_singular_test() ->
    Result = beamtalk_repl_json:format_error_message({actors_exist, counter, 1}),
    ?assert(binary:match(Result, <<"1 actor">>) =/= nomatch).

format_error_message_actors_exist_plural_test() ->
    Result = beamtalk_repl_json:format_error_message({actors_exist, counter, 3}),
    ?assert(binary:match(Result, <<"3 actors">>) =/= nomatch).

format_error_message_class_not_found_test() ->
    Result = beamtalk_repl_json:format_error_message({class_not_found, 'Foo'}),
    ?assertMatch(<<"Unknown class: ", _/binary>>, Result).

format_error_message_method_not_found_test() ->
    Result = beamtalk_repl_json:format_error_message(
        {method_not_found, 'Counter', <<"increment">>}
    ),
    ?assert(binary:match(Result, <<"does not understand">>) =/= nomatch).

format_error_message_session_creation_failed_test() ->
    Result = beamtalk_repl_json:format_error_message({session_creation_failed, timeout}),
    ?assertMatch(<<"Failed to create session: ", _/binary>>, Result).

format_error_message_missing_module_name_test() ->
    Result = beamtalk_repl_json:format_error_message({missing_module_name, reload}),
    ?assert(binary:match(Result, <<":reload">>) =/= nomatch).

format_error_message_inspect_failed_test() ->
    Result = beamtalk_repl_json:format_error_message({inspect_failed, "<0.100.0>"}),
    ?assertMatch(<<"Failed to inspect actor: ", _/binary>>, Result).

format_error_message_actor_not_alive_test() ->
    Result = beamtalk_repl_json:format_error_message({actor_not_alive, "<0.100.0>"}),
    ?assertMatch(<<"Actor is not alive: ", _/binary>>, Result).

format_error_message_no_source_file_test() ->
    Result = beamtalk_repl_json:format_error_message({no_source_file, "counter"}),
    ?assertMatch(<<"No source file recorded for module: ", _/binary>>, Result).

format_error_message_module_not_loaded_test() ->
    Result = beamtalk_repl_json:format_error_message({module_not_loaded, <<"counter">>}),
    ?assertMatch(<<"Module not loaded: ", _/binary>>, Result).

format_error_message_invalid_module_name_test() ->
    Result = beamtalk_repl_json:format_error_message({invalid_module_name, <<"123bad">>}),
    ?assertMatch(<<"Invalid module name: ", _/binary>>, Result).

%%% ============================================================================
%%% encode_reloaded tests
%%% ============================================================================

encode_reloaded_test() ->
    Classes = [#{name => "Counter"}],
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\":\"eval\",\"id\":\"msg1\",\"session\":\"sess1\"}">>
    ),
    Result = beamtalk_repl_json:encode_reloaded(Classes, 5, [], Msg),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([<<"Counter">>], maps:get(<<"classes">>, Decoded)),
    ?assertEqual(5, maps:get(<<"affected_actors">>, Decoded)),
    ?assertEqual(0, maps:get(<<"migration_failures">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ?assertEqual(<<"msg1">>, maps:get(<<"id">>, Decoded)).

encode_reloaded_with_failures_test() ->
    Classes = [#{name => "Counter"}, #{name => "Timer"}],
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\":\"reload\",\"id\":\"msg2\"}">>
    ),
    Failures = [{self(), some_error}],
    Result = beamtalk_repl_json:encode_reloaded(Classes, 3, Failures, Msg),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual([<<"Counter">>, <<"Timer">>], maps:get(<<"classes">>, Decoded)),
    ?assertEqual(3, maps:get(<<"affected_actors">>, Decoded)),
    ?assertEqual(1, maps:get(<<"migration_failures">>, Decoded)).

%%% ============================================================================
%%% Additional tests migrated from beamtalk_repl_server_tests (BT-831)
%%% ============================================================================

%%% format_response additional tests

format_response_string_test() ->
    Response = beamtalk_repl_json:format_response("hello"),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"hello">>, maps:get(<<"value">>, Decoded)).

format_response_map_test() ->
    Response = beamtalk_repl_json:format_response(#{x => 1, y => 2}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-535: Maps are now pre-formatted as Beamtalk syntax strings
    ?assert(is_binary(Value)),
    ?assert(binary:match(Value, <<"#x => 1">>) =/= nomatch),
    ?assert(binary:match(Value, <<"#y => 2">>) =/= nomatch).

format_response_pid_test() ->
    Pid = self(),
    Response = beamtalk_repl_json:format_response(Pid),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should be formatted as #Actor<...>
    ?assert(binary:match(Value, <<"#Actor<">>) =/= nomatch).

format_response_function_test() ->
    Fun = fun(X) -> X + 1 end,
    Response = beamtalk_repl_json:format_response(Fun),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should be formatted as "a Block/N"
    ?assert(binary:match(Value, <<"a Block/">>) =/= nomatch).

format_response_tuple_test() ->
    Response = beamtalk_repl_json:format_response({ok, value}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-536: Tuples are formatted as {el1, el2, ...} with symbol notation
    ?assertEqual(<<"{#ok, #value}">>, Value).

format_response_nil_test() ->
    Response = beamtalk_repl_json:format_response(nil),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"nil">>, maps:get(<<"value">>, Decoded)).

format_response_large_number_test() ->
    Response = beamtalk_repl_json:format_response(999999999999999),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(999999999999999, maps:get(<<"value">>, Decoded)).

format_response_complex_pid_test() ->
    %% Test that PID formatting includes #Actor< prefix
    Parent = self(),
    Pid = spawn(fun() ->
        receive
            stop -> Parent ! stopped
        after 100 -> ok
        end
    end),
    Response = beamtalk_repl_json:format_response(Pid),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"#Actor<">>) =/= nomatch),
    ?assert(binary:match(Value, <<">">>) =/= nomatch),
    %% Clean up spawned process
    Pid ! stop,
    receive
        stopped -> ok
    after 200 -> ok
    end.

format_response_multi_arity_function_test() ->
    %% Test function with different arity
    Fun0 = fun() -> ok end,
    Fun3 = fun(A, B, C) -> {A, B, C} end,

    Response0 = beamtalk_repl_json:format_response(Fun0),
    Response3 = beamtalk_repl_json:format_response(Fun3),

    Decoded0 = jsx:decode(Response0, [return_maps]),
    Decoded3 = jsx:decode(Response3, [return_maps]),

    Value0 = maps:get(<<"value">>, Decoded0),
    Value3 = maps:get(<<"value">>, Decoded3),

    ?assert(binary:match(Value0, <<"a Block/0">>) =/= nomatch),
    ?assert(binary:match(Value3, <<"a Block/3">>) =/= nomatch).

%%% format_error additional tests

format_error_compile_error_test() ->
    Response = beamtalk_repl_json:format_error({compile_error, <<"Syntax error">>}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"Syntax error">>, maps:get(<<"message">>, Decoded)).

format_error_eval_error_test() ->
    Response = beamtalk_repl_json:format_error({eval_error, error, badarg}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Evaluation error">>) =/= nomatch).

format_error_load_error_test() ->
    Response = beamtalk_repl_json:format_error({load_error, badfile}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to load bytecode">>) =/= nomatch).

format_error_read_error_test() ->
    Response = beamtalk_repl_json:format_error({read_error, eacces}),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to read file">>) =/= nomatch).

format_error_generic_tuple_test() ->
    Response = beamtalk_repl_json:format_error({unknown_error_type, "details"}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(is_binary(Message)).

format_error_generic_atom_test() ->
    Response = beamtalk_repl_json:format_error(some_unknown_error),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    %% Generic errors are formatted with ~p, so should contain the atom name
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0).

%% BT-238: format_error with #beamtalk_error{} record (with selector)
format_error_beamtalk_error_with_selector_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ErrorWithSelector = beamtalk_error:with_selector(Error, super),
    Response = beamtalk_repl_json:format_error(ErrorWithSelector),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Counter">>) =/= nomatch),
    ?assert(binary:match(Message, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Message, <<"'super'">>) =/= nomatch).

format_error_beamtalk_error_with_hint_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Integer'),
    Error1 = beamtalk_error:with_selector(Error0, foo),
    Error = beamtalk_error:with_hint(Error1, <<"Check spelling">>),
    Response = beamtalk_repl_json:format_error(Error),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Integer">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Hint:">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Check spelling">>) =/= nomatch).

%%% format_response with beamtalk_error tests

format_response_beamtalk_error_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'String'),
    Error = beamtalk_error:with_selector(Error0, typo),
    Response = beamtalk_repl_json:format_response(Error),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"String">>) =/= nomatch),
    ?assert(binary:match(Value, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Value, <<"'typo'">>) =/= nomatch).

format_response_future_rejected_beamtalk_error_test() ->
    Error0 = beamtalk_error:new(does_not_understand, 'Actor'),
    Error = beamtalk_error:with_selector(Error0, badMethod),
    Response = beamtalk_repl_json:format_response({future_rejected, Error}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    ?assert(binary:match(Value, <<"#Future<rejected:">>) =/= nomatch),
    ?assert(binary:match(Value, <<"Actor">>) =/= nomatch),
    ?assert(binary:match(Value, <<"does not understand">>) =/= nomatch),
    ?assert(binary:match(Value, <<"'badMethod'">>) =/= nomatch).

format_rejection_reason_fallback_test() ->
    %% Test that the fallback can handle arbitrary terms without crashing
    Result = beamtalk_repl_json:format_response({future_rejected, {custom_error, some, data}}),
    Decoded = jsx:decode(Result, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% Should format the tuple with ~p
    ?assert(binary:match(Value, <<"#Future<rejected:">>) =/= nomatch),
    ?assert(is_binary(Value)).

%%% format_bindings additional tests

format_bindings_single_test() ->
    Response = beamtalk_repl_json:format_bindings(#{x => 42}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"x">>, Bindings)).

format_bindings_multiple_test() ->
    Response = beamtalk_repl_json:format_bindings(#{x => 1, y => 2, z => 3}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(1, maps:get(<<"x">>, Bindings)),
    ?assertEqual(2, maps:get(<<"y">>, Bindings)),
    ?assertEqual(3, maps:get(<<"z">>, Bindings)).

format_bindings_complex_values_test() ->
    Response = beamtalk_repl_json:format_bindings(#{
        atom => ok,
        str => "hello",
        list => [1, 2, 3]
    }),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(<<"ok">>, maps:get(<<"atom">>, Bindings)),
    ?assertEqual(<<"hello">>, maps:get(<<"str">>, Bindings)),
    ?assertEqual([1, 2, 3], maps:get(<<"list">>, Bindings)).

format_bindings_with_special_characters_test() ->
    %% Test bindings with special characters in keys
    Response = beamtalk_repl_json:format_bindings(#{'_privateVar' => 42, 'CamelCase' => "test"}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"_privateVar">>, Bindings)),
    ?assertEqual(<<"test">>, maps:get(<<"CamelCase">>, Bindings)).

%%% format_loaded additional tests

format_loaded_single_test() ->
    Response = beamtalk_repl_json:format_loaded([#{name => "Counter", superclass => "Actor"}]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual([<<"Counter">>], maps:get(<<"classes">>, Decoded)).

format_loaded_multiple_test() ->
    Response = beamtalk_repl_json:format_loaded([
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

format_loaded_preserves_order_test() ->
    %% Verify that loaded classes are returned in the same order
    Classes = [
        #{name => "Zebra", superclass => "Object"},
        #{name => "Alpha", superclass => "Object"},
        #{name => "Middle", superclass => "Object"}
    ],
    Response = beamtalk_repl_json:format_loaded(Classes),
    Decoded = jsx:decode(Response, [return_maps]),
    Result = maps:get(<<"classes">>, Decoded),
    ?assertEqual([<<"Zebra">>, <<"Alpha">>, <<"Middle">>], Result).

%%% term_to_json additional tests

term_to_json_known_atom_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_json:term_to_json(hello)).

term_to_json_invalid_utf8_binary_test() ->
    Result = beamtalk_repl_json:term_to_json(<<255, 254>>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(<<255, 254>>, Result).

term_to_json_printable_list_unicode_test() ->
    %% Use explicit codepoints for encoding independence
    ?assertEqual(<<"café"/utf8>>, beamtalk_repl_json:term_to_json("caf" ++ [16#00E9])).

term_to_json_nested_list_test() ->
    Result = beamtalk_repl_json:term_to_json([<<"a">>, 1, true]),
    ?assertEqual([<<"a">>, 1, true], Result).

term_to_json_map_with_atom_keys_test() ->
    %% BT-535: Maps are pre-formatted as Beamtalk syntax strings
    Result = beamtalk_repl_json:term_to_json(#{name => <<"test">>, count => 5}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#count => 5">>) =/= nomatch),
    ?assert(binary:match(Result, <<"#name => \"test\"">>) =/= nomatch).

term_to_json_map_with_binary_keys_test() ->
    Result = beamtalk_repl_json:term_to_json(#{<<"key">> => <<"val">>}),
    ?assertEqual(<<"#{\"key\" => \"val\"}">>, Result).

term_to_json_map_with_list_keys_test() ->
    %% BT-535: List keys are printed using print_string format
    Result = beamtalk_repl_json:term_to_json(#{"stringkey" => 42}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"42">>) =/= nomatch).

term_to_json_map_with_non_printable_list_key_test() ->
    Result = beamtalk_repl_json:term_to_json(#{[1, 2, 3] => <<"val">>}),
    ?assert(is_binary(Result)).

term_to_json_map_with_integer_key_test() ->
    Result = beamtalk_repl_json:term_to_json(#{99 => <<"val">>}),
    ?assert(is_binary(Result)).

term_to_json_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        error(process_did_not_die)
    end,
    ?assertNot(is_process_alive(Pid)),
    Result = beamtalk_repl_json:term_to_json(Pid),
    ?assert(binary:match(Result, <<"#Dead<">>) =/= nomatch).

term_to_json_alive_pid_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
        receive
            stop -> Parent ! done
        end
    end),
    Result = beamtalk_repl_json:term_to_json(Pid),
    ?assert(binary:match(Result, <<"#Actor<">>) =/= nomatch),
    Pid ! stop,
    receive
        done -> ok
    after 200 -> ok
    end.

term_to_json_function_arity_0_test() ->
    Result = beamtalk_repl_json:term_to_json(fun() -> ok end),
    ?assertEqual(<<"a Block/0">>, Result).

term_to_json_function_arity_2_test() ->
    Result = beamtalk_repl_json:term_to_json(fun(A, B) -> {A, B} end),
    ?assertEqual(<<"a Block/2">>, Result).

term_to_json_beamtalk_object_tuple_test() ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json({beamtalk_object, 'Counter', counter, Pid}),
    ?assert(binary:match(Result, <<"#Actor<">>) =/= nomatch),
    ?assert(binary:match(Result, <<"Counter">>) =/= nomatch),
    exit(Pid, kill).

term_to_json_future_timeout_test() ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json({future_timeout, Pid}),
    ?assert(binary:match(Result, <<"#Future<timeout,">>) =/= nomatch),
    exit(Pid, kill).

term_to_json_tagged_future_timeout_test() ->
    %% BT-840: Tagged future variant of future_timeout
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json({future_timeout, {beamtalk_future, Pid}}),
    ?assert(binary:match(Result, <<"#Future<timeout,">>) =/= nomatch),
    exit(Pid, kill).

term_to_json_future_rejected_test() ->
    Result = beamtalk_repl_json:term_to_json({future_rejected, some_reason}),
    ?assert(binary:match(Result, <<"#Future<rejected:">>) =/= nomatch).

term_to_json_generic_tuple_test() ->
    Result = beamtalk_repl_json:term_to_json({a, b, c}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#a, #b, #c}">>, Result).

term_to_json_fallback_reference_test() ->
    Ref = make_ref(),
    Result = beamtalk_repl_json:term_to_json(Ref),
    ?assert(is_binary(Result)).

%%% format_error_message additional tests

format_error_message_module_not_found_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({module_not_found, <<"counter">>}),
    ?assert(binary:match(Msg, <<"Module not loaded">>) =/= nomatch).

format_error_message_invalid_module_name_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({invalid_module_name, <<"123bad">>}),
    ?assert(binary:match(Msg, <<"Invalid module name">>) =/= nomatch).

format_error_message_actors_exist_singular_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({actors_exist, counter, 1}),
    ?assert(binary:match(Msg, <<"1 actor still running">>) =/= nomatch),
    ?assert(binary:match(Msg, <<":kill">>) =/= nomatch).

format_error_message_actors_exist_plural_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({actors_exist, counter, 3}),
    ?assert(binary:match(Msg, <<"3 actors still running">>) =/= nomatch).

format_error_message_unknown_op_test() ->
    Msg = beamtalk_repl_json:format_error_message({unknown_op, <<"badop">>}),
    ?assert(binary:match(Msg, <<"Unknown operation">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"badop">>) =/= nomatch).

format_error_message_inspect_failed_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({inspect_failed, "<0.1.0>"}),
    ?assert(binary:match(Msg, <<"Failed to inspect actor">>) =/= nomatch).

format_error_message_actor_not_alive_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({actor_not_alive, "<0.1.0>"}),
    ?assert(binary:match(Msg, <<"Actor is not alive">>) =/= nomatch).

format_error_message_reload_module_not_loaded_test() ->
    Msg = beamtalk_repl_json:format_error_message({module_not_loaded, "counter"}),
    ?assert(binary:match(Msg, <<"Module not loaded">>) =/= nomatch).

format_error_message_no_source_file_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({no_source_file, "Counter"}),
    ?assert(binary:match(Msg, <<"No source file recorded">>) =/= nomatch).

format_error_message_missing_module_name_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({missing_module_name, reload}),
    ?assert(binary:match(Msg, <<"Usage">>) =/= nomatch).

format_error_message_session_creation_failed_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({session_creation_failed, max_children}),
    ?assert(binary:match(Msg, <<"Failed to create session">>) =/= nomatch).

format_error_message_invalid_request_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({invalid_request, bad_format}),
    ?assert(binary:match(Msg, <<"Invalid request">>) =/= nomatch).

format_error_message_parse_error_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({parse_error, syntax}),
    ?assert(binary:match(Msg, <<"Parse error">>) =/= nomatch).

format_error_message_compile_error_list_test() ->
    Msg = beamtalk_repl_json:format_error_message({compile_error, "something wrong"}),
    ?assertEqual(<<"something wrong">>, Msg).

format_error_message_fallback_test() ->
    Msg = beamtalk_repl_json:format_error_message({completely_unknown, stuff}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

%% BT-237: eval_error with #beamtalk_error{} formatting

format_error_message_eval_error_beamtalk_error_test() ->
    Error = beamtalk_error:with_selector(
        beamtalk_error:new(does_not_understand, 'Integer'), nonExistentMethod
    ),
    Msg = beamtalk_repl_json:format_error_message({eval_error, error, Error}),
    ?assertEqual(<<"Integer does not understand 'nonExistentMethod'">>, Msg).

format_error_message_eval_error_beamtalk_error_with_hint_test() ->
    Error0 = beamtalk_error:new(instantiation_error, 'Actor'),
    Error1 = beamtalk_error:with_selector(Error0, new),
    Error = beamtalk_error:with_hint(Error1, <<"Use spawn instead">>),
    Msg = beamtalk_repl_json:format_error_message({eval_error, error, Error}),
    ?assert(binary:match(Msg, <<"Actor">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"Hint: Use spawn instead">>) =/= nomatch).

%%% format_actors additional tests

format_actors_single_test() ->
    Pid = self(),
    Actors = [#{pid => Pid, class => 'Counter', module => counter, spawned_at => 1234567890}],
    Response = beamtalk_repl_json:format_actors(Actors),
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
    Response = beamtalk_repl_json:format_actors(Actors),
    Decoded = jsx:decode(Response, [return_maps]),
    ActorList = maps:get(<<"actors">>, Decoded),
    ?assertEqual(2, length(ActorList)).

%%% format_modules additional tests

format_modules_empty_test() ->
    Response = beamtalk_repl_json:format_modules([]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"modules">>, Decoded)).

format_modules_single_test() ->
    Info =
        {counter, #{
            name => <<"Counter">>,
            source_file => "/tmp/counter.bt",
            actor_count => 3,
            load_time => 1234567890,
            time_ago => "2 minutes ago"
        }},
    Response = beamtalk_repl_json:format_modules([Info]),
    Decoded = jsx:decode(Response, [return_maps]),
    Modules = maps:get(<<"modules">>, Decoded),
    ?assertEqual(1, length(Modules)),
    [Mod] = Modules,
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, Mod)),
    ?assertEqual(<<"/tmp/counter.bt">>, maps:get(<<"source_file">>, Mod)),
    ?assertEqual(3, maps:get(<<"actor_count">>, Mod)).

%%% format_response edge cases

format_response_empty_map_test() ->
    Response = beamtalk_repl_json:format_response(#{}),
    Decoded = jsx:decode(Response, [return_maps]),
    %% BT-535: Empty map is pre-formatted as "#{}"
    ?assertEqual(<<"#{}">>, maps:get(<<"value">>, Decoded)).

format_response_nested_map_test() ->
    Response = beamtalk_repl_json:format_response(#{a => #{b => 1}}),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-535: Nested maps are pre-formatted as Beamtalk syntax
    ?assert(is_binary(Value)),
    ?assertEqual(<<"#{#a => #{#b => 1}}">>, Value).

format_response_true_false_test() ->
    ResponseT = beamtalk_repl_json:format_response(true),
    DecodedT = jsx:decode(ResponseT, [return_maps]),
    ?assertEqual(true, maps:get(<<"value">>, DecodedT)),
    ResponseF = beamtalk_repl_json:format_response(false),
    DecodedF = jsx:decode(ResponseF, [return_maps]),
    ?assertEqual(false, maps:get(<<"value">>, DecodedF)).

%%% format_response_with_warnings additional tests

format_response_with_warnings_no_warnings_test() ->
    Response = beamtalk_repl_json:format_response_with_warnings(42, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_response_with_warnings_single_warning_test() ->
    Response = beamtalk_repl_json:format_response_with_warnings(42, [<<"watch out">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(42, maps:get(<<"value">>, Decoded)),
    ?assertEqual([<<"watch out">>], maps:get(<<"warnings">>, Decoded)).

format_response_with_warnings_multiple_warnings_test() ->
    Warnings = [<<"warn1">>, <<"warn2">>, <<"warn3">>],
    Response = beamtalk_repl_json:format_response_with_warnings("hello", Warnings),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"hello">>, maps:get(<<"value">>, Decoded)),
    ?assertEqual(Warnings, maps:get(<<"warnings">>, Decoded)).

format_response_with_warnings_complex_value_test() ->
    Response = beamtalk_repl_json:format_response_with_warnings(
        #{x => 1, y => 2}, [<<"shadow">>]
    ),
    Decoded = jsx:decode(Response, [return_maps]),
    Value = maps:get(<<"value">>, Decoded),
    %% BT-535: Maps are pre-formatted as Beamtalk syntax strings
    ?assert(is_binary(Value)),
    ?assert(binary:match(Value, <<"#x => 1">>) =/= nomatch),
    ?assertEqual([<<"shadow">>], maps:get(<<"warnings">>, Decoded)).

%%% format_error_with_warnings additional tests

format_error_with_warnings_no_warnings_test() ->
    Response = beamtalk_repl_json:format_error_with_warnings(timeout, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Request timed out">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual(error, maps:find(<<"warnings">>, Decoded)).

format_error_with_warnings_single_warning_test() ->
    Response = beamtalk_repl_json:format_error_with_warnings(
        empty_expression, [<<"deprecated syntax">>]
    ),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Empty expression">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual([<<"deprecated syntax">>], maps:get(<<"warnings">>, Decoded)).

format_error_with_warnings_multiple_warnings_test() ->
    Warnings = [<<"w1">>, <<"w2">>],
    Response = beamtalk_repl_json:format_error_with_warnings(empty_expression, Warnings),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Empty expression">>, maps:get(<<"message">>, Decoded)),
    ?assertEqual(Warnings, maps:get(<<"warnings">>, Decoded)).

format_error_with_warnings_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    ErrorWithSel = beamtalk_error:with_selector(Error, foo),
    Response = beamtalk_repl_json:format_error_with_warnings(ErrorWithSel, [<<"hint">>]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Integer">>) =/= nomatch),
    ?assertEqual([<<"hint">>], maps:get(<<"warnings">>, Decoded)).

%%% format_docs additional tests

format_docs_simple_test() ->
    Response = beamtalk_repl_json:format_docs(<<"Counter is a class">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<"Counter is a class">>, maps:get(<<"docs">>, Decoded)).

format_docs_empty_test() ->
    Response = beamtalk_repl_json:format_docs(<<>>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"docs">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual(<<>>, maps:get(<<"docs">>, Decoded)).

format_docs_multiline_test() ->
    Doc = <<"Counter\n\nA simple counter class.\nMethods: increment, decrement">>,
    Response = beamtalk_repl_json:format_docs(Doc),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(Doc, maps:get(<<"docs">>, Decoded)).

%%% format_bindings with non-atom key types

format_bindings_list_key_test() ->
    Response = beamtalk_repl_json:format_bindings(#{"myVar" => 42}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(42, maps:get(<<"myVar">>, Bindings)).

format_bindings_integer_key_test() ->
    Response = beamtalk_repl_json:format_bindings(#{99 => true}),
    Decoded = jsx:decode(Response, [return_maps]),
    Bindings = maps:get(<<"bindings">>, Decoded),
    ?assertEqual(true, maps:get(<<"99">>, Bindings)).

%%% term_to_json unicode error paths

term_to_json_list_with_bad_unicode_test() ->
    %% A list with high codepoints that fail characters_to_binary
    %% This should fall back to io_lib:format
    Result = beamtalk_repl_json:term_to_json([16#FFFF, 16#FFFE]),
    ?assert(is_list(Result) orelse is_binary(Result)).

%%% term_to_json with class object display

term_to_json_beamtalk_class_object_test() ->
    %% A class object has Class that is a class name
    %% We need beamtalk_object_class to be accessible; test the tuple pattern
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json({beamtalk_object, 'TestClassName', some_module, Pid}),
    %% Should either be a class display name or #Actor<...> format
    ?assert(is_binary(Result)),
    exit(Pid, kill).

%%% format_error_message with list and complex names

format_error_message_with_list_name_test() ->
    Msg = beamtalk_repl_json:format_error_message({file_not_found, "test/file.bt"}),
    ?assert(binary:match(Msg, <<"File not found">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"test/file.bt">>) =/= nomatch).

format_error_message_with_complex_reason_test() ->
    %% Triggers format_name fallback with non-atom/binary/list
    Msg = beamtalk_repl_json:format_error_message({read_error, {posix, enoent}}),
    ?assert(binary:match(Msg, <<"Failed to read file">>) =/= nomatch).

%%% format_response crash safety tests

format_response_deeply_nested_test() ->
    Response = beamtalk_repl_json:format_response([1, [2, [3, [4]]]]),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

format_response_with_warnings_error_fallback_test() ->
    %% Even bad values should produce valid JSON via fallback
    Response = beamtalk_repl_json:format_response_with_warnings(42, []),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)).

%%% format_error fallback tests

format_error_with_warnings_fallback_test() ->
    %% Generic atom errors should still produce valid JSON with warnings
    Response = beamtalk_repl_json:format_error_with_warnings(
        some_bizarre_error, [<<"w1">>]
    ),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))),
    ?assertEqual([<<"w1">>], maps:get(<<"warnings">>, Decoded)).

%%% format_modules multiple entries

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
    Response = beamtalk_repl_json:format_modules(Modules),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"modules">>, maps:get(<<"type">>, Decoded)),
    ModList = maps:get(<<"modules">>, Decoded),
    ?assertEqual(2, length(ModList)),
    [First, Second] = ModList,
    ?assertEqual(<<"Counter">>, maps:get(<<"name">>, First)),
    ?assertEqual(2, maps:get(<<"actor_count">>, First)),
    ?assertEqual(<<"Point">>, maps:get(<<"name">>, Second)),
    ?assertEqual(0, maps:get(<<"actor_count">>, Second)).

%%% format_error_message additional coverage

format_error_message_unknown_actor_test() ->
    Msg = beamtalk_repl_json:format_error_message({unknown_actor, "<0.99.0>"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_invalid_pid_test() ->
    Msg = beamtalk_repl_json:format_error_message({invalid_pid, "not-a-pid"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_class_not_found_binary_test() ->
    Msg = beamtalk_repl_json:format_error_message({class_not_found, <<"Foo">>}),
    ?assert(binary:match(Msg, <<"Unknown class">>) =/= nomatch).

format_error_message_class_not_found_with_modules_hint_test() ->
    %% Ensure the atom exists first
    _ = 'NonExistentClass',
    Msg = beamtalk_repl_json:format_error_message({class_not_found, 'NonExistentClass'}),
    ?assert(binary:match(Msg, <<"Unknown class">>) =/= nomatch),
    ?assert(binary:match(Msg, <<":modules">>) =/= nomatch).

format_error_message_method_not_found_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({method_not_found, 'Integer', <<"+:">>}),
    ?assert(binary:match(Msg, <<"Integer">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"does not understand">>) =/= nomatch).

format_error_message_wrapped_exception_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ErrorWithSel = beamtalk_error:with_selector(Error, badMethod),
    Wrapped = #{'$beamtalk_class' => 'Exception', error => ErrorWithSel},
    Msg = beamtalk_repl_json:format_error_message(Wrapped),
    ?assert(binary:match(Msg, <<"Exception">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"Counter">>) =/= nomatch).

format_error_message_eval_error_wrapped_exception_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = #{'$beamtalk_class' => 'TypeError', error => Error},
    Msg = beamtalk_repl_json:format_error_message({eval_error, error, Wrapped}),
    ?assert(binary:match(Msg, <<"TypeError">>) =/= nomatch).

%%% term_to_json additional tests

term_to_json_future_pending_test() ->
    %% BT-840: Futures are now tagged tuples {beamtalk_future, Pid}
    Future = beamtalk_future:new(),
    timer:sleep(50),
    Result = beamtalk_repl_json:term_to_json(Future),
    ?assertEqual(<<"#Future<pending>">>, Result),
    beamtalk_future:resolve(Future, ok).

term_to_json_nested_map_test() ->
    %% BT-535: Nested maps are pre-formatted as Beamtalk syntax
    Result = beamtalk_repl_json:term_to_json(#{a => #{b => #{c => 42}}}),
    ?assertEqual(<<"#{#a => #{#b => #{#c => 42}}}">>, Result).

term_to_json_empty_map_test() ->
    ?assertEqual(<<"#{}">>, beamtalk_repl_json:term_to_json(#{})).

term_to_json_map_with_mixed_keys_test() ->
    %% BT-535: Mixed keys are formatted as Beamtalk syntax
    Result = beamtalk_repl_json:term_to_json(#{atom_key => 1, <<"bin_key">> => 2}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#atom_key => 1">>) =/= nomatch),
    ?assert(binary:match(Result, <<"\"bin_key\" => 2">>) =/= nomatch).

term_to_json_single_element_tuple_test() ->
    Result = beamtalk_repl_json:term_to_json({only}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#only}">>, Result).

term_to_json_large_tuple_test() ->
    Result = beamtalk_repl_json:term_to_json({a, b, c, d, e}),
    %% BT-536: Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#a, #b, #c, #d, #e}">>, Result).

term_to_json_mixed_tuple_test() ->
    %% BT-536: Tuple with mixed types: integer, atom, string
    Result = beamtalk_repl_json:term_to_json({ok, 42, <<"hello">>}),
    ?assertEqual(<<"{#ok, 42, \"hello\"}">>, Result).

term_to_json_empty_tuple_test() ->
    %% BT-536: Empty tuple
    Result = beamtalk_repl_json:term_to_json({}),
    ?assertEqual(<<"{}">>, Result).

term_to_json_nested_list_with_maps_test() ->
    %% BT-535: Maps in lists are pre-formatted as strings
    Result = beamtalk_repl_json:term_to_json([#{a => 1}, #{b => 2}]),
    ?assertEqual(2, length(Result)),
    [First, Second] = Result,
    ?assertEqual(<<"#{#a => 1}">>, First),
    ?assertEqual(<<"#{#b => 2}">>, Second).

term_to_json_list_with_pids_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
        receive
            stop -> Parent ! done
        end
    end),
    Result = beamtalk_repl_json:term_to_json([Pid, 42]),
    ?assertEqual(2, length(Result)),
    [PidJson, NumJson] = Result,
    ?assert(binary:match(PidJson, <<"#Actor<">>) =/= nomatch),
    ?assertEqual(42, NumJson),
    Pid ! stop,
    receive
        done -> ok
    after 200 -> ok
    end.

term_to_json_reference_test() ->
    Ref = make_ref(),
    Result = beamtalk_repl_json:term_to_json(Ref),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

term_to_json_port_test() ->
    %% Create a port deterministically so the test doesn't depend on environment state
    %% Use cross-platform command: Windows cmd /c exit, Unix true
    Command =
        case os:type() of
            {win32, _} -> "cmd /c exit 0";
            _ -> "true"
        end,
    Port = open_port({spawn, Command}, []),
    Result = beamtalk_repl_json:term_to_json(Port),
    ?assert(is_binary(Result)),
    catch port_close(Port).

%%% format_response with port value test

format_response_with_unserializable_test() ->
    %% Port references are handled by term_to_json's fallback via io_lib:format
    %% Use cross-platform command: Windows cmd /c exit, Unix true
    Command =
        case os:type() of
            {win32, _} -> "cmd /c exit 0";
            _ -> "true"
        end,
    Port = open_port({spawn, Command}, []),
    Response = beamtalk_repl_json:format_response(Port),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"result">>, maps:get(<<"type">>, Decoded)),
    catch port_close(Port).

%%% format_error_message v2 additional edge cases

format_error_message_unknown_actor_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({unknown_actor, "<0.99.0>"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_invalid_pid_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message({invalid_pid, "garbage"}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

format_error_message_load_error_atom_test() ->
    Msg = beamtalk_repl_json:format_error_message({load_error, {bad_module, counter}}),
    ?assert(binary:match(Msg, <<"Failed to load bytecode">>) =/= nomatch).

format_error_message_read_error_enoent_test() ->
    Msg = beamtalk_repl_json:format_error_message({read_error, enoent}),
    ?assert(binary:match(Msg, <<"Failed to read file">>) =/= nomatch).

format_error_message_empty_expression_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message(empty_expression),
    ?assertEqual(<<"Empty expression">>, Msg).

format_error_message_timeout_v2_test() ->
    Msg = beamtalk_repl_json:format_error_message(timeout),
    ?assertEqual(<<"Request timed out">>, Msg).

format_error_message_invalid_module_test() ->
    Msg = beamtalk_repl_json:format_error_message({invalid_module, missing}),
    %% Falls through to generic formatter
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).
