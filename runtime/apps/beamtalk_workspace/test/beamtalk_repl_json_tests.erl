%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% **DDD Context:** Language Service

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
