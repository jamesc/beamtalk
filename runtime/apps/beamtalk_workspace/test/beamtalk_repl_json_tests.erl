%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** REPL Session Context

-module(beamtalk_repl_json_tests).

-moduledoc """
EUnit tests for beamtalk_repl_json.

Tests JSON formatting for REPL protocol responses: format_error,
term_to_json, format_error_message, parse_json.
""".
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
%%% term_to_json tests
%%% ============================================================================

term_to_json_integer_test() ->
    ?assertEqual(42, beamtalk_repl_json:term_to_json(42)).

term_to_json_float_test() ->
    %% Floats are converted to binary strings to preserve decimal point.
    ?assertEqual(<<"3.14">>, beamtalk_repl_json:term_to_json(3.14)).

term_to_json_whole_float_test() ->
    %% Whole-number floats preserve ".0".
    ?assertEqual(<<"6.0">>, beamtalk_repl_json:term_to_json(6.0)).

term_to_json_negative_float_test() ->
    ?assertEqual(<<"-2.5">>, beamtalk_repl_json:term_to_json(-2.5)).

%% ADR 0141: an unaligned Bitstring renders in Erlang notation, on one line
%% however long (not the multi-line `~p` fallback).
term_to_json_bitstring_test() ->
    ?assertEqual(<<"<<2:3>>">>, beamtalk_repl_json:term_to_json(<<2:3>>)),
    Long = beamtalk_repl_json:term_to_json(<<0:(8 * 100), 1:1>>),
    ?assertEqual(nomatch, binary:match(Long, <<"\n">>)).

term_to_json_boolean_test() ->
    ?assertEqual(true, beamtalk_repl_json:term_to_json(true)),
    ?assertEqual(false, beamtalk_repl_json:term_to_json(false)).

term_to_json_atom_test() ->
    ?assertEqual(<<"nil">>, beamtalk_repl_json:term_to_json(nil)),
    ?assertEqual(<<"ok">>, beamtalk_repl_json:term_to_json(ok)).

term_to_json_binary_test() ->
    %% BT-4167: Strings display quoted (ADR 0138 spelling).
    ?assertEqual(<<"\"hello\"">>, beamtalk_repl_json:term_to_json(<<"hello">>)).

term_to_json_binary_escapes_test() ->
    %% BT-4167: quoted Strings escape per the shared ADR 0138 mapping
    %% ("" doubling, \{, \\, \r, \n, \t, \0) — same spelling as
    %% String printString and Rust unparse_literal_display.
    ?assertEqual(
        <<"\"say \"\"hi\"\"\"">>,
        beamtalk_repl_json:term_to_json(<<"say \"hi\"">>)
    ),
    ?assertEqual(
        <<"\"a\\{b\"">>,
        beamtalk_repl_json:term_to_json(<<"a{b">>)
    ),
    ?assertEqual(
        <<"\"a\\\\b\"">>,
        beamtalk_repl_json:term_to_json(<<"a\\b">>)
    ),
    ?assertEqual(
        <<"\"a\\nb\"">>,
        beamtalk_repl_json:term_to_json(<<"a\nb">>)
    ).

term_to_json_binary_non_utf8_test() ->
    %% BT-4167: Binaries (non-UTF-8) display as <<AB CD>> hex, distinct
    %% from quoted Strings — same spelling as Binary printString.
    %% Empty binary is valid UTF-8, so it prints as quoted empty.
    ?assertEqual(<<"\"\"">>, beamtalk_repl_json:term_to_json(<<>>)),
    ?assertEqual(<<"<<FF 00>>">>, beamtalk_repl_json:term_to_json(<<255, 0>>)).

term_to_json_list_elements_not_prequoted_test() ->
    %% BT-4203: list elements are bare (JSON encoding quotes them); top-level is quoted.
    ?assertEqual(
        [<<"a">>, <<"b">>], beamtalk_repl_json:term_to_json([<<"a">>, <<"b">>])
    ),
    ?assertEqual(
        [[<<"a">>], [<<"b">>]],
        beamtalk_repl_json:term_to_json([[<<"a">>], [<<"b">>]])
    ),
    ?assertEqual(
        [<<"<<FF 00>>">>], beamtalk_repl_json:term_to_json([<<255, 0>>])
    ).

element_to_json_tagged_string_test() ->
    %% Tagged String elements render bare; invalid UTF-8 is contained like at top level.
    ?assertEqual(<<"a">>, beamtalk_repl_json:element_to_json({beamtalk_string, <<"a">>})),
    ?assertEqual(
        [<<"a">>, <<"b">>],
        beamtalk_repl_json:term_to_json([{beamtalk_string, <<"a">>}, {beamtalk_string, <<"b">>}])
    ),
    Bad = beamtalk_repl_json:element_to_json({beamtalk_string, <<255, 0>>}),
    ?assertEqual(Bad, unicode:characters_to_binary(Bad)).

term_to_json_character_tag_test() ->
    %% BT-4167: tagged Character results display as $X, distinct from Integers.
    ?assertEqual(<<"$A">>, beamtalk_repl_json:term_to_json({beamtalk_character, 65})),
    ?assertEqual(<<"$a">>, beamtalk_repl_json:term_to_json({beamtalk_character, 97})).

term_to_json_character_tag_invalid_test() ->
    %% BT-4167: invalid scalars fall back to Integer display, never crash.
    ?assertEqual(
        <<"55296">>, beamtalk_repl_json:term_to_json({beamtalk_character, 16#D800})
    ),
    ?assertEqual(
        <<"-1">>, beamtalk_repl_json:term_to_json({beamtalk_character, -1})
    ),
    ?assertEqual(
        <<"1114112">>, beamtalk_repl_json:term_to_json({beamtalk_character, 16#110000})
    ).

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

%%% ADR 0094 / Critical Risk #3: actor & supervisor kind-headed labels.

term_to_json_dead_actor_fallback_test() ->
    %% A dead actor instance must render via the tuple-derived fallback —
    %% Actor(ClassName, pid) — without hanging or crashing the REPL formatter.
    Pid = spawn(fun() -> ok end),
    MRef = monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    end,
    Obj = {beamtalk_object, 'WedgedCounter', bt@wedged, Pid},
    Result = beamtalk_repl_json:term_to_json(Obj),
    ?assertMatch(<<"Actor(WedgedCounter, ", _/binary>>, Result),
    ?assert(binary:match(Result, <<"#Actor<">>) =:= nomatch).

term_to_json_unresponsive_actor_fallback_test() ->
    %% A live but unresponsive actor (never replies to gen_server:call) must
    %% degrade to the tuple-derived label after the short timeout, not hang.
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Obj = {beamtalk_object, 'WedgedCounter', bt@wedged, Pid},
    Result = beamtalk_repl_json:term_to_json(Obj),
    ?assertMatch(<<"Actor(WedgedCounter, ", _/binary>>, Result),
    Pid ! stop.

term_to_json_supervisor_label_test() ->
    %% A supervisor tuple renders Supervisor(ClassName, pid) directly (no
    %% gen_server round-trip) — see ADR 0094.
    Pid = self(),
    Sup = {beamtalk_supervisor, 'MySup', some_module, Pid},
    Result = beamtalk_repl_json:term_to_json(Sup),
    ?assertMatch(<<"Supervisor(MySup, ", _/binary>>, Result),
    ?assert(binary:match(Result, <<"#Supervisor<">>) =:= nomatch).

term_to_json_function_test() ->
    F = fun(X) -> X end,
    Result = beamtalk_repl_json:term_to_json(F),
    ?assertEqual(<<"Block/1">>, Result).

term_to_json_function_arity0_test() ->
    F = fun() -> ok end,
    Result = beamtalk_repl_json:term_to_json(F),
    ?assertEqual(<<"Block/0">>, Result).

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

%%% format_error additional tests

format_error_compile_error_test() ->
    Response = beamtalk_repl_json:format_error({compile_error, <<"Syntax error">>}),
    Decoded = json:decode(Response),
    ?assertEqual(<<"Syntax error">>, maps:get(<<"message">>, Decoded)).

format_error_eval_error_test() ->
    Response = beamtalk_repl_json:format_error({eval_error, error, badarg}),
    Decoded = json:decode(Response),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Evaluation error">>) =/= nomatch).

format_error_load_error_test() ->
    Response = beamtalk_repl_json:format_error({load_error, badfile}),
    Decoded = json:decode(Response),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to load bytecode">>) =/= nomatch).

format_error_read_error_test() ->
    Response = beamtalk_repl_json:format_error({read_error, eacces}),
    Decoded = json:decode(Response),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Failed to read file">>) =/= nomatch).

format_error_generic_tuple_test() ->
    Response = beamtalk_repl_json:format_error({unknown_error_type, "details"}),
    Decoded = json:decode(Response),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(is_binary(Message)).

format_error_generic_atom_test() ->
    Response = beamtalk_repl_json:format_error(some_unknown_error),
    Decoded = json:decode(Response),
    Message = maps:get(<<"message">>, Decoded),
    %% Generic errors are formatted with ~p, so should contain the atom name
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0).

%% format_error with #beamtalk_error{} record (with selector)
format_error_beamtalk_error_with_selector_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ErrorWithSelector = beamtalk_error:with_selector(Error, super),
    Response = beamtalk_repl_json:format_error(ErrorWithSelector),
    Decoded = json:decode(Response),
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
    Decoded = json:decode(Response),
    Message = maps:get(<<"message">>, Decoded),
    ?assert(binary:match(Message, <<"Integer">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Hint:">>) =/= nomatch),
    ?assert(binary:match(Message, <<"Check spelling">>) =/= nomatch).

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
    %% BT-4203: List elements are not pre-quoted (top-level Strings still are).
    ?assertEqual([<<"a">>, 1, true], Result).

term_to_json_map_with_atom_keys_test() ->
    %% Maps are pre-formatted as Beamtalk syntax strings
    Result = beamtalk_repl_json:term_to_json(#{name => <<"test">>, count => 5}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#count => 5">>) =/= nomatch),
    ?assert(binary:match(Result, <<"#name => \"test\"">>) =/= nomatch).

term_to_json_map_with_binary_keys_test() ->
    Result = beamtalk_repl_json:term_to_json(#{<<"key">> => <<"val">>}),
    ?assertEqual(<<"#{\"key\" => \"val\"}">>, Result).

term_to_json_map_with_list_keys_test() ->
    %% List keys are printed using print_string format
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
    ?assertEqual(<<"Block/0">>, Result).

term_to_json_function_arity_2_test() ->
    Result = beamtalk_repl_json:term_to_json(fun(A, B) -> {A, B} end),
    ?assertEqual(<<"Block/2">>, Result).

term_to_json_beamtalk_object_tuple_test() ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    %% ADR 0094: actor instances render kind-headed Actor(ClassName, pid).
    %% The pid is unresponsive (not a gen_server), so the formatter degrades to
    %% the tuple-derived fallback after the short timeout.
    Result = beamtalk_repl_json:term_to_json({beamtalk_object, 'Counter', counter, Pid}),
    ?assertMatch(<<"Actor(Counter, ", _/binary>>, Result),
    ?assert(binary:match(Result, <<"#Actor<">>) =:= nomatch),
    exit(Pid, kill).

term_to_json_beamtalk_object_registered_proxy_test() ->
    %% ADR 0079: name-resolving proxies carry `{registered, Name}`
    %% in the identity slot instead of a pid. The formatter must not call
    %% `pid_to_list/1` on the tuple (which would crash and tear down the
    %% REPL WebSocket). Instead, render the name so the proxy is
    %% operator-legible in REPL output.
    %% ADR 0094: rendered kind-headed as Actor(ClassName, registered, Name).
    Result = beamtalk_repl_json:term_to_json(
        {beamtalk_object, 'Counter', counter, {registered, my_counter}}
    ),
    ?assertEqual(<<"Actor(Counter, registered, my_counter)">>, Result).

term_to_json_supervisor_tuple_test() ->
    %% ADR 0094: Supervisor instances render kind-headed as
    %% Supervisor(ClassName, pid) / DynamicSupervisor(ClassName, pid).
    Pid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Result = beamtalk_repl_json:term_to_json({beamtalk_supervisor, 'WebApp', bt_webapp, Pid}),
    ?assertMatch(<<"Supervisor(WebApp, ", _/binary>>, Result),
    ?assert(binary:match(Result, <<"#Supervisor<">>) =:= nomatch),
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
    %% Tagged future variant of future_timeout
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
    %% Tuples formatted with symbol notation for atoms
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

format_error_message_compile_error_structured_diagnostics_test() ->
    %% structured diagnostic maps extract first message
    Msg = beamtalk_repl_json:format_error_message(
        {compile_error, [#{message => <<"Unused variable `x`">>, line => 3}]}
    ),
    ?assertEqual(<<"Unused variable `x`">>, Msg).

format_error_message_compile_error_list_test() ->
    Msg = beamtalk_repl_json:format_error_message({compile_error, "something wrong"}),
    ?assertEqual(<<"something wrong">>, Msg).

format_error_message_fallback_test() ->
    Msg = beamtalk_repl_json:format_error_message({completely_unknown, stuff}),
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

%%% ============================================================================
%%% Regression tests: format_error_message/1 derives from
%%% beamtalk_repl_errors:ensure_structured_error/1 (one canonical table)
%%% ============================================================================

%% Previously {registration_error, ...} was handled by ensure_structured_error/1
%% but absent from format_error_message/1's separate table, so it fell through
%% to a raw `~p` dump instead of the structured message.
format_error_message_registration_error_tuple_test() ->
    Msg = beamtalk_repl_json:format_error_message(
        {registration_error, {'Counter', already_registered}}
    ),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Class registration failed for Counter">>)),
    %% A raw `~p` fallthrough would render as a bare `{registration_error, {...}}`
    %% tuple — assert the structured prose replaced it, not just co-exists with it.
    ?assertEqual(nomatch, binary:match(Msg, <<"{registration_error">>)).

format_error_message_registration_error_reason_only_test() ->
    Msg = beamtalk_repl_json:format_error_message({registration_error, bad_module}),
    ?assertEqual(<<"Class registration failed: bad_module">>, Msg).

%% The structured (ensure_structured_error/1) path always extracted a `hint`
%% from a compile_error diagnostic map; format_error_message/1 used to drop it
%% via its own separate clause. Assert the hint now survives end-to-end.
format_error_message_compile_error_hint_propagation_test() ->
    Msg = beamtalk_repl_json:format_error_message(
        {compile_error, [
            #{
                message => <<"Unused variable `x`">>,
                line => 3,
                hint => <<"Prefix with _ to ignore">>
            }
        ]}
    ),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Unused variable `x`">>)),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Hint: Prefix with _ to ignore">>)).

%% DNU rendered by beamtalk_error:generate_message/3 only — the selector is
%% quoted, matching the canonical wording (previously this hand-rolled copy
%% left it unquoted).
format_error_message_method_not_found_quoted_selector_test() ->
    Msg = beamtalk_repl_json:format_error_message(
        {method_not_found, 'Counter', <<"increment">>}
    ),
    ?assertEqual(
        <<"Counter does not understand 'increment'\nHint: Use :help Counter to see available methods.">>,
        Msg
    ).

%% the two eval_error prose variants ("Evaluation error: Class:Reason")
%% previously drifted between ensure_structured_error/1 (dropped Class) and
%% format_error_message/1 (kept it). Assert both entry points now agree.
format_error_message_eval_error_generic_matches_ensure_structured_error_test() ->
    ViaJson = beamtalk_repl_json:format_error_message({eval_error, error, badarg}),
    ViaErrors = beamtalk_error:format(
        beamtalk_repl_errors:ensure_structured_error({eval_error, error, badarg})
    ),
    ?assertEqual(ViaErrors, ViaJson),
    ?assertMatch(<<"Evaluation error: error:badarg">>, ViaJson).

%% eval_error with #beamtalk_error{} formatting

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
    %% Should either be a class display name or Actor(...) format (ADR 0094)
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

format_error_message_class_not_found_with_hint_test() ->
    %% Ensure the atom exists first
    _ = 'NonExistentClass',
    Msg = beamtalk_repl_json:format_error_message({class_not_found, 'NonExistentClass'}),
    ?assert(binary:match(Msg, <<"Unknown class">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"Workspace classes">>) =/= nomatch).

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
    %% Futures are now tagged tuples {beamtalk_future, Pid}
    Future = beamtalk_future:new(),
    {beamtalk_future, FuturePid} = Future,
    ok = beamtalk_test_wait:sync(FuturePid),
    Result = beamtalk_repl_json:term_to_json(Future),
    ?assertEqual(<<"#Future<pending>">>, Result),
    beamtalk_future:resolve(Future, ok).

term_to_json_nested_map_test() ->
    %% Nested maps are pre-formatted as Beamtalk syntax
    Result = beamtalk_repl_json:term_to_json(#{a => #{b => #{c => 42}}}),
    ?assertEqual(<<"#{#a => #{#b => #{#c => 42}}}">>, Result).

term_to_json_empty_map_test() ->
    ?assertEqual(<<"#{}">>, beamtalk_repl_json:term_to_json(#{})).

term_to_json_map_with_mixed_keys_test() ->
    %% Mixed keys are formatted as Beamtalk syntax
    Result = beamtalk_repl_json:term_to_json(#{atom_key => 1, <<"bin_key">> => 2}),
    ?assert(is_binary(Result)),
    ?assert(binary:match(Result, <<"#atom_key => 1">>) =/= nomatch),
    ?assert(binary:match(Result, <<"\"bin_key\" => 2">>) =/= nomatch).

term_to_json_single_element_tuple_test() ->
    Result = beamtalk_repl_json:term_to_json({only}),
    %% Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#only}">>, Result).

term_to_json_large_tuple_test() ->
    Result = beamtalk_repl_json:term_to_json({a, b, c, d, e}),
    %% Tuples formatted with symbol notation for atoms
    ?assertEqual(<<"{#a, #b, #c, #d, #e}">>, Result).

term_to_json_mixed_tuple_test() ->
    %% Tuple with mixed types: integer, atom, string
    Result = beamtalk_repl_json:term_to_json({ok, 42, <<"hello">>}),
    ?assertEqual(<<"{#ok, 42, \"hello\"}">>, Result).

term_to_json_empty_tuple_test() ->
    %% Empty tuple
    Result = beamtalk_repl_json:term_to_json({}),
    ?assertEqual(<<"{}">>, Result).

term_to_json_nested_list_with_maps_test() ->
    %% Maps in lists are pre-formatted as strings
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
    (try
        port_close(Port)
    catch
        _:_ -> ok
    end).

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

%%% ============================================================================
%%% Catch-block coverage tests (BT-nightly-20260820)
%%% ============================================================================

%% format_error/1 catch branch (lines 88, 94-95).
%% A tagged-exception map whose `error` field is not a #beamtalk_error{} record
%% causes beamtalk_error:format/1 to throw function_clause inside
%% format_error_message/1.  The outer try/catch in format_error/1 produces a
%% fallback error response instead of crashing.
format_error_catch_test() ->
    Reason = #{'$beamtalk_class' => 'TestClass', error => not_a_beamtalk_error_record},
    Result = beamtalk_repl_json:format_error(Reason),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assertEqual(<<"error">>, maps:get(<<"type">>, Decoded)),
    ?assert(is_binary(maps:get(<<"message">>, Decoded))).

%%% ============================================================================
%%% encode_error/2, /4, /5 (lines 236, 241, 251)
%%% ============================================================================

encode_error_2_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\":\"eval\",\"id\":\"eid1\",\"session\":\"sess1\"}">>
    ),
    Result = beamtalk_repl_json:encode_error(some_error, Msg),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

encode_error_4_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\":\"eval\",\"id\":\"eid2\",\"session\":\"sess1\"}">>
    ),
    Result = beamtalk_repl_json:encode_error(some_error, Msg, <<"stdout">>, []),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

encode_error_5_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode(
        <<"{\"op\":\"eval\",\"id\":\"eid3\",\"session\":\"sess1\"}">>
    ),
    Result = beamtalk_repl_json:encode_error(some_error, Msg, <<"stdout">>, [], #{
        <<"hint">> => <<"check syntax">>
    }),
    {ok, Decoded} = beamtalk_repl_json:parse_json(Result),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    ?assertEqual(<<"check syntax">>, maps:get(<<"hint">>, Decoded)).

%%% ============================================================================
%%% term_to_json_future_pid/1 completed path (line 383)
%%% ============================================================================

%% The `false` branch of is_process_alive/1 in term_to_json_future_pid/1 fires
%% when the underlying future process has already exited.  Monitor guarantees
%% the process is gone before the assertion.
term_to_json_future_completed_test() ->
    Pid = spawn(fun() -> ok end),
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 1000 -> error(timeout)
    end,
    Result = beamtalk_repl_json:term_to_json({beamtalk_future, Pid}),
    ?assertEqual(<<"#Future<completed>">>, Result).
