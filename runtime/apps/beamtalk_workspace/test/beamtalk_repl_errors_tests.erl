%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_errors_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_errors (BT-2097).

Covers all five exported functions:
  safe_to_existing_atom/1  — empty-binary, known atom, nonexistent atom, non-binary
  format_name/1            — atom, binary, list, other (integer, tuple)
  ensure_structured_error/1 — all ~15 error-pattern branches
  ensure_structured_error/2 — delegation to /1 for known patterns + class-contextual fallback
  normalize_diagnostic/1   — map (all fields, partial, empty), binary passthrough, fallback
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% safe_to_existing_atom/1
%%% ============================================================================

safe_to_existing_atom_empty_binary_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_errors:safe_to_existing_atom(<<>>)).

safe_to_existing_atom_known_atom_test() ->
    ?assertEqual({ok, ok}, beamtalk_repl_errors:safe_to_existing_atom(<<"ok">>)).

safe_to_existing_atom_nonexistent_atom_test() ->
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_errors:safe_to_existing_atom(
            <<"bt_repl_errors_test_nonexistent_atom_xyzzy_7891_ab">>
        )
    ).

safe_to_existing_atom_non_binary_integer_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_errors:safe_to_existing_atom(42)).

safe_to_existing_atom_non_binary_atom_test() ->
    ?assertEqual({error, badarg}, beamtalk_repl_errors:safe_to_existing_atom(ok)).

%%% ============================================================================
%%% format_name/1
%%% ============================================================================

format_name_atom_test() ->
    ?assertEqual(<<"ok">>, beamtalk_repl_errors:format_name(ok)).

format_name_binary_test() ->
    ?assertEqual(<<"hello">>, beamtalk_repl_errors:format_name(<<"hello">>)).

format_name_list_test() ->
    ?assertEqual(<<"foo">>, beamtalk_repl_errors:format_name("foo")).

format_name_integer_test() ->
    Result = beamtalk_repl_errors:format_name(42),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

format_name_tuple_test() ->
    Result = beamtalk_repl_errors:format_name({ok, value}),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%%% ============================================================================
%%% ensure_structured_error/1 — passthrough cases
%%% ============================================================================

ensure_structured_error_passthrough_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error(Error)).

ensure_structured_error_passthrough_tagged_map_test() ->
    Error = beamtalk_error:new(type_error, 'Integer'),
    Wrapped = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error(Wrapped)).

%%% ============================================================================
%%% ensure_structured_error/1 — eval_error variants
%%% ============================================================================

ensure_structured_error_eval_error_tagged_map_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = #{'$beamtalk_class' => 'Exception', error => Error},
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error({eval_error, error, Wrapped})).

ensure_structured_error_eval_error_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Actor'),
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error({eval_error, error, Error})).

ensure_structured_error_eval_error_known_tuple_delegates_test() ->
    %% {eval_error, _, Reason} where Reason is a known tuple chains to /1
    Result = beamtalk_repl_errors:ensure_structured_error(
        {eval_error, error, {undefined_variable, <<"x">>}}
    ),
    ?assertMatch(#beamtalk_error{kind = undefined_variable}, Result).

ensure_structured_error_eval_error_unknown_reason_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({eval_error, error, some_weird_reason}),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result).

ensure_structured_error_eval_error_same_tag_different_arity_test() ->
    %% BT-3084: is_known_error_reason/1 matches {Tag, Arity} pairs, not bare
    %% Tag. beamtalk_behaviour_intrinsics.erl:706 constructs a 4-arity
    %% {class_not_found, _, Path, Defined}, distinct from the REPL's own
    %% 2-arity {class_not_found, ClassName} clause in ensure_structured_error/1.
    %% A 4-arity reason must NOT be misidentified as "known" and delegated
    %% (there is no ensure_structured_error/1 clause for that arity) — it
    %% should fall through to the generic "Evaluation error: Class:Reason"
    %% wrapper, preserving the eval_error Class context.
    Result = beamtalk_repl_errors:ensure_structured_error(
        {eval_error, error, {class_not_found, some_mod, "path", true}}
    ),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result),
    ?assertMatch(
        <<"Evaluation error: error:", _/binary>>,
        Result#beamtalk_error.message
    ).

%%% ============================================================================
%%% ensure_structured_error/1 — compile_error variants
%%% ============================================================================

ensure_structured_error_compile_error_diagnostic_with_binary_hint_test() ->
    Diag = #{
        message => <<"Unused variable `x`">>,
        hint => <<"Remove or prefix with `_`">>,
        line => 3
    },
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, [Diag]}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assertEqual(<<"Unused variable `x`">>, Result#beamtalk_error.message),
    ?assertEqual(<<"Remove or prefix with `_`">>, Result#beamtalk_error.hint).

ensure_structured_error_compile_error_diagnostic_without_hint_test() ->
    Diag = #{message => <<"Type mismatch">>, line => 5},
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, [Diag]}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assertEqual(<<"Type mismatch">>, Result#beamtalk_error.message),
    ?assertEqual(undefined, Result#beamtalk_error.hint).

ensure_structured_error_compile_error_diagnostic_non_binary_hint_test() ->
    %% hint key present but value is not binary — falls through to Err1 (hint = undefined)
    Diag = #{message => <<"err">>, hint => not_a_binary},
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, [Diag]}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assertEqual(undefined, Result#beamtalk_error.hint).

ensure_structured_error_compile_error_diagnostic_multiple_entries_test() ->
    %% Only the first diagnostic is used; additional entries are ignored
    First = #{message => <<"first error">>, line => 1},
    Second = #{message => <<"second error">>, line => 2},
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, [First, Second]}),
    ?assertEqual(<<"first error">>, Result#beamtalk_error.message).

ensure_structured_error_compile_error_binary_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(
        {compile_error, <<"syntax error near `+`">>}
    ),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assertEqual(<<"syntax error near `+`">>, Result#beamtalk_error.message).

ensure_structured_error_compile_error_list_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, "syntax error"}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assertEqual(<<"syntax error">>, Result#beamtalk_error.message).

ensure_structured_error_compile_error_other_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({compile_error, bad_module}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assert(is_binary(Result#beamtalk_error.message)).

%%% ============================================================================
%%% ensure_structured_error/1 — remaining known-tuple patterns
%%% ============================================================================

ensure_structured_error_undefined_variable_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({undefined_variable, <<"counter">>}),
    ?assertMatch(#beamtalk_error{kind = undefined_variable}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"counter">>) =/= nomatch).

ensure_structured_error_file_not_found_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({file_not_found, <<"missing.bt">>}),
    ?assertMatch(#beamtalk_error{kind = file_not_found}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"missing.bt">>) =/= nomatch).

ensure_structured_error_read_error_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({read_error, enoent}),
    ?assertMatch(#beamtalk_error{kind = io_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"Failed to read file">>) =/= nomatch).

ensure_structured_error_load_error_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({load_error, badfile}),
    ?assertMatch(#beamtalk_error{kind = io_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"Failed to load bytecode">>) =/= nomatch).

ensure_structured_error_registration_error_tuple_test() ->
    %% {ModuleName, Reason} form — ModuleName passed to ~s, use a charlist
    Result = beamtalk_repl_errors:ensure_structured_error(
        {registration_error, {"Counter", already_started}}
    ),
    ?assertMatch(#beamtalk_error{kind = registration_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"Counter">>) =/= nomatch).

ensure_structured_error_registration_error_plain_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({registration_error, noproc}),
    ?assertMatch(#beamtalk_error{kind = registration_error}, Result),
    ?assert(is_binary(Result#beamtalk_error.message)).

ensure_structured_error_parse_error_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({parse_error, <<"unexpected ']'">>}),
    ?assertMatch(#beamtalk_error{kind = compile_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"Parse error">>) =/= nomatch).

ensure_structured_error_invalid_request_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(
        {invalid_request, <<"missing op field">>}
    ),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"Invalid request">>) =/= nomatch).

ensure_structured_error_empty_expression_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(empty_expression),
    ?assertMatch(#beamtalk_error{kind = empty_expression}, Result),
    ?assertEqual(<<"Empty expression">>, Result#beamtalk_error.message).

ensure_structured_error_timeout_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(timeout),
    ?assertMatch(#beamtalk_error{kind = timeout}, Result),
    ?assertEqual(<<"Request timed out">>, Result#beamtalk_error.message).

ensure_structured_error_fallback_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({some_unknown_error, details}),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result),
    ?assert(is_binary(Result#beamtalk_error.message)).

%%% ============================================================================
%%% ensure_structured_error/2 — passthrough cases
%%% ============================================================================

ensure_structured_error_2_passthrough_beamtalk_error_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error(Error, error)).

ensure_structured_error_2_passthrough_tagged_map_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    ?assertEqual(Error, beamtalk_repl_errors:ensure_structured_error(Wrapped, error)).

%%% ============================================================================
%%% ensure_structured_error/2 — delegation to /1 for known patterns
%%% ============================================================================

ensure_structured_error_2_delegates_compile_error_test() ->
    Reason = {compile_error, <<"err">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_eval_error_test() ->
    Error = beamtalk_error:new(type_error, 'Integer'),
    Reason = {eval_error, error, Error},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_undefined_variable_test() ->
    Reason = {undefined_variable, <<"x">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_file_not_found_test() ->
    Reason = {file_not_found, <<"f.bt">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_read_error_test() ->
    Reason = {read_error, enoent},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_load_error_test() ->
    Reason = {load_error, badfile},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_parse_error_test() ->
    Reason = {parse_error, <<"bad syntax">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_invalid_request_test() ->
    Reason = {invalid_request, <<"bad format">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_registration_error_test() ->
    Reason = {registration_error, noproc},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

%%% ============================================================================
%%% ensure_structured_error/2 — fallback includes exception class in message
%%% ============================================================================

ensure_structured_error_2_fallback_includes_class_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(something_weird, throw),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result),
    ?assert(binary:match(Result#beamtalk_error.message, <<"throw">>) =/= nomatch).

ensure_structured_error_2_fallback_different_classes_test() ->
    ResultErr = beamtalk_repl_errors:ensure_structured_error(unknown_reason, error),
    ResultExit = beamtalk_repl_errors:ensure_structured_error(unknown_reason, exit),
    ?assert(binary:match(ResultErr#beamtalk_error.message, <<"error">>) =/= nomatch),
    ?assert(binary:match(ResultExit#beamtalk_error.message, <<"exit">>) =/= nomatch).

%%% ============================================================================
%%% BT-3084: vocabulary previously only handled by
%%% beamtalk_repl_json:format_error_message/1's separate dispatch table.
%%% Folded into ensure_structured_error/1 so there is one canonical table.
%%% ============================================================================

ensure_structured_error_module_not_found_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({module_not_found, <<"counter">>}),
    ?assertMatch(#beamtalk_error{kind = module_not_found}, Result),
    ?assertEqual(<<"Module not loaded: counter">>, Result#beamtalk_error.message).

ensure_structured_error_invalid_module_name_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({invalid_module_name, <<"123bad">>}),
    ?assertMatch(#beamtalk_error{kind = invalid_module_name}, Result),
    ?assertEqual(<<"Invalid module name: 123bad">>, Result#beamtalk_error.message).

ensure_structured_error_actors_exist_singular_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({actors_exist, counter, 1}),
    ?assertMatch(#beamtalk_error{kind = actors_exist}, Result),
    ?assertNotEqual(
        nomatch, binary:match(Result#beamtalk_error.message, <<"1 actor still running">>)
    ).

ensure_structured_error_actors_exist_plural_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({actors_exist, counter, 3}),
    ?assertNotEqual(
        nomatch, binary:match(Result#beamtalk_error.message, <<"3 actors still running">>)
    ),
    ?assertNotEqual(nomatch, binary:match(Result#beamtalk_error.message, <<":kill">>)).

ensure_structured_error_class_not_found_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({class_not_found, 'Foo'}),
    ?assertMatch(#beamtalk_error{kind = class_not_found}, Result),
    ?assertEqual(
        <<"Unknown class: Foo. Use Workspace classes to see loaded classes.">>,
        Result#beamtalk_error.message
    ).

%% BT-3084 acceptance criteria: DNU rendered only by
%% beamtalk_error:generate_message/3 — the selector is quoted, and the
%% record's `selector` field is populated when the selector is an atom.
ensure_structured_error_method_not_found_atom_selector_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(
        {method_not_found, 'Counter', increment}
    ),
    ?assertMatch(#beamtalk_error{kind = does_not_understand}, Result),
    ?assertEqual(<<"Counter does not understand 'increment'">>, Result#beamtalk_error.message),
    ?assertEqual(increment, Result#beamtalk_error.selector).

%% Selector arriving as a binary (no existing atom to attach to the record)
%% still renders the canonical quoted wording via generate_message/3.
ensure_structured_error_method_not_found_binary_selector_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(
        {method_not_found, 'Counter', <<"increment">>}
    ),
    ?assertMatch(#beamtalk_error{kind = does_not_understand}, Result),
    ?assertEqual(<<"Counter does not understand 'increment'">>, Result#beamtalk_error.message).

ensure_structured_error_unknown_op_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({unknown_op, <<"badop">>}),
    ?assertMatch(#beamtalk_error{kind = unknown_op}, Result),
    ?assertEqual(<<"Unknown operation: badop">>, Result#beamtalk_error.message).

ensure_structured_error_inspect_failed_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({inspect_failed, "<0.100.0>"}),
    ?assertMatch(#beamtalk_error{kind = inspect_failed}, Result),
    ?assertEqual(<<"Failed to inspect actor: <0.100.0>">>, Result#beamtalk_error.message).

ensure_structured_error_actor_not_alive_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({actor_not_alive, "<0.100.0>"}),
    ?assertMatch(#beamtalk_error{kind = actor_not_alive}, Result),
    ?assertEqual(<<"Actor is not alive: <0.100.0>">>, Result#beamtalk_error.message).

ensure_structured_error_no_source_file_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({no_source_file, "counter"}),
    ?assertMatch(#beamtalk_error{kind = no_source_file}, Result),
    ?assertNotEqual(
        nomatch,
        binary:match(Result#beamtalk_error.message, <<"No source file recorded for module">>)
    ).

ensure_structured_error_module_not_loaded_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({module_not_loaded, <<"counter">>}),
    ?assertMatch(#beamtalk_error{kind = module_not_loaded}, Result),
    ?assertNotEqual(
        nomatch, binary:match(Result#beamtalk_error.message, <<"Module not loaded: counter">>)
    ).

ensure_structured_error_missing_module_name_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({missing_module_name, reload}),
    ?assertMatch(#beamtalk_error{kind = missing_module_name}, Result),
    ?assertNotEqual(nomatch, binary:match(Result#beamtalk_error.message, <<":reload">>)).

ensure_structured_error_session_creation_failed_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({session_creation_failed, timeout}),
    ?assertMatch(#beamtalk_error{kind = session_creation_failed}, Result),
    ?assertEqual(<<"Failed to create session: timeout">>, Result#beamtalk_error.message).

%% BT-3084 acceptance criteria: no raw-`~p` fallthrough for known tuples —
%% {registration_error, ...} was already structured here, but was previously
%% absent from beamtalk_repl_json's separate table (fixed by unifying on
%% this one). Assert the message never degrades to a bare tuple dump.
ensure_structured_error_registration_error_no_raw_fallthrough_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error(
        {registration_error, {'Counter', already_registered}}
    ),
    ?assertMatch(#beamtalk_error{kind = registration_error}, Result),
    ?assertEqual(nomatch, binary:match(Result#beamtalk_error.message, <<"{registration_error">>)).

%% BT-3084: previously this clause silently dropped the exception Class,
%% diverging from beamtalk_repl_json's separate "Evaluation error: Class:Reason"
%% wording for the same shape. Assert Class now survives.
ensure_structured_error_eval_error_generic_preserves_class_test() ->
    Result = beamtalk_repl_errors:ensure_structured_error({eval_error, error, badarg}),
    ?assertMatch(#beamtalk_error{kind = internal_error}, Result),
    ?assertEqual(<<"Evaluation error: error:badarg">>, Result#beamtalk_error.message).

%%% ============================================================================
%%% ensure_structured_error/2 — delegation for the BT-3084 vocabulary above
%%% ============================================================================

ensure_structured_error_2_delegates_module_not_found_test() ->
    Reason = {module_not_found, <<"counter">>},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_method_not_found_test() ->
    Reason = {method_not_found, 'Counter', increment},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

ensure_structured_error_2_delegates_actors_exist_test() ->
    Reason = {actors_exist, counter, 1},
    ?assertEqual(
        beamtalk_repl_errors:ensure_structured_error(Reason),
        beamtalk_repl_errors:ensure_structured_error(Reason, error)
    ).

%%% ============================================================================
%%% normalize_diagnostic/1
%%% ============================================================================

normalize_diagnostic_map_all_fields_test() ->
    D = #{message => <<"Unused var">>, line => 5, hint => <<"prefix with _">>},
    ?assertEqual(
        #{message => <<"Unused var">>, line => 5, hint => <<"prefix with _">>},
        beamtalk_repl_errors:normalize_diagnostic(D)
    ).

normalize_diagnostic_map_message_and_line_test() ->
    D = #{message => <<"Type error">>, line => 12},
    ?assertEqual(
        #{message => <<"Type error">>, line => 12},
        beamtalk_repl_errors:normalize_diagnostic(D)
    ).

normalize_diagnostic_map_message_only_test() ->
    D = #{message => <<"Parse error">>},
    ?assertEqual(
        #{message => <<"Parse error">>},
        beamtalk_repl_errors:normalize_diagnostic(D)
    ).

normalize_diagnostic_map_missing_message_uses_default_test() ->
    D = #{line => 3},
    ?assertEqual(
        #{message => <<"Unknown error">>, line => 3},
        beamtalk_repl_errors:normalize_diagnostic(D)
    ).

normalize_diagnostic_map_non_integer_line_omitted_test() ->
    D = #{message => <<"Error">>, line => <<"not-an-int">>},
    ?assertEqual(#{message => <<"Error">>}, beamtalk_repl_errors:normalize_diagnostic(D)).

normalize_diagnostic_map_non_binary_hint_omitted_test() ->
    D = #{message => <<"Error">>, hint => some_atom},
    ?assertEqual(#{message => <<"Error">>}, beamtalk_repl_errors:normalize_diagnostic(D)).

normalize_diagnostic_binary_passthrough_test() ->
    ?assertEqual(
        #{message => <<"plain error text">>},
        beamtalk_repl_errors:normalize_diagnostic(<<"plain error text">>)
    ).

normalize_diagnostic_atom_fallback_test() ->
    Result = beamtalk_repl_errors:normalize_diagnostic(some_atom),
    ?assertMatch(#{message := _}, Result),
    #{message := Msg} = Result,
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).

normalize_diagnostic_tuple_fallback_test() ->
    Result = beamtalk_repl_errors:normalize_diagnostic({error, reason}),
    ?assertMatch(#{message := _}, Result),
    #{message := Msg} = Result,
    ?assert(is_binary(Msg)),
    ?assert(byte_size(Msg) > 0).
