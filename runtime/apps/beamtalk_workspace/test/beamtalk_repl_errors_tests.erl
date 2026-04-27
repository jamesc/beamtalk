%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_errors_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_errors (BT-2097).

Covers all four exported functions:
  safe_to_existing_atom/1  — empty-binary, known atom, nonexistent atom, non-binary
  format_name/1            — atom, binary, list, other (integer, tuple)
  ensure_structured_error/1 — all ~15 error-pattern branches
  ensure_structured_error/2 — delegation to /1 for known patterns + class-contextual fallback
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
