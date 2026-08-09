%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compile_diagnostics_tests).

-moduledoc """
Tests for beamtalk_compile_diagnostics (BT-3115).
""".

-include_lib("eunit/include/eunit.hrl").

%% A synthetic core_lint unbound_var error, in the exact shape
%% compile:forms/2 returns from its {error, Errors, Warnings} tuple.
format_errors_unbound_var_test() ->
    Errors = [{"my_module", [{none, core_lint, {unbound_var, 'State', {foo, 1}}}]}],
    Formatted = beamtalk_compile_diagnostics:format_errors(Errors),
    ?assert(is_binary(Formatted)),
    ?assertEqual(nomatch, binary:match(Formatted, <<"{unbound_var,">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"unbound variable">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"'State'">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"foo/1">>)),
    ?assertEqual(<<"my_module: unbound variable 'State' in foo/1\n">>, Formatted).

%% Multiple errors across files/lines all render, one line each.
format_errors_multiple_test() ->
    Errors = [
        {"mod_a", [{none, core_lint, {duplicate_var, 'I', {bar, 2}}}]},
        {"mod_b", [{none, core_lint, {unbound_var, 'X', {baz, 0}}}]}
    ],
    Formatted = beamtalk_compile_diagnostics:format_errors(Errors),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"duplicate variable 'I' in bar/2">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"unbound variable 'X' in baz/0">>)).

%% A binary file identifier (as some compile:forms/2 callers may produce)
%% is accepted the same as a string one.
format_errors_binary_file_test() ->
    Errors = [{<<"my_module">>, [{none, core_lint, {unbound_var, 'Y', {qux, 3}}}]}],
    Formatted = beamtalk_compile_diagnostics:format_errors(Errors),
    ?assertNotEqual(
        nomatch, binary:match(Formatted, <<"my_module: unbound variable 'Y' in qux/3">>)
    ).

%% Empty error list renders to an empty binary.
format_errors_empty_test() ->
    ?assertEqual(<<>>, beamtalk_compile_diagnostics:format_errors([])).
