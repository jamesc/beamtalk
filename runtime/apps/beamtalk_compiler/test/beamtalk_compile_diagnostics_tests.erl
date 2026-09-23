%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compile_diagnostics_tests).

-moduledoc """
Tests for beamtalk_compile_diagnostics.
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
    ?assertEqual(
        <<
            "Beamtalk compiler bug: code generation produced Core Erlang that does not "
            "compile. This is a bug in the beamtalk compiler itself, NOT an error in your "
            ".bt source; please file an issue with a minimal repro: "
            "https://github.com/jamesc/beamtalk/issues/new\n\n"
            "my_module: unbound variable 'State' in foo/1\n"
        >>,
        Formatted
    ).

%% Every non-empty result is prefixed with a clear "this is a compiler bug,
%% not your source" header — the whole point of BT-3581's follow-up.
format_errors_prepends_bug_header_test() ->
    Errors = [{"my_module", [{none, core_lint, {unbound_var, 'State', {foo, 1}}}]}],
    Formatted = beamtalk_compile_diagnostics:format_errors(Errors),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Beamtalk compiler bug">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"NOT an error in your .bt source">>)),
    %% The header must come first, before any per-error detail.
    {Start, _Len} = binary:match(Formatted, <<"Beamtalk compiler bug">>),
    ?assertEqual(0, Start).

%% A `'class_<selector>'' Core Erlang export name (ADR 0032 — what a
%% class-side method actually compiles to) is demangled to `class method
%% '<selector>''` so the message names what the `.bt` author wrote, not the
%% internal export.
format_errors_demangles_class_method_test() ->
    Errors = [
        {"my_module", [
            {none, core_lint, {unbound_var, 'State', {'class_handleCancel:engine:', 4}}}
        ]}
    ],
    Formatted = beamtalk_compile_diagnostics:format_errors(Errors),
    ?assertNotEqual(
        nomatch, binary:match(Formatted, <<"class method 'handleCancel:engine:'">>)
    ),
    ?assertEqual(nomatch, binary:match(Formatted, <<"'class_handleCancel:engine:'">>)).

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

%% format_warnings/1 — same rendering as format_errors/1 but with "Warning: " prefix.

format_warnings_unbound_var_test() ->
    Warnings = [{"my_module", [{none, core_lint, {unbound_var, 'State', {foo, 1}}}]}],
    Formatted = beamtalk_compile_diagnostics:format_warnings(Warnings),
    ?assert(is_binary(Formatted)),
    ?assertEqual(nomatch, binary:match(Formatted, <<"{unbound_var,">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Warning: ">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"unbound variable">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"'State'">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"foo/1">>)),
    ?assertEqual(<<"my_module: Warning: unbound variable 'State' in foo/1\n">>, Formatted).

format_warnings_multiple_test() ->
    Warnings = [
        {"mod_a", [{none, core_lint, {duplicate_var, 'I', {bar, 2}}}]},
        {"mod_b", [{none, core_lint, {unbound_var, 'X', {baz, 0}}}]}
    ],
    Formatted = beamtalk_compile_diagnostics:format_warnings(Warnings),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Warning: ">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"duplicate variable 'I' in bar/2">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"unbound variable 'X' in baz/0">>)).

%% A binary file identifier is accepted the same as a string one.
format_warnings_binary_file_test() ->
    Warnings = [{<<"my_module">>, [{none, core_lint, {unbound_var, 'Y', {qux, 3}}}]}],
    Formatted = beamtalk_compile_diagnostics:format_warnings(Warnings),
    ?assertNotEqual(
        nomatch, binary:match(Formatted, <<"my_module: Warning: unbound variable 'Y' in qux/3">>)
    ).

%% Empty warning list renders to an empty binary.
format_warnings_empty_test() ->
    ?assertEqual(<<>>, beamtalk_compile_diagnostics:format_warnings([])).

%% print_warnings/1 — empty list returns ok without writing.
print_warnings_empty_test() ->
    ?assertEqual(ok, beamtalk_compile_diagnostics:print_warnings([])).

%% print_warnings/1 — non-empty list writes to stderr and returns ok.
print_warnings_nonempty_test() ->
    Warnings = [{"my_module", [{none, core_lint, {unbound_var, 'State', {foo, 1}}}]}],
    ?assertEqual(ok, beamtalk_compile_diagnostics:print_warnings(Warnings)).
