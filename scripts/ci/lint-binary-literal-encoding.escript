#!/usr/bin/env escript
%% -*- erlang -*-
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Lint: reject non-ASCII characters inside Erlang binary literals that lack a
%% UTF-8 type specifier.
%%
%% Rationale (BT-3026): binary string literals have *byte* semantics, so
%% `<<"—">>` truncates U+2014 (8212) to its low 8 bits — 0x14, a DC4 control
%% character. The compiler emits no warning, so every error hint written with a
%% typographic dash reached the user silently mangled. Adding `/utf8` (or
%% `/utf16`, `/utf32`) encodes the character properly:
%%
%%     <<"a — b">>          %% BROKEN: em-dash becomes 0x14
%%     <<"a — b"/utf8>>     %% OK:     encoded as UTF-8 bytes
%%     <<"a; b">>           %% OK:     ASCII only
%%
%% Plain (non-binary) strings are unaffected — they are lists of codepoints, so
%% `?LOG_ERROR("a — b")` and `-doc "a — b"` are fine and are not flagged.
%%
%% ── How to clear a failure ──────────────────────────────────────────────────
%% Either rewrite the literal in ASCII (preferred for error messages and hints;
%% use `;` where an em-dash joined two clauses), or append `/utf8` when the
%% character is genuinely needed (arrows in the inspector, Unicode test data).
%%
%% This lint tokenises with `erl_scan` rather than grepping, so it is exact:
%% it sees through comments, escapes, and binaries spanning multiple lines.
%%
%% Usage:
%%   escript scripts/ci/lint-binary-literal-encoding.escript

-mode(compile).

main(_Args) ->
    %% Offender snippets are printed verbatim, so both devices must speak Unicode.
    io:setopts(standard_io, [{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    Files = erlang_sources(),
    Offenders = lists:flatmap(fun scan_file/1, Files),
    io:format("🔍 Linting ~b Erlang source file(s) for non-ASCII in binary literals...~n", [
        length(Files)
    ]),
    case Offenders of
        [] ->
            io:format("✅ No non-ASCII binary literals missing /utf8.~n"),
            halt(0);
        _ ->
            io:format(standard_error, "~n❌ Non-ASCII character(s) in binary literal without /utf8:~n~n", []),
            lists:foreach(fun report/1, Offenders),
            io:format(
                standard_error,
                "~nBinary literals are bytes: a non-ASCII character is truncated to its low 8~n"
                "bits and reaches the user as a control character.~n~n"
                "Fix: rewrite the literal in ASCII (use `;` where an em-dash joined two~n"
                "     clauses), or append /utf8 when the character is genuinely needed.~n"
                "     See the header of this script for details.~n",
                []
            ),
            halt(1)
    end.

report({File, Line, Chars, Snippet}) ->
    Described = lists:join(", ", [describe(C) || C <- Chars]),
    io:format(standard_error, "  ~ts:~b: ~ts~n      contains ~ts~n", [
        File, Line, Snippet, Described
    ]).

describe(C) ->
    lists:flatten(io_lib:format("U+~4.16.0B '~ts'", [C, [C]])).

%% ── File discovery ──────────────────────────────────────────────────────────

%% Tracked files plus untracked-but-not-ignored ones, so a new offender is
%% caught before it is committed.
erlang_sources() ->
    Tracked = git(["ls-files", "*.erl", "*.hrl", "*.escript"]),
    Untracked = git(["ls-files", "--others", "--exclude-standard", "*.erl", "*.hrl", "*.escript"]),
    lists:usort(Tracked ++ Untracked).

git(Args) ->
    Cmd = "git " ++ lists:join(" ", [quote(A) || A <- Args]),
    Out = os:cmd(Cmd),
    [L || L <- string:lexemes(Out, "\n"), L =/= ""].

quote(A) -> "'" ++ A ++ "'".

%% ── Scanning ────────────────────────────────────────────────────────────────

scan_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case unicode:characters_to_list(Bin, utf8) of
                Chars when is_list(Chars) ->
                    scan_tokens(File, Chars);
                _ ->
                    %% Not valid UTF-8; leave it to the compiler to complain.
                    []
            end;
        {error, _} ->
            []
    end.

scan_tokens(File, Chars) ->
    case erl_scan:string(Chars, 1) of
        {ok, Tokens, _} -> walk(Tokens, 0, File, []);
        {error, _, _} -> []
    end.

%% Walk the token stream tracking binary-literal nesting depth. Inside a binary
%% (`Depth > 0`), a string or character token holding a non-ASCII codepoint is an
%% offender unless its type-specifier list names a Unicode encoding.
walk([], _Depth, _File, Acc) ->
    lists:reverse(Acc);
walk([{'<<', _} | Rest], Depth, File, Acc) ->
    walk(Rest, Depth + 1, File, Acc);
walk([{'>>', _} | Rest], Depth, File, Acc) ->
    walk(Rest, max(0, Depth - 1), File, Acc);
walk([Tok | Rest], Depth, File, Acc) when Depth > 0 ->
    case literal_codepoints(Tok) of
        none ->
            walk(Rest, Depth, File, Acc);
        {Line, Cps, Snippet} ->
            NonAscii = [C || C <- Cps, C > 127],
            case NonAscii =/= [] andalso not unicode_spec(Rest) of
                true -> walk(Rest, Depth, File, [{File, Line, NonAscii, Snippet} | Acc]);
                false -> walk(Rest, Depth, File, Acc)
            end
    end;
walk([_ | Rest], Depth, File, Acc) ->
    walk(Rest, Depth, File, Acc).

literal_codepoints({string, Anno, S}) ->
    {erl_anno:line(Anno), S, ["\"", S, "\""]};
literal_codepoints({char, Anno, C}) ->
    {erl_anno:line(Anno), [C], ["$", [C]]};
literal_codepoints(_) ->
    none.

%% A segment's type specifiers follow as `/ Spec ('-' Spec)*`. Any of the
%% `utf8`/`utf16`/`utf32` types encodes codepoints correctly.
unicode_spec([{'/', _} | Rest]) -> specs_have_unicode(Rest);
unicode_spec(_) -> false.

specs_have_unicode([{atom, _, Type} | Rest]) ->
    case lists:member(Type, [utf8, utf16, utf32]) of
        true -> true;
        false -> continue_specs(Rest)
    end;
%% `unit:8` and friends arrive as separate tokens; keep walking the spec list.
specs_have_unicode([{integer, _, _} | Rest]) ->
    continue_specs(Rest);
specs_have_unicode(_) ->
    false.

continue_specs([{'-', _} | Rest]) -> specs_have_unicode(Rest);
continue_specs([{':', _} | Rest]) -> specs_have_unicode(Rest);
continue_specs(_) -> false.
