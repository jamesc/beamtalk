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
    %% Anchor at the repo root so paths are stable regardless of cwd, and so a
    %% git failure is caught here rather than surfacing as bogus "files" later
    %% (os:cmd folds stderr into its output, so an error message would otherwise
    %% be mistaken for a filename and the lint would report a false pass).
    ok = goto_repo_root(),
    Files = erlang_sources(),
    Files =:= [] andalso
        begin
            io:format(standard_error, "❌ No Erlang sources found under the repository root.~n", []),
            halt(1)
        end,
    io:format("🔍 Linting ~b Erlang source file(s) for non-ASCII in binary literals...~n", [
        length(Files)
    ]),
    Results = [scan_file(F) || F <- Files],
    Offenders = lists:append([O || {ok, O} <- Results]),
    Skipped = [{F, Why} || {skipped, F, Why} <- Results],
    %% A file this lint could not parse is not a file this lint has checked. Say
    %% so rather than letting it pass silently, but do not fail the build: an
    %% intentionally-malformed fixture is legitimate, and a real syntax error
    %% fails the compile anyway.
    Skipped =:= [] orelse
        begin
            io:format(standard_error, "~n⚠️  ~b file(s) could not be parsed and were NOT checked:~n", [
                length(Skipped)
            ]),
            [io:format(standard_error, "  ~ts (~p)~n", [F, W]) || {F, W} <- Skipped],
            io:format(standard_error, "~n", [])
        end,
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

goto_repo_root() ->
    case git(["rev-parse", "--show-toplevel"]) of
        [Root] ->
            file:set_cwd(Root);
        _ ->
            io:format(standard_error, "❌ Not a git repository - cannot enumerate sources.~n", []),
            halt(1)
    end.

%% Tracked files plus untracked-but-not-ignored ones, so a new offender is
%% caught before it is committed.
erlang_sources() ->
    Pats = ["*.erl", "*.hrl", "*.escript"],
    Tracked = git(["ls-files" | Pats]),
    Untracked = git(["ls-files", "--others", "--exclude-standard" | Pats]),
    lists:usort(Tracked ++ Untracked).

%% stderr is discarded rather than folded into the result: os:cmd merges it into
%% stdout, where a git diagnostic would be indistinguishable from a filename.
git(Args) ->
    Cmd = "git " ++ lists:join(" ", [quote(A) || A <- Args]) ++ " 2>/dev/null",
    [L || L <- string:lexemes(os:cmd(Cmd), "\n"), L =/= ""].

quote(A) -> "'" ++ A ++ "'".

%% ── Scanning ────────────────────────────────────────────────────────────────

scan_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            case unicode:characters_to_list(Bin, utf8) of
                Chars when is_list(Chars) ->
                    scan_tokens(File, Chars);
                _ ->
                    {skipped, File, not_utf8}
            end;
        {error, Reason} ->
            {skipped, File, Reason}
    end.

scan_tokens(File, Chars) ->
    case erl_scan:string(Chars, 1) of
        {ok, Tokens, _} -> {ok, walk(Tokens, 0, File, [])};
        {error, {_, _, Reason}, _} -> {skipped, File, Reason}
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
