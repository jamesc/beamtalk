%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_reshape_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Pure-Erlang semantics (no compiler port required)
%%====================================================================

identity_on_empty_base_indent_test() ->
    %% An empty base indent is the identity transform for canonical (column-0)
    %% input.
    Src = <<"foo => 42">>,
    ?assertEqual(Src, beamtalk_workspace_reshape:reindent_method_source(Src, <<>>)).

prepends_base_indent_test() ->
    ?assertEqual(
        <<"  foo => 42">>,
        beamtalk_workspace_reshape:reindent_method_source(<<"foo => 42">>, <<"  ">>)
    ).

preserves_relative_indentation_test() ->
    %% Canonical body with a 2-space relative step; reshaped to a 4-space base
    %% keeps the relative step (6 spaces on the inner line).
    Src = <<"foo =>\n  ^bar">>,
    ?assertEqual(
        <<"    foo =>\n      ^bar">>,
        beamtalk_workspace_reshape:reindent_method_source(Src, <<"    ">>)
    ).

strips_min_indent_then_reindents_test() ->
    %% A pre-indented body: the least non-blank indent (2) is stripped from every
    %% line before the base indent is prepended.
    Src = <<"  foo =>\n    ^bar">>,
    ?assertEqual(
        <<"foo =>\n  ^bar">>,
        beamtalk_workspace_reshape:reindent_method_source(Src, <<>>)
    ).

blank_lines_stay_empty_test() ->
    %% A whitespace-only line between a doc comment and the method body is
    %% emitted empty — no base indent, no trailing whitespace.
    Src = <<"/// doc\n   \nfoo => 42">>,
    ?assertEqual(
        <<"  /// doc\n\n  foo => 42">>,
        beamtalk_workspace_reshape:reindent_method_source(Src, <<"  ">>)
    ).

trailing_newline_preserved_test() ->
    %% A trailing newline leaves a final empty segment that stays empty.
    ?assertEqual(
        <<"  foo => 42\n">>,
        beamtalk_workspace_reshape:reindent_method_source(<<"foo => 42\n">>, <<"  ">>)
    ).

all_blank_lines_use_zero_min_indent_test() ->
    %% No non-blank line ⇒ min indent is 0 (Rust's `.unwrap_or(0)'); blank lines
    %% stay empty regardless of base indent.
    ?assertEqual(
        <<"\n\n">>,
        beamtalk_workspace_reshape:reindent_method_source(<<"   \n\t\n">>, <<"  ">>)
    ).

leading_ws_test() ->
    ?assertEqual(<<>>, beamtalk_workspace_reshape:leading_ws(<<"foo">>)),
    ?assertEqual(<<"  ">>, beamtalk_workspace_reshape:leading_ws(<<"  foo">>)),
    ?assertEqual(<<"\t ">>, beamtalk_workspace_reshape:leading_ws(<<"\t foo\n  bar">>)).

strip_trailing_newlines_test() ->
    ?assertEqual(<<>>, beamtalk_workspace_reshape:strip_trailing_newlines(<<>>)),
    ?assertEqual(<<"a">>, beamtalk_workspace_reshape:strip_trailing_newlines(<<"a\n\n">>)),
    ?assertEqual(<<"a">>, beamtalk_workspace_reshape:strip_trailing_newlines(<<"a">>)).

ensure_trailing_newline_test() ->
    ?assertEqual(<<"\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<>>)),
    ?assertEqual(<<"a\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<"a">>)),
    ?assertEqual(<<"a\n">>, beamtalk_workspace_reshape:ensure_trailing_newline(<<"a\n">>)).

%%====================================================================
%% Byte-identical parity with the Rust reference (BT-2592)
%%
%% `beamtalk_workspace_reshape:reindent_method_source/2' is a pure-Erlang mirror
%% of `beamtalk_core::unparse::reindent_method_source' (reached here via the
%% compiler port `beamtalk_compiler:reindent_method_source/2'). The Rust corpus
%% round-trip test (`corpus_reshape_round_trip_is_byte_identical') covers real
%% corpus methods; this sweep pins the two implementations to byte-for-byte
%% agreement across the whitespace structures the algorithm actually branches on
%% (varied indentation widths, tabs, blank lines, doc comments, trailing
%% newlines) crossed with a range of base indents.
%%====================================================================

parity_test_() ->
    {setup, fun setup_compiler/0, fun teardown_compiler/1, fun(_) ->
        [
            {parity_label(Src, Indent), fun() -> assert_parity(Src, Indent) end}
         || Src <- sample_sources(), Indent <- sample_indents()
        ]
    end}.

setup_compiler() ->
    application:ensure_all_started(compiler),
    %% `ensure_all_started/1` returns `{ok, _}` (including `{ok, []}` when the apps
    %% are already running) — the `{already_started, _}` tuple is from
    %% `application:start/1`, not this call. Surface a genuine start failure
    %% (missing port binary, crashed dependency) instead of `badmatch`-crashing.
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, Reason} -> error({compiler_start_failed, Reason})
    end,
    ok.

teardown_compiler(_) ->
    %% Leave the shared EUnit node at the baseline "compiler not running" state,
    %% matching beamtalk_repl_loader_tests' teardown.
    _ = application:stop(beamtalk_compiler),
    ok.

assert_parity(Src, Indent) ->
    Pure = beamtalk_workspace_reshape:reindent_method_source(Src, Indent),
    {ok, Rust} = beamtalk_compiler:reindent_method_source(Src, Indent),
    ?assertEqual(Rust, Pure).

parity_label(Src, Indent) ->
    lists:flatten(
        io_lib:format("parity src=~p indent=~p", [Src, Indent])
    ).

%% Representative method sources: canonical column-0 bodies (as `unparse_method'
%% emits), pre-indented bodies, doc comments with blank separators, tabs, and
%% trailing-newline variants.
sample_sources() ->
    [
        <<"foo => 42">>,
        <<"foo => 42\n">>,
        <<"area =>\n  ^width * height">>,
        <<"area =>\n  ^width * height\n">>,
        <<"/// Returns the area.\nareaOf: shape =>\n  ^shape width * shape height">>,
        <<"/// Doc line one.\n///\n/// Doc line three.\nfoo =>\n  ^bar">>,
        <<"foo =>\n  x := 1\n\n  ^x + 1">>,
        <<"  foo =>\n    ^already indented">>,
        <<"\tfoo =>\n\t\t^tab indented">>,
        <<"foo =>\n  bar do: [ :each |\n    each println ]">>,
        <<"   \nfoo => 1\n   \n">>,
        <<>>,
        <<"\n">>
    ].

%% Base indents the install hook / flush derive from disk: none, the stdlib
%% 2-space convention, deeper nesting, and a tab.
sample_indents() ->
    [<<>>, <<"  ">>, <<"    ">>, <<"      ">>, <<"\t">>].
