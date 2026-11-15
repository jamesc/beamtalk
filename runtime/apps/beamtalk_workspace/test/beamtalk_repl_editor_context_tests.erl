%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_editor_context_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for the pure lexical rules in `beamtalk_repl_editor_context`:
the lexeme skippers, the send-chain tokenisers and receiver-literal
classification. Each is pinned to the Rust lexer by a shared JSON corpus in
`test/fixtures/` (BT-4916); the Rust side asserts the same files in
`crates/beamtalk-core/src/source_analysis/lexer.rs`.
""".

-include_lib("eunit/include/eunit.hrl").

load_fixture(Name) ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime", "apps", "beamtalk_workspace", "test", "fixtures", Name
    ]),
    ?assert(length(Cases) > 0),
    Cases.

%%====================================================================
%% Lexeme skippers
%%====================================================================

%% An interpolation or nested string left unterminated is `unsupported`.
skip_string_literal_unterminated_interpolation_test() ->
    ?assertEqual(unsupported, beamtalk_repl_editor_context:skip_string_literal("\"a {b")),
    ?assertEqual(unsupported, beamtalk_repl_editor_context:skip_string_literal("\"a {b \"c")),
    ?assertEqual(unsupported, beamtalk_repl_editor_context:skip_string_literal("\"a {b /* c")).

%% (Per AGENTS.md Essential Rules: a "mirrors" claim across the
%% Rust/Erlang boundary needs a shared conformance fixture, not
%% just a comment): `skip_string_literal/1`/`skip_character_literal/1` are
%% hand-rolled Erlang mirrors of the Rust lexer's `lex_string/0`/
%% `lex_character/0` span computation. Both sides run the exact same cases
%% from `test/fixtures/string_and_character_literal_span_corpus.json` — see
%% `source_analysis::lexer::tests::string_and_character_literal_span_matches_shared_corpus`
%% for the Rust side.
string_and_character_literal_span_matches_shared_corpus_test() ->
    Cases = load_fixture("string_and_character_literal_span_corpus.json"),
    ?assert(length(Cases) > 0),
    lists:foreach(fun assert_literal_span_case/1, Cases).

assert_literal_span_case(#{
    <<"kind">> := Kind,
    <<"source">> := SourceBin,
    <<"expected_end">> := ExpectedEnd,
    <<"why">> := Why
}) ->
    Source = unicode:characters_to_list(SourceBin),
    Remaining =
        case Kind of
            <<"string">> ->
                {ok, R} = beamtalk_repl_editor_context:skip_string_literal(Source),
                R;
            <<"character">> ->
                beamtalk_repl_editor_context:skip_character_literal(Source)
        end,
    Consumed = length(Source) - length(Remaining),
    ?assertEqual(
        ExpectedEnd,
        Consumed,
        lists:flatten(io_lib:format("span-end mismatch for ~p (~ts)", [Source, Why]))
    ).

%% (Per AGENTS.md Essential Rules: a "mirrors" claim across the
%% Rust/Erlang boundary needs a shared conformance fixture, not just a
%% comment): `skip_line_comment/1`/`skip_block_comment/1` are hand-rolled
%% Erlang mirrors of the Rust lexer's `lex_line_comment/0`/
%% `lex_block_comment/0` span computation. Both sides run the exact same
%% cases from `test/fixtures/comment_span_corpus.json` — see
%% `source_analysis::lexer::tests::comment_span_matches_shared_corpus` for
%% the Rust side.
comment_span_matches_shared_corpus_test() ->
    Cases = load_fixture("comment_span_corpus.json"),
    ?assert(length(Cases) > 0),
    lists:foreach(fun assert_comment_span_case/1, Cases).

assert_comment_span_case(#{
    <<"kind">> := Kind,
    <<"source">> := SourceBin,
    <<"expected_end">> := ExpectedEnd,
    <<"why">> := Why
}) ->
    Source = unicode:characters_to_list(SourceBin),
    Remaining =
        case Kind of
            <<"line_comment">> ->
                beamtalk_repl_editor_context:skip_line_comment(Source);
            <<"block_comment">> ->
                beamtalk_repl_editor_context:skip_block_comment(Source)
        end,
    Consumed = length(Source) - length(Remaining),
    ?assertEqual(
        ExpectedEnd,
        Consumed,
        lists:flatten(io_lib:format("span-end mismatch for ~p (~ts)", [Source, Why]))
    ).

%%====================================================================
%% tokenise_send_chain/1 and tokenise_binary_chain/1 — shared corpus
%%====================================================================

%% Every case in `completion_send_chain_corpus.json` must tokenise here exactly
%% as its `unary` / `binary` expectation says. The Rust side derives the chain
%% from the real lexer's tokens and must agree with the case's `lexer_view`
%% (or, absent one, the same expectation); a case with a `divergence` note is
%% an intended difference. See
%% `source_analysis::lexer::tests::completion_send_chain_matches_shared_corpus`.
send_chain_matches_shared_corpus_test() ->
    Cases = load_fixture("completion_send_chain_corpus.json"),
    lists:foreach(fun assert_send_chain_case/1, Cases).

assert_send_chain_case(#{<<"expr">> := Expr} = Case) ->
    %% The tokenisers use `binary_to_existing_atom/2`; intern the selectors the
    %% corpus expects so the case does not depend on what else is loaded.
    intern_selectors(Case),
    Unary = maps:get(<<"unary">>, Case),
    Binary = maps:get(<<"binary">>, Case),
    ?assertEqual(
        expected_unary(Unary),
        beamtalk_repl_editor_context:tokenise_send_chain(Expr),
        {unary_corpus_mismatch, Expr}
    ),
    ?assertEqual(
        expected_binary(Binary),
        beamtalk_repl_editor_context:tokenise_binary_chain(Expr),
        {binary_corpus_mismatch, Expr}
    ).

intern_selectors(Case) ->
    Unary = maps:get(<<"unary">>, Case),
    Binary = maps:get(<<"binary">>, Case),
    Names =
        case Unary of
            null -> [];
            #{<<"selectors">> := Sels} -> Sels
        end ++
            case Binary of
                null -> [];
                #{<<"hops">> := Hops} -> [S || [_, S] <- Hops]
            end,
    lists:foreach(fun(N) -> binary_to_atom(N, utf8) end, Names).

expected_unary(null) ->
    error;
expected_unary(#{<<"receiver">> := Receiver, <<"selectors">> := Sels}) ->
    {ok, Receiver, [binary_to_atom(S, utf8) || S <- Sels]}.

expected_binary(null) ->
    error;
expected_binary(#{<<"receiver">> := Receiver, <<"hops">> := Hops}) ->
    {ok, Receiver, [{binary_to_atom(Kind, utf8), binary_to_atom(S, utf8)} || [Kind, S] <- Hops]}.

%%====================================================================
%% receiver_literal_class/1 — shared corpus
%%====================================================================

%% See `source_analysis::lexer::tests::completion_receiver_literal_matches_shared_corpus`.
receiver_literal_class_matches_shared_corpus_test() ->
    Cases = load_fixture("completion_receiver_literal_corpus.json"),
    lists:foreach(
        fun(#{<<"receiver">> := Receiver, <<"class">> := Class}) ->
            Expected =
                case Class of
                    null -> undefined;
                    Name -> binary_to_atom(Name, utf8)
                end,
            ?assertEqual(
                Expected,
                beamtalk_repl_editor_context:receiver_literal_class(Receiver),
                {corpus_mismatch, Receiver}
            )
        end,
        Cases
    ).
