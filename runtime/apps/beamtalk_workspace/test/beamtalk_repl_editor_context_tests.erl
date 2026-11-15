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

%%====================================================================
%% Moved from beamtalk_repl_server_tests / beamtalk_repl_ops_dev_tests (BT-4900)
%%====================================================================

parse_receiver_class_and_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer s">>)
    ).

parse_receiver_class_empty_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer ">>)
    ).

parse_no_receiver_single_word_test() ->
    ?assertEqual(
        {undefined, <<"Counter">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Counter">>)
    ).

parse_no_receiver_empty_test() ->
    ?assertEqual(
        {undefined, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<>>)
    ).

parse_receiver_string_literal_test() ->
    ?assertEqual(
        {<<"\"hello\"">>, <<"up">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"\"hello\" up">>)
    ).

parse_receiver_keyword_selector_test() ->
    %% Colons are identifier chars — "ifT:" is the prefix, not just "ifT"
    ?assertEqual(
        {<<"Integer">>, <<"ifT:">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer ifT:">>)
    ).

parse_receiver_multi_keyword_selector_test() ->
    %% Multi-keyword selectors like "ifTrue:ifFalse:" complete as a unit
    ?assertEqual(
        {<<"Boolean">>, <<"ifTrue:ifFalse:">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Boolean ifTrue:ifFalse:">>)
    ).

tokenise_send_chain_simple_chain_test() ->
    %% Single hop unary chain
    ?assertEqual(
        {ok, <<"\"hello\"">>, [size]},
        beamtalk_repl_editor_context:tokenise_send_chain(<<"\"hello\" size">>)
    ).

tokenise_send_chain_multi_hop_test() ->
    %% Two-hop unary chain
    ?assertEqual(
        {ok, <<"counter">>, [getValue, reversed]},
        beamtalk_repl_editor_context:tokenise_send_chain(<<"counter getValue reversed">>)
    ).

tokenise_send_chain_single_token_returns_error_test() ->
    %% Single token — no sends to chain
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"hello">>)).

tokenise_send_chain_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<>>)).

tokenise_send_chain_keyword_mid_chain_returns_error_test() ->
    %% Keyword send mid-chain is not a unary chain
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"myList inject: 0">>)).

tokenise_send_chain_paren_returns_error_test() ->
    %% Parenthesised subexpression — not supported
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"(myList size)">>)).

parse_receiver_multi_token_expression_test() ->
    %% Multi-token receiver returns {expression, ReceiverExpr, Prefix}
    ?assertEqual(
        {expression, <<"\"hello\" size">>, <<"cl">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"\"hello\" size cl">>)
    ).

parse_receiver_multi_token_empty_prefix_test() ->
    %% Multi-token expression with trailing space (empty prefix)
    ?assertEqual(
        {expression, <<"\"hello\" size">>, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"\"hello\" size ">>)
    ).

parse_receiver_multi_token_three_tokens_test() ->
    %% Three-token expression with trailing space
    ?assertEqual(
        {expression, <<"counter getValue reversed">>, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"counter getValue reversed ">>)
    ).

parse_receiver_leading_whitespace_single_token_test() ->
    %% Leading whitespace must NOT trigger multi-token expression path
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"  Integer s">>)
    ).

walk_chain_empty_selectors_string_test() ->
    %% walk_chain with no selectors returns the class unchanged — no registry call
    ?assertEqual({ok, 'String', instance}, beamtalk_repl_editor_context:walk_chain('String', [])).

walk_chain_single_hop_found_test() ->
    Pid = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainTestA', #{size => 'Integer'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'Integer', instance},
            beamtalk_repl_editor_context:walk_chain('WalkChainTestA', [size])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainTestA', Pid)
    end.

walk_chain_broken_chain_test() ->
    %% Selector not in return types → mock returns {error, not_found} → undefined.
    %% Uses 'size' (a known atom) so tokenise_send_chain can convert it; the mock
    %% has no return type for it, so the registry returns {error, not_found}.
    Pid = beamtalk_test_mock_class:spawn_with_return_types('WalkChainTestB', #{}, #{}),
    try
        ?assertEqual(
            undefined,
            beamtalk_repl_editor_context:walk_chain('WalkChainTestB', [size])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainTestB', Pid)
    end.

walk_chain_class_single_hop_found_test() ->
    Pid = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainClassTestA', #{}, #{new => 'WalkChainClassTestA'}
    ),
    try
        ?assertEqual(
            {ok, 'WalkChainClassTestA', instance},
            beamtalk_repl_editor_context:walk_chain_class('WalkChainClassTestA', [new])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainClassTestA', Pid)
    end.

%% `ClassName class` chain — `class` is an instance method on ProtoObject
%% (no class-side return-type annotation), but sending it to a class object returns
%% the metaclass, which has the same class-side methods. The chain should stay on the
%% class side rather than returning undefined.
walk_chain_class_class_selector_stays_on_class_side_test() ->
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_editor_context:walk_chain_class('Integer', [class])
    ).

walk_chain_class_class_then_method_test() ->
    Pid = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainClassB', #{}, #{new => 'WalkChainClassB'}
    ),
    try
        %% `WalkChainClassB class new` — `class` stays on class side, then `new` resolves
        ?assertEqual(
            {ok, 'WalkChainClassB', instance},
            beamtalk_repl_editor_context:walk_chain_class('WalkChainClassB', [class, new])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainClassB', Pid)
    end.

walk_chain_multi_hop_test() ->
    %% Two-hop chain: WalkChainMultiA →size→ WalkChainMultiB →reversed→ String
    PidA = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainMultiA', #{size => 'WalkChainMultiB'}, #{}
    ),
    PidB = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainMultiB', #{reversed => 'String'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'String', instance},
            beamtalk_repl_editor_context:walk_chain('WalkChainMultiA', [size, reversed])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainMultiA', PidA),
        beamtalk_test_mock_class:cleanup('WalkChainMultiB', PidB)
    end.

walk_chain_superclass_inheritance_test() ->
    %% 'WalkChainSub' has no return type for 'size'; its superclass 'WalkChainSuper'
    %% does.  get_method_return_type/2 walks the ETS hierarchy table, so walk_chain/2
    %% should resolve through the superclass.
    beamtalk_test_boot:registry(),
    Saved = ets:tab2list(beamtalk_class_metadata),
    ets:delete_all_objects(beamtalk_class_metadata),
    beamtalk_class_metadata:insert(
        'WalkChainSub', undefined, undefined, 'WalkChainSuper', undefined
    ),
    PidSub = beamtalk_test_mock_class:spawn_with_return_types('WalkChainSub', #{}, #{}),
    PidSuper = beamtalk_test_mock_class:spawn_with_return_types(
        'WalkChainSuper', #{size => 'Integer'}, #{}
    ),
    try
        ?assertEqual(
            {ok, 'Integer', instance},
            beamtalk_repl_editor_context:walk_chain('WalkChainSub', [size])
        )
    after
        beamtalk_test_mock_class:cleanup('WalkChainSub', PidSub),
        beamtalk_test_mock_class:cleanup('WalkChainSuper', PidSuper),
        ets:delete_all_objects(beamtalk_class_metadata),
        ets:insert(beamtalk_class_metadata, Saved),
        %% A raw restore bypasses the metadata API, so drop derived
        %% facts cached against the test rows (BT-4064).
        beamtalk_class_derived_cache:invalidate()
    end.

parse_empty_returns_undefined_empty_test() ->
    ?assertEqual({undefined, <<>>}, beamtalk_repl_editor_context:parse_receiver_and_prefix(<<>>)).

parse_bare_prefix_returns_undefined_test() ->
    ?assertEqual(
        {undefined, <<"Int">>}, beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Int">>)
    ).

parse_single_char_prefix_test() ->
    ?assertEqual(
        {undefined, <<"s">>}, beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"s">>)
    ).

parse_receiver_and_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer s">>)
    ).

parse_receiver_empty_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer ">>)
    ).

parse_receiver_integer_literal_test() ->
    ?assertEqual(
        {<<"42">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"42 s">>)
    ).

parse_multitoken_returns_expression_test() ->
    ?assertMatch(
        {expression, _, <<"c">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"\"hello\" size c">>)
    ).

parse_multitoken_expression_content_test() ->
    {expression, Expr, Prefix} = beamtalk_repl_editor_context:parse_receiver_and_prefix(
        <<"\"hello\" size c">>
    ),
    ?assertEqual(<<"\"hello\" size">>, Expr),
    ?assertEqual(<<"c">>, Prefix).

parse_leading_whitespace_single_token_test() ->
    %% Leading whitespace before a single receiver should not create expression path
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"  Integer s">>)
    ).

parse_keyword_selector_as_prefix_test() ->
    %% Colons are identifier chars, so "ifTrue:" is a single prefix token
    ?assertEqual(
        {<<"x">>, <<"ifTrue:">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"x ifTrue:">>)
    ).

%% Parenthesised subexpressions as receivers
parse_paren_binary_send_returns_expression_test() ->
    %% ("foo" ++ "bar") cla<TAB> — paren expression is multi-token, must use expression path
    ?assertMatch(
        {expression, _, <<"cla">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"(\"foo\" ++ \"bar\") cla">>)
    ).

parse_paren_binary_send_expression_content_test() ->
    %% Verify the ReceiverExpr captures the full parenthesised expression
    {expression, Expr, Prefix} = beamtalk_repl_editor_context:parse_receiver_and_prefix(
        <<"(\"foo\" ++ \"bar\") cla">>
    ),
    ?assertEqual(<<"(\"foo\" ++ \"bar\")">>, Expr),
    ?assertEqual(<<"cla">>, Prefix).

parse_paren_unary_send_returns_expression_test() ->
    %% (myList size) sq<TAB>
    ?assertMatch(
        {expression, _, <<"sq">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"(myList size) sq">>)
    ).

parse_paren_unary_send_expression_content_test() ->
    {expression, Expr, Prefix} = beamtalk_repl_editor_context:parse_receiver_and_prefix(
        <<"(myList size) sq">>
    ),
    ?assertEqual(<<"(myList size)">>, Expr),
    ?assertEqual(<<"sq">>, Prefix).

parse_paren_expr_empty_prefix_test() ->
    %% ("foo" ++ "bar") <TAB> — empty prefix, expression path
    ?assertMatch(
        {expression, _, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"(\"foo\" ++ \"bar\") ">>)
    ).

tokenise_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<>>)).

tokenise_single_token_returns_error_test() ->
    %% Single token means no send chain -- nothing to walk
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"Integer">>)).

tokenise_two_token_unary_test() ->
    %% "size" is a known atom in Erlang/OTP
    Result = beamtalk_repl_editor_context:tokenise_send_chain(<<"\"hello\" size">>),
    ?assertMatch({ok, <<"\"hello\"">>, _}, Result),
    {ok, _, Selectors} = Result,
    ?assert(lists:member(size, Selectors)).

tokenise_paren_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"(myList size)">>)).

tokenise_bitshift_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_send_chain(<<"x >> y">>)).

tokenise_keyword_send_returns_error_test() ->
    %% Selectors with colons are keyword sends, not valid unary
    ?assertEqual(
        error, beamtalk_repl_editor_context:tokenise_send_chain(<<"list inject: 0 into:">>)
    ).

walk_chain_empty_selectors_test() ->
    %% With no selectors, walk_chain returns {ok, ClassName, instance} immediately
    ?assertEqual({ok, 'Integer', instance}, beamtalk_repl_editor_context:walk_chain('Integer', [])).

walk_chain_unknown_selector_test() ->
    %% Non-existent registry -> not_found -> undefined
    Result = beamtalk_repl_editor_context:walk_chain('Integer', [unknownSelector]),
    ?assertEqual(undefined, Result).

walk_chain_class_transition_test() ->
    %% `instance class` transitions to class side for completions
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_editor_context:walk_chain('Integer', [class])
    ).

walk_chain_class_empty_selectors_test() ->
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_editor_context:walk_chain_class('Integer', [])
    ).

walk_chain_class_meta_selector_test() ->
    %% `class` selector on a class object stays on the class side
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_editor_context:walk_chain_class('Integer', [class])
    ).

walk_chain_class_multi_class_selectors_test() ->
    ?assertEqual(
        {ok, 'Foo', class}, beamtalk_repl_editor_context:walk_chain_class('Foo', [class, class])
    ).

walk_chain_class_unknown_selector_test() ->
    Result = beamtalk_repl_editor_context:walk_chain_class('Integer', [unknownMethod]),
    ?assertEqual(undefined, Result).

tokenise_binary_chain_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_binary_chain(<<>>)).

tokenise_binary_chain_single_token_returns_error_test() ->
    %% Single token — no chain to walk
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"counter">>)).

tokenise_binary_chain_unary_only_succeeds_test() ->
    %% tokenise_binary_chain also accepts pure unary chains (superset of tokenise_send_chain).
    %% The difference is that tokenise_send_chain returns [atom()] while
    %% tokenise_binary_chain returns [{unary, atom()}].
    %% `size` is a known atom (used widely in Erlang/OTP)
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"x size">>),
    ?assertMatch({ok, <<"x">>, [{unary, size}]}, Result).

tokenise_binary_chain_with_plus_test() ->
    %% `+` is a known Erlang atom (used as BIF name erlang:'+'/2)
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"counter value + 1">>),
    ?assertMatch({ok, <<"counter">>, _}, Result),
    {ok, _, Hops} = Result,
    ?assertEqual([{unary, value}, {binary, '+'}], Hops).

tokenise_binary_chain_single_binary_op_test() ->
    %% `+` is a known Erlang atom (used as BIF name erlang:'+'/2),
    %% so we can safely test a simple binary operator chain here.
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"42 + 1">>),
    ?assertMatch({ok, <<"42">>, [{binary, '+'}]}, Result).

tokenise_binary_chain_multi_binary_test() ->
    %% `x + 1 * 2` → receiver x, hops [{binary,'+'}, {binary,'*'}]
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"x + 1 * 2">>),
    ?assertMatch({ok, <<"x">>, [{binary, '+'}, {binary, '*'}]}, Result).

tokenise_binary_chain_mixed_hops_test() ->
    %% `myList size + offset` → receiver myList, hops [{unary,size},{binary,'+'}]
    %% `size` is a known atom (used all over the codebase)
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"myList size + offset">>),
    ?assertMatch({ok, <<"myList">>, [{unary, size}, {binary, '+'}]}, Result).

tokenise_binary_chain_paren_returns_error_test() ->
    ?assertEqual(
        error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"(myList size) + 1">>)
    ).

tokenise_binary_chain_bitshift_returns_error_test() ->
    %% >> is an invalid chain character
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"x >> y">>)).

tokenise_binary_chain_binary_op_no_arg_returns_error_test() ->
    %% Binary op at end of expression with no argument
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"counter value +">>)).

tokenise_binary_chain_keyword_arg_returns_error_test() ->
    %% Keyword-style arg token (contains `:`) after a binary op must be rejected
    ?assertEqual(error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"x + ifTrue:">>)).

tokenise_binary_chain_keyword_returns_error_test() ->
    %% Keyword sends (tokens with `:`) are not valid unary selectors
    ?assertEqual(
        error, beamtalk_repl_editor_context:tokenise_binary_chain(<<"list inject: 0 into: x">>)
    ).

tokenise_binary_chain_compound_selector_test() ->
    %% Multi-character binary selectors like `<=` are valid
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"42 <= 10">>),
    ?assertMatch({ok, <<"42">>, [{binary, '<='}]}, Result).

tokenise_binary_chain_equality_selector_test() ->
    %% `=:=` is a three-character binary selector
    Result = beamtalk_repl_editor_context:tokenise_binary_chain(<<"42 =:= 42">>),
    ?assertMatch({ok, <<"42">>, [{binary, '=:='}]}, Result).

walk_mixed_chain_empty_hops_test() ->
    %% No hops — returns the starting class immediately
    ?assertEqual(
        {ok, 'Integer', instance}, beamtalk_repl_editor_context:walk_mixed_chain('Integer', [])
    ).

walk_mixed_chain_unknown_unary_selector_test() ->
    %% No registry — get_method_return_type returns not_found → undefined
    Result = beamtalk_repl_editor_context:walk_mixed_chain('Integer', [{unary, unknownSel}]),
    ?assertEqual(undefined, Result).

walk_mixed_chain_unknown_binary_selector_test() ->
    %% Graceful fallback: a binary selector not in method_return_types → undefined
    Result = beamtalk_repl_editor_context:walk_mixed_chain('Integer', [{binary, 'noSuchBinOp'}]),
    ?assertEqual(undefined, Result).

walk_mixed_chain_class_empty_hops_test() ->
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_editor_context:walk_mixed_chain_class('Integer', [])
    ).

walk_mixed_chain_class_meta_selector_test() ->
    %% `{unary, class}` on a class object stays on the class side
    ?assertEqual(
        {ok, 'Integer', class},
        beamtalk_repl_editor_context:walk_mixed_chain_class('Integer', [{unary, class}])
    ).

walk_mixed_chain_class_unknown_selector_test() ->
    Result = beamtalk_repl_editor_context:walk_mixed_chain_class('Integer', [{unary, unknownMethod}]),
    ?assertEqual(undefined, Result).

parse_tab_separated_test() ->
    %% Tabs are treated as whitespace separators
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"Integer\ts">>)
    ).

parse_underscore_prefix_test() ->
    %% Underscores are identifier chars
    ?assertEqual(
        {undefined, <<"_foo">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"_foo">>)
    ).

parse_multi_keyword_selector_test() ->
    %% Multi-keyword selector like "ifTrue:ifFalse:" is one prefix token
    ?assertEqual(
        {<<"x">>, <<"ifTrue:ifFalse:">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"x ifTrue:ifFalse:">>)
    ).

parse_keyword_send_mid_chain_returns_expression_test() ->
    %% `myList collect: [:x | x * 2] si<TAB>`
    %% The full receiver expression is everything before `si`.
    ?assertMatch(
        {expression, _, <<"si">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(
            <<"myList collect: [:x | x * 2] si">>
        )
    ).

parse_keyword_send_mid_chain_expression_content_test() ->
    %% Verify the ReceiverExpr captures the full keyword send expression
    {expression, Expr, Prefix} = beamtalk_repl_editor_context:parse_receiver_and_prefix(
        <<"myList collect: [:x | x * 2] si">>
    ),
    ?assertEqual(<<"myList collect: [:x | x * 2]">>, Expr),
    ?assertEqual(<<"si">>, Prefix).

parse_keyword_send_inject_into_returns_expression_test() ->
    %% `myList inject: 0 into: [:acc :x | acc + x] pr<TAB>`
    {expression, Expr, Prefix} = beamtalk_repl_editor_context:parse_receiver_and_prefix(
        <<"myList inject: 0 into: [:acc :x | acc + x] pr">>
    ),
    ?assertEqual(<<"myList inject: 0 into: [:acc :x | acc + x]">>, Expr),
    ?assertEqual(<<"pr">>, Prefix).

parse_qualified_name_as_receiver_test() ->
    %% "json@Parser " should parse as receiver="json@Parser", prefix=<<>>
    %% (the @ is now an identifier char so it stays as one token)
    ?assertEqual(
        {<<"json@Parser">>, <<>>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"json@Parser ">>)
    ).

parse_qualified_name_with_prefix_test() ->
    %% "json@Parser pa" should parse as receiver="json@Parser", prefix="pa"
    ?assertEqual(
        {<<"json@Parser">>, <<"pa">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"json@Parser pa">>)
    ).

parse_bare_qualified_name_test() ->
    %% "json@Parser" with no trailing space is a bare prefix (no receiver)
    ?assertEqual(
        {undefined, <<"json@Parser">>},
        beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"json@Parser">>)
    ).

%% Conformance: every case in the shared corpus must classify
%% identically here and in the Rust side's `is_completion_word_char`
%% (`crates/beamtalk-core/src/source_analysis/mod.rs`). The corpus is the
%% single source of truth both implementations are pinned to; the Rust side
%% asserts the identical cases in
%% `source_analysis::completion_word_boundary_tests::completion_word_char_matches_shared_corpus`.
is_identifier_char_matches_shared_corpus_test() ->
    Cases = load_fixture("completion_word_boundary_corpus.json"),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(Case) ->
            [Char | _] = unicode:characters_to_list(maps:get(<<"char">>, Case)),
            Expected = maps:get(<<"is_word_char">>, Case),
            Why = maps:get(<<"why">>, Case, <<>>),
            ?assertEqual(
                Expected,
                beamtalk_repl_editor_context:is_identifier_char(Char),
                {corpus_mismatch, Char, Why}
            )
        end,
        Cases
    ).

%% Conformance: completing a pipeline stage, the REPL/MCP/LiveView engine
%% (`parse_receiver_and_prefix/1`) and the Rust LSP (`pipeline_receiver_end`
%% in `crates/beamtalk-language-service/src/queries/completion_provider.rs`)
%% must agree on the stage's receiver: the value before the last `|>`. Both
%% assert the cases in the shared corpus; the Rust side in
%% `pipeline_receiver_matches_shared_corpus`.
pipeline_receiver_matches_shared_corpus_test() ->
    Cases = load_fixture("completion_pipeline_receiver_corpus.json"),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(Case) ->
            Line = maps:get(<<"line">>, Case),
            Expected = {maps:get(<<"receiver">>, Case), maps:get(<<"prefix">>, Case)},
            Actual =
                case beamtalk_repl_editor_context:parse_receiver_and_prefix(Line) of
                    {expression, Receiver, Prefix} -> {Receiver, Prefix};
                    {Receiver, Prefix} -> {Receiver, Prefix}
                end,
            ?assertEqual(Expected, Actual, {corpus_mismatch, Line})
        end,
        Cases
    ).

pipeline_with_nothing_before_has_no_receiver_test() ->
    ?assertEqual(
        {undefined, <<"ab">>}, beamtalk_repl_editor_context:parse_receiver_and_prefix(<<"|> ab">>)
    ).

tokenise_send_chain_drops_pipeline_tokens_test() ->
    %% `"hello" |> size abs` is the unary chain `"hello" size abs`, so it
    %% resolves without the compiler port.
    ?assertEqual(
        {ok, <<"\"hello\"">>, [size, abs]},
        beamtalk_repl_editor_context:tokenise_send_chain(<<"\"hello\" |> size |> abs">>)
    ).

tokenise_binary_chain_drops_pipeline_tokens_test() ->
    ?assertEqual(
        {ok, <<"x">>, [{binary, '+'}, {unary, abs}]},
        beamtalk_repl_editor_context:tokenise_binary_chain(<<"x + 1 |> abs">>)
    ).
