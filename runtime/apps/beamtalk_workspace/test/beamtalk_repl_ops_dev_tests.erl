%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_repl_ops_dev module.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Tests pure parsing functions (parse_receiver_and_prefix, tokenise_send_chain),
%%% chain-resolution helpers (walk_chain, walk_chain_class, resolve_chain_type),
%%% protocol helpers (base_protocol_response, make_class_not_found_error),
%%% and handle/4 operations that do not require a running workspace.

-module(beamtalk_repl_ops_dev_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op, Id, Session, Legacy) ->
    {protocol_msg, Op, Id, Session, #{}, Legacy}.

%%====================================================================
%% parse_receiver_and_prefix/1
%%====================================================================

parse_empty_returns_undefined_empty_test() ->
    ?assertEqual({undefined, <<>>}, beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<>>)).

parse_bare_prefix_returns_undefined_test() ->
    ?assertEqual(
        {undefined, <<"Int">>}, beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Int">>)
    ).

parse_single_char_prefix_test() ->
    ?assertEqual({undefined, <<"s">>}, beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"s">>)).

parse_receiver_and_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer s">>)
    ).

parse_receiver_empty_prefix_test() ->
    ?assertEqual(
        {<<"Integer">>, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer ">>)
    ).

parse_receiver_integer_literal_test() ->
    ?assertEqual(
        {<<"42">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"42 s">>)
    ).

parse_multitoken_returns_expression_test() ->
    ?assertMatch(
        {expression, _, <<"c">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"\"hello\" size c">>)
    ).

parse_multitoken_expression_content_test() ->
    {expression, Expr, Prefix} = beamtalk_repl_ops_dev:parse_receiver_and_prefix(
        <<"\"hello\" size c">>
    ),
    ?assertEqual(<<"\"hello\" size">>, Expr),
    ?assertEqual(<<"c">>, Prefix).

parse_leading_whitespace_single_token_test() ->
    %% Leading whitespace before a single receiver should not create expression path
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"  Integer s">>)
    ).

parse_keyword_selector_as_prefix_test() ->
    %% Colons are identifier chars, so "ifTrue:" is a single prefix token
    ?assertEqual(
        {<<"x">>, <<"ifTrue:">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"x ifTrue:">>)
    ).

%% BT-1070: parenthesised subexpressions as receivers
parse_paren_binary_send_returns_expression_test() ->
    %% ("foo" ++ "bar") cla<TAB> — paren expression is multi-token, must use expression path
    ?assertMatch(
        {expression, _, <<"cla">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"(\"foo\" ++ \"bar\") cla">>)
    ).

parse_paren_binary_send_expression_content_test() ->
    %% Verify the ReceiverExpr captures the full parenthesised expression
    {expression, Expr, Prefix} = beamtalk_repl_ops_dev:parse_receiver_and_prefix(
        <<"(\"foo\" ++ \"bar\") cla">>
    ),
    ?assertEqual(<<"(\"foo\" ++ \"bar\")">>, Expr),
    ?assertEqual(<<"cla">>, Prefix).

parse_paren_unary_send_returns_expression_test() ->
    %% (myList size) sq<TAB>
    ?assertMatch(
        {expression, _, <<"sq">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"(myList size) sq">>)
    ).

parse_paren_unary_send_expression_content_test() ->
    {expression, Expr, Prefix} = beamtalk_repl_ops_dev:parse_receiver_and_prefix(
        <<"(myList size) sq">>
    ),
    ?assertEqual(<<"(myList size)">>, Expr),
    ?assertEqual(<<"sq">>, Prefix).

parse_paren_expr_empty_prefix_test() ->
    %% ("foo" ++ "bar") <TAB> — empty prefix, expression path
    ?assertMatch(
        {expression, _, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"(\"foo\" ++ \"bar\") ">>)
    ).

%%====================================================================
%% tokenise_send_chain/1
%%====================================================================

tokenise_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<>>)).

tokenise_single_token_returns_error_test() ->
    %% Single token means no send chain -- nothing to walk
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"Integer">>)).

tokenise_two_token_unary_test() ->
    %% "size" is a known atom in Erlang/OTP
    Result = beamtalk_repl_ops_dev:tokenise_send_chain(<<"\"hello\" size">>),
    ?assertMatch({ok, <<"\"hello\"">>, _}, Result),
    {ok, _, Selectors} = Result,
    ?assert(lists:member(size, Selectors)).

tokenise_paren_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"(myList size)">>)).

tokenise_bitshift_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"x >> y">>)).

tokenise_keyword_send_returns_error_test() ->
    %% Selectors with colons are keyword sends, not valid unary
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_send_chain(<<"list inject: 0 into:">>)).

%%====================================================================
%% base_protocol_response/1
%%====================================================================

base_protocol_response_with_id_and_session_test() ->
    Msg = make_msg(<<"eval">>, <<"req-1">>, <<"sess-a">>, false),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(<<"req-1">>, maps:get(<<"id">>, Result)),
    ?assertEqual(<<"sess-a">>, maps:get(<<"session">>, Result)).

base_protocol_response_no_id_test() ->
    Msg = make_msg(<<"eval">>, undefined, <<"sess-b">>, false),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(error, maps:find(<<"id">>, Result)),
    ?assertEqual(<<"sess-b">>, maps:get(<<"session">>, Result)).

base_protocol_response_no_session_test() ->
    Msg = make_msg(<<"eval">>, <<"req-2">>, undefined, false),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(<<"req-2">>, maps:get(<<"id">>, Result)),
    ?assertEqual(error, maps:find(<<"session">>, Result)).

base_protocol_response_neither_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined, false),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(#{}, Result).

%%====================================================================
%% make_class_not_found_error/1
%%====================================================================

make_class_not_found_error_atom_test() ->
    Err = beamtalk_repl_ops_dev:make_class_not_found_error('NoSuchClass'),
    ?assertEqual(class_not_found, Err#beamtalk_error.kind),
    ?assertEqual('REPL', Err#beamtalk_error.class),
    ?assert(is_binary(Err#beamtalk_error.message)),
    ?assert(binary:match(Err#beamtalk_error.message, <<"NoSuchClass">>) =/= nomatch).

make_class_not_found_error_binary_test() ->
    Err = beamtalk_repl_ops_dev:make_class_not_found_error(<<"UnknownBin">>),
    ?assertEqual(class_not_found, Err#beamtalk_error.kind),
    ?assert(binary:match(Err#beamtalk_error.message, <<"UnknownBin">>) =/= nomatch),
    ?assert(is_binary(Err#beamtalk_error.hint)).

%%====================================================================
%% list_class_methods_for_ws/1 -- unknown class
%%====================================================================

list_class_methods_for_ws_unknown_atom_test() ->
    %% A class name that has never been loaded -- safe_to_existing_atom returns badarg
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(
        <<"NonExistentClass99999">>
    ),
    ?assertEqual([], Result).

list_class_methods_for_ws_empty_binary_test() ->
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<>>),
    ?assertEqual([], Result).

%%====================================================================
%% get_completions/1 -- empty path
%%====================================================================

get_completions_empty_returns_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_dev:get_completions(<<>>)).

%%====================================================================
%% get_context_completions/1 and /2 -- empty paths
%%====================================================================

get_context_completions_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_dev:get_context_completions(<<>>)).

get_context_completions_with_bindings_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_dev:get_context_completions(<<>>, #{})).

%%====================================================================
%% handle/4 -- describe op (no runtime needed)
%%====================================================================

handle_describe_returns_ops_map_test() ->
    Msg = make_msg(<<"describe">>, <<"d-1">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"ops">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

handle_describe_contains_eval_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-2">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"eval">>, Ops)).

handle_describe_contains_versions_test() ->
    Msg = make_msg(<<"describe">>, <<"d-3">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    Versions = maps:get(<<"versions">>, Decoded),
    ?assert(maps:is_key(<<"protocol">>, Versions)),
    ?assert(maps:is_key(<<"beamtalk">>, Versions)).

handle_describe_legacy_format_test() ->
    Msg = make_msg(<<"describe">>, undefined, undefined, true),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"describe">>, maps:get(<<"type">>, Decoded)),
    ?assert(maps:is_key(<<"ops">>, Decoded)).

%%====================================================================
%% handle/4 -- show-codegen with empty code
%%====================================================================

handle_show_codegen_empty_code_error_test() ->
    Msg = make_msg(<<"show-codegen">>, <<"sc-1">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

handle_show_codegen_missing_code_error_test() ->
    Msg = make_msg(<<"show-codegen">>, <<"sc-2">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(<<"show-codegen">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% handle/4 -- complete with old protocol (no cursor field)
%%====================================================================

handle_complete_legacy_empty_prefix_test() ->
    %% Old protocol: no "cursor" field, empty code -> empty completions
    Msg = make_msg(<<"complete">>, undefined, undefined, true),
    Result = beamtalk_repl_ops_dev:handle(
        <<"complete">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"completions">>, maps:get(<<"type">>, Decoded)),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)).

%%====================================================================
%% handle/4 -- methods with unknown class
%%====================================================================

handle_methods_unknown_class_test() ->
    Msg = make_msg(<<"methods">>, <<"m-1">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"methods">>, #{<<"class">> => <<"NonExistentXyz9999">>}, Msg, self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([], maps:get(<<"methods">>, Decoded)),
    ?assertEqual([], maps:get(<<"state_vars">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% resolve_chain_type/2 -- no runtime (all paths return undefined)
%%====================================================================

resolve_chain_type_no_registry_test() ->
    %% With no class registry running, all type lookups return undefined
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"x size">>, #{}),
    ?assertEqual(undefined, Result).

resolve_chain_type_keyword_send_test() ->
    %% Keyword sends cannot be parsed as unary chains
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"list inject: 0 into:">>, #{}),
    ?assertEqual(undefined, Result).

resolve_chain_type_single_token_test() ->
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"Integer">>, #{}),
    ?assertEqual(undefined, Result).

%%====================================================================
%% walk_chain/2 -- without running registry
%%====================================================================

walk_chain_empty_selectors_test() ->
    %% With no selectors, walk_chain returns {ok, ClassName} immediately
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_chain('Integer', [])).

walk_chain_unknown_selector_test() ->
    %% Non-existent registry -> not_found -> undefined
    Result = beamtalk_repl_ops_dev:walk_chain('Integer', [unknownSelector]),
    ?assertEqual(undefined, Result).

%%====================================================================
%% walk_chain_class/2 -- without running registry
%%====================================================================

walk_chain_class_empty_selectors_test() ->
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [])).

walk_chain_class_meta_selector_test() ->
    %% `class` selector on a class object stays on the class side
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [class])).

walk_chain_class_multi_class_selectors_test() ->
    ?assertEqual({ok, 'Foo'}, beamtalk_repl_ops_dev:walk_chain_class('Foo', [class, class])).

walk_chain_class_unknown_selector_test() ->
    Result = beamtalk_repl_ops_dev:walk_chain_class('Integer', [unknownMethod]),
    ?assertEqual(undefined, Result).

%%====================================================================
%% handle/4 -- docs with non-existing class (badarg path)
%%====================================================================

handle_docs_unknown_class_returns_error_test() ->
    Msg = make_msg(<<"docs">>, <<"doc-1">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"docs">>, #{<<"class">> => <<"NonExistentDocClass999">>}, Msg, self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% parse_receiver_and_prefix/1 -- additional edge cases
%%====================================================================

parse_tab_separated_test() ->
    %% Tabs are treated as whitespace separators
    ?assertEqual(
        {<<"Integer">>, <<"s">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"Integer\ts">>)
    ).

parse_underscore_prefix_test() ->
    %% Underscores are identifier chars
    ?assertEqual(
        {undefined, <<"_foo">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"_foo">>)
    ).

parse_multi_keyword_selector_test() ->
    %% Multi-keyword selector like "ifTrue:ifFalse:" is one prefix token
    ?assertEqual(
        {<<"x">>, <<"ifTrue:ifFalse:">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"x ifTrue:ifFalse:">>)
    ).

%%====================================================================
%% handle/4 -- describe deprecated ops
%%====================================================================

handle_describe_deprecated_ops_have_migrate_to_test() ->
    Msg = make_msg(<<"describe">>, <<"d-dep">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = jsx:decode(Result, [return_maps]),
    Ops = maps:get(<<"ops">>, Decoded),
    DocsOp = maps:get(<<"docs">>, Ops),
    ?assertEqual(true, maps:get(<<"deprecated">>, DocsOp)),
    ?assert(maps:is_key(<<"migrate_to">>, DocsOp)).

%%====================================================================
%% handle/4 -- complete new format with no cursor
%%====================================================================

handle_complete_new_format_empty_prefix_test() ->
    %% New format without cursor field falls back to get_completions
    Msg = make_msg(<<"complete">>, <<"c-1">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"complete">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).
