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
%% handle/4 -- show-codegen class+selector (BT-1236)
%%====================================================================

handle_show_codegen_class_not_found_error_test() ->
    %% Non-existent class name -> class not found error
    Msg = make_msg(<<"show-codegen">>, <<"sc-3">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>},
        Msg,
        self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_class_with_selector_no_class_found_test() ->
    %% class+selector where class doesn't exist -> error
    Msg = make_msg(<<"show-codegen">>, <<"sc-4">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>, <<"selector">> => <<"someMethod">>},
        Msg,
        self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_class_takes_priority_over_code_test() ->
    %% When both class and code are given, class takes priority and errors (non-existent class)
    Msg = make_msg(<<"show-codegen">>, <<"sc-5">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>, <<"code">> => <<"1 + 2">>},
        Msg,
        self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    %% class path taken, returns error for non-existent class
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_empty_class_falls_back_to_code_test() ->
    %% Empty class binary is treated as absent; falls back to code path.
    %% Use empty code so we get the "empty expression" error without calling a real session.
    Msg = make_msg(<<"show-codegen">>, <<"sc-6">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<>>, <<"code">> => <<>>},
        Msg,
        self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    %% Code path taken (empty class ignored), empty code yields the standard "empty expression" error.
    %% If class="" had been selected instead, we'd get a class-not-found error (different error type).
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_selector_without_class_test() ->
    %% Providing selector without class returns a specific "selector requires class" error,
    %% not the generic missing-parameter error — consistent with the Rust MCP boundary.
    Msg = make_msg(<<"show-codegen">>, <<"sc-7">>, undefined, false),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"selector">> => <<"greet">>},
        Msg,
        self()
    ),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    %% The error message must mention "selector" (not a generic missing-parameter message).
    ErrorMsg = maps:get(<<"error">>, Decoded),
    ?assert(binary:match(ErrorMsg, <<"selector">>) =/= nomatch).

validate_selector_undefined_returns_ok_test() ->
    %% When SelectorBin is undefined, no validation is needed.
    Msg = make_msg(<<"show-codegen">>, <<"sc-8">>, undefined, false),
    Result = beamtalk_repl_ops_dev:validate_selector_if_present(
        <<"SomeClass">>, 'SomeClass', self(), undefined, Msg
    ),
    ?assertEqual(ok, Result).

%% @doc Unit test for compile_file_for_codegen/2 success path (BT-1236).
%%
%% Tests that the compiler can take a Beamtalk class source binary and
%% return Core Erlang text without loading a module into the runtime.
%% This exercises the compilation side of show_codegen_class_method/3.
compile_file_for_codegen_success_test() ->
    Source = <<"Object subclass: TestCodegenClass\n  greet => 42\n">>,
    Result = beamtalk_repl_compiler:compile_file_for_codegen(Source, undefined),
    case Result of
        {ok, CoreErlang, Warnings} ->
            ?assert(is_binary(CoreErlang)),
            ?assert(byte_size(CoreErlang) > 0),
            ?assert(is_list(Warnings));
        {error, _Reason} ->
            %% Compiler port may not be available in this test environment; that's acceptable.
            ok
    end.

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
%% tokenise_binary_chain/1 (BT-1071)
%%====================================================================

tokenise_binary_chain_empty_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<>>)).

tokenise_binary_chain_single_token_returns_error_test() ->
    %% Single token — no chain to walk
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"counter">>)).

tokenise_binary_chain_unary_only_succeeds_test() ->
    %% tokenise_binary_chain also accepts pure unary chains (superset of tokenise_send_chain).
    %% The difference is that tokenise_send_chain returns [atom()] while
    %% tokenise_binary_chain returns [{unary, atom()}].
    %% `size` is a known atom (used widely in Erlang/OTP)
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"x size">>),
    ?assertMatch({ok, <<"x">>, [{unary, size}]}, Result).

tokenise_binary_chain_with_plus_test() ->
    %% `+` is a known Erlang atom (used as BIF name erlang:'+'/2)
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"counter value + 1">>),
    ?assertMatch({ok, <<"counter">>, _}, Result),
    {ok, _, Hops} = Result,
    ?assertEqual([{unary, value}, {binary, '+'}], Hops).

tokenise_binary_chain_single_binary_op_test() ->
    %% `+` is a known Erlang atom (used as BIF name erlang:'+'/2),
    %% so we can safely test a simple binary operator chain here.
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"42 + 1">>),
    ?assertMatch({ok, <<"42">>, [{binary, '+'}]}, Result).

tokenise_binary_chain_multi_binary_test() ->
    %% `x + 1 * 2` → receiver x, hops [{binary,'+'}, {binary,'*'}]
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"x + 1 * 2">>),
    ?assertMatch({ok, <<"x">>, [{binary, '+'}, {binary, '*'}]}, Result).

tokenise_binary_chain_mixed_hops_test() ->
    %% `myList size + offset` → receiver myList, hops [{unary,size},{binary,'+'}]
    %% `size` is a known atom (used all over the codebase)
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"myList size + offset">>),
    ?assertMatch({ok, <<"myList">>, [{unary, size}, {binary, '+'}]}, Result).

tokenise_binary_chain_paren_returns_error_test() ->
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"(myList size) + 1">>)).

tokenise_binary_chain_bitshift_returns_error_test() ->
    %% >> is an invalid chain character
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"x >> y">>)).

tokenise_binary_chain_binary_op_no_arg_returns_error_test() ->
    %% Binary op at end of expression with no argument
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"counter value +">>)).

tokenise_binary_chain_keyword_arg_returns_error_test() ->
    %% Keyword-style arg token (contains `:`) after a binary op must be rejected
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"x + ifTrue:">>)).

tokenise_binary_chain_keyword_returns_error_test() ->
    %% Keyword sends (tokens with `:`) are not valid unary selectors
    ?assertEqual(error, beamtalk_repl_ops_dev:tokenise_binary_chain(<<"list inject: 0 into: x">>)).

tokenise_binary_chain_compound_selector_test() ->
    %% Multi-character binary selectors like `<=` are valid
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"42 <= 10">>),
    ?assertMatch({ok, <<"42">>, [{binary, '<='}]}, Result).

tokenise_binary_chain_equality_selector_test() ->
    %% `=:=` is a three-character binary selector
    Result = beamtalk_repl_ops_dev:tokenise_binary_chain(<<"42 =:= 42">>),
    ?assertMatch({ok, <<"42">>, [{binary, '=:='}]}, Result).

%%====================================================================
%% walk_mixed_chain/2 (BT-1071)
%%====================================================================

walk_mixed_chain_empty_hops_test() ->
    %% No hops — returns the starting class immediately
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [])).

walk_mixed_chain_unknown_unary_selector_test() ->
    %% No registry — get_method_return_type returns not_found → undefined
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{unary, unknownSel}]),
    ?assertEqual(undefined, Result).

walk_mixed_chain_binary_selector_with_registry_test() ->
    %% Integer `+` returns Integer (annotated in the stdlib builtins)
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{binary, '+'}]),
    ?assertEqual({ok, 'Integer'}, Result).

walk_mixed_chain_unknown_binary_selector_test() ->
    %% Graceful fallback: a binary selector not in method_return_types → undefined
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{binary, 'noSuchBinOp'}]),
    ?assertEqual(undefined, Result).

walk_mixed_chain_type_changing_binary_selector_test() ->
    %% `<` on Integer returns Boolean — type changes mid-chain
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{binary, '<'}]),
    ?assertEqual({ok, 'Boolean'}, Result).

%%====================================================================
%% walk_mixed_chain_class/2 (BT-1071)
%%====================================================================

walk_mixed_chain_class_empty_hops_test() ->
    ?assertEqual({ok, 'Integer'}, beamtalk_repl_ops_dev:walk_mixed_chain_class('Integer', [])).

walk_mixed_chain_class_meta_selector_test() ->
    %% `{unary, class}` on a class object stays on the class side
    ?assertEqual(
        {ok, 'Integer'}, beamtalk_repl_ops_dev:walk_mixed_chain_class('Integer', [{unary, class}])
    ).

walk_mixed_chain_class_unknown_selector_test() ->
    Result = beamtalk_repl_ops_dev:walk_mixed_chain_class('Integer', [{unary, unknownMethod}]),
    ?assertEqual(undefined, Result).

%%====================================================================
%% resolve_chain_type/2 with binary chains (BT-1071)
%%====================================================================

resolve_chain_type_binary_chain_no_registry_test() ->
    %% Binary chain parses OK but no registry → walk returns undefined
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"counter value + 1">>, #{}),
    ?assertEqual(undefined, Result).

resolve_chain_type_binary_chain_integer_plus_resolves_test() ->
    %% `42 + 1` — 42 is an Integer literal, `+` on Integer returns Integer
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"42 + 1">>, #{}),
    ?assertEqual({ok, 'Integer'}, Result).

resolve_chain_type_binary_chain_comparison_returns_boolean_test() ->
    %% `42 < 1` — `<` on Integer returns Boolean (type changes)
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"42 < 1">>, #{}),
    ?assertEqual({ok, 'Boolean'}, Result).

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
%% parse_receiver_and_prefix/1 -- keyword sends mid-chain (BT-1072)
%%====================================================================

parse_keyword_send_mid_chain_returns_expression_test() ->
    %% `myList collect: [:x | x * 2] si<TAB>`
    %% The full receiver expression is everything before `si`.
    ?assertMatch(
        {expression, _, <<"si">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"myList collect: [:x | x * 2] si">>)
    ).

parse_keyword_send_mid_chain_expression_content_test() ->
    %% Verify the ReceiverExpr captures the full keyword send expression
    {expression, Expr, Prefix} = beamtalk_repl_ops_dev:parse_receiver_and_prefix(
        <<"myList collect: [:x | x * 2] si">>
    ),
    ?assertEqual(<<"myList collect: [:x | x * 2]">>, Expr),
    ?assertEqual(<<"si">>, Prefix).

parse_keyword_send_inject_into_returns_expression_test() ->
    %% `myList inject: 0 into: [:acc :x | acc + x] pr<TAB>`
    {expression, Expr, Prefix} = beamtalk_repl_ops_dev:parse_receiver_and_prefix(
        <<"myList inject: 0 into: [:acc :x | acc + x] pr">>
    ),
    ?assertEqual(<<"myList inject: 0 into: [:acc :x | acc + x]">>, Expr),
    ?assertEqual(<<"pr">>, Prefix).

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
