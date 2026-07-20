%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_dev_tests).
-compile(nowarn_deprecated_catch).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for beamtalk_repl_ops_dev module.

Tests pure parsing functions (parse_receiver_and_prefix, tokenise_send_chain),
chain-resolution helpers (walk_chain, walk_chain_class, resolve_chain_type),
protocol helpers (base_protocol_response, make_class_not_found_error),
and handle/4 operations that do not require a running workspace.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

make_msg(Op, Id, Session) ->
    {protocol_msg, Op, Id, Session, #{}}.

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
    Msg = make_msg(<<"eval">>, <<"req-1">>, <<"sess-a">>),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(<<"req-1">>, maps:get(<<"id">>, Result)),
    ?assertEqual(<<"sess-a">>, maps:get(<<"session">>, Result)).

base_protocol_response_no_id_test() ->
    Msg = make_msg(<<"eval">>, undefined, <<"sess-b">>),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(error, maps:find(<<"id">>, Result)),
    ?assertEqual(<<"sess-b">>, maps:get(<<"session">>, Result)).

base_protocol_response_no_session_test() ->
    Msg = make_msg(<<"eval">>, <<"req-2">>, undefined),
    Result = beamtalk_repl_ops_dev:base_protocol_response(Msg),
    ?assertEqual(<<"req-2">>, maps:get(<<"id">>, Result)),
    ?assertEqual(error, maps:find(<<"session">>, Result)).

base_protocol_response_neither_test() ->
    Msg = make_msg(<<"eval">>, undefined, undefined),
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
    Msg = make_msg(<<"describe">>, <<"d-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"ops">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

handle_describe_contains_eval_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"eval">>, Ops)).

%% BT-2557: the load-tests op is advertised so clients can discover the
%% test-runner pane's "Load tests" affordance.
handle_describe_contains_load_tests_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-2b">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"load-tests">>, Ops)).

handle_describe_contains_versions_test() ->
    Msg = make_msg(<<"describe">>, <<"d-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Versions = maps:get(<<"versions">>, Decoded),
    ?assert(maps:is_key(<<"protocol">>, Versions)),
    ?assert(maps:is_key(<<"beamtalk">>, Versions)).

%%====================================================================
%% handle/4 -- show-codegen with empty code
%%====================================================================

handle_show_codegen_empty_code_error_test() ->
    Msg = make_msg(<<"show-codegen">>, <<"sc-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ?assertEqual([<<"done">>, <<"error">>], maps:get(<<"status">>, Decoded)).

handle_show_codegen_missing_code_error_test() ->
    Msg = make_msg(<<"show-codegen">>, <<"sc-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"show-codegen">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% handle_term/4 + handle/4 -- diagnostics (BT-2556)
%%
%% Only the empty-buffer + encoding paths are exercised here: they short-circuit
%% before the Rust compiler port (which a bare EUnit run does not start), so the
%% routing, the `{diagnostics, _}` term shape, and the JSON encoding are all
%% covered without a running workspace. The real parse path is covered by the
%% facade test (mock) and the browser e2e (live workspace).
%%====================================================================

diagnostics_empty_code_returns_empty_term_test() ->
    Msg = make_msg(<<"diagnostics">>, <<"dg-1">>, undefined),
    ?assertEqual(
        {diagnostics, []},
        beamtalk_repl_ops_dev:handle_term(
            <<"diagnostics">>, #{<<"code">> => <<>>}, Msg, self()
        )
    ).

diagnostics_missing_code_returns_empty_term_test() ->
    %% No `code` key at all defaults to an empty buffer -> no diagnostics, no port.
    Msg = make_msg(<<"diagnostics">>, <<"dg-2">>, undefined),
    ?assertEqual(
        {diagnostics, []},
        beamtalk_repl_ops_dev:handle_term(<<"diagnostics">>, #{}, Msg, self())
    ).

diagnostics_non_binary_mode_returns_empty_term_test() ->
    %% BT-2569: a non-binary `mode` (a raw TCP/MCP client could send a JSON
    %% number) degrades to [] at the Erlang boundary via the diagnostics_for/2
    %% catch-all, rather than crashing the session. No compiler/port call is made,
    %% so this is covered without a running workspace.
    Msg = make_msg(<<"diagnostics">>, <<"dg-4">>, undefined),
    ?assertEqual(
        {diagnostics, []},
        beamtalk_repl_ops_dev:handle_term(
            <<"diagnostics">>, #{<<"code">> => <<"x">>, <<"mode">> => 42}, Msg, self()
        )
    ).

diagnostics_empty_code_encodes_done_status_test() ->
    %% handle/4 runs the term through encode_diagnostics -> WebSocket JSON shape.
    Msg = make_msg(<<"diagnostics">>, <<"dg-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"diagnostics">>, #{<<"code">> => <<>>}, Msg, self()),
    Decoded = json:decode(Result),
    ?assertEqual([], maps:get(<<"diagnostics">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% normalize_diagnostics_mode/1 -- diagnostics mode normalisation (BT-2572)
%%
%% The Erlang op boundary normalises an unknown-binary `mode` to
%% <<"expression">>, mirroring the Elixir BtAttach.Facade (anything but
%% "method" -> "expression"). A non-binary `mode` is passed through unchanged so
%% diagnostics_for/2's catch-all still degrades it to [] (BT-2569). These are
%% white-box checks of the pure normaliser, so no compiler/port is needed.
%%====================================================================

diagnostics_unknown_binary_mode_normalised_to_expression_test() ->
    %% BT-2572: an unknown binary mode (e.g. <<"foo">>) is normalised to the safe
    %% default at the Erlang layer, not just in the Rust port.
    ?assertEqual(
        <<"expression">>,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(<<"foo">>)
    ).

diagnostics_method_mode_preserved_test() ->
    %% No behaviour change for the known <<"method">> mode.
    ?assertEqual(
        <<"method">>,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(<<"method">>)
    ).

diagnostics_expression_mode_preserved_test() ->
    %% No behaviour change for the known <<"expression">> mode.
    ?assertEqual(
        <<"expression">>,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(<<"expression">>)
    ).

diagnostics_empty_binary_mode_normalised_to_expression_test() ->
    %% An empty binary is "not method" and so normalises to expression.
    ?assertEqual(
        <<"expression">>,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(<<>>)
    ).

diagnostics_non_binary_mode_passed_through_unchanged_test() ->
    %% A non-binary mode is NOT normalised — it is passed through so the
    %% diagnostics_for/2 catch-all degrades it to [] (BT-2569), rather than being
    %% silently coerced into expression mode.
    ?assertEqual(
        42,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(42)
    ),
    ?assertEqual(
        undefined,
        beamtalk_repl_ops_dev:normalize_diagnostics_mode(undefined)
    ).

%%====================================================================
%% handle/4 -- show-codegen class+selector (BT-1236)
%%====================================================================

handle_show_codegen_class_not_found_error_test() ->
    %% Non-existent class name -> class not found error
    Msg = make_msg(<<"show-codegen">>, <<"sc-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_class_with_selector_no_class_found_test() ->
    %% class+selector where class doesn't exist -> error
    Msg = make_msg(<<"show-codegen">>, <<"sc-4">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>, <<"selector">> => <<"someMethod">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_class_takes_priority_over_code_test() ->
    %% When both class and code are given, class takes priority and errors (non-existent class)
    Msg = make_msg(<<"show-codegen">>, <<"sc-5">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"NonExistentClass99999">>, <<"code">> => <<"1 + 2">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    %% class path taken, returns error for non-existent class
    ?assert(maps:is_key(<<"error">>, Decoded)).

handle_show_codegen_empty_class_falls_back_to_code_test() ->
    %% Empty class binary is treated as absent; falls back to code path.
    %% Use empty code so we get the "empty expression" error without calling a real session.
    Msg = make_msg(<<"show-codegen">>, <<"sc-6">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<>>, <<"code">> => <<>>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    %% Code path taken: error must say "Empty expression", not "class not found".
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrorMsg = maps:get(<<"error">>, Decoded),
    ?assert(binary:match(ErrorMsg, <<"Empty expression">>) =/= nomatch).

handle_show_codegen_selector_without_class_test() ->
    %% Providing selector without class returns a specific "selector requires class" error,
    %% not the generic missing-parameter error — consistent with the Rust MCP boundary.
    Msg = make_msg(<<"show-codegen">>, <<"sc-7">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"selector">> => <<"greet">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    %% The error message must mention "selector" (not a generic missing-parameter message).
    ErrorMsg = maps:get(<<"error">>, Decoded),
    ?assert(binary:match(ErrorMsg, <<"selector">>) =/= nomatch).

validate_selector_undefined_returns_ok_test() ->
    %% When SelectorBin is undefined, no validation is needed.
    Result = beamtalk_repl_ops_dev:validate_selector_if_present(
        <<"SomeClass">>, 'SomeClass', self(), undefined
    ),
    ?assertEqual(ok, Result).

-doc """
Unit test for compile_file_for_codegen/2 success path (BT-1236).

Tests that the compiler can take a Beamtalk class source binary and
return Core Erlang text without loading a module into the runtime.
This exercises the compilation side of show_codegen_class_method/3.
""".
compile_file_for_codegen_success_test() ->
    Source = <<"Object subclass: TestCodegenClass\n  greet => 42\n">>,
    Result = beamtalk_repl_compiler:compile_file_for_codegen(Source, undefined),
    case Result of
        {ok, CoreErlang, Warnings} ->
            ?assert(is_binary(CoreErlang)),
            ?assert(byte_size(CoreErlang) > 0),
            ?assert(is_list(Warnings));
        {error, {compile_error, Reason}} when is_binary(Reason) ->
            %% Tolerate only "compiler not available" — any other compile error is a real failure.
            case binary:match(Reason, <<"Compiler not available">>) of
                nomatch -> error({unexpected_compile_error, Reason});
                _ -> ok
            end;
        {error, OtherReason} ->
            error({unexpected_compile_error, OtherReason})
    end.

%%====================================================================
%% handle/4 -- hover (BT-2555)
%%====================================================================

handle_hover_empty_code_test() ->
    %% Empty code short-circuits before any binding/registry lookup, so this
    %% needs no runtime: hover returns empty docs ("nothing to show"), which the
    %% client renders as no tooltip.
    Msg = make_msg(<<"hover">>, undefined, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"hover">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(<<>>, maps:get(<<"docs">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% handle/4 -- methods with unknown class
%%====================================================================

handle_methods_unknown_class_test() ->
    Msg = make_msg(<<"methods">>, <<"m-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"methods">>, #{<<"class">> => <<"NonExistentXyz9999">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
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
    %% With no selectors, walk_chain returns {ok, ClassName, instance} immediately
    ?assertEqual({ok, 'Integer', instance}, beamtalk_repl_ops_dev:walk_chain('Integer', [])).

walk_chain_unknown_selector_test() ->
    %% Non-existent registry -> not_found -> undefined
    Result = beamtalk_repl_ops_dev:walk_chain('Integer', [unknownSelector]),
    ?assertEqual(undefined, Result).

walk_chain_class_transition_test() ->
    %% `instance class` transitions to class side for completions
    ?assertEqual({ok, 'Integer', class}, beamtalk_repl_ops_dev:walk_chain('Integer', [class])).

%%====================================================================
%% walk_chain_class/2 -- without running registry
%%====================================================================

walk_chain_class_empty_selectors_test() ->
    ?assertEqual({ok, 'Integer', class}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [])).

walk_chain_class_meta_selector_test() ->
    %% `class` selector on a class object stays on the class side
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_ops_dev:walk_chain_class('Integer', [class])
    ).

walk_chain_class_multi_class_selectors_test() ->
    ?assertEqual({ok, 'Foo', class}, beamtalk_repl_ops_dev:walk_chain_class('Foo', [class, class])).

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
    ?assertEqual({ok, 'Integer', instance}, beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [])).

walk_mixed_chain_unknown_unary_selector_test() ->
    %% No registry — get_method_return_type returns not_found → undefined
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{unary, unknownSel}]),
    ?assertEqual(undefined, Result).

walk_mixed_chain_unknown_binary_selector_test() ->
    %% Graceful fallback: a binary selector not in method_return_types → undefined
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('Integer', [{binary, 'noSuchBinOp'}]),
    ?assertEqual(undefined, Result).

%%====================================================================
%% walk_mixed_chain_class/2 (BT-1071)
%%====================================================================

walk_mixed_chain_class_empty_hops_test() ->
    ?assertEqual(
        {ok, 'Integer', class}, beamtalk_repl_ops_dev:walk_mixed_chain_class('Integer', [])
    ).

walk_mixed_chain_class_meta_selector_test() ->
    %% `{unary, class}` on a class object stays on the class side
    ?assertEqual(
        {ok, 'Integer', class},
        beamtalk_repl_ops_dev:walk_mixed_chain_class('Integer', [{unary, class}])
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

%% BT-2091: the `docs` op was hard-removed from `beamtalk_repl_ops_dev:handle/4`.
%% Migration target: `Beamtalk help: ClassName` (or `selector: #sel`).
%% See `beamtalk_repl_server_tests:handle_op_docs_unknown_op_test/0` for the
%% surface-level confirmation that sending `op: "docs"` returns `unknown_op`.

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
%% handle/4 -- describe omits hard-removed ops (BT-2091)
%%====================================================================

handle_describe_omits_removed_ops_test() ->
    %% BT-2091: the deprecated ops `docs`, `load-file`, `reload`, and `modules`
    %% were removed; describe must no longer advertise them.
    Msg = make_msg(<<"describe">>, <<"d-dep">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assertEqual(false, maps:is_key(<<"docs">>, Ops)),
    ?assertEqual(false, maps:is_key(<<"load-file">>, Ops)),
    ?assertEqual(false, maps:is_key(<<"reload">>, Ops)),
    ?assertEqual(false, maps:is_key(<<"modules">>, Ops)),
    %% Protocol version was bumped to 2.0 to mark the breaking change.
    Versions = maps:get(<<"versions">>, Decoded),
    ?assertEqual(<<"2.0">>, maps:get(<<"protocol">>, Versions)).

handle_describe_contains_actors_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-actors">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"actors">>, Ops)),
    ?assertEqual([], maps:get(<<"params">>, maps:get(<<"actors">>, Ops))).

handle_describe_contains_inspect_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-inspect">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"inspect">>, Ops)),
    ?assertEqual([<<"actor">>], maps:get(<<"params">>, maps:get(<<"inspect">>, Ops))).

handle_describe_contains_kill_op_test() ->
    Msg = make_msg(<<"describe">>, <<"d-kill">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"describe">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    Ops = maps:get(<<"ops">>, Decoded),
    ?assert(maps:is_key(<<"kill">>, Ops)),
    ?assertEqual([<<"actor">>], maps:get(<<"params">>, maps:get(<<"kill">>, Ops))).

%%====================================================================
%% handle/4 -- complete new format with no cursor
%%====================================================================

handle_complete_new_format_empty_prefix_test() ->
    %% New format without cursor field falls back to get_completions
    Msg = make_msg(<<"complete">>, <<"c-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"complete">>, #{<<"code">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

%%====================================================================
%% Chain walking with class registry (fixture-based)
%%====================================================================

chain_with_registry_test_() ->
    {setup, fun setup_chain_registry/0, fun teardown_chain_registry/1, [
        fun walk_mixed_chain_binary_plus_returns_integer/0,
        fun walk_mixed_chain_binary_lt_returns_boolean/0
    ]}.

setup_chain_registry() ->
    %% Start pg if not running
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    %% Ensure hierarchy table exists
    beamtalk_class_registry:ensure_hierarchy_table(),
    %% Register a minimal TestInteger class with return type annotations
    {ok, IntPid} = beamtalk_object_class:start('TestChainInteger', #{
        name => 'TestChainInteger',
        module => 'bt@test@chain_integer',
        superclass => none,
        instance_methods => #{
            '+' => #{block => fun(_, _) -> 0 end, arity => 1},
            '<' => #{block => fun(_, _) -> false end, arity => 1}
        },
        method_return_types => #{
            '+' => 'TestChainInteger',
            '<' => 'TestChainBoolean'
        }
    }),
    {ok, BoolPid} = beamtalk_object_class:start('TestChainBoolean', #{
        name => 'TestChainBoolean',
        module => 'bt@test@chain_boolean',
        superclass => none,
        instance_methods => #{}
    }),
    [IntPid, BoolPid].

teardown_chain_registry(Pids) ->
    lists:foreach(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    MRef = monitor(process, Pid),
                    exit(Pid, kill),
                    receive
                        {'DOWN', MRef, process, Pid, _} -> ok
                    after 1000 -> ok
                    end;
                false ->
                    ok
            end
        end,
        Pids
    ).

walk_mixed_chain_binary_plus_returns_integer() ->
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('TestChainInteger', [{binary, '+'}]),
    ?assertEqual({ok, 'TestChainInteger', instance}, Result).

walk_mixed_chain_binary_lt_returns_boolean() ->
    Result = beamtalk_repl_ops_dev:walk_mixed_chain('TestChainInteger', [{binary, '<'}]),
    ?assertEqual({ok, 'TestChainBoolean', instance}, Result).

%%====================================================================
%% resolve_qualified_class_name/1 (BT-1659)
%%====================================================================

resolve_plain_class_name_test() ->
    %% Plain class name that exists as an atom — should resolve normally.
    ?assertEqual(
        {ok, 'Integer'}, beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"Integer">>)
    ).

resolve_plain_class_name_nonexistent_test() ->
    %% Plain class name that does not exist as an atom.
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"XyzzyNonexistent99991">>)
    ).

resolve_qualified_class_nonexistent_package_test() ->
    %% Qualified class from a non-existent package.
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"xyzzy@Parser">>)
    ).

resolve_qualified_class_stdlib_test() ->
    %% A stdlib class via package-qualified name: stdlib@Integer.
    %% The module atom `bt@stdlib@integer` should exist after stdlib is loaded.
    ?assertEqual(
        {ok, 'Integer'},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"stdlib@Integer">>)
    ).

resolve_qualified_class_empty_class_test() ->
    %% "json@" — empty class name after @
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"json@">>)
    ).

resolve_qualified_class_empty_package_test() ->
    %% "@Parser" — empty package before @
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"@Parser">>)
    ).

%%====================================================================
%% parse_receiver_and_prefix with @ (BT-1659)
%%====================================================================

parse_qualified_name_as_receiver_test() ->
    %% "json@Parser " should parse as receiver="json@Parser", prefix=<<>>
    %% (the @ is now an identifier char so it stays as one token)
    ?assertEqual(
        {<<"json@Parser">>, <<>>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"json@Parser ">>)
    ).

parse_qualified_name_with_prefix_test() ->
    %% "json@Parser pa" should parse as receiver="json@Parser", prefix="pa"
    ?assertEqual(
        {<<"json@Parser">>, <<"pa">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"json@Parser pa">>)
    ).

parse_bare_qualified_name_test() ->
    %% "json@Parser" with no trailing space is a bare prefix (no receiver)
    ?assertEqual(
        {undefined, <<"json@Parser">>},
        beamtalk_repl_ops_dev:parse_receiver_and_prefix(<<"json@Parser">>)
    ).

%% dedupe_keyword_aliases and format_beamtalk_signature tests moved to
%% beamtalk_erlang_help_tests.erl (these functions were extracted in BT-1903).

%%====================================================================
%% handle/4 -- erlang-help op (BT-1852)
%%====================================================================

erlang_help_missing_module_returns_error_test() ->
    %% Empty module binary -> "Module name required" error.
    Msg = make_msg(<<"erlang-help">>, <<"eh-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-help">>, #{<<"module">> => <<>>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"Module name required">>)).

erlang_help_unknown_module_returns_not_found_test() ->
    %% A module name that is not an existing atom -> badarg -> not-found error.
    Msg = make_msg(<<"erlang-help">>, <<"eh-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-help">>,
        #{<<"module">> => <<"no_such_erlang_module_xyz99999">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"not found">>)).

erlang_help_known_module_returns_docs_test() ->
    %% `lists` is always loaded; format_module_help should succeed and return docs.
    Msg = make_msg(<<"erlang-help">>, <<"eh-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-help">>, #{<<"module">> => <<"lists">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    %% Success path encodes docs (no error key).
    ?assertEqual(false, maps:is_key(<<"error">>, Decoded)).

erlang_help_known_module_unknown_function_returns_error_test() ->
    %% Known module, function that does not exist -> function not-found error.
    Msg = make_msg(<<"erlang-help">>, <<"eh-4">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-help">>,
        #{<<"module">> => <<"lists">>, <<"function">> => <<"no_such_fn_xyz99999">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    %% Hint mentions using :help Erlang <module> to list functions.
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"not found">>)).

erlang_help_known_module_known_function_returns_docs_test() ->
    %% `lists:map/2` exists -> function help succeeds.
    Msg = make_msg(<<"erlang-help">>, <<"eh-5">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-help">>,
        #{<<"module">> => <<"lists">>, <<"function">> => <<"map">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(false, maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% handle/4 -- erlang-complete op (BT-1903)
%%====================================================================

erlang_complete_module_prefix_test() ->
    %% No module param -> complete module names by prefix. "list" should match "lists".
    Msg = make_msg(<<"erlang-complete">>, <<"ec-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-complete">>, #{<<"prefix">> => <<"list">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    Completions = maps:get(<<"completions">>, Decoded),
    ?assert(is_list(Completions)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    %% Every completion must start with the requested prefix.
    lists:foreach(
        fun(C) -> ?assertEqual({0, 4}, binary:match(C, <<"list">>)) end,
        Completions
    ).

erlang_complete_function_prefix_loaded_module_test() ->
    %% Module given + already loaded -> complete its exported function names.
    %% `lists` is loaded; functions like `map`, `member` start with "m".
    Msg = make_msg(<<"erlang-complete">>, <<"ec-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-complete">>,
        #{<<"prefix">> => <<"m">>, <<"module">> => <<"lists">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    Completions = maps:get(<<"completions">>, Decoded),
    ?assert(is_list(Completions)),
    %% module_info must be filtered out, and all results start with "m".
    ?assertEqual(false, lists:member(<<"module_info">>, Completions)),
    lists:foreach(
        fun(C) -> ?assertEqual({0, 1}, binary:match(C, <<"m">>)) end,
        Completions
    ),
    %% `map` is a well-known lists export.
    ?assert(lists:member(<<"map">>, Completions)).

erlang_complete_unknown_module_returns_empty_test() ->
    %% Module name that is not an existing atom -> badarg -> empty completions.
    Msg = make_msg(<<"erlang-complete">>, <<"ec-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"erlang-complete">>,
        #{<<"prefix">> => <<"x">>, <<"module">> => <<"no_such_mod_xyz99999">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)).

%%====================================================================
%% handle/4 -- test / test-all op validation (BT no runtime)
%%====================================================================

handle_test_class_and_file_mutually_exclusive_test() ->
    %% Both class and file given -> mutually-exclusive error.
    Msg = make_msg(<<"test">>, <<"t-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"test">>,
        #{<<"class">> => <<"FooTest">>, <<"file">> => <<"foo.bt">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"mutually exclusive">>)).

handle_test_file_not_binary_test() ->
    %% file param that is not a binary -> "'file' must be a binary path" error.
    Msg = make_msg(<<"test">>, <<"t-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"test">>, #{<<"file">> => 12345}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"binary path">>)).

handle_test_unknown_class_returns_class_not_found_test() ->
    %% A class name that has never been loaded as an atom -> class-not-found error.
    Msg = make_msg(<<"test">>, <<"t-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"test">>, #{<<"class">> => <<"NoSuchTestClassXyz99999">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"Unknown class">>)).

%%====================================================================
%% handle/4 -- list-classes filter validation (BT-1404)
%%====================================================================

handle_list_classes_unknown_filter_returns_error_test() ->
    %% A filter that is not stdlib/user and not an existing atom -> argument error.
    Msg = make_msg(<<"list-classes">>, <<"lc-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>,
        #{<<"filter">> => <<"NoSuchFilterClassXyz99999">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"Unknown filter">>)).

%%====================================================================
%% encode helpers: {codegen, _, _} term via show-codegen success path
%%====================================================================

%% validate_list_classes_filter/1 is internal; its observable behaviour is
%% exercised through handle(<<"list-classes">>, ...). The success branch with
%% an empty registry is covered by the fixture-based list-classes tests below.

%%====================================================================
%% Fixture: a live class registry with real classes (exercises the
%% completion + reflection + show-codegen-class paths).
%%====================================================================

dev_runtime_test_() ->
    {setup, fun setup_dev_runtime/0, fun teardown_dev_runtime/1, fun(_) ->
        [
            {"complete without cursor with class prefix",
                fun complete_no_cursor_class_prefix_returns_class/0},
            {"get_completions matches registered class", fun get_completions_matches_class/0},
            {"get_completions includes builtin keyword", fun get_completions_includes_keyword/0},
            {"context completion: class receiver -> class methods",
                fun context_completion_class_receiver/0},
            {"context completion: class receiver empty prefix",
                fun context_completion_class_empty_prefix/0},
            {"context completion: integer literal receiver",
                fun context_completion_integer_literal/0},
            {"context completion: string literal receiver",
                fun context_completion_string_literal/0},
            {"context completion: binding receiver", fun context_completion_binding_receiver/0},
            {"context completion: unknown lowercase receiver -> empty",
                fun context_completion_unknown_lowercase/0},
            {"methods op returns instance + class methods", fun methods_op_returns_methods/0},
            {"methods op returns state vars", fun methods_op_returns_state_vars/0},
            {"list_class_methods_for_ws includes inherited side tags",
                fun list_class_methods_known_class/0},
            {"list-classes op returns the registered class", fun list_classes_op_returns_class/0},
            {"list-classes filter stdlib excludes non-stdlib class",
                fun list_classes_filter_stdlib/0},
            {"list-classes superclass filter", fun list_classes_superclass_filter/0},
            {"show-codegen class: selector not found error",
                fun show_codegen_class_selector_not_found/0},
            {"validate_selector_if_present known selector -> ok", fun validate_selector_known/0},
            {"validate_selector_if_present unknown selector -> error",
                fun validate_selector_unknown/0},
            {"classify class receiver via get_methods_for_receiver",
                fun get_methods_for_receiver_class/0},
            {"resolve_chain_type class side via registry", fun resolve_chain_type_class_side/0},
            {"show-codegen class without selector compiles source",
                fun show_codegen_class_compiles_source/0},
            {"cross-package internal class excluded from completions",
                fun completions_exclude_cross_package_internal/0},
            {"expression completion: class chain -> instance methods",
                fun context_completion_expression_instance_methods/0},
            {"expression completion: class-side chain -> class methods",
                fun context_completion_expression_class_methods/0},
            {"expression completion: unresolvable chain -> empty",
                fun context_completion_expression_unresolvable/0},
            {"methods op via package-qualified class name",
                fun list_class_methods_qualified_name/0},
            {"test op for a known (non-TestCase) class returns a response",
                fun test_op_known_class_returns_response/0},
            {"walk_chain follows instance return type", fun walk_chain_follows_return_type/0},
            {"walk_chain instance-then-class transition", fun walk_chain_instance_then_class/0},
            {"walk_chain_class first hop transitions to instance",
                fun walk_chain_class_first_hop_instance/0},
            {"walk_mixed_chain unary class transition",
                fun walk_mixed_chain_unary_class_transition/0},
            {"walk_mixed_chain_class first hop transitions to instance",
                fun walk_mixed_chain_class_first_hop_instance/0},
            {"resolve_chain_type instance chain via registry",
                fun resolve_chain_type_instance_chain/0},
            {"expression completion empty prefix lists all instance methods",
                fun context_completion_expression_empty_prefix/0},
            {"uppercase binding (non-class) classifies via binding",
                fun classify_uppercase_binding/0},
            {"qualified @ receiver classifies as class",
                fun context_completion_qualified_receiver/0},
            {"list-classes user filter includes user class",
                fun list_classes_user_filter_includes/0},
            {"single-line doc surfaces verbatim in list-classes",
                fun list_classes_single_line_doc/0},
            {"type-annotation position (space before ::) offers class names",
                fun type_annotation_completion_class_name_spaced/0},
            {"type-annotation position (no space before ::) offers class names",
                fun type_annotation_completion_class_name_attached/0},
            {"type-annotation position (no space, keyword-send receiver) offers class names",
                fun type_annotation_completion_class_name_attached_keyword_receiver/0},
            {"type-annotation position (space, keyword-send receiver) offers class names",
                fun type_annotation_completion_class_name_spaced_keyword_receiver/0},
            {"type-annotation position offers live alias names alongside classes",
                fun type_annotation_completion_alias_name/0},
            {"type-annotation position with empty prefix offers all candidates",
                fun type_annotation_completion_empty_prefix/0},
            {"single-colon keyword-selector prefix is not annotation position",
                fun single_colon_not_annotation_position/0}
        ]
    end}.

context_completion_expression_empty_prefix() ->
    %% "WidgetDev create " — resolved instance receiver with empty prefix returns
    %% all instance methods (filter_by_prefix empty-prefix usort branch).
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev create ">>),
    ?assert(lists:member(<<"render">>, Result)),
    ?assert(lists:member(<<"resize">>, Result)),
    ?assert(lists:member(<<"next">>, Result)).

classify_uppercase_binding() ->
    %% `Transcript` is an existing atom but not a registered class here, so the
    %% uppercase branch of classify_receiver falls back to the binding lookup.
    %% The binding holds an integer (primitive_class_of → 'Integer'), and the
    %% Integer fixture class is registered, so completion offers Integer methods.
    Bindings = #{'Transcript' => 99},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"Transcript ab">>, Bindings),
    ?assert(lists:member(<<"abs">>, Result)).

context_completion_qualified_receiver() ->
    %% "test@WidgetDev re" — the @-qualified receiver resolves to WidgetDev (a
    %% class object), so class-side completions are offered. Exercises the
    %% resolve_qualified_class_name branch inside classify_receiver/2.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"test@WidgetDev cr">>),
    ?assert(lists:member(<<"create">>, Result)).

list_classes_user_filter_includes() ->
    %% With WidgetDev registered under a non-stdlib module, the "user" filter
    %% must include it (exercises should_include_class/4 not-stdlib branch).
    Msg = make_msg(<<"list-classes">>, <<"lcui-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>, #{<<"filter">> => <<"user">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ClassList = maps:get(<<"class_list">>, Decoded),
    Names = [maps:get(<<"name">>, C) || C <- ClassList],
    ?assert(lists:member(<<"WidgetDev">>, Names)).

list_classes_single_line_doc() ->
    %% OneLineDocDev's doc has no newline; first_line/1 returns it verbatim.
    Msg = make_msg(<<"list-classes">>, <<"lcd-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"list-classes">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ClassList = maps:get(<<"class_list">>, Decoded),
    [Row] = [C || C <- ClassList, maps:get(<<"name">>, C) =:= <<"OneLineDocDev">>],
    ?assertEqual(<<"Single line doc no newline">>, maps:get(<<"doc">>, Row)).

walk_chain_follows_return_type() ->
    %% next returns a WidgetDev instance, so walk_chain follows the hop.
    ?assertEqual(
        {ok, 'WidgetDev', instance},
        beamtalk_repl_ops_dev:walk_chain('WidgetDev', [next])
    ).

walk_chain_instance_then_class() ->
    %% "next class" — follow next (→ WidgetDev instance) then transition to class.
    ?assertEqual(
        {ok, 'WidgetDev', class},
        beamtalk_repl_ops_dev:walk_chain('WidgetDev', [next, class])
    ).

walk_chain_class_first_hop_instance() ->
    %% Class-side create returns a WidgetDev instance, so the chain switches
    %% to the instance side for the remaining (empty) hops.
    ?assertEqual(
        {ok, 'WidgetDev', instance},
        beamtalk_repl_ops_dev:walk_chain_class('WidgetDev', [create])
    ).

walk_mixed_chain_unary_class_transition() ->
    %% {unary, class} after a resolvable hop transitions instance → class.
    ?assertEqual(
        {ok, 'WidgetDev', class},
        beamtalk_repl_ops_dev:walk_mixed_chain('WidgetDev', [{unary, next}, {unary, class}])
    ).

walk_mixed_chain_class_first_hop_instance() ->
    %% Class-side create returns an instance; mixed-chain-class switches sides.
    ?assertEqual(
        {ok, 'WidgetDev', instance},
        beamtalk_repl_ops_dev:walk_mixed_chain_class('WidgetDev', [{unary, create}])
    ).

resolve_chain_type_instance_chain() ->
    %% "WidgetDev create next" — class create → instance, then next → instance.
    ?assertEqual(
        {ok, 'WidgetDev', instance},
        beamtalk_repl_ops_dev:resolve_chain_type(<<"WidgetDev create next">>, #{})
    ).

test_op_known_class_returns_response() ->
    %% WidgetDev is a real registered class atom (so safe_to_existing_atom
    %% succeeds), but it is not a TestCase subclass. run_test_op/2 therefore
    %% runs the runner branch: it either returns test results or a structured
    %% error — both are well-formed JSON with a status field. This exercises
    %% the run_class_by_name path rather than the early class-not-found return.
    Msg = make_msg(<<"test">>, <<"tk-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"test">>, #{<<"class">> => <<"WidgetDev">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(is_map(Decoded)),
    ?assert(maps:is_key(<<"status">>, Decoded)).

setup_dev_runtime() ->
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    %% Base class (superclass) so the hierarchy walk has > 1 level.
    {ok, BasePid} = beamtalk_object_class:start('WidgetDevBase', #{
        name => 'WidgetDevBase',
        module => 'bt@test@widget_dev_base',
        superclass => none,
        instance_methods => #{
            'inheritedGreet' => #{block => fun(_, _) -> ok end, arity => 0}
        }
    }),
    %% Concrete class with instance + class methods, fields, and a doc string.
    {ok, WidgetPid} = beamtalk_object_class:start('WidgetDev', #{
        name => 'WidgetDev',
        module => 'bt@test@widget_dev',
        superclass => 'WidgetDevBase',
        doc => <<"A widget for dev tests.\nSecond line ignored.">>,
        fields => [width, height],
        instance_methods => #{
            'render' => #{block => fun(_, _) -> ok end, arity => 0},
            'resize' => #{block => fun(_, _, _) -> ok end, arity => 2},
            'next' => #{block => fun(_, _) -> ok end, arity => 0}
        },
        method_return_types => #{
            'next' => 'WidgetDev'
        },
        class_methods => #{
            'create' => #{block => fun(_, _) -> ok end, arity => 0}
        },
        class_method_return_types => #{
            'create' => 'WidgetDev'
        }
    }),
    %% A class named "Integer" so a binding holding an integer (whose
    %% primitive_class_of is 'Integer') classifies as an instance receiver,
    %% and "String" for string-literal classification.
    %% 'Integer'/'String' may already be registered by the runtime bootstrap (or
    %% by another test module in the shared EUnit node), so tolerate
    %% already_started and only kill the ones this fixture actually owns.
    {IntPid, IntOwned} = start_dev_class('Integer', #{
        name => 'Integer',
        module => 'bt@test@integer_dev',
        superclass => none,
        instance_methods => #{
            'abs' => #{block => fun(_, _) -> 0 end, arity => 0}
        }
    }),
    %% Use the real String method name 'uppercase' so the string-literal
    %% completion test passes whether this fixture's String is used (isolated
    %% run) or the bootstrap String is reused (combined run).
    {StrPid, StrOwned} = start_dev_class('String', #{
        name => 'String',
        module => 'bt@test@string_dev',
        superclass => none,
        instance_methods => #{
            'uppercase' => #{block => fun(_, _) -> ok end, arity => 0}
        }
    }),
    %% ADR 0071 Phase 5: an internal class in a *named* package (test) — the REPL
    %% runs in the implicit nil package, so this class must be filtered out of
    %% cross-package completions (exercises is_cross_package_internal/1 true path).
    {ok, HiddenPid} = beamtalk_object_class:start('HiddenDevWidget', #{
        name => 'HiddenDevWidget',
        module => 'bt@test@hidden_dev_widget',
        superclass => none,
        is_internal => true,
        instance_methods => #{
            'secret' => #{block => fun(_, _) -> ok end, arity => 0}
        }
    }),
    %% A class whose doc has no newline, exercising first_line/1's no-split branch.
    {ok, OneLinePid} = beamtalk_object_class:start('OneLineDocDev', #{
        name => 'OneLineDocDev',
        module => 'bt@test@one_line_doc_dev',
        superclass => none,
        doc => <<"Single line doc no newline">>,
        instance_methods => #{}
    }),
    [BasePid, WidgetPid, HiddenPid, OneLinePid] ++
        [IntPid || IntOwned] ++ [StrPid || StrOwned].

%% Start a class for the dev-runtime fixture, tolerating a class already
%% registered by the runtime bootstrap or another module. Returns {Pid, Owned}
%% where Owned is false for a reused registration (teardown must not kill it).
start_dev_class(Name, Spec) ->
    case beamtalk_object_class:start(Name, Spec) of
        {ok, Pid} -> {Pid, true};
        {error, {already_started, Pid}} -> {Pid, false}
    end.

teardown_dev_runtime(Pids) ->
    lists:foreach(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    MRef = monitor(process, Pid),
                    exit(Pid, kill),
                    receive
                        {'DOWN', MRef, process, Pid, _} -> ok
                    after 1000 -> ok
                    end;
                false ->
                    ok
            end
        end,
        Pids
    ).

complete_no_cursor_class_prefix_returns_class() ->
    %% No "cursor" field: get_completions matches the class by prefix.
    Msg = make_msg(<<"complete">>, undefined, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"complete">>, #{<<"code">> => <<"WidgetD">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    Completions = maps:get(<<"completions">>, Decoded),
    ?assert(lists:member(<<"WidgetDev">>, Completions)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

get_completions_matches_class() ->
    Result = beamtalk_repl_ops_dev:get_completions(<<"WidgetD">>),
    ?assert(lists:member(<<"WidgetDev">>, Result)),
    %% A non-matching prefix yields no class names.
    ?assertEqual(
        false, lists:member(<<"WidgetDev">>, beamtalk_repl_ops_dev:get_completions(<<"Zzz">>))
    ).

get_completions_includes_keyword() ->
    %% "sel" should match the builtin keyword "self".
    Result = beamtalk_repl_ops_dev:get_completions(<<"sel">>),
    ?assert(lists:member(<<"self">>, Result)).

context_completion_class_receiver() ->
    %% "WidgetDev cr" -> class-side method "create" plus ProtoObject methods.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev cr">>),
    ?assert(lists:member(<<"create">>, Result)).

context_completion_class_empty_prefix() ->
    %% "WidgetDev " (empty prefix) -> all class-side methods.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev ">>),
    ?assert(lists:member(<<"create">>, Result)).

context_completion_integer_literal() ->
    %% Integer literal receiver -> instance methods of Integer ("abs").
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"42 ab">>),
    ?assert(lists:member(<<"abs">>, Result)).

context_completion_string_literal() ->
    %% String literal receiver -> String instance methods ("uppercase").
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"\"hi\" up">>),
    ?assert(lists:member(<<"uppercase">>, Result)).

context_completion_binding_receiver() ->
    %% A lowercase binding holding an integer classifies as Integer instance.
    Bindings = #{n => 7},
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"n ab">>, Bindings),
    ?assert(lists:member(<<"abs">>, Result)).

context_completion_unknown_lowercase() ->
    %% A lowercase identifier with no binding -> no receiver classification -> [].
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"undefinedvar re">>, #{}),
    ?assertEqual([], Result).

methods_op_returns_methods() ->
    Msg = make_msg(<<"methods">>, <<"mm-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"methods">>, #{<<"class">> => <<"WidgetDev">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    Methods = maps:get(<<"methods">>, Decoded),
    Names = [maps:get(<<"name">>, M) || M <- Methods],
    ?assert(lists:member(<<"render">>, Names)),
    ?assert(lists:member(<<"create">>, Names)),
    %% Side tags must distinguish instance from class methods.
    Sides = [maps:get(<<"side">>, M) || M <- Methods, maps:get(<<"name">>, M) =:= <<"create">>],
    ?assertEqual([<<"class">>], Sides).

methods_op_returns_state_vars() ->
    Msg = make_msg(<<"methods">>, <<"mm-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"methods">>, #{<<"class">> => <<"WidgetDev">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    StateVars = maps:get(<<"state_vars">>, Decoded),
    ?assertEqual([<<"height">>, <<"width">>], StateVars).

list_class_methods_known_class() ->
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"WidgetDev">>),
    Names = [maps:get(<<"name">>, M) || M <- Result],
    ?assert(lists:member(<<"render">>, Names)),
    ?assert(lists:member(<<"resize">>, Names)),
    ?assert(lists:member(<<"create">>, Names)).

list_classes_op_returns_class() ->
    Msg = make_msg(<<"list-classes">>, <<"lc-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"list-classes">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)),
    ClassList = maps:get(<<"class_list">>, Decoded),
    Names = [maps:get(<<"name">>, C) || C <- ClassList],
    ?assert(lists:member(<<"WidgetDev">>, Names)),
    %% The WidgetDev row must carry the first line of its doc and its superclass.
    [Row] = [C || C <- ClassList, maps:get(<<"name">>, C) =:= <<"WidgetDev">>],
    ?assertEqual(<<"A widget for dev tests.">>, maps:get(<<"doc">>, Row)),
    ?assertEqual(<<"WidgetDevBase">>, maps:get(<<"superclass">>, Row)),
    ?assertEqual(0, maps:get(<<"actor_count">>, Row)).

list_classes_filter_stdlib() ->
    %% WidgetDev is registered with a non-stdlib module, so the stdlib filter
    %% must exclude it.
    Msg = make_msg(<<"list-classes">>, <<"lc-3">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>, #{<<"filter">> => <<"stdlib">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ClassList = maps:get(<<"class_list">>, Decoded),
    Names = [maps:get(<<"name">>, C) || C <- ClassList],
    ?assertEqual(false, lists:member(<<"WidgetDev">>, Names)).

list_classes_superclass_filter() ->
    %% Filtering by superclass WidgetDevBase should include WidgetDev (which
    %% inherits from it) and exclude unrelated classes.
    Msg = make_msg(<<"list-classes">>, <<"lc-4">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>, #{<<"filter">> => <<"WidgetDevBase">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    %% Success (not an error) — the filter resolves to an existing atom.
    ?assertEqual(false, maps:is_key(<<"error">>, Decoded)),
    ClassList = maps:get(<<"class_list">>, Decoded),
    Names = [maps:get(<<"name">>, C) || C <- ClassList],
    ?assert(lists:member(<<"WidgetDev">>, Names)),
    ?assertEqual(false, lists:member(<<"Integer">>, Names)).

show_codegen_class_selector_not_found() ->
    %% Class exists, selector does not -> "Selector ... not found" error
    %% (exercises validate_selector_if_present failure branch).
    Msg = make_msg(<<"show-codegen">>, <<"scc-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>,
        #{<<"class">> => <<"WidgetDev">>, <<"selector">> => <<"noSuchSelectorXyz">>},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"not found">>)).

validate_selector_known() ->
    Pid = beamtalk_runtime_api:whereis_class('WidgetDev'),
    Result = beamtalk_repl_ops_dev:validate_selector_if_present(
        <<"WidgetDev">>, 'WidgetDev', Pid, <<"render">>
    ),
    ?assertEqual(ok, Result).

validate_selector_unknown() ->
    Pid = beamtalk_runtime_api:whereis_class('WidgetDev'),
    %% BT-2402: validate_selector_if_present/4 returns a structured error term
    %% (no longer a pre-encoded JSON binary).
    Result = beamtalk_repl_ops_dev:validate_selector_if_present(
        <<"WidgetDev">>, 'WidgetDev', Pid, <<"noSuchSelectorXyz">>
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result).

get_methods_for_receiver_class() ->
    %% A class-name receiver with empty prefix returns class-side methods plus
    %% ProtoObject instance methods, via get_context_completions.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev ">>),
    ?assert(lists:member(<<"create">>, Result)),
    %% Instance-only methods (render) must NOT appear for a class-object receiver.
    ?assertEqual(false, lists:member(<<"render">>, Result)).

resolve_chain_type_class_side() ->
    %% "WidgetDev create" — class-side send returning WidgetDev (instance).
    Result = beamtalk_repl_ops_dev:resolve_chain_type(<<"WidgetDev create">>, #{}),
    ?assertEqual({ok, 'WidgetDev', instance}, Result).

completions_exclude_cross_package_internal() ->
    %% HiddenDevWidget is internal and lives in the named package "test"; the
    %% REPL's implicit nil package means it must be filtered from completions.
    %% WidgetDev (public, package "test") is still offered.
    Result = beamtalk_repl_ops_dev:get_completions(<<"">>),
    %% Empty prefix short-circuits to []; use a matching prefix instead.
    ?assertEqual([], Result),
    Hidden = beamtalk_repl_ops_dev:get_completions(<<"HiddenDev">>),
    ?assertEqual(false, lists:member(<<"HiddenDevWidget">>, Hidden)),
    %% Sanity: the public class with the same package prefix IS offered.
    Public = beamtalk_repl_ops_dev:get_completions(<<"WidgetD">>),
    ?assert(lists:member(<<"WidgetDev">>, Public)).

context_completion_expression_instance_methods() ->
    %% Multi-token receiver "WidgetDev create" resolves (class-side create returns
    %% a WidgetDev instance) → complete_instance_methods filters by "re".
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev create re">>),
    ?assert(lists:member(<<"render">>, Result)),
    ?assert(lists:member(<<"resize">>, Result)).

context_completion_expression_class_methods() ->
    %% "WidgetDev class cr" — `class` keeps the chain on the class side, so
    %% complete_class_methods offers the class-side selector "create".
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev class cr">>),
    ?assert(lists:member(<<"create">>, Result)).

context_completion_expression_unresolvable() ->
    %% A multi-token receiver whose type cannot be resolved (unknown selector,
    %% no compiler) yields []. `WidgetDev render` — render has no return-type
    %% annotation, so the chain breaks → undefined → [].
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"WidgetDev render xy">>),
    ?assertEqual([], Result).

type_annotation_completion_class_name_spaced() ->
    %% "policy :: Widg" (space before `::`) — parse_receiver_and_prefix returns
    %% {expression, <<"policy ::">>, <<"Widg">>}; ends_with_double_colon detects
    %% annotation position and class_name_completions offers WidgetDev, matching
    %% the static LSP path's class-name-in-annotation-position behaviour (BT-2918).
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"policy :: Widg">>),
    ?assert(lists:member(<<"WidgetDev">>, Result)),
    %% Instance/class methods must NOT leak into annotation-position completions.
    ?assertEqual(false, lists:member(<<"create">>, Result)).

type_annotation_completion_class_name_attached() ->
    %% "policy ::Widg" (no space before the prefix) — parse_receiver_and_prefix
    %% returns {<<"policy">>, <<"::Widg">>}; type_annotation_prefix/1 strips the
    %% leading "::" via strip_double_colon_prefix/1.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"policy ::Widg">>),
    ?assert(lists:member(<<"WidgetDev">>, Result)).

type_annotation_completion_class_name_attached_keyword_receiver() ->
    %% "deposit: amount ::Widg" — a MULTI-TOKEN keyword-send receiver with an
    %% attached (no-space) "::" prefix. parse_receiver_and_prefix/1 returns
    %% {expression, <<"deposit: amount">>, <<"::Widg">>} — the receiver
    %% expression does NOT end in "::" here (the attached "::" landed in
    %% Prefix instead), so this exercises type_annotation_prefix/1's
    %% strip_double_colon_prefix/1 check on Prefix within the {expression, _,
    %% _} clause, not just ends_with_double_colon/1 on ReceiverExpr.
    %% Regression coverage for a gap an adversarial review pass caught: this
    %% previously fell through to resolve_chain_type/2 (which cannot resolve
    %% a keyword-send receiver) and silently returned [] instead of offering
    %% class names — exactly the annotation position a method's keyword
    %% parameter list types are declared in.
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"deposit: amount ::Widg">>),
    ?assert(lists:member(<<"WidgetDev">>, Result)).

type_annotation_completion_class_name_spaced_keyword_receiver() ->
    %% "deposit: amount :: Widg" — a MULTI-TOKEN keyword-send receiver with a
    %% SPACED "::" (the fourth of the four parse shapes type_annotation_prefix/1
    %% documents). parse_receiver_and_prefix/1 returns
    %% {expression, <<"deposit: amount ::">>, <<"Widg">>} here — the receiver
    %% expression itself ends in "::", so this exercises ends_with_double_colon/1
    %% on a multi-token expression (previously only single-token "policy :: Widg"
    %% covered that path).
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"deposit: amount :: Widg">>),
    ?assert(lists:member(<<"WidgetDev">>, Result)).

type_annotation_completion_alias_name() ->
    %% AliasNames threaded via get_context_completions/3 are offered in
    %% annotation position alongside class names — the live-REPL counterpart to
    %% completion_provider.rs's add_alias_name_completions (BT-2901).
    AliasNames = [<<"RestartStrategy">>, <<"OtherAlias">>],
    Result = beamtalk_repl_ops_dev:get_context_completions(
        <<"policy :: Restart">>, #{}, AliasNames
    ),
    ?assert(lists:member(<<"RestartStrategy">>, Result)),
    %% "OtherAlias" doesn't match the "Restart" prefix — must not leak in.
    ?assertEqual(false, lists:member(<<"OtherAlias">>, Result)).

type_annotation_completion_empty_prefix() ->
    %% "policy :: " (nothing typed yet) — empty prefix matches every class and
    %% every live alias name, not [] (unlike get_completions/1's <<>> short-circuit).
    AliasNames = [<<"RestartStrategy">>],
    Result = beamtalk_repl_ops_dev:get_context_completions(<<"policy :: ">>, #{}, AliasNames),
    ?assert(lists:member(<<"WidgetDev">>, Result)),
    ?assert(lists:member(<<"RestartStrategy">>, Result)).

single_colon_not_annotation_position() ->
    %% "policy at:" — a single-colon keyword-selector-in-progress prefix must
    %% NOT be mistaken for annotation position; type_annotation_prefix/1 only
    %% fires on a literal double colon. Falls through to ordinary
    %% receiver-method completion, so the alias name must not appear.
    Result = beamtalk_repl_ops_dev:get_context_completions(
        <<"policy at:">>, #{}, [<<"RestartStrategy">>]
    ),
    ?assertEqual(false, lists:member(<<"RestartStrategy">>, Result)).

list_class_methods_qualified_name() ->
    %% Package-qualified name "test@WidgetDev" resolves to the WidgetDev class
    %% (its module atom bt@test@widget_dev exists), exercising the qualified
    %% branch of list_class_methods_for_ws/1.
    Result = beamtalk_repl_ops_dev:list_class_methods_for_ws(<<"test@WidgetDev">>),
    Names = [maps:get(<<"name">>, M) || M <- Result],
    ?assert(lists:member(<<"render">>, Names)).

show_codegen_class_compiles_source() ->
    %% Class exists, no selector → compile_class_source/4 runs. The fixture
    %% class has a synthetic module not on disk and no workspace_meta source,
    %% so resolve_source_path returns "unknown" and we hit the no_source branch
    %% producing a "No source" runtime error (rather than a class-not-found).
    Msg = make_msg(<<"show-codegen">>, <<"scs-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>, #{<<"class">> => <<"WidgetDev">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"No source">>)).

%%====================================================================
%% Live REPL session: show-codegen `code` path + {codegen, _, _} term
%%====================================================================

session_codegen_test_() ->
    {setup, fun setup_session/0, fun teardown_session/1, fun(Pid) ->
        [
            {"show-codegen code routes through the session code path", fun() ->
                session_show_codegen_code_path(Pid)
            end},
            {"show-codegen code compile error returns structured error", fun() ->
                session_show_codegen_code_error(Pid)
            end}
        ]
    end}.

setup_session() ->
    application:ensure_all_started(beamtalk_runtime),
    {ok, Pid} = beamtalk_repl_shell:start_link(<<"ops-dev-codegen-session">>),
    Pid.

teardown_session(Pid) ->
    catch beamtalk_repl_shell:stop(Pid),
    ok.

session_show_codegen_code_path(Pid) ->
    %% A non-empty `code` param with a live session routes handle/4 through the
    %% code branch and into beamtalk_repl_shell:show_codegen/2. Compiler-port
    %% availability is environment-dependent in the shared EUnit node: with the
    %% port up show_codegen returns a codegen result, without it a structured
    %% error. Either way the response is a well-formed, non-crashing map whose
    %% status list ends with the done marker.
    Msg = make_msg(<<"show-codegen">>, <<"scd-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>, #{<<"code">> => <<"1 + 2">>}, Msg, Pid
    ),
    Decoded = json:decode(Result),
    ?assert(is_map(Decoded)),
    Status = maps:get(<<"status">>, Decoded),
    ?assert(lists:member(<<"done">>, Status)).

session_show_codegen_code_error(Pid) ->
    %% A syntactically invalid expression also flows through the code path's
    %% error branch and returns a structured error.
    Msg = make_msg(<<"show-codegen">>, <<"scd-2">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"show-codegen">>, #{<<"code">> => <<"@@@ invalid syntax @@@">>}, Msg, Pid
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% Additional coverage: cheap pure helpers + remaining handle branches
%%====================================================================

handle_complete_with_cursor_no_session_bindings_test() ->
    %% New protocol with a "cursor" field exercises the binding-merge branch.
    %% self() is not a real session, so get_session_bindings catches and returns
    %% #{}; the merge still produces context completions (here: bare prefix → []).
    Msg = make_msg(<<"complete">>, <<"cc-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"complete">>,
        #{<<"code">> => <<>>, <<"cursor">> => 0},
        Msg,
        self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual([], maps:get(<<"completions">>, Decoded)),
    ?assertEqual([<<"done">>], maps:get(<<"status">>, Decoded)).

get_session_bindings_dead_pid_returns_empty_test() ->
    %% A non-session pid (self) makes beamtalk_repl_shell:get_bindings throw,
    %% which the helper catches, returning an empty map.
    ?assertEqual(#{}, beamtalk_repl_ops_dev:get_session_bindings(self())).

get_session_alias_names_dead_pid_returns_empty_test() ->
    %% A non-session pid (self) makes beamtalk_repl_shell:get_alias_table throw,
    %% which the helper catches, returning [] (BT-2918).
    ?assertEqual([], beamtalk_repl_ops_dev:get_session_alias_names(self())).

handle_test_all_returns_response_test() ->
    %% test-all runs the (possibly empty) TestCase suite. With no TestCase
    %% subclasses loaded it returns a structured test-results response; if the
    %% runner raises, a structured error is returned. Either way it is a
    %% well-formed JSON object — never a crash.
    Msg = make_msg(<<"test-all">>, <<"ta-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(<<"test-all">>, #{}, Msg, self()),
    Decoded = json:decode(Result),
    ?assert(is_map(Decoded)),
    ?assert(maps:is_key(<<"status">>, Decoded)).

handle_test_file_returns_response_test() ->
    %% A file path with no matching TestCase subclasses returns a well-formed
    %% response (empty results or structured error), exercising run_test_op_file/2.
    Msg = make_msg(<<"test">>, <<"tf-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"test">>, #{<<"file">> => <<"no_such_file_xyz99999.bt">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(is_map(Decoded)),
    ?assert(maps:is_key(<<"status">>, Decoded)).

handle_list_tests_returns_classes_term_test() ->
    %% BT-2557: list-tests discovers TestCase subclasses via the live registry.
    %% The bare EUnit image loads no TestCase subclasses, so discovery returns an
    %% empty `classes` list — exercising the op routing + the `{value, _}` term
    %% shape without depending on any fixture class being present.
    Msg = make_msg(<<"list-tests">>, <<"lt-1">>, undefined),
    {value, Value} = beamtalk_repl_ops_dev:handle_term(<<"list-tests">>, #{}, Msg, self()),
    ?assert(is_list(maps:get(<<"classes">>, Value))).

%% BT-2801: lazily start `beamtalk_workspace_findings_store` for the
%% duration of `TestFun`, mirroring the `case whereis(...)` precedent used
%% by `beamtalk_repl_loader_recheck_tests.erl` for the same gen_server. This
%% file's tests otherwise don't require a running workspace (see the module
%% doc), so the reload-findings tests must bring their own dependency up —
%% and, critically, tear it back down in an `after` (even on assertion
%% failure) so a store *this helper* started never leaks into
%% `beamtalk_workspace_findings_store_tests.erl`'s own `setup/0`, which
%% unconditionally `start_link/0`s and badmatches on `{already_started, _}`
%% if a prior test in the same `rebar3 eunit` run left one behind. A store
%% that was already running (owned by some other test) is left alone.
with_findings_store(TestFun) ->
    Started =
        case whereis(beamtalk_workspace_findings_store) of
            undefined ->
                {ok, _} = beamtalk_workspace_findings_store:start_link(),
                true;
            _Pid ->
                false
        end,
    try
        TestFun()
    after
        case Started of
            true -> catch gen_server:stop(beamtalk_workspace_findings_store);
            false -> ok
        end
    end.

handle_reload_findings_returns_empty_snapshot_test() ->
    with_findings_store(fun() ->
        %% BT-2801: reload-findings is a request/response snapshot read of
        %% `beamtalk_workspace_findings_store:all/0`. With no findings
        %% recorded (the store's initial state), the op still exercises op
        %% routing + the `{value, _}` term shape and returns an empty list,
        %% not an error.
        Msg = make_msg(<<"reload-findings">>, <<"rf-1">>, undefined),
        {value, Value} = beamtalk_repl_ops_dev:handle_term(<<"reload-findings">>, #{}, Msg, self()),
        ?assertEqual([], maps:get(<<"findings">>, Value))
    end).

handle_reload_findings_returns_wire_shaped_finding_test() ->
    with_findings_store(fun() ->
        %% BT-2801: a recorded finding must round-trip through the op in the
        %% same wire shape `encode_reload_finding/1` produces for the
        %% `reload_check` push frame — binary keys, atom
        %% `classification`/`category` fields stringified, `undefined`
        %% optional fields mapped to `null`.
        Finding = #{
            owner => <<"Caller">>,
            changed_class => <<"Changed">>,
            selector => <<"foo">>,
            classification => removal,
            severity => <<"hint">>,
            category => undefined,
            message => <<"foo was removed">>,
            note => undefined,
            sites => [#{method => <<"bar">>, line => 3}],
            start => 0,
            'end' => 0
        },
        _ = beamtalk_workspace_findings_store:put_owner_origin(
            <<"Caller">>, <<"Changed">>, [Finding]
        ),
        Msg = make_msg(<<"reload-findings">>, <<"rf-2">>, undefined),
        {value, Value} = beamtalk_repl_ops_dev:handle_term(<<"reload-findings">>, #{}, Msg, self()),
        [Wire] = maps:get(<<"findings">>, Value),
        ?assertEqual(<<"Caller">>, maps:get(<<"owner">>, Wire)),
        ?assertEqual(<<"Changed">>, maps:get(<<"changedClass">>, Wire)),
        ?assertEqual(<<"removal">>, maps:get(<<"classification">>, Wire)),
        ?assertEqual(null, maps:get(<<"category">>, Wire))
    end).

validate_list_classes_filter_user_test() ->
    %% list-classes with the "user" filter takes the not-stdlib branch and must
    %% include the (non-stdlib) WidgetDev fixture-free path: here we only assert
    %% the op succeeds and returns a class_list (the user filter resolves cheaply).
    Msg = make_msg(<<"list-classes">>, <<"lcu-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>, #{<<"filter">> => <<"user">>}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assertEqual(false, maps:is_key(<<"error">>, Decoded)),
    ?assert(is_list(maps:get(<<"class_list">>, Decoded))).

resolve_qualified_consecutive_uppercase_test() ->
    %% Class name with consecutive uppercase letters exercises the
    %% camel_to_snake conversion (PrevWasLower=false branch). The module atom
    %% bt@stdlib@a_b_c_widget does not exist, so the result is {error, badarg},
    %% but the conversion path is still walked.
    ?assertEqual(
        {error, badarg},
        beamtalk_repl_ops_dev:resolve_qualified_class_name(<<"stdlib@ABCWidget">>)
    ).

handle_list_classes_non_binary_filter_test() ->
    %% A filter that is neither undefined nor a binary hits the catch-all clause
    %% of validate_list_classes_filter/1 → {error, FilterStr} → argument error.
    Msg = make_msg(<<"list-classes">>, <<"lcn-1">>, undefined),
    Result = beamtalk_repl_ops_dev:handle(
        <<"list-classes">>, #{<<"filter">> => 12345}, Msg, self()
    ),
    Decoded = json:decode(Result),
    ?assert(maps:is_key(<<"error">>, Decoded)),
    ErrMsg = maps:get(<<"error">>, Decoded),
    ?assertNotEqual(nomatch, binary:match(ErrMsg, <<"Unknown filter">>)).
