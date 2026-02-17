%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_erlang_proxy module (BT-676).
%%
%% Tests ErlangModule proxy dispatch, Erlang class-side proxy,
%% error wrapping, and printString formatting.
-module(beamtalk_erlang_proxy_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ===================================================================
%%% Proxy construction
%%% ===================================================================

new_creates_tagged_map_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual('ErlangModule', maps:get('$beamtalk_class', Proxy)),
    ?assertEqual(lists, maps:get(module, Proxy)).

new_different_module_test() ->
    Proxy = beamtalk_erlang_proxy:new(erlang),
    ?assertEqual(erlang, maps:get(module, Proxy)).

%%% ===================================================================
%%% class_of via tagged map
%%% ===================================================================

class_of_returns_erlang_module_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual('ErlangModule', beamtalk_tagged_map:class_of(Proxy)).

class_of_via_primitive_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual('ErlangModule', beamtalk_primitive:class_of(Proxy)).

%%% ===================================================================
%%% dispatch/3 — Object protocol
%%% ===================================================================

dispatch_class_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual('ErlangModule', beamtalk_erlang_proxy:dispatch('class', [], Proxy)).

dispatch_print_string_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual(<<"#ErlangModule<lists>">>, beamtalk_erlang_proxy:dispatch('printString', [], Proxy)).

dispatch_print_string_erlang_test() ->
    Proxy = beamtalk_erlang_proxy:new(erlang),
    ?assertEqual(<<"#ErlangModule<erlang>">>, beamtalk_erlang_proxy:dispatch('printString', [], Proxy)).

%%% ===================================================================
%%% dispatch/3 — Erlang function calls
%%% ===================================================================

dispatch_keyword_single_arg_test() ->
    %% lists:reverse([1,2,3]) → [3,2,1]
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('reverse:', [[1, 2, 3]], Proxy),
    ?assertEqual([3, 2, 1], Result).

dispatch_keyword_multi_arg_test() ->
    %% lists:nth(1, [a, b, c]) → a
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('nth:from:', [1, [a, b, c]], Proxy),
    ?assertEqual(a, Result).

dispatch_unary_zero_arg_test() ->
    %% erlang:node() → current node
    Proxy = beamtalk_erlang_proxy:new(erlang),
    Result = beamtalk_erlang_proxy:dispatch('node', [], Proxy),
    ?assert(is_atom(Result)).

dispatch_unary_with_arg_test() ->
    %% erlang:length([1,2,3]) → 3
    Proxy = beamtalk_erlang_proxy:new(erlang),
    Result = beamtalk_erlang_proxy:dispatch('length:', [[1, 2, 3]], Proxy),
    ?assertEqual(3, Result).

dispatch_lists_sort_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('sort:', [[3, 1, 2]], Proxy),
    ?assertEqual([1, 2, 3], Result).

%%% ===================================================================
%%% dispatch/3 — Error wrapping
%%% ===================================================================

dispatch_unknown_function_raises_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('nonexistent_function:', [foo], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertEqual('nonexistent_function:', Inner#beamtalk_error.selector),
            ?assert(is_binary(Inner#beamtalk_error.hint))
    end.

dispatch_unknown_module_raises_test() ->
    Proxy = beamtalk_erlang_proxy:new(nonexistent_module_xyz),
    try
        beamtalk_erlang_proxy:dispatch('some_func:', [1], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class)
    end.

%%% ===================================================================
%%% Integration via beamtalk_primitive:send/3
%%% ===================================================================

send_erlang_module_proxy_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_primitive:send(Proxy, 'reverse:', [[a, b, c]]),
    ?assertEqual([c, b, a], Result).

send_erlang_module_class_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual('ErlangModule', beamtalk_primitive:send(Proxy, 'class', [])).

send_erlang_module_print_string_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual(<<"#ErlangModule<lists>">>, beamtalk_primitive:send(Proxy, 'printString', [])).

%%% ===================================================================
%%% Erlang class-side proxy (construct ErlangModule)
%%% ===================================================================

erlang_proxy_creates_module_proxy_test() ->
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    Result = beamtalk_primitive:send(ErlangObj, 'lists', []),
    ?assertEqual('ErlangModule', maps:get('$beamtalk_class', Result)),
    ?assertEqual(lists, maps:get(module, Result)).

erlang_proxy_class_test() ->
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    ?assertEqual('Erlang', beamtalk_primitive:send(ErlangObj, 'class', [])).

erlang_proxy_print_string_test() ->
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    ?assertEqual(<<"Erlang">>, beamtalk_primitive:send(ErlangObj, 'printString', [])).

%%% ===================================================================
%%% End-to-end: Erlang → module → call
%%% ===================================================================

erlang_to_module_to_call_test() ->
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    ListsProxy = beamtalk_primitive:send(ErlangObj, 'lists', []),
    Result = beamtalk_primitive:send(ListsProxy, 'reverse:', [[1, 2, 3]]),
    ?assertEqual([3, 2, 1], Result).

%%% ===================================================================
%%% print_string via beamtalk_primitive
%%% ===================================================================

primitive_print_string_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assertEqual(<<"#ErlangModule<lists>">>, beamtalk_primitive:print_string(Proxy)).

%%% ===================================================================
%%% responds_to/2
%%% ===================================================================

responds_to_erlang_module_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    ?assert(beamtalk_primitive:responds_to(Proxy, 'class')),
    ?assert(beamtalk_primitive:responds_to(Proxy, 'printString')),
    ?assert(beamtalk_primitive:responds_to(Proxy, 'reverse:')).

responds_to_erlang_test() ->
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    ?assert(beamtalk_primitive:responds_to(ErlangObj, 'class')),
    ?assert(beamtalk_primitive:responds_to(ErlangObj, 'lists')).

%%% ===================================================================
%%% Edge cases from adversarial review
%%% ===================================================================

erlang_keyword_selector_raises_test() ->
    %% Keyword selectors on Erlang proxy should raise, not create invalid module
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    try
        beamtalk_primitive:send(ErlangObj, 'reverse:', [foo]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('Erlang', Inner#beamtalk_error.class)
    end.

dispatch_function_clause_error_wraps_test() ->
    %% function_clause errors should be caught and wrapped
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        %% lists:nth with invalid args triggers function_clause
        beamtalk_erlang_proxy:dispatch('nth:', [0, [a]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class)
    end.

erlang_proxy_with_args_raises_arity_mismatch_test() ->
    %% Module lookup should reject non-empty args
    ErlangObj = #{'$beamtalk_class' => 'Erlang'},
    try
        beamtalk_primitive:send(ErlangObj, 'lists', [unexpected_arg]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            ?assertEqual('Erlang', Inner#beamtalk_error.class)
    end.
