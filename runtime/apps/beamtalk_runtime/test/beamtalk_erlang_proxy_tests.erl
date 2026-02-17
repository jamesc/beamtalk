%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Unit tests for beamtalk_erlang_proxy module (BT-676).
%%
%% **DDD Context:** Runtime — BEAM Interop
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
    %% function_clause errors should be caught and wrapped as arity_mismatch
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        %% lists:nth with invalid args triggers function_clause
        beamtalk_erlang_proxy:dispatch('nth:', [0, [a]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
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

%%% ===================================================================
%%% ===================================================================
%%% Exception mapping — BT-678
%%% ===================================================================

dispatch_exit_wraps_as_exit_error_test() ->
    %% exit:Reason should be caught and wrapped as erlang_exit
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('exit:', [killed], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(erlang_exit, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assert(is_binary(Inner#beamtalk_error.hint)),
            ?assertNotEqual(undefined, Inner#beamtalk_error.hint)
    end.

dispatch_throw_wraps_as_throw_error_test() ->
    %% throw:Value should be caught and wrapped as erlang_throw
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('throw:', [oops], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(erlang_throw, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assert(is_binary(Inner#beamtalk_error.hint)),
            ?assertNotEqual(undefined, Inner#beamtalk_error.hint)
    end.

dispatch_badarith_wraps_as_type_error_test() ->
    %% error:badarith should be caught and wrapped as type_error
    %% erlang:'+'(1, a) raises badarith
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('+:', [1, a], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarith, maps:get(erlang_error, Details))
    end.

dispatch_exit_preserves_details_test() ->
    %% exit errors should preserve the original reason in details
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('exit:', [normal], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(erlang_exit, Inner#beamtalk_error.kind),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(normal, maps:get(erlang_exit_reason, Details))
    end.

dispatch_throw_preserves_details_test() ->
    %% throw errors should preserve the original value in details
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('throw:', [{custom, value}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(erlang_throw, Inner#beamtalk_error.kind),
            Details = Inner#beamtalk_error.details,
            ?assertEqual({custom, value}, maps:get(erlang_throw_value, Details))
    end.

dispatch_exit_hint_includes_module_test() ->
    %% Hint should include module and function name
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('exit:', [killed], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"erlang">>)),
            ?assertNotEqual(nomatch, binary:match(Hint, <<"exit">>))
    end.

dispatch_throw_hint_includes_module_test() ->
    %% Hint should include module and function name
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('throw:', [oops], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"erlang">>)),
            ?assertNotEqual(nomatch, binary:match(Hint, <<"throw">>))
    end.

dispatch_badarg_from_binary_to_atom_preserves_details_test() ->
    %% binary_to_existing_atom with non-existent atom raises badarg,
    %% which is caught by the specific badarg handler
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('binary_to_existing_atom:', [<<"zzzz_nonexistent_atom_xyz">>], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assert(is_binary(Inner#beamtalk_error.hint)),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarg, maps:get(erlang_error, Details))
    end.

dispatch_function_clause_is_arity_mismatch_test() ->
    %% function_clause should map to arity_mismatch kind (ADR 0028)
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('nth:', [0, [a]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind)
    end.

dispatch_badarg_preserves_details_test() ->
    %% badarg errors should include erlang_error in details (ADR 0028)
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('abs:', [not_a_number], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarg, maps:get(erlang_error, Details))
    end.

dispatch_function_clause_preserves_details_test() ->
    %% function_clause errors should include erlang_error in details (ADR 0028)
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('nth:', [0, [a]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            Details = Inner#beamtalk_error.details,
            ?assertEqual(function_clause, maps:get(erlang_error, Details))
    end.

dispatch_missing_function_details_test() ->
    %% BT-679: Missing function should include details about module/function/arity
    %% (Previously tested undef, but BT-679 export validation catches this first)
    Proxy = beamtalk_erlang_proxy:new(math),
    try
        beamtalk_erlang_proxy:dispatch('init', [], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(math, maps:get(module, Details)),
            ?assertEqual(init, maps:get(function, Details)),
            ?assertEqual(0, maps:get(arity, Details))
    end.

dispatch_reraises_beamtalk_exceptions_test() ->
    %% If the called Erlang function raises a Beamtalk exception,
    %% the proxy must re-raise it unchanged, not re-wrap as runtime_error
    Proxy = beamtalk_erlang_proxy:new(erlang),
    OrigError = beamtalk_error:new(type_error, 'Integer'),
    OrigError1 = beamtalk_error:with_selector(OrigError, '+:'),
    Wrapped = beamtalk_exception_handler:wrap(OrigError1),
    try
        %% erlang:error/1 with an already-wrapped Beamtalk exception
        beamtalk_erlang_proxy:dispatch('error:', [Wrapped], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            %% Should preserve original kind/class, not re-wrap as runtime_error
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('Integer', Inner#beamtalk_error.class),
            ?assertEqual('+:', Inner#beamtalk_error.selector)
    end.

dispatch_generic_error_maps_to_runtime_error_test() ->
    %% Generic error:Reason should map to runtime_error and preserve details
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        %% erlang:error/1 with a custom reason triggers generic error handler
        beamtalk_erlang_proxy:dispatch('error:', [{custom_reason, 42}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assert(is_binary(Inner#beamtalk_error.hint)),
            Details = Inner#beamtalk_error.details,
            ?assertEqual({custom_reason, 42}, maps:get(erlang_error, Details))
    end.

%%% ===================================================================
%%% BT-679: Export introspection — wrong arity
%%% ===================================================================

dispatch_wrong_arity_raises_test() ->
    %% lists:reverse exists with arity 1 and 2, not 0
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('reverse', [], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertEqual('reverse', Inner#beamtalk_error.selector),
            %% Hint should mention existing arities
            ?assert(is_binary(Inner#beamtalk_error.hint)),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"exists but was called with">>))
    end.

dispatch_wrong_arity_keyword_raises_test() ->
    %% lists:reverse/1 called with 3 args
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('reverse:extra:extra:', [[1], foo, bar], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"exists but was called with 3 arguments">>))
    end.

%%% ===================================================================
%%% BT-679: Export introspection — missing function
%%% ===================================================================

dispatch_missing_function_raises_test() ->
    %% lists:nonexistent_xyz does not exist
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('nonexistent_xyz:', [1], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint,
                <<"This Erlang function does not exist">>))
    end.

%%% ===================================================================
%%% BT-679: Export introspection — unloaded module
%%% ===================================================================

dispatch_unloaded_module_raises_test() ->
    Proxy = beamtalk_erlang_proxy:new(completely_bogus_module_xyz),
    try
        beamtalk_erlang_proxy:dispatch('reverse:', [[1]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint,
                <<"is not loaded. Is it on the code path?">>))
    end.

%%% ===================================================================
%%% BT-679: methods — REPL discoverability
%%% ===================================================================

dispatch_methods_returns_exports_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('methods', [], Proxy),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),
    %% Should contain reverse/1
    ?assert(lists:member(<<"reverse/1">>, Result)),
    %% Should contain sort/1
    ?assert(lists:member(<<"sort/1">>, Result)),
    %% Should NOT contain module_info
    ?assertNot(lists:any(fun(S) ->
        binary:match(S, <<"module_info">>) =/= nomatch
    end, Result)).

dispatch_methods_sorted_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('methods', [], Proxy),
    ?assertEqual(Result, lists:sort(Result)).

dispatch_methods_unloaded_module_raises_test() ->
    Proxy = beamtalk_erlang_proxy:new(completely_bogus_module_xyz),
    try
        beamtalk_erlang_proxy:dispatch('methods', [], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint,
                <<"is not loaded. Is it on the code path?">>))
    end.

%%% ===================================================================
%%% BT-679: call:args: escape hatch
%%% ===================================================================

dispatch_call_args_basic_test() ->
    %% call:args: should bypass selector mapping and call the function directly
    Proxy = beamtalk_erlang_proxy:new(lists),
    Result = beamtalk_erlang_proxy:dispatch('call:args:', [reverse, {[1, 2, 3]}], Proxy),
    ?assertEqual([3, 2, 1], Result).

dispatch_call_args_zero_arity_test() ->
    %% call:args: with empty args tuple calls zero-arg function
    Proxy = beamtalk_erlang_proxy:new(erlang),
    Result = beamtalk_erlang_proxy:dispatch('call:args:', [node, {}], Proxy),
    ?assert(is_atom(Result)).

dispatch_call_args_missing_function_test() ->
    %% call:args: should still validate exports
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [nonexistent_xyz, {}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind)
    end.

dispatch_call_args_wrong_arity_test() ->
    %% call:args: validates arity too
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [reverse, {}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"exists but was called with">>))
    end.

dispatch_call_args_non_atom_selector_test() ->
    %% call:args: rejects non-atom selector
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [<<"reverse">>, {}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"must be a Symbol">>))
    end.

dispatch_call_args_non_tuple_args_test() ->
    %% call:args: rejects non-tuple args
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [reverse, [[1, 2, 3]]], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(Hint, <<"must be a Tuple">>))
    end.

%%% ===================================================================
%%% BT-679: Beamtalk error passthrough
%%% ===================================================================

dispatch_preserves_beamtalk_errors_test() ->
    %% If an Erlang function raises a beamtalk_error, it should pass through
    %% without being re-wrapped as a type_error
    Proxy = beamtalk_erlang_proxy:new(beamtalk_error),
    %% beamtalk_error:raise/1 raises a structured error — calling it through
    %% the proxy should preserve the error kind
    try
        Error = beamtalk_error:new(does_not_understand, 'TestClass'),
        beamtalk_erlang_proxy:dispatch('raise:', [Error], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            %% Should preserve the original error, not wrap as type_error
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('TestClass', Inner#beamtalk_error.class)
    end.
