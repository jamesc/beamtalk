%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_proxy_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_erlang_proxy module.

Tests ErlangModule proxy dispatch, Erlang class-side proxy,
error wrapping, and printString formatting.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Exported so the proxy can `erlang:apply/3` them (external call). Fixtures for
%% the charlist-retry exit/throw tests: raise `badarg` on a binary arg (to
%% trigger the retry) then `exit`/`throw` on the charlist-coerced retry arg.
-export([retry_then_exit/1, retry_then_throw/1, write_then_badarg/1, retry_class/0]).
%% BT-4021 fixtures: a target that raises `undef` itself, and one whose charlist
%% retry hits a nested `undef`.
-export([explicit_undef_stub/1, retry_then_nested_undef/1, empty_stack_undef_stub/1]).
%% BT-4153 fixtures: bare-ok and charlist results for mode-equivalence tests.
-export([bt4153_ok_stub/0, bt4153_charlist_stub/0]).

bt4153_ok_stub() -> ok.

bt4153_charlist_stub() -> "hi".

%% Post-BT-4173: error message/hint and coerced FFI text are tagged Strings.
bt_bin({beamtalk_string, B}) -> B.

explicit_undef_stub(_Arg) -> erlang:error(undef).

%% Raises `undef` with an empty stacktrace from inside an exported target.
empty_stack_undef_stub(_Arg) -> erlang:raise(error, undef, []).

retry_then_nested_undef(Arg) when is_binary(Arg) -> erlang:error(badarg);
retry_then_nested_undef(Arg) when is_list(Arg) -> completely_bogus_module_xyz:missing(1).

retry_then_exit(Arg) when is_binary(Arg) -> erlang:error(badarg);
retry_then_exit(Arg) when is_list(Arg) -> exit(retry_exit_reason).

retry_then_throw(Arg) when is_binary(Arg) -> erlang:error(badarg);
retry_then_throw(Arg) when is_list(Arg) -> throw(retry_throw_value).

%% BT-3728: a failed first attempt writes class variable `a` then raises `badarg`
%% (what a block run inside the FFI call could do); the charlist retry reports the
%% value of `a` it sees.
write_then_badarg(Arg) when is_binary(Arg) ->
    beamtalk_class_vars:put(retry_self_obj(), a, 99),
    erlang:error(badarg);
write_then_badarg(Arg) when is_list(Arg) ->
    beamtalk_class_vars:get(retry_self_obj(), a).

%% The base class atom must exist (beamtalk_class_vars resolves a class tag to its
%% name with an existing-atom lookup); this function keeps it in the atom table.
retry_class() -> 'ProxyRetryTestClass'.

retry_self_obj() ->
    #beamtalk_object{class = 'ProxyRetryTestClass class', class_mod = prtc, pid = self()}.

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
    ?assertEqual(
        <<"#ErlangModule<lists>">>, beamtalk_erlang_proxy:dispatch('printString', [], Proxy)
    ).

dispatch_print_string_erlang_test() ->
    Proxy = beamtalk_erlang_proxy:new(erlang),
    ?assertEqual(
        <<"#ErlangModule<erlang>">>, beamtalk_erlang_proxy:dispatch('printString', [], Proxy)
    ).

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
            ?assertMatch({beamtalk_string, _}, Inner#beamtalk_error.hint)
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
%%% Exception mapping — ADR 0101 Part 2
%%%
%%% ADR 0101 unifies both apply paths: `exit:*` propagates and `throw:*`
%%% passes through (rolling back the former erlang_exit / erlang_throw
%%% wrapping in validate_and_apply). `error:*` is still wrapped.
%%% ===================================================================

dispatch_exit_propagates_test() ->
    %% ADR 0101: exit must propagate, NOT be wrapped as erlang_exit
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('exit:', [killed], Proxy),
        ?assert(false)
    catch
        exit:killed -> ok
    end.

dispatch_throw_passes_through_test() ->
    %% ADR 0101: throw must pass through unchanged, NOT be wrapped as erlang_throw
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('throw:', [oops], Proxy),
        ?assert(false)
    catch
        throw:oops -> ok
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

dispatch_exit_preserves_reason_test() ->
    %% ADR 0101: the exit propagates with its original reason intact
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('exit:', [{shutdown, restart}], Proxy),
        ?assert(false)
    catch
        exit:{shutdown, restart} -> ok
    end.

dispatch_throw_preserves_value_test() ->
    %% ADR 0101: the throw passes through with its original value intact
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('throw:', [{custom, value}], Proxy),
        ?assert(false)
    catch
        throw:{custom, value} -> ok
    end.

dispatch_badarg_from_binary_to_atom_preserves_details_test() ->
    %% binary_to_existing_atom with non-existent atom raises badarg,
    %% which is caught by the specific badarg handler
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch(
            'binary_to_existing_atom:', [<<"zzzz_nonexistent_atom_xyz">>], Proxy
        ),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertMatch({beamtalk_string, _}, Inner#beamtalk_error.hint),
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
    %% badarg errors should include erlang_error and erlang_stacktrace in details (ADR 0028)
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        beamtalk_erlang_proxy:dispatch('abs:', [not_a_number], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarg, maps:get(erlang_error, Details)),
            ?assert(is_list(maps:get(erlang_stacktrace, Details)))
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
    %% Missing function should include details about module/function/arity
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
    %% ADR 0101: a generic error:Reason is wrapped via ensure_wrapped (the
    %% canonical classifier). An unclassified reason maps to runtime_error,
    %% located on the ErlangModule breadcrumb, preserving the reason in details.
    Proxy = beamtalk_erlang_proxy:new(erlang),
    try
        %% erlang:error/1 with a custom reason triggers the generic catch-all
        beamtalk_erlang_proxy:dispatch('error:', [{custom_reason, 42}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertEqual('error:', Inner#beamtalk_error.selector),
            ?assertMatch({beamtalk_string, _}, Inner#beamtalk_error.message),
            Details = Inner#beamtalk_error.details,
            ?assertEqual({custom_reason, 42}, maps:get(reason, Details)),
            %% The generic path also carries the FFI `erlang_error`
            %% contract key (+ stacktrace), matching the specific clauses, so
            %% `details at: #erlang_error` resolves uniformly.
            ?assertEqual({custom_reason, 42}, maps:get(erlang_error, Details)),
            ?assert(is_list(maps:get(erlang_stacktrace, Details)))
    end.

dispatch_badkey_is_readable_key_error_test() ->
    %% ADR 0101: a {badkey, K} BEAM error from FFI is wrapped by default
    %% via ensure_wrapped into a readable key_error (not a raw leak).
    Proxy = beamtalk_erlang_proxy:new(maps),
    try
        beamtalk_erlang_proxy:dispatch('get:with:', [missingKey, #{other => 1}], Proxy),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(key_error, Inner#beamtalk_error.kind),
            Msg = Inner#beamtalk_error.message,
            ?assertMatch({beamtalk_string, _}, Msg),
            ?assertNotEqual(nomatch, binary:match(bt_bin(Msg), <<"key not found">>))
    end.

%%% ===================================================================
%%% Export introspection — wrong arity
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
            ?assertMatch({beamtalk_string, _}, Inner#beamtalk_error.hint),
            Hint = Inner#beamtalk_error.hint,
            ?assertNotEqual(nomatch, binary:match(bt_bin(Hint), <<"exists but was called with">>))
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
            ?assertNotEqual(
                nomatch, binary:match(bt_bin(Hint), <<"exists but was called with 3 arguments">>)
            )
    end.

%%% ===================================================================
%%% Export introspection — missing function
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
            ?assertNotEqual(
                nomatch,
                binary:match(
                    bt_bin(Hint),
                    <<"This Erlang function does not exist">>
                )
            )
    end.

%%% ===================================================================
%%% Export introspection — unloaded module
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
            ?assertNotEqual(
                nomatch,
                binary:match(
                    bt_bin(Hint),
                    <<"is not loaded. Is it on the code path?">>
                )
            )
    end.

%%% ===================================================================
%%% methods — REPL discoverability
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
    ?assertNot(
        lists:any(
            fun(S) ->
                binary:match(S, <<"module_info">>) =/= nomatch
            end,
            Result
        )
    ).

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
            ?assertNotEqual(
                nomatch,
                binary:match(
                    bt_bin(Hint),
                    <<"is not loaded. Is it on the code path?">>
                )
            )
    end.

%%% ===================================================================
%%% call:args: escape hatch
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
            ?assertNotEqual(nomatch, binary:match(bt_bin(Hint), <<"exists but was called with">>))
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
            ?assertNotEqual(nomatch, binary:match(bt_bin(Hint), <<"must be a Symbol">>))
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
            ?assertNotEqual(nomatch, binary:match(bt_bin(Hint), <<"must be a Tuple">>))
    end.

%%% ===================================================================
%%% direct_call/3 — charlist coercion
%%% ===================================================================

direct_call_basic_test() ->
    %% Direct call to lists:reverse works normally
    ?assertEqual([3, 2, 1], beamtalk_erlang_proxy:direct_call(lists, reverse, [[1, 2, 3]])).

direct_call_os_cmd_coerces_string_test() ->
    %% os:cmd/1 expects a charlist — binary arg should be auto-coerced.
    %% Use string:trim/2 (no Characters arg) to strip all trailing whitespace;
    %% string:trim/3 with "\r\n" does not work in OTP 28 because CRLF is a
    %% single grapheme cluster and [13,10] does not match it.
    Result = beamtalk_erlang_proxy:direct_call(os, cmd, [<<"echo hello">>]),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertEqual(<<"hello">>, string:trim(bt_bin(Result), trailing)).

direct_call_exit_propagates_test() ->
    %% exit should propagate naturally, NOT be wrapped
    try
        beamtalk_erlang_proxy:direct_call(erlang, exit, [killed]),
        ?assert(false)
    catch
        exit:killed -> ok
    end.

direct_call_throw_propagates_test() ->
    %% throw should propagate naturally, NOT be wrapped
    try
        beamtalk_erlang_proxy:direct_call(erlang, throw, [oops]),
        ?assert(false)
    catch
        throw:oops -> ok
    end.

direct_call_missing_function_raises_test() ->
    try
        beamtalk_erlang_proxy:direct_call(lists, nonexistent_xyz, [1]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind)
    end.

direct_call_wrong_arity_raises_test() ->
    try
        beamtalk_erlang_proxy:direct_call(lists, reverse, []),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind)
    end.

direct_call_unloaded_module_raises_test() ->
    try
        beamtalk_erlang_proxy:direct_call(completely_bogus_module_xyz, foo, []),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind)
    end.

direct_call_badarg_no_binary_raises_test() ->
    %% badarg with no binary args should raise (no coercion possible)
    try
        beamtalk_erlang_proxy:direct_call(erlang, abs, [not_a_number]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarg, maps:get(erlang_error, Details))
    end.

direct_call_coercion_retry_still_fails_test() ->
    %% binary_to_existing_atom with non-existent atom: badarg after coercion
    %% should still raise structured error
    try
        beamtalk_erlang_proxy:direct_call(
            erlang, binary_to_existing_atom, [<<"zzzz_nonexistent_atom_xyz">>]
        ),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind)
    end.

direct_call_charlist_result_without_badarg_test() ->
    %% Functions that accept non-binary args but return charlists
    %% should still have their results coerced to Strings.
    %% calendar:system_time_to_rfc3339/1 accepts an integer and returns a charlist.
    Now = erlang:system_time(second),
    Result = beamtalk_erlang_proxy:direct_call(calendar, system_time_to_rfc3339, [Now]),
    ?assertMatch({beamtalk_string, _}, Result).

direct_call_integer_list_not_coerced_test() ->
    %% Ensure lists of integers that aren't valid charlists are NOT coerced.
    %% lists:seq/2 returns a plain integer list.
    Result = beamtalk_erlang_proxy:direct_call(lists, seq, [1, 5]),
    ?assertEqual([1, 2, 3, 4, 5], Result).

direct_call_empty_list_not_coerced_test() ->
    %% Empty list must stay as [], not be coerced to <<>> (empty binary).
    Result = beamtalk_erlang_proxy:direct_call(maps, keys, [#{}]),
    ?assertEqual([], Result).

%%% ===================================================================
%%% dispatch/3 charlist coercion via validate_and_apply
%%% ===================================================================

dispatch_os_cmd_coerces_string_test() ->
    %% Verify coercion also works through dispatch path.
    %% Use string:trim/2 (no Characters arg) — see direct_call_os_cmd_coerces_string_test.
    Proxy = beamtalk_erlang_proxy:new(os),
    Result = beamtalk_erlang_proxy:dispatch('cmd:', [<<"echo hello">>], Proxy),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertEqual(<<"hello">>, string:trim(bt_bin(Result), trailing)).

%%% ===================================================================
%%% Beamtalk error passthrough
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

%%% ===================================================================
%%% Actor proxy → Pid coercion for Erlang FFI
%%% ===================================================================

direct_call_actor_to_pid_coercion_test() ->
    %% Passing a #beamtalk_object{pid=Pid} to an Erlang function that
    %% expects a raw pid should auto-coerce on badarg retry.
    {ok, Counter} = test_counter:start_link(0),
    ActorObj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Counter},
    %% erlang:is_process_alive/1 expects a pid — actor object should be coerced
    Result = beamtalk_erlang_proxy:direct_call(erlang, is_process_alive, [ActorObj]),
    ?assertEqual(true, Result),
    gen_server:stop(Counter).

direct_call_actor_monitor_coercion_test() ->
    %% erlang:monitor(process, ActorObj) should coerce actor to pid
    {ok, Counter} = test_counter:start_link(0),
    ActorObj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Counter},
    Ref = beamtalk_erlang_proxy:direct_call(erlang, monitor, [process, ActorObj]),
    ?assert(is_reference(Ref)),
    erlang:demonitor(Ref, [flush]),
    gen_server:stop(Counter).

%%% ===================================================================
%%% ADR 0076: ok/error tuple → Result coercion
%%% ===================================================================

coerce_result_ok_tuple_test() ->
    %% {ok, Value} → Result ok: Value
    Result = beamtalk_erlang_proxy:coerce_result({ok, 42}),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    ?assertEqual(42, maps:get('okValue', Result)).

coerce_result_error_tuple_test() ->
    %% {error, Reason} → Result error: Reason (wrapped as Exception)
    Result = beamtalk_erlang_proxy:coerce_result({error, enoent}),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(false, maps:get('isOk', Result)),
    ?assertEqual(nil, maps:get('okValue', Result)),
    %% errReason should be wrapped as an Exception tagged map via ensure_wrapped
    ErrReason = maps:get('errReason', Result),
    ?assert(is_map(ErrReason)),
    ?assert(maps:is_key('$beamtalk_class', ErrReason)).

coerce_result_bare_ok_atom_test() ->
    %% bare ok → Result ok: nil
    Result = beamtalk_erlang_proxy:coerce_result(ok),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    ?assertEqual(nil, maps:get('okValue', Result)).

coerce_ffi_result_beamtalk_module_bare_error_passthrough_test() ->
    %% BT-3660: bare `error` from a beamtalk_* module is a value, not a sentinel
    ?assertEqual(error, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_logging_config, error)),
    ?assertEqual(error, beamtalk_erlang_proxy:coerce_ffi_result('bt@stdlib@list', error)),
    %% ...but {error, _} and ok are still coerced
    ?assertEqual(
        'Result',
        maps:get(
            '$beamtalk_class',
            beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_logging_config, {error, x})
        )
    ),
    %% stock Erlang modules keep the sentinel coercion
    ?assertEqual(
        false,
        maps:get('isOk', beamtalk_erlang_proxy:coerce_ffi_result(maps, error))
    ).

coerce_result_bare_error_atom_test() ->
    %% bare error → Result error: nil (nil wrapped as Exception)
    Result = beamtalk_erlang_proxy:coerce_result(error),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(false, maps:get('isOk', Result)),
    ?assertEqual(nil, maps:get('okValue', Result)),
    %% errReason wraps nil via ensure_wrapped
    ErrReason = maps:get('errReason', Result),
    ?assert(is_map(ErrReason)).

coerce_result_three_element_ok_tuple_passthrough_test() ->
    %% {ok, V1, V2} passes through unchanged (3+ elements)
    Input = {ok, 1, 2},
    ?assertEqual(Input, beamtalk_erlang_proxy:coerce_result(Input)).

coerce_result_three_element_error_tuple_passthrough_test() ->
    %% {error, R1, R2} passes through unchanged (3+ elements)
    Input = {error, reason, details},
    ?assertEqual(Input, beamtalk_erlang_proxy:coerce_result(Input)).

coerce_result_non_ok_error_tuple_passthrough_test() ->
    %% {foo, bar} passes through unchanged
    Input = {foo, bar},
    ?assertEqual(Input, beamtalk_erlang_proxy:coerce_result(Input)).

coerce_result_non_tuple_passthrough_test() ->
    %% Non-tuple values pass through unchanged
    ?assertEqual(42, beamtalk_erlang_proxy:coerce_result(42)),
    ?assertEqual(<<"hello">>, beamtalk_erlang_proxy:coerce_result(<<"hello">>)),
    ?assertEqual([1, 2, 3], beamtalk_erlang_proxy:coerce_result([1, 2, 3])),
    ?assertEqual(foo, beamtalk_erlang_proxy:coerce_result(foo)).

coerce_result_ok_charlist_normalized_to_string_test() ->
    %% {ok, Charlist} should normalize charlist to a String inside Result
    Result = beamtalk_erlang_proxy:coerce_result({ok, "hello"}),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    ?assertEqual({beamtalk_string, <<"hello">>}, maps:get('okValue', Result)).

coerce_result_error_charlist_normalized_to_string_test() ->
    %% {error, Charlist} should normalize charlist to a String inside Result
    Result = beamtalk_erlang_proxy:coerce_result({error, "not found"}),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(false, maps:get('isOk', Result)),
    %% The error reason is wrapped as an Exception; check the inner value
    ErrReason = maps:get('errReason', Result),
    ?assert(is_map(ErrReason)).

coerce_result_ok_non_charlist_unchanged_test() ->
    %% {ok, NonCharlist} should not be affected by normalization
    Result = beamtalk_erlang_proxy:coerce_result({ok, 42}),
    ?assertEqual(42, maps:get('okValue', Result)),
    Result2 = beamtalk_erlang_proxy:coerce_result({ok, <<"binary">>}),
    ?assertEqual(<<"binary">>, maps:get('okValue', Result2)).

direct_call_ok_tuple_coerced_to_result_test() ->
    %% file:get_cwd/0 returns {ok, Dir} — should become Result ok: Dir (String)
    Result = beamtalk_erlang_proxy:direct_call(file, get_cwd, []),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    %% okValue should be a String (charlist coerced inside Result)
    OkValue = maps:get('okValue', Result),
    ?assertNotEqual(nil, OkValue),
    ?assertMatch({beamtalk_string, _}, OkValue).

direct_call_error_tuple_coerced_to_result_test() ->
    %% file:read_file on nonexistent file returns {error, enoent}
    Result = beamtalk_erlang_proxy:direct_call(file, read_file, [<<"/nonexistent_file_xyz_12345">>]),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(false, maps:get('isOk', Result)).

dispatch_ok_tuple_coerced_to_result_test() ->
    %% Dispatch path also coerces ok/error tuples
    Proxy = beamtalk_erlang_proxy:new(file),
    Result = beamtalk_erlang_proxy:dispatch('get_cwd', [], Proxy),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    %% charlist inside Result should be coerced to a String
    ?assertMatch({beamtalk_string, _}, maps:get('okValue', Result)).

%%% ===================================================================
%%% ADR 0101 Part 2 — unified wrap-by-default on the inline-FFI boundary
%%%
%%% direct_call/3 previously leaked function_clause / generic error:* (it only
%%% caught badarg/undef). The unified apply path now wraps all error:* while
%%% propagating exit/throw. These cover each exception class on the inline path.
%%% ===================================================================

direct_call_function_clause_wraps_as_arity_mismatch_test() ->
    %% lists:nth(0, [a]) raises function_clause — previously leaked from direct_call
    try
        beamtalk_erlang_proxy:direct_call(lists, nth, [0, [a]]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(function_clause, maps:get(erlang_error, Details))
    end.

direct_call_generic_error_wraps_as_runtime_error_test() ->
    %% erlang:error/1 with a custom reason — previously leaked raw from direct_call,
    %% now wrapped via ensure_wrapped into a structured runtime_error.
    try
        beamtalk_erlang_proxy:direct_call(erlang, error, [{custom_reason, 42}]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            Details = Inner#beamtalk_error.details,
            ?assertEqual({custom_reason, 42}, maps:get(reason, Details)),
            %% Generic path carries the FFI `erlang_error` key too.
            ?assertEqual({custom_reason, 42}, maps:get(erlang_error, Details)),
            ?assert(is_list(maps:get(erlang_stacktrace, Details)))
    end.

direct_call_beamtalk_error_passthrough_test() ->
    %% An already-#beamtalk_error{} must re-raise unchanged (idempotent)
    OrigError = beamtalk_error:new(type_error, 'Integer'),
    OrigError1 = beamtalk_error:with_selector(OrigError, '+:'),
    Wrapped = beamtalk_exception_handler:wrap(OrigError1),
    try
        beamtalk_erlang_proxy:direct_call(erlang, error, [Wrapped]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('Integer', Inner#beamtalk_error.class),
            ?assertEqual('+:', Inner#beamtalk_error.selector)
    end.

direct_call_bare_beamtalk_error_record_passthrough_test() ->
    %% A bare #beamtalk_error{} record (e.g. error(#beamtalk_error{...}) from an
    %% Erlang shim like beamtalk_tracing) must be wrapped into the map form while
    %% PRESERVING its kind — not reclassified as a generic runtime_error.
    BareRecord = beamtalk_error:with_selector(
        beamtalk_error:new(type_error, 'Tracing'), 'hotMethods:'
    ),
    try
        %% erlang:error/1 raises the bare record directly (not the map form)
        beamtalk_erlang_proxy:direct_call(erlang, error, [BareRecord]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('Tracing', Inner#beamtalk_error.class),
            ?assertEqual('hotMethods:', Inner#beamtalk_error.selector)
    end.

%%% ===================================================================
%%% ADR 0101 Part 1/2 — native_call/4 boundary
%%%
%%% native_call skips the export pre-check but routes through the same
%%% apply_with_coercion/5 — same error:* wrapping + 0076 Result coercion as
%%% inline FFI, carrying {Class, Sel} so errors read e.g. Stream>>take:.
%%% ===================================================================

native_call_returns_plain_value_test() ->
    %% Happy path: a non-tuple return is passed through with self-as-first-arg
    %% (lists:reverse(Self) where Self = [1,2,3])
    ?assertEqual(
        [3, 2, 1],
        beamtalk_erlang_proxy:native_call(lists, reverse, [[1, 2, 3]], {'Stream', 'asList'})
    ).

native_call_ok_tuple_coerced_to_result_test() ->
    %% ADR 0076: {ok, V} return is coerced to a Result value (orthogonal to wrapping)
    Result = beamtalk_erlang_proxy:native_call(file, get_cwd, [], {'File', 'cwd'}),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(true, maps:get('isOk', Result)),
    ?assertMatch({beamtalk_string, _}, maps:get('okValue', Result)).

native_call_error_tuple_coerced_to_result_test() ->
    %% ADR 0076: {error, R} return is coerced to a Result error value
    Result = beamtalk_erlang_proxy:native_call(
        file, read_file, [<<"/nonexistent_file_xyz_12345">>], {'File', 'readAll:'}
    ),
    ?assertEqual('Result', maps:get('$beamtalk_class', Result)),
    ?assertEqual(false, maps:get('isOk', Result)).

native_call_undef_is_does_not_understand_with_context_test() ->
    %% Hot-reload TOCTOU: a since-removed backing function must surface a
    %% structured does_not_understand carrying the Beamtalk {Class, Sel}.
    try
        beamtalk_erlang_proxy:native_call(
            lists, nonexistent_fn_xyz, [foo], {'Stream', 'take:'}
        ),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('Stream', Inner#beamtalk_error.class),
            ?assertEqual('take:', Inner#beamtalk_error.selector)
    end.

native_call_badarg_wraps_with_context_test() ->
    %% error:badarg → type_error, but carrying the Beamtalk class/selector
    try
        beamtalk_erlang_proxy:native_call(erlang, abs, [not_a_number], {'Math', 'abs'}),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('Math', Inner#beamtalk_error.class),
            ?assertEqual('abs', Inner#beamtalk_error.selector),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(badarg, maps:get(erlang_error, Details))
    end.

native_call_function_clause_wraps_with_context_test() ->
    %% error:function_clause → arity_mismatch, carrying the Beamtalk class/selector
    try
        beamtalk_erlang_proxy:native_call(lists, nth, [0, [a]], {'List', 'at:'}),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            ?assertEqual('List', Inner#beamtalk_error.class),
            ?assertEqual('at:', Inner#beamtalk_error.selector)
    end.

native_call_generic_error_wraps_with_context_test() ->
    %% any other error:Reason → runtime_error, carrying the Beamtalk class/selector
    try
        beamtalk_erlang_proxy:native_call(
            erlang, error, [{custom_reason, 7}], {'Stream', 'collect:'}
        ),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual('Stream', Inner#beamtalk_error.class),
            ?assertEqual('collect:', Inner#beamtalk_error.selector),
            %% Generic path carries the FFI `erlang_error` key too.
            Details = Inner#beamtalk_error.details,
            ?assertEqual({custom_reason, 7}, maps:get(erlang_error, Details)),
            ?assert(is_list(maps:get(erlang_stacktrace, Details)))
    end.

native_call_exit_propagates_test() ->
    %% ADR 0101: exit propagates unchanged, even with native context
    try
        beamtalk_erlang_proxy:native_call(erlang, exit, [killed], {'Stream', 'take:'}),
        ?assert(false)
    catch
        exit:killed -> ok
    end.

native_call_throw_passes_through_test() ->
    %% ADR 0101: throw passes through unchanged, even with native context
    try
        beamtalk_erlang_proxy:native_call(erlang, throw, [oops], {'Stream', 'take:'}),
        ?assert(false)
    catch
        throw:oops -> ok
    end.

%%% exit/throw raised on the charlist-coercion RETRY (the inner try in
%%% maybe_retry_badarg/6, not the first apply) must still
%%% propagate unchanged. The retry routes through the same classify_ffi_exception/9
%%% catch-all as the first pass, so these guard the shared classifier's retry arm.
native_call_retry_exit_propagates_test() ->
    %% First apply on the binary arg raises badarg → retry with the charlist-coerced
    %% arg, which exits. The exit must propagate, not become a #beamtalk_error{}.
    try
        beamtalk_erlang_proxy:native_call(
            ?MODULE, retry_then_exit, [<<"x">>], {'Stream', 'take:'}
        ),
        ?assert(false)
    catch
        exit:retry_exit_reason -> ok
    end.

native_call_retry_throw_passes_through_test() ->
    %% Same, but the charlist retry throws — the throw must pass through unchanged.
    try
        beamtalk_erlang_proxy:native_call(
            ?MODULE, retry_then_throw, [<<"x">>], {'Stream', 'take:'}
        ),
        ?assert(false)
    catch
        throw:retry_throw_value -> ok
    end.

native_call_badarg_retry_discards_first_attempt_class_var_writes_test() ->
    %% BT-3728: the retry is a fresh attempt; writes the failed first attempt made
    %% (here `a := 99`) must not be visible to it, nor survive the call.
    beamtalk_class_vars_test_support:with_home('ProxyRetryTestClass', #{a => 1}, fun() ->
        ?assertEqual(
            1,
            beamtalk_erlang_proxy:native_call(
                ?MODULE, write_then_badarg, [<<"x">>], {'Stream', 'take:'}
            )
        ),
        ?assertEqual(1, beamtalk_class_vars:get(retry_self_obj(), a))
    end).

native_call_beamtalk_error_passthrough_test() ->
    %% An already-#beamtalk_error{} re-raises unchanged (innermost wins)
    OrigError = beamtalk_error:new(does_not_understand, 'TestClass'),
    OrigError1 = beamtalk_error:with_selector(OrigError, 'foo'),
    Wrapped = beamtalk_exception_handler:wrap(OrigError1),
    try
        beamtalk_erlang_proxy:native_call(erlang, error, [Wrapped], {'Stream', 'take:'}),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('TestClass', Inner#beamtalk_error.class),
            ?assertEqual('foo', Inner#beamtalk_error.selector)
    end.

native_call_charlist_coercion_retry_test() ->
    %% native: path also retries binary→charlist on badarg.
    %% os:cmd/1 expects a charlist; a binary self/arg must be coerced.
    Result = beamtalk_erlang_proxy:native_call(os, cmd, [<<"echo hello">>], {'Os', 'cmd:'}),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertEqual(<<"hello">>, string:trim(bt_bin(Result), trailing)).

%%% ===================================================================
%%% BT-4021 — lazy export validation
%%%
%%% direct_call/3 and validate_and_apply/4 no longer read
%%% module_info(exports) before every apply. The export list is read only after
%%% an error:undef whose top frame is the FFI target. These pin that an undef
%%% raised *inside* the callee is not misreported as the target missing, and
%%% that hot reload / purge stays correct with no cache.
%%% ===================================================================

%% A callee argument that raises error:undef from a nested call to a missing module.
undef_inside_callee_fun() ->
    fun(X) -> completely_bogus_module_xyz:missing(X) end.

assert_undef_inside_callee_is_runtime_error(Fun) ->
    try
        Fun(),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            %% Not does_not_understand / arity_mismatch on the FFI target: the
            %% canonical undef classifier names the *nested* missing function.
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual(
                {beamtalk_string, <<"Undefined function: completely_bogus_module_xyz:missing/1">>},
                Inner#beamtalk_error.message
            ),
            Details = Inner#beamtalk_error.details,
            ?assertEqual(undef, maps:get(erlang_error, Details)),
            ?assertMatch(
                [{completely_bogus_module_xyz, missing, [1], _} | _],
                maps:get(erlang_stacktrace, Details)
            )
    end.

direct_call_undef_inside_callee_is_not_missing_function_test() ->
    %% lists:map/2 exists; the undef comes from the fun it calls.
    assert_undef_inside_callee_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:direct_call(lists, map, [undef_inside_callee_fun(), [1]])
    end).

dispatch_undef_inside_callee_is_not_missing_function_test() ->
    Proxy = beamtalk_erlang_proxy:new(lists),
    assert_undef_inside_callee_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:dispatch('map:with:', [undef_inside_callee_fun(), [1]], Proxy)
    end).

native_call_undef_inside_callee_is_not_missing_function_test() ->
    assert_undef_inside_callee_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:native_call(
            lists, map, [undef_inside_callee_fun(), [1]], {'Stream', 'collect:'}
        )
    end).

direct_call_wrong_arity_error_unchanged_test() ->
    %% The lazily-built arity error keeps the pre-BT-4021 message and details.
    try
        beamtalk_erlang_proxy:direct_call(lists, reverse, [[1], [2], [3]]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertEqual(reverse, Inner#beamtalk_error.selector),
            ?assertEqual(
                {beamtalk_string, <<"lists:reverse/1, 2 exists but was called with 3 arguments">>},
                Inner#beamtalk_error.hint
            ),
            ?assertEqual(
                #{
                    module => lists,
                    function => reverse,
                    expected_arities => [1, 2],
                    actual_arity => 3
                },
                Inner#beamtalk_error.details
            )
    end.

direct_call_missing_function_error_unchanged_test() ->
    try
        beamtalk_erlang_proxy:direct_call(lists, nonexistent_xyz, [1]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('ErlangModule', Inner#beamtalk_error.class),
            ?assertEqual(nonexistent_xyz, Inner#beamtalk_error.selector),
            ?assertEqual(
                {beamtalk_string,
                    <<"This Erlang function does not exist. Check spelling and arity.">>},
                Inner#beamtalk_error.hint
            ),
            ?assertEqual(
                #{module => lists, function => nonexistent_xyz, arity => 1},
                Inner#beamtalk_error.details
            )
    end.

direct_call_unloaded_module_error_unchanged_test() ->
    try
        beamtalk_erlang_proxy:direct_call(completely_bogus_module_xyz, foo, []),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertNotEqual(
                nomatch,
                binary:match(
                    bt_bin(Inner#beamtalk_error.hint), <<"is not loaded. Is it on the code path?">>
                )
            )
    end.

direct_call_explicit_undef_in_target_is_not_arity_mismatch_test() ->
    %% The target exists and runs, then raises `undef` itself. Its own frame is on
    %% top (integer-arity form), but it must not read as "exists but was called
    %% with 1 arguments".
    try
        beamtalk_erlang_proxy:direct_call(?MODULE, explicit_undef_stub, [x]),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual(undef, maps:get(erlang_error, Inner#beamtalk_error.details))
    end.

%% An `undef` with an empty stacktrace carries no frame to compare, so it is
%% treated as "maybe the target" and confirmed against the loaded exports. The
%% target here is exported, so both inline FFI and `native:` must wrap it as a
%% runtime_error, not report the target as missing.
assert_empty_stack_undef_is_runtime_error(Fun) ->
    try
        Fun(),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual(undef, maps:get(erlang_error, Inner#beamtalk_error.details)),
            ?assertEqual([], maps:get(erlang_stacktrace, Inner#beamtalk_error.details))
    end.

direct_call_empty_stack_undef_from_exported_target_test() ->
    assert_empty_stack_undef_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:direct_call(?MODULE, empty_stack_undef_stub, [x])
    end).

native_call_empty_stack_undef_from_exported_target_test() ->
    assert_empty_stack_undef_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:native_call(
            ?MODULE, empty_stack_undef_stub, [x], {'Stream', 'take:'}
        )
    end).

%% Run `Fun` with `backtrace_depth` 0, restoring the old value afterwards.
with_backtrace_depth_zero(Fun) ->
    Old = erlang:system_flag(backtrace_depth, 0),
    try
        Fun()
    after
        erlang:system_flag(backtrace_depth, Old)
    end.

backtrace_depth_zero_keeps_ffi_errors_test() ->
    %% With backtrace_depth 0 the VM still records the undef frame, so a missing
    %% target is still reported as missing (inline and native:), and a nested
    %% undef is still a runtime_error, not a missing FFI function.
    with_backtrace_depth_zero(fun() ->
        try
            beamtalk_erlang_proxy:direct_call(lists, nonexistent_xyz, [1]),
            ?assert(false)
        catch
            error:#{error := Inline} ->
                ?assertEqual(does_not_understand, Inline#beamtalk_error.kind),
                ?assertEqual(
                    #{module => lists, function => nonexistent_xyz, arity => 1},
                    Inline#beamtalk_error.details
                )
        end,
        try
            beamtalk_erlang_proxy:native_call(
                lists, nonexistent_xyz, [1], {'Stream', 'take:'}
            ),
            ?assert(false)
        catch
            error:#{error := Native} ->
                ?assertEqual(does_not_understand, Native#beamtalk_error.kind),
                ?assertEqual('Stream', Native#beamtalk_error.class),
                ?assertEqual('take:', Native#beamtalk_error.selector)
        end,
        assert_undef_inside_callee_is_runtime_error(fun() ->
            beamtalk_erlang_proxy:native_call(
                lists, map, [undef_inside_callee_fun(), [1]], {'Stream', 'collect:'}
            )
        end),
        assert_empty_stack_undef_is_runtime_error(fun() ->
            beamtalk_erlang_proxy:native_call(
                ?MODULE, empty_stack_undef_stub, [x], {'Stream', 'take:'}
            )
        end)
    end).

direct_call_retry_nested_undef_is_runtime_error_test() ->
    %% badarg → charlist retry → nested undef: the retry pass classifies it too.
    assert_undef_inside_callee_is_runtime_error(fun() ->
        beamtalk_erlang_proxy:direct_call(?MODULE, retry_then_nested_undef, [<<"x">>])
    end).

dispatch_call_args_errors_unchanged_test() ->
    %% The `call:args:` path reports its own selector with the pre-BT-4021 text.
    Proxy = beamtalk_erlang_proxy:new(lists),
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [reverse, {}], Proxy),
        ?assert(false)
    catch
        error:#{error := Arity} ->
            ?assertEqual(arity_mismatch, Arity#beamtalk_error.kind),
            ?assertEqual('call:args:', Arity#beamtalk_error.selector),
            ?assertEqual(
                {beamtalk_string, <<"lists:reverse/1, 2 exists but was called with 0 arguments">>},
                Arity#beamtalk_error.hint
            )
    end,
    try
        beamtalk_erlang_proxy:dispatch('call:args:', [nonexistent_xyz, {1}], Proxy),
        ?assert(false)
    catch
        error:#{error := Missing} ->
            ?assertEqual(does_not_understand, Missing#beamtalk_error.kind),
            ?assertEqual('call:args:', Missing#beamtalk_error.selector),
            ?assertEqual(
                #{module => lists, function => nonexistent_xyz, arity => 1},
                Missing#beamtalk_error.details
            )
    end.

direct_call_handle_undefined_function_test() ->
    %% Without the export pre-check, a module's '$handle_undefined_function'/2
    %% sees unknown calls like it would from plain Erlang. When it re-raises via
    %% error_handler, the old missing-function error comes back.
    Mod = bt4021_handle_undef_probe,
    {ok, Mod, Bin} = compile:forms(
        [
            {attribute, 1, module, Mod},
            {attribute, 2, export, [{'$handle_undefined_function', 2}]},
            {function, 3, '$handle_undefined_function', 2, [
                {clause, 3, [{atom, 3, magic}, {var, 3, 'Args'}], [], [
                    {tuple, 3, [{atom, 3, handled}, {var, 3, 'Args'}]}
                ]},
                {clause, 4, [{var, 4, 'F'}, {var, 4, 'Args'}], [], [
                    {call, 4,
                        {remote, 4, {atom, 4, error_handler}, {atom, 4, raise_undef_exception}}, [
                            {atom, 4, Mod}, {var, 4, 'F'}, {var, 4, 'Args'}
                        ]}
                ]}
            ]}
        ],
        []
    ),
    try
        {module, Mod} = code:load_binary(Mod, "bt4021_handle_undef_probe.erl", Bin),
        ?assertEqual({handled, [1]}, beamtalk_erlang_proxy:direct_call(Mod, magic, [1])),
        try
            beamtalk_erlang_proxy:direct_call(Mod, other, [1]),
            ?assert(false)
        catch
            error:#{error := Inner} ->
                ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
                ?assertEqual(
                    #{module => Mod, function => other, arity => 1},
                    Inner#beamtalk_error.details
                )
        end
    after
        _ = code:purge(Mod),
        _ = code:delete(Mod),
        _ = code:purge(Mod)
    end.

%% Load an in-memory module `Mod` exporting each `{Name, Return}` as a
%% zero-arity function returning `Return`.
load_hot_module(Mod, Funs) ->
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 2, export, [{Name, 0} || {Name, _} <- Funs]}
        | [
            {function, 3, Name, 0, [{clause, 3, [], [], [erl_parse:abstract(Ret)]}]}
         || {Name, Ret} <- Funs
        ]
    ],
    {ok, Mod, Bin} = compile:forms(Forms, []),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, "bt4021_hot_reload_probe.erl", Bin),
    ok.

direct_call_tracks_hot_reload_and_purge_test() ->
    Mod = bt4021_hot_reload_probe,
    try
        load_hot_module(Mod, [{answer, 1}]),
        ?assertEqual(1, beamtalk_erlang_proxy:direct_call(Mod, answer, [])),
        %% Reload: answer/0 changes value and gone/0 appears.
        load_hot_module(Mod, [{answer, 2}, {gone, x}]),
        ?assertEqual(2, beamtalk_erlang_proxy:direct_call(Mod, answer, [])),
        ?assertEqual(x, beamtalk_erlang_proxy:direct_call(Mod, gone, [])),
        %% Reload without gone/0: the next call reports it missing.
        load_hot_module(Mod, [{answer, 3}]),
        ?assertEqual(3, beamtalk_erlang_proxy:direct_call(Mod, answer, [])),
        try
            beamtalk_erlang_proxy:direct_call(Mod, gone, []),
            ?assert(false)
        catch
            error:#{error := Missing} ->
                ?assertEqual(does_not_understand, Missing#beamtalk_error.kind),
                ?assertEqual(
                    #{module => Mod, function => gone, arity => 0},
                    Missing#beamtalk_error.details
                )
        end,
        %% Delete + purge: the module (in-memory only) can no longer be loaded.
        _ = code:purge(Mod),
        true = code:delete(Mod),
        _ = code:purge(Mod),
        try
            beamtalk_erlang_proxy:direct_call(Mod, answer, []),
            ?assert(false)
        catch
            error:#{error := Purged} ->
                ?assertEqual(does_not_understand, Purged#beamtalk_error.kind),
                ?assertNotEqual(
                    nomatch,
                    binary:match(bt_bin(Purged#beamtalk_error.hint), <<"is not loaded">>)
                )
        end
    after
        _ = code:purge(Mod),
        _ = code:delete(Mod),
        _ = code:purge(Mod)
    end.

%%% ============================================================================
%%% BT-4055 — an error raised by compiled Beamtalk code the callee ran (a block
%%% passed to a `native:` method) propagates unchanged
%%% ============================================================================

%% Loads `'bt@bt4055@raiser'`, standing in for a compiled Beamtalk module:
%% `boom/1` raises the given error reason, `count_badarg/0` counts its runs in
%% the process dictionary and raises `badarg`.
load_bt_raiser() ->
    load_bt_raiser('bt@bt4055@raiser').

load_bt_raiser(Mod) ->
    {ok, Mod, Bin} = compile:forms(
        [
            {attribute, 1, module, Mod},
            {attribute, 2, export, [{boom, 1}, {count_badarg, 0}]},
            {function, 3, boom, 1, [
                {clause, 3, [{var, 3, 'R'}], [], [
                    {call, 3, {remote, 3, {atom, 3, erlang}, {atom, 3, error}}, [{var, 3, 'R'}]}
                ]}
            ]},
            {function, 4, count_badarg, 0, [
                {clause, 4, [], [], [
                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, put}}, [
                        {atom, 4, bt4055_runs},
                        {op, 4, '+',
                            {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, get}}, [
                                {atom, 4, bt4055_runs}
                            ]},
                            {integer, 4, 1}}
                    ]},
                    {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, error}}, [{atom, 4, badarg}]}
                ]}
            ]}
        ],
        []
    ),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    Mod.

unload_bt_raiser(Mod) ->
    _ = code:purge(Mod),
    _ = code:delete(Mod),
    _ = code:purge(Mod),
    ok.

native_call_block_error_from_beamtalk_code_is_unchanged_test() ->
    Mod = load_bt_raiser(),
    try
        lists:foreach(
            fun(Reason) ->
                Block = fun(_) -> Mod:boom(Reason) end,
                ?assertError(
                    Reason,
                    beamtalk_erlang_proxy:native_call(
                        lists, foreach, [Block, [1]], {'Telemetry', 'span:metadata:do:'}
                    )
                )
            end,
            [badarith, function_clause, {badmatch, 1}, undef]
        )
    after
        unload_bt_raiser(Mod)
    end.

native_call_block_badarg_from_beamtalk_code_is_not_retried_test() ->
    Mod = load_bt_raiser(),
    put(bt4055_runs, 0),
    try
        %% A binary argument would trigger the charlist retry, re-running the block.
        Block = fun(_) -> Mod:count_badarg() end,
        ?assertError(
            badarg,
            beamtalk_erlang_proxy:native_call(lists, foreach, [Block, [<<"x">>]], {'X', 'y:'})
        ),
        ?assertEqual(1, get(bt4055_runs))
    after
        erase(bt4055_runs),
        unload_bt_raiser(Mod)
    end.

native_call_erlang_error_is_still_classified_test() ->
    %% Raised by Erlang code (here a plain fun in this test module), so the FFI
    %% classification still applies.
    Block = fun(_) -> erlang:error(badarith) end,
    try
        beamtalk_erlang_proxy:native_call(lists, foreach, [Block, [1]], {'X', 'y:'}),
        ?assert(false)
    catch
        error:#{error := Err} -> ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

native_call_block_error_from_repl_eval_module_is_unchanged_test() ->
    %% REPL and inspector code compiles to `beamtalk_repl_eval_<N>` /
    %% `beamtalk_inspector_eval_<N>`, not `bt@…`; it is Beamtalk code too.
    lists:foreach(
        fun(Name) ->
            Mod = load_bt_raiser(Name),
            try
                Block = fun(_) -> Mod:boom(badarith) end,
                ?assertError(
                    badarith,
                    beamtalk_erlang_proxy:native_call(lists, foreach, [Block, [1]], {'X', 'y:'})
                )
            after
                unload_bt_raiser(Mod)
            end
        end,
        %% Built by the same functions `beamtalk_repl_eval` names its modules with.
        [
            beamtalk_module_name:repl_eval_module(4055),
            beamtalk_module_name:inspector_eval_module(4055)
        ]
    ).

native_call_badarg_with_fun_arg_is_not_retried_test() ->
    %% Even when the stack shows no Beamtalk frame, a call that was passed a fun
    %% is never retried: the retry would run the fun again.
    put(bt4055_runs, 0),
    try
        Block = fun(_) ->
            put(bt4055_runs, get(bt4055_runs) + 1),
            erlang:error(badarg)
        end,
        try
            beamtalk_erlang_proxy:native_call(lists, foreach, [Block, [<<"x">>]], {'X', 'y:'}),
            ?assert(false)
        catch
            error:#{error := Err} -> ?assertEqual(type_error, Err#beamtalk_error.kind)
        end,
        ?assertEqual(1, get(bt4055_runs))
    after
        erase(bt4055_runs)
    end.

%%% ===================================================================
%%% BT-4153: coercion fast path (shape-first + precomputed mode)
%%% ===================================================================

is_beamtalk_module_conformance_test() ->
    %% Same table as `test_is_beamtalk_module` in
    %% crates/beamtalk-codegen/src/core_erlang/util.rs. A rule change applied
    %% to only one side (impl plus its own test) stays green on both sides
    %% while codegen and runtime diverge — so update both tables together,
    %% never one alone. Probed through coerce_ffi_result/2: a charlist result
    %% comes back a binary in stock mode and an unchanged list in beamtalk
    %% mode.
    Beamtalk = [
        beamtalk_actor, beamtalk_stream, beamtalk_, 'bt@stdlib@list', 'bt@test@foo', 'bt@'
    ],
    Stock = [
        lists,
        erlang,
        maps,
        vs_bench,
        beamtalk,
        beamtalkx,
        bt,
        'btx@foo',
        my_beamtalk_mod,
        'BEAMTALK_FOO',
        ''
    ],
    [
        ?assertEqual("hi", beamtalk_erlang_proxy:coerce_ffi_result(M, "hi"))
     || M <- Beamtalk
    ],
    [
        ?assertEqual({beamtalk_string, <<"hi">>}, beamtalk_erlang_proxy:coerce_ffi_result(M, "hi"))
     || M <- Stock
    ].

coerce_ffi_result_3_equivalence_test() ->
    %% /3 with the runtime-derived mode answers exactly what /2 does, for
    %% every result shape in each module class (stock, beamtalk, raw).
    {module, beamtalk_ets} = code:ensure_loaded(beamtalk_ets),
    Shapes = [
        42,
        -1,
        4.5,
        <<>>,
        <<"bin">>,
        self(),
        make_ref(),
        fun() -> ok end,
        #{},
        #{a => 1},
        [],
        foo,
        nil,
        true,
        ok,
        error,
        {ok, 1},
        {error, x},
        {ok, "hi"},
        {error, "hi"},
        {ok, {ok, 1}},
        {ok, 1, 2},
        {ok},
        {error},
        {a, b},
        {},
        {1},
        [1, 2, 3],
        "hi",
        [<<>>, ok],
        [1 | 2],
        <<1:1>>,
        #{a => "hi"}
    ],
    Modes = [{lists, stock}, {beamtalk_stream, beamtalk}, {beamtalk_ets, beamtalk}],
    [
        ?assertEqual(
            beamtalk_erlang_proxy:coerce_ffi_result(M, S),
            beamtalk_erlang_proxy:coerce_ffi_result(M, S, Mode)
        )
     || {M, Mode} <- Modes, S <- Shapes
    ].

coerce_ffi_result_3_absolute_test() ->
    %% Absolute pins for the fast-path identity invariant and the
    %% mode-divergent shapes (BT-4153 review T1/T2). The equivalence test
    %% compares /3 against /2, which delegates to /3 — it cannot catch a
    %% per-mode semantic bug that agrees with itself (e.g. swapped slow-path
    %% bodies). These assertions pin values, not agreement.
    {module, beamtalk_ets} = code:ensure_loaded(beamtalk_ets),
    %% Fast shapes are the identity in every module class: stock,
    %% beamtalk-nonraw and beamtalk-raw must all answer the input unchanged.
    Fast = [
        42,
        -1,
        4.5,
        <<>>,
        <<"bin">>,
        <<1:1>>,
        self(),
        make_ref(),
        fun() -> ok end,
        #{},
        #{a => "hi"},
        [],
        foo,
        nil,
        true,
        {ok},
        {ok, 1, 2},
        {error},
        {a, b},
        {},
        {1}
    ],
    [
        begin
            ?assertEqual(S, beamtalk_erlang_proxy:coerce_ffi_result(lists, S, stock)),
            ?assertEqual(S, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_stream, S, beamtalk)),
            ?assertEqual(S, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_ets, S, beamtalk))
        end
     || S <- Fast
    ],
    %% Mode-divergent shapes, pinned per mode: a charlist payload converts
    %% under stock but is preserved under beamtalk-nonraw.
    StockOkCharlist = beamtalk_erlang_proxy:coerce_ffi_result(lists, {ok, "hi"}, stock),
    ?assertEqual({beamtalk_string, <<"hi">>}, maps:get('okValue', StockOkCharlist)),
    BtOkCharlist = beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_stream, {ok, "hi"}, beamtalk),
    ?assertEqual("hi", maps:get('okValue', BtOkCharlist)),
    %% Raw-result modules pass even ok/error-tagged results through ...
    ?assertEqual(ok, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_ets, ok, beamtalk)),
    ?assertEqual(
        {ok, 1}, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_ets, {ok, 1}, beamtalk)
    ),
    ?assertEqual(
        {error, "hi"},
        beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_ets, {error, "hi"}, beamtalk)
    ),
    %% ... while non-raw beamtalk modules still wrap `ok`, and stock still
    %% wraps the bare `error` atom that beamtalk passes through (BT-3660).
    Wrapped = beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_stream, ok, beamtalk),
    ?assertEqual('Result', maps:get('$beamtalk_class', Wrapped)),
    StockErr = beamtalk_erlang_proxy:coerce_ffi_result(maps, error, stock),
    ?assertEqual('Result', maps:get('$beamtalk_class', StockErr)),
    ?assertEqual(error, beamtalk_erlang_proxy:coerce_ffi_result(beamtalk_stream, error, beamtalk)),
    %% One level of recursion only: the inner tuple is never re-wrapped.
    StockNested = beamtalk_erlang_proxy:coerce_ffi_result(lists, {ok, {ok, 1}}, stock),
    ?assertEqual({ok, 1}, maps:get('okValue', StockNested)).

direct_call_4_rejects_bad_mode_test() ->
    %% A mode typo fails fast with function_clause on the entry point — never
    %% wrapped and misattributed to the callee (BT-4153 review M1).
    ?assertError(
        function_clause, beamtalk_erlang_proxy:direct_call(erlang, phash2, [5], 'Stock')
    ),
    ?assertError(
        function_clause,
        beamtalk_erlang_proxy:native_call(lists, reverse, [[1]], {'X', 'y'}, 'Stock')
    ).

direct_call_4_equivalence_test() ->
    %% /4 with the runtime-derived mode behaves exactly like /3.
    ?assertEqual(
        beamtalk_erlang_proxy:direct_call(erlang, phash2, [5]),
        beamtalk_erlang_proxy:direct_call(erlang, phash2, [5], stock)
    ),
    ?assertEqual(
        beamtalk_erlang_proxy:direct_call(erlang, integer_to_binary, [5]),
        beamtalk_erlang_proxy:direct_call(erlang, integer_to_binary, [5], stock)
    ),
    ?assertEqual(
        beamtalk_erlang_proxy:direct_call(maps, find, [k, #{k => 3}]),
        beamtalk_erlang_proxy:direct_call(maps, find, [k, #{k => 3}], stock)
    ),
    %% beamtalk mode: a charlist result passes through unconverted.
    ?assertEqual(
        "hi", beamtalk_erlang_proxy:direct_call(?MODULE, bt4153_charlist_stub, [], beamtalk)
    ),
    ?assertEqual(
        beamtalk_erlang_proxy:direct_call(?MODULE, bt4153_charlist_stub, []),
        beamtalk_erlang_proxy:direct_call(?MODULE, bt4153_charlist_stub, [], beamtalk)
    ),
    %% stock mode wraps even a beamtalk-module `ok` (the mode is the contract).
    StockOk = beamtalk_erlang_proxy:direct_call(?MODULE, bt4153_ok_stub, [], stock),
    ?assertEqual('Result', maps:get('$beamtalk_class', StockOk)),
    ?assertEqual(true, maps:get('isOk', StockOk)),
    %% ... and beamtalk mode wraps it too here (?MODULE is not a raw module).
    BtOk = beamtalk_erlang_proxy:direct_call(?MODULE, bt4153_ok_stub, [], beamtalk),
    ?assertEqual('Result', maps:get('$beamtalk_class', BtOk)).

direct_call_4_error_equivalence_test() ->
    %% /4 classifies errors exactly like /3.
    assert_same_ffi_error(
        fun() -> beamtalk_erlang_proxy:direct_call(no_such_mod_bt4153, f, []) end,
        fun() -> beamtalk_erlang_proxy:direct_call(no_such_mod_bt4153, f, [], stock) end
    ),
    assert_same_ffi_error(
        fun() -> beamtalk_erlang_proxy:direct_call(lists, no_such_fun_bt4153, [1]) end,
        fun() -> beamtalk_erlang_proxy:direct_call(lists, no_such_fun_bt4153, [1], stock) end
    ).

native_call_5_equivalence_test() ->
    %% /5 with the runtime-derived mode behaves exactly like /4.
    Ctx = {'Stream', 'asList'},
    ?assertEqual(
        beamtalk_erlang_proxy:native_call(lists, reverse, [[1, 2, 3]], Ctx),
        beamtalk_erlang_proxy:native_call(lists, reverse, [[1, 2, 3]], Ctx, stock)
    ),
    ?assertEqual(
        beamtalk_erlang_proxy:native_call(?MODULE, bt4153_charlist_stub, [], Ctx),
        beamtalk_erlang_proxy:native_call(?MODULE, bt4153_charlist_stub, [], Ctx, beamtalk)
    ).

assert_same_ffi_error(FunA, FunB) ->
    ?assertEqual(catch_ffi_error(FunA), catch_ffi_error(FunB)).

catch_ffi_error(Fun) ->
    try Fun() of
        Unexpected -> {unexpected_return, Unexpected}
    catch
        error:#{error := Err} ->
            {
                Err#beamtalk_error.kind,
                Err#beamtalk_error.class,
                Err#beamtalk_error.selector,
                Err#beamtalk_error.hint
            }
    end.
