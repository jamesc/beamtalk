%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_erlang_class_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_erlang_class dispatch (BT-1974).

Tests Erlang class bridging dispatch: class/printString builtins,
unary module lookup forwarding, keyword selector error, args error,
object protocol delegation, and has_method/1.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

erlang_self() ->
    #{
        '$beamtalk_class' => 'Erlang'
    }.

%%====================================================================
%% dispatch/3 — builtin selectors
%%====================================================================

dispatch_class_returns_erlang_test() ->
    ?assertEqual('Erlang', beamtalk_erlang_class:dispatch('class', [], erlang_self())).

dispatch_print_string_returns_erlang_test() ->
    ?assertEqual(<<"Erlang">>, beamtalk_erlang_class:dispatch('printString', [], erlang_self())).

%%====================================================================
%% dispatch/3 — unary module lookup
%%====================================================================

dispatch_unary_returns_erlang_module_proxy_test() ->
    Result = beamtalk_erlang_class:dispatch('lists', [], erlang_self()),
    ?assertEqual('ErlangModule', maps:get('$beamtalk_class', Result)),
    ?assertEqual(lists, maps:get(module, Result)).

dispatch_unary_different_module_test() ->
    Result = beamtalk_erlang_class:dispatch('maps', [], erlang_self()),
    ?assertEqual(maps, maps:get(module, Result)).

%%====================================================================
%% dispatch/3 — keyword selector error
%%====================================================================

dispatch_keyword_selector_raises_dnu_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'Erlang'}
        },
        beamtalk_erlang_class:dispatch('foo:', [bar], erlang_self())
    ).

%%====================================================================
%% dispatch/3 — unary with args error
%%====================================================================

dispatch_unary_with_args_raises_arity_mismatch_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = arity_mismatch, class = 'Erlang'}
        },
        beamtalk_erlang_class:dispatch('lists', [extra], erlang_self())
    ).

%%====================================================================
%% dispatch/3 — object protocol delegation
%%====================================================================

dispatch_object_protocol_responds_to_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        Result = beamtalk_erlang_class:dispatch('respondsTo:', ['class'], erlang_self()),
        ?assertEqual(true, Result)
    end}.

dispatch_object_protocol_hash_test_() ->
    {setup, fun() -> beamtalk_extensions:init() end, fun(_) -> ok end, fun() ->
        Result = beamtalk_erlang_class:dispatch('hash', [], erlang_self()),
        ?assert(is_integer(Result))
    end}.

%%====================================================================
%% has_method/1
%%====================================================================

has_method_always_true_test() ->
    ?assert(beamtalk_erlang_class:has_method('class')),
    ?assert(beamtalk_erlang_class:has_method('printString')),
    ?assert(beamtalk_erlang_class:has_method('lists')),
    ?assert(beamtalk_erlang_class:has_method('nonexistent')),
    ?assert(beamtalk_erlang_class:has_method('anySelector')).
