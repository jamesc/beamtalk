%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiled_method_ops_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_compiled_method_ops direct API (BT-1974).

Tests dispatch/3 for all 8 builtin selectors, has_method/1 true/false,
and does_not_understand error. Complements beamtalk_compiled_method_tests
by directly exercising the ops module for doc dispatch and edge cases.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

test_method() ->
    #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => 'greet:',
        '__source__' => <<"greet: name => 'Hello, ' ++ name">>,
        '__doc__' => <<"Greets the given name.">>,
        '__method_info__' => #{arity => 1, type => keyword}
    }.

test_method_no_doc() ->
    #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => 'run',
        '__source__' => <<"run => nil">>,
        '__doc__' => nil,
        '__method_info__' => #{arity => 0, type => unary}
    }.

%%====================================================================
%% dispatch/3 — all 8 builtin selectors
%%====================================================================

dispatch_selector_test() ->
    ?assertEqual('greet:', beamtalk_compiled_method_ops:dispatch('selector', [], test_method())).

dispatch_source_test() ->
    ?assertEqual(
        <<"greet: name => 'Hello, ' ++ name">>,
        beamtalk_compiled_method_ops:dispatch('source', [], test_method())
    ).

dispatch_doc_returns_docstring_test() ->
    ?assertEqual(
        <<"Greets the given name.">>,
        beamtalk_compiled_method_ops:dispatch('doc', [], test_method())
    ).

dispatch_doc_returns_nil_when_absent_test() ->
    ?assertEqual(nil, beamtalk_compiled_method_ops:dispatch('doc', [], test_method_no_doc())).

dispatch_argument_count_test() ->
    ?assertEqual(1, beamtalk_compiled_method_ops:dispatch('argumentCount', [], test_method())).

dispatch_class_test() ->
    ?assertEqual(
        'CompiledMethod',
        beamtalk_compiled_method_ops:dispatch('class', [], test_method())
    ).

dispatch_print_string_test() ->
    Result = beamtalk_compiled_method_ops:dispatch('printString', [], test_method()),
    ?assertEqual(<<"a CompiledMethod(greet:)">>, Result).

dispatch_as_string_matches_print_string_test() ->
    PS = beamtalk_compiled_method_ops:dispatch('printString', [], test_method()),
    AS = beamtalk_compiled_method_ops:dispatch('asString', [], test_method()),
    ?assertEqual(PS, AS).

dispatch_responds_to_known_test() ->
    ?assertEqual(
        true,
        beamtalk_compiled_method_ops:dispatch('respondsTo:', ['selector'], test_method())
    ).

dispatch_responds_to_unknown_test() ->
    ?assertEqual(
        false,
        beamtalk_compiled_method_ops:dispatch('respondsTo:', ['foo'], test_method())
    ).

%%====================================================================
%% has_method/1
%%====================================================================

has_method_true_for_all_builtins_test() ->
    Builtins = ['selector', 'source', 'doc', 'argumentCount',
                'class', 'printString', 'asString', 'respondsTo:'],
    lists:foreach(fun(S) ->
        ?assert(beamtalk_compiled_method_ops:has_method(S))
    end, Builtins).

has_method_false_for_unknown_test() ->
    ?assertNot(beamtalk_compiled_method_ops:has_method('nonexistent')),
    ?assertNot(beamtalk_compiled_method_ops:has_method('value')).

%%====================================================================
%% does_not_understand error
%%====================================================================

unknown_selector_raises_dnu_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'CompiledMethod'}
        },
        beamtalk_compiled_method_ops:dispatch('unknown', [], test_method())
    ).
