%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_compiled_method module (BT-346).
%%%
%%% Tests dispatch, introspection, and error handling for CompiledMethod objects.

-module(beamtalk_compiled_method_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

test_method() ->
    #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => 'increment',
        '__source__' => <<"increment => self.value := self.value + 1">>,
        '__method_info__' => #{arity => 0, type => unary}
    }.

test_method_with_args() ->
    #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => 'add:',
        '__source__' => <<"add: x => self.value := self.value + x">>,
        '__method_info__' => #{arity => 1, type => keyword}
    }.

%%====================================================================
%% Builtin dispatch tests
%%====================================================================

selector_returns_method_name_test() ->
    Method = test_method(),
    ?assertEqual('increment', beamtalk_compiled_method:dispatch('selector', [], Method)).

source_returns_source_code_test() ->
    Method = test_method(),
    ?assertEqual(<<"increment => self.value := self.value + 1">>,
                 beamtalk_compiled_method:dispatch('source', [], Method)).

argument_count_zero_test() ->
    Method = test_method(),
    ?assertEqual(0, beamtalk_compiled_method:dispatch('argumentCount', [], Method)).

argument_count_one_test() ->
    Method = test_method_with_args(),
    ?assertEqual(1, beamtalk_compiled_method:dispatch('argumentCount', [], Method)).

argument_count_missing_arity_defaults_to_zero_test() ->
    Method = #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => 'foo',
        '__source__' => <<"foo => nil">>,
        '__method_info__' => #{}
    },
    ?assertEqual(0, beamtalk_compiled_method:dispatch('argumentCount', [], Method)).

class_returns_compiled_method_test() ->
    Method = test_method(),
    ?assertEqual('CompiledMethod', beamtalk_compiled_method:dispatch('class', [], Method)).

print_string_test() ->
    Method = test_method(),
    Result = beamtalk_compiled_method:dispatch('printString', [], Method),
    ?assertEqual(<<"a CompiledMethod(increment)">>, Result).

as_string_same_as_print_string_test() ->
    Method = test_method(),
    PS = beamtalk_compiled_method:dispatch('printString', [], Method),
    AS = beamtalk_compiled_method:dispatch('asString', [], Method),
    ?assertEqual(PS, AS).

print_string_keyword_selector_test() ->
    Method = test_method_with_args(),
    Result = beamtalk_compiled_method:dispatch('printString', [], Method),
    ?assertEqual(<<"a CompiledMethod('add:')">>, Result).

%%====================================================================
%% respondsTo: tests
%%====================================================================

responds_to_known_selectors_test() ->
    Method = test_method(),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['selector'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['source'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['argumentCount'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['class'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['printString'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['asString'], Method)),
    ?assertEqual(true, beamtalk_compiled_method:dispatch('respondsTo:', ['respondsTo:'], Method)).

responds_to_unknown_selector_test() ->
    Method = test_method(),
    ?assertEqual(false, beamtalk_compiled_method:dispatch('respondsTo:', ['nonexistent'], Method)).

%%====================================================================
%% has_method tests
%%====================================================================

has_method_known_test() ->
    ?assert(beamtalk_compiled_method:has_method('selector')),
    ?assert(beamtalk_compiled_method:has_method('source')),
    ?assert(beamtalk_compiled_method:has_method('argumentCount')),
    ?assert(beamtalk_compiled_method:has_method('class')),
    ?assert(beamtalk_compiled_method:has_method('printString')),
    ?assert(beamtalk_compiled_method:has_method('asString')),
    ?assert(beamtalk_compiled_method:has_method('respondsTo:')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_compiled_method:has_method('nonexistent')),
    ?assertNot(beamtalk_compiled_method:has_method('foo')).

%%====================================================================
%% does_not_understand error tests
%%====================================================================

unknown_selector_raises_error_test() ->
    Method = test_method(),
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'CompiledMethod'},
                 beamtalk_compiled_method:dispatch('nonexistent', [], Method)).

unknown_selector_error_has_selector_test() ->
    Method = test_method(),
    try
        beamtalk_compiled_method:dispatch('badMethod', [], Method),
        ?assert(false)
    catch
        error:#beamtalk_error{selector = Sel, hint = Hint} ->
            ?assertEqual('badMethod', Sel),
            ?assertNotEqual(undefined, Hint)
    end.

unknown_selector_error_has_hint_test() ->
    Method = test_method(),
    try
        beamtalk_compiled_method:dispatch('nope', [], Method),
        ?assert(false)
    catch
        error:#beamtalk_error{hint = Hint} ->
            ?assert(is_binary(Hint)),
            %% Hint should mention available selectors
            ?assertNotEqual(nomatch, binary:match(Hint, <<"selector">>))
    end.
