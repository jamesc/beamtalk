%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_float_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Test Setup/Cleanup
%%% ============================================================================

%% Setup: Initialize extension registry for DNU tests
setup() ->
    beamtalk_extensions:init(),
    ok.

%% Cleanup: Clear any test extensions
cleanup(_) ->
    ok.

%%% ============================================================================
%%% Arithmetic Operations Tests
%%% ============================================================================

addition_test() ->
    ?assert(abs(beamtalk_float:dispatch('+', [1.5], 3.14) - 4.64) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('+', [-5.0], 5.0)) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('+', [-3.0], -7.0) - (-10.0)) < 1.0e-10).

subtraction_test() ->
    ?assert(abs(beamtalk_float:dispatch('-', [1.0], 3.14) - 2.14) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('-', [-5.0], 5.0) - 10.0) < 1.0e-10).

multiplication_test() ->
    ?assert(abs(beamtalk_float:dispatch('*', [2.0], 3.5) - 7.0) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('*', [0.0], 42.5)) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('*', [-4.0], 5.0) - (-20.0)) < 1.0e-10).

division_test() ->
    ?assert(abs(beamtalk_float:dispatch('/', [2.0], 7.0) - 3.5) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('/', [4.0], 10.0) - 2.5) < 1.0e-10).

division_by_zero_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Division by float zero raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Float', selector = '/'},
                         beamtalk_float:dispatch('/', [0.0], 42.0))
        end},
        {"Division by integer zero raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Float', selector = '/'},
                         beamtalk_float:dispatch('/', [0], 42.0))
        end}
    ]}.

mixed_float_integer_arithmetic_test() ->
    %% Float operations should accept integers
    ?assert(abs(beamtalk_float:dispatch('+', [2], 3.14) - 5.14) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('*', [3], 2.5) - 7.5) < 1.0e-10).

%%% ============================================================================
%%% Comparison Operations Tests
%%% ============================================================================

equality_test() ->
    ?assertEqual(true, beamtalk_float:dispatch('=', [3.14], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('=', [3.15], 3.14)),
    %% Strict equality: float =/= integer
    ?assertEqual(false, beamtalk_float:dispatch('=', [42], 42.0)),
    %% Non-numeric comparison always returns false
    ?assertEqual(false, beamtalk_float:dispatch('=', [<<"42">>], 42.0)).

less_than_test() ->
    ?assertEqual(true, beamtalk_float:dispatch('<', [5.0], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('<', [3.14], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('<', [2.0], 3.14)).

greater_than_test() ->
    ?assertEqual(false, beamtalk_float:dispatch('>', [5.0], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('>', [3.14], 3.14)),
    ?assertEqual(true, beamtalk_float:dispatch('>', [2.0], 3.14)).

less_than_or_equal_test() ->
    ?assertEqual(true, beamtalk_float:dispatch('<=', [5.0], 3.14)),
    ?assertEqual(true, beamtalk_float:dispatch('<=', [3.14], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('<=', [2.0], 3.14)).

greater_than_or_equal_test() ->
    ?assertEqual(false, beamtalk_float:dispatch('>=', [5.0], 3.14)),
    ?assertEqual(true, beamtalk_float:dispatch('>=', [3.14], 3.14)),
    ?assertEqual(true, beamtalk_float:dispatch('>=', [2.0], 3.14)).

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('Float', beamtalk_float:dispatch('class', [], 3.14)),
    ?assertEqual('Float', beamtalk_float:dispatch('class', [], 0.0)),
    ?assertEqual('Float', beamtalk_float:dispatch('class', [], -999.99)).

responds_to_test() ->
    ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['+'], 3.14)),
    ?assertEqual(true, beamtalk_float:dispatch('respondsTo', ['class'], 3.14)),
    ?assertEqual(false, beamtalk_float:dispatch('respondsTo', ['unknownMethod'], 3.14)).

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"3.14">>, beamtalk_float:dispatch('asString', [], 3.14)),
    ?assertEqual(<<"0.0">>, beamtalk_float:dispatch('asString', [], 0.0)),
    ?assertEqual(<<"-1.5">>, beamtalk_float:dispatch('asString', [], -1.5)),
    ?assertEqual(<<"100.0">>, beamtalk_float:dispatch('asString', [], 100.0)).

%%% ============================================================================
%%% Numeric Operations Tests
%%% ============================================================================

abs_test() ->
    ?assert(abs(beamtalk_float:dispatch('abs', [], 3.14) - 3.14) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('abs', [], -3.14) - 3.14) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('abs', [], 0.0)) < 1.0e-10).

negated_test() ->
    ?assert(abs(beamtalk_float:dispatch('negated', [], 3.14) - (-3.14)) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('negated', [], -3.14) - 3.14) < 1.0e-10),
    ?assert(abs(beamtalk_float:dispatch('negated', [], 0.0)) < 1.0e-10).

%%% ============================================================================
%%% DNU and Extension Registry Tests
%%% ============================================================================

does_not_understand_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Float', selector = 'unknownMethod'},
                         beamtalk_float:dispatch('unknownMethod', [], 3.14))
        end}
    ]}.

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods are dispatched correctly", fun() ->
            Fun = fun([], X) -> X * X end,
            beamtalk_extensions:register('Float', 'squared', Fun, test_module),
            Result = beamtalk_float:dispatch('squared', [], 3.0),
            ?assert(abs(Result - 9.0) < 1.0e-10)
        end}
    ]}.

%%% ============================================================================
%%% has_method/1 Tests
%%% ============================================================================

has_method_builtin_test() ->
    ?assert(beamtalk_float:has_method('+')),
    ?assert(beamtalk_float:has_method('-')),
    ?assert(beamtalk_float:has_method('*')),
    ?assert(beamtalk_float:has_method('/')),
    ?assert(beamtalk_float:has_method('=')),
    ?assert(beamtalk_float:has_method('<')),
    ?assert(beamtalk_float:has_method('>')),
    ?assert(beamtalk_float:has_method('<=')),
    ?assert(beamtalk_float:has_method('>=')),
    ?assert(beamtalk_float:has_method('class')),
    ?assert(beamtalk_float:has_method('respondsTo')),
    ?assert(beamtalk_float:has_method('perform')),
    ?assert(beamtalk_float:has_method('perform:withArgs:')),
    ?assert(beamtalk_float:has_method('asString')),
    ?assert(beamtalk_float:has_method('abs')),
    ?assert(beamtalk_float:has_method('negated')),
    ?assert(beamtalk_float:has_method('instVarNames')),
    ?assert(beamtalk_float:has_method('instVarAt')),
    ?assert(beamtalk_float:has_method('instVarAt:put:')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_float:has_method('unknownMethod')),
    ?assertNot(beamtalk_float:has_method('foo')),
    ?assertNot(beamtalk_float:has_method('bar')).

has_method_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"has_method returns true for registered extensions", fun() ->
            ?assertNot(beamtalk_float:has_method('customMethod')),
            Fun = fun([], X) -> X * 2 end,
            beamtalk_extensions:register('Float', 'customMethod', Fun, test_module),
            ?assert(beamtalk_float:has_method('customMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Instance Variable Reflection Tests
%%% ============================================================================

instVarNames_test() ->
    ?assertEqual([], beamtalk_float:dispatch('instVarNames', [], 3.14)).

instVarAt_test() ->
    ?assertEqual(nil, beamtalk_float:dispatch('instVarAt', [anyField], 3.14)),
    ?assertEqual(nil, beamtalk_float:dispatch('instVarAt', [value], 1.5)).

instVarAt_put_immutable_error_test() ->
    ?assertError({beamtalk_error, immutable_primitive, 'Float', 'instVarAt:put:', _, _, _},
                 beamtalk_float:dispatch('instVarAt:put:', [value, 99], 3.14)).

%%% ============================================================================
%%% Edge Cases Tests
%%% ============================================================================

special_values_test() ->
    ?assertEqual('Float', beamtalk_float:dispatch('class', [], 0.0)),
    ?assertEqual(<<"0.0">>, beamtalk_float:dispatch('asString', [], 0.0)),
    ?assert(abs(beamtalk_float:dispatch('negated', [], 0.0)) < 1.0e-10).
