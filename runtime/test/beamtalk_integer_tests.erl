%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_integer_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Setup/Cleanup
%%% ============================================================================

%% Setup: Initialize extension registry for DNU tests
setup() ->
    beamtalk_extensions:init(),
    ok.

%% Cleanup: Clear extension registry
cleanup(_) ->
    %% Clear any test extensions
    ok.

%%% ============================================================================
%%% Arithmetic Operations Tests
%%% ============================================================================

addition_test() ->
    ?assertEqual(50, beamtalk_integer:dispatch('+', [8], 42)),
    ?assertEqual(0, beamtalk_integer:dispatch('+', [-5], 5)),
    ?assertEqual(-10, beamtalk_integer:dispatch('+', [-7], -3)).

subtraction_test() ->
    ?assertEqual(34, beamtalk_integer:dispatch('-', [8], 42)),
    ?assertEqual(10, beamtalk_integer:dispatch('-', [-5], 5)),
    ?assertEqual(-4, beamtalk_integer:dispatch('-', [3], -1)).

multiplication_test() ->
    ?assertEqual(42, beamtalk_integer:dispatch('*', [6], 7)),
    ?assertEqual(0, beamtalk_integer:dispatch('*', [0], 42)),
    ?assertEqual(-20, beamtalk_integer:dispatch('*', [-4], 5)).

division_test() ->
    ?assertEqual(7.0, beamtalk_integer:dispatch('/', [6], 42)),
    ?assertEqual(2.5, beamtalk_integer:dispatch('/', [4], 10)),
    ?assertEqual(-5.0, beamtalk_integer:dispatch('/', [2], -10)).

division_by_zero_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Division by zero raises does_not_understand", fun() ->
            %% Division by zero should not match the guard and fall through to not_found
            ?assertError({does_not_understand, 'Integer', '/', 1}, 
                         beamtalk_integer:dispatch('/', [0], 42))
        end}
    ]}.

%%% ============================================================================
%%% Comparison Operations Tests
%%% ============================================================================

equality_test() ->
    ?assertEqual(true, beamtalk_integer:dispatch('=', [42], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('=', [43], 42)),
    ?assertEqual(true, beamtalk_integer:dispatch('=', [-5], -5)),
    ?assertEqual(false, beamtalk_integer:dispatch('=', [<<"42">>], 42)).

less_than_test() ->
    ?assertEqual(true, beamtalk_integer:dispatch('<', [43], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('<', [42], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('<', [41], 42)).

greater_than_test() ->
    ?assertEqual(false, beamtalk_integer:dispatch('>', [43], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('>', [42], 42)),
    ?assertEqual(true, beamtalk_integer:dispatch('>', [41], 42)).

less_than_or_equal_test() ->
    ?assertEqual(true, beamtalk_integer:dispatch('<=', [43], 42)),
    ?assertEqual(true, beamtalk_integer:dispatch('<=', [42], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('<=', [41], 42)).

greater_than_or_equal_test() ->
    ?assertEqual(false, beamtalk_integer:dispatch('>=', [43], 42)),
    ?assertEqual(true, beamtalk_integer:dispatch('>=', [42], 42)),
    ?assertEqual(true, beamtalk_integer:dispatch('>=', [41], 42)).

comparison_type_safety_test() ->
    %% Comparing integer with non-numeric types
    %% Equality with non-number should return false
    ?assertEqual(false, beamtalk_integer:dispatch('=', [<<"42">>], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('=', ['atom'], 42)),
    
    %% Ordering comparisons with non-numbers should raise does_not_understand
    ?assertError({does_not_understand, 'Integer', '<', 1},
                 beamtalk_integer:dispatch('<', [<<"42">>], 42)),
    ?assertError({does_not_understand, 'Integer', '>', 1},
                 beamtalk_integer:dispatch('>', ['atom'], 42)).

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], 42)),
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], 0)),
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], -999)).

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"42">>, beamtalk_integer:dispatch('asString', [], 42)),
    ?assertEqual(<<"0">>, beamtalk_integer:dispatch('asString', [], 0)),
    ?assertEqual(<<"-123">>, beamtalk_integer:dispatch('asString', [], -123)),
    ?assertEqual(<<"999999">>, beamtalk_integer:dispatch('asString', [], 999999)).

%%% ============================================================================
%%% Numeric Operations Tests
%%% ============================================================================

abs_test() ->
    ?assertEqual(42, beamtalk_integer:dispatch('abs', [], 42)),
    ?assertEqual(42, beamtalk_integer:dispatch('abs', [], -42)),
    ?assertEqual(0, beamtalk_integer:dispatch('abs', [], 0)).

negated_test() ->
    ?assertEqual(-42, beamtalk_integer:dispatch('negated', [], 42)),
    ?assertEqual(42, beamtalk_integer:dispatch('negated', [], -42)),
    ?assertEqual(0, beamtalk_integer:dispatch('negated', [], 0)).

%%% ============================================================================
%%% DNU and Extension Registry Tests
%%% ============================================================================

does_not_understand_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Integer', 'unknownMethod', 0},
                         beamtalk_integer:dispatch('unknownMethod', [], 42))
        end}
    ]}.

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods are dispatched correctly", fun() ->
            %% Register an extension method: squared_plus adds Y then squares the result
            Fun = fun([Y], X) -> (X + Y) * (X + Y) end,
            beamtalk_extensions:register('Integer', 'squared_plus', Fun, test_module),
            
            %% Should dispatch to extension: (42 + 8) * (42 + 8) = 50 * 50 = 2500
            ?assertEqual(2500, beamtalk_integer:dispatch('squared_plus', [8], 42))
        end}
    ]}.

%%% ============================================================================
%%% has_method/1 Tests
%%% ============================================================================

has_method_builtin_test() ->
    ?assert(beamtalk_integer:has_method('+')),
    ?assert(beamtalk_integer:has_method('-')),
    ?assert(beamtalk_integer:has_method('*')),
    ?assert(beamtalk_integer:has_method('/')),
    ?assert(beamtalk_integer:has_method('=')),
    ?assert(beamtalk_integer:has_method('<')),
    ?assert(beamtalk_integer:has_method('>')),
    ?assert(beamtalk_integer:has_method('<=')),
    ?assert(beamtalk_integer:has_method('>=')),
    ?assert(beamtalk_integer:has_method('class')),
    ?assert(beamtalk_integer:has_method('asString')),
    ?assert(beamtalk_integer:has_method('abs')),
    ?assert(beamtalk_integer:has_method('negated')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_integer:has_method('unknownMethod')),
    ?assertNot(beamtalk_integer:has_method('foo')),
    ?assertNot(beamtalk_integer:has_method('bar')).

has_method_extension_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"has_method returns true for registered extensions", fun() ->
            %% Initially false
            ?assertNot(beamtalk_integer:has_method('customMethod')),
            
            %% Register extension
            Fun = fun([], X) -> X * 2 end,
            beamtalk_extensions:register('Integer', 'customMethod', Fun, test_module),
            
            %% Now returns true
            ?assert(beamtalk_integer:has_method('customMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Edge Cases Tests
%%% ============================================================================

large_integer_test() ->
    Large = 999999999999999999,
    ?assertEqual(Large + 1, beamtalk_integer:dispatch('+', [1], Large)),
    ?assertEqual('Integer', beamtalk_integer:dispatch('class', [], Large)).

zero_test() ->
    ?assertEqual(0, beamtalk_integer:dispatch('abs', [], 0)),
    ?assertEqual(0, beamtalk_integer:dispatch('negated', [], 0)),
    ?assertEqual(<<"0">>, beamtalk_integer:dispatch('asString', [], 0)).

negative_numbers_test() ->
    ?assertEqual(-34, beamtalk_integer:dispatch('+', [-42], 8)),
    ?assertEqual(-50, beamtalk_integer:dispatch('-', [8], -42)),
    ?assertEqual(42, beamtalk_integer:dispatch('*', [-6], -7)).

mixed_integer_float_test() ->
    %% Integer operations should work with floats for comparisons
    ?assertEqual(true, beamtalk_integer:dispatch('<', [42.5], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('>', [42.5], 42)),
    ?assertEqual(false, beamtalk_integer:dispatch('=', [42.0], 42)),  % 42 =:= 42.0 is false (strict equality)
    
    %% Arithmetic with float should work (Erlang handles mixed arithmetic)
    ?assertEqual(50.5, beamtalk_integer:dispatch('+', [8.5], 42)),
    ?assertEqual(5.0, beamtalk_integer:dispatch('/', [8.4], 42)).
