%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_tuple_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Setup/Cleanup
%%% ============================================================================

setup() ->
    beamtalk_extensions:init(),
    ok.

cleanup(_) ->
    ok.

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('Tuple', beamtalk_tuple:dispatch('class', [], {a, b})),
    ?assertEqual('Tuple', beamtalk_tuple:dispatch('class', [], {ok, 42})),
    ?assertEqual('Tuple', beamtalk_tuple:dispatch('class', [], {})).

%%% ============================================================================
%%% Size Tests
%%% ============================================================================

size_test() ->
    ?assertEqual(0, beamtalk_tuple:dispatch('size', [], {})),
    ?assertEqual(1, beamtalk_tuple:dispatch('size', [], {a})),
    ?assertEqual(2, beamtalk_tuple:dispatch('size', [], {a, b})),
    ?assertEqual(3, beamtalk_tuple:dispatch('size', [], {a, b, c})).

%%% ============================================================================
%%% Access Tests
%%% ============================================================================

at_test() ->
    ?assertEqual(a, beamtalk_tuple:dispatch('at:', [1], {a, b, c})),
    ?assertEqual(b, beamtalk_tuple:dispatch('at:', [2], {a, b, c})),
    ?assertEqual(c, beamtalk_tuple:dispatch('at:', [3], {a, b, c})),
    ?assertEqual(42, beamtalk_tuple:dispatch('at:', [2], {ok, 42})).

at_out_of_bounds_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Out of bounds access raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Tuple', 'at:', 1},
                         beamtalk_tuple:dispatch('at:', [0], {a, b})),
            ?assertError({does_not_understand, 'Tuple', 'at:', 1},
                         beamtalk_tuple:dispatch('at:', [3], {a, b})),
            ?assertError({does_not_understand, 'Tuple', 'at:', 1},
                         beamtalk_tuple:dispatch('at:', [-1], {a, b}))
        end}
    ]}.

at_empty_tuple_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Empty tuple access raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Tuple', 'at:', 1},
                         beamtalk_tuple:dispatch('at:', [1], {}))
        end}
    ]}.

%%% ============================================================================
%%% Pattern Matching Tests
%%% ============================================================================

is_ok_test() ->
    ?assertEqual(true, beamtalk_tuple:dispatch('isOk', [], {ok, 42})),
    ?assertEqual(true, beamtalk_tuple:dispatch('isOk', [], {ok, value})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isOk', [], {error, reason})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isOk', [], {a, b})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isOk', [], {ok})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isOk', [], {})).

is_error_test() ->
    ?assertEqual(true, beamtalk_tuple:dispatch('isError', [], {error, not_found})),
    ?assertEqual(true, beamtalk_tuple:dispatch('isError', [], {error, reason})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isError', [], {ok, 42})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isError', [], {a, b})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isError', [], {error})),
    ?assertEqual(false, beamtalk_tuple:dispatch('isError', [], {})).

%%% ============================================================================
%%% Unwrapping Tests
%%% ============================================================================

unwrap_ok_test() ->
    ?assertEqual(42, beamtalk_tuple:dispatch('unwrap', [], {ok, 42})),
    ?assertEqual(hello, beamtalk_tuple:dispatch('unwrap', [], {ok, hello})),
    ?assertEqual({a, b}, beamtalk_tuple:dispatch('unwrap', [], {ok, {a, b}})).

unwrap_error_test() ->
    ?assertError({unwrap_error, not_found},
                 beamtalk_tuple:dispatch('unwrap', [], {error, not_found})),
    ?assertError({unwrap_error, reason},
                 beamtalk_tuple:dispatch('unwrap', [], {error, reason})).

unwrap_invalid_pattern_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Unwrap on non-result tuple raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Tuple', 'unwrap', 0},
                         beamtalk_tuple:dispatch('unwrap', [], {a, b}))
        end}
    ]}.

unwrap_or_test() ->
    ?assertEqual(42, beamtalk_tuple:dispatch('unwrapOr:', [default], {ok, 42})),
    ?assertEqual(default, beamtalk_tuple:dispatch('unwrapOr:', [default], {error, reason})),
    ?assertEqual(default, beamtalk_tuple:dispatch('unwrapOr:', [default], {a, b})),
    ?assertEqual(nil, beamtalk_tuple:dispatch('unwrapOr:', [nil], {error, not_found})).

unwrap_or_else_test() ->
    ?assertEqual(42, beamtalk_tuple:dispatch('unwrapOrElse:', [fun() -> default end], {ok, 42})),
    ?assertEqual(default, beamtalk_tuple:dispatch('unwrapOrElse:', [fun() -> default end], {error, reason})),
    ?assertEqual(default, beamtalk_tuple:dispatch('unwrapOrElse:', [fun() -> default end], {a, b})).

unwrap_or_else_side_effects_test() ->
    %% Block should not be evaluated for {ok, _}
    Self = self(),
    beamtalk_tuple:dispatch('unwrapOrElse:', [fun() -> Self ! evaluated, default end], {ok, 42}),
    receive 
        evaluated -> ?assert(false)
    after 10 -> 
        ok
    end,
    
    %% Block should be evaluated for other patterns
    beamtalk_tuple:dispatch('unwrapOrElse:', [fun() -> Self ! evaluated, default end], {error, reason}),
    receive 
        evaluated -> ok
    after 100 -> 
        ?assert(false)
    end.

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"{a, b}">>, beamtalk_tuple:dispatch('asString', [], {a, b})),
    ?assertEqual(<<"{ok, 42}">>, beamtalk_tuple:dispatch('asString', [], {ok, 42})),
    ?assertEqual(<<"{error, not_found}">>, beamtalk_tuple:dispatch('asString', [], {error, not_found})),
    ?assertEqual(<<"{hello}">>, beamtalk_tuple:dispatch('asString', [], {hello})),
    ?assertEqual(<<"{}">>, beamtalk_tuple:dispatch('asString', [], {})).

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_builtin_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Builtin methods return true", fun() ->
            ?assertEqual(true, beamtalk_tuple:has_method('class')),
            ?assertEqual(true, beamtalk_tuple:has_method('size')),
            ?assertEqual(true, beamtalk_tuple:has_method('at:')),
            ?assertEqual(true, beamtalk_tuple:has_method('isOk')),
            ?assertEqual(true, beamtalk_tuple:has_method('isError')),
            ?assertEqual(true, beamtalk_tuple:has_method('unwrap')),
            ?assertEqual(true, beamtalk_tuple:has_method('unwrapOr:')),
            ?assertEqual(true, beamtalk_tuple:has_method('unwrapOrElse:')),
            ?assertEqual(true, beamtalk_tuple:has_method('asString')),
            ?assertEqual(false, beamtalk_tuple:has_method('unknownMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Extension Registry Tests
%%% ============================================================================

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods work via DNU", fun() ->
            %% Register a custom extension method
            ExtFun = fun([], _Tuple) -> <<"custom">> end,
            beamtalk_extensions:register('Tuple', 'customMethod', ExtFun, test_module),
            
            %% Should find the extension method
            ?assertEqual(true, beamtalk_tuple:has_method('customMethod')),
            
            %% Should dispatch to extension
            Result = beamtalk_tuple:dispatch('customMethod', [], {a, b}),
            ?assertEqual(<<"custom">>, Result)
        end},
        
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Tuple', 'unknownMethod', 0},
                         beamtalk_tuple:dispatch('unknownMethod', [], {a, b}))
        end}
    ]}.

%%% ============================================================================
%%% Type Safety Tests
%%% ============================================================================

type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Non-function argument to unwrapOrElse: raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Tuple', 'unwrapOrElse:', 1},
                         beamtalk_tuple:dispatch('unwrapOrElse:', [42], {error, reason}))
        end}
    ]}.

%%% ============================================================================
%%% Instance Variable Reflection Tests (BT-164)
%%% ============================================================================

instVarNames_test() ->
    %% Primitives have no instance variables
    ?assertEqual([], beamtalk_tuple:dispatch('instVarNames', [], {a, b})),
    ?assertEqual([], beamtalk_tuple:dispatch('instVarNames', [], {})).

instVarAt_test() ->
    %% Primitives always return nil for instVarAt:
    ?assertEqual(nil, beamtalk_tuple:dispatch('instVarAt', [anyField], {ok, 42})),
    ?assertEqual(nil, beamtalk_tuple:dispatch('instVarAt', [value], {a, b, c})).

instVarAt_put_immutable_error_test() ->
    %% Trying to mutate a primitive should raise an immutable_primitive error
    ?assertError({beamtalk_error, immutable_primitive, 'Tuple', 'instVarAt:put:', _, _, _},
                 beamtalk_tuple:dispatch('instVarAt:put:', [value, 99], {a, b})).
