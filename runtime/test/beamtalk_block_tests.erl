%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_block_tests).
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
    ?assertEqual('Block', beamtalk_block:dispatch('class', [], fun() -> ok end)),
    ?assertEqual('Block', beamtalk_block:dispatch('class', [], fun(X) -> X end)),
    ?assertEqual('Block', beamtalk_block:dispatch('class', [], fun(X, Y) -> X + Y end)).

%%% ============================================================================
%%% Evaluation Tests
%%% ============================================================================

value_test() ->
    ?assertEqual(42, beamtalk_block:dispatch('value', [], fun() -> 42 end)),
    ?assertEqual(ok, beamtalk_block:dispatch('value', [], fun() -> ok end)),
    ?assertEqual(hello, beamtalk_block:dispatch('value', [], fun() -> hello end)).

value_with_one_arg_test() ->
    ?assertEqual(11, beamtalk_block:dispatch('value:', [10], fun(X) -> X + 1 end)),
    ?assertEqual(20, beamtalk_block:dispatch('value:', [10], fun(X) -> X * 2 end)),
    ?assertEqual(hello, beamtalk_block:dispatch('value:', [greeting], fun(_) -> hello end)).

value_with_two_args_test() ->
    ?assertEqual(15, beamtalk_block:dispatch('value:value:', [10, 5], fun(X, Y) -> X + Y end)),
    ?assertEqual(50, beamtalk_block:dispatch('value:value:', [10, 5], fun(X, Y) -> X * Y end)),
    ?assertEqual({a, b}, beamtalk_block:dispatch('value:value:', [a, b], fun(X, Y) -> {X, Y} end)).

value_closure_test() ->
    %% Test that blocks capture variables
    Multiplier = 3,
    Block = fun(X) -> X * Multiplier end,
    ?assertEqual(15, beamtalk_block:dispatch('value:', [5], Block)),
    ?assertEqual(30, beamtalk_block:dispatch('value:', [10], Block)).

value_side_effects_test() ->
    %% Test that blocks execute and produce side effects
    Self = self(),
    Block = fun() -> Self ! evaluated, 42 end,
    Result = beamtalk_block:dispatch('value', [], Block),
    ?assertEqual(42, Result),
    receive 
        evaluated -> ok
    after 100 -> 
        ?assert(false)
    end.

%%% ============================================================================
%%% Arity Tests
%%% ============================================================================

arity_test() ->
    ?assertEqual(0, beamtalk_block:dispatch('arity', [], fun() -> ok end)),
    ?assertEqual(1, beamtalk_block:dispatch('arity', [], fun(_) -> ok end)),
    ?assertEqual(2, beamtalk_block:dispatch('arity', [], fun(_, _) -> ok end)),
    ?assertEqual(3, beamtalk_block:dispatch('arity', [], fun(_, _, _) -> ok end)).

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"<Block>">>, beamtalk_block:dispatch('asString', [], fun() -> ok end)),
    ?assertEqual(<<"<Block>">>, beamtalk_block:dispatch('asString', [], fun(X) -> X end)).

%%% ============================================================================
%%% Error Cases Tests
%%% ============================================================================

value_arity_mismatch_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Calling value on 1-arg block raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Block', 'value', 0},
                         beamtalk_block:dispatch('value', [], fun(X) -> X end))
        end},
        {"Calling value: on 0-arg block raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Block', 'value:', 1},
                         beamtalk_block:dispatch('value:', [42], fun() -> ok end))
        end},
        {"Calling value:value: on 1-arg block raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Block', 'value:value:', 2},
                         beamtalk_block:dispatch('value:value:', [1, 2], fun(X) -> X end))
        end}
    ]}.

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_builtin_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Builtin methods return true", fun() ->
            ?assertEqual(true, beamtalk_block:has_method('class')),
            ?assertEqual(true, beamtalk_block:has_method('value')),
            ?assertEqual(true, beamtalk_block:has_method('value:')),
            ?assertEqual(true, beamtalk_block:has_method('value:value:')),
            ?assertEqual(true, beamtalk_block:has_method('arity')),
            ?assertEqual(true, beamtalk_block:has_method('asString')),
            ?assertEqual(false, beamtalk_block:has_method('unknownMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Extension Registry Tests
%%% ============================================================================

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods work via DNU", fun() ->
            %% Register a custom extension method
            ExtFun = fun([], _Block) -> <<"custom">> end,
            beamtalk_extensions:register('Block', 'customMethod', ExtFun, test_module),
            
            %% Should find the extension method
            ?assertEqual(true, beamtalk_block:has_method('customMethod')),
            
            %% Should dispatch to extension
            Result = beamtalk_block:dispatch('customMethod', [], fun() -> ok end),
            ?assertEqual(<<"custom">>, Result)
        end},
        
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'Block', 'unknownMethod', 0},
                         beamtalk_block:dispatch('unknownMethod', [], fun() -> ok end))
        end}
    ]}.

%%% ============================================================================
%%% Instance Variable Reflection Tests (BT-164)
%%% ============================================================================

instVarNames_test() ->
    %% Primitives have no instance variables
    ?assertEqual([], beamtalk_block:dispatch('instVarNames', [], fun() -> ok end)),
    ?assertEqual([], beamtalk_block:dispatch('instVarNames', [], fun(X) -> X end)).

instVarAt_test() ->
    %% Primitives always return nil for instVarAt:
    ?assertEqual(nil, beamtalk_block:dispatch('instVarAt', [anyField], fun() -> ok end)),
    ?assertEqual(nil, beamtalk_block:dispatch('instVarAt', [value], fun(X) -> X * 2 end)).

instVarAt_put_immutable_error_test() ->
    %% Trying to mutate a primitive should raise an immutable_primitive error
    ?assertError({beamtalk_error, immutable_primitive, 'Block', 'instVarAt:put:', _, _, _},
                 beamtalk_block:dispatch('instVarAt:put:', [value, 42], fun() -> ok end)).
