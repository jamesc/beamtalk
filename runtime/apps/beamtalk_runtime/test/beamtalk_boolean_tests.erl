%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_boolean_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
    ?assertEqual('Boolean', beamtalk_boolean:dispatch('class', [], true)),
    ?assertEqual('Boolean', beamtalk_boolean:dispatch('class', [], false)).

%%% ============================================================================
%%% Control Flow Tests
%%% ============================================================================

if_true_test() ->
    ?assertEqual(42, beamtalk_boolean:dispatch('ifTrue:', [fun() -> 42 end], true)),
    ?assertEqual(false, beamtalk_boolean:dispatch('ifTrue:', [fun() -> 42 end], false)).

if_false_test() ->
    ?assertEqual(42, beamtalk_boolean:dispatch('ifFalse:', [fun() -> 42 end], false)),
    ?assertEqual(true, beamtalk_boolean:dispatch('ifFalse:', [fun() -> 42 end], true)).

if_true_if_false_test() ->
    ?assertEqual(yes, beamtalk_boolean:dispatch('ifTrue:ifFalse:', 
                                                  [fun() -> yes end, fun() -> no end], 
                                                  true)),
    ?assertEqual(no, beamtalk_boolean:dispatch('ifTrue:ifFalse:', 
                                                 [fun() -> yes end, fun() -> no end], 
                                                 false)).

control_flow_side_effects_test() ->
    %% Ensure blocks are only evaluated when appropriate
    Self = self(),
    
    %% ifTrue: should not evaluate block for false
    beamtalk_boolean:dispatch('ifTrue:', [fun() -> Self ! evaluated, ok end], false),
    receive evaluated -> ?assert(false) after 10 -> ok end,
    
    %% ifFalse: should not evaluate block for true
    beamtalk_boolean:dispatch('ifFalse:', [fun() -> Self ! evaluated, ok end], true),
    receive evaluated -> ?assert(false) after 10 -> ok end,
    
    %% ifTrue: should evaluate block for true
    beamtalk_boolean:dispatch('ifTrue:', [fun() -> Self ! evaluated, ok end], true),
    receive evaluated -> ok after 10 -> ?assert(false) end.

%%% ============================================================================
%%% Logical Operations Tests
%%% ============================================================================

not_test() ->
    ?assertEqual(false, beamtalk_boolean:dispatch('not', [], true)),
    ?assertEqual(true, beamtalk_boolean:dispatch('not', [], false)).

and_test() ->
    ?assertEqual(true, beamtalk_boolean:dispatch('and:', [fun() -> true end], true)),
    ?assertEqual(false, beamtalk_boolean:dispatch('and:', [fun() -> false end], true)),
    ?assertEqual(false, beamtalk_boolean:dispatch('and:', [fun() -> true end], false)).

and_short_circuit_test() ->
    %% AND should short-circuit - block not evaluated if receiver is false
    Self = self(),
    beamtalk_boolean:dispatch('and:', [fun() -> Self ! evaluated, true end], false),
    receive 
        evaluated -> ?assert(false)
    after 10 -> 
        ok
    end.

and_non_boolean_result_test() ->
    %% Non-boolean result from block is treated as false
    ?assertEqual(false, beamtalk_boolean:dispatch('and:', [fun() -> 42 end], true)),
    ?assertEqual(false, beamtalk_boolean:dispatch('and:', [fun() -> nil end], true)).

or_test() ->
    ?assertEqual(true, beamtalk_boolean:dispatch('or:', [fun() -> true end], false)),
    ?assertEqual(false, beamtalk_boolean:dispatch('or:', [fun() -> false end], false)),
    ?assertEqual(true, beamtalk_boolean:dispatch('or:', [fun() -> false end], true)).

or_short_circuit_test() ->
    %% OR should short-circuit - block not evaluated if receiver is true
    Self = self(),
    beamtalk_boolean:dispatch('or:', [fun() -> Self ! evaluated, false end], true),
    receive 
        evaluated -> ?assert(false)
    after 10 -> 
        ok
    end.

or_non_boolean_result_test() ->
    %% Non-boolean result from block is treated as true
    ?assertEqual(true, beamtalk_boolean:dispatch('or:', [fun() -> 42 end], false)),
    ?assertEqual(true, beamtalk_boolean:dispatch('or:', [fun() -> nil end], false)).

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"true">>, beamtalk_boolean:dispatch('asString', [], true)),
    ?assertEqual(<<"false">>, beamtalk_boolean:dispatch('asString', [], false)).

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_builtin_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Builtin methods return true", fun() ->
            ?assertEqual(true, beamtalk_boolean:has_method('class')),
            ?assertEqual(true, beamtalk_boolean:has_method('ifTrue:')),
            ?assertEqual(true, beamtalk_boolean:has_method('ifFalse:')),
            ?assertEqual(true, beamtalk_boolean:has_method('ifTrue:ifFalse:')),
            ?assertEqual(true, beamtalk_boolean:has_method('not')),
            ?assertEqual(true, beamtalk_boolean:has_method('and:')),
            ?assertEqual(true, beamtalk_boolean:has_method('or:')),
            ?assertEqual(true, beamtalk_boolean:has_method('asString')),
            ?assertEqual(false, beamtalk_boolean:has_method('unknownMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Extension Registry Tests
%%% ============================================================================

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods work via DNU", fun() ->
            %% Register a custom extension method
            ExtFun = fun([],_) -> <<"custom">> end,
            beamtalk_extensions:register('Boolean', 'customMethod', ExtFun, test_module),
            
            %% Should find the extension method
            ?assertEqual(true, beamtalk_boolean:has_method('customMethod')),
            
            %% Should dispatch to extension
            Result = beamtalk_boolean:dispatch('customMethod', [], true),
            ?assertEqual(<<"custom">>, Result)
        end},
        
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Boolean', selector = 'unknownMethod'},
                         beamtalk_boolean:dispatch('unknownMethod', [], true))
        end}
    ]}.

%%% ============================================================================
%%% Type Safety Tests
%%% ============================================================================

type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Non-function argument to ifTrue: raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Boolean', selector = 'ifTrue:'},
                         beamtalk_boolean:dispatch('ifTrue:', [42], true))
        end},
        {"Non-function argument to and: raises does_not_understand", fun() ->
            ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Boolean', selector = 'and:'},
                         beamtalk_boolean:dispatch('and:', [42], true))
        end}
    ]}.

%%% ============================================================================
%%% Instance Variable Reflection Tests (BT-164)
%%% ============================================================================

instVarNames_test() ->
    %% Primitives have no instance variables
    ?assertEqual([], beamtalk_boolean:dispatch('instVarNames', [], true)),
    ?assertEqual([], beamtalk_boolean:dispatch('instVarNames', [], false)).

instVarAt_test() ->
    %% Primitives always return nil for instVarAt:
    ?assertEqual(nil, beamtalk_boolean:dispatch('instVarAt', [anyField], true)),
    ?assertEqual(nil, beamtalk_boolean:dispatch('instVarAt', [value], false)).

instVarAt_put_immutable_error_test() ->
    %% Trying to mutate a primitive should raise an immutable_primitive error
    ?assertError({beamtalk_error, immutable_primitive, 'Boolean', 'instVarAt:put:', _, _, _},
                 beamtalk_boolean:dispatch('instVarAt:put:', [value, false], true)).
