%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_nil_tests).
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
    ?assertEqual('UndefinedObject', beamtalk_nil:dispatch('class', [], nil)).

%%% ============================================================================
%%% Nil Checking Tests
%%% ============================================================================

is_nil_test() ->
    ?assertEqual(true, beamtalk_nil:dispatch('isNil', [], nil)).

%%% ============================================================================
%%% Control Flow Tests
%%% ============================================================================

if_nil_test() ->
    ?assertEqual(42, beamtalk_nil:dispatch('ifNil:', [fun() -> 42 end], nil)),
    ?assertEqual(default, beamtalk_nil:dispatch('ifNil:', [fun() -> default end], nil)).

if_not_nil_test() ->
    %% nil never evaluates the notNil block
    ?assertEqual(nil, beamtalk_nil:dispatch('ifNotNil:', [fun() -> 42 end], nil)).

if_not_nil_side_effects_test() ->
    %% Ensure block is not evaluated
    Self = self(),
    beamtalk_nil:dispatch('ifNotNil:', [fun() -> Self ! evaluated, ok end], nil),
    receive 
        evaluated -> ?assert(false)
    after 10 -> 
        ok
    end.

if_nil_if_not_nil_test() ->
    ?assertEqual(was_nil, beamtalk_nil:dispatch('ifNil:ifNotNil:', 
                                                   [fun() -> was_nil end, 
                                                    fun() -> was_not_nil end], 
                                                   nil)).

if_nil_if_not_nil_side_effects_test() ->
    %% Ensure only the nil block is evaluated
    Self = self(),
    beamtalk_nil:dispatch('ifNil:ifNotNil:', 
                           [fun() -> Self ! nil_block, ok end,
                            fun() -> Self ! not_nil_block, ok end], 
                           nil),
    receive 
        nil_block -> ok
    after 10 -> 
        ?assert(false)
    end,
    receive 
        not_nil_block -> ?assert(false)
    after 10 -> 
        ok
    end.

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_string_test() ->
    ?assertEqual(<<"nil">>, beamtalk_nil:dispatch('asString', [], nil)).

%%% ============================================================================
%%% has_method Tests
%%% ============================================================================

has_method_builtin_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Builtin methods return true", fun() ->
            ?assertEqual(true, beamtalk_nil:has_method('class')),
            ?assertEqual(true, beamtalk_nil:has_method('isNil')),
            ?assertEqual(true, beamtalk_nil:has_method('ifNil:')),
            ?assertEqual(true, beamtalk_nil:has_method('ifNotNil:')),
            ?assertEqual(true, beamtalk_nil:has_method('ifNil:ifNotNil:')),
            ?assertEqual(true, beamtalk_nil:has_method('asString')),
            ?assertEqual(false, beamtalk_nil:has_method('unknownMethod'))
        end}
    ]}.

%%% ============================================================================
%%% Extension Registry Tests
%%% ============================================================================

extension_method_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Extension methods work via DNU", fun() ->
            %% Register a custom extension method
            ExtFun = fun([], nil) -> <<"custom">> end,
            beamtalk_extensions:register('UndefinedObject', 'customMethod', ExtFun, test_module),
            
            %% Should find the extension method
            ?assertEqual(true, beamtalk_nil:has_method('customMethod')),
            
            %% Should dispatch to extension
            Result = beamtalk_nil:dispatch('customMethod', [], nil),
            ?assertEqual(<<"custom">>, Result)
        end},
        
        {"Unknown method raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'UndefinedObject', 'unknownMethod', 0},
                         beamtalk_nil:dispatch('unknownMethod', [], nil))
        end}
    ]}.

%%% ============================================================================
%%% Type Safety Tests
%%% ============================================================================

type_safety_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Non-function argument to ifNil: raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'UndefinedObject', 'ifNil:', 1},
                         beamtalk_nil:dispatch('ifNil:', [42], nil))
        end},
        {"Non-function argument to ifNil:ifNotNil: raises does_not_understand", fun() ->
            ?assertError({does_not_understand, 'UndefinedObject', 'ifNil:ifNotNil:', 2},
                         beamtalk_nil:dispatch('ifNil:ifNotNil:', [42, fun() -> ok end], nil))
        end}
    ]}.
