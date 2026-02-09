%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_map_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('Dictionary', beamtalk_dictionary:dispatch('class', [], #{a => 1})),
    ?assertEqual('Dictionary', beamtalk_dictionary:dispatch('class', [], #{})).

responds_to_test() ->
    beamtalk_extensions:init(),
    ?assertEqual(true, beamtalk_dictionary:has_method('class')),
    ?assertEqual(true, beamtalk_dictionary:has_method('keys')),
    ?assertEqual(true, beamtalk_dictionary:has_method('values')),
    ?assertEqual(true, beamtalk_dictionary:has_method('size')),
    ?assertEqual(true, beamtalk_dictionary:has_method('at:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('at:put:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('at:ifAbsent:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('includesKey:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('removeKey:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('merge:')),
    ?assertEqual(true, beamtalk_dictionary:has_method('keysAndValuesDo:')),
    ?assertEqual(false, beamtalk_dictionary:has_method('nonExistent')).

%%% ============================================================================
%%% Basic Operations
%%% ============================================================================

keys_test() ->
    ?assertEqual([a], beamtalk_dictionary:dispatch('keys', [], #{a => 1})),
    ?assertEqual([], beamtalk_dictionary:dispatch('keys', [], #{})).

values_test() ->
    ?assertEqual([1], beamtalk_dictionary:dispatch('values', [], #{a => 1})).

size_test() ->
    ?assertEqual(2, beamtalk_dictionary:dispatch('size', [], #{a => 1, b => 2})),
    ?assertEqual(0, beamtalk_dictionary:dispatch('size', [], #{})).

at_test() ->
    ?assertEqual(1, beamtalk_dictionary:dispatch('at:', [a], #{a => 1, b => 2})).

at_put_test() ->
    ?assertEqual(#{a => 1, b => 2},
        beamtalk_dictionary:dispatch('at:put:', [b, 2], #{a => 1})).

at_if_absent_test() ->
    ?assertEqual(1, beamtalk_dictionary:dispatch('at:ifAbsent:', [a, fun() -> 99 end], #{a => 1})),
    ?assertEqual(99, beamtalk_dictionary:dispatch('at:ifAbsent:', [b, fun() -> 99 end], #{a => 1})).

includes_key_test() ->
    ?assertEqual(true, beamtalk_dictionary:dispatch('includesKey:', [a], #{a => 1})),
    ?assertEqual(false, beamtalk_dictionary:dispatch('includesKey:', [b], #{a => 1})).

remove_key_test() ->
    ?assertEqual(#{}, beamtalk_dictionary:dispatch('removeKey:', [a], #{a => 1})).

merge_test() ->
    ?assertEqual(#{a => 1, b => 2},
        beamtalk_dictionary:dispatch('merge:', [#{b => 2}], #{a => 1})).

keys_and_values_do_test() ->
    ?assertEqual(nil,
        beamtalk_dictionary:dispatch('keysAndValuesDo:', [fun(_, _) -> ok end], #{a => 1})).

%%% ============================================================================
%%% Does Not Understand
%%% ============================================================================

does_not_understand_test() ->
    beamtalk_extensions:init(),
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Dictionary'},
        beamtalk_dictionary:dispatch('nonExistent', [], #{a => 1})).
