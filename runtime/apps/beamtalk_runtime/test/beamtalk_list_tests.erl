%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_list_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Reflection Tests
%%% ============================================================================

class_test() ->
    ?assertEqual('Array', beamtalk_list:dispatch('class', [], [1, 2, 3])),
    ?assertEqual('Array', beamtalk_list:dispatch('class', [], [])).

responds_to_test() ->
    beamtalk_extensions:init(),
    ?assertEqual(true, beamtalk_list:has_method('class')),
    ?assertEqual(true, beamtalk_list:has_method('size')),
    ?assertEqual(true, beamtalk_list:has_method('isEmpty')),
    ?assertEqual(true, beamtalk_list:has_method('first')),
    ?assertEqual(true, beamtalk_list:has_method('do:')),
    ?assertEqual(true, beamtalk_list:has_method('collect:')),
    ?assertEqual(true, beamtalk_list:has_method('select:')),
    ?assertEqual(true, beamtalk_list:has_method('reject:')),
    ?assertEqual(true, beamtalk_list:has_method('inject:into:')),
    ?assertEqual(false, beamtalk_list:has_method('nonExistent')).

%%% ============================================================================
%%% Basic Operations
%%% ============================================================================

size_test() ->
    ?assertEqual(3, beamtalk_list:dispatch('size', [], [1, 2, 3])),
    ?assertEqual(0, beamtalk_list:dispatch('size', [], [])).

is_empty_test() ->
    ?assertEqual(true, beamtalk_list:dispatch('isEmpty', [], [])),
    ?assertEqual(false, beamtalk_list:dispatch('isEmpty', [], [1])).

first_test() ->
    ?assertEqual(1, beamtalk_list:dispatch('first', [], [1, 2, 3])).

first_empty_test() ->
    ?assertError(_, beamtalk_list:dispatch('first', [], [])).

rest_test() ->
    ?assertEqual([2, 3], beamtalk_list:dispatch('rest', [], [1, 2, 3])),
    ?assertEqual([], beamtalk_list:dispatch('rest', [], [])).

%%% ============================================================================
%%% Iteration Operations
%%% ============================================================================

do_test() ->
    ?assertEqual(nil, beamtalk_list:dispatch('do:', [fun(_) -> ok end], [1, 2, 3])).

collect_test() ->
    ?assertEqual([2, 4, 6],
        beamtalk_list:dispatch('collect:', [fun(X) -> X * 2 end], [1, 2, 3])).

select_test() ->
    ?assertEqual([2, 4],
        beamtalk_list:dispatch('select:', [fun(X) -> X rem 2 =:= 0 end], [1, 2, 3, 4])).

reject_test() ->
    ?assertEqual([1, 3],
        beamtalk_list:dispatch('reject:', [fun(X) -> X rem 2 =:= 0 end], [1, 2, 3, 4])).

inject_into_test() ->
    ?assertEqual(10,
        beamtalk_list:dispatch('inject:into:', [0, fun(Acc, X) -> Acc + X end], [1, 2, 3, 4])).

%%% ============================================================================
%%% Does Not Understand
%%% ============================================================================

does_not_understand_test() ->
    beamtalk_extensions:init(),
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'Array'},
        beamtalk_list:dispatch('nonExistent', [], [1, 2, 3])).
