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
    ?assertEqual(true, beamtalk_list:has_method('last')),
    ?assertEqual(true, beamtalk_list:has_method('at:')),
    ?assertEqual(true, beamtalk_list:has_method('includes:')),
    ?assertEqual(true, beamtalk_list:has_method('sort')),
    ?assertEqual(true, beamtalk_list:has_method('sort:')),
    ?assertEqual(true, beamtalk_list:has_method('reversed')),
    ?assertEqual(true, beamtalk_list:has_method('detect:')),
    ?assertEqual(true, beamtalk_list:has_method('detect:ifNone:')),
    ?assertEqual(true, beamtalk_list:has_method('do:')),
    ?assertEqual(true, beamtalk_list:has_method('collect:')),
    ?assertEqual(true, beamtalk_list:has_method('select:')),
    ?assertEqual(true, beamtalk_list:has_method('reject:')),
    ?assertEqual(true, beamtalk_list:has_method('inject:into:')),
    ?assertEqual(true, beamtalk_list:has_method('take:')),
    ?assertEqual(true, beamtalk_list:has_method('drop:')),
    ?assertEqual(true, beamtalk_list:has_method('flatten')),
    ?assertEqual(true, beamtalk_list:has_method('flatMap:')),
    ?assertEqual(true, beamtalk_list:has_method('count:')),
    ?assertEqual(true, beamtalk_list:has_method('anySatisfy:')),
    ?assertEqual(true, beamtalk_list:has_method('allSatisfy:')),
    ?assertEqual(true, beamtalk_list:has_method('zip:')),
    ?assertEqual(true, beamtalk_list:has_method('unique')),
    ?assertEqual(true, beamtalk_list:has_method('add:')),
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

%%% ============================================================================
%%% Tier 1: Essential Methods
%%% ============================================================================

last_test() ->
    ?assertEqual(3, beamtalk_list:dispatch('last', [], [1, 2, 3])),
    ?assertEqual(1, beamtalk_list:dispatch('last', [], [1])).

last_empty_test() ->
    ?assertError(_, beamtalk_list:dispatch('last', [], [])).

at_test() ->
    ?assertEqual(1, beamtalk_list:dispatch('at:', [1], [1, 2, 3])),
    ?assertEqual(2, beamtalk_list:dispatch('at:', [2], [1, 2, 3])),
    ?assertEqual(3, beamtalk_list:dispatch('at:', [3], [1, 2, 3])).

at_out_of_bounds_test() ->
    ?assertError(_, beamtalk_list:dispatch('at:', [0], [1, 2, 3])),
    ?assertError(_, beamtalk_list:dispatch('at:', [4], [1, 2, 3])).

includes_test() ->
    ?assertEqual(true, beamtalk_list:dispatch('includes:', [2], [1, 2, 3])),
    ?assertEqual(false, beamtalk_list:dispatch('includes:', [4], [1, 2, 3])),
    ?assertEqual(false, beamtalk_list:dispatch('includes:', [1], [])).

sort_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list:dispatch('sort', [], [3, 1, 2])),
    ?assertEqual([], beamtalk_list:dispatch('sort', [], [])).

sort_with_block_test() ->
    %% Sort descending
    ?assertEqual([3, 2, 1],
        beamtalk_list:dispatch('sort:', [fun(A, B) -> A >= B end], [1, 3, 2])).

reversed_test() ->
    ?assertEqual([3, 2, 1], beamtalk_list:dispatch('reversed', [], [1, 2, 3])),
    ?assertEqual([], beamtalk_list:dispatch('reversed', [], [])).

detect_test() ->
    ?assertEqual(2,
        beamtalk_list:dispatch('detect:', [fun(X) -> X > 1 end], [1, 2, 3])).

detect_not_found_test() ->
    ?assertError(_, beamtalk_list:dispatch('detect:', [fun(X) -> X > 10 end], [1, 2, 3])).

detect_if_none_test() ->
    ?assertEqual(2,
        beamtalk_list:dispatch('detect:ifNone:', [fun(X) -> X > 1 end, 0], [1, 2, 3])),
    ?assertEqual(0,
        beamtalk_list:dispatch('detect:ifNone:', [fun(X) -> X > 10 end, 0], [1, 2, 3])).

%%% ============================================================================
%%% Tier 2: Functional Methods
%%% ============================================================================

take_test() ->
    ?assertEqual([1, 2], beamtalk_list:dispatch('take:', [2], [1, 2, 3])),
    ?assertEqual([1, 2, 3], beamtalk_list:dispatch('take:', [5], [1, 2, 3])),
    ?assertEqual([], beamtalk_list:dispatch('take:', [0], [1, 2, 3])).

drop_test() ->
    ?assertEqual([3], beamtalk_list:dispatch('drop:', [2], [1, 2, 3])),
    ?assertEqual([], beamtalk_list:dispatch('drop:', [5], [1, 2, 3])),
    ?assertEqual([1, 2, 3], beamtalk_list:dispatch('drop:', [0], [1, 2, 3])).

flatten_test() ->
    ?assertEqual([1, 2, 3, 4], beamtalk_list:dispatch('flatten', [], [[1, 2], [3, 4]])),
    ?assertEqual([1, 2, 3], beamtalk_list:dispatch('flatten', [], [[1, [2, 3]]])).

flat_map_test() ->
    ?assertEqual([1, 1, 2, 2, 3, 3],
        beamtalk_list:dispatch('flatMap:', [fun(X) -> [X, X] end], [1, 2, 3])).

count_test() ->
    ?assertEqual(2,
        beamtalk_list:dispatch('count:', [fun(X) -> X > 1 end], [1, 2, 3])).

any_satisfy_test() ->
    ?assertEqual(true,
        beamtalk_list:dispatch('anySatisfy:', [fun(X) -> X > 2 end], [1, 2, 3])),
    ?assertEqual(false,
        beamtalk_list:dispatch('anySatisfy:', [fun(X) -> X > 5 end], [1, 2, 3])).

all_satisfy_test() ->
    ?assertEqual(true,
        beamtalk_list:dispatch('allSatisfy:', [fun(X) -> X > 0 end], [1, 2, 3])),
    ?assertEqual(false,
        beamtalk_list:dispatch('allSatisfy:', [fun(X) -> X > 1 end], [1, 2, 3])).

%%% ============================================================================
%%% Tier 3: Advanced Methods
%%% ============================================================================

zip_test() ->
    Result = beamtalk_list:dispatch('zip:', [[4, 5, 6]], [1, 2, 3]),
    ?assertEqual([#{<<"key">> => 1, <<"value">> => 4},
                  #{<<"key">> => 2, <<"value">> => 5},
                  #{<<"key">> => 3, <<"value">> => 6}], Result).

group_by_test() ->
    Result = beamtalk_list:dispatch('groupBy:',
        [fun(X) -> X rem 2 =:= 0 end], [1, 2, 3, 4]),
    ?assertEqual(#{false => [1, 3], true => [2, 4]}, Result).

partition_test() ->
    Result = beamtalk_list:dispatch('partition:',
        [fun(X) -> X > 2 end], [1, 2, 3, 4]),
    ?assertEqual(#{<<"matching">> => [3, 4], <<"nonMatching">> => [1, 2]}, Result).

unique_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list:dispatch('unique', [], [3, 1, 2, 1, 3])).

take_while_test() ->
    ?assertEqual([1, 2],
        beamtalk_list:dispatch('takeWhile:', [fun(X) -> X < 3 end], [1, 2, 3, 4])).

drop_while_test() ->
    ?assertEqual([3, 4],
        beamtalk_list:dispatch('dropWhile:', [fun(X) -> X < 3 end], [1, 2, 3, 4])).

intersperse_test() ->
    ?assertEqual([1, 0, 2, 0, 3],
        beamtalk_list:dispatch('intersperse:', [0], [1, 2, 3])),
    ?assertEqual([], beamtalk_list:dispatch('intersperse:', [0], [])),
    ?assertEqual([1], beamtalk_list:dispatch('intersperse:', [0], [1])).

add_test() ->
    ?assertEqual([1, 2, 3, 4], beamtalk_list:dispatch('add:', [4], [1, 2, 3])),
    ?assertEqual([1], beamtalk_list:dispatch('add:', [1], [])).
