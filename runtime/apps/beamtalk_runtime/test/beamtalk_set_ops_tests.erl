%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_set_ops — Set operations using ordsets in tagged maps.
%%
%% BT-73: Verifies all Set operations work correctly with the tagged map
%% representation #{'$beamtalk_class' => 'Set', elements => OrdsetData}.
-module(beamtalk_set_ops_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Creation Tests
%%% ============================================================================

new_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => []}, Set).

from_list_test() ->
    Set = beamtalk_set_ops:from_list([3, 1, 2]),
    %% ordsets sorts elements
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3]}, Set).

from_list_dedup_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 2, 3, 1]),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3]}, Set).

from_list_empty_test() ->
    Set = beamtalk_set_ops:from_list([]),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => []}, Set).

%%% ============================================================================
%%% Size Tests
%%% ============================================================================

size_empty_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual(0, beamtalk_set_ops:size(Set)).

size_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(3, beamtalk_set_ops:size(Set)).

%%% ============================================================================
%%% isEmpty Tests
%%% ============================================================================

is_empty_true_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual(true, beamtalk_set_ops:is_empty(Set)).

is_empty_false_test() ->
    Set = beamtalk_set_ops:from_list([1]),
    ?assertEqual(false, beamtalk_set_ops:is_empty(Set)).

%%% ============================================================================
%%% Membership Tests
%%% ============================================================================

includes_true_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(true, beamtalk_set_ops:includes(Set, 2)).

includes_false_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(false, beamtalk_set_ops:includes(Set, 5)).

includes_empty_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual(false, beamtalk_set_ops:includes(Set, 1)).

%%% ============================================================================
%%% Add Tests
%%% ============================================================================

add_new_element_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    Result = beamtalk_set_ops:add(Set, 3),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3]}, Result).

add_existing_element_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Result = beamtalk_set_ops:add(Set, 2),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3]}, Result).

add_to_empty_test() ->
    Set = beamtalk_set_ops:new(),
    Result = beamtalk_set_ops:add(Set, 42),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [42]}, Result).

%%% ============================================================================
%%% Remove Tests
%%% ============================================================================

remove_existing_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Result = beamtalk_set_ops:remove(Set, 2),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 3]}, Result).

remove_nonexistent_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Result = beamtalk_set_ops:remove(Set, 5),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3]}, Result).

%%% ============================================================================
%%% Set Operation Tests
%%% ============================================================================

union_test() ->
    A = beamtalk_set_ops:from_list([1, 2, 3]),
    B = beamtalk_set_ops:from_list([3, 4, 5]),
    Result = beamtalk_set_ops:union(A, B),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1, 2, 3, 4, 5]}, Result).

union_empty_test() ->
    A = beamtalk_set_ops:from_list([1, 2]),
    B = beamtalk_set_ops:new(),
    ?assertEqual(A, beamtalk_set_ops:union(A, B)).

intersection_test() ->
    A = beamtalk_set_ops:from_list([1, 2, 3]),
    B = beamtalk_set_ops:from_list([2, 3, 4]),
    Result = beamtalk_set_ops:intersection(A, B),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [2, 3]}, Result).

intersection_disjoint_test() ->
    A = beamtalk_set_ops:from_list([1, 2]),
    B = beamtalk_set_ops:from_list([3, 4]),
    Result = beamtalk_set_ops:intersection(A, B),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => []}, Result).

difference_test() ->
    A = beamtalk_set_ops:from_list([1, 2, 3]),
    B = beamtalk_set_ops:from_list([2, 3, 4]),
    Result = beamtalk_set_ops:difference(A, B),
    ?assertEqual(#{'$beamtalk_class' => 'Set', elements => [1]}, Result).

difference_no_overlap_test() ->
    A = beamtalk_set_ops:from_list([1, 2]),
    B = beamtalk_set_ops:from_list([3, 4]),
    ?assertEqual(A, beamtalk_set_ops:difference(A, B)).

%%% ============================================================================
%%% Predicate Tests
%%% ============================================================================

is_subset_of_true_test() ->
    A = beamtalk_set_ops:from_list([1, 2]),
    B = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(true, beamtalk_set_ops:is_subset_of(A, B)).

is_subset_of_false_test() ->
    A = beamtalk_set_ops:from_list([1, 2, 4]),
    B = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(false, beamtalk_set_ops:is_subset_of(A, B)).

is_subset_of_equal_test() ->
    A = beamtalk_set_ops:from_list([1, 2, 3]),
    B = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(true, beamtalk_set_ops:is_subset_of(A, B)).

is_subset_of_empty_test() ->
    A = beamtalk_set_ops:new(),
    B = beamtalk_set_ops:from_list([1, 2]),
    ?assertEqual(true, beamtalk_set_ops:is_subset_of(A, B)).

%%% ============================================================================
%%% Conversion Tests
%%% ============================================================================

as_list_test() ->
    Set = beamtalk_set_ops:from_list([3, 1, 2]),
    ?assertEqual([1, 2, 3], beamtalk_set_ops:as_list(Set)).

as_list_empty_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual([], beamtalk_set_ops:as_list(Set)).

%%% ============================================================================
%%% Iteration Tests
%%% ============================================================================

do_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Ref = make_ref(),
    put(Ref, []),
    beamtalk_set_ops:do(Set, fun(E) -> put(Ref, [E | get(Ref)]) end),
    ?assertEqual([3, 2, 1], get(Ref)).

do_returns_nil_test() ->
    Set = beamtalk_set_ops:from_list([1]),
    ?assertEqual(nil, beamtalk_set_ops:do(Set, fun(_) -> ok end)).
