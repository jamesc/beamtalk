%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_set_ops helper module and compiled
%% bt@stdlib@set stdlib dispatch (BT-73).
%%
%% Tests cover:
%% - beamtalk_set_ops: tagged map operations using ordsets
%% - 'bt@stdlib@set':dispatch/3: compiled stdlib dispatch
%% - 'bt@stdlib@set':has_method/1: method reflection
%% - beamtalk_primitive:send/3: runtime dispatch integration
%% - Error handling: type_error, does_not_understand
-module(beamtalk_set_ops_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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

%%% ============================================================================
%%% Compiled bt@stdlib@set dispatch/3
%%% ============================================================================

set_dispatch_class_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertEqual('Set', 'bt@stdlib@set':dispatch('class', [], Set)).

set_dispatch_class_empty_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual('Set', 'bt@stdlib@set':dispatch('class', [], Set)).

set_dispatch_size_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(3, 'bt@stdlib@set':dispatch('size', [], Set)).

set_dispatch_includes_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(true, 'bt@stdlib@set':dispatch('includes:', [2], Set)),
    ?assertEqual(false, 'bt@stdlib@set':dispatch('includes:', [5], Set)).

set_dispatch_add_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    Result = 'bt@stdlib@set':dispatch('add:', [3], Set),
    ?assertEqual(true, beamtalk_set_ops:includes(Result, 3)).

set_dispatch_remove_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Result = 'bt@stdlib@set':dispatch('remove:', [2], Set),
    ?assertEqual(false, beamtalk_set_ops:includes(Result, 2)).

set_dispatch_union_test() ->
    A = beamtalk_set_ops:from_list([1, 2]),
    B = beamtalk_set_ops:from_list([2, 3]),
    Result = 'bt@stdlib@set':dispatch('union:', [B], A),
    ?assertEqual(3, beamtalk_set_ops:size(Result)).

set_dispatch_asList_test() ->
    Set = beamtalk_set_ops:from_list([3, 1, 2]),
    ?assertEqual([1, 2, 3], 'bt@stdlib@set':dispatch('asList', [], Set)).

%%% ============================================================================
%%% Compiled bt@stdlib@set has_method/1
%%% ============================================================================

set_responds_to_test() ->
    beamtalk_extensions:init(),
    ?assertEqual(true, 'bt@stdlib@set':has_method('class')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('size')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('isEmpty')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('includes:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('add:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('remove:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('union:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('intersection:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('difference:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('isSubsetOf:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('asList')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('fromList:')),
    ?assertEqual(true, 'bt@stdlib@set':has_method('do:')),
    ?assertEqual(false, 'bt@stdlib@set':has_method('nonExistent')).

%%% ============================================================================
%%% Does Not Understand
%%% ============================================================================

set_does_not_understand_test() ->
    beamtalk_extensions:init(),
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'Set'}
        },
        'bt@stdlib@set':dispatch('nonExistent', [], Set)
    ).

%%% ============================================================================
%%% Runtime Dispatch Integration (beamtalk_primitive)
%%% ============================================================================

primitive_class_of_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertEqual('Set', beamtalk_primitive:class_of(Set)).

primitive_send_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    ?assertEqual(3, beamtalk_primitive:send(Set, 'size', [])).

primitive_responds_to_test() ->
    Set = beamtalk_set_ops:from_list([1]),
    ?assertEqual(true, beamtalk_primitive:responds_to(Set, 'size')),
    ?assertEqual(false, beamtalk_primitive:responds_to(Set, 'nonExistent')).

primitive_print_string_test() ->
    Set = beamtalk_set_ops:from_list([1, 2, 3]),
    Result = beamtalk_primitive:print_string(Set),
    ?assertEqual(<<"Set(1, 2, 3)">>, Result).

primitive_print_string_empty_test() ->
    Set = beamtalk_set_ops:new(),
    ?assertEqual(<<"Set()">>, beamtalk_primitive:print_string(Set)).

%%% ============================================================================
%%% Type Error Tests
%%% ============================================================================

from_list_type_error_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'fromList:'}
        },
        beamtalk_set_ops:from_list(42)
    ).

union_type_error_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'union:'}
        },
        beamtalk_set_ops:union(Set, not_a_set)
    ).

intersection_type_error_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'intersection:'}
        },
        beamtalk_set_ops:intersection(Set, 42)
    ).

difference_type_error_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'difference:'}
        },
        beamtalk_set_ops:difference(Set, 42)
    ).

is_subset_of_type_error_test() ->
    Set = beamtalk_set_ops:from_list([1, 2]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'isSubsetOf:'}
        },
        beamtalk_set_ops:is_subset_of(Set, 42)
    ).

do_type_error_test() ->
    Set = beamtalk_set_ops:from_list([1]),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = type_error, class = 'Set', selector = 'do:'}
        },
        beamtalk_set_ops:do(Set, not_a_function)
    ).
