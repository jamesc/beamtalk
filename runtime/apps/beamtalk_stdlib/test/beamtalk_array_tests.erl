%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_array module (BT-1088).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests from_list, size, is_empty, at, at_put, do, includes,
%%% collect, select, inject_into, print_string, and error paths.

-module(beamtalk_array_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Helpers
%%% ============================================================================

make_array(List) ->
    beamtalk_array:from_list(List).

%%% ============================================================================
%%% from_list/1
%%% ============================================================================

from_list_basic_test() ->
    A = make_array([1, 2, 3]),
    ?assertEqual('Array', maps:get('$beamtalk_class', A)),
    ?assert(maps:is_key(data, A)).

from_list_empty_test() ->
    A = make_array([]),
    ?assertEqual(0, beamtalk_array:size(A)).

from_list_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:from_list(not_a_list)
    ).

%%% ============================================================================
%%% size/1 and is_empty/1
%%% ============================================================================

size_test() ->
    ?assertEqual(3, beamtalk_array:size(make_array([a, b, c]))).

size_empty_test() ->
    ?assertEqual(0, beamtalk_array:size(make_array([]))).

is_empty_true_test() ->
    ?assert(beamtalk_array:is_empty(make_array([]))).

is_empty_false_test() ->
    ?assertNot(beamtalk_array:is_empty(make_array([1]))).

%%% ============================================================================
%%% at/2
%%% ============================================================================

at_first_test() ->
    A = make_array([a, b, c]),
    ?assertEqual(a, beamtalk_array:at(A, 1)).

at_last_test() ->
    A = make_array([a, b, c]),
    ?assertEqual(c, beamtalk_array:at(A, 3)).

at_middle_test() ->
    A = make_array([10, 20, 30]),
    ?assertEqual(20, beamtalk_array:at(A, 2)).

at_out_of_bounds_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:at(A, 4)
    ).

at_zero_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:at(A, 0)
    ).

at_non_integer_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:at(A, <<"1">>)
    ).

%%% ============================================================================
%%% at_put/3
%%% ============================================================================

at_put_basic_test() ->
    A = make_array([1, 2, 3]),
    A2 = beamtalk_array:at_put(A, 2, 99),
    ?assertEqual(99, beamtalk_array:at(A2, 2)),
    %% Original unchanged
    ?assertEqual(2, beamtalk_array:at(A, 2)).

at_put_first_test() ->
    A = make_array([1, 2, 3]),
    A2 = beamtalk_array:at_put(A, 1, 100),
    ?assertEqual(100, beamtalk_array:at(A2, 1)).

at_put_out_of_bounds_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:at_put(A, 5, 99)
    ).

at_put_zero_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:at_put(A, 0, 99)
    ).

at_put_non_integer_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:at_put(A, <<"1">>, 99)
    ).

%%% ============================================================================
%%% do/2
%%% ============================================================================

do_applies_to_each_test() ->
    A = make_array([1, 2, 3]),
    Ref = make_ref(),
    put(Ref, []),
    beamtalk_array:do(A, fun(E) -> put(Ref, [E | get(Ref)]) end),
    Result = lists:sort(get(Ref)),
    erase(Ref),
    ?assertEqual([1, 2, 3], Result).

do_empty_array_test() ->
    A = make_array([]),
    Result = beamtalk_array:do(A, fun(_E) -> error(should_not_be_called) end),
    ?assertEqual(nil, Result).

do_returns_nil_test() ->
    A = make_array([1]),
    ?assertEqual(nil, beamtalk_array:do(A, fun(_E) -> ok end)).

do_type_error_test() ->
    A = make_array([1]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:do(A, not_a_function)
    ).

%%% ============================================================================
%%% includes/2
%%% ============================================================================

includes_true_test() ->
    A = make_array([1, 2, 3]),
    ?assert(beamtalk_array:includes(A, 2)).

includes_false_test() ->
    A = make_array([1, 2, 3]),
    ?assertNot(beamtalk_array:includes(A, 99)).

includes_empty_test() ->
    A = make_array([]),
    ?assertNot(beamtalk_array:includes(A, 1)).

%%% ============================================================================
%%% collect/2
%%% ============================================================================

collect_basic_test() ->
    A = make_array([1, 2, 3]),
    A2 = beamtalk_array:collect(A, fun(E) -> E * 2 end),
    ?assertEqual(2, beamtalk_array:at(A2, 1)),
    ?assertEqual(4, beamtalk_array:at(A2, 2)),
    ?assertEqual(6, beamtalk_array:at(A2, 3)).

collect_size_preserved_test() ->
    A = make_array([a, b, c, d]),
    A2 = beamtalk_array:collect(A, fun(E) -> E end),
    ?assertEqual(4, beamtalk_array:size(A2)).

collect_type_error_test() ->
    A = make_array([1]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:collect(A, not_a_function)
    ).

%%% ============================================================================
%%% select/2
%%% ============================================================================

select_basic_test() ->
    A = make_array([1, 2, 3, 4, 5]),
    A2 = beamtalk_array:select(A, fun(E) -> E rem 2 =:= 0 end),
    ?assertEqual(2, beamtalk_array:size(A2)),
    ?assertEqual(2, beamtalk_array:at(A2, 1)),
    ?assertEqual(4, beamtalk_array:at(A2, 2)).

select_all_match_test() ->
    A = make_array([2, 4, 6]),
    A2 = beamtalk_array:select(A, fun(E) -> E rem 2 =:= 0 end),
    ?assertEqual(3, beamtalk_array:size(A2)).

select_none_match_test() ->
    A = make_array([1, 3, 5]),
    A2 = beamtalk_array:select(A, fun(E) -> E rem 2 =:= 0 end),
    ?assertEqual(0, beamtalk_array:size(A2)).

select_type_error_test() ->
    A = make_array([1]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:select(A, not_a_function)
    ).

%%% ============================================================================
%%% inject_into/3
%%% ============================================================================

inject_into_sum_test() ->
    A = make_array([1, 2, 3, 4]),
    Sum = beamtalk_array:inject_into(A, 0, fun(Acc, E) -> Acc + E end),
    ?assertEqual(10, Sum).

inject_into_empty_test() ->
    A = make_array([]),
    Result = beamtalk_array:inject_into(A, 42, fun(Acc, _E) -> Acc end),
    ?assertEqual(42, Result).

inject_into_build_list_test() ->
    A = make_array([1, 2, 3]),
    Result = beamtalk_array:inject_into(A, [], fun(Acc, E) -> Acc ++ [E] end),
    ?assertEqual([1, 2, 3], Result).

inject_into_type_error_test() ->
    A = make_array([1]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:inject_into(A, 0, not_a_function)
    ).

%%% ============================================================================
%%% print_string/1
%%% ============================================================================

print_string_empty_test() ->
    A = make_array([]),
    ?assertEqual(<<"#[]">>, beamtalk_array:print_string(A)).

print_string_integers_test() ->
    A = make_array([1, 2, 3]),
    Result = beamtalk_array:print_string(A),
    ?assertMatch(<<"#[", _/binary>>, Result),
    ?assert(binary:match(Result, <<"1">>) =/= nomatch).

%%% ============================================================================
%%% Slicing
%%% ============================================================================

slice_from_middle_test() ->
    A = make_array([10, 20, 30, 40, 50]),
    Result = beamtalk_array:slice_from(A, 3),
    ?assertEqual(3, beamtalk_array:size(Result)),
    ?assertEqual(30, beamtalk_array:at(Result, 1)),
    ?assertEqual(40, beamtalk_array:at(Result, 2)),
    ?assertEqual(50, beamtalk_array:at(Result, 3)).

slice_from_first_test() ->
    A = make_array([1, 2, 3]),
    Result = beamtalk_array:slice_from(A, 1),
    ?assertEqual(3, beamtalk_array:size(Result)),
    ?assertEqual(1, beamtalk_array:at(Result, 1)).

slice_from_last_test() ->
    A = make_array([1, 2, 3]),
    Result = beamtalk_array:slice_from(A, 3),
    ?assertEqual(1, beamtalk_array:size(Result)),
    ?assertEqual(3, beamtalk_array:at(Result, 1)).

slice_from_past_end_test() ->
    A = make_array([1, 2]),
    Result = beamtalk_array:slice_from(A, 3),
    ?assertEqual(0, beamtalk_array:size(Result)).

slice_from_well_past_end_test() ->
    A = make_array([1, 2]),
    Result = beamtalk_array:slice_from(A, 100),
    ?assertEqual(0, beamtalk_array:size(Result)).

slice_from_empty_array_test() ->
    A = make_array([]),
    Result = beamtalk_array:slice_from(A, 1),
    ?assertEqual(0, beamtalk_array:size(Result)).

slice_from_zero_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:slice_from(A, 0)
    ).

slice_from_negative_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = index_out_of_bounds}},
        beamtalk_array:slice_from(A, -1)
    ).

slice_from_non_integer_index_test() ->
    A = make_array([1, 2, 3]),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_array:slice_from(A, <<"1">>)
    ).
