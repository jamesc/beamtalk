%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% **DDD Context:** Runtime

%%% @doc EUnit tests for beamtalk_list_ops (BT-708).
%%%
%%% Tests list operations: at, detect, detect_if_none, do, reject,
%%% zip, group_by, partition, intersperse, take, drop, sort_with,
%%% from_to — including error cases.
%%%
%%% Note: index_of/2 and each_with_index/2 were removed in BT-816
%%% (self-hosted in pure Beamtalk in List.bt).

-module(beamtalk_list_ops_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% at/2 tests
%%% ============================================================================

at_valid_index_test() ->
    ?assertEqual(b, beamtalk_list_ops:at([a, b, c], 2)).

at_first_test() ->
    ?assertEqual(1, beamtalk_list_ops:at([1, 2, 3], 1)).

at_last_test() ->
    ?assertEqual(3, beamtalk_list_ops:at([1, 2, 3], 3)).

at_out_of_bounds_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = does_not_understand,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list_ops:at([1, 2, 3], 10)
    ).

at_zero_index_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = does_not_understand,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list_ops:at([1, 2, 3], 0)
    ).

at_negative_index_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = does_not_understand,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list_ops:at([1, 2, 3], -1)
    ).

at_non_integer_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list_ops:at([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% detect/2 tests
%%% ============================================================================

detect_found_test() ->
    ?assertEqual(
        4,
        beamtalk_list_ops:detect(
            [1, 2, 3, 4, 5],
            fun(X) -> X > 3 end
        )
    ).

detect_first_match_test() ->
    ?assertEqual(
        2,
        beamtalk_list_ops:detect(
            [1, 2, 3, 4],
            fun(X) -> X rem 2 =:= 0 end
        )
    ).

detect_not_found_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = does_not_understand,
                class = 'List',
                selector = 'detect:'
            }
        },
        beamtalk_list_ops:detect(
            [1, 2, 3],
            fun(X) -> X > 10 end
        )
    ).

detect_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'detect:'
            }
        },
        beamtalk_list_ops:detect([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% detect_if_none/3 tests
%%% ============================================================================

detect_if_none_found_test() ->
    ?assertEqual(
        4,
        beamtalk_list_ops:detect_if_none(
            [1, 2, 3, 4, 5],
            fun(X) -> X > 3 end,
            default
        )
    ).

detect_if_none_not_found_value_test() ->
    ?assertEqual(
        default,
        beamtalk_list_ops:detect_if_none(
            [1, 2, 3],
            fun(X) -> X > 10 end,
            default
        )
    ).

detect_if_none_not_found_block_test() ->
    ?assertEqual(
        42,
        beamtalk_list_ops:detect_if_none(
            [1, 2, 3],
            fun(X) -> X > 10 end,
            fun() -> 42 end
        )
    ).

detect_if_none_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'detect:ifNone:'
            }
        },
        beamtalk_list_ops:detect_if_none(
            [1, 2, 3],
            not_a_function,
            default
        )
    ).

%%% ============================================================================
%%% do/2 tests
%%% ============================================================================

do_iterates_test() ->
    Ref = make_ref(),
    Self = self(),
    beamtalk_list_ops:do([1, 2, 3], fun(X) -> Self ! {Ref, X} end),
    ?assertEqual(
        {Ref, 1},
        receive
            {Ref, M1} -> {Ref, M1}
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        {Ref, 2},
        receive
            {Ref, M2} -> {Ref, M2}
        after 100 -> timeout
        end
    ),
    ?assertEqual(
        {Ref, 3},
        receive
            {Ref, M3} -> {Ref, M3}
        after 100 -> timeout
        end
    ).

do_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_list_ops:do([1, 2], fun(_) -> ok end)).

do_empty_list_test() ->
    ?assertEqual(nil, beamtalk_list_ops:do([], fun(_) -> ok end)).

do_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'do:'
            }
        },
        beamtalk_list_ops:do([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% reject/2 tests
%%% ============================================================================

reject_test() ->
    ?assertEqual(
        [1, 3, 5],
        beamtalk_list_ops:reject(
            [1, 2, 3, 4, 5],
            fun(X) -> X rem 2 =:= 0 end
        )
    ).

reject_none_test() ->
    ?assertEqual(
        [1, 2, 3],
        beamtalk_list_ops:reject(
            [1, 2, 3],
            fun(X) -> X > 10 end
        )
    ).

reject_all_test() ->
    ?assertEqual(
        [],
        beamtalk_list_ops:reject(
            [1, 2, 3],
            fun(_) -> true end
        )
    ).

reject_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'reject:'
            }
        },
        beamtalk_list_ops:reject([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% take/2 tests
%%% ============================================================================

take_test() ->
    ?assertEqual([1, 2], beamtalk_list_ops:take([1, 2, 3, 4], 2)).

take_zero_test() ->
    ?assertEqual([], beamtalk_list_ops:take([1, 2, 3], 0)).

take_more_than_length_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list_ops:take([1, 2, 3], 10)).

take_negative_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'take:'
            }
        },
        beamtalk_list_ops:take([1, 2, 3], -1)
    ).

take_non_integer_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'take:'
            }
        },
        beamtalk_list_ops:take([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% drop/2 tests
%%% ============================================================================

drop_test() ->
    ?assertEqual([3, 4], beamtalk_list_ops:drop([1, 2, 3, 4], 2)).

drop_zero_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list_ops:drop([1, 2, 3], 0)).

drop_more_than_length_test() ->
    ?assertEqual([], beamtalk_list_ops:drop([1, 2, 3], 10)).

drop_negative_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'drop:'
            }
        },
        beamtalk_list_ops:drop([1, 2, 3], -1)
    ).

drop_non_integer_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'drop:'
            }
        },
        beamtalk_list_ops:drop([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% sort_with/2 tests
%%% ============================================================================

sort_with_ascending_test() ->
    ?assertEqual(
        [1, 2, 3],
        beamtalk_list_ops:sort_with(
            [3, 1, 2],
            fun(A, B) -> A =< B end
        )
    ).

sort_with_descending_test() ->
    ?assertEqual(
        [3, 2, 1],
        beamtalk_list_ops:sort_with(
            [1, 3, 2],
            fun(A, B) -> A >= B end
        )
    ).

sort_with_empty_test() ->
    ?assertEqual([], beamtalk_list_ops:sort_with([], fun(A, B) -> A =< B end)).

sort_with_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'sort:'
            }
        },
        beamtalk_list_ops:sort_with([1, 2], not_a_function)
    ).

sort_with_non_list_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'sort:'
            }
        },
        beamtalk_list_ops:sort_with(not_a_list, fun(A, B) -> A >= B end)
    ).

%%% ============================================================================
%%% zip/2 tests
%%% ============================================================================

zip_equal_test() ->
    Result = beamtalk_list_ops:zip([a, b], [1, 2]),
    ?assertEqual(
        [
            #{<<"key">> => a, <<"value">> => 1},
            #{<<"key">> => b, <<"value">> => 2}
        ],
        Result
    ).

zip_unequal_test() ->
    Result = beamtalk_list_ops:zip([a, b, c], [1, 2]),
    ?assertEqual(
        [
            #{<<"key">> => a, <<"value">> => 1},
            #{<<"key">> => b, <<"value">> => 2}
        ],
        Result
    ).

zip_empty_test() ->
    ?assertEqual([], beamtalk_list_ops:zip([], [1, 2])).

zip_invalid_arg_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'zip:'
            }
        },
        beamtalk_list_ops:zip([1, 2], not_a_list)
    ).

%%% ============================================================================
%%% group_by/2 tests
%%% ============================================================================

group_by_test() ->
    Result = beamtalk_list_ops:group_by(
        [1, 2, 3, 4, 5],
        fun(X) -> X rem 2 end
    ),
    ?assertEqual([1, 3, 5], maps:get(1, Result)),
    ?assertEqual([2, 4], maps:get(0, Result)).

group_by_empty_test() ->
    ?assertEqual(#{}, beamtalk_list_ops:group_by([], fun(X) -> X end)).

group_by_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'groupBy:'
            }
        },
        beamtalk_list_ops:group_by([1, 2], not_a_function)
    ).

%%% ============================================================================
%%% partition/2 tests
%%% ============================================================================

partition_test() ->
    Result = beamtalk_list_ops:partition(
        [1, 2, 3, 4, 5],
        fun(X) -> X > 3 end
    ),
    ?assertEqual([4, 5], maps:get(<<"matching">>, Result)),
    ?assertEqual([1, 2, 3], maps:get(<<"nonMatching">>, Result)).

partition_none_match_test() ->
    Result = beamtalk_list_ops:partition(
        [1, 2, 3],
        fun(X) -> X > 10 end
    ),
    ?assertEqual([], maps:get(<<"matching">>, Result)),
    ?assertEqual([1, 2, 3], maps:get(<<"nonMatching">>, Result)).

partition_invalid_block_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'partition:'
            }
        },
        beamtalk_list_ops:partition([1, 2], not_a_function)
    ).

%%% ============================================================================
%%% intersperse/2 tests
%%% ============================================================================

intersperse_test() ->
    ?assertEqual(
        [1, 0, 2, 0, 3],
        beamtalk_list_ops:intersperse([1, 2, 3], 0)
    ).

intersperse_single_test() ->
    ?assertEqual([1], beamtalk_list_ops:intersperse([1], 0)).

intersperse_empty_test() ->
    ?assertEqual([], beamtalk_list_ops:intersperse([], 0)).

%%% ============================================================================
%%% from_to/3 tests
%%% ============================================================================

from_to_test() ->
    ?assertEqual(
        [2, 3, 4],
        beamtalk_list_ops:from_to([1, 2, 3, 4, 5], 2, 4)
    ).

from_to_single_test() ->
    ?assertEqual(
        [3],
        beamtalk_list_ops:from_to([1, 2, 3, 4, 5], 3, 3)
    ).

from_to_end_before_start_test() ->
    ?assertEqual(
        [],
        beamtalk_list_ops:from_to([1, 2, 3], 3, 1)
    ).

from_to_non_integer_start_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'from:to:'
            }
        },
        beamtalk_list_ops:from_to([1, 2, 3], foo, 2)
    ).

from_to_non_integer_end_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{
                kind = type_error,
                class = 'List',
                selector = 'from:to:'
            }
        },
        beamtalk_list_ops:from_to([1, 2, 3], 1, foo)
    ).

from_to_negative_start_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = does_not_understand,
                class = 'List',
                selector = 'from:to:'
            }
        },
        beamtalk_list_ops:from_to([1, 2, 3], -1, 2)
    ).
