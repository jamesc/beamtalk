%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_list_tests).

-moduledoc """
EUnit tests for beamtalk_list (BT-708).

Tests list operations: at, detect, detect_if_none, do, reject,
zip, group_by, partition, intersperse, take, drop, sort_with,
from_to — including error cases.

Note: index_of/2 and each_with_index/2 were removed in BT-816
(self-hosted in pure Beamtalk in list.bt).
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% at/2 tests
%%% ============================================================================

at_valid_index_test() ->
    ?assertEqual(b, beamtalk_list:at([a, b, c], 2)).

at_first_test() ->
    ?assertEqual(1, beamtalk_list:at([1, 2, 3], 1)).

at_last_test() ->
    ?assertEqual(3, beamtalk_list:at([1, 2, 3], 3)).

at_out_of_bounds_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list:at([1, 2, 3], 10)
    ).

at_zero_index_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list:at([1, 2, 3], 0)
    ).

at_negative_index_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list:at([1, 2, 3], -1)
    ).

%% BT-3021: indexing an *empty* List is `empty_collection`, a distinct
%% condition from an out-of-range index into a populated one.
at_empty_list_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = empty_collection,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list:at([], 1)
    ).

%% An index below 1 is malformed regardless of emptiness, so the out-of-bounds
%% clause is checked before the emptiness clause.
at_empty_list_zero_index_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'at:'
            }
        },
        beamtalk_list:at([], 0)
    ).

%% A non-integer index stays a type_error even on an empty List.
at_empty_list_non_integer_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'TypeError',
            error := #beamtalk_error{kind = type_error, class = 'List', selector = 'at:'}
        },
        beamtalk_list:at([], foo)
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
        beamtalk_list:at([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% detect/2 tests
%%% ============================================================================

detect_found_test() ->
    ?assertEqual(
        4,
        beamtalk_list:detect(
            [1, 2, 3, 4, 5],
            fun(X) -> X > 3 end
        )
    ).

detect_first_match_test() ->
    ?assertEqual(
        2,
        beamtalk_list:detect(
            [1, 2, 3, 4],
            fun(X) -> X rem 2 =:= 0 end
        )
    ).

%% BT-3025: no element matched is `not_found`, not `does_not_understand` —
%% the List understands `detect:`, the search just came up empty.
detect_not_found_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = not_found,
                class = 'List',
                selector = 'detect:'
            }
        },
        beamtalk_list:detect(
            [1, 2, 3],
            fun(X) -> X > 10 end
        )
    ).

%% The message names the condition rather than a missing selector.
detect_not_found_message_test() ->
    Message =
        try
            beamtalk_list:detect([1, 2, 3], fun(X) -> X > 10 end)
        catch
            error:#{error := #beamtalk_error{message = M}} -> M
        end,
    ?assertNotEqual(nomatch, binary:match(Message, <<"not found">>)),
    ?assertEqual(nomatch, binary:match(Message, <<"does not understand">>)).

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
        beamtalk_list:detect([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% detect_if_none/3 tests
%%% ============================================================================

detect_if_none_found_test() ->
    ?assertEqual(
        4,
        beamtalk_list:detect_if_none(
            [1, 2, 3, 4, 5],
            fun(X) -> X > 3 end,
            default
        )
    ).

detect_if_none_not_found_value_test() ->
    ?assertEqual(
        default,
        beamtalk_list:detect_if_none(
            [1, 2, 3],
            fun(X) -> X > 10 end,
            default
        )
    ).

detect_if_none_not_found_block_test() ->
    ?assertEqual(
        42,
        beamtalk_list:detect_if_none(
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
        beamtalk_list:detect_if_none(
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
    beamtalk_list:do([1, 2, 3], fun(X) -> Self ! {Ref, X} end),
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
    ?assertEqual(nil, beamtalk_list:do([1, 2], fun(_) -> ok end)).

do_empty_list_test() ->
    ?assertEqual(nil, beamtalk_list:do([], fun(_) -> ok end)).

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
        beamtalk_list:do([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% reject/2 tests
%%% ============================================================================

reject_test() ->
    ?assertEqual(
        [1, 3, 5],
        beamtalk_list:reject(
            [1, 2, 3, 4, 5],
            fun(X) -> X rem 2 =:= 0 end
        )
    ).

reject_none_test() ->
    ?assertEqual(
        [1, 2, 3],
        beamtalk_list:reject(
            [1, 2, 3],
            fun(X) -> X > 10 end
        )
    ).

reject_all_test() ->
    ?assertEqual(
        [],
        beamtalk_list:reject(
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
        beamtalk_list:reject([1, 2, 3], not_a_function)
    ).

%%% ============================================================================
%%% take/2 tests
%%% ============================================================================

take_test() ->
    ?assertEqual([1, 2], beamtalk_list:take([1, 2, 3, 4], 2)).

take_zero_test() ->
    ?assertEqual([], beamtalk_list:take([1, 2, 3], 0)).

take_more_than_length_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list:take([1, 2, 3], 10)).

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
        beamtalk_list:take([1, 2, 3], -1)
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
        beamtalk_list:take([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% drop/2 tests
%%% ============================================================================

drop_test() ->
    ?assertEqual([3, 4], beamtalk_list:drop([1, 2, 3, 4], 2)).

drop_zero_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list:drop([1, 2, 3], 0)).

drop_more_than_length_test() ->
    ?assertEqual([], beamtalk_list:drop([1, 2, 3], 10)).

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
        beamtalk_list:drop([1, 2, 3], -1)
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
        beamtalk_list:drop([1, 2, 3], foo)
    ).

%%% ============================================================================
%%% sort_with/2 tests
%%% ============================================================================

sort_with_ascending_test() ->
    ?assertEqual(
        [1, 2, 3],
        beamtalk_list:sort_with(
            [3, 1, 2],
            fun(A, B) -> A =< B end
        )
    ).

sort_with_descending_test() ->
    ?assertEqual(
        [3, 2, 1],
        beamtalk_list:sort_with(
            [1, 3, 2],
            fun(A, B) -> A >= B end
        )
    ).

sort_with_empty_test() ->
    ?assertEqual([], beamtalk_list:sort_with([], fun(A, B) -> A =< B end)).

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
        beamtalk_list:sort_with([1, 2], not_a_function)
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
        beamtalk_list:sort_with(not_a_list, fun(A, B) -> A >= B end)
    ).

%%% ============================================================================
%%% zip/2 tests
%%% ============================================================================

zip_equal_test() ->
    Result = beamtalk_list:zip([a, b], [1, 2]),
    ?assertEqual([[a, 1], [b, 2]], Result).

zip_unequal_test() ->
    Result = beamtalk_list:zip([a, b, c], [1, 2]),
    ?assertEqual([[a, 1], [b, 2]], Result).

zip_empty_test() ->
    ?assertEqual([], beamtalk_list:zip([], [1, 2])).

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
        beamtalk_list:zip([1, 2], not_a_list)
    ).

%%% ============================================================================
%%% group_by/2 tests
%%% ============================================================================

group_by_test() ->
    Result = beamtalk_list:group_by(
        [1, 2, 3, 4, 5],
        fun(X) -> X rem 2 end
    ),
    ?assertEqual([1, 3, 5], maps:get(1, Result)),
    ?assertEqual([2, 4], maps:get(0, Result)).

group_by_empty_test() ->
    ?assertEqual(#{}, beamtalk_list:group_by([], fun(X) -> X end)).

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
        beamtalk_list:group_by([1, 2], not_a_function)
    ).

%%% ============================================================================
%%% partition/2 tests
%%% ============================================================================

partition_test() ->
    Result = beamtalk_list:partition(
        [1, 2, 3, 4, 5],
        fun(X) -> X > 3 end
    ),
    ?assertEqual([4, 5], maps:get(<<"matching">>, Result)),
    ?assertEqual([1, 2, 3], maps:get(<<"nonMatching">>, Result)).

partition_none_match_test() ->
    Result = beamtalk_list:partition(
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
        beamtalk_list:partition([1, 2], not_a_function)
    ).

%%% ============================================================================
%%% intersperse/2 tests
%%% ============================================================================

intersperse_test() ->
    ?assertEqual(
        [1, 0, 2, 0, 3],
        beamtalk_list:intersperse([1, 2, 3], 0)
    ).

intersperse_single_test() ->
    ?assertEqual([1], beamtalk_list:intersperse([1], 0)).

intersperse_empty_test() ->
    ?assertEqual([], beamtalk_list:intersperse([], 0)).

%%% ============================================================================
%%% from_to/3 tests
%%% ============================================================================

from_to_test() ->
    ?assertEqual(
        [2, 3, 4],
        beamtalk_list:from_to([1, 2, 3, 4, 5], 2, 4)
    ).

from_to_single_test() ->
    ?assertEqual(
        [3],
        beamtalk_list:from_to([1, 2, 3, 4, 5], 3, 3)
    ).

from_to_end_before_start_test() ->
    ?assertEqual(
        [],
        beamtalk_list:from_to([1, 2, 3], 3, 1)
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
        beamtalk_list:from_to([1, 2, 3], foo, 2)
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
        beamtalk_list:from_to([1, 2, 3], 1, foo)
    ).

%% BT-3025: a sub-1 start index is `index_out_of_bounds`, matching `at:` —
%% the index is malformed, the selector is not.
from_to_negative_start_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'from:to:'
            }
        },
        beamtalk_list:from_to([1, 2, 3], -1, 2)
    ).

from_to_zero_start_test() ->
    ?assertException(
        error,
        #{
            '$beamtalk_class' := 'RuntimeError',
            error := #beamtalk_error{
                kind = index_out_of_bounds,
                class = 'List',
                selector = 'from:to:'
            }
        },
        beamtalk_list:from_to([1, 2, 3], 0, 2)
    ).

%%% ============================================================================
%%% Strict (`=:=`) element identity — BT-2997
%%% ============================================================================

unique_strict_keeps_int_and_float_test() ->
    %% `lists:usort/1` would collapse these to [1].
    ?assertEqual([1, 1.0], beamtalk_list:unique([1, 1.0, 1, 1.0])).

unique_removes_strict_duplicates_test() ->
    ?assertEqual([1, 2, 3], beamtalk_list:unique([3, 1, 2, 1, 3])).

unique_empty_and_singleton_test() ->
    ?assertEqual([], beamtalk_list:unique([])),
    ?assertEqual([a], beamtalk_list:unique([a])).

unique_output_is_sorted_test() ->
    ?assertEqual([1, 2, 3, 4], beamtalk_list:unique([4, 3, 2, 1])).

unique_non_numeric_terms_test() ->
    ?assertEqual([a, b, c], beamtalk_list:unique([c, b, a, b])),
    ?assertEqual([<<"a">>, <<"b">>], beamtalk_list:unique([<<"b">>, <<"a">>, <<"b">>])).

strict_member_distinguishes_int_from_float_test() ->
    ?assert(beamtalk_list:strict_member_sorted(1, [1])),
    ?assertNot(beamtalk_list:strict_member_sorted(1.0, [1])),
    ?assertNot(beamtalk_list:strict_member_sorted(1, [1.0])),
    %% Both present: each is found on its own terms.
    ?assert(beamtalk_list:strict_member_sorted(1, [1, 1.0])),
    ?assert(beamtalk_list:strict_member_sorted(1.0, [1, 1.0])).

strict_member_absent_and_empty_test() ->
    ?assertNot(beamtalk_list:strict_member_sorted(9, [1, 2, 3])),
    ?assertNot(beamtalk_list:strict_member_sorted(1, [])),
    %% Stops early once the sorted list passes the element.
    ?assertNot(beamtalk_list:strict_member_sorted(0, [1, 2, 3])).

sorted_strict_unique_preserves_equal_run_test() ->
    %% A run of mutually-`==` terms keeps one of each distinct value.
    ?assertEqual([1, 1.0], beamtalk_list:sorted_strict_unique([1, 1.0, 1.0, 1])).
