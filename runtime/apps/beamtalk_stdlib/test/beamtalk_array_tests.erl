%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_array_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_array module (BT-1088).

Tests from_list, size, is_empty, at, at_put, do, includes,
collect, select, inject_into, print_string, and error paths.
""".

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

%% BT-2362: at_put must return an Array whose internal representation is
%% structurally identical (=:=) to a literal-equivalent Array, so that
%% `#[1,2,3] at: 2 put: 99` compares equal to `#[1,99,3]`. array:set/3 alone
%% leaves a stale copy-on-write cache node and breaks this invariant.
at_put_equals_literal_equivalent_test() ->
    Updated = beamtalk_array:at_put(make_array([1, 2, 3]), 2, 99),
    Literal = make_array([1, 99, 3]),
    ?assert(Updated =:= Literal),
    ?assertEqual(Literal, Updated).

%% BT-2362: equality must be reflected by hashing — two arrays that compare
%% equal must produce the same phash2, otherwise dictionary/set keys break.
at_put_hash_matches_literal_equivalent_test() ->
    Updated = beamtalk_array:at_put(make_array([1, 2, 3]), 2, 99),
    Literal = make_array([1, 99, 3]),
    ?assertEqual(erlang:phash2(Literal), erlang:phash2(Updated)).

%% BT-2362: chained at_put calls must also stay canonical.
at_put_chained_equals_literal_equivalent_test() ->
    Updated = beamtalk_array:at_put(
        beamtalk_array:at_put(make_array([1, 2, 3]), 1, 7), 3, 9
    ),
    Literal = make_array([7, 2, 9]),
    ?assert(Updated =:= Literal).

%%% ============================================================================
%%% Canonicality (ADR 0090 / BT-2682)
%%%
%%% Every construction path yielding the same element sequence must produce
%%% terms that are mutually =:= and erlang:phash2-equal, regardless of edit
%%% history. This is the load-bearing invariant of the map-backed representation.
%%% ============================================================================

%% from_list vs repeated at_put reaching the same sequence.
canonical_from_list_vs_atput_test() ->
    Literal = make_array([1, 2, 3, 4, 5]),
    Built = lists:foldl(
        fun({I, V}, Acc) -> beamtalk_array:at_put(Acc, I, V) end,
        make_array([0, 0, 0, 0, 0]),
        [{1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}]
    ),
    ?assert(Built =:= Literal),
    ?assertEqual(erlang:phash2(Literal), erlang:phash2(Built)).

%% at_put applied in a different order still canonicalises to the literal.
canonical_atput_order_independent_test() ->
    Literal = make_array([10, 20, 30]),
    Forward = beamtalk_array:at_put(
        beamtalk_array:at_put(beamtalk_array:at_put(make_array([0, 0, 0]), 1, 10), 2, 20), 3, 30
    ),
    Backward = beamtalk_array:at_put(
        beamtalk_array:at_put(beamtalk_array:at_put(make_array([0, 0, 0]), 3, 30), 2, 20), 1, 10
    ),
    ?assert(Forward =:= Literal),
    ?assert(Backward =:= Literal),
    ?assert(Forward =:= Backward),
    ?assertEqual(erlang:phash2(Forward), erlang:phash2(Backward)).

%% collect/2 result is canonical against a from_list of the mapped sequence.
canonical_collect_test() ->
    Mapped = beamtalk_array:collect(make_array([1, 2, 3]), fun(E) -> E * 10 end),
    Literal = make_array([10, 20, 30]),
    ?assert(Mapped =:= Literal),
    ?assertEqual(erlang:phash2(Literal), erlang:phash2(Mapped)).

%% select/2 re-indexes kept elements to 0..M-1, so it is canonical too.
canonical_select_test() ->
    Selected = beamtalk_array:select(make_array([1, 2, 3, 4, 5]), fun(E) -> E rem 2 =:= 0 end),
    Literal = make_array([2, 4]),
    ?assert(Selected =:= Literal),
    ?assertEqual(erlang:phash2(Literal), erlang:phash2(Selected)).

%% slice_from/2 result is canonical against a from_list of the tail.
canonical_slice_from_test() ->
    Sliced = beamtalk_array:slice_from(make_array([9, 8, 7, 6, 5]), 3),
    Literal = make_array([7, 6, 5]),
    ?assert(Sliced =:= Literal),
    ?assertEqual(erlang:phash2(Literal), erlang:phash2(Sliced)).

%% Property: many random construction paths to the same sequence collapse to one
%% canonical term. Seeded for determinism.
canonical_random_paths_property_test() ->
    rand:seed(exsss, {2680, 2681, 2682}),
    N = 40,
    Target = [rand:uniform(100) || _ <- lists:seq(1, N)],
    Literal = make_array(Target),
    %% Build the same sequence via 50 independent random at_put orderings,
    %% each starting from a different scrambled base array.
    Paths = [build_via_random_path(Target) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun(A) -> A =:= Literal end, Paths)),
    Hashes = [erlang:phash2(A) || A <- [Literal | Paths]],
    ?assertEqual(1, length(lists:usort(Hashes))).

%% Helper: reach Target by starting from a scrambled array and applying at_put
%% for every index in a random order.
build_via_random_path(Target) ->
    N = length(Target),
    Base = make_array([rand:uniform(1000) || _ <- lists:seq(1, N)]),
    Order = shuffle(lists:seq(1, N)),
    lists:foldl(
        fun(I, Acc) -> beamtalk_array:at_put(Acc, I, lists:nth(I, Target)) end,
        Base,
        Order
    ).

shuffle(List) ->
    [E || {_, E} <- lists:sort([{rand:uniform(), X} || X <- List])].

%% Arrays from different construction paths work as identical Dictionary keys
%% because the underlying terms are =:= (the whole point of ADR 0090).
canonical_as_map_key_test() ->
    KeyA = beamtalk_array:at_put(make_array([1, 2]), 2, 9),
    KeyLit = make_array([1, 9]),
    D = #{KeyA => "hit"},
    ?assertEqual("hit", maps:get(KeyLit, D, miss)).

%%% ============================================================================
%%% Performance guard (ADR 0090 / BT-2682)
%%%
%%% Repeated at_put over a large array must be O(log n) per update, not the
%%% O(n) re-canonicalisation that made the interim path O(n^2) (5000 updates on
%%% a 200k array took ~97s). Asserts bounded completion, not a microbenchmark.
%%% ============================================================================

at_put_loop_is_not_quadratic_test_() ->
    %% Generous timeout so the guard is a hard ceiling, not a flaky benchmark.
    {timeout, 30, fun() ->
        N = 200000,
        Updates = 5000,
        Big = make_array(lists:seq(1, N)),
        {Micros, Result} = timer:tc(fun() ->
            lists:foldl(
                fun(K, Acc) -> beamtalk_array:at_put(Acc, (K rem N) + 1, 0) end,
                Big,
                lists:seq(1, Updates)
            )
        end),
        ?assertEqual(N, beamtalk_array:size(Result)),
        %% O(n^2) interim path was ~10^8 us here; O(log n) is ~10^3 us. A 5s
        %% ceiling cleanly separates the two without flaking on a busy CI box.
        ?assert(Micros < 5000000)
    end}.

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
