%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_collection_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_collection module (BT-1088).

Tests inject_into/3 and to_list/1 for list inputs.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% inject_into/3
%%% ============================================================================

inject_into_sum_test() ->
    ?assertEqual(
        10, beamtalk_collection:inject_into([1, 2, 3, 4], 0, fun(Acc, E) -> Acc + E end)
    ).

inject_into_empty_list_test() ->
    ?assertEqual(99, beamtalk_collection:inject_into([], 99, fun(Acc, _E) -> Acc end)).

inject_into_build_list_test() ->
    Result = beamtalk_collection:inject_into([1, 2, 3], [], fun(Acc, E) -> Acc ++ [E] end),
    ?assertEqual([1, 2, 3], Result).

inject_into_accumulator_order_test() ->
    %% Block receives (Acc, Elem) — accumulator first, element second
    Result = beamtalk_collection:inject_into([a, b, c], [], fun(Acc, E) -> [E | Acc] end),
    %% foldl processes left to right, so reversed result
    ?assertEqual([c, b, a], Result).

inject_into_single_element_test() ->
    ?assertEqual(42, beamtalk_collection:inject_into([42], 0, fun(_Acc, E) -> E end)).

%%% ============================================================================
%%% to_list/1 — list input (trivial path)
%%% ============================================================================

to_list_already_list_test() ->
    List = [1, 2, 3],
    ?assertEqual(List, beamtalk_collection:to_list(List)).

to_list_empty_list_test() ->
    ?assertEqual([], beamtalk_collection:to_list([])).

to_list_single_element_test() ->
    ?assertEqual([x], beamtalk_collection:to_list([x])).

to_list_nested_list_test() ->
    ?assertEqual([[1, 2], [3, 4]], beamtalk_collection:to_list([[1, 2], [3, 4]])).

to_list_mixed_types_test() ->
    ?assertEqual([1, a, <<"str">>], beamtalk_collection:to_list([1, a, <<"str">>])).

%%% ============================================================================
%%% inject_into/3 — additional coverage
%%% ============================================================================

inject_into_string_accumulator_test() ->
    %% Build a string by accumulating elements
    Result = beamtalk_collection:inject_into(
        [<<"a">>, <<"b">>, <<"c">>],
        <<>>,
        fun(Acc, E) -> <<Acc/binary, E/binary>> end
    ),
    ?assertEqual(<<"abc">>, Result).

inject_into_map_accumulator_test() ->
    %% Build a map by accumulating key-value pairs
    Result = beamtalk_collection:inject_into(
        [{a, 1}, {b, 2}],
        #{},
        fun(Acc, {K, V}) -> maps:put(K, V, Acc) end
    ),
    ?assertEqual(#{a => 1, b => 2}, Result).

inject_into_count_test() ->
    %% Count elements
    Result = beamtalk_collection:inject_into([x, y, z], 0, fun(Acc, _E) -> Acc + 1 end),
    ?assertEqual(3, Result).
