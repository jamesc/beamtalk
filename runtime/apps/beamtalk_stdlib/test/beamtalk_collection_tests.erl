%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_collection module (BT-1088).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests inject_into/3 and to_list/1 for list inputs.

-module(beamtalk_collection_tests).

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
