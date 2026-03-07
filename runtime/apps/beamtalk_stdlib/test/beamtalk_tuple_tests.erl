%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_tuple module (BT-1173).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests at, as_string, do, and error paths.

-module(beamtalk_tuple_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% at/2
%%% ============================================================================

at_first_element_test() ->
    ?assertEqual(a, beamtalk_tuple:at({a, b, c}, 1)).

at_last_element_test() ->
    ?assertEqual(c, beamtalk_tuple:at({a, b, c}, 3)).

at_middle_element_test() ->
    ?assertEqual(b, beamtalk_tuple:at({a, b, c}, 2)).

at_single_element_test() ->
    ?assertEqual(42, beamtalk_tuple:at({42}, 1)).

at_out_of_bounds_high_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_tuple:at({a, b}, 3)
    ).

at_out_of_bounds_zero_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_tuple:at({a, b}, 0)
    ).

at_out_of_bounds_negative_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_tuple:at({a, b}, -1)
    ).

at_type_error_non_integer_index_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_tuple:at({a, b}, <<"1">>)
    ).

at_type_error_atom_index_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_tuple:at({a, b}, first)
    ).

%%% ============================================================================
%%% as_string/1
%%% ============================================================================

as_string_empty_tuple_test() ->
    ?assertEqual(<<"{}">>, beamtalk_tuple:as_string({})).

as_string_single_element_test() ->
    Result = beamtalk_tuple:as_string({42}),
    ?assertEqual(<<"{42}">>, Result).

as_string_two_elements_test() ->
    Result = beamtalk_tuple:as_string({1, 2}),
    ?assertEqual(<<"{1, 2}">>, Result).

as_string_three_elements_test() ->
    Result = beamtalk_tuple:as_string({a, b, c}),
    ?assertMatch(<<"{", _/binary>>, Result),
    ?assert(binary:match(Result, <<"a">>) =/= nomatch).

as_string_binary_elements_test() ->
    Result = beamtalk_tuple:as_string({<<"hello">>, <<"world">>}),
    ?assertMatch(<<"{", _/binary>>, Result).

%%% ============================================================================
%%% do/2
%%% ============================================================================

do_iterates_all_elements_test() ->
    Ref = make_ref(),
    put(Ref, []),
    beamtalk_tuple:do({1, 2, 3}, fun(E) -> put(Ref, [E | get(Ref)]) end),
    Result = lists:sort(get(Ref)),
    erase(Ref),
    ?assertEqual([1, 2, 3], Result).

do_empty_tuple_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_tuple:do({}, fun(_E) -> error(should_not_be_called) end)).

do_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_tuple:do({1}, fun(_E) -> ok end)).

do_single_element_test() ->
    Ref = make_ref(),
    put(Ref, []),
    beamtalk_tuple:do({hello}, fun(E) -> put(Ref, [E | get(Ref)]) end),
    Result = get(Ref),
    erase(Ref),
    ?assertEqual([hello], Result).
