%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_random module.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests class-side methods (next, nextInteger:, new, seed:),
%%% instance methods (instanceNext, instanceNextInteger:),
%%% collection integration (atRandom), has_method/1, and error paths.

-module(beamtalk_random_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% next/0 — class-side float
%%% ============================================================================

next_returns_float_test() ->
    V = beamtalk_random:'next'(),
    ?assert(is_float(V)).

next_in_range_test() ->
    V = beamtalk_random:'next'(),
    ?assert(V >= 0.0),
    ?assert(V < 1.0).

%%% ============================================================================
%%% nextInteger:/1 — class-side integer
%%% ============================================================================

next_integer_returns_integer_test() ->
    V = beamtalk_random:'nextInteger:'(10),
    ?assert(is_integer(V)).

next_integer_in_range_test() ->
    V = beamtalk_random:'nextInteger:'(100),
    ?assert(V >= 1),
    ?assert(V =< 100).

next_integer_one_test() ->
    ?assertEqual(1, beamtalk_random:'nextInteger:'(1)).

next_integer_zero_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'nextInteger:'(0)
    ).

next_integer_negative_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'nextInteger:'(-5)
    ).

next_integer_non_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'nextInteger:'(<<"10">>)
    ).

%%% ============================================================================
%%% new/0 — constructor
%%% ============================================================================

new_returns_random_map_test() ->
    R = beamtalk_random:'new'(),
    ?assertEqual('Random', maps:get('$beamtalk_class', R)).

new_has_state_test() ->
    R = beamtalk_random:'new'(),
    ?assert(maps:is_key(state, R)).

new_produces_distinct_instances_test() ->
    R1 = beamtalk_random:'new'(),
    R2 = beamtalk_random:'new'(),
    %% Different auto-seeds should produce distinct states
    ?assertNotEqual(maps:get(state, R1), maps:get(state, R2)).

%%% ============================================================================
%%% seed:/1 — seeded constructor
%%% ============================================================================

seed_returns_random_map_test() ->
    R = beamtalk_random:'seed:'(42),
    ?assertEqual('Random', maps:get('$beamtalk_class', R)).

seed_same_seed_same_state_test() ->
    R1 = beamtalk_random:'seed:'(42),
    R2 = beamtalk_random:'seed:'(42),
    ?assertEqual(maps:get(state, R1), maps:get(state, R2)).

seed_different_seeds_different_state_test() ->
    R1 = beamtalk_random:'seed:'(1),
    R2 = beamtalk_random:'seed:'(2),
    ?assertNotEqual(maps:get(state, R1), maps:get(state, R2)).

seed_non_integer_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'seed:'(<<"42">>)
    ).

%%% ============================================================================
%%% instanceNext/1 — instance float
%%% ============================================================================

instance_next_returns_float_test() ->
    R = beamtalk_random:'seed:'(42),
    V = beamtalk_random:'instanceNext'(R),
    ?assert(is_float(V)).

instance_next_in_range_test() ->
    R = beamtalk_random:'seed:'(42),
    V = beamtalk_random:'instanceNext'(R),
    ?assert(V >= 0.0),
    ?assert(V < 1.0).

instance_next_deterministic_test() ->
    R = beamtalk_random:'seed:'(99),
    V1 = beamtalk_random:'instanceNext'(R),
    V2 = beamtalk_random:'instanceNext'(R),
    %% Same instance = same state = same result (value object semantics)
    ?assertEqual(V1, V2).

instance_next_bad_receiver_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'instanceNext'(not_a_random)
    ).

%%% ============================================================================
%%% instanceNextInteger:/2 — instance integer
%%% ============================================================================

instance_next_integer_returns_integer_test() ->
    R = beamtalk_random:'seed:'(42),
    V = beamtalk_random:'instanceNextInteger:'(R, 100),
    ?assert(is_integer(V)).

instance_next_integer_in_range_test() ->
    R = beamtalk_random:'seed:'(42),
    V = beamtalk_random:'instanceNextInteger:'(R, 100),
    ?assert(V >= 1),
    ?assert(V =< 100).

instance_next_integer_deterministic_test() ->
    R = beamtalk_random:'seed:'(7),
    V1 = beamtalk_random:'instanceNextInteger:'(R, 10),
    V2 = beamtalk_random:'instanceNextInteger:'(R, 10),
    ?assertEqual(V1, V2).

instance_next_integer_zero_max_test() ->
    R = beamtalk_random:'seed:'(1),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'instanceNextInteger:'(R, 0)
    ).

instance_next_integer_negative_max_test() ->
    R = beamtalk_random:'seed:'(1),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'instanceNextInteger:'(R, -1)
    ).

instance_next_integer_non_integer_max_test() ->
    R = beamtalk_random:'seed:'(1),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'instanceNextInteger:'(R, 1.5)
    ).

instance_next_integer_bad_receiver_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'instanceNextInteger:'(not_a_random, 10)
    ).

%%% ============================================================================
%%% atRandom/1 — collection integration
%%% ============================================================================

at_random_list_returns_element_test() ->
    List = [a, b, c],
    V = beamtalk_random:'atRandom'(List),
    ?assert(lists:member(V, List)).

at_random_single_element_list_test() ->
    ?assertEqual(only, beamtalk_random:'atRandom'([only])).

at_random_tuple_returns_element_test() ->
    Tuple = {x, y, z},
    V = beamtalk_random:'atRandom'(Tuple),
    ?assert(lists:member(V, tuple_to_list(Tuple))).

at_random_single_element_tuple_test() ->
    ?assertEqual(sole, beamtalk_random:'atRandom'({sole})).

at_random_empty_list_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'atRandom'([])
    ).

at_random_empty_tuple_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_random:'atRandom'({})
    ).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_known_test() ->
    ?assert(beamtalk_random:has_method('next')),
    ?assert(beamtalk_random:has_method('nextInteger:')),
    ?assert(beamtalk_random:has_method('new')),
    ?assert(beamtalk_random:has_method('seed:')),
    ?assert(beamtalk_random:has_method('instanceNext')),
    ?assert(beamtalk_random:has_method('instanceNextInteger:')).

has_method_unknown_test() ->
    ?assertNot(beamtalk_random:has_method('atRandom')),
    ?assertNot(beamtalk_random:has_method(frobnicateWidget)).
