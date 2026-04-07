%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_queue_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_queue module (BT-1250).

Tests cover:
- new/0 — empty queue creation
- enqueue/2 — element addition (success, type error)
- dequeue/1 — element removal (success, empty queue error, type error)
- peek/1 — front inspection (success, empty queue error, type error)
- isEmpty/1 — empty check (empty and non-empty, type error)
- size/1 — element count (0, 1, many, type error)
- FIFO ordering — enqueue multiple elements, dequeue in order
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% new/0
%%% ============================================================================

new_returns_queue_map_test() ->
    Q = beamtalk_queue:'new'(),
    ?assertMatch(#{'$beamtalk_class' := 'Queue'}, Q).

new_is_empty_test() ->
    Q = beamtalk_queue:'new'(),
    ?assert(beamtalk_queue:isEmpty(Q)).

new_size_is_zero_test() ->
    Q = beamtalk_queue:'new'(),
    ?assertEqual(0, beamtalk_queue:size(Q)).

%%% ============================================================================
%%% enqueue/2
%%% ============================================================================

enqueue_returns_new_queue_test() ->
    Q = beamtalk_queue:'new'(),
    Q2 = beamtalk_queue:enqueue(Q, hello),
    ?assertMatch(#{'$beamtalk_class' := 'Queue'}, Q2).

enqueue_does_not_mutate_original_test() ->
    Q = beamtalk_queue:'new'(),
    _Q2 = beamtalk_queue:enqueue(Q, hello),
    ?assert(beamtalk_queue:isEmpty(Q)).

enqueue_increases_size_test() ->
    Q0 = beamtalk_queue:'new'(),
    Q1 = beamtalk_queue:enqueue(Q0, a),
    Q2 = beamtalk_queue:enqueue(Q1, b),
    ?assertEqual(2, beamtalk_queue:size(Q2)).

enqueue_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Queue'}},
        beamtalk_queue:enqueue(not_a_queue, hello)
    ).

%%% ============================================================================
%%% dequeue/1
%%% ============================================================================

dequeue_returns_value_and_new_queue_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), hello),
    {Value, Q2} = beamtalk_queue:dequeue(Q),
    ?assertEqual(hello, Value),
    ?assertMatch(#{'$beamtalk_class' := 'Queue'}, Q2).

dequeue_leaves_remaining_elements_test() ->
    Q0 = beamtalk_queue:'new'(),
    Q1 = beamtalk_queue:enqueue(Q0, first),
    Q2 = beamtalk_queue:enqueue(Q1, second),
    {_, Q3} = beamtalk_queue:dequeue(Q2),
    ?assertEqual(1, beamtalk_queue:size(Q3)).

dequeue_empty_queue_error_test() ->
    Q = beamtalk_queue:'new'(),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = empty_queue, class = 'Queue'}},
        beamtalk_queue:dequeue(Q)
    ).

dequeue_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Queue'}},
        beamtalk_queue:dequeue(not_a_queue)
    ).

%%% ============================================================================
%%% peek/1
%%% ============================================================================

peek_returns_front_element_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), 42),
    ?assertEqual(42, beamtalk_queue:peek(Q)).

peek_does_not_remove_element_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), hello),
    _Front = beamtalk_queue:peek(Q),
    ?assertEqual(1, beamtalk_queue:size(Q)).

peek_empty_queue_error_test() ->
    Q = beamtalk_queue:'new'(),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = empty_queue, class = 'Queue'}},
        beamtalk_queue:peek(Q)
    ).

peek_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Queue'}},
        beamtalk_queue:peek(not_a_queue)
    ).

%%% ============================================================================
%%% isEmpty/1
%%% ============================================================================

isEmpty_true_for_new_queue_test() ->
    ?assert(beamtalk_queue:isEmpty(beamtalk_queue:'new'())).

isEmpty_false_after_enqueue_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), x),
    ?assertNot(beamtalk_queue:isEmpty(Q)).

isEmpty_true_after_dequeue_all_test() ->
    Q0 = beamtalk_queue:'new'(),
    Q1 = beamtalk_queue:enqueue(Q0, x),
    {_, Q2} = beamtalk_queue:dequeue(Q1),
    ?assert(beamtalk_queue:isEmpty(Q2)).

isEmpty_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Queue'}},
        beamtalk_queue:isEmpty(not_a_queue)
    ).

%%% ============================================================================
%%% size/1
%%% ============================================================================

size_zero_for_new_queue_test() ->
    ?assertEqual(0, beamtalk_queue:size(beamtalk_queue:'new'())).

size_one_after_one_enqueue_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), a),
    ?assertEqual(1, beamtalk_queue:size(Q)).

size_decreases_after_dequeue_test() ->
    Q0 = beamtalk_queue:'new'(),
    Q1 = beamtalk_queue:enqueue(Q0, a),
    Q2 = beamtalk_queue:enqueue(Q1, b),
    {_, Q3} = beamtalk_queue:dequeue(Q2),
    ?assertEqual(1, beamtalk_queue:size(Q3)).

size_type_error_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error, class = 'Queue'}},
        beamtalk_queue:size(not_a_queue)
    ).

%%% ============================================================================
%%% FIFO ordering
%%% ============================================================================

fifo_order_test() ->
    %% Elements must dequeue in the order they were enqueued
    Q0 = beamtalk_queue:'new'(),
    Q1 = beamtalk_queue:enqueue(Q0, first),
    Q2 = beamtalk_queue:enqueue(Q1, second),
    Q3 = beamtalk_queue:enqueue(Q2, third),
    {V1, Q4} = beamtalk_queue:dequeue(Q3),
    {V2, Q5} = beamtalk_queue:dequeue(Q4),
    {V3, _Q6} = beamtalk_queue:dequeue(Q5),
    ?assertEqual(first, V1),
    ?assertEqual(second, V2),
    ?assertEqual(third, V3).

peek_matches_first_dequeue_test() ->
    Q = beamtalk_queue:enqueue(beamtalk_queue:'new'(), peek_me),
    Front = beamtalk_queue:peek(Q),
    {Value, _} = beamtalk_queue:dequeue(Q),
    ?assertEqual(Front, Value).
