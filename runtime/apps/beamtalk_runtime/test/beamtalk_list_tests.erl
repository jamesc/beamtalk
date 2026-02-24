%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Integration tests for List compiled dispatch and primitive send.
%%
%% Tests compiled dispatch via 'bt@stdlib@list':dispatch/3 (List.bt BIF mappings)
%% and beamtalk_primitive:send/3 integration for the List class.
%%
%% For direct beamtalk_list_ops module tests (at, detect, reject, etc.),
%% see beamtalk_list_ops_tests.erl.
-module(beamtalk_list_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Class name via beamtalk_primitive
%%% ============================================================================

class_of_test() ->
    ?assertEqual('List', beamtalk_primitive:class_of([1, 2, 3])),
    ?assertEqual('List', beamtalk_primitive:class_of([])).

%%% ============================================================================
%%% Compiled dispatch integration tests
%%% Tests via 'bt@stdlib@list':dispatch/3 (compiled from lib/List.bt)
%%% to verify BIF mappings are wired correctly end-to-end.
%%% ============================================================================

dispatch_size_test() ->
    ?assertEqual(3, 'bt@stdlib@list':dispatch('size', [], [1, 2, 3])),
    ?assertEqual(0, 'bt@stdlib@list':dispatch('size', [], [])).

dispatch_is_empty_test() ->
    ?assertEqual(true, 'bt@stdlib@list':dispatch('isEmpty', [], [])),
    ?assertEqual(false, 'bt@stdlib@list':dispatch('isEmpty', [], [1])).

dispatch_first_test() ->
    ?assertEqual(1, 'bt@stdlib@list':dispatch('first', [], [1, 2, 3])).

dispatch_first_empty_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'List', selector = 'first'}
        },
        'bt@stdlib@list':dispatch('first', [], [])
    ).

dispatch_rest_test() ->
    ?assertEqual([2, 3], 'bt@stdlib@list':dispatch('rest', [], [1, 2, 3])),
    ?assertEqual([], 'bt@stdlib@list':dispatch('rest', [], [])).

dispatch_last_test() ->
    ?assertEqual(3, 'bt@stdlib@list':dispatch('last', [], [1, 2, 3])).

dispatch_at_test() ->
    ?assertEqual(2, 'bt@stdlib@list':dispatch('at:', [2], [1, 2, 3])).

dispatch_includes_test() ->
    ?assertEqual(true, 'bt@stdlib@list':dispatch('includes:', [2], [1, 2, 3])),
    ?assertEqual(false, 'bt@stdlib@list':dispatch('includes:', [4], [1, 2, 3])).

dispatch_sort_test() ->
    ?assertEqual([1, 2, 3], 'bt@stdlib@list':dispatch('sort', [], [3, 1, 2])).

dispatch_reversed_test() ->
    ?assertEqual([3, 2, 1], 'bt@stdlib@list':dispatch('reversed', [], [1, 2, 3])).

dispatch_collect_test() ->
    ?assertEqual(
        [2, 4, 6],
        'bt@stdlib@list':dispatch('collect:', [fun(X) -> X * 2 end], [1, 2, 3])
    ).

dispatch_select_test() ->
    ?assertEqual(
        [2, 4],
        'bt@stdlib@list':dispatch('select:', [fun(X) -> X rem 2 =:= 0 end], [1, 2, 3, 4])
    ).

dispatch_inject_into_test() ->
    ?assertEqual(
        6,
        'bt@stdlib@list':dispatch('inject:into:', [0, fun(Acc, X) -> Acc + X end], [1, 2, 3])
    ).

dispatch_add_test() ->
    ?assertEqual([1, 2, 3], 'bt@stdlib@list':dispatch('add:', [3], [1, 2])).

dispatch_flatten_test() ->
    ?assertEqual([1, 2, 3, 4], 'bt@stdlib@list':dispatch('flatten', [], [[1, 2], [3, 4]])).

%%% ============================================================================
%%% beamtalk_primitive:send integration tests
%%% ============================================================================

primitive_send_size_test() ->
    ?assertEqual(3, beamtalk_primitive:send([1, 2, 3], 'size', [])),
    ?assertEqual(0, beamtalk_primitive:send([], 'size', [])).

primitive_send_first_test() ->
    ?assertEqual(1, beamtalk_primitive:send([1, 2, 3], 'first', [])).

primitive_send_reversed_test() ->
    ?assertEqual([3, 2, 1], beamtalk_primitive:send([1, 2, 3], 'reversed', [])).

primitive_send_includes_test() ->
    ?assertEqual(true, beamtalk_primitive:send([1, 2, 3], 'includes:', [2])),
    ?assertEqual(false, beamtalk_primitive:send([1, 2, 3], 'includes:', [4])).

primitive_send_collect_test() ->
    ?assertEqual(
        [2, 4, 6],
        beamtalk_primitive:send([1, 2, 3], 'collect:', [fun(X) -> X * 2 end])
    ).
