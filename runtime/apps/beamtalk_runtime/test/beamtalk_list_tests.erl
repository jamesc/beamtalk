%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for List runtime helper operations.
%%
%% Tests the beamtalk_list_ops module which provides complex List methods
%% that can't be expressed as simple BIF calls.
%%
%% BT-419: Updated from bt@stdlib@list dispatch tests to beamtalk_list_ops tests.
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
%%% Access Operations (beamtalk_list_ops)
%%% ============================================================================

at_test() ->
    ?assertEqual(1, beamtalk_list_ops:at([1, 2, 3], 1)),
    ?assertEqual(2, beamtalk_list_ops:at([1, 2, 3], 2)),
    ?assertEqual(3, beamtalk_list_ops:at([1, 2, 3], 3)).

at_out_of_bounds_test() ->
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'List'},
        beamtalk_list_ops:at([1, 2, 3], 0)),
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'List'},
        beamtalk_list_ops:at([1, 2, 3], 4)),
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'List'},
        beamtalk_list_ops:at([1, 2, 3], -1)).

at_invalid_type_test() ->
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:at([1, 2, 3], foo)).

%%% ============================================================================
%%% Search Operations
%%% ============================================================================

detect_test() ->
    ?assertEqual(2,
        beamtalk_list_ops:detect([1, 2, 3], fun(X) -> X > 1 end)).

detect_not_found_test() ->
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'List'},
        beamtalk_list_ops:detect([1, 2, 3], fun(X) -> X > 10 end)).

detect_if_none_test() ->
    ?assertEqual(2,
        beamtalk_list_ops:detect_if_none([1, 2, 3], fun(X) -> X > 1 end, 0)),
    ?assertEqual(0,
        beamtalk_list_ops:detect_if_none([1, 2, 3], fun(X) -> X > 10 end, 0)).

%%% ============================================================================
%%% Iteration Operations
%%% ============================================================================

do_test() ->
    ?assertEqual(nil, beamtalk_list_ops:do([1, 2, 3], fun(_) -> ok end)).

reject_test() ->
    ?assertEqual([1, 3],
        beamtalk_list_ops:reject([1, 2, 3, 4], fun(X) -> X rem 2 =:= 0 end)).

%%% ============================================================================
%%% Functional Operations
%%% ============================================================================

take_test() ->
    ?assertEqual([1, 2], beamtalk_list_ops:take([1, 2, 3], 2)),
    ?assertEqual([1, 2, 3], beamtalk_list_ops:take([1, 2, 3], 5)),
    ?assertEqual([], beamtalk_list_ops:take([1, 2, 3], 0)).

take_invalid_test() ->
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:take([1, 2, 3], -1)),
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:take([1, 2, 3], foo)).

drop_test() ->
    ?assertEqual([3], beamtalk_list_ops:drop([1, 2, 3], 2)),
    ?assertEqual([], beamtalk_list_ops:drop([1, 2, 3], 5)),
    ?assertEqual([1, 2, 3], beamtalk_list_ops:drop([1, 2, 3], 0)).

drop_invalid_test() ->
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:drop([1, 2, 3], -1)),
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:drop([1, 2, 3], foo)).

sort_with_test() ->
    ?assertEqual([3, 2, 1],
        beamtalk_list_ops:sort_with([1, 3, 2], fun(A, B) -> A >= B end)).

sort_with_invalid_test() ->
    ?assertError(#beamtalk_error{kind = type_error, class = 'List'},
        beamtalk_list_ops:sort_with([1, 2, 3], not_a_function)).

%%% ============================================================================
%%% Advanced Operations
%%% ============================================================================

zip_test() ->
    Result = beamtalk_list_ops:zip([1, 2, 3], [4, 5, 6]),
    ?assertEqual([#{<<"key">> => 1, <<"value">> => 4},
                  #{<<"key">> => 2, <<"value">> => 5},
                  #{<<"key">> => 3, <<"value">> => 6}], Result).

group_by_test() ->
    Result = beamtalk_list_ops:group_by([1, 2, 3, 4],
        fun(X) -> X rem 2 =:= 0 end),
    ?assertEqual(#{false => [1, 3], true => [2, 4]}, Result).

partition_test() ->
    Result = beamtalk_list_ops:partition([1, 2, 3, 4],
        fun(X) -> X > 2 end),
    ?assertEqual(#{<<"matching">> => [3, 4], <<"nonMatching">> => [1, 2]}, Result).

intersperse_test() ->
    ?assertEqual([1, 0, 2, 0, 3],
        beamtalk_list_ops:intersperse([1, 2, 3], 0)),
    ?assertEqual([], beamtalk_list_ops:intersperse([], 0)),
    ?assertEqual([1], beamtalk_list_ops:intersperse([1], 0)).

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
    ?assertError(#beamtalk_error{kind = does_not_understand, class = 'List', selector = 'first'},
        'bt@stdlib@list':dispatch('first', [], [])).

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
    ?assertEqual([2, 4, 6],
        'bt@stdlib@list':dispatch('collect:', [fun(X) -> X * 2 end], [1, 2, 3])).

dispatch_select_test() ->
    ?assertEqual([2, 4],
        'bt@stdlib@list':dispatch('select:', [fun(X) -> X rem 2 =:= 0 end], [1, 2, 3, 4])).

dispatch_inject_into_test() ->
    ?assertEqual(6,
        'bt@stdlib@list':dispatch('inject:into:', [0, fun(Acc, X) -> Acc + X end], [1, 2, 3])).

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
    ?assertEqual([2, 4, 6],
        beamtalk_primitive:send([1, 2, 3], 'collect:', [fun(X) -> X * 2 end])).

