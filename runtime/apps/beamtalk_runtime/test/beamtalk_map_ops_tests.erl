%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Tests for beamtalk_map_ops helper module and compiled
%% bt@stdlib@dictionary stdlib dispatch (BT-418).
-module(beamtalk_map_ops_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% at_if_absent/3
%%% ============================================================================

at_if_absent_found_test() ->
    ?assertEqual(1, beamtalk_map_ops:at_if_absent(#{a => 1}, a, fun() -> 99 end)).

at_if_absent_missing_test() ->
    ?assertEqual(99, beamtalk_map_ops:at_if_absent(#{a => 1}, b, fun() -> 99 end)).

at_if_absent_empty_map_test() ->
    ?assertEqual(42, beamtalk_map_ops:at_if_absent(#{}, x, fun() -> 42 end)).

%%% ============================================================================
%%% keys_and_values_do/2
%%% ============================================================================

keys_and_values_do_test() ->
    ?assertEqual(nil, beamtalk_map_ops:keys_and_values_do(#{a => 1}, fun(_, _) -> ok end)).

keys_and_values_do_empty_test() ->
    ?assertEqual(nil, beamtalk_map_ops:keys_and_values_do(#{}, fun(_, _) -> ok end)).

%%% ============================================================================
%%% Compiled bt@stdlib@dictionary dispatch/3
%%% ============================================================================

dictionary_class_test() ->
    ?assertEqual('Dictionary', 'bt@stdlib@dictionary':dispatch('class', [], #{a => 1})),
    ?assertEqual('Dictionary', 'bt@stdlib@dictionary':dispatch('class', [], #{})).

dictionary_keys_test() ->
    ?assertEqual([a], 'bt@stdlib@dictionary':dispatch('keys', [], #{a => 1})),
    ?assertEqual([], 'bt@stdlib@dictionary':dispatch('keys', [], #{})).

dictionary_values_test() ->
    ?assertEqual([1], 'bt@stdlib@dictionary':dispatch('values', [], #{a => 1})).

dictionary_size_test() ->
    ?assertEqual(2, 'bt@stdlib@dictionary':dispatch('size', [], #{a => 1, b => 2})),
    ?assertEqual(0, 'bt@stdlib@dictionary':dispatch('size', [], #{})).

dictionary_at_test() ->
    ?assertEqual(1, 'bt@stdlib@dictionary':dispatch('at:', [a], #{a => 1, b => 2})).

dictionary_at_put_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        'bt@stdlib@dictionary':dispatch('at:put:', [b, 2], #{a => 1})
    ).

dictionary_at_if_absent_test() ->
    ?assertEqual(
        1, 'bt@stdlib@dictionary':dispatch('at:ifAbsent:', [a, fun() -> 99 end], #{a => 1})
    ),
    ?assertEqual(
        99, 'bt@stdlib@dictionary':dispatch('at:ifAbsent:', [b, fun() -> 99 end], #{a => 1})
    ).

dictionary_includes_key_test() ->
    ?assertEqual(true, 'bt@stdlib@dictionary':dispatch('includesKey:', [a], #{a => 1})),
    ?assertEqual(false, 'bt@stdlib@dictionary':dispatch('includesKey:', [b], #{a => 1})).

dictionary_remove_key_test() ->
    ?assertEqual(#{}, 'bt@stdlib@dictionary':dispatch('removeKey:', [a], #{a => 1})).

dictionary_merge_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        'bt@stdlib@dictionary':dispatch('merge:', [#{b => 2}], #{a => 1})
    ).

dictionary_keys_and_values_do_test() ->
    ?assertEqual(
        nil,
        'bt@stdlib@dictionary':dispatch('keysAndValuesDo:', [fun(_, _) -> ok end], #{a => 1})
    ).

%%% ============================================================================
%%% Compiled bt@stdlib@dictionary has_method/1
%%% ============================================================================

dictionary_responds_to_test() ->
    beamtalk_extensions:init(),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('class')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('keys')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('values')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('size')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:put:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:ifAbsent:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('includesKey:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('removeKey:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('merge:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('keysAndValuesDo:')),
    ?assertEqual(false, 'bt@stdlib@dictionary':has_method('nonExistent')).

%%% ============================================================================
%%% Does Not Understand
%%% ============================================================================

dictionary_does_not_understand_test() ->
    beamtalk_extensions:init(),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'Dictionary'}
        },
        'bt@stdlib@dictionary':dispatch('nonExistent', [], #{a => 1})
    ).

%%% ============================================================================
%%% Error paths
%%% ============================================================================

dictionary_at_missing_key_test() ->
    %% maps:get raises {badkey, Key} for missing keys — same as pre-migration
    ?assertError(
        {badkey, missing},
        'bt@stdlib@dictionary':dispatch('at:', [missing], #{a => 1})
    ).

%%% ============================================================================
%%% beamtalk_primitive:send integration tests
%%% ============================================================================

primitive_send_size_test() ->
    ?assertEqual(2, beamtalk_primitive:send(#{a => 1, b => 2}, 'size', [])),
    ?assertEqual(0, beamtalk_primitive:send(#{}, 'size', [])).

primitive_send_keys_test() ->
    ?assertEqual([a], beamtalk_primitive:send(#{a => 1}, 'keys', [])).

primitive_send_at_test() ->
    ?assertEqual(1, beamtalk_primitive:send(#{a => 1}, 'at:', [a])).

primitive_send_at_put_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        beamtalk_primitive:send(#{a => 1}, 'at:put:', [b, 2])
    ).

primitive_send_includes_key_test() ->
    ?assertEqual(true, beamtalk_primitive:send(#{a => 1}, 'includesKey:', [a])),
    ?assertEqual(false, beamtalk_primitive:send(#{a => 1}, 'includesKey:', [b])).
