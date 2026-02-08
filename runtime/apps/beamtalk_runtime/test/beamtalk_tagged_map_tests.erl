%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_tagged_map module.

-module(beamtalk_tagged_map_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% class_key/0 tests
%%% ============================================================================

class_key_returns_atom_test() ->
    ?assert(is_atom(beamtalk_tagged_map:class_key())),
    ?assertEqual('$beamtalk_class', beamtalk_tagged_map:class_key()).

%%% ============================================================================
%%% class_of/1 tests
%%% ============================================================================

class_of_tagged_map_test() ->
    Map = #{'$beamtalk_class' => 'Counter', value => 0},
    ?assertEqual('Counter', beamtalk_tagged_map:class_of(Map)).

class_of_compiled_method_test() ->
    Map = #{'$beamtalk_class' => 'CompiledMethod', '__selector__' => test},
    ?assertEqual('CompiledMethod', beamtalk_tagged_map:class_of(Map)).

class_of_plain_map_test() ->
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(#{})),
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(#{a => 1})).

class_of_non_atom_class_test() ->
    %% Non-atom $beamtalk_class values are not valid tagged maps
    Map = #{'$beamtalk_class' => <<"Counter">>},
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(Map)).

class_of_old_class_key_test() ->
    %% BT-324: Maps with old '__class__' key are NOT tagged maps
    Map = #{'__class__' => 'Counter', value => 0},
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(Map)).

class_of_non_map_test() ->
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(42)),
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(<<"string">>)),
    ?assertEqual(undefined, beamtalk_tagged_map:class_of([1, 2])),
    ?assertEqual(undefined, beamtalk_tagged_map:class_of(atom)).

%%% ============================================================================
%%% class_of/2 tests
%%% ============================================================================

class_of_with_default_tagged_test() ->
    Map = #{'$beamtalk_class' => 'Point', x => 0},
    ?assertEqual('Point', beamtalk_tagged_map:class_of(Map, 'Object')).

class_of_with_default_plain_test() ->
    ?assertEqual('Object', beamtalk_tagged_map:class_of(#{}, 'Object')),
    ?assertEqual(unknown, beamtalk_tagged_map:class_of(#{a => 1}, unknown)).

class_of_with_default_non_map_test() ->
    ?assertEqual('Object', beamtalk_tagged_map:class_of(42, 'Object')).

%%% ============================================================================
%%% is_tagged/1 tests
%%% ============================================================================

is_tagged_true_test() ->
    Map = #{'$beamtalk_class' => 'Counter'},
    ?assert(beamtalk_tagged_map:is_tagged(Map)).

is_tagged_false_plain_map_test() ->
    ?assertNot(beamtalk_tagged_map:is_tagged(#{})),
    ?assertNot(beamtalk_tagged_map:is_tagged(#{a => 1})).

is_tagged_false_non_atom_test() ->
    Map = #{'$beamtalk_class' => <<"Counter">>},
    ?assertNot(beamtalk_tagged_map:is_tagged(Map)).

is_tagged_false_non_map_test() ->
    ?assertNot(beamtalk_tagged_map:is_tagged(42)),
    ?assertNot(beamtalk_tagged_map:is_tagged([])).

%%% ============================================================================
%%% internal_fields/0 tests
%%% ============================================================================

internal_fields_contains_expected_test() ->
    Fields = beamtalk_tagged_map:internal_fields(),
    ?assert(lists:member('$beamtalk_class', Fields)),
    ?assert(lists:member('__class_mod__', Fields)),
    ?assert(lists:member('__methods__', Fields)),
    ?assert(lists:member('__registry_pid__', Fields)).

internal_fields_excludes_user_fields_test() ->
    Fields = beamtalk_tagged_map:internal_fields(),
    ?assertNot(lists:member(value, Fields)),
    ?assertNot(lists:member(x, Fields)).

%%% ============================================================================
%%% user_field_keys/1 tests
%%% ============================================================================

user_field_keys_filters_internals_test() ->
    State = #{
        '$beamtalk_class' => 'Counter',
        '__class_mod__' => counter,
        '__methods__' => #{},
        value => 0,
        name => <<"test">>
    },
    Keys = beamtalk_tagged_map:user_field_keys(State),
    ?assert(lists:member(value, Keys)),
    ?assert(lists:member(name, Keys)),
    ?assertNot(lists:member('$beamtalk_class', Keys)),
    ?assertNot(lists:member('__class_mod__', Keys)),
    ?assertNot(lists:member('__methods__', Keys)).

user_field_keys_empty_state_test() ->
    State = #{'$beamtalk_class' => 'Empty', '__methods__' => #{}},
    ?assertEqual([], beamtalk_tagged_map:user_field_keys(State)).

user_field_keys_filters_registry_pid_test() ->
    State = #{
        '$beamtalk_class' => 'Test',
        '__registry_pid__' => self(),
        field => 1
    },
    Keys = beamtalk_tagged_map:user_field_keys(State),
    ?assert(lists:member(field, Keys)),
    ?assertNot(lists:member('__registry_pid__', Keys)).

%%% ============================================================================
%%% format_for_display/1 tests
%%% ============================================================================

format_for_display_tagged_test() ->
    Map = #{'$beamtalk_class' => 'Counter', value => 0},
    ?assertEqual(<<"a Counter">>, beamtalk_tagged_map:format_for_display(Map)).

format_for_display_plain_map_test() ->
    Map = #{a => 1},
    Result = beamtalk_tagged_map:format_for_display(Map),
    ?assert(is_binary(Result)),
    %% Should be io_lib:format output, not "a ..." format
    ?assertNot(binary:match(Result, <<"a ">>) =:= {0, 2}).
