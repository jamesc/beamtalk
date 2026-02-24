%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_reflection module.
%%%
%%% Tests field name extraction, struct reflection, and runtime introspection helpers.

-module(beamtalk_reflection_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% field_names/1 tests
%%% ============================================================================

field_names_returns_user_fields_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 0, label => <<"test">>},
    Names = beamtalk_reflection:field_names(State),
    ?assert(lists:member(value, Names)),
    ?assert(lists:member(label, Names)),
    ?assertNot(lists:member('$beamtalk_class', Names)).

field_names_excludes_all_internal_fields_test() ->
    State = #{
        '$beamtalk_class' => 'MyClass',
        '__class_mod__' => my_mod,
        '__methods__' => #{},
        '__registry_pid__' => self(),
        user_field => 42
    },
    Names = beamtalk_reflection:field_names(State),
    ?assertEqual([user_field], Names).

field_names_empty_object_test() ->
    State = #{'$beamtalk_class' => 'Empty'},
    ?assertEqual([], beamtalk_reflection:field_names(State)).

%%% ============================================================================
%%% read_field/2 tests
%%% ============================================================================

read_field_existing_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 42},
    ?assertEqual(42, beamtalk_reflection:read_field(value, State)).

read_field_nonexistent_returns_nil_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 42},
    ?assertEqual(nil, beamtalk_reflection:read_field(missing, State)).

read_field_nil_value_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => nil},
    ?assertEqual(nil, beamtalk_reflection:read_field(value, State)).

read_field_complex_value_test() ->
    List = [1, 2, 3],
    State = #{'$beamtalk_class' => 'Container', items => List},
    ?assertEqual(List, beamtalk_reflection:read_field(items, State)).

%%% ============================================================================
%%% write_field/3 tests
%%% ============================================================================

write_field_existing_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 0},
    {Value, NewState} = beamtalk_reflection:write_field(value, 42, State),
    ?assertEqual(42, Value),
    ?assertEqual(42, maps:get(value, NewState)).

write_field_creates_new_field_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 0},
    {Value, NewState} = beamtalk_reflection:write_field(new_field, <<"hello">>, State),
    ?assertEqual(<<"hello">>, Value),
    ?assertEqual(<<"hello">>, maps:get(new_field, NewState)),
    %% Original field unchanged
    ?assertEqual(0, maps:get(value, NewState)).

write_field_returns_value_not_state_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 0},
    {Value, _NewState} = beamtalk_reflection:write_field(value, 99, State),
    ?assertEqual(99, Value).

write_field_preserves_other_fields_test() ->
    State = #{'$beamtalk_class' => 'Obj', a => 1, b => 2, c => 3},
    {_, NewState} = beamtalk_reflection:write_field(b, 20, State),
    ?assertEqual(1, maps:get(a, NewState)),
    ?assertEqual(20, maps:get(b, NewState)),
    ?assertEqual(3, maps:get(c, NewState)),
    ?assertEqual('Obj', maps:get('$beamtalk_class', NewState)).
