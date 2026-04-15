%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_reflection_tests).

-moduledoc """
EUnit tests for beamtalk_reflection module.

Tests field name extraction, struct reflection, and runtime introspection helpers.
""".

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

%%% ============================================================================
%%% source_file_from_module/1 tests
%%% ============================================================================

source_file_from_module_returns_nil_for_stdlib_module_test() ->
    %% Standard Erlang/OTP modules have no beamtalk_source attribute
    ?assertEqual(nil, beamtalk_reflection:source_file_from_module(lists)).

source_file_from_module_returns_nil_for_nonexistent_module_test() ->
    ?assertEqual(nil, beamtalk_reflection:source_file_from_module('NonExistentModule_BT_1091')).

source_file_from_module_returns_nil_for_beamtalk_runtime_module_test() ->
    %% beamtalk_reflection itself is compiled without beamtalk_source attribute
    ?assertEqual(nil, beamtalk_reflection:source_file_from_module(beamtalk_reflection)).

source_file_from_module_returns_path_for_compiled_bt_module_test() ->
    %% Beamtalk-compiled modules have a beamtalk_source attribute
    case code:ensure_loaded('bt@counter') of
        {module, 'bt@counter'} ->
            PathBin = beamtalk_reflection:source_file_from_module('bt@counter'),
            ?assert(is_binary(PathBin)),
            PathStr = binary_to_list(PathBin),
            ?assert(lists:suffix("counter.bt", PathStr));
        {error, _} ->
            %% Skip if stdlib not compiled
            ok
    end.

%%% ============================================================================
%%% inspect_string/1 tests
%%% ============================================================================

inspect_string_empty_fields_test() ->
    State = #{'$beamtalk_class' => 'Empty'},
    Result = beamtalk_reflection:inspect_string(State),
    ?assertEqual(<<"a Empty">>, Result).

inspect_string_single_field_test() ->
    State = #{'$beamtalk_class' => 'Counter', value => 0},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Counter">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"value">>)).

inspect_string_multiple_fields_test() ->
    State = #{'$beamtalk_class' => 'Point', x => 10, y => 20},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Point">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"x">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"y">>)).

inspect_string_with_string_field_test() ->
    State = #{'$beamtalk_class' => 'Greeter', name => <<"Alice">>},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"Greeter">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"name">>)).

inspect_string_nil_field_test() ->
    State = #{'$beamtalk_class' => 'Obj', value => nil},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"value">>)).

inspect_string_defaults_class_to_object_test() ->
    %% State without $beamtalk_class should default to 'Object'
    State = #{},
    Result = beamtalk_reflection:inspect_string(State),
    ?assertEqual(<<"a Object">>, Result).

inspect_string_boolean_field_test() ->
    State = #{'$beamtalk_class' => 'Flag', active => true},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"active">>)).

inspect_string_list_field_test() ->
    State = #{'$beamtalk_class' => 'Container', items => [1, 2, 3]},
    Result = beamtalk_reflection:inspect_string(State),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"items">>)).

%%% ============================================================================
%%% field_names/1 additional edge cases
%%% ============================================================================

field_names_filters_all_internal_prefixes_test() ->
    %% Ensure all four internal fields are filtered
    State = #{
        '$beamtalk_class' => 'MyClass',
        '__class_mod__' => my_mod,
        '__methods__' => #{},
        '__registry_pid__' => self(),
        user_field => 42
    },
    Names = beamtalk_reflection:field_names(State),
    ?assertEqual([user_field], Names).

%%% ============================================================================
%%% write_field/3 additional edge cases
%%% ============================================================================

write_field_overwrite_nil_test() ->
    State = #{'$beamtalk_class' => 'Obj', value => nil},
    {Value, NewState} = beamtalk_reflection:write_field(value, 42, State),
    ?assertEqual(42, Value),
    ?assertEqual(42, maps:get(value, NewState)).

write_field_with_complex_key_test() ->
    State = #{'$beamtalk_class' => 'Obj'},
    {Value, NewState} = beamtalk_reflection:write_field(some_long_field_name, <<"data">>, State),
    ?assertEqual(<<"data">>, Value),
    ?assertEqual(<<"data">>, maps:get(some_long_field_name, NewState)).
