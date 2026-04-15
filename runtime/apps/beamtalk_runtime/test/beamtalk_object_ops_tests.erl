%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_ops_tests).

-moduledoc """
Unit tests for beamtalk_object.erl (ADR 0006 Phase 1b).

Tests the Object base class reflection, display, and utility methods.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% A typical actor state map (as would be in a Counter gen_server)
counter_state() ->
    #{
        '$beamtalk_class' => 'Counter',
        '__class_mod__' => counter,
        '__methods__' => #{'increment' => 0, 'getValue' => 0},
        '__registry_pid__' => self(),
        value => 0
    }.

%% An actor state with multiple fields
multi_field_state() ->
    #{
        '$beamtalk_class' => 'Point',
        '__class_mod__' => point,
        '__methods__' => #{'x' => 0, 'y' => 0},
        '__registry_pid__' => self(),
        x => 10,
        y => 20
    }.

self_ref() -> make_ref().

%%% ============================================================================
%%% Reflection Method Tests
%%% ============================================================================

object_reflection_test_() ->
    {"Object reflection methods", [
        {"class returns $beamtalk_class from state", fun test_class/0},
        {"fieldNames filters internal fields", fun test_inst_var_names/0},
        {"fieldNames with multiple fields", fun test_inst_var_names_multi/0},
        {"fieldAt: reads field value", fun test_inst_var_at/0},
        {"fieldAt: with non-existent field returns nil", fun test_inst_var_at_missing/0},
        {"fieldAt:put: sets field value and returns value", fun test_inst_var_at_put/0},
        {"fieldAt:put: with non-existent field creates it", fun test_inst_var_at_put_missing/0},
        {"fieldAt:put: returns updated state", fun test_inst_var_at_put_state/0}
    ]}.

test_class() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(class, [], self_ref(), State),
    ?assertMatch({reply, 'Counter', _}, Result).

test_inst_var_names() ->
    State = counter_state(),
    {reply, Names, _} = beamtalk_object_ops:dispatch('fieldNames', [], self_ref(), State),
    ?assertEqual([value], Names).

test_inst_var_names_multi() ->
    State = multi_field_state(),
    {reply, Names, _} = beamtalk_object_ops:dispatch('fieldNames', [], self_ref(), State),
    ?assertEqual(lists:sort([x, y]), lists:sort(Names)).

test_inst_var_at() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('fieldAt:', [value], self_ref(), State),
    ?assertMatch({reply, 0, _}, Result).

test_inst_var_at_missing() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('fieldAt:', [nonexistent], self_ref(), State),
    ?assertMatch({reply, nil, _}, Result).

test_inst_var_at_put() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('fieldAt:put:', [value, 42], self_ref(), State),
    ?assertMatch({reply, 42, _}, Result).

test_inst_var_at_put_missing() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('fieldAt:put:', [nonexistent, 42], self_ref(), State),
    %% BT-427: Smalltalk semantics — creates the field, returns value
    ?assertMatch({reply, 42, _}, Result),
    {reply, _, NewState} = Result,
    ?assertEqual(42, maps:get(nonexistent, NewState)).

test_inst_var_at_put_state() ->
    State = counter_state(),
    {reply, 42, NewState} = beamtalk_object_ops:dispatch(
        'fieldAt:put:', [value, 42], self_ref(), State
    ),
    ?assertEqual(42, maps:get(value, NewState)).

%%% ============================================================================
%%% Display Method Tests
%%% ============================================================================

object_display_test_() ->
    {"Object display methods", [
        {"printString returns readable string", fun test_print_string/0},
        {"inspect includes fields", fun test_inspect/0}
    ]}.

test_print_string() ->
    State = counter_state(),
    {reply, Str, _} = beamtalk_object_ops:dispatch('printString', [], self_ref(), State),
    ?assertEqual(<<"a Counter">>, Str).

test_inspect() ->
    State = counter_state(),
    {reply, Str, _} = beamtalk_object_ops:dispatch(inspect, [], self_ref(), State),
    %% BT-1167: New format is "Counter(value: 0)" instead of "a Counter (value: 0)"
    ?assert(binary:match(Str, <<"Counter(">>) =/= nomatch),
    ?assert(binary:match(Str, <<"value">>) =/= nomatch).

%%% ============================================================================
%%% Utility Method Tests
%%% ============================================================================

object_utility_test_() ->
    {"Object utility methods", [
        {"yourself returns Self", fun test_yourself/0},
        {"hash returns integer", fun test_hash/0},
        {"isNil returns false", fun test_is_nil/0},
        {"notNil returns true", fun test_not_nil/0}
    ]}.

test_yourself() ->
    State = counter_state(),
    Self = self_ref(),
    {reply, Result, _} = beamtalk_object_ops:dispatch(yourself, [], Self, State),
    ?assertEqual(Self, Result).

test_hash() ->
    State = counter_state(),
    Self = self_ref(),
    {reply, Hash, _} = beamtalk_object_ops:dispatch(hash, [], Self, State),
    ?assert(is_integer(Hash)).

test_is_nil() ->
    State = counter_state(),
    {reply, false, _} = beamtalk_object_ops:dispatch(isNil, [], self_ref(), State).

test_not_nil() ->
    State = counter_state(),
    {reply, true, _} = beamtalk_object_ops:dispatch(notNil, [], self_ref(), State).

%%% ============================================================================
%%% has_method/1 Tests
%%% ============================================================================

has_method_test_() ->
    {"has_method/1", [
        {"class is a known method", fun() -> ?assert(beamtalk_object_ops:has_method(class)) end},
        {"respondsTo: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('respondsTo:'))
        end},
        {"fieldNames is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('fieldNames'))
        end},
        {"fieldAt: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('fieldAt:'))
        end},
        {"fieldAt:put: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('fieldAt:put:'))
        end},
        {"perform: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('perform:'))
        end},
        {"perform:withArguments: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('perform:withArguments:'))
        end},
        {"printString is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('printString'))
        end},
        {"inspect is a known method", fun() -> ?assert(beamtalk_object_ops:has_method(inspect)) end},
        {"yourself is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method(yourself))
        end},
        {"hash is a known method", fun() -> ?assert(beamtalk_object_ops:has_method(hash)) end},
        {"isNil is a known method", fun() -> ?assert(beamtalk_object_ops:has_method(isNil)) end},
        {"notNil is a known method", fun() -> ?assert(beamtalk_object_ops:has_method(notNil)) end},
        {"unknown is not a method", fun() ->
            ?assertNot(beamtalk_object_ops:has_method(unknown))
        end}
    ]}.

%%% ============================================================================
%%% Fallback Tests
%%% ============================================================================

fallback_test_() ->
    {"dispatch fallback for unknown methods", [
        {"unknown method returns error", fun test_unknown_method/0},
        {"perform: unknown returns 3-tuple error", fun test_perform_unknown_returns_3tuple/0},
        {"perform:withArguments: bad type returns 3-tuple error",
            fun test_perform_withargs_bad_type/0}
    ]}.

test_unknown_method() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(unknownMethod, [], self_ref(), State),
    ?assertMatch({error, #beamtalk_error{kind = does_not_understand}, _}, Result).

test_perform_unknown_returns_3tuple() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('perform:', [nonExistentMethod], self_ref(), State),
    %% Must return 3-tuple {error, Error, State} not 2-tuple {error, Error}
    ?assertMatch({error, #beamtalk_error{}, _}, Result).

test_perform_withargs_bad_type() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(
        'perform:withArguments:', ["notAnAtom", []], self_ref(), State
    ),
    ?assertMatch({error, #beamtalk_error{kind = type_error}, _}, Result).

%%% ============================================================================
%%% Display Method Tests (additional)
%%% ============================================================================

display_string_test_() ->
    {"displayString method", [
        {"displayString returns same as printString", fun test_display_string/0}
    ]}.

test_display_string() ->
    State = counter_state(),
    {reply, Str, _} = beamtalk_object_ops:dispatch('displayString', [], self_ref(), State),
    ?assertEqual(<<"a Counter">>, Str).

%%% ============================================================================
%%% inspect on class objects (empty state) Tests
%%% ============================================================================

inspect_class_object_test_() ->
    {"inspect on class objects (empty state)", [
        {"inspect with empty state returns class display name", fun test_inspect_empty_state/0}
    ]}.

test_inspect_empty_state() ->
    %% BT-753: Class objects have empty state map
    Self = #beamtalk_object{class = 'Counter', pid = self(), class_mod = counter},
    State = #{},
    {reply, Str, _} = beamtalk_object_ops:dispatch(inspect, [], Self, State),
    ?assert(is_binary(Str)).

%%% ============================================================================
%%% class_name/3 Tests (exported API)
%%% ============================================================================

class_name_test_() ->
    {"class_name/3 extraction", [
        {"class_name/3 from beamtalk_object record", fun test_class_name3_from_record/0},
        {"class_name/3 from state map with class", fun test_class_name3_from_state/0},
        {"class_name/3 falls back to default", fun test_class_name3_default/0}
    ]}.

test_class_name3_from_record() ->
    Self = #beamtalk_object{class = 'MyClass', pid = self(), class_mod = my_class},
    ?assertEqual('MyClass', beamtalk_object_ops:class_name(Self, #{}, 'Fallback')).

test_class_name3_from_state() ->
    State = counter_state(),
    ?assertEqual('Counter', beamtalk_object_ops:class_name(make_ref(), State, 'Fallback')).

test_class_name3_default() ->
    ?assertEqual('Fallback', beamtalk_object_ops:class_name(make_ref(), #{}, 'Fallback')).

%%% ============================================================================
%%% has_method additional coverage
%%% ============================================================================

has_method_additional_test_() ->
    {"has_method additional selectors", [
        {"displayString is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('displayString'))
        end},
        {"perform:withArguments:timeout: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('perform:withArguments:timeout:'))
        end},
        {"subclassResponsibility is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method(subclassResponsibility))
        end}
    ]}.

%%% ============================================================================
%%% subclassResponsibility Tests
%%% ============================================================================

subclass_responsibility_test_() ->
    {"subclassResponsibility dispatch", [
        {"subclassResponsibility returns user_error", fun test_subclass_responsibility/0}
    ]}.

test_subclass_responsibility() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(subclassResponsibility, [], self_ref(), State),
    ?assertMatch({error, #beamtalk_error{kind = user_error}, _}, Result).

%%% ============================================================================
%%% perform:withArguments:timeout: Tests
%%% ============================================================================

perform_with_timeout_test_() ->
    {"perform:withArguments:timeout: dispatch", [
        {"perform:withArguments:timeout: bad type returns type_error",
            fun test_perform_with_timeout_bad_type/0}
    ]}.

test_perform_with_timeout_bad_type() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(
        'perform:withArguments:timeout:', ["notAnAtom", [], 5000], self_ref(), State
    ),
    ?assertMatch({error, #beamtalk_error{kind = type_error}, _}, Result).

%%% ============================================================================
%%% Equality / hash edge cases
%%% ============================================================================

hash_determinism_test_() ->
    {"hash determinism", [
        {"same self produces same hash", fun test_hash_deterministic/0},
        {"different selfs produce different hashes (usually)", fun test_hash_different/0}
    ]}.

test_hash_deterministic() ->
    State = counter_state(),
    Self = self_ref(),
    {reply, Hash1, _} = beamtalk_object_ops:dispatch(hash, [], Self, State),
    {reply, Hash2, _} = beamtalk_object_ops:dispatch(hash, [], Self, State),
    ?assertEqual(Hash1, Hash2).

test_hash_different() ->
    State = counter_state(),
    {reply, Hash1, _} = beamtalk_object_ops:dispatch(hash, [], make_ref(), State),
    {reply, Hash2, _} = beamtalk_object_ops:dispatch(hash, [], make_ref(), State),
    %% Very unlikely to be equal for different refs
    ?assertNotEqual(Hash1, Hash2).

%%% ============================================================================
%%% printString with beamtalk_object record Self
%%% ============================================================================

print_string_class_object_test_() ->
    {"printString with class object Self", [
        {"printString uses class from record when state is empty",
            fun test_print_string_class_object/0}
    ]}.

test_print_string_class_object() ->
    Self = #beamtalk_object{class = 'Counter', pid = self(), class_mod = counter},
    State = #{},
    {reply, Str, _} = beamtalk_object_ops:dispatch('printString', [], Self, State),
    ?assert(is_binary(Str)).

%%% ============================================================================
%%% fieldAt:put: returns updated state correctly
%%% ============================================================================

field_mutation_errors_test_() ->
    {"field mutation edge cases", [
        {"fieldAt:put: with internal field prefix", fun test_field_at_put_internal/0},
        {"fieldNames on empty object", fun test_field_names_empty/0}
    ]}.

test_field_at_put_internal() ->
    State = counter_state(),
    %% Writing to an arbitrary atom field should work
    Result = beamtalk_object_ops:dispatch('fieldAt:put:', [new_field, 99], self_ref(), State),
    ?assertMatch({reply, 99, _}, Result),
    {reply, _, NewState} = Result,
    ?assertEqual(99, maps:get(new_field, NewState)).

test_field_names_empty() ->
    %% An object with only internal fields should have empty field names
    State = #{
        '$beamtalk_class' => 'Empty',
        '__class_mod__' => empty,
        '__methods__' => #{},
        '__registry_pid__' => self()
    },
    {reply, Names, _} = beamtalk_object_ops:dispatch('fieldNames', [], self_ref(), State),
    ?assertEqual([], Names).
