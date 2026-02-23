%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_object.erl (ADR 0006 Phase 1b).
%%%
%%% Tests the Object base class reflection, display, and utility methods.
-module(beamtalk_object_ops_tests).

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
        {"instVarNames filters internal fields", fun test_inst_var_names/0},
        {"instVarNames with multiple fields", fun test_inst_var_names_multi/0},
        {"instVarAt: reads field value", fun test_inst_var_at/0},
        {"instVarAt: with non-existent field returns nil", fun test_inst_var_at_missing/0},
        {"instVarAt:put: sets field value and returns value", fun test_inst_var_at_put/0},
        {"instVarAt:put: with non-existent field creates it", fun test_inst_var_at_put_missing/0},
        {"instVarAt:put: returns updated state", fun test_inst_var_at_put_state/0}
    ]}.

test_class() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch(class, [], self_ref(), State),
    ?assertMatch({reply, 'Counter', _}, Result).

test_inst_var_names() ->
    State = counter_state(),
    {reply, Names, _} = beamtalk_object_ops:dispatch('instVarNames', [], self_ref(), State),
    ?assertEqual([value], Names).

test_inst_var_names_multi() ->
    State = multi_field_state(),
    {reply, Names, _} = beamtalk_object_ops:dispatch('instVarNames', [], self_ref(), State),
    ?assertEqual(lists:sort([x, y]), lists:sort(Names)).

test_inst_var_at() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('instVarAt:', [value], self_ref(), State),
    ?assertMatch({reply, 0, _}, Result).

test_inst_var_at_missing() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('instVarAt:', [nonexistent], self_ref(), State),
    ?assertMatch({reply, nil, _}, Result).

test_inst_var_at_put() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('instVarAt:put:', [value, 42], self_ref(), State),
    ?assertMatch({reply, 42, _}, Result).

test_inst_var_at_put_missing() ->
    State = counter_state(),
    Result = beamtalk_object_ops:dispatch('instVarAt:put:', [nonexistent, 42], self_ref(), State),
    %% BT-427: Smalltalk semantics — creates the field, returns value
    ?assertMatch({reply, 42, _}, Result),
    {reply, _, NewState} = Result,
    ?assertEqual(42, maps:get(nonexistent, NewState)).

test_inst_var_at_put_state() ->
    State = counter_state(),
    {reply, 42, NewState} = beamtalk_object_ops:dispatch(
        'instVarAt:put:', [value, 42], self_ref(), State
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
    %% Should start with "a Counter" and include the value field
    ?assert(binary:match(Str, <<"a Counter">>) =/= nomatch),
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
        {"instVarNames is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('instVarNames'))
        end},
        {"instVarAt: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('instVarAt:'))
        end},
        {"instVarAt:put: is a known method", fun() ->
            ?assert(beamtalk_object_ops:has_method('instVarAt:put:'))
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
