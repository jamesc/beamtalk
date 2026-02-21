%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_method_resolver module (BT-623).
%%%
%%% Tests method resolution from various class reference formats:
%%% pid, atom, class object tuple, and invalid inputs.

-module(beamtalk_method_resolver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    ok.

teardown(_) ->
    ok.

resolver_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"resolve with class pid returns method", fun test_resolve_with_pid/0},
            {"resolve with class pid returns nil for missing method",
                fun test_resolve_with_pid_missing/0},
            {"resolve with class name atom", fun test_resolve_with_atom/0},
            {"resolve with unknown class name raises error", fun test_resolve_unknown_class/0},
            {"resolve with class object tuple", fun test_resolve_with_class_tuple/0},
            {"resolve with instance tuple raises type_error",
                fun test_resolve_with_instance_tuple/0},
            {"resolve with non-atom/non-pid raises type_error",
                fun test_resolve_with_invalid_ref/0},
            {"resolve with integer raises type_error", fun test_resolve_with_integer/0}
        ]
    end}.

%%====================================================================
%% Test Cases
%%====================================================================

test_resolve_with_pid() ->
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    %% Integer has '+' method — CompiledMethod uses __selector__
    Result = beamtalk_method_resolver:resolve(IntegerPid, '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_with_pid_missing() ->
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    Result = beamtalk_method_resolver:resolve(IntegerPid, 'totallyFakeMethod'),
    ?assertEqual(nil, Result).

test_resolve_with_atom() ->
    Result = beamtalk_method_resolver:resolve('Integer', '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_unknown_class() ->
    %% beamtalk_error:raise wraps the error in a tagged map
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_method_resolver:resolve('NonExistentClass', '+')
    ).

test_resolve_with_class_tuple() ->
    %% Class objects have 'ClassName class' as their tag
    IntegerPid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, IntegerPid),
    ClassObj = #beamtalk_object{
        class = 'Integer class', class_mod = beamtalk_object_class, pid = IntegerPid
    },
    Result = beamtalk_method_resolver:resolve(ClassObj, '+'),
    ?assertMatch(#{'__selector__' := '+'}, Result).

test_resolve_with_instance_tuple() ->
    %% Instance tuples (non-class tag) should raise type_error
    Obj = #beamtalk_object{class = 'Integer', class_mod = beamtalk_integer, pid = self()},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve(Obj, '+')
    ).

test_resolve_with_invalid_ref() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve("not_a_class", '+')
    ).

test_resolve_with_integer() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_method_resolver:resolve(42, '+')
    ).
