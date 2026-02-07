%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_stdlib module.
-module(beamtalk_stdlib_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Setup and teardown for stdlib tests
stdlib_setup() ->
    %% Ensure pg is running
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    %% Initialize extensions ETS table
    beamtalk_extensions:init(),
    %% Start bootstrap (creates ProtoObject, Object, Actor)
    {ok, _} = beamtalk_bootstrap:start_link(),
    ok.

stdlib_teardown(_) ->
    %% Classes are gen_servers that get cleaned up automatically
    ok.

stdlib_test_() ->
    {setup,
     fun stdlib_setup/0,
     fun stdlib_teardown/1,
     [
        {"init registers all primitive classes", fun init_registers_all_classes_test/0},
        {"init is idempotent", fun init_idempotent_test/0},
        {"Integer class is registered", fun integer_class_registered_test/0},
        {"String class is registered", fun string_class_registered_test/0},
        {"Boolean class is registered", fun boolean_class_registered_test/0},
        {"UndefinedObject class is registered", fun nil_class_registered_test/0},
        {"Block class is registered", fun block_class_registered_test/0},
        {"Tuple class is registered", fun tuple_class_registered_test/0},
        {"Beamtalk class is registered", fun beamtalk_class_registered_test/0},
        {"Integer class has correct superclass", fun integer_superclass_test/0},
        {"Integer class has expected methods", fun integer_methods_test/0},
        %% Beamtalk class method tests
        {"Beamtalk allClasses returns all class names", fun beamtalk_all_classes_test/0},
        {"Beamtalk classNamed: finds existing class", fun beamtalk_class_named_found_test/0},
        {"Beamtalk classNamed: returns nil for missing class", fun beamtalk_class_named_not_found_test/0},
        {"Beamtalk globals returns map", fun beamtalk_globals_test/0},
        {"Beamtalk version returns version string", fun beamtalk_version_test/0},
        {"Beamtalk has_method returns true for known methods", fun beamtalk_has_method_test/0}
     ]}.

%%% ============================================================================
%%% Test Cases
%%% ============================================================================

init_registers_all_classes_test() ->
    %% Initialize stdlib (may already be initialized from fixture)
    ok = beamtalk_stdlib:init(),
    
    %% After init, should have bootstrap + stdlib classes
    ClassesAfter = [beamtalk_object_class:class_name(Pid) || Pid <- beamtalk_object_class:all_classes()],
    ?assertEqual(11, length(ClassesAfter)),  % 3 bootstrap + 8 stdlib
    
    %% Verify expected classes are present
    ?assert(lists:member('ProtoObject', ClassesAfter)),
    ?assert(lists:member('Object', ClassesAfter)),
    ?assert(lists:member('Actor', ClassesAfter)),
    ?assert(lists:member('Integer', ClassesAfter)),
    ?assert(lists:member('String', ClassesAfter)),
    ?assert(lists:member('Boolean', ClassesAfter)),
    ?assert(lists:member('UndefinedObject', ClassesAfter)),
    ?assert(lists:member('Block', ClassesAfter)),
    ?assert(lists:member('Tuple', ClassesAfter)),
    ?assert(lists:member('Beamtalk', ClassesAfter)).

init_idempotent_test() ->
    %% Call init multiple times
    ok = beamtalk_stdlib:init(),
    ok = beamtalk_stdlib:init(),
    
    %% Should still have same number of classes (no duplicates)
    Classes = [beamtalk_object_class:class_name(Pid) || Pid <- beamtalk_object_class:all_classes()],
    ?assertEqual(11, length(Classes)).

integer_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Integer'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Integer', beamtalk_object_class:class_name(Pid)).

string_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('String'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('String', beamtalk_object_class:class_name(Pid)).

boolean_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Boolean'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Boolean', beamtalk_object_class:class_name(Pid)).

nil_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('UndefinedObject'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('UndefinedObject', beamtalk_object_class:class_name(Pid)).

block_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Block'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Block', beamtalk_object_class:class_name(Pid)).

tuple_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Tuple'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Tuple', beamtalk_object_class:class_name(Pid)).

beamtalk_class_registered_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Beamtalk'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Beamtalk', beamtalk_object_class:class_name(Pid)).

integer_superclass_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Integer'),
    Superclass = beamtalk_object_class:superclass(Pid),
    ?assertEqual('Object', Superclass).

integer_methods_test() ->
    ok = beamtalk_stdlib:init(),
    Pid = beamtalk_object_class:whereis_class('Integer'),
    Methods = beamtalk_object_class:methods(Pid),
    
    %% Check some expected methods
    ?assert(lists:member('+', Methods)),
    ?assert(lists:member('-', Methods)),
    ?assert(lists:member('*', Methods)),
    ?assert(lists:member('class', Methods)),
    ?assert(lists:member('asString', Methods)).

%%% ============================================================================
%%% Beamtalk Class Method Tests
%%% ============================================================================

beamtalk_all_classes_test() ->
    ok = beamtalk_stdlib:init(),
    
    %% Call Beamtalk allClasses via dispatch
    Classes = beamtalk_stdlib:dispatch(allClasses, [], 'Beamtalk'),
    
    %% Should return a list of atoms (class names)
    ?assert(is_list(Classes)),
    ?assert(length(Classes) >= 10),  % At least bootstrap + stdlib classes
    
    %% All stdlib classes should be present
    ?assert(lists:member('Integer', Classes)),
    ?assert(lists:member('String', Classes)),
    ?assert(lists:member('Boolean', Classes)),
    ?assert(lists:member('Block', Classes)),
    ?assert(lists:member('Beamtalk', Classes)),
    ?assert(lists:member('ProtoObject', Classes)),
    ?assert(lists:member('Object', Classes)),
    ?assert(lists:member('Actor', Classes)).

beamtalk_class_named_found_test() ->
    ok = beamtalk_stdlib:init(),
    
    %% Look up an existing class
    Result = beamtalk_stdlib:dispatch('classNamed:', ['Integer'], 'Beamtalk'),
    
    %% Should return a wrapped class object
    ?assertMatch({beamtalk_object, 'Integer', beamtalk_object_class, _Pid}, Result),
    
    %% Extract the pid and verify it's the Integer class
    {beamtalk_object, 'Integer', beamtalk_object_class, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assertEqual('Integer', beamtalk_object_class:class_name(Pid)).

beamtalk_class_named_not_found_test() ->
    ok = beamtalk_stdlib:init(),
    
    %% Look up a non-existent class
    Result = beamtalk_stdlib:dispatch('classNamed:', ['NonExistentClass'], 'Beamtalk'),
    
    %% Should return nil
    ?assertEqual(nil, Result).

beamtalk_globals_test() ->
    ok = beamtalk_stdlib:init(),
    
    %% Call Beamtalk globals
    Globals = beamtalk_stdlib:dispatch(globals, [], 'Beamtalk'),
    
    %% Should return a map (currently empty placeholder)
    ?assert(is_map(Globals)).

beamtalk_version_test() ->
    ok = beamtalk_stdlib:init(),
    
    %% Call Beamtalk version
    Version = beamtalk_stdlib:dispatch(version, [], 'Beamtalk'),
    
    %% Should return a binary version string
    ?assert(is_binary(Version)),
    ?assertEqual(<<"0.1.0">>, Version).

beamtalk_has_method_test() ->
    %% Check has_method for known methods
    ?assert(beamtalk_stdlib:has_method(allClasses)),
    ?assert(beamtalk_stdlib:has_method('classNamed:')),
    ?assert(beamtalk_stdlib:has_method(globals)),
    ?assert(beamtalk_stdlib:has_method(version)),
    
    %% Check has_method for unknown methods
    ?assertNot(beamtalk_stdlib:has_method(unknownMethod)),
    ?assertNot(beamtalk_stdlib:has_method(fooBar)).
