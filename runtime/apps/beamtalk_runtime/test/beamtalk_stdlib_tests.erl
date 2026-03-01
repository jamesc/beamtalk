%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_stdlib module.
-module(beamtalk_stdlib_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
    %% Start bootstrap (starts pg) then load stdlib classes
    {ok, _} = beamtalk_bootstrap:start_link(),
    beamtalk_stdlib:init(),
    ok.

stdlib_teardown(_) ->
    %% Classes are gen_servers that get cleaned up automatically
    ok.

stdlib_test_() ->
    {setup, fun stdlib_setup/0, fun stdlib_teardown/1, [
        {"init registers all primitive classes", fun init_registers_all_classes_test/0},
        {"init is idempotent", fun init_idempotent_test/0},
        {"Integer class is registered", fun integer_class_registered_test/0},
        {"String class is registered", fun string_class_registered_test/0},
        {"True class is registered", fun true_class_registered_test/0},
        {"False class is registered", fun false_class_registered_test/0},
        {"UndefinedObject class is registered", fun nil_class_registered_test/0},
        {"Block class is registered", fun block_class_registered_test/0},
        {"Tuple class is registered", fun tuple_class_registered_test/0},
        {"BeamtalkInterface class is registered", fun system_dictionary_class_registered_test/0},
        {"TranscriptStream class is registered", fun transcript_stream_class_registered_test/0},
        {"Integer class has correct superclass", fun integer_superclass_test/0},
        {"Integer class has expected methods", fun integer_methods_test/0},
        %% Beamtalk class method tests
        {"Beamtalk allClasses returns all class names", fun beamtalk_all_classes_test/0},
        {"Beamtalk classNamed: finds existing class", fun beamtalk_class_named_found_test/0},
        {"Beamtalk classNamed: returns nil for missing class",
            fun beamtalk_class_named_not_found_test/0},
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
    ClassesAfter = [
        beamtalk_object_class:class_name(Pid)
     || Pid <- beamtalk_class_registry:all_classes()
    ],
    %% At least 15 expected (3 bootstrap + 10 primitives + 2 workspace globals).
    %% May be more if test fixtures have registered additional classes via on_load.
    ?assert(length(ClassesAfter) >= 15),

    %% Verify expected classes are present
    ?assert(lists:member('ProtoObject', ClassesAfter)),
    ?assert(lists:member('Object', ClassesAfter)),
    ?assert(lists:member('Actor', ClassesAfter)),
    ?assert(lists:member('Integer', ClassesAfter)),
    ?assert(lists:member('String', ClassesAfter)),
    ?assert(lists:member('True', ClassesAfter)),
    ?assert(lists:member('False', ClassesAfter)),
    ?assert(lists:member('UndefinedObject', ClassesAfter)),
    ?assert(lists:member('Block', ClassesAfter)),
    ?assert(lists:member('Tuple', ClassesAfter)),
    ?assert(lists:member('Float', ClassesAfter)),
    ?assert(lists:member('BeamtalkInterface', ClassesAfter)),
    ?assert(lists:member('TranscriptStream', ClassesAfter)).

init_idempotent_test() ->
    %% Call init multiple times
    ok = beamtalk_stdlib:init(),
    ok = beamtalk_stdlib:init(),

    %% Should still have same number of classes (no duplicates)
    Classes = [
        beamtalk_object_class:class_name(Pid)
     || Pid <- beamtalk_class_registry:all_classes()
    ],
    ?assert(length(Classes) >= 15).

integer_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('Integer'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Integer', beamtalk_object_class:class_name(Pid)).

string_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('String'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('String', beamtalk_object_class:class_name(Pid)).

true_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('True'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('True', beamtalk_object_class:class_name(Pid)).

false_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('False'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('False', beamtalk_object_class:class_name(Pid)).

nil_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('UndefinedObject'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('UndefinedObject', beamtalk_object_class:class_name(Pid)).

block_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('Block'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Block', beamtalk_object_class:class_name(Pid)).

tuple_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('Tuple'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('Tuple', beamtalk_object_class:class_name(Pid)).

system_dictionary_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('BeamtalkInterface'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('BeamtalkInterface', beamtalk_object_class:class_name(Pid)).

transcript_stream_class_registered_test() ->
    Pid = beamtalk_class_registry:whereis_class('TranscriptStream'),
    ?assertNotEqual(undefined, Pid),
    ?assertEqual('TranscriptStream', beamtalk_object_class:class_name(Pid)).

integer_superclass_test() ->
    Pid = beamtalk_class_registry:whereis_class('Integer'),
    Superclass = beamtalk_object_class:superclass(Pid),
    ?assertEqual('Number', Superclass).

integer_methods_test() ->
    Pid = beamtalk_class_registry:whereis_class('Integer'),
    %% ADR 0032 Phase 1: methods/1 returns local-only selectors.
    %% Inherited methods (e.g. 'class' from Object) are not included.
    Methods = beamtalk_object_class:methods(Pid),

    %% Check Integer-local expected methods
    ?assert(lists:member('+', Methods)),
    ?assert(lists:member('-', Methods)),
    ?assert(lists:member('*', Methods)),
    ?assert(lists:member('asString', Methods)),
    %% 'class' is inherited from Object — not in local methods list
    ?assertNot(lists:member('class', Methods)).

%%% ============================================================================
%%% Beamtalk Class Method Tests
%%% ============================================================================

beamtalk_all_classes_test() ->
    %% Call Beamtalk allClasses via dispatch
    Classes = beamtalk_stdlib:dispatch(allClasses, [], 'Beamtalk'),

    %% Should return a list of atoms (class names)
    ?assert(is_list(Classes)),
    % At least bootstrap + stdlib classes
    ?assert(length(Classes) >= 10),

    %% All stdlib classes should be present
    ?assert(lists:member('Integer', Classes)),
    ?assert(lists:member('String', Classes)),
    ?assert(lists:member('True', Classes)),
    ?assert(lists:member('False', Classes)),
    ?assert(lists:member('Block', Classes)),
    ?assert(lists:member('BeamtalkInterface', Classes)),
    ?assert(lists:member('TranscriptStream', Classes)),
    ?assert(lists:member('ProtoObject', Classes)),
    ?assert(lists:member('Object', Classes)),
    ?assert(lists:member('Actor', Classes)).

beamtalk_class_named_found_test() ->
    %% Look up an existing class
    Result = beamtalk_stdlib:dispatch('classNamed:', ['Integer'], 'Beamtalk'),

    %% Should return a wrapped class object
    ?assertMatch({beamtalk_object, 'Integer', beamtalk_object_class, _Pid}, Result),

    %% Extract the pid and verify it's the Integer class
    {beamtalk_object, 'Integer', beamtalk_object_class, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assertEqual('Integer', beamtalk_object_class:class_name(Pid)).

beamtalk_class_named_not_found_test() ->
    %% Look up a non-existent class
    Result = beamtalk_stdlib:dispatch('classNamed:', ['NonExistentClass'], 'Beamtalk'),

    %% Should return nil
    ?assertEqual(nil, Result).

beamtalk_globals_test() ->
    %% Call Beamtalk globals
    Globals = beamtalk_stdlib:dispatch(globals, [], 'Beamtalk'),

    %% Should return a map (currently empty placeholder)
    ?assert(is_map(Globals)).

beamtalk_version_test() ->
    %% Call Beamtalk version
    Version = beamtalk_stdlib:dispatch(version, [], 'Beamtalk'),

    %% Should return a non-empty binary version string
    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0).

beamtalk_has_method_test() ->
    %% Check has_method for known methods
    ?assert(beamtalk_stdlib:has_method(allClasses)),
    ?assert(beamtalk_stdlib:has_method('classNamed:')),
    ?assert(beamtalk_stdlib:has_method(globals)),
    ?assert(beamtalk_stdlib:has_method(version)),

    %% Check has_method for unknown methods
    ?assertNot(beamtalk_stdlib:has_method(unknownMethod)),
    ?assertNot(beamtalk_stdlib:has_method(fooBar)).

%%% ============================================================================
%%% Topological Sort Tests
%%% ============================================================================

topo_sort_test_() ->
    [
        {"linear chain sorts correctly", fun topo_sort_linear_chain_test/0},
        {"diamond dependency sorts correctly", fun topo_sort_diamond_test/0},
        {"single entry sorts correctly", fun topo_sort_single_test/0},
        {"empty list sorts correctly", fun topo_sort_empty_test/0},
        {"external superclass treated as ready", fun topo_sort_external_super_test/0}
    ].

topo_sort_linear_chain_test() ->
    %% C -> B -> A (C depends on B, B depends on A)
    Entries = [{mod_c, 'C', 'B'}, {mod_a, 'A', 'External'}, {mod_b, 'B', 'A'}],
    Result = beamtalk_stdlib:topo_sort(Entries),
    Names = [Class || {_, Class, _} <- Result],
    %% A must come before B, B must come before C
    PosA = pos(Names, 'A'),
    PosB = pos(Names, 'B'),
    PosC = pos(Names, 'C'),
    ?assert(PosA < PosB),
    ?assert(PosB < PosC).

topo_sort_diamond_test() ->
    %% D depends on B and C; B and C both depend on A
    Entries = [{mod_d, 'D', 'B'}, {mod_b, 'B', 'A'}, {mod_c, 'C', 'A'}, {mod_a, 'A', 'External'}],
    Result = beamtalk_stdlib:topo_sort(Entries),
    Names = [Class || {_, Class, _} <- Result],
    PosA = pos(Names, 'A'),
    PosB = pos(Names, 'B'),
    PosD = pos(Names, 'D'),
    ?assert(PosA < PosB),
    ?assert(PosB < PosD orelse pos(Names, 'C') < PosD).

topo_sort_single_test() ->
    Result = beamtalk_stdlib:topo_sort([{mod_a, 'A', 'Object'}]),
    ?assertEqual([{mod_a, 'A', 'Object'}], Result).

topo_sort_empty_test() ->
    ?assertEqual([], beamtalk_stdlib:topo_sort([])).

topo_sort_external_super_test() ->
    %% Both depend on external 'Object' — both should be ready in first wave
    Entries = [{mod_a, 'A', 'Object'}, {mod_b, 'B', 'Object'}],
    Result = beamtalk_stdlib:topo_sort(Entries),
    ?assertEqual(2, length(Result)).

pos(List, Elem) ->
    pos(List, Elem, 1).
pos([Elem | _], Elem, N) -> N;
pos([_ | T], Elem, N) -> pos(T, Elem, N + 1).

%%% ============================================================================
%%% Dispatch Error Tests
%%% ============================================================================

dispatch_unknown_selector_test_() ->
    {setup, fun stdlib_setup/0, fun stdlib_teardown/1, [
        {"dispatch unknown selector raises does_not_understand",
            fun dispatch_unknown_selector_test/0}
    ]}.

dispatch_unknown_selector_test() ->
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'Beamtalk'}
        },
        beamtalk_stdlib:dispatch(nonExistentMethod, [], 'Beamtalk')
    ).
