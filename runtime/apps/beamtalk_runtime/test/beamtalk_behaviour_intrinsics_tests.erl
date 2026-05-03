%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_behaviour_intrinsics module (BT-1088, BT-1959).

Tests intrinsic functions for class reflection: metaclassNew, classClass,
className, classLocalMethods, classFieldNames, classIncludesSelector,
classDoc, classSetDoc, classSetMethodDoc, classSubclasses, classAllSubclasses,
classMethods (including inheritance and deduplication), classCanUnderstand
(including inherited selectors), classInheritsFrom (transitive), classIncludesBehaviour,
classWhichIncludesSelector (inherited), classAllFieldNames (inherited),
classRemoveFromSystem, classRemoveFromSystemByName, metaclass primitives
(thisClass, classMethods, localClassMethods, includesSelector, allMethods).

Uses a minimal setup (pg + ETS hierarchy table) with dynamically-created
test classes via beamtalk_class_builder.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Setup / Teardown
%%% ============================================================================

setup() ->
    case pg:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    [].

teardown(ClassNames) ->
    lists:foreach(
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        ClassNames
    ).

%% Create a class and return {ClassObj, Pid}.
register_class(ClassName, FieldSpecs, MethodSpecs) ->
    State = #{
        className => ClassName,
        superclassRef => 'Object',
        fieldSpecs => FieldSpecs,
        methodSpecs => MethodSpecs
    },
    {ok, Pid} = beamtalk_class_builder:register(State),
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    Module = beamtalk_object_class:module_name(Pid),
    ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
    {ClassObj, Pid}.

%% BT-1635: Create a class with class methods and return {ClassObj, Pid}.
register_class_with_class_methods(ClassName, FieldSpecs, MethodSpecs, ClassMethodSpecs) ->
    State = #{
        className => ClassName,
        superclassRef => 'Object',
        fieldSpecs => FieldSpecs,
        methodSpecs => MethodSpecs,
        classMethods => ClassMethodSpecs
    },
    {ok, Pid} = beamtalk_class_builder:register(State),
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    Module = beamtalk_object_class:module_name(Pid),
    ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
    {ClassObj, Pid}.

%% Create a class with a specific superclass and return {ClassObj, Pid}.
register_class_with_super(ClassName, SuperclassRef, FieldSpecs, MethodSpecs) ->
    State = #{
        className => ClassName,
        superclassRef => SuperclassRef,
        fieldSpecs => FieldSpecs,
        methodSpecs => MethodSpecs
    },
    {ok, Pid} = beamtalk_class_builder:register(State),
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    Module = beamtalk_object_class:module_name(Pid),
    ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
    {ClassObj, Pid}.

%%% ============================================================================
%%% metaclassNew/0 — always raises user_error
%%% ============================================================================

metaclass_new_raises_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = user_error}},
        beamtalk_behaviour_intrinsics:metaclassNew()
    ).

%%% ============================================================================
%%% classClass/1 — returns a Metaclass-tagged object
%%% ============================================================================

class_class_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiClassClass', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ?assertEqual('Metaclass', MetaObj#beamtalk_object.class),
                    ?assertEqual(Pid, MetaObj#beamtalk_object.pid)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% className/1
%%% ============================================================================

class_name_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiClassName', #{}, #{}),
                try
                    ?assertEqual(
                        'BT1088BiClassName',
                        beamtalk_behaviour_intrinsics:className(ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classLocalMethods/1
%%% ============================================================================

class_local_methods_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiNoMethods', #{}, #{}),
                try
                    Methods = beamtalk_behaviour_intrinsics:classLocalMethods(ClassObj),
                    ?assert(is_list(Methods)),
                    ?assertEqual([], Methods)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_local_methods_with_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1088BiWithMethods',
                    #{},
                    #{'getValue' => MethodFun, 'setValue:' => MethodFun}
                ),
                try
                    Methods = beamtalk_behaviour_intrinsics:classLocalMethods(ClassObj),
                    ?assert(lists:member('getValue', Methods)),
                    ?assert(lists:member('setValue:', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% BT-1635: classLocalMethods on metaclass object returns class methods
class_local_methods_metaclass_with_class_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {_ClassObj, Pid} = register_class_with_class_methods(
                    'BT1635MetaHasMethods',
                    #{},
                    #{},
                    #{'myClassMethod' => ClassMethodFun, 'otherClassMethod' => ClassMethodFun}
                ),
                try
                    %% Build metaclass object: class = 'Metaclass', same pid
                    MetaObj = #beamtalk_object{
                        class = 'Metaclass',
                        class_mod = beamtalk_metaclass_bt,
                        pid = Pid
                    },
                    Methods = beamtalk_behaviour_intrinsics:classLocalMethods(MetaObj),
                    ?assert(is_list(Methods)),
                    ?assert(lists:member('myClassMethod', Methods)),
                    ?assert(lists:member('otherClassMethod', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% BT-1635: classLocalMethods on metaclass with no class methods returns []
class_local_methods_metaclass_no_class_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ClassObj, Pid} = register_class(
                    'BT1635MetaNoMethods',
                    #{},
                    #{'instanceMethod' => MethodFun}
                ),
                try
                    MetaObj = #beamtalk_object{
                        class = 'Metaclass',
                        class_mod = beamtalk_metaclass_bt,
                        pid = Pid
                    },
                    Methods = beamtalk_behaviour_intrinsics:classLocalMethods(MetaObj),
                    ?assert(is_list(Methods)),
                    ?assertEqual([], Methods),
                    %% Verify instance methods are NOT returned
                    ?assertNot(lists:member('instanceMethod', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% BT-1635: classIncludesSelector on metaclass checks class methods
class_includes_selector_metaclass_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {_ClassObj, Pid} = register_class_with_class_methods(
                    'BT1635MetaIncludes',
                    #{},
                    #{},
                    #{'myClassMethod' => ClassMethodFun}
                ),
                try
                    MetaObj = #beamtalk_object{
                        class = 'Metaclass',
                        class_mod = beamtalk_metaclass_bt,
                        pid = Pid
                    },
                    ?assert(
                        beamtalk_behaviour_intrinsics:classIncludesSelector(
                            MetaObj, 'myClassMethod'
                        )
                    ),
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classIncludesSelector(
                            MetaObj, 'bogusMethod'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classFieldNames/1
%%% ============================================================================

class_field_names_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiNoFields', #{}, #{}),
                try
                    Fields = beamtalk_behaviour_intrinsics:classFieldNames(ClassObj),
                    ?assert(is_list(Fields)),
                    ?assertEqual([], Fields)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_field_names_with_fields_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class(
                    'BT1088BiWithFields',
                    #{x => 0, y => 0},
                    #{}
                ),
                try
                    Fields = beamtalk_behaviour_intrinsics:classFieldNames(ClassObj),
                    ?assert(is_list(Fields)),
                    ?assert(lists:member(x, Fields)),
                    ?assert(lists:member(y, Fields))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classIncludesSelector/2
%%% ============================================================================

class_includes_selector_true_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1088BiSelectorTrue',
                    #{},
                    #{'increment' => MethodFun}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classIncludesSelector(ClassObj, 'increment')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_includes_selector_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiSelectorFalse', #{}, #{}),
                try
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classIncludesSelector(ClassObj, 'nonExistent')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classDoc/1 and classSetDoc/2
%%% ============================================================================

class_doc_nil_initially_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiDocNil', #{}, #{}),
                try
                    ?assertEqual(nil, beamtalk_behaviour_intrinsics:classDoc(ClassObj))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_set_doc_and_get_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiDocSet', #{}, #{}),
                try
                    Doc = <<"This is a test class">>,
                    RetObj = beamtalk_behaviour_intrinsics:classSetDoc(ClassObj, Doc),
                    ?assertEqual(ClassObj, RetObj),
                    ?assertEqual(Doc, beamtalk_behaviour_intrinsics:classDoc(ClassObj))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classSetMethodDoc/3 and classDocForMethod/2
%%% ============================================================================

class_set_method_doc_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1088BiMethodDoc',
                    #{},
                    #{'myMethod' => MethodFun}
                ),
                try
                    Doc = <<"Does something useful">>,
                    RetObj = beamtalk_behaviour_intrinsics:classSetMethodDoc(
                        ClassObj, 'myMethod', Doc
                    ),
                    ?assertEqual(ClassObj, RetObj),
                    ?assertEqual(
                        Doc,
                        beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'myMethod')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_doc_for_nonexistent_method_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiDocForMethod', #{}, #{}),
                try
                    ?assertEqual(
                        nil,
                        beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'nosuchMethod')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassSuperclass/1 — via gen_server:call(Pid, superclass) (BT-1186)
%%% ============================================================================

%% BT-1186: metaclassSuperclass/1 now calls gen_server:call(Pid, superclass)
%% directly (BT-1185 ensures the gen_server always holds the correct superclass).
metaclass_superclass_returns_metaclass_or_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1169BiSuperclassNoMeta', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    Result = beamtalk_behaviour_intrinsics:metaclassSuperclass(MetaObj),
                    %% If Object is registered (full test suite), returns its metaclass;
                    %% otherwise nil.
                    case beamtalk_class_registry:whereis_class('Object') of
                        undefined ->
                            ?assertEqual(nil, Result);
                        _ ->
                            ?assertMatch(#beamtalk_object{class = 'Metaclass'}, Result)
                    end
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

metaclass_superclass_with_registered_parent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ParentObj, ParentPid} = register_class('BT1792BiMetaSuperParent', #{}, #{}),
                {ClassObj, ChildPid} = register_class_with_super(
                    'BT1792BiMetaSuperChild', 'BT1792BiMetaSuperParent', #{}, #{}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    Result = beamtalk_behaviour_intrinsics:metaclassSuperclass(MetaObj),
                    %% Should return the metaclass of the parent
                    ?assertMatch(#beamtalk_object{class = 'Metaclass'}, Result)
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classSubclasses/1
%%% ============================================================================

class_subclasses_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiLeafClass', #{}, #{}),
                try
                    Subclasses = beamtalk_behaviour_intrinsics:classSubclasses(ClassObj),
                    ?assert(is_list(Subclasses)),
                    ?assertEqual([], Subclasses)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classSuperclass/1
%%% ============================================================================

class_superclass_returns_object_or_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiSuperclass', #{}, #{}),
                try
                    Super = beamtalk_behaviour_intrinsics:classSuperclass(ClassObj),
                    %% Superclass is 'Object'. If Object is registered (full test suite),
                    %% we get a class object; if not (isolated eunit), we get nil.
                    case beamtalk_class_registry:whereis_class('Object') of
                        undefined ->
                            ?assertEqual(nil, Super);
                        _ ->
                            ?assertMatch(#beamtalk_object{}, Super),
                            ?assertEqual(
                                'Object',
                                beamtalk_behaviour_intrinsics:className(Super)
                            )
                    end
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_superclass_with_registered_parent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ParentObj, ParentPid} = register_class('BT1792BiSuperParent', #{}, #{}),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1792BiSuperChild', 'BT1792BiSuperParent', #{}, #{}
                ),
                try
                    Super = beamtalk_behaviour_intrinsics:classSuperclass(ChildObj),
                    ?assertMatch(#beamtalk_object{}, Super),
                    ?assertEqual(
                        'BT1792BiSuperParent',
                        beamtalk_behaviour_intrinsics:className(Super)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classAllSuperclasses/1
%%% ============================================================================

class_all_superclasses_returns_list_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiAllSupers', #{}, #{}),
                try
                    Supers = beamtalk_behaviour_intrinsics:classAllSuperclasses(ClassObj),
                    ?assert(is_list(Supers))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_all_superclasses_with_hierarchy_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_GrandObj, GrandPid} = register_class('BT1792BiGrand', #{}, #{}),
                {_ParentObj, ParentPid} = register_class_with_super(
                    'BT1792BiMiddle', 'BT1792BiGrand', #{}, #{}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1792BiBottom', 'BT1792BiMiddle', #{}, #{}
                ),
                try
                    Supers = beamtalk_behaviour_intrinsics:classAllSuperclasses(ChildObj),
                    SuperNames = [beamtalk_behaviour_intrinsics:className(S) || S <- Supers],
                    ?assert(lists:member('BT1792BiMiddle', SuperNames)),
                    ?assert(lists:member('BT1792BiGrand', SuperNames))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classAllSubclasses/1
%%% ============================================================================

class_all_subclasses_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiAllSubEmpty', #{}, #{}),
                try
                    AllSub = beamtalk_behaviour_intrinsics:classAllSubclasses(ClassObj),
                    ?assert(is_list(AllSub)),
                    ?assertEqual([], AllSub)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_subclasses_with_child_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ParentObj, ParentPid} = register_class('BT1792BiParent', #{}, #{}),
                {_ChildObj, ChildPid} = register_class_with_super(
                    'BT1792BiChild', 'BT1792BiParent', #{}, #{}
                ),
                try
                    Subclasses = beamtalk_behaviour_intrinsics:classSubclasses(ParentObj),
                    SubNames = [beamtalk_behaviour_intrinsics:className(S) || S <- Subclasses],
                    ?assert(lists:member('BT1792BiChild', SubNames)),
                    %% Also test allSubclasses
                    AllSub = beamtalk_behaviour_intrinsics:classAllSubclasses(ParentObj),
                    AllSubNames = [beamtalk_behaviour_intrinsics:className(S) || S <- AllSub],
                    ?assert(lists:member('BT1792BiChild', AllSubNames))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classMethods/1 — full inheritance chain
%%% ============================================================================

class_methods_includes_local_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1792BiMethodsFull',
                    #{},
                    #{'foo' => MethodFun, 'bar' => MethodFun}
                ),
                try
                    Methods = beamtalk_behaviour_intrinsics:classMethods(ClassObj),
                    ?assert(is_list(Methods)),
                    ?assert(lists:member('foo', Methods)),
                    ?assert(lists:member('bar', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classCanUnderstand/2
%%% ============================================================================

class_can_understand_true_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1792BiCanUnderstand',
                    #{},
                    #{'quux' => MethodFun}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classCanUnderstand(ClassObj, 'quux')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_can_understand_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiCanUnderstandNo', #{}, #{}),
                try
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classCanUnderstand(ClassObj, 'nope')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classCanUnderstandFromName/2
%%% ============================================================================

class_can_understand_from_name_true_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ClassObj, Pid} = register_class(
                    'BT1792BiCanUnderstandName',
                    #{},
                    #{'ping' => MethodFun}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                            'BT1792BiCanUnderstandName', 'ping'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_can_understand_from_name_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ClassObj, Pid} = register_class('BT1792BiCanUnderstandNameNo', #{}, #{}),
                try
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                            'BT1792BiCanUnderstandNameNo', 'missing'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_can_understand_from_name_unregistered_test() ->
    %% An unregistered class name should return false
    ?assertNot(
        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
            'BT1792NoSuchClass', 'anything'
        )
    ).

%%% ============================================================================
%%% classInheritsFrom/2
%%% ============================================================================

class_inherits_from_direct_parent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ParentObj, ParentPid} = register_class('BT1792BiInheritsParent', #{}, #{}),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1792BiInheritsChild', 'BT1792BiInheritsParent', #{}, #{}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classInheritsFrom(ChildObj, ParentObj)
                    ),
                    %% Parent does NOT inherit from child
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classInheritsFrom(ParentObj, ChildObj)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classIncludesBehaviour/2
%%% ============================================================================

class_includes_behaviour_self_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiIncludesBeh', #{}, #{}),
                try
                    %% A class includes itself as a behaviour
                    ?assert(
                        beamtalk_behaviour_intrinsics:classIncludesBehaviour(ClassObj, ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_includes_behaviour_ancestor_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ParentObj, ParentPid} = register_class('BT1792BiBehParent', #{}, #{}),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1792BiBehChild', 'BT1792BiBehParent', #{}, #{}
                ),
                try
                    %% Child includes parent
                    ?assert(
                        beamtalk_behaviour_intrinsics:classIncludesBehaviour(ChildObj, ParentObj)
                    ),
                    %% Parent does NOT include child
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classIncludesBehaviour(ParentObj, ChildObj)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classWhichIncludesSelector/2
%%% ============================================================================

class_which_includes_selector_found_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1792BiWhichInclSel',
                    #{},
                    #{'findMe' => MethodFun}
                ),
                try
                    Result = beamtalk_behaviour_intrinsics:classWhichIncludesSelector(
                        ClassObj, 'findMe'
                    ),
                    ?assertMatch(#beamtalk_object{}, Result),
                    ?assertEqual(
                        'BT1792BiWhichInclSel',
                        beamtalk_behaviour_intrinsics:className(Result)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_which_includes_selector_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiWhichInclNil', #{}, #{}),
                try
                    Result = beamtalk_behaviour_intrinsics:classWhichIncludesSelector(
                        ClassObj, 'noSuchMethod'
                    ),
                    ?assertEqual(nil, Result)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classAllFieldNames/1
%%% ============================================================================

class_all_field_names_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class(
                    'BT1792BiAllFields',
                    #{x => 0, y => 0},
                    #{}
                ),
                try
                    AllFields = beamtalk_behaviour_intrinsics:classAllFieldNames(ClassObj),
                    ?assert(is_list(AllFields)),
                    ?assert(lists:member(x, AllFields)),
                    ?assert(lists:member(y, AllFields))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_all_field_names_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiAllFieldsEmpty', #{}, #{}),
                try
                    AllFields = beamtalk_behaviour_intrinsics:classAllFieldNames(ClassObj),
                    ?assert(is_list(AllFields))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassThisClass/1
%%% ============================================================================

metaclass_this_class_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiMetaThisClass', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ThisClass = beamtalk_behaviour_intrinsics:metaclassThisClass(MetaObj),
                    ?assertMatch(#beamtalk_object{}, ThisClass),
                    ?assertEqual(
                        'BT1792BiMetaThisClass',
                        beamtalk_behaviour_intrinsics:className(ThisClass)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassAllMethods/1
%%% ============================================================================

metaclass_all_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {ClassObj, Pid} = register_class_with_class_methods(
                    'BT1792BiMetaAllMethods',
                    #{},
                    #{},
                    #{'myClassFn' => ClassMethodFun}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    AllMethods = beamtalk_behaviour_intrinsics:metaclassAllMethods(MetaObj),
                    ?assert(is_list(AllMethods)),
                    %% Should include the class method we defined
                    ?assert(lists:member('myClassFn', AllMethods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassClassMethods/1
%%% ============================================================================

metaclass_class_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {ClassObj, Pid} = register_class_with_class_methods(
                    'BT1792BiMetaClassMethods',
                    #{},
                    #{},
                    #{'classM1' => ClassMethodFun, 'classM2' => ClassMethodFun}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ClassMethods = beamtalk_behaviour_intrinsics:metaclassClassMethods(MetaObj),
                    ?assert(is_list(ClassMethods)),
                    ?assert(lists:member('classM1', ClassMethods)),
                    ?assert(lists:member('classM2', ClassMethods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassLocalClassMethods/1
%%% ============================================================================

metaclass_local_class_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {ClassObj, Pid} = register_class_with_class_methods(
                    'BT1792BiMetaLocalClassM',
                    #{},
                    #{},
                    #{'localCM' => ClassMethodFun}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    LocalCM = beamtalk_behaviour_intrinsics:metaclassLocalClassMethods(MetaObj),
                    ?assert(is_list(LocalCM)),
                    ?assert(lists:member('localCM', LocalCM))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

metaclass_local_class_methods_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiMetaLocalEmpty', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    LocalCM = beamtalk_behaviour_intrinsics:metaclassLocalClassMethods(MetaObj),
                    ?assert(is_list(LocalCM)),
                    ?assertEqual([], LocalCM)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% metaclassIncludesSelector/2
%%% ============================================================================

metaclass_includes_selector_true_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {ClassObj, Pid} = register_class_with_class_methods(
                    'BT1792BiMetaInclSel',
                    #{},
                    #{},
                    #{'checkMe' => ClassMethodFun}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ?assert(
                        beamtalk_behaviour_intrinsics:metaclassIncludesSelector(
                            MetaObj, 'checkMe'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

metaclass_includes_selector_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiMetaInclSelNo', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:metaclassIncludesSelector(
                            MetaObj, 'nope'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classConformsTo/2 and classProtocols/1
%%% ============================================================================

class_conforms_to_unknown_protocol_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiConformsTo', #{}, #{}),
                try
                    %% Unknown protocol returns false (BT-2136)
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classConformsTo(
                            ClassObj, 'NoSuchProtocol'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

class_protocols_returns_list_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiProtocols', #{}, #{}),
                try
                    Protocols = beamtalk_behaviour_intrinsics:classProtocols(ClassObj),
                    ?assert(is_list(Protocols))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classSourceFile/1
%%% ============================================================================

class_source_file_nil_for_dynamic_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiSourceFile', #{}, #{}),
                try
                    %% Dynamic classes have no source file
                    ?assertEqual(
                        nil,
                        beamtalk_behaviour_intrinsics:classSourceFile(ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% classClass/1 — idempotent on metaclass objects
%%% ============================================================================

class_class_idempotent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1792BiClassClassIdem', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    MetaMetaObj = beamtalk_behaviour_intrinsics:classClass(MetaObj),
                    %% classClass is idempotent — MetaObj == MetaMetaObj
                    ?assertEqual('Metaclass', MetaMetaObj#beamtalk_object.class),
                    ?assertEqual(
                        MetaObj#beamtalk_object.pid,
                        MetaMetaObj#beamtalk_object.pid
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% BT-1959: Additional coverage tests
%%% ============================================================================

%%% --- classAllSubclasses/1 — multi-level hierarchy ---

class_all_subclasses_multi_level_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {GrandObj, GrandPid} = register_class('BT1959AllSubGrand', #{}, #{}),
                {_ParentObj, ParentPid} = register_class_with_super(
                    'BT1959AllSubParent', 'BT1959AllSubGrand', #{}, #{}
                ),
                {_ChildObj, ChildPid} = register_class_with_super(
                    'BT1959AllSubChild', 'BT1959AllSubParent', #{}, #{}
                ),
                try
                    AllSub = beamtalk_behaviour_intrinsics:classAllSubclasses(GrandObj),
                    AllSubNames = [beamtalk_behaviour_intrinsics:className(S) || S <- AllSub],
                    ?assert(lists:member('BT1959AllSubParent', AllSubNames)),
                    ?assert(lists:member('BT1959AllSubChild', AllSubNames)),
                    ?assertEqual(2, length(AllSub))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classMethods/1 — inherited methods from parent ---

class_methods_inherits_from_parent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ParentObj, ParentPid} = register_class(
                    'BT1959MethodsParent',
                    #{},
                    #{'parentMethod' => MethodFun}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959MethodsChild',
                    'BT1959MethodsParent',
                    #{},
                    #{'childMethod' => MethodFun}
                ),
                try
                    Methods = beamtalk_behaviour_intrinsics:classMethods(ChildObj),
                    ?assert(lists:member('childMethod', Methods)),
                    ?assert(lists:member('parentMethod', Methods))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%% BT-1635: classMethods on metaclass receiver collects class methods
class_methods_metaclass_receiver_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {_ClassObj, Pid} = register_class_with_class_methods(
                    'BT1959MethodsMeta',
                    #{},
                    #{},
                    #{'metaFoo' => ClassMethodFun}
                ),
                try
                    MetaObj = #beamtalk_object{
                        class = 'Metaclass',
                        class_mod = beamtalk_metaclass_bt,
                        pid = Pid
                    },
                    Methods = beamtalk_behaviour_intrinsics:classMethods(MetaObj),
                    ?assert(is_list(Methods)),
                    ?assert(lists:member('metaFoo', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classCanUnderstand/2 — inherited selectors ---

class_can_understand_inherited_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ParentObj, ParentPid} = register_class(
                    'BT1959CanUndParent',
                    #{},
                    #{'inherited' => MethodFun}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959CanUndChild', 'BT1959CanUndParent', #{}, #{}
                ),
                try
                    %% Child can understand parent's method via inheritance
                    ?assert(
                        beamtalk_behaviour_intrinsics:classCanUnderstand(ChildObj, 'inherited')
                    ),
                    %% But not a nonexistent method
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classCanUnderstand(ChildObj, 'bogus')
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classCanUnderstandFromName/2 — inherited selectors ---

class_can_understand_from_name_inherited_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ParentObj, ParentPid} = register_class(
                    'BT1959CanUndNameParent',
                    #{},
                    #{'fromParent' => MethodFun}
                ),
                {_ChildObj, ChildPid} = register_class_with_super(
                    'BT1959CanUndNameChild', 'BT1959CanUndNameParent', #{}, #{}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                            'BT1959CanUndNameChild', 'fromParent'
                        )
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classInheritsFrom/2 — transitive (grandparent) ---

class_inherits_from_grandparent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {GrandObj, GrandPid} = register_class('BT1959InheritsGrand', #{}, #{}),
                {_ParentObj, ParentPid} = register_class_with_super(
                    'BT1959InheritsMiddle', 'BT1959InheritsGrand', #{}, #{}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959InheritsBottom', 'BT1959InheritsMiddle', #{}, #{}
                ),
                try
                    %% Grandchild inherits from grandparent
                    ?assert(
                        beamtalk_behaviour_intrinsics:classInheritsFrom(ChildObj, GrandObj)
                    ),
                    %% Grandparent does NOT inherit from grandchild
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classInheritsFrom(GrandObj, ChildObj)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%% A class does NOT inherit from itself (strict)
class_inherits_from_self_is_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959InheritsSelf', #{}, #{}),
                try
                    ?assertNot(
                        beamtalk_behaviour_intrinsics:classInheritsFrom(ClassObj, ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classIncludesBehaviour/2 — transitive (grandparent) ---

class_includes_behaviour_grandparent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {GrandObj, GrandPid} = register_class('BT1959BehGrand', #{}, #{}),
                {_ParentObj, ParentPid} = register_class_with_super(
                    'BT1959BehMiddle', 'BT1959BehGrand', #{}, #{}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959BehBottom', 'BT1959BehGrand', #{}, #{}
                ),
                try
                    ?assert(
                        beamtalk_behaviour_intrinsics:classIncludesBehaviour(ChildObj, GrandObj)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classWhichIncludesSelector/2 — found in parent ---

class_which_includes_selector_in_parent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ParentObj, ParentPid} = register_class(
                    'BT1959WhichInclParent',
                    #{},
                    #{'parentOnly' => MethodFun}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959WhichInclChild', 'BT1959WhichInclParent', #{}, #{}
                ),
                try
                    Result = beamtalk_behaviour_intrinsics:classWhichIncludesSelector(
                        ChildObj, 'parentOnly'
                    ),
                    ?assertMatch(#beamtalk_object{}, Result),
                    ?assertEqual(
                        'BT1959WhichInclParent',
                        beamtalk_behaviour_intrinsics:className(Result)
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classAllFieldNames/1 — inherited fields from parent ---

class_all_field_names_inherited_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ParentObj, ParentPid} = register_class(
                    'BT1959FieldsParent',
                    #{x => 0},
                    #{}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959FieldsChild',
                    'BT1959FieldsParent',
                    #{y => 0},
                    #{}
                ),
                try
                    AllFields = beamtalk_behaviour_intrinsics:classAllFieldNames(ChildObj),
                    ?assert(lists:member(x, AllFields)),
                    ?assert(lists:member(y, AllFields))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classSetDoc/2 — overwrite existing doc ---

class_set_doc_overwrite_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959DocOverwrite', #{}, #{}),
                try
                    Doc1 = <<"First doc">>,
                    Doc2 = <<"Updated doc">>,
                    beamtalk_behaviour_intrinsics:classSetDoc(ClassObj, Doc1),
                    ?assertEqual(Doc1, beamtalk_behaviour_intrinsics:classDoc(ClassObj)),
                    beamtalk_behaviour_intrinsics:classSetDoc(ClassObj, Doc2),
                    ?assertEqual(Doc2, beamtalk_behaviour_intrinsics:classDoc(ClassObj))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classDocForMethod/2 — method exists but no doc ---

class_doc_for_method_no_doc_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1959DocNoDoc',
                    #{},
                    #{'undocumented' => MethodFun}
                ),
                try
                    %% Method exists but has no doc set — should return nil
                    ?assertEqual(
                        nil,
                        beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'undocumented')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classSetMethodDoc/3 — overwrite existing method doc ---

class_set_method_doc_overwrite_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {ClassObj, Pid} = register_class(
                    'BT1959MethodDocOvr',
                    #{},
                    #{'documented' => MethodFun}
                ),
                try
                    Doc1 = <<"First method doc">>,
                    Doc2 = <<"Updated method doc">>,
                    beamtalk_behaviour_intrinsics:classSetMethodDoc(ClassObj, 'documented', Doc1),
                    ?assertEqual(
                        Doc1,
                        beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'documented')
                    ),
                    beamtalk_behaviour_intrinsics:classSetMethodDoc(ClassObj, 'documented', Doc2),
                    ?assertEqual(
                        Doc2,
                        beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'documented')
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classRemoveFromSystemByName/1 — class not found ---

class_remove_from_system_by_name_not_found_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = class_not_found}},
        beamtalk_behaviour_intrinsics:classRemoveFromSystemByName('BT1959NoSuchClass')
    ).

%%% --- classRemoveFromSystemByName/1 — has subclasses ---

class_remove_from_system_has_subclasses_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ParentObj, ParentPid} = register_class('BT1959RemoveParent', #{}, #{}),
                {_ChildObj, ChildPid} = register_class_with_super(
                    'BT1959RemoveChild', 'BT1959RemoveParent', #{}, #{}
                ),
                try
                    %% Should raise because parent has subclasses
                    ?assertError(
                        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
                        beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(
                            'BT1959RemoveParent'
                        )
                    )
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classRemoveFromSystem/1 — dynamic class hits code:delete failure ---
%%% Dynamic classes (class_builder) can't be code:delete'd, so removal raises
%%% a runtime_error at the delete step. This test verifies the error path.

class_remove_from_system_code_delete_fails_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959RemoveCodeDel', #{}, #{}),
                try
                    %% Verify class exists before removal attempt
                    ?assertNotEqual(
                        undefined,
                        beamtalk_class_registry:whereis_class('BT1959RemoveCodeDel')
                    ),
                    %% Dynamic class removal fails at code:delete — raises runtime_error
                    ?assertError(
                        #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{kind = runtime_error}
                        },
                        beamtalk_behaviour_intrinsics:classRemoveFromSystem(ClassObj)
                    )
                after
                    %% Class gen_server was stopped by removeFromSystem before error;
                    %% stop is a no-op if already dead
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classRemoveFromSystemByName/1 — dynamic class hits code:delete failure ---

class_remove_from_system_by_name_code_delete_fails_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_ClassObj, Pid} = register_class('BT1959RemoveByNameFail', #{}, #{}),
                try
                    ?assertNotEqual(
                        undefined,
                        beamtalk_class_registry:whereis_class('BT1959RemoveByNameFail')
                    ),
                    ?assertError(
                        #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{kind = runtime_error}
                        },
                        beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(
                            'BT1959RemoveByNameFail'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classMethods/1 — empty class returns list ---

class_methods_empty_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959MethodsEmpty', #{}, #{}),
                try
                    Methods = beamtalk_behaviour_intrinsics:classMethods(ClassObj),
                    ?assert(is_list(Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classAllSuperclasses/1 — order is immediate parent to root ---

class_all_superclasses_order_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_GrandObj, GrandPid} = register_class('BT1959OrderGrand', #{}, #{}),
                {_ParentObj, ParentPid} = register_class_with_super(
                    'BT1959OrderMiddle', 'BT1959OrderGrand', #{}, #{}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959OrderBottom', 'BT1959OrderMiddle', #{}, #{}
                ),
                try
                    Supers = beamtalk_behaviour_intrinsics:classAllSuperclasses(ChildObj),
                    SuperNames = [beamtalk_behaviour_intrinsics:className(S) || S <- Supers],
                    %% First element should be immediate parent
                    [First | _] = SuperNames,
                    ?assertEqual('BT1959OrderMiddle', First)
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classSubclasses/1 — multiple children ---

class_subclasses_multiple_children_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ParentObj, ParentPid} = register_class('BT1959MultiParent', #{}, #{}),
                {_Child1Obj, Child1Pid} = register_class_with_super(
                    'BT1959MultiChild1', 'BT1959MultiParent', #{}, #{}
                ),
                {_Child2Obj, Child2Pid} = register_class_with_super(
                    'BT1959MultiChild2', 'BT1959MultiParent', #{}, #{}
                ),
                try
                    Subclasses = beamtalk_behaviour_intrinsics:classSubclasses(ParentObj),
                    SubNames = [beamtalk_behaviour_intrinsics:className(S) || S <- Subclasses],
                    ?assert(lists:member('BT1959MultiChild1', SubNames)),
                    ?assert(lists:member('BT1959MultiChild2', SubNames)),
                    ?assertEqual(2, length(Subclasses))
                after
                    (try
                        gen_server:stop(Child1Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(Child2Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- metaclassClassMethods/1 — inherited class methods ---

metaclass_class_methods_inherited_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassMethodFun = fun(_Self, ClassVars) -> {reply, ok, ClassVars} end,
                {_ParentObj, ParentPid} = register_class_with_class_methods(
                    'BT1959MetaCMParent',
                    #{},
                    #{},
                    #{'parentCM' => ClassMethodFun}
                ),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959MetaCMChild', 'BT1959MetaCMParent', #{}, #{}
                ),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ChildObj),
                    ClassMethods = beamtalk_behaviour_intrinsics:metaclassClassMethods(MetaObj),
                    ?assert(is_list(ClassMethods)),
                    ?assert(lists:member('parentCM', ClassMethods))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- metaclassThisClass/1 — round-trip identity ---

metaclass_this_class_round_trip_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959MetaRoundTrip', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ThisClass = beamtalk_behaviour_intrinsics:metaclassThisClass(MetaObj),
                    %% Round-trip: classClass -> metaclassThisClass returns same name
                    ?assertEqual(
                        beamtalk_behaviour_intrinsics:className(ClassObj),
                        beamtalk_behaviour_intrinsics:className(ThisClass)
                    ),
                    %% Same pid
                    ?assertEqual(
                        ClassObj#beamtalk_object.pid,
                        ThisClass#beamtalk_object.pid
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classClass/1 — class_mod is beamtalk_metaclass_bt ---

class_class_module_is_metaclass_bt_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1959ClassMod', #{}, #{}),
                try
                    MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                    ?assertEqual(beamtalk_metaclass_bt, MetaObj#beamtalk_object.class_mod)
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classMethods/1 — deduplication across hierarchy ---

class_methods_deduplicates_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(_Self, _Args, State) -> {reply, ok, State, _Self} end,
                {_ParentObj, ParentPid} = register_class(
                    'BT1959DedupParent',
                    #{},
                    #{'shared' => MethodFun, 'parentOnly' => MethodFun}
                ),
                %% Child overrides 'shared'
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959DedupChild',
                    'BT1959DedupParent',
                    #{},
                    #{'shared' => MethodFun, 'childOnly' => MethodFun}
                ),
                try
                    Methods = beamtalk_behaviour_intrinsics:classMethods(ChildObj),
                    %% 'shared' should appear only once (ordset dedup)
                    SharedCount = length([M || M <- Methods, M =:= 'shared']),
                    ?assertEqual(1, SharedCount),
                    ?assert(lists:member('parentOnly', Methods)),
                    ?assert(lists:member('childOnly', Methods))
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(ParentPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% --- classFieldNames/1 — multiple fields ---

class_field_names_multiple_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class(
                    'BT1959FieldsMulti',
                    #{a => 1, b => 2, c => 3},
                    #{}
                ),
                try
                    Fields = beamtalk_behaviour_intrinsics:classFieldNames(ClassObj),
                    ?assertEqual(3, length(Fields)),
                    ?assert(lists:member(a, Fields)),
                    ?assert(lists:member(b, Fields)),
                    ?assert(lists:member(c, Fields))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%%% --- classSuperclass/1 — chain parent to grandparent ---

class_superclass_chain_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {_GrandObj, GrandPid} = register_class('BT1959SuperChainGrand', #{}, #{}),
                {ChildObj, ChildPid} = register_class_with_super(
                    'BT1959SuperChainChild', 'BT1959SuperChainGrand', #{}, #{}
                ),
                try
                    Super = beamtalk_behaviour_intrinsics:classSuperclass(ChildObj),
                    ?assertMatch(#beamtalk_object{}, Super),
                    ?assertEqual(
                        'BT1959SuperChainGrand',
                        beamtalk_behaviour_intrinsics:className(Super)
                    ),
                    %% Grandparent's superclass is Object (if registered) or nil
                    GrandSuper = beamtalk_behaviour_intrinsics:classSuperclass(Super),
                    case beamtalk_class_registry:whereis_class('Object') of
                        undefined ->
                            ?assertEqual(nil, GrandSuper);
                        _ ->
                            ?assertMatch(#beamtalk_object{}, GrandSuper)
                    end
                after
                    (try
                        gen_server:stop(ChildPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end),
                    (try
                        gen_server:stop(GrandPid, normal, 5000)
                    catch
                        _:_ -> ok
                    end)
                end
            end)
        ]
    end}.

%%% ============================================================================
%%% BT-1982: Additional coverage for behaviour intrinsics
%%% ============================================================================

%% classReload/1 on a class with no source file (dynamic class) raises
%% no_source_file — covers the nil branch of classReload/1.
bt1982_class_reload_no_source_file_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1982ReloadNoSrc', #{}, #{}),
                try
                    ?assertError(
                        #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{kind = no_source_file}
                        },
                        beamtalk_behaviour_intrinsics:classReload(ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% classRemoveFromSystemByName rejects stdlib-module classes with runtime_error +
%% a "stdlib" hint. Creates a class whose module atom starts with bt@stdlib@
%% to exercise the stdlib protection branch.
bt1982_class_remove_rejects_stdlib_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Construct a class whose module name looks like a stdlib module.
                %% start/2 allows any atom as module; the stdlib check is by name prefix.
                ClassInfo = #{
                    name => 'BT1982StdlibProtectedX',
                    module => 'bt@stdlib@BT1982StdlibProtectedX',
                    superclass => none,
                    instance_methods => #{},
                    class_methods => #{}
                },
                {ok, Pid} = beamtalk_object_class:start(
                    'BT1982StdlibProtectedX', ClassInfo
                ),
                try
                    ?assertError(
                        #{
                            '$beamtalk_class' := _,
                            error := #beamtalk_error{kind = runtime_error}
                        },
                        beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(
                            'BT1982StdlibProtectedX'
                        )
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% metaclassSuperclass on a root class (superclass=none) returns nil —
%% covers the none branch of metaclassSuperclass/1.
bt1982_metaclass_superclass_root_returns_nil_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'BT1982RootMeta',
                    module => test_class,
                    superclass => none,
                    instance_methods => #{},
                    class_methods => #{}
                },
                {ok, Pid} = beamtalk_object_class:start('BT1982RootMeta', ClassInfo),
                ClassObj = #beamtalk_object{
                    class = 'BT1982RootMeta class',
                    class_mod = test_class,
                    pid = Pid
                },
                try
                    ?assertEqual(
                        nil,
                        beamtalk_behaviour_intrinsics:metaclassSuperclass(ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% classSuperclass on a class whose module has no __beamtalk_meta/0 (dynamic /
%% class_builder class) exercises the gen_server-fallback branch — and the
%% 'none' result should normalize to nil.
bt1982_class_superclass_root_falls_through_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                ClassInfo = #{
                    name => 'BT1982SuperRootFB',
                    %% test_class doesn't export __beamtalk_meta/0.
                    module => test_class,
                    superclass => none,
                    instance_methods => #{},
                    class_methods => #{}
                },
                {ok, Pid} = beamtalk_object_class:start('BT1982SuperRootFB', ClassInfo),
                ClassObj = #beamtalk_object{
                    class = 'BT1982SuperRootFB class',
                    class_mod = test_class,
                    pid = Pid
                },
                try
                    ?assertEqual(
                        nil,
                        beamtalk_behaviour_intrinsics:classSuperclass(ClassObj)
                    )
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.

%% stop_class_actors is a no-op when the actor registry is not running.
%% It's exported as a plain function, but only called internally from
%% classRemoveFromSystemByName — exercise that path by attempting a successful
%% removal, which calls stop_class_actors in its success branch.
bt1982_class_remove_success_when_registry_absent_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register a class using a module name that permits code:delete.
                %% We install a freshly-compiled module so code:soft_purge/1 +
                %% code:delete/1 succeed (exercising the happy-path through
                %% ensure_code_step/publish_class_removed/stop_class_actors).
                ModAtom = bt1982_removable_mod,
                Forms = [
                    {attribute, 1, module, ModAtom},
                    {attribute, 2, export, []}
                ],
                {ok, ModAtom, Bin} = compile:forms(Forms, [return_errors]),
                {module, ModAtom} = code:load_binary(
                    ModAtom, "bt1982_removable_mod.erl", Bin
                ),
                ClassName = 'BT1982Removable',
                ClassInfo = #{
                    name => ClassName,
                    module => ModAtom,
                    superclass => none,
                    instance_methods => #{},
                    class_methods => #{}
                },
                {ok, _Pid} = beamtalk_object_class:start(ClassName, ClassInfo),

                %% Remove: this calls stop_class_actors, gen_server:stop,
                %% soft_purge, delete, soft_purge (again), publish_class_removed.
                ?assertEqual(
                    nil,
                    beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassName)
                ),
                %% Verify the class is gone from the registry.
                ?assertEqual(
                    undefined, beamtalk_class_registry:whereis_class(ClassName)
                )
            end)
        ]
    end}.

%% metaclassAllMethods returns the combined method set (class-side + Behaviour
%% protocol). Even for a dynamic class without the stdlib Class hierarchy
%% loaded, the function should at least return a list.
bt1982_metaclass_all_methods_returns_list_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class_with_class_methods(
                    'BT1982MetaAll', #{}, #{}, #{
                        'customFactory' => #{
                            arity => 0, block => fun(_, _, _) -> ok end
                        }
                    }
                ),
                try
                    Methods = beamtalk_behaviour_intrinsics:metaclassAllMethods(ClassObj),
                    ?assert(is_list(Methods)),
                    ?assert(lists:member('customFactory', Methods))
                after
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
                end
            end)
        ]
    end}.
