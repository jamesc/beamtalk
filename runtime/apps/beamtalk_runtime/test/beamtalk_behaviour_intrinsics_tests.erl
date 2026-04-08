%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_behaviour_intrinsics module (BT-1088).

Tests intrinsic functions for class reflection: metaclassNew, classClass,
className, classLocalMethods, classFieldNames, classIncludesSelector,
classDoc, classSetDoc, classSetMethodDoc, and classSubclasses.

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
                    %% Unknown protocol returns true (conservative)
                    ?assert(
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
