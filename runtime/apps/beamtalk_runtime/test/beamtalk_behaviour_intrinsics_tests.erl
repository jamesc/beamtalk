%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_behaviour_intrinsics module (BT-1088).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests intrinsic functions for class reflection: metaclassNew, classClass,
%%% className, classLocalMethods, classFieldNames, classIncludesSelector,
%%% classDoc, classSetDoc, classSetMethodDoc, and classSubclasses.
%%%
%%% Uses a minimal setup (pg + ETS hierarchy table) with dynamically-created
%%% test classes via beamtalk_class_builder.

-module(beamtalk_behaviour_intrinsics_tests).

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
                    catch gen_server:stop(Pid, normal, 5000)
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
                MetaObj = beamtalk_behaviour_intrinsics:classClass(ClassObj),
                %% Metaclass tag
                ?assertEqual('Metaclass', MetaObj#beamtalk_object.class),
                %% Same pid wrapped
                ?assertEqual(Pid, MetaObj#beamtalk_object.pid),
                gen_server:stop(Pid, normal, 5000)
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
                ?assertEqual(
                    'BT1088BiClassName', beamtalk_behaviour_intrinsics:className(ClassObj)
                ),
                gen_server:stop(Pid, normal, 5000)
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
                Methods = beamtalk_behaviour_intrinsics:classLocalMethods(ClassObj),
                ?assert(is_list(Methods)),
                ?assertEqual([], Methods),
                gen_server:stop(Pid, normal, 5000)
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
                Methods = beamtalk_behaviour_intrinsics:classLocalMethods(ClassObj),
                ?assert(lists:member('getValue', Methods)),
                ?assert(lists:member('setValue:', Methods)),
                gen_server:stop(Pid, normal, 5000)
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
                Fields = beamtalk_behaviour_intrinsics:classFieldNames(ClassObj),
                ?assert(is_list(Fields)),
                gen_server:stop(Pid, normal, 5000)
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
                Fields = beamtalk_behaviour_intrinsics:classFieldNames(ClassObj),
                ?assert(is_list(Fields)),
                ?assert(lists:member(x, Fields)),
                ?assert(lists:member(y, Fields)),
                gen_server:stop(Pid, normal, 5000)
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
                ?assert(beamtalk_behaviour_intrinsics:classIncludesSelector(ClassObj, 'increment')),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

class_includes_selector_false_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiSelectorFalse', #{}, #{}),
                ?assertNot(
                    beamtalk_behaviour_intrinsics:classIncludesSelector(ClassObj, 'nonExistent')
                ),
                gen_server:stop(Pid, normal, 5000)
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
                ?assertEqual(nil, beamtalk_behaviour_intrinsics:classDoc(ClassObj)),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

class_set_doc_and_get_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiDocSet', #{}, #{}),
                Doc = <<"This is a test class">>,
                RetObj = beamtalk_behaviour_intrinsics:classSetDoc(ClassObj, Doc),
                %% setDoc returns Self
                ?assertEqual(ClassObj, RetObj),
                %% doc is now set
                ?assertEqual(Doc, beamtalk_behaviour_intrinsics:classDoc(ClassObj)),
                gen_server:stop(Pid, normal, 5000)
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
                Doc = <<"Does something useful">>,
                RetObj = beamtalk_behaviour_intrinsics:classSetMethodDoc(ClassObj, 'myMethod', Doc),
                %% Returns Self
                ?assertEqual(ClassObj, RetObj),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

class_doc_for_nonexistent_method_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                {ClassObj, Pid} = register_class('BT1088BiDocForMethod', #{}, #{}),
                ?assertEqual(
                    nil,
                    beamtalk_behaviour_intrinsics:classDocForMethod(ClassObj, 'nosuchMethod')
                ),
                gen_server:stop(Pid, normal, 5000)
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
                Subclasses = beamtalk_behaviour_intrinsics:classSubclasses(ClassObj),
                ?assert(is_list(Subclasses)),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.
