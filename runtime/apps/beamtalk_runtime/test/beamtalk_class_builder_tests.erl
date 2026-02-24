%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% **DDD Context:** Object System

%%% @doc EUnit tests for beamtalk_class_builder:register/1 (ADR 0038 Phase 1, BT-835).
%%%
%%% Tests:
%%%   - happy path: successful class registration
%%%   - hot reload: registering an existing class updates it
%%%   - missing superclass error: superclassRef = nil
%%%   - sealed superclass error: superclass has is_sealed = true
%%%   - bad name error: className is not an atom (nil or non-atom)
%%%   - bootstrap assertion: Class respondsTo: #classBuilder

-module(beamtalk_class_builder_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    %% Ensure pg is started (required by class registry)
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    %% Ensure the ETS hierarchy table exists
    beamtalk_class_registry:ensure_hierarchy_table(),
    %% Track class names created during each test for cleanup
    [].

teardown(ClassNames) ->
    %% Stop and unregister any classes created during the test
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

%%====================================================================
%% Happy Path
%%====================================================================

register_happy_path_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835TestClassA',
                    superclassRef => 'Object',
                    fieldSpecs => #{x => 0, y => 0},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({ok, _Pid}, Result),
                {ok, Pid} = Result,
                ?assert(is_pid(Pid)),
                ?assert(is_process_alive(Pid)),
                %% Verify class is registered
                ?assertEqual(Pid, beamtalk_class_registry:whereis_class('BT835TestClassA')),
                %% Verify superclass recorded
                ?assertEqual('Object', beamtalk_object_class:superclass(Pid)),
                %% Cleanup
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

register_with_methods_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                MethodFun = fun(Self, _Args, State) ->
                    {reply, maps:get(val, State, 0), State, Self}
                end,
                State = #{
                    className => 'BT835TestClassB',
                    superclassRef => 'Object',
                    fieldSpecs => #{val => 0},
                    methodSpecs => #{'getValue' => MethodFun}
                },
                {ok, Pid} = beamtalk_class_builder:register(State),
                ?assert(is_process_alive(Pid)),
                %% Verify method is registered
                ?assert(beamtalk_object_class:has_method(Pid, 'getValue')),
                gen_server:stop(Pid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% Hot Reload
%%====================================================================

register_hot_reload_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State1 = #{
                    className => 'BT835HotReloadClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{a => 0},
                    methodSpecs => #{}
                },
                {ok, Pid1} = beamtalk_class_builder:register(State1),
                ?assert(is_process_alive(Pid1)),

                %% Register again (hot reload): should update, not crash
                State2 = #{
                    className => 'BT835HotReloadClass',
                    superclassRef => 'Object',
                    fieldSpecs => #{a => 0, b => 1},
                    methodSpecs => #{}
                },
                Result2 = beamtalk_class_builder:register(State2),
                ?assertMatch({ok, _}, Result2),

                %% The class gen_server should still be alive (same or new pid)
                CurrentPid = beamtalk_class_registry:whereis_class('BT835HotReloadClass'),
                ?assert(is_pid(CurrentPid)),
                ?assert(is_process_alive(CurrentPid)),

                gen_server:stop(CurrentPid, normal, 5000)
            end)
        ]
    end}.

%%====================================================================
%% Validation Errors
%%====================================================================

register_missing_superclass_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835NoSuperclass',
                    superclassRef => nil,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = no_superclass}}, Result)
            end)
        ]
    end}.

register_sealed_superclass_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                %% Register a sealed class first
                SealedClassInfo = #{
                    name => 'BT835SealedClass',
                    superclass => 'Object',
                    module => beamtalk_class_builder_bt,
                    fields => [],
                    class_methods => #{},
                    instance_methods => #{},
                    is_sealed => true
                },
                {ok, SealedPid} = beamtalk_object_class:start('BT835SealedClass', SealedClassInfo),

                %% Try to subclass the sealed class
                State = #{
                    className => 'BT835SealedSubclass',
                    superclassRef => 'BT835SealedClass',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = instantiation_error}}, Result),

                %% Cleanup
                gen_server:stop(SealedPid, normal, 5000)
            end)
        ]
    end}.

register_bad_name_nil_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => nil,
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = missing_parameter}}, Result)
            end)
        ]
    end}.

register_bad_name_non_atom_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => <<"NotAnAtom">>,
                    superclassRef => 'Object',
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

register_bad_superclass_ref_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835BadSuperRef',
                    superclassRef => <<"NotValidRef">>,
                    fieldSpecs => #{},
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

register_bad_specs_type_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                State = #{
                    className => 'BT835BadSpecs',
                    superclassRef => 'Object',
                    fieldSpecs => not_a_map,
                    methodSpecs => #{}
                },
                Result = beamtalk_class_builder:register(State),
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
            end)
        ]
    end}.

%%====================================================================
%% Bootstrap Assertion: Class respondsTo: #classBuilder
%%====================================================================

class_responds_to_classbuilder_test_() ->
    {setup,
        fun() ->
            %% Need a running bootstrap to have the Class stub registered
            case whereis(pg) of
                undefined -> {ok, _} = pg:start_link();
                _ -> ok
            end,
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_bt:register_class(),
            []
        end,
        fun teardown/1, fun(_) ->
            [
                ?_test(begin
                    ClassPid = beamtalk_class_registry:whereis_class('Class'),
                    ?assert(is_pid(ClassPid)),
                    %% Post-bootstrap assertion: Class respondsTo: #classBuilder
                    %% (ADR 0038 Phase 1 acceptance criterion)
                    ?assert(beamtalk_object_class:has_method(ClassPid, 'classBuilder'))
                end)
            ]
        end}.

%%====================================================================
%% Bootstrap Sequence: ClassBuilder is registered after Metaclass
%%====================================================================

classbuilder_registered_in_bootstrap_test_() ->
    {setup,
        fun() ->
            case whereis(pg) of
                undefined -> {ok, _} = pg:start_link();
                _ -> ok
            end,
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_bt:register_class(),
            beamtalk_metaclass_bt:register_class(),
            beamtalk_class_builder_bt:register_class(),
            []
        end,
        fun teardown/1, fun(_) ->
            [
                ?_test(begin
                    ClassBuilderPid = beamtalk_class_registry:whereis_class('ClassBuilder'),
                    ?assert(is_pid(ClassBuilderPid)),
                    ?assert(is_process_alive(ClassBuilderPid)),
                    %% ClassBuilder has superclass 'Actor'
                    ?assertEqual('Actor', beamtalk_object_class:superclass(ClassBuilderPid))
                end)
            ]
        end}.
