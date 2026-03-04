%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

%%% @doc EUnit tests for beamtalk_class_registry (BT-708).
%%%
%%% Tests class lookup, hierarchy queries, process group management,
%%% and class object identity functions.

-module(beamtalk_class_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% registry_name tests
%%% ============================================================================

registry_name_test() ->
    ?assertEqual(
        beamtalk_class_Counter,
        beamtalk_class_registry:registry_name('Counter')
    ).

registry_name_object_test() ->
    ?assertEqual(
        beamtalk_class_Object,
        beamtalk_class_registry:registry_name('Object')
    ).

%%% ============================================================================
%%% class_object_tag tests
%%% ============================================================================

class_object_tag_test() ->
    ?assertEqual('Point class', beamtalk_class_registry:class_object_tag('Point')).

class_object_tag_object_test() ->
    ?assertEqual('Object class', beamtalk_class_registry:class_object_tag('Object')).

%%% ============================================================================
%%% is_class_name tests
%%% ============================================================================

is_class_name_true_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Integer class')).

is_class_name_false_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('Integer')).

is_class_name_short_atom_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('ab')).

is_class_name_non_atom_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name(<<"Integer class">>)).

is_class_name_object_class_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Object class')).

%%% ============================================================================
%%% is_class_object tests
%%% ============================================================================

is_class_object_true_test() ->
    Obj = {beamtalk_object, 'Counter class', counter, self()},
    ?assertEqual(true, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_false_instance_test() ->
    Obj = {beamtalk_object, 'Counter', counter, self()},
    ?assertEqual(false, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_non_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object(42)).

is_class_object_wrong_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object({wrong, tuple})).

%%% ============================================================================
%%% class_display_name tests
%%% ============================================================================

class_display_name_strips_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name('Counter class')
    ).

class_display_name_no_suffix_test() ->
    ?assertEqual(
        <<"Counter">>,
        beamtalk_class_registry:class_display_name('Counter')
    ).

class_display_name_object_test() ->
    ?assertEqual(
        <<"Object">>,
        beamtalk_class_registry:class_display_name('Object class')
    ).

%%% ============================================================================
%%% ensure_pg_started tests
%%% ============================================================================

ensure_pg_started_test() ->
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()),
    %% Calling again should also succeed (idempotent)
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()).

%%% ============================================================================
%%% ensure_hierarchy_table tests
%%% ============================================================================

ensure_hierarchy_table_test_() ->
    {setup,
        fun() ->
            %% Save existing entries to restore after test
            case ets:info(beamtalk_class_hierarchy) of
                undefined -> {missing, []};
                _ -> {exists, ets:tab2list(beamtalk_class_hierarchy)}
            end
        end,
        fun
            ({missing, _}) ->
                %% Table didn't exist before; delete if we created it and we own it
                case ets:info(beamtalk_class_hierarchy, owner) of
                    undefined -> ok;
                    Owner when Owner =:= self() -> ets:delete(beamtalk_class_hierarchy);
                    _ -> ok
                end;
            ({exists, Saved}) ->
                %% Restore original entries
                ets:delete_all_objects(beamtalk_class_hierarchy),
                ets:insert(beamtalk_class_hierarchy, Saved)
        end,
        fun() ->
            ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table()),
            ?assertNotEqual(undefined, ets:info(beamtalk_class_hierarchy)),
            %% Idempotent
            ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table())
        end}.

%%% ============================================================================
%%% inherits_from tests
%%% ============================================================================

inherits_from_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_hierarchy),
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, {'Object', none}),
            ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
            ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'}),
            Saved
        end,
        fun(Saved) ->
            %% Restore original state
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, Saved)
        end,
        [
            {"same class", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Counter'))
            end},
            {"direct parent", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Actor'))
            end},
            {"grandparent", fun() ->
                ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Object'))
            end},
            {"not related", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from('Object', 'Counter'))
            end},
            {"none ancestor", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from(none, 'Object'))
            end},
            {"unknown class", fun() ->
                ?assertEqual(false, beamtalk_class_registry:inherits_from('Unknown', 'Object'))
            end}
        ]}.

%%% ============================================================================
%%% direct_subclasses tests
%%% ============================================================================

direct_subclasses_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_hierarchy),
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, {'Object', none}),
            ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
            ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'}),
            ets:insert(beamtalk_class_hierarchy, {'Timer', 'Actor'}),
            Saved
        end,
        fun(Saved) ->
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, Saved)
        end,
        [
            {"direct children of Actor", fun() ->
                Result = beamtalk_class_registry:direct_subclasses('Actor'),
                ?assertEqual(['Counter', 'Timer'], Result)
            end},
            {"direct children of Object", fun() ->
                ?assertEqual(['Actor'], beamtalk_class_registry:direct_subclasses('Object'))
            end},
            {"leaf class has no children", fun() ->
                ?assertEqual([], beamtalk_class_registry:direct_subclasses('Counter'))
            end},
            {"unknown class", fun() ->
                ?assertEqual([], beamtalk_class_registry:direct_subclasses('Unknown'))
            end}
        ]}.

%%% ============================================================================
%%% all_subclasses tests
%%% ============================================================================

all_subclasses_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_hierarchy),
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, {'Object', none}),
            ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
            ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'}),
            ets:insert(beamtalk_class_hierarchy, {'Timer', 'Actor'}),
            Saved
        end,
        fun(Saved) ->
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, Saved)
        end,
        [
            {"all subclasses of Object", fun() ->
                Result = beamtalk_class_registry:all_subclasses('Object'),
                ?assertEqual(['Actor', 'Counter', 'Timer'], Result)
            end},
            {"all subclasses of Actor", fun() ->
                Result = beamtalk_class_registry:all_subclasses('Actor'),
                ?assertEqual(['Counter', 'Timer'], Result)
            end},
            {"leaf class has no subclasses", fun() ->
                ?assertEqual([], beamtalk_class_registry:all_subclasses('Counter'))
            end}
        ]}.

%%% ============================================================================
%%% whereis_class tests
%%% ============================================================================

whereis_class_unregistered_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:whereis_class('NonExistentClass12345')
    ).

whereis_class_registered_test() ->
    RegName = beamtalk_class_registry:registry_name('TestClassBT708'),
    register(RegName, self()),
    ?assertEqual(self(), beamtalk_class_registry:whereis_class('TestClassBT708')),
    unregister(RegName).

%%% ============================================================================
%%% all_classes tests
%%% ============================================================================

all_classes_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    %% all_classes returns whatever is in the pg group
    Result = beamtalk_class_registry:all_classes(),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% get_method_return_type / get_class_method_return_type tests (BT-1002)
%%% ============================================================================

get_method_return_type_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            Saved = ets:tab2list(beamtalk_class_hierarchy),
            ets:delete_all_objects(beamtalk_class_hierarchy),
            %% Create a simple hierarchy: Child -> Parent -> Object
            {ok, ObjPid} = beamtalk_object_class:start('TestObject1002', #{
                superclass => none,
                method_return_types => #{getValue => 'Integer'}
            }),
            ets:insert(beamtalk_class_hierarchy, {'TestObject1002', none}),
            {ok, ParentPid} = beamtalk_object_class:start('TestParent1002', #{
                superclass => 'TestObject1002',
                method_return_types => #{name => 'String'},
                class_method_return_types => #{'from:' => 'TestParent1002'}
            }),
            ets:insert(beamtalk_class_hierarchy, {'TestParent1002', 'TestObject1002'}),
            {ok, ChildPid} = beamtalk_object_class:start('TestChild1002', #{
                superclass => 'TestParent1002',
                method_return_types => #{size => 'Integer'}
            }),
            ets:insert(beamtalk_class_hierarchy, {'TestChild1002', 'TestParent1002'}),
            {Saved, [ObjPid, ParentPid, ChildPid]}
        end,
        fun({Saved, Pids}) ->
            lists:foreach(
                fun(Pid) ->
                    catch gen_server:stop(Pid)
                end,
                Pids
            ),
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, Saved)
        end,
        [
            {"local hit", fun() ->
                ?assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', size)
                )
            end},
            {"superclass chain hit", fun() ->
                ?assertEqual(
                    {ok, 'String'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', name)
                )
            end},
            {"grandparent chain hit", fun() ->
                ?assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', getValue)
                )
            end},
            {"not found anywhere", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type('TestChild1002', unknown)
                )
            end},
            {"none class", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type(none, size)
                )
            end},
            {"unknown class", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_method_return_type('NoSuchClass1002', size)
                )
            end},
            {"class method chain hit", fun() ->
                ?assertEqual(
                    {ok, 'TestParent1002'},
                    beamtalk_class_registry:get_class_method_return_type('TestChild1002', 'from:')
                )
            end},
            {"class method not found", fun() ->
                ?assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_class_method_return_type('TestChild1002', unknown)
                )
            end}
        ]}.

%%% ============================================================================
%%% ADR 0032 Phase 1: invalidate_subclass_flattened_tables removed
%%% ============================================================================
%% The rebuild broadcast (invalidate_subclass_flattened_tables) was removed in
%% ADR 0032 Phase 1. Chain walk at dispatch time replaces the flattened table
%% cache, so no broadcast is needed when class methods change.
%%
%% Test removed: invalidate_subclass_flattened_tables_test
