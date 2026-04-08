%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_registry_tests).

-moduledoc """
EUnit tests for beamtalk_class_registry (BT-708).

Tests class lookup, hierarchy queries, process group management,
and class object identity functions.
""".
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
%%% live_class_entries tests (BT-1090)
%%% ============================================================================

live_class_entries_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
            {ok, Pid} = beamtalk_object_class:start_link('LiveEntriesTestClass1090', ClassInfo),
            Pid
        end,
        fun(Pid) ->
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end)
        end,
        [
            {"returns a list", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                ?assert(is_list(Result))
            end},
            {"entries are {Name, Module, Pid} tuples", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                lists:foreach(
                    fun({Name, Mod, Pid}) ->
                        ?assert(is_atom(Name)),
                        ?assert(is_atom(Mod)),
                        ?assert(is_pid(Pid))
                    end,
                    Result
                )
            end},
            {"includes the started class", fun() ->
                Result = beamtalk_class_registry:live_class_entries(),
                Names = [Name || {Name, _Mod, _Pid} <- Result],
                ?assert(lists:member('LiveEntriesTestClass1090', Names))
            end}
        ]}.

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
                    (try
                        gen_server:stop(Pid)
                    catch
                        _:_ -> ok
                    end)
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
%%% user_classes tests (BT-1092)
%%% ============================================================================

user_classes_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
            {ok, Pid} = beamtalk_object_class:start_link('UserClassesTestClass1092', ClassInfo),
            Pid
        end,
        fun(Pid) ->
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end)
        end,
        [
            {"returns a list", fun() ->
                Result = beamtalk_class_registry:user_classes(),
                ?assert(is_list(Result))
            end},
            {"excludes classes without source file", fun() ->
                %% Test class has no compiled BEAM module with beamtalk_source
                %% attribute, so it should be filtered out by user_classes/0
                Result = beamtalk_class_registry:user_classes(),
                Tags = [Tag || {beamtalk_object, Tag, _Mod, _Pid} <- Result],
                ?assertNot(lists:member('UserClassesTestClass1092 class', Tags))
            end},
            {"entries are {beamtalk_object, Tag, Mod, Pid} tuples", fun() ->
                Result = beamtalk_class_registry:user_classes(),
                lists:foreach(
                    fun({beamtalk_object, Tag, Mod, Pid}) ->
                        ?assert(is_atom(Tag)),
                        ?assert(is_atom(Mod)),
                        ?assert(is_pid(Pid))
                    end,
                    Result
                )
            end}
        ]}.

%%% ============================================================================
%%% ETS table ownership / heir tests (BT-1888)
%%% ============================================================================

pid_table_survives_owner_death_test_() ->
    {setup,
        fun() ->
            %% Delete the table if it exists so we can recreate it
            %% from a spawned (transient) process to test heir behavior.
            case ets:info(beamtalk_class_pids) of
                undefined ->
                    {missing, []};
                _ ->
                    Saved = ets:tab2list(beamtalk_class_pids),
                    %% We can only delete if we own the table. If we don't,
                    %% skip the test setup — table is already owned by a
                    %% persistent process (the correct state).
                    case ets:info(beamtalk_class_pids, owner) of
                        Owner when Owner =:= self() ->
                            ets:delete(beamtalk_class_pids),
                            {owned, Saved};
                        _ ->
                            {foreign, Saved}
                    end
            end
        end,
        fun
            ({missing, _}) ->
                %% Ensure the table exists again for other tests
                beamtalk_class_registry:ensure_pid_table();
            ({owned, Saved}) ->
                %% Restore table and entries
                beamtalk_class_registry:ensure_pid_table(),
                ets:insert(beamtalk_class_pids, Saved);
            ({foreign, _Saved}) ->
                ok
        end,
        fun() ->
            %% When runtime sup is alive, heir option is set and the table
            %% should survive the creating process dying.
            case whereis(beamtalk_runtime_sup) of
                undefined ->
                    %% Can't test heir behavior without the supervisor.
                    %% Just verify ensure_pid_table is idempotent.
                    beamtalk_class_registry:ensure_pid_table(),
                    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids));
                SupPid ->
                    %% Delete existing table to recreate from a spawned process
                    case ets:info(beamtalk_class_pids) of
                        undefined ->
                            ok;
                        _ ->
                            case ets:info(beamtalk_class_pids, owner) =:= self() of
                                true ->
                                    ets:delete(beamtalk_class_pids);
                                false ->
                                    %% Can't delete a table we don't own;
                                    %% just test that heir is set.
                                    Heir = ets:info(beamtalk_class_pids, heir),
                                    ?assertEqual(SupPid, Heir),
                                    ok
                            end
                    end,
                    case ets:info(beamtalk_class_pids) of
                        undefined ->
                            %% Create table from a transient process
                            TestPid = self(),
                            Spawned = spawn(fun() ->
                                beamtalk_class_registry:ensure_pid_table(),
                                ets:insert(beamtalk_class_pids, {self(), 'TestHeirClass'}),
                                TestPid ! {table_created, ets:info(beamtalk_class_pids, heir)},
                                receive
                                    stop -> ok
                                end
                            end),
                            receive
                                {table_created, HeirPid} ->
                                    ?assertEqual(SupPid, HeirPid),
                                    %% Kill the owner process and wait for it to exit
                                    MRef = erlang:monitor(process, Spawned),
                                    Spawned ! stop,
                                    receive
                                        {'DOWN', MRef, process, Spawned, _} -> ok
                                    after 5000 ->
                                        ?assert(false)
                                    end,
                                    %% Table should still exist (inherited by supervisor)
                                    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids))
                            after 5000 ->
                                ?assert(false)
                            end;
                        _ ->
                            %% Table already exists and we verified heir above
                            ok
                    end
            end
        end}.

heir_option_returns_empty_when_no_supervisor_test() ->
    %% heir_option/0 is private but we can test via ensure_pid_table behavior.
    %% When no runtime supervisor is running, ensure_pid_table still creates the table.
    %% We can't easily test the private function directly, but we verify that
    %% the table creation doesn't crash regardless of supervisor state.
    beamtalk_class_registry:ensure_pid_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_pids)).

%%% ============================================================================
%%% restart_class tests (BT-1888)
%%% ============================================================================

restart_class_no_module_test() ->
    %% Attempting to restart a class with no module table entry should fail.
    ?assertMatch(
        {error, {no_module_for_class, 'NonExistentRestart1888'}},
        beamtalk_class_registry:restart_class('NonExistentRestart1888')
    ).

restart_class_recovery_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            HierSaved = ets:tab2list(beamtalk_class_hierarchy),
            %% Start a class, then kill it to test recovery
            ClassInfo = #{
                superclass => none,
                methods => #{},
                class_methods => #{}
            },
            {ok, Pid} = beamtalk_object_class:start('RestartTest1888', ClassInfo),
            %% Verify it's alive and registered
            ?assertNotEqual(undefined, beamtalk_class_registry:whereis_class('RestartTest1888')),
            {HierSaved, Pid}
        end,
        fun({HierSaved, _Pid}) ->
            %% Clean up the restarted class if it exists
            case beamtalk_class_registry:whereis_class('RestartTest1888') of
                undefined ->
                    ok;
                P ->
                    (try
                        gen_server:stop(P)
                    catch
                        _:_ -> ok
                    end)
            end,
            ets:delete_all_objects(beamtalk_class_hierarchy),
            ets:insert(beamtalk_class_hierarchy, HierSaved)
        end,
        [
            {"kill and restart class", fun() ->
                OldPid = beamtalk_class_registry:whereis_class('RestartTest1888'),
                ?assert(is_pid(OldPid)),
                %% Kill the class process and wait for it to exit
                MRef = erlang:monitor(process, OldPid),
                exit(OldPid, kill),
                receive
                    {'DOWN', MRef, process, OldPid, _} -> ok
                after 5000 ->
                    ?assert(false)
                end,
                %% Should be unregistered now
                ?assertEqual(undefined, beamtalk_class_registry:whereis_class('RestartTest1888')),
                %% Restart it
                {ok, NewPid} = beamtalk_class_registry:restart_class('RestartTest1888'),
                ?assert(is_pid(NewPid)),
                ?assertNotEqual(OldPid, NewPid),
                %% Should be registered again
                ?assertEqual(NewPid, beamtalk_class_registry:whereis_class('RestartTest1888'))
            end}
        ]}.

%%% ============================================================================
%%% class_name_for_pid tests (BT-1888)
%%% ============================================================================

class_name_for_pid_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pid_table(),
            ok
        end,
        fun(_) -> ok end, [
            {"record and lookup", fun() ->
                Pid = self(),
                beamtalk_class_registry:record_class_pid(Pid, 'PidLookupTest1888'),
                ?assertEqual(
                    {ok, 'PidLookupTest1888'}, beamtalk_class_registry:class_name_for_pid(Pid)
                ),
                %% Clean up
                ets:delete(beamtalk_class_pids, Pid)
            end},
            {"not found for unknown pid", fun() ->
                FakePid = list_to_pid("<0.99999.0>"),
                ?assertEqual(not_found, beamtalk_class_registry:class_name_for_pid(FakePid))
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
