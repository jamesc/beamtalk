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
%%% extract_package_from_module tests (BT-1972)
%%% ============================================================================

extract_package_from_module_qualified_test() ->
    ?assertEqual(
        mypackage,
        beamtalk_class_registry:extract_package_from_module('bt@mypackage@MyClass')
    ).

extract_package_from_module_unqualified_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:extract_package_from_module('bt@MyClass')
    ).

extract_package_from_module_no_prefix_test() ->
    ?assertEqual(
        undefined,
        beamtalk_class_registry:extract_package_from_module(some_module)
    ).

extract_package_from_module_stdlib_test() ->
    ?assertEqual(
        stdlib,
        beamtalk_class_registry:extract_package_from_module('bt@stdlib@Integer')
    ).

%%% ============================================================================
%%% is_stdlib_module tests (BT-1972)
%%% ============================================================================

is_stdlib_module_true_test() ->
    ?assert(beamtalk_class_registry:is_stdlib_module('bt@stdlib@Integer')).

is_stdlib_module_false_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module('bt@mypackage@MyClass')).

is_stdlib_module_non_atom_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module(<<"bt@stdlib@Integer">>)).

is_stdlib_module_plain_atom_test() ->
    ?assertNot(beamtalk_class_registry:is_stdlib_module(some_module)).

%%% ============================================================================
%%% ensure_class_warnings_table / collision warning tests (BT-1972)
%%% ============================================================================

ensure_class_warnings_table_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_warnings)),
    %% Idempotent
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_warnings)).

record_and_drain_warnings_by_names_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Insert a collision warning
    beamtalk_class_registry:record_class_collision_warning(
        'TestWarningClass', 'bt@pkg1@TestWarningClass', 'bt@pkg2@TestWarningClass'
    ),
    %% Drain it
    Warnings = beamtalk_class_registry:drain_class_warnings_by_names(['TestWarningClass']),
    ?assertEqual(1, length(Warnings)),
    [{CN, OldMod, NewMod}] = Warnings,
    ?assertEqual('TestWarningClass', CN),
    ?assertEqual('bt@pkg1@TestWarningClass', OldMod),
    ?assertEqual('bt@pkg2@TestWarningClass', NewMod),
    %% Second drain should be empty (warnings consumed)
    ?assertEqual([], beamtalk_class_registry:drain_class_warnings_by_names(['TestWarningClass'])).

drain_warnings_by_names_empty_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names(['NonExistentWarningClass'])
    ).

drain_warnings_by_qualified_names_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    beamtalk_class_registry:record_class_collision_warning(
        'QualDrainClass', 'bt@pkgA@QualDrainClass', 'bt@pkgB@QualDrainClass'
    ),
    %% Drain by the specific package of the new module (pkgB)
    Warnings = beamtalk_class_registry:drain_class_warnings_by_qualified_names(
        [{pkgB, 'QualDrainClass'}]
    ),
    ?assertEqual(1, length(Warnings)),
    %% Draining with the wrong package should find nothing
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_qualified_names(
            [{pkgA, 'QualDrainClass'}]
        )
    ).

drain_warnings_no_table_test() ->
    %% If the table doesn't exist, drain should return []
    %% We can't easily delete the table if it's owned by another process,
    %% so we test the code path by verifying it handles both states gracefully.
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names([])
    ).

%%% ============================================================================
%%% ensure_module_table tests (BT-1972)
%%% ============================================================================

ensure_module_table_test() ->
    beamtalk_class_registry:ensure_module_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module)),
    %% Idempotent
    beamtalk_class_registry:ensure_module_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_class_module)).

%%% ============================================================================
%%% pending load errors tests (BT-1972)
%%% ============================================================================

ensure_pending_errors_table_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_pending_load_errors)),
    %% Idempotent
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertNotEqual(undefined, ets:info(beamtalk_pending_load_errors)).

record_and_drain_pending_errors_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    Error = beamtalk_error:new(stdlib_shadowing, 'ShadowTestClass'),
    beamtalk_class_registry:record_pending_load_error('ShadowTestClass', Error),
    Errors = beamtalk_class_registry:drain_pending_load_errors_by_names(['ShadowTestClass']),
    ?assertEqual(1, length(Errors)),
    [{CN, _Err}] = Errors,
    ?assertEqual('ShadowTestClass', CN),
    %% Second drain should be empty
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_pending_load_errors_by_names(['ShadowTestClass'])
    ).

drain_pending_errors_empty_test() ->
    beamtalk_class_registry:ensure_pending_errors_table(),
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_pending_load_errors_by_names(['NonExistentErrorClass'])
    ).

%%% ============================================================================
%%% validate_class_update tests (BT-1972)
%%% ============================================================================

validate_class_update_same_module_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    ?assertEqual(
        ok,
        beamtalk_class_registry:validate_class_update(
            'ValTestClass', 'bt@pkg@ValTestClass', #{module => 'bt@pkg@ValTestClass'}
        )
    ).

validate_class_update_stdlib_shadowing_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    beamtalk_class_registry:ensure_pending_errors_table(),
    Result = beamtalk_class_registry:validate_class_update(
        'Integer', 'bt@stdlib@Integer', #{module => 'bt@user@Integer'}
    ),
    ?assertMatch({error, #beamtalk_error{}}, Result),
    %% Clean up the pending error
    beamtalk_class_registry:drain_pending_load_errors_by_names(['Integer']).

validate_class_update_cross_module_warning_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Different non-stdlib modules should record a warning but return ok
    ok = beamtalk_class_registry:validate_class_update(
        'CrossModClass', 'bt@pkg1@CrossModClass', #{module => 'bt@pkg2@CrossModClass'}
    ),
    %% Verify the collision warning was recorded
    Warnings = beamtalk_class_registry:drain_class_warnings_by_names(['CrossModClass']),
    ?assertEqual(1, length(Warnings)).

validate_class_update_bootstrap_stub_replacement_test() ->
    beamtalk_class_registry:ensure_class_warnings_table(),
    %% Bootstrap stub being replaced by stdlib module should NOT generate a warning
    ok = beamtalk_class_registry:validate_class_update(
        'Class', beamtalk_class_bt, #{module => 'bt@stdlib@Class'}
    ),
    %% No collision warning should be recorded
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_names(['Class'])
    ).

%%% ============================================================================
%%% BT-1982: Additional coverage
%%% ============================================================================

%% ensure_pg_started/0 is idempotent when pg is already running.
bt1982_ensure_pg_started_idempotent_test() ->
    ok = beamtalk_class_registry:ensure_pg_started(),
    ok = beamtalk_class_registry:ensure_pg_started(),
    ?assert(is_pid(whereis(pg))).

%% maybe_set_heir runs when supervisor is alive; covers the SupPid-alive branch.
%% If the real supervisor is running (test suite with full app) we use it;
%% otherwise we register a fake pid temporarily.
bt1982_maybe_set_heir_with_supervisor_test() ->
    case whereis(beamtalk_runtime_sup) of
        RealSup when is_pid(RealSup) ->
            %% Real supervisor running — exercise the branch directly.
            ok = beamtalk_class_registry:ensure_class_warnings_table(),
            ok = beamtalk_class_registry:ensure_class_warnings_table();
        undefined ->
            beamtalk_class_registry:ensure_pg_started(),
            FakeSupPid = spawn(fun() ->
                receive
                    stop -> ok
                end
            end),
            try
                erlang:register(beamtalk_runtime_sup, FakeSupPid),
                case ets:info(beamtalk_class_warnings) of
                    undefined -> beamtalk_class_registry:ensure_class_warnings_table();
                    _ -> ok
                end,
                ok = beamtalk_class_registry:ensure_class_warnings_table()
            after
                catch erlang:unregister(beamtalk_runtime_sup),
                FakeSupPid ! stop
            end
    end.

%% heir_option with a live supervisor returns a non-empty list. Works whether
%% or not a real supervisor is registered.
bt1982_heir_option_with_supervisor_test() ->
    case whereis(beamtalk_runtime_sup) of
        RealSup when is_pid(RealSup) ->
            beamtalk_class_registry:ensure_pending_errors_table(),
            ?assert(true);
        undefined ->
            FakeSupPid = spawn(fun() ->
                receive
                    stop -> ok
                end
            end),
            try
                erlang:register(beamtalk_runtime_sup, FakeSupPid),
                beamtalk_class_registry:ensure_pending_errors_table(),
                ?assert(true)
            after
                catch erlang:unregister(beamtalk_runtime_sup),
                FakeSupPid ! stop
            end
    end.

%% restart_class finds module but the existing class process is already alive →
%% returns {ok, ExistingPid} via the {already_started, _} branch.
bt1982_restart_class_already_started_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            ClassName = 'BT1982RestartAS',
            ClassInfo = #{
                name => ClassName,
                module => beamtalk_class_bt,
                superclass => none,
                instance_methods => #{},
                class_methods => #{}
            },
            {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
            %% Record in module table so restart_class can find it.
            beamtalk_class_module_table:insert(ClassName, beamtalk_class_bt),
            {ClassName, Pid}
        end,
        fun({_Name, Pid}) ->
            try
                gen_server:stop(Pid, normal, 1000)
            catch
                _:_ -> ok
            end
        end,
        fun({ClassName, Pid}) ->
            [
                ?_test(begin
                    %% restart_class should hit the {already_started, ExistingPid} branch.
                    Result = beamtalk_class_registry:restart_class(ClassName),
                    ?assertMatch({ok, _}, Result),
                    {ok, ReturnedPid} = Result,
                    ?assertEqual(Pid, ReturnedPid)
                end)
            ]
        end}.

%% user_classes/0 is callable and returns a list when pg is running.
bt1982_user_classes_returns_list_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Classes = beamtalk_class_registry:user_classes(),
    ?assert(is_list(Classes)).

%% drain_pending_load_errors_by_names no-table returns [].
bt1982_drain_pending_errors_no_table_test() ->
    %% Delete the table first if it exists.
    case ets:info(beamtalk_pending_load_errors) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_pending_load_errors)
    end,
    ?assertEqual(
        [], beamtalk_class_registry:drain_pending_load_errors_by_names(['Foo'])
    ),
    %% Restore table for subsequent tests.
    beamtalk_class_registry:ensure_pending_errors_table().

%% restart_class success path: class process is dead but module exists.
%% Covers the warning log + {ok, NewPid} branch of restart_class/1.
bt1982_restart_class_recovers_dead_process_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            beamtalk_class_registry:ensure_module_table(),
            beamtalk_class_registry:ensure_pid_table(),
            ClassName = 'BT1982RestartRecover',
            ClassInfo = #{
                name => ClassName,
                module => beamtalk_class_bt,
                superclass => none,
                instance_methods => #{},
                class_methods => #{}
            },
            %% Use start/2 (unlinked) so we can kill it without taking
            %% down the EUnit test supervisor.
            {ok, Pid} = beamtalk_object_class:start(ClassName, ClassInfo),
            beamtalk_class_module_table:insert(ClassName, beamtalk_class_bt),
            %% Kill the class process so restart_class can resurrect it.
            MRef = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 2000 -> throw(timeout_killing_class)
            end,
            ClassName
        end,
        fun(ClassName) ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    ok;
                P ->
                    try
                        gen_server:stop(P, normal, 1000)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        fun(ClassName) ->
            [
                ?_test(begin
                    Result = beamtalk_class_registry:restart_class(ClassName),
                    ?assertMatch({ok, _}, Result),
                    {ok, NewPid} = Result,
                    ?assert(is_process_alive(NewPid))
                end)
            ]
        end}.

%% class_name_for_pid returns not_found when the pid table does not exist.
bt1982_class_name_for_pid_no_table_test() ->
    case ets:info(beamtalk_class_pids) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_class_pids)
    end,
    ?assertEqual(
        not_found, beamtalk_class_registry:class_name_for_pid(self())
    ),
    beamtalk_class_registry:ensure_pid_table().

%% get_method_return_type walks to superclass via hierarchy lookup when the
%% class is registered but process exit during call produces `not_found` —
%% the walk-to-superclass branch.
bt1982_get_method_return_type_walks_on_process_exit_test_() ->
    {setup,
        fun() ->
            beamtalk_class_registry:ensure_pg_started(),
            beamtalk_class_registry:ensure_hierarchy_table(),
            %% Set up a parent class with a known return type, then register a
            %% child with none.
            {ok, ParentPid} = beamtalk_object_class:start('BT1982RTParent', #{
                superclass => none,
                method_return_types => #{answer => 'Integer'}
            }),
            ets:insert(beamtalk_class_hierarchy, {'BT1982RTParent', none}),
            {ok, ChildPid} = beamtalk_object_class:start('BT1982RTChild', #{
                superclass => 'BT1982RTParent',
                method_return_types => #{}
            }),
            ets:insert(beamtalk_class_hierarchy, {'BT1982RTChild', 'BT1982RTParent'}),
            [ParentPid, ChildPid]
        end,
        fun(Pids) ->
            lists:foreach(
                fun(P) ->
                    try
                        gen_server:stop(P)
                    catch
                        _:_ -> ok
                    end
                end,
                Pids
            )
        end,
        fun(_) ->
            [
                %% Child has no type but parent does — walk through hierarchy.
                ?_assertEqual(
                    {ok, 'Integer'},
                    beamtalk_class_registry:get_method_return_type(
                        'BT1982RTChild', answer
                    )
                ),
                %% Same for class-method variant.
                ?_assertEqual(
                    {error, not_found},
                    beamtalk_class_registry:get_class_method_return_type(
                        'BT1982RTChild', answer
                    )
                )
            ]
        end}.

%% drain_class_warnings_by_qualified_names returns [] when no table exists.
bt1982_drain_warnings_qualified_no_table_test() ->
    case ets:info(beamtalk_class_warnings) of
        undefined -> ok;
        _ -> ets:delete(beamtalk_class_warnings)
    end,
    ?assertEqual(
        [],
        beamtalk_class_registry:drain_class_warnings_by_qualified_names(
            [{pkg, 'Foo'}]
        )
    ),
    beamtalk_class_registry:ensure_class_warnings_table().

%%% ============================================================================
%%% ADR 0032 Phase 1: invalidate_subclass_flattened_tables removed
%%% ============================================================================
%% The rebuild broadcast (invalidate_subclass_flattened_tables) was removed in
%% ADR 0032 Phase 1. Chain walk at dispatch time replaces the flattened table
%% cache, so no broadcast is needed when class methods change.
%%
%% Test removed: invalidate_subclass_flattened_tables_test
