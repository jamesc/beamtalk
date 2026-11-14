%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_instances_tests).

-moduledoc """
Unit tests for the beamtalk_object_instances registry.
Tests cover registration, lookup, auto-cleanup on process death,
and iteration over instances.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Use existing supervised server if running, else start a fresh one
    case whereis(beamtalk_object_instances) of
        undefined ->
            {ok, Pid} = beamtalk_object_instances:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the server — it may be supervised by the application.
    %% Clear all registrations to prevent leaking between tests.
    try
        ets:delete_all_objects(beamtalk_instance_registry),
        ets:delete_all_objects(beamtalk_instance_class)
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Tests
%%====================================================================

register_and_all_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn a process to act as an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                %% Register it
                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Should be in all instances
                ?assertEqual([Instance], beamtalk_object_instances:all('Counter')),

                %% Should not be in other class
                ?assertEqual([], beamtalk_object_instances:all('Point')),

                %% Clean up
                Instance ! stop
            end)
        ]
    end}.

count_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Initially zero
                ?assertEqual(0, beamtalk_object_instances:count('Counter')),

                %% Spawn three instances
                Self = self(),
                Spawn = fun() ->
                    spawn(fun() ->
                        Self ! ready,
                        receive
                            stop -> ok
                        end
                    end)
                end,
                I1 = Spawn(),
                I2 = Spawn(),
                I3 = Spawn(),
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,

                %% Register them
                ok = beamtalk_object_instances:register('Counter', I1),
                ok = beamtalk_object_instances:register('Counter', I2),
                ok = beamtalk_object_instances:register('Counter', I3),

                %% Count should be 3
                ?assertEqual(3, beamtalk_object_instances:count('Counter')),

                %% Clean up
                I1 ! stop,
                I2 ! stop,
                I3 ! stop
            end)
        ]
    end}.

class_of_test_() ->
    %% BT-4113: the pid-keyed index answers a pid's class in one lookup, and
    %% follows register, unregister and process death.
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Self = self(),
                Spawn = fun() ->
                    P = spawn(fun() ->
                        Self ! {ready, self()},
                        receive
                            stop -> ok
                        end
                    end),
                    receive
                        {ready, P} -> P
                    end
                end,
                I1 = Spawn(),
                I2 = Spawn(),
                ?assertEqual(undefined, beamtalk_object_instances:class_of(I1)),
                ok = beamtalk_object_instances:register('Counter', I1),
                ok = beamtalk_object_instances:register('Point', I2),
                ?assertEqual('Counter', beamtalk_object_instances:class_of(I1)),
                ?assertEqual('Point', beamtalk_object_instances:class_of(I2)),
                %% Unregistering under another class leaves the entry alone.
                ok = beamtalk_object_instances:unregister('Point', I1),
                ?assertEqual('Counter', beamtalk_object_instances:class_of(I1)),
                ok = beamtalk_object_instances:unregister('Counter', I1),
                ?assertEqual(undefined, beamtalk_object_instances:class_of(I1)),
                %% Process death clears it (the registry handles the DOWN).
                Ref = monitor(process, I2),
                I2 ! stop,
                receive
                    {'DOWN', Ref, process, I2, _} -> ok
                end,
                _ = sys:get_state(beamtalk_object_instances),
                ?assertEqual(undefined, beamtalk_object_instances:class_of(I2)),
                I1 ! stop
            end)
        ]
    end}.

unregister_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn and register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),

                %% Unregister it
                ok = beamtalk_object_instances:unregister('Counter', Instance),
                ?assertEqual(0, beamtalk_object_instances:count('Counter')),
                ?assertEqual([], beamtalk_object_instances:all('Counter')),

                %% Unregistering again should be ok
                ok = beamtalk_object_instances:unregister('Counter', Instance),

                %% Clean up
                Instance ! stop
            end)
        ]
    end}.

auto_cleanup_on_process_death_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn and register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),

                %% Kill the process
                Instance ! stop,

                %% Wait for the monitor to fire and the row to be removed
                ok = beamtalk_test_wait:until(fun() ->
                    beamtalk_object_instances:all('Counter') =:= []
                end),

                %% Should be automatically removed
                ?assertEqual(0, beamtalk_object_instances:count('Counter')),
                ?assertEqual([], beamtalk_object_instances:all('Counter'))
            end)
        ]
    end}.

duplicate_register_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                %% Register twice - should be idempotent
                ok = beamtalk_object_instances:register('Counter', Instance),
                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Count should still be 1
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),

                %% Clean up
                Instance ! stop
            end)
        ]
    end}.

multiple_classes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn instances for different classes
                Self = self(),
                Spawn = fun() ->
                    spawn(fun() ->
                        Self ! ready,
                        receive
                            stop -> ok
                        end
                    end)
                end,
                Counter1 = Spawn(),
                Counter2 = Spawn(),
                Point1 = Spawn(),
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,

                %% Register under different classes
                ok = beamtalk_object_instances:register('Counter', Counter1),
                ok = beamtalk_object_instances:register('Counter', Counter2),
                ok = beamtalk_object_instances:register('Point', Point1),

                %% Check counts
                ?assertEqual(2, beamtalk_object_instances:count('Counter')),
                ?assertEqual(1, beamtalk_object_instances:count('Point')),

                %% Check all instances
                CounterInstances = beamtalk_object_instances:all('Counter'),
                ?assertEqual(2, length(CounterInstances)),
                ?assert(lists:member(Counter1, CounterInstances)),
                ?assert(lists:member(Counter2, CounterInstances)),

                ?assertEqual([Point1], beamtalk_object_instances:all('Point')),

                %% Clean up
                Counter1 ! stop,
                Counter2 ! stop,
                Point1 ! stop
            end)
        ]
    end}.

each_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn instances
                Self = self(),
                Spawn = fun() ->
                    spawn(fun() ->
                        Self ! ready,
                        receive
                            {ping, From} -> From ! pong;
                            stop -> ok
                        end
                    end)
                end,
                I1 = Spawn(),
                I2 = Spawn(),
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', I1),
                ok = beamtalk_object_instances:register('Counter', I2),

                %% Use each to send a message to all instances
                beamtalk_object_instances:each('Counter', fun(P) ->
                    P ! {ping, Self}
                end),

                %% Should receive pong from both
                receive
                    pong -> ok
                after 100 -> ?assert(false)
                end,
                receive
                    pong -> ok
                after 100 -> ?assert(false)
                end,

                %% Clean up
                I1 ! stop,
                I2 ! stop
            end)
        ]
    end}.

each_empty_class_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% each on empty class should do nothing
                CallCount = erlang:make_ref(),
                put(CallCount, 0),
                beamtalk_object_instances:each('NonExistent', fun(_P) ->
                    put(CallCount, get(CallCount) + 1)
                end),
                ?assertEqual(0, get(CallCount))
            end)
        ]
    end}.

filters_dead_processes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn two instances
                Self = self(),
                Spawn = fun() ->
                    spawn(fun() ->
                        Self ! ready,
                        receive
                            stop -> ok
                        end
                    end)
                end,
                I1 = Spawn(),
                I2 = Spawn(),
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', I1),
                ok = beamtalk_object_instances:register('Counter', I2),

                %% Kill one
                I1 ! stop,
                beamtalk_test_wait:dead(I1),

                %% all() should only return the live one
                Instances = beamtalk_object_instances:all('Counter'),
                ?assertEqual([I2], Instances),

                %% count() should be 1
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),

                %% Clean up
                I2 ! stop
            end)
        ]
    end}.

ets_table_ownership_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Verify that the ETS table is owned by the gen_server process
                TableInfo = ets:info(beamtalk_instance_registry),
                ?assertNotEqual(undefined, TableInfo),

                Owner = proplists:get_value(owner, TableInfo),
                ?assertEqual(Pid, Owner),

                %% On a crash the table passes to beamtalk_ets_heir (when it is
                %% running) and the restarted server reclaims it (BT-4123,
                %% pinned in beamtalk_ets_heir_tests).
                ok
            end)
        ]
    end}.

monitor_cleanup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Spawn and register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Verify instance is tracked via public API
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),
                ?assert(lists:member(Instance, beamtalk_object_instances:all('Counter'))),

                %% Kill the instance
                Instance ! stop,
                beamtalk_test_wait:dead(Instance),

                %% Verify instance is removed from registry after death
                ?assertEqual(0, beamtalk_object_instances:count('Counter')),
                ?assertEqual([], beamtalk_object_instances:all('Counter'))
            end)
        ]
    end}.

same_pid_multiple_classes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% A single process can be registered as an instance of multiple classes
                %% This is valid - the key is {Class, Pid}
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                %% Register under multiple classes
                ok = beamtalk_object_instances:register('Counter', Instance),
                ok = beamtalk_object_instances:register('Actor', Instance),
                ok = beamtalk_object_instances:register('Object', Instance),

                %% Should appear in all classes
                ?assertEqual([Instance], beamtalk_object_instances:all('Counter')),
                ?assertEqual([Instance], beamtalk_object_instances:all('Actor')),
                ?assertEqual([Instance], beamtalk_object_instances:all('Object')),

                %% Counts should all be 1
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),
                ?assertEqual(1, beamtalk_object_instances:count('Actor')),
                ?assertEqual(1, beamtalk_object_instances:count('Object')),

                %% Kill the process
                Instance ! stop,
                beamtalk_test_wait:dead(Instance),

                %% Should be removed from all classes
                ?assertEqual(0, beamtalk_object_instances:count('Counter')),
                ?assertEqual(0, beamtalk_object_instances:count('Actor')),
                ?assertEqual(0, beamtalk_object_instances:count('Object'))
            end)
        ]
    end}.

concurrent_registration_stress_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Spawn many instances and register them concurrently
                NumInstances = 50,
                Self = self(),

                Instances = [
                    spawn(fun() ->
                        Self ! ready,
                        receive
                            stop -> ok
                        end
                    end)
                 || _ <- lists:seq(1, NumInstances)
                ],

                %% Wait for all to be ready
                [
                    receive
                        ready -> ok
                    after 1000 -> ?assert(false)
                    end
                 || _ <- lists:seq(1, NumInstances)
                ],

                %% Register all concurrently (spawn registration tasks)
                _RegPids = [
                    spawn(fun() ->
                        ok = beamtalk_object_instances:register('Counter', Pid),
                        Self ! {registered, Pid}
                    end)
                 || Pid <- Instances
                ],

                %% Wait for all registrations to complete
                [
                    receive
                        {registered, _} -> ok
                    after 1000 -> ?assert(false)
                    end
                 || _ <- lists:seq(1, NumInstances)
                ],

                %% Verify count

                %% register/2 is a synchronous call, so the rows are already visible
                ?assertEqual(NumInstances, beamtalk_object_instances:count('Counter')),

                %% Clean up
                [Pid ! stop || Pid <- Instances],
                ok = beamtalk_test_wait:until(fun() ->
                    beamtalk_object_instances:all('Counter') =:= []
                end),

                %% Verify all cleaned up
                ?assertEqual(0, beamtalk_object_instances:count('Counter'))
            end)
        ]
    end}.

terminate_callback_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Get the state
                State = sys:get_state(Pid),

                %% Call terminate callback
                ok = beamtalk_object_instances:terminate(normal, State),

                %% This just verifies terminate/2 doesn't crash
                Instance ! stop
            end)
        ]
    end}.

handle_cast_ignored_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Send a cast message (not part of API, should be ignored)
                gen_server:cast(Pid, {unknown, cast}),

                %% Wait until the server has processed the cast
                ok = beamtalk_test_wait:sync(Pid),

                %% Verify server still works
                ?assertEqual(1, beamtalk_object_instances:count('Counter')),

                %% Clean up
                Instance ! stop
            end)
        ]
    end}.

unknown_call_format_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Register an instance
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', Instance),

                %% Send a malformed call request
                Result = gen_server:call(Pid, unknown_call_format),
                ?assertEqual({error, unknown_request}, Result),

                %% Clean up
                Instance ! stop
            end)
        ]
    end}.

handle_info_down_multiple_classes_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Register instance under multiple classes, then kill it
                %% This tests the DOWN handler's ability to clean up multiple entries
                Self = self(),
                Instance = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,

                %% Register under three different classes
                ok = beamtalk_object_instances:register('ClassA', Instance),
                ok = beamtalk_object_instances:register('ClassB', Instance),
                ok = beamtalk_object_instances:register('ClassC', Instance),

                %% Verify all registered
                ?assertEqual(1, beamtalk_object_instances:count('ClassA')),
                ?assertEqual(1, beamtalk_object_instances:count('ClassB')),
                ?assertEqual(1, beamtalk_object_instances:count('ClassC')),

                %% Kill the instance
                Instance ! stop,
                beamtalk_test_wait:dead(Instance),

                %% Verify cleaned up from all three classes
                ?assertEqual(0, beamtalk_object_instances:count('ClassA')),
                ?assertEqual(0, beamtalk_object_instances:count('ClassB')),
                ?assertEqual(0, beamtalk_object_instances:count('ClassC'))
            end)
        ]
    end}.

code_change_callback_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        [
            ?_test(begin
                %% Register some instances
                Self = self(),
                I1 = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                I2 = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,
                receive
                    ready -> ok
                end,

                ok = beamtalk_object_instances:register('Counter', I1),
                ok = beamtalk_object_instances:register('Counter', I2),

                %% Get state
                State = sys:get_state(Pid),

                %% Call code_change
                {ok, NewState} = beamtalk_object_instances:code_change(old_version, State, extra),

                %% State should be unchanged
                ?assertEqual(State, NewState),

                %% Verify server still works
                ?assertEqual(2, beamtalk_object_instances:count('Counter')),

                %% Clean up
                I1 ! stop,
                I2 ! stop
            end)
        ]
    end}.

%%====================================================================
%% BT-4845: register/2 writes the rows in the caller, casts only the monitor
%%====================================================================

%% An actor that has already exited when it is registered (the buccaneer
%% stream shape: `continueWith: #run` then `stopWith:`) must not leave a row
%% behind: the monitor cast arrives after the insert, gets an immediate
%% `noproc` DOWN, and the server removes the rows.
register_of_already_dead_pid_leaves_no_rows_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                {Pid, Ref} = spawn_monitor(fun() -> ok end),
                receive
                    {'DOWN', Ref, process, Pid, _} -> ok
                end,
                ok = beamtalk_object_instances:register('Counter', Pid),
                ok = beamtalk_test_wait:until(fun() ->
                    ets:lookup(beamtalk_instance_registry, 'Counter') =:= [] andalso
                        beamtalk_object_instances:class_of(Pid) =:= undefined
                end)
            end)
        ]
    end}.

%% Both lookups see a live instance the moment register/2 returns, with no
%% wait for the server to process anything.
register_is_visible_on_return_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Pids = [spawn(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, 50)],
                lists:foreach(
                    fun(P) ->
                        ok = beamtalk_object_instances:register('Counter', P),
                        ?assertEqual('Counter', beamtalk_object_instances:class_of(P)),
                        ?assert(lists:member(P, beamtalk_object_instances:all('Counter')))
                    end,
                    Pids
                ),
                ?assertEqual(50, beamtalk_object_instances:count('Counter')),
                [P ! stop || P <- Pids],
                ok = beamtalk_test_wait:until(fun() ->
                    beamtalk_object_instances:all('Counter') =:= []
                end)
            end)
        ]
    end}.

%% unregister/2 right after register/2 removes the rows even if the server has
%% not yet handled the monitor cast, and the late cast does not bring them back.
unregister_right_after_register_wins_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                P = spawn(fun() -> receive stop -> ok end end),
                ok = beamtalk_object_instances:register('Counter', P),
                ok = beamtalk_object_instances:unregister('Counter', P),
                %% Flush the server's mailbox: the cast was queued first.
                _ = sys:get_state(beamtalk_object_instances),
                ?assertEqual([], ets:lookup(beamtalk_instance_registry, 'Counter')),
                ?assertEqual(undefined, beamtalk_object_instances:class_of(P)),
                P ! stop
            end)
        ]
    end}.
