%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for the beamtalk_instances registry.
%%% Tests cover registration, lookup, auto-cleanup on process death,
%%% and iteration over instances.
-module(beamtalk_instances_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Start the instances server
    {ok, Pid} = beamtalk_instances:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% Tests
%%====================================================================

register_and_all_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn a process to act as an instance
              Self = self(),
              Instance = spawn(fun() ->
                  Self ! ready,
                  receive stop -> ok end
              end),
              receive ready -> ok end,
              
              %% Register it
              ok = beamtalk_instances:register('Counter', Instance),
              
              %% Should be in all instances
              ?assertEqual([Instance], beamtalk_instances:all('Counter')),
              
              %% Should not be in other class
              ?assertEqual([], beamtalk_instances:all('Point')),
              
              %% Clean up
              Instance ! stop
          end)]
     end}.

count_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Initially zero
              ?assertEqual(0, beamtalk_instances:count('Counter')),
              
              %% Spawn three instances
              Self = self(),
              Spawn = fun() -> 
                  spawn(fun() ->
                      Self ! ready,
                      receive stop -> ok end
                  end)
              end,
              I1 = Spawn(),
              I2 = Spawn(),
              I3 = Spawn(),
              receive ready -> ok end,
              receive ready -> ok end,
              receive ready -> ok end,
              
              %% Register them
              ok = beamtalk_instances:register('Counter', I1),
              ok = beamtalk_instances:register('Counter', I2),
              ok = beamtalk_instances:register('Counter', I3),
              
              %% Count should be 3
              ?assertEqual(3, beamtalk_instances:count('Counter')),
              
              %% Clean up
              I1 ! stop,
              I2 ! stop,
              I3 ! stop
          end)]
     end}.

unregister_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn and register an instance
              Self = self(),
              Instance = spawn(fun() ->
                  Self ! ready,
                  receive stop -> ok end
              end),
              receive ready -> ok end,
              
              ok = beamtalk_instances:register('Counter', Instance),
              ?assertEqual(1, beamtalk_instances:count('Counter')),
              
              %% Unregister it
              ok = beamtalk_instances:unregister('Counter', Instance),
              ?assertEqual(0, beamtalk_instances:count('Counter')),
              ?assertEqual([], beamtalk_instances:all('Counter')),
              
              %% Unregistering again should be ok
              ok = beamtalk_instances:unregister('Counter', Instance),
              
              %% Clean up
              Instance ! stop
          end)]
     end}.

auto_cleanup_on_process_death_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn and register an instance
              Self = self(),
              Instance = spawn(fun() ->
                  Self ! ready,
                  receive stop -> ok end
              end),
              receive ready -> ok end,
              
              ok = beamtalk_instances:register('Counter', Instance),
              ?assertEqual(1, beamtalk_instances:count('Counter')),
              
              %% Kill the process
              Instance ! stop,
              
              %% Give the monitor time to fire
              timer:sleep(50),
              
              %% Should be automatically removed
              ?assertEqual(0, beamtalk_instances:count('Counter')),
              ?assertEqual([], beamtalk_instances:all('Counter'))
          end)]
     end}.

duplicate_register_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn an instance
              Self = self(),
              Instance = spawn(fun() ->
                  Self ! ready,
                  receive stop -> ok end
              end),
              receive ready -> ok end,
              
              %% Register twice - should be idempotent
              ok = beamtalk_instances:register('Counter', Instance),
              ok = beamtalk_instances:register('Counter', Instance),
              
              %% Count should still be 1
              ?assertEqual(1, beamtalk_instances:count('Counter')),
              
              %% Clean up
              Instance ! stop
          end)]
     end}.

multiple_classes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn instances for different classes
              Self = self(),
              Spawn = fun() -> 
                  spawn(fun() ->
                      Self ! ready,
                      receive stop -> ok end
                  end)
              end,
              Counter1 = Spawn(),
              Counter2 = Spawn(),
              Point1 = Spawn(),
              receive ready -> ok end,
              receive ready -> ok end,
              receive ready -> ok end,
              
              %% Register under different classes
              ok = beamtalk_instances:register('Counter', Counter1),
              ok = beamtalk_instances:register('Counter', Counter2),
              ok = beamtalk_instances:register('Point', Point1),
              
              %% Check counts
              ?assertEqual(2, beamtalk_instances:count('Counter')),
              ?assertEqual(1, beamtalk_instances:count('Point')),
              
              %% Check all instances
              CounterInstances = beamtalk_instances:all('Counter'),
              ?assertEqual(2, length(CounterInstances)),
              ?assert(lists:member(Counter1, CounterInstances)),
              ?assert(lists:member(Counter2, CounterInstances)),
              
              ?assertEqual([Point1], beamtalk_instances:all('Point')),
              
              %% Clean up
              Counter1 ! stop,
              Counter2 ! stop,
              Point1 ! stop
          end)]
     end}.

each_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
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
              receive ready -> ok end,
              receive ready -> ok end,
              
              ok = beamtalk_instances:register('Counter', I1),
              ok = beamtalk_instances:register('Counter', I2),
              
              %% Use each to send a message to all instances
              beamtalk_instances:each('Counter', fun(P) ->
                  P ! {ping, Self}
              end),
              
              %% Should receive pong from both
              receive pong -> ok after 100 -> ?assert(false) end,
              receive pong -> ok after 100 -> ?assert(false) end,
              
              %% Clean up
              I1 ! stop,
              I2 ! stop
          end)]
     end}.

each_empty_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% each on empty class should do nothing
              CallCount = erlang:make_ref(),
              put(CallCount, 0),
              beamtalk_instances:each('NonExistent', fun(_P) ->
                  put(CallCount, get(CallCount) + 1)
              end),
              ?assertEqual(0, get(CallCount))
          end)]
     end}.

filters_dead_processes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
              %% Spawn two instances
              Self = self(),
              Spawn = fun() -> 
                  spawn(fun() ->
                      Self ! ready,
                      receive stop -> ok end
                  end)
              end,
              I1 = Spawn(),
              I2 = Spawn(),
              receive ready -> ok end,
              receive ready -> ok end,
              
              ok = beamtalk_instances:register('Counter', I1),
              ok = beamtalk_instances:register('Counter', I2),
              
              %% Kill one
              I1 ! stop,
              timer:sleep(50),
              
              %% all() should only return the live one
              Instances = beamtalk_instances:all('Counter'),
              ?assertEqual([I2], Instances),
              
              %% count() should be 1
              ?assertEqual(1, beamtalk_instances:count('Counter')),
              
              %% Clean up
              I2 ! stop
          end)]
     end}.

ets_table_ownership_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
              %% Verify that the ETS table is owned by the gen_server process
              TableInfo = ets:info(beamtalk_instance_registry),
              ?assertNotEqual(undefined, TableInfo),
              
              Owner = proplists:get_value(owner, TableInfo),
              ?assertEqual(Pid, Owner),
              
              %% This confirms that when the gen_server crashes (without supervisor),
              %% the ETS table will be automatically deleted by the VM.
              %% In production with supervision, the supervisor would restart
              %% the gen_server and it would recreate the table in init/1.
              ok
          end)]
     end}.

monitor_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
              %% Spawn and register an instance
              Self = self(),
              Instance = spawn(fun() ->
                  Self ! ready,
                  receive stop -> ok end
              end),
              receive ready -> ok end,
              
              ok = beamtalk_instances:register('Counter', Instance),
              
              %% Get the state to verify monitor is registered
              State1 = sys:get_state(Pid),
              Monitors1 = element(2, State1),  % #state.monitors
              ?assertEqual(1, maps:size(Monitors1)),
              ?assert(maps:is_key({'Counter', Instance}, Monitors1)),
              
              %% Kill the instance
              Instance ! stop,
              timer:sleep(50),
              
              %% Verify monitor is cleaned up from state
              State2 = sys:get_state(Pid),
              Monitors2 = element(2, State2),
              ?assertEqual(0, maps:size(Monitors2)),
              ?assertNot(maps:is_key({'Counter', Instance}, Monitors2))
          end)]
     end}.
