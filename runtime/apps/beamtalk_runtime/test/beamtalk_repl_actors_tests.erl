%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_actors_tests).

-moduledoc "Tests for REPL actor registry".
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Registry Lifecycle Tests
%%% ===========================================================================

registry_starts_and_stops_test() ->
    %% Start registry
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    ?assert(is_process_alive(RegistryPid)),

    %% Stop registry
    gen_server:stop(RegistryPid),
    ?assertNot(is_process_alive(RegistryPid)).

%%% ===========================================================================
%%% Actor Registration Tests
%%% ===========================================================================

register_actor_stores_metadata_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),

    %% Spawn a test counter actor
    {ok, ActorPid} = test_counter:start_link(0),

    %% Register it
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),

    %% Verify it's in the registry
    {ok, Metadata} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),
    ?assertEqual(ActorPid, maps:get(pid, Metadata)),
    ?assertEqual('Counter', maps:get(class, Metadata)),
    ?assertEqual(test_counter, maps:get(module, Metadata)),
    ?assert(is_integer(maps:get(spawned_at, Metadata))),

    %% Cleanup
    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

unregister_actor_removes_from_registry_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    {ok, _} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),

    %% Unregister
    ok = beamtalk_repl_actors:unregister_actor(RegistryPid, ActorPid),

    %% Verify it's gone
    ?assertEqual({error, not_found}, beamtalk_repl_actors:get_actor(RegistryPid, ActorPid)),

    %% Cleanup
    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

actor_termination_auto_unregisters_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    {ok, _} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),

    %% Kill the actor
    gen_server:stop(ActorPid),

    %% Wait for the registry to process the monitor's DOWN message
    ok = beamtalk_test_wait:until(fun() ->
        beamtalk_repl_actors:get_actor(RegistryPid, ActorPid) =:= {error, not_found}
    end),

    %% Verify it's automatically unregistered
    ?assertEqual({error, not_found}, beamtalk_repl_actors:get_actor(RegistryPid, ActorPid)),

    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% List Actors Tests
%%% ===========================================================================

list_actors_returns_empty_initially_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),

    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    ?assertEqual([], Actors),

    gen_server:stop(RegistryPid).

list_actors_returns_all_registered_actors_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),

    %% Register multiple actors
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    {ok, Actor3} = test_counter:start_link(20),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor3, 'OtherClass', other_module),

    %% List actors
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    ?assertEqual(3, length(Actors)),

    %% Verify PIDs are present
    Pids = [maps:get(pid, A) || A <- Actors],
    ?assert(lists:member(Actor1, Pids)),
    ?assert(lists:member(Actor2, Pids)),
    ?assert(lists:member(Actor3, Pids)),

    %% Cleanup
    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(Actor3),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Kill Actor Tests
%%% ===========================================================================

kill_actor_terminates_actor_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    ?assert(is_process_alive(ActorPid)),

    %% Kill actor - use process flag to not propagate the kill signal to test process
    OldTrapExit = process_flag(trap_exit, true),
    ok = beamtalk_repl_actors:kill_actor(RegistryPid, ActorPid),

    %% Wait for termination
    ok = beamtalk_test_wait:dead(ActorPid),

    %% Verify actor is dead
    ?assertNot(is_process_alive(ActorPid)),

    process_flag(trap_exit, OldTrapExit),
    gen_server:stop(RegistryPid).

kill_nonexistent_actor_returns_error_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),

    %% Try to kill a non-registered PID
    FakePid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    ?assertEqual({error, not_found}, beamtalk_repl_actors:kill_actor(RegistryPid, FakePid)),

    %% Cleanup fake PID
    exit(FakePid, kill),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Registry Termination Tests
%%% ===========================================================================

registry_termination_leaves_actors_alive_test() ->
    OldTrapExit = process_flag(trap_exit, true),

    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),

    %% Register multiple actors
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    {ok, Actor3} = test_counter:start_link(20),

    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor3, 'Counter', test_counter),

    ?assert(is_process_alive(Actor1)),
    ?assert(is_process_alive(Actor2)),
    ?assert(is_process_alive(Actor3)),

    %% Stop registry
    gen_server:stop(RegistryPid),

    %% sleep: asserts nothing happens within a window (class 4)
    timer:sleep(100),

    %% Actors are owned by their supervisors, not by the registry
    ?assert(is_process_alive(Actor1)),
    ?assert(is_process_alive(Actor2)),
    ?assert(is_process_alive(Actor3)),
    lists:foreach(
        fun(P) ->
            unlink(P),
            exit(P, kill)
        end,
        [Actor1, Actor2, Actor3]
    ),

    process_flag(trap_exit, OldTrapExit).

%%% ===========================================================================
%%% get_pids_for_module/2 Tests
%%% ===========================================================================

get_pids_for_module_empty_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, Pids} = beamtalk_repl_actors:get_pids_for_module(RegistryPid, nonexistent_module),
    ?assertEqual([], Pids),
    gen_server:stop(RegistryPid).

get_pids_for_module_single_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    {ok, Pids} = beamtalk_repl_actors:get_pids_for_module(RegistryPid, test_counter),
    ?assertEqual([ActorPid], Pids),
    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

get_pids_for_module_multiple_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    {ok, Actor3} = test_counter:start_link(20),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor3, 'OtherClass', other_module),
    {ok, Pids} = beamtalk_repl_actors:get_pids_for_module(RegistryPid, test_counter),
    ?assertEqual(2, length(Pids)),
    ?assert(lists:member(Actor1, Pids)),
    ?assert(lists:member(Actor2, Pids)),
    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(Actor3),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% count_actors_for_module/2 Direct Tests
%%% ===========================================================================

count_actors_for_module_zero_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, Count} = beamtalk_repl_actors:count_actors_for_module(RegistryPid, nonexistent),
    ?assertEqual(0, Count),
    gen_server:stop(RegistryPid).

count_actors_for_module_multiple_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, Actor2, 'Counter', test_counter),
    {ok, Count} = beamtalk_repl_actors:count_actors_for_module(RegistryPid, test_counter),
    ?assertEqual(2, Count),
    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Gen_server Callback Edge Case Tests
%%% ===========================================================================

unknown_call_returns_error_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    ?assertEqual({error, unknown_request}, gen_server:call(RegistryPid, some_random_request)),
    gen_server:stop(RegistryPid).

unknown_cast_is_ignored_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    gen_server:cast(RegistryPid, some_random_message),
    %% Registry should still be alive and functional
    ?assertEqual([], beamtalk_repl_actors:list_actors(RegistryPid)),
    gen_server:stop(RegistryPid).

unknown_info_is_ignored_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    RegistryPid ! {some_random, message},
    ok = beamtalk_test_wait:sync(RegistryPid),
    %% Registry should still be alive and functional
    ?assertEqual([], beamtalk_repl_actors:list_actors(RegistryPid)),
    gen_server:stop(RegistryPid).
