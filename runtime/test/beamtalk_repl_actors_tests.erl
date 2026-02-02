%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Tests for REPL actor registry

-module(beamtalk_repl_actors_tests).
-include_lib("eunit/include/eunit.hrl").

%%% ===========================================================================
%%% Registry Lifecycle Tests
%%% ===========================================================================

registry_starts_and_stops_test() ->
    %% Start registry
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    ?assert(is_process_alive(RegistryPid)),
    
    %% Stop registry
    gen_server:stop(RegistryPid),
    ?assertNot(is_process_alive(RegistryPid)).

%%% ===========================================================================
%%% Actor Registration Tests
%%% ===========================================================================

register_actor_stores_metadata_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    
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
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
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
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    {ok, ActorPid} = test_counter:start_link(0),
    
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    {ok, _} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),
    
    %% Kill the actor
    gen_server:stop(ActorPid),
    
    %% Give monitor time to process DOWN message
    timer:sleep(50),
    
    %% Verify it's automatically unregistered
    ?assertEqual({error, not_found}, beamtalk_repl_actors:get_actor(RegistryPid, ActorPid)),
    
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% List Actors Tests
%%% ===========================================================================

list_actors_returns_empty_initially_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    ?assertEqual([], Actors),
    
    gen_server:stop(RegistryPid).

list_actors_returns_all_registered_actors_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    
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
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    {ok, ActorPid} = test_counter:start_link(0),
    
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    ?assert(is_process_alive(ActorPid)),
    
    %% Kill actor - use process flag to not propagate the kill signal to test process
    process_flag(trap_exit, true),
    ok = beamtalk_repl_actors:kill_actor(RegistryPid, ActorPid),
    
    %% Give time for termination
    timer:sleep(50),
    
    %% Verify actor is dead
    ?assertNot(is_process_alive(ActorPid)),
    
    gen_server:stop(RegistryPid).

kill_nonexistent_actor_returns_error_test() ->
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    
    %% Try to kill a non-registered PID
    FakePid = spawn(fun() -> receive _ -> ok end end),
    ?assertEqual({error, not_found}, beamtalk_repl_actors:kill_actor(RegistryPid, FakePid)),
    
    %% Cleanup fake PID
    exit(FakePid, kill),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Registry Termination Tests
%%% ===========================================================================

registry_termination_kills_all_actors_test() ->
    process_flag(trap_exit, true),
    
    {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
    
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
    
    %% Give time for shutdown signals
    timer:sleep(100),
    
    %% All actors should be dead
    ?assertNot(is_process_alive(Actor1)),
    ?assertNot(is_process_alive(Actor2)),
    ?assertNot(is_process_alive(Actor3)).
