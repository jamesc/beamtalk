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

    %% Give monitor time to process DOWN message
    timer:sleep(50),

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
    process_flag(trap_exit, true),
    ok = beamtalk_repl_actors:kill_actor(RegistryPid, ActorPid),

    %% Give time for termination
    timer:sleep(50),

    %% Verify actor is dead
    ?assertNot(is_process_alive(ActorPid)),

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

registry_termination_kills_all_actors_test() ->
    process_flag(trap_exit, true),

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

    %% Give time for shutdown signals
    timer:sleep(100),

    %% All actors should be dead
    ?assertNot(is_process_alive(Actor1)),
    ?assertNot(is_process_alive(Actor2)),
    ?assertNot(is_process_alive(Actor3)).

%%% ===========================================================================
%%% Callback Failure Tests (BT-391)
%%% ===========================================================================

on_actor_spawned_succeeds_with_running_registry_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),

    Result = beamtalk_repl_actors:on_actor_spawned(RegistryPid, ActorPid, 'Counter', test_counter),
    ?assertEqual(ok, Result),

    %% Verify actor was registered
    {ok, Metadata} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),
    ?assertEqual('Counter', maps:get(class, Metadata)),

    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

on_actor_spawned_returns_error_when_registry_down_test() ->
    %% Start and immediately stop a registry to get a dead pid
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    gen_server:stop(RegistryPid),
    timer:sleep(50),

    {ok, ActorPid} = test_counter:start_link(0),

    %% on_actor_spawned should return error, not crash
    Result = beamtalk_repl_actors:on_actor_spawned(RegistryPid, ActorPid, 'Counter', test_counter),
    ?assertMatch({error, {registry_failed, _}}, Result),

    gen_server:stop(ActorPid).

on_actor_spawned_returns_error_when_registry_not_a_process_test() ->
    %% Use a PID that was never a gen_server
    FakePid = spawn(fun() -> ok end),
    timer:sleep(50),

    {ok, ActorPid} = test_counter:start_link(0),

    Result = beamtalk_repl_actors:on_actor_spawned(FakePid, ActorPid, 'Counter', test_counter),
    ?assertMatch({error, {registry_failed, _}}, Result),

    gen_server:stop(ActorPid).

%%% ===========================================================================
%%% on_actor_spawned/4 Callback Integration Tests (from main)
%%% ===========================================================================

on_actor_spawned_registers_actor_in_registry_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),

    %% Call on_actor_spawned — should register actor in registry
    ok = beamtalk_repl_actors:on_actor_spawned(RegistryPid, ActorPid, 'Counter', test_counter),

    %% Verify actor appears in registry
    {ok, Metadata} = beamtalk_repl_actors:get_actor(RegistryPid, ActorPid),
    ?assertEqual(ActorPid, maps:get(pid, Metadata)),
    ?assertEqual('Counter', maps:get(class, Metadata)),
    ?assertEqual(test_counter, maps:get(module, Metadata)),

    %% Cleanup
    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

on_actor_spawned_returns_error_on_registry_failure_test() ->
    %% Use a dead registry PID — register_actor will fail with noproc
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    gen_server:stop(RegistryPid),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),

    %% Returns {error, _} since BT-391 surfaces registry failures
    ?assertMatch(
        {error, {registry_failed, _}},
        beamtalk_repl_actors:on_actor_spawned(RegistryPid, ActorPid, 'Counter', test_counter)
    ),

    exit(ActorPid, kill).

on_actor_spawned_multiple_actors_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, Actor1} = test_counter:start_link(0),
    {ok, Actor2} = test_counter:start_link(10),

    ok = beamtalk_repl_actors:on_actor_spawned(RegistryPid, Actor1, 'Counter', test_counter),
    ok = beamtalk_repl_actors:on_actor_spawned(RegistryPid, Actor2, 'Counter', test_counter),

    %% Both should be registered
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    ?assertEqual(2, length(Actors)),

    %% Cleanup
    gen_server:stop(Actor1),
    gen_server:stop(Actor2),
    gen_server:stop(RegistryPid).

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
    timer:sleep(10),
    %% Registry should still be alive and functional
    ?assertEqual([], beamtalk_repl_actors:list_actors(RegistryPid)),
    gen_server:stop(RegistryPid).

%%% ===========================================================================
%%% Subscriber Lifecycle Notification Tests (BT-690)
%%% ===========================================================================

subscriber_receives_actor_spawned_notification_test() ->
    process_flag(trap_exit, true),
    unregister_if_alive(beamtalk_actor_registry),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    try
        beamtalk_repl_actors:subscribe(),
        %% Sync point — ensures cast is processed
        sys:get_state(RegistryPid),

        {ok, ActorPid} = test_counter:start_link(0),
        ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),

        receive
            {actor_spawned, Metadata} ->
                ?assertEqual(ActorPid, maps:get(pid, Metadata)),
                ?assertEqual('Counter', maps:get(class, Metadata)),
                ?assertEqual(test_counter, maps:get(module, Metadata))
        after 500 ->
            ?assert(false)
        end,
        gen_server:stop(ActorPid)
    after
        gen_server:stop(RegistryPid),
        flush_messages()
    end.

subscriber_receives_actor_stopped_notification_test() ->
    process_flag(trap_exit, true),
    unregister_if_alive(beamtalk_actor_registry),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    try
        beamtalk_repl_actors:subscribe(),
        sys:get_state(RegistryPid),

        {ok, ActorPid} = test_counter:start_link(0),
        ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),

        receive
            {actor_spawned, _} -> ok
        after 500 -> ok
        end,

        gen_server:stop(ActorPid),

        receive
            {actor_stopped, StopInfo} ->
                ?assertEqual(ActorPid, maps:get(pid, StopInfo)),
                ?assertEqual('Counter', maps:get(class, StopInfo))
        after 500 ->
            ?assert(false)
        end
    after
        gen_server:stop(RegistryPid),
        flush_messages()
    end.

unsubscribe_stops_notifications_test() ->
    process_flag(trap_exit, true),
    unregister_if_alive(beamtalk_actor_registry),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    try
        beamtalk_repl_actors:subscribe(),
        sys:get_state(RegistryPid),

        beamtalk_repl_actors:unsubscribe(),
        sys:get_state(RegistryPid),

        {ok, ActorPid} = test_counter:start_link(0),
        ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),

        receive
            {actor_spawned, _} -> ?assert(false)
        after 100 ->
            ok
        end,
        gen_server:stop(ActorPid)
    after
        gen_server:stop(RegistryPid),
        flush_messages()
    end.

dead_subscriber_auto_removed_test() ->
    process_flag(trap_exit, true),
    unregister_if_alive(beamtalk_actor_registry),
    {ok, RegistryPid} = gen_server:start_link(
        {local, beamtalk_actor_registry}, beamtalk_repl_actors, [], []
    ),
    try
        Self = self(),
        SubPid = spawn(fun() ->
            beamtalk_repl_actors:subscribe(),
            Self ! subscribed,
            receive
                stop -> ok
            end
        end),
        receive
            subscribed -> ok
        after 500 -> ok
        end,
        %% Sync: ensure subscribe cast processed
        sys:get_state(RegistryPid),

        %% Verify subscriber was added
        {state, _A1, _M1, SubsBefore} = sys:get_state(RegistryPid),
        ?assertEqual(1, maps:size(SubsBefore)),

        exit(SubPid, kill),
        timer:sleep(50),

        %% Verify subscriber was auto-removed
        {state, _A2, _M2, SubsAfter} = sys:get_state(RegistryPid),
        ?assertEqual(0, maps:size(SubsAfter)),

        {ok, ActorPid} = test_counter:start_link(0),
        ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),

        ?assert(is_process_alive(RegistryPid)),
        gen_server:stop(ActorPid)
    after
        gen_server:stop(RegistryPid),
        flush_messages()
    end.

%% Helper: stop and unregister a named process if it's still alive.
unregister_if_alive(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end,
            case whereis(Name) of
                Pid -> unregister(Name);
                _ -> ok
            end
    end.

%% Helper: drain all messages from the test process mailbox.
flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 -> ok
    end.

%%% ===========================================================================
%%% Workspace App Callback Env Tests
%%% ===========================================================================

workspace_app_start_sets_callback_env_test() ->
    %% Ensure env is clean
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    ?assertEqual(undefined, application:get_env(beamtalk_runtime, actor_spawn_callback)),

    %% Call actual start/2 — supervisor has no static children, safe to start
    {ok, SupPid} = beamtalk_workspace_app:start(normal, []),
    try
        ?assertEqual(
            {ok, beamtalk_repl_actors},
            application:get_env(beamtalk_runtime, actor_spawn_callback)
        )
    after
        process_flag(trap_exit, true),
        exit(SupPid, shutdown),
        receive
            {'EXIT', SupPid, _} -> ok
        after 1000 -> ok
        end,
        application:unset_env(beamtalk_runtime, actor_spawn_callback)
    end.

workspace_app_stop_unsets_callback_env_test() ->
    %% Set the env first (simulating what start/2 does)
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_repl_actors),
    ?assertEqual(
        {ok, beamtalk_repl_actors},
        application:get_env(beamtalk_runtime, actor_spawn_callback)
    ),

    %% Call actual stop/1 function
    ok = beamtalk_workspace_app:stop(undefined),

    ?assertEqual(undefined, application:get_env(beamtalk_runtime, actor_spawn_callback)).
