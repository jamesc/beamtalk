%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_meta_tests).

-moduledoc """
Unit tests for beamtalk_workspace_meta module

Tests workspace metadata tracking and activity updates.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Test helper

test_metadata() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/bt_test/workspace">>,
        % Use seconds, not milliseconds
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }.

%%% Metadata initialization tests

init_state_test() ->
    %% Test that init/1 creates proper state
    %% We can't directly test records, so just verify init succeeds
    InitMeta = test_metadata(),
    {ok, _State} = beamtalk_workspace_meta:init(InitMeta),
    ok.

%%% Activity tracking tests

update_activity_updates_timestamp_test() ->
    %% Ensure no previous process
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    %% Start the server
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Get initial metadata
    {ok, InitialMeta} = beamtalk_workspace_meta:get_metadata(),
    InitialActivity = maps:get(last_activity, InitialMeta),

    %% Wait a bit to ensure timestamp changes (seconds resolution)
    timer:sleep(1100),

    %% Update activity
    ok = beamtalk_workspace_meta:update_activity(),

    %% Get updated metadata
    {ok, UpdatedMeta} = beamtalk_workspace_meta:get_metadata(),
    UpdatedActivity = maps:get(last_activity, UpdatedMeta),

    %% last_activity should have changed
    ?assert(UpdatedActivity > InitialActivity),

    %% Other fields should remain the same
    ?assertEqual(maps:get(workspace_id, InitialMeta), maps:get(workspace_id, UpdatedMeta)),
    ?assertEqual(maps:get(project_path, InitialMeta), maps:get(project_path, UpdatedMeta)),
    ?assertEqual(maps:get(created_at, InitialMeta), maps:get(created_at, UpdatedMeta)),

    %% Cleanup
    gen_server:stop(Pid).

get_last_activity_test() ->
    %% Ensure no previous process
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    %% Start the server
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Get last activity
    {ok, Activity1} = beamtalk_workspace_meta:get_last_activity(),
    ?assert(is_integer(Activity1)),

    %% Update activity

    % Ensure time passes (seconds resolution)
    timer:sleep(1100),
    ok = beamtalk_workspace_meta:update_activity(),

    %% Get updated activity
    {ok, Activity2} = beamtalk_workspace_meta:get_last_activity(),
    ?assert(Activity2 > Activity1),

    %% Cleanup
    gen_server:stop(Pid).

%%% Metadata retrieval tests

get_metadata_includes_all_fields_test() ->
    %% Ensure no previous process
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    %% Start the server with repl_port
    Meta = (test_metadata())#{repl_port => 9001},
    {ok, Pid} = beamtalk_workspace_meta:start_link(Meta),

    %% Get metadata
    {ok, M} = beamtalk_workspace_meta:get_metadata(),

    %% Should have all required fields (including new ones)
    ?assert(maps:is_key(workspace_id, M)),
    ?assert(maps:is_key(project_path, M)),
    ?assert(maps:is_key(created_at, M)),
    ?assert(maps:is_key(last_activity, M)),
    ?assert(maps:is_key(node_name, M)),
    ?assert(maps:is_key(repl_port, M)),
    ?assert(maps:is_key(supervised_actors, M)),
    ?assert(maps:is_key(loaded_modules, M)),

    %% Verify values
    ?assertEqual(<<"test123">>, maps:get(workspace_id, M)),
    ?assertEqual(<<"/bt_test/workspace">>, maps:get(project_path, M)),
    ?assertEqual(9001, maps:get(repl_port, M)),
    ?assertEqual([], maps:get(supervised_actors, M)),
    ?assert(is_atom(maps:get(node_name, M))),

    %% Cleanup
    gen_server:stop(Pid).

%%% Gen_server behavior tests - Removed (test public API instead)
%%% These test internal implementation details (records) which are private

%%% Public API correctness tests

get_metadata_when_not_started_test() ->
    %% Ensure no server is running
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,

    %% Should return error when not started
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:get_metadata()).

get_last_activity_when_not_started_test() ->
    %% Ensure no server is running
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,

    %% Should return error when not started
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:get_last_activity()).

%%% Alias test

get_alias_returns_same_as_get_metadata_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    ?assertEqual(
        beamtalk_workspace_meta:get_metadata(),
        beamtalk_workspace_meta:get()
    ),
    gen_server:stop(Pid).

%%% Actor tracking tests

register_actor_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Initially no actors
    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),

    %% Spawn a process to register
    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    % Let cast process
    timer:sleep(50),

    {ok, Actors} = beamtalk_workspace_meta:supervised_actors(),
    ?assertEqual([ActorPid], Actors),

    %% Registering same PID again should not duplicate
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    timer:sleep(50),
    {ok, Actors2} = beamtalk_workspace_meta:supervised_actors(),
    ?assertEqual([ActorPid], Actors2),

    %% Cleanup
    ActorPid ! stop,
    gen_server:stop(Pid).

unregister_actor_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    timer:sleep(50),

    ok = beamtalk_workspace_meta:unregister_actor(ActorPid),
    timer:sleep(50),

    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),

    ActorPid ! stop,
    gen_server:stop(Pid).

actor_down_cleanup_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Spawn and register an actor that will exit
    ActorPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    timer:sleep(50),

    {ok, [ActorPid]} = beamtalk_workspace_meta:supervised_actors(),

    %% Kill the actor - DOWN message should auto-remove it
    ActorPid ! stop,
    timer:sleep(100),

    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),

    gen_server:stop(Pid).

%%% Module tracking tests

register_module_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Get initial module count
    {ok, InitialModules} = beamtalk_workspace_meta:loaded_modules(),
    InitialCount = length(InitialModules),

    %% Register a fresh module name unlikely to already exist
    % elp:fixme W0023 intentional atom creation
    ModuleName = list_to_atom("test_module_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),

    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assertEqual(InitialCount + 1, length(Modules)),
    ?assert(lists:keymember(ModuleName, 1, Modules)),

    %% Registering same module again should not duplicate
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),
    {ok, Modules2} = beamtalk_workspace_meta:loaded_modules(),
    ?assertEqual(InitialCount + 1, length(Modules2)),

    gen_server:stop(Pid).

%%% Not-started behavior tests for new APIs

supervised_actors_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:supervised_actors()).

loaded_modules_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:loaded_modules()).

register_actor_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    %% Should not crash
    ?assertEqual(ok, beamtalk_workspace_meta:register_actor(self())).

register_module_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    %% Should not crash
    ?assertEqual(ok, beamtalk_workspace_meta:register_module(some_module)).

%% BT-1239: unregister_module removes a module from the loaded_modules map.
unregister_module_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    % elp:fixme W0023 intentional atom creation
    ModuleName = list_to_atom(
        "test_unrg_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),
    {ok, Modules1} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:keymember(ModuleName, 1, Modules1)),

    ok = beamtalk_workspace_meta:unregister_module(ModuleName),
    timer:sleep(50),
    {ok, Modules2} = beamtalk_workspace_meta:loaded_modules(),
    ?assertNot(lists:keymember(ModuleName, 1, Modules2)),

    gen_server:stop(Pid).

unregister_module_nonexistent_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    %% Unregistering a module that was never registered should not crash.
    ?assertEqual(ok, beamtalk_workspace_meta:unregister_module(never_registered_xyz)),
    timer:sleep(50),

    gen_server:stop(Pid).

unregister_module_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    %% Should not crash when server is not running.
    ?assertEqual(ok, beamtalk_workspace_meta:unregister_module(some_module)).

%%% Persistence round-trip tests

persist_and_restore_modules_test() ->
    %% Use a unique workspace ID so we control the metadata file
    WsId = <<"persist_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    MetaFile = metadata_path_for(WsId),
    MetaDir = filename:dirname(MetaFile),

    %% Clean up any leftover file
    _ = file:delete(MetaFile),

    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    %% Start server, register a module, then stop (triggers terminate persist)
    {ok, Pid1} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/persist">>,
        created_at => 1000000
    }),
    ok = beamtalk_workspace_meta:register_module(lists),
    ok = beamtalk_workspace_meta:register_module(maps, "/bt_test/src/maps_test.bt"),
    %% Wait for debounce timer to fire
    timer:sleep(2500),
    gen_server:stop(Pid1),
    timer:sleep(50),

    %% Verify file was written
    ?assert(filelib:is_file(MetaFile)),

    %% Start a new server with same workspace - modules should be restored
    {ok, Pid2} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/persist">>,
        created_at => 1000000
    }),

    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:keymember(lists, 1, Modules)),
    %% Source path should survive the persist/restore round-trip
    ?assertEqual({maps, "/bt_test/src/maps_test.bt"}, lists:keyfind(maps, 1, Modules)),
    %% Module registered without source should have undefined
    ?assertEqual({lists, undefined}, lists:keyfind(lists, 1, Modules)),

    %% Actors should NOT be restored (always fresh)
    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),

    %% Cleanup
    gen_server:stop(Pid2),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).

load_corrupt_json_falls_back_test() ->
    %% Use a unique workspace ID
    WsId = <<"corrupt_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    MetaFile = metadata_path_for(WsId),
    MetaDir = filename:dirname(MetaFile),

    %% Write corrupt JSON
    filelib:ensure_dir(MetaFile),
    ok = file:write_file(MetaFile, <<"not valid json {{{{">>),

    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    %% Should start successfully despite corrupt file
    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/corrupt">>,
        created_at => 2000000
    }),

    %% Should have default state (not crash)
    {ok, Meta} = beamtalk_workspace_meta:get_metadata(),
    ?assertEqual(<<"/bt_test/corrupt">>, maps:get(project_path, Meta)),
    ?assertEqual([], maps:get(supervised_actors, Meta)),
    ?assertEqual(2000000, maps:get(created_at, Meta)),

    gen_server:stop(Pid),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).

%%% Class source storage tests (BT-1174)

set_get_class_source_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Unknown class returns undefined
    ?assertEqual(undefined, beamtalk_workspace_meta:get_class_source(<<"Unknown">>)),

    %% Store and retrieve source text
    Source = "Object subclass: Counter [\n  count := 0\n]\n",
    ok = beamtalk_workspace_meta:set_class_source(<<"Counter">>, Source),
    ?assertEqual(Source, beamtalk_workspace_meta:get_class_source(<<"Counter">>)),

    %% Overwrite with updated source
    Source2 = "Object subclass: Counter [\n  count := 0\n  increment => count := count + 1\n]\n",
    ok = beamtalk_workspace_meta:set_class_source(<<"Counter">>, Source2),
    ?assertEqual(Source2, beamtalk_workspace_meta:get_class_source(<<"Counter">>)),

    %% Other class names are still undefined
    ?assertEqual(undefined, beamtalk_workspace_meta:get_class_source(<<"Point">>)),

    gen_server:stop(Pid).

set_class_source_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    %% Should not crash
    ?assertEqual(ok, beamtalk_workspace_meta:set_class_source(<<"Foo">>, "source")).

get_class_source_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end,
    %% Should return undefined gracefully
    ?assertEqual(undefined, beamtalk_workspace_meta:get_class_source(<<"Foo">>)).

%%% Debounce coalescing test

debounce_coalesces_rapid_changes_test() ->
    %% Use a unique workspace ID to control the metadata file
    WsId = <<"debounce_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    MetaFile = metadata_path_for(WsId),
    MetaDir = filename:dirname(MetaFile),

    %% Clean up any leftover file
    _ = file:delete(MetaFile),

    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/debounce">>,
        created_at => 3000000
    }),

    %% Register 3 modules in rapid succession (< 2s debounce window)
    % elp:fixme W0023 intentional atom creation
    Mod1 = list_to_atom("debounce_mod1_" ++ integer_to_list(erlang:unique_integer([positive]))),
    % elp:fixme W0023 intentional atom creation
    Mod2 = list_to_atom("debounce_mod2_" ++ integer_to_list(erlang:unique_integer([positive]))),
    % elp:fixme W0023 intentional atom creation
    Mod3 = list_to_atom("debounce_mod3_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = beamtalk_workspace_meta:register_module(Mod1),
    timer:sleep(100),
    ok = beamtalk_workspace_meta:register_module(Mod2),
    timer:sleep(100),
    ok = beamtalk_workspace_meta:register_module(Mod3),

    %% All 3 should be in state immediately
    timer:sleep(50),
    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:keymember(Mod1, 1, Modules)),
    ?assert(lists:keymember(Mod2, 1, Modules)),
    ?assert(lists:keymember(Mod3, 1, Modules)),

    %% Wait for debounce to fire (2s from LAST change, not first)
    timer:sleep(2500),

    %% Verify all 3 modules were persisted to disk
    ?assert(filelib:is_file(MetaFile)),
    {ok, Binary} = file:read_file(MetaFile),
    Map = json:decode(Binary),
    PersistedModules = maps:get(<<"loaded_modules">>, Map, []),
    PersistedNames = [maps:get(<<"name">>, E) || E <- PersistedModules, is_map(E)],
    ?assert(lists:member(atom_to_binary(Mod1, utf8), PersistedNames)),
    ?assert(lists:member(atom_to_binary(Mod2, utf8), PersistedNames)),
    ?assert(lists:member(atom_to_binary(Mod3, utf8), PersistedNames)),

    %% Cleanup
    gen_server:stop(Pid),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).

%%% Run mode tests (repl=false, BT-1317)

run_mode_no_disk_write_test() ->
    %% In run mode (repl=false), no entry should be written to ~/.beamtalk/workspaces/
    WsId = <<"run_mode_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    %% Mirror beamtalk_workspace_meta's metadata path computation exactly.
    MetaFile = metadata_path_for(WsId),

    %% Ensure no leftover file
    _ = file:delete(MetaFile),

    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/run_mode">>,
        created_at => erlang:system_time(second),
        repl => false
    }),

    %% Register a module and wait longer than the debounce window
    ok = beamtalk_workspace_meta:register_module(lists),
    timer:sleep(2500),

    %% No file should have been written
    ?assertNot(filelib:is_file(MetaFile)),

    gen_server:stop(Pid).

run_mode_metadata_accessible_test() ->
    %% Even in run mode, get_metadata/0 must work normally
    WsId = <<"run_meta_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,

    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/bt_test/run_meta">>,
        created_at => erlang:system_time(second),
        repl => false
    }),

    {ok, Meta} = beamtalk_workspace_meta:get_metadata(),
    ?assertEqual(WsId, maps:get(workspace_id, Meta)),
    ?assertEqual(<<"/bt_test/run_meta">>, maps:get(project_path, Meta)),

    gen_server:stop(Pid).

%%% BT-1242: class_removed cast triggered by classRemoveFromSystemByName

class_removed_cast_unregisters_module_test() ->
    %% Verify that casting {unregister_module, Module} to beamtalk_workspace_meta
    %% (which is what publish_class_removed/2 does) removes the module entry.
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        OldPid ->
            gen_server:stop(OldPid),
            timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),

    % elp:fixme W0023 intentional atom creation
    ModuleName = list_to_atom(
        "bt1242_meta_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),
    {ok, Modules1} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:keymember(ModuleName, 1, Modules1)),

    %% Simulate the direct cast from publish_class_removed/2
    gen_server:cast(beamtalk_workspace_meta, {unregister_module, ModuleName}),
    timer:sleep(50),
    {ok, Modules2} = beamtalk_workspace_meta:loaded_modules(),
    ?assertNot(lists:keymember(ModuleName, 1, Modules2)),

    gen_server:stop(Pid).

%%% Test helpers

%% Mirror beamtalk_workspace_meta's metadata_path computation so tests check
%% the same file the module would write to.
metadata_path_for(WsId) ->
    Base =
        case beamtalk_platform:home_dir() of
            false ->
                filename:join(filename:basedir(user_cache, "beamtalk"), "workspaces");
            Home ->
                filename:join([Home, ".beamtalk", "workspaces"])
        end,
    filename:join([Base, binary_to_list(WsId), "metadata.json"]).
