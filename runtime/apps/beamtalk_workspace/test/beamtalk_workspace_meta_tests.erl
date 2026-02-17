%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace_meta module
%%%
%%% Tests workspace metadata tracking and activity updates.

-module(beamtalk_workspace_meta_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Test helper

test_metadata() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second),  % Use seconds, not milliseconds
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
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
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
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    
    %% Start the server
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    %% Get last activity
    {ok, Activity1} = beamtalk_workspace_meta:get_last_activity(),
    ?assert(is_integer(Activity1)),
    
    %% Update activity
    timer:sleep(1100),  % Ensure time passes (seconds resolution)
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
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
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
    ?assertEqual(<<"/tmp/test">>, maps:get(project_path, M)),
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
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    
    %% Should return error when not started
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:get_metadata()).

get_last_activity_when_not_started_test() ->
    %% Ensure no server is running
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    
    %% Should return error when not started
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:get_last_activity()).

%%% Alias test

get_alias_returns_same_as_get_metadata_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    ?assertEqual(beamtalk_workspace_meta:get_metadata(),
                 beamtalk_workspace_meta:get()),
    gen_server:stop(Pid).

%%% Actor tracking tests

register_actor_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    %% Initially no actors
    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),
    
    %% Spawn a process to register
    ActorPid = spawn(fun() -> receive stop -> ok end end),
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    timer:sleep(50),  % Let cast process
    
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
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    ActorPid = spawn(fun() -> receive stop -> ok end end),
    ok = beamtalk_workspace_meta:register_actor(ActorPid),
    timer:sleep(50),
    
    ok = beamtalk_workspace_meta:unregister_actor(ActorPid),
    timer:sleep(50),
    
    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),
    
    ActorPid ! stop,
    gen_server:stop(Pid).

actor_down_cleanup_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    %% Spawn and register an actor that will exit
    ActorPid = spawn(fun() -> receive stop -> ok end end),
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
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    %% Get initial module count
    {ok, InitialModules} = beamtalk_workspace_meta:loaded_modules(),
    InitialCount = length(InitialModules),
    
    %% Register a fresh module name unlikely to already exist
    ModuleName = list_to_atom("test_module_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),
    
    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assertEqual(InitialCount + 1, length(Modules)),
    ?assert(lists:member(ModuleName, Modules)),
    
    %% Registering same module again should not duplicate
    ok = beamtalk_workspace_meta:register_module(ModuleName),
    timer:sleep(50),
    {ok, Modules2} = beamtalk_workspace_meta:loaded_modules(),
    ?assertEqual(InitialCount + 1, length(Modules2)),
    
    gen_server:stop(Pid).

%%% Not-started behavior tests for new APIs

supervised_actors_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:supervised_actors()).

loaded_modules_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    ?assertEqual({error, not_started}, beamtalk_workspace_meta:loaded_modules()).

register_actor_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    %% Should not crash
    ?assertEqual(ok, beamtalk_workspace_meta:register_actor(self())).

register_module_when_not_started_test() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(10)
    end,
    %% Should not crash
    ?assertEqual(ok, beamtalk_workspace_meta:register_module(some_module)).

%%% Persistence round-trip tests

persist_and_restore_modules_test() ->
    %% Use a unique workspace ID so we control the metadata file
    WsId = <<"persist_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Home = case beamtalk_platform:home_dir() of
        false -> filename:basedir(user_cache, "beamtalk");
        HomeDir -> HomeDir
    end,
    MetaDir = filename:join([Home, ".beamtalk", "workspaces", binary_to_list(WsId)]),
    MetaFile = filename:join(MetaDir, "metadata.json"),
    
    %% Clean up any leftover file
    _ = file:delete(MetaFile),
    
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    
    %% Start server, register a module, then stop (triggers terminate persist)
    {ok, Pid1} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/tmp/persist_test">>,
        created_at => 1000000
    }),
    ok = beamtalk_workspace_meta:register_module(lists),
    %% Wait for debounce timer to fire
    timer:sleep(2500),
    gen_server:stop(Pid1),
    timer:sleep(50),
    
    %% Verify file was written
    ?assert(filelib:is_file(MetaFile)),
    
    %% Start a new server with same workspace - modules should be restored
    {ok, Pid2} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/tmp/persist_test">>,
        created_at => 1000000
    }),
    
    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:member(lists, Modules)),
    
    %% Actors should NOT be restored (always fresh)
    ?assertEqual({ok, []}, beamtalk_workspace_meta:supervised_actors()),
    
    %% Cleanup
    gen_server:stop(Pid2),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).

load_corrupt_json_falls_back_test() ->
    %% Use a unique workspace ID
    WsId = <<"corrupt_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Home = os:getenv("HOME", "/tmp"),
    MetaDir = filename:join([Home, ".beamtalk", "workspaces", binary_to_list(WsId)]),
    MetaFile = filename:join(MetaDir, "metadata.json"),
    
    %% Write corrupt JSON
    filelib:ensure_dir(MetaFile),
    ok = file:write_file(MetaFile, <<"not valid json {{{{">>),
    
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    
    %% Should start successfully despite corrupt file
    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/tmp/corrupt_test">>,
        created_at => 2000000
    }),
    
    %% Should have default state (not crash)
    {ok, Meta} = beamtalk_workspace_meta:get_metadata(),
    ?assertEqual(<<"/tmp/corrupt_test">>, maps:get(project_path, Meta)),
    ?assertEqual([], maps:get(supervised_actors, Meta)),
    ?assertEqual(2000000, maps:get(created_at, Meta)),
    
    gen_server:stop(Pid),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).

%%% Debounce coalescing test

debounce_coalesces_rapid_changes_test() ->
    %% Use a unique workspace ID to control the metadata file
    WsId = <<"debounce_test_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Home = case beamtalk_platform:home_dir() of
        false -> filename:basedir(user_cache, "beamtalk");
        HomeDir -> HomeDir
    end,
    MetaDir = filename:join([Home, ".beamtalk", "workspaces", binary_to_list(WsId)]),
    MetaFile = filename:join(MetaDir, "metadata.json"),
    
    %% Clean up any leftover file
    _ = file:delete(MetaFile),
    
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        OldPid -> gen_server:stop(OldPid), timer:sleep(10)
    end,
    
    {ok, Pid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WsId,
        project_path => <<"/tmp/debounce_test">>,
        created_at => 3000000
    }),
    
    %% Register 3 modules in rapid succession (< 2s debounce window)
    Mod1 = list_to_atom("debounce_mod1_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Mod2 = list_to_atom("debounce_mod2_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Mod3 = list_to_atom("debounce_mod3_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = beamtalk_workspace_meta:register_module(Mod1),
    timer:sleep(100),
    ok = beamtalk_workspace_meta:register_module(Mod2),
    timer:sleep(100),
    ok = beamtalk_workspace_meta:register_module(Mod3),
    
    %% All 3 should be in state immediately
    timer:sleep(50),
    {ok, Modules} = beamtalk_workspace_meta:loaded_modules(),
    ?assert(lists:member(Mod1, Modules)),
    ?assert(lists:member(Mod2, Modules)),
    ?assert(lists:member(Mod3, Modules)),
    
    %% Wait for debounce to fire (2s from LAST change, not first)
    timer:sleep(2500),
    
    %% Verify all 3 modules were persisted to disk
    ?assert(filelib:is_file(MetaFile)),
    {ok, Binary} = file:read_file(MetaFile),
    Map = jsx:decode(Binary, [return_maps]),
    PersistedModules = maps:get(<<"loaded_modules">>, Map, []),
    ?assert(lists:member(atom_to_binary(Mod1, utf8), PersistedModules)),
    ?assert(lists:member(atom_to_binary(Mod2, utf8), PersistedModules)),
    ?assert(lists:member(atom_to_binary(Mod3, utf8), PersistedModules)),
    
    %% Cleanup
    gen_server:stop(Pid),
    _ = file:delete(MetaFile),
    _ = file:del_dir(MetaDir).
