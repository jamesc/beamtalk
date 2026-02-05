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
    
    %% Start the server
    {ok, Pid} = beamtalk_workspace_meta:start_link(test_metadata()),
    
    %% Get metadata
    {ok, Meta} = beamtalk_workspace_meta:get_metadata(),
    
    %% Should have all required fields
    ?assert(maps:is_key(workspace_id, Meta)),
    ?assert(maps:is_key(project_path, Meta)),
    ?assert(maps:is_key(created_at, Meta)),
    ?assert(maps:is_key(last_activity, Meta)),
    
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
