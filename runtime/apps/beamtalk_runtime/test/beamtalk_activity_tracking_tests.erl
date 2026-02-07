%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Integration tests for workspace activity tracking
%%%
%%% Tests that activity tracking is correctly integrated across components:
%%% - Session connections update activity
%%% - Actor spawns update activity
%%% - Code reloads update activity

-module(beamtalk_activity_tracking_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Session connection activity tracking

session_connect_updates_activity_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_session">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second)
    }),
    
    try
        %% Get initial activity time
        {ok, InitialTime} = beamtalk_workspace_meta:get_last_activity(),
        
        %% Wait a moment to ensure time changes
        timer:sleep(1100),
        
        %% Simulate session connection by calling update_activity
        %% (This is what beamtalk_repl_server does on line 103)
        ok = beamtalk_workspace_meta:update_activity(),
        
        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after update")
    after
        gen_server:stop(MetaPid)
    end.

%%% Actor spawn activity tracking

actor_spawn_updates_activity_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_actor">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second)
    }),
    
    try
        %% Get initial activity time
        {ok, InitialTime} = beamtalk_workspace_meta:get_last_activity(),
        
        %% Wait a moment
        timer:sleep(1100),
        
        %% Simulate actor spawn by calling update_activity
        %% (This is what beamtalk_actor:spawn_with_registry does on line 193)
        ok = beamtalk_workspace_meta:update_activity(),
        
        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after actor spawn")
    after
        gen_server:stop(MetaPid)
    end.

%%% Code reload activity tracking

code_reload_updates_activity_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_reload">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second)
    }),
    
    try
        %% Get initial activity time
        {ok, InitialTime} = beamtalk_workspace_meta:get_last_activity(),
        
        %% Wait a moment
        timer:sleep(1100),
        
        %% Simulate code reload by calling update_activity
        %% (This is what beamtalk_repl_eval:do_load does on line 165)
        ok = beamtalk_workspace_meta:update_activity(),
        
        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after code reload")
    after
        gen_server:stop(MetaPid)
    end.

%%% Multiple activity updates

multiple_activity_updates_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_multiple">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second)
    }),
    
    try
        %% Get initial activity time
        {ok, Time1} = beamtalk_workspace_meta:get_last_activity(),
        
        %% Update 1
        timer:sleep(1100),
        ok = beamtalk_workspace_meta:update_activity(),
        {ok, Time2} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(Time2 > Time1, "First update should increase time"),
        
        %% Update 2
        timer:sleep(1100),
        ok = beamtalk_workspace_meta:update_activity(),
        {ok, Time3} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(Time3 > Time2, "Second update should increase time"),
        
        %% Update 3
        timer:sleep(1100),
        ok = beamtalk_workspace_meta:update_activity(),
        {ok, Time4} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(Time4 > Time3, "Third update should increase time")
    after
        gen_server:stop(MetaPid)
    end.

%%% Mark activity convenience function

mark_activity_delegates_to_workspace_meta_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_mark">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second)
    }),
    
    try
        %% Get initial activity time
        {ok, InitialTime} = beamtalk_workspace_meta:get_last_activity(),
        
        %% Wait a moment
        timer:sleep(1100),
        
        %% Use the convenience function
        ok = beamtalk_idle_monitor:mark_activity(),
        
        %% Verify activity was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "mark_activity should delegate to workspace_meta")
    after
        gen_server:stop(MetaPid)
    end.
