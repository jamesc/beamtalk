%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for workspace_meta activity tracking API
%%%
%%% Tests that workspace_meta:update_activity/0 correctly updates the
%%% last_activity timestamp. These are unit tests for the activity tracking
%%% mechanism itself. Integration tests that exercise the real call sites
%%% (repl_server session creation, actor spawn, code reload) are in
%%% their respective test modules.

-module(beamtalk_activity_tracking_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Stop any pre-existing beamtalk_workspace_meta process to ensure
%% a clean slate for each test.
stop_if_running() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            ok
    end.

%%====================================================================
%% Tests
%%====================================================================

%%% Session connection activity tracking

session_connect_updates_activity_test() ->
    stop_if_running(),
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
        %% (This is what beamtalk_repl_server does when a session is created)
        ok = beamtalk_workspace_meta:update_activity(),

        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after update")
    after
        gen_server:stop(MetaPid)
    end.

%%% Actor spawn activity tracking

actor_spawn_updates_activity_test() ->
    stop_if_running(),
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
        %% (This is what beamtalk_actor:register_spawned does after registration)
        ok = beamtalk_workspace_meta:update_activity(),

        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after actor spawn")
    after
        gen_server:stop(MetaPid)
    end.

%%% Code reload activity tracking

code_reload_updates_activity_test() ->
    stop_if_running(),
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
        %% (This is what beamtalk_repl_eval:do_load does after module load)
        ok = beamtalk_workspace_meta:update_activity(),

        %% Verify activity time was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "Activity time should increase after code reload")
    after
        gen_server:stop(MetaPid)
    end.

%%% Multiple activity updates

multiple_activity_updates_test() ->
    stop_if_running(),
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
    stop_if_running(),
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
