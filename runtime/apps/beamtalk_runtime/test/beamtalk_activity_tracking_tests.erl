%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_activity_tracking_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
Unit tests for workspace_meta activity tracking API

Tests that workspace_meta:update_activity/0 correctly updates the
last_activity timestamp. These are unit tests for the activity tracking
mechanism itself. Integration tests that exercise the real call sites
(repl_server session creation, actor spawn, code reload) are in
their respective test modules.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

test_project_path() ->
    iolist_to_binary([element(2, beamtalk_file:'tempDirectory'()), "/bt-activity-test"]).

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
        project_path => test_project_path(),
        created_at => erlang:system_time(second)
    }),

    try
        %% Back-date the stamp so the next update is strictly newer without
        %% waiting for the second-resolution clock to roll over
        InitialTime = backdate_activity(MetaPid),

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
        project_path => test_project_path(),
        created_at => erlang:system_time(second)
    }),

    try
        %% Back-date the stamp so the next update is strictly newer without
        %% waiting for the second-resolution clock to roll over
        InitialTime = backdate_activity(MetaPid),

        %% Simulate actor spawn by calling update_activity directly (this test
        %% pins the activity clock, not a spawn path)
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
        project_path => test_project_path(),
        created_at => erlang:system_time(second)
    }),

    try
        %% Back-date the stamp so the next update is strictly newer without
        %% waiting for the second-resolution clock to roll over
        InitialTime = backdate_activity(MetaPid),

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
        project_path => test_project_path(),
        created_at => erlang:system_time(second)
    }),

    try
        %% Each round back-dates the stamp, so the update must move it forward
        lists:foreach(
            fun(Round) ->
                Before = backdate_activity(MetaPid),
                ok = beamtalk_workspace_meta:update_activity(),
                {ok, After} = beamtalk_workspace_meta:get_last_activity(),
                ?assert(After > Before, {update_did_not_advance_activity, Round})
            end,
            [1, 2, 3]
        )
    after
        gen_server:stop(MetaPid)
    end.

%%% Mark activity convenience function

mark_activity_delegates_to_workspace_meta_test() ->
    stop_if_running(),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"test_mark">>,
        project_path => test_project_path(),
        created_at => erlang:system_time(second)
    }),

    try
        %% Back-date the stamp so the next update is strictly newer without
        %% waiting for the second-resolution clock to roll over
        InitialTime = backdate_activity(MetaPid),

        %% Use the convenience function
        ok = beamtalk_idle_monitor:mark_activity(),

        %% Verify activity was updated
        {ok, NewTime} = beamtalk_workspace_meta:get_last_activity(),
        ?assert(NewTime > InitialTime, "mark_activity should delegate to workspace_meta")
    after
        gen_server:stop(MetaPid)
    end.

%% Move workspace_meta's last-activity stamp (second resolution) 10 s into the
%% past and return it, so the next update gets a strictly larger value with no
%% wait for the wall clock to tick. Test-only state edit via sys:replace_state/2.
backdate_activity(MetaPid) ->
    {ok, Now} = beamtalk_workspace_meta:get_last_activity(),
    Aged = Now - 10,
    %% #state.last_activity is element 6 (record tag, workspace_id,
    %% project_path, package_name, created_at, last_activity); the guard fails
    %% loudly if the record layout ever shifts.
    _ = sys:replace_state(MetaPid, fun(State) ->
        Now = element(6, State),
        setelement(6, State, Aged)
    end),
    {ok, Aged} = beamtalk_workspace_meta:get_last_activity(),
    Aged.
