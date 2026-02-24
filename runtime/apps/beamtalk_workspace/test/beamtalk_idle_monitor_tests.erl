%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_idle_monitor module
%%%
%%% Tests idle monitoring and auto-cleanup logic.

-module(beamtalk_idle_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Test helper

test_config() ->
    % Disabled for testing
    #{enabled => false, max_idle_seconds => 14400}.

test_metadata() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/tmp/test">>,
        % Use seconds, not milliseconds
        created_at => erlang:system_time(second),
        last_activity => erlang:system_time(second)
    }.

%%% Initialization tests

init_test() ->
    {ok, _State} = beamtalk_idle_monitor:init(test_config()),
    ok.

%%% Integration tests (require workspace_meta to be running)

idle_monitor_starts_successfully_test() ->
    %% Start workspace_meta (required dependency)
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Start idle monitor
    Config = #{enabled => true, max_idle_seconds => 14400},
    {ok, MonitorPid} = beamtalk_idle_monitor:start_link(Config),

    %% Verify both are running
    ?assert(is_process_alive(MetaPid)),
    ?assert(is_process_alive(MonitorPid)),

    %% Cleanup
    gen_server:stop(MonitorPid),
    gen_server:stop(MetaPid).

idle_monitor_checks_recent_activity_test() ->
    %% Start workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(test_metadata()),

    %% Update activity to ensure it's recent
    ok = beamtalk_workspace_meta:update_activity(),

    %% Start idle monitor with short timeout
    Config = #{enabled => true, max_idle_seconds => 14400},
    {ok, MonitorPid} = beamtalk_idle_monitor:start_link(Config),

    %% Wait a moment
    timer:sleep(50),

    %% Monitor should still be alive (not idle)
    ?assert(is_process_alive(MonitorPid)),

    %% Cleanup
    gen_server:stop(MonitorPid),
    gen_server:stop(MetaPid).

%%% Unit tests for idle calculation logic

calculate_idle_time_test() ->
    %% Test idle time calculation logic
    CurrentTime = erlang:system_time(millisecond),

    %% 1 minute ago
    OneMinuteAgo = CurrentTime - (1 * 60 * 1000),
    IdleTime1 = CurrentTime - OneMinuteAgo,
    ?assert(IdleTime1 >= 60000),
    ?assert(IdleTime1 < 120000),

    %% 5 hours ago
    FiveHoursAgo = CurrentTime - (5 * 60 * 60 * 1000),
    IdleTime5 = CurrentTime - FiveHoursAgo,
    ?assert(IdleTime5 >= (5 * 60 * 60 * 1000)),

    %% Verify threshold comparison
    FourHourThreshold = 4 * 60 * 60 * 1000,
    ?assert(IdleTime1 < FourHourThreshold),
    ?assert(IdleTime5 >= FourHourThreshold).

%%% Direct callback tests (calling exported callbacks with crafted state)

init_disabled_no_timer_test() ->
    %% When enabled=false, init should not start a timer
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false, max_idle_seconds => 100}),
    %% State is opaque but we can pass it through callbacks to verify behavior
    ?assertMatch({noreply, _}, beamtalk_idle_monitor:handle_info(check_idle, State)).

init_enabled_starts_timer_test() ->
    %% When enabled=true, init should start a timer (timer_ref set)
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => true, max_idle_seconds => 100}),
    %% Verify state contains a valid timer reference via sys:get_state format
    %% State is an opaque record, but we can verify via terminate + callback behavior
    %% The real proof is that handle_info(check_idle, State) reschedules (not no-ops)
    {noreply, _} = beamtalk_idle_monitor:handle_info(check_idle, State),
    %% Cleanup timer
    ?assertEqual(ok, beamtalk_idle_monitor:terminate(normal, State)).

init_defaults_test() ->
    %% Default config: enabled=true, max_idle_seconds=4 hours
    {ok, State} = beamtalk_idle_monitor:init(#{}),
    %% Cleanup timer
    beamtalk_idle_monitor:terminate(normal, State).

handle_call_unknown_request_test() ->
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    From = {self(), make_ref()},
    ?assertMatch(
        {reply, {error, unknown_request}, _},
        beamtalk_idle_monitor:handle_call(any_request, From, State)
    ).

handle_cast_unknown_message_test() ->
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    ?assertMatch({noreply, _}, beamtalk_idle_monitor:handle_cast(any_message, State)).

handle_info_unknown_message_test() ->
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    ?assertMatch({noreply, _}, beamtalk_idle_monitor:handle_info(unexpected_msg, State)).

handle_info_check_idle_disabled_test() ->
    %% When enabled=false, check_idle should be a no-op
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    {noreply, NewState} = beamtalk_idle_monitor:handle_info(check_idle, State),
    %% Should still be disabled — sending check_idle again also no-ops
    ?assertMatch({noreply, _}, beamtalk_idle_monitor:handle_info(check_idle, NewState)).

terminate_with_undefined_timer_test() ->
    %% When timer_ref is undefined (disabled monitor), terminate should be ok
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    ?assertEqual(ok, beamtalk_idle_monitor:terminate(normal, State)).

terminate_with_active_timer_test() ->
    %% When timer_ref is set (enabled monitor), terminate should cancel it
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => true, max_idle_seconds => 100}),
    ?assertEqual(ok, beamtalk_idle_monitor:terminate(shutdown, State)).

code_change_preserves_state_test() ->
    {ok, State} = beamtalk_idle_monitor:init(#{enabled => false}),
    ?assertEqual({ok, State}, beamtalk_idle_monitor:code_change(old_vsn, State, extra)),
    ok.

%%% Process-level tests (with running gen_server)

check_idle_not_idle_reschedules_test() ->
    %% Start workspace_meta with recent activity
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(test_metadata()),
    ok = beamtalk_workspace_meta:update_activity(),

    %% Start idle monitor with enabled=true and long timeout
    Config = #{enabled => true, max_idle_seconds => 999999},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        %% Send check_idle — activity is recent, should reschedule (not stop)
        MonPid ! check_idle,
        timer:sleep(50),

        %% Process should still be alive (not idle, rescheduled)
        ?assert(is_process_alive(MonPid))
    after
        gen_server:stop(MonPid),
        gen_server:stop(MetaPid)
    end.

disabled_monitor_ignores_check_idle_test() ->
    %% Start disabled monitor
    Config = #{enabled => false, max_idle_seconds => 1},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        %% Send check_idle — should be ignored since disabled
        MonPid ! check_idle,
        timer:sleep(50),

        ?assert(is_process_alive(MonPid))
    after
        gen_server:stop(MonPid)
    end.

monitor_handles_unknown_cast_test() ->
    Config = #{enabled => false, max_idle_seconds => 100},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        gen_server:cast(MonPid, unknown_message),
        timer:sleep(50),

        ?assert(is_process_alive(MonPid))
    after
        gen_server:stop(MonPid)
    end.

monitor_handles_unknown_call_test() ->
    Config = #{enabled => false, max_idle_seconds => 100},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        Result = gen_server:call(MonPid, unknown_request),
        ?assertEqual({error, unknown_request}, Result)
    after
        gen_server:stop(MonPid)
    end.

mark_activity_delegates_test() ->
    %% mark_activity should delegate to workspace_meta
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(test_metadata()),

    ?assertEqual(ok, beamtalk_idle_monitor:mark_activity()),

    gen_server:stop(MetaPid).

%%% has_active_sessions error path tests

has_active_sessions_no_supervisor_test() ->
    %% When beamtalk_session_sup is not registered, has_active_sessions()
    %% returns false. With recent activity and a long timeout, the monitor
    %% should stay alive (reschedule, not terminate).
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(test_metadata()),
    ok = beamtalk_workspace_meta:update_activity(),

    Config = #{enabled => true, max_idle_seconds => 999999},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        MonPid ! check_idle,
        timer:sleep(50),
        ?assert(is_process_alive(MonPid))
    after
        gen_server:stop(MonPid),
        gen_server:stop(MetaPid)
    end.

get_last_activity_error_path_test() ->
    %% When workspace_meta is NOT running, get_last_activity returns error.
    %% should_terminate should return false (safe default).
    %% Verify monitor stays alive when workspace_meta is unavailable.
    Config = #{enabled => true, max_idle_seconds => 1},
    {ok, MonPid} = beamtalk_idle_monitor:start_link(Config),

    try
        %% No workspace_meta running — get_last_activity will error
        %% Send check_idle — should_terminate returns false due to error path
        MonPid ! check_idle,
        timer:sleep(50),

        %% Process stays alive (error path → don't terminate)
        ?assert(is_process_alive(MonPid))
    after
        gen_server:stop(MonPid)
    end.
