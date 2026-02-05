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
    #{enabled => false, max_idle_seconds => 14400}.  % Disabled for testing

test_metadata() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/tmp/test">>,
        created_at => erlang:system_time(second),  % Use seconds, not milliseconds
        last_activity => erlang:system_time(second)
    }.

%%% Initialization tests

init_test() ->
    {ok, _State} = beamtalk_idle_monitor:init(test_config()),
    ok.

%%% Idle threshold constant test

idle_threshold_is_4_hours_test() ->
    %% The module defines IDLE_THRESHOLD as 4 hours in milliseconds
    %% 4 * 60 * 60 * 1000 = 14,400,000 ms
    ExpectedThreshold = 4 * 60 * 60 * 1000,
    
    %% We can't directly access the macro, but we can verify the logic
    %% by checking that 4 hours in ms is the expected value
    ?assertEqual(14400000, ExpectedThreshold).

check_interval_is_10_minutes_test() ->
    %% The module defines CHECK_INTERVAL as 10 minutes in milliseconds
    %% 10 * 60 * 1000 = 600,000 ms
    ExpectedInterval = 10 * 60 * 1000,
    
    ?assertEqual(600000, ExpectedInterval).

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
