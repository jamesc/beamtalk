%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_io_capture module

-module(beamtalk_io_capture_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests: start/stop
%%====================================================================

start_stop_returns_captured_output_test() ->
    Handle = beamtalk_io_capture:start(),
    io:put_chars("hello"),
    Output = beamtalk_io_capture:stop(Handle),
    ?assertEqual(<<"hello">>, Output).

start_stop_empty_output_test() ->
    Handle = beamtalk_io_capture:start(),
    Output = beamtalk_io_capture:stop(Handle),
    ?assertEqual(<<>>, Output).

%%====================================================================
%% Tests: reset_captured_group_leaders — dead process guard (BT-1172)
%%====================================================================

reset_captured_group_leaders_dead_process_test() ->
    %% Spawn a process, make the capture PID its group leader, then kill it.
    %% reset_captured_group_leaders must not crash with badarg.
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> receive stop -> ok end end),
    Pid = spawn(fun() -> receive stop -> ok end end),
    group_leader(CapturePid, Pid),
    %% Kill the target process to trigger the race condition
    exit(Pid, kill),
    %% Give the scheduler a chance to process the exit
    timer:sleep(10),
    %% This must not raise badarg
    ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
    exit(CapturePid, kill).

reset_captured_group_leaders_live_process_test() ->
    %% Live process whose group_leader is CapturePid gets reset to OldGL.
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> receive stop -> ok end end),
    Pid = spawn(fun() -> receive stop -> ok end end),
    group_leader(CapturePid, Pid),
    ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
    {group_leader, GL} = erlang:process_info(Pid, group_leader),
    ?assertEqual(OldGL, GL),
    exit(Pid, kill),
    exit(CapturePid, kill).

reset_captured_group_leaders_different_gl_left_alone_test() ->
    %% Process with a different group_leader is not touched.
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> receive stop -> ok end end),
    OtherPid = spawn(fun() -> receive stop -> ok end end),
    %% OtherPid's GL is OldGL (default), not CapturePid
    ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
    {group_leader, GL2} = erlang:process_info(OtherPid, group_leader),
    ?assertEqual(OldGL, GL2),
    exit(OtherPid, kill),
    exit(CapturePid, kill).
