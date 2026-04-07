%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_io_capture_tests).

-moduledoc """
EUnit tests for beamtalk_io_capture, focused on the BT-1172
dead-process guard in reset_captured_group_leaders/2.

General start/stop and io_capture_with_output coverage lives in
beamtalk_repl_eval_tests.erl.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

spawn_monitored(Fun) ->
    Pid = spawn(Fun),
    Ref = erlang:monitor(process, Pid),
    {Pid, Ref}.

await_down(Ref) ->
    receive
        {'DOWN', Ref, process, _, _} -> ok
    after 5000 ->
        error(timeout_waiting_for_down)
    end.

%%====================================================================
%% Tests: reset_captured_group_leaders — dead process guard (BT-1172)
%%====================================================================

%% BT-1172: The TOCTOU race — a process whose group_leader is CapturePid
%% dies between is_process_alive/1 returning true and group_leader/2 being
%% called. We approximate this by issuing exit(Pid, kill) and immediately
%% calling reset without waiting. The kill signal may arrive during the scan,
%% exercising the try/catch. The key invariant: no exception is raised.
reset_captured_group_leaders_toctou_race_test() ->
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {Pid, _PRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    group_leader(CapturePid, Pid),
    %% Issue kill without waiting — Pid may still appear in erlang:processes()
    exit(Pid, kill),
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL))
    after
        exit(CapturePid, kill),
        await_down(CRef)
    end.

reset_captured_group_leaders_live_process_test() ->
    %% Live process whose group_leader is CapturePid gets reset to OldGL.
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {Pid, PRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    group_leader(CapturePid, Pid),
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
        {group_leader, GL} = erlang:process_info(Pid, group_leader),
        ?assertEqual(OldGL, GL)
    after
        exit(Pid, kill),
        exit(CapturePid, kill),
        await_down(PRef),
        await_down(CRef)
    end.

reset_captured_group_leaders_different_gl_left_alone_test() ->
    %% Process with a different group_leader is not touched.
    OldGL = group_leader(),
    {CapturePid, CRef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    {OtherPid, ORef} = spawn_monitored(fun() ->
        receive
            stop -> ok
        end
    end),
    %% OtherPid's GL is OldGL (default), not CapturePid
    try
        ?assertEqual(ok, beamtalk_io_capture:reset_captured_group_leaders(CapturePid, OldGL)),
        {group_leader, GL2} = erlang:process_info(OtherPid, group_leader),
        ?assertEqual(OldGL, GL2)
    after
        exit(OtherPid, kill),
        exit(CapturePid, kill),
        await_down(ORef),
        await_down(CRef)
    end.
