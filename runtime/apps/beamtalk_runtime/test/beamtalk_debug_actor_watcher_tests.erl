%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_debug_actor_watcher_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_debug_actor_watcher` error paths that lacked coverage
per the nightly erlang-coverage-reports artifact (69% / 7 of 92 lines uncovered,
run 44908043238, 2026-11-21).

Covered here:
- Line 43  — `watch/1` exception arm: fires when the gen_server is not running.
- Line 55  — `init/1` fold: pre-existing debug-target rows are re-monitored on restart.
- Line 67  — `handle_call({watch,Pid})`: pid already present in the Monitors map.
- Line 68  — `handle_call({watch,Pid})`: pid absent from the ETS table.
- Line 72  — `handle_call` catch-all for unknown requests.
- Line 85  — `handle_cast` catch-all for unknown casts.
- Line 91  — `handle_info` catch-all for unknown info messages.

No Beamtalk runtime is started. `beamtalk_logging_config:ensure_table/0` creates
the `beamtalk_debug_targets` ETS table safely without the runtime app (the heir
process is absent, so the table is owned by the calling test process).
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_debug_targets).

%%====================================================================
%% Test: line 43 — watch/1 exception arm when watcher is not running
%%====================================================================

%% When `beamtalk_debug_actor_watcher` is not registered, `gen_server:call/2`
%% exits with `{noproc, {gen_server, call, _}}`.  `watch/1` catches it (line 43)
%% and returns `ok`.
watch_when_not_running_returns_ok_test() ->
    beamtalk_test_proc:stop_registered(beamtalk_debug_actor_watcher),
    ?assertEqual(ok, beamtalk_debug_actor_watcher:watch(self())).

%%====================================================================
%% Test: line 55 — init/1 fold over pre-existing debug-target rows
%%====================================================================

%% `init/1` is exported, so it can be called directly to exercise the fold
%% (line 55) without starting a new registered gen_server process.  This avoids
%% `{already_started, _}` errors when the full test suite has already started
%% `beamtalk_debug_actor_watcher` via the runtime supervisor.
init_with_preexisting_target_monitors_it_test() ->
    beamtalk_logging_config:ensure_table(),
    IdlePid = beamtalk_test_proc:spawn_idle(),
    ets:insert(?TABLE, {IdlePid, actor_instance, 'SomeClass'}),
    try
        {ok, Monitors} = beamtalk_debug_actor_watcher:init([]),
        ?assertMatch(#{IdlePid := _}, Monitors),
        #{IdlePid := MRef} = Monitors,
        demonitor(MRef, [flush])
    after
        beamtalk_test_proc:kill_and_wait(IdlePid),
        ets:delete(?TABLE, IdlePid)
    end.

%%====================================================================
%% Tests requiring a running gen_server: lines 67, 68, 72, 85, 91
%%====================================================================

debug_actor_watcher_running_test_() ->
    {setup,
        fun() ->
            beamtalk_logging_config:ensure_table(),
            case whereis(beamtalk_debug_actor_watcher) of
                undefined ->
                    {ok, Pid} = beamtalk_debug_actor_watcher:start_link(),
                    {started, Pid};
                Pid ->
                    {already_running, Pid}
            end
        end,
        fun({started, Pid}) -> beamtalk_test_proc:stop(Pid);
           ({already_running, _}) -> ok
        end,
        fun(_) ->
            [
                fun watch_pid_not_in_table_returns_ok/0,
                fun watch_pid_already_monitored_returns_ok/0,
                fun unknown_call_returns_error/0,
                fun unknown_cast_does_not_crash/0,
                fun unknown_info_does_not_crash/0
            ]
        end}.

%%====================================================================
%% Individual tests (run inside the setup above)
%%====================================================================

%% Line 68: `watch/1` for a pid that is NOT in the debug-targets table returns ok.
watch_pid_not_in_table_returns_ok() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    try
        ?assertEqual(ok, beamtalk_debug_actor_watcher:watch(Pid))
    after
        beamtalk_test_proc:kill_and_wait(Pid)
    end.

%% Line 67: `watch/1` for a pid already in Monitors returns ok.
%%
%% First watch (Pid in table, not yet monitored) exercises line 69.
%% Second watch (Pid already in Monitors map) exercises line 67.
watch_pid_already_monitored_returns_ok() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    ets:insert(?TABLE, {Pid, actor_instance, 'SomeClass'}),
    try
        ?assertEqual(ok, beamtalk_debug_actor_watcher:watch(Pid)),
        ?assertEqual(ok, beamtalk_debug_actor_watcher:watch(Pid))
    after
        beamtalk_test_proc:kill_and_wait(Pid),
        ets:delete(?TABLE, Pid)
    end.

%% Line 72: an unknown `gen_server:call` returns `{error, unknown_request}`.
unknown_call_returns_error() ->
    Result = gen_server:call(beamtalk_debug_actor_watcher, an_unknown_request),
    ?assertEqual({error, unknown_request}, Result).

%% Line 85: an unknown `gen_server:cast` is handled without crashing.
unknown_cast_does_not_crash() ->
    gen_server:cast(beamtalk_debug_actor_watcher, an_unknown_cast),
    %% Synchronise: a subsequent call waits until the cast is processed (FIFO mailbox).
    _ = gen_server:call(beamtalk_debug_actor_watcher, sync),
    ?assertNotEqual(undefined, whereis(beamtalk_debug_actor_watcher)).

%% Line 91: an unknown info message is handled without crashing.
unknown_info_does_not_crash() ->
    beamtalk_debug_actor_watcher ! an_unknown_info_message,
    %% Synchronise: a subsequent call waits until the info is processed (FIFO mailbox).
    _ = gen_server:call(beamtalk_debug_actor_watcher, sync),
    ?assertNotEqual(undefined, whereis(beamtalk_debug_actor_watcher)).
