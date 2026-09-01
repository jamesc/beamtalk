%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_monitor_tests).

-moduledoc """
Direct EUnit tests for `beamtalk_class_monitor` (BT-3236) against plain
dummy processes — no class gen_server involved.

`beamtalk_class_sup_tests` already exercises this module's happy-path
restart behaviour end-to-end through real class processes (eager restart,
deliberate stop via `gen_server:stop/1`, restart-budget exhaustion,
`unwatch/1`, and monitor-restart re-adoption from `pg`); these tests fill in
the branches that suite's real-class setup never reaches:

- `watch/2`/`unwatch/1` called while no monitor is registered at all (the
  `whereis(?MODULE) =:= undefined` no-op paths — a no-op class start/stop
  in a bare EUnit context with no supervision tree running).
- The three generic gen_server catch-all clauses (`handle_call`,
  `handle_cast`, `handle_info`) for messages this module doesn't know
  about.
- A `'DOWN'` for a monitor ref this module never registered — already
  removed, or never watched.
- `do_watch/3`'s dedup: watching the same live pid twice must install only
  one `erlang:monitor/2` ref, not two (which would double-fire eager
  recovery on that pid's death).
- `is_deliberate_stop/1`'s `shutdown` and `{shutdown, _}` clauses (only
  `normal`, `noproc`, and abnormal reasons are exercised elsewhere).
- What happens when the eager restart itself fails: `restart_class/1`
  returning `{error, _}` or raising, both of which must be logged and
  swallowed rather than taking the monitor itself down — and the restart
  budget actually stopping further attempts once exhausted.

Fault injection uses `meck` on `beamtalk_class_registry:restart_class/1`
(scoped to one function per test, unloaded immediately after, following the
same narrow-scope convention as
`beamtalk_repl_loader_rewrite_sites_tests:setup_with_install_fault/0`,
BT-3280) — a plain spawned dummy process can be watched and killed, but it
cannot stand in for a real class gen_server that `restart_class/1` itself
would rebuild from ETS metadata.

BT-2962 spike (OTP 29 native records): on this branch, every `meck:new/2`
call below crashes. `meck_code_gen:to_forms/2` rebuilds a mock module's
attributes from `Mod:module_info(attributes)`, which always list-wraps a
custom attribute's value (`[{beamtalk_error,[beamtalk_error]}]`) even for
a single occurrence — but `erl_lint:import_native_record/3` pattern-matches
the raw `{Mod, Rs}` tuple and has no clause for the list-wrapped form, so
recompiling the synthesized mock module crashes the compiler with a
`function_clause` in `erl_lint.erl`. `-import_record` is not round-trip-safe
through `module_info(attributes)`, which breaks `meck` for any module that
imports a native record — see the BT-2962 Linear issue for the full
writeup and reproduction.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown — start an isolated monitor under the module's fixed
%% registered name, displacing (and later restoring) whatever a prior
%% suite in this shared EUnit node left registered. Mirrors
%% beamtalk_class_sup_tests:setup/1,teardown/1.
%%====================================================================

setup() ->
    setup(#{}).

setup(Opts) ->
    Restore = displace_registered_monitor(),
    {ok, Pid} = beamtalk_class_monitor:start_link(Opts),
    #{monitor => Pid, restore => Restore}.

teardown(#{monitor := Pid, restore := Restore}) ->
    stop_if_alive(Pid),
    %% A test may have replaced the monitor; stop whatever currently holds
    %% the registered name too.
    stop_if_alive(whereis(beamtalk_class_monitor)),
    case Restore of
        true -> catch supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_monitor);
        false -> ok
    end,
    ok.

displace_registered_monitor() ->
    case whereis(beamtalk_class_monitor) of
        undefined ->
            false;
        Pid ->
            case whereis(beamtalk_runtime_sup) of
                undefined ->
                    stop_if_alive(Pid),
                    false;
                _ ->
                    case supervisor:terminate_child(beamtalk_runtime_sup, beamtalk_class_monitor) of
                        ok ->
                            true;
                        {error, _} ->
                            stop_if_alive(Pid),
                            false
                    end
            end
    end.

stop_if_alive(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            MRef = erlang:monitor(process, Pid),
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> exit(Pid, kill)
            end),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 2000 -> ok
            end;
        false ->
            ok
    end;
stop_if_alive(_) ->
    ok.

%% A plain, non-trapping process — `watch/2` only needs a pid to monitor,
%% not a gen_server or a real class process.
spawn_dummy() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

kill_and_wait(Pid, Reason) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, Reason),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ?assert(false)
    end.

wait_until(Fun) ->
    wait_until(Fun, 100).

wait_until(_Fun, 0) ->
    timeout;
wait_until(Fun, Retries) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_until(Fun, Retries - 1)
    end.

%%====================================================================
%% watch/2, unwatch/1 — no monitor registered at all
%%====================================================================

watch_and_unwatch_when_monitor_not_running_test_() ->
    {setup, fun displace_registered_monitor/0, fun restore_displaced/1, fun(_) ->
        [
            {"watch/2 is a no-op ok when no monitor is registered", fun() ->
                ?assertEqual(undefined, whereis(beamtalk_class_monitor)),
                ?assertEqual(ok, beamtalk_class_monitor:watch('NoMonitorClass', self()))
            end},
            {"unwatch/1 is a no-op ok when no monitor is registered", fun() ->
                ?assertEqual(undefined, whereis(beamtalk_class_monitor)),
                ?assertEqual(ok, beamtalk_class_monitor:unwatch('NoMonitorClass'))
            end}
        ]
    end}.

restore_displaced(Restore) ->
    case Restore of
        true -> catch supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_monitor);
        false -> ok
    end,
    ok.

%%====================================================================
%% Generic gen_server catch-all clauses + unknown 'DOWN' ref
%%====================================================================

gen_server_catchall_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(Ctx) ->
        #{monitor := Pid} = Ctx,
        [
            {"an unrecognised gen_server:call returns {error, unknown_call}", fun() ->
                ?assertEqual({error, unknown_call}, gen_server:call(Pid, {something, unexpected}))
            end},
            {"an unrecognised cast is ignored and the monitor stays responsive", fun() ->
                gen_server:cast(Pid, {something, unexpected}),
                ?assertEqual({error, unknown_call}, gen_server:call(Pid, still_unknown))
            end},
            {"an unrecognised info message is ignored and the monitor stays responsive", fun() ->
                Pid ! {totally, unexpected, message},
                ?assertEqual({error, unknown_call}, gen_server:call(Pid, still_unknown))
            end},
            {"a 'DOWN' for a ref the monitor never watched is ignored", fun() ->
                Pid ! {'DOWN', erlang:make_ref(), process, self(), normal},
                ?assertEqual({error, unknown_call}, gen_server:call(Pid, still_unknown))
            end}
        ]
    end}.

%%====================================================================
%% do_watch/3 dedup — watching the same live pid twice
%%====================================================================

watch_dedup_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Ctx) ->
        {"watching the same live pid twice installs only one monitor ref", fun() ->
            meck:new(beamtalk_class_registry, [passthrough]),
            meck:expect(beamtalk_class_registry, restart_class, fun(_Name) ->
                {ok, spawn_dummy()}
            end),
            try
                Dummy = spawn_dummy(),
                ok = beamtalk_class_monitor:watch('DedupClass', Dummy),
                %% Second watch/2 on the same still-live pid must be a no-op
                %% (do_watch/3's `maps:is_key(Pid, Watched) -> true` branch) —
                %% otherwise this pid's death would fire two independent
                %% 'DOWN' messages and restart_class/1 would run twice.
                ok = beamtalk_class_monitor:watch('DedupClass', Dummy),
                kill_and_wait(Dummy, kill),
                ?assertEqual(
                    ok,
                    wait_until(fun() ->
                        meck:num_calls(beamtalk_class_registry, restart_class, ['DedupClass']) > 0
                    end)
                ),
                %% Give a wrongly-duplicated second 'DOWN' time to arrive.
                timer:sleep(200),
                ?assertEqual(
                    1, meck:num_calls(beamtalk_class_registry, restart_class, ['DedupClass'])
                )
            after
                meck:unload(beamtalk_class_registry)
            end
        end}
    end}.

%%====================================================================
%% is_deliberate_stop/1 — shutdown / {shutdown, _} are never restarted
%%====================================================================

deliberate_stop_shutdown_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Ctx) ->
        [
            {"a plain 'shutdown' exit is not treated as a crash", fun() ->
                assert_no_restart_on_exit('ShutdownClass1', shutdown)
            end},
            {"a {shutdown, Reason} exit is not treated as a crash", fun() ->
                assert_no_restart_on_exit('ShutdownClass2', {shutdown, going_away})
            end}
        ]
    end}.

assert_no_restart_on_exit(ClassName, ExitReason) ->
    meck:new(beamtalk_class_registry, [passthrough]),
    meck:expect(beamtalk_class_registry, restart_class, fun(_Name) ->
        {ok, spawn_dummy()}
    end),
    try
        Dummy = spawn_dummy(),
        ok = beamtalk_class_monitor:watch(ClassName, Dummy),
        kill_and_wait(Dummy, ExitReason),
        %% Give a wrongly-triggered restart time to happen, then assert none did.
        timer:sleep(200),
        ?assertEqual(0, meck:num_calls(beamtalk_class_registry, restart_class, [ClassName]))
    after
        meck:unload(beamtalk_class_registry)
    end.

%%====================================================================
%% restart_class/1 itself failing — the monitor must log and carry on
%%====================================================================

restart_failure_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(Ctx) ->
        #{monitor := MonPid} = Ctx,
        [
            {"restart_class returning {error, _} is logged; the monitor survives", fun() ->
                meck:new(beamtalk_class_registry, [passthrough]),
                meck:expect(beamtalk_class_registry, restart_class, fun(_Name) ->
                    {error, injected_failure}
                end),
                try
                    Dummy = spawn_dummy(),
                    ok = beamtalk_class_monitor:watch('RestartFailClass', Dummy),
                    kill_and_wait(Dummy, kill),
                    ?assertEqual(
                        ok,
                        wait_until(fun() ->
                            meck:num_calls(
                                beamtalk_class_registry, restart_class, ['RestartFailClass']
                            ) > 0
                        end)
                    ),
                    ?assert(is_process_alive(MonPid))
                after
                    meck:unload(beamtalk_class_registry)
                end
            end},
            {"restart_class raising an exception is caught; the monitor survives", fun() ->
                meck:new(beamtalk_class_registry, [passthrough]),
                meck:expect(beamtalk_class_registry, restart_class, fun(_Name) ->
                    error(injected_crash)
                end),
                try
                    Dummy = spawn_dummy(),
                    ok = beamtalk_class_monitor:watch('RestartCrashClass', Dummy),
                    kill_and_wait(Dummy, kill),
                    ?assertEqual(
                        ok,
                        wait_until(fun() ->
                            meck:num_calls(
                                beamtalk_class_registry, restart_class, ['RestartCrashClass']
                            ) > 0
                        end)
                    ),
                    ?assert(is_process_alive(MonPid))
                after
                    meck:unload(beamtalk_class_registry)
                end
            end}
        ]
    end}.

%%====================================================================
%% Restart budget exhaustion — deterministic, without a real class registry
%%====================================================================

restart_budget_exhausted_test_() ->
    {setup, fun() -> setup(#{max_restarts => 1, window_ms => 60000}) end, fun teardown/1, fun(_Ctx) ->
        {"a second crash inside the window is not retried once the budget is spent", fun() ->
            meck:new(beamtalk_class_registry, [passthrough]),
            meck:expect(beamtalk_class_registry, restart_class, fun(_Name) ->
                {ok, spawn_dummy()}
            end),
            try
                Dummy1 = spawn_dummy(),
                ok = beamtalk_class_monitor:watch('BudgetClass', Dummy1),
                kill_and_wait(Dummy1, kill),
                ?assertEqual(
                    ok,
                    wait_until(fun() ->
                        meck:num_calls(beamtalk_class_registry, restart_class, ['BudgetClass']) =:=
                            1
                    end)
                ),
                %% The mock's {ok, NewPid} return doesn't itself re-invoke watch/2
                %% the way the real restart_class/1 -> beamtalk_object_class:start
                %% chain would — re-watch a fresh dummy to stand in for "the
                %% restarted instance", so the second crash lands inside the same
                %% budget window as the first.
                Dummy2 = spawn_dummy(),
                ok = beamtalk_class_monitor:watch('BudgetClass', Dummy2),
                kill_and_wait(Dummy2, kill),
                timer:sleep(200),
                ?assertEqual(
                    1, meck:num_calls(beamtalk_class_registry, restart_class, ['BudgetClass'])
                )
            after
                meck:unload(beamtalk_class_registry)
            end
        end}
    end}.
