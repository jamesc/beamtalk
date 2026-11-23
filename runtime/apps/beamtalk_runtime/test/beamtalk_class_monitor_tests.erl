%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_monitor_tests).

-moduledoc """
Direct EUnit tests for `beamtalk_class_monitor` against plain
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
`beamtalk_repl_loader_rewrite_sites_tests:setup_with_install_fault/0`) —
a plain spawned dummy process can be watched and killed, but it
cannot stand in for a real class gen_server that `restart_class/1` itself
would rebuild from ETS metadata.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% Polling uses `beamtalk_test_wait:until/1` (default budget, ~6s: 100 x 20ms
%% proved too tight under full-suite parallel load, BT-4330); the loop still
%% exits as soon as the predicate is true.
%% `kill_and_wait/2`'s own 'DOWN' wait.
-define(KILL_WAIT_MS, 5000).

%% Every test that polls with `beamtalk_test_wait:until/1` (or waits in `kill_and_wait/2`)
%% runs under this explicit EUnit timeout, via `timed/1`. EUnit's default
%% is 5s — shorter than one wait budget — so without it a slow
%% 'DOWN' -> restart_class path (meck recompiling the cover-compiled
%% registry under the full parallel suite) was killed as `cancelled`
%% instead of reaching the assertion (BT-4427).
-define(TEST_TIMEOUT_S, 30).
%% Worst case per test (two `until/1` waits + two `kill_and_wait/2` calls) is
%% ~22s, under this timeout.

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
    ok = sys:log(Pid, {true, 50}),
    #{monitor => Pid, restore => Restore}.

teardown(#{monitor := Pid, restore := Restore}) ->
    stop_if_alive(Pid),
    %% A test may have replaced the monitor; stop whatever currently holds
    %% the registered name too.
    stop_if_alive(whereis(beamtalk_class_monitor)),
    case Restore of
        true ->
            try
                supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_monitor)
            catch
                _:_ -> ok
            end;
        false ->
            ok
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

kill_and_wait(Pid, Reason) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, Reason),
    ?awaitMessage({'DOWN', MRef, process, Pid, _}, ?KILL_WAIT_MS).

timed(Fun) ->
    {timeout, ?TEST_TIMEOUT_S, Fun}.

%% Install a `restart_class/1` mock that reports every call to `Self` as
%% `{restart_class_called, Name}` before returning a fresh idle pid, so a test
%% synchronises on the monitor actually having run the restart instead of
%% polling meck's call counter against a timeout (BT-4915). The message is sent
%% from the monitor process while it handles the 'DOWN'.
expect_restart_reports_to(Self, Result) ->
    meck:expect(beamtalk_class_registry, restart_class, fun(Name) ->
        Self ! {restart_class_called, Name},
        Result()
    end).

fresh_idle_pid() ->
    {ok, beamtalk_test_proc:spawn_idle()}.

%% Block until the mock reported a restart of `Name` (an event, not a poll).
await_restart_called(Name) ->
    try
        ?awaitMessage({restart_class_called, Name}, ?KILL_WAIT_MS)
    catch
        error:{await_message_timeout, Info} ->
            erlang:error({await_message_timeout, Info ++ [{monitor, monitor_diagnostics()}]})
    end.

%% Watch `Pid` and return only once `Pid` itself reports the monitor in its
%% `monitored_by` list. `watch/2` replies after the monitor has *requested* the
%% monitor, but the request reaches `Pid` as a signal, and the BEAM orders
%% signals only per sender pair: the test's own `exit(Pid, kill)`, sent right
%% after `watch/2` returned, can overtake it. `Pid` then dies first and the
%% monitor gets `'DOWN'` with `noproc` instead of `killed` -- which it rightly
%% treats as a deliberate stop and never restarts (BT-5102). Killing only after
%% the target confirms the monitor makes the `'DOWN'` reason `killed`.
watch_armed(ClassName, Pid) ->
    ok = beamtalk_class_monitor:watch(ClassName, Pid),
    beamtalk_test_wait:monitored_by(Pid, beamtalk_class_monitor).

%% What the registered monitor was doing when a wait for it expired, as a list
%% of lines (EUnit truncates deeply nested terms). Its own event log shows
%% whether it saw the 'DOWN', with which reason, and what it did about it.
monitor_diagnostics() ->
    case whereis(beamtalk_class_monitor) of
        undefined ->
            ["no monitor registered"];
        Mon ->
            Events =
                try sys:log(Mon, get) of
                    {ok, Log} -> lists:sublist(lists:reverse(Log), 8);
                    Other -> [{log_unavailable, Other}]
                catch
                    Class:Reason -> [{log_unavailable, {Class, Reason}}]
                end,
            Registry =
                try meck:history(beamtalk_class_registry) of
                    History ->
                        [{element(2, element(2, H)), element(3, element(2, H))} || H <- History]
                catch
                    Class2:Reason2 -> {not_mocked, {Class2, Reason2}}
                end,
            State =
                try
                    sys:get_state(Mon, 1000)
                catch
                    Class3:Reason3 -> {unavailable, {Class3, Reason3}}
                end,
            [
                line("pid", Mon),
                line(
                    "process_info",
                    process_info(Mon, [status, message_queue_len, current_stacktrace])
                ),
                line("state", State),
                line("registry calls", Registry)
            ] ++ [line("event (newest first)", E) || E <- Events]
    end.

line(Label, Term) ->
    lists:flatten(io_lib:format("~s: ~0P", [Label, Term, 40])).

%% Count (and consume) the `restart_class/1` reports still in the mailbox. Call
%% only after a sync with the monitor (`sys:get_state/1`, whose reply follows
%% every earlier message from the monitor): meck's own call history is written
%% by an asynchronous cast, so `meck:num_calls/3` can lag behind the call.
drain_restart_calls(Name) ->
    receive
        {restart_class_called, Name} -> 1 + drain_restart_calls(Name)
    after 0 -> 0
    end.

%% Whether the registered monitor currently watches Pid. Matches the
%% `#state{}` tuple positionally, so a change to the record's shape fails
%% this match loudly instead of reading the wrong field.
is_watched(Pid) ->
    {state, _Max, _Window, Watched, _Refs, _History} = sys:get_state(beamtalk_class_monitor),
    maps:is_key(Pid, Watched).

%% How many monitor refs the registered monitor holds for ClassName.
watch_count(ClassName) ->
    {state, _Max, _Window, _Watched, Refs, _History} = sys:get_state(beamtalk_class_monitor),
    length([R || R := N <- Refs, N =:= ClassName]).

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
        true ->
            try
                supervisor:restart_child(beamtalk_runtime_sup, beamtalk_class_monitor)
            catch
                _:_ -> ok
            end;
        false ->
            ok
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
        {"watching the same live pid twice installs only one monitor ref",
            timed(fun() ->
                meck:new(beamtalk_class_registry, [passthrough]),
                expect_restart_reports_to(self(), fun fresh_idle_pid/0),
                try
                    Dummy = beamtalk_test_proc:spawn_idle(),
                    ok = watch_armed('DedupClass', Dummy),
                    %% Second watch/2 on the same still-live pid must be a no-op
                    %% (do_watch/3's `maps:is_key(Pid, Watched) -> true` branch) —
                    %% otherwise this pid's death would fire two independent
                    %% 'DOWN' messages and restart_class/1 would run twice.
                    ok = watch_armed('DedupClass', Dummy),
                    %% Structural check, no waiting: the second watch/2 above is a
                    %% synchronous call, so the monitor holds exactly one ref now.
                    ?assertEqual(1, watch_count('DedupClass')),
                    kill_and_wait(Dummy, kill),
                    ok = await_restart_called('DedupClass'),
                    %% A wrongly-duplicated ref would have queued a second 'DOWN'
                    %% (and a second restart message) at the same exit; sys:get_state
                    %% is served after the monitor has handled everything already in
                    %% its mailbox, so by here a duplicate would have been counted.
                    ?assertEqual(0, watch_count('DedupClass')),
                    ?assertEqual(0, drain_restart_calls('DedupClass'))
                after
                    meck:unload(beamtalk_class_registry)
                end
            end)}
    end}.

%%====================================================================
%% is_deliberate_stop/1 — shutdown / {shutdown, _} are never restarted
%%====================================================================

deliberate_stop_shutdown_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Ctx) ->
        [
            {"a plain 'shutdown' exit is not treated as a crash",
                timed(fun() ->
                    assert_no_restart_on_exit('ShutdownClass1', shutdown)
                end)},
            {"a {shutdown, Reason} exit is not treated as a crash",
                timed(fun() ->
                    assert_no_restart_on_exit('ShutdownClass2', {shutdown, going_away})
                end)}
        ]
    end}.

assert_no_restart_on_exit(ClassName, ExitReason) ->
    meck:new(beamtalk_class_registry, [passthrough]),
    expect_restart_reports_to(self(), fun fresh_idle_pid/0),
    try
        Dummy = beamtalk_test_proc:spawn_idle(),
        ok = watch_armed(ClassName, Dummy),
        kill_and_wait(Dummy, ExitReason),
        %% The 'DOWN' handler drops Dummy from `watched` in the same step that
        %% decides whether to restart, so once it is gone the decision is final:
        %% no fixed sleep to hope a wrong restart would have shown up by then.
        ?assertEqual(ok, beamtalk_test_wait:until(fun() -> not is_watched(Dummy) end)),
        ?assertEqual(0, drain_restart_calls(ClassName))
    after
        meck:unload(beamtalk_class_registry)
    end.

%%====================================================================
%% BT-5110: a process that died before the monitor request reached it
%%====================================================================

early_death_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_Ctx) ->
        [
            {"noproc with metadata still present restarts the class eagerly",
                timed(fun() -> early_death('EarlyDeathKept', {ok, some_module}, true) end)},
            {"noproc with the metadata gone (a removed class) is not resurrected",
                timed(fun() -> early_death('EarlyDeathRemoved', not_found, false) end)},
            {"noproc for a class a lazy recovery already restarted is left alone",
                timed(fun() -> early_death('EarlyDeathLive', {ok, some_module}, false, self()) end)}
        ]
    end}.

early_death(ClassName, MetadataLookup, ExpectRestart) ->
    early_death(ClassName, MetadataLookup, ExpectRestart, undefined).

early_death(ClassName, MetadataLookup, ExpectRestart, LivePid) ->
    meck:new(beamtalk_class_registry, [passthrough]),
    LivePid =:= undefined orelse
        meck:expect(beamtalk_class_registry, whereis_class, fun(_) -> LivePid end),
    meck:new(beamtalk_class_metadata, [passthrough]),
    meck:expect(beamtalk_class_metadata, lookup_module, fun
        (Name) when Name =:= ClassName -> MetadataLookup;
        (Name) -> meck:passthrough([Name])
    end),
    expect_restart_reports_to(self(), fun fresh_idle_pid/0),
    try
        Dead = beamtalk_test_proc:spawn_idle(),
        beamtalk_test_proc:stop(Dead),
        ok = beamtalk_class_monitor:watch(ClassName, Dead),
        case ExpectRestart of
            true ->
                await_restart_called(ClassName);
            false ->
                ?assertEqual(ok, beamtalk_test_wait:until(fun() -> not is_watched(Dead) end)),
                ?assertEqual(0, drain_restart_calls(ClassName))
        end
    after
        meck:unload(beamtalk_class_metadata),
        meck:unload(beamtalk_class_registry)
    end.

%%====================================================================
%% BT-5046: crash history is dropped when the class is removed
%%====================================================================

unwatch_prunes_crash_history_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(#{monitor := MonPid}) ->
        [
            {"unwatch/1 drops the class's crash history",
                timed(fun() ->
                    meck:new(beamtalk_class_registry, [passthrough]),
                    expect_restart_reports_to(self(), fun() -> {error, injected_failure} end),
                    try
                        Dummy = beamtalk_test_proc:spawn_idle(),
                        ok = watch_armed('HistoryPruneClass', Dummy),
                        kill_and_wait(Dummy, kill),
                        ok = await_restart_called('HistoryPruneClass'),
                        _ = sys:get_state(MonPid),
                        {state, _, _, _, _, History1} = sys:get_state(MonPid),
                        ?assert(maps:is_key('HistoryPruneClass', History1)),
                        ok = beamtalk_class_monitor:unwatch('HistoryPruneClass'),
                        {state, _, _, _, _, History2} = sys:get_state(MonPid),
                        ?assertNot(maps:is_key('HistoryPruneClass', History2))
                    after
                        meck:unload(beamtalk_class_registry)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% restart_class/1 itself failing — the monitor must log and carry on
%%====================================================================

restart_failure_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(Ctx) ->
        #{monitor := MonPid} = Ctx,
        [
            {"restart_class returning {error, _} is logged; the monitor survives",
                timed(fun() ->
                    meck:new(beamtalk_class_registry, [passthrough]),
                    expect_restart_reports_to(self(), fun() -> {error, injected_failure} end),
                    try
                        Dummy = beamtalk_test_proc:spawn_idle(),
                        ok = watch_armed('RestartFailClass', Dummy),
                        kill_and_wait(Dummy, kill),
                        ok = await_restart_called('RestartFailClass'),
                        % Synchronise with the monitor finishing the 'DOWN' handler
                        % (which logs and swallows the failure) before checking it lives.
                        _ = sys:get_state(MonPid),
                        ?assert(is_process_alive(MonPid))
                    after
                        meck:unload(beamtalk_class_registry)
                    end
                end)},
            {"restart_class raising an exception is caught; the monitor survives",
                timed(fun() ->
                    meck:new(beamtalk_class_registry, [passthrough]),
                    expect_restart_reports_to(self(), fun() -> error(injected_crash) end),
                    try
                        Dummy = beamtalk_test_proc:spawn_idle(),
                        ok = watch_armed('RestartCrashClass', Dummy),
                        kill_and_wait(Dummy, kill),
                        ok = await_restart_called('RestartCrashClass'),
                        % Synchronise with the monitor finishing the 'DOWN' handler
                        % (which logs and swallows the failure) before checking it lives.
                        _ = sys:get_state(MonPid),
                        ?assert(is_process_alive(MonPid))
                    after
                        meck:unload(beamtalk_class_registry)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% Restart budget exhaustion — deterministic, without a real class registry
%%====================================================================

restart_budget_exhausted_test_() ->
    {setup, fun() -> setup(#{max_restarts => 1, window_ms => 60000}) end, fun teardown/1, fun(_Ctx) ->
        {"a second crash inside the window is not retried once the budget is spent",
            timed(fun() ->
                meck:new(beamtalk_class_registry, [passthrough]),
                expect_restart_reports_to(self(), fun fresh_idle_pid/0),
                try
                    Dummy1 = beamtalk_test_proc:spawn_idle(),
                    ok = watch_armed('BudgetClass', Dummy1),
                    kill_and_wait(Dummy1, kill),
                    %% Wait for the event, not a meck call counter: the monitor
                    %% reports from inside restart_class/1 itself (BT-4915).
                    ok = await_restart_called('BudgetClass'),
                    %% Let the 'DOWN' handler finish so the restart is counted in
                    %% the budget history before the second crash is delivered.
                    _ = sys:get_state(beamtalk_class_monitor),
                    %% The mock's {ok, NewPid} return doesn't itself re-invoke watch/2
                    %% the way the real restart_class/1 -> beamtalk_object_class:start
                    %% chain would — re-watch a fresh dummy to stand in for "the
                    %% restarted instance", so the second crash lands inside the same
                    %% budget window as the first.
                    Dummy2 = beamtalk_test_proc:spawn_idle(),
                    ok = watch_armed('BudgetClass', Dummy2),
                    %% watch_armed/2 returned, so Dummy2 is already watched.
                    ?assert(is_watched(Dummy2)),
                    kill_and_wait(Dummy2, kill),
                    %% The 'DOWN' handler drops Dummy2 from `watched` in the same
                    %% step that decides (here: refuses) the restart, so once it
                    %% is gone the restart count is final (BT-4382).
                    ?assertEqual(ok, beamtalk_test_wait:until(fun() -> not is_watched(Dummy2) end)),
                    ?assertEqual(0, drain_restart_calls('BudgetClass'))
                after
                    meck:unload(beamtalk_class_registry)
                end
            end)}
    end}.
