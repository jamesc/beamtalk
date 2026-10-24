%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_trace_store_tests).

-moduledoc """
EUnit tests for beamtalk_trace_store (ADR 0069 Phase 1).

Tests cover: direct insert, query, ring buffer eviction, counters aggregates,
clear, composite key uniqueness, gen_server crash recovery (heir),
telemetry handler callbacks, and telemetry_poller VM stats.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    %% Ensure telemetry is available
    _ = application:ensure_all_started(telemetry),
    _ = application:ensure_all_started(telemetry_poller),
    %% Start the trace store if not already running
    case whereis(beamtalk_trace_store) of
        undefined ->
            {ok, _Pid} = beamtalk_trace_store:start_link(),
            ok;
        _Pid ->
            ok
    end.

cleanup(_State) ->
    %% Clear all data between tests
    try
        beamtalk_trace_store:clear(),
        beamtalk_trace_store:disable()
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Test: Direct insert and query
%%====================================================================

direct_insert_and_query_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Record a dispatch directly (bypassing telemetry)
                TestPid = self(),
                beamtalk_trace_store:record_dispatch(
                    TestPid, increment, 5000, ok, sync, 'Counter'
                ),

                %% Always-on class entry, and no per-actor entry while disabled
                ?assertEqual(#{}, beamtalk_trace_store:get_stats(TestPid)),
                #{<<"methods">> := #{<<"increment">> := IncStats}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(1, maps:get(<<"calls">>, IncStats)),
                ?assertEqual(1, maps:get(<<"ok">>, IncStats)),
                ?assertEqual(0, maps:get(<<"errors">>, IncStats)),
                ?assertEqual(5000, maps:get(<<"total_duration_ns">>, IncStats)),

                %% Enabled, the per-actor entry appears too
                beamtalk_trace_store:enable(),
                beamtalk_trace_store:record_dispatch(
                    TestPid, increment, 7000, ok, sync, 'Counter'
                ),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                #{PidKey := #{<<"methods">> := #{<<"increment">> := PidInc}}} =
                    beamtalk_trace_store:get_stats(TestPid),
                ?assertEqual(1, maps:get(<<"calls">>, PidInc)),
                ?assertEqual(7000, maps:get(<<"total_duration_ns">>, PidInc))
            end)
        ]
    end}.

%%====================================================================
%% Test: Class name wired into actor stats
%%====================================================================

class_in_actor_stats_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {"class from telemetry metadata keys the always-on entry",
                ?_test(begin
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, increment, 5000, ok, sync, 'EventStore'
                    ),
                    Entry = class_entry(<<"EventStore">>),
                    ?assertEqual(<<"EventStore">>, maps:get(<<"class">>, Entry)),
                    ?assertMatch(
                        #{<<"increment">> := #{<<"calls">> := 1}},
                        maps:get(<<"methods">>, Entry)
                    )
                end)},
            {"unknown class aggregates under class:unknown",
                ?_test(begin
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, increment, 5000, ok, sync, unknown
                    ),
                    Entry = class_entry(<<"unknown">>),
                    ?assertEqual(<<"unknown">>, maps:get(<<"class">>, Entry))
                end)},
            {"per-actor entry carries the telemetry class while enabled",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, increment, 5000, ok, sync, 'EventStore'
                    ),
                    PidKey = list_to_binary(pid_to_list(TestPid)),
                    #{PidKey := PidStats} = beamtalk_trace_store:get_stats(TestPid),
                    ?assertEqual(<<"EventStore">>, maps:get(<<"class">>, PidStats))
                end)},
            {"class appears in collect_method_stats via slow_methods",
                ?_test(begin
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, slow_op, 50000, ok, sync, 'WorkflowEngine'
                    ),
                    Result = beamtalk_trace_store:slow_methods(1),
                    [Top | _] = Result,
                    ?assertEqual(<<"WorkflowEngine">>, maps:get(<<"class">>, Top))
                end)}
        ]
    end}.

%%====================================================================
%% Test: Trace event recording when enabled
%%====================================================================

trace_event_recording_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),
                %% No events when disabled
                beamtalk_trace_store:record_trace_event(
                    TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                ),
                ?assertEqual([], beamtalk_trace_store:get_traces()),

                %% Enable and record
                beamtalk_trace_store:enable(),
                ?assert(beamtalk_trace_store:is_enabled()),

                beamtalk_trace_store:record_trace_event(
                    TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                ),
                beamtalk_trace_store:record_trace_event(
                    TestPid, 'Counter', decrement, sync, 3000, ok, #{}, stop
                ),

                Traces = beamtalk_trace_store:get_traces(),
                ?assertEqual(2, length(Traces)),

                %% Newest first
                [First | _] = Traces,
                ?assertEqual(<<"decrement">>, maps:get(<<"selector">>, First)),

                %% Filter by pid
                TracesForPid = beamtalk_trace_store:get_traces(TestPid),
                ?assertEqual(2, length(TracesForPid)),

                %% Filter by pid + selector
                TracesForSel = beamtalk_trace_store:get_traces(TestPid, increment),
                ?assertEqual(1, length(TracesForSel)),
                ?assertEqual(<<"increment">>, maps:get(<<"selector">>, hd(TracesForSel)))
            end)
        ]
    end}.

%%====================================================================
%% Test: Ring buffer eviction
%%====================================================================

ring_buffer_eviction_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                MaxEvents = 50,
                %% Insert exactly 55 events: one sweep removes 10%
                %% (55 * 10 div 100 = 5), leaving exactly 50 = MaxEvents.
                %% This makes further sweeps no-ops (50 > 50 is false),
                %% so the result is stable regardless of whether the
                %% periodic 1s sweep timer also fires.
                InsertCount = 55,
                ExpectedAfterSweep = InsertCount - (InsertCount * 10 div 100),

                %% Clear any events from other tests to start clean
                beamtalk_trace_store:clear(),

                %% Capture original max_events so we restore the real
                %% value, not a hard-coded constant.
                OrigMax = beamtalk_trace_store:max_events(),

                %% Set a small max for testing; restore in `after`
                %% so a failing assertion doesn't leave max_events=50
                %% for subsequent tests.
                beamtalk_trace_store:max_events(MaxEvents),
                try
                    ?assertEqual(MaxEvents, beamtalk_trace_store:max_events()),

                    %% Enable tracing and insert more than max
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    lists:foreach(
                        fun(I) ->
                            beamtalk_trace_store:record_trace_event(
                                TestPid, 'Counter', increment, sync, I * 100, ok, #{}, stop
                            )
                        end,
                        lists:seq(1, InsertCount)
                    ),

                    %% Trigger sweep and wait for it to complete.
                    %% sys:get_state/1 forces a round-trip through the
                    %% gen_server mailbox, ensuring the sweep_eviction
                    %% message has been processed.
                    beamtalk_trace_store ! sweep_eviction,
                    sys:get_state(beamtalk_trace_store),

                    %% After sweep, exactly ExpectedAfterSweep events
                    %% should remain for our pid. Filter by TestPid to
                    %% avoid interference from concurrent test suites.
                    PostSweep = beamtalk_trace_store:get_traces(TestPid),
                    ?assertEqual(ExpectedAfterSweep, length(PostSweep))
                after
                    beamtalk_trace_store:max_events(OrigMax)
                end
            end)
        ]
    end}.

%%====================================================================
%% Test: Counters aggregates
%%====================================================================

counters_aggregates_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                beamtalk_trace_store:enable(),
                TestPid = self(),

                %% Record multiple dispatches
                beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, increment, 2000, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(
                    TestPid, increment, 3000, error, sync, 'Counter'
                ),

                %% Check aggregated per-actor stats
                Stats = beamtalk_trace_store:get_stats(TestPid),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats = maps:get(PidKey, Stats),
                IncStats = maps:get(<<"increment">>, maps:get(<<"methods">>, PidStats)),

                ?assertEqual(3, maps:get(<<"calls">>, IncStats)),
                ?assertEqual(2, maps:get(<<"ok">>, IncStats)),
                ?assertEqual(1, maps:get(<<"errors">>, IncStats)),
                ?assertEqual(6000, maps:get(<<"total_duration_ns">>, IncStats)),
                ?assertEqual(2000, maps:get(<<"avg_duration_ns">>, IncStats)),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, IncStats)),
                ?assertEqual(3000, maps:get(<<"max_duration_ns">>, IncStats)),

                %% The always-on class entry carries the same totals
                #{<<"methods">> := #{<<"increment">> := ClassInc}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(3, maps:get(<<"calls">>, ClassInc)),
                ?assertEqual(6000, maps:get(<<"total_duration_ns">>, ClassInc))
            end)
        ]
    end}.

%%====================================================================
%% Test: Clear resets all data
%%====================================================================

clear_resets_all_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Insert some data
                beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync, 'Counter'),
                beamtalk_trace_store:enable(),
                beamtalk_trace_store:record_trace_event(
                    TestPid, 'Counter', increment, sync, 1000, ok, #{}, stop
                ),

                %% Verify data exists
                ?assertNotEqual(#{}, beamtalk_trace_store:get_stats()),
                ?assertNotEqual([], beamtalk_trace_store:get_traces()),

                %% Clear
                beamtalk_trace_store:clear(),

                %% Verify all data is gone
                ?assertEqual(#{}, beamtalk_trace_store:get_stats()),
                ?assertEqual([], beamtalk_trace_store:get_traces())
            end)
        ]
    end}.

%%====================================================================
%% Test: Composite key uniqueness
%%====================================================================

composite_key_uniqueness_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                beamtalk_trace_store:enable(),
                TestPid = self(),

                %% Insert events rapidly — composite key should ensure uniqueness
                lists:foreach(
                    fun(_) ->
                        beamtalk_trace_store:record_trace_event(
                            TestPid, 'Counter', increment, sync, 100, ok, #{}, stop
                        )
                    end,
                    lists:seq(1, 100)
                ),

                Traces = beamtalk_trace_store:get_traces(),
                ?assertEqual(100, length(Traces))
            end)
        ]
    end}.

%%====================================================================
%% Test: Slow methods query
%%====================================================================

slow_methods_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Record methods with different durations
                beamtalk_trace_store:record_dispatch(TestPid, fast, 100, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, slow, 10000, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, medium, 5000, ok, sync, 'Counter'),

                Result = beamtalk_trace_store:slow_methods(2),
                ?assertEqual(2, length(Result)),
                [Slowest | _] = Result,
                ?assertEqual(slow, maps:get(<<"selector">>, Slowest))
            end)
        ]
    end}.

%%====================================================================
%% Test: Hot methods query
%%====================================================================

hot_methods_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Record methods with different call counts
                beamtalk_trace_store:record_dispatch(TestPid, rare, 100, ok, sync, 'Counter'),
                lists:foreach(
                    fun(_) ->
                        beamtalk_trace_store:record_dispatch(
                            TestPid, popular, 100, ok, sync, 'Counter'
                        )
                    end,
                    lists:seq(1, 10)
                ),

                Result = beamtalk_trace_store:hot_methods(1),
                ?assertEqual(1, length(Result)),
                ?assertEqual(popular, maps:get(<<"selector">>, hd(Result)))
            end)
        ]
    end}.

%%====================================================================
%% Test: Error methods query
%%====================================================================

error_methods_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Record methods with different error rates
                beamtalk_trace_store:record_dispatch(TestPid, reliable, 100, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, reliable, 100, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, flaky, 100, error, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, flaky, 100, timeout, sync, 'Counter'),

                Result = beamtalk_trace_store:error_methods(1),
                ?assertEqual(1, length(Result)),
                ?assertEqual(flaky, maps:get(<<"selector">>, hd(Result)))
            end)
        ]
    end}.

%%====================================================================
%% Test: Actor health
%%====================================================================

actor_health_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Health of a live process
                TestPid = self(),
                Health = beamtalk_trace_store:actor_health(TestPid),
                ?assertEqual(list_to_binary(pid_to_list(TestPid)), maps:get(<<"pid">>, Health)),
                ?assertNotEqual(dead, maps:get(<<"status">>, Health)),
                ?assert(is_integer(maps:get(<<"queue_depth">>, Health))),
                ?assert(is_integer(maps:get(<<"memory_kb">>, Health))),
                ?assert(is_integer(maps:get(<<"reductions">>, Health)))
            end),
            ?_test(begin
                %% Health of a dead process
                DeadPid = spawn(fun() -> ok end),
                timer:sleep(50),
                Health = beamtalk_trace_store:actor_health(DeadPid),
                ?assertEqual(dead, maps:get(<<"status">>, Health)),
                ?assertEqual(<<"process not alive">>, maps:get(<<"error">>, Health))
            end)
        ]
    end}.

%%====================================================================
%% Test: System health
%%====================================================================

system_health_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                Health = beamtalk_trace_store:system_health(),
                ?assert(is_integer(maps:get(<<"scheduler_count">>, Health))),
                ?assert(maps:get(<<"scheduler_count">>, Health) > 0),
                ?assert(is_integer(maps:get(<<"process_count">>, Health))),
                ?assert(is_map(maps:get(<<"memory">>, Health))),
                Memory = maps:get(<<"memory">>, Health),
                ?assert(is_integer(maps:get(<<"total_mb">>, Memory))),
                ?assert(is_integer(maps:get(<<"run_queue">>, Health)))
            end)
        ]
    end}.

%%====================================================================
%% Test: Bottlenecks query
%%====================================================================

bottlenecks_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% bottlenecks/1 scans the instance registry, so it needs no
                %% dispatches and works whether or not tracing is enabled.
                case whereis(beamtalk_object_instances) of
                    undefined -> {ok, _} = beamtalk_object_instances:start_link();
                    _ -> ok
                end,
                Self = self(),
                Pid = spawn(fun() ->
                    Self ! ready,
                    receive
                        stop -> ok
                    end
                end),
                receive
                    ready -> ok
                end,
                ok = beamtalk_object_instances:register('Counter', Pid),
                try
                    Result = beamtalk_trace_store:bottlenecks(10),
                    ?assert(is_list(Result)),
                    %% Our registered process should be in the list
                    PidBin = list_to_binary(pid_to_list(Pid)),
                    Pids = [maps:get(<<"actor">>, R) || R <- Result],
                    ?assert(lists:member(PidBin, Pids)),
                    [Entry] = [R || R <- Result, maps:get(<<"actor">>, R) =:= PidBin],
                    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, Entry)),

                    %% An unregistered process with dispatches never appears
                    Ghost = spawn(fun() ->
                        Self ! ghost_ready,
                        receive
                            stop -> ok
                        end
                    end),
                    receive
                        ghost_ready -> ok
                    end,
                    beamtalk_trace_store:record_dispatch(
                        Ghost, test_method, 100, ok, sync, 'Counter'
                    ),
                    Result2 = beamtalk_trace_store:bottlenecks(1000),
                    GhostBin = list_to_binary(pid_to_list(Ghost)),
                    ?assertNot(
                        lists:member(GhostBin, [maps:get(<<"actor">>, R) || R <- Result2])
                    ),
                    Ghost ! stop
                after
                    beamtalk_object_instances:unregister('Counter', Pid),
                    Pid ! stop
                end
            end),
            {"a pid registered under two classes reports once",
                ?_test(begin
                    case whereis(beamtalk_object_instances) of
                        undefined ->
                            {ok, _} = beamtalk_object_instances:start_link();
                        _ ->
                            ok
                    end,
                    Self = self(),
                    Pid = spawn(fun() ->
                        Self ! ready,
                        receive
                            stop -> ok
                        end
                    end),
                    receive
                        ready -> ok
                    end,
                    ok = beamtalk_object_instances:register('Counter', Pid),
                    ok = beamtalk_object_instances:register('Other', Pid),
                    try
                        Result = beamtalk_trace_store:bottlenecks(1000),
                        PidBin = list_to_binary(pid_to_list(Pid)),
                        Matches = [R || R <- Result, maps:get(<<"actor">>, R) =:= PidBin],
                        ?assertEqual(1, length(Matches))
                    after
                        beamtalk_object_instances:unregister('Counter', Pid),
                        beamtalk_object_instances:unregister('Other', Pid),
                        Pid ! stop
                    end
                end)}
        ]
    end}.

%%====================================================================
%% Test: Telemetry handler - dispatch stop
%%====================================================================

telemetry_dispatch_stop_handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),
                Measurements = #{duration => 5000},
                Metadata = #{
                    pid => TestPid,
                    class => 'Counter',
                    selector => increment,
                    mode => sync,
                    outcome => ok
                },

                %% Call handler directly
                beamtalk_trace_store:handle_dispatch_stop(
                    [beamtalk, actor, dispatch, stop],
                    Measurements,
                    Metadata,
                    #{}
                ),

                %% Verify the always-on class aggregate was recorded
                #{<<"methods">> := #{<<"increment">> := IncStats}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(1, maps:get(<<"calls">>, IncStats))
            end)
        ]
    end}.

%%====================================================================
%% Test: Telemetry handler - dispatch exception
%%====================================================================

telemetry_dispatch_exception_handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),
                Measurements = #{duration => 3000},
                Metadata = #{
                    pid => TestPid,
                    class => 'Counter',
                    selector => badmethod,
                    mode => sync,
                    kind => error,
                    reason => badarg
                },

                %% Call handler directly
                beamtalk_trace_store:handle_dispatch_exception(
                    [beamtalk, actor, dispatch, exception],
                    Measurements,
                    Metadata,
                    #{}
                ),

                %% Verify error was recorded in the always-on class entry
                #{<<"methods">> := #{<<"badmethod">> := MethodStats}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(1, maps:get(<<"errors">>, MethodStats))
            end)
        ]
    end}.

%%====================================================================
%% Test: Enable/disable toggle
%%====================================================================

enable_disable_toggle_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Initially disabled
                ?assertNot(beamtalk_trace_store:is_enabled()),

                %% Enable
                beamtalk_trace_store:enable(),
                ?assert(beamtalk_trace_store:is_enabled()),

                %% Disable
                beamtalk_trace_store:disable(),
                ?assertNot(beamtalk_trace_store:is_enabled())
            end)
        ]
    end}.

%%====================================================================
%% Test: Crash recovery preserves exact aggregates (heir)
%%====================================================================

%% Killing or gracefully stopping the store must not lose aggregate data:
%% the ETS tables pass to the heir and the restarted store reclaims them
%% with exact counts, and the trace events survive too. Both death paths
%% are pinned. The store is taken direct first (a supervising tree's child
%% is terminated, so no restart races the assertions) and handed back
%% afterwards, whatever the ambient suite state.
crash_recovery_preserves_exact_counts_test_() ->
    {setup, fun setup/0, fun crash_cleanup/1, fun(_State) ->
        [
            {"graceful restart preserves aggregates exactly",
                ?_test(begin
                    take_store_direct(),
                    TestPid = seed_crash_test_data(),
                    gen_server:stop(beamtalk_trace_store),
                    timer:sleep(50),
                    {ok, _} = beamtalk_trace_store:start_link(),
                    assert_seeded_data_intact(TestPid)
                end)},
            {"kill preserves aggregates exactly",
                ?_test(begin
                    take_store_direct(),
                    TestPid = seed_crash_test_data(),
                    StorePid = whereis(beamtalk_trace_store),
                    %% The store is linked here (direct start above): trap
                    %% the kill's EXIT instead of dying with it.
                    process_flag(trap_exit, true),
                    try
                        exit(StorePid, kill),
                        receive
                            {'EXIT', StorePid, killed} -> ok
                        after 5000 -> ?assert(false)
                        end,
                        {ok, _} = beamtalk_trace_store:start_link(),
                        assert_seeded_data_intact(TestPid)
                    after
                        process_flag(trap_exit, false)
                    end
                end)}
        ]
    end}.

-doc """
Take direct ownership of the trace store however it currently runs: a
supervising tree's child is terminated (dormant spec, so nothing restarts
it) and any directly started store is stopped; then the heir is ensured
and the store started directly, so its tables are heir-protected.
""".
take_store_direct() ->
    case whereis(beamtalk_runtime_sup) of
        undefined ->
            ok;
        SupPid ->
            _ = catch supervisor:terminate_child(SupPid, beamtalk_trace_store)
    end,
    _ = catch gen_server:stop(beamtalk_trace_store),
    wait_until(fun() -> whereis(beamtalk_trace_store) =:= undefined end),
    case whereis(beamtalk_ets_heir) of
        undefined -> {ok, _} = beamtalk_ets_heir:start_link();
        _ -> ok
    end,
    {ok, _} = beamtalk_trace_store:start_link(),
    ok.

-doc """
Hand the store back to its supervisor when there is one (whatever ran
before, the suite state after this test matches it), then clean.
""".
crash_cleanup(State) ->
    case whereis(beamtalk_runtime_sup) of
        undefined ->
            ok;
        SupPid ->
            _ = catch gen_server:stop(beamtalk_trace_store),
            timer:sleep(50),
            case catch supervisor:restart_child(SupPid, beamtalk_trace_store) of
                {ok, _} ->
                    ok;
                _ ->
                    %% No managed child (or it is already running): a
                    %% directly started store serves just as well.
                    case beamtalk_trace_store:start_link() of
                        {ok, _} -> ok;
                        {error, {already_started, _}} -> ok
                    end
            end
    end,
    cleanup(State).

-doc "Seed one dispatch aggregate plus one trace event; answers the test pid.".
seed_crash_test_data() ->
    beamtalk_trace_store:clear(),
    beamtalk_trace_store:enable(),
    TestPid = self(),
    beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync, 'Counter'),
    beamtalk_trace_store:record_trace_event(
        TestPid, 'Counter', increment, sync, 1000, ok, #{}, stop
    ),
    TestPid.

-doc "Assert the seeded dispatch aggregate and trace event survived a restart.".
assert_seeded_data_intact(TestPid) ->
    ?assert(beamtalk_trace_store:is_enabled()),
    #{<<"methods">> := #{<<"increment">> := Inc}} = class_entry(<<"Counter">>),
    ?assertEqual(1, maps:get(<<"calls">>, Inc)),
    ?assertEqual(1000, maps:get(<<"total_duration_ns">>, Inc)),
    PidKey = list_to_binary(pid_to_list(TestPid)),
    #{PidKey := #{<<"methods">> := #{<<"increment">> := PidInc}}} =
        beamtalk_trace_store:get_stats(TestPid),
    ?assertEqual(1, maps:get(<<"calls">>, PidInc)),
    ?assertEqual(1, length(beamtalk_trace_store:get_traces())),
    ok.

%%====================================================================
%% Test: VM measurements handler
%%====================================================================

vm_measurements_handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Simulate a VM measurement event
                Measurements = #{total => 100000, processes => 50000, ets => 20000},
                beamtalk_trace_store:handle_vm_measurements(
                    [vm, memory],
                    Measurements,
                    #{},
                    #{}
                ),
                timer:sleep(50),

                %% System health should include the VM stats
                Health = beamtalk_trace_store:system_health(),
                VmStats = maps:get(<<"vm_stats">>, Health),
                ?assert(is_map(VmStats))
            end)
        ]
    end}.

%%====================================================================
%% Test: Wall-clock timestamp in trace events
%%====================================================================

wall_clock_timestamp_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),
                beamtalk_trace_store:enable(),

                Before = erlang:system_time(microsecond),
                beamtalk_trace_store:record_trace_event(
                    TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                ),
                After = erlang:system_time(microsecond),

                [Trace] = beamtalk_trace_store:get_traces(),
                Ts = maps:get(<<"timestamp_us">>, Trace),
                %% Wall-clock timestamp should be between Before and After
                ?assert(Ts >= Before),
                ?assert(Ts =< After),
                %% Should be a reasonable epoch timestamp (after 2025-01-01)
                ?assert(Ts > 1735689600000000)
            end)
        ]
    end}.

%%====================================================================
%% Test: Serialized counter grow
%%====================================================================

serialized_counter_grow_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Fill up counter slots to trigger a grow.
                %% Initial size is 8000 slots, 8 per key = 1000 keys before grow.
                %% Record enough unique {Class, Selector} keys to force a grow.
                TestPid = self(),
                lists:foreach(
                    fun(I) ->
                        % elp:fixme W0023 intentional atom creation
                        Sel = list_to_atom("method_" ++ integer_to_list(I)),
                        beamtalk_trace_store:record_dispatch(TestPid, Sel, 100, ok, sync, 'Counter')
                    end,
                    lists:seq(1, 1010)
                ),
                %% Verify the atomics grew — stats should have all 1010 methods
                #{<<"methods">> := Methods} = class_entry(<<"Counter">>),
                ?assert(maps:size(Methods) >= 1010)
            end)
        ]
    end}.

%%====================================================================
%% Test: Concurrent hits during grow lose nothing (BT-4103)
%%====================================================================

concurrent_hits_during_grow_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        {timeout, 60,
            ?_test(begin
                %% Hammer one class key from many writers while fresh keys
                %% force a grow underneath: every hit must land exactly once.
                %% A grow that copied the array would lose hit-path writes
                %% landing between the copy and the re-pointing; rows keep
                %% their birth array instead, so there is no window.
                Self = self(),
                Hitters = [
                    spawn(fun() ->
                        [
                            beamtalk_trace_store:record_dispatch(
                                self(), hammered, I, ok, sync, 'Racy'
                            )
                         || I <- lists:seq(1, 500)
                        ],
                        Self ! {hitter_done, self()}
                    end)
                 || _ <- lists:seq(1, 32)
                ],
                %% Class rows bump-allocate (never the free list), so this
                %% many fresh keys guarantees a grow mid-hammering.
                {_Ref, Size, Alloc} = persistent_term:get(beamtalk_trace_counters),
                Pos = atomics:get(Alloc, 1),
                NeedKeys = (Size - Pos) div 8 + 100,
                Growers = [
                    spawn(fun() ->
                        % elp:fixme W0023 intentional atom creation
                        [
                            beamtalk_trace_store:record_dispatch(
                                self(),
                                list_to_atom(
                                    "growrace_" ++ integer_to_list(Idx) ++ "_" ++
                                        integer_to_list(I)
                                ),
                                1,
                                ok,
                                sync,
                                'Racy'
                            )
                         || I <- lists:seq(1, (NeedKeys + 3) div 4)
                        ],
                        Self ! {grower_done, self()}
                    end)
                 || Idx <- lists:seq(1, 4)
                ],
                [
                    receive
                        {hitter_done, H} -> ok
                    end
                 || H <- Hitters
                ],
                [
                    receive
                        {grower_done, G} -> ok
                    end
                 || G <- Growers
                ],
                #{<<"methods">> := #{<<"hammered">> := Hammered}} =
                    class_entry(<<"Racy">>),
                ?assertEqual(32 * 500, maps:get(<<"calls">>, Hammered))
            end)}
    end}.

%%====================================================================
%% Test: Min/max duration tracking
%%====================================================================

min_max_duration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                beamtalk_trace_store:enable(),
                TestPid = self(),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidPing = fun() ->
                    #{PidKey := #{<<"methods">> := #{<<"ping">> := S}}} =
                        beamtalk_trace_store:get_stats(TestPid),
                    S
                end,
                ClassPing = fun() ->
                    #{<<"methods">> := #{<<"ping">> := S}} = class_entry(<<"Counter">>),
                    S
                end,

                %% Single call — min and max should be the same
                beamtalk_trace_store:record_dispatch(TestPid, ping, 5000, ok, sync, 'Counter'),
                ?assertEqual(5000, maps:get(<<"min_duration_ns">>, PidPing())),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, PidPing())),
                ?assertEqual(5000, maps:get(<<"min_duration_ns">>, ClassPing())),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, ClassPing())),

                %% Add a faster call — min should decrease, max stays
                beamtalk_trace_store:record_dispatch(TestPid, ping, 1000, ok, sync, 'Counter'),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, PidPing())),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, PidPing())),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, ClassPing())),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, ClassPing())),

                %% Add a slower call — max should increase, min stays
                beamtalk_trace_store:record_dispatch(TestPid, ping, 11000, ok, sync, 'Counter'),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, PidPing())),
                ?assertEqual(11000, maps:get(<<"max_duration_ns">>, PidPing())),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, ClassPing())),
                ?assertEqual(11000, maps:get(<<"max_duration_ns">>, ClassPing()))
            end)
        ]
    end}.

%%====================================================================
%% Test: Min/max CAS correctness under concurrent updates
%%====================================================================

min_max_concurrent_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 30,
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),

                    %% Pre-allocate the slot with an initial dispatch to avoid the
                    %% first-observation allocation race between concurrent writers.
                    beamtalk_trace_store:record_dispatch(
                        TestPid, concurrent_method, 5000, ok, sync, 'Counter'
                    ),

                    %% Spawn multiple processes that all record dispatches concurrently
                    %% with varying durations. The known min is 199, max is 10000.
                    NumProcesses = 10,
                    CallsPerProcess = 100,
                    Parent = self(),
                    Pids = [
                        spawn(fun() ->
                            lists:foreach(
                                fun(J) ->
                                    Duration = 100 + (J * 99),
                                    beamtalk_trace_store:record_dispatch(
                                        TestPid, concurrent_method, Duration, ok, sync, 'Counter'
                                    )
                                end,
                                lists:seq(1, CallsPerProcess)
                            ),
                            Parent ! {done, self()}
                        end)
                     || _ <- lists:seq(1, NumProcesses)
                    ],

                    %% Wait for all processes to finish
                    lists:foreach(
                        fun(Pid) ->
                            receive
                                {done, Pid} -> ok
                            end
                        end,
                        Pids
                    ),

                    %% Verify min and max
                    Stats = beamtalk_trace_store:get_stats(TestPid),
                    PidKey = list_to_binary(pid_to_list(TestPid)),
                    PidStats = maps:get(PidKey, Stats),
                    MethodStats = maps:get(
                        <<"concurrent_method">>,
                        maps:get(<<"methods">>, PidStats)
                    ),

                    %% Slot pre-allocated, so all concurrent increments hit the same slot.
                    %% 1 pre-allocation + NumProcesses * CallsPerProcess
                    ExpectedCalls = 1 + NumProcesses * CallsPerProcess,
                    ?assertEqual(ExpectedCalls, maps:get(<<"calls">>, MethodStats)),
                    %% Min should be 100 + (1 * 99) = 199 (lower than pre-alloc 5000)
                    ?assertEqual(199, maps:get(<<"min_duration_ns">>, MethodStats)),
                    %% Max should be 100 + (100 * 99) = 10000 (higher than pre-alloc 5000)
                    ?assertEqual(10000, maps:get(<<"max_duration_ns">>, MethodStats))
                end)}
        ]
    end}.

%%====================================================================
%% Test: Clear resets min sentinel
%%====================================================================

clear_resets_min_sentinel_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                beamtalk_trace_store:enable(),
                TestPid = self(),

                %% Record a dispatch to establish min/max
                beamtalk_trace_store:record_dispatch(
                    TestPid, sentinel_check, 5000, ok, sync, 'Counter'
                ),
                Stats1 = beamtalk_trace_store:get_stats(TestPid),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats1 = maps:get(PidKey, Stats1),
                MethodStats1 = maps:get(
                    <<"sentinel_check">>,
                    maps:get(<<"methods">>, PidStats1)
                ),
                ?assertEqual(5000, maps:get(<<"min_duration_ns">>, MethodStats1)),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, MethodStats1)),

                %% Clear stats
                beamtalk_trace_store:clear(),

                %% Stats should be empty
                ?assertEqual(#{}, beamtalk_trace_store:get_stats()),

                %% Record a new dispatch — min should be set fresh, not stuck.
                %% The class row is kept (zeroed in place) while the pid row
                %% is dropped and re-created: both must reset the sentinel.
                beamtalk_trace_store:record_dispatch(
                    TestPid, sentinel_check, 8000, ok, sync, 'Counter'
                ),
                Stats2 = beamtalk_trace_store:get_stats(TestPid),
                PidStats2 = maps:get(PidKey, Stats2),
                MethodStats2 = maps:get(
                    <<"sentinel_check">>,
                    maps:get(<<"methods">>, PidStats2)
                ),
                ?assertEqual(8000, maps:get(<<"min_duration_ns">>, MethodStats2)),
                ?assertEqual(8000, maps:get(<<"max_duration_ns">>, MethodStats2)),
                #{<<"methods">> := #{<<"sentinel_check">> := ClassStats}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(8000, maps:get(<<"min_duration_ns">>, ClassStats)),
                ?assertEqual(8000, maps:get(<<"max_duration_ns">>, ClassStats))
            end)
        ]
    end}.

%%====================================================================
%% Test: Slow methods sort by max duration
%%====================================================================

slow_methods_sort_by_max_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Method 'steady' has high avg but low max (consistent)
                beamtalk_trace_store:record_dispatch(TestPid, steady, 9000, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, steady, 9000, ok, sync, 'Counter'),

                %% Method 'spiky' has low avg but high max (outlier)
                beamtalk_trace_store:record_dispatch(TestPid, spiky, 1000, ok, sync, 'Counter'),
                beamtalk_trace_store:record_dispatch(TestPid, spiky, 20000, ok, sync, 'Counter'),

                %% By avg, 'steady' should be first (avg=9000 vs avg=10500)
                %% Actually spiky avg = (1000+20000)/2 = 10500, steady avg = 9000
                %% So by avg, spiky is first
                ByAvg = beamtalk_trace_store:slow_methods(2),
                ?assertEqual(spiky, maps:get(<<"selector">>, hd(ByAvg))),

                %% By max, 'spiky' should still be first (max=20000 vs max=9000)
                ByMax = beamtalk_trace_store:slow_methods(2, max_duration_ns),
                ?assertEqual(spiky, maps:get(<<"selector">>, hd(ByMax))),

                %% Verify the second entry is 'steady' when sorted by max
                [_, Second] = ByMax,
                ?assertEqual(steady, maps:get(<<"selector">>, Second))
            end)
        ]
    end}.

%%====================================================================
%% Test: export_traces
%%====================================================================

export_traces_no_filters_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {"export with no traces produces empty file",
                ?_test(begin
                    TmpFile = tmp_export_path("no_traces"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{path := TmpFile, count := 0}}, Result),
                    %% Verify file exists and contains valid JSON
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    ?assertEqual(
                        0, maps:get(<<"total_events">>, maps:get(<<"metadata">>, Decoded))
                    ),
                    ?assertEqual([], maps:get(<<"traces">>, Decoded)),
                    file:delete(TmpFile)
                end)},
            {"export captures recorded trace events",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 3000, ok, #{}, stop
                    ),
                    TmpFile = tmp_export_path("with_traces"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{count := 2}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    Meta = maps:get(<<"metadata">>, Decoded),
                    ?assertEqual(2, maps:get(<<"total_events">>, Meta)),
                    Traces = maps:get(<<"traces">>, Decoded),
                    ?assertEqual(2, length(Traces)),
                    file:delete(TmpFile)
                end)}
        ]
    end}.

export_traces_with_filters_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {"export with selector filter",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 3000, ok, #{}, stop
                    ),
                    TmpFile = tmp_export_path("filtered"),
                    Result = beamtalk_trace_store:export_traces(#{
                        path => TmpFile,
                        actor => TestPid,
                        selector => increment
                    }),
                    ?assertMatch({ok, #{count := 1}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    Traces = maps:get(<<"traces">>, Decoded),
                    ?assertEqual(1, length(Traces)),
                    [Trace] = Traces,
                    ?assertEqual(<<"increment">>, maps:get(<<"selector">>, Trace)),
                    file:delete(TmpFile)
                end)},
            {"export with limit",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    lists:foreach(
                        fun(I) ->
                            % elp:fixme W0023 intentional atom creation
                            Sel = list_to_atom("m_" ++ integer_to_list(I)),
                            beamtalk_trace_store:record_trace_event(
                                TestPid, 'Counter', Sel, sync, 1000, ok, #{}, stop
                            )
                        end,
                        lists:seq(1, 10)
                    ),
                    TmpFile = tmp_export_path("limited"),
                    Result = beamtalk_trace_store:export_traces(#{
                        path => TmpFile,
                        limit => 3
                    }),
                    ?assertMatch({ok, #{count := 3}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    ?assertEqual(3, length(maps:get(<<"traces">>, Decoded))),
                    file:delete(TmpFile)
                end)}
        ]
    end}.

export_traces_default_path_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {"export with default path generates timestamped file",
                ?_test(begin
                    Result = beamtalk_trace_store:export_traces(#{}),
                    ?assertMatch({ok, #{path := _, count := 0}}, Result),
                    {ok, #{path := Path}} = Result,
                    %% Verify the default path matches the expected pattern
                    ?assertMatch(<<"traces-", _/binary>>, Path),
                    ?assert(binary:match(Path, <<".json">>) =/= nomatch),
                    file:delete(Path)
                end)}
        ]
    end}.

%%====================================================================
%% Test: Opts map filters (class, outcome, min_duration_ns)
%%====================================================================

opts_map_class_filter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"filter by class returns only matching class",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'EventStore', put, sync, 3000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 2000, ok, #{}, stop
                    ),
                    All = beamtalk_trace_store:get_traces(),
                    ?assertEqual(3, length(All)),

                    CounterOnly = beamtalk_trace_store:get_traces(#{class => 'Counter'}),
                    ?assertEqual(2, length(CounterOnly)),
                    lists:foreach(
                        fun(T) -> ?assertEqual(<<"Counter">>, maps:get(<<"class">>, T)) end,
                        CounterOnly
                    ),

                    EventStoreOnly = beamtalk_trace_store:get_traces(#{class => 'EventStore'}),
                    ?assertEqual(1, length(EventStoreOnly)),
                    ?assertEqual(<<"EventStore">>, maps:get(<<"class">>, hd(EventStoreOnly)))
                end)}
        ]
    end}.

opts_map_outcome_filter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"filter by outcome returns only matching outcome",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 3000, error, #{error => badarg}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', reset, sync, 2000, ok, #{}, stop
                    ),
                    All = beamtalk_trace_store:get_traces(),
                    ?assertEqual(3, length(All)),

                    Errors = beamtalk_trace_store:get_traces(#{outcome => error}),
                    ?assertEqual(1, length(Errors)),
                    ?assertEqual(<<"decrement">>, maps:get(<<"selector">>, hd(Errors))),

                    Oks = beamtalk_trace_store:get_traces(#{outcome => ok}),
                    ?assertEqual(2, length(Oks))
                end)}
        ]
    end}.

opts_map_min_duration_filter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"filter by min_duration_ns returns only slow traces",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', fast_op, sync, 1000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', medium_op, sync, 5000000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', slow_op, sync, 10000000, ok, #{}, stop
                    ),
                    All = beamtalk_trace_store:get_traces(),
                    ?assertEqual(3, length(All)),

                    %% Only events >= 5ms (5_000_000 ns)
                    Slow = beamtalk_trace_store:get_traces(#{min_duration_ns => 5000000}),
                    ?assertEqual(2, length(Slow)),

                    %% Only events >= 10ms
                    VerySlow = beamtalk_trace_store:get_traces(#{min_duration_ns => 10000000}),
                    ?assertEqual(1, length(VerySlow)),
                    ?assertEqual(<<"slow_op">>, maps:get(<<"selector">>, hd(VerySlow)))
                end)}
        ]
    end}.

opts_map_combined_filters_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"combined class + outcome filter",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 3000, error, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'EventStore', put, sync, 4000, error, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'EventStore', get, sync, 2000, ok, #{}, stop
                    ),

                    %% Counter errors only
                    CounterErrors = beamtalk_trace_store:get_traces(#{
                        class => 'Counter', outcome => error
                    }),
                    ?assertEqual(1, length(CounterErrors)),
                    ?assertEqual(<<"decrement">>, maps:get(<<"selector">>, hd(CounterErrors))),

                    %% EventStore errors only
                    ESErrors = beamtalk_trace_store:get_traces(#{
                        class => 'EventStore', outcome => error
                    }),
                    ?assertEqual(1, length(ESErrors)),
                    ?assertEqual(<<"put">>, maps:get(<<"selector">>, hd(ESErrors)))
                end)},
            {"selector without actor filter works independently",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', decrement, sync, 3000, ok, #{}, stop
                    ),

                    %% Selector without actor should work
                    Result = beamtalk_trace_store:get_traces(#{selector => increment}),
                    ?assertEqual(1, length(Result)),
                    ?assertEqual(<<"increment">>, maps:get(<<"selector">>, hd(Result)))
                end)},
            {"all filters combined: class + outcome + min_duration_ns + selector",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 1000, ok, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 10000000, error, #{}, stop
                    ),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'EventStore', put, sync, 20000000, error, #{}, stop
                    ),

                    %% Counter + error + slow + increment
                    Result = beamtalk_trace_store:get_traces(#{
                        class => 'Counter',
                        outcome => error,
                        min_duration_ns => 5000000,
                        selector => increment
                    }),
                    ?assertEqual(1, length(Result)),
                    ?assertEqual(<<"Counter">>, maps:get(<<"class">>, hd(Result))),
                    ?assertEqual(<<"increment">>, maps:get(<<"selector">>, hd(Result)))
                end)},
            {"empty opts map returns all traces",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    All = beamtalk_trace_store:get_traces(#{}),
                    ?assertEqual(1, length(All))
                end)}
        ]
    end}.

%%====================================================================
%% Test: export_traces with atom/non-JSON metadata
%%====================================================================

export_traces_atom_metadata_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {"export succeeds with atom values in metadata",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    %% Simulate lifecycle stop event metadata: reason => normal (atom)
                    beamtalk_trace_store:record_trace_event(
                        TestPid,
                        'EventStore',
                        stop,
                        lifecycle,
                        0,
                        ok,
                        #{reason => normal},
                        stop
                    ),
                    TmpFile = tmp_export_path("atom_meta"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{count := 1}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    [Trace] = maps:get(<<"traces">>, Decoded),
                    %% Atom reason should be converted to binary string
                    ?assertEqual(<<"normal">>, maps:get(<<"reason">>, Trace)),
                    file:delete(TmpFile)
                end)},
            {"export succeeds with pid values in metadata",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid,
                        'EventStore',
                        stop,
                        lifecycle,
                        0,
                        ok,
                        #{caller => TestPid},
                        stop
                    ),
                    TmpFile = tmp_export_path("pid_meta"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{count := 1}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    [Trace] = maps:get(<<"traces">>, Decoded),
                    %% Pid should be converted to string
                    CallerBin = maps:get(<<"caller">>, Trace),
                    ?assert(is_binary(CallerBin)),
                    file:delete(TmpFile)
                end)},
            {"export succeeds with tuple values in metadata",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid,
                        'EventStore',
                        stop,
                        lifecycle,
                        0,
                        error,
                        #{reason => {shutdown, timeout}},
                        stop
                    ),
                    TmpFile = tmp_export_path("tuple_meta"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{count := 1}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    [Trace] = maps:get(<<"traces">>, Decoded),
                    %% Tuple should be converted to string representation
                    ReasonBin = maps:get(<<"reason">>, Trace),
                    ?assert(is_binary(ReasonBin)),
                    file:delete(TmpFile)
                end)},
            {"export succeeds with mixed metadata types",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid,
                        'EventStore',
                        stop,
                        lifecycle,
                        0,
                        ok,
                        #{
                            reason => normal,
                            count => 42,
                            name => <<"test">>,
                            flag => true
                        },
                        stop
                    ),
                    TmpFile = tmp_export_path("mixed_meta"),
                    Result = beamtalk_trace_store:export_traces(#{path => TmpFile}),
                    ?assertMatch({ok, #{count := 1}}, Result),
                    {ok, Bin} = file:read_file(TmpFile),
                    Decoded = json:decode(Bin),
                    [Trace] = maps:get(<<"traces">>, Decoded),
                    ?assertEqual(<<"normal">>, maps:get(<<"reason">>, Trace)),
                    ?assertEqual(42, maps:get(<<"count">>, Trace)),
                    ?assertEqual(<<"test">>, maps:get(<<"name">>, Trace)),
                    ?assertEqual(true, maps:get(<<"flag">>, Trace)),
                    file:delete(TmpFile)
                end)}
        ]
    end}.

%%====================================================================
%% Test: Empty store edge cases
%%====================================================================

empty_store_get_traces_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"get_traces on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:get_traces())
                end)},
            {"get_traces with filters on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:get_traces(#{class => 'Counter'})),
                    ?assertEqual([], beamtalk_trace_store:get_traces(#{outcome => error})),
                    ?assertEqual([], beamtalk_trace_store:get_traces(#{min_duration_ns => 1000}))
                end)},
            {"get_stats on empty store returns empty map",
                ?_test(begin
                    ?assertEqual(#{}, beamtalk_trace_store:get_stats())
                end)},
            {"slow_methods on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:slow_methods(10))
                end)},
            {"hot_methods on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:hot_methods(10))
                end)},
            {"error_methods on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:error_methods(10))
                end)},
            {"bottlenecks on empty store returns empty list",
                ?_test(begin
                    ?assertEqual([], beamtalk_trace_store:bottlenecks(10))
                end)}
        ]
    end}.

%%====================================================================
%% Test: Lifecycle event handler
%%====================================================================

lifecycle_event_handler_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"lifecycle start event records aggregate and trace",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    Metadata = #{pid => TestPid, class => 'Counter'},
                    beamtalk_trace_store:handle_lifecycle_event(
                        [beamtalk, actor, lifecycle, start],
                        #{},
                        Metadata,
                        #{}
                    ),
                    %% Aggregate should record the lifecycle action as selector
                    Stats = beamtalk_trace_store:get_stats(TestPid),
                    PidKey = list_to_binary(pid_to_list(TestPid)),
                    PidStats = maps:get(PidKey, Stats),
                    Methods = maps:get(<<"methods">>, PidStats),
                    ?assertMatch(#{<<"start">> := _}, Methods),
                    StartStats = maps:get(<<"start">>, Methods),
                    ?assertEqual(1, maps:get(<<"calls">>, StartStats)),
                    %% Trace event should be recorded with lifecycle mode
                    Traces = beamtalk_trace_store:get_traces(),
                    ?assertEqual(1, length(Traces)),
                    [Trace] = Traces,
                    ?assertEqual(<<"lifecycle">>, maps:get(<<"mode">>, Trace)),
                    ?assertEqual(<<"start">>, maps:get(<<"selector">>, Trace)),
                    ?assertEqual(<<"ok">>, maps:get(<<"outcome">>, Trace))
                end)},
            {"lifecycle stop with normal reason records ok outcome",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    Metadata = #{pid => TestPid, class => 'Counter', reason => normal},
                    beamtalk_trace_store:handle_lifecycle_event(
                        [beamtalk, actor, lifecycle, stop],
                        #{},
                        Metadata,
                        #{}
                    ),
                    Traces = beamtalk_trace_store:get_traces(),
                    ?assertEqual(1, length(Traces)),
                    ?assertEqual(<<"ok">>, maps:get(<<"outcome">>, hd(Traces)))
                end)},
            {"lifecycle stop with crash reason records error outcome",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    Metadata = #{pid => TestPid, class => 'Counter', reason => badarg},
                    beamtalk_trace_store:handle_lifecycle_event(
                        [beamtalk, actor, lifecycle, stop],
                        #{},
                        Metadata,
                        #{}
                    ),
                    Traces = beamtalk_trace_store:get_traces(),
                    ?assertEqual(1, length(Traces)),
                    ?assertEqual(<<"error">>, maps:get(<<"outcome">>, hd(Traces)))
                end)},
            {"lifecycle stop with shutdown tuple records ok outcome",
                ?_test(begin
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    Metadata = #{pid => TestPid, class => 'Counter', reason => {shutdown, timeout}},
                    beamtalk_trace_store:handle_lifecycle_event(
                        [beamtalk, actor, lifecycle, stop],
                        #{},
                        Metadata,
                        #{}
                    ),
                    Traces = beamtalk_trace_store:get_traces(),
                    ?assertEqual(1, length(Traces)),
                    ?assertEqual(<<"ok">>, maps:get(<<"outcome">>, hd(Traces)))
                end)}
        ]
    end}.

%%====================================================================
%% Test: Span ID generation
%%====================================================================

next_span_id_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"next_span_id returns monotonically increasing integers",
                ?_test(begin
                    Id1 = beamtalk_trace_store:next_span_id(),
                    Id2 = beamtalk_trace_store:next_span_id(),
                    Id3 = beamtalk_trace_store:next_span_id(),
                    ?assert(Id1 > 0),
                    ?assert(Id2 > Id1),
                    ?assert(Id3 > Id2)
                end)}
        ]
    end}.

%%====================================================================
%% Test: Timeout and cast outcome tracking in aggregates
%%====================================================================

timeout_and_cast_outcome_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"timeout outcome increments timeout counter",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, slow_method, 50000, timeout, sync, 'Counter'
                    ),
                    Stats = beamtalk_trace_store:get_stats(TestPid),
                    PidKey = list_to_binary(pid_to_list(TestPid)),
                    PidStats = maps:get(PidKey, Stats),
                    MethodStats = maps:get(<<"slow_method">>, maps:get(<<"methods">>, PidStats)),
                    ?assertEqual(1, maps:get(<<"calls">>, MethodStats)),
                    ?assertEqual(1, maps:get(<<"timeouts">>, MethodStats)),
                    ?assertEqual(0, maps:get(<<"ok">>, MethodStats)),
                    ?assertEqual(0, maps:get(<<"errors">>, MethodStats))
                end)},
            {"cast outcome increments ok counter",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    TestPid = self(),
                    beamtalk_trace_store:record_dispatch(
                        TestPid, async_method, 1000, cast, async, 'Counter'
                    ),
                    Stats = beamtalk_trace_store:get_stats(TestPid),
                    PidKey = list_to_binary(pid_to_list(TestPid)),
                    PidStats = maps:get(PidKey, Stats),
                    MethodStats = maps:get(<<"async_method">>, maps:get(<<"methods">>, PidStats)),
                    ?assertEqual(1, maps:get(<<"calls">>, MethodStats)),
                    ?assertEqual(1, maps:get(<<"ok">>, MethodStats)),
                    ?assertEqual(0, maps:get(<<"errors">>, MethodStats)),
                    ?assertEqual(0, maps:get(<<"timeouts">>, MethodStats))
                end)}
        ]
    end}.

%%====================================================================
%% Test: export_traces badarg
%%====================================================================

export_traces_badarg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"export_traces with non-map arg returns badarg error",
                ?_test(begin
                    ?assertEqual({error, badarg}, beamtalk_trace_store:export_traces(not_a_map))
                end)}
        ]
    end}.

%%====================================================================
%% Test: max_events configuration
%%====================================================================

max_events_config_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"max_events returns default initially",
                ?_test(begin
                    Max = beamtalk_trace_store:max_events(),
                    ?assert(is_integer(Max)),
                    ?assert(Max > 0)
                end)},
            {"max_events/1 updates the ring buffer capacity",
                ?_test(begin
                    OrigMax = beamtalk_trace_store:max_events(),
                    beamtalk_trace_store:max_events(500),
                    try
                        ?assertEqual(500, beamtalk_trace_store:max_events())
                    after
                        beamtalk_trace_store:max_events(OrigMax)
                    end
                end)}
        ]
    end}.

%%====================================================================
%% Test: Telemetry handler attachment check
%%====================================================================

telemetry_attached_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"telemetry_attached returns true when handlers are active",
                ?_test(begin
                    ?assert(beamtalk_trace_store:telemetry_attached())
                end)}
        ]
    end}.

%%====================================================================
%% Test: Tracing disabled skips trace event recording
%%====================================================================

tracing_disabled_skips_events_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"record_trace_event is a no-op when tracing is disabled",
                ?_test(begin
                    %% Ensure disabled
                    beamtalk_trace_store:disable(),
                    ?assertNot(beamtalk_trace_store:is_enabled()),
                    TestPid = self(),
                    beamtalk_trace_store:record_trace_event(
                        TestPid, 'Counter', increment, sync, 5000, ok, #{}, stop
                    ),
                    %% No traces should have been recorded
                    ?assertEqual([], beamtalk_trace_store:get_traces())
                end)},
            {"dispatch handler does not record trace events when disabled",
                ?_test(begin
                    beamtalk_trace_store:disable(),
                    TestPid = self(),
                    Measurements = #{duration => 5000},
                    Metadata = #{
                        pid => TestPid,
                        class => 'Counter',
                        selector => increment,
                        mode => sync,
                        outcome => ok
                    },
                    beamtalk_trace_store:handle_dispatch_stop(
                        [beamtalk, actor, dispatch, stop],
                        Measurements,
                        Metadata,
                        #{}
                    ),
                    %% Class aggregates should still be recorded, but no
                    %% per-actor entry and no trace events
                    ?assertEqual(#{}, beamtalk_trace_store:get_stats(TestPid)),
                    ?assertNotEqual(#{}, beamtalk_trace_store:get_stats()),
                    ?assertEqual([], beamtalk_trace_store:get_traces())
                end)}
        ]
    end}.

%%====================================================================
%% Test: Unknown request handling
%%====================================================================

unknown_request_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"unknown gen_server call returns error",
                ?_test(begin
                    Result = gen_server:call(beamtalk_trace_store, {completely_unknown, 42}),
                    ?assertEqual({error, unknown_request}, Result)
                end)}
        ]
    end}.

-doc "Fetch the `class:Class` entry of `get_stats/0` (the always-on aggregates).".
class_entry(ClassBin) ->
    maps:get(<<"class:", ClassBin/binary>>, beamtalk_trace_store:get_stats()).

-doc "Generate a unique temporary export path.".
tmp_export_path(Tag) ->
    iolist_to_binary([
        "beamtalk_test_export_",
        Tag,
        "_",
        integer_to_list(erlang:unique_integer([positive])),
        ".json"
    ]).

%%====================================================================
%% Test: Churn bounds without monitors (BT-4103)
%%====================================================================

churn_setup() ->
    setup(),
    {module, telemetry} = code:ensure_loaded(telemetry),
    case whereis(beamtalk_object_instances) of
        undefined -> {ok, _} = beamtalk_object_instances:start_link();
        _ -> ok
    end,
    beamtalk_trace_store:clear(),
    beamtalk_trace_store:disable(),
    ok.

-doc "Aggregate table sizes plus the current counters array size.".
agg_footprint() ->
    {_Ref, AtomicsSize, _Alloc} = persistent_term:get(beamtalk_trace_counters),
    #{
        class_stats => ets:info(beamtalk_agg_class_stats, size),
        pid => ets:info(beamtalk_agg_pid, size),
        pid_keys => ets:info(beamtalk_agg_pid_keys, size),
        free_slots => ets:info(beamtalk_agg_free_slots, size),
        atomics_size => AtomicsSize
    }.

-doc "Poll `Fun` until it answers true (or fail after ~5 s).".
wait_until(Fun) -> wait_until(Fun, 250).
wait_until(Fun, 0) ->
    ?assert(Fun());
wait_until(Fun, Tries) ->
    case Fun() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_until(Fun, Tries - 1)
    end.

-doc """
Start a real Counter actor, send it one message, then end it: a normal
`gen_server:stop` (runs `terminate/2`, emitting the lifecycle `stop` event)
or, when `Kill` is true, an untrapped `kill` (no `terminate/2`).
""".
churn_one_actor(Kill) ->
    {ok, Pid} = test_counter:start(0),
    %% Register like a compiled actor spawn does, so dispatch telemetry
    %% attributes to 'Counter' (the registry is unregistered on death).
    ok = beamtalk_object_instances:register('Counter', Pid),
    ?assertEqual(0, beamtalk_actor:sync_send(Pid, getValue, [])),
    MRef = erlang:monitor(process, Pid),
    case Kill of
        true -> exit(Pid, kill);
        false -> gen_server:stop(Pid)
    end,
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ?assert(false)
    end.

actor_churn_stays_flat_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {timeout, 120,
            ?_test(begin
                %% Tracing stays disabled: the always-on path only.
                ?assertNot(beamtalk_trace_store:is_enabled()),
                %% Warm up: the first dispatches create the bounded
                %% per-{Class, Selector} rows.
                lists:foreach(fun(I) -> churn_one_actor(I rem 2 =:= 0) end, lists:seq(1, 4)),
                Baseline = agg_footprint(),
                #{<<"methods">> := #{<<"getValue">> := #{<<"calls">> := WarmCalls}}} =
                    class_entry(<<"Counter">>),

                N = 1500,
                lists:foreach(fun(I) -> churn_one_actor(I rem 3 =:= 0) end, lists:seq(1, N)),

                %% No per-actor state exists, the class table only holds the
                %% handful of {Counter, Selector} rows, and the atomics array
                %% never grew: the footprint is identical to baseline.
                ?assertEqual(Baseline, agg_footprint()),
                ?assertEqual(0, maps:get(pid, agg_footprint())),
                ?assertEqual(0, maps:get(pid_keys, agg_footprint())),
                %% Every dispatch is still counted, in the class entry.
                #{<<"methods">> := #{<<"getValue">> := GetValue}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(WarmCalls + N, maps:get(<<"calls">>, GetValue)),
                ?assertEqual(WarmCalls + N, maps:get(<<"ok">>, GetValue)),

                %% The trace store holds no monitors and arms none on actors.
                StorePid = whereis(beamtalk_trace_store),
                ?assertEqual([], element(2, erlang:process_info(StorePid, monitors))),
                {ok, Live} = test_counter:start(0),
                ok = beamtalk_object_instances:register('Counter', Live),
                ?assertEqual(0, beamtalk_actor:sync_send(Live, getValue, [])),
                {monitored_by, MonitoredBy} = erlang:process_info(Live, monitored_by),
                ?assertNot(lists:member(StorePid, MonitoredBy)),
                gen_server:stop(Live)
            end)}
    end}.

million_actor_churn_stays_flat_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {timeout, 600,
            ?_test(begin
                %% 1M short-lived actors through the always-on path (disabled).
                %% Dead pids are fine: the write path never checks liveness,
                %% and while disabled nothing is keyed by pid at all.
                ?assertNot(beamtalk_trace_store:is_enabled()),
                %% Warm up with the loop's key so the baseline includes its
                %% row, then clear so the loop's own count is exact.
                beamtalk_trace_store:record_dispatch(
                    self(), churned, 1, ok, sync, 'Counter'
                ),
                beamtalk_trace_store:clear(),
                Baseline = agg_footprint(),
                N = 1000000,
                lists:foreach(
                    fun(_) ->
                        Pid = spawn(fun() -> ok end),
                        beamtalk_trace_store:record_dispatch(
                            Pid, churned, 1, ok, sync, 'Counter'
                        )
                    end,
                    lists:seq(1, N)
                ),
                ?assertEqual(Baseline, agg_footprint()),
                #{<<"methods">> := #{<<"churned">> := Churned}} =
                    class_entry(<<"Counter">>),
                ?assertEqual(N, maps:get(<<"calls">>, Churned))
            end)}
    end}.

per_actor_stats_need_tracing_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        [
            {"class totals survive actor death; per-pid rows are swept",
                ?_test(begin
                    beamtalk_trace_store:enable(),
                    Spawn = fun() ->
                        spawn(fun() ->
                            receive
                                stop -> ok
                            end
                        end)
                    end,
                    Live = Spawn(),
                    Dead1 = Spawn(),
                    Dead2 = Spawn(),
                    Record = fun(Pid, Duration, Outcome) ->
                        beamtalk_trace_store:record_dispatch(
                            Pid, increment, Duration, Outcome, sync, 'Counter'
                        )
                    end,
                    Record(Live, 1000, ok),
                    Record(Dead1, 2000, ok),
                    Record(Dead1, 4000, error),
                    Record(Dead2, 500, timeout),

                    %% While all three live, stats holds the class entry plus
                    %% one entry per pid.
                    Stats0 = beamtalk_trace_store:get_stats(),
                    LiveKey = list_to_binary(pid_to_list(Live)),
                    ?assertEqual(
                        4, maps:size(Stats0)
                    ),
                    #{<<"methods">> := #{<<"increment">> := LiveInc}} =
                        maps:get(LiveKey, Stats0),
                    ?assertEqual(1, maps:get(<<"calls">>, LiveInc)),
                    ?assertEqual(1000, maps:get(<<"total_duration_ns">>, LiveInc)),

                    Dead1 ! stop,
                    Dead2 ! stop,
                    %% No monitors: the sweep (1s timer) reclaims the dead
                    %% pids' rows once they are gone.
                    wait_until(fun() ->
                        beamtalk_trace_store:get_stats(Dead1) =:= #{} andalso
                            beamtalk_trace_store:get_stats(Dead2) =:= #{}
                    end),
                    ?assertEqual(1, ets:info(beamtalk_agg_pid_keys, size)),

                    %% The class entry kept every dispatch across the deaths.
                    #{<<"methods">> := #{<<"increment">> := Inc}} =
                        class_entry(<<"Counter">>),
                    ?assertEqual(4, maps:get(<<"calls">>, Inc)),
                    ?assertEqual(2, maps:get(<<"ok">>, Inc)),
                    ?assertEqual(1, maps:get(<<"errors">>, Inc)),
                    ?assertEqual(1, maps:get(<<"timeouts">>, Inc)),
                    ?assertEqual(7500, maps:get(<<"total_duration_ns">>, Inc)),
                    ?assertEqual(7500 div 4, maps:get(<<"avg_duration_ns">>, Inc)),
                    ?assertEqual(500, maps:get(<<"min_duration_ns">>, Inc)),
                    ?assertEqual(4000, maps:get(<<"max_duration_ns">>, Inc)),

                    %% The method queries see one class entry with no pid.
                    Hot = beamtalk_trace_store:hot_methods(10),
                    [CounterHot] = [
                        H
                     || H <- Hot, maps:get(<<"class">>, H) =:= <<"Counter">>
                    ],
                    ?assertEqual(4, maps:get(<<"calls">>, CounterHot)),
                    ?assertNot(maps:is_key(<<"pid">>, CounterHot)),
                    [TopError | _] = beamtalk_trace_store:error_methods(10),
                    ?assertNot(maps:is_key(<<"pid">>, TopError)),
                    [Slowest | _] = beamtalk_trace_store:slow_methods(10, max_duration_ns),
                    ?assertEqual(4000, maps:get(<<"max_duration_ns">>, Slowest)),

                    %% A later death changes nothing in the class entry but
                    %% removes the last pid row.
                    Record(Live, 100, ok),
                    Live ! stop,
                    wait_until(fun() -> beamtalk_trace_store:get_stats(Live) =:= #{} end),
                    #{<<"methods">> := #{<<"increment">> := Inc2}} =
                        class_entry(<<"Counter">>),
                    ?assertEqual(5, maps:get(<<"calls">>, Inc2)),
                    ?assertEqual(7600, maps:get(<<"total_duration_ns">>, Inc2)),
                    ?assertEqual(100, maps:get(<<"min_duration_ns">>, Inc2)),
                    ?assertEqual(0, ets:info(beamtalk_agg_pid, size)),
                    ?assertEqual(0, ets:info(beamtalk_agg_pid_keys, size))
                end)},
            {"disable drops the per-actor rows; the class entry stays",
                ?_test(begin
                    %% Cases share one setup: start clean.
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    Pid = idle_process(),
                    beamtalk_trace_store:record_dispatch(Pid, increment, 10, ok, sync, 'Counter'),
                    PidKey = list_to_binary(pid_to_list(Pid)),
                    ?assertMatch(
                        #{PidKey := _}, beamtalk_trace_store:get_stats(Pid)
                    ),
                    beamtalk_trace_store:disable(),
                    ?assertEqual(#{}, beamtalk_trace_store:get_stats(Pid)),
                    ?assertEqual(0, ets:info(beamtalk_agg_pid, size)),
                    ?assertEqual(0, ets:info(beamtalk_agg_pid_keys, size)),
                    #{<<"methods">> := #{<<"increment">> := Inc}} =
                        class_entry(<<"Counter">>),
                    ?assertEqual(1, maps:get(<<"calls">>, Inc)),
                    Pid ! stop
                end)},
            {"a stop event does not sweep a live actor; its exit does",
                ?_test(begin
                    %% Cases share one setup: start clean.
                    beamtalk_trace_store:clear(),
                    beamtalk_trace_store:enable(),
                    Self = self(),
                    Pid = spawn(fun() ->
                        beamtalk_trace_store:record_dispatch(
                            self(), increment, 10, ok, sync, 'Counter'
                        ),
                        beamtalk_trace_store:handle_lifecycle_event(
                            [beamtalk, actor, lifecycle, stop],
                            #{},
                            #{pid => self(), class => 'Counter', reason => badarg},
                            #{}
                        ),
                        Self ! stop_emitted,
                        receive
                            stop -> ok
                        end
                    end),
                    receive
                        stop_emitted -> ok
                    after 5000 -> ?assert(false)
                    end,
                    %% Still alive: its per-pid stats stay until it exits.
                    PidKey = list_to_binary(pid_to_list(Pid)),
                    #{PidKey := #{<<"methods">> := LiveMethods}} =
                        beamtalk_trace_store:get_stats(Pid),
                    ?assertMatch(#{<<"increment">> := #{<<"calls">> := 1}}, LiveMethods),
                    Pid ! stop,
                    wait_until(fun() -> beamtalk_trace_store:get_stats(Pid) =:= #{} end),
                    #{<<"methods">> := Methods} = class_entry(<<"Counter">>),
                    ?assertMatch(#{<<"calls">> := 1}, maps:get(<<"increment">>, Methods)),
                    %% The crash shows up as an error on the class `stop`.
                    ?assertMatch(
                        #{<<"calls">> := 1, <<"errors">> := 1}, maps:get(<<"stop">>, Methods)
                    )
                end)}
        ]
    end}.

non_pid_metadata_is_dropped_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {"a non-pid `pid` is dropped and cannot crash the trace store",
            ?_test(begin
                StorePid = whereis(beamtalk_trace_store),
                [
                    ?assertEqual(
                        ok,
                        beamtalk_trace_store:record_dispatch(Bad, increment, 1, ok, sync, 'Counter')
                    )
                 || Bad <- [not_a_pid, {some_name, node()}, 42]
                ],
                Live = idle_process(),
                ?assertEqual(
                    ok,
                    beamtalk_trace_store:record_dispatch(Live, increment, 1, ok, sync, "Counter")
                ),
                ?assertEqual(
                    ok,
                    beamtalk_trace_store:record_dispatch(Live, <<"inc">>, 1, ok, sync, 'Counter')
                ),
                Live ! stop,
                ?assertEqual(#{}, beamtalk_trace_store:get_stats()),
                ?assertEqual(StorePid, whereis(beamtalk_trace_store))
            end)}
    end}.

-doc "Spawn a process that waits for `stop`.".
idle_process() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

sweep_reclaims_dead_pid_rows_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {"rows a caller re-creates for a dead pid are swept again",
            ?_test(begin
                beamtalk_trace_store:enable(),
                Pid = idle_process(),
                beamtalk_trace_store:record_dispatch(Pid, first, 10, ok, sync, 'Late'),
                Pid ! stop,
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end),
                %% Callers report dispatches for the dead pid after it was
                %% swept, from two processes at once.
                Parent = self(),
                Writers = [
                    spawn(fun() ->
                        beamtalk_trace_store:record_dispatch(Pid, Sel, 10, ok, sync, 'Late'),
                        Parent ! {written, self()}
                    end)
                 || Sel <- [foo, bar]
                ],
                [
                    receive
                        {written, W} -> ok
                    end
                 || W <- Writers
                ],
                wait_until(fun() ->
                    ets:info(beamtalk_agg_pid, size) =:= 0 andalso
                        ets:info(beamtalk_agg_pid_keys, size) =:= 0
                end),
                #{<<"methods">> := Methods} = class_entry(<<"Late">>),
                ?assertMatch(#{<<"calls">> := 1}, maps:get(<<"first">>, Methods)),
                ?assertMatch(#{<<"calls">> := 1}, maps:get(<<"foo">>, Methods)),
                ?assertMatch(#{<<"calls">> := 1}, maps:get(<<"bar">>, Methods))
            end)}
    end}.

pid_cap_drops_and_recovers_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {timeout, 120,
            ?_test(begin
                beamtalk_trace_store:enable(),
                %% Fill the pid table to its cap with live minimal procs
                %% (~a few hundred MB transiently): live rows are never
                %% swept, so the at-cap window below is deterministic — no
                %% sweep tick can invalidate it.
                N = 100000,
                Fill = [
                    spawn(fun() ->
                        receive
                            _ -> ok
                        end
                    end)
                 || _ <- lists:seq(1, N)
                ],
                lists:foreach(
                    fun(P) ->
                        beamtalk_trace_store:record_dispatch(P, capfill, 1, ok, sync, 'Cap')
                    end,
                    Fill
                ),
                %% Single-threaded fill: exactly at cap, no overshoot.
                ?assertEqual(N, ets:info(beamtalk_agg_pid, size)),
                %% A fresh pid past the cap is not tracked ...
                Ghost = idle_process(),
                beamtalk_trace_store:record_dispatch(
                    Ghost, capped_method, 10, ok, sync, 'Cap'
                ),
                ?assertEqual([], ets:lookup(beamtalk_agg_pid, {Ghost, capped_method})),
                ?assertEqual(#{}, beamtalk_trace_store:get_stats(Ghost)),
                %% ... but its class aggregates still record.
                #{<<"methods">> := #{<<"capped_method">> := Capped}} =
                    class_entry(<<"Cap">>),
                ?assertEqual(1, maps:get(<<"calls">>, Capped)),
                %% Once the sweep reclaims dead rows there is room again: a
                %% new pid is tracked.
                Ghost ! stop,
                lists:foreach(fun(P) -> exit(P, kill) end, Fill),
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end),
                Live = idle_process(),
                beamtalk_trace_store:record_dispatch(Live, capped_method, 10, ok, sync, 'Cap'),
                ?assertMatch([_], ets:lookup(beamtalk_agg_pid, {Live, capped_method})),
                Live ! stop
            end)}
    end}.

concurrent_alloc_during_grow_and_clear_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {timeout, 60,
            ?_test(begin
                beamtalk_trace_store:enable(),
                %% 700 live actors x 2 keys = 1400 pid keys: past the initial
                %% 1000, so the array grows under concurrent allocation.
                N = 700,
                Parent = self(),
                Pids = [idle_process() || _ <- lists:seq(1, N)],
                Writers = [
                    spawn(fun() ->
                        beamtalk_trace_store:record_dispatch(P, alpha, 10, ok, sync, 'Grow'),
                        beamtalk_trace_store:record_dispatch(P, beta, 30, error, sync, 'Grow'),
                        Parent ! {done, self()}
                    end)
                 || P <- Pids
                ],
                [
                    receive
                        {done, W} -> ok
                    end
                 || W <- Writers
                ],
                %% A burst of 1200 more keys on one pid while the others die
                %% and the sweep reclaims them: allocation, grow and sweep
                %% run concurrently.
                [P ! stop || P <- Pids],
                Burst = idle_process(),
                [
                    % elp:fixme W0023 intentional atom creation
                    beamtalk_trace_store:record_dispatch(
                        Burst, list_to_atom("burst_" ++ integer_to_list(I)), 1, ok, sync, 'Grow'
                    )
                 || I <- lists:seq(1, 1200)
                ],
                Burst ! stop,
                wait_until(fun() ->
                    ets:info(beamtalk_agg_pid, size) =:= 0 andalso
                        ets:info(beamtalk_agg_pid_keys, size) =:= 0
                end),
                %% The class entry holds every dispatch exactly once.
                #{<<"methods">> := #{<<"alpha">> := Alpha, <<"beta">> := Beta}} =
                    class_entry(<<"Grow">>),
                ?assertMatch(#{<<"calls">> := N, <<"ok">> := N}, Alpha),
                ?assertEqual(10 * N, maps:get(<<"total_duration_ns">>, Alpha)),
                ?assertMatch(#{<<"calls">> := N, <<"errors">> := N}, Beta),
                ?assertEqual(30 * N, maps:get(<<"total_duration_ns">>, Beta)),

                %% Every slot is accounted for exactly once: on the free list
                %% or held by a class row, never both. Slots are {Ref, Base}
                %% pairs: rows keep their birth array across grows.
                Free = [{R, B} || {{_, R, B}} <- ets:tab2list(beamtalk_agg_free_slots)],
                Held = [{R, B} || {_, R, B} <- ets:tab2list(beamtalk_agg_class_stats)],
                ?assertEqual([], [S || S <- Held, lists:member(S, Free)]),
                ?assertEqual(length(Held), length(lists:usort(Held))),

                %% After clear, new keys get distinct, zeroed slots.
                beamtalk_trace_store:clear(),
                Live = idle_process(),
                [
                    beamtalk_trace_store:record_dispatch(Live, Sel, 5, ok, sync, 'Grow')
                 || Sel <- [one, two, three]
                ],
                Bases = [
                    B
                 || {{P, _}, _, B, _} <- ets:tab2list(beamtalk_agg_pid), P =:= Live
                ],
                ?assertEqual(3, length(lists:usort(Bases))),
                #{<<"methods">> := LiveMethods} =
                    maps:get(
                        list_to_binary(pid_to_list(Live)), beamtalk_trace_store:get_stats(Live)
                    ),
                [
                    ?assertMatch(
                        #{<<"calls">> := 1, <<"total_duration_ns">> := 5}, maps:get(S, LiveMethods)
                    )
                 || S <- [<<"one">>, <<"two">>, <<"three">>]
                ],
                Live ! stop
            end)}
    end}.

upgrade_resets_aggregates_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {"pre-BT-4103 tables are reset by code_change; trace events survive",
            ?_test(begin
                %% Seed current-version data plus one trace event.
                beamtalk_trace_store:enable(),
                beamtalk_trace_store:record_dispatch(self(), ping, 1, ok, sync, 'Counter'),
                beamtalk_trace_store:record_trace_event(
                    self(), 'Counter', ping, sync, 1, ok, #{}, stop
                ),
                ?assertNotEqual(#{}, beamtalk_trace_store:get_stats()),
                ?assertEqual(1, length(beamtalk_trace_store:get_traces())),

                %% Simulate the old layout's tables surviving a code upgrade.
                ets:new(beamtalk_agg_index, [named_table, public, set]),
                ets:new(beamtalk_agg_class, [named_table, public, set]),
                ets:new(beamtalk_agg_retired, [named_table, public, set]),

                %% Run the release-upgrade path on the live process: no
                %% restart, so a supervising tree cannot interfere (a restart
                %% races the supervisor's own restart of the store). The
                %% resume is unconditional — a suspended store would hang
                %% every later caller.
                StorePid = whereis(beamtalk_trace_store),
                ok = sys:suspend(StorePid),
                try
                    ?assertEqual(ok, sys:change_code(StorePid, beamtalk_trace_store, "2", []))
                after
                    ok = sys:resume(StorePid)
                end,

                %% Old tables are gone, the aggregates are empty and
                %% functional, and the trace event survived (its table is
                %% never touched by the reset).
                ?assertEqual(undefined, ets:whereis(beamtalk_agg_index)),
                ?assertEqual(undefined, ets:whereis(beamtalk_agg_class)),
                ?assertEqual(undefined, ets:whereis(beamtalk_agg_retired)),
                ?assertEqual(#{}, beamtalk_trace_store:get_stats()),
                ?assertEqual(0, ets:info(beamtalk_agg_class_stats, size)),
                ?assertEqual(0, ets:info(beamtalk_agg_pid, size)),
                ?assertEqual(1, length(beamtalk_trace_store:get_traces())),
                beamtalk_trace_store:record_dispatch(self(), pong, 1, ok, sync, 'Counter'),
                #{<<"methods">> := #{<<"pong">> := Pong}} = class_entry(<<"Counter">>),
                ?assertEqual(1, maps:get(<<"calls">>, Pong))
            end)}
    end}.

%%====================================================================
%% Test: Free-list contention under concurrent churn
%%====================================================================

-doc """
One churn window: `Workers` processes race a start barrier, then each
records `DispatchesPerPid` dispatches for each of `PerWorker` fresh pids.
Every first dispatch allocates a pid row — through `take_slot/0` when the
free list holds released slots, else the bump allocator — while the shared
{Class, Selector} keeps every dispatch on the class fast path. When
`Trace` is true the workers' `ets:take/2` calls are call-traced and
counted. Answers `{ElapsedUs, Pids, Takes}`.
""".
churn_window(Workers, PerWorker, Class, Selector, DispatchesPerPid, Trace) ->
    Parent = self(),
    Collector =
        case Trace of
            true -> spawn_link(fun() -> take_counter_loop(0) end);
            false -> undefined
        end,
    case Trace of
        true -> 1 = erlang:trace_pattern({ets, take, 2}, [{'_', [], []}], [global]);
        false -> ok
    end,
    try
        do_churn_window(
            Parent, Collector, Workers, PerWorker, Class, Selector, DispatchesPerPid, Trace
        )
    after
        case Trace of
            true -> _ = erlang:trace_pattern({ets, take, 2}, false, [global]);
            false -> ok
        end,
        case Collector of
            undefined -> ok;
            _ -> Collector ! stop
        end
    end.

do_churn_window(Parent, Collector, Workers, PerWorker, Class, Selector, DispatchesPerPid, Trace) ->
    Work = fun() ->
        case Trace of
            true -> 1 = erlang:trace(self(), true, [call, {tracer, Collector}]);
            false -> ok
        end,
        receive
            go -> ok
        after 60000 -> exit(churn_barrier_timeout)
        end,
        Ps = [
            begin
                P = idle_process(),
                [
                    beamtalk_trace_store:record_dispatch(P, Selector, 10, ok, sync, Class)
                 || _ <- lists:seq(1, DispatchesPerPid)
                ],
                P
            end
         || _ <- lists:seq(1, PerWorker)
        ],
        Parent ! {churn_done, self(), Ps},
        case Trace of
            %% Stay alive until the parent lifts the trace flags: tracing
            %% a dead pid raises.
            true ->
                receive
                    release -> ok
                after 60000 -> exit(churn_release_timeout)
                end;
            false ->
                ok
        end
    end,
    Wins = [spawn_link(Work) || _ <- lists:seq(1, Workers)],
    T0 = erlang:monotonic_time(microsecond),
    [W ! go || W <- Wins],
    AllPids = collect_churn_pids(Workers),
    T1 = erlang:monotonic_time(microsecond),
    Takes =
        case Trace of
            true ->
                [1 = erlang:trace(W, false, [call]) || W <- Wins],
                Collector ! {get, self()},
                Taken =
                    receive
                        {takes, N} -> N
                    after 5000 -> exit(churn_count_timeout)
                    end,
                [W ! release || W <- Wins],
                Taken;
            false ->
                0
        end,
    {T1 - T0, AllPids, Takes}.

-doc "Collect one `{churn_done, Pid, Pids}` per worker.".
collect_churn_pids(0) ->
    [];
collect_churn_pids(N) ->
    receive
        {churn_done, _W, Ps} -> Ps ++ collect_churn_pids(N - 1)
    after 120000 -> exit(churn_collect_timeout)
    end.

-doc "Count call-traced `ets:take/2` calls on the free-slots table.".
take_counter_loop(N) ->
    receive
        {trace, _Pid, call, {ets, take, [beamtalk_agg_free_slots, _Key]}} ->
            take_counter_loop(N + 1);
        {trace, _Pid, call, _MFA} ->
            take_counter_loop(N);
        {get, From} ->
            From ! {takes, N},
            take_counter_loop(N);
        stop ->
            ok
    end.

free_list_churn_stays_flat_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {timeout, 300,
            ?_test(begin
                beamtalk_trace_store:enable(),
                %% The free list is a pure reuse cache: drain it so the
                %% sizes below start from zero rather than earlier tests'
                %% leftovers.
                ets:delete_all_objects(beamtalk_agg_free_slots),
                Workers = 32,
                PerWorker = 500,
                Allocs = Workers * PerWorker,
                Class = 'ChurnFree',
                Selector = churned,
                %% Window 1: the bump path (the free list starts empty).
                %% Two dispatches per pid, so a reused slot still holding
                %% its previous owner's counts is detectable below.
                {T1, Win1, _} = churn_window(Workers, PerWorker, Class, Selector, 2, false),
                ?assertEqual(Allocs, ets:info(beamtalk_agg_pid, size)),
                [exit(P, kill) || P <- Win1],
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end),
                ?assertEqual(Allocs, ets:info(beamtalk_agg_free_slots, size)),
                %% Releases stripe across all shards: a fixed-shard
                %% regression would pile every release onto one.
                Shards = lists:usort([
                    S
                 || {{S, _, _}} <- ets:tab2list(beamtalk_agg_free_slots)
                ]),
                ?assertEqual(64, length(Shards)),

                %% Window 2: the same shape, now through take_slot/0 — nearly
                %% every allocation reuses a released slot. (Exact drain is
                %% racy by design: a take that loses every probe to faster
                %% racers bumps a fresh slot instead of spinning, leaving a
                %% straggler or two for the next window.)
                {T2, Win2, _} = churn_window(Workers, PerWorker, Class, Selector, 1, false),
                ?assertEqual(Allocs, ets:info(beamtalk_agg_pid, size)),
                FreeW2 = ets:info(beamtalk_agg_free_slots, size),
                ?assert(FreeW2 =< Allocs div 100),
                %% Flat: the aged node churns at the fresh node's rate.
                ?debugFmt("BT-4120 windows: fresh=~pus aged=~pus", [T1, T2]),
                ?assert(T2 =< T1 * 6),

                %% Every new row holds a distinct slot ...
                Slots = [{R, B} || {_, R, B, _} <- ets:tab2list(beamtalk_agg_pid)],
                ?assertEqual(Allocs, length(lists:usort(Slots))),
                %% ... zeroed before reuse: one dispatch reads as one call,
                %% not one plus the previous owner's two.
                [Sample | _] = Win2,
                SampleKey = list_to_binary(pid_to_list(Sample)),
                #{SampleKey := #{<<"methods">> := #{<<"churned">> := SampleStats}}} =
                    beamtalk_trace_store:get_stats(Sample),
                ?assertEqual(1, maps:get(<<"calls">>, SampleStats)),
                %% ... and the class entry holds every dispatch exactly once.
                #{<<"methods">> := #{<<"churned">> := Churned}} =
                    class_entry(<<"ChurnFree">>),
                ?assertEqual(3 * Allocs, maps:get(<<"calls">>, Churned)),

                %% Window 3 (traced): ~1 free-list take per new actor.
                [exit(P, kill) || P <- Win2],
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end),
                %% The sweep releases exactly the window-2 rows on top of
                %% window 2's stragglers.
                Pool3 = ets:info(beamtalk_agg_free_slots, size),
                ?assertEqual(FreeW2 + Allocs, Pool3),
                {_T3, Win3, Takes} = churn_window(Workers, PerWorker, Class, Selector, 1, true),
                ?assertEqual(Allocs, ets:info(beamtalk_agg_pid, size)),
                FreeW3 = ets:info(beamtalk_agg_free_slots, size),
                ?assert(Pool3 - FreeW3 >= Allocs * 99 div 100),
                ?debugFmt("BT-4120 takes: ~p takes for ~p allocs", [Takes, Allocs]),
                ?assert(Takes > 0),
                ?assert(Takes =< Allocs * 3 div 2),
                [exit(P, kill) || P <- Win3],
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end)
            end)}
    end}.

free_list_release_reuse_roundtrip_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {"released slots are reused with identical slot identity, zeroed",
            ?_test(begin
                beamtalk_trace_store:enable(),
                ets:delete_all_objects(beamtalk_agg_free_slots),
                Class = 'ReuseProbe',
                Selector = probed,
                Pids = [idle_process() || _ <- lists:seq(1, 3)],
                [
                    beamtalk_trace_store:record_dispatch(P, Selector, 10, ok, sync, Class)
                 || P <- Pids
                ],
                FirstSlots = lists:sort([
                    {R, B}
                 || {{P, _}, R, B, _} <- ets:tab2list(beamtalk_agg_pid), lists:member(P, Pids)
                ]),
                ?assertEqual(3, length(FirstSlots)),
                [exit(P, kill) || P <- Pids],
                wait_until(fun() -> ets:info(beamtalk_agg_pid, size) =:= 0 end),
                Freed = lists:sort([
                    {R, B}
                 || {{_, R, B}} <- ets:tab2list(beamtalk_agg_free_slots)
                ]),
                ?assertEqual(FirstSlots, Freed),
                %% Fresh pids reuse exactly those slots.
                Pids2 = [idle_process() || _ <- lists:seq(1, 3)],
                [
                    beamtalk_trace_store:record_dispatch(P, Selector, 10, ok, sync, Class)
                 || P <- Pids2
                ],
                SecondSlots = lists:sort([
                    {R, B}
                 || {{P, _}, R, B, _} <- ets:tab2list(beamtalk_agg_pid), lists:member(P, Pids2)
                ]),
                ?assertEqual(FirstSlots, SecondSlots),
                ?assertEqual(0, ets:info(beamtalk_agg_free_slots, size)),
                %% Zeroed: one dispatch reads as exactly one call.
                [
                    begin
                        K = list_to_binary(pid_to_list(P)),
                        #{K := #{<<"methods">> := #{<<"probed">> := S}}} =
                            beamtalk_trace_store:get_stats(P),
                        ?assertEqual(1, maps:get(<<"calls">>, S))
                    end
                 || P <- Pids2
                ],
                [P ! stop || P <- Pids2]
            end)}
    end}.

free_slots_upgrade_recreates_unsharded_table_test_() ->
    {setup, fun churn_setup/0, fun cleanup/1, fun(_) ->
        {"a pre-BT-4120 `set` free list is dropped and recreated sharded",
            ?_test(begin
                StorePid = whereis(beamtalk_trace_store),
                ok = sys:suspend(StorePid),
                try
                    %% Simulate the pre-sharding layout: any process may
                    %% delete a named table.
                    true = ets:delete(beamtalk_agg_free_slots),
                    _ = ets:new(beamtalk_agg_free_slots, [named_table, public, set]),
                    Ref = atomics:new(8, [{signed, true}]),
                    true = ets:insert(beamtalk_agg_free_slots, {{Ref, 1}}),
                    ?assertEqual(ok, sys:change_code(StorePid, beamtalk_trace_store, "2", []))
                after
                    ok = sys:resume(StorePid)
                end,
                ?assertEqual(ordered_set, ets:info(beamtalk_agg_free_slots, type)),
                ?assertEqual(0, ets:info(beamtalk_agg_free_slots, size)),
                %% The store is functional and tracks new pid rows.
                beamtalk_trace_store:enable(),
                P = idle_process(),
                beamtalk_trace_store:record_dispatch(P, ping, 1, ok, sync, 'Counter'),
                ?assertMatch([_], ets:lookup(beamtalk_agg_pid, {P, ping})),
                P ! stop
            end)}
    end}.
