%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_trace_store (ADR 0069 Phase 1).
%%%
%%% Tests cover: direct insert, query, ring buffer eviction, counters aggregates,
%%% clear, composite key uniqueness, gen_server crash recovery (heir),
%%% telemetry handler callbacks, and telemetry_poller VM stats.
-module(beamtalk_trace_store_tests).
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
                    TestPid, increment, 5000, ok, sync
                ),

                %% Check stats
                Stats = beamtalk_trace_store:get_stats(),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                ?assertMatch(#{PidKey := _}, Stats),
                PidStats = maps:get(PidKey, Stats),
                Methods = maps:get(<<"methods">>, PidStats),
                IncStats = maps:get(<<"increment">>, Methods),
                ?assertEqual(1, maps:get(<<"calls">>, IncStats)),
                ?assertEqual(1, maps:get(<<"ok">>, IncStats)),
                ?assertEqual(0, maps:get(<<"errors">>, IncStats)),
                ?assertEqual(5000, maps:get(<<"total_duration_ns">>, IncStats))
            end)
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
                %% Set a small max for testing
                beamtalk_trace_store:max_events(50),
                ?assertEqual(50, beamtalk_trace_store:max_events()),

                %% Enable tracing and insert more than max
                beamtalk_trace_store:enable(),
                TestPid = self(),
                lists:foreach(
                    fun(I) ->
                        beamtalk_trace_store:record_trace_event(
                            TestPid, 'Counter', increment, sync, I * 100, ok, #{}, stop
                        )
                    end,
                    lists:seq(1, 60)
                ),

                %% Should have 60 events before sweep
                PreSweep = beamtalk_trace_store:get_traces(),
                ?assertEqual(60, length(PreSweep)),

                %% Trigger sweep by sending the message directly
                beamtalk_trace_store ! sweep_eviction,
                timer:sleep(50),

                %% After sweep, oldest 10% (6 events) should be deleted
                PostSweep = beamtalk_trace_store:get_traces(),
                ?assertEqual(54, length(PostSweep)),

                %% Reset max_events to default
                beamtalk_trace_store:max_events(100000)
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
                TestPid = self(),

                %% Record multiple dispatches
                beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, increment, 2000, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, increment, 3000, error, sync),

                %% Check aggregated stats
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
                ?assertEqual(3000, maps:get(<<"max_duration_ns">>, IncStats))
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
                beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync),
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
                beamtalk_trace_store:record_dispatch(TestPid, fast, 100, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, slow, 10000, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, medium, 5000, ok, sync),

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
                beamtalk_trace_store:record_dispatch(TestPid, rare, 100, ok, sync),
                lists:foreach(
                    fun(_) ->
                        beamtalk_trace_store:record_dispatch(TestPid, popular, 100, ok, sync)
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
                beamtalk_trace_store:record_dispatch(TestPid, reliable, 100, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, reliable, 100, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, flaky, 100, error, sync),
                beamtalk_trace_store:record_dispatch(TestPid, flaky, 100, timeout, sync),

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
                %% Create a process and record dispatches for it
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

                beamtalk_trace_store:record_dispatch(Pid, test_method, 100, ok, sync),

                Result = beamtalk_trace_store:bottlenecks(10),
                ?assert(is_list(Result)),
                %% Our spawned process should be in the list
                PidBin = list_to_binary(pid_to_list(Pid)),
                Pids = [maps:get(<<"actor">>, R) || R <- Result],
                ?assert(lists:member(PidBin, Pids)),

                Pid ! stop
            end)
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

                %% Verify aggregate was recorded
                Stats = beamtalk_trace_store:get_stats(TestPid),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                ?assertMatch(#{PidKey := _}, Stats)
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

                %% Verify error was recorded
                Stats = beamtalk_trace_store:get_stats(TestPid),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats = maps:get(PidKey, Stats),
                MethodStats = maps:get(<<"badmethod">>, maps:get(<<"methods">>, PidStats)),
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
%% Test: Gen_server crash recovery (heir)
%%====================================================================

%% Test crash recovery by gracefully stopping and restarting, verifying that
%% persistent_term data (counters, enabled flag) survives the restart and
%% ETS tables are recreated. This tests the same recovery path as a crash
%% without the EUnit process-link complications of kill signals.
gen_server_crash_recovery_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_State) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Insert data via persistent_term counters
                beamtalk_trace_store:record_dispatch(TestPid, increment, 1000, ok, sync),
                beamtalk_trace_store:enable(),

                %% Verify data exists
                StatsBefore = beamtalk_trace_store:get_stats(),
                ?assertNotEqual(#{}, StatsBefore),
                ?assert(beamtalk_trace_store:is_enabled()),

                %% Gracefully stop the gen_server (simulates restart after crash)
                gen_server:stop(beamtalk_trace_store),
                timer:sleep(50),

                %% Verify persistent_term data survives (this is the key crash
                %% recovery property — counters and enabled flag persist)
                ?assert(beamtalk_trace_store:is_enabled()),

                %% Restart
                case beamtalk_trace_store:start_link() of
                    {ok, _NewPid} -> ok;
                    {error, {already_started, _}} -> ok
                end,

                %% Store should be functional with inherited persistent_terms
                beamtalk_trace_store:record_dispatch(TestPid, after_restart, 500, ok, sync),
                StatsAfterRestart = beamtalk_trace_store:get_stats(),
                ?assertNotEqual(#{}, StatsAfterRestart)
            end)
        ]
    end}.

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
%% Test: Wall-clock timestamp in trace events (BT-1620)
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
%% Test: Serialized counter grow (BT-1621)
%%====================================================================

serialized_counter_grow_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                %% Fill up counter slots to trigger a grow.
                %% Initial size is 8000 slots, 8 per key = 1000 keys before grow.
                %% Record enough unique {Pid, Selector} keys to force a grow.
                TestPid = self(),
                lists:foreach(
                    fun(I) ->
                        Sel = list_to_atom("method_" ++ integer_to_list(I)),
                        beamtalk_trace_store:record_dispatch(TestPid, Sel, 100, ok, sync)
                    end,
                    lists:seq(1, 1010)
                ),
                %% Verify the atomics grew — stats should have all 1010 methods
                Stats = beamtalk_trace_store:get_stats(),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats = maps:get(PidKey, Stats),
                Methods = maps:get(<<"methods">>, PidStats),
                ?assert(maps:size(Methods) >= 1010)
            end)
        ]
    end}.

%%====================================================================
%% Test: Min/max duration tracking (BT-1628)
%%====================================================================

min_max_duration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Single call — min and max should be the same
                beamtalk_trace_store:record_dispatch(TestPid, ping, 5000, ok, sync),
                Stats1 = beamtalk_trace_store:get_stats(TestPid),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats1 = maps:get(PidKey, Stats1),
                PingStats1 = maps:get(<<"ping">>, maps:get(<<"methods">>, PidStats1)),
                ?assertEqual(5000, maps:get(<<"min_duration_ns">>, PingStats1)),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, PingStats1)),

                %% Add a faster call — min should decrease, max stays
                beamtalk_trace_store:record_dispatch(TestPid, ping, 1000, ok, sync),
                Stats2 = beamtalk_trace_store:get_stats(TestPid),
                PidStats2 = maps:get(PidKey, Stats2),
                PingStats2 = maps:get(<<"ping">>, maps:get(<<"methods">>, PidStats2)),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, PingStats2)),
                ?assertEqual(5000, maps:get(<<"max_duration_ns">>, PingStats2)),

                %% Add a slower call — max should increase, min stays
                beamtalk_trace_store:record_dispatch(TestPid, ping, 11000, ok, sync),
                Stats3 = beamtalk_trace_store:get_stats(TestPid),
                PidStats3 = maps:get(PidKey, Stats3),
                PingStats3 = maps:get(<<"ping">>, maps:get(<<"methods">>, PidStats3)),
                ?assertEqual(1000, maps:get(<<"min_duration_ns">>, PingStats3)),
                ?assertEqual(11000, maps:get(<<"max_duration_ns">>, PingStats3))
            end)
        ]
    end}.

%%====================================================================
%% Test: Min/max CAS correctness under concurrent updates (BT-1628)
%%====================================================================

min_max_concurrent_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            {timeout, 30,
                ?_test(begin
                    TestPid = self(),

                    %% Pre-allocate the slot with an initial dispatch to avoid the
                    %% first-observation allocation race between concurrent writers.
                    beamtalk_trace_store:record_dispatch(
                        TestPid, concurrent_method, 5000, ok, sync
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
                                        TestPid, concurrent_method, Duration, ok, sync
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
%% Test: Clear resets min sentinel (BT-1628)
%%====================================================================

clear_resets_min_sentinel_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Record a dispatch to establish min/max
                beamtalk_trace_store:record_dispatch(TestPid, sentinel_check, 5000, ok, sync),
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

                %% Record a new dispatch — min should be set fresh, not stuck
                beamtalk_trace_store:record_dispatch(TestPid, sentinel_check, 8000, ok, sync),
                Stats2 = beamtalk_trace_store:get_stats(TestPid),
                PidStats2 = maps:get(PidKey, Stats2),
                MethodStats2 = maps:get(
                    <<"sentinel_check">>,
                    maps:get(<<"methods">>, PidStats2)
                ),
                ?assertEqual(8000, maps:get(<<"min_duration_ns">>, MethodStats2)),
                ?assertEqual(8000, maps:get(<<"max_duration_ns">>, MethodStats2))
            end)
        ]
    end}.

%%====================================================================
%% Test: Slow methods sort by max duration (BT-1628)
%%====================================================================

slow_methods_sort_by_max_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_test(begin
                TestPid = self(),

                %% Method 'steady' has high avg but low max (consistent)
                beamtalk_trace_store:record_dispatch(TestPid, steady, 9000, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, steady, 9000, ok, sync),

                %% Method 'spiky' has low avg but high max (outlier)
                beamtalk_trace_store:record_dispatch(TestPid, spiky, 1000, ok, sync),
                beamtalk_trace_store:record_dispatch(TestPid, spiky, 20000, ok, sync),

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
