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
                ?assertEqual(2000, maps:get(<<"avg_duration_ns">>, IncStats))
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
                %% Initial size is 6000 slots, 6 per key = 1000 keys before grow.
                %% Record enough unique {Pid, Selector} keys to force a grow.
                TestPid = self(),
                lists:foreach(
                    fun(I) ->
                        Sel = list_to_atom("method_" ++ integer_to_list(I)),
                        beamtalk_trace_store:record_dispatch(TestPid, Sel, 100, ok, sync)
                    end,
                    lists:seq(1, 1010)
                ),
                %% Verify the counters grew — stats should have all 1010 methods
                Stats = beamtalk_trace_store:get_stats(),
                PidKey = list_to_binary(pid_to_list(TestPid)),
                PidStats = maps:get(PidKey, Stats),
                Methods = maps:get(<<"methods">>, PidStats),
                ?assert(maps:size(Methods) >= 1010)
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
                    Decoded = jsx:decode(Bin, [return_maps]),
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
                    Decoded = jsx:decode(Bin, [return_maps]),
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
                    Decoded = jsx:decode(Bin, [return_maps]),
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
                    Decoded = jsx:decode(Bin, [return_maps]),
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

%% @private Generate a unique temporary export path.
tmp_export_path(Tag) ->
    iolist_to_binary([
        "beamtalk_test_export_",
        Tag,
        "_",
        integer_to_list(erlang:unique_integer([positive])),
        ".json"
    ]).
