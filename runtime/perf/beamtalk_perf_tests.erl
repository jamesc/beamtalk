%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Performance test suite for actor call latency (BT-202)
%%%
%%% Benchmarks key actor system operations to track regressions and
%%% document expected performance characteristics.
%%%
%%% Run with: just perf
%%%
%%% Tests output parseable results in the format:
%%%   PERF: <benchmark_name> <median_us>us (mean: <mean_us>us, ...)

-module(beamtalk_perf_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Number of iterations per benchmark (more = more stable results)
-define(ITERATIONS, 1000).
%% Number of warmup iterations (discarded)
-define(WARMUP, 100).

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_bootstrap) of
        undefined ->
            case beamtalk_bootstrap:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok
            end;
        _ ->
            ok
    end,
    wait_for_actor_class(100),
    ok.

wait_for_actor_class(0) ->
    error(actor_class_not_registered_after_timeout);
wait_for_actor_class(N) ->
    case beamtalk_class_registry:whereis_class('Actor') of
        undefined ->
            timer:sleep(50),
            wait_for_actor_class(N - 1);
        _Pid ->
            ok
    end.

%%====================================================================
%% Benchmark Helpers
%%====================================================================

%% @doc Run a benchmark function N times, return list of microsecond timings.
-spec run_benchmark(fun(() -> term()), pos_integer(), non_neg_integer()) -> [non_neg_integer()].
run_benchmark(Fun, Iterations, Warmup) ->
    %% Warmup phase (discard results)
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, Warmup)),
    %% Measurement phase
    lists:map(fun(_) ->
        {Time, _Result} = timer:tc(Fun),
        Time
    end, lists:seq(1, Iterations)).

%% @doc Calculate statistics from a list of timings.
-spec stats([non_neg_integer()]) -> #{median := number(), mean := number(),
                                       min := number(), max := number(),
                                       p95 := number(), p99 := number()}.
stats(Timings) ->
    Sorted = lists:sort(Timings),
    Len = length(Sorted),
    Sum = lists:sum(Sorted),
    #{
        median => lists:nth(Len div 2 + 1, Sorted),
        mean => Sum / Len,
        min => hd(Sorted),
        max => lists:last(Sorted),
        p95 => lists:nth(round(Len * 0.95), Sorted),
        p99 => lists:nth(round(Len * 0.99), Sorted)
    }.

%% @doc Format and print benchmark results in parseable format.
-spec report(string(), #{median := number(), mean := number(),
                          min := number(), max := number(),
                          p95 := number(), p99 := number()},
             pos_integer()) -> ok.
report(Name, #{median := Median, mean := Mean, min := Min,
               max := Max, p95 := P95, p99 := P99}, N) ->
    io:format(standard_error,
        "PERF: ~s ~.1fus (mean: ~.1fus, min: ~bus, max: ~bus, p95: ~bus, p99: ~bus, n: ~b)~n",
        [Name, float(Median), float(Mean), Min, Max, P95, P99, N]).

%%====================================================================
%% Benchmarks
%%====================================================================

perf_test_() ->
    {setup,
     fun setup/0,
     fun(_) -> ok end,
     {timeout, 120, fun all_benchmarks/0}}.

all_benchmarks() ->
    bench_raw_message_roundtrip(),
    bench_future_create_resolve_await(),
    bench_actor_sync_call(),
    bench_actor_async_call(),
    bench_gen_server_call_vs_cast_future(),
    bench_throughput(),
    bench_concurrent_callers(),
    bench_overhead_comparison(),
    bench_overhead_comparison_hires(),
    bench_block_threading().

%% --- 1. Raw message send/receive (baseline) ---

bench_raw_message_roundtrip() ->
    Echo = spawn_link(fun Loop() ->
        receive
            stop -> ok;
            {From, Msg} ->
                From ! {reply, Msg},
                Loop()
        end
    end),
    Timings = run_benchmark(fun() ->
        Echo ! {self(), ping},
        receive {reply, ping} -> ok
        after 1000 -> erlang:error({perf_timeout, raw_message_roundtrip})
        end
    end, ?ITERATIONS, ?WARMUP),
    Echo ! stop,
    S = stats(Timings),
    report("raw_message_roundtrip", S, ?ITERATIONS),
    %% Sanity: raw message roundtrip should be under 100us
    ?assert(maps:get(median, S) < 100).

%% --- 2. Future creation + resolve + await ---

bench_future_create_resolve_await() ->
    Timings = run_benchmark(fun() ->
        Future = beamtalk_future:new(),
        beamtalk_future:resolve(Future, 42),
        42 = beamtalk_future:await(Future),
        ok
    end, ?ITERATIONS, ?WARMUP),
    S = stats(Timings),
    report("future_create_resolve_await", S, ?ITERATIONS),
    %% Sanity: future lifecycle should be under 500us
    ?assert(maps:get(median, S) < 500).

%% --- 3. Full actor method call (sync via gen_server:call) ---

bench_actor_sync_call() ->
    {ok, Counter} = test_counter:start_link(0),
    try
        Timings = run_benchmark(fun() ->
            gen_server:call(Counter, {getValue, []})
        end, ?ITERATIONS, ?WARMUP),
        S = stats(Timings),
        report("actor_sync_call", S, ?ITERATIONS),
        %% Sanity: sync call should be under 200us
        ?assert(maps:get(median, S) < 200)
    after
        gen_server:stop(Counter)
    end.

%% --- 4. Full actor method call (async via cast + future + await) ---

bench_actor_async_call() ->
    {ok, Counter} = test_counter:start_link(0),
    try
        Timings = run_benchmark(fun() ->
            Future = beamtalk_future:new(),
            gen_server:cast(Counter, {increment, [], Future}),
            beamtalk_future:await(Future)
        end, ?ITERATIONS, ?WARMUP),
        S = stats(Timings),
        report("actor_async_call", S, ?ITERATIONS),
        %% Sanity: async call should be under 500us
        ?assert(maps:get(median, S) < 500)
    after
        gen_server:stop(Counter)
    end.

%% --- 5. gen_server:call vs cast+future comparison ---

bench_gen_server_call_vs_cast_future() ->
    {ok, Counter} = test_counter:start_link(0),
    try
        %% Measure sync (gen_server:call)
        SyncTimings = run_benchmark(fun() ->
            gen_server:call(Counter, {getValue, []})
        end, ?ITERATIONS, ?WARMUP),

        %% Measure async (cast + future) using same selector for fair comparison
        AsyncTimings = run_benchmark(fun() ->
            Future = beamtalk_future:new(),
            gen_server:cast(Counter, {getValue, [], Future}),
            beamtalk_future:await(Future)
        end, ?ITERATIONS, ?WARMUP),

        SyncStats = stats(SyncTimings),
        AsyncStats = stats(AsyncTimings),
        report("comparison_sync_call", SyncStats, ?ITERATIONS),
        report("comparison_async_cast_future", AsyncStats, ?ITERATIONS),

        %% Report the overhead ratio
        SyncMedian = maps:get(median, SyncStats),
        AsyncMedian = maps:get(median, AsyncStats),
        Ratio = case SyncMedian of
            0 -> 0.0;
            _ -> AsyncMedian / SyncMedian
        end,
        io:format(standard_error, "PERF: future_overhead_ratio ~.2fx~n", [Ratio])
    after
        gen_server:stop(Counter)
    end.

%% --- 6. Throughput test (calls/sec under load) ---

bench_throughput() ->
    {ok, Counter} = test_counter:start_link(0),
    NumCalls = 10000,

    {Elapsed, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            gen_server:call(Counter, {increment, []})
        end, lists:seq(1, NumCalls))
    end),

    gen_server:stop(Counter),

    CallsPerSec = round(NumCalls / (Elapsed / 1_000_000)),
    ElapsedMs = Elapsed / 1000.0,
    io:format(standard_error,
        "PERF: throughput_sync_serial ~b calls/sec (~b calls in ~.1fms)~n",
        [CallsPerSec, NumCalls, ElapsedMs]),
    %% Sanity: should sustain at least 10k calls/sec on any hardware
    ?assert(CallsPerSec > 10000).

%% --- 8. Overhead comparison: raw process vs Erlang gen_server vs Beamtalk actor ---
%%
%% Measures the same counter (increment + get_value) across four implementations
%% to isolate where overhead is introduced:
%%
%%   raw_process        – bare receive loop, no gen_server
%%   erlang_gs_record   – idiomatic gen_server with record state (fastest gen_server)
%%   erlang_gs_map      – gen_server with map state, no dispatch layer
%%   beamtalk_actor     – full Beamtalk runtime: make_self + method table lookup + fun call
%%
%% Each level adds overhead relative to the one below. The difference between
%% erlang_gs_map and beamtalk_actor is the pure Beamtalk dispatch tax.

bench_overhead_comparison() ->
    %% Raw process counter (inline — no module needed)
    RawPid = spawn_link(fun() ->
        raw_counter_loop(0)
    end),
    %% Warmup raw
    lists:foreach(fun(_) ->
        RawPid ! {increment, self()},
        receive ok -> ok after 1000 -> error({perf_timeout, overhead_raw_process, increment}) end
    end, lists:seq(1, ?WARMUP)),
    RawTimings = lists:map(fun(_) ->
        {T, _} = timer:tc(fun() ->
            RawPid ! {increment, self()},
            receive ok -> ok after 1000 -> error({perf_timeout, overhead_raw_process, increment}) end,
            RawPid ! {get_value, self()},
            receive {value, _} -> ok after 1000 -> error({perf_timeout, overhead_raw_process, get_value}) end
        end),
        T
    end, lists:seq(1, ?ITERATIONS)),
    RawPid ! stop,

    %% gen_server with record state
    {ok, GsRecord} = bench_erlang_gs_record:start_link(),
    GsRecordTimings = run_benchmark(fun() ->
        bench_erlang_gs_record:increment(GsRecord),
        bench_erlang_gs_record:get_value(GsRecord)
    end, ?ITERATIONS, ?WARMUP),
    gen_server:stop(GsRecord),

    %% gen_server with map state (no Beamtalk dispatch)
    {ok, GsMap} = bench_erlang_gs_map:start_link(),
    GsMapTimings = run_benchmark(fun() ->
        bench_erlang_gs_map:increment(GsMap),
        bench_erlang_gs_map:get_value(GsMap)
    end, ?ITERATIONS, ?WARMUP),
    gen_server:stop(GsMap),

    %% Beamtalk actor (full runtime dispatch)
    {ok, BtActor} = test_counter:start_link(0),
    BtTimings = run_benchmark(fun() ->
        gen_server:call(BtActor, {increment, []}),
        gen_server:call(BtActor, {getValue, []})
    end, ?ITERATIONS, ?WARMUP),
    gen_server:stop(BtActor),

    RawStats = stats(RawTimings),
    RecordStats = stats(GsRecordTimings),
    MapStats = stats(GsMapTimings),
    BtStats = stats(BtTimings),

    report("overhead/raw_process_counter", RawStats, ?ITERATIONS),
    report("overhead/erlang_gs_record_counter", RecordStats, ?ITERATIONS),
    report("overhead/erlang_gs_map_counter", MapStats, ?ITERATIONS),
    report("overhead/beamtalk_actor_counter", BtStats, ?ITERATIONS),

    %% Report overhead ratios vs raw process baseline
    RawMedian = maps:get(median, RawStats),
    RecordMedian = maps:get(median, RecordStats),
    MapMedian = maps:get(median, MapStats),
    BtMedian = maps:get(median, BtStats),

    io:format(standard_error,
        "PERF: overhead/gs_record_vs_raw ~.2fx~n",
        [RecordMedian / max(RawMedian, 1)]),
    io:format(standard_error,
        "PERF: overhead/gs_map_vs_record ~.2fx~n",
        [MapMedian / max(RecordMedian, 1)]),
    io:format(standard_error,
        "PERF: overhead/beamtalk_vs_gs_map ~.2fx~n",
        [BtMedian / max(MapMedian, 1)]),
    io:format(standard_error,
        "PERF: overhead/beamtalk_vs_raw ~.2fx~n",
        [BtMedian / max(RawMedian, 1)]).

raw_counter_loop(N) ->
    receive
        stop ->
            ok;
        {increment, From} ->
            From ! ok,
            raw_counter_loop(N + 1);
        {get_value, From} ->
            From ! {value, N},
            raw_counter_loop(N)
    end.

%% --- 9. High-resolution gen_server overhead comparison ---
%%
%% timer:tc resolution is 1us, which floors all results when calls take <2us.
%% This variant batches ?HIRES_BATCH calls per sample to get sub-microsecond
%% per-call figures by dividing total time by batch size.
%%
%% Reports per-call latency in nanoseconds for each layer:
%%   raw_process, erlang_gs_record, erlang_gs_map, beamtalk_actor

-define(HIRES_BATCH, 100).
-define(HIRES_ITERATIONS, 1000).
-define(HIRES_WARMUP, 200).

bench_overhead_comparison_hires() ->
    %% Raw process
    RawPid = spawn_link(fun() -> raw_counter_loop(0) end),
    lists:foreach(fun(_) ->
        RawPid ! {increment, self()},
        receive ok -> ok after 1000 -> error({perf_timeout, overhead_raw_process_hires, warmup_increment}) end
    end, lists:seq(1, ?HIRES_WARMUP * ?HIRES_BATCH)),
    RawBatchTimings = lists:map(fun(_) ->
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                RawPid ! {increment, self()},
                receive ok -> ok after 1000 -> error({perf_timeout, overhead_raw_process_hires, increment}) end
            end, lists:seq(1, ?HIRES_BATCH))
        end),
        %% Convert to nanoseconds per call
        T * 1000 div ?HIRES_BATCH
    end, lists:seq(1, ?HIRES_ITERATIONS)),
    RawPid ! stop,

    %% gen_server with record state
    {ok, GsRecord} = bench_erlang_gs_record:start_link(),
    lists:foreach(fun(_) ->
        bench_erlang_gs_record:increment(GsRecord)
    end, lists:seq(1, ?HIRES_WARMUP * ?HIRES_BATCH)),
    RecordBatchTimings = lists:map(fun(_) ->
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                bench_erlang_gs_record:increment(GsRecord)
            end, lists:seq(1, ?HIRES_BATCH))
        end),
        T * 1000 div ?HIRES_BATCH
    end, lists:seq(1, ?HIRES_ITERATIONS)),
    gen_server:stop(GsRecord),

    %% gen_server with map state
    {ok, GsMap} = bench_erlang_gs_map:start_link(),
    lists:foreach(fun(_) ->
        bench_erlang_gs_map:increment(GsMap)
    end, lists:seq(1, ?HIRES_WARMUP * ?HIRES_BATCH)),
    MapBatchTimings = lists:map(fun(_) ->
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                bench_erlang_gs_map:increment(GsMap)
            end, lists:seq(1, ?HIRES_BATCH))
        end),
        T * 1000 div ?HIRES_BATCH
    end, lists:seq(1, ?HIRES_ITERATIONS)),
    gen_server:stop(GsMap),

    %% Beamtalk actor
    {ok, BtActor} = test_counter:start_link(0),
    lists:foreach(fun(_) ->
        gen_server:call(BtActor, {increment, []})
    end, lists:seq(1, ?HIRES_WARMUP * ?HIRES_BATCH)),
    BtBatchTimings = lists:map(fun(_) ->
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                gen_server:call(BtActor, {increment, []})
            end, lists:seq(1, ?HIRES_BATCH))
        end),
        T * 1000 div ?HIRES_BATCH
    end, lists:seq(1, ?HIRES_ITERATIONS)),
    gen_server:stop(BtActor),

    RawNs    = stats(RawBatchTimings),
    RecordNs = stats(RecordBatchTimings),
    MapNs    = stats(MapBatchTimings),
    BtNs     = stats(BtBatchTimings),

    report_ns("hires/raw_process_increment", RawNs, ?HIRES_ITERATIONS),
    report_ns("hires/erlang_gs_record_increment", RecordNs, ?HIRES_ITERATIONS),
    report_ns("hires/erlang_gs_map_increment", MapNs, ?HIRES_ITERATIONS),
    report_ns("hires/beamtalk_actor_increment", BtNs, ?HIRES_ITERATIONS),

    RawNsMedian    = maps:get(median, RawNs),
    RecordNsMedian = maps:get(median, RecordNs),
    MapNsMedian    = maps:get(median, MapNs),
    BtNsMedian     = maps:get(median, BtNs),
    io:format(standard_error,
        "PERF: hires/gs_record_vs_raw ~.3fx~n",
        [RecordNsMedian / max(RawNsMedian, 1)]),
    io:format(standard_error,
        "PERF: hires/gs_map_vs_record ~.3fx~n",
        [MapNsMedian / max(RecordNsMedian, 1)]),
    io:format(standard_error,
        "PERF: hires/beamtalk_vs_gs_map ~.3fx~n",
        [BtNsMedian / max(MapNsMedian, 1)]),
    io:format(standard_error,
        "PERF: hires/beamtalk_vs_raw ~.3fx~n",
        [BtNsMedian / max(RawNsMedian, 1)]).

%% @doc Like report/3 but values are nanoseconds.
report_ns(Name, #{median := Median, mean := Mean, min := Min,
                  max := Max, p95 := P95, p99 := P99}, N) ->
    io:format(standard_error,
        "PERF: ~s ~bns (mean: ~.1fns, min: ~bns, max: ~bns, p95: ~bns, p99: ~bns, n: ~b)~n",
        [Name, Median, float(Mean), Min, Max, P95, P99, N]).

%% --- 7. Concurrent callers test ---
%%
%% 10 callers each making 1000 calls to a single actor — measures queue depth
%% and lock contention under concurrent load.

bench_concurrent_callers() ->
    {ok, Counter} = test_counter:start_link(0),
    NumCallers = 10,
    CallsPerCaller = 1000,
    Parent = self(),

    {Elapsed, _} = timer:tc(fun() ->
        Pids = lists:map(fun(_) ->
            spawn_link(fun() ->
                lists:foreach(fun(_) ->
                    gen_server:call(Counter, {increment, []})
                end, lists:seq(1, CallsPerCaller)),
                Parent ! {done, self()}
            end)
        end, lists:seq(1, NumCallers)),
        lists:foreach(fun(Pid) ->
            receive {done, Pid} -> ok
            after 60000 -> erlang:error({timeout_waiting_for_done, Pid})
            end
        end, Pids)
    end),

    %% Verify all increments landed
    FinalValue = gen_server:call(Counter, {getValue, []}),
    gen_server:stop(Counter),

    TotalCalls = NumCallers * CallsPerCaller,
    CallsPerSec = round(TotalCalls / (Elapsed / 1_000_000)),
    ElapsedMs = Elapsed / 1000.0,
    io:format(standard_error,
        "PERF: throughput_concurrent ~b calls/sec (~b callers x ~b calls in ~.1fms)~n",
        [CallsPerSec, NumCallers, CallsPerCaller, ElapsedMs]),
    ?assertEqual(TotalCalls, FinalValue),
    %% Sanity: concurrent throughput should still be at least 10k calls/sec
    ?assert(CallsPerSec > 10000).

%%====================================================================
%% Block threading benchmarks
%%====================================================================

%% Measures the overhead of:
%%   1. StateAcc map threading (maps:get/maps:put per iteration) vs
%%      idiomatic tail-recursive accumulator
%%   2. maybe_await wrappers on each arithmetic operand
%%   3. Stateful block calling convention for collect: / inject:into:
%%
%% Each benchmark runs the same computation (sum 1..N, double a list,
%% fold a list) at three levels:
%%   native         — idiomatic Erlang, no overhead
%%   stateacc       — maps:get/put per iteration (Beamtalk's timesRepeat: pattern)
%%   stateacc+await — stateacc + maybe_await on each operand (full codegen output)

bench_block_threading() ->
    N = 10000,
    List = lists:seq(1, N),

    %% --- timesRepeat: / sum loop ---
    NativeTimings = run_benchmark(fun() ->
        bench_block_threading:sum_native(N)
    end, ?ITERATIONS, ?WARMUP),
    StateAccTimings = run_benchmark(fun() ->
        bench_block_threading:sum_stateacc(N)
    end, ?ITERATIONS, ?WARMUP),
    StateAccAwaitTimings = run_benchmark(fun() ->
        bench_block_threading:sum_stateacc_maybe_await(N)
    end, ?ITERATIONS, ?WARMUP),
    ProcDictTimings = run_benchmark(fun() ->
        bench_block_threading:sum_procdict(N)
    end, ?ITERATIONS, ?WARMUP),

    DirectParamsTimings = run_benchmark(fun() ->
        bench_block_threading:sum_direct_params(N)
    end, ?ITERATIONS, ?WARMUP),
    DirectParamsAwaitTimings = run_benchmark(fun() ->
        bench_block_threading:sum_direct_params_maybe_await(N)
    end, ?ITERATIONS, ?WARMUP),

    NativeStats = stats(NativeTimings),
    StateAccStats = stats(StateAccTimings),
    StateAccAwaitStats = stats(StateAccAwaitTimings),
    ProcDictStats = stats(ProcDictTimings),
    DirectParamsStats = stats(DirectParamsTimings),
    DirectParamsAwaitStats = stats(DirectParamsAwaitTimings),

    report("block/sum_native", NativeStats, ?ITERATIONS),
    report("block/sum_stateacc", StateAccStats, ?ITERATIONS),
    report("block/sum_stateacc_maybe_await", StateAccAwaitStats, ?ITERATIONS),
    report("block/sum_procdict", ProcDictStats, ?ITERATIONS),
    report("block/sum_direct_params", DirectParamsStats, ?ITERATIONS),
    report("block/sum_direct_params_maybe_await", DirectParamsAwaitStats, ?ITERATIONS),

    NativeMedian = maps:get(median, NativeStats),
    StateAccMedian = maps:get(median, StateAccStats),
    StateAccAwaitMedian = maps:get(median, StateAccAwaitStats),
    ProcDictMedian = maps:get(median, ProcDictStats),
    DirectParamsMedian = maps:get(median, DirectParamsStats),
    DirectParamsAwaitMedian = maps:get(median, DirectParamsAwaitStats),
    io:format(standard_error,
        "PERF: block/stateacc_overhead ~.2fx vs native~n",
        [StateAccMedian / max(NativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/maybe_await_overhead ~.2fx vs stateacc~n",
        [StateAccAwaitMedian / max(StateAccMedian, 1)]),
    io:format(standard_error,
        "PERF: block/procdict_vs_native ~.2fx~n",
        [ProcDictMedian / max(NativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/procdict_vs_stateacc ~.2fx~n",
        [ProcDictMedian / max(StateAccMedian, 1)]),
    %% BT-1275: Direct-params overhead should be ≤ 3x native (vs ~26-30x for StateAcc).
    DirectParamsOverhead = DirectParamsMedian / max(NativeMedian, 1),
    DirectParamsAwaitOverhead = DirectParamsAwaitMedian / max(NativeMedian, 1),
    io:format(standard_error,
        "PERF: block/direct_params_overhead ~.2fx vs native~n",
        [DirectParamsOverhead]),
    io:format(standard_error,
        "PERF: block/direct_params_await_overhead ~.2fx vs native~n",
        [DirectParamsAwaitOverhead]),
    ?assert(DirectParamsOverhead =< 3.0),

    %% --- collect: / lists:map ---
    CollectNativeTimings = run_benchmark(fun() ->
        bench_block_threading:double_native(List)
    end, ?ITERATIONS, ?WARMUP),
    CollectStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:double_stateacc_block(List)
    end, ?ITERATIONS, ?WARMUP),

    CollectNativeStats = stats(CollectNativeTimings),
    CollectStateAccStats = stats(CollectStateAccTimings),

    report("block/collect_native", CollectNativeStats, ?ITERATIONS),
    report("block/collect_stateacc_block", CollectStateAccStats, ?ITERATIONS),
    io:format(standard_error,
        "PERF: block/collect_overhead ~.2fx vs native~n",
        [maps:get(median, CollectStateAccStats) / max(maps:get(median, CollectNativeStats), 1)]),

    %% --- inject:into: / foldl ---
    FoldNativeTimings = run_benchmark(fun() ->
        bench_block_threading:fold_native(List)
    end, ?ITERATIONS, ?WARMUP),
    FoldStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:fold_stateacc_block(List)
    end, ?ITERATIONS, ?WARMUP),

    FoldNativeStats = stats(FoldNativeTimings),
    FoldStateAccStats = stats(FoldStateAccTimings),

    report("block/fold_native", FoldNativeStats, ?ITERATIONS),
    report("block/fold_stateacc_block", FoldStateAccStats, ?ITERATIONS),
    io:format(standard_error,
        "PERF: block/fold_overhead ~.2fx vs native~n",
        [maps:get(median, FoldStateAccStats) / max(maps:get(median, FoldNativeStats), 1)]).

