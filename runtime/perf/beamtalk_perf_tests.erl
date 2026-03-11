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

    DirectParamsTimings = run_benchmark(fun() ->
        bench_block_threading:sum_direct_params(N)
    end, ?ITERATIONS, ?WARMUP),

    %% BT-1286: scale loop — best-case for literal-skipping optimisation.
    %% result := result * 2 repeated N times: 1 variable + 1 literal per op.
    ScaleNativeTimings = run_benchmark(fun() ->
        bench_block_threading:scale_native(N)
    end, ?ITERATIONS, ?WARMUP),
    ScaleLiteralOptTimings = run_benchmark(fun() ->
        bench_block_threading:scale_direct_params_literal_opt(N)
    end, ?ITERATIONS, ?WARMUP),

    NativeStats = stats(NativeTimings),
    StateAccStats = stats(StateAccTimings),
    DirectParamsStats = stats(DirectParamsTimings),
    ScaleNativeStats = stats(ScaleNativeTimings),
    ScaleLiteralOptStats = stats(ScaleLiteralOptTimings),

    report("block/sum_native", NativeStats, ?ITERATIONS),
    report("block/sum_stateacc", StateAccStats, ?ITERATIONS),
    report("block/sum_direct_params", DirectParamsStats, ?ITERATIONS),
    report("block/scale_native", ScaleNativeStats, ?ITERATIONS),
    report("block/scale_literal_opt", ScaleLiteralOptStats, ?ITERATIONS),

    NativeMedian = maps:get(median, NativeStats),
    StateAccMedian = maps:get(median, StateAccStats),
    DirectParamsMedian = maps:get(median, DirectParamsStats),
    ScaleNativeMedian = maps:get(median, ScaleNativeStats),
    ScaleLiteralOptMedian = maps:get(median, ScaleLiteralOptStats),
    io:format(standard_error,
        "PERF: block/stateacc_overhead ~.2fx vs native~n",
        [StateAccMedian / max(NativeMedian, 1)]),
    %% BT-1275: Direct-params overhead should be ≤ 3x native (vs ~26-30x for StateAcc).
    DirectParamsOverhead = DirectParamsMedian / max(NativeMedian, 1),
    io:format(standard_error,
        "PERF: block/direct_params_overhead ~.2fx vs native~n",
        [DirectParamsOverhead]),
    %% BT-1286: Literal-skipping overhead on scale loop vs native.
    io:format(standard_error,
        "PERF: block/scale_literal_opt_overhead ~.2fx vs native~n",
        [ScaleLiteralOptMedian / max(ScaleNativeMedian, 1)]),
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
        "PERF: block/collect_pure_overhead ~.2fx vs native~n",
        [maps:get(median, CollectStateAccStats) / max(maps:get(median, CollectNativeStats), 1)]),

    %% --- inject:into: / foldl ---
    FoldNativeTimings = run_benchmark(fun() ->
        bench_block_threading:fold_native(List)
    end, ?ITERATIONS, ?WARMUP),
    FoldStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:fold_stateacc_block(List)
    end, ?ITERATIONS, ?WARMUP),

    %% BT-1327: inject:into: pure-path variants (no mutations)
    FoldInjectIntoTimings = run_benchmark(fun() ->
        bench_block_threading:fold_inject_into_wrapper(List)
    end, ?ITERATIONS, ?WARMUP),
    FoldInlineSwapTimings = run_benchmark(fun() ->
        bench_block_threading:fold_inline_swap(List)
    end, ?ITERATIONS, ?WARMUP),

    FoldNativeStats = stats(FoldNativeTimings),
    FoldStateAccStats = stats(FoldStateAccTimings),
    FoldInjectIntoStats = stats(FoldInjectIntoTimings),
    FoldInlineSwapStats = stats(FoldInlineSwapTimings),

    report("block/fold_native", FoldNativeStats, ?ITERATIONS),
    report("block/fold_stateacc_block", FoldStateAccStats, ?ITERATIONS),
    report("block/fold_inject_into_wrapper", FoldInjectIntoStats, ?ITERATIONS),
    report("block/fold_inline_swap", FoldInlineSwapStats, ?ITERATIONS),
    io:format(standard_error,
        "PERF: block/fold_stateacc_overhead ~.2fx vs native~n",
        [maps:get(median, FoldStateAccStats) / max(maps:get(median, FoldNativeStats), 1)]),
    io:format(standard_error,
        "PERF: block/fold_inject_into_overhead ~.2fx vs native~n",
        [maps:get(median, FoldInjectIntoStats) / max(maps:get(median, FoldNativeStats), 1)]),
    io:format(standard_error,
        "PERF: block/fold_pure_overhead ~.2fx vs native~n",
        [maps:get(median, FoldInlineSwapStats) / max(maps:get(median, FoldNativeStats), 1)]),

    %% --- BT-1276: list-op with local variable mutation (StateAcc map vs tuple acc) ---
    DoNativeMutTimings = run_benchmark(fun() ->
        bench_block_threading:do_native_mutation(List)
    end, ?ITERATIONS, ?WARMUP),
    DoStateAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:do_stateacc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),
    DoTupleAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:do_tuple_acc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),

    DoNativeMutStats = stats(DoNativeMutTimings),
    DoStateAccMutStats = stats(DoStateAccMutTimings),
    DoTupleAccMutStats = stats(DoTupleAccMutTimings),

    report("block/do_native_mutation", DoNativeMutStats, ?ITERATIONS),
    report("block/do_stateacc_mutation", DoStateAccMutStats, ?ITERATIONS),
    report("block/do_tuple_acc_mutation", DoTupleAccMutStats, ?ITERATIONS),
    DoNativeMutMedian = maps:get(median, DoNativeMutStats),
    DoMutImprovementRatio = maps:get(median, DoStateAccMutStats) /
        max(maps:get(median, DoTupleAccMutStats), 1),
    io:format(standard_error,
        "PERF: block/do_mutation_improvement ~.2fx tuple vs stateacc~n",
        [DoMutImprovementRatio]),
    io:format(standard_error,
        "PERF: block/do_tuple_acc_overhead ~.2fx vs native~n",
        [maps:get(median, DoTupleAccMutStats) / max(DoNativeMutMedian, 1)]),
    %% BT-1276: Tuple-acc should be at least 1.5x faster than StateAcc for do: with mutation.
    ?assert(DoMutImprovementRatio >= 1.5),

    CollectStateAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:collect_stateacc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),
    CollectTupleAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:collect_tuple_acc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),

    CollectStateAccMutStats = stats(CollectStateAccMutTimings),
    CollectTupleAccMutStats = stats(CollectTupleAccMutTimings),

    report("block/collect_stateacc_mutation", CollectStateAccMutStats, ?ITERATIONS),
    report("block/collect_tuple_acc_mutation", CollectTupleAccMutStats, ?ITERATIONS),
    CollectMutImprovementRatio = maps:get(median, CollectStateAccMutStats) /
        max(maps:get(median, CollectTupleAccMutStats), 1),
    io:format(standard_error,
        "PERF: block/collect_mutation_improvement ~.2fx tuple vs stateacc~n",
        [CollectMutImprovementRatio]),
    %% BT-1276: Tuple-acc should be at least 1.5x faster than StateAcc for collect: with mutation.
    ?assert(CollectMutImprovementRatio >= 1.5),

    FoldStateAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:fold_stateacc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),
    FoldTupleAccMutTimings = run_benchmark(fun() ->
        bench_block_threading:fold_tuple_acc_mutation(List)
    end, ?ITERATIONS, ?WARMUP),

    FoldStateAccMutStats = stats(FoldStateAccMutTimings),
    FoldTupleAccMutStats = stats(FoldTupleAccMutTimings),

    report("block/fold_stateacc_mutation", FoldStateAccMutStats, ?ITERATIONS),
    report("block/fold_tuple_acc_mutation", FoldTupleAccMutStats, ?ITERATIONS),
    FoldMutImprovementRatio = maps:get(median, FoldStateAccMutStats) /
        max(maps:get(median, FoldTupleAccMutStats), 1),
    io:format(standard_error,
        "PERF: block/fold_mutation_improvement ~.2fx tuple vs stateacc~n",
        [FoldMutImprovementRatio]),
    %% BT-1276: Tuple-acc should be at least 1.5x faster than StateAcc for inject: with mutation.
    ?assert(FoldMutImprovementRatio >= 1.5),

    %% --- BT-1329: nested list op inside counted loop (Tier-2 StateAcc fallback) ---
    %%
    %% Outer loop runs 10 times; inner inject:into: runs over a 10,000-element list
    %% mutating an outer-scope variable on every element — the classic Tier-2 pattern.
    %% Reduced sample count (100 iters / 10 warmup) keeps this in the same wall-clock
    %% budget as the rest of the suite (~120s); OuterN=100 with ?ITERATIONS would be ~1e9
    %% inner callbacks total.
    OuterN = 10,
    NestedIterations = 100,
    NestedWarmup = 10,
    NestedStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:nested_stateacc_list_op(OuterN, List)
    end, NestedIterations, NestedWarmup),
    NestedTupleTimings = run_benchmark(fun() ->
        bench_block_threading:nested_tuple_list_op(OuterN, List)
    end, NestedIterations, NestedWarmup),

    NestedStateAccStats = stats(NestedStateAccTimings),
    NestedTupleStats = stats(NestedTupleTimings),

    report("block/nested_stateacc_list_op", NestedStateAccStats, NestedIterations),
    report("block/nested_tuple_list_op", NestedTupleStats, NestedIterations),
    NestedImprovementRatio = maps:get(median, NestedStateAccStats) /
        max(maps:get(median, NestedTupleStats), 1),
    io:format(standard_error,
        "PERF: block/nested_list_op_improvement ~.2fx tuple vs stateacc~n",
        [NestedImprovementRatio]),
    %% BT-1329: Tuple-acc should be at least 1.5x faster than StateAcc for nested list ops.
    ?assert(NestedImprovementRatio >= 1.5),

    %% --- BT-1326: counted loop with both local + field mutations (hybrid vs StateAcc) ---
    MixedNativeTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_native(N)
    end, ?ITERATIONS, ?WARMUP),
    MixedStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_stateacc(N)
    end, ?ITERATIONS, ?WARMUP),
    MixedHybridTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_hybrid(N)
    end, ?ITERATIONS, ?WARMUP),

    MixedNativeStats = stats(MixedNativeTimings),
    MixedStateAccStats = stats(MixedStateAccTimings),
    MixedHybridStats = stats(MixedHybridTimings),

    report("block/mixed_native", MixedNativeStats, ?ITERATIONS),
    report("block/mixed_stateacc", MixedStateAccStats, ?ITERATIONS),
    report("block/mixed_hybrid", MixedHybridStats, ?ITERATIONS),

    MixedNativeMedian = maps:get(median, MixedNativeStats),
    MixedStateAccMedian = maps:get(median, MixedStateAccStats),
    MixedHybridMedian = maps:get(median, MixedHybridStats),

    io:format(standard_error,
        "PERF: block/mixed_stateacc_overhead ~.2fx vs native~n",
        [MixedStateAccMedian / max(MixedNativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_hybrid_overhead ~.2fx vs native~n",
        [MixedHybridMedian / max(MixedNativeMedian, 1)]),
    MixedHybridImprovementRatio = MixedStateAccMedian / max(MixedHybridMedian, 1),
    io:format(standard_error,
        "PERF: block/mixed_hybrid_improvement ~.2fx hybrid vs stateacc~n",
        [MixedHybridImprovementRatio]),
    %% BT-1326: Hybrid mode trades maps:get/maps:put ops (eliminated for locals)
    %% against an extra tail-call argument. On BEAM's JIT, small-map ops are
    %% heavily optimised, so the trade-off varies with map size and field count.
    %% The real codegen uses letrec (2 vs 3 args); this benchmark simulation
    %% uses module functions (3 vs 4 args) which slightly overstates arg overhead.
    %% Primary value: code clarity, read-only field pre-extraction, and scaling
    %% with more local variables where multiple maps:get/put are eliminated.
    %% No hard assertion — tracked as informational benchmark.

    %% --- Full extraction with small map (proves JIT anomaly disappears) ---
    SmallmapFullExtractTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_full_extract(N)
    end, ?ITERATIONS, ?WARMUP),
    SmallmapFullExtractStats = stats(SmallmapFullExtractTimings),
    report("block/mixed_full_extract", SmallmapFullExtractStats, ?ITERATIONS),
    SmallmapFullExtractMedian = maps:get(median, SmallmapFullExtractStats),
    io:format(standard_error,
        "PERF: block/mixed_full_extract_overhead ~.2fx vs native~n",
        [SmallmapFullExtractMedian / max(MixedNativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_full_extract_vs_stateacc ~.2fx full vs stateacc~n",
        [MixedStateAccMedian / max(SmallmapFullExtractMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_full_extract_vs_hybrid ~.2fx full vs hybrid~n",
        [MixedHybridMedian / max(SmallmapFullExtractMedian, 1)]),

    %% --- Crossover: stateacc vs full extract on small map, 1..4 mutated fields ---
    CrossoverResults = lists:map(fun(NumFields) ->
        SAFun = list_to_atom("crossover_stateacc_" ++ integer_to_list(NumFields) ++ "field" ++
            case NumFields of 1 -> ""; _ -> "s" end),
        FEFun = list_to_atom("crossover_full_extract_" ++ integer_to_list(NumFields) ++ "field" ++
            case NumFields of 1 -> ""; _ -> "s" end),
        SATimings = run_benchmark(fun() ->
            bench_block_threading:SAFun(N)
        end, ?ITERATIONS, ?WARMUP),
        FETimings = run_benchmark(fun() ->
            bench_block_threading:FEFun(N)
        end, ?ITERATIONS, ?WARMUP),
        SAStats = stats(SATimings),
        FEStats = stats(FETimings),
        SALabel = "block/crossover_sa_" ++ integer_to_list(NumFields) ++ "f",
        FELabel = "block/crossover_fe_" ++ integer_to_list(NumFields) ++ "f",
        report(SALabel, SAStats, ?ITERATIONS),
        report(FELabel, FEStats, ?ITERATIONS),
        SAMedian = maps:get(median, SAStats),
        FEMedian = maps:get(median, FEStats),
        Ratio = SAMedian / max(FEMedian, 1),
        io:format(standard_error,
            "PERF: block/crossover_~Bf winner=~s ratio=~.2fx~n",
            [NumFields,
             case Ratio >= 1.0 of true -> "full_extract"; false -> "stateacc" end,
             case Ratio >= 1.0 of true -> Ratio; false -> 1.0 / Ratio end]),
        {NumFields, SAMedian, FEMedian, Ratio}
    end, [1, 2, 3, 4]),
    _ = CrossoverResults,

    %% --- BT-1326: large-map variants (>32 keys, past BEAM small-map threshold) ---
    BigmapStateAccTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_stateacc(N)
    end, ?ITERATIONS, ?WARMUP),
    BigmapHybridTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_hybrid(N)
    end, ?ITERATIONS, ?WARMUP),

    BigmapStateAccStats = stats(BigmapStateAccTimings),
    BigmapHybridStats = stats(BigmapHybridTimings),

    report("block/mixed_bigmap_stateacc", BigmapStateAccStats, ?ITERATIONS),
    report("block/mixed_bigmap_hybrid", BigmapHybridStats, ?ITERATIONS),

    BigmapStateAccMedian = maps:get(median, BigmapStateAccStats),
    BigmapHybridMedian = maps:get(median, BigmapHybridStats),

    io:format(standard_error,
        "PERF: block/mixed_bigmap_stateacc_overhead ~.2fx vs native~n",
        [BigmapStateAccMedian / max(MixedNativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_bigmap_hybrid_overhead ~.2fx vs native~n",
        [BigmapHybridMedian / max(MixedNativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_bigmap_hybrid_improvement ~.2fx hybrid vs stateacc~n",
        [BigmapStateAccMedian / max(BigmapHybridMedian, 1)]),

    BigmapFullExtractTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_full_extract(N)
    end, ?ITERATIONS, ?WARMUP),
    BigmapFullExtractStats = stats(BigmapFullExtractTimings),
    report("block/mixed_bigmap_full_extract", BigmapFullExtractStats, ?ITERATIONS),
    BigmapFullExtractMedian = maps:get(median, BigmapFullExtractStats),
    io:format(standard_error,
        "PERF: block/mixed_bigmap_full_extract_overhead ~.2fx vs native~n",
        [BigmapFullExtractMedian / max(MixedNativeMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_bigmap_full_extract_improvement ~.2fx full vs stateacc~n",
        [BigmapStateAccMedian / max(BigmapFullExtractMedian, 1)]),
    io:format(standard_error,
        "PERF: block/mixed_bigmap_full_extract_vs_hybrid ~.2fx full vs hybrid~n",
        [BigmapHybridMedian / max(BigmapFullExtractMedian, 1)]),

    %% --- Arity scaling: does BEAM degrade with many params? ---
    FullExtract8pTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_full_extract_8params(N)
    end, ?ITERATIONS, ?WARMUP),
    FullExtract12pTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_full_extract_12params(N)
    end, ?ITERATIONS, ?WARMUP),
    FullExtract16pTimings = run_benchmark(fun() ->
        bench_block_threading:mixed_bigmap_full_extract_16params(N)
    end, ?ITERATIONS, ?WARMUP),

    FullExtract8pStats = stats(FullExtract8pTimings),
    FullExtract12pStats = stats(FullExtract12pTimings),
    FullExtract16pStats = stats(FullExtract16pTimings),

    report("block/full_extract_5params", BigmapFullExtractStats, ?ITERATIONS),
    report("block/full_extract_8params", FullExtract8pStats, ?ITERATIONS),
    report("block/full_extract_12params", FullExtract12pStats, ?ITERATIONS),
    report("block/full_extract_16params", FullExtract16pStats, ?ITERATIONS),

    FullExtract8pMedian = maps:get(median, FullExtract8pStats),
    FullExtract12pMedian = maps:get(median, FullExtract12pStats),
    FullExtract16pMedian = maps:get(median, FullExtract16pStats),

    io:format(standard_error,
        "PERF: block/arity_scaling 5p=~.1fx 8p=~.1fx 12p=~.1fx 16p=~.1fx vs native~n",
        [BigmapFullExtractMedian / max(MixedNativeMedian, 1),
         FullExtract8pMedian / max(MixedNativeMedian, 1),
         FullExtract12pMedian / max(MixedNativeMedian, 1),
         FullExtract16pMedian / max(MixedNativeMedian, 1)]),

    ok.

