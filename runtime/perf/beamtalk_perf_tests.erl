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
    bench_dynamic_class_call(),
    bench_class_creation().

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

%% --- 7. Concurrent callers test ---

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

%% --- 8. Dynamic class method calls vs compiled class ---

bench_dynamic_class_call() ->
    %% Create a dynamic counter class with unique name to avoid collisions
    ClassName = list_to_atom("PerfCounter_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {ok, ClassPid} = beamtalk_object_class:create_subclass('Actor', ClassName, #{
        instance_variables => [value],
        instance_methods => #{
            increment => fun(_Self, [], State) ->
                Value = maps:get(value, State, 0),
                {reply, Value + 1, maps:put(value, Value + 1, State)}
            end,
            getValue => fun(_Self, [], State) ->
                {reply, maps:get(value, State, 0), State}
            end
        }
    }),
    {ok, #beamtalk_object{pid = DynPid}} = beamtalk_object_class:new(ClassPid, [#{value => 0}]),

    %% Also start a compiled counter for comparison
    {ok, CompiledPid} = test_counter:start_link(0),

    %% Benchmark dynamic class
    DynTimings = run_benchmark(fun() ->
        gen_server:call(DynPid, {getValue, []})
    end, ?ITERATIONS, ?WARMUP),

    %% Benchmark compiled class
    CompiledTimings = run_benchmark(fun() ->
        gen_server:call(CompiledPid, {getValue, []})
    end, ?ITERATIONS, ?WARMUP),

    gen_server:stop(DynPid),
    gen_server:stop(CompiledPid),
    gen_server:stop(ClassPid, normal, 5000),

    DynStats = stats(DynTimings),
    CompiledStats = stats(CompiledTimings),
    report("dynamic_class_call", DynStats, ?ITERATIONS),
    report("compiled_class_call", CompiledStats, ?ITERATIONS),

    DynMedian = maps:get(median, DynStats),
    CompiledMedian = maps:get(median, CompiledStats),
    Ratio = case CompiledMedian of
        0 -> 0.0;
        _ -> DynMedian / CompiledMedian
    end,
    io:format(standard_error, "PERF: dynamic_vs_compiled_ratio ~.2fx~n", [Ratio]).

%% --- 9. Class creation overhead ---

bench_class_creation() ->
    NumClasses = 50,
    %% Note: list_to_atom creates permanent atoms (55 total including warmup).
    %% Acceptable for on-demand perf tests; would need list_to_existing_atom
    %% or a cleanup mechanism for continuous/CI use.
    Timings = run_benchmark(fun() ->
        ClassName = list_to_atom("PerfTestClass_" ++ integer_to_list(erlang:unique_integer([positive]))),
        {ok, Pid} = beamtalk_object_class:create_subclass('Actor', ClassName, #{
            instance_variables => [x],
            instance_methods => #{
                getX => fun(_Self, [], State) -> {reply, maps:get(x, State, 0), State} end
            }
        }),
        gen_server:stop(Pid, normal, 5000),
        ok
    end, NumClasses, 5),
    S = stats(Timings),
    report("class_creation", S, NumClasses),
    %% Sanity: class creation should be under 10ms
    ?assert(maps:get(median, S) < 10000).
