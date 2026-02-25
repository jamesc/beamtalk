# Beamtalk Performance Characteristics

This document describes the performance characteristics of the beamtalk actor system and provides baseline numbers for tracking regressions.

## Running Benchmarks

```bash
just perf
```

This runs the performance test suite (`runtime/perf/beamtalk_perf_tests.erl`) separately from unit tests. Results are printed in a parseable format.

## Benchmark Results

Baseline numbers measured on a development machine (codespaces, 2-core). Your numbers will vary by hardware.

### Message Passing Latency

| Operation | Median | Mean | Notes |
|-----------|--------|------|-------|
| Raw message roundtrip | <1 µs | <1 µs | Erlang baseline (send + receive) |
| Future create + resolve + await | ~3 µs | ~5 µs | Full future lifecycle |
| Actor sync call (gen_server:call) | ~1-3 µs | ~2-4 µs | Direct call/reply |
| Actor async call (cast + future) | ~5-6 µs | ~6-8 µs | Full beamtalk actor call |

### Future Overhead

The async call path (cast + future + await) adds ~4-5x overhead compared to a direct `gen_server:call`. This comes from:

1. **Future process spawn** — creating a lightweight BEAM process (~2 KB)
2. **Extra message hops** — caller → actor → future, future → caller
3. **Process dictionary lookup** — timeout tracking in future process

### Throughput

| Scenario | Calls/sec | Notes |
|----------|-----------|-------|
| Serial sync calls | ~300k+ | Single caller, sequential gen_server:call |
| Concurrent (10 callers) | ~390k+ | 10 processes calling same actor |

## Sanity Thresholds

The performance tests include sanity checks (not strict thresholds) to catch severe regressions:

- Raw message roundtrip: < 100 µs
- Future lifecycle: < 500 µs
- Sync actor call: < 200 µs
- Async actor call: < 500 µs
- Serial throughput: > 10,000 calls/sec
- Concurrent throughput: > 10,000 calls/sec
These thresholds are intentionally generous to avoid CI flakiness across different hardware.

## Optimization Opportunities

Potential areas for future optimization:

1. **Pooled future processes** — reuse processes instead of spawning per-call
2. **Sync call mode** — bypass futures for latency-sensitive paths
3. **Batched message dispatch** — amortize overhead across multiple calls
4. **Inline small methods** — avoid gen_server overhead for trivial operations

## Parseable Output Format

Benchmark results are printed to stderr in a machine-readable format:

```
PERF: <name> <median>us (mean: <mean>us, min: <min>us, max: <max>us, p95: <p95>us, p99: <p99>us, n: <iterations>)
PERF: throughput_sync_serial <calls_per_sec> calls/sec (<calls> calls in <elapsed>ms)
PERF: future_overhead_ratio <ratio>x
```

This format can be parsed by CI scripts for trend tracking.
