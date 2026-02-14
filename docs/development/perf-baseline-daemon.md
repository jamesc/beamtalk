# Compilation Latency Baseline — Daemon Architecture

Baseline performance measurements for the current daemon-based compilation
architecture, captured before the OTP Port migration (ADR 0022).

## Test Environment

| Property       | Value |
|----------------|-------|
| OS             | Linux 6.6.87.2-microsoft-standard-WSL2 (x86_64) |
| CPU            | 11th Gen Intel Core i7-11700K @ 3.60GHz |
| CPU cores      | 16 |
| RAM            | 23 GiB |
| Rust           | rustc 1.93.0 (254b59607 2026-01-19) |
| Erlang/OTP     | 25 (ERTS 13.1.5) |
| Beamtalk       | commit 7844df7 |
| Build          | debug (unoptimized) |
| Iterations     | 100 per benchmark |
| Date           | 2026-02-14 |

## File Compilation (`beamtalk build`)

End-to-end latency for `beamtalk build <file>`, which includes:
1. Rust binary startup
2. Source reading
3. Lexing → parsing → semantic analysis → Core Erlang codegen (Rust)
4. Core Erlang → BEAM bytecode compilation (erlc via escript)

### End-to-End

| Benchmark                            | p50 (ms) | p95 (ms) | p99 (ms) | mean (ms) | min (ms) | max (ms) |
|--------------------------------------|----------|----------|----------|-----------|----------|----------|
| Counter (26 lines, minimal)          | 258.3    | 296.1    | 328.8    | 264.2     | 238.1    | 464.5    |
| ChatMember (55 lines, typical)       | 261.6    | 296.8    | 308.4    | 264.9     | 238.6    | 337.2    |
| ChatRoom (69 lines, multi-actor)     | 266.0    | 304.9    | 389.2    | 272.6     | 247.4    | 495.7    |

### BEAM Phase Only (`.core` → `.beam` via `erlc +from_core`)

Measured independently using `erlc +from_core` on pre-generated `.core` files.

| Benchmark                      | p50 (ms) | p95 (ms) | p99 (ms) | mean (ms) | min (ms) | max (ms) |
|--------------------------------|----------|----------|----------|-----------|----------|----------|
| Counter (26 lines) (BEAM)      | 229.9    | 273.0    | 346.8    | 235.9     | 213.2    | 396.4    |
| ChatMember (55 lines) (BEAM)   | 235.1    | 252.6    | 263.6    | 237.1     | 222.2    | 269.3    |
| ChatRoom (69 lines) (BEAM)     | 240.2    | 266.0    | 347.6    | 245.2     | 223.3    | 404.5    |

### Phase Analysis

Rust compilation time ≈ End-to-End − BEAM phase (includes process startup):

| File         | E2E p50 (ms) | BEAM p50 (ms) | Rust + startup (ms) | Rust % |
|--------------|--------------|---------------|----------------------|--------|
| Counter      | 258          | 230           | ~28                  | ~11%   |
| ChatMember   | 262          | 235           | ~27                  | ~10%   |
| ChatRoom     | 266          | 240           | ~26                  | ~10%   |

**Key insight:** ~90% of file compilation time is `erlc` (BEAM VM startup + Core
Erlang → BEAM). The Rust compiler (lex + parse + analyze + codegen) is only ~10%
of the total. This is dominated by `erlc` process startup overhead, not the
actual compilation work — the BEAM phase takes ~230ms regardless of file size.

## REPL Expression Evaluation

Round-trip latency for evaluating expressions in a running REPL workspace.
Measures the full path:

```
TCP connect → JSON request → REPL server → daemon IPC (Unix socket) →
Rust compile → Core Erlang → daemon response → BEAM compile:forms →
execute → format result → JSON response → TCP response
```

| Benchmark                         | p50 (ms) | p95 (ms) | p99 (ms) | mean (ms) | min (ms) | max (ms) |
|-----------------------------------|----------|----------|----------|-----------|----------|----------|
| Minimal expression (`1 + 2`)      | 49.5     | 54.1     | 95.9     | 51.0      | 41.3     | 98.3     |
| Factorial (`100 factorial`)       | 49.7     | 62.8     | 97.0     | 51.2      | 36.9     | 101.1    |
| Counter spawn (fixture pre-loaded)| 50.2     | 52.3     | 61.3     | 50.3      | 40.5     | 64.1     |

### Notes

- **~50ms baseline** for any REPL expression, regardless of complexity.
  This is the fixed overhead of daemon IPC + compilation round-trip.
- `1 + 2` and `100 factorial` have nearly identical latency, confirming that
  compilation dominates execution time for simple expressions.
- **Counter spawn** has comparable latency because the fixture is pre-loaded
  and actor creation is fast.
- The p99 spikes (~95-101ms) suggest occasional GC pauses or scheduling
  delays in the BEAM VM.
- REPL eval avoids the `erlc` startup penalty because the BEAM VM is already
  running — it uses `compile:forms/2` in-process, which is much faster than
  spawning a new `erlc` process.

## Latency Breakdown (Estimated)

Based on the measurements, the approximate breakdown for a REPL eval:

| Phase                          | Estimated (ms) | Notes |
|--------------------------------|----------------|-------|
| TCP round-trip (benchmark tool)| ~2-5           | Python socket connect + JSON encode/decode |
| REPL server dispatch           | ~1-2           | JSON parse, routing |
| Daemon IPC (Unix socket)       | ~2-5           | Socket connect + JSON-RPC + response |
| Rust compilation               | ~10-20         | Lex + parse + analyze + codegen |
| Core Erlang → BEAM             | ~15-25         | `compile:forms` in BEAM VM (no erlc startup) |
| Execution + formatting         | ~1-5           | Depends on expression |
| **Total**                      | **~45-55**     | Matches observed p50 |

## Implications for ADR 0022 (OTP Port Migration)

The Port migration replaces daemon IPC (Unix socket + JSON-RPC) with OTP Port
communication. Expected impact:

- **Daemon IPC overhead (~2-5ms)** will be replaced by Port protocol overhead
  (likely similar or slightly lower)
- **Rust compilation (~10-20ms)** is unchanged
- **The big win:** Eliminating daemon lifecycle management complexity, not raw
  latency. The ~50ms REPL eval time is already acceptable for interactive use.
- **File compilation:** The ~230ms `erlc` startup dominates; Port migration
  won't affect this. Future optimization: use in-process `compile:forms/2`
  instead of spawning `erlc`.

## Reproduction

```bash
# Build first
just build

# Run full benchmark (100 iterations, ~15 minutes)
scripts/bench-compilation.sh

# Quick check (10 iterations, ~2 minutes)
scripts/bench-compilation.sh --iterations 10

# File-only or REPL-only
scripts/bench-compilation.sh --file-only
scripts/bench-compilation.sh --repl-only
```

## References

- [ADR 0022: Embedded Compiler via OTP Port](../ADR/0022-embedded-compiler-via-otp-port.md)
- [BT-543: Epic — OTP Port Migration](https://linear.app/beamtalk/issue/BT-543)
- [BT-544: Establish Compilation Latency Baseline](https://linear.app/beamtalk/issue/BT-544)
