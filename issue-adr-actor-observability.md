# Linear Issue: ADR — Actor Observability and MCP-Exposed Tracing

**Team:** BT
**Assignee:** jamesc.000@gmail.com
**Labels:** `agent-ready`, `Documentation`, `runtime`, `M`
**Priority:** 2 (High)
**Type:** Documentation

---

## Title

ADR: Actor Observability and MCP-Exposed Tracing

## Context

Beamtalk actors (gen_server processes) currently have no built-in performance
instrumentation. When debugging timeouts, bottlenecks, or failures, developers
must use ad-hoc Erlang tools (sys, recon, dbg) manually. AI agents working via
MCP have no way to understand actor performance characteristics.

We need an ADR for adding structured tracing and performance telemetry to the
actor runtime, exposed via MCP tools so agents can follow a workflow of:
**enable tracing → run tests → get traces → debug perf**.

## Design Summary

### Vision

Turn on tracing → run test suite → get structured traces → agent debugs perf.

### Agent Workflow

```
1. agent calls enable_tracing          → starts capture
2. agent calls test (existing tool)    → runs BUnit suite
3. agent calls get_traces              → gets all captured events as structured JSON
4. agent calls actor_stats             → gets aggregate stats (computed from traces)
5. agent calls bottlenecks             → finds slow methods, high error rates
6. agent calls actor_health pid=X      → drills into live process state
7. agent calls disable_tracing         → stops capture, optionally clears buffer
```

### Architecture

```
MCP Tools (Rust)
    ↕ REPL ops over WebSocket
REPL Op Handlers (beamtalk_repl_ops_perf.erl)
    ↕
Trace Buffer (beamtalk_actor_stats.erl — gen_server + ETS)
    ↑ events written by
Instrumented Send Wrappers (beamtalk_actor.erl)
```

### Layer 1: beamtalk_actor_stats.erl — Trace Collection

Gen_server that owns two ETS tables:

**Table 1: beamtalk_trace_events (ordered_set)**
Captures individual call events when tracing is enabled.

```erlang
%% Key: monotonic timestamp (unique, ordered)
%% Row: {Timestamp, Pid, Class, Selector, Duration, Outcome, Metadata}
%% Outcome: ok | error | timeout | cast
%% Metadata: #{error => Term} for failures, #{} otherwise
```

**Table 2: beamtalk_actor_agg_stats (set)**
Running aggregates, always updated (low overhead).

```erlang
%% Key: {Pid, Selector}
%% Counters: calls, ok, errors, timeouts, total_duration, min_dur, max_dur
```

**API:**

```erlang
%% Tracing lifecycle
-spec enable() -> ok.
-spec disable() -> ok.
-spec is_enabled() -> boolean().
-spec clear() -> ok.

%% Event recording (called from send wrappers — must be fast)
-spec record(pid(), atom(), atom(), ok | error | timeout | cast, integer(), map()) -> ok.

%% Query — trace events
-spec get_traces() -> [map()].
-spec get_traces(pid()) -> [map()].
-spec get_traces(pid(), atom()) -> [map()].

%% Query — aggregates
-spec get_stats() -> map().
-spec get_stats(pid()) -> map().
-spec hot_methods(pos_integer()) -> [map()].
-spec slow_methods(pos_integer()) -> [map()].
-spec error_methods(pos_integer()) -> [map()].
-spec bottlenecks(pos_integer()) -> [map()].

%% Process health (live)
-spec actor_health(pid()) -> map().
-spec system_health() -> map().
```

**Design Decisions:**

- Aggregates always on — `ets:update_counter/3` is ~100ns, negligible
- Trace events only when enabled — controlled via `persistent_term` for zero-cost check when off
- Trace buffer capped at 100k events (configurable) — ring buffer semantics, oldest dropped
- Duration in microseconds in output, native units internally
- Class name captured at record time from actor state (`$beamtalk_class`) so traces are readable

**Overhead:**

| Operation | Cost | When |
|-----------|------|------|
| Aggregate update | ~100-200ns | Always |
| Trace event insert | ~300-500ns | Only when enabled |
| persistent_term check | ~5ns | Always (branch on trace enabled) |
| **Total per-call** | **~200ns (off) / ~600ns (on)** | |

### Layer 2: Instrument beamtalk_actor.erl

**sync_send/3 (the main dispatch path):**

```erlang
sync_send(ActorPid, Selector, Args) ->
    Start = erlang:monotonic_time(native),
    try
        Result = do_sync_send(ActorPid, Selector, Args),
        record_if_enabled(ActorPid, Selector, ok, Start, #{}),
        Result
    catch
        error:#beamtalk_error{kind = timeout} = E ->
            record_if_enabled(ActorPid, Selector, timeout, Start, #{error => E}),
            error(E);
        error:#beamtalk_error{} = E ->
            record_if_enabled(ActorPid, Selector, error, Start, #{error => E}),
            error(E)
    end.

record_if_enabled(Pid, Selector, Outcome, Start, Meta) ->
    Duration = erlang:monotonic_time(native) - Start,
    beamtalk_actor_stats:record_agg(Pid, Selector, Outcome, Duration),
    case persistent_term:get(beamtalk_tracing_enabled, false) of
        true ->
            Class = get_class_for_pid(Pid),
            beamtalk_actor_stats:record_trace(Pid, Class, Selector, Outcome, Duration, Meta);
        false -> ok
    end.
```

- `async_send/4` — record at send time with outcome=async
- `cast_send/3` — record at send time with outcome=cast
- Lifecycle methods (isAlive, stop, pid, etc.) — NOT instrumented (handled locally)

### Layer 3: REPL Ops — beamtalk_repl_ops_perf.erl

| Op | Params | Description |
|----|--------|-------------|
| `enable-tracing` | `buffer_size` (optional, default 100000) | Start capturing trace events |
| `disable-tracing` | — | Stop capturing |
| `get-traces` | `actor` (optional PID), `selector` (optional), `limit` (default 1000) | Get trace events as JSON array |
| `actor-stats` | `actor` (optional PID) | Aggregate stats per method |
| `actor-health` | `actor` (required PID) | Live process info |
| `bottlenecks` | `limit` (default 10) | Actors with largest message queues |
| `slow-methods` | `limit` (default 10) | Methods with highest avg latency |
| `error-methods` | `limit` (default 10) | Methods with highest error/timeout rates |
| `system-health` | — | VM stats: schedulers, memory, process count, run queues |
| `clear-traces` | — | Clear all collected data |

### Layer 4: MCP Tools

**enable_tracing / disable_tracing** — Toggle trace event capture.

**get_traces** — Returns structured JSON:

```json
{
  "trace_count": 4523,
  "returned": 1000,
  "events": [
    {
      "timestamp_us": 1234567890,
      "actor": "<0.456.0>",
      "class": "Counter",
      "selector": "increment",
      "duration_us": 42,
      "outcome": "ok"
    }
  ]
}
```

**actor_stats** — Per-method aggregates:

```json
{
  "actors": {
    "<0.456.0>": {
      "class": "Counter",
      "methods": {
        "increment": {
          "calls": 1000, "ok": 998, "errors": 1, "timeouts": 1,
          "avg_us": 42, "min_us": 12, "max_us": 5023
        }
      }
    }
  }
}
```

**actor_health** — Live process info (queue depth, memory, reductions, status).

**bottlenecks** — Top N actors by message queue length.

**slow_methods** — Top N methods by average latency.

**system_health** — VM overview (schedulers, memory, run queues).

### Files to Create

- `runtime/apps/beamtalk_runtime/src/beamtalk_actor_stats.erl` — Trace + aggregate collection
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_perf.erl` — REPL op handlers

### Files to Modify

- `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` — Instrument sync_send, async_send, cast_send
- `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_sup.erl` — Add beamtalk_actor_stats to supervision tree
- `runtime/apps/beamtalk_runtime/src/beamtalk_runtime.app.src` — Add module to app
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl` — Register perf ops in handle_op
- `runtime/apps/beamtalk_workspace/src/beamtalk_repl_protocol.erl` — Add encoding functions
- `crates/beamtalk-mcp/src/server.rs` — Add 7-8 new MCP tools

### Implementation Order

1. `beamtalk_actor_stats.erl` — Core module: ETS tables, record/query API
2. Instrument `beamtalk_actor.erl` — Wire up send wrappers
3. Supervision — Add to runtime sup tree
4. `beamtalk_repl_ops_perf.erl` + protocol encoding — REPL ops
5. Wire into `beamtalk_repl_server.erl` — Register ops
6. MCP tools in `server.rs` — Rust MCP layer
7. Tests — Unit tests for stats module, integration test for MCP workflow

### Key ADR Questions to Address

- Where to instrument: send wrappers (beamtalk_actor.erl) vs codegen (safe_dispatch) vs both
- Storage: ETS tables for trace events and aggregates, ring buffer sizing, cleanup policy
- Overhead budget: always-on aggregates (~200ns/call) vs opt-in trace events (~600ns/call)
- Toggle mechanism: persistent_term for zero-cost check when tracing is off
- MCP tool surface: which tools to expose
- Data model: what fields per trace event, what aggregates to maintain
- Integration with existing test tool: how traces correlate with test runs
- Alternatives considered: OpenTelemetry, telemetry library, sys module, pure codegen approach

### Future Extensions (not in scope)

- Flame graph generation from trace data
- Distributed tracing across nodes
- Automatic anomaly detection
- Beamtalk-level API (`Tracer enable. Tracer traces.`)
- Per-test trace isolation (tag traces with test name)

## Acceptance Criteria

- [ ] ADR follows project ADR template in `docs/ADR/`
- [ ] Documents the instrumentation approach (send wrapper vs codegen)
- [ ] Documents the data model for trace events and aggregates
- [ ] Documents the MCP tool surface and agent workflow
- [ ] Documents overhead analysis with benchmarks or estimates
- [ ] Documents alternatives considered
- [ ] Documents toggle/configuration mechanism
- [ ] Addresses cleanup/lifecycle (actor death, buffer limits)
- [ ] References existing infrastructure

## Dependencies

None

## References

- Design plan: `plan.md` on branch `claude/erlang-genserver-tracing-DfzoF`
- `beamtalk_actor.erl` sync_send/3, async_send/4, cast_send/3
- MCP server: `crates/beamtalk-mcp/src/server.rs`
- REPL ops pattern: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_actors.erl`
- Existing perf tests: `runtime/perf/beamtalk_perf_tests.erl`
