# Actor Observability for MCP Agents

## Vision
Turn on tracing → run test suite → get structured traces → agent debugs perf.

## Agent Workflow
```
1. agent calls `enable_tracing`          → starts capture
2. agent calls `test` (existing tool)    → runs BUnit suite
3. agent calls `get_traces`              → gets all captured events as structured JSON
4. agent calls `actor_stats`             → gets aggregate stats (computed from traces)
5. agent calls `bottlenecks`             → finds slow methods, high error rates
6. agent calls `actor_health pid=X`      → drills into live process state
7. agent calls `disable_tracing`         → stops capture, optionally clears buffer
```

## Architecture

```
MCP Tools (Rust)
    ↕ REPL ops over WebSocket
REPL Op Handlers (beamtalk_repl_ops_perf.erl)
    ↕
Trace Buffer (beamtalk_actor_stats.erl — gen_server + ETS)
    ↑ events written by
Instrumented Send Wrappers (beamtalk_actor.erl)
```

## Layer 1: `beamtalk_actor_stats.erl` — Trace Collection

Gen_server that owns two ETS tables:

### Table 1: `beamtalk_trace_events` (ordered_set)
Captures individual call events when tracing is enabled.

```erlang
%% Key: monotonic timestamp (unique, ordered)
%% Row: {Timestamp, Pid, Class, Selector, Duration, Outcome, Metadata}
%%
%% Outcome: ok | error | timeout | cast
%% Metadata: #{error => Term} for failures, #{} otherwise
```

### Table 2: `beamtalk_actor_agg_stats` (set)
Running aggregates, always updated (low overhead).

```erlang
%% Key: {Pid, Selector}
%% Counters: calls, ok, errors, timeouts, total_duration, min_dur, max_dur
```

### API
```erlang
%% Tracing lifecycle
-spec enable() -> ok.               %% Start capturing trace events
-spec disable() -> ok.              %% Stop capturing (keeps buffer)
-spec is_enabled() -> boolean().
-spec clear() -> ok.                %% Clear trace buffer + aggregates

%% Event recording (called from send wrappers — must be fast)
-spec record(pid(), atom(), atom(), ok | error | timeout | cast,
             integer(), map()) -> ok.
%% Args: ActorPid, Class, Selector, Outcome, DurationNative, Meta

%% Query — trace events
-spec get_traces() -> [map()].                    %% All events, oldest first
-spec get_traces(pid()) -> [map()].               %% Events for one actor
-spec get_traces(pid(), atom()) -> [map()].       %% Events for one actor+method

%% Query — aggregates
-spec get_stats() -> map().                        %% All actors
-spec get_stats(pid()) -> map().                   %% One actor
-spec hot_methods(pos_integer()) -> [map()].       %% Top N by call count
-spec slow_methods(pos_integer()) -> [map()].      %% Top N by avg duration
-spec error_methods(pos_integer()) -> [map()].     %% Top N by error rate
-spec bottlenecks(pos_integer()) -> [map()].       %% Top N by queue length (live)

%% Process health (live — wraps erlang:process_info)
-spec actor_health(pid()) -> map().
-spec system_health() -> map().
```

### Design Decisions
- **Aggregates always on** — `ets:update_counter/3` is ~100ns, negligible
- **Trace events only when enabled** — controlled via `persistent_term` for zero-cost check when off
- **Trace buffer capped** at 100k events (configurable) — ring buffer semantics, oldest dropped
- **Duration in microseconds** in output, native units internally
- **Class name captured** at record time from actor state (`$beamtalk_class`) so traces are readable

### Overhead
| Operation | Cost | When |
|-----------|------|------|
| Aggregate update | ~100-200ns | Always |
| Trace event insert | ~300-500ns | Only when enabled |
| persistent_term check | ~5ns | Always (branch on trace enabled) |
| **Total per-call** | **~200ns (off) / ~600ns (on)** | |

## Layer 2: Instrument `beamtalk_actor.erl`

### `sync_send/3` (the main dispatch path)
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
    %% Aggregates always recorded
    beamtalk_actor_stats:record_agg(Pid, Selector, Outcome, Duration),
    %% Trace events only when enabled
    case persistent_term:get(beamtalk_tracing_enabled, false) of
        true ->
            Class = get_class_for_pid(Pid),
            beamtalk_actor_stats:record_trace(Pid, Class, Selector, Outcome, Duration, Meta);
        false -> ok
    end.
```

### `async_send/4` — record at send time with outcome=`async`
### `cast_send/3` — record at send time with outcome=`cast`

### Lifecycle methods (isAlive, stop, pid, etc.) — NOT instrumented
These are handled locally, not gen_server calls. No point tracing.

## Layer 3: REPL Ops — `beamtalk_repl_ops_perf.erl`

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

## Layer 4: MCP Tools

### `enable_tracing` / `disable_tracing`
```
"Enable actor call tracing. Once enabled, all actor message sends are
captured with timing, outcome (ok/error/timeout), and caller info.
Run your test suite while tracing is enabled, then use get_traces
and actor_stats to analyze performance."
```

### `get_traces`
```
"Get captured trace events as structured JSON. Each event includes:
timestamp, actor PID, class, method selector, duration (μs),
outcome (ok/error/timeout/cast), and error details if applicable.
Returns events in chronological order. Use 'actor' param to filter
to a specific actor, 'limit' to cap results."
```

Returns:
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
    },
    {
      "timestamp_us": 1234567932,
      "actor": "<0.456.0>",
      "class": "Counter",
      "selector": "getValue",
      "duration_us": 5023,
      "outcome": "timeout",
      "error": "Actor did not respond within the timeout period..."
    }
  ]
}
```

### `actor_stats`
```
"Get aggregate call statistics for actors. Shows per-method call counts,
success/error/timeout breakdown, and latency (min/avg/max) in microseconds.
Always available (aggregates are collected even without tracing enabled)."
```

Returns:
```json
{
  "actors": {
    "<0.456.0>": {
      "class": "Counter",
      "methods": {
        "increment": {
          "calls": 1000,
          "ok": 998,
          "errors": 1,
          "timeouts": 1,
          "avg_us": 42,
          "min_us": 12,
          "max_us": 5023,
          "total_us": 42000
        }
      }
    }
  }
}
```

### `actor_health`
```
"Check a live actor's process health. Returns message queue depth,
memory usage, reduction count, process status, and current function.
High message_queue_len indicates the actor can't keep up with demand."
```

### `bottlenecks`
```
"Find actors that may be bottlenecked. Returns top N actors sorted by
message queue length, with class info and method stats summary.
Use this as a starting point to identify which actors need investigation."
```

### `slow_methods`
```
"Find the slowest actor methods by average latency. Useful for
identifying which specific operations are causing performance issues."
```

### `system_health`
```
"Get BEAM VM health overview: scheduler utilization, total process count,
memory breakdown (processes, ETS, atoms, binaries), and run queue lengths.
Use this to understand whether the system is CPU-bound, memory-bound,
or has scheduling issues."
```

## Files to Create

| File | Description |
|------|-------------|
| `runtime/apps/beamtalk_runtime/src/beamtalk_actor_stats.erl` | Trace + aggregate collection |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_perf.erl` | REPL op handlers |

## Files to Modify

| File | Change |
|------|--------|
| `runtime/apps/beamtalk_runtime/src/beamtalk_actor.erl` | Instrument sync_send, async_send, cast_send |
| `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_sup.erl` | Add beamtalk_actor_stats to supervision tree |
| `runtime/apps/beamtalk_runtime/src/beamtalk_runtime.app.src` | Add module to app |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl` | Register perf ops in handle_op |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_protocol.erl` | Add encoding functions for perf data |
| `crates/beamtalk-mcp/src/server.rs` | Add 7-8 new MCP tools |

## Implementation Order

1. **`beamtalk_actor_stats.erl`** — Core module: ETS tables, record/query API
2. **Instrument `beamtalk_actor.erl`** — Wire up send wrappers
3. **Supervision** — Add to runtime sup tree
4. **`beamtalk_repl_ops_perf.erl`** + protocol encoding — REPL ops
5. **Wire into `beamtalk_repl_server.erl`** — Register ops
6. **MCP tools in `server.rs`** — Rust MCP layer
7. **Tests** — Unit tests for stats module, integration test for MCP workflow

## Future Extensions (not in scope)
- Flame graph generation from trace data
- Distributed tracing across nodes
- Automatic anomaly detection ("this method got 10x slower")
- Beamtalk-level API (`Tracer enable. Tracer traces.`)
- Per-test trace isolation (tag traces with test name)
