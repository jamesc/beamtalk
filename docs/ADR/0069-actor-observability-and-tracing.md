# ADR 0069: Actor Observability and Tracing

## Status
Accepted (2026-03-21)

## Context

### Problem

Beamtalk actors (gen_server processes) have no built-in performance instrumentation. When debugging timeouts, bottlenecks, or failures, developers must use ad-hoc Erlang tools (`sys`, `recon`, `dbg`) manually — tools that require knowing internal module names and produce unstructured output. AI agents working via MCP have no way to understand actor performance characteristics at all.

The core workflow we need to enable:

```
enable tracing → run tests → get structured traces → find bottlenecks → debug
```

This must work for both human developers in the REPL and AI agents via MCP tools.

### Current State

**ADR 0064** (Runtime Logging Control) established `Beamtalk enableDebug:` for per-class/per-actor log filtering — visibility into *what's happening*. This ADR addresses the complementary concern: *how fast is it happening* — timing, call counts, error rates, bottleneck detection. This was explicitly deferred as BT-1429 during ADR 0064's design.

**Existing instrumentation in `beamtalk_actor.erl`:**
- `?LOG_DEBUG("Actor dispatch (sync/async)", ...)` — logs dispatch start with class, selector, caller
- `log_dispatch_complete(...)` — logs completion with `duration_us`, `changed_keys`, mode
- These produce unstructured log messages — not queryable, not aggregatable, not available via MCP

**OTP provides the building blocks** but they're not exposed:
- `sys:trace/2` — per-process debug output to stdout (text, not structured)
- `sys:statistics/2` — basic counters (reductions, messages_in, messages_out)
- `erlang:process_info/2` — message queue length, memory, status (point-in-time snapshot)

**No Beamtalk-level API exists** — there is no tracing class, no `profile:` method, nothing discoverable from the REPL.

### Constraints

- Must expose a **Beamtalk-level API** — MCP tools wrap thin Erlang shims, following the established pattern (System, Logger, Workspace)
- Must not overload `Beamtalk` or `Object` with additional methods
- Always-on aggregate stats must have negligible overhead (~150ns/call budget)
- Trace event capture must be opt-in with zero cost when disabled
- Data must persist after actor death (the test workflow: enable → run tests → actors die → query results)
- Should use standards-based telemetry infrastructure where viable, not reinvent the wheel
- Must produce structured output consumable by both REPL exploration and MCP JSON responses

## Decision

### Class Design: `Tracing` as a Standalone Observability Facade

Tracing is exposed via a dedicated `Tracing` class — a sealed, class-only facade following the System/Logger pattern.

**Why a standalone class, not methods on `Beamtalk`?**

ADR 0064 placed logging *configuration* on `Beamtalk`, arguing that runtime configuration belongs on the system facade (DDD boundary: Logger is stdlib, logging config is Runtime Context). That decision was sound for a small surface (~5 methods) that naturally extended `Beamtalk`'s existing system introspection role.

Tracing is different in scale: ~15 methods spanning control, queries, analysis, and live health. Adding these to `Beamtalk` would double its method count and blur its identity from "system introspection" to "system introspection + performance telemetry." The Smalltalk precedent confirms this separation — Pharo's `MessageTally` and VisualWorks' `PerformanceProfiler` are standalone classes, not methods on the `Smalltalk` global. Profiling has always been a distinct tool in Smalltalk environments.

**Why `Tracing`, not `Tracer`?**

The gerund form (`Tracing`) names the *subsystem*, not the *agent*. This establishes a naming convention for observability facades: `Tracing` for performance telemetry, with a future `Logging` class planned to absorb the logging configuration methods currently on `Beamtalk` (`logLevel:`, `enableDebug:`, `debugTargets`, etc.). The pair `Logging` / `Tracing` reads as two sibling observability subsystems, each owning its full surface. `Logger` remains the emit interface (`Logger info:`, `Logger error:`), consistent with ADR 0064's emit/config separation.

This gives us optionality — the `Logging` migration is a separate future ADR, not a prerequisite for this work. But naming `Tracing` now signals the direction and avoids a rename later.

### Architecture: `telemetry` event bus + lock-free storage

Use the Erlang [`telemetry`](https://github.com/beam-telemetry/telemetry) library as the event bus, with lock-free storage for aggregation and direct ETS writes for trace capture. The `beamtalk_trace_store` gen_server owns the tables and handles queries/lifecycle but is **not in the write path** — no serialization bottleneck on the hot path.

```
Beamtalk API (Tracing class — stdlib/src/Tracing.bt)
    ↓ delegates to
Erlang shim (beamtalk_tracing.erl — runtime)
    ↓ queries
Storage:
  counters (lock-free aggregates, ~50ns writes)
  ETS index ({Pid, Selector} → counter ref + slot)
  ETS ordered_set (trace events, direct insert)
    ↑ all writes happen in the CALLING process (no gen_server in write path)
beamtalk_trace_store gen_server:
  - owns tables, handles enable/disable/clear/queries
  - periodic sweep for ring buffer eviction
  - NOT in the write path
    ↑ events emitted by
Instrumented send wrappers (beamtalk_actor.erl)
    ↑ using
telemetry:execute/3 (standard BEAM event bus)
    ↑ VM measurements via
telemetry_poller (periodic VM stats for systemHealth)
```

**Design principle: no bottlenecks on the hot path.** Aggregate updates use the `counters` module (truly lock-free, ~50ns). Trace event inserts go directly to ETS from the calling process. The gen_server only handles control operations (enable/disable/clear) and periodic maintenance (ring buffer eviction). A crash of the gen_server loses accumulated data but does not affect actor dispatch.

**Why `telemetry` + `telemetry_poller`?**
- `telemetry`: Single dependency, zero transitive deps, pure Erlang (~500 LOC). De facto BEAM ecosystem standard (Phoenix, Ecto, Broadway). Composable at the Erlang layer — system developers familiar with BEAM internals can attach custom handlers in Erlang (export to StatsD, Datadog, Prometheus) via `telemetry:attach/4` without modifying Beamtalk internals. This is an escape hatch for operations/infrastructure teams, not a general Beamtalk developer concern. Critically, adopting `telemetry` now means the OpenTelemetry upgrade is purely additive — users add the `opentelemetry_telemetry` bridge package to their project deps, and correlated distributed traces work immediately with no Beamtalk changes (see "Propagated Context Across Actor Boundaries").
- `telemetry_poller`: Periodic VM measurements (scheduler utilization, memory, run queues) — eliminates custom implementation for `Tracing systemHealth`. Pure Erlang, zero transitive deps beyond `telemetry`.

**Why not OpenTelemetry directly?**
- 6-10+ transitive dependencies (gRPC stack, HTTP/2, protobuf)
- ~10-50μs per span (vs ~1-5μs for telemetry dispatch) — 10x overhead
- Designed for distributed export, not local REPL querying — no built-in "queryable in-memory store"
- Erlang SDK is secondary to Elixir in practice — documentation and examples lag
- Available as an add-on via the telemetry bridge when needed

**Why not `telemetry_metrics`?**
- Provides declarative metric definitions (counter, summary, last_value) but no storage — requires a "reporter" to consume the definitions
- No existing reporter does "in-memory queryable store" (reporters export to Prometheus, StatsD, etc.)
- We would write a custom reporter, which is essentially our `beamtalk_trace_store` conforming to an additional interface — overhead without payoff for v1
- Deferred: design aggregates so they *could* be wrapped in a `telemetry_metrics` reporter later

### Telemetry Events

Beamtalk emits telemetry events from the actor send wrappers in `beamtalk_actor.erl`:

```erlang
%% Event: [beamtalk, actor, dispatch, start]
%% Measurements: #{system_time => erlang:system_time()}
%% Metadata: #{pid => Pid, class => Class, selector => Selector, mode => sync | async | cast}

%% Event: [beamtalk, actor, dispatch, stop]
%% Measurements: #{duration => NativeTime}
%% Metadata: #{pid => Pid, class => Class, selector => Selector, mode => sync | async | cast, outcome => ok | error | timeout}

%% Event: [beamtalk, actor, dispatch, exception]
%% Measurements: #{duration => NativeTime}
%% Metadata: #{pid => Pid, class => Class, selector => Selector, mode => sync | async | cast, kind => Kind, reason => Reason, stacktrace => Stack}
```

These follow the [`telemetry:span/3`](https://hexdocs.pm/telemetry/telemetry.html#span/3) naming convention (`[prefix, ..., start | stop | exception]`), making them compatible with standard BEAM tooling.

### Instrumentation Point: Send Wrappers

Instrument `sync_send/3`, `async_send/4`, and `cast_send/3` in `beamtalk_actor.erl` — the three entry points for all actor message sends:

```erlang
sync_send(ActorPid, Selector, Args) ->
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
    telemetry:span([beamtalk, actor, dispatch], Metadata, fun() ->
        Result = do_sync_send(ActorPid, Selector, Args),
        {Result, Metadata#{outcome => ok}}
    end).
```

`telemetry:span/3` automatically emits `start`, `stop`, and `exception` events with timing. The `stop` event includes `duration` in native time units.

**Class name resolution:** The actor's class name is not accessible from the caller's process (it lives in the gen_server's state map). The `beamtalk_object_instances` ETS table already maintains a `{Class, Pid}` registry populated at actor spawn time — a reverse lookup (`ets:match` on the bag table by Pid) provides the class name. This is a single ETS read (~50-100ns), acceptable within the overhead budget. If the Pid is not found (e.g., Erlang process, not a Beamtalk actor), the class defaults to `unknown`.

**What the timing measures:** Send-wrapper instrumentation captures different things depending on the send mode:
- **`sync_send`** — **caller-perspective round-trip time**: the full duration from `gen_server:call` to reply, including message queue wait and scheduling jitter. This is NOT actor method execution time.
- **`async_send` / `cast_send`** — **dispatch-to-mailbox time**: the duration from the caller's perspective to deliver the message to the actor's mailbox. For async sends, the actual method execution happens later (resolved via Future). For cast sends, there is no response.

To distinguish queueing delay from execution time on sync calls, use `Tracing healthFor:` which reports message queue depth. A high round-trip time with a deep queue indicates queueing; a high round-trip with an empty queue indicates slow method execution.

**`telemetry:span/3` and error handling:** The existing `sync_send/3` wraps `gen_server:call` in a `try/catch` that converts `exit:{timeout, _}` and `exit:{noproc, _}` to structured `#beamtalk_error{}` records. `telemetry:span/3` catches exceptions to emit the `exception` event and reraises via `erlang:raise/3`, preserving the original exception class and reason. Phase 0 (validation spike) will verify this composition works correctly before committing to the architecture.

**Lifecycle methods** (`isAlive`, `stop`, `pid`, `monitor`, `onExit:`, `demonitor`) are NOT instrumented — they are handled locally in the send wrappers without entering the gen_server message loop.

**Codegen-level instrumentation** (wrapping `safe_dispatch` in generated code) is deferred as a future enhancement. Send-wrapper instrumentation captures the caller-perspective round-trip time, which is the actionable metric for bottleneck detection. Codegen-level instrumentation would add precise method-body timing if needed later.

### Storage: Lock-Free Aggregates + ETS Trace Buffer

`beamtalk_trace_store.erl` — a `gen_server` that owns tables/counters and handles queries/lifecycle, but is **not in the write path**.

**Aggregates: `counters` module + ETS index (always-on)**

```erlang
%% ETS index table: beamtalk_agg_index (set)
%% Key: {Pid, Selector}
%% Value: {Key, CounterRef, SlotBase}
%%   — SlotBase is the base offset into the counters array
%%   — Each key uses 6 slots: Calls, OkCount, ErrorCount, TimeoutCount, TotalDuration, reserved

%% counters reference: created by counters:new(InitialSize, [write_concurrency])
%%   — Truly lock-free writes via counters:add/3
%%   — ~50ns per increment (vs ~100-200ns for ets:update_counter)
```

- On first observation of a new `{Pid, Selector}`, allocate slots in the counter array and insert an index entry (ETS write — rare, once per unique key)
- On every subsequent dispatch, `counters:add(Ref, Slot, 1)` for the call count and outcome — truly lock-free, no per-bucket locking
- Duration tracking: `counters:add(Ref, TotalDurationSlot, Duration)` for running total. Min/max require `atomics:compare_exchange` loop (still lock-free) or deferral to query-time computation from trace events
- Persists until explicitly cleared (survives actor death for post-mortem analysis)
- Counter array can grow dynamically by creating a new `counters` reference and atomically swapping via `persistent_term`

**Trace events: ETS `ordered_set` (opt-in, direct insert)**

```erlang
%% Table: beamtalk_trace_events (ordered_set, public, {write_concurrency, true})
%% Key: {erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])}
%%   — composite key ensures uniqueness across concurrent schedulers
%% Row: {Key, Pid, Class, Selector, Mode, Duration_ns, Outcome, Metadata}
%% Outcome: ok | error | timeout | cast
%% Metadata: #{error => Term} for failures, #{} otherwise
```

- **Written directly from the calling process** — no gen_server in the write path
- Capped at 100,000 events (configurable via `Tracing maxEvents:`)
- Ring buffer eviction: the `beamtalk_trace_store` gen_server runs a periodic sweep (e.g., every 1s or triggered when a size counter exceeds threshold). Eviction deletes the oldest 10% via `ets:select_delete` on the smallest keys. The buffer may temporarily exceed `maxEvents` between sweeps — this is a soft bound, not a hard contract.
- Only populated when tracing is enabled
- Toggle via `persistent_term:put(beamtalk_tracing_enabled, true | false)` — ~5ns check

**`beamtalk_trace_store` gen_server responsibilities:**

The gen_server is the owner and lifecycle manager, NOT the write path:

| Responsibility | Hot path? | Notes |
|---------------|-----------|-------|
| Own ETS tables + counter refs | No | Created in `init/1` |
| `enable` / `disable` / `clear` | No | Control operations, infrequent |
| Periodic ring buffer eviction | No | Timer-based sweep, async |
| Query: `traces`, `stats`, `slowMethods:` | No | Read-only, called from REPL/MCP |
| Crash recovery | No | ETS tables survive via `heir` option (ownership transfers to supervisor); counters refs stored in `persistent_term` survive. Gen_server restart inherits existing data. |

**Overhead budget:**

| Operation | Cost | When |
|-----------|------|------|
| `telemetry` handler dispatch | ~10-50ns | Always |
| `counters:add/3` (aggregate increment) | ~50ns | Always |
| `persistent_term` check (trace enabled?) | ~5ns | Always |
| Class lookup (`beamtalk_object_instances`) | ~50-100ns | Always |
| ETS `insert` (trace event) | ~300-500ns | Only when tracing enabled |
| **Total per-call** | **~150ns (tracing off) / ~500ns (tracing on)** | |

### Beamtalk API: `Tracing` Class

```beamtalk
sealed Object subclass: Tracing

  // === Tracing lifecycle ===

  class sealed enable -> Nil
  // Start capturing trace events (aggregates are always on)
  // Tracing enable

  class sealed disable -> Nil
  // Stop capturing trace events
  // Tracing disable

  class sealed isEnabled -> Boolean
  // Check whether trace capture is active
  // Tracing isEnabled   // => true

  class sealed clear -> Nil
  // Clear all trace events and aggregate stats
  // Tracing clear

  // === Trace event queries ===

  class sealed traces -> List
  // All captured trace events (up to buffer limit), newest first
  // Tracing traces

  class sealed tracesFor: actor :: Object -> List
  // Trace events for a specific actor
  // Tracing tracesFor: myCounter

  class sealed tracesFor: actor :: Object selector: sel :: Symbol -> List
  // Trace events for a specific actor + method
  // Tracing tracesFor: myCounter selector: #increment

  // === Aggregate stats ===

  class sealed stats -> Dictionary
  // Per-actor, per-method aggregate stats (always available, even without tracing)
  // Tracing stats

  class sealed statsFor: actor :: Object -> Dictionary
  // Aggregate stats for a specific actor
  // Tracing statsFor: myCounter

  // === Analysis (computed from aggregates) ===

  class sealed slowMethods: limit :: Integer -> List
  // Top N methods by average duration (descending)
  // Tracing slowMethods: 10

  class sealed hotMethods: limit :: Integer -> List
  // Top N methods by call count (descending)
  // Tracing hotMethods: 10

  class sealed errorMethods: limit :: Integer -> List
  // Top N methods by error + timeout rate (descending)
  // Tracing errorMethods: 5

  class sealed bottlenecks: limit :: Integer -> List
  // Top N actors by message queue length — live snapshot via erlang:process_info/2
  // Tracing bottlenecks: 5

  // === Live process health ===

  class sealed healthFor: actor :: Object -> Dictionary
  // Live process info: queue depth, memory, reductions, status, uptime
  // Tracing healthFor: myCounter

  class sealed systemHealth -> Dictionary
  // VM overview: scheduler count, memory breakdown, process count, run queue lengths
  // Tracing systemHealth

  // === Configuration ===

  class sealed maxEvents -> Integer
  // Current ring buffer capacity
  // Tracing maxEvents   // => 100000

  class sealed maxEvents: size :: Integer -> Nil
  // Set ring buffer capacity (clears existing events)
  // Tracing maxEvents: 50000
```

### REPL Session

```beamtalk
// Always-on aggregates — no setup needed
c := Counter spawn.
1 to: 100 do: [:i | c increment].
Tracing stats
// => {"<0.456.0>" => {"class" => "Counter", "methods" => {"increment" => {"calls" => 100, "ok" => 100, "errors" => 0, "avg_us" => 38, "min_us" => 12, "max_us" => 523}}}}

// Enable trace capture for detailed events
Tracing enable.
c increment.
c increment.
c increment.
Tracing traces
// => [{"timestamp_us" => 1234567890, "actor" => "<0.456.0>", "class" => "Counter", "selector" => "increment", "duration_us" => 42, "outcome" => "ok"}, ...]

// Find bottlenecks
Tracing slowMethods: 5
// => [{"class" => "DatabasePool", "selector" => "query:", "avg_us" => 15023, "calls" => 47}, ...]

Tracing bottlenecks: 3
// => [{"actor" => "<0.789.0>", "class" => "WorkQueue", "queue_depth" => 142, "memory_kb" => 256}, ...]

// Live health check
Tracing healthFor: c
// => {"pid" => "<0.456.0>", "class" => "Counter", "status" => "waiting", "queue_depth" => 0, "memory_kb" => 4, "reductions" => 1523, "uptime_ms" => 45000}

// Clean up
Tracing disable.
Tracing clear.
```

### Error Behavior

```beamtalk
// Querying traces when not enabled — returns empty list (not an error)
Tracing traces   // => []

// Stats for unknown actor — returns empty dictionary
Tracing statsFor: somethingInvalid   // => {}

// Health for dead actor
Tracing healthFor: deadActor
// => {"pid" => "<0.456.0>", "class" => "Counter", "status" => "dead", "error" => "process not alive"}
```

### MCP Tools: Lean Surface + `evaluate`

The existing MCP `evaluate` tool can execute any Beamtalk expression, including `Tracing` methods. Rather than 10 dedicated MCP tools that are thin wrappers around `evaluate("Tracing ...")`, we start with a minimal dedicated tool surface and let agents discover the rest via `evaluate` + `Beamtalk help: Tracing`:

**Dedicated MCP tools (v1):**

| MCP Tool | Beamtalk API | Why dedicated? |
|----------|-------------|----------------|
| `enable-tracing` | `Tracing enable` | Common workflow entry point, high discoverability value |
| `get-traces` | `Tracing traces` / `tracesFor:` | Structured JSON output with filtering params (`actor`, `selector`, `limit`) |
| `actor-stats` | `Tracing stats` / `statsFor:` | Structured JSON output, the main query after test runs |

**Everything else via `evaluate`:**

```
evaluate("Tracing slowMethods: 10")
evaluate("Tracing bottlenecks: 5")
evaluate("Tracing healthFor: myCounter")
evaluate("Tracing systemHealth")
evaluate("Tracing disable")
evaluate("Tracing clear")
evaluate("Beamtalk help: Tracing")   // agent discovers available methods
```

This approach reduces implementation effort (3 tools vs 10), avoids API surface bloat in the MCP layer, and tests whether agents can discover the full `Tracing` API organically. If monitoring shows agents struggle to find specific methods, we can promote them to dedicated tools in a follow-up — additive, not a redesign.

### Agent Workflow

```
1. agent calls enable-tracing          → dedicated tool
2. agent calls test (existing tool)    → runs BUnit suite, actors spawn and die
3. agent calls get-traces              → dedicated tool (structured JSON)
4. agent calls actor-stats             → dedicated tool (structured JSON)
5. agent calls evaluate("Tracing slowMethods: 10")   → via evaluate
6. agent calls evaluate("Tracing healthFor: ...")     → via evaluate
7. agent calls evaluate("Tracing disable")            → via evaluate
8. agent calls evaluate("Tracing clear")              → via evaluate
```

### Cleanup and Lifecycle

- **Trace events**: Bounded by ring buffer (100k default). Oldest dropped on overflow via batch eviction. Explicit `Tracing clear` resets.
- **Aggregate stats**: Persist until `Tracing clear`. Size is proportional to unique `{Pid, Selector}` combinations, not call volume — bounded by the number of actors × methods seen.
- **Actor death**: Stats and traces for dead actors are **retained** for post-mortem analysis. This is essential for the test workflow where actors spawn, run, and die before results are queried.
- **`beamtalk_trace_store` gen_server**: Supervised under `beamtalk_runtime_sup`. Owns ETS tables (created with `{heir, SupervisorPid, HeirData}`) and counter references (stored in `persistent_term`). On gen_server crash: ETS ownership transfers to the supervisor via the `heir` mechanism, counters survive in `persistent_term`. The restarted gen_server inherits the existing tables — **no data loss on process crash**. Only a full VM crash (SIGKILL, OOM) loses data.

### Future: Block-Scoped Profiling

The Smalltalk `MessageTally spyOn: [block]` pattern is compelling for targeted profiling. A future enhancement could add:

```beamtalk
Tracing profile: [
  c := Counter spawn.
  1 to: 1000 do: [:i | c increment].
]
// => {"traces" => [...], "stats" => {...}, "duration_us" => 45230}
```

This would automatically enable tracing, execute the block, disable tracing, and return the captured results — eliminating the enable/disable lifecycle for the common case. The global mode (`Tracing enable`) remains necessary for the MCP agent workflow (enable → run tests in separate tool call → query results).

### Propagated Context Across Actor Boundaries

Actor messages carry a **propagated context map** as a third element: `{Selector, Args, PropagatedCtx}`. This context flows automatically through every `sync_send`, `async_send`, and `cast_send` — the user never sees it.

```erlang
%% In beamtalk_actor.erl — send side
get_propagated_ctx() ->
    #{otel => get_otel_ctx()}.   %% extensible map — future keys added here

get_otel_ctx() ->
    case erlang:function_exported(otel_ctx, get_current, 0) of
        true -> otel_ctx:get_current();
        false -> undefined
    end.

%% In generated handle_call — actor side
handle_call({Selector, Args, PropCtx}, _From, State) ->
    restore_propagated_ctx(PropCtx),
    dispatch(Selector, Args, State).

restore_propagated_ctx(#{otel := Ctx}) when Ctx =/= undefined ->
    otel_ctx:attach(Ctx);   %% only called if otel_ctx module is loaded
restore_propagated_ctx(_) -> ok.
```

**No compile-time dependency on OpenTelemetry.** `erlang:function_exported/3` is cached by the VM (~10ns). When OTel isn't loaded, the context map contains `#{otel => undefined}` — restoration is a no-op pattern match.

**Cost:**

| Operation | Cost | When |
|-----------|------|------|
| `function_exported` check | ~10ns | Always |
| Context map construction | ~10ns | Always |
| Extra bytes in message copy | ~25 bytes | Always |
| `otel_ctx:attach/1` | ~20ns | Only when OTel is loaded |
| **Total** | **~20ns (no OTel) / ~40ns (with OTel)** | Negligible vs gen_server call (~5-10μs) |

**User experience:** Add `opentelemetry` + `opentelemetry_telemetry` to your project's deps → correlated parent/child traces across actor calls work immediately. No Beamtalk upgrade, no recompile. The `telemetry` bridge auto-subscribes to our events, and the propagated context provides the parent/child span correlation.

**Extensible context map — future uses:**

The `PropagatedCtx` map is designed to carry more than OTel context. Future extensions (each a separate ADR):

| Key | What it carries | Use case |
|-----|----------------|----------|
| `otel` | OTel trace context | Distributed tracing (this ADR) |
| `request_id` | Correlation ID (string) | Log correlation — `Logger` auto-includes the ID in all logs from downstream actor calls, without the user threading it manually |
| `deadline` | Monotonic time of expiry | Timeout propagation — like Go's `context.WithTimeout`. If A→B has a 5s timeout and B takes 3s, B→C automatically gets 2s |
| `causality` | Logical clock / vector clock | Debugging message ordering in concurrent actor systems |

The map starts with just `otel` in v1. Adding keys is additive — no message format change, no codegen change, no recompile.

**Why `telemetry` matters for this path:** Without `telemetry`, exporting spans requires coupling `beamtalk_actor.erl` directly to the OTel SDK. With `telemetry`, the OTel bridge is a user-side dependency that auto-subscribes to our events — zero changes to Beamtalk. This is the primary justification for adopting `telemetry` now rather than deferring it.

## Prior Art

### Erlang/OTP — sys module
Every OTP behaviour supports `sys:trace(Pid, true)` for per-process debug output and `sys:statistics(Pid, true)` for basic counters (reductions, messages_in, messages_out). The output is unstructured text to stdout — useful for quick debugging but not queryable or aggregatable. The `dbg` module provides lower-level function tracing with pattern matching. Observer adds a GUI. `recon` (third-party) adds production-safe rate-limited tracing and process ranking by message queue length, reductions, or memory.

**What we adopt:** The concept of always-available aggregate stats (like `sys:statistics`) and opt-in detailed tracing (like `sys:trace`). **What we improve:** Structured output (queryable dictionaries, not stdout text), Beamtalk-level API (no need to know Erlang module names), and MCP exposure for agent consumption.

### Elixir — telemetry ecosystem
Elixir's `telemetry` library is the de facto BEAM standard for instrumented observability. Libraries emit events via `telemetry:execute/3`; applications attach handlers. Phoenix LiveDashboard provides a web UI showing real-time metrics. GenServer itself does not auto-emit telemetry events — libraries add instrumentation explicitly.

**What we adopt:** The `telemetry` library as our event bus, following the same event naming conventions (`[prefix, object, action, phase]`). **What we adapt:** Beamtalk instruments at the runtime level (all actor sends emit events automatically) rather than requiring library authors to add instrumentation manually. Our `Tracing` class provides the query API that the telemetry ecosystem leaves to third-party dashboards.

### Akka — Cinnamon/Insights
Akka Insights provides per-actor-class instrumentation via configuration (no code changes). Four key metrics: mailbox size, mailbox time (queue wait), processing time, and stash size. Configuration supports per-class, per-instance, and wildcard selection with threshold alerts.

**What we adopt:** The four-metric model (our aggregates track call count, duration, errors, timeouts — analogous). Always-on aggregates with opt-in detailed tracing matches Akka's approach. **What we differ on:** Akka's instrumentation is configuration-driven and commercial. Ours is API-driven (`Tracing enable`) and built-in.

### Pharo Smalltalk — MessageTally
Pharo's `MessageTally spyOn: [block]` profiles a block expression, producing a hierarchical tree of methods with execution time percentages. Research by Bergel et al. showed that counting message sends (not sampling) produces perfectly stable profiles. Deeply integrated into the IDE — right-click to profile, visual hierarchy browser. VisualWorks' `PerformanceProfiler` follows a similar pattern as a standalone class-side singleton.

**What we adopt:** The standalone profiler class pattern — both Pharo (`MessageTally`) and VisualWorks (`PerformanceProfiler`) keep profiling as a dedicated tool, not methods on the `Smalltalk` global. This validates our `Tracing` class design. **What we defer:** The block-scoped `profile: [block]` invocation pattern is planned as a future enhancement (see Decision section). Our v1 uses global enable/disable because the MCP agent workflow requires tracing to span multiple tool calls.

### Pony — Flight Recorder
Pony's runtime supports a **flight recorder** mode: a circular in-memory buffer that captures trace events continuously with low overhead and dumps to stderr on crash (SIGILL/SIGSEGV/SIGBUS). This provides post-mortem debugging data without the overhead of continuous export.

**What we learn:** Our ring buffer design is essentially a flight recorder. The Pony model of "always capturing, only materializing on demand" aligns with our approach of always-on aggregates + opt-in trace capture.

### BEAM Telemetry Ecosystem — In-Memory Storage
The pattern of "telemetry handler → ETS store → query API" is standard on BEAM — every application that needs local metrics does it, typically in ~20-30 lines. No one packages it as a general-purpose library because the query API is always application-specific. Several projects demonstrate the pattern:

- **`Mobius`** (Elixir, [github.com/mattludwigs/mobius](https://github.com/mattludwigs/mobius)) — ETS circular buffer with `Mobius.query/1` for historical metric retrieval. Designed for Nerves (resource-constrained devices). The closest analogue to our design — same pattern, different data model.
- **`peep`** (Elixir, [github.com/rkallos/peep](https://github.com/rkallos/peep)) — High-performance `telemetry_metrics` reporter using `atomics`/`persistent_term`. Validates our `counters`-based approach for lock-free aggregation.
- **`telemetry_metrics_prometheus_core`** (Erlang) — Stores aggregated metrics in ETS for Prometheus scraping. Pure Erlang, same handler-to-ETS pattern.
- **Phoenix LiveDashboard** — Stores recent data points in a GenServer's process state (small circular buffer) for real-time charting.

**What we learn:** The storage pattern is well-established on BEAM. What's specific to Beamtalk is the data model (actor-scoped traces + per-method aggregates), the query API (`slowMethods:`, `bottlenecks:`), and the Beamtalk class wrapping it for REPL + MCP consumption. `peep`'s success with `atomics`/`counters` validates our lock-free aggregate design.

## User Impact

### Newcomer (from Python/JS/Ruby)
- `Tracing stats` and `Tracing slowMethods: 5` are immediately discoverable and self-explanatory
- No setup required for aggregate stats — they just work
- `Tracing enable` / `Tracing traces` is a simple workflow, no OTP concepts needed
- The REPL output format (dictionaries with string keys) is familiar from JSON

### Smalltalk Developer
- `Tracing` as a standalone class follows the Smalltalk tradition of separate profiler tools (`MessageTally`, `PerformanceProfiler`)
- Future `Tracing profile: [block]` would mirror Pharo's `MessageTally spyOn:` — familiar territory
- The class does not pollute `Object` or `Beamtalk` — clean separation of concerns
- The `Tracing` / `Logging` naming convention (gerund-form subsystems) parallels how Smalltalk organizes system tools as separate classes

### Erlang/BEAM Developer
- The `telemetry` event bus is the standard they expect — compatible with their existing tooling
- `telemetry:attach/4` lets them add custom handlers (StatsD, Datadog) at the Erlang level
- `Tracing healthFor:` wraps `erlang:process_info/2` — familiar data, Beamtalk syntax
- OpenTelemetry integration is available via the standard `opentelemetry_telemetry` bridge

### Production Operator
- Always-on aggregates with ~150ns overhead are safe for production (lock-free `counters` module)
- Trace capture is opt-in — zero overhead when disabled
- `Tracing systemHealth` provides the VM overview they need for capacity planning
- Structured output integrates with existing monitoring pipelines via custom telemetry handlers
- Ring buffer prevents unbounded memory growth

## Steelman Analysis

### Alternative A: Custom ETS-Only (no telemetry dependency)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "Zero dependencies means nothing can break. `Tracing enable` just works — no library version to worry about." |
| **Smalltalk purist** | "ETS and `counters` are already part of OTP — the runtime we ship with. Adding `telemetry` means core observability depends on a third-party library's release cycle. OTP primitives won't break on a hex version bump." |
| **BEAM veteran** | "You've already built the hard part — context propagation across gen_server boundaries. That's the real work. The `telemetry` event bus is just a dispatch layer I could replace with a direct function call. If a user wants OTel export, they can write a 20-line handler on your ETS tables. `telemetry` is a convenience, not a necessity." |
| **Operator** | "Fewer moving parts in production. I can inspect the ETS tables directly with `ets:tab2list/1` — no abstraction hiding the data." |
| **Language designer** | "The `persistent_term` toggle pattern is simpler and faster than telemetry's handler dispatch. We're optimizing for one specific use case, not building a general-purpose event bus." |

### Alternative B: OpenTelemetry-native

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "OpenTelemetry is the industry standard I learned in my distributed systems class. I can export Beamtalk traces to Jaeger and see them in the same dashboard as my Go microservices." |
| **Smalltalk purist** | "If you're going to add an external dependency anyway, pick the one that gives you distributed tracing. `telemetry` is a half-measure — a BEAM-only event bus that still needs a custom store. OTel is the complete package." |
| **BEAM veteran** | "If we're going to add observability, do it once with the standard. `telemetry` is BEAM-only — OpenTelemetry gives us cross-service distributed tracing from day one." |
| **Operator** | "I already have OTel collectors running in production. One more service that speaks OTLP is zero ops effort. A custom ETS store is one more bespoke thing I have to learn and monitor." |
| **Language designer** | "OTel's span model gives you parent/child tracing across actor calls for free — if Counter.increment calls DatabasePool.query:, the span hierarchy shows the causal chain. `telemetry` events are flat; you'd have to build correlation yourself." |

### Alternative C: Tracing methods on `Beamtalk` (ADR 0064 pattern)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "One place for system tools. I already know `Beamtalk logLevel:` and `Beamtalk enableDebug:` — `Beamtalk enableTracing` is the obvious next thing I'd try." |
| **Smalltalk purist** | "Smalltalk's `Smalltalk` global is the ONE place you go for system tools. That's the beauty — one entry point, everything discoverable via `help:`. Splitting across classes means I have to know `Tracing` exists." |
| **BEAM veteran** | "DDD consistency. ADR 0064 said runtime configuration goes on `Beamtalk`. Tracing enable/disable is runtime configuration. Splitting it to a new class contradicts the precedent." |
| **Operator** | "One object to query from MCP. `Beamtalk` is already exposed — I don't need to discover that `Tracing` exists as a separate entry point." |
| **Language designer** | "Fewer top-level names in the namespace. `Beamtalk` is already known; `Tracing` is a new name to discover. Discoverability is better when tools are co-located." |

### Decided: `telemetry` + `Tracing` class (hybrid)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "The Beamtalk API is all I see — `Tracing enable`, `Tracing stats`. The telemetry library is invisible to me." |
| **Smalltalk purist** | "`MessageTally` is a standalone class in Pharo; `Tracing` is a standalone class here. This is the Smalltalk way — profiling tools are their own thing, not bolted onto the system dictionary." |
| **BEAM veteran** | "`telemetry` events mean I can `telemetry:attach/4` my own handlers from the Erlang shell — export to Prometheus, StatsD, whatever. Context propagation across actor boundaries means `opentelemetry_telemetry` gives me correlated distributed traces just by adding the dep. No other BEAM framework does this out of the box." |
| **Operator** | "Best of both worlds: local ETS queries for REPL debugging, composable handlers for production export, and an OTel upgrade path." |
| **Language designer** | "The `Tracing` / `Logging` pair is a cleaner taxonomy than everything on `Beamtalk`. Each subsystem owns its full surface. This scales — a future `Metrics` class has an obvious home." |

### Tension Points

- **ADR 0064 consistency:** BEAM veterans and newcomers have a genuine argument that `Tracing enable` should live on `Beamtalk` per the ADR 0064 DDD precedent. The counter-argument is scale: logging config is ~5 methods, tracing is ~15. At that size, a standalone class is the Smalltalk-native solution. The planned `Logging` migration will retroactively make both subsystems consistent. **Discoverability escape hatch:** if users struggle to find `Tracing`, we can add `Beamtalk tracing` (returns the `Tracing` singleton) and later `Beamtalk logging` — namespace delegation via one method per subsystem, not method duplication.
- **BEAM veterans** would prefer OpenTelemetry-native for distributed tracing. The bridge via `opentelemetry_telemetry` is a good compromise but adds one more package when they need it.
- **Purists** would prefer zero dependencies. The argument is genuine — `telemetry` is tiny and stable, but it IS a dependency. The composability benefit (user-added handlers, OTel bridge) justifies the cost.
- **All cohorts agree** on: always-on aggregates, opt-in detailed tracing, structured output, ring-buffer bounded storage.

## Alternatives Considered

### Alternative: Custom ETS-Only (no telemetry library)
Direct `ets:insert/2` and `ets:update_counter/3` from instrumented send wrappers, with a `persistent_term` toggle for trace capture.

**Rejected because:** Loses composability. Users cannot attach their own handlers without modifying Beamtalk internals. No bridge to OpenTelemetry. The ~50ns additional overhead from telemetry dispatch (vs direct ETS write) is negligible, while the extensibility benefit is significant. We would be reimplementing the handler dispatch that `telemetry` already provides as a battle-tested, zero-dependency library.

### Alternative: OpenTelemetry SDK
Use `opentelemetry-erlang` as the primary instrumentation layer with spans, metrics, and OTLP export.

**Rejected because:** 6-10+ transitive dependencies including a gRPC stack. ~10-50μs per span (10x the overhead of our budget). Designed for distributed export, not local REPL querying — fighting the abstraction for our primary use case. Erlang SDK documentation lags behind Go/Java/Python. Available as an add-on via the `opentelemetry_telemetry` bridge when distributed tracing is actually needed.

### Alternative: sys module integration
Use OTP's built-in `sys:trace/2` and `sys:statistics/2` for per-actor tracing.

**Rejected because:** `sys:trace` produces unstructured text to stdout — not queryable, not aggregatable, not exposable via MCP. `sys:statistics` only tracks reductions, messages_in, messages_out — no per-method breakdown, no timing, no error classification. Would require significant wrapping to produce the structured output needed by the `Tracing` API and MCP tools.

### Alternative: Tracing in codegen (safe_dispatch)
Instrument the generated `safe_dispatch` function in each actor's Core Erlang code, measuring method body execution time.

**Rejected for now:** Adds codegen complexity and requires recompilation to enable/disable. Send-wrapper instrumentation captures the caller-perspective round-trip time (including message queue wait), which is the more actionable metric for bottleneck detection. Message queue depth from `Tracing healthFor:` disambiguates queueing vs execution time. Codegen-level instrumentation remains a future enhancement if method-body timing is needed.

### Alternative: Tracing control on `Beamtalk` (ADR 0064 DDD pattern)
Place `enableTracing` / `disableTracing` / `tracingEnabled` on `Beamtalk`, with only query methods on a separate class.

**Rejected because:** Adds 3 more methods to an already-large `BeamtalkInterface` (currently ~20 methods). More importantly, it splits the tracing API across two objects — users must know that control is on `Beamtalk` but queries are on `Tracing`. A standalone class with the full surface is simpler to discover and document. The ADR 0064 precedent was sound for logging config (~5 methods extending an existing introspection role), but tracing is a large enough domain (~15 methods) to justify its own namespace. The planned future `Logging` class will retroactively make both subsystems consistent as standalone facades.

## Consequences

### Positive
- First Beamtalk-level observability API — discoverable, self-explanatory, works in REPL
- AI agents can autonomously debug performance via MCP tools
- Always-on aggregates provide baseline visibility without setup
- Standard `telemetry` event bus enables user-extensible monitoring (StatsD, Datadog, OpenTelemetry bridge)
- Ring buffer prevents unbounded memory growth
- Post-mortem analysis works — data survives actor death
- `Tracing` / `Logging` naming convention scales to future observability subsystems

### Negative
- New dependencies: `telemetry` + `telemetry_poller` (mitigated: zero transitive deps, pure Erlang, de facto BEAM standard)
- ~150ns overhead per actor message send (always-on aggregates via `counters`) — negligible but non-zero
- BEAM VM crash (SIGKILL, OOM) loses all in-memory tracing data (mitigated: tracing data is ephemeral by nature; a future enhancement could periodically checkpoint aggregates to the workspace log)
- Trace events reference PIDs which are opaque after actor death (mitigated: class name is captured at record time via `beamtalk_object_instances` lookup)
- Aggregate `{Pid, Selector}` space grows without bound in long-running sessions with actor churn (PIDs are not reused within a BEAM session). Mitigated: `Tracing clear` resets; a future enhancement could auto-evict entries for dead PIDs on a periodic sweep
- Standalone `Tracing` class is temporarily inconsistent with ADR 0064's pattern of logging config on `Beamtalk` (mitigated: planned `Logging` class migration will make both consistent)

### Neutral
- Does not affect actors that don't send messages (value objects, pure computation)
- Does not change any existing Beamtalk syntax or semantics
- The `telemetry` dependency will likely already be present if users integrate with Elixir libraries
- `Tracing` class follows the established sealed-facade pattern — no new language concepts

## Implementation

### Phase 0: Validation Spike (S)
**Affected components:** Runtime (Erlang), rebar.config

Prove the core assumptions before committing to the full architecture:

1. Add `telemetry` and `telemetry_poller` dependencies to `runtime/rebar.config`
2. Write a minimal EUnit test that wraps a `gen_server:call` with `telemetry:span/3` and verifies:
   - The `stop` event fires with correct duration
   - The `exception` event fires on timeout and preserves the `exit:{timeout, _}` shape (critical: existing `sync_send/3` error handling depends on this)
   - An attached handler can write to ETS / increment `counters` from the calling process
3. Verify `beamtalk_object_instances` reverse lookup (Pid → Class) works and measure overhead
4. Microbenchmark: `telemetry:span/3` + `counters:add/3` under the actual scheduler count — validate the ~150ns budget
5. Verify `telemetry_poller` can emit periodic VM stats (scheduler utilization, memory) as telemetry events

This spike should be a single branch with ~100 lines of test code. If the telemetry/try-catch composition breaks, fall back to direct ETS writes (the Custom ETS-Only alternative).

### Phase 1: Core Infrastructure (M)
**Affected components:** Runtime (Erlang)

1. Create `beamtalk_trace_store.erl` — gen_server owning ETS tables + `counters` refs, telemetry handler callbacks, query API, periodic eviction sweep
2. Add `beamtalk_trace_store` to `beamtalk_runtime_sup` supervision tree
3. Configure `telemetry_poller` for periodic VM measurements (feeds `Tracing systemHealth`)
4. EUnit tests for trace store: direct insert, query, ring buffer eviction sweep, `counters`-based aggregates, clear, composite key uniqueness, gen_server crash recovery

### Phase 2: Actor Instrumentation + Context Propagation (M)
**Affected components:** Runtime (`beamtalk_actor.erl`), Codegen (`callbacks.rs`)

1. Instrument `sync_send/3`, `async_send/4`, `cast_send/3` with `telemetry:span/3`
2. Wire aggregate updates (always on) and trace event capture (when enabled)
3. Add propagated context: `get_propagated_ctx()` in send wrappers, `restore_propagated_ctx/1` in runtime
4. Update codegen to generate 3-tuple `handle_call({Selector, Args, PropCtx}, ...)` with context restoration
5. EUnit tests: verify telemetry events, verify overhead budget, verify context propagation (with and without OTel module present)

### Phase 3: Beamtalk API (M)
**Affected components:** Stdlib (`Tracing.bt`), Runtime (`beamtalk_tracing.erl`)

1. Create `beamtalk_tracing.erl` — Erlang shim that the Tracing class delegates to
2. Create `Tracing.bt` — sealed class-only facade
3. BUnit tests for Tracing API: enable/disable, traces, stats, slowMethods, healthFor, systemHealth

### Phase 4: MCP Tools (S)
**Affected components:** Runtime (`beamtalk_repl_ops_perf.erl`), Rust MCP server (`server.rs`)

1. Create `beamtalk_repl_ops_perf.erl` — REPL op handlers for `enable-tracing`, `get-traces`, `actor-stats`
2. Wire ops into `beamtalk_repl_server.erl`
3. Add 3 MCP tools to `server.rs` (following existing pattern)
4. Integration tests: MCP workflow (enable-tracing → test → get-traces → actor-stats)
5. Monitor agent usage — promote additional methods to dedicated tools if discoverability is an issue

### Phase 5: Documentation and Polish (S)
1. Update `docs/beamtalk-language-features.md` with Tracing class documentation
2. Add examples to the example corpus for MCP search
3. Update ADR 0064 to cross-reference this ADR

## References
- Related issues: BT-1429 (deferred from ADR 0064)
- Related ADRs: ADR 0064 (Runtime Logging Control — complementary), ADR 0065 (OTP Primitives — Actor/Server hierarchy), ADR 0043 (Sync-by-Default Messaging)
- Code: `beamtalk_actor.erl` (sync_send, async_send, cast_send), `server.rs` (MCP tool pattern)
- External: [beam-telemetry/telemetry](https://github.com/beam-telemetry/telemetry), [beam-telemetry/telemetry_poller](https://github.com/beam-telemetry/telemetry_poller), [opentelemetry_telemetry bridge](https://github.com/open-telemetry/opentelemetry-erlang-contrib)
- Prior art: Erlang sys/recon, Elixir telemetry/LiveDashboard, Akka Insights, Pharo MessageTally/PerformanceProfiler, Pony flight recorder, Mobius (Elixir in-memory store), peep (lock-free counters)
- Design document: `issue-adr-actor-observability.md` on branch `claude/erlang-genserver-tracing-DfzoF`
