# ADR 0069: Actor Observability and Tracing

## Status
Proposed (2026-03-21)

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
- Always-on aggregate stats must have negligible overhead (~200ns/call budget)
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

### Architecture: `telemetry` event bus + ETS storage handlers

Use the Erlang [`telemetry`](https://github.com/beam-telemetry/telemetry) library as the event bus, with custom ETS-backed handlers for aggregation and trace capture. This is a **hybrid approach**: standard event emission, Beamtalk-specific storage and query.

```
Beamtalk API (Tracing class — stdlib/src/Tracing.bt)
    ↓ delegates to
Erlang shim (beamtalk_tracing.erl — runtime)
    ↓ queries
ETS storage (beamtalk_trace_store.erl — telemetry handler + gen_server)
    ↑ events emitted by
Instrumented send wrappers (beamtalk_actor.erl)
    ↑ using
telemetry:execute/3 (standard BEAM event bus)
```

**Why `telemetry` over custom ETS-only?**
- Single dependency, zero transitive deps, pure Erlang (~500 LOC)
- De facto BEAM ecosystem standard (Phoenix, Ecto, Broadway all use it)
- `telemetry:persist/0` copies handlers into `persistent_term` — near-zero dispatch overhead when no handlers are attached
- Composable: users can attach their own handlers (export to StatsD, Datadog, etc.) without modifying Beamtalk internals
- Bridge to OpenTelemetry available via `opentelemetry_telemetry` when distributed tracing is needed — additive, not a rewrite

**Why not OpenTelemetry directly?**
- 6-10+ transitive dependencies (gRPC stack, HTTP/2, protobuf)
- ~10-50μs per span (vs ~1-5μs for telemetry dispatch) — 10x overhead
- Designed for distributed export, not local REPL querying
- Erlang SDK is secondary to Elixir in practice — documentation and examples lag
- Available as an add-on via the telemetry bridge when needed

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
    Class = get_class_for_pid(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
    telemetry:span([beamtalk, actor, dispatch], Metadata, fun() ->
        Result = do_sync_send(ActorPid, Selector, Args),
        {Result, Metadata#{outcome => ok}}
    end).
```

`telemetry:span/3` automatically emits `start`, `stop`, and `exception` events with timing. The `stop` event includes `duration` in native time units.

**What the timing measures:** Send-wrapper instrumentation captures the **caller-perspective round-trip time** — the full duration from message send to reply received, including message queue wait and scheduling jitter. This is NOT actor method execution time. To distinguish queueing delay from execution time, use `Tracing healthFor:` which reports message queue depth. A high round-trip time with a deep queue indicates queueing; a high round-trip with an empty queue indicates slow method execution.

**Lifecycle methods** (`isAlive`, `stop`, `pid`, `monitor`, `onExit:`, `demonitor`) are NOT instrumented — they are handled locally in the send wrappers without entering the gen_server message loop.

**Codegen-level instrumentation** (wrapping `safe_dispatch` in generated code) is deferred as a future enhancement. Send-wrapper instrumentation captures the caller-perspective round-trip time, which is the actionable metric for bottleneck detection. Codegen-level instrumentation would add precise method-body timing if needed later.

### Storage: Two ETS Tables

`beamtalk_trace_store.erl` — a `gen_server` that owns the ETS tables and acts as a `telemetry` handler:

**Table 1: `beamtalk_trace_events` (ordered_set, ring buffer)**

Captures individual dispatch events when tracing is enabled.

```erlang
%% Key: {erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])}
%%   — composite key ensures uniqueness across concurrent schedulers
%% Row: {Key, Pid, Class, Selector, Mode, Duration_ns, Outcome, Metadata}
%% Outcome: ok | error | timeout | cast
%% Metadata: #{error => Term} for failures, #{} otherwise
```

- Capped at 100,000 events (configurable via `Tracing maxEvents:`)
- Ring buffer semantics: on insert, if `ets:info(Tab, size) >= MaxEvents`, delete the oldest N entries (batch eviction of 10% to amortize cleanup cost) via `ets:select_delete` on the smallest keys
- Only populated when tracing is enabled
- Toggle via `persistent_term:put(beamtalk_tracing_enabled, true | false)` — ~5ns check

**Table 2: `beamtalk_actor_agg` (set, always-on counters)**

Running aggregates, updated on every dispatch regardless of tracing state.

```erlang
%% Key: {Pid, Selector}
%% Value: {Key, Calls, OkCount, ErrorCount, TimeoutCount, TotalDuration_ns, MinDuration_ns, MaxDuration_ns}
```

- Updated via `ets:update_counter/3` — atomic, ~100-200ns per call
- Always on — no toggle needed, negligible overhead
- Persists until explicitly cleared (survives actor death for post-mortem analysis)

**Overhead budget:**

| Operation | Cost | When |
|-----------|------|------|
| `telemetry:span/3` dispatch + aggregate update | ~150-250ns | Always |
| `persistent_term` check (trace enabled?) | ~5ns | Always |
| Trace event insert (`ets:insert` + eviction amortized) | ~300-500ns | Only when tracing enabled |
| **Total per-call** | **~200ns (tracing off) / ~600ns (tracing on)** | |

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

### MCP Tools

MCP tools wrap the `Tracing` Beamtalk API via thin REPL ops in `beamtalk_repl_ops_perf.erl`, following the established pattern:

| MCP Tool | Beamtalk API | Description |
|----------|-------------|-------------|
| `enable-tracing` | `Tracing enable` | Start trace event capture |
| `disable-tracing` | `Tracing disable` | Stop trace event capture |
| `get-traces` | `Tracing traces` / `tracesFor:` | Get trace events as JSON |
| `actor-stats` | `Tracing stats` / `statsFor:` | Aggregate stats per method |
| `actor-health` | `Tracing healthFor:` | Live process info |
| `bottlenecks` | `Tracing bottlenecks:` | Top N by queue depth |
| `slow-methods` | `Tracing slowMethods:` | Top N by avg latency |
| `error-methods` | `Tracing errorMethods:` | Top N by error rate |
| `system-health` | `Tracing systemHealth` | VM overview |
| `clear-traces` | `Tracing clear` | Clear all data |

The MCP tools add filtering parameters (e.g., `actor`, `selector`, `limit`) that map to the corresponding Beamtalk method variants.

### Agent Workflow

```
1. agent calls enable-tracing        → Tracing enable
2. agent calls test (existing tool)   → runs BUnit suite, actors spawn and die
3. agent calls get-traces             → Tracing traces (structured JSON)
4. agent calls actor-stats            → Tracing stats (aggregates survived actor death)
5. agent calls slow-methods           → Tracing slowMethods: 10
6. agent calls actor-health pid=X     → Tracing healthFor: (for live actors)
7. agent calls disable-tracing        → Tracing disable
8. agent calls clear-traces           → Tracing clear
```

### Cleanup and Lifecycle

- **Trace events**: Bounded by ring buffer (100k default). Oldest dropped on overflow via batch eviction. Explicit `Tracing clear` resets.
- **Aggregate stats**: Persist until `Tracing clear`. Size is proportional to unique `{Pid, Selector}` combinations, not call volume — bounded by the number of actors × methods seen.
- **Actor death**: Stats and traces for dead actors are **retained** for post-mortem analysis. This is essential for the test workflow where actors spawn, run, and die before results are queried.
- **`beamtalk_trace_store` gen_server**: Supervised under `beamtalk_runtime_sup`. Owns both ETS tables. Crash recovery: tables are lost (acceptable — tracing data is ephemeral by nature).

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
- Always-on aggregates with ~200ns overhead are safe for production
- Trace capture is opt-in — zero overhead when disabled
- `Tracing systemHealth` provides the VM overview they need for capacity planning
- Structured output integrates with existing monitoring pipelines via custom telemetry handlers
- Ring buffer prevents unbounded memory growth

## Steelman Analysis

### Alternative A: Custom ETS-Only (no telemetry dependency)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "Zero dependencies means nothing can break. `Tracing enable` just works — no library version to worry about." |
| **Smalltalk purist** | "Self-contained is elegant. Beamtalk should own its full stack, not delegate core observability to an external library." |
| **BEAM veteran** | "One fewer dependency to manage. ETS is the right tool for this — `telemetry` adds an indirection layer we don't need when we control both sides." |
| **Operator** | "Fewer moving parts in production. I can inspect the ETS tables directly with `ets:tab2list/1` — no abstraction hiding the data." |
| **Language designer** | "The `persistent_term` toggle pattern is simpler and faster than telemetry's handler dispatch. We're optimizing for one specific use case, not building a general-purpose event bus." |

### Alternative B: OpenTelemetry-native

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "OpenTelemetry is the industry standard I learned in my distributed systems class. I can export Beamtalk traces to Jaeger and see them in the same dashboard as my Go microservices." |
| **BEAM veteran** | "If we're going to add observability, do it once with the standard. `telemetry` is BEAM-only — OpenTelemetry gives us cross-service distributed tracing from day one." |
| **Operator** | "Every APM tool I use (Datadog, New Relic, Grafana Tempo) speaks OTLP. Native OpenTelemetry means zero custom integration work." |

### Alternative C: Tracing methods on `Beamtalk` (ADR 0064 pattern)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "One place for system tools. I already know `Beamtalk logLevel:` and `Beamtalk enableDebug:` — `Beamtalk enableTracing` is the obvious next thing I'd try." |
| **BEAM veteran** | "DDD consistency. ADR 0064 said runtime configuration goes on `Beamtalk`. Tracing enable/disable is runtime configuration. Splitting it to a new class contradicts the precedent." |
| **Language designer** | "Fewer top-level names in the namespace. `Beamtalk` is already known; `Tracing` is a new name to discover. Discoverability is better when tools are co-located." |

### Decided: `telemetry` + `Tracing` class (hybrid)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "The Beamtalk API is all I see — `Tracing enable`, `Tracing stats`. The telemetry library is invisible to me." |
| **Smalltalk purist** | "`MessageTally` is a standalone class in Pharo; `Tracing` is a standalone class here. This is the Smalltalk way — profiling tools are their own thing, not bolted onto the system dictionary." |
| **BEAM veteran** | "`telemetry` events mean I can `telemetry:attach/4` my own handlers from the Erlang shell — export to Prometheus, StatsD, whatever. And `opentelemetry_telemetry` bridges to OTel when I need it." |
| **Operator** | "Best of both worlds: local ETS queries for REPL debugging, composable handlers for production export, and an OTel upgrade path." |
| **Language designer** | "The `Tracing` / `Logging` pair is a cleaner taxonomy than everything on `Beamtalk`. Each subsystem owns its full surface. This scales — a future `Metrics` class has an obvious home." |

### Tension Points

- **ADR 0064 consistency:** BEAM veterans and newcomers have a genuine argument that `Tracing enable` should live on `Beamtalk` per the ADR 0064 DDD precedent. The counter-argument is scale: logging config is ~5 methods, tracing is ~15. At that size, a standalone class is the Smalltalk-native solution. The planned `Logging` migration will retroactively make both subsystems consistent.
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
- New dependency: `telemetry` library (mitigated: zero transitive deps, pure Erlang, ~500 LOC, de facto BEAM standard)
- ~200ns overhead per actor message send (always-on aggregates) — negligible but non-zero
- `beamtalk_trace_store` gen_server is a single point of contention for ETS writes under extreme load (mitigated: `ets:update_counter` is lock-free for concurrent writers on `set` tables)
- Trace events reference PIDs which are opaque after actor death (mitigated: class name is captured at record time)
- Standalone `Tracing` class is temporarily inconsistent with ADR 0064's pattern of logging config on `Beamtalk` (mitigated: planned `Logging` class migration will make both consistent)

### Neutral
- Does not affect actors that don't send messages (value objects, pure computation)
- Does not change any existing Beamtalk syntax or semantics
- The `telemetry` dependency will likely already be present if users integrate with Elixir libraries
- `Tracing` class follows the established sealed-facade pattern — no new language concepts

## Implementation

### Phase 1: Core Infrastructure (M)
**Affected components:** Runtime (Erlang), rebar.config

1. Add `telemetry` dependency to `runtime/apps/beamtalk_runtime/rebar.config`
2. Create `beamtalk_trace_store.erl` — gen_server owning ETS tables, telemetry handler callbacks, query API
3. Add `beamtalk_trace_store` to `beamtalk_runtime_sup` supervision tree
4. EUnit tests for trace store: insert, query, ring buffer overflow (batch eviction), aggregate counters, clear, composite key uniqueness

### Phase 2: Actor Instrumentation (S)
**Affected components:** Runtime (`beamtalk_actor.erl`)

1. Instrument `sync_send/3`, `async_send/4`, `cast_send/3` with `telemetry:span/3`
2. Wire aggregate updates (always on) and trace event capture (when enabled)
3. EUnit tests: verify telemetry events are emitted, verify overhead is within budget

### Phase 3: Beamtalk API (M)
**Affected components:** Stdlib (`Tracing.bt`), Runtime (`beamtalk_tracing.erl`)

1. Create `beamtalk_tracing.erl` — Erlang shim that the Tracing class delegates to
2. Create `Tracing.bt` — sealed class-only facade
3. BUnit tests for Tracing API: enable/disable, traces, stats, slowMethods, healthFor, systemHealth

### Phase 4: MCP Tools (M)
**Affected components:** Runtime (`beamtalk_repl_ops_perf.erl`), Rust MCP server (`server.rs`)

1. Create `beamtalk_repl_ops_perf.erl` — REPL op handlers for tracing operations
2. Wire ops into `beamtalk_repl_server.erl`
3. Add MCP tools to `server.rs` (10 tools, following existing pattern)
4. Integration tests: MCP workflow (enable → eval → get-traces → stats)

### Phase 5: Documentation and Polish (S)
1. Update `docs/beamtalk-language-features.md` with Tracing class documentation
2. Add examples to the example corpus for MCP search
3. Update ADR 0064 to cross-reference this ADR

## References
- Related issues: BT-1429 (deferred from ADR 0064)
- Related ADRs: ADR 0064 (Runtime Logging Control — complementary), ADR 0065 (OTP Primitives — Actor/Server hierarchy), ADR 0043 (Sync-by-Default Messaging)
- Code: `beamtalk_actor.erl` (sync_send, async_send, cast_send), `server.rs` (MCP tool pattern)
- External: [beam-telemetry/telemetry](https://github.com/beam-telemetry/telemetry), [opentelemetry_telemetry bridge](https://github.com/open-telemetry/opentelemetry-erlang-contrib)
- Prior art: Erlang sys/recon, Elixir telemetry/LiveDashboard, Akka Insights, Pharo MessageTally/PerformanceProfiler, Pony flight recorder
- Design document: `issue-adr-actor-observability.md` on branch `claude/erlang-genserver-tracing-DfzoF`
