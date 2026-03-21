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

**No Beamtalk-level API exists** — there is no `Tracer` class, no `profile:` method, nothing discoverable from the REPL.

### Constraints

- Must expose a **Beamtalk-level API** (dedicated `Tracer` class) — MCP tools wrap thin Erlang shims, following the established pattern (System, Logger, Workspace)
- Must not overload `Beamtalk` or `Object` with additional methods — the `Tracer` class owns this surface
- Always-on aggregate stats must have negligible overhead (~200ns/call budget)
- Trace event capture must be opt-in with zero cost when disabled
- Data must persist after actor death (the test workflow: enable → run tests → actors die → query results)
- Should use standards-based telemetry infrastructure where viable, not reinvent the wheel
- Must produce structured output consumable by both REPL exploration and MCP JSON responses

## Decision

### Architecture: `telemetry` event bus + ETS storage handlers

Use the Erlang [`telemetry`](https://github.com/beam-telemetry/telemetry) library as the event bus, with custom ETS-backed handlers for aggregation and trace capture. This is a **hybrid approach**: standard event emission, Beamtalk-specific storage and query.

```
Beamtalk API (Tracer class — stdlib/src/Tracer.bt)
    ↓ delegates to
Erlang shim (beamtalk_tracer.erl — runtime)
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

**Lifecycle methods** (`isAlive`, `stop`, `pid`, `monitor`, `onExit:`, `demonitor`) are NOT instrumented — they are handled locally in the send wrappers without entering the gen_server message loop.

**Codegen-level instrumentation** (wrapping `safe_dispatch` in generated code) is deferred as a future enhancement. Send-wrapper instrumentation captures the caller-perspective round-trip time, which is the actionable metric. Message queue depth from `actor_health` disambiguates queueing delay vs method execution time when needed.

### Storage: Two ETS Tables

`beamtalk_trace_store.erl` — a `gen_server` that owns the ETS tables and acts as a `telemetry` handler:

**Table 1: `beamtalk_trace_events` (ordered_set, ring buffer)**

Captures individual dispatch events when tracing is enabled.

```erlang
%% Key: erlang:monotonic_time(nanosecond) — unique, ordered
%% Row: {Timestamp, Pid, Class, Selector, Mode, Duration_ns, Outcome, Metadata}
%% Outcome: ok | error | timeout | cast
%% Metadata: #{error => Term} for failures, #{} otherwise
```

- Capped at 100,000 events (configurable via `Tracer maxEvents:`)
- Ring buffer semantics: oldest events dropped on overflow
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
| Aggregate update (`ets:update_counter`) | ~100-200ns | Always |
| `persistent_term` check (trace enabled?) | ~5ns | Always |
| Telemetry dispatch (no trace handler) | ~10-50ns | Always (near-zero with `persist/0`) |
| Trace event insert (`ets:insert`) | ~300-500ns | Only when tracing enabled |
| **Total per-call** | **~200ns (tracing off) / ~600ns (tracing on)** | |

### Beamtalk API: `Tracer` Class

A sealed, class-only facade following the System/Logger/Workspace pattern:

```beamtalk
sealed Object subclass: Tracer

  // === Tracing lifecycle ===

  class sealed enable -> Nil
  // Start capturing trace events (aggregates are always on)
  // Tracer enable

  class sealed disable -> Nil
  // Stop capturing trace events
  // Tracer disable

  class sealed isEnabled -> Boolean
  // Check whether trace capture is active
  // Tracer isEnabled   // => true

  class sealed clear -> Nil
  // Clear all trace events and aggregate stats
  // Tracer clear

  // === Trace event queries ===

  class sealed traces -> List
  // All captured trace events (up to buffer limit), newest first
  // Tracer traces

  class sealed tracesFor: actor :: Object -> List
  // Trace events for a specific actor
  // Tracer tracesFor: myCounter

  class sealed tracesFor: actor :: Object selector: sel :: Symbol -> List
  // Trace events for a specific actor + method
  // Tracer tracesFor: myCounter selector: #increment

  // === Aggregate stats ===

  class sealed stats -> Dictionary
  // Per-actor, per-method aggregate stats (always available, even without tracing)
  // Tracer stats

  class sealed statsFor: actor :: Object -> Dictionary
  // Aggregate stats for a specific actor
  // Tracer statsFor: myCounter

  // === Analysis (computed from aggregates) ===

  class sealed slowMethods: limit :: Integer -> List
  // Top N methods by average latency
  // Tracer slowMethods: 10

  class sealed hotMethods: limit :: Integer -> List
  // Top N methods by call count
  // Tracer hotMethods: 10

  class sealed errorMethods: limit :: Integer -> List
  // Top N methods by error + timeout rate
  // Tracer errorMethods: 5

  class sealed bottlenecks: limit :: Integer -> List
  // Top N actors by message queue length (live snapshot)
  // Tracer bottlenecks: 5

  // === Live process health ===

  class sealed healthFor: actor :: Object -> Dictionary
  // Live process info: queue depth, memory, reductions, status, uptime
  // Tracer healthFor: myCounter

  class sealed systemHealth -> Dictionary
  // VM overview: scheduler count, memory breakdown, process count, run queue lengths
  // Tracer systemHealth

  // === Configuration ===

  class sealed maxEvents -> Integer
  // Current ring buffer capacity
  // Tracer maxEvents   // => 100000

  class sealed maxEvents: size :: Integer -> Nil
  // Set ring buffer capacity (clears existing events)
  // Tracer maxEvents: 50000
```

### REPL Session

```beamtalk
// Always-on aggregates — no setup needed
c := Counter spawn.
1 to: 100 do: [:i | c increment].
Tracer stats
// => {"<0.456.0>" => {"class" => "Counter", "methods" => {"increment" => {"calls" => 100, "ok" => 100, "errors" => 0, "avg_us" => 38, "min_us" => 12, "max_us" => 523}}}}

// Enable trace capture for detailed events
Tracer enable.
c increment.
c increment.
c increment.
Tracer traces
// => [{"timestamp_us" => 1234567890, "actor" => "<0.456.0>", "class" => "Counter", "selector" => "increment", "duration_us" => 42, "outcome" => "ok"}, ...]

// Find bottlenecks
Tracer slowMethods: 5
// => [{"class" => "DatabasePool", "selector" => "query:", "avg_us" => 15023, "calls" => 47}, ...]

Tracer bottlenecks: 3
// => [{"actor" => "<0.789.0>", "class" => "WorkQueue", "queue_depth" => 142, "memory_kb" => 256}, ...]

// Live health check
Tracer healthFor: c
// => {"pid" => "<0.456.0>", "class" => "Counter", "status" => "waiting", "queue_depth" => 0, "memory_kb" => 4, "reductions" => 1523, "uptime_ms" => 45000}

// Clean up
Tracer disable.
Tracer clear.
```

### Error Behavior

```beamtalk
// Querying traces when not enabled — returns empty list (not an error)
Tracer traces   // => []

// Stats for unknown actor — returns empty dictionary
Tracer statsFor: somethingInvalid   // => {}

// Health for dead actor
Tracer healthFor: deadActor
// => {"pid" => "<0.456.0>", "class" => "Counter", "status" => "dead", "error" => "process not alive"}
```

### MCP Tools

MCP tools wrap the `Tracer` Beamtalk API via thin REPL ops in `beamtalk_repl_ops_perf.erl`, following the established pattern:

| MCP Tool | Beamtalk API | Description |
|----------|-------------|-------------|
| `enable-tracing` | `Tracer enable` | Start trace event capture |
| `disable-tracing` | `Tracer disable` | Stop trace event capture |
| `get-traces` | `Tracer traces` / `tracesFor:` | Get trace events as JSON |
| `actor-stats` | `Tracer stats` / `statsFor:` | Aggregate stats per method |
| `actor-health` | `Tracer healthFor:` | Live process info |
| `bottlenecks` | `Tracer bottlenecks:` | Top N by queue depth |
| `slow-methods` | `Tracer slowMethods:` | Top N by avg latency |
| `error-methods` | `Tracer errorMethods:` | Top N by error rate |
| `system-health` | `Tracer systemHealth` | VM overview |
| `clear-traces` | `Tracer clear` | Clear all data |

The MCP tools add filtering parameters (e.g., `actor`, `selector`, `limit`) that map to the corresponding Beamtalk method variants.

### Agent Workflow

```
1. agent calls enable-tracing        → Tracer enable
2. agent calls test (existing tool)   → runs BUnit suite, actors spawn and die
3. agent calls get-traces             → Tracer traces (structured JSON)
4. agent calls actor-stats            → Tracer stats (aggregates survived actor death)
5. agent calls slow-methods           → Tracer slowMethods: 10
6. agent calls actor-health pid=X     → Tracer healthFor: (for live actors)
7. agent calls disable-tracing        → Tracer disable
8. agent calls clear-traces           → Tracer clear
```

### Cleanup and Lifecycle

- **Trace events**: Bounded by ring buffer (100k default). Oldest dropped on overflow. Explicit `Tracer clear` resets.
- **Aggregate stats**: Persist until `Tracer clear`. Size is proportional to unique `{Pid, Selector}` combinations, not call volume — bounded by the number of actors × methods seen.
- **Actor death**: Stats and traces for dead actors are **retained** for post-mortem analysis. This is essential for the test workflow where actors spawn, run, and die before results are queried.
- **`beamtalk_trace_store` gen_server**: Supervised under `beamtalk_runtime_sup`. Owns both ETS tables. Crash recovery: tables are lost (acceptable — tracing data is ephemeral by nature).

## Prior Art

### Erlang/OTP — sys module
Every OTP behaviour supports `sys:trace(Pid, true)` for per-process debug output and `sys:statistics(Pid, true)` for basic counters (reductions, messages_in, messages_out). The output is unstructured text to stdout — useful for quick debugging but not queryable or aggregatable. The `dbg` module provides lower-level function tracing with pattern matching. Observer adds a GUI. `recon` (third-party) adds production-safe rate-limited tracing and process ranking by message queue length, reductions, or memory.

**What we adopt:** The concept of always-available aggregate stats (like `sys:statistics`) and opt-in detailed tracing (like `sys:trace`). **What we improve:** Structured output (queryable dictionaries, not stdout text), Beamtalk-level API (no need to know Erlang module names), and MCP exposure for agent consumption.

### Elixir — telemetry ecosystem
Elixir's `telemetry` library is the de facto BEAM standard for instrumented observability. Libraries emit events via `telemetry:execute/3`; applications attach handlers. Phoenix LiveDashboard provides a web UI showing real-time metrics. GenServer itself does not auto-emit telemetry events — libraries add instrumentation explicitly.

**What we adopt:** The `telemetry` library as our event bus, following the same event naming conventions (`[prefix, object, action, phase]`). **What we adapt:** Beamtalk instruments at the runtime level (all actor sends emit events automatically) rather than requiring library authors to add instrumentation manually. Our `Tracer` class provides the query API that the telemetry ecosystem leaves to third-party dashboards.

### Akka — Cinnamon/Insights
Akka Insights provides per-actor-class instrumentation via configuration (no code changes). Four key metrics: mailbox size, mailbox time (queue wait), processing time, and stash size. Configuration supports per-class, per-instance, and wildcard selection with threshold alerts.

**What we adopt:** The four-metric model (our aggregates track call count, duration, errors, timeouts — analogous). Always-on aggregates with opt-in detailed tracing matches Akka's approach. **What we differ on:** Akka's instrumentation is configuration-driven and commercial. Ours is API-driven (`Tracer enable`) and built-in.

### Pharo Smalltalk — MessageTally
Pharo's `MessageTally spyOn: [block]` profiles a block expression, producing a hierarchical tree of methods with execution time percentages. Research by Bergel et al. showed that counting message sends (not sampling) produces perfectly stable profiles. Deeply integrated into the IDE — right-click to profile, visual hierarchy browser.

**What we learn:** The `profile: [block]` pattern is compelling for targeted profiling. Our current design focuses on continuous tracing rather than block-scoped profiling — a future `Tracer profile: [block]` method could layer on top of the event infrastructure. The message-counting insight validates our always-on aggregate counters.

### Pony — Flight Recorder
Pony's runtime supports a **flight recorder** mode: a circular in-memory buffer that captures trace events continuously with low overhead and dumps to stderr on crash (SIGILL/SIGSEGV/SIGBUS). This provides post-mortem debugging data without the overhead of continuous export.

**What we learn:** Our ring buffer design is essentially a flight recorder. The Pony model of "always capturing, only materializing on demand" aligns with our approach of always-on aggregates + opt-in trace capture.

## User Impact

### Newcomer (from Python/JS/Ruby)
- `Tracer stats` and `Tracer slowMethods: 5` are immediately discoverable and self-explanatory
- No setup required for aggregate stats — they just work
- `Tracer enable` / `Tracer traces` is a simple workflow, no OTP concepts needed
- The REPL output format (dictionaries with string keys) is familiar from JSON

### Smalltalk Developer
- `Tracer` as a sealed class-only facade follows the same pattern as `System` and `Logger` — consistent design vocabulary
- `Tracer profile: [block]` (future) would mirror Pharo's `MessageTally spyOn:` — familiar territory
- The class does not pollute `Object` or `Beamtalk` — clean separation of concerns

### Erlang/BEAM Developer
- The `telemetry` event bus is the standard they expect — compatible with their existing tooling
- `telemetry:attach/4` lets them add custom handlers (StatsD, Datadog) at the Erlang level
- `Tracer healthFor:` wraps `erlang:process_info/2` — familiar data, Beamtalk syntax
- OpenTelemetry integration is available via the standard `opentelemetry_telemetry` bridge

### Production Operator
- Always-on aggregates with ~200ns overhead are safe for production
- Trace capture is opt-in — zero overhead when disabled
- `Tracer systemHealth` provides the VM overview they need for capacity planning
- Structured output integrates with existing monitoring pipelines via custom telemetry handlers
- Ring buffer prevents unbounded memory growth

## Steelman Analysis

### Alternative A: Custom ETS-Only (no telemetry dependency)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "Zero dependencies means nothing can break. `Tracer enable` just works — no library version to worry about." |
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

### Decided: `telemetry` + custom ETS handlers (hybrid)

| Cohort | Best argument |
|--------|---------------|
| **Newcomer** | "The Beamtalk API is all I see — `Tracer enable`, `Tracer stats`. The telemetry library is invisible to me." |
| **Smalltalk purist** | "The `Tracer` class is a clean facade. The implementation detail (telemetry) is hidden behind Beamtalk's message-passing API." |
| **BEAM veteran** | "`telemetry` events mean I can `telemetry:attach/4` my own handlers from the Erlang shell — export to Prometheus, StatsD, whatever. And `opentelemetry_telemetry` bridges to OTel when I need it." |
| **Operator** | "Best of both worlds: local ETS queries for REPL debugging, composable handlers for production export, and an OTel upgrade path." |
| **Language designer** | "Standard event emission with custom storage. We don't reinvent the bus, but we own the query semantics. And we can swap storage implementations without changing the Beamtalk API." |

### Tension Points

- **BEAM veterans** would prefer OpenTelemetry-native for distributed tracing. The bridge via `opentelemetry_telemetry` is a good compromise but adds one more package when they need it.
- **Purists** would prefer zero dependencies. The argument is genuine — `telemetry` is tiny and stable, but it IS a dependency. The composability benefit (user-added handlers, OTel bridge) justifies the cost.
- **All cohorts agree** on: always-on aggregates, opt-in detailed tracing, dedicated `Tracer` class (not on `Beamtalk`), structured output, ring-buffer bounded storage.

## Alternatives Considered

### Alternative: Custom ETS-Only (no telemetry library)
Direct `ets:insert/2` and `ets:update_counter/3` from instrumented send wrappers, with a `persistent_term` toggle for trace capture.

**Rejected because:** Loses composability. Users cannot attach their own handlers without modifying Beamtalk internals. No bridge to OpenTelemetry. The ~50ns additional overhead from telemetry dispatch (vs direct ETS write) is negligible, while the extensibility benefit is significant. We would be reimplementing the handler dispatch that `telemetry` already provides as a battle-tested, zero-dependency library.

### Alternative: OpenTelemetry SDK
Use `opentelemetry-erlang` as the primary instrumentation layer with spans, metrics, and OTLP export.

**Rejected because:** 6-10+ transitive dependencies including a gRPC stack. ~10-50μs per span (10x the overhead of our budget). Designed for distributed export, not local REPL querying — fighting the abstraction for our primary use case. Erlang SDK documentation lags behind Go/Java/Python. Available as an add-on via the `opentelemetry_telemetry` bridge when distributed tracing is actually needed.

### Alternative: sys module integration
Use OTP's built-in `sys:trace/2` and `sys:statistics/2` for per-actor tracing.

**Rejected because:** `sys:trace` produces unstructured text to stdout — not queryable, not aggregatable, not exposable via MCP. `sys:statistics` only tracks reductions, messages_in, messages_out — no per-method breakdown, no timing, no error classification. Would require significant wrapping to produce the structured output needed by the `Tracer` API and MCP tools.

### Alternative: Tracing in codegen (safe_dispatch)
Instrument the generated `safe_dispatch` function in each actor's Core Erlang code, measuring method body execution time.

**Rejected for now:** Adds codegen complexity and requires recompilation to enable/disable. Send-wrapper instrumentation captures the caller-perspective round-trip time (including message queue wait), which is the more actionable metric for bottleneck detection. Message queue depth from `Tracer healthFor:` disambiguates queueing vs execution time. Codegen-level instrumentation remains a future enhancement if method-body timing is needed.

## Consequences

### Positive
- First Beamtalk-level observability API — discoverable, self-explanatory, works in REPL
- AI agents can autonomously debug performance via MCP tools
- Always-on aggregates provide baseline visibility without setup
- Standard `telemetry` event bus enables user-extensible monitoring (StatsD, Datadog, OpenTelemetry bridge)
- Ring buffer prevents unbounded memory growth
- Post-mortem analysis works — data survives actor death

### Negative
- New dependency: `telemetry` library (mitigated: zero transitive deps, pure Erlang, ~500 LOC, de facto BEAM standard)
- ~200ns overhead per actor message send (always-on aggregates) — negligible but non-zero
- `beamtalk_trace_store` gen_server is a single point of contention for ETS writes under extreme load (mitigated: `ets:update_counter` is lock-free for concurrent writers on `set` tables)
- Trace events reference PIDs which are opaque after actor death (mitigated: class name is captured at record time)

### Neutral
- Does not affect actors that don't send messages (value objects, pure computation)
- Does not change any existing Beamtalk syntax or semantics
- The `telemetry` dependency will likely already be present if users integrate with Elixir libraries
- `Tracer` class follows the established sealed-facade pattern — no new language concepts

## Implementation

### Phase 1: Core Infrastructure (M)
**Affected components:** Runtime (Erlang), rebar.config

1. Add `telemetry` dependency to `runtime/apps/beamtalk_runtime/rebar.config`
2. Create `beamtalk_trace_store.erl` — gen_server owning ETS tables, telemetry handler callbacks, query API
3. Add `beamtalk_trace_store` to `beamtalk_runtime_sup` supervision tree
4. EUnit tests for trace store: insert, query, ring buffer overflow, aggregate counters, clear

### Phase 2: Actor Instrumentation (S)
**Affected components:** Runtime (`beamtalk_actor.erl`)

1. Instrument `sync_send/3`, `async_send/4`, `cast_send/3` with `telemetry:span/3`
2. Wire aggregate updates (always on) and trace event capture (when enabled)
3. EUnit tests: verify telemetry events are emitted, verify overhead is within budget

### Phase 3: Beamtalk API (M)
**Affected components:** Stdlib (`Tracer.bt`), Runtime (`beamtalk_tracer.erl`)

1. Create `beamtalk_tracer.erl` — Erlang shim that the Tracer class delegates to
2. Create `Tracer.bt` — sealed class-only facade
3. BUnit tests for Tracer API: enable/disable, traces, stats, slowMethods, healthFor, systemHealth

### Phase 4: MCP Tools (M)
**Affected components:** Runtime (`beamtalk_repl_ops_perf.erl`), Rust MCP server (`server.rs`)

1. Create `beamtalk_repl_ops_perf.erl` — REPL op handlers for tracing operations
2. Wire ops into `beamtalk_repl_server.erl`
3. Add MCP tools to `server.rs` (10 tools, following existing pattern)
4. Integration tests: MCP workflow (enable → eval → get-traces → stats)

### Phase 5: Documentation and Polish (S)
1. Update `docs/beamtalk-language-features.md` with Tracer class documentation
2. Add examples to the example corpus for MCP search
3. Update ADR 0064 to cross-reference this ADR

## References
- Related issues: BT-1429 (deferred from ADR 0064)
- Related ADRs: ADR 0064 (Runtime Logging Control — complementary), ADR 0065 (OTP Primitives — Actor/Server hierarchy), ADR 0043 (Sync-by-Default Messaging)
- Code: `beamtalk_actor.erl` (sync_send, async_send, cast_send), `server.rs` (MCP tool pattern)
- External: [beam-telemetry/telemetry](https://github.com/beam-telemetry/telemetry), [opentelemetry_telemetry bridge](https://github.com/open-telemetry/opentelemetry-erlang-contrib)
- Prior art: Erlang sys/recon, Elixir telemetry/LiveDashboard, Akka Insights, Pharo MessageTally, Pony flight recorder
- Design document: `issue-adr-actor-observability.md` on branch `claude/erlang-genserver-tracing-DfzoF`
