# ADR 0064: Runtime Logging Control and Observability API

## Status
Accepted (2026-03-17)

## Context

### Problem

Beamtalk's logging infrastructure has three gaps that make runtime debugging unnecessarily difficult:

1. **No query API.** `Logger setLevel:` exists but there is no `getLevel`, no way to discover what subsystems exist, and no way to see what debug filtering is currently active.

2. **No targeted filtering.** Logging is all-or-nothing. `Logger setLevel: #debug` turns on debug for the *entire VM* — every stdlib module load, every dispatch lookup, every bootstrap stub. When debugging a supervisor tree, you get buried in noise from unrelated subsystems. OTP's `logger:set_module_level/2` supports per-module filtering, but it's not exposed to Beamtalk and requires knowing internal Erlang module names.

3. **No log access from outside the VM.** Workspace logs go to `~/.beamtalk/workspaces/{id}/workspace.log` but there's no CLI command to view them. Operators must manually find the workspace ID, navigate to the directory, and `tail -f` the file. There's no way to filter by level or follow logs from a running workspace.

These gaps were felt acutely when debugging BT-1417 (supervisor bypasses Actor initialize hook on child start) and BT-1420 (actor self-call state mutations silently lost via safe_dispatch). In both cases, understanding the sequence of supervisor starts, actor initializations, and message dispatches required adding ad-hoc print statements because the existing logging couldn't be targeted.

### Current State

**In-VM logging (Logger class):**
```beamtalk
Logger info: "message"              // emit log — works
Logger setLevel: #debug             // sets OTP primary level globally — works
Logger setLevel: #warning           // restore — works
// No getLevel, no per-class filtering, no handler control
```

**Logger implementation** (`beamtalk_logger.erl`): Thin wrapper around OTP `?LOG_*` macros. `setLevel:` calls `logger:set_primary_config(level, Level)`.

**File logger** (`beamtalk_workspace_sup.erl`): Workspace startup adds a `logger_std_h` handler (`beamtalk_file_log`) at debug level. Captures all debug+ events to `~/.beamtalk/workspaces/{id}/workspace.log` with 1MB rotation × 5 files. Disabled by `BEAMTALK_NO_FILE_LOG=1`.

**OTP logger capabilities not exposed:**
- `logger:set_module_level(Module, Level)` — per-module level override
- `logger:get_module_level()` — list all module-level overrides
- `logger:i()` — full logger configuration dump
- `logger:set_handler_config(HandlerId, level, Level)` — per-handler level
- `logger:add_primary_filter/2` — filter pipeline

**Two-object model (ADR 0040):**
- `Beamtalk` (BeamtalkInterface) — system facade: `version`, `allClasses`, `classNamed:`, `help:`. "What does the language know?"
- `Workspace` (WorkspaceInterface) — project facade: `load:`, `classes`, `actors`, `test`. "What is my working context?"

### Constraints

- Must build on OTP logger — not a replacement, a Beamtalk-idiomatic interface to it
- Must support both Beamtalk user classes and internal runtime subsystems as debug targets
- Users should never need to know Erlang module names (e.g., `beamtalk_supervisor`)
- The API must be discoverable through normal Beamtalk exploration (`help:`, tab completion)
- CLI log access must work without a running REPL session (for operators, CI, MCP tools)
- Must not break existing `Logger info:/warn:/error:/debug:` emission API

## Decision

### 1. Logging Control on `Beamtalk` (BeamtalkInterface)

Logging configuration belongs on the system facade, not on `Logger`. Rationale:

- **DDD boundary.** `Logger` is a stdlib class in the Object System Context — it emits log messages. Logging *configuration* (managing OTP handlers, installing filters, setting per-module/per-process levels) is a Runtime Context operation. A stdlib class reaching into runtime to call `logger:set_module_level`, install primary filters, and manage handler formatters crosses DDD boundaries. The stdlib should be pure; system tooling belongs higher up.
- **Industry pattern.** The dominant pattern across language runtimes is to separate the emit interface from the control interface: Java (`Logger` / `LogManager`), .NET (`ILogger` / `ILoggerFactory`), Go (`slog.Logger` / `Handler`), Rust (`tracing` macros / `Subscriber`). Putting configuration on the emitter is the minority pattern.
- **`Beamtalk` already lives at the runtime boundary** — it queries the class registry, introspects the workspace, talks to runtime infrastructure. Logging control is the same kind of system-level operation. It belongs alongside `allClasses` and `help:`, not alongside `info:` and `debug:`.
- **Discoverability**: users exploring with `Beamtalk help: Beamtalk` will find debug tools alongside class introspection.

ADR 0040 describes `Beamtalk` as "read-only for user code" in the context of the class registry namespace. Logging configuration is system tooling, not namespace mutation — the same category as `help:`.

#### Global Level Control

```beamtalk
Beamtalk logLevel                    // => #debug
Beamtalk logLevel: #warning          // reduce log verbosity
Beamtalk logLevel: #debug            // restore full capture
```

`logLevel:` sets the OTP **primary log level** — the same thing `Logger setLevel:` does today. This controls the minimum severity for all log events across all handlers. In Beamtalk's workspace model there is no separate "console" — the only meaningful log destination is the workspace file logger (`~/.beamtalk/workspaces/{id}/workspace.log`), which is viewed via `beamtalk logs`.

Replaces `Logger setLevel:` as the canonical API. `Logger setLevel:` remains as a deprecated alias for backwards compatibility.

#### Targeted Debug Filtering

```beamtalk
// Discover available debug targets
Beamtalk debugTargets
// => #[#actor, #compiler, #dispatch, #supervisor, #workspace, #mcp]

// Enable debug logging for a specific subsystem
Beamtalk enableDebug: #supervisor
// => nil  (supervisor lifecycle events now logged at debug level)

// Enable debug logging for a specific user class (all instances)
Beamtalk enableDebug: Counter
// => nil  (all Counter instances now logged at debug level)

// Enable debug logging for a specific actor instance
counter := Counter new
Beamtalk enableDebug: counter
// => nil  (only this specific Counter actor logged at debug level)

// See what's currently enabled
Beamtalk activeDebugTargets
// => [#supervisor, Counter, counter (<0.234.0>)]

// Disable
Beamtalk disableDebug: #supervisor
// => nil

// Disable all
Beamtalk disableAllDebug
// => nil
```

`enableDebug:` is polymorphic on the argument type:

| Argument | Mechanism | Scope |
|----------|-----------|-------|
| Symbol (`#supervisor`) | `logger:set_module_level/2` on module group | All runtime modules in group |
| Class (`Counter`) | Metadata filter on `#{beamtalk_class => 'Counter'}` | All instances of the class |
| Actor instance (`counter`) | `logger:set_process_level(Pid, debug)` | Single actor process |

Per-actor debug is **transient** — if the actor crashes and is restarted by a supervisor, it gets a new pid and the debug setting is lost. This is appropriate for live debugging sessions. For persistent class-wide debugging, use `enableDebug:` with the class.

#### Log Format Switching

For production deployments, operators need JSON-formatted logs for aggregation systems (Datadog, Splunk, ELK, Loki). The standard BEAM pattern is stdout as JSON + sidecar (Fluentd/Promtail/Datadog Agent). OTP logger supports this via pluggable formatters — the same `?LOG_*` call sites produce different output depending on the configured formatter.

```beamtalk
// Query current format
Beamtalk logFormat                   // => #text

// Switch to JSON output (structured logging for production)
Beamtalk logFormat: #json
// Log output becomes:
// {"time":"2026-03-17T14:23:01.234Z","level":"info","domain":"runtime","msg":"Actor initialized","class":"Counter","state_keys":["count"]}

// Switch back to human-readable
Beamtalk logFormat: #text
// Log output becomes:
// 2026-03-17T14:23:01.234Z [info] [runtime] beamtalk_actor:init/1 Actor initialized: class=Counter, state_keys=[count]
```

This changes the formatter on the file handler. Under the hood, `#json` configures `logger_formatter_json` (pure Erlang, depends only on `thoas` for JSON encoding). `#text` restores the default `logger_formatter`.

#### Full Configuration Dump

```beamtalk
Beamtalk loggerInfo
// => "Log level: debug
//     Format: text
//     File: ~/.beamtalk/workspaces/abc123/workspace.log (1MB × 5 rotation)
//     Active debug targets:
//       #supervisor → [beamtalk_supervisor, beamtalk_actor_sup, beamtalk_runtime_sup]
//       Counter → metadata filter (beamtalk_class = 'Counter')"
```

Beamtalk equivalent of `logger:i()`. Returns a formatted string for human consumption.

#### Error Handling

```beamtalk
Beamtalk logLevel: #potato
// => BeamtalkInterface does not understand logLevel: #potato
//    argument_error: Level must be one of: #emergency, #alert, #critical, #error, #warning, #notice, #info, #debug

Beamtalk enableDebug: #nonexistent
// => BeamtalkInterface does not understand enableDebug: #nonexistent
//    argument_error: Unknown debug target. Use Beamtalk debugTargets for available targets.

Beamtalk enableDebug: 42
// => BeamtalkInterface does not understand enableDebug: 42
//    type_error: Expected a Symbol (subsystem name), Class, or Actor instance
```

Errors use `#beamtalk_error{}` records (see ADR 0015, ADR 0060) with `class: 'BeamtalkInterface'`, `selector:`, and `hint:` fields — consistent with all other Beamtalk error reporting.

### 2. SASL Report Integration

SASL reports (supervisor reports, crash reports, progress reports) are standard OTP logger events with domain `[otp, sasl]`. Since OTP 21, they flow through the same logger infrastructure — no separate system.

**Default behavior:** SASL crash reports and supervisor reports flow at their natural levels (error/warning) and are always visible. SASL **progress reports** (every successful child start) are suppressed by default using OTP's built-in `logger_filters:progress/2` filter, since they're verbose.

**Integration with `enableDebug: #supervisor`:** Enabling `#supervisor` debug does two things:
1. Sets debug level on Beamtalk's supervisor modules (`beamtalk_supervisor`, etc.)
2. Removes the progress report filter, so OTP's own supervisor start/restart events also appear

This means `Beamtalk enableDebug: #supervisor` gives you the full picture: both Beamtalk's custom supervisor logging (BT-1425) *and* OTP's built-in SASL reports. Disabling reverts both.

```beamtalk
// Default: crash reports visible, progress reports suppressed
Beamtalk enableDebug: #supervisor
// Now you see everything:
//   [info] beamtalk_supervisor:startChild/2 Starting child counter_1 under actor_sup
//   [info] supervisor: {local,actor_sup} started: [{pid,<0.234.0>},{id,counter_1},...]
//   [debug] beamtalk_actor:init/1 Actor initialized: class=Counter

Beamtalk disableDebug: #supervisor
// Back to crash/error reports only
```

**Note on SASL:** Since OTP 21, supervisor crash reports and error reports flow through kernel's default logger regardless of whether SASL is started — they are *not* gated behind the SASL application. Progress reports are emitted at `info` level by the stdlib supervisor module and are visible whenever the primary log level allows (the workspace already sets primary to `debug`). Starting SASL (BT-1424) adds additional report types (release handler, etc.) and ensures consistent behavior, but is not strictly required for basic supervisor observability. The `enableDebug: #supervisor` toggle controls the *volume* of supervisor logging, not its existence.

### 3. Log Event Metadata and Domains

All log events carry structured metadata that distinguishes their origin. This uses OTP logger's standard `domain` mechanism plus Beamtalk-specific metadata fields.

#### Domains

| Domain | Origin | Example |
|--------|--------|---------|
| `[beamtalk, runtime]` | Erlang runtime internals — supervisor, dispatch, compiler, actor lifecycle | `?LOG_DEBUG(#{msg => "Starting child"})` in `beamtalk_supervisor.erl` |
| `[beamtalk, user]` | Beamtalk user code — `Logger debug:` calls from user classes | `Logger debug: "incrementing"` in Counter |
| `[beamtalk, stdlib]` | Stdlib class logging | Class loading, registry operations |
| `[otp]` | OTP's own events — SASL supervisor/crash/progress reports | Supervisor child started, process crashed |

#### Beamtalk-Specific Metadata

When the compiler encounters `Logger debug:`, `Logger info:`, etc., it generates a **direct `logger:log/3` call** at the call site rather than dispatching through `beamtalk_logger.erl`. This is the same approach Elixir uses — `Logger.debug/1` is a macro that expands inline, injecting caller metadata. Beamtalk achieves the same via codegen optimization.

```erlang
%% What the compiler generates for `Logger debug: "incrementing"` inside Counter >> increment:
logger:log(debug, <<"incrementing">>, #{
    domain => [beamtalk, user],
    beamtalk_class => 'Counter',
    beamtalk_selector => 'increment'
})
```

This means:
- OTP logger sees the **compiled class module** (`bt@user@counter`) as the caller, not `beamtalk_logger`
- Per-module level filtering via `logger:set_module_level/2` works correctly for user classes
- Metadata enables richer filtering (by class, by selector, by domain)
- The `beamtalk_logger.erl` module remains for Erlang FFI usage and programmatic access, but the compiler's fast path bypasses it

The `Logger debug:metadata:` variant merges user-provided metadata with the compiler-injected metadata:

```beamtalk
Logger debug: "processing" metadata: #{item => 42}
// Generated metadata: #{domain => [beamtalk, user], beamtalk_class => 'Counter',
//                       beamtalk_selector => 'process:', item => 42}
```

This is analogous to how `+` on integers compiles to `erlang:'+'` directly rather than going through full dispatch — the source syntax is a message send, but the compiler optimizes the code path.

#### Log Output Format

The domain and metadata enable distinct formatting for different log origins:

```text
[debug] [runtime] beamtalk_supervisor:startChild/2 Starting child counter_1 under actor_sup
[debug] [Counter] increment: incrementing
[info]  [otp] supervisor: {local,actor_sup} started: [{pid,<0.234.0>},{id,counter_1}]
```

In JSON format:
```json
{"time":"...","level":"debug","domain":"runtime","module":"beamtalk_supervisor","fn":"startChild/2","msg":"Starting child counter_1"}
{"time":"...","level":"debug","domain":"user","class":"Counter","selector":"increment","msg":"incrementing"}
{"time":"...","level":"info","domain":"otp","msg":"supervisor: {local,actor_sup} started: [...]"}
```

### 4. Subsystem Name Registry and Filtering Mechanism

Debug targets use a **dual filtering mechanism**: per-module level overrides for runtime subsystems, and metadata-based filters for user classes.

#### Runtime Subsystems (per-module level)

Subsystem symbols map to sets of Erlang modules. `enableDebug: #supervisor` calls `logger:set_module_level/2` on each module in the group:

| Symbol | Erlang Modules | Additional behavior |
|--------|---------------|---------------------|
| `#actor` | `beamtalk_actor` | |
| `#supervisor` | `beamtalk_supervisor`, `beamtalk_actor_sup`, `beamtalk_runtime_sup` | Also enables OTP progress reports |
| `#dispatch` | `beamtalk_dispatch`, `beamtalk_message_dispatch`, `beamtalk_class_dispatch` | |
| `#compiler` | `beamtalk_compiler_server`, `beamtalk_compiler_port`, `beamtalk_repl_compiler` | |
| `#workspace` | `beamtalk_workspace_sup`, `beamtalk_workspace_bootstrap` | |
| `#mcp` | (Rust-side: `beamtalk_mcp` tracing targets) | Cross-boundary — see Phase 4 |
| `#stdlib` | `beamtalk_stdlib`, `beamtalk_class_registry` | |
| `#hotreload` | `beamtalk_hot_reload` | |
| `#runtime` | All `[beamtalk, runtime]` domain modules | Domain-based filter for all runtime internals |
| `#user` | All `[beamtalk, user]` domain events | Domain-based filter for all user code logs |

#### User Classes (metadata filter)

`enableDebug: Counter` installs a **primary logger filter** that allows debug events matching `#{beamtalk_class => 'Counter'}` metadata. This works because the compiler injects class metadata at every `Logger` call site:

```beamtalk
Beamtalk enableDebug: Counter
// Installs filter: allow debug events where beamtalk_class == 'Counter'
// Also sets module level on bt@user@counter for any direct ?LOG_* calls
```

The dual approach (metadata filter + module level) ensures both codegen-emitted `Logger` calls and any direct `?LOG_*` calls in Erlang FFI modules for the class are captured.

#### State Tracking

Active debug targets are tracked in a Beamtalk-owned ETS table (similar to `beamtalk_wi_user_bindings`), not solely in OTP logger config. This allows `activeDebugTargets` to distinguish between "user enabled `#supervisor` via Beamtalk API" and "someone called `logger:set_module_level/2` directly via FFI." The ETS table records:

```erlang
%% {target, type, filter_ids}
{supervisor, subsystem, []}              % per-module level, no filters needed
{'Counter', user_class, [filter_id_1]}   % metadata filter installed
```

New subsystems can be registered by adding entries to the subsystem map in the implementation module. This is an internal extension point, not a user-facing API.

### 5. `beamtalk logs` CLI Command

A new CLI subcommand for viewing workspace logs without entering the REPL:

```bash
# Show recent log entries (default: last 50 lines)
beamtalk logs

# Follow log output (tail -f style)
beamtalk logs --follow

# Filter by minimum level
beamtalk logs --level debug
beamtalk logs --level error

# Show logs for a specific workspace
beamtalk logs --workspace <id>

# Show log file path
beamtalk logs --path
```

**Workspace discovery:** Uses the same workspace discovery logic as `beamtalk attach` — finds the most recently active workspace in `~/.beamtalk/workspaces/`. If multiple workspaces exist, defaults to the most recent and supports `--workspace` to select explicitly.

**Implementation:** Pure file reading — no workspace connection required. Works even when the workspace is stopped (reading historical logs). The `--follow` flag uses file watching (similar to `tail -f`).

**Output format:** Passes through the logger formatter output. Domain prefixes distinguish log origins:
```text
2026-03-17T14:23:01.234Z [debug] [runtime] beamtalk_supervisor:startChild/2 Starting child counter_1 under actor_sup
2026-03-17T14:23:01.235Z [info]  [runtime] beamtalk_actor:init/1 Actor initialized: class=Counter, state_keys=[count]
2026-03-17T14:23:01.236Z [debug] [Counter] increment: incrementing
2026-03-17T14:23:01.240Z [debug] [runtime] beamtalk_dispatch:lookup/5 Counter >> increment: resolved in 12μs
2026-03-17T14:23:01.241Z [info]  [otp] supervisor: {local,actor_sup} started: [{pid,<0.234.0>}]
```

## Prior Art

### Erlang/OTP Logger
The gold standard for per-module runtime control. `logger:set_module_level(Module, Level)` takes effect immediately. `logger:i()` dumps full configuration. `logger:get_module_level()` lists all overrides. Beamtalk wraps this directly but hides Erlang module names behind symbolic subsystem names and class references.

### Elixir Logger
Wraps OTP logger with clean Elixir API. `Logger.put_module_level(MyModule, :debug)` per-module. `Logger.put_application_level(:my_app, :debug)` per-OTP-app (sets all modules in the app). Also supports per-process level via `Logger.put_process_level(pid, :debug)`. Beamtalk's subsystem grouping is analogous to Elixir's per-application level — one symbol enables multiple related modules.

Critically, Elixir's `Logger.debug/1` is a **macro** that expands inline at the call site, injecting `__MODULE__`, function name, and line number as caller metadata. This means OTP logger sees the correct calling module for per-module filtering. Beamtalk adopts the same approach via codegen — the compiler recognizes `Logger debug:` and generates inline `logger:log/3` calls with class metadata, avoiding the indirection through `beamtalk_logger.erl`.

### Pharo Smalltalk (Beacon)
Filtering is by signal *class* (Smalltalk class), not by emitting class or severity. You subscribe loggers to specific signal types. The live image makes everything introspectable via browsers. Beamtalk's approach is closer to OTP than Beacon — we use severity levels with per-target filtering rather than signal-type subscriptions.

### Python logging
Hierarchical dot-separated namespace (`getLogger('myapp.db')`). Children inherit parent config. `getLogger(__name__)` convention means module structure = logger structure. The hierarchy is elegant but doesn't map naturally to Beamtalk's flat class namespace. Beamtalk's explicit subsystem registry is simpler and more discoverable.

### Elixir Mix / Phoenix
`mix phx.server` outputs logs to console. No separate "show me the logs" command because logs go to stdout. Beamtalk's workspace model (background process with file logging) requires a separate CLI command.

## User Impact

### Newcomer (coming from Python/JS/Ruby)
- `Beamtalk logLevel: #debug` is immediately intuitive
- `Beamtalk debugTargets` provides discoverability — no need to guess subsystem names
- `beamtalk logs --follow` matches their expectations from `heroku logs --tail` or `docker logs -f`
- Error messages guide them to valid options

### Smalltalk Developer
- `Beamtalk` as the system facade for logging config follows Pharo's pattern where `Smalltalk` (the image) owns system-level configuration
- Message-passing semantics preserved — `enableDebug:` is a message send, not a magic command
- Discoverable via `Beamtalk help: Beamtalk` — no separate tooling needed

### Erlang/BEAM Developer
- Recognizes the OTP logger underneath — `Beamtalk loggerInfo` shows the familiar handler/filter structure
- Can drop to `logger:i()` in an Erlang shell if needed — no abstraction leaks
- Per-module level granularity matches their expectations from `logger:set_module_level/2`
- Subsystem grouping is a convenience they don't get in raw OTP

### Production Operator
- `beamtalk logs --follow --level error` for monitoring without a REPL
- `beamtalk logs --path` for integration with external log aggregation (filebeat, fluentd)
- `Beamtalk logFormat: #json` for structured logging to aggregation systems (Datadog, Splunk, ELK, Loki) — follows the standard BEAM production pattern of JSON to file + sidecar
- No workspace connection required for log access — works on stopped workspaces

## Steelman Analysis

### Alternative B: Logging Control on `Workspace` (WorkspaceInterface)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Workspace already has `actors`, `load:`, `test` — I'd look here for debugging tools because debugging is part of my working session" |
| **Smalltalk purist** | "In Pharo, the Transcript (which is session-scoped) is the primary debugging output — session-level control makes sense" |
| **BEAM veteran** | "In IEx, you configure Logger in your session — it's conceptually per-session even if it affects the VM" |
| **Operator** | "Workspace is where I manage running state — actors, classes, tests. Logging is another operational concern" |

**Why we didn't choose this:** Logging configuration affects the entire VM, not just one workspace. If Beamtalk ever supports multiple workspaces per node, `Workspace logLevel: #debug` would be ambiguous. `Beamtalk` is the right scope because it represents the runtime, not the project session.

### Alternative C: Enhanced `Logger` Class

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Logger logs things, Logger configures logging — it's one concept, one class. Python does it this way." |
| **BEAM veteran** | "This maps 1:1 to OTP's `logger` module — `Logger level` = `logger:get_primary_config()`, `Logger enableDebug:` = `logger:set_module_level/2`. Exact same mental model." |
| **Language designer** | "Clean domain cohesion. Emission and configuration are both facets of the logging domain. Separating them across two unrelated classes is arbitrary." |

**Why we didn't choose this:** `Logger` is a stdlib class in the Object System Context. Logging configuration (OTP handler management, per-module/per-process level overrides, filter installation) is a Runtime Context concern. Adding these to `Logger` would cross DDD boundaries — the same reason we don't put class registry queries on `String`. This mirrors the industry pattern: Java's `Logger` doesn't hold `LogManager` config, .NET's `ILogger` doesn't hold `ILoggerFactory` config, Rust's `tracing` macros don't hold `Subscriber` config. Emission and configuration are separate concerns in separate contexts.

### Tension Points

- **Erlang developers** would naturally look at `Logger` for configuration (matching `logger:set_module_level/2`). They'll need to learn that Beamtalk splits emission from configuration.
- **The "read-only" principle** from ADR 0040 is bent but not broken. `Beamtalk` was described as read-only for user code in the context of namespace mutation. Logging configuration is system tooling. The same argument applies to a future `Beamtalk gc` or `Beamtalk memoryUsage`. However, this is a genuinely close call — the adversarial review noted that `Beamtalk` with 15+ methods spanning introspection, documentation, and logging configuration is "already fragmented within itself." The deciding factor is discoverability (users type `Beamtalk` first) rather than a clear architectural principle. If `Beamtalk` grows further beyond logging (tracing, metrics, GC control), a separate system-tooling object may become necessary.
- **Alternative C (Logger) is stronger than it first appears.** The BEAM veteran argument — "Logger controls logging, full stop, just like OTP's `logger` module" — is architecturally clean. If `Beamtalk help: Logger` showed configuration methods alongside emission methods, discoverability would be equivalent. The DDD boundary argument (stdlib vs runtime context) is principled, but a counterargument exists: OTP's own `logger` module combines emission and configuration in a single module, and OTP is the gold standard for BEAM architecture. The split is defensible, not obvious.

## Alternatives Considered

### Alternative B: Logging Control on `Workspace`

See Steelman Analysis above. Rejected because logging is VM-scoped, not project-scoped.

### Alternative C: Enhanced `Logger` Class

See Steelman Analysis above. Rejected because it conflates emission with configuration and reduces discoverability.

### Alternative D: Do Nothing — Document Erlang FFI

Users can already call OTP logger directly via Erlang FFI:
```beamtalk
(Erlang logger) set_module_level: #beamtalk_supervisor level: #debug
(Erlang logger) i   // full config dump
```
Rejected because: requires knowing Erlang module names (`beamtalk_supervisor` not `#supervisor`), provides no discoverability, no subsystem grouping, no CLI access. The FFI escape hatch remains available for power users, but the 90% case should be Beamtalk-native.

### Alternative E: New `LogConfig` or `Debugging` Class

A dedicated new singleton class for logging configuration. Rejected because it adds yet another global singleton to discover. The system already has `Beamtalk` and `Workspace` — a third introspection object fragments the mental model. If a user wants to understand the system, they should only need `Beamtalk`.

## Consequences

### Positive
- Targeted debugging becomes possible without drowning in noise — enable `#supervisor` only, see only supervisor events
- Full discoverability — `debugTargets` lists what's available, `activeDebugTargets` shows what's on
- Log domains (`[beamtalk, runtime]` vs `[beamtalk, user]`) clearly distinguish Erlang runtime internals from Beamtalk user code in log output
- Operators can access logs without a REPL session via `beamtalk logs`
- Builds directly on OTP logger — no custom logging framework, no abstraction inversion
- `Logger` stays simple and focused on emission
- Compiler-inlined Logger calls match Elixir's proven approach and ensure per-class filtering works correctly

### Negative
- `Beamtalk` grows from 6 instance methods to 16 (including format switching in Phase 3; 14 without). The core debugging feature adds 8 methods. For comparison, Pharo's `SmalltalkImage` has 50+ methods.
- `Logger setLevel:` is deprecated but must remain for backwards compatibility
- Subsystem name registry is manually maintained — adding a new subsystem requires updating the Erlang map
- Erlang developers may initially look for configuration on `Logger` before finding it on `Beamtalk`
- Codegen change (Phase 0) means `Logger debug:` is no longer a regular message send at the BEAM level — the compiler special-cases it, similar to how `+` on integers is optimized. This is a pragmatic departure from "everything is a message send"
- User-class metadata filtering adds a primary logger filter per enabled class, which has a small per-event overhead. OTP logger evaluates all primary filters on every log event. Unlikely to matter in practice, but worth noting.

### Neutral
- OTP logger's full filter pipeline is not exposed — `Beamtalk` provides the 90% use case (per-target level override). Power users can still call Erlang's `logger` module directly via FFI
- `beamtalk logs` reads files directly, not via the workspace protocol — it works offline but won't show in-memory log events that haven't been flushed
- Per-actor debug (`enableDebug: myCounter`) is transient — it's lost if the actor restarts. This is appropriate for live debugging but means you can't persistently monitor a specific actor across restarts. Class-level debug (`enableDebug: Counter`) is persistent and covers all instances including newly created ones
- Custom log handler management (adding/removing handlers via `Beamtalk`) is not included. Users needing additional handlers (e.g., a separate audit log or a remote log shipper) can use Erlang FFI (`(Erlang logger) add_handler: ...`). A Beamtalk-native handler API can be added later when there's demand, likely alongside the metrics/instrumentation work (BT-1429)

## Implementation

### Phase 0: Logger Codegen — Inline `Logger` Calls at Compile Time (M)

**Prerequisite for user-class debug filtering.** The compiler must recognize `Logger debug:`, `Logger info:`, `Logger warn:`, `Logger error:` (and their `*:metadata:` variants) and generate direct `logger:log/3` calls at the call site, injecting domain and class metadata. Without this, per-class filtering cannot work because all `Logger` calls would originate from `beamtalk_logger.erl`.

- Add pattern recognition in codegen for `Logger` message sends
- Generate `logger:log(Level, Msg, #{domain => [beamtalk, user], beamtalk_class => ClassName, beamtalk_selector => Selector})` inline
- `Logger debug:metadata:` merges user metadata with compiler-injected metadata
- `beamtalk_logger.erl` remains for Erlang FFI usage and `Logger setLevel:` (deprecated)
- Stdlib and e2e tests for codegen-emitted logger calls
- Verify that per-module level filtering works on compiled class modules

**Affected components:** `crates/beamtalk-core/src/codegen/core_erlang/` (Logger call recognition + inline emission), `beamtalk_logger.erl` (unchanged for FFI), stdlib tests, e2e tests

### Phase 1: Core API on BeamtalkInterface (M)
- Create new `beamtalk_logging_config.erl` in `beamtalk_runtime` app — this keeps logging configuration in the Runtime Context, not the stdlib Object System Context. `BeamtalkInterface.bt` delegates logging methods via `(Erlang beamtalk_logging_config)`, the same way it delegates introspection methods via `(Erlang beamtalk_interface)`
- Implement: `logLevel/0`, `logLevel/1`, `debugTargets/0`, `enableDebug/1`, `disableDebug/1`, `activeDebugTargets/0`, `disableAllDebug/0`, `loggerInfo/0`
- `logLevel/1` sets OTP primary log level (same as current `Logger setLevel:`)
- Add subsystem name registry (symbol → module list map)
- Add metadata-based filter installation for user classes (`enableDebug: Counter` → filter on `#{beamtalk_class => 'Counter'}`)
- Add per-actor debug via `logger:set_process_level/2` (detects actor references by tuple shape)
- Add debug target tracking ETS table
- Add user class → BEAM module resolution via class registry
- Update `BeamtalkInterface.bt` with Erlang FFI calls to `beamtalk_logging_config`
- Deprecate `Logger setLevel:` (keep working, add `?LOG_WARNING` suggesting `Beamtalk logLevel:`)
- EUnit tests for all new functions

**Affected components:** `runtime/apps/beamtalk_runtime/src/beamtalk_logging_config.erl` (new), `stdlib/src/BeamtalkInterface.bt`, `runtime/apps/beamtalk_stdlib/src/beamtalk_logger.erl` (deprecation warning)

### Phase 2: `beamtalk logs` CLI Command (M)
- Add `logs` subcommand to `beamtalk-cli`
- Implement workspace log file discovery (reuse workspace discovery from `attach`)
- Implement `--follow` (file watching), `--level` (line-level filtering), `--path` (print path)
- Add CLI integration tests

**Affected components:** `crates/beamtalk-cli/src/commands/logs.rs` (new), `crates/beamtalk-cli/src/main.rs` (register subcommand)

### Phase 3: Log Format Switching (S)
- Add `logger_formatter_json` (or `thoas` + custom formatter) as a runtime dependency
- Add `logFormat/0`, `logFormat/1` to `beamtalk_logging_config.erl`
- Implement formatter swap via `logger:update_handler_config(beamtalk_file_log, formatter, {Module, Config})`
- Add `--format json|text` flag to `beamtalk logs` CLI command
- EUnit tests

**Affected components:** `runtime/apps/beamtalk_runtime/src/beamtalk_logging_config.erl`, `stdlib/src/BeamtalkInterface.bt`, `runtime/rebar.config` (new dep), `crates/beamtalk-cli/src/commands/logs.rs`

### Phase 4: MCP Debug Target (S)
- Wire `#mcp` subsystem symbol to Rust-side tracing filter
- Requires cross-boundary communication (Beamtalk → Rust) for the MCP tracing subscriber
- May use workspace protocol message or environment variable

**Affected components:** `crates/beamtalk-mcp/src/main.rs`, `runtime/apps/beamtalk_runtime/src/beamtalk_logging_config.erl`

## Migration Path

`Logger setLevel:` is deprecated in favor of `Beamtalk logLevel:`. Migration is straightforward:

```beamtalk
// Old
Logger setLevel: #debug

// New
Beamtalk logLevel: #debug
```

`Logger setLevel:` continues to work but emits a deprecation warning at the call site suggesting the new API. All other `Logger` emission methods (`info:`, `warn:`, `error:`, `debug:`, `*:metadata:`) are unchanged.

## Performance Note

Per-module level filtering via `logger:set_module_level/2` is evaluated *before* the log message is formatted. This means `?LOG_DEBUG(...)` calls on the dispatch hot path have near-zero overhead when debug is not enabled for that module — OTP checks the module level and short-circuits. This is critical because the logging epic (BT-1423) adds debug calls to performance-sensitive paths like `beamtalk_dispatch:lookup/5`.

## Test Considerations

The test `sys.config` sets `logger_level` to `error` to suppress log noise during test runs. This is unaffected by the API changes — `Beamtalk logLevel:` modifies runtime state, not config files. E2e tests that check REPL output are unaffected since they use separate BEAM nodes with their own logger config.

## References
- Related issues: BT-1423 (Runtime Observability Logging epic), BT-1424 (SASL), BT-1429 (Runtime metrics ADR)
- Related ADRs: ADR 0040 (Workspace-Native REPL Commands — two-object model), ADR 0010 (Global Objects and Singleton Dispatch)
- OTP Logger docs: https://www.erlang.org/doc/apps/kernel/logger.html
- Elixir Logger docs: https://hexdocs.pm/logger/Logger.html
