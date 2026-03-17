# ADR 0065: Complete OTP Primitives for Actor Lifecycle and Supervision

## Status
Accepted (2026-03-17)

## Context

With BT-1442 (Actor `monitor`/`pid`/`onExit:`) landed and `terminate:` codegen wired (BT-1451 in progress), Beamtalk now has most OTP primitives needed for production supervision. However, several gaps prevent idiomatic OTP usage.

### What We Have (Solid)

| Primitive | OTP Equivalent | Status |
|-----------|---------------|--------|
| `Actor` (spawn, stop, isAlive, state) | gen_server | Implemented |
| `initialize` | init/1 lifecycle | Implemented |
| `terminate:` | terminate/2 lifecycle | Codegen exists, runtime fix in progress (BT-1451) |
| `Supervisor` / `DynamicSupervisor` | supervisor / dynamic_supervisor | Implemented (ADR 0059) |
| `pid`, `monitor`, `demonitor` | erlang:monitor | Implemented (BT-1442) |
| `onExit:` | monitor + DOWN handler | Implemented (BT-1442) |
| `method!` (async cast) | gen_server:cast | Implemented (ADR 0043) |
| `SupervisionSpec` | child_spec | Implemented (ADR 0059) |

### Gaps Evaluated

This ADR evaluates five gaps and makes a disposition for each:

1. **`handleInfo:`** — Receiving raw messages (timer events, DOWN tuples, system messages)
2. **Named registration** — Actor lookup by name instead of reference
3. **Links** — Explicit bidirectional process linking
4. **Graceful shutdown propagation** — `terminate:` with proper reason and timeout
5. **Hot code reload** — User-defined state migration on code upgrade

## Decision

### Core design: Actor/Server class hierarchy

Introduce `Server` as an **abstract** subclass of `Actor` that exposes BEAM-level OTP primitives. The class hierarchy expresses the abstraction boundary:

- **`Actor`** — Beamtalk-level: message-passing, state, Timer API, lifecycle (`initialize`, `terminate:`). Most users, most of the time.
- **`Server`** (abstract) — BEAM-level: raw message handling (`handleInfo:`), and the natural home for future OTP features (named registration, `trapExit`, `codeChange:from:`).

```text
Object
  └── Actor           # Beamtalk objects — messages, state, Timer
        └── Server    # BEAM processes — handleInfo:, raw OTP interop (abstract)
```

This uses OO's core strength: the class you extend communicates your relationship to the runtime. `Actor subclass: Counter` says "I'm a Beamtalk object." `Server subclass: PeriodicWorker` says "I'm a BEAM process that happens to be written in Beamtalk."

Server is abstract — you subclass it, you don't spawn it directly. Defining `handleInfo:` on a Server subclass is optional; a Server without `handleInfo:` simply ignores raw messages (same as Actor). The value of extending Server is signaling intent and unlocking future OTP features.

**Why introduce Server now, not later?** Server gates only `handleInfo:` today, but it prevents future OTP features (`trapExit`, named registration, `codeChange:from:`) from accumulating on Actor as optional methods. Adding Server later would require migrating existing code. Adding it now — when the API surface is minimal — is the cheapest time to establish the hierarchy.

### Actor — simple actors (no changes)

```beamtalk
Actor subclass: Counter
  state: count = 0
  increment => self.count := self.count + 1
  getValue => self.count
```

Timer API works on any Actor:

```beamtalk
Actor subclass: Ticker
  state: count = 0

  initialize =>
    Timer every: 1000 do: [self tick!]   // async cast — MUST use ! not .

  tick => self.count := self.count + 1
  getValue => self.count
```

No `state:` for the timer, no `terminate:` cleanup — Timer processes are **linked to the calling process** via `spawn_link`. When the actor dies, the linked Timer process dies automatically.

### Server — full OTP interop

```beamtalk
Server subclass: PeriodicWorker
  state: count = 0

  initialize =>
    Erlang erlang send_after: 1000 dest: (self pid) msg: #tick

  handleInfo: msg =>
    msg match: [
      #tick -> [
        self.count := self.count + 1.
        Erlang erlang send_after: 1000 dest: (self pid) msg: #tick
      ];
      {#DOWN, _ref, #process, _pid, reason} -> [
        Logger info: "Monitored process exited: " ++ reason displayString
      ];
      _ -> nil
    ]

  getValue => self.count
```

`handleInfo:` is defined on `Server` with a default no-op implementation (`handleInfo: msg => nil`). Actor does not define it. This means:
- Server subclasses can override `handleInfo:` to handle raw messages
- Sending `handleInfo:` to a plain Actor raises a normal `doesNotUnderstand` — no special-case compiler code needed
- The codegen uses `is_server_subclass()` to decide which `handle_info/2` to generate (dispatch vs ignore stub), following the same pattern as the existing `is_supervisor_subclass` check

#### Migration: Actor to Server

Promoting an Actor to a Server is a one-word change. All existing methods continue to work — Server inherits everything from Actor:

```beamtalk
// Before
Actor subclass: MyThing
  // ...

// After — all existing methods still work, handleInfo: now available
Server subclass: MyThing
  handleInfo: msg => ...
```

#### What lives on Server (now and future)

| Feature | Status | Rationale |
|---------|--------|-----------|
| `handleInfo:` | This ADR | Raw BEAM message reception |
| Named registration (`spawnAs:`) | Future ADR | Process-level identity |
| `trapExit` | Future | Process-level exit signal handling |
| `codeChange:from:` | Future | Process-level state migration |

Features that stay on Actor (all processes need them):
- `initialize`, `terminate:`, `pid`, `monitor`, `onExit:`, `spawn`, `stop`, `isAlive`

### handleInfo: semantics

**Return value:** Always discarded. The method is called for its side effects (state mutation, logging, spawning work).

**Error handling — log and continue (not crash):** If `handleInfo:` raises a Beamtalk error (DNU, match failure, etc.), the generated `handle_info/2` logs a warning (including error details) via `?LOG_WARNING` and returns `{noreply, State}` with the **pre-call state**. The actor does **not** crash.

This is a **deliberate divergence from OTP's default**, but it matches what the Elixir ecosystem converged on in practice. In OTP, an unmatched `handle_info` clause crashes the gen_server. This is annoying enough that virtually every production Elixir GenServer includes the same boilerplate catch-all:

```elixir
# Every production Elixir GenServer writes this:
def handle_info(unexpected, state) do
  Logger.warning("Unexpected message: #{inspect(unexpected)}")
  {:noreply, state}
end
```

This pattern is so universal it has been debated repeatedly in the Elixir community (see [References](#references)). The consensus: `handle_info` receives external messages you don't fully control — stale timers after hot reload, library internals, monitoring messages from code you didn't write. Crashing on these punishes the receiver for the sender's mistake.

Beamtalk bakes this best practice in: errors in `handleInfo:` are logged and the server continues. This means one fewer thing for developers to remember, at the cost of a semantic difference from raw OTP that BEAM veterans should be aware of.

**Deadlock rules (ADR 0043 applies):** `handleInfo:` executes inside the actor's gen_server process. The same sync-call deadlock rules from ADR 0043 apply: if your handler calls another actor via `.` (sync) that may call back into `self`, it will deadlock. Use `!` (async cast) for outbound sends from `handleInfo:` when re-entrant calls are possible.

### Timer lifecycle: linked to caller

Timer processes (`Timer every:do:` and `Timer after:do:`) are **linked to the calling process** via `spawn_link`. This is a change from the current `spawn` implementation. The link ensures:
- When the actor dies, the Timer process dies automatically — no orphaned ticks
- When the Timer process crashes (unlikely — block execution is wrapped in `catch`), the link propagates the crash to the actor, which triggers supervisor restart

This eliminates the entire class of "forgot to cancel timer in terminate:" bugs. `cancel` remains available for explicit lifecycle control when needed.

**Compiler lint — sync send in Timer block:** A `self method.` (sync call) inside a `Timer every:do:` or `Timer after:do:` block emits a warning:

```text
warning: Sync send 'self tick.' inside Timer block will deadlock.
  --> src/Ticker.bt:5:40
   |
 5 |     Timer every: 1000 do: [self tick.]
   |                            ^^^^^^^^^^
   = hint: Use 'self tick!' (async cast) instead — Timer blocks execute in a separate process
```

### Codegen changes

For **Server subclasses** (class extends Server, or any ancestor is Server), the generated `handle_info/2` dispatches to `handleInfo:`:

```erlang
'handle_info'/2 = fun (Msg, State) ->
    let Self = call 'beamtalk_actor':'make_self'(State) in
    case call 'Module':'dispatch'('handleInfo:', [Msg], Self, State) of
        <{'reply', _Result, NewState}> when 'true' -> {'noreply', NewState}
        <{'error', Error, _ErrState}> when 'true' ->
            call 'logger':'warning'(<<"handleInfo: raised error: ~p">>, [Error], #{})
            {'noreply', State}    % explicitly use pre-call state
        <_Other> when 'true' -> {'noreply', State}
    end
```

For **Actor subclasses** (not Server), the current ignore-all stub is generated:

```erlang
'handle_info'/2 = fun (_Msg, State) ->
    {'noreply', State}
```

The check is simple: "is this class a Server?" — no need to walk the hierarchy looking for a `handleInfo:` method definition.

### trap_exit interaction

Servers that set `process_flag(trap_exit, true)` via Erlang FFI will receive `{'EXIT', Pid, Reason}` tuples in `handleInfo:`. This interacts with `onExit:` (BT-1442): if a monitored process is also linked, the server receives both a `{'DOWN', ...}` tuple (from the monitor) and an `{'EXIT', ...}` tuple (from the trapped link). The double-notification scenario is not prevented — users combining `trap_exit` with `onExit:` must handle deduplication themselves. A first-class `trapExit` method on Server is a future enhancement.

### Testing handleInfo:

BUnit tests can trigger `handleInfo:` by sending raw messages to the server's pid:

```beamtalk
testHandleInfoTick =>
  worker := PeriodicWorker spawn.
  Erlang erlang send: worker pid msg: #tick.
  Timer sleep: 50.
  self assert: worker getValue equals: 2   // 1 from initialize + 1 from test
```

### Gap 2: Named Registration — Defer

`Supervisor which:` is the blessed pattern for service discovery in v0.1:

```beamtalk
app := MySupervisor current.
db := app which: DatabasePool.
db query: "SELECT 1"
```

Named registration (`spawnAs:` / `Server named:`) is deferred to a dedicated ADR post-v0.1. When implemented, it will live on Server — process-level identity is a BEAM concept. Rationale for deferral:
- Supervision tree discovery is idiomatic OTP
- Named registration adds complexity: name conflicts, crash recovery re-registration, global vs local scope
- The runtime already supports `{local, Name}` in `beamtalk_actor:start_link/3` — the plumbing exists when we need it

**Known limitation:** `which:` performs a linear scan via `supervisor:which_children/1` + `lists:search/2` — O(n) in the number of children. This is fine for static supervisors with a handful of children, but would be slow on DynamicSupervisors with thousands of children. Named registration would provide O(1) lookup. This is acceptable for v0.1 but is a motivating use case for the future named registration ADR.

### Gap 3: Links — Reject

Explicit `link`/`unlink` is rejected for Beamtalk's API surface. Rationale:
- Monitors (`onExit:`) provide unidirectional crash notification — sufficient for nearly all patterns
- Supervisors provide bidirectional crash propagation within a tree
- No peer language in Beamtalk's reference set (Pony, Newspeak, Akka Typed) exposes raw links
- Links are low-level OTP plumbing; exposing them would encourage anti-patterns (non-supervised relationships)
- Erlang FFI (`Erlang erlang link:`) remains available as an escape hatch for power users

### Gap 4: Graceful Shutdown Propagation — Implement (BT-1451 in progress)

BT-1451 is already addressing this. The expected behavior:

```beamtalk
Actor subclass: HttpServer
  terminate: reason =>
    self closeConnections.
    self deregisterFromLoadBalancer.
    Logger info: "Shutting down" metadata: #{"reason" => reason displayString}
```

Shutdown timeout should be configurable via `SupervisionSpec`. Currently the shutdown value is hardcoded (5000ms for workers, `#infinity` for supervisors). BT-1451 or a follow-up should add `withShutdown:` to the fluent builder:

```beamtalk
HttpServer supervisionSpec withShutdown: 30000   // 30s graceful shutdown (new method)
```

BT-1451 (approved, PR #1478) addresses `terminate:` runtime support. The `withShutdown:` builder method is not included in BT-1451 — it will be created as a follow-up issue during `/plan-adr`.

### Gap 5: Hot Code Reload — Defer

The current `code_change/3` handles basic field migration (adding/removing fields via `beamtalk_hot_reload`). A user-facing `codeChange:from:` method for custom state migration is deferred until there's a real production use case. When implemented, it will live on Server. The current mechanism is sufficient for development workflows.

## Prior Art

### Elixir GenServer — `handle_info`
Elixir's `handle_info/2` is the direct model. All messages not sent via `call`/`cast` flow through `handle_info` — timer ticks, monitor DOWN notifications, linked process exits. The canonical timer pattern uses `Process.send_after(self(), :tick, interval)` in both `init/1` and `handle_info/2`. This is simple and well-understood but requires manual timer cancellation in `terminate/2`.

A well-known pain point: an unmatched `handle_info` clause crashes the GenServer. The Elixir community consensus is that every production GenServer needs a catch-all `handle_info` that logs and ignores unexpected messages. This is effectively universal boilerplate.

**What we adopt:** The `handleInfo:` method mirrors `handle_info/2` semantics — same OTP callback, Beamtalk syntax. We bake in the community's log-and-continue best practice as the default error contract, eliminating the boilerplate catch-all.

**What we adapt:** Beamtalk separates the simple actor API (Timer, message-passing) from the OTP escape hatch (handleInfo:) via the Actor/Server class hierarchy. Elixir has no such separation — all GenServers are equally low-level.

### Akka Typed — Signals vs Messages
Akka Typed separates *signals* (lifecycle events: `PreRestart`, `PostStop`, `Terminated`) from *messages* (user-defined typed protocol) via `receiveSignal`. Timers use `Behaviors.withTimers` with a keyed `TimerScheduler` — timers are automatically cancelled on actor restart and deduplicated by key.

**What we learn:** The signal/message split is clean but heavyweight. Beamtalk's Actor/Server hierarchy achieves similar layering with less ceremony — Actor handles lifecycle, Server adds raw message access. Akka's keyed timers are elegant — a future `Timer` enhancement could add key-based deduplication.

### Pony — No Raw Messages
Pony's actor model is fully typed: every message is a statically typed behavior call. There is no catch-all handler, no raw message reception, and no global actor registry. Discovery is by reference passing only.

**What we learn:** Pony validates that a typed-first approach (Beamtalk's Actor + Timer API) is the right default. But Beamtalk runs on BEAM where raw messages are a reality (monitors, timers, interop), so the Server escape hatch is necessary.

### Newspeak — Promise-Based Actors
Newspeak's actors communicate via asynchronous message sends that return promises. No `handle_info` equivalent exists — all messages are typed method calls. No built-in supervision or actor registry.

**What we learn:** Newspeak confirms that Beamtalk's high-level approach (Actor, normal method sends) is aligned with the Smalltalk tradition. Server's `handleInfo:` is a pragmatic BEAM departure.

### Pharo Smalltalk — Green Threads and Class Hierarchy
Pharo uses cooperative green threads sharing a heap — no actor isolation, no mailbox, no supervision. But Pharo's class hierarchy (`Object → Process → different kinds of process`) demonstrates the pattern of using inheritance to layer process capabilities. Beamtalk's Actor/Server hierarchy follows the same Smalltalk tradition: use the class hierarchy to communicate intent.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Actor** is all they need. Timer API reads naturally, no OTP concepts to learn
- **Server** exists but is clearly labelled "advanced" — they'll encounter it when they're ready
- **Migration is a one-word change**: `Actor subclass:` → `Server subclass:` when they need raw OTP features
- **`Supervisor which:`** for discovery is more explicit than global names — easier to trace

### Smalltalk Developer
- **Actor/Server hierarchy** is textbook Smalltalk design — use the class hierarchy to organize capabilities
- **Timer API** preserves message-passing purity on Actor — `self tick!` is just a message send
- **Server** is explicitly an FFI escape hatch — like Smalltalk's primitive access. The boundary is clear
- **Rejecting links** keeps the abstraction clean — Smalltalk has no link equivalent

### Erlang/BEAM Developer
- **Server** maps directly to "I'm writing a gen_server" — `handleInfo:` is handle_info/2
- **`match:` with tuple destructuring** makes DOWN/EXIT handling feel natural
- **Actor** may feel restrictive at first, but they'll appreciate that teammates who don't know OTP can be productive with Actor while they use Server for the systems work
- **Log-and-continue** differs from OTP's crash-by-default — documented clearly, matches Elixir community practice

### Production Operator
- **Server** processes in observer are immediately identifiable as "full OTP" — expect raw message handling
- **Actor** processes are simpler — pure message-passing, no raw message surprises
- **Graceful shutdown** (BT-1451) enables proper drain/cleanup sequences
- Both types generate standard gen_server — visible in observer, debuggable with `sys:get_state/1`

### Tooling Developer
- **`handleInfo:`** is a regular method on Server — LSP can provide completions
- **Compile-time check** is a simple superclass check (extends Server?), not a method-existence walk
- **No new syntax** — Server is a stdlib class, `handleInfo:` is a conventionally-named method

## Steelman Analysis

### Timer-Only — no handleInfo: at all (rejected)

| Cohort | Best argument for Timer-Only |
|--------|------------------------------|
| **Newcomer** | "One API to learn. `Timer every:` and `onExit:` cover every pattern I'll hit in my first year. I never need to know what a tuple is." |
| **Smalltalk purist** | "Clean separation. No Erlang terms leak into my object world — everything is message-passing. The BEAM is an implementation detail, not something my code should know about." |
| **BEAM veteran** | "Timer + `onExit:` covers 90% of use cases. For the other 10%, I can use Erlang FFI directly. A half-baked handleInfo: that swallows errors differently from OTP is worse than no handleInfo: — it's a trap for anyone who thinks they know handle_info semantics." |
| **Language designer** | "Every abstraction layer you add is a layer someone has to learn. Timer is sufficient. Adding Server and handleInfo: to the class hierarchy for 10% of users adds permanent complexity to the type system for everyone." |
| **Operator** | "Every Timer is a visible process in observer. With handleInfo:, timer ticks are invisible — just messages in a mailbox. Visible processes are easier to debug than invisible messages." |

### handleInfo: on any Actor — no Server class (rejected)

| Cohort | Best argument for handleInfo: on any Actor |
|--------|---------------------------------------------|
| **Newcomer** | "One base class, one concept. I don't have to choose between Actor and Server — I just add handleInfo: when I need it. Less to learn upfront." |
| **BEAM veteran** | "Every actor IS a gen_server. Hiding handle_info behind a subclass creates a false distinction. If I'm debugging a production issue and the actor is receiving unexpected messages, I want handle_info available everywhere, not gated behind a class change." |
| **Language designer** | "The Actor/Server split adds a class to the hierarchy, a migration path to document, and a decision point ('which base class?') that doesn't exist today. Optional methods are simpler than class hierarchies for gating capabilities." |
| **Operator** | "In production, I sometimes need to add handleInfo: to an actor that wasn't designed for it — to add instrumentation, catch stray messages, or debug a leak. A class hierarchy change is heavier than adding a method." |

### Actor/Server hierarchy (decided)

| Cohort | Best argument for Actor/Server |
|--------|--------------------------------|
| **Newcomer** | "`Actor subclass: Counter` — that's all I know, that's all I need. Server is labelled 'advanced'. If I ever need it, the compiler error tells me exactly what to change — one word." |
| **Smalltalk purist** | "This is textbook Smalltalk design. The class hierarchy communicates intent. `Actor` says 'I'm an object.' `Server` says 'I'm a process.' Pharo does the same with `Object → Process`. The inheritance chain IS the documentation." |
| **BEAM veteran** | "When I see `Server subclass:`, I immediately know: this is a full OTP service with handle_info, maybe trap_exit, maybe named registration. When I see `Actor subclass:`, I know it's simplified — I don't need to think about raw messages. The class name is triage information." |
| **Language designer** | "This eliminates the 'two mechanisms on one class' objection entirely. Timer is Actor's mechanism. handleInfo: is Server's. They're not redundant — they're on different classes at different abstraction levels. The FAQ 'when do I use which?' answers itself from the class you extend. And future OTP features (named registration, trapExit, codeChange) have a natural home without polluting Actor's API." |
| **Operator** | "In observer, I can tell the difference between an Actor (simple, message-passing) and a Server (full OTP). That's free observability from the class hierarchy." |

### Tension Points

- **"One class is simpler" vs hierarchy:** The argument that optional methods are simpler than a class hierarchy is genuine. The counter: optional methods require lints, error messages about "only valid on Actors", and documentation explaining when to use which. The class hierarchy makes all of that implicit — the type system enforces the boundary.
- **"Every actor IS a gen_server" vs hiding capabilities:** BEAM veterans are right that Actor/Server is a false distinction at the BEAM level. But Beamtalk's value proposition is that most users shouldn't need to know it's a gen_server. The hierarchy serves the 90% (Actor users) while giving the 10% (Server users) full power.
- **Error contract:** BEAM veterans expect crash-on-error in handle_info (OTP default). Beamtalk's Server swallows errors (Elixir community best practice). Both positions are defensible. We chose swallowing because it eliminates universal boilerplate.
- **Named registration:** Deferred. `Supervisor which:` (O(n) scan) is sufficient for v0.1. Server is the natural home when it's implemented.
- **All cohorts agree** on rejecting links and deferring hot code reload.

## Alternatives Considered

### Alternative: Timer-Only (No handleInfo:)
Rely entirely on the Timer class for periodic work and `onExit:` for lifecycle notifications. Actors would never receive raw Erlang messages.

**Rejected because:** This makes BEAM interop impossible for legitimate use cases — receiving messages from Erlang processes, handling `erlang:send_after` ticks, processing raw monitor notifications. The Timer API works for Beamtalk-to-Beamtalk communication but breaks down at the BEAM boundary.

### Alternative: handleInfo: on any Actor (no Server class)
Allow any Actor subclass to define `handleInfo:` as an optional method, with a compile error if defined on a value type.

**Rejected because:** This puts two timer mechanisms (Timer API and send_after + handleInfo:) on the same class, creating a permanent FAQ: "when do I use Timer vs handleInfo:?" The Actor/Server hierarchy makes the answer structural — Timer is for Actor, handleInfo: is for Server. It also clutters Actor's conceptual surface with OTP escape hatches that 90% of users don't need, and provides no natural home for future OTP features (named registration, trapExit) without continuing to add optional methods to Actor.

### Alternative: Named Registration Now
Add `spawnAs:` and `Server named:` for v0.1.

**Rejected because:** Named registration introduces complexity (atom exhaustion risk, name conflict handling, crash recovery re-registration, global vs local vs via scope) that isn't justified by current use cases. `Supervisor which:` covers service discovery within a supervision tree. The runtime plumbing (`beamtalk_actor:start_link/3` already accepts `{local, Name}`) is ready when we need it.

### Alternative: Expose Link/Unlink
Add `actor link` and `actor unlink` to mirror `erlang:link/1` and `erlang:unlink/1`.

**Rejected because:** Links provide bidirectional crash propagation, but this is exactly what supervision trees already provide in a structured way. Exposing raw links encourages unstructured process relationships that are hard to reason about and debug. Monitors (`onExit:`) handle the "notify me when X dies" use case; supervisors handle "restart X when it dies". The Erlang FFI (`Erlang erlang link:`) is available for the rare case where raw links are truly needed.

## Consequences

### Positive
- Clean separation: Actor for Beamtalk-level programming, Server for BEAM-level programming
- The class hierarchy communicates intent — developers and operators know what to expect from each
- Future OTP features have a natural home on Server without polluting Actor's API
- No "two mechanisms on one class" confusion — Timer is Actor's, handleInfo: is Server's
- Compiler-guided migration from Actor to Server (one-word change, actionable error message)
- Timer `spawn_link` eliminates orphaned timer processes — no cleanup boilerplate needed
- handleInfo: error contract (log-and-continue) eliminates universal Elixir catch-all boilerplate

### Negative
- One more class in the hierarchy (Actor → Server) — adds a decision point for users
- Server's log-and-continue error handling differs from OTP's crash-by-default — may surprise BEAM veterans
- Timer implementation changes from `spawn` to `spawn_link` — behavioral change from the current Timer (if Timer process crashes, crash propagates to caller via link)
- Deferring named registration means some OTP patterns require workarounds (`Supervisor which:`), and `which:` is O(n) in child count

### Neutral
- Rejecting links has no practical impact — no known use case requires them beyond what monitors + supervisors provide
- Deferring hot code reload is low-risk — the current field migration handles development workflows
- `handleInfo:` return values are always discarded — the method is called for side effects only

## Implementation

### Phase 1: Server class and handleInfo: (M)
**Affected components:** Stdlib (Server.bt), Codegen (callbacks.rs, actor detection), Runtime (beamtalk_actor.erl)

1. **Stdlib:** Add `Server.bt` — `abstract Actor subclass: Server` with a default `handleInfo: msg => nil` (overridable no-op). Actor does not define `handleInfo:` — plain Actors raise DNU if sent this message. Verify that `Server.bt` generates a correct `init/1` that passes `InitArgs` through the two-hop chain (`UserClass → server:init → Actor base`), including `spawnWith:` args.
2. **Hierarchy resolution:** Add `is_server_subclass()` to `ClassHierarchy`, following the existing `is_supervisor_subclass` pattern. Enrich `class_superclass_index` in the package compiler so cross-file inheritance (`Server subclass: Foo` in one file, `Foo subclass: Bar` in another) resolves correctly — analogous to how `Actor` is handled today.
3. **Codegen:** In `generate_handle_info()` (callbacks.rs ~445-472), check if the class is a Server subclass via `is_server_subclass()`. If yes, generate dispatch to `handleInfo:` with error logging; if no (plain Actor), generate the current ignore-all stub.
4. **No special validation needed:** `handleInfo:` is a regular method on Server. Actor doesn't define it — DNU is the natural error. The codegen `is_server_subclass()` check determines which `handle_info/2` to generate.
4. **Timer: `spawn` → `spawn_link`:** Change `beamtalk_timer.erl` to use `spawn_link` instead of `spawn` for `after:do:` and `every:do:`. This links the Timer process to the calling process, ensuring automatic cleanup on caller death. The existing `catch Block()` in the timer loop prevents block errors from crashing the Timer process.
5. **Lint — sync send in Timer block:** Warn when a `self method.` (sync call) appears inside a `Timer every:do:` or `Timer after:do:` block. Suggest `self method!` (async cast) instead.
7. **Tests:** BUnit tests for Server + handleInfo: (using `Erlang erlang send:` to inject raw messages), DOWN message handling, unknown message ignoring, error-in-handler recovery. Integration tests for the two-hop init chain (`Server subclass: Foo` with `spawn` and `spawnWith:`). EUnit tests for the dispatch path and Timer `spawn_link` behavior. Lint tests for sync-in-block warning.

### Phase 2: Graceful Shutdown — BT-1451 (approved, PR #1478)
BT-1451 delivers `terminate:` runtime support. Follow-up issue for `withShutdown:` on `SupervisionSpec` will be created during `/plan-adr`.

### Deferred
- Named registration (`spawnAs:` / `Server named:`) — separate ADR post-v0.1, lives on Server
- `trapExit` as first-class Server method — when a use case requires it beyond FFI
- User-defined `codeChange:from:` on Server — when a production use case emerges
- Links — rejected; revisit only if a compelling pattern requires them

## References
- Related issues: BT-1452 (this ADR), BT-1442 (monitor/pid/onExit — landed), BT-1451 (terminate: runtime fix — in progress)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0013 (Class Methods/Instantiation), ADR 0043 (Sync-by-Default Messaging), ADR 0059 (Supervision Tree Syntax)
- Code: `callbacks.rs:445-472` (handle_info codegen), `beamtalk_actor.erl:778-784` (handle_info runtime), `Timer.bt` (Timer class)
- Documentation: `docs/beamtalk-language-features.md` (actors, supervision, pattern matching)
- Elixir community precedent for log-and-continue in handle_info:
  - [Should we catch stray messages in a GenServer?](https://elixirforum.com/t/should-we-catch-stray-messages-in-a-genserver/2656)
  - [Ignoring in handle_info/2 is OK. Really?](https://elixirforum.com/t/ignoring-in-handle-info-2-is-ok-really/2064)
