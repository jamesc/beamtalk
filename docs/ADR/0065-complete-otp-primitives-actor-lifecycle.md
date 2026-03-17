# ADR 0065: Complete OTP Primitives for Actor Lifecycle and Supervision

## Status
Proposed (2026-03-17)

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

### Gap 1: `handleInfo:` — Implement (High-level API + escape hatch)

Actors gain an optional `handleInfo:` method that receives raw Erlang messages. The existing `Timer` API remains the recommended path for periodic work. `handleInfo:` is the escape hatch for OTP interop.

#### Timer API (recommended for most users)

```beamtalk
Actor subclass: Ticker
  state: count = 0
  state: timer = nil

  initialize =>
    self.timer := Timer every: 1000 do: [self tick!]   // async cast — MUST use ! not .

  tick => self.count := self.count + 1
  getValue => self.count
  terminate: reason => self.timer cancel
```

**Important:** The block passed to `Timer every:do:` executes in the Timer's process, not the actor's. Always use `!` (async cast) for sends back to the actor — using `.` (sync call) will deadlock the Timer process waiting for a reply from a potentially busy actor. Always cancel timers in `terminate:` — the Timer process is unlinked and will otherwise keep ticking into a dead pid indefinitely.

#### `handleInfo:` escape hatch (for OTP interop)

```beamtalk
Actor subclass: PeriodicWorker
  state: count = 0

  initialize =>
    // Use Erlang's send_after for in-process timer ticks
    Erlang erlang send_after: 1000 dest: self pid msg: #tick

  handleInfo: msg =>
    msg match: [
      #tick -> [
        self.count := self.count + 1.
        Erlang erlang send_after: 1000 dest: self pid msg: #tick
      ];
      {#DOWN, _ref, #process, _pid, reason} -> [
        Logger info: "Monitored process exited: " ++ reason displayString
      ];
      _ -> nil   // ignore unknown messages
    ]

  getValue => self.count
```

#### Error on misuse

```beamtalk
// Defining handleInfo: on a value type is a compile error:
Object subclass: NotAnActor
  handleInfo: msg => msg   // => Error: handleInfo: is only valid on Actor subclasses
```

#### Semantics and error contract

**Return value:** The return value of `handleInfo:` is always discarded. The method is called for its side effects (state mutation, logging, spawning work). The codegen extracts the updated state from the dispatch result and returns `{noreply, NewState}` to OTP.

**Error handling — log and continue (not crash):** If `handleInfo:` raises a Beamtalk error (DNU, match failure, etc.), the generated `handle_info/2` logs a warning (including error details) via `?LOG_WARNING` and returns `{noreply, State}` with the **pre-call state**. The actor does **not** crash. Any state mutations from the partially-executed method are rolled back to the pre-call state.

This is a **deliberate divergence from OTP's default**, but it matches what the Elixir ecosystem converged on in practice. In OTP, an unmatched `handle_info` clause crashes the gen_server. This is annoying enough that virtually every production Elixir GenServer includes the same boilerplate catch-all:

```elixir
# Every production Elixir GenServer writes this:
def handle_info(unexpected, state) do
  Logger.warning("Unexpected message: #{inspect(unexpected)}")
  {:noreply, state}
end
```

This pattern is so universal it has been debated repeatedly in the Elixir community (see [References](#references)). The consensus: `handle_info` receives external messages you don't fully control — stale timers after hot reload, library internals, monitoring messages from code you didn't write. Crashing on these punishes the receiver for the sender's mistake.

Beamtalk bakes this best practice in: errors in `handleInfo:` are logged and the actor continues. This means one fewer thing for developers to remember, at the cost of a semantic difference from raw OTP that BEAM veterans should be aware of.

**Deadlock rules (ADR 0043 applies):** `handleInfo:` executes inside the actor's gen_server process. The same sync-call deadlock rules from ADR 0043 apply: if your handler calls another actor via `.` (sync) that may call back into `self`, it will deadlock. Use `!` (async cast) for outbound sends from `handleInfo:` when re-entrant calls are possible.

#### Timer cleanup

When using the Timer API (`Timer every: 1000 do: [self tick!]`), the Timer spawns an **unlinked, unmonitored** process. If the actor dies without cancelling, the Timer process continues ticking — its `self tick!` casts silently succeed (OTP's `gen_server:cast` to a dead pid returns `ok`) and the timer lives forever. This is why the recommended Ticker example above includes `terminate: reason => self.timer cancel`.

The `handleInfo:` + `send_after` pattern avoids this issue entirely — timer messages are delivered to the actor's own mailbox and stop when the process dies.

#### trap_exit interaction

Actors that set `process_flag(trap_exit, true)` via Erlang FFI will receive `{'EXIT', Pid, Reason}` tuples in `handleInfo:`. This interacts with `onExit:` (BT-1442): if a monitored process is also linked, the actor receives both a `{'DOWN', ...}` tuple (from the monitor) and an `{'EXIT', ...}` tuple (from the trapped link). This double-notification scenario is not prevented — users combining `trap_exit` with `onExit:` must handle deduplication themselves. Explicit `trap_exit` support is not a first-class Beamtalk feature; it is available only through the Erlang FFI escape hatch.

#### Testing handleInfo:

BUnit tests can trigger `handleInfo:` by using `Erlang erlang send:` to send raw messages to the actor's pid:

```beamtalk
testHandleInfoTick =>
  worker := PeriodicWorker spawn.
  Erlang erlang send: worker pid msg: #tick.
  Timer sleep: 50.
  self assert: worker getValue equals: 2   // 1 from initialize + 1 from test
```

#### Codegen changes

The generated `handle_info/2` callback changes from unconditional delegation to conditional dispatch. The codegen must check whether `handleInfo:` is defined **anywhere in the class hierarchy** (not just the immediate class), since `dispatch` walks the inheritance chain and would otherwise fall through to `doesNotUnderstand:` — crashing the actor on a raw Erlang message.

```erlang
% If the class (or any ancestor) defines handleInfo:
'handle_info'/2 = fun (Msg, State) ->
    let Self = call 'beamtalk_actor':'make_self'(State) in
    case call 'Module':'dispatch'('handleInfo:', [Msg], Self, State) of
        <{'reply', _Result, NewState}> when 'true' -> {'noreply', NewState}
        <{'error', Error, _ErrState}> when 'true' ->
            call 'logger':'warning'(<<"handleInfo: raised error: ~p">>, [Error], #{})
            {'noreply', State}    % explicitly use pre-call state, not partial error state
        <_Other> when 'true' -> {'noreply', State}
    end

% If NO class in the hierarchy defines handleInfo: (current behavior, unchanged)
'handle_info'/2 = fun (_Msg, State) ->
    {'noreply', State}
```

The `{'error', Error, _ErrState}` arm logs a warning with the error details and explicitly returns the pre-call `State` — even though `_ErrState` is the same value in practice (Core Erlang exception unwinding discards intermediate state bindings), using `State` documents the intent: errors always roll back to pre-call state. The `_Other` arm is a defensive catch-all with the same semantics.

### Gap 2: Named Registration — Defer

`Supervisor which:` is the blessed pattern for service discovery in v0.1:

```beamtalk
app := MySupervisor current.
db := app which: DatabasePool.
db query: "SELECT 1"
```

Named registration (`spawnAs:` / `Actor named:`) is deferred to a dedicated ADR post-v0.1. Rationale:
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

What BT-1451 must verify:
- `terminate:` receives `#shutdown` reason during supervisor stop
- `SupervisionSpec #shutdown` timeout propagates correctly to OTP child spec
- Children receive `terminate:` before supervisor exits
- Default shutdown timeout is reasonable (5000ms, matching OTP default)

### Gap 5: Hot Code Reload — Defer

The current `code_change/3` handles basic field migration (adding/removing fields via `beamtalk_hot_reload`). A user-facing `codeChange:from:` method for custom state migration is deferred until there's a real production use case. The current mechanism is sufficient for development workflows.

## Prior Art

### Elixir GenServer — `handle_info`
Elixir's `handle_info/2` is the direct model. All messages not sent via `call`/`cast` flow through `handle_info` — timer ticks, monitor DOWN notifications, linked process exits. The canonical timer pattern uses `Process.send_after(self(), :tick, interval)` in both `init/1` and `handle_info/2`. This is simple and well-understood but requires manual timer cancellation in `terminate/2`.

A well-known pain point: an unmatched `handle_info` clause crashes the GenServer. The Elixir community consensus is that every production GenServer needs a catch-all `handle_info` that logs and ignores unexpected messages. This is effectively universal boilerplate.

**What we adopt:** The `handleInfo:` method mirrors `handle_info/2` semantics — same OTP callback, Beamtalk syntax. We bake in the community's log-and-continue best practice as the default error contract, eliminating the boilerplate catch-all.

**What we adapt:** Beamtalk recommends the `Timer` API as the default, with `handleInfo:` as an explicit escape hatch. Elixir has no such layering — all timer patterns require `handle_info`.

### Akka Typed — Signals vs Messages
Akka Typed separates *signals* (lifecycle events: `PreRestart`, `PostStop`, `Terminated`) from *messages* (user-defined typed protocol) via `receiveSignal`. Timers use `Behaviors.withTimers` with a keyed `TimerScheduler` — timers are automatically cancelled on actor restart and deduplicated by key.

**What we learn:** The signal/message split is clean but heavyweight. Beamtalk's `terminate:` + `handleInfo:` achieves similar separation with less ceremony. Akka's keyed timers are elegant — a future `Timer` enhancement could add key-based deduplication.

### Pony — No Raw Messages
Pony's actor model is fully typed: every message is a statically typed behavior call. There is no catch-all handler, no raw message reception, and no global actor registry. Discovery is by reference passing only.

**What we learn:** Pony validates that a typed-first approach (Beamtalk's `Timer` API) is the right default. But Beamtalk runs on BEAM where raw messages are a reality (monitors, timers, interop), so an escape hatch is necessary.

### Newspeak — Promise-Based Actors
Newspeak's actors communicate via asynchronous message sends that return promises. No `handle_info` equivalent exists — all messages are typed method calls. No built-in supervision or actor registry.

**What we learn:** Newspeak confirms that Beamtalk's high-level approach (normal method sends for actor communication) is aligned with the Smalltalk tradition. `handleInfo:` is a pragmatic BEAM departure.

### Pharo Smalltalk — Green Threads
Pharo uses cooperative green threads sharing a heap — no actor isolation, no mailbox, no supervision. Timer patterns use `Delay wait` in loops. No named process registry.

**What we learn:** Pharo's lack of actor infrastructure validates Beamtalk's value proposition: BEAM gives us what Pharo can't — true process isolation, supervision, and fault tolerance.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Timer API** is immediately accessible: `Timer every: 1000 do: [self tick!]` reads naturally
- **`handleInfo:`** can be ignored until they need OTP interop — progressive disclosure
- **Error on value types** prevents confusion about which objects can receive raw messages
- **`Supervisor which:`** for discovery is more explicit than global names — easier to trace

### Smalltalk Developer
- **Timer API** preserves message-passing purity — `self tick!` is just a message send
- **`handleInfo:`** is a named method on an actor — still message-passing, just with raw Erlang terms as arguments
- **Rejecting links** keeps the abstraction clean — Smalltalk has no link equivalent
- **No global registry** aligns with Smalltalk's object-reference model (pass references, don't look up names)

### Erlang/BEAM Developer
- **`handleInfo:`** maps directly to `handle_info/2` — no new concepts to learn
- **`match:` with tuple destructuring** makes DOWN/EXIT handling feel natural: `{#DOWN, _ref, #process, _pid, reason}`
- **Timer API** may feel indirect at first, but `self tick!` generating a `gen_server:cast` is standard OTP
- **Deferring named registration** is acceptable — `Supervisor which:` covers the main use case; raw `erlang:register` is available via FFI

### Production Operator
- **`handleInfo:`** generates standard OTP callbacks — visible in observer, debuggable with `sys:get_state/1`
- **Graceful shutdown** (BT-1451) enables proper drain/cleanup sequences
- **No hidden magic** — Timer creates a visible process, `handleInfo:` dispatches through standard gen_server

### Tooling Developer
- **`handleInfo:`** is a regular method — LSP can provide completions and diagnostics
- **Compile-time check** (actor-only) is a simple AST validation
- **No new syntax** — just a conventionally-named method

## Steelman Analysis

### Timer-Only (rejected alternative)
- **Newcomer**: "I never need to think about OTP internals — `Timer every:` just works"
- **Smalltalk purist**: "Clean — no Erlang tuples leak into my Smalltalk world"
- **Language designer**: "Having two timer mechanisms is genuinely confusing. Documentation must explain both, beginners will find both, and we'll forever answer 'why not just use handleInfo:?'. The Timer API saves 3 lines of boilerplate over send_after — is that worth a second mechanism with its own orphan-leak footgun? One mechanism, well-understood, is strictly simpler."
- **Operator**: "Fewer moving parts. Timer processes are visible but unexplained in observer — 'what's that extra process?' One mechanism means one thing to debug."

### Raw `handleInfo:` Only (rejected alternative)
- **BEAM veteran**: "I can use standard OTP patterns — `send_after`, DOWN handling, `trap_exit` — nothing hidden. Every Erlang dev knows handle_info. No orphaned timer processes, no cleanup in terminate:, no deadlock footgun from sync calls in Timer blocks."
- **Operator**: "Standard OTP — I know exactly what's happening at the BEAM level. No extra processes to trace."

### Both — Timer API + `handleInfo:` escape hatch (decided)
- **Newcomer**: "Timer works out of the box; I can go deeper when I'm ready"
- **BEAM veteran**: "Best of both worlds — Timer for quick work, handleInfo for real OTP"
- **Language designer**: "Progressive disclosure — complexity is available but not required"

### Tension Points
- **Newcomers vs BEAM veterans** on handle_info: newcomers prefer Timer-only simplicity; BEAM veterans need raw handle_info for real OTP interop. Resolution: layered approach satisfies both.
- **Smalltalk purists vs BEAM veterans** on named registration: purists want reference-passing only; BEAM vets want atom registration. Resolution: defer — reference-passing (`Supervisor which:`) is sufficient for v0.1.
- **All cohorts agree** on rejecting links and deferring hot code reload.

## Alternatives Considered

### Alternative: Timer-Only (No handleInfo:)
Rely entirely on the Timer class for periodic work and `onExit:` for lifecycle notifications. Actors would never receive raw Erlang messages.

**Rejected because:** This makes BEAM interop impossible for legitimate use cases — receiving messages from Erlang processes, handling `erlang:send_after` ticks, processing raw monitor notifications. The Timer API works for Beamtalk-to-Beamtalk communication but breaks down at the BEAM boundary.

### Alternative: Named Registration Now
Add `spawnAs:` and `Actor named:` for v0.1.

**Rejected because:** Named registration introduces complexity (atom exhaustion risk, name conflict handling, crash recovery re-registration, global vs local vs via scope) that isn't justified by current use cases. `Supervisor which:` covers service discovery within a supervision tree. The runtime plumbing (`beamtalk_actor:start_link/3` already accepts `{local, Name}`) is ready when we need it.

### Alternative: Expose Link/Unlink
Add `actor link` and `actor unlink` to mirror `erlang:link/1` and `erlang:unlink/1`.

**Rejected because:** Links provide bidirectional crash propagation, but this is exactly what supervision trees already provide in a structured way. Exposing raw links encourages unstructured process relationships that are hard to reason about and debug. Monitors (`onExit:`) handle the "notify me when X dies" use case; supervisors handle "restart X when it dies". The Erlang FFI (`Erlang erlang link:`) is available for the rare case where raw links are truly needed.

## Consequences

### Positive
- Actors can participate in full OTP messaging patterns (timer ticks, monitor notifications, interop with Erlang processes)
- Progressive disclosure: newcomers use Timer, experienced users drop to `handleInfo:`
- Graceful shutdown (BT-1451) enables production-quality lifecycle management
- No new syntax — `handleInfo:` is just a method name convention recognized by codegen

### Negative
- Two ways to do timers (Timer API vs `send_after` + `handleInfo:`) — documentation must clearly recommend Timer as default
- `handleInfo:` exposes raw Erlang terms (tuples, atoms) — breaks the pure Beamtalk abstraction for users who use it
- Timer API (`Timer every:`) spawns an unlinked process — if the actor dies without cancelling in `terminate:`, the timer keeps ticking into the void. Documentation must recommend cancelling timers in `terminate:`
- Deferring named registration means some OTP patterns require workarounds (`Supervisor which:`), and `which:` is O(n) in child count

### Neutral
- Rejecting links has no practical impact — no known use case requires them beyond what monitors + supervisors provide
- Deferring hot code reload is low-risk — the current field migration handles development workflows
- `handleInfo:` is opt-in — actors that don't define it behave exactly as before
- `handleInfo:` errors are logged-and-continued (not crash-the-actor) — matches what the Elixir community does manually in every production GenServer, but may surprise BEAM veterans who expect OTP's crash-by-default
- `handleInfo:` return values are always discarded — the method is called for side effects only

## Implementation

### Phase 1: `handleInfo:` (M)
**Affected components:** Parser (no changes), Codegen (callbacks.rs), Runtime (beamtalk_actor.erl)

1. **Codegen:** In `generate_handle_info()` (callbacks.rs ~445-472), check if the class **or any ancestor** defines a `handleInfo:` method. If yes, generate dispatch to it with error logging; if no, generate the current ignore-all stub.
2. **Validation:** Compile-time error if `handleInfo:` is defined on a non-Actor class.
3. **Tests:** BUnit tests for timer ticks via `handleInfo:` (using `Erlang erlang send:` to inject raw messages), DOWN message handling, unknown message ignoring, and error-in-handler recovery. EUnit tests in `beamtalk_actor_tests.erl` for the dispatch path itself.

### Phase 2: Graceful Shutdown Verification (S) — BT-1451
**Affected components:** Runtime (beamtalk_actor.erl, beamtalk_supervisor.erl)

1. Verify `terminate:` receives `#shutdown` during supervisor stop
2. Add `withShutdown:` to `SupervisionSpec` fluent builder (currently hardcoded to 5000ms / `#infinity`)
3. Verify `SupervisionSpec #shutdown` timeout propagates correctly to OTP child spec
4. Add tests for shutdown ordering and timeout behavior

### Deferred
- Named registration (`spawnAs:` / `Actor named:`) — separate ADR post-v0.1
- User-defined `codeChange:from:` — when a production use case emerges
- Links — rejected; revisit only if a compelling pattern requires them

## References
- Related issues: BT-1452 (this ADR), BT-1442 (monitor/pid/onExit — landed), BT-1451 (terminate: runtime fix — in progress)
- Related ADRs: ADR 0005 (BEAM Object Model), ADR 0013 (Class Methods/Instantiation), ADR 0043 (Sync-by-Default Messaging), ADR 0059 (Supervision Tree Syntax)
- Code: `callbacks.rs:445-472` (handle_info codegen), `beamtalk_actor.erl:778-784` (handle_info runtime), `Timer.bt` (Timer class)
- Documentation: `docs/beamtalk-language-features.md` (actors, supervision, pattern matching)
- Elixir community precedent for log-and-continue in handle_info:
  - [Should we catch stray messages in a GenServer?](https://elixirforum.com/t/should-we-catch-stray-messages-in-a-genserver/2656)
  - [Ignoring in handle_info/2 is OK. Really?](https://elixirforum.com/t/ignoring-in-handle-info-2-is-ok-really/2064)
