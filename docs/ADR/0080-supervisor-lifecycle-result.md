# ADR 0080: Migrate Supervisor Lifecycle to Result

## Status
Accepted (2026-04-16). Phase 0a probe (BT-1994) empirically selected
**Option 2** (hook extension) over Option 3 (synchronous helper
process); see §Phase 0a Probe Outcome below and §Phase 1 for the
updated prescription.

## Context

ADR 0079 established `Result(Actor, Error)` as the return shape for registry-boundary operations on `Actor`: `spawnAs:`, `spawnWith:as:`, `registerAs:`, and `named:` all return `Result(Self, Error)`. The rationale — that boundary operations legitimately carry multiple outcomes the caller needs to distinguish, and that tagged-tuple/Result returns are the BEAM-idiomatic answer at startup/registration boundaries — applies equally to supervisor lifecycle methods that currently raise.

Three groups of `Supervisor` and `DynamicSupervisor` lifecycle methods currently **raise** `beamtalk_error` on failure:

| Method | Current signature | Underlying OTP call | Failure modes |
|---|---|---|---|
| `Supervisor>>supervise` (class) | `-> Supervisor` | `supervisor:start_link({local, Name}, ...)` | `{error, {already_started, Pid}}`, `{error, Reason}` |
| `DynamicSupervisor>>supervise` (class) | `-> Self` | same | same |
| `DynamicSupervisor>>startChild` | `-> C` | `supervisor:start_child(Sup, [])` | `{error, Reason}` (init crash, resource exhaustion) |
| `DynamicSupervisor>>startChild:` | `-> C` | `supervisor:start_child(Sup, [Args])` | same |
| `Supervisor>>terminate:` | `-> Nil` | `supervisor:terminate_child(Sup, ChildId)` | `{error, not_found}`, `{error, Reason}` |
| `DynamicSupervisor>>terminateChild:` | `-> Nil` | `supervisor:terminate_child(Sup, ChildPid)` | `{error, not_found}`, `{error, Reason}` |

The current implementation is internally consistent — it either returns a tagged tuple on success or calls `error(beamtalk_exception_handler:ensure_wrapped(Error))` — but it is inconsistent with ADR 0079's precedent and with the way ADR 0060 tells users to think about expected failure. A caller writing modern Beamtalk against the `Actor` API has to mix two error-handling idioms once a supervisor enters the picture:

```beamtalk
// Actor API — Result-shaped (ADR 0079)
(Counter spawnAs: #counter)
  ifOk:    [:c | c increment]
  ifError: [:e | Logger warn: "could not register"]

// Supervisor API — raise-shaped (current)
sup := [WebApp supervise] on: Error do: [:e | ...]   // must bracket or let crash
sup terminate: StaleChild                             // raises if child already gone
```

ADR 0079's "Future Work" section explicitly committed to closing this gap with its own ADR, noting the `{already_started, Pid}` case in `supervisor:start_link`'s return and the `{error, not_found}` case in `terminate_child` as genuine multi-outcome returns that raise-style flattens.

### Current state

**`supervise` is already idempotent.** `beamtalk_supervisor:startLink/1` pattern-matches `{error, {already_started, Pid}}` and returns the existing supervisor tuple — so callers of `supervise` today cannot distinguish "I just started it" from "it was already running." Only genuine `start_link` failures (Reason other than `already_started`) raise. This is good behavior; the migration preserves it.

**`startChild` does not encounter `already_started` in practice.** `DynamicSupervisor` uses `simple_one_for_one`, where children are anonymous and unnamed. `{error, already_present}` and `{error, {already_started, Pid}}` are returned only by `supervisor:start_child/2` on non-simple supervisors with declared child-spec ids — a surface Beamtalk does not currently expose (see Future Work).

**`terminate:` / `terminateChild:` is where `not_found` is the load-bearing case.** Callers legitimately want to treat "child already gone" as success (idempotent cleanup) while letting real errors propagate. With raise-style, they must `on: Error do: [:_e | nil]` and risk swallowing unrelated failures.

### Constraints

- Must not regress the idempotent `already_running` behavior of `supervise`. That behavior is preserved by keeping it an `Ok` outcome, not an `Error`.
- Must leave space for a future `Supervisor startChild:` method (Future Work item 1) that could hit `{error, already_present}` and `{error, {already_started, Pid}}` — the convention chosen here must not force a second migration later.
- External user code (beamtalk-exdura, beamtalk-symphony) calls `supervise` directly. Migration is a breaking API change; the transition plan must be coordinated, not silent.
- The existing `with_live_supervisor/3` helper in `beamtalk_supervisor.erl` already normalizes stale-handle errors into `beamtalk_error` — that machinery stays; we swap the `error/1` raise for a `Result error:` construction.

## Decision

Migrate six methods from raise-on-failure to `Result`-returning. Adopt the **idempotent-startup convention** — "already running" is an `Ok` outcome — across all supervisor lifecycle methods, current and future.

### API

**Static supervisor (`stdlib/src/Supervisor.bt`):**

```beamtalk
class supervise -> Result(Supervisor, Error) =>
  (Erlang beamtalk_supervisor) startLink: self

terminate: aClass -> Result(Nil, Error) =>
  (Erlang beamtalk_supervisor) terminateChild: self class: aClass
```

**Dynamic supervisor (`stdlib/src/DynamicSupervisor.bt`):**

```beamtalk
class supervise -> Result(Self, Error) =>
  (Erlang beamtalk_supervisor) startLink: self

startChild -> Result(C, Error) =>
  (Erlang beamtalk_supervisor) startChild: self

startChild: args -> Result(C, Error) =>
  (Erlang beamtalk_supervisor) startChild: self with: args

terminateChild: child :: C -> Result(Nil, Error) =>
  (Erlang beamtalk_supervisor) terminateChild: self child: child
```

**Unchanged (deliberately):**

| Method | Return | Rationale |
|---|---|---|
| `current`, `which:` | `Supervisor \| nil` / `Actor \| nil` | Lookup; nil-on-miss matches OTP `whereis` semantics (ADR 0079) |
| `children`, `count` | `Array` / `Integer` | Inspection of already-valid supervisor handle |
| `stop`, `kill` | `Nil` | Teardown of own resource; let-it-crash applies (ADR 0079 teardown rule) |

### Idempotent-startup convention

Across every current and future supervisor lifecycle method, **an operation is `Ok` when the caller's target end state is already in effect**, regardless of whether this call or a prior one established it.

- `supervise` — target is "this supervisor is running." `Ok(sup)` whether this call started it OR it was already registered under the class name (`{already_started, Pid}` path).
- `startChild` / `startChild:` — target is "a child of the requested class is running." `Ok(child)` on fresh start. A future `Supervisor startChild: spec` hitting `{error, {already_started, Pid}}` (same spec id running) matches the target and is also `Ok(existingChild)`.
- `terminate:` / `terminateChild:` — target is "this child is not running." `Ok(nil)` when the child is now gone, whether this call terminated it OR it was already dead (`{error, not_found}` path).

The rule is "does the target state hold now?" — not "did this call change the input?" This matters for the distinction between `{already_started, Pid}` and `{error, already_present}` in OTP's `start_child/2`:

- `{already_started, Pid}` — spec is registered, process is running → target state holds → **Ok**.
- `{error, already_present}` — spec is registered, process is **not** running → target state does NOT hold; caller must act (`restartChild:` or `deleteChild:` then retry) → **Error** with reason `#child_already_registered`.

`Error` is reserved for outcomes the caller cannot trivially ignore:
- `start_link` failure with non-`already_started` reason (init crash, resource exhaustion, config error).
- `start_child` failure from the child's `init/1` (init crash, unsatisfied dependency).
- `start_child` with `{error, already_present}` — target state unmet, separate recovery API required.
- `terminate_child` failure from a reason other than `not_found` (supervisor crash, timeout).
- Stale supervisor handle — the supervisor process itself is dead.

This convention is the load-bearing reason the migration is forward-compatible: it binds the meaning of `Ok` on supervisor methods to a precise property (target-state-holds) so future additions (see Future Work) can classify their outcomes consistently without reshaping existing methods.

### Error variants

Errors are structured `beamtalk_error` values with a `reason` Symbol. The error shape matches ADR 0079's `Actor` errors so user code using `(beamtalk_error <reason>)` destructuring works uniformly across Actor and Supervisor boundaries.

| Method | `Error` reason | When |
|---|---|---|
| `supervise` | `#supervisor_start_failed` | `start_link` returned `{error, Reason}` with Reason ≠ `already_started` |
| `supervise` | `#stale_handle` | Supervisor class gen_server unreachable (defensive) |
| `startChild` / `startChild:` | `#child_start_failed` | `supervisor:start_child/2` returned `{error, Reason}` — typically child `init/1` crash |
| `startChild` / `startChild:` | `#stale_handle` | Supervisor pid is dead |
| `terminate:` / `terminateChild:` | `#stale_handle` | Supervisor pid is dead |
| `terminate:` / `terminateChild:` | `#terminate_failed` | `supervisor:terminate_child/2` returned `{error, Reason}` with Reason ≠ `not_found` |

`not_found` from `terminate_child` is **not** an error — per the idempotent convention, it becomes `Ok(nil)`.

Each error carries the originating class and selector (matching existing `beamtalk_error:new/4` shape from ADR 0015) plus a human-readable `message` describing the underlying Reason for logging and REPL display.

### REPL session

```text
> app := (WebApp supervise) unwrap
 => #Supervisor<WebApp,_>

// supervise is idempotent — both calls Ok, both return the running supervisor
> (WebApp supervise) isOk
 => true

> pool := (WorkerPool supervise) unwrap
 => #DynamicSupervisor<WorkerPool,_>

> (pool startChild) ifOk: [:w | w ping] ifError: [:e | Logger warn: e message]
 => #ok   (side-effect: w ping evaluated)

// Misconfigured child — init raises
> (BrokenPool startChild)
 => Result error: (beamtalk_error child_start_failed)

// Terminate is idempotent — already-gone is Ok
> (app terminate: Counter) isOk
 => true
> (app terminate: Counter) isOk   // second call: already gone
 => true

// Stale handle
> pool stop
 => #ok
> (pool startChild)
 => Result error: (beamtalk_error stale_handle)
```

## Prior Art

**Erlang / OTP.** `supervisor:start_link/3`, `supervisor:start_child/2`, and `supervisor:terminate_child/2` all return tagged tuples:
- `supervisor:start_link/3` → `{ok, Pid} | {error, {already_started, Pid}} | {error, Reason}`
- `supervisor:start_child/2` → `{ok, Pid} | {ok, Pid, Info} | {error, already_present} | {error, {already_started, Pid}} | {error, Reason}`
- `supervisor:terminate_child/2` → `ok | {error, not_found} | {error, simple_one_for_one}`

Erlang does not raise; it is entirely tagged-tuple-shaped. This ADR adopts Erlang's shape and promotes it from raw tuples to Beamtalk's `Result` object per ADR 0076's FFI coercion policy.

**Elixir.** `DynamicSupervisor.start_child/2` mirrors Erlang's return shape: `{:ok, pid} | {:ok, pid, info} | {:error, reason}`. `Supervisor.start_link/2,3` likewise. Elixir users pattern-match on these in `case` blocks or chain with `with`. Our `Result` plays the `with` role via `andThen:` / `ifOk:ifError:` (ADR 0060).

**Gleam.** `gleam_otp`'s supervisor API returns `Result(Pid, StartError)` where `StartError` is a sum type enumerating the OTP cases (`Already_started(Pid)`, `Init_timeout`, `Init_failed(Reason)`, etc.). Gleam's richer sum type is appealing but requires ADTs at the type level — Beamtalk's `beamtalk_error` with a Symbol reason covers the same ground at a coarser granularity. Gleam explicitly treats `Already_started` as a `StartError` variant (not `Ok`), diverging from our idempotent-startup convention. Gleam's choice is consistent with "literal transcription of OTP return shapes"; ours is consistent with "the ergonomic case users overwhelmingly want at supervisor-start boundaries."

**Akka (Scala).** `ActorSystem.actorOf` historically raised `InvalidActorNameException` on duplicate names. Modern Akka Typed (`ActorSystem.systemActorOf`) returns `Future[ActorRef[T]]` which completes with the new ref or fails with a well-typed exception. The Future + typed exception pattern is Akka's equivalent of a `Result`; our design lands similarly with synchronous `Result` values (BEAM doesn't need futures for same-node calls).

**Pharo / Squeak.** No direct analogue — Smalltalk process model lacks OTP-style supervision. The closest pattern is `SystemNotification` / `Error` signal-resume, which is exception-shaped. Smalltalk-pure thinking would argue for keeping raise; the BEAM-native argument for Result wins here on consistency with the surrounding Actor API.

**Beamtalk precedent.** ADR 0079 (`Actor spawnAs:`, `named:`) is the direct template. ADR 0076 (ok/error → Result at FFI boundary) establishes the conversion policy. ADR 0060 (Result type) provides the combinator surface.

## User Impact

**Newcomer (Python/JS background).** Startup flows read consistently — every boundary operation returns something the caller branches on. REPL display shows `Result error: (beamtalk_error child_start_failed)` with a clear class/selector/message shape. Forgotten error handling surfaces as "you are calling `increment` on a `Result`, not a `Counter`" at the type checker or first send, not a silent crash. Learning cost: one extra method call (`unwrap` or `ifOk:ifError:`) in tutorial code, offset by a consistent error model across the language.

**Smalltalk developer.** Raise-shaped supervisor APIs will feel more natural on first contact — Smalltalk's error model is exception-shaped. The ADR 0060 framing (Result for *expected* failure categories; exceptions for programming errors) reframes this: supervisor-start failure is an expected category (config error, port in use) for which Result is the right tool. The migration brings supervisor APIs in line with the `Actor` API users already work with.

**Erlang/Elixir developer.** The mapping to OTP return shapes is direct and familiar. `(sup supervise) ifOk: ... ifError: ...` reads as a Beamtalk-flavoured `case supervisor:start_link(...) of ...`. The idempotent-startup convention is Beamtalk-specific; it is explicitly documented and consistent with the Beamtalk idiom that `supervise` means "make sure this is running" (not "start this fresh"). Erlang veterans who specifically need the `{already_started, Pid}` distinction — a niche case in production, absent from Beamtalk's current API surface — get it back with the future `Supervisor startChild:` method (Future Work).

**Production operator.** `beamtalk_error` values with symbolic reasons are greppable in logs and aggregatable in metrics (error counters keyed by `#supervisor_start_failed` vs `#child_start_failed` vs `#stale_handle`). The `with_live_supervisor` layer continues to catch stale-handle OTP exits and translate them to structured errors — no new failure-mode surface. Release upgrades during the transition window carry the usual BEAM concern: if a running node has old user code calling `supervise` under the raise contract and new stdlib returns a Result, the user code's shape expectation is broken. The migration is a breaking change requiring a coordinated restart, not a pure hot-reload.

**Tooling developer (LSP, IDE).** Typed return signatures (`Result(Supervisor, Error)`, `Result(C, Error)`) enable precise completions after `.` — `ifOk:ifError:`, `map:`, `andThen:`, `mapError:`, `unwrap`. Type narrowing: `(pool startChild) ifOk: [:c | c.method] ifError: [...]` — `c` narrows to the DynamicSupervisor's `C` type parameter, so LSP offers the right method set. Diagnostics can now warn on unchecked Result returns from supervisor methods (the same lint applies as for `Actor spawnAs:`).

## Steelman Analysis

### Option B: Distinguish `already_started` via Error variant

- 🧑‍💻 **Newcomer**: "If two processes race to start the same supervisor, I want to know which one won and which one attached. Treating both as `Ok` hides a decision I might care about — restore state, issue a metric, skip initialization I'd have run on fresh start."
- 🎩 **Smalltalk purist**: (no strong view — Result shape is the same either way; the only question is which variant carries which outcome)
- ⚙️ **BEAM veteran**: "OTP distinguishes `{ok, Pid}` from `{error, {already_started, Pid}}` for a reason — it is the programmer's signal that initialization side effects should be skipped. Collapsing them to `Ok` discards that signal at the API boundary; the caller can no longer recover it without a side-channel probe."
- 🏭 **Operator**: "Metrics on fresh-start vs already-running are useful during incident triage — did the user-visible slowness come from a restart storm or from normal traffic? If the API discards that distinction, I need a second mechanism to recover it."
- 🎨 **Language designer**: "The OTP convention is rich; Beamtalk should preserve it. Idempotency is a *caller* concern (the caller chooses to treat both as success); baking it into the API flattens the expressiveness."

**Why not chosen:** In Beamtalk's current surface, `supervise` is the only lifecycle method that can hit `{already_started, Pid}` today, and no current caller — in-tree or (per the issue description) in beamtalk-exdura — depends on the distinction. That is "no existing demand," not "overwhelmingly prefers idempotent." The stronger argument is structural: forcing callers to handle `Error(#already_running, sup)` as a success case breaks the happy-path short-circuit at every use site (`app := (WebApp supervise) unwrap` blows up on the second call) and the ergonomic of `supervise` as "make sure this is running." The distinction is recoverable via a `wasFreshlyStarted` accessor on the returned supervisor (Future Work) if post-migration usage shows demand. If such demand materialises and the accessor is insufficient, Option B remains viable as an additive `superviseFresh:` variant without disturbing the primary method.

### Option C: Carry `wasFresh` flag in Ok via a wrapper type

- 🧑‍💻 **Newcomer**: "One happy path and full information? Best of both."
- 🎨 **Language designer**: "A `SupervisorStart` value-object with `.supervisor` and `.wasFreshlyStarted` is the right factoring — it names the thing being returned (the start outcome) rather than flattening it."
- ⚙️ **BEAM veteran**: "It preserves the OTP information without the branching ceremony."
- 🏭 **Operator**: "Auditable — `metric.increment(\"sup_start\", fresh: start.wasFreshlyStarted)` is a one-liner."
- 🎩 **Smalltalk purist**: (no strong view)

**Why not chosen:** The wrapper introduces asymmetry with other lifecycle methods. `startChild` has no equivalent flag (always fresh); `terminate:` has no equivalent (either gone or failed). Either the wrapper applies only to `supervise` — in which case the API has an unprincipled one-off — or it applies uniformly, which over-specifies the other methods. More importantly, when a future `Supervisor startChild:` arrives (Future Work item 1) it hits the same decision: does it return `Result(ChildStart, Error)` matching `supervise`, or `Result(C, Error)` matching `DynamicSupervisor startChild`? Either answer is API churn. Option A (plain Result, idempotent Ok) avoids the question entirely.

### Option D: Partial migration — Result for `terminate:`/`terminateChild:` only, keep `supervise`/`startChild` raising

- 🧑‍💻 **Newcomer**: "Less to learn — the big wins (idempotent cleanup) without the ceremony on the happy path."
- 🎩 **Smalltalk purist**: "Raise for startup is the Smalltalk-y choice. Result for cleanup is a pragmatic concession where it actually helps."
- ⚙️ **BEAM veteran**: "`start_link` failure during boot is a crash-the-application kind of error — raise is what you want. `terminate_child` returning `not_found` is the genuine multi-outcome case."
- 🏭 **Operator**: (split view — consistency helps operations, but so does letting supervisor-start failures crash loudly at the top of the boot sequence)
- 🎨 **Language designer**: "Minimal migration — touch only what clearly benefits."

**Why not chosen:** This is the most defensible rejected option — the bulk of today's pain is `terminate:`/`terminateChild:` where `not_found` is genuinely load-bearing, and `supervise` is already idempotent at the runtime layer. Three arguments tip the decision to full migration. First, leaving `supervise`/`startChild` raising means a user writing a mixed flow (`supervise` → `startChild` → `terminate:`) handles two error idioms per call site — the cost compounds with every future supervisor method we add. Second, when Future Work item 1 lands (`Supervisor startChild:` for non-simple supervisors, which does hit `{already_started, Pid}`), we would migrate that one too — a second migration with its own external coordination. Third, `unwrap` preserves the raise-at-boot option for D's advocates at one extra keyword per call site, so the ergonomic cost of full migration is minor. Partial migration remains a defensible choice; the decision is a call, not a forced answer.

### Option Z: Status quo — keep all methods raising

- 🎩 **Smalltalk purist**: "Smalltalk error model is exceptions. Result is an intrusion. Raise keeps the language coherent."
- ⚙️ **BEAM veteran**: "Let it crash has a 30-year production track record. Supervisor-start failure at boot *should* crash — that is the point."
- 🏭 **Operator**: "Raising gives me a stack trace at the source of failure, not a `Result` value propagated halfway across the codebase before someone calls `unwrap`."
- 🎨 **Language designer**: "The ADR 0060 framing (Result for expected, exceptions for programming errors) places supervisor-start in the grey zone. Leaving it exception-shaped is a defensible reading."
- 🧑‍💻 **Newcomer**: (no strong view — defers to whichever idiom the docs teach first)

**Why not chosen:** ADR 0079 already committed the direction — `Actor spawnAs:` returns Result — and the Supervisor API is the other half of the startup boundary. Leaving supervisors exception-shaped while actors are Result-shaped creates the two-idiom problem described in Context. The "let it crash at boot" argument is preserved via `unwrap`: `(WebApp supervise) unwrap` crashes loudly on failure, satisfying the operator/veteran concern while keeping the Result contract for callers who want to handle the error explicitly. The migration is a net win for consistency and costs one keyword at boot-time call sites.

### Tension Points

- **OTP fidelity vs ergonomic idempotency (A vs B).** BEAM veterans and language designers have the strongest case for preserving `already_started` as an `Error` variant. The decision turns on how often the distinction is load-bearing in Beamtalk's actual API surface — which is rare today (see Context). If usage data post-migration shows callers consistently reaching for a probe to recover the distinction, Option B (or a `wasFresh` accessor on the returned supervisor) becomes the follow-up.
- **Partial vs full migration (A vs D).** D's steelman lands on "touch only what clearly benefits." A's case is consistency: splitting the API across two idioms imposes a mental-model tax on every caller and every teacher. The Result pattern has a `unwrap` escape hatch that preserves the raise-at-boot semantics D's advocates want, so the cost of choosing A over D is small.
- **Future-proofing convention (A vs C).** C's wrapper is locally nicer for `supervise` but forces a choice when `Supervisor startChild:` arrives. A's idempotent-startup convention is the smallest rule that keeps all current and all plausible future methods compatible.

## Alternatives Considered

### Alternative B: Distinguish `{already_started, Pid}` via Error variant

`supervise` returns `Ok(sup)` only when the call starts the supervisor fresh; returns `Error(#already_running, sup)` when the supervisor was already registered; returns `Error(Reason)` for genuine start_link failures.

```beamtalk
class supervise -> Result(Self, Error) =>
  // Ok when this call started it
  // Error #already_running (with existing sup) when already registered
  // Error #supervisor_start_failed for real failures
```

**Rejected because:** Breaks the happy-path ergonomics for the overwhelmingly-common case (idempotent "start or get"). Every caller of `supervise` in examples/otp-tree, stdlib/test, and e2e fixtures would need to treat `Error(#already_running)` as success, effectively collapsing it back to `Ok` at every use site. See Steelman Analysis for the full argument.

### Alternative C: Wrapper type carrying `wasFreshlyStarted`

Introduce a `SupervisorStart` value-object returned in the `Ok` variant:

```beamtalk
class supervise -> Result(SupervisorStart, Error)
  // SupervisorStart carries .supervisor and .wasFreshlyStarted
```

**Rejected because:** Creates asymmetry with `startChild` and `terminate:`, and would force a second decision (preserve the wrapper? match the flat shape?) when future supervisor `startChild:` arrives. See Steelman Analysis.

### Alternative D: Partial migration — only `terminate:`/`terminateChild:`

Migrate only the methods where `not_found` is the load-bearing multi-outcome case. Leave `supervise` and `startChild` raising.

**Rejected because:** Splits the supervisor API across two error idioms, defeating the consistency motivation. `unwrap` provides the raise-at-boot semantics partial-migration advocates want, so the cost of full migration is low.

### Alternative E: Migrate to a bespoke `SupervisorError` sum type (Gleam style)

Replace `Result(X, Error)` with `Result(X, SupervisorError)` where `SupervisorError` is a sealed enum (`#stale_handle`, `#already_started(Pid)`, `#init_failed(Reason)`, etc.).

**Rejected because:** Beamtalk has no ADTs at the type level; the closest mechanism is class-per-variant, which is heavyweight for the value carried here. `beamtalk_error` with a Symbol `reason` and an optional `data` field (ADR 0015) covers the same ground with the machinery we already have. If a richer type emerges — pattern matching on `SupervisorError` variants with exhaustiveness checking — that is its own ADR, not a detour in this one.

### Alternative F: Dual API during a deprecation window (`supervise!` alongside `supervise`)

Introduce a `supervise!` raise-form alongside a new `supervise` Result-form for one release cycle, letting external projects (beamtalk-exdura, beamtalk-symphony) migrate on their own schedule. Remove `supervise!` in the release after that.

**Rejected because:** The benefit (decoupling external project timelines) is lower than the cost of maintaining two parallel APIs and teaching both in documentation during the window. The `unwrap` escape hatch on the new Result-form already achieves most of what `supervise!` would provide (one keyword for raise-at-failure); adding a second named method for the same effect is pure duplication. If coordinating the external project transitions proves harder than expected, reconsider — this remains a cheap fallback.

### Alternative Z: Status quo — keep raise

Retain the current behavior; document the raise cases on each method.

**Rejected because:** Inconsistent with ADR 0079's `Actor` API and with ADR 0060's Result convention for expected failures. See Steelman Analysis.

## Consequences

### Positive
- Consistent error model across `Actor` and `Supervisor` APIs — a call site that chains actor spawn and supervisor operations no longer mixes raise-shaped and Result-shaped returns.
- `terminate:` / `terminateChild:` idempotency becomes expressible without swallowing unrelated errors — callers explicitly match `Error(#terminate_failed)` vs treating `Ok(nil)` as success whether the child was alive or not.
- REPL displays supervisor errors as `Result error: (beamtalk_error <reason>)` with a greppable Symbol, matching actor errors.
- LSP/IDE benefits: precise `Result(T, E)` return types unlock method completions and unchecked-Result diagnostics.
- Forward-compatible — the idempotent-startup convention leaves room for future `Supervisor startChild:`, `restartChild:`, etc. without reshape of existing methods.
- `unwrap` gives callers a one-keyword path to "crash at failure" — useful at application boot, test setup, and REPL exploration. The raised exception carries the existing `ensure_wrapped`-shaped class/selector/message, matching the pre-migration crash payload; what changes is the stack shape (raise site moves from `startLink/1` into `unwrap` in `beamtalk_result.erl`). Operators who pattern-match on the crash location in logs will see a different frame, though the human-readable message is unchanged.

### Negative
- **Breaking API change.** Every caller of `supervise`, `startChild`, `startChild:`, `terminate:`, `terminateChild:` must update. Migration Path below documents the mechanical rewrite; out-of-tree projects (beamtalk-exdura, beamtalk-symphony) require coordinated updates. A follow-up issue after this ADR's implementation epic may add a linter/warning that flags pre-migration call sites (e.g., use of `supervise` without `unwrap`/`ifOk:ifError:`/`value` at the top-level expression).
- **`already_started` distinction is lost at the `supervise` boundary.** Callers that need "was this a fresh start?" must use a separate probe. Today the distinction is not exposed anywhere, so this is a non-regression — but it forecloses retroactively exposing it from `supervise` without a follow-up ADR. An accessor on the returned supervisor (`supervise wasFreshlyStarted`) or a block-form (`supervise onFreshStart: [...]`) is the most likely future answer if demand emerges.
- **Minor REPL ceremony.** `app := WebApp supervise` becomes `app := (WebApp supervise) unwrap` (or a Result-aware binding). Most REPL sessions are short enough that the difference is nominal, but documentation examples require updating.
- **Training surface.** Existing docs, examples, and the `examples/otp-tree` project teach the raise-shaped API. All of these must migrate in lockstep with stdlib changes; a half-migrated set would be worse than either pure-raise or pure-Result.

### Neutral
- `stop`, `kill`, `current`, `which:`, `children`, `count` unchanged. Their semantics (teardown, lookup, inspection of an already-valid handle) remain outside the migration per the rules established in ADR 0079.
- `with_live_supervisor/3` in `beamtalk_supervisor.erl` stays — it already normalises stale-handle OTP exits to `beamtalk_error`, which the migration simply wraps in `Result error:` instead of `error()`-raising.
- No new AST nodes, no codegen changes, no new runtime intrinsics. The migration is a stdlib signature change plus a local edit in each Erlang runtime function to return `{error, BtError}` instead of calling `error(...)`, letting ADR 0076's FFI coercion wrap the result.

## Implementation

### Phase 0: Probes — validate two load-bearing runtime assumptions before touching stdlib

Two implementation questions must be resolved by probe commits before Phase 1 is scoped. Either answer is viable for the decision; we need the answer to pick the Phase 1 approach.

**Probe 0a — FFI coercion vs the `beamtalk_supervisor_new` post-dispatch hook (BT-1542).**

Today, `beamtalk_supervisor:startLink/1` returns a bare 4-tuple `{beamtalk_supervisor_new, ClassName, Module, Pid}` on fresh start. The hook in `beamtalk_class_dispatch.erl:113-120` inspects the class gen_server's `{ok, Result}` reply, sees the `_new` tag, rewrites to the standard `{beamtalk_supervisor, ...}` tag, and synchronously runs `run_initialize/1` **in the caller's process** (a documented deadlock-avoidance guarantee from ADR 0059 / BT-1285).

Migrating `startLink/1` to `{ok, SupTuple} | {error, BtError}` causes ADR 0076's FFI coercion in `beamtalk_erlang_proxy:direct_call/3` to produce a `Result` tagged map for the class method body's return value. The class gen_server replies `{ok, <Result tagged map>}`. The existing hook's pattern match on `{beamtalk_supervisor_new, ...}` does NOT match a Result tagged map — so `initialize:` silently stops running and the returned tag is never rewritten. This is a correctness regression, not a cosmetic one.

Three candidate resolutions, each with real tradeoffs:

1. **Keep `startLink/1` raising; wrap at the stdlib layer with `Result tryDo:`.** No runtime change to `startLink/1`; `supervise` body becomes `Result tryDo: [(Erlang beamtalk_supervisor) startLink: self]`. **This alone does NOT preserve the existing `_new` tag rewrite / `initialize:` hook.** The class method's return value is the wrapped `Result`, which is what `beamtalk_class_dispatch` sees in the `{ok, Result}` gen_server reply — the hook's pattern match is against the outer Result map, not the inner `{beamtalk_supervisor_new, ...}` tuple. So option (1) is useful as a *baseline for error-shape experimentation only*: it changes the Error side to an Exception-wrapped value (carrying `ensure_wrapped`'s class/selector/message) without solving the initialize regression. To preserve `initialize:`, option (1) must be paired with runtime work from option (2) or (3) — at which point it collapses into a variant of one of those.
2. **Teach the class_dispatch hook to inspect the `Result` tagged map.** The hook pattern-matches a second case for `Result ok: {beamtalk_supervisor_new, ...}`, rewrites the inner tag, runs `run_initialize/1` in the caller's process, and rewraps as `Result ok: {beamtalk_supervisor, ...}` before returning. Preserves all existing semantics (initialize runs in caller's process, error shape is structured `Result error:`). **Concern:** couples `beamtalk_class_dispatch` to Result's internal representation; leaks supervisor-specific logic into a generic dispatch layer.
3. **Run initialize: synchronously from `startLink/1` via a short-lived helper process.** Spawns a transient process to run `run_initialize/1` before returning; caller waits for it. `startLink/1` then returns `{ok, {beamtalk_supervisor, ...}}` (already normalized) and FFI coercion does the rest. **Concern:** changes the documented "runs in caller's process" guarantee (ADR 0059 / BT-1285) — downstream implications on `which:` and recursive-send deadlock avoidance are unknown; probably needs its own mini-ADR.

Probe 0a compares the runtime-bearing approaches (options (2) and (3)) empirically on the `Supervisor>>supervise` / `startLink/1` path — the only path that triggers the `beamtalk_supervisor_new` post-dispatch hook. The probe commit implements each option and verifies, on a fresh start, that (a) the `_new` tag is rewritten to `{beamtalk_supervisor, ...}`, (b) `class initialize:` runs exactly once, and (c) the e2e supervisor test suite stays green. The chosen option is the one that achieves the above with the least collateral code. Option (1) can be evaluated separately for Error-side ergonomics on instance methods (`startChild`, `terminateChild:`) where no post-dispatch hook is in play.

#### Phase 0a Probe Outcome (BT-1994, 2026-04-16)

**Result: Option (2) wins. Option (3) is disqualified by a deadlock that surfaces whenever `initialize:` sends a method-dispatching message to the just-started supervisor.**

Both options were implemented on separate branches (`BT-1994-probe-option-2`, `BT-1994-probe-option-3`) with identical test coverage: EUnit for runtime-level behaviour, BUnit for stdlib wiring, and the e2e supervisor suite for end-to-end correctness. Both probes used a stdlib shim — `class supervise => ((Erlang beamtalk_supervisor) startLink: self) unwrap` — so the user-facing signature stayed `Supervisor` / `Self`, keeping existing callers working without Phase 1 stdlib migration.

| Property | Option 2 (hook extension) | Option 3 (helper process) |
|---|---|---|
| EUnit (`beamtalk_supervisor_tests`) | 69/69 ✅ | 72/72 ✅ |
| EUnit (`beamtalk_class_dispatch_tests`) | 80/80 ✅ | 80/80 (hook removed, so rewrap test subsumed) |
| BUnit (full suite, 199 files) | 1860/1860 ✅ | — (blocked by e2e deadlock, not rerun) |
| E2E (1118 cases) | 1118/1118 ✅ | **67 failures** — `supervisor_initialize.btscript` deadlocks |
| `initialize:` runs in caller's process | Yes (ADR 0059 / BT-1285 invariant preserved) | No (helper process) |
| Collateral code (runtime) | +60 LOC (hook matches two shapes) | +120 LOC (helper, monitor, timeout, teardown) |
| Semantic risk | Low — hook change is additive, idempotent, and pattern-local | High — helper trades a known invariant for a known-concern one |

**Option 3 failure mode (empirical).** When `initialize:` on `E2EInitSupervisor` calls `sup which: E2EInitChildA`, and when `initialize:` on `E2EInitDynSupervisor` calls `sup startChild`, the e2e harness hit `exit:{timeout, {gen_server, call, [ClassPid, {method, startChild}]}}`. Trace:

1. REPL sends `E2EInitDynSupervisor supervise` → `class_send_dispatch` issues `gen_server:call(ClassPid, {class_method_call, supervise, []})`.
2. The class gen_server runs the method body, which invokes `startLink/1` **inside the class gen_server's `handle_call`**.
3. Option 3's `startLink/1` spawns a helper process and waits for it before returning — still blocking the class gen_server's `handle_call`.
4. The helper runs `initialize:`, whose body sends `sup startChild`.
5. `beamtalk_message_dispatch:send({beamtalk_supervisor, ...}, startChild, [])` → `beamtalk_dispatch:lookup` → `beamtalk_object_class:has_method(ClassPid, startChild)` → `gen_server:call(ClassPid, {method, startChild})`.
6. `ClassPid` is the class gen_server from step (2), which is **still blocked** waiting for startLink/1 (step 3) to return. Deadlock.

This is precisely the concern the ADR raised in Phase 0a ("downstream implications on `which:` and recursive-send deadlock avoidance are unknown; probably needs its own mini-ADR"). The probe confirms the concern is not hypothetical: it is triggered by the existing e2e fixtures. A redesign that fully decouples the helper from the class gen_server's `handle_call` would require making `supervise` asynchronous — a far larger API change than Option 2 — or routing method resolution through an out-of-band channel that bypasses `ClassPid` entirely (untested, adds a second concurrency protocol). Neither is justified when Option 2 succeeds cleanly.

**Option 2 behaviour (empirical).** Hook at `beamtalk_class_dispatch:class_send_dispatch/3` now matches two shapes on the class gen_server's `{ok, Result}` reply:
- Bare `{beamtalk_supervisor_new, ClassName, Module, Pid}` — the shape after `supervise`'s `unwrap` extracts the `okValue` from the FFI-wrapped Result, i.e. the pre-Phase-1 path exercised today.
- `Result` tagged map `#{'$beamtalk_class' => 'Result', isOk => true, okValue => {beamtalk_supervisor_new, ...}}` — the shape that will arrive once Phase 1 removes the `.unwrap` from the stdlib and `supervise` returns `Result(Supervisor, Error)` directly.

Both branches rewrite the inner tag to `{beamtalk_supervisor, ...}`, call `beamtalk_supervisor:run_initialize/1` **in the caller's process** (preserving the ADR 0059 / BT-1285 guarantee), and return the normalised shape (bare tuple or re-wrapped Result map, matching what they received). Idempotent `{beamtalk_supervisor, ...}` returns and `{error, ...}` branches flow through unchanged — no initialize re-run.

The dual-shape match means Phase 1's migration to `supervise -> Result(Supervisor, Error)` will not require another hook change: both the pre-Phase-1 shim (`unwrap` in stdlib) and the post-Phase-1 typed signature (no `unwrap`, callers handle the Result) produce shapes the hook already handles.

**Probe 0b — `Result(C, Error)` type substitution for `DynamicSupervisor(C)`.**

ADR 0079 / BT-1986 probed that `Result(Self, Error)` works in generic position. ADR 0080 proposes `Result(C, Error)` where `C` is the `DynamicSupervisor`'s parametric type argument (ADR 0068). BT-1992 ("Thread receiver type arguments through Self substitution") should cover this, but `C` is not `Self` — it's a class-level type parameter. Probe 0b writes a minimal stdlib method `class foo -> Result(C, Error)` on `DynamicSupervisor` and a concrete subclass test, verifying the typechecker narrows `C` correctly at call sites. If it does not, a targeted typechecker extension is in scope (matching ADR 0079's scope for `Self`).

**Probe 0b outcome (BT-1995): substitution works unchanged — no typechecker extension required.** A probe method `probeC -> Result(C, Error)` on `DynamicSupervisor(C)` with a concrete subclass extending `DynamicSupervisor(Counter)` narrows to `Result(Counter, Error)` at the call site under the existing substitution machinery. Class-level type parameters flow through `build_inherited_substitution_map` (ADR 0068 Phase 1b, BT-1577) — the same path BT-1986 / BT-1992 extended for `Self`, applied independently here — which resolves `C → Counter` from the subclass's `superclass_type_args`. The probe method was deleted after validation; the permanent regression record is the unit test `test_class_type_param_in_generic_return_narrows_to_concrete` in `crates/beamtalk-core/src/semantic_analysis/type_checker/tests.rs`. Phase 2 stdlib migration can consume `Result(C, Error)` signatures directly.

Both probes are small (<1 day each) and their outcomes determine the exact shape of Phase 1.

### Phase 1: Runtime — return Result-shaped values where direct coercion is safe
- **`runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`:**
  - `startLink/1` — **Option 2 from Phase 0a (BT-1994):** returns `{ok, {beamtalk_supervisor_new, ClassName, Module, Pid}}` on a fresh start, `{ok, {beamtalk_supervisor, ...}}` on the idempotent `{already_started, Pid}` branch, and `{error, #beamtalk_error{kind = supervisor_start_failed, ...}}` on start failure. The inner `_new` tag signals the post-dispatch hook in `beamtalk_class_dispatch`. FFI coercion wraps the outer tuple as a `Result` tagged map for the class method body's return.
  - `startChild/1`, `startChild/2` — instance methods (no post-dispatch hook). On `{error, Reason}`, return `{error, BtError}` with `reason = child_start_failed`. On `{ok, Pid}`, return `{ok, wrap_child(...)}`. FFI coercion handles both.
  - `terminateChild/2` (both arities) — on `{error, not_found}`, return `{ok, nil}` (idempotent); on other `{error, Reason}`, return `{error, BtError}` with `reason = terminate_failed`. **Behavior change on static path:** `terminateChild/2` at lines 276-292 currently raises *all* `{error, Reason}` uniformly — it does not special-case `not_found`. The migration adds the `not_found` → `Ok(nil)` branch to the static path, matching the existing dynamic-path special-case at lines 304-311. Any caller relying on the static path raising for a missing child loses that signal. Audit in Phase 2.
  - `with_live_supervisor/3` — return `{error, BtError}` with `reason = stale_handle` instead of raising. Inner funs must now return `{ok, Value}` on success so FFI coercion sees a consistent shape.
- **`runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`:**
  - `class_send_dispatch/3` — extend the BT-1542 post-dispatch hook to pattern-match both the bare `{beamtalk_supervisor_new, ...}` tuple (pre-Phase-1 shim path via `.unwrap` in the stdlib) **and** a `Result` tagged map wrapping that tuple (post-Phase-1 path, where stdlib returns `Result(Supervisor, Error)` without unwrapping). On the Result-map match, rewrite `okValue` to `{beamtalk_supervisor, ...}` and return the re-wrapped Result; on the bare-tuple match, rewrite and return the bare tuple. `class initialize:` always runs **in the caller's process**, preserving the ADR 0059 / BT-1285 guarantee.
- **EUnit tests:** `beamtalk_supervisor_tests.erl` — cover each Result variant end-to-end, including the `{already_started, Pid}` → `Ok` path, the `not_found` → `Ok(nil)` idempotent path on BOTH static and dynamic supervisors, `stale_handle` defensive coverage, and an explicit test that `class initialize:` still runs on fresh start after the Phase 0a resolution. `beamtalk_class_dispatch_tests.erl` — extend the existing `class_send_supervisor_new_rewrap_test_` to drive the Result-tagged-map branch via `beamtalk_result:from_tagged_tuple({ok, ...})` in the test helper.
- **Effort:** S-M. Phase 0a probe landed (BT-1994); the path is now unambiguous.

### Phase 2: Stdlib signature migration
- **`stdlib/src/Supervisor.bt`:** update `class supervise` return type to `Result(Supervisor, Error)`; update `terminate:` return type to `Result(Nil, Error)`. No body changes (FFI call already returns the Result per Phase 1).
- **`stdlib/src/DynamicSupervisor.bt`:** update `class supervise`, `startChild`, `startChild:`, `terminateChild:` return types to their Result-shaped equivalents. No body changes.
- **BUnit tests** (`stdlib/test/`): update `dynamic_supervisor_defaults_test.bt`, `dynamic_supervisor_initialize_test.bt`, `supervisor_initialize_test.bt`, `supervision_class_method_test.bt`, fixtures under `stdlib/test/fixtures/`. Rewrite `pool := TestSup supervise` to `pool := (TestSup supervise) unwrap` and similar for `startChild`/`terminate:`.
- **Effort:** S.

### Phase 3: e2e, examples, and docs
- **E2E btscript tests** (`tests/e2e/cases/`): `supervisor.btscript`, `supervisor_class_method.btscript`, `supervisor_initialize.btscript`, `named-actors.btscript` — update expected REPL output. These tests exercise the full lifecycle and are the canonical migration reference.
- **E2E fixtures** (`tests/e2e/fixtures/`): `e2e_init_dyn_supervisor.bt`, `e2e_named_supervisor.bt`, `e2e_initialize_supervisor.bt` — update `initialize:` hooks that call `sup startChild` to handle Result.
- **`examples/otp-tree/`:** `src/worker_pool.bt`, `test/otp_tree_test.bt`, `README.md`, `AGENTS.md`, `.github/copilot-instructions.md` — update all raise-shaped examples to Result-shaped.
- **Documentation:** `docs/beamtalk-language-features.md` (supervision chapter) — document the new signatures and the idempotent-startup convention; cross-link to ADR 0079's actor registration section for the parallel.
- **CHANGELOG.md:** entry under "Standard Library" and "Runtime" subsections describing the breaking change and the mechanical migration (add `unwrap` / `ifOk:ifError:` at call sites).
- **Effort:** M (mostly mechanical rewrites + doc prose).

### Phase 4: External project migration
- **beamtalk-exdura, beamtalk-symphony, any internal user code:** coordinated branch updates. Tracked as separate issues outside this epic — they consume stdlib, not source to modify here. Migration guide produced in Phase 3 is the authoritative reference.
- **Effort:** S per project (each has <20 callers).

### Critical path
Phase 1 → Phase 2 (blocks stdlib signatures on runtime returning Result) → Phase 3 (e2e exercises the full pipeline). Phase 4 can begin once stdlib is published but does not block the ADR's epic.

### Affected components (summary)
- Runtime: `beamtalk_supervisor.erl` only.
- Stdlib: `Supervisor.bt`, `DynamicSupervisor.bt`.
- Tests: 5 BUnit files (`supervisor_defaults_test`, `supervisor_initialize_test`, `dynamic_supervisor_defaults_test`, `dynamic_supervisor_initialize_test`, `supervision_class_method_test`), 4 e2e btscripts (`supervisor`, `supervisor_class_method`, `supervisor_initialize`, `named-actors`), ~10 e2e supervisor fixtures under `tests/e2e/fixtures/`.
- Docs: language-features supervision chapter + CHANGELOG.
- External: out-of-tree projects (Exdura, symphony) via coordinated branches.

Estimated total effort: M (4–6 small-medium Linear issues, plus coordinated external migration).

## Migration Path

Mechanical migration. Each call site falls into one of three patterns.

### Pattern 1: Boot-style "crash on failure"

```beamtalk
// Before
app := WebApp supervise

// After
app := (WebApp supervise) unwrap
```

Use `unwrap` when failure should propagate as an exception (application boot, test setup where failure aborts the run, REPL quick exploration).

### Pattern 2: Recoverable start

```beamtalk
// Before
app := [WebApp supervise] on: Error do: [:e | Logger error: e. nil]
app isNil ifFalse: [app count]

// After
(WebApp supervise)
  ifOk:    [:app | app count]
  ifError: [:e | Logger error: e]
```

Or, for chained start-and-configure:

```beamtalk
(WebApp supervise)
  andThen: [:app | Result ok: (app which: HealthCheck)]
  ifError: [:e | Logger warn: e message]
```

### Pattern 3: Idempotent terminate

```beamtalk
// Before — had to swallow Error because not_found raises
[app terminate: Counter] on: Error do: [:_e | nil]

// After — idempotent naturally: Ok(nil) whether fresh terminate or already gone
app terminate: Counter
// real failures still surface as Result error: (beamtalk_error terminate_failed)
(app terminate: Counter) ifError: [:e | Logger warn: e message]
```

### Common migration gotchas

- **Chained sends on return value.** Code that wrote `WebApp supervise stop` (never inspected) must insert `unwrap` before the chained call: `(WebApp supervise) unwrap stop`. Result does not forward supervisor methods.
- **Type-narrowing in tests.** `self assertEquals: #Supervisor actual: (WebApp supervise)` now compares against a `Result`, not the supervisor. Wrap with `unwrap` or compare explicitly: `self assert: (WebApp supervise) isOk`.
- **REPL display.** Interactive users will see `Result ok: #Supervisor<WebApp,_>` where they previously saw the bare supervisor. This is a training issue, not a correctness one — documentation should preview the new display shape prominently.

## Future Work

This ADR deliberately narrows scope to the Result migration. Several OTP capabilities the current Beamtalk supervisor surface does not cover are candidate follow-up ADRs. Each is listed with the Result variants it will need, to validate that the idempotent-startup convention remains sufficient.

1. **`Supervisor startChild: spec` for non-`simple_one_for_one` supervisors** — allow runtime addition of heterogeneous children to a `one_for_one` / `one_for_all` / `rest_for_one` tree. This is the case where `{error, already_present}` and `{error, {already_started, Pid}}` from OTP become load-bearing. Expected shape: `Result(Actor, Error)` with `Ok(child)` on fresh start AND on `{already_started, Pid}` (per the convention), and `Error(#child_already_registered)` for the distinct `already_present` case (spec registered, process dead — genuinely different from running). Establishes the need for `restartChild:` and `deleteChild:` (items 2–3).
2. **`Supervisor restartChild: class`** — wraps `supervisor:restart_child/2`. Restart a child whose spec is registered but whose process has terminated. Shape: `Result(Actor, Error)`.
3. **`Supervisor deleteChild: class`** — wraps `supervisor:delete_child/2`. Remove a child's spec from the supervisor (only applicable when the child has terminated and will not be auto-restarted). Shape: `Result(Nil, Error)` with `Ok(nil)` when deleted or already absent.
4. **Anonymous supervisor start** — `supervisor:start_link/2` without `{local, Name}`. Enables per-tenant / per-request supervision trees where the same `Supervisor` subclass is instantiated multiple times. Requires a companion ADR on how auto-registration-by-class-name interacts with explicit anonymous starts. Shape: `Result(Self, Error)`.
5. **`Supervisor getChildspec: class`** — wraps `supervisor:get_childspec/2`. Inspection of a single child's spec. Low priority; niche.

None of these require changes to the methods migrated in this ADR — the idempotent-startup convention is the load-bearing guarantee that makes them additive.

Separately:
- **`wasFreshlyStarted` accessor or `onFreshStart:` block form** on the supervisor returned by `supervise`, if post-migration usage shows callers consistently reach for the distinction. Not needed for the ADR; flagged here for decision-context continuity.
- **Linter for pre-migration callsites** — warns on `sup := Class supervise` (no `unwrap`/`value`/`ifOk:`/etc.) during the transition window. Built once, decommissioned once external projects migrate.

## Implementation Tracking

**Epic:** [BT-1993](https://linear.app/beamtalk/issue/BT-1993) — Migrate Supervisor Lifecycle to Result (ADR 0080)
**Status:** Complete — all phases landed across BT-1994..BT-2001.

| # | Issue | Phase | Summary |
|---|---|---|---|
| 1 | [BT-1994](https://linear.app/beamtalk/issue/BT-1994) | 0 | Probe FFI coercion vs `beamtalk_supervisor_new` post-dispatch hook — selected Option 2 (hook extension) |
| 2 | [BT-1995](https://linear.app/beamtalk/issue/BT-1995) | 0 | Probe `Result(C, Error)` typechecker substitution for `DynamicSupervisor(C)` — substitution works unchanged |
| 3 | [BT-1996](https://linear.app/beamtalk/issue/BT-1996) | 1 | Runtime: migrate `startLink/1` to Result-shaped return |
| 4 | [BT-1997](https://linear.app/beamtalk/issue/BT-1997) | 1 | Runtime: migrate `startChild/1,2` and `with_live_supervisor/3` to Result-shaped returns |
| 5 | [BT-1998](https://linear.app/beamtalk/issue/BT-1998) | 1 | Runtime: migrate `terminateChild/2` (both arities) with idempotent `not_found` on static path |
| 6 | [BT-1999](https://linear.app/beamtalk/issue/BT-1999) | 2 | Stdlib: update `Supervisor.bt` / `DynamicSupervisor.bt` return types and migrate BUnit tests |
| 7 | [BT-2000](https://linear.app/beamtalk/issue/BT-2000) | 3 | E2E + examples: migrate supervisor btscripts and `examples/otp-tree` to Result-shaped API |
| 8 | [BT-2001](https://linear.app/beamtalk/issue/BT-2001) | 3 | Docs: language-features supervision chapter + CHANGELOG + this Implementation Tracking section |

**Critical path:** BT-1994 → BT-1996 → BT-1997 → BT-1998 → BT-1999 → BT-2000 → BT-2001.
**Parallelizable:** BT-1995 alongside BT-1994 (independent probes). Runtime migrations BT-1996..BT-1998 landed sequentially but touch disjoint functions in `beamtalk_supervisor.erl` so later reordering would also have been safe.

**Divergence from proposed Phase 2 signature:** the proposal above (§Phase 1, §Phase 2) specifies `Supervisor class>>supervise -> Result(Supervisor, Error)` for the static path. Phase 2 shipped with `-> Result(Self, Error)` instead, unifying the static and dynamic paths and matching the ADR 0079 precedent for `Actor` registry operations (`spawnAs:`, `named:`). This means `AppSupervisor supervise` type-narrows to `Result(AppSupervisor, Error)` at the call site via the standard `Self` → concrete-subclass substitution. See `stdlib/src/Supervisor.bt:90` for the shipped signature.

## References
- Related issues: [BT-1977](https://linear.app/beamtalk/issue/BT-1977) — this ADR's driving issue
- Related ADRs:
  - ADR 0079 — Named Actor Registration (precedent for `Result(Self, Error)` on boundary operations; Future Work section committed to this ADR)
  - ADR 0060 — Result Type, Hybrid Error Handling (Result semantics and combinator API)
  - ADR 0076 — Convert Erlang ok/error Tuples to Result at FFI Boundary (coercion policy)
  - ADR 0065 — Complete OTP Primitives and Actor Lifecycle (original surface definition)
  - ADR 0059 — Supervision Tree Syntax (supervisor API design foundation)
  - ADR 0015 — Signal-Time Exception Objects and Error Class Hierarchy (`beamtalk_error` shape)
- External: [`supervisor(3erl)`](https://www.erlang.org/doc/man/supervisor), [Elixir `Supervisor.start_link/3`](https://hexdocs.pm/elixir/Supervisor.html), [Gleam `gleam_otp`](https://hexdocs.pm/gleam_otp/)
- External projects needing coordinated migration: beamtalk-exdura, beamtalk-symphony
