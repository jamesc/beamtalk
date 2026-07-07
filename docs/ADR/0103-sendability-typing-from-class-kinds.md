# ADR 0103: Sendability Typing from Class Kinds

## Status
Accepted (2026-07-07)

## Implementation Tracking

**Epic:** [BT-2748](https://linear.app/beamtalk/issue/BT-2748)
**Issues:**

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 0 | [BT-2753](https://linear.app/beamtalk/issue/BT-2753) | Builtin tier table + tier-derivation core + `HandleScoped(#process)` message-arg check | M | – |
| 1 | [BT-2754](https://linear.app/beamtalk/issue/BT-2754) | `handleScope:` class-side keyword (parser, `ClassHierarchy`, `__beamtalk_meta`) | M | – |
| 1 | [BT-2755](https://linear.app/beamtalk/issue/BT-2755) | Full tier derivation (`Value` composition + `type_args`) + `spawnWith:` value check + hover + stdlib audit | M | BT-2753, BT-2754 |
| 2 | [BT-2756](https://linear.app/beamtalk/issue/BT-2756) | Block-capture check (Phase 3 validator × `type_map`) | M | BT-2753, BT-2755 |
| 2 | [BT-2757](https://linear.app/beamtalk/issue/BT-2757) | Announcement payloads + undeclared-handle companion lint | S | BT-2755 |
| 3 | [BT-2758](https://linear.app/beamtalk/issue/BT-2758) | Diagnostic integration tests + docs + status flip | S | BT-2756, BT-2757 |

Sibling epic **[BT-2747](https://linear.app/beamtalk/issue/BT-2747)** (ADR 0104): BT-2755 shares the `spawnWith:` `MapLiteral` call-site inspection with BT-2750 (`related-to`, either order). Deferred: remote-node grading of `#node` scopes (blocked on a future cluster-registration ADR).

**Status:** Planned

## Context

### Problem statement

Values cross process boundaries constantly in Beamtalk — actor message
arguments and returns, `spawnWith:` initial state, Announcement payloads, and
blocks handed to actors (e.g. `Timer every: 1000 do: [...]`). On the BEAM,
a send *copies* the term. Whether that copy means what the sender thinks it
means depends on what kind of value it is — and Beamtalk's three class kinds
(ADR 0067) already name exactly the distinction that matters:

| Kind | Runtime shape | What a send does | Hazard |
|---|---|---|---|
| `Value` | plain Erlang map, immutable | perfect copy — semantically free | none |
| `Actor` | Pid reference | copies the *reference*; identity preserved | dies with the process; remote-node OK |
| `Object` | no Beamtalk-managed data, but instances may wrap runtime-backed handles (ETS refs, ports — e.g. `Subscription`'s `SubRef`) | copies the handle | handle validity is **scoped** — many are node-global, some process-bound (ports), none survive naïve use after the owner dies; distribution to a remote node can silently break them |

Today the type checker knows a receiver's class but does nothing with the
*kind*. Sending a port-wrapping Object to an actor on another node, or
capturing one in a block passed to `Timer`, is silently fine at compile time
and confusing at runtime.

### Why Beamtalk specifically (positioning)

Elixir and Gleam cannot express this *question*: all their data is immutable,
and their identity idioms (bare Pids, ETS handles, refs) carry no type-level
distinction at all. Pony answers it with six reference capabilities and a
famously steep learning curve. Beamtalk's surface syntax **already encodes the
answer** — users write `Value subclass:` / `Actor subclass:` /
`Object subclass:` today. The checker just has to use it. See
`docs/internal/positioning.md` (Seed 1).

### Constraints

- **Advisory only.** ADR 0100 governs: sendability findings are warnings/lints
  graded by knowledge completeness, never compile blockers. Dynamic receivers
  and arguments are silent.
- **No new annotations required** for the common case — the class kind *is*
  the annotation.
- **No runtime cost.** This is edit/compile-time analysis; codegen unchanged.

## Decision

Derive a **sendability tier** for every inferred type from its class kind, and
warn when a value's tier is too weak for the boundary it crosses.

### Tiers

```text
Sendable       — Value kinds, primitives (Integer, String, …), Symbol,
                 unions/collections of Sendable; also opaque tokens with no
                 owner (e.g. Reference — a raw make_ref() is hazard-free)
SendableRef    — Pid-backed references: Actor kinds and the builtin `Pid`
                 itself; reference semantics survive the send, validity tied
                 to the process lifetime
HandleScoped   — Object kinds wrapping runtime-backed state whose validity
                 has a scope (see below)
Unknown        — Dynamic, untyped FFI results (ADR 0075's
                 Dynamic(DynamicSpec)), or Object kinds with no
                 classification (silent, per ADR 0100)
```

`Value` composes structurally, taking the **weakest** field tier: a `Value`
whose field types are all `Sendable` is `Sendable`; a `Value` carrying a
`SendableRef` field (and no `HandleScoped` field) inherits `SendableRef` (the
copy embeds a Pid — reference semantics travel with it, and hover must say
so); a `Value` carrying a `HandleScoped` field inherits `HandleScoped` (the
copy embeds the handle). Ordering: `Sendable < SendableRef < HandleScoped`,
with `Unknown` fields making the composite `Unknown`.

Honest scoping note: **no diagnostic rule consumes `SendableRef` in v1**, and
rather than leave the tier's fate as an implementation choice (which would let
hover behaviour diverge between implementations), its v1 contract is fixed:
**hover displays `SendableRef` for `Pid`- and Actor-typed values** — a
testable requirement, shipped with the Phase 0 builtin tier table. That is the
tier's *only* v1 consumer; the first diagnostic rule that gates on it (if any)
arrives with the deferred remote-grading work. It earns a stored tier (rather
than collapsing to three) because `Pid`'s behaviour genuinely differs from
both neighbours and hover must say so truthfully.

**Builtins are classified directly, not via annotation.** The flagship hazard
classes already exist in the stdlib as `sealed typed Object subclass:` with no
kind distinction — and one of them breaks a naive kind→tier rule: `Pid` is
Object-kind but *behaves* as `SendableRef` (a live cross-node-valid process
reference). Tier assignments for builtins ship as a table in the checker, not
as source annotations:

| Builtin | Tier | Why |
|---|---|---|
| `Pid` | `SendableRef` | live process reference; valid across nodes |
| `Port` | `HandleScoped(#process)` | port dies with / is bound to its owner |
| `Reference` | `Sendable` | opaque comparison token, no owner, no hazard |
| `Subscription` | `HandleScoped(#node)` | wraps a node-local ETS row |
| `FileHandle` | `HandleScoped(#process)` *(approximation)* | documented as "must not escape" its `open:do:` block — its true scope is *dynamic extent*, tighter than any process; `#process` is the conservative expressible bound (see Consequences) |

This table **is the Phase 0 deliverable** — it makes the canonical hazards
(`Port` in an actor message) warn on day one with zero user annotations.

### Stdlib inventory audit (BT-2755)

The FFI-wrapping stdlib `Object subclass: … native: …` classes were audited to
confirm no process-bound handle escapes classification:

| Class | Backing | Tier | Rationale |
|---|---|---|---|
| `Port`, `FileHandle` | ports / file handles | `HandleScoped(#process)` | **builtin table** — the true `#process` hazards |
| `Subscription` | ETS `SubRef` | `HandleScoped(#node)` | builtin table; node-local, silent in v1 |
| `Ets`, `AtomicCounter` | ETS table / `atomics` | `#node` *(candidate)* | node-local, reachable by name/ref across processes; silent in v1 |
| `Timer` | `send_after` ref | `#node` *(candidate)* | node-local timer reference |
| `Console`, `File`, `Json`, `System`, `OS`, `Program`, `Tracing`, `Session`, `BindingsView`, `TestRunner`, `TestResult` | stateless facades | `Unknown` (silent) | class-method APIs with no per-instance runtime handle — nothing to scope |

**Conclusion:** every `#process`-bound handle in the stdlib is covered by the
builtin table. The node-local candidates (`Ets`, `AtomicCounter`, `Timer`) are
`#node`-scoped — **silent in v1** regardless of whether they carry an explicit
`handleScope:` — so declarations are deferred; the undeclared-handle companion
lint (BT-2757) will nudge for them without changing diagnostics. The stateless
facades correctly stay `Unknown`. No new `handleScope:` declarations are
required for v1 correctness.

### Declaring handle scope (the one new annotation)

User-defined `Object subclass:` classes that wrap runtime state may declare
their scope; undeclared Object kinds stay `Unknown` (silent):

```beamtalk
// node-global handle: fine to send within the node, not across nodes
sealed typed Object subclass: MetricsTable
  handleScope: #node
```

The scope is symbol-valued and the set is deliberately **open** — `#node` and
`#process` ship first; a `#dynamicExtent` value (the `FileHandle` case) is a
plausible later addition rather than a redesign. Construction of such classes
follows the existing FFI-wrapping pattern (ADR 0101 `native:` / delegate, as
`Ets` and `Subscription` do today).

### Checked boundaries

1. **Actor message arguments and `spawnWith:` maps** — warn when a
   `HandleScoped(#process)` value is passed. `#node`-scoped values are
   **silent in v1**: warning usefully requires static knowledge that the
   receiver is remote, and no such knowledge exists — ADR 0079 is explicitly
   *local* (per-node) registration; cluster-wide registration is a deferred
   future ADR. When that ADR lands, `#node`-scoped sends to known-remote
   receivers gain an info-level note. Recorded as blocked, not designed here.
2. **Blocks sent to actors** (including `Timer every:do:`, `!` casts) — warn
   when the block captures a `HandleScoped(#process)` value. **This is new
   analysis, not reuse**: the compiler has no closure-conversion pass (Core
   Erlang funs capture natively), and the two existing capture datasets are
   both name-only and invisible to the type checker —
   `BlockMutationAnalysis.captured_reads` is computed at codegen time, and
   the semantic-analysis `Analyser::collect_captures_and_mutations`
   (`CapturedVar{name, defined_at}`) runs in Phase 3, *after* type checking.
   The check therefore lives in a Phase 3 validator that joins
   `CapturedVar`s with the type checker's `type_map` — budgeted as real work
   in Implementation, with a tier-derivation function shared between it and
   the Phase 2 message-argument checks.
3. **Announcement payloads** — same rule as message arguments.

### Error example

```beamtalk
port := (Erlang erlang) openPort: cmd     // :: Port — builtin tier table
worker schedule: [port readLine]
// ⚠️ block captures `port` (Port — process-bound handle) and is sent to
//    another process — a port is only usable by its owning process.
//    Consider passing data, or an Actor that owns the port.
```

(The block crosses the boundary as an ordinary message argument; the same
check applies when the send is a postfix cast — `worker schedule: [...]!`.
There is no infix `!` send in Beamtalk — see ADR 0104 §2.)

### REPL session

```
bt> p := Point x: 1 y: 2         // Value — Sendable
bt> counter add: p               // fine, no diagnostic
bt> worker consume: port
   ⚠️ `port` (Port — process-bound handle) passed in an actor message; it is
      only usable by its owning process
```

## Prior Art

| System | Approach | Take / leave |
|---|---|---|
| **Pony** | Six reference capabilities (`iso`, `val`, `ref`, …) checked soundly | The *goal* (deny bad sends) without the cost: Pony's capabilities annotate every type use; Beamtalk reads three class kinds users already declare. We accept advisory instead of sound. |
| **Rust** | `Send`/`Sync` auto-traits, structural derivation | The composition model — sendability derived structurally through fields — is exactly our `Value` rule. |
| **Erlang/Elixir** | None; copy silently, handles carry no distinction | The gap this fills. |
| **Gleam** | All-immutable so data is trivially sendable; typed `Subject` covers references only | Confirms the reference tier is worth typing; Gleam has no handle tier at all. |

## User Impact

- **Newcomer:** never sees this until they wrap a port and send it — then gets
  an explanation instead of a hang. `handleScope:` is discoverable via the
  class-kind docs.
- **Smalltalk developer:** no change to message semantics; one optional
  declaration keyword on FFI-wrapping classes.
- **Erlang/BEAM developer:** finally a compile-time answer to "is it safe to
  send this?" — a question they answer today by reading implementation.
- **Operator:** fewer "works on one node, breaks distributed" surprises;
  zero runtime change.
- **Tooling developer:** tiers derive from existing `ClassHierarchy` kind
  data; hover can show a value's tier.

## Steelman Analysis

- 🧑‍💻 **Newcomer, for doing nothing:** "I don't distribute; this warning is
  noise." — Countered: `#process`-scoped warnings fire only on real
  cross-process sends, which bite single-node users too.
- ⚙️ **BEAM veteran, for runtime checks instead:** "Just crash early on bad
  handle use." — Countered: the crash happens far from the send; edit-time
  attribution is the point. Runtime checks also cost every send.
- 🎨 **Language designer, for full capabilities (Pony):** "Advisory tiers are
  unsound; mutation through a copied alias still lurks." — Acknowledged:
  Beamtalk chooses gradual/advisory across the board (ADR 0100); soundness is
  explicitly not the product (see positioning.md, "What this rules out").

### Tension point
Whether `#node`-scoped sends to *possibly*-remote receivers warn or stay
silent: veterans want silence (distribution is deliberate), newcomers want the
note. Resolved by grading on *knowledge* — and today the checker has **none**:
there is no static source of receiver remoteness (ADR 0079 is local-only
registration; cluster registration is a future ADR). So `#node` findings are
silent in v1, becoming info-level only when a cluster-registration ADR gives
the checker something to grade on.

## Alternatives Considered

### Do nothing
Handles keep failing at a distance. Rejected: the class-kind information is
already declared; not using it wastes Beamtalk's one structural advantage here.

### Runtime send-guards
Wrap `gen_server` sends with tier checks. Rejected: per-send cost, crashes
instead of edit-time diagnostics, and cannot see block captures.

### Full capability system (Pony-style)
Sound but demands per-use annotations and a learning curve incompatible with
gradual adoption. Rejected; recorded as the sound end-state this deliberately
under-shoots.

## Consequences

### Positive
- The first BEAM language where "will this value survive the send?" is a
  typed, hover-visible property — with the canonical hazards (`Port`) covered
  by the builtin tier table at **zero** annotation cost.

### Negative
- **This wires class-kind data into the type checker for the first time.**
  `ClassHierarchy::resolve_class_kind` exists and is reachable, but nothing in
  `inference.rs`/`validation.rs` consumes kind today (only downstream Phase 5
  validators do) — this is net-new checker logic, not a refactor.
- **Block-capture checking is genuinely new analysis** (see Checked
  boundaries #2): a Phase 3 validator joining name-only `CapturedVar` data
  with the type checker's `type_map`, plus a tier-derivation function shared
  with the Phase 2 message-argument checks. Two phases, one shared core —
  the split must be kept explicit or the tiers will drift.
- `handleScope:` is a new class-side keyword — and ADR 0067's precedent says
  such keywords are not cheap (its comparable keyword change took 11 tracked
  issues across parser, ClassHierarchy, codegen metadata, and docs).
- **Perverse incentive:** declaring `handleScope:` is what turns silence into
  (advisory) findings, so authors of handle-wrapping classes are mildly
  incentivised *not* to declare. Mitigation: a companion lint nudging
  FFI-wrapping `Object` classes with no classification toward declaring, and
  the builtin table covering the stdlib regardless.
- The two-value scope enum is a knowing approximation: `FileHandle`'s true
  scope is *dynamic extent* (must not escape `open:do:`), tighter than
  `#process`. The open symbol set leaves room; the approximation is accepted
  for v1.
- Tier derivation for generic collections needs care
  (`List(Port)` is `HandleScoped`); interacts with ADR 0102's `type_args`
  rules.
- False silence is guaranteed under `Dynamic` — must be documented as
  advisory, not a guarantee.

### Neutral
- No runtime or codegen behaviour change; metadata only.
- Remote-node grading of `#node`-scoped sends is **blocked on a future
  cluster-registration ADR** (ADR 0079 is local-only); nothing in v1 claims
  remoteness knowledge.

## Implementation

1. **Phase 0 (napkin, ~S):** builtin tier table (`Pid`, `Port`, `Reference`,
   `Subscription`, `FileHandle`) + the message-argument check for
   `HandleScoped(#process)`, wired into the type checker (first consumer of
   class-kind data there). Proves the value with zero new syntax — `Port` in
   an actor message warns.
2. **Phase 1 (~M):** `handleScope:` keyword (parser, `ClassDefinition`,
   `ClassHierarchy`, `__beamtalk_meta` — budget per the ADR 0067 precedent),
   tier derivation incl. `Value` composition and `type_args`, `spawnWith:`
   checks, hover rendering, stdlib inventory audit (Ets, Announcer, …).
3. **Phase 2 (~M):** block-capture checking (Phase 3 validator + `type_map`
   join + shared tier-derivation core); Announcement payloads; the
   undeclared-handle-class companion lint.
4. **Deferred (blocked):** remote-receiver grading of `#node` scopes — needs
   the future cluster-registration ADR first.

**Affected:** parser/AST, class hierarchy, type checker (first kind-aware
logic), a Phase 3 validator, hover; **not** codegen output semantics or
runtime dispatch.

## Implementation Tracking

**Epic:** [BT-2748](https://linear.app/beamtalk/issue/BT-2748)
**Issues:**

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 0 | [BT-2753](https://linear.app/beamtalk/issue/BT-2753) | Builtin tier table + tier-derivation core + `HandleScoped(#process)` message-arg check | M | – |
| 1 | [BT-2754](https://linear.app/beamtalk/issue/BT-2754) | Parser + class hierarchy: `handleScope:` class-side keyword | M | – |
| 1 | [BT-2755](https://linear.app/beamtalk/issue/BT-2755) | Full tier derivation (`Value` composition + `type_args`) + `spawnWith:` check + hover + stdlib audit | M | BT-2753, BT-2754 |
| 2 | [BT-2756](https://linear.app/beamtalk/issue/BT-2756) | Block-capture sendability check (Phase 3 validator × `type_map`) | M | BT-2753, BT-2755 |
| 2 | [BT-2757](https://linear.app/beamtalk/issue/BT-2757) | Announcement payload sendability + undeclared-handle companion lint | S | BT-2755 |
| 3 | [BT-2758](https://linear.app/beamtalk/issue/BT-2758) | Diagnostic integration tests + docs + status flip | S | BT-2756, BT-2757 |

**Deferred (blocked):** remote-receiver grading of `#node` scopes — needs a
future cluster-registration ADR (ADR 0079 is local-only). Until then,
`#node`-scoped sends are silent in v1.

**Status:** Implemented (Phases 0–3).

## References
- Seed: `docs/internal/positioning.md` (Seed 1)
- Related ADRs: ADR 0067 (class kinds — incl. the keyword-cost precedent),
  ADR 0042 (value types), ADR 0100 (advisory severity), ADR 0079 (named actor
  registration — **local-only**; cluster registration deferred), ADR 0075
  (FFI type definitions — untyped FFI results are `Dynamic(DynamicSpec)`,
  hence tier `Unknown`), ADR 0101 (`native:` FFI-wrapping construction
  pattern), ADR 0102 (type operators, `type_args` rules), ADR 0093
  (Announcements)
- Prior art: Pony reference capabilities; Rust `Send`/`Sync`
