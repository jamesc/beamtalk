# ADR 0103: Sendability Typing from Class Kinds

## Status
Proposed (2026-07-05)

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
                 unions/collections of Sendable
SendableRef    — Actor kinds (Pid-backed): reference semantics survive the
                 send; validity tied to process lifetime
HandleScoped   — Object kinds wrapping runtime-backed state (declared, see
                 below): valid within a scope (node-global or process-bound)
Unknown        — Dynamic, or Object kinds with no declaration (silent, per
                 ADR 0100)
```

`Value` composes structurally: a `Value` whose field types are all `Sendable`
is `Sendable`; a `Value` carrying a `HandleScoped` field inherits
`HandleScoped` (the copy embeds the handle).

### Declaring handle scope (the one new annotation)

`Object subclass:` classes that wrap runtime state may declare their scope;
undeclared Object kinds stay `Unknown` (silent):

```beamtalk
// node-global handle: fine to send within the node, not across nodes
sealed typed Object subclass: Subscription
  handleScope: #node

// process-bound handle (e.g. wraps a port): only meaningful in the owner
sealed typed Object subclass: SocketHandle
  handleScope: #process
```

### Checked boundaries

1. **Actor message arguments and `spawnWith:` maps** — warn when a
   `HandleScoped(#process)` value is passed; info-level note for
   `#node`-scoped values only when the receiver is known to be remote
   (named-actor registration on another node, ADR 0079).
2. **Blocks sent to actors** (including `Timer every:do:`, `!` casts) — warn
   when the block's *captured environment* (already computed for closure
   conversion) includes a `HandleScoped(#process)` value.
3. **Announcement payloads** — same rule as message arguments.

### Error example

```beamtalk
sock := SocketHandle open: 8080
worker ! [sock readLine]
// ⚠️ block captures `sock` (SocketHandle, handleScope: #process) and is sent
//    to another process — the handle is only valid in the owning process.
//    Consider passing data, or an Actor that owns the socket.
```

### REPL session

```
bt> p := Point x: 1 y: 2        // Value — Sendable
bt> counter ! (p)                // fine, no diagnostic
bt> sub := announcer when: Tick do: [:e | e]
bt> remoteWorker register: sub   // remote receiver + #node handle
   ℹ️ `sub` (Subscription, handleScope: #node) sent to a remote node — the
      handle will not be valid there
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
note. Resolved by grading on *knowledge*: warn only when remoteness is
statically known (ADR 0079 registration), otherwise silent.

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
  typed, hover-visible property — at near-zero annotation cost.
- Block-capture checking reuses closure-conversion data; no new analysis pass.

### Negative
- `handleScope:` is a new class-side keyword (parser + ClassHierarchy +
  codegen metadata).
- Tier derivation for generic collections needs care
  (`List(SocketHandle)` is `HandleScoped`); interacts with ADR 0102's
  `type_args` rules.
- False silence is guaranteed under `Dynamic` — must be documented as
  advisory, not a guarantee.

### Neutral
- No runtime or codegen behaviour change; metadata only.

## Implementation

1. **Phase 0 (napkin):** derive tiers from class kinds only (no
   `handleScope:`), warn on the single clearest case — `#process`-scoped
   stdlib types (ports) passed in actor messages. Validates the plumbing.
2. **Phase 1:** `handleScope:` keyword (parser, `ClassDefinition`,
   `ClassHierarchy`, `__beamtalk_meta`), tier derivation incl. `Value`
   composition, message-argument + `spawnWith:` checks. ~M.
3. **Phase 2:** block-capture checking; Announcement payloads; remote-receiver
   grading via ADR 0079 registration data. ~M.

**Affected:** parser/AST, class hierarchy, type checker, hover; **not**
codegen output semantics or runtime dispatch.

## References
- Seed: `docs/internal/positioning.md` (Seed 1)
- Related ADRs: ADR 0067 (class kinds), ADR 0042 (value types), ADR 0100
  (advisory severity), ADR 0079 (named/remote actors), ADR 0102 (type
  operators, `type_args` rules), ADR 0093 (Announcements)
- Prior art: Pony reference capabilities; Rust `Send`/`Sync`
