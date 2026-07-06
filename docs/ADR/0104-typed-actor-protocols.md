# ADR 0104: Typed Actor Protocols — the Class Interface Is the Protocol

## Status
Proposed (2026-07-05)

## Context

### Problem statement

Nobody on the BEAM types the process boundary well. Erlang/Elixir `GenServer`
funnels every message through `handle_call/3` — the compiler cannot relate a
call site to the clause that handles it. Gleam gets typed messaging only by
inventing `Subject(msg)`, a parallel, non-OTP-standard actor layer.

Beamtalk's model — **actor = class, message = selector, sync send =
`gen_server:call`** — means the actor's class interface *already is* its
protocol type. `docs/internal/type-system-design.md` names this Option A
("messages typed by actor's interface") and recommends it, but no ADR ratifies
it or defines the async edges. Meanwhile the checker already infers through
sync actor sends as ordinary method sends — by accident of representation
rather than by decision.

### What is undefined today

1. **Cast (`!`).** `worker doThing!` uses `gen_server:cast` and returns `nil`
   immediately (`beamtalk-language-features.md` §Explicit Async Cast). What is
   its static type — and should a cast to a method whose return value matters
   be flagged?
2. **`spawn` / `spawnWith:`.** Class-side constructors returning an actor
   reference; `spawnWith:` takes an initial-state map that should check
   against declared `state:` slots.
3. **Timeouts.** `db withTimeout: 30000` returns a `TimeoutProxy` that
   forwards everything — the proxy should be *transparent* to typing (same
   interface as the wrapped actor), and a timed-out call raises rather than
   returning, so return types are unchanged.
4. **Cross-process DNU.** An unknown selector raises `does_not_understand` in
   the *callee* process. Open-world policy (ADR 0100) applies with the same
   knowledge grading as local sends.

### Constraints

- **No parallel messaging layer.** The whole point (contra Gleam) is that
  typing falls out of the class interface. No `Subject`, no channel types.
- **Advisory** per ADR 0100.
- **OTP-transparent:** typed actors remain plain `gen_server`s callable from
  Erlang/Elixir.

## Decision

Ratify Option A and define the four edges. An `Actor subclass:`'s **public
method set is its message protocol**; the checker types actor sends exactly as
method sends, with these rules:

### 1. Sync send — already works; now guaranteed

```beamtalk
counter :: Counter := Counter spawn
counter increment      // :: Integer — the method's declared/inferred return
```

### 2. Cast (`!`) types as `Nil` — the misuse case is already a parse error

```beamtalk
counter increment!          // :: Nil — fire-and-forget, bare statement
x := counter getCount!      // ✗ already a PARSE ERROR today:
                            //   cast_in_expression_error — a cast cannot be
                            //   assigned, returned, or passed as an argument
```

The grammar already enforces more than a warning could: casts are legal
**only as bare statements** (`cast_in_expression_error`, enforced in the
parser and pinned by parser tests — `x := foo bar!` and `^foo bar!` are
rejected outright). For avoidance of doubt: the postfix statement-cast is the
**only** `!` form — there is no infix `actor ! expr` in Beamtalk (raw Erlang
`!` is not exposed as surface syntax), so there is exactly one async-send
construct for the type rule and for ADR 0103's sendability checks to cover. So no "discarded reply" dataflow analysis is needed or
possible — the only remaining rule is the *type*: a cast statement's value is
`Nil` (today it is `Dynamic`).

**That retype is not free.** Typing casts as `Nil` activates a currently
dormant check: `check_return_type` skips `Dynamic` bodies, so a method
declared `-> SomeType` whose body *ends* in a bare cast produces no
diagnostic today but would newly warn ("body returns Nil"). That is usually a
genuine catch (the method doesn't return what it declares), but it is a
new-diagnostic wave on existing code — see Migration Path.

One deliberate non-feature: there is **no** "you cast to a method with a
meaningful return" warning. Fire-and-forget casts to result-returning methods
(`self tick!`, `counter increment!` where `increment` returns the new count)
are idiomatic — intent is not statically detectable, and unannotated mutators
infer concrete return types (there is no implicit-`Self` convention to key
off), so any such warning would mostly fire on correct code.

### 3. Constructors check state

```beamtalk
Counter spawnWith: #{count => 0}     // :: Counter — keys checked against
                                     //    declared `state:` slots and types
Counter spawnWith: #{cuont => 0}     // ⚠️ unknown state key `cuont` —
                                     //    did you mean `count`?
```

`spawn` / `spawnWith:` on `Meta{C}` return `Known{C}` (instance type), using
ADR 0083's metaclass-aware inference.

### 4. Proxy return-type transparency; DNU follows ADR 0100

Getting the *severity* right costs nothing — `TimeoutProxy` overrides
`doesNotUnderstand:` to forward, and ADR 0100 already makes unresolved
selectors on DNU-overriding receivers silent, so there are no false
diagnostics today. What is actually missing is **positive inference**:
`withTimeout:` is hardcoded to return `TimeoutProxy`
(`generated_builtins.rs`), so `slowDb query: sql` types as `Dynamic` instead
of the wrapped class's real return type. The rule: `withTimeout:` on a
receiver of type `C` returns a value *typed as `C`* (a timeout raises rather
than returning, so method return types are unchanged). Unknown selectors on a
statically-known actor class get the same knowledge-graded diagnostic as any
send (ADR 0100) — the process boundary adds no new severity rule.

### Error example

```beamtalk
logger := Logger spawn
logger logg: "hi"
// ⚠️ Logger does not understand `logg:` — did you mean `log:`?
//    (same open-world grading as a local send; the process boundary is
//     invisible to the diagnostic)
```

## Prior Art

| System | Approach | Take / leave |
|---|---|---|
| **Gleam `gleam_otp`** | `Subject(msg)` — typed channel, parallel to OTP | Proves demand for typed actors on BEAM; rejects the parallel layer. Beamtalk's dispatch *is* the typed surface, and stays OTP-standard. |
| **Erlang/Elixir GenServer** | Untyped `handle_call` funnel | The baseline being improved on; Beamtalk actors remain callable as plain gen_servers. |
| **Akka Typed (Scala)** | `Behavior[Msg]` — protocol as message ADT | Same goal via sum types; Beamtalk gets it via selectors, which also gives per-message docs/completion for free. |
| **Pony** | Behaviours (async methods) are typed and return nothing | Validates "casts type as no-reply"; Pony makes it structural (`be` methods), we make it call-site (`!`). |
| **Session types (research)** | Type message *sequences*, not just signatures | Deliberately out of scope: per-message typing is the 80%; protocol-state typing is research-grade and fights liveness. |

## User Impact

- **Newcomer:** actor calls autocomplete and type-check like any method call —
  the process boundary costs nothing cognitively. The `!`-discards-reply
  warning catches the classic beginner bug.
- **Smalltalk developer:** "messages all the way down" (principle 6) now
  extends to typing; nothing new to learn.
- **Erlang/BEAM developer:** `spawnWith:` key checking is the map-typo safety
  `init/1` never had. Cross-node calls type the same as local (location
  transparency preserved).
- **Operator:** none — no runtime or wire change.
- **Tooling developer:** REPL/LSP completion on actor references needs no
  special case; `TimeoutProxy` transparency is one inference rule.

## Steelman Analysis

- 🎨 **Language designer, for session types:** "Signatures don't catch
  calling `stop` before `start` — protocol *state* is the real actor bug." —
  True and acknowledged: recorded as future work; requires typestate that
  fights hot reload (a state machine checked against code that can change
  mid-protocol).
- ⚙️ **BEAM veteran, for doing nothing:** "gen_server has worked untyped for
  30 years; dialyzer specs exist." — Countered: specs don't check call sites
  against handlers; Beamtalk gets that for free from its own dispatch — the
  cost of ratifying it is near zero.
- 🧑‍💻 **Newcomer, for a discarded-reply warning:** "Warn me when I cast to
  something that returns a value I probably wanted!" — Rejected after checking
  the grammar: casts are *already* illegal in expression position (parse
  error), so the reply can never be accidentally consumed; and fire-and-forget
  casts to result-returning methods are idiomatic, so an intent-guessing
  warning would mostly fire on correct code (see Decision §2).

### Tension point
Whether `spawnWith:` unknown-key checking should be an error (it is a certain
runtime problem) or stay advisory — and how `Warning` squares with ADR 0100,
whose Rule 1 caps a single closed-complete receiver's *unresolved selector*
at `Hint`. The reconciliation: an unknown `spawnWith:` key is not an
unresolved selector but a **provably failing** construction — the class's
`state:` slots are a closed set declared in the same program, and a key
outside it cannot succeed — which is the tier ADR 0100 *does* grade `Warning`.
Proposed as `Warning` on that basis (not as a silent escalation); revisit with
strict mode.

## Alternatives Considered

### Typed channels (`Subject`-style)
A parallel typed messaging layer à la Gleam. Rejected: duplicates dispatch,
breaks "messages all the way down", and abandons OTP-standard call semantics —
the exact trade Beamtalk exists to avoid.

### Protocol objects per actor
Generate a separate protocol type from each actor class and require it in
annotations (`:: CounterProtocol`). Rejected: the class name already serves;
a second name per actor is ceremony without information.

### Session/typestate protocols
Type legal message *sequences*. Rejected for now (see steelman) — research
grade, poor fit with hot reload; per-message typing delivers most value.

## Consequences

### Positive
- Typed actors with **zero new syntax** for the sync path — ratification plus
  four small rules, most reusing existing inference (ADR 0083 metaclass
  inference, ordinary method checking).
- The cast-discard warning eliminates a real, common silent bug.
- Distinctive vs whole ecosystem: typed actor messaging that is also plain
  OTP.

### Negative
- `spawnWith:` checking is **not** blocked on metadata — `state:` slot types
  are already checker-visible (`ClassHierarchy::state_field_type`, already
  consumed in inference). The real work is at the **call site**: map-literal
  inference currently collapses to one joined `Dictionary(K, V)`, discarding
  the individual literal keys, so key checking needs AST-level `MapLiteral`
  pair inspection plus typo-suggestion logic that doesn't exist anywhere yet.
- Slot *value* checking against union-typed slots (e.g. `TimeoutProxy`'s own
  `timeoutMs :: Integer | #infinity`) rides on ADR 0102's union algebra.
- `TimeoutProxy` transparency is a special-case inference rule (proxy class →
  wrapped class, replacing today's hardcoded `TimeoutProxy` return); any
  future proxy needs the same treatment or a general `forwards ::` mechanism
  (the north-star DNU section's option 3).
- The cast retype (`Dynamic` → `Nil`) activates the dormant
  `check_return_type` path for bare-cast method tails — a new-diagnostic wave
  (see Migration Path).

### Neutral
- No wire, runtime, or codegen change; Erlang callers see identical
  gen_servers.

## Implementation

1. **Phase 1 (~S):** ratify sync-send typing with tests (it works today —
   pin it); `!` retypes as `Nil` **behind the migration audit** (below); fix
   the `beamtalk-language-features.md` cast-syntax inconsistency (its §Explicit
   Async Cast shows a prefix `c ! increment` example the grammar does not
   accept — real usage and the parser are postfix `c increment!`).
2. **Phase 2 (~M):** `spawnWith:` key checking — AST-level `MapLiteral` pair
   inspection at the call site, unknown-key `Warning` (provably-failing tier),
   typo suggestions; slot-value checking where ADR 0102's algebra permits.
3. **Phase 3 (~S):** `TimeoutProxy` return-type transparency rule (replace
   the hardcoded `TimeoutProxy` return); cross-process DNU wording unified
   with ADR 0100 diagnostics.

**Affected:** type checker (inference + validation), diagnostics, one
language-features doc fix; **not** runtime, codegen, or the wire protocol.

## Migration Path

The only behaviour change on existing code is the cast retype: methods with a
declared return type whose body **ends in a bare cast** currently escape
`check_return_type` (`Dynamic` bails out) and will newly warn that the body
returns `Nil`. This is usually a genuine mismatch worth surfacing, but it is
a new-diagnostic wave: before shipping Phase 1, audit the stdlib and test
corpus for bare-cast method tails and fix or annotate them in the same
change. Everything else (`spawnWith:` checking, proxy transparency) is
purely additive and advisory.

## References
- Seed: `docs/internal/positioning.md` (Seed 2)
- Design basis: `docs/internal/type-system-design.md` §Actor Message Passing
  (Option A); `docs/beamtalk-principles.md` 6, 7;
  `docs/internal/set-theoretic-types-north-star.md` §Working around
  `doesNotUnderstand:` (option 3 — `forwards ::` for typed proxies)
- Related ADRs: ADR 0100 (severity — incl. the provably-failing tier the
  `spawnWith:` Warning rests on), ADR 0083 (metaclass inference), ADR 0079
  (named actors), ADR 0067 (`state:`), ADR 0102 (union algebra for
  union-typed slots)
- Prior art: Gleam `gleam_otp` `Subject`; Akka Typed; Pony behaviours
