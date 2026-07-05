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

### 2. Cast (`!`) types as `Nil` — and warns on discarded meaning

```beamtalk
counter increment!          // :: Nil — fire-and-forget
x := counter getCount!      // ⚠️ cast discards the reply: `getCount` returns
                            //    Integer, but `!` always yields nil
```

The rule: `expr selector!` has type `Nil`. When the target method's return
type is neither `Nil` nor `Self` (conventional for fluent/command methods) and
the cast's value is *used* (assigned, returned, or an argument), warn — the
reply is silently thrown away.

### 3. Constructors check state

```beamtalk
Counter spawnWith: #{count => 0}     // :: Counter — keys checked against
                                     //    declared `state:` slots and types
Counter spawnWith: #{cuont => 0}     // ⚠️ unknown state key `cuont` —
                                     //    did you mean `count`?
```

`spawn` / `spawnWith:` on `Meta{C}` return `Known{C}` (instance type), using
ADR 0083's metaclass-aware inference.

### 4. Proxies are transparent; DNU follows ADR 0100

`withTimeout:` returns a value typed as the *wrapped actor's class* (the
`TimeoutProxy` forwards everything; a timeout raises, so return types are
unchanged). Unknown selectors on a statically-known actor class get the same
knowledge-graded diagnostic as any send (ADR 0100) — the process boundary adds
no new severity rule.

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
- 🧑‍💻 **Newcomer, against the cast warning:** "I cast to methods with
  returns all the time — the reply is *optional*." — Accommodated: the warning
  fires only when the cast's value is **used**; bare `counter increment!` with
  a meaningful return is idiomatic and silent.

### Tension point
Whether `spawnWith:` unknown-key checking should be an error (it is a certain
runtime problem) or stay advisory. ADR 0100's ladder says: closed knowledge
(own class, declared slots) permits higher severity — proposed as `Warning`
now, revisit with strict mode.

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
- `spawnWith:` checking needs `state:` slot types in `ClassHierarchy` metadata
  (they exist for codegen; must be surfaced to the checker).
- `TimeoutProxy` transparency is a special-case inference rule (proxy class →
  wrapped class); any future proxy needs the same treatment or a general
  `forwards ::` mechanism (see north-star DNU section — this is its option 3).
- Cast-usage analysis ("is the value used?") adds a small dataflow check.

### Neutral
- No wire, runtime, or codegen change; Erlang callers see identical
  gen_servers.

## Implementation

1. **Phase 1 (~S):** ratify sync-send typing with tests (it works today —
   pin it); `!` types as `Nil`.
2. **Phase 2 (~M):** cast-discard warning (usage analysis); `spawnWith:`
   key/type checking against `state:` slots.
3. **Phase 3 (~S):** `TimeoutProxy` transparency rule; cross-process DNU
   wording unified with ADR 0100 diagnostics.

**Affected:** type checker (inference + validation), class-hierarchy metadata,
diagnostics; **not** runtime, codegen, or the wire protocol.

## References
- Seed: `docs/internal/positioning.md` (Seed 2)
- Design basis: `docs/internal/type-system-design.md` §Actor Message Passing
  (Option A); `docs/beamtalk-principles.md` 6, 7
- Related ADRs: ADR 0100 (severity), ADR 0083 (metaclass inference), ADR 0079
  (named actors), ADR 0067 (`state:`), ADR 0102 (operators)
- Prior art: Gleam `gleam_otp` `Subject`; Akka Typed; Pony behaviours
