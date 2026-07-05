# Beamtalk in the BEAM Ecosystem — Positioning

**Status:** Working position. Complements [beamtalk-principles.md](../beamtalk-principles.md)
(which states what Beamtalk *is*) by stating what Beamtalk is *relative to its
peers*, with emphasis on the type system's role.
**Related:** [set-theoretic-types-north-star.md](set-theoretic-types-north-star.md) ·
[ADR 0100](../ADR/0100-open-world-diagnostic-policy.md) ·
[ADR 0102](../ADR/0102-set-theoretic-type-operators.md)

---

## The map, typing edition

Every BEAM language answers a different question:

| Language | The question it answers | Types | Liveness |
|---|---|---|---|
| **Erlang** | "Can a system run forever?" | Dynamic + Dialyzer/eqWAlizer (post-hoc, ops-grade) | Hot reload as *ops mechanism* |
| **Elixir** | "Can BEAM be mainstream?" | Set-theoretic **inference** rolling in (1.17+); annotation culture still forming | IEx + recompile; reload is not a dev UX |
| **Gleam** | "Can BEAM have ML soundness?" | Sound HM, closed world | **None** — no reflection, no DNU, no live redefinition; soundness bought by giving liveness up |
| **Alpaca, Caramel, Hamler, Purerl** | Same as Gleam, earlier | Sound-ish | None — and mostly dead |
| **Beamtalk** | **"Can types serve a *live* system?"** | Gradual, annotation-first, advisory (ADR 0100), set-theoretic operators (ADR 0102) | The whole point |

Two readings of that table drive the strategy:

**1. The graveyard has a lesson.** Every stalled typed-BEAM language fought the
VM's dynamism to get soundness. Gleam survived by being excellent *and* adding
a JS target — but it still had to amputate hot reload, reflection, and
open-world dispatch to keep its guarantees. The dynamism-shaped hole in the
typed-BEAM space is not an accident; it is the hard part everyone routed
around. Beamtalk's bet is to make that hole the product.

**2. The unclaimed quadrant is liveness × types.** Dialyzer is post-hoc.
Elixir's types live compiler-side. Gleam's types cannot coexist with a mutating
image. Nobody has **types as live tooling in a system that changes while it
runs** — advisory severities graded by open-world knowledge (ADR 0100),
narrowing and exhaustiveness as editor feedback (ADR 0102), conformance
re-derived against the *current* image. This is the TypeScript playbook
(principle 12 states it explicitly) — but TypeScript never had a live image to
point it at.

**The one-sentence placement:**

> Elixir types *existing* code; Gleam types *closed* code; **Beamtalk types
> *running* code.**

Beamtalk is not competing with Elixir for "mainstream BEAM" or with Gleam for
"sound BEAM." It occupies the corner both structurally cannot: the live one.
The type system's job description follows: advisory not gatekeeping,
open-world honest, tooling-fuel first, reload-surviving.

## Three type-system opportunities this opens (ADR seeds)

These are differentiators available to Beamtalk *specifically because* of
decisions already shipped. Each should graduate to its own ADR/Linear issue
when timely.

### Seed 1 — Sendability typing from class kinds

The cheapest big win nobody on BEAM has. Beamtalk already declares the
three-way split the BEAM cares about:

- `Value` — immutable; copy-on-send is semantically free ⇒ **sendable**
- `Actor` — a shareable *reference* (Pid) ⇒ **sendable as a handle**
- stateful, identity-bearing `Object` — meaningful only within its process ⇒
  **process-local**

That is a capability system **already present in the syntax** (ADR 0067's
class-kind split). Pony needed six reference capabilities and a famously steep
learning curve to track this; Beamtalk can check "you're sending a
process-local mutable object to an actor" at edit time using distinctions
users already write down, at near-zero annotation cost. Gleam cannot offer
this (all-immutable, the question dissolves); Elixir silently deep-copies and
lets identity bugs happen.

### Seed 2 — Typed actor protocols without a parallel messaging layer

Gleam's typed actors required inventing `Subject(msg)` — a non-OTP-standard
actor layer — because bare functions-over-messages have no interface to type.
Beamtalk's "actor = class, message = selector" means **the actor's class
interface already is its protocol type**: typed actor messaging falls out of
ordinary method checking (Option A in
[type-system-design.md](type-system-design.md)). Remaining design work is the
async edges: what `cast` returns (principle 7's sync-by-default makes the sync
path trivial) and what DNU means across a process boundary (see the DNU
section of the north-star doc). Small ADR, distinctive capability.

### Seed 3 — Types × hot reload: the killer demo

The unsolved question in the whole ecosystem: what happens to "checked" when a
module is redefined live? ADR 0100's knowledge-graded severity is the
foundation; the missing piece is **incremental re-checking of the image on
reload** — change a method's signature in a running system and the workspace
immediately flags the now-stale callers as live diagnostics instead of letting
them crash later. No BEAM language can demo that today, and it is the demo
that makes "types for a live system" legible in thirty seconds.

## The orthogonal axis: agent-native development

Beamtalk treats **MCP as a first-class surface** with parity requirements
alongside CLI/REPL/LSP ([surface-parity.md](../development/surface-parity.md)).
None of the peer languages were designed with agent-driven development as a
surface. Combined with liveness — an agent that can *query and mutate a
running image* through a structured protocol — this is a positioning axis
orthogonal to the type story, and it may age into the more important one:
the live image is as good an environment for an AI pair as it is for a human.

## What this positioning rules out

Consistency cuts both ways. Claiming the live corner means *not* chasing:

- **Soundness competitions with Gleam.** Beamtalk's types are advisory by
  design (ADR 0100); a soundness pitch would be both false and off-brand.
- **Elixir's ecosystem breadth.** Interop (principle 9, ADR 0101) is the
  answer to "where are the libraries?" — not a parallel ecosystem.
- **Types as a compilation gate.** Any future "strict mode" must remain
  opt-in; the default posture is TypeScript-style tooling, not OCaml-style
  gatekeeping.

## References

- [beamtalk-principles.md](../beamtalk-principles.md) — especially 1
  (Interactive-First), 2 (Hot Reload is Core), 7 (Sync-by-Default Actors),
  12 (Compiler is the Language Service)
- [set-theoretic-types-north-star.md](set-theoretic-types-north-star.md) —
  the type-system destination and the nominal/structural recommendation
- [ADR 0100](../ADR/0100-open-world-diagnostic-policy.md) — the advisory,
  knowledge-graded diagnostic posture
- [ADR 0102](../ADR/0102-set-theoretic-type-operators.md) — atoms, unions,
  narrowing, exhaustiveness
- Elixir set-theoretic types: [docs](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html);
  Gleam: [gleam.run](https://gleam.run) (typed subjects in `gleam_otp`);
  eqWAlizer: [github.com/WhatsApp/eqwalizer](https://github.com/WhatsApp/eqwalizer)
