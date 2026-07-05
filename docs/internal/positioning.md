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
| **Erlang** | "Can a system run forever?" | Dynamic + Dialyzer (post-hoc success typing) + eqWAlizer (spec-driven, IDE-integrated via ELP) | Hot reload as *ops mechanism* |
| **Elixir** | "Can BEAM be mainstream?" | **v1.20 (June 2026): gradually typed** — full set-theoretic *inference*, **zero annotations by design**; type signatures explicitly deferred pending research (v1.21+) | IEx recompile + Phoenix dev reload — module-level, state-discarding; no image |
| **Gleam** | "Can BEAM have ML soundness?" | Sound HM, closed world | VM-level reload works but is **untyped** — Gleam's own FAQ: upgrades cannot be type-checked, so reloads drop to "the usual Erlang amount of safety" |
| **Alpaca, Caramel, Hamler** | Same as Gleam, earlier | Sound-ish | None — and dormant |
| **Beamtalk** | **"Can types serve a *live* system?"** | Gradual, annotation-first, advisory (ADR 0100), set-theoretic operators (ADR 0102) | The whole point |

(Purerl — the PureScript Erlang backend — is deliberately *not* in the
graveyard row: it remains maintained and in production use at id3as, as a
niche "bring PureScript to BEAM" tool rather than a BEAM-native language.)

Two readings of that table drive the strategy:

**1. The graveyard has a lesson.** Every stalled typed-BEAM language fought the
VM's dynamism to get soundness. Gleam survived by being excellent *and* adding
a JS target — but its guarantees stop at the reload boundary: reflection and
open-world dispatch are out, and while VM-level hot reload still works, Gleam's
own FAQ concedes upgrades cannot be type-checked, so a reloaded system falls
back to "the usual Erlang amount of safety." The dynamism-shaped hole in the
typed-BEAM space is not an accident; it is the hard part everyone routed
around. Beamtalk's bet is to make that hole the product.

**2. The unclaimed quadrant is liveness × types.** Dialyzer is post-hoc.
Elixir's types are inference-only by explicit design (v1.20's milestone was
"type-check every program *without introducing annotations*"; signatures are
deferred research). Gleam's types stop at the reload boundary. Nobody has
**types as live tooling in a system that changes while it runs** — advisory
severities graded by open-world knowledge (ADR 0100), narrowing and
exhaustiveness as editor feedback (ADR 0102), conformance re-derived against
the *current* image. This is the TypeScript playbook (principle 12 states it
explicitly) — but TypeScript never had a live image to point it at. Note the
annotation contrast is *sharper* post-Elixir-1.20, not weaker: Elixir bet on
inference-without-annotations; Beamtalk bets on **annotations as
developer-facing tooling fuel**. Both are gradual; they are different
products.

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
three-way split the BEAM cares about (ADR 0067, `beamtalk-language-features.md`
§Three Class Kinds):

- `Value` — immutable plain map; `self.slot :=` is a compile error;
  copy-on-send is semantically free ⇒ **freely sendable**
- `Actor` — identity is a Pid; state lives in the process ⇒ **sendable as a
  reference**, identity preserved across the send
- `Object` — no Beamtalk-managed data, but instances may wrap *runtime-backed
  state* (ETS handles, ports, refs — e.g. `Subscription`'s `SubRef`) ⇒
  **sendability depends on the handle's scope**: many handles are node-global
  but none survive distribution to a remote node

That is a capability lattice **already present in the syntax** — users write
`Value subclass:` / `Actor subclass:` / `Object subclass:` today. Pony needed
six reference capabilities and a famously steep learning curve to track this;
Beamtalk can check "this value's meaning doesn't survive the send you're
making" at edit time using distinctions users already write down, at
near-zero annotation cost. The peers can't express the *question*: in Elixir
and Gleam all data is immutable, so there is nothing identity-bearing to
mis-send — but that also means their idioms for identity (bare Pids, ETS
handles, refs) carry **no type-level distinction at all**. Beamtalk is the
only BEAM language whose surface syntax already knows which values are data,
which are references, and which are handles — the checker just has to use it.

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
them crash later. No BEAM language can demo that today — Gleam's FAQ
explicitly concedes the gap ("it is not possible to type check the upgrades
themselves"), and Elixir's inference has no image to re-check — and it is the
demo that makes "types for a live system" legible in thirty seconds.

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
- Elixir set-theoretic types: [docs](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html) ·
  [v1.20 release — "now a gradually typed language"](https://elixir-lang.org/blog/2026/06/03/elixir-v1-20-0-released/) ·
  [type-inference milestone & 15-month roadmap](https://elixir-lang.org/blog/2026/01/09/type-inference-of-all-and-next-15/)
- Gleam: [FAQ — hot code reloading works but upgrades cannot be type-checked](https://gleam.run/frequently-asked-questions/) ·
  typed subjects in `gleam_otp`
- eqWAlizer (IDE-integrated via ELP): [github.com/WhatsApp/eqwalizer](https://github.com/WhatsApp/eqwalizer)
- Purerl (maintained, niche): [purerl.fun](https://purerl.fun/) ·
  [purescript-backend-erl](https://github.com/id3as/purescript-backend-erl)
