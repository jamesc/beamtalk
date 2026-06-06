# ADR 0093: Announcements — Typed Event Substrate (Runtime Bus + stdlib Veneer)

## Status
Proposed (2026-06-06)

## Context

### The problem

Beamtalk has no first-class way for one part of the system to say "X happened"
and another part to react. Application code wants a typed Observer
(`when: SomethingChanged do: [...]`). And the *system itself* increasingly
wants to publish: a LiveView IDE pane needs Transcript lines, actor
spawn/stop, class loads, binding changes, and supervision-tree deltas pushed
to it as they happen.

This ADR decides **what the pub/sub mechanism is, and — critically — where it
lives** (runtime / stdlib / package), so the rest of the system has one
substrate to build on instead of many.

### Why now: five ADRs are each about to reinvent this

Investigating ADR 0092 (supervision-tree introspection) surfaced that **five
separate accepted/proposed ADRs each independently plan their own bespoke
"subscribe / broadcast" channel** — and none is built yet:

| ADR | Bespoke notification it plans |
|---|---|
| 0017 (browser connectivity) | `beamtalk_transcript_stream:subscribe/unsubscribe`; "actor-spawned/stopped … *requires new registry notification infrastructure*" |
| 0046 (VSCode sidebar) | new `class-loaded` push event + `actors` spawned/stopped channel |
| 0054 (communication protocols) | WebSocket push for Transcript + actor lifecycle; future `{future_resolved}` notifications |
| 0091 (remote workspace front) | push-subscription facade for transcript / actors / classes / bindings |
| 0092 (supervision introspection) | supervision-tree deltas (deferred to "the Announcements package") |

That is five teams about to build five slightly-different subscribe/broadcast
mechanisms. Deciding the substrate now **prevents** the fragmentation rather
than forcing a later consolidation.

### What already exists (and what it is *not*)

The runtime already has two event-ish mechanisms — neither is a typed Observer:

1. **`pg` process groups** — started at boot (`pg:start_link/0` in
   `beamtalk_bootstrap`) and used today as a **membership registry**:
   `beamtalk_object_class` joins the `beamtalk_classes` group so
   `beamtalk_class_registry` can enumerate live class processes. `pg` is
   untyped, stateless broadcast — no event types, no per-subscriber liveness
   beyond group membership, no subclass matching.
2. **`telemetry`** — adopted by **ADR 0069** as the *measurement* bus
   (`telemetry:span/3` around actor dispatch, lock-free counters, ETS trace
   store). It is deliberately fire-and-forget, untyped (string-list event
   names), with handlers run synchronously in the caller process. It is the
   right tool for spans/metrics and the wrong tool for "subscribe to a typed
   domain event and react in app logic."

So Beamtalk has a **membership primitive** and a **measurement bus** — but no
**typed domain-event** layer. That gap is what every one of the five ADRs is
working around.

### Prior history of this decision

- **BT-567** originally specced an Announcements framework *in stdlib*.
- **BT-2193** re-scoped it to a standalone **package** (`beamtalk-announcements`),
  after ADRs 0070/0073 made packages first-class — on the reasoning that it is
  "self-contained, optional, decoupled from runtime bootstrap."
- **BT-2396** (this ADR) was filed to formalise the design before the package
  is built.

The package framing assumed Announcements is a *leaf* capability that apps opt
into. The five-consumer finding challenges that: if the **system** publishes
through it, it cannot live in an optional package, because **stdlib and the
runtime cannot depend on a package** (CLAUDE.md / architecture: "dependencies
flow down only"). This is exactly why Pharo keeps Announcements (and
`SystemAnnouncer`) in the **core image**, not a loadable package.

### Constraints

1. **Dependency direction is the hard constraint.** stdlib/runtime → may be
   depended on; a package → depends on them, never the reverse. Any system
   publisher forces the typed core *below* the package line.
2. **Do not add a third ad-hoc mechanism.** We already have `pg` (membership)
   and `telemetry` (measurement). A typed Observer must either reuse one or
   justify why neither fits — and must not blur the line with `telemetry`'s
   measurement role (ADR 0069).
3. **BEAM-native liveness.** Subscribers are processes that can die. Cleanup
   must be `erlang:monitor/2`-driven (the BEAM analogue of Pharo's weak
   subscriptions), not manual.
4. **Fault isolation.** A crashing subscriber handler must not take down the
   publisher or other subscribers (let-it-crash, Principle 10).
5. **Hot-code-reload safe.** Subscriptions are process-rooted, not
   module-rooted, so they survive method/class reload.
6. **Structured errors & logging** (CLAUDE.md): `#beamtalk_error{}`, `?LOG_*`
   with `domain => [beamtalk, announcements]`.

## Decision

**Adopt a three-layer design: a runtime event bus, a stdlib typed-Observer
veneer (the part that was going to be a package), and a thin package for the
genuinely-optional extras.** Draw a firm line between this typed-domain-event
substrate and `telemetry`'s measurement role.

```
┌──────────────────────────────────────────────────────────────┐
│ package: beamtalk-announcements (OPTIONAL)                     │
│   distributed cross-node delivery · RecordingAnnouncer ·       │
│   telemetry/OTel bridge                                        │
├──────────────────────────────────────────────────────────────┤
│ stdlib (CORE IMAGE)                                            │
│   Announcement (base event) · Announcer (per-instance) ·       │
│   SystemAnnouncer (singleton) · when:do: / announce: / …       │
├──────────────────────────────────────────────────────────────┤
│ runtime (Erlang)  beamtalk_announcements                       │
│   typed dispatch over pg topic-groups + monitor-based liveness │
│   MRO match · sync(call) + async(cast) · fault isolation       │
├──────────────────────────────────────────────────────────────┤
│ existing: pg (membership)   ·   telemetry (measurement, 0069)  │
└──────────────────────────────────────────────────────────────┘
```

### 1. Layer 1 — runtime bus (`beamtalk_announcements`)

A runtime module providing typed publish/subscribe, slotted into
`beamtalk_runtime_sup` as a worker (template: `beamtalk_xref`, ADR 0087).
**It reuses `pg` as the transport** — one `pg` group per *announcer identity*
— and layers the typed concerns on top that `pg` lacks:

- **Typed dispatch with MRO matching.** Subscriptions are keyed by
  `Announcement` class. On `announce: anEvent`, the bus walks the event's
  superclass chain and delivers to subscribers of the event class *or any
  ancestor* (subscribe to `UIEvent`, receive `ButtonClicked`). The MRO walk
  happens at announce time, so live class-hierarchy changes are respected.
- **Liveness.** `erlang:monitor/2` on each subscriber; a `DOWN` removes its
  subscriptions (the `pg` group membership *is* the liveness for the broadcast
  fan-out; monitors handle the typed-subscription table).
- **Sync + async.** `announce:` → `cast` (fire-and-forget); `announceAndWait:`
  → `call` (blocks until every subscriber's handler returns).
- **Fault isolation.** Each handler runs guarded; a crash is caught, logged
  (`domain => [beamtalk, announcements]`), and does not affect siblings or the
  publisher.

Why `pg` rather than BT-2193's ETS-per-announcer or a brand-new bus: `pg` is
already started, already the project's broadcast primitive, cluster-ready
(the v2 distributed path is then *free* — same code, `pg` spans nodes on OTP
23+), and avoids standing up bespoke ETS ownership/heir machinery. The typed
table (class → subscribers, monitors) is small per-announcer state the module
owns; `pg` carries the fan-out. (BT-2193's "no pg" note predates this
consolidation goal — it optimised a single package in isolation.)

### 2. Layer 2 — stdlib typed veneer

Three classes in the **core image** (so the system can publish):

```beamtalk
// Base event type — apps subclass it; carries typed state.
abstract Object subclass: Announcement

// A dispatcher instance. Apps spawn their own; the system has a singleton.
Object subclass: Announcer
  class spawn -> Announcer => (Erlang beamtalk_announcements) newAnnouncer
  when: aClass :: Class do: aBlock :: Block -> Subscription => ...
  when: aClass :: Class send: sel :: Symbol to: receiver -> Subscription => ...
  when: aClass :: Class doOnce: aBlock :: Block -> Subscription => ...
  announce: anEvent :: Announcement -> Nil => ...           // async (cast)
  announceAndWait: anEvent :: Announcement -> Nil => ...     // sync (call)
  unsubscribe: receiver -> Nil => ...

// The one the *system* publishes through — Pharo's SystemAnnouncer.
sealed Announcer subclass: SystemAnnouncer
  class current -> SystemAnnouncer => (Erlang beamtalk_announcements) system
```

`SystemAnnouncer current` is the shared bus the runtime emits onto and tools
subscribe to. System facilities publish well-known `Announcement` subclasses
(stdlib-provided): `TranscriptChanged`, `ActorSpawned` / `ActorStopped`,
`ClassLoaded` / `ClassRemoved`, `BindingChanged`, and (ADR 0092)
`SupervisionChildAdded` / `SupervisionChildCrashed`. These are the **five
consuming ADRs' channels, unified** — a tool subscribes once and filters by
event class instead of wiring five bespoke transports.

### 3. Layer 3 — the package (re-scoped BT-2193)

`beamtalk-announcements` shrinks to the genuinely-optional:

- **Distributed cross-node delivery** beyond what single-node `pg` gives.
- **`RecordingAnnouncer`** — the test double that records `announce:` calls
  without dispatching.
- **`telemetry` / OpenTelemetry bridge** — re-emit announcements as telemetry
  spans for ops pipelines (the bridge direction ADR 0069 anticipated).

### 4. The line vs `telemetry` (ADR 0069)

| | `telemetry` (ADR 0069) | Announcements (this ADR) |
|---|---|---|
| Purpose | **Measurement** — spans, counters, durations | **Typed domain events** you react to |
| Event identity | string list `[beamtalk, actor, dispatch]` | `Announcement` subclass (typed, MRO) |
| Delivery | sync, in caller process, fire-and-forget | async or sync; isolated; monitored |
| Subscriber | metrics handler (export to StatsD/OTel) | app logic / IDE pane reacting |
| Liveness | none (handlers are module funs) | `monitor`-based per subscriber |

Rule of thumb: *measure* with `telemetry`, *react* with Announcements. Actor
spawn/stop can legitimately produce **both** (a telemetry counter for ops, a
`SystemAnnouncer` event for the IDE) — the package's bridge is how one becomes
the other, without coupling the two buses.

### REPL session

```beamtalk
bt> Announcement subclass: PriceChanged
...>   state: newPrice :: Money = nil
PriceChanged

bt> a := Announcer spawn
#Announcer<<0.421.0>>

bt> a when: PriceChanged do: [:e | Transcript showLine: "now " ++ e newPrice printString]
#Subscription<...>

bt> a announce: (PriceChanged new: #{ #newPrice => 42 })
now 42

bt> "Subscribe to the system bus — one feed for everything the system emits:"
bt> SystemAnnouncer current when: ActorSpawned do: [:e | Transcript showLine: e actorClass name]
#Subscription<...>

bt> Counter spawn
Counter        "← the subscription above fires"
```

### Error / edge behaviour

```beamtalk
bt> a when: 42 do: [:e | e]
// type error: when:do: expects a Class, got Integer (42)

bt> a when: PriceChanged do: [:e | e boom]   // handler crashes on announce
bt> a announce: (PriceChanged new: #{...})
// other subscribers still run; the crash is logged
// (domain => [beamtalk, announcements]), not propagated to the announcer

bt> "dead subscriber process is auto-removed via monitor — no manual cleanup"
```

## Prior Art

| Source | Model | What we take / leave |
|---|---|---|
| **Pharo Announcements + `SystemAnnouncer`** | Typed `Announcement` subclasses, `when:do:` / `when:send:to:`, subclass dispatch; `SystemAnnouncer uniqueInstance` is **core image**, the system publishes class/method change events through it. | **Take:** the whole typed-Observer protocol *and the it-lives-in-the-core decision*. This ADR's central move is recognising Beamtalk is in the same position. **Adapt:** weak refs → BEAM `monitor`. |
| **GemStone Announcements** | Same family, server-side. | Confirms the model scales to a shared multi-client image — our exact LiveView situation. |
| **Phoenix.PubSub** | topic-string + cluster broadcast over `pg2`/`pg`. | **Take:** `pg` as the cluster-ready transport (free v2 distribution). **Leave:** topic *strings* — we want typed events + MRO, not stringly-typed topics. |
| **Erlang `gen_event`** | Serialised handler dispatch in one process. | **Leave:** single-process serialisation is a bottleneck and a shared failure domain; `pg` fan-out + isolated handlers is more BEAM-idiomatic. |
| **Erlang `telemetry`** (ADR 0069) | Untyped measurement bus. | **Leave** as the measurement layer; **bridge** to it from the package. Explicitly *not* the typed Observer. |
| **ROS typed topics / Akka EventStream** | Typed publish/subscribe by message class. | Confirms type-keyed (not string-keyed) subscription is the ergonomic choice for domain events. |

## User Impact

- **Newcomer.** `when: X do: [...]` reads like every other block-taking message
  they already use; `Announcer spawn` mirrors `Counter spawn`. They can watch
  the system live (`SystemAnnouncer current when: ActorSpawned do: …`) — a
  tangible way to *see* what the runtime is doing.
- **Smalltalk developer.** It *is* Pharo Announcements, including
  `SystemAnnouncer` in the image — instant familiarity, and the right answer to
  "where's the system's announcer?"
- **Erlang/BEAM developer.** It's `pg` + `monitor` underneath — no magic, no
  new bottleneck process, cluster-ready. They can also attach in Erlang. The
  `telemetry` boundary means their metrics pipeline is untouched.
- **Operator.** Subscriptions are monitored (no leak), handlers are isolated
  (no cascade), and the bus is `pg` (inspectable with standard tools). The
  package bridge exports to existing observability without coupling.
- **Tooling developer.** One typed feed replaces five bespoke transports;
  filtering by `Announcement` subclass is a clean, evolvable contract for the
  LiveView/VSCode panes (ADRs 0017/0046/0085/0091).

## Steelman Analysis

### Location fork — the core decision

**(a) Pure package (status-quo BT-2193).**
- 🎨 **Language designer:** "Smallest core; pub/sub is a library, like `http`/`json`. Apps that don't need it don't pay."
- ⚙️ **BEAM veteran:** "Optional deps are the OTP way; don't bloat the kernel."
- *Fatal tension:* the **system** can't publish through a package (dependency direction). Every one of the five consumers lives in runtime/stdlib, so (a) forces them to keep their bespoke channels — it doesn't solve the actual problem. Only viable if we commit that the system *never* announces, which contradicts ADRs 0017/0046/0091/0092.

**(b) All in stdlib.**
- 🎩 **Smalltalk purist:** "Pharo puts it in the image. Done. One mechanism, always present, `SystemAnnouncer` included."
- 🧑‍💻 **Newcomer:** "It's just there in the REPL, no dependency to add."
- *Tension:* bundles genuinely-optional weight (cross-node distribution, telemetry bridge, test doubles) into the core that most programs never use.

**(c) Layered — runtime bus + stdlib veneer + package extras (chosen).**
- 🎩 **Smalltalk purist:** "Typed Observer + `SystemAnnouncer` are in the image where they belong."
- ⚙️ **BEAM veteran:** "Built on `pg`, no new bottleneck, distribution is free later; the heavy/optional parts stay out of the kernel."
- 🏭 **Operator:** "Monitored, isolated, inspectable; ops bridge is opt-in."
- 🎨 **Language designer:** "Each concern at the right altitude; the package line falls exactly on what's optional."
- *Tension:* three layers is more moving parts than 'one package' — justified only because the system-publisher requirement is real (five consumers).

### Substrate fork

- **`pg` (chosen):** already started, already our broadcast primitive, cluster-ready, no new ownership machinery. *Con:* typed/MRO/liveness must be layered on top (we do).
- **ETS-per-announcer + monitors (BT-2193's original):** fine for isolated app announcers; but doesn't give cluster distribution for free and re-introduces bespoke ETS heir/ownership handling the runtime already solved with `pg`.
- **`gen_event`:** rejected — serialised single-process dispatch, shared failure domain.
- **`telemetry` as the substrate:** rejected — untyped, no liveness/MRO, and it would overload ADR 0069's measurement bus with reactive domain logic.

### Tension points
- Designers split (a)↔(b) on "lean core"; the dependency-direction fact breaks the tie toward (b)/(c).
- BEAM veterans like `pg`; the original package author chose ETS to keep the package self-contained — consolidation flips that trade.

## Alternatives Considered

Covered in the steelman: **(a) pure package**, **(b) all-stdlib**, **(c) layered**
(chosen); substrate alternatives **ETS-per-announcer**, **gen_event**,
**telemetry-as-substrate**. Also considered and rejected: **"do nothing, let
each ADR build its own channel"** — explicitly the fragmentation this ADR
exists to stop.

## Consequences

### Positive
- One typed substrate replaces five bespoke push channels before they are built.
- System facilities and app code share one mechanism (`SystemAnnouncer` is the
  Pharo-correct home).
- Free single-node→cluster path via `pg`; clean `telemetry` boundary preserved.
- Monitored + isolated by construction; hot-reload-safe (process-rooted subs).

### Negative
- Three layers to build and document vs BT-2193's single package.
- Re-scopes BT-2193 and requires follow-up edits to five ADRs (0017/0046/0054/
  0091/0092) to point at the shared substrate.
- Two buses coexist (`telemetry` + Announcements); the "measure vs react" line
  must be taught or people will reach for the wrong one. (Mitigated by the
  package bridge for the overlap cases.)

### Neutral
- Adds a runtime worker (`beamtalk_announcements`) under `beamtalk_runtime_sup`
  and 3–4 stdlib classes + a handful of system `Announcement` subclasses.
- `pg` gains a second role (membership *and* announcement transport); both are
  standard `pg` usage.

## Implementation

Downstream of acceptance (the `/plan-adr` step owns the epic + the BT-2193
re-scope):

0. **Phase 0 (napkin):** `beamtalk_announcements` with one `Announcement`
   class, `Announcer spawn`, `when:do:`, `announce:` (async) over a single `pg`
   group + monitor cleanup. Prove typed dispatch end-to-end from the REPL.
1. **Runtime bus (full):** MRO match, `announceAndWait:` (sync), `when:send:to:`,
   `when:doOnce:`, fault isolation, sup wiring. EUnit.
2. **stdlib veneer:** `Announcement` / `Announcer` / `SystemAnnouncer` +
   registration (`generated_builtins.rs`, `build_stdlib.rs`); typed signatures.
   BUnit.
3. **System events + consumer consolidation:** define the system `Announcement`
   subclasses; migrate the runtime emit points; update ADRs 0017/0046/0054/0091/
   0092 to subscribe via `SystemAnnouncer` (and ADR 0092 §8 to point here).
4. **Package re-scope (BT-2193):** distributed delivery, `RecordingAnnouncer`,
   telemetry bridge; cross-repo CI.
5. **Docs + e2e:** language-features chapter, surface-parity, e2e btscript;
   flip this ADR to Implemented.

Affected components: **runtime** (new worker), **stdlib** (veneer + system
events), **package** (re-scoped), **docs/tests**. No parser/codegen changes.

## Migration Path

- **BT-2193** is re-scoped from "whole framework as a package" to "optional
  extras as a package," with a new stdlib/runtime epic for Layers 1–2.
- The five consuming ADRs migrate their planned bespoke channels to
  `SystemAnnouncer` subscriptions as part of Phase 3; until then nothing
  regresses (none of those channels is built yet).

## References
- Related issues: BT-2396 (this ADR), BT-2193 (package — to be re-scoped),
  BT-567 (original stdlib spec, superseded), BT-1242 (`class_removed` via pg)
- Related ADRs: ADR 0069 (Tracing / `telemetry` measurement bus — the boundary),
  ADR 0092 (Supervision Introspection — consumer; §8 points here), ADR 0017
  (browser), ADR 0046 (VSCode sidebar), ADR 0054 (protocols), ADR 0091 (remote
  front), ADR 0070/0073 (package namespaces/distribution)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_sup.erl`,
  `beamtalk_xref.erl` (gen_server template), `beamtalk_object_class.erl` /
  `beamtalk_class_registry.erl` (existing `pg` membership use),
  `beamtalk_bootstrap.erl` (`pg:start_link`)
- External: Pharo Announcements + `SystemAnnouncer`; Phoenix.PubSub; Erlang
  `gen_event`; Erlang `telemetry`; GemStone Announcements
