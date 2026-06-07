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

```text
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
│   ETS subscription table (gen_server owns writes + monitors);  │
│   dispatch runs caller-side off concurrent ETS reads.          │
│   MRO match (deduped) · async + isolated sync · pg = system    │
│   subscriber-group registry (membership, not transport)        │
├──────────────────────────────────────────────────────────────┤
│ existing: pg (membership)   ·   telemetry (measurement, 0069)  │
└──────────────────────────────────────────────────────────────┘
```

### 1. Layer 1 — runtime bus (`beamtalk_announcements`)

A runtime module providing typed publish/subscribe, slotted into
`beamtalk_runtime_sup` as a worker **after `beamtalk_bootstrap`** (which starts
`pg`) — template: `beamtalk_xref` (ADR 0087). The design follows the same
hot-path discipline as `beamtalk_xref` and `beamtalk_trace_store`: **the
gen_server is not in the dispatch path.**

- **Subscription table = ETS, gen_server owns writes only.** Subscriptions
  `{AnnouncementClass, SubscriberPid, Handler, OnceFlag}` live in an ETS table.
  `when:…` / `unsubscribe:` go through the gen_server (serialised writes, and
  it arms `erlang:monitor/2` per subscriber). The ETS table is created with
  `{heir, SupervisorPid, …}` so it **survives a bus crash** (the
  `beamtalk_trace_store` pattern); on restart the gen_server re-arms monitors
  from the table, so subscriptions are not silently lost.
- **Dispatch runs in the announcing process, off concurrent ETS reads** — not
  in the gen_server. `announce:` / `announceAndWait:` do a `read_concurrency`
  ETS lookup of matching subscribers and fan out with direct `Pid ! Msg`
  (async) or a gathered request/response (sync) **from the caller's own
  process**. This is the ADR-0087/0092 lesson: routing dispatch through a
  central gen_server would (a) self-deadlock when a handler re-announces under
  `announceAndWait:` and (b) serialise every event through one mailbox. Keeping
  dispatch caller-side eliminates both.
- **`pg`'s role is *subscriber-group registry for `SystemAnnouncer`*, not a
  message transport.** `pg` has no send primitive — you `pg:get_members/1` and
  send yourself. For the singleton system bus we register subscribers in a `pg`
  group so that *membership is cluster-wide for free*. **Delivery to any
  connected node is also free, and is in scope for v1** — `pg` membership spans
  connected nodes, `erlang:monitor/2` works cross-node, and `Pid ! Msg` is
  location-transparent, so a subscriber on another node (e.g. an ADR-0091
  LiveView IDE process on the Phoenix node, which is connected to the workspace
  node over Erlang distribution) subscribes, is monitored, and receives events
  with no extra mechanism. What is *not* free — and is deferred to the package
  (Layer 3) — is delivery across the **absence** of a connection: netsplit /
  partition tolerance, buffering and replay for a reconnecting subscriber, and
  multi-node *workspace clusters*. Single-cluster cross-node delivery to live,
  connected nodes needs none of that. Per-instance `Announcer`s use the ETS
  table directly (no `pg` group), so there is no dynamic-atom pressure; where a
  `pg` group is needed it is keyed by a `{beamtalk_announcer, Ref}` tuple, never
  a minted atom.
- **Typed dispatch with MRO matching.** On `announce: anEvent` the matcher
  walks the event's superclass chain (ETS metadata reads) and delivers to
  subscribers of the event class *or any ancestor* (subscribe to `UIEvent`,
  receive `ButtonClicked`); the walk is at announce time so live hierarchy
  changes are respected. **Delivery is de-duplicated per subscriber process**
  across the MRO walk — a process that subscribed to *both* `UIEvent` and
  `ButtonClicked` receives one delivery per *subscription* it registered
  (Pharo's rule), and the matcher never double-sends for a single ancestor
  walk. (See cost note below; this is why high-volume streaming is *excluded* —
  §5.)
- **Sync + async, fault-isolated.** `announce:` is async (direct sends, no
  wait). `announceAndWait:` gathers replies in the caller via
  `spawn_monitor`-per-handler + `receive` with a timeout, so a slow/blocked
  handler cannot wedge the bus and a crashing handler is isolated (caught,
  logged `domain => [beamtalk, announcements]`, never propagated to siblings or
  the announcer). `announceAndWait:` from inside a handler is therefore safe —
  there is no shared gen_server to deadlock on.
- **`doOnce:` is claimed atomically.** Since dispatch is caller-side and two
  `announce:` can run concurrently, a `doOnce` subscription is taken with an
  atomic `ets:take/2` (or gen_server compare-and-delete) — whichever announce
  wins delivers, the loser sees it gone, so it fires at most once. (Single-node
  guarantee; weakens under the package's distributed path, noted there.)

This reverses BT-2193's "no pg / ETS-per-announcer" choice *only* for the
shared `SystemAnnouncer` (where cluster-wide membership is worth having);
per-instance announcers keep the lean ETS model BT-2193 specced.

### 2. Layer 2 — stdlib typed veneer

Three classes in the **core image** (so the system can publish):

```beamtalk
// Base event — an immutable typed payload. Apps subclass it and add `field:`
// slots; an announcement is a *fact*, so it is a Value (never mutable). Value
// inheritance with an abstract base mirrors `abstract Value subclass: Number`.
abstract Value subclass: Announcement

// A dispatcher handle — an immutable Value wrapping a runtime subscription ref.
// NOT an Actor: there is deliberately no per-announcer process (that would
// reintroduce the central mailbox rejected in §4). The subscriber set lives in
// the runtime ETS table keyed by `ref`; dispatch runs caller-side. `class new`
// mints a fresh ref via FFI — the same pattern as `Random class new`.
Value subclass: Announcer
  field: ref :: Any = nil
  class new -> Announcer => (Erlang beamtalk_announcements) newAnnouncer
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
subscribe to. System facilities publish well-known **discrete** `Announcement`
subclasses (stdlib-provided): `ActorSpawned` / `ActorStopped`, `ClassLoaded` /
`ClassRemoved`, `BindingChanged`, and (ADR 0092) `SupervisionChildAdded` /
`SupervisionChildCrashed`. A tool subscribes once and filters by event class
instead of wiring four bespoke transports.

### 5. What does *not* belong on this bus (scope boundary)

Two of the "five consumers" are **not** discrete domain events and must stay
off `SystemAnnouncer`:

- **Transcript line streaming (ADR 0017/0054).** This is a high-volume,
  ordered byte/line stream, not a typed domain event. Routing it through the
  typed bus would add a hop, force per-line MRO matching, and make the
  announcements path a throughput bottleneck shared with discrete events.
  Transcript keeps its **own dedicated direct-push channel**
  (`beamtalk_transcript_stream` with `subscribe/1`/`unsubscribe/1`, sending
  `Pid ! {transcript_output, …}` straight from the Transcript process). It is
  a *stream*, governed by ADR 0017/0054, not this ADR.
- **`telemetry` measurement events** (spans/counters) — §4; the measurement
  bus, bridged from, not merged.

Per-consumer placement:

| Consumer (ADR) | Needs typed MRO events? | Mechanism |
|---|---|---|
| actor spawn/stop (0017/0046) | discrete, typed | `SystemAnnouncer` (`ActorSpawned`/`Stopped`) — *or* a `telemetry` attach if the consumer only needs a counter (§4) |
| class load/remove (0046) | discrete, typed | `SystemAnnouncer` (`ClassLoaded`/`Removed`) |
| bindings changed (0091) | discrete, typed | `SystemAnnouncer` (`BindingChanged`) |
| supervision deltas (0092) | discrete, typed | `SystemAnnouncer` (`SupervisionChild…`) |
| **Transcript lines (0017/0054)** | **no — a stream** | **dedicated `beamtalk_transcript_stream`, NOT this bus** |

So this substrate consolidates the **discrete** push channels; it deliberately
does not swallow the Transcript *stream*. That is the honest line, and it keeps
the bus from becoming a throughput chokepoint.

### 6. Security — subscription is a read capability (ties to ADR 0091)

Subscribing to `SystemAnnouncer` observes everything the system emits, so it is
a **read** operation under ADR 0091's role model. The remote front's
push-subscription facade (ADR 0091) is the gate: it wraps `SystemAnnouncer`
subscription as a named, RBAC-checked op (Observer role may subscribe to the
curated system events; subscribing to *all* events, or to internal/infra
announcements, is gated like ADR 0092's `system` scope). Direct
`SystemAnnouncer current` access from in-image Beamtalk code is unrestricted —
but that already implies an `eval`-capable (Owner) session, which ADR 0091
treats as fully privileged, so no boundary is bypassed. The ADR-0091
implementation issue owns adding `subscribe`/`unsubscribe` to the authoritative
op list; this ADR fixes the requirement.

### 3. Layer 3 — the package (re-scoped BT-2193)

`beamtalk-announcements` shrinks to the genuinely-optional:

- **Hardened distributed delivery** beyond live connected nodes: netsplit /
  partition tolerance, buffering + replay for reconnecting subscribers, and
  multi-node *workspace clusters*. (Plain cross-node delivery to a *connected*
  node — including the ADR-0091 LiveView front — is already handled by the core
  bus over Erlang distribution; see Layer 1.)
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
...>   field: newPrice :: Money = nil
PriceChanged

bt> a := Announcer new
an Announcer

bt> a when: PriceChanged do: [:e | Transcript showLine: "now " ++ e newPrice printString]
#Subscription<...>

bt> a announce: (PriceChanged newPrice: 42)
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
  they already use; `Announcer new` is an ordinary value constructor. They can watch
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
- **Tooling developer.** One typed feed replaces the discrete bespoke
  transports (Transcript streaming stays its own channel, §5);
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
- ⚙️ **BEAM veteran:** "Built on `pg`, no new bottleneck; connected-node delivery is free now (the LiveView front is just another node on the dist mesh), and the heavy partition/replay parts stay out of the kernel."
- 🏭 **Operator:** "Monitored, isolated, inspectable; ops bridge is opt-in."
- 🎨 **Language designer:** "Each concern at the right altitude; the package line falls exactly on what's optional."
- *Tension:* three layers is more moving parts than 'one package' — justified only because the system-publisher requirement is real (five consumers).

### Substrate fork

- **ETS subscription table + caller-side dispatch (chosen):** gen_server owns
  writes + monitors + ETS heir (crash-survivable); dispatch runs in the
  announcing process off `read_concurrency` ETS, so there is no central
  dispatch mailbox to deadlock (`announceAndWait:` reentrancy) or bottleneck.
  This is the `beamtalk_xref` / `beamtalk_trace_store` pattern.
- **`pg` for `SystemAnnouncer` membership:** gives cluster-wide *membership* for
  the shared bus at no extra cost — `pg` has no send primitive, so it is a
  registry, not a transport, but combined with location-transparent `Pid ! Msg`
  and cross-node monitors it delivers to any *connected* node in v1 (the
  ADR-0091 LiveView front gets live updates for free over the existing dist
  link). Only partition tolerance / replay / multi-workspace-cluster fall to the
  package. Per-instance announcers don't use `pg`.
- **Central dispatch gen_server (rejected):** the obvious "announce → call the
  bus → it sends to everyone" design self-deadlocks on reentrant
  `announceAndWait:` and serialises all events through one mailbox.
- **`gen_event` (rejected):** serialised single-process dispatch, shared
  failure domain.
- **`telemetry` as the substrate (rejected):** untyped, no liveness/MRO, and it
  would overload ADR 0069's measurement bus with reactive domain logic.

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
- One typed substrate replaces the **discrete** bespoke push channels before
  they are built (Transcript streaming stays on its own channel — §5).
- System facilities and app code share one mechanism (`SystemAnnouncer` is the
  Pharo-correct home).
- Dispatch off the hot path (caller-side, concurrent ETS) → no central mailbox
  to deadlock or bottleneck; crash-survivable via ETS heir; monitored + isolated
  by construction; hot-reload-safe (process-rooted subs). Clean `telemetry`
  boundary preserved (§4).
- `pg` membership + location-transparent send + cross-node monitors give
  `SystemAnnouncer` free delivery to any *connected* node in v1 — the ADR-0091
  LiveView front (a separate node over Erlang distribution) gets live updates
  with no package. Only partition tolerance / replay / multi-workspace-cluster
  fall to the package.

### Negative
- Three layers to build and document vs BT-2193's single package.
- Re-scopes BT-2193 and requires follow-up edits to four ADRs (0046/0054/0091/
  0092) to point at the shared substrate, plus confirming Transcript stays on
  ADR 0017/0054's stream.
- Two buses coexist (`telemetry` + Announcements); the "measure vs react" line
  must be taught or people will reach for the wrong one. (Mitigated by §4 + the
  package bridge.)
- **No cross-event ordering guarantee** across different announcing processes
  (each fans out from its own process); within a single `announce:` order is
  per-subscription. Consumers needing causal order (e.g. `ActorSpawned` before a
  dependent event) must not assume it — documented, not engineered, in v1.
- MRO matching costs an ETS-metadata walk per announce (~ depth × ~150 ns + fan-
  out send cost). Fine for discrete events at human/IDE rates; **not** fine for
  line-rate streams — which is precisely why Transcript is excluded (§5).

### Neutral
- Adds a runtime worker (`beamtalk_announcements`) under `beamtalk_runtime_sup`
  (after `beamtalk_bootstrap`) and 3–4 stdlib classes + a handful of system
  `Announcement` subclasses.
- `pg` gains a second use (class-registry membership *and* `SystemAnnouncer`
  subscriber membership); both are ordinary `pg` group usage.
- `announceAndWait:` is `spawn_monitor`-per-handler — N transient processes per
  sync announce; acceptable for the low-frequency sync path, not the async one.

## Implementation

Downstream of acceptance (the `/plan-adr` step owns the epic + the BT-2193
re-scope):

0. **Phase 0 (napkin):** `beamtalk_announcements` with one `Announcement`
   class, `Announcer new`, `when:do:`, `announce:` (async) over a single `pg`
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
  ADR 0092 (Supervision Introspection — consumer; §8 updated to point here), ADR 0017
  (browser), ADR 0046 (VSCode sidebar), ADR 0054 (protocols), ADR 0091 (remote
  front), ADR 0070/0073 (package namespaces/distribution)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_runtime_sup.erl`,
  `beamtalk_xref.erl` (gen_server template), `beamtalk_object_class.erl` /
  `beamtalk_class_registry.erl` (existing `pg` membership use),
  `beamtalk_bootstrap.erl` (`pg:start_link`)
- External: Pharo Announcements + `SystemAnnouncer`; Phoenix.PubSub; Erlang
  `gen_event`; Erlang `telemetry`; GemStone Announcements
