# ADR 0093: Announcements — Typed Event Substrate (Runtime Bus + stdlib Veneer)

## Status
Implemented (2026-06-08)

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
│   AnnouncementNavigation + SubscriptionNode (introspection)    │
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

- **Subscription table = ETS, gen_server owns writes only.** Each subscription
  is a row `{SubRef, AnnouncerRef, AnnouncementClass, SubscriberPid, Handler,
  OnceFlag}` in an ETS **`set` keyed by a unique `SubRef`** (the ref the returned
  `Subscription` wraps), with a secondary by-class index (`ordered_set` on
  `{AnnouncerRef, AnnouncementClass, SubRef}`) for the MRO lookup. The
  `AnnouncerRef` dimension scopes each subscription to **one announcer
  namespace** so distinct `Announcer` instances are isolated (**Amendment
  BT-2454**, see below): a `reference()` per `Announcer new`, or the well-known
  `beamtalk_system_announcer` atom shared by `SystemAnnouncer` and the raw Layer-1
  API. Keying by `SubRef` — *not* by `{Class, Pid}` — means a process may hold
  **multiple distinct subscriptions to the same class** (Pharo's rule, and what
  the per-*subscription* de-dup below assumes): a second `when: C do: […]` adds a
  row, it never silently replaces the first. `doOnce:`'s `ets:take(SubRef)` stays atomic and selective on the unique
  key. `when:…` / `unsubscribe:` go through the gen_server (serialised writes,
  and it arms `erlang:monitor/2` per subscriber). The ETS table is created with
  `{heir, SupervisorPid, …}` so it **survives a bus crash** (the
  `beamtalk_trace_store` pattern). On restart the gen_server re-arms monitors
  from the table — and because a subscriber may have **died during the
  crash→restart gap** (its `DOWN` went to the dead bus and is lost), re-arm does
  an `is_process_alive/1` check per row and prunes the dead ones, so the table
  cannot accumulate stale pids. (New exposure vs `beamtalk_trace_store`, which
  monitors nothing.)
- **Dispatch runs in the announcing process, off concurrent ETS reads** — not
  in the gen_server. `announce:` / `announceAndWait:` do a `read_concurrency`
  ETS lookup of matching subscribers and fan out with direct `Pid ! Msg`
  (async) or a gathered request/response (sync) **from the caller's own
  process**. This is the ADR-0087/0092 lesson: routing dispatch through a
  central gen_server would (a) self-deadlock when a handler re-announces under
  `announceAndWait:` and (b) serialise every event through one mailbox. Keeping
  dispatch caller-side eliminates both.
- **`pg`'s role is *subscriber-group registry for `SystemAnnouncer`*, not a
  message transport.** `pg` has no send primitive — you `pg:get_members/2` and
  send yourself. Use a **dedicated Beamtalk scope** (`pg:start_link(beamtalk_pg)`),
  not the default `pg` scope — both to keep announcer groups separate from the
  existing `beamtalk_classes` membership use, and to avoid colliding with any
  `pg` use on a connected non-Beamtalk node. **(Amendment BT-2530 — what was
  actually built:)** the implementation starts the `beamtalk_pg` scope but
  subscribers are **never joined to pg groups** — subscription membership is the
  workspace-local ETS table (`beamtalk_announcement_subs` + its by-class index),
  so the original "membership is cluster-wide for free" claim does not describe
  the shipped design. Cross-node delivery in v1 is instead via **explicit-pid
  registration**: a dist-attached consumer (e.g. the ADR-0091 front) subscribes
  its own remote pid through the normal subscribe path on the workspace node,
  and from there `Pid ! Msg` is location-transparent and `erlang:monitor/2`
  works cross-node. The original claim that this "works for free" also missed
  that all delivery paths guarded sends with `erlang:is_process_alive/1`, which
  is **local-only** (`badarg` on a remote pid) — fixed in BT-2530 by
  discriminating on `node(Pid) =:= node()`: local pids keep the fast-path
  liveness skip; remote pids are always sent to, with the cross-node monitor
  handling cleanup (up to ~`net_ticktime` lag for an undetected node loss).
  What is genuinely *out of scope* and deferred to the package (Layer 3) is
  delivery across the **absence** of a connection: netsplit / partition
  tolerance, buffering and replay for a reconnecting subscriber, and multi-node
  *workspace clusters*. Per-instance `Announcer`s use the ETS table directly,
  so there is no dynamic-atom pressure; if a `pg` group is ever needed it is
  keyed by a `{beamtalk_announcer, Ref}` tuple, never a minted atom.
- **Typed dispatch with MRO matching, scoped per announcer.** On
  `anAnnouncer announce: anEvent` the matcher walks the event's superclass chain
  (ETS metadata reads) and delivers to subscribers — *on that same announcer* —
  of the event class *or any ancestor* (subscribe to `UIEvent`, receive
  `ButtonClicked`); the walk is at announce time so live hierarchy changes are
  respected. The by-class index is keyed `{AnnouncerRef, Class, SubRef}`, so the
  walk reads `{AnnouncerRef, CurrentClass, '_'}` at each level and a subscription
  on a *different* announcer is never matched (**Amendment BT-2454**). **Delivery is de-duplicated per subscriber process**
  across the MRO walk — a process that subscribed to *both* `UIEvent` and
  `ButtonClicked` receives one delivery per *subscription* it registered
  (Pharo's rule), and the matcher never double-sends for a single ancestor
  walk. (See cost note below; this is why high-volume streaming is *excluded* —
  §5.)
- **Sync + async, fault-isolated.** `announce:` is async (direct sends, no
  wait). `announceAndWait:` gathers replies in the caller via
  `spawn_monitor`-per-handler + `receive` with a timeout, so a slow/blocked
  handler cannot wedge the bus and a crashing handler is isolated. The reply
  protocol is explicit: each spawned handler process sends `{Ref, ok}` on
  success; the caller's `receive` loop *also* matches
  `{'DOWN', Ref, process, _, Reason}` as the handler-crashed signal (logged
  `domain => [beamtalk, announcements]`, never propagated to siblings or the
  announcer) and the per-handler timeout ejects the loop — so a crashed handler
  can never leave the caller blocked on a reply that never comes. The timeout
  **defaults to 5 s** and is configurable via `announceAndWait:timeout:` (ms).
  `announceAndWait:` from inside a handler is therefore safe — there is no shared
  gen_server to deadlock on. **`announceAndWait:` is *not* available on
  `SystemAnnouncer`** (it is sealed async-only): the system bus can have many
  subscribers, and `spawn_monitor`-per-handler there would be an unbounded
  process storm under rapid system events (startup actor spawns). Sync gather is
  for per-instance announcers with a known, small subscriber set. **Enforcement
  is at runtime** — `SystemAnnouncer` overrides `announceAndWait:` to raise
  `(UnsupportedOperation announceAndWait:)`, *not* a type-only prohibition,
  because a value statically typed `Announcer` may dynamically be the system bus,
  so the guard must hold at the call, not just the declared type.
- **`doOnce:` is claimed atomically.** Since dispatch is caller-side and two
  `announce:` can run concurrently, a `doOnce` subscription is consumed with an
  atomic `ets:take(SubRef)` on its unique key —
  whichever announce wins the `take` delivers, the loser gets `[]` and skips it,
  so it fires at most once. (A concurrent announcer may waste its MRO walk on a
  row that's already taken — harmless; correctness rests on the single atomic
  `take`, not the walk.) Single-node guarantee; weakens under the package's
  distributed path, noted there.

The original design reversed BT-2193's "no pg / ETS-per-announcer" choice for
the shared `SystemAnnouncer`; as built (Amendment BT-2530) the reversal never
materialised — the system bus and per-instance announcers alike use the lean
ETS model BT-2193 specced, and cross-node subscribers register explicit pids.

### 2. Layer 2 — stdlib typed veneer

Three classes in the **core image** (so the system can publish):

```beamtalk
// Base event — an immutable typed payload. Apps subclass it and add `field:`
// slots; an announcement is a *fact*, so it is a Value (never mutable). Value
// inheritance with an abstract base mirrors `abstract Value subclass: Number`.
// NOTE: as a Value, two announcements with equal fields compare `==`
// (structural). That is fine here — the dispatch path keys on event *class* +
// subscription, never on event equality, so distinct-occurrence identity is
// never needed. (Pharo's Announcement uses identity; we accept structural
// equality as the price of Value's `field:`/keyword-ctor ergonomics.)
abstract Value subclass: Announcement

// A dispatcher handle — an opaque reflection of runtime state, exactly like
// `Pid` / `Port` / `Reference` (ADR 0067, "Object's Three Roles"): an `Object
// subclass:` with no declared data. The underlying announcer is an Erlang
// `reference()` carried as the object's native boxing and reached via FFI;
// equality is by identity (`=:=`), like `Pid`.
//   - NOT a Value: we don't want `with*:` setters, structural equality,
//     `new:`-map construction, or `fieldAt:` reflection on a capability handle.
//   - NOT an Actor: there is deliberately no per-announcer process (that would
//     reintroduce the central mailbox rejected in §4). The subscriber set lives
//     in the shared runtime ETS table keyed by the ref; dispatch runs
//     caller-side. `class new` mints a fresh announcer via FFI.
typed Object subclass: Announcer
  class new -> Announcer => (Erlang beamtalk_announcements) newAnnouncer
  when: aClass :: Class do: aBlock :: Block -> Subscription => ...
  when: aClass :: Class send: sel :: Symbol to: receiver -> Subscription => ...
  when: aClass :: Class doOnce: aBlock :: Block -> Subscription => ...
  announce: anEvent :: Announcement -> Nil => ...           // async (cast)
  announceAndWait: anEvent :: Announcement -> Nil => ...     // sync (call), 5s default timeout
  announceAndWait: anEvent :: Announcement timeout: ms :: Integer -> Nil => ...
  unsubscribe: receiver -> Nil => ...
  // self-inspection (object-knows-itself; reads its own ETS rows by ref) — §7
  subscriptions -> List(SubscriptionNode) => ...
  subscribersOf: aClass :: Class -> List(SubscriptionNode) => ...
  subscriptionCount -> Integer => ...

// The one the *system* publishes through — Pharo's SystemAnnouncer.
sealed Announcer subclass: SystemAnnouncer
  class current -> SystemAnnouncer => (Erlang beamtalk_announcements) system

// The unsubscribe token returned by when:… — another opaque runtime handle
// (Object, like Announcer), wrapping the subscription's unique SubRef. Each
// when:… returns a *distinct* Subscription (a process may hold several to the
// same class — Pharo's rule), so re-subscribing never silently replaces.
sealed typed Object subclass: Subscription
  unsubscribe -> Nil => ...        // removes exactly this SubRef's row
  isActive    -> Boolean => ...    // ets:member on the SubRef; false after unsubscribe
```

`SystemAnnouncer current` is the shared bus the runtime emits onto and tools
subscribe to. System facilities publish well-known **discrete** `Announcement`
subclasses (stdlib-provided): `ActorSpawned` / `ActorStopped`, `ClassLoaded` /
`ClassRemoved`, `BindingChanged`, and (ADR 0092) `SupervisionChildAdded` /
`SupervisionChildCrashed`. A tool subscribes once and filters by event class
instead of wiring four bespoke transports.

**System events are announced only *after* the triggering action's metadata
writes commit** — e.g. `ClassLoaded` fires from `beamtalk_object_class`'s
`handle_call` reply path *after* the class's `beamtalk_class_metadata` row is
written, so the MRO walk for the event (which reads that metadata) sees a
consistent hierarchy. The trade is a small observable gap (class live before its
announcement) over an inconsistent-read risk; the gap is the safe choice.

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

**Publish authority (who may *announce*, not just subscribe).** Unlike C#'s
`event` — where only the declaring type can *raise* — Beamtalk's "an announcer
is just an object" model does not language-enforce who may publish (nor does
Pharo's `SystemAnnouncer`). Two boundaries matter:

- **Remote front (the one that matters):** the ADR-0091 push facade is
  **subscribe-only and must never expose a publish capability**. A remote
  Observer can listen to curated system events but can never `announce:` onto
  `SystemAnnouncer` — otherwise it could forge `ActorSpawned`/`ClassRemoved` and
  mislead other subscribers. Same owner as the subscribe op list above.
- **In-image:** publishing on `SystemAnnouncer` is unrestricted, but that again
  implies an Owner/`eval` session — a forger with `eval` doesn't need to forge,
  so no real boundary is crossed. For *app-level* encapsulation (C#'s "only the
  owner raises"), the idiomatic answer is an **object-capability split** — the
  owner privately holds the `Announcer` and hands out a subscribe-only view —
  not a language keyword. No publish-side enforcement is added in v1.

### 7. Introspection & navigation — the third navigation sibling

A pub/sub bus that can't be inspected is a black box, which would contradict the
very reason this lives in the core (observability/tooling, §"Why now"). Beamtalk
already exposes runtime *wiring* along two axes that share one shape — a
**navigator** (`Object`) returning **snapshot records** (`Value`):
`SystemNavigation` walks static structure (classes, `sendersOf:`); ADR 0092's
`ProcessNavigation` walks the live supervision tree (`SupervisionNode`).
Subscriptions are wiring of the same kind, so the substrate ships the **third
sibling** in v1 — the pattern is settled, so there is no reason to defer it.
(Risk, accepted: `ProcessNavigation` is Planned, not yet Implemented (ADR 0092
BT-2427…2433); if its shape shifts during build, `AnnouncementNavigation` —
deliberately the *same* shape — co-evolves with it. The `Announcer`
self-inspection half costs nothing extra and is independent of that risk.)

Two levels, mirroring ADR 0092's *object-knows-itself / navigator-discovers-
system* split:

- **Object-knows-itself.** A live `Announcer` / `SystemAnnouncer` inspects its
  own subscriptions (`subscriptions`, `subscribersOf:`, `subscriptionCount`
  above) by reading its own ETS rows — cheap, caller-side.
- **Navigator-discovers-system.** `AnnouncementNavigation` is the dynamic twin
  of `SystemNavigation`, a `sealed typed Object subclass` (a query handle, not
  an actor — exactly like `SystemNavigation`/`ProcessNavigation`):

```beamtalk
class default                          -> AnnouncementNavigation   // the system bus
class of: anAnnouncer :: Announcer     -> AnnouncementNavigation   // one announcer
  subscriptions                        -> List(SubscriptionNode)   // snapshot
  subscribersOf: aClass :: Class       -> List(SubscriptionNode)
  announcedClasses                     -> List(Class)              // distinct event types in use
```

It returns a **read-only snapshot** of the subscription graph as immutable
`Value` records:

```beamtalk
sealed typed Value subclass: SubscriptionNode
  field: announcementClass :: Class      = nil   // the subscribed-to event type
  field: announcer         :: Announcer  = nil   // which bus
  field: subscriber        :: Pid        = nil   // the live handler process
  field: handlerKind       :: Symbol     = nil   // #do | #send | #doOnce
  field: once              :: Boolean    = false
```

The **read-vs-mutate rule** is ADR 0092 §3a's, applied identically: a
`SubscriptionNode` is a frozen fact and read-only; to *act* you cross back to
the live `Subscription` token (`unsubscribe`) or the `Announcer`. This keeps the
Object/Value split consistent across all three navigators — navigators
(`SystemNavigation`, `ProcessNavigation`, `AnnouncementNavigation`) and live
handles (`Announcer`, `Subscription`) are `Object`; snapshot records
(`SupervisionNode`, `SubscriptionNode`) and domain events (`Announcement`) are
`Value`. The IDE's "event wiring" pane (ADR 0017/0091) is `AnnouncementNavigation
default subscribersOf: …` grouped by `announcementClass`. The navigator reads
are gated as a `system`-scope read under ADR 0091, like the supervision
navigator (§6).

**Future work (not in scope): declarative `event:` emission manifest.** The
navigator above makes *subscriptions* (who listens) discoverable — a runtime
fact. The symmetric half — *emissions* (which `Announcement` subclasses a class
publishes) — is today invisible, buried in `announce:` calls in method bodies.
A declarative `event:` member (alongside `state:` / `field:` / `classState:`,
ADR 0067) would make emissions part of a class's published contract, enable
`announce:` type-checking, and complete the event graph (publishers *and*
subscribers). It is **deliberately deferred to its own ADR**: it is a new
declarative member kind touching parser / semantic analysis / reflection /
codegen, which would break this ADR's "no parser/codegen changes" property, and
it is a different shape from C#'s `event` (a *manifest* of emitted types, not a
per-object channel — the channel here is the `Announcer`). A pure-stdlib interim
(`class emittedEvents -> List(Class)`, hand-declared, no new syntax) can deliver
the discoverability win first if wanted.

**Update (BT-2475): the discoverability win shipped as a derived static
analysis instead.** Rather than a declarative manifest, the emission-side dual
of `AnnouncementNavigation` is now `SystemNavigation default announcementsSentBy: aClass`
— it mines the `announce:` / `announceAndWait:` / `announceAndWait:timeout:` call
sites out of a class's method bodies and resolves each event argument to its
`Announcement` subclass, completing the publisher↔subscriber graph with zero new
syntax and zero drift. It is deliberately *advisory* (constructor-call arguments
resolve; `Dynamic`/indirect arguments are skipped), not a sound contract. This
made the `event:` keyword track (BT-2437) redundant, and it was cancelled. See
`docs/beamtalk-language-features.md` → "`SystemNavigation` — Cross-class code
queries".

### REPL session

```beamtalk
bt> Announcement subclass: PriceChanged
...>   field: newPrice :: Number = nil
PriceChanged

bt> a := Announcer new
#Announcer<0.521.0>

bt> a when: PriceChanged do: [:e | Transcript showLine: "now " ++ e newPrice printString]
#Subscription<...>

bt> a announce: (PriceChanged newPrice: 42)
now 42

bt> "Subscribe to the system bus — one feed for everything the system emits:"
bt> SystemAnnouncer current when: ActorSpawned do: [:e | Transcript showLine: e actorClass name]
#Subscription<...>

bt> Counter spawn
Counter        "← the subscription above fires"

bt> "Introspect the wiring — the third navigation sibling (§7):"
bt> a subscriptions size
1
bt> (AnnouncementNavigation default subscribersOf: ActorSpawned) first handlerKind
#do
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
| **Phoenix.PubSub** | topic-string + cluster broadcast over `pg2`/`pg`. | **Take (original design):** `pg` for cluster-wide membership. **(Amendment BT-2530:** as built, membership is the local ETS table and cross-node delivery is explicit-pid registration — see Layer 1; partition tolerance still to the package.**)** **Leave:** topic *strings* — we want typed events + MRO, not stringly-typed topics. |
| **Erlang `gen_event`** | Serialised handler dispatch in one process. | **Leave:** single-process serialisation is a bottleneck and a shared failure domain; caller-side ETS fan-out + isolated handlers is more BEAM-idiomatic. |
| **Erlang `telemetry`** (ADR 0069) | Untyped measurement bus. | **Leave** as the measurement layer; **bridge** to it from the package. Explicitly *not* the typed Observer. |
| **ROS typed topics / Akka EventStream** | Typed publish/subscribe by message class. | Confirms type-keyed (not string-keyed) subscription is the ergonomic choice for domain events. |

## User Impact

- **Newcomer.** `when: X do: [...]` reads like every other block-taking message
  they already use; `Announcer new` hands back a plain handle. They can watch
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
- **`pg` for `SystemAnnouncer` membership:** the original design point — `pg`
  has no send primitive, so it is a registry, not a transport. **(Amendment
  BT-2530:)** as built, only the `beamtalk_pg` *scope* is started; subscribers
  are never joined to pg groups — membership is the workspace-local ETS table,
  and a connected node (the ADR-0091 LiveView front) gets delivery by
  registering its own remote pid through the normal subscribe path
  (location-transparent `Pid ! Msg` + cross-node monitors; see the Layer 1
  amendment). Only partition tolerance / replay / multi-workspace-cluster fall
  to the package. Per-instance announcers don't use `pg`.
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
- Explicit-pid registration + location-transparent send + cross-node monitors
  let `SystemAnnouncer` deliver to a *connected* node in v1 with no package —
  useful for the ADR-0091 LiveView front, which subscribes its remote pid
  through the normal subscribe path (Amendment BT-2530; `pg` groups are not
  used for membership). Only partition tolerance / replay /
  multi-workspace-cluster fall to the package.
- The bus is **navigable in v1** (§7): `AnnouncementNavigation` + the
  `Announcer` self-inspection methods make subscriptions a first-class,
  walkable part of the runtime — the third sibling of `SystemNavigation` /
  `ProcessNavigation`, with the same `Object`-navigator / `Value`-snapshot shape.
  Pub/sub is not a black box.

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
  out send cost — the ns figure is an order-of-magnitude estimate, to be
  confirmed by the Phase-1 EUnit benchmark). Fine for discrete events at
  human/IDE rates; **not** fine for line-rate streams — which is precisely why
  Transcript is excluded (§5).
- **At-most-one in-flight delivery after `unsubscribe:`.** Because dispatch is
  caller-side, a subscriber can receive a message for an `announce:` that was
  already fanning out when its `unsubscribe:` completed. Handlers must be
  idempotent / tolerate one spurious post-unsubscribe delivery — a real
  behavioural difference from a centralised bus, and a documented contract, not
  a bug.
- **MRO walk truncates if a class is removed mid-announce.** A concurrent
  `ClassRemoved` can make the superclass walk hit `not_found` and stop, so
  subscribers of ancestors above the removed class miss that one event. Accepted
  for the intended interactive/IDE use; documented, not engineered around.
- **Cross-node sends absorb up to `net_ticktime` on unreachability short of a
  full netsplit.** If a connected subscriber's node becomes unreachable
  (port congestion, half-open TCP) the announcer keeps sending to it until the
  cross-node monitor fires (default ~60 s). This is normal distribution jitter,
  not partition handling (deferred to the package); cross-node consumers accept
  it as v1 behaviour.

### Neutral
- Adds a runtime worker (`beamtalk_announcements`) under `beamtalk_runtime_sup`
  (after `beamtalk_bootstrap`) and ~6 stdlib classes (`Announcement`,
  `Announcer`, `SystemAnnouncer`, `Subscription`, `AnnouncementNavigation`,
  `SubscriptionNode`) + a handful of system `Announcement` subclasses.
- `pg` gains only the dedicated `beamtalk_pg` scope, started by the bus
  (Amendment BT-2530: subscriber membership is the ETS subscription table, not
  pg groups — the scope is standing infrastructure with no group joins today).
- `announceAndWait:` is `spawn_monitor`-per-handler — N transient processes per
  sync announce; acceptable for the low-frequency sync path, not the async one.

## Implementation

Downstream of acceptance (the `/plan-adr` step owns the epic + the BT-2193
re-scope):

0. **Phase 0 (napkin):** `beamtalk_announcements` with one `Announcement`
   class, `Announcer new`, `when:do:`, `announce:` (async) over a single `pg`
   group + monitor cleanup. Prove typed dispatch end-to-end from the REPL.
1. **Runtime bus (full):** MRO match, `announceAndWait:` (sync), `when:send:to:`,
   `when:doOnce:`, fault isolation, sup wiring. **EUnit must prove the
   concurrency properties a REPL demo cannot** (the hardest-to-get-right parts):
   (a) concurrent `announce:` from N processes against one `doOnce` subscription
   → *exactly one* delivery; (b) subscriber dies before dispatch → no delivery,
   no announcer crash; (c) bus crashes + restarts → subscriptions survive (heir)
   and pids that died in the gap are pruned; (d) handler crash under
   `announceAndWait:` → caller still returns (DOWN handled), siblings unaffected;
   plus the MRO-walk microbenchmark backing the cost estimate.
2. **stdlib veneer + introspection:** `Announcement` / `Announcer` /
   `SystemAnnouncer`, the `Announcer` self-inspection methods, and the
   `AnnouncementNavigation` navigator + `SubscriptionNode` value class (§7) —
   the navigation pattern is settled by `SystemNavigation`/`ProcessNavigation`,
   so it ships in v1, not later. Registration (`generated_builtins.rs`,
   `build_stdlib.rs`); typed signatures. BUnit.
3. **System events + consumer consolidation:** define the system `Announcement`
   subclasses; migrate the runtime emit points; update ADRs 0017/0046/0054/0091/
   0092 to subscribe via `SystemAnnouncer` (and ADR 0092 §8 to point here).
4. **Package re-scope (BT-2193):** distributed delivery, `RecordingAnnouncer`,
   telemetry bridge; cross-repo CI.
5. **Docs + e2e:** language-features chapter, surface-parity, e2e btscript;
   flip this ADR to Implemented.

Affected components: **runtime** (new worker), **stdlib** (veneer + system
events), **package** (re-scoped), **docs/tests**. No parser/codegen changes.

## Implementation Tracking

**Epic:** [BT-2438](https://linear.app/beamtalk/issue/BT-2438) — Announcements: Typed Event Substrate (ADR 0093)
**Status:** Implemented (Layers 1–2 + system events + introspection + docs/e2e; Layer 3 package = BT-2193, optional, separate track)

| Phase | Issue | Scope |
|---|---|---|
| 1 | BT-2439 | Runtime bus skeleton + ETS sub table + subscribe + async `announce:` |
| 1 | BT-2440 | MRO subclass matching + per-subscription de-dup |
| 1 | BT-2441 | `announceAndWait:` (sync) + `doOnce:` + `when:send:to:` + fault isolation |
| 1 | BT-2442 | ETS heir crash-survival re-arm + dead-pid prune |
| 2 | BT-2443 | stdlib veneer — `Announcement`/`Announcer`/`SystemAnnouncer`/`Subscription` |
| 2 | BT-2444 | Introspection — self-inspection + `AnnouncementNavigation` + `SubscriptionNode` |
| 3 | BT-2445 | System `Announcement` subclasses + runtime emit points |
| 3 | BT-2446 | Point consumer ADRs (0017/0046/0054/0091/0092) at `SystemAnnouncer` |
| 4 | BT-2193 | Package re-scope — distributed/`RecordingAnnouncer`/telemetry bridge |
| 5 | BT-2447 | Language docs + surface-parity + e2e btscript; flip ADR → Implemented |

Related: BT-2437 (`event:` emission manifest — separate track, §7). ADR-authoring issue BT-2396 is Done.

## Migration Path

- **BT-2193** is re-scoped from "whole framework as a package" to "optional
  extras as a package," with a new stdlib/runtime epic for Layers 1–2.
- The five consuming ADRs migrate their planned bespoke channels to
  `SystemAnnouncer` subscriptions as part of Phase 3; until then nothing
  regresses (none of those channels is built yet).

## Amendments

### BT-2454 — true per-instance `Announcer` subscription isolation

**Context.** As originally specced (and shipped in Phase 1–2), the subscription
table had **no announcer dimension**: it was keyed by `SubRef` with a by-class
index `{Class, SubRef}`, so every `Announcer new` shared one class-keyed bus. A
subscription made on announcer A received events announced on announcer B for the
same class. This contradicted the intuitive "fresh, independent dispatcher"
reading of `Announcer new` (and Pharo, where each `Announcer` owns its own
`SubscriptionRegistry`), and was a latent cross-talk footgun between unrelated
libraries that happen to share an event class.

**Decision: isolate.** Each subscription row and by-class index entry now carries
an `AnnouncerRef` dimension (a `reference()` per `Announcer new`; the
`beamtalk_system_announcer` atom for `SystemAnnouncer` and the raw Layer-1 API).
Matching (`announce:` / `announceAndWait:`) and introspection
(`subscriptions` / `subscribersOf:` / `subscriptionCount`, and
`AnnouncementNavigation`'s `default` vs `of:`) all scope to one announcer.

**Why this is cheap and does not reverse §4.** §4 rejected a per-announcer
*process* (a central mailbox to deadlock/bottleneck). Isolation reintroduces
none of that — it is purely a **matching-key change**. Dispatch stays caller-side
off concurrent ETS reads, the MRO walk is unchanged except for the extra key
field, and there is still exactly one shared ETS table pair. `SystemAnnouncer`
falls out as just the atom-keyed namespace; `system_announce/2` publishes there.

**`AnnouncementNavigation`** becomes a scope-carrying handle (FFI-minted with its
announcer, like `Announcer` carries its ref) so `default` (system bus) and
`of: anAnnouncer` (that announcer) report different subscription sets — realising
the §7 design that was vestigial under the shared bus.

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
