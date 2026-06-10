# ADR 0095: Rich Navigable Inspector — Drillable, Evaluable, Live-Updating Object Views

## Status
Proposed (2026-06-10)

## Context

### The problem

Beamtalk can *render* an object (ADR 0094: `printString` →
`Point(x: 3, y: 4)` / `Actor(Counter, 0.123.0)`) and it can *enumerate the
running system* (ADR 0092 `ProcessNavigation`; ADR 0093 `AnnouncementNavigation`).
What it cannot do is let a developer **navigate into a single object** the way a
Pharo Inspector does:

- **Drill in.** Open a field's value as its own inspectable view, recursively.
- **Evaluate in context.** A doIt pane where expressions evaluate with `self`
  bound to the inspected object.
- **Live update.** When the inspected actor's state changes, the view reflects
  it without the developer re-issuing the query.

Today there are two impoverished paths and nothing in between:

1. **Agent-only typed JSON.** The REPL `"op": "inspect"`
   (`beamtalk_repl_ops_actors.erl:57`) does a guarded `sys:get_state(Pid, 5000)`
   and returns the actor's user fields as a flat JSON map. It is one-shot, flat
   (no drill), actor-only, and per the surface-parity doc is deliberately
   *agent-only* (`docs/development/surface-parity.md:86`).
2. **Humans get `printString`.** `Transcript show: anObject` gives the one-line
   structural string from ADR 0094 — legible, but a dead end: you cannot click a
   field, cannot evaluate against the object, cannot watch it change.

The data needed for a real inspector already exists — `Value` field slots, actor
state behind `sys:get_state`, `erlang:process_info`, the metaclass tower
(ADR 0036), and the runtime structural renderer (ADR 0094). What is missing is
the **language-level API** that composes them into a navigable object model that
every surface (REPL text tree, MCP JSON tree, browser interactive pane) renders
from one source.

This is the surface the Cockpit LiveView IDE's Inspector pane (BT-2486, BT-2492)
and a future Pharo-style debugger both sit on top of. This ADR designs that API.

### This is the `inspect` redesign ADR 0094 deferred

ADR 0094 split the string-display protocols (`printString` = Debug,
`displayString` = Display) but **explicitly left `inspect` untouched** —
returning a `String`, delegating to `printString` — and named its end-state:

> *"a follow-up ADR, designed against the LiveView live-tooling surface, will
> repurpose it as the verb that produces or opens a structured inspector."*
> — ADR 0094 §1

This is that follow-up. It owns the breaking `inspect` return-type change and the
migration risks ADR 0094 pre-documented (the 14 `inspect -> String` overrides, the
gradual-typing crash audit, the `runtimeCalledSelectors` entry, the `"op":
"inspect"` naming collision).

### The fourth navigation sibling

Beamtalk already exposes runtime structure along three axes that share one shape —
a **navigator** (`Object`) handing back **immutable snapshot records** (`Value`):

| Navigator (`Object`) | Walks | Snapshot record (`Value`) | ADR |
|---|---|---|---|
| `SystemNavigation` | static code (senders/implementors) | site records | 0087 |
| `ProcessNavigation` | live supervision tree | `SupervisionNode` | 0092 |
| `AnnouncementNavigation` | subscription graph | `SubscriptionNode` | 0093 |

The Inspector is adjacent but **not** a fourth navigator of the same shape: those
three each enumerate a *system-wide collection*; the Inspector focuses on *one
object* and drills *into* it. It reuses their hard-won conventions —
`Object`-handle vs `Value`-record split, lazy/timeout-guarded state capture,
snapshot-then-refresh semantics, the read-vs-mutate rule — but its handle is a
**cursor over a single subject**, not a system query.

### Constraints

1. **Actor opacity (ADR 0094, ADR 0067).** An actor's state lives behind its
   message boundary. Reading it for inspection means a synchronous
   `sys:get_state` — racy, blocking on a busy actor, a confidentiality concern.
   State capture must be **lazy, timeout-guarded, and snapshot-based**, never on
   the display hot path. This is the same discipline ADR 0092 applies to
   `sys:get_status`.
2. **No self-deadlock on the walk.** Like `SystemNavigation`/`ProcessNavigation`,
   the inspector's traversal must run as plain code in the calling eval worker,
   never routed through a class-side gen_server — or inspecting the eval worker's
   own supervisor would deadlock.
3. **Three-kind data model (ADR 0067).** `Value` (immutable `field:` slots),
   `Actor` (`state:` behind a process), plain `Object` (no Beamtalk-visible data;
   wraps a foreign handle). The inspector must handle all three *plus* genuinely
   foreign OTP processes (package gen_servers) that have no Beamtalk wrapper.
4. **Codegen rule (CLAUDE.md).** Any generated string/structure flows through the
   `Document`/typed-leaf API, never `format!`.
5. **Surface parity (CLAUDE.md).** The same `Inspector` must drive REPL, MCP, and
   browser. The *navigation API* is shared; only the *rendering* is per surface.
6. **Bounded by construction.** Large collections, deep nesting, and cyclic
   object graphs must never hang or blow memory — reusing ADR 0094's depth/width/
   length caps and adding pagination + a cycle guard.

## Decision

Introduce a single polymorphic **`Inspector`** class — a `sealed typed Object
subclass` (a live cursor handle, like the three navigators) — returned by
`anObject inspect` and `Inspector on: anObject`. It exposes a uniform navigation
protocol (`fields` / `at:` / `path` / `printString` / `refresh`) over **all**
subject kinds (Value, Actor, foreign process, collection), with a per-class
**protocol hook** (`inspectorFields`) for classes that want a custom field view.
Drilling returns a new `Inspector` cursor; fields are immutable `InspectorField`
**Value** records. State capture for actors is a lazy, timeout-guarded snapshot.
`evaluate:` runs **out-of-band** against the snapshot by default; `evaluateLive:`
is the explicit escape hatch that routes into the live actor. **Live updates are
poll-only in v1** (`refresh` re-snapshots); push-based live tracking is a
documented follow-up (BT-2489).

### 1. Core type — one `Inspector`, polymorphic over subject kind

```beamtalk
sealed typed Object subclass: Inspector
  // construction
  class on: anObject               -> Inspector       // explicit
  // navigation
  subject                          -> Object          // the thing being inspected (or its snapshot proxy)
  kind                             -> Symbol           // #value #actor #foreign #collection #sealed
  fields                           -> List(InspectorField)   // drillable field/element records
  at: aName                        -> Result(Inspector, Error) // drill into one field → child cursor
  parent                           -> Inspector | nil   // the cursor we drilled from
  root                             -> Inspector         // the top of this drill chain
  path                             -> List(Symbol)      // breadcrumb from root (for UI)
  // evaluation
  evaluate: src :: String          -> Result(Object, Error)  // out-of-band, self = snapshot/value
  evaluateLive: src :: String      -> Result(Object, Error)  // routed into the live actor (actors only)
  // refresh (poll-based liveness, §6)
  refresh                          -> Inspector         // re-snapshot the subject in place
  // rendering
  printString                      -> String            // text tree, depth-1 (REPL)
  printStringExpanded: depth :: Integer -> String       // text tree to N levels
  asTree                           -> Dictionary         // structured tree (MCP/browser)
```

`inspect` is **repurposed** (the ADR-0094 deferral): `Object >> inspect` now
returns `Inspector on: self`, not a `String`. `Inspector on:` is the explicit
spelling; `anObject inspect` is the shorthand. The two surfaces document one
entry point.

**Why one polymorphic class, not per-class inspectors or a bare protocol.** A
single `Inspector` gives newcomers *one* thing to learn, keeps the four
navigation siblings teachable as a set, and realises ADR 0094's stated end-state
verbatim. Per-class customisation — Pharo's killer Inspector feature — is still
available, but through a **protocol hook on the subject class**, not a parallel
hierarchy of `CounterInspector` subclasses:

```beamtalk
class Matrix
  // Optional: override the default field view. Returns InspectorField records.
  // Absent → the Inspector derives fields generically from the subject's kind.
  inspectorFields -> List(InspectorField) =>
    ^ { InspectorField name: #rows value: self rowCount.
        InspectorField name: #cols value: self colCount.
        InspectorField name: #data  value: self asGrid }
```

`Inspector` checks `subject respondsTo: #inspectorFields` and uses it when
present, otherwise falls back to the kind-derived default (§3). This is Pharo's
*content/custom-presenter* extensibility without the class-explosion: one
inspector, many optional per-class views.

### 2. The field record — `InspectorField` (`Value`)

Each drillable thing is an immutable record, mirroring `SupervisionNode` /
`SubscriptionNode`:

```beamtalk
sealed typed Value subclass: InspectorField
  field: name     :: Object  = nil    // Symbol (slot) | Integer (element index) | key
  field: label    :: String  = ""     // display label (e.g. "x", "[0]", "#key →")
  field: value    :: Object  = nil     // the field's value (or a lazy handle for big children)
  field: kind     :: Symbol  = #slot   // #slot #element #association #processInfo #computed
  field: drillable :: Boolean = true   // false for leaf scalars (numbers, atoms)
```

`fields` is the **flat record API** the acceptance criteria asks for; `at:` is the
**navigation** layer on top — exactly the dual `ProcessNavigation` strikes with
`tree nodes` (flat) vs `tree root` (navigable). A surface that just wants to list
renders `fields`; one that drills calls `at:`.

**Read-vs-mutate rule (ADR 0092 §3a, applied identically).** An `InspectorField`
is a frozen fact and read-only — there is no `field setValue:`. To *change*
something you discovered, cross back to the live object: send it a message, or use
`evaluateLive:` (§5). Snapshot facts stay on the record; mutation stays on the
live subject.

### 3. Field derivation by subject kind

`Inspector kind` classifies the subject (reusing ADR 0092's kind machinery for
processes) and derives `fields` accordingly:

| `kind` | Subject | `fields` are… | `at:` drills to… |
|---|---|---|---|
| `#value` | a `Value` instance | its `field:` slots (sorted, ADR 0094 order) | the slot's value |
| `#actor` | a Beamtalk `Actor` | state slots from a **lazy snapshot** (§4) | the slot's value (out-of-band) |
| `#collection` | `List`/`Array`/`Set`/`Dictionary`/`Bag` | windowed elements / associations (§8) | the element / value |
| `#foreign` | a non-Beamtalk OTP process (`Pid`) | best-effort `process_info` + `sys:get_state` (§4) | the value where derivable |
| `#sealed` | a class opting out (§9) | `[]` — redacted | — (drill refused) |

For `#value` the derivation is pure structural traversal of the immutable record
(no process contact). For `#actor`/`#foreign` it reads a snapshot (§4). A subject
whose class defines `inspectorFields` (§1) bypasses the table and uses that.

### 4. Actor & foreign state — lazy, timeout-guarded snapshot

Constructing an `Inspector on: anActor` does **not** read state. State is captured
the first time `fields` (or `at:`) needs it, via a guarded
`sys:get_state(Pid, Timeout)` (the same primitive the current `"op": "inspect"`
uses, `beamtalk_repl_ops_actors.erl:71`) wrapped in the ADR-0092 guard:

- alive + responsive → a **frozen snapshot** of the user state slots; `fields`
  reads from it, and the snapshot is reused until `refresh`.
- busy past the timeout, dead, or not `sys`-compliant → `fields` returns a single
  diagnostic `InspectorField` (`#status → #unavailable`), **not** a crash —
  exactly ADR 0092's "`status` returns `nil`, never raises."

This makes the Inspector safe to open on any actor — a wedged actor degrades to
"state unavailable" instead of hanging the REPL, the same guarantee ADR 0094's
display path gives.

**Foreign processes** (`#foreign`): `process_info(Pid, [registered_name,
current_function, message_queue_len, …])` plus a guarded `sys:get_state` when the
process is `sys`-compliant; fields are best-effort and tagged `kind:
#processInfo`. A foreign process that doesn't answer `sys:get_state` still shows
its `process_info` fields.

**Snapshot semantics (ADR 0092 §4, inherited).** The captured state is a
*point-in-time* truth. Once frozen, drilling never re-contacts the actor, so the
walk is internally consistent and cannot deadlock or observe mid-mutation. To see
later state, `refresh` (§6).

### 5. Evaluate-in-context — out-of-band default, explicit live escape hatch

```beamtalk
i := someValue inspect
i evaluate: "self magnitude"        // => Result ok: 5.0   (pure, self = the value)

c := aCounter inspect
c evaluate: "self count > 10"       // => Result ok: false  (self = the SNAPSHOT)
c evaluateLive: "self increment"    // => Result ok: 1      (routed INTO the live actor)
```

Two methods, two safety profiles:

- **`evaluate:` (out-of-band, the default).** Compiles the expression with `self`
  bound to the **snapshot** (for actors) or the **value itself** (for values),
  and evaluates it in the calling eval worker. No message reaches the live actor;
  nothing mutates. For **values** this is fully faithful (values are immutable, so
  snapshot == live). For **actors** it answers "what holds *given the captured
  state*" — field reads and pure computations resolve against the snapshot; an
  expression that would require live behaviour (sending a message that mutates, or
  reading a slot the snapshot didn't capture) returns
  `Result error: (beamtalk_error needs_live_eval)` directing the caller to
  `evaluateLive:`. This is the safe, deadlock-free default and honours ADR 0094's
  "do not read/mutate a live actor for display."
- **`evaluateLive:` (routed into the actor).** Sends the expression into the live
  actor via a reserved, guarded debug-eval call (§Implementation), so `self` is
  the **real actor**, methods run, and mutations take effect. It can block or fail
  (busy/dead actor) and so returns a `Result`. This is the doIt pane's "actually
  poke it" button — deliberately a *different, named* verb so a side effect is
  never accidental. `evaluateLive:` on a non-actor value is identical to
  `evaluate:`.

### 6. Live updates — poll-only in v1

The Inspector is **snapshot-then-refresh** in v1, matching `ProcessNavigation`
(ADR 0092 §4) and Pharo's manual-refresh Inspector:

```beamtalk
i := aCounter inspect          // snapshot at T0
aCounter increment             // state changes
i fields                       // still shows T0 (frozen snapshot)
i refresh fields               // re-snapshots → shows T1
```

A LiveView pane that wants a "live" feel re-issues `refresh` on a timer (cheap —
one guarded `sys:get_state`) and diffs snapshots for a field-flash effect. This is
exactly what LiveDashboard does and what the Cockpit Phase 3 design (BT-2492)
calls for.

**Push-based live tracking is a documented follow-up (BT-2489), not v1.**
Publishing on *every* actor state mutation is expensive, so the eventual design is
**opt-in**: an actor declared `observable: true` publishes an `ObjectStateChanged`
announcement on `SystemAnnouncer` (ADR 0093 substrate) after each committed state
write, and an Inspector subscribed to it refreshes reactively. The schema is
specified now so BT-2489 has a target:

```beamtalk
// Deferred to BT-2489 — specified here, not shipped in v1.
sealed Announcement subclass: ObjectStateChanged
  field: pid          :: Pid    = nil    // the actor whose state changed
  field: actorClass   :: Class  = nil
  field: changedSlots :: List(Symbol) = #()   // best-effort; #() = "unknown, refresh all"
```

v1 ships poll-only because (a) it unblocks the Inspector and the Cockpit panes
immediately, (b) the push cost/opt-in trade is its own design (the `observable:`
member is a new declarative kind, like ADR 0093 deferred its `event:` manifest),
and (c) poll is the proven LiveDashboard/Pharo model. This ADR therefore *blocks*
BT-2489 (it defines the seam) without depending on it.

### 7. Cross-surface rendering — one API, three renderers

The `Inspector` is the single source; each surface renders it differently by
calling the **same** navigation methods:

| Surface | Renders via | Shape | Drill / eval |
|---|---|---|---|
| **REPL** | `printString` (depth-1) / `printStringExpanded:` | indented text tree | `i at: #foo`, `i evaluate: "…"` typed at the prompt |
| **MCP** | `asTree` → structured JSON, depth + **lazy children** | JSON tree; large nodes report `childCount` + a fetch token | tool calls re-issue `at:` / `evaluate:` against the cursor |
| **Browser (Cockpit)** | `asTree` + per-field click → `at:`; doIt pane → `evaluate:`/`evaluateLive:`; timer → `refresh` | interactive tree, live-flash | click-to-drill, doIt pane, poll-refresh |

REPL default depth is **1** (show the object and its immediate fields; deeper on
`expand:`) so a `printString`-at-the-prompt stays terse, while the structural
`printString` of ADR 0094 (which expands bounded-deep inline) remains the
*one-line* form. `asTree` returns a `Dictionary` the MCP/LiveView layers serialise
— it carries `kind`, `path`, and `fields` (each with `name`/`label`/`drillable`/a
depth-bounded `value` preview), plus a `childCount` + truncation marker for nodes
past the window so a 100k-element list ships one page, not the whole thing (§8).

The current MCP/REPL `"op": "inspect"` (flat JSON, agent-only) is **superseded**
by `asTree` (a navigable tree). Surface-parity is updated: `inspect` graduates
from "agent-only typed introspection" to a cross-surface navigable view.

### 8. Large collections — windowing & pagination

A `#collection` subject does not materialise every element. `fields` returns a
**window** (default page size **50**, matching ADR 0094's width cap and Elixir's
`:limit`); beyond it the Inspector reports the total and hands back a next-page
cursor:

```beamtalk
big := (1 to: 100000) asList inspect
big size                       // => 100000   (cheap — collection size, not a walk)
big fields size                // => 50        (first window)
big page: 2                    // => Inspector window [50..99]
big at: 73                     // => Inspector on the 74th element (direct index, no walk)
```

`at:` on a collection indexes directly (no full traversal). `asTree` emits the
window plus `childCount` and a page token, so the MCP/browser layers fetch
subsequent pages lazily. This is the lazy-children pattern the acceptance criteria
asks for, reusing ADR 0092's `simple_one_for_one` child-cap precedent (don't copy
a giant child list out in one shot).

### 9. Cycle detection & sealing

**Cycles.** Mutually-referencing actors (A holds B holds A) must not infinite-loop
on drill-down or in `printStringExpanded:`/`asTree`. The Inspector carries a
**per-traversal seen-set** keyed by object identity — pid for actors/foreign
processes, term identity for the rare cyclic value graph (immutable `Value`s are
trees post-ADR 0042, so cycles arise almost only through actor references). On
revisiting a seen node the renderer emits a **back-reference marker**
(`↩ Actor(A, 0.123.0)`) instead of recursing. This is Pharo's per-traversal
seen-set, and it reuses ADR 0094's cycle-guard infrastructure (the structural
renderer already tracks a cycle set for inline `printString`).

**Sealing.** A class may refuse inspection — rare, for credential-bearing actors
(a `Vault`, a session holding tokens):

```beamtalk
sealedFromInspection actor: CredentialStore
  state: secret :: String = ""
  // ...
```

`Inspector on: aCredentialStore` returns a `#sealed` inspector: `fields` → `[]`,
`at:` → `Result error: (beamtalk_error sealed_from_inspection)`, `evaluate:`/
`evaluateLive:` refused, `printString` → `Inspector(CredentialStore, sealed)`. The
class still *exists* and prints normally via ADR 0094 (`Actor(CredentialStore,
0.x.0)`); only the *drill/evaluate* capability is withdrawn. Sealing is a class
modifier (parser/semantic flag) checked by the runtime inspector primitive, so it
cannot be bypassed by constructing the `Inspector` a different way. Default is
**unsealed** — sealing is a deliberate, rare opt-out.

### REPL session

```beamtalk
bt> p := Point x: 3 y: 4
Point(x: 3, y: 4)

bt> i := p inspect
Inspector(Point)
  x: 3
  y: 4

bt> i at: #x
Inspector(Integer = 3)

bt> i evaluate: "self dist: (Point x: 0 y: 0)"
Result ok: 5.0

bt> c := Counter spawn
Actor(Counter, 0.123.0)
bt> c increment; increment
bt> ci := c inspect
Inspector(Counter)            "lazy snapshot taken on first `fields`"
  count: 2

bt> ci evaluate: "self count * 10"     "out-of-band, against the snapshot"
Result ok: 20
bt> c increment                         "state moves on"
bt> ci count                            "stale — still the snapshot"
// (drill: ci at: #count → Inspector(Integer = 2))
bt> ci refresh at: #count
Inspector(Integer = 3)                  "re-snapshotted"

bt> ci evaluateLive: "self increment"  "routed INTO the live actor"
Result ok: 4
```

### Error / edge behaviour

```beamtalk
bt> (busyActor inspect) fields
// => [ InspectorField(status: #unavailable) ]   "sys:get_state timed out — no hang, no raise"

bt> credStore inspect
Inspector(CredentialStore, sealed)
bt> (credStore inspect) at: #secret
// => Result error: (beamtalk_error sealed_from_inspection)

bt> (aCounter inspect) evaluate: "self increment"
// => Result error: (beamtalk_error needs_live_eval)
//      "out-of-band eval cannot mutate a live actor; use evaluateLive:"

bt> (aCounter inspect) at: #nonesuch
// => Result error: (beamtalk_error no_such_field)
```

## Prior Art

| Source | Model | What we take / leave |
|---|---|---|
| **Pharo Inspector** (`Spec2-Inspector`) | `anObject inspect` opens a window; the Inspector is a Model; per-class custom tabs via `inspectionXxx` pragma methods; doIt pane with `self` bound; manual refresh; per-traversal cycle handling. | **Take:** `inspect` as the verb, the doIt-with-`self` pane, per-class custom views, manual refresh, the seen-set. **Adapt:** one polymorphic `Inspector` + an `inspectorFields` hook instead of a window/Model + pragma scan; actor state is a *guarded snapshot*, not a live image read. |
| **Newspeak Hopscotch** | Composable, navigable "subject" presenters; drilling pushes a new presenter onto a path. | **Take:** drill = push a child cursor; `path` as a first-class breadcrumb. **Leave:** the full presenter-composition UI framework — that's the browser layer, not the API. |
| **Smalltalk-80 Browser** | Class/protocol/method navigation panes. | **Take:** navigation-by-drilling idiom. **Leave:** it's a code browser, not an object inspector. |
| **IPython `_repr_html_` / repr protocol** | Object opts into a rich per-surface representation; text vs HTML chosen by the frontend. | **Take:** one object, per-surface rendering (`printString` vs `asTree`); the per-class opt-in hook (`inspectorFields` ≈ `_repr_html_`). |
| **Phoenix LiveDashboard** (process detail) | LiveView render of `process_info`/`sys:get_state`, periodic re-poll, lazy detail, infra filtering. | **Take:** poll-refresh as the v1 live model; lazy/guarded state; foreign-process detail. **Confirms:** push events are *not* required for a usable live pane. |
| **BEAM `:observer`** (process tab) | `process_info` + `sys:get_state` on selection; mixes process kinds; lazy state. | **Take:** the exact primitive set; lazy state-on-select; show foreign processes. **Leave:** wx GUI with no programmatic surface — we expose the *data*, the IDE renders it. |
| **Elixir `IEx` `dbg`/`i` + `Inspect`** | `i/1` describes a term; `Inspect` is bounded structural rendering with `:limit`. | **Take:** bounded structural rendering and the window size; the structural-by-default stance (already adopted in ADR 0094). |

**Net:** Pharo's Inspector *behaviour* (drill, doIt, custom views, refresh,
cycle-safety), `:observer`/LiveDashboard's *safe BEAM state capture*
(guarded snapshot, lazy, poll-refresh, foreign processes), IPython's *one-object/
per-surface-render* split — exposed as one message-passing `Inspector` object,
not a GUI.

## User Impact

- **Newcomer.** `anObject inspect` does the obvious thing — opens a navigable view
  in the IDE, prints an indented tree in the REPL. `at:`/`fields`/`evaluate:` are
  the same message idioms used everywhere. Drilling teaches object structure
  interactively. Getting it wrong is gentle: a wedged actor shows "unavailable",
  a bad field name returns a `Result error:`, never a crash.
- **Smalltalk developer.** It *is* the Pharo Inspector — `inspect` opens it, the
  doIt pane binds `self`, per-class custom views via `inspectorFields`, manual
  refresh. The departure (`evaluate:` defaults to *out-of-band*, with a separate
  `evaluateLive:`) is justified by actor opacity — Pharo has no message-boundary
  to respect; Beamtalk does.
- **Erlang/BEAM developer.** Under the hood it's `sys:get_state` +
  `process_info`, guarded and lazy — no magic, no bookkeeping process, the same
  primitives `:observer` uses. Foreign gen_servers inspect correctly as
  `#foreign`. They can still drop to FFI for raw terms.
- **Operator.** Opening an inspector never reads state on the hot path and never
  hangs on a busy actor (timeout-guarded). `evaluate:` cannot mutate a production
  actor by accident — that needs the explicit `evaluateLive:`. Sealing protects
  credential-bearing actors from being drilled over the remote front (ADR 0091).
- **Tooling developer.** One `Inspector` API drives REPL text, MCP JSON, and the
  LiveView pane — write the navigation once, render three ways. `asTree`'s
  lazy-children + `path` make a 100k-element list and a deep graph both
  renderable. Immutable `InspectorField` records are trivial to diff for
  field-flash (BT-2492).

## Steelman Analysis

### (a) JSON-only — keep `"op": "inspect"` as the flat typed map (status quo)
- ⚙️ **BEAM veteran:** "One `sys:get_state` → a JSON map. No new classes, no
  abstraction. The agent already parses it."
- 🏭 **Operator:** "Smallest attack surface; it's read-only and flat, nothing to
  drill or evaluate that could misbehave."
- *Why rejected:* it is one-shot, flat, actor-only, and explicitly agent-only —
  it cannot drill, evaluate, or update, which are the *entire* ask. It is the
  baseline the ADR exists to replace; `asTree` subsumes it (the flat map is one
  level of the tree).

### (b) Per-class inspector classes (`CounterInspector`, …) — Pharo's literal model
- 🎩 **Smalltalk purist:** "This is how Pharo does custom inspection — a subclass
  per type with bespoke tabs. Maximum flexibility."
- 🎨 **Language designer:** "Each class owns its view; no central polymorphic
  switch."
- *Why rejected:* a class-explosion (one inspector subclass per inspected type)
  for a feature most classes want *generically*. The same customisation is
  available through the `inspectorFields` hook on the subject class — Pharo itself
  has moved toward pragma-annotated methods on the domain class for exactly this
  reason. One `Inspector` + an optional per-class hook gets the flexibility without
  the parallel hierarchy.

### (c) Protocol-based (`InspectableProtocol`) — no Inspector class at all
- 🎨 **Language designer:** "Define `fields`/`at:`/`evaluate:` as a protocol; any
  class conforming is auto-handled. No new object, pure structural typing
  (ADR 0068)."
- 🧑‍💻 **Newcomer:** "Fewer types — I just implement an interface."
- *Why rejected:* it pushes the *generic* machinery (snapshotting, windowing,
  cycle-guard, path, refresh) onto every conforming class, or into free functions
  with nowhere to live. The cursor needs *state* (the snapshot, the seen-set, the
  path, the page) — that state wants an object. The protocol *hook*
  (`inspectorFields`) is the good half of this idea and is adopted (§1); making the
  *whole* inspector a bare protocol just relocates the cursor's state with no
  home.

### (d) Raw reflection only — surfaces compose everything
- ⚙️ **BEAM veteran:** "Give me `fieldsOf:` / `valueAt:` / `stateOf:` primitives
  and let each surface build its own navigation. No opinionated middle layer."
- *Why rejected:* abandons every other cohort and *guarantees* drift — REPL, MCP,
  and browser would each re-implement snapshotting, windowing, cycle-guard, and
  eval-routing slightly differently, the exact surface-parity failure the project
  fights. The whole point is *one* navigable model rendered three ways.

### Tension points
- **Inspector as `Object` vs `Value`.** A purist could argue the snapshot makes it
  value-like (immutable until `refresh`). We make it an **`Object`** (a live
  cursor that holds a subject reference and re-snapshots in place), consistent
  with the three navigators which are all `Object` handles; the *records* it hands
  back (`InspectorField`) are the `Value`s. This keeps the Object-handle /
  Value-record split uniform across all four navigation tools.
- **`evaluate:` out-of-band vs live.** A Smalltalker expects the Pharo doIt to hit
  the live object. A BEAM operator wants no accidental mutation of a production
  actor. We resolve the tension by *defaulting* to the safe out-of-band form and
  giving the live form a distinct, deliberate name (`evaluateLive:`) — the
  developer chooses the hazard explicitly.
- **Poll vs push for v1.** The Cockpit live-flash design wants push; the cost of
  publishing on every mutation wants opt-in. v1 ships poll (immediate, proven);
  push is specified for BT-2489 behind an `observable:` opt-in.

## Alternatives Considered

See Steelman for the four core-type alternatives (a JSON-only, b per-class,
c protocol, d raw-reflection). Three further points:

### Keep `inspect -> String`; add a new `inspector` verb
Leave `inspect` returning a string (no breaking change) and introduce
`anObject inspector` / `Inspector on:` as the navigable entry.
- **Pro:** zero churn on the 14 `inspect -> String` overrides; no gradual-typing
  crash risk; `inspect` keeps meaning "give me a string."
- **Con:** strands a well-known verb on the *wrong* behaviour permanently —
  `inspect` *should* open the inspector (Pharo, and the name itself promises it),
  which is precisely why ADR 0094 *reserved* it for this. Adding `inspector`
  alongside a string `inspect` perpetuates the name/behaviour mismatch ADR 0094
  set out to fix.
- **Disposition: rejected.** The user chose to **repurpose `inspect`** — this ADR
  owns the breaking change and its migration (below), which is exactly the
  ownership ADR 0094 assigned to "the follow-up."

### Push-first live updates (no poll)
Make every actor observable and push `ObjectStateChanged` from the start.
- **Pro:** the live-flash UX is automatic; no timer.
- **Con:** a per-mutation publish cost on *every* actor whether observed or not;
  couples the Inspector's v1 to a new declarative `observable:` member (parser/
  semantic/codegen) — the same scope creep ADR 0093 deferred for `event:`.
- **Disposition: deferred to BT-2489** as opt-in push; v1 polls.

### Inspector subscribes to ADR 0093 `SystemAnnouncer` for refresh (no new event)
Reuse existing system events (`ActorSpawned`/`BindingChanged`) to trigger refresh.
- **Con:** none of the existing events is *per-object state-changed* — they are
  lifecycle/binding events. There is no signal for "this actor's slot changed"
  without the new `ObjectStateChanged` (above), which is the BT-2489 work.
- **Disposition:** the seam (subscribe to `ObjectStateChanged` on `SystemAnnouncer`)
  is specified (§6); the event itself is BT-2489.

## Consequences

### Positive
- First navigable object model in Beamtalk: drill, evaluate-in-context, refresh —
  the substrate the Cockpit Inspector pane (BT-2486/2492) and a future debugger
  need.
- One `Inspector` API renders to REPL / MCP / browser, so the three surfaces
  cannot drift (one snapshot, one windowing, one cycle-guard, one eval-routing).
- Realises ADR 0094's reserved end-state for `inspect` and supersedes the flat
  agent-only `"op": "inspect"` with a navigable tree.
- Safe by construction: lazy guarded snapshots (no hot-path state reads, no hang
  on a wedged actor), out-of-band eval default (no accidental mutation), sealing
  for credential actors, bounded windowing + cycle-guard for big/cyclic graphs.
- Per-class customisation (`inspectorFields`) without a class-explosion; immutable
  `InspectorField` records diff trivially for live-flash.

### Negative
- **Breaking `inspect` return-type change** (`String` → `Inspector`), the change
  ADR 0094 deferred. Owns: removing the 14 `inspect -> String` stdlib overrides;
  the **gradual-typing crash audit** — a leftover `… ++ x inspect` compiles but
  crashes at runtime, so every `inspect`-result-used-as-string site must be found
  (ADR 0094 Critical Risk #2); the `SystemNavigation.bt` `runtimeCalledSelectors`
  `#inspect` entry; and the `"op": "inspect"` (REPL protocol) vs `inspect`-method
  naming unification (ADR 0094 Critical Risk #6). Mitigation: ADR 0094 already
  decoupled structural recursion from `inspect` (it recurses via `printString`),
  so display printing cannot break.
- A new runtime debug-eval channel for `evaluateLive:` (a reserved actor call that
  evaluates an expression in the actor's context) — the heaviest new mechanism,
  and a capability that must be gated under ADR 0091 (it runs arbitrary code in a
  live actor). It is phaseable: out-of-band `evaluate:` and all of drill/render
  ship without it.
- Two snapshot-capture paths to keep coherent with ADR 0092 (`sys:get_state` here
  vs `sys:get_status` there) — both guarded the same way, but a shared guard
  helper is warranted.
- Snapshots are stale by construction (ADR 0092's caveat): `i count` after the
  actor moved on shows the snapshot until `refresh`. Documented, not a bug.

### Neutral
- Adds two stdlib classes (`Inspector`, `InspectorField`) + one runtime primitive
  (the snapshot/derive/window/eval shim) + the optional `inspectorFields` hook and
  `sealedFromInspection` modifier.
- `sealedFromInspection` is a new class modifier (parser/semantic flag) — small
  but it *is* a grammar touch, unlike ADR 0092/0093 which were parser-free.
- Live push (BT-2489) and the `observable:` member are out of scope; v1 polls.

## Implementation

High-level, downstream of acceptance (separate issues via `/plan-adr`). Sequenced
so the cheapest thing that proves the design ships first.

0. **Phase 0 (napkin).** Runtime primitive returning the **flat
   `List(InspectorField)`** for a `#value` subject (pure structural traversal) and
   for an `#actor` (guarded `sys:get_state` snapshot), plus a minimal `Inspector`
   `Object subclass` with `on:`/`fields`/`printString` (depth-1 text tree) and
   `InspectorField` `Value subclass`. This alone gives the REPL a drillless tree
   and de-risks the snapshot guard + kind classification. If awkward, the design
   is wrong.
1. **Navigation core.** `at:` (drill → child cursor, `Result`), `parent`/`root`/
   `path`, the per-traversal **cycle seen-set**, and `refresh` (re-snapshot in
   place). Reuse ADR 0092's kind classification and ADR 0094's cycle-guard.
2. **Collections + windowing.** `#collection` derivation, page size 50, `page:`,
   direct-index `at:`, and `asTree`'s `childCount` + page-token lazy children (§8).
3. **Evaluate.** `evaluate:` (out-of-band: compile expr with `self` = snapshot/
   value, eval in the worker; `needs_live_eval` for live-only expressions). Then
   `evaluateLive:` (reserved guarded actor debug-eval call) — gated under ADR 0091,
   phaseable.
4. **`inspect` repurpose + migration.** `Object >> inspect` → `Inspector on: self`;
   remove the 14 `inspect -> String` overrides; **audit every
   `inspect`-result-used-as-string** site (ADR 0094 Risk #2); update
   `runtimeCalledSelectors`; unify/document the `"op": "inspect"` protocol op vs
   the method (ADR 0094 Risk #6).
5. **Foreign + sealing.** `#foreign` (`process_info` + guarded `sys:get_state`);
   `sealedFromInspection` class modifier (parser/semantic flag + runtime check) →
   `#sealed` redacted inspector.
6. **Surfaces.** MCP: replace flat `"op": "inspect"` with `asTree` (depth + lazy
   children); REPL: `printStringExpanded:`/`expand:`; **update
   `docs/development/surface-parity.md`** (inspect → cross-surface navigable view).
   Browser (Cockpit) consumes `asTree`/`at:`/`evaluate:`/`refresh` — BT-2486/2492.
7. **Live push seam (defines BT-2489, does not implement it).** Document the
   `ObjectStateChanged` schema and the `observable:` opt-in; leave v1 poll-only.
8. **Docs + e2e.** Language-features chapter (Inspector protocol), BUnit (value/
   actor/collection drill, cycle back-ref, window pagination, sealed redaction,
   out-of-band vs live eval, busy-actor `unavailable`), REPL e2e btscript.

Affected components: **stdlib** (`Inspector`, `InspectorField`, `Object >>
inspect`, the `inspectorFields` hook), **runtime** (snapshot/derive/window/eval
shim; `sys:get_state` guard shared with ADR 0092), **parser/semantic** (only the
`sealedFromInspection` modifier), **MCP/REPL/LSP surfaces**, **docs/tests**.

## Migration Path

- **`inspect` callers expecting a `String`.** This is the breaking change. Every
  `… ++ anObject inspect` or `anObject inspect , …` must move to
  `anObject printString` (the structural string, ADR 0094) before the contract
  flips, or it compiles and crashes at runtime (gradual typing). Phase 4 greps all
  call sites and removes the 14 `inspect -> String` overrides together with the
  audit.
- **Agents/tools using `"op": "inspect"` flat JSON.** `asTree` is a superset (the
  flat map is depth-0). Consumers move to `asTree`'s tree shape; the flat op can be
  kept as a thin compatibility shim during transition if needed.
- **`Transcript show: anObject` (human path).** Unchanged — that uses
  `printString`/`displayString` (ADR 0094), not `inspect`.
- **Custom `printString` overrides.** Unaffected — display is ADR 0094's concern.

## References
- Related issues: BT-2397 (this ADR), BT-2489 (Cockpit Phase 3 backend —
  per-object change subscriptions + pid stats; *blocked by* this ADR, owns the
  push-update follow-up), BT-2492 (Cockpit Phase 3 — Inspector live tracking),
  BT-2486 (Cockpit Phase 1 — docked Inspector), BT-2482 (Cockpit epic),
  BT-2396 (Announcements ADR, the push substrate)
- Related ADRs: ADR 0094 (printString/displayString — *reserved* `inspect` for
  this redesign; owns the deferred migration risks), ADR 0092 (Supervision
  Introspection — navigator/snapshot-record pattern, lazy guarded state, snapshot
  semantics, read-vs-mutate rule), ADR 0093 (Announcements — the live-push
  substrate, §6 seam), ADR 0067 (state/field keywords by class kind), ADR 0042
  (immutable values / actor-only mutable state — why value cycles are rare),
  ADR 0036 (Full Metaclass Tower — reflection foundation), ADR 0033 (Runtime-
  Embedded Documentation), ADR 0085 (Editor Live-Image Representation — consumer),
  ADR 0091 (Remote Workspace Access — `evaluateLive:` and sealing gating),
  ADR 0068 (structural protocol conformance — the `inspectorFields` hook)
- Code: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_actors.erl:57`
  (current `"op": "inspect"`, `sys:get_state`), `stdlib/src/Value.bt`
  (`inspect`/`printString`), `stdlib/src/Object.bt` (`inspect` default),
  `stdlib/src/SystemNavigation.bt` (`runtimeCalledSelectors`),
  `docs/development/surface-parity.md:86,245` (inspect row + deferral note)
- External: Pharo Inspector (`Spec2-Inspector`); Newspeak Hopscotch; Phoenix
  LiveDashboard process detail; BEAM `:observer`
  (`erts/lib/observer/src/observer_procinfo.erl`); IPython `_repr_html_`
