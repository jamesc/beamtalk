# ADR 0095: Rich Navigable Inspector — Drillable, Live-Refreshing Object Views

## Status
Implemented (2026-06-11)

## Implementation Tracking

**Epic:** [BT-2501](https://linear.app/beamtalk/issue/BT-2501) — Rich Navigable Inspector v1 (ADR 0095)
**Design issue:** [BT-2397](https://linear.app/beamtalk/issue/BT-2397)
**Status:** Implemented (2026-06-11)

| Phase | Issue | Title | Size | Status |
|---|---|---|---|---|
| 1 | [BT-2502](https://linear.app/beamtalk/issue/BT-2502) | Inspector core — `Inspector`/`InspectorField` classes + navigation (`#value`/`#actor`, drill, cycle guard, render) | L | Done (#2520) |
| 2 | [BT-2503](https://linear.app/beamtalk/issue/BT-2503) | Collection + foreign kinds, windowing, value `evaluate:` | M | Done (#2521) |
| 3 | [BT-2504](https://linear.app/beamtalk/issue/BT-2504) | Repurpose `inspect` → `Inspector` — migration, lint, MCP/REPL surfaces | L | Done (#2522) |
| 4 | [BT-2505](https://linear.app/beamtalk/issue/BT-2505) | Language docs, BUnit, REPL e2e btscript; flip ADR → Implemented | M | Done |

```text
Wave 1: BT-2502  (#2520, merged)
Wave 2: BT-2503  (#2521, merged) → BT-2504 (#2522, merged)   (serialised — both touch the Inspector class)
Wave 3: BT-2505                   (epic closer → flips Status to Implemented)
```

**Deferred follow-ups (ADR §7 — not in this epic):** actor evaluate-in-context
(snapshot-proxy / live routing, ADR 0091 gated), per-class `inspectorFields` custom
views, `sealedFromInspection` (parser-free class-side method), and push live
updates ([BT-2489](https://linear.app/beamtalk/issue/BT-2489)). Browser consumer:
Cockpit [BT-2486](https://linear.app/beamtalk/issue/BT-2486) /
[BT-2492](https://linear.app/beamtalk/issue/BT-2492).

## Context

### The problem

Beamtalk can *render* an object (ADR 0094: `printString` →
`Point(x: 3, y: 4)` / `Actor(Counter, 0.123.0)`) and it can *enumerate the
running system* (ADR 0092 `ProcessNavigation`; ADR 0093 `AnnouncementNavigation`).
What it cannot do is let a developer **navigate into a single object** the way a
Pharo Inspector does:

- **Drill in.** Open a field's value as its own inspectable view, recursively.
- **Live refresh.** Re-read an actor's state and see what changed.
- **Evaluate in context.** A doIt pane where expressions evaluate with `self`
  bound to the inspected object.

Today there are two impoverished paths and nothing in between:

1. **Agent-only typed JSON.** The REPL `"op": "inspect"`
   (`beamtalk_repl_ops_actors.erl:57`) does a guarded `sys:get_state(Pid, 5000)`
   and returns the actor's user fields as a flat JSON map. It is one-shot, flat
   (no drill), actor-only, and per the surface-parity doc is deliberately
   *agent-only* (`docs/development/surface-parity.md:86`).
2. **Humans get `printString`.** `Transcript show: anObject` gives the one-line
   structural string from ADR 0094 — legible, but a dead end: you cannot click a
   field, cannot watch it change, cannot evaluate against the object.

The data needed already exists — `Value` field slots, actor state behind
`sys:get_state`, `erlang:process_info`, the metaclass tower (ADR 0036), and the
runtime structural renderer (ADR 0094). What is missing is the **language-level
API** that composes them into a navigable object model that every surface
(REPL text tree, MCP/browser tree) renders from one source.

This is the surface the Cockpit LiveView IDE's Inspector pane (BT-2486, BT-2492)
and a future Pharo-style debugger sit on top of. This ADR designs that API, scoped
to a **lean v1** — drill, render, refresh — with the heavier capabilities
(evaluate-in-context for actors, per-class custom views, sealing, push updates)
deferred behind documented seams (§7).

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

### Reuses the navigator conventions (without being a fourth navigator)

Beamtalk exposes runtime structure along three axes that share one shape — a
**navigator** (`Object`) handing back **immutable snapshot records** (`Value`):
`SystemNavigation` (static code, ADR 0087), `ProcessNavigation`
(`SupervisionNode`, ADR 0092), `AnnouncementNavigation` (`SubscriptionNode`,
ADR 0093). The Inspector is *not* a fourth navigator — those each enumerate a
*system-wide collection*, while the Inspector is a **cursor over one object** and
drills *into* it. But it reuses their hard-won conventions verbatim:
`Object`-handle vs `Value`-record split, lazy/timeout-guarded state capture,
snapshot-then-refresh semantics, the read-vs-mutate rule, and a typed
`asDictionaries` wire form.

### Constraints

1. **Actor opacity (ADR 0094, ADR 0067).** An actor's state lives behind its
   message boundary. Reading it means a synchronous `sys:get_state` — racy,
   blocking on a busy actor, a confidentiality concern. State capture must be
   **lazy, timeout-guarded, and snapshot-based**, never on the display hot path —
   the same discipline ADR 0092 applies to `sys:get_status`.
2. **No self-deadlock on the walk.** Like the three navigators, the traversal runs
   as plain code in the calling eval worker, never routed through a class-side
   gen_server — or inspecting the eval worker's own supervisor would deadlock.
3. **Three-kind data model (ADR 0067).** `Value` (immutable `field:` slots),
   `Actor` (`state:` behind a process), plain `Object` (foreign handle), *plus*
   genuinely foreign OTP processes (package gen_servers) with no Beamtalk wrapper.
4. **No parser/codegen changes in v1.** Like ADR 0092/0093, v1 is stdlib +
   runtime only. (Anything needing a grammar touch — e.g. an inspection-sealing
   modifier — is deferred, §7.)
5. **Surface parity (CLAUDE.md).** One `Inspector` drives REPL and MCP/browser;
   the navigation API is shared, only the rendering is per surface.
6. **Bounded by construction.** Large collections, deep nesting, and cyclic graphs
   must never hang or blow memory — reusing ADR 0094's depth/width/length caps,
   plus pagination and a cycle guard.

## Decision

Introduce a single polymorphic **`Inspector`** — a `sealed typed Object subclass`
(a live cursor handle, like the three navigators) — returned by `anObject inspect`
and `Inspector on: anObject`. It exposes a uniform navigation protocol over all
subject kinds (Value, Actor, foreign process, collection). Drilling returns a new
`Inspector` cursor; fields are immutable `InspectorField` **Value** records. Actor
state is a lazy, timeout-guarded snapshot. **Live updates are poll-only**
(`refresh` re-snapshots). `evaluate:` is supported for **values** (pure eval, `self`
= the value); **actor expression-eval is deferred** (§7) — v1 inspects actor state
by *drilling*, not by evaluating against the live process. The design has **no
parser/codegen changes**.

### 1. Core type — one `Inspector`, polymorphic over subject kind

```beamtalk
sealed typed Object subclass: Inspector
  // construction
  class on: anObject               -> Inspector       // explicit; `anObject inspect` is the shorthand
  // identity
  subject                          -> Object           // the inspected object (or snapshot for actors)
  kind                             -> Symbol            // #value #actor #foreign #collection
  // navigation
  fields                           -> List(InspectorField)    // drillable field/element records
  at: aName                        -> Result(Inspector, Error) // drill into one field → child cursor
  parent                           -> Inspector | nil    // the cursor we drilled from (nil at root)
  root                             -> Inspector          // the top of this drill chain
  path                             -> List(Symbol)       // breadcrumb from root (for UI)
  // value-only evaluate-in-context (actors deferred, §7)
  evaluate: src :: String          -> Result(Object, Error)
  // poll-based liveness (§5)
  refresh                          -> Inspector          // a cursor on a freshly-captured snapshot
  // rendering
  printString                      -> String             // indented text tree, depth-1 (REPL)
  printStringExpanded: depth :: Integer -> String        // text tree to N levels
  asDictionaries                   -> List(Dictionary)    // typed records → wire form (MCP/browser)
```

`inspect` is **repurposed** (the ADR-0094 deferral): `Object >> inspect` now
returns `Inspector on: self`, not a `String`. `Inspector on:` is the explicit
spelling; `anObject inspect` is the shorthand.

**The cursor is immutable.** Every navigation message returns a *new* `Inspector`
— `at:` returns a child cursor (with `parent` set), `refresh` returns a cursor on
a freshly-captured snapshot of the same subject, `page:` (§6) returns a cursor on
the next window. Nothing mutates in place. This keeps the model easy to reason
about (a cursor is a value-like view) and lets a UI hold several cursors at once.

**Why one polymorphic class, not per-class inspectors or a bare protocol.** A
single `Inspector` gives newcomers *one* thing to learn and realises ADR 0094's
stated end-state verbatim. The `kind`-based field derivation (§3) is a switch over
a **closed, runtime-owned set of four kinds** — appropriate to centralise in one
place — not over the open set of user classes (which is what per-class inspector
subclasses would fan out, §Steelman). Per-class *custom views* are a natural
extension point (`inspectorFields`, §7) but are **not v1**: the kind-derived
fields cover the common case.

**One class with a `kind` tag — not a subclass per kind.** `Inspector` is *not*
split into `ValueInspector`/`ActorInspector`/`CollectionInspector`/
`ForeignInspector`. This is the same fork ADR 0092 faced and settled: its
`SupervisionNode` carries *five* `kind` values (`#beamtalkActor`/
`#beamtalkSupervisor`/`#otpSupervisor`/`#otpProcess`/`#restarting`) in **one
record**, with classification done in the runtime shim — not five subclasses. We
mirror that exactly, for three reasons: (1) the kinds barely diverge in *public
protocol* — all answer `fields`/`at:`/`path`/`refresh`/`asDictionaries`; the only
kind-specific message is collection `page:` (a `Result error:` elsewhere), too
little to justify a hierarchy; (2) the work that genuinely varies — subject
classification and actor state capture — lives in the **runtime shim** (reusing
ADR 0092's kind machinery), so there is no big `case` in Beamtalk `Inspector`
code, just a tagged `InspectorField` list coming back; (3) a fat abstract base +
four thin leaves overriding ~one method each is more surface (four more
`generated_builtins.rs`/`build_stdlib.rs` registrations, four files) for no
behavioural gain, against the lean-v1 goal. Subclasses would earn their keep only
if a kind exposed a *different protocol*, which none does. Keeping `Inspector` one
class also keeps the user-visible type stable — `inspect` always returns "an
`Inspector`," never a kind-specific class the caller must not depend on.

### 2. The field record — `InspectorField` (`Value`)

Each drillable thing is an immutable record, mirroring `SupervisionNode` /
`SubscriptionNode`:

```beamtalk
sealed typed Value subclass: InspectorField
  field: name      :: Object  = nil   // Symbol (slot) | Integer (element index) | key
  field: label     :: String  = ""    // display label (e.g. "x", "[0]", "#key →")
  field: value     :: Object  = nil    // the field's value
  field: kind      :: Symbol  = #slot  // #slot #element #association #processInfo
  field: drillable :: Boolean = true   // false for leaf scalars (numbers, atoms, booleans)
```

`fields` is the **flat record API** the acceptance criteria asks for; `at:` is the
**navigation** layer on top — the same flat/navigable dual `ProcessNavigation`
strikes (`tree nodes` vs `tree root`). A surface that just lists renders `fields`;
one that drills calls `at:`.

**Read-vs-mutate rule (ADR 0092 §3a, applied identically).** An `InspectorField`
is a frozen fact and read-only. To *change* something you discovered, cross back
to the live object and send it a message. Snapshot facts stay on the record;
mutation stays on the live subject.

### 3. Field derivation by subject kind

`Inspector kind` classifies the subject (reusing ADR 0092's kind machinery for
processes) and derives `fields`:

| `kind` | Subject | `fields` are… | `at:` drills to… |
|---|---|---|---|
| `#value` | a `Value` instance | its `field:` slots (sorted, ADR 0094 order) | the slot's value |
| `#actor` | a Beamtalk `Actor` | state slots from a **lazy snapshot** (§4) | the slot's value |
| `#collection` | `List`/`Array`/`Set`/`Dictionary`/`Bag` | windowed elements / associations (§6) | the element / value |
| `#foreign` | a non-Beamtalk OTP process (`Pid`) | best-effort `process_info` + guarded `sys:get_state` | the value where derivable |

`#value` is pure structural traversal (no process contact). `#actor`/`#foreign`
read a snapshot (§4). A drilled scalar (number, atom) is a leaf
(`drillable: false`).

### 4. Actor & foreign state — lazy, timeout-guarded snapshot

Constructing an `Inspector on: anActor` does **not** read state. State is captured
the first time `fields` (or `at:`) needs it, via a guarded
`sys:get_state(Pid, Timeout)` (the primitive the current `"op": "inspect"` already
uses, `beamtalk_repl_ops_actors.erl:71`) wrapped in the ADR-0092 guard:

- alive + responsive → a **frozen snapshot** of the user state slots; `fields`
  reads from it, reused until `refresh`.
- busy past the timeout, dead, or not `sys`-compliant → `fields` returns a single
  diagnostic `InspectorField` (`name: #status, value: #unavailable`), **not** a
  crash — ADR 0092's "returns a marker, never raises."

This makes the Inspector safe to open on any actor: a wedged actor degrades to
"state unavailable" instead of hanging the REPL, the same guarantee ADR 0094's
display path gives. The guard helper is **shared with ADR 0092's `sys:get_status`
guard** (one place that knows how to safely interrogate a live process).

**Foreign processes** (`#foreign`): `process_info(Pid, [registered_name,
current_function, message_queue_len, …])` plus a guarded `sys:get_state` when the
process is `sys`-compliant; fields are best-effort, tagged `kind: #processInfo`.

**Snapshot semantics (ADR 0092 §4, inherited).** The captured state is a
point-in-time truth. Once frozen, drilling never re-contacts the actor, so the
walk is internally consistent and cannot deadlock or observe mid-mutation. To see
later state, `refresh`.

### 5. Live updates — poll-only

The Inspector is **snapshot-then-refresh**, matching `ProcessNavigation`
(ADR 0092 §4) and Pharo's manual-refresh Inspector:

```beamtalk
i := aCounter inspect          // snapshot at T0
aCounter increment             // state changes
i fields                       // still shows T0 (frozen snapshot)
i refresh fields               // a fresh cursor at T1
```

A LiveView pane that wants a "live" feel re-issues `refresh` on a timer (cheap —
one guarded `sys:get_state`) and diffs snapshots for a field-flash effect — what
LiveDashboard does and what the Cockpit Phase 3 design (BT-2492) calls for.

**Push-based live tracking is a documented follow-up (BT-2489), not v1.**
Publishing on *every* actor state mutation is expensive, so the eventual design is
**opt-in**: an actor declared observable publishes an `ObjectStateChanged`
announcement on `SystemAnnouncer` (ADR 0093 substrate) after each committed state
write, and a subscribed Inspector refreshes reactively. The schema is specified
now so BT-2489 has a target:

```beamtalk
// Deferred to BT-2489 — specified here, not shipped in v1.
sealed Announcement subclass: ObjectStateChanged
  field: pid          :: Pid    = nil
  field: actorClass   :: Class  = nil
  field: changedSlots :: List(Symbol) = #()   // #() = "unknown, refresh all"
```

v1 ships poll-only because it unblocks the Inspector and the Cockpit panes
immediately, push's cost/opt-in trade is its own design, and poll is the proven
LiveDashboard/Pharo model. This ADR *blocks* BT-2489 (it defines the seam) without
depending on it.

### 6. Large collections — windowing & pagination

A `#collection` subject does not materialise every element. `fields` returns a
**window** (default page size **50**, matching ADR 0094's width cap / Elixir's
`:limit`); beyond it the cursor exposes a next page:

```beamtalk
big := (1 to: 100000) asList inspect
big size                       // => 100000   (cheap — collection size, not a walk)
big fields size                // => 50        (first window)
big page: 2                    // => a new Inspector cursor on elements [50..99]
big at: 73                     // => Inspector on the 74th element (direct index, no walk)
```

`at:` on a collection indexes directly (no full traversal). `asDictionaries` emits
the window plus `childCount` and the page index, so the MCP/browser layers fetch
subsequent pages lazily — the lazy-children pattern the acceptance criteria asks
for, reusing ADR 0092's `simple_one_for_one` child-cap precedent (don't copy a
giant child list out in one shot).

### 7. Cross-surface rendering — one cursor, two renderers

| Surface | Renders via | Shape | Drill |
|---|---|---|---|
| **REPL** | `printString` (depth-1) / `printStringExpanded:` | indented text tree | `i at: #foo` typed at the prompt |
| **MCP / browser** | `asDictionaries` | typed records serialised to JSON; large nodes carry `childCount` + page index | tool/UI re-issues `at:` / `refresh` |

REPL default depth is **1** (the object and its immediate fields; deeper on
`printStringExpanded:`), keeping a `printString`-at-the-prompt terse while
ADR 0094's inline structural `printString` remains the one-line form.

**Wire form is the typed records, not an ad-hoc dictionary.** `asDictionaries`
serialises the cursor's `InspectorField` list (each record → a dictionary of its
named, typed fields, plus the cursor's `kind`/`path` and any window
`childCount`/page index) — the *exact* pattern `SupervisionNode asDictionaries`
(ADR 0092) and `SubscriptionNode` (ADR 0093) use, so the schema is pinned by the
typed records, not redefined per surface. The current MCP/REPL flat
`"op": "inspect"` JSON is **superseded** by `asDictionaries` (a navigable tree of
which the flat map is depth-0); a thin compatibility shim can keep the old op
during transition.

### Deferred to follow-ups (explicitly out of v1 scope)

These are designed-against seams, not v1 deliverables — each carries its own risk
that would dilute this ADR's review surface, and v1 (drill + render + refresh)
is useful without any of them:

1. **Evaluate-in-context for actors.** v1 `evaluate:` is **values-only** (`self` =
   the value, a pure compiled dispatch — values are real Beamtalk objects, so this
   is sound). For an **actor**, "evaluate `self count > 10`" cannot bind `self` to
   a frozen state map: compiled method dispatch (`beamtalk_actor:call(Self, …)`)
   needs a live process, and a snapshot is a plain term. Two honest options exist
   — a **snapshot-proxy** (re-instantiate the class in an ephemeral isolated
   process seeded with the snapshot, eval there; mutations discarded, outbound
   sends to peers are real) and **live routing** (a reserved guarded debug-eval
   call into the real actor; mutations real). Both need new runtime machinery and
   ADR 0091 gating (running arbitrary code in/against an actor). **Deferred to its
   own issue** — v1 inspects actor state by drilling (`at:`), which covers the
   primary "what's in this field" need.
2. **Per-class custom views (`inspectorFields`).** A class could override the
   kind-derived fields with a curated view (Pharo's custom-inspector feature),
   checked via `respondsTo: #inspectorFields`. Deferred — the generic kind
   derivation (§3) covers v1; the hook is a clean additive extension later.
3. **Sealing (`sealedFromInspection`).** Refusing inspection for credential-bearing
   actors. Deferred because it is the **only** part needing a grammar touch (a new
   class modifier), and keeping it out preserves v1's "no parser changes." Until
   it lands, confidentiality on the **remote** surface is enforced where it
   matters anyway — by **ADR 0091 op-gating** of the remote `inspect` op (the in-
   image caller is already Owner/`eval`-privileged and can read state regardless,
   per ADR 0091/0093's stance). When sealing does land it should be a **class-side
   method** (`class sealedFromInspection -> Boolean => true`), not new syntax, so
   it stays parser-free; and because it is a class-side method any caller could
   read, it is honestly a *tooling* control, not a capability boundary.
4. **Push live updates** (BT-2489) — §5.

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

bt> i evaluate: "self dist: (Point x: 0 y: 0)"   "value eval — self = the Point"
Result ok: 5.0

bt> c := Counter spawn
Actor(Counter, 0.123.0)
bt> c increment; increment
bt> ci := c inspect            "lazy snapshot on first `fields`"
Inspector(Counter)
  count: 2

bt> ci at: #count
Inspector(Integer = 2)
bt> c increment                 "state moves on"
bt> ci refresh at: #count       "re-snapshot"
Inspector(Integer = 3)
```

### Error / edge behaviour

```beamtalk
bt> (busyActor inspect) fields
// => [ InspectorField(status: #unavailable) ]   "sys:get_state timed out — no hang, no raise"

bt> (aCounter inspect) at: #nonesuch
// => Result error: (beamtalk_error no_such_field)

bt> (aCounter inspect) evaluate: "self increment"
// => Result error: (beamtalk_error actor_eval_unsupported)
//      "evaluate: runs against values; actor evaluate-in-context is not in v1 — drill with at:"
```

## Prior Art

| Source | Model | What we take / leave |
|---|---|---|
| **Pharo Inspector** (`Spec2-Inspector`) | `anObject inspect` opens a window; per-class custom tabs; doIt pane with `self`; manual refresh; per-traversal cycle handling. | **Take:** `inspect` as the verb, manual refresh, the seen-set, drill-into-fields. **Adapt:** one polymorphic `Inspector` (window/Model is the surface, not the API); actor state is a *guarded snapshot*, not a live image read. **Defer:** the doIt pane (value-only in v1) and custom tabs (`inspectorFields`, §7). |
| **Newspeak Hopscotch** | Composable navigable presenters; drilling pushes a new presenter onto a path. | **Take:** drill = push a child cursor; `path` as a first-class breadcrumb. **Leave:** the presenter-composition UI framework — that's the browser layer. |
| **Smalltalk-80 Browser** | Class/protocol/method navigation panes. | **Take:** navigation-by-drilling idiom. **Leave:** it's a code browser. |
| **IPython `_repr_html_`** | Object opts into a rich per-surface representation; frontend picks text vs HTML. | **Take:** one object, per-surface rendering (`printString` vs `asDictionaries`). **Defer:** the per-class opt-in hook (`inspectorFields` ≈ `_repr_html_`). |
| **Phoenix LiveDashboard** (process detail) | LiveView render of `process_info`/`sys:get_state`, periodic re-poll, lazy detail. | **Take:** poll-refresh as the v1 live model; lazy/guarded state; foreign-process detail. **Confirms:** push events are *not* required for a usable live pane. |
| **BEAM `:observer`** (process tab) | `process_info` + `sys:get_state` on selection; mixes process kinds; lazy state. | **Take:** the exact primitive set; lazy state-on-select; foreign processes. **Leave:** wx GUI — we expose the *data*, the IDE renders it. |
| **Elixir `IEx` `i`/`Inspect`** | `i/1` describes a term; `Inspect` is bounded structural rendering with `:limit`. | **Take:** bounded rendering and the window size (adopted in ADR 0094). |

**Net:** Pharo's Inspector *navigation* (drill, refresh, cycle-safety),
`:observer`/LiveDashboard's *safe BEAM state capture* (guarded snapshot, lazy,
poll-refresh, foreign processes), IPython's *one-object/per-surface-render* split
— exposed as one message-passing `Inspector`, with the heavier doIt/custom-view
features deferred (§7).

## User Impact

- **Newcomer.** `anObject inspect` does the obvious thing — a navigable view in the
  IDE, an indented tree in the REPL. `at:`/`fields` are the same message idioms
  used everywhere; drilling teaches object structure interactively. Getting it
  wrong is gentle: a wedged actor shows "unavailable", a bad field name returns a
  `Result error:`, never a crash.
- **Smalltalk developer.** It *is* the Pharo Inspector for navigation — `inspect`
  opens it, drill into fields, manual refresh. The doIt pane is value-only in v1
  (actor evaluate-in-context deferred, §7) — a known gap, justified by actor
  opacity (Pharo has no message boundary to respect; Beamtalk does).
- **Erlang/BEAM developer.** Under the hood it's `sys:get_state` + `process_info`,
  guarded and lazy — the same primitives `:observer` uses, no bookkeeping process.
  Foreign gen_servers inspect correctly as `#foreign`. FFI is still there for raw
  terms.
- **Operator.** Opening an inspector never reads state on the hot path and never
  hangs on a busy actor (timeout-guarded). v1 cannot mutate a production actor at
  all (no actor eval), and the remote `inspect` op is ADR-0091 gated. Refresh is a
  cheap single guarded call.
- **Tooling developer.** One `Inspector` drives REPL text and the MCP/LiveView tree
  via typed `asDictionaries` records — write navigation once, render two ways. The
  immutable `InspectorField` records are trivial to diff for field-flash (BT-2492),
  and `path` + lazy windowing make a 100k-element list renderable.

## Steelman Analysis

### (a) JSON-only — keep `"op": "inspect"` as the flat typed map (status quo)
- ⚙️ **BEAM veteran:** "One `sys:get_state` → a JSON map. No new classes. The agent
  already parses it."
- 🏭 **Operator:** "Smallest surface; read-only and flat, nothing to drill that
  could misbehave."
- *Why rejected:* one-shot, flat, actor-only, explicitly agent-only — it cannot
  drill or refresh, the core ask. `asDictionaries` subsumes it (the flat map is
  depth-0).

### (b) Per-class inspector classes (`CounterInspector`, …) — Pharo's literal model
- 🎩 **Smalltalk purist:** "This is how Pharo does custom inspection — a subclass
  per type. Maximum flexibility."
- 🎨 **Language designer:** "Each class owns its view; no central type switch."
- *Why rejected:* it fans a parallel hierarchy across the **open set of user
  classes** for a feature most classes want *generically*. The chosen design's
  internal `kind` switch (§3) is admittedly a type switch too — but over a
  **closed set of four runtime-owned kinds**, which is exactly the case where
  centralising is right; open-set per-class variation is served instead by the
  additive `inspectorFields` hook (§7) without a class explosion. So the
  customisation Pharo gets from subclasses, we get from one optional method — when
  we add it.

### (c) Protocol-based (`InspectableProtocol`) — no Inspector class at all
- 🎨 **Language designer:** "Define `fields`/`at:` as a protocol; any conforming
  class is auto-handled. No new object."
- 🧑‍💻 **Newcomer:** "Fewer types — I just implement an interface."
- *Why rejected:* the cursor needs *state* — the snapshot, the seen-set, the path,
  the page window. That state needs a home; a bare protocol just relocates it with
  nowhere to live. The good half of this idea is the per-class *hook*
  (`inspectorFields`, §7), which we adopt later — not making the whole inspector a
  protocol.

### (d) Raw reflection only — surfaces compose everything
- ⚙️ **BEAM veteran:** "Give me `fieldsOf:` / `stateOf:` primitives and let each
  surface build navigation. No opinionated middle layer."
- *Why rejected:* guarantees drift — REPL and MCP would each re-implement
  snapshotting, windowing, and cycle-guard slightly differently, the exact
  surface-parity failure the project fights. The point is *one* navigable model.

### Tension points
- **doIt scope.** A Smalltalker expects the doIt to hit the live object; a BEAM
  operator wants no accidental mutation of a production actor. v1 sidesteps the
  tension by shipping value-only `evaluate:` and **deferring** actor eval (§7) —
  rather than guessing the safe/live default before the Cockpit doIt pane exists to
  inform it.
- **Inspector as `Object` vs `Value`.** The cursor caches a snapshot and a window;
  making it a `Value` would force all that into returned slots. It is an `Object`
  (consistent with the navigators); the *records* it returns are the `Value`s.
- **Poll vs push.** Cockpit wants push; per-mutation publish cost wants opt-in. v1
  polls (proven); push is the BT-2489 follow-up with a specified seam.

## Alternatives Considered

See Steelman for the four core-type alternatives (a JSON-only, b per-class,
c protocol, d raw-reflection). Two further points:

### Keep `inspect -> String`; add a new `inspector` verb
Leave `inspect` returning a string and add `anObject inspector` as the navigable
entry.
- **Pro:** zero churn on the 14 `inspect -> String` overrides; no gradual-typing
  crash risk.
- **Con:** strands a well-known verb on the wrong behaviour permanently — `inspect`
  *should* open the inspector (Pharo, and the name promises it), which is why
  ADR 0094 *reserved* it for this. A parallel `inspector` perpetuates the mismatch.
- **Disposition: rejected** — repurpose `inspect` (this ADR owns the migration).

### Ship the full inspector (actor eval, custom views, sealing) in one ADR
- **Con:** actor evaluate-in-context needs a new debug-eval/proxy mechanism +
  ADR 0091 gating; sealing needs a grammar touch; custom views need the hook — each
  its own risk. Bundling them dilutes review and delays the 80%-value core.
- **Disposition: rejected** — lean v1 (drill/render/refresh) ships first; the rest
  are documented seams (§7).

### Cycle handling — identity seen-set vs depth bound
- v1 uses a **pid-keyed seen-set for processes** plus the **ADR-0094 depth/width/
  length caps for value sub-trees** (see Cycle handling below) — not a structural
  identity set, which Beamtalk doesn't expose reliably for values.

## Cycle handling & bounds

Cycles in an object graph form **only through process references** — an immutable
`Value` graph is a tree post-ADR 0042, but a `Value` field (or an actor slot) can
hold an `Actor`/`Pid`, and actor A's state can reference B whose state references
A. So the traversal:

- keys a **seen-set by pid** for every actor/foreign node it descends into; on
  revisiting a seen pid it emits a back-reference marker
  (`↩ Actor(A, 0.123.0)`) instead of recursing;
- bounds **pure value sub-trees by the ADR-0094 depth/width/length caps** (no
  identity set needed — they cannot cycle), rendering `...` at a cap.

This reuses ADR 0094's existing cycle/bound infrastructure (the structural renderer
already tracks these for inline `printString`) and is honest about the two distinct
cycle sources: process refs (pid-keyed) vs depth (capped).

## Consequences

### Positive
- First navigable object model in Beamtalk: drill, render, refresh — the substrate
  the Cockpit Inspector pane (BT-2486/2492) and a future debugger need.
- One `Inspector` API renders to REPL and MCP/browser via typed `asDictionaries`,
  so the surfaces cannot drift (one snapshot, one windowing, one cycle-guard).
- Realises ADR 0094's reserved end-state for `inspect`; supersedes the flat
  agent-only `"op": "inspect"` with a navigable tree.
- Safe by construction: lazy guarded snapshots (no hot-path reads, no hang on a
  wedged actor), no actor mutation in v1, bounded windowing + pid-keyed cycle guard.
- **No parser/codegen changes** (like ADR 0092/0093) — stdlib + runtime only.

### Negative
- **Breaking `inspect` return-type change** (`String` → `Inspector`), the change
  ADR 0094 deferred. Owns: removing the 14 `inspect -> String` stdlib overrides
  (`Array`, `Bag`, `BindingsView`, `Boolean`, `ChangeLog`, `Dictionary`,
  `Interval`, `List`, `Number`, `Object`, `Set`, `String`, `Tuple`, `Value`); the
  **gradual-typing crash audit** (a leftover `… ++ x inspect` compiles but crashes
  at runtime — ADR 0094 Risk #2); the `SystemNavigation.bt:2235`
  `runtimeCalledSelectors` `#inspect` entry; the `"op": "inspect"` (REPL protocol)
  vs `inspect`-method naming unification (ADR 0094 Risk #6). Mitigation below.
- **The migration window does not close.** Because typing is gradual and code hot-
  reloads (ADR 0093 `ClassLoaded`), there is no compile-time guard preventing a
  *future* caller from using `inspect` in string context after the contract flips.
  The audit finds today's 14+ sites; new ones can reappear. Mitigation: ship a
  **transitional lint** (a `SystemNavigation`-driven check flagging `inspect` sends
  used directly in `++`/string position) and call the contract change out in
  release notes; the structural recursion is *already* decoupled from `inspect`
  (ADR 0094 recurses via `printString`), so display printing cannot break.
- The v1 doIt is value-only — actor evaluate-in-context (the Pharo feature a
  Smalltalker reaches for) is deferred (§7).
- Snapshots are stale by construction (ADR 0092's caveat): `i` after the actor moved
  on shows the snapshot until `refresh`. Documented, not a bug.

### Neutral
- Adds two stdlib classes (`Inspector`, `InspectorField`) + one runtime primitive
  (snapshot/derive/window shim), sharing ADR 0092's `sys:get_*` guard helper.
- Deferred extension points (§7): actor eval, `inspectorFields`, `sealedFromInspection`,
  push updates (BT-2489) — each its own issue.

## Implementation

High-level, downstream of acceptance (separate issues via `/plan-adr`). Sequenced
so the cheapest proof ships first.

0. **Phase 0 (napkin).** Runtime primitive returning the **flat
   `List(InspectorField)`** for `#value` (pure structural traversal) and `#actor`
   (guarded `sys:get_state` snapshot), plus a minimal `Inspector` `Object subclass`
   (`on:`/`fields`/`printString` depth-1) and `InspectorField` `Value subclass`.
   De-risks the snapshot guard + kind classification. If awkward, the design is
   wrong.
1. **Navigation core.** `at:` (drill → child cursor, `Result`), `parent`/`root`/
   `path`, the **pid-keyed cycle seen-set** + ADR-0094 depth bounds, and `refresh`
   (cursor on a fresh snapshot). Reuse ADR 0092 kind classification.
2. **Collections + windowing.** `#collection` derivation, page size 50, `page:`,
   direct-index `at:`.
3. **Value `evaluate:`.** Compile the expression with `self` = the value, eval in
   the worker, return `Result`. Actor eval returns `actor_eval_unsupported`
   (deferred, §7).
4. **`inspect` repurpose + migration.** `Object >> inspect` → `Inspector on: self`;
   remove the 14 `inspect -> String` overrides; the gradual-typing audit +
   transitional lint; update `runtimeCalledSelectors`; unify/document the
   `"op": "inspect"` op vs the method.
5. **Foreign + surfaces.** `#foreign` (`process_info` + guarded `sys:get_state`);
   MCP `asDictionaries` (replacing flat `"op": "inspect"`, compat shim);
   `printStringExpanded:`; **update `docs/development/surface-parity.md`** (inspect
   → cross-surface navigable view). Browser (Cockpit) consumes `asDictionaries`/
   `at:`/`refresh` — BT-2486/2492.
6. **Docs + e2e.** Language-features chapter; BUnit (value/actor/collection drill,
   cycle back-ref, window pagination, busy-actor `unavailable`, value `evaluate:`);
   REPL e2e btscript.

Affected components: **stdlib** (`Inspector`, `InspectorField`, `Object >>
inspect`), **runtime** (snapshot/derive/window shim; shared `sys:get_*` guard),
**MCP/REPL surfaces**, **docs/tests**. **No parser/codegen changes.**

## Migration Path

- **`inspect` callers expecting a `String`.** The breaking change. Every
  `… ++ anObject inspect` must move to `anObject printString` (the structural
  string, ADR 0094) before the contract flips, or it compiles and crashes at
  runtime (gradual typing). Phase 4 greps all sites, removes the 14 overrides, and
  ships the transitional lint for *future* sites (the window does not close — see
  Consequences).
- **Agents/tools using `"op": "inspect"` flat JSON.** `asDictionaries` is a
  superset (the flat map is depth-0); a thin compat shim keeps the old op during
  transition.
- **`Transcript show: anObject` (human path).** Unchanged — uses
  `printString`/`displayString` (ADR 0094), not `inspect`.
- **Custom `printString` overrides.** Unaffected — display is ADR 0094's concern.

## References
- Related issues: BT-2397 (this ADR), BT-2489 (Cockpit Phase 3 backend — per-object
  change subscriptions + pid stats; *blocked by* this ADR, owns the push follow-up),
  BT-2492 (Cockpit Phase 3 — Inspector live tracking), BT-2486 (Cockpit Phase 1 —
  docked Inspector), BT-2482 (Cockpit epic), BT-2396 (Announcements ADR, the push
  substrate)
- Related ADRs: ADR 0094 (printString/displayString — *reserved* `inspect` for this
  redesign; owns the deferred migration risks), ADR 0092 (Supervision Introspection
  — navigator/snapshot-record pattern, lazy guarded state, snapshot semantics,
  read-vs-mutate rule, `asDictionaries` wire form), ADR 0093 (Announcements — the
  live-push substrate, §5 seam), ADR 0067 (state/field keywords by class kind),
  ADR 0042 (immutable values — why value graphs are trees), ADR 0036 (Metaclass
  Tower — reflection foundation), ADR 0085 (Editor Live-Image Representation —
  consumer), ADR 0091 (Remote Workspace Access — remote `inspect` op gating)
- Code: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_actors.erl:57,71`
  (current `"op": "inspect"`, `sys:get_state`), `stdlib/src/Value.bt`,
  `stdlib/src/Object.bt` (the 14 `inspect -> String` overrides live across
  `stdlib/src/*.bt`), `stdlib/src/SystemNavigation.bt:2235`
  (`runtimeCalledSelectors`), `docs/development/surface-parity.md:86,245`
- External: Pharo Inspector (`Spec2-Inspector`); Newspeak Hopscotch; Phoenix
  LiveDashboard process detail; BEAM `:observer`
  (`erts/lib/observer/src/observer_procinfo.erl`); IPython `_repr_html_`
