# ADR 0092: Supervision Tree Introspection API (Runtime Query Surface)

## Status
Accepted (2026-06-06)

## Implementation Tracking

**Epic:** [BT-2425](https://linear.app/beamtalk/issue/BT-2425) — Supervision Tree Introspection API (ADR 0092)
**Design issue:** [BT-2395](https://linear.app/beamtalk/issue/BT-2395)
**Status:** Planned

| Phase | Issue | Title | Size | Blocked by |
|---|---|---|---|---|
| 1 | [BT-2426](https://linear.app/beamtalk/issue/BT-2426) | Runtime shim: flat `default`-scope snapshot (`beamtalk_process_navigation`) | M | – |
| 1 | [BT-2427](https://linear.app/beamtalk/issue/BT-2427) | `SupervisionNode` value class + flat `Workspace processes` | M | BT-2426 |
| 2 | [BT-2428](https://linear.app/beamtalk/issue/BT-2428) | Foreign procs, `system` scope, dynamic-sup cap, `from:`, lazy `status` | M | BT-2426 |
| 3 | [BT-2429](https://linear.app/beamtalk/issue/BT-2429) | `SupervisionTree` + `ProcessNavigation` classes | M | BT-2427, BT-2428 |
| 3 | [BT-2430](https://linear.app/beamtalk/issue/BT-2430) | Repoint `Workspace processes` to `ProcessNavigation default tree` | S | BT-2429 |
| 4 | [BT-2432](https://linear.app/beamtalk/issue/BT-2432) | ADR-0091 role placement (`default`→Read, `system`→privileged) | S | BT-2430 |
| 4 | [BT-2431](https://linear.app/beamtalk/issue/BT-2431) | Surfaces: LSP/MCP/browser + surface-parity | M | BT-2430 |
| 4 | [BT-2433](https://linear.app/beamtalk/issue/BT-2433) | E2E + comprehensive tests + docs + ADR → Implemented | M | BT-2430, BT-2431 |

```text
Wave 1: BT-2426
Wave 2: BT-2427  BT-2428          (parallel)
Wave 3: BT-2429
Wave 4: BT-2430
Wave 5: BT-2432  BT-2431          (parallel)
Wave 6: BT-2433                   (epic closer → flips Status to Implemented)
```

BT-2432 additionally depends on ADR-0091 Phase-2 role infrastructure landing.

## Context

### The Problem

Beamtalk has supervision-tree *syntax* (ADR 0059, BT-448) and live runtime
supervisors (BT-1387), but there is **no Beamtalk-language API to introspect
the running supervision tree** — no way to ask, from `.bt` code or the REPL,
"what is running, under whose supervision, in what state?"

The data already exists on the Erlang side. OTP exposes
`supervisor:which_children/1`, `supervisor:count_children/1`, and
`sys:get_status/1`; BEAM's `:observer` GUI is built entirely on those
primitives. What is missing is the **Beamtalk surface** that turns those
process-level facts into a walkable, message-passing object — the data source
a LiveView IDE's Observer-style pane (ADR 0085, the "Editor Live-Image
Representation") would render, and the thing a developer reaches for at the
REPL when an actor silently vanished.

This ADR is the *dynamic process structure* counterpart to ADR 0087, which is
about *static code structure*. The two are deliberately parallel:
`SystemNavigation` answers "who implements `#asString`, who sends `#+`";
this ADR's surface answers "who is this supervisor's child, what's its restart
history, is it alive."

### Current State

The pieces that exist today, and the gap between them:

| Capability | Today | Gap |
|---|---|---|
| Declare a supervision tree | `Supervisor subclass:` / `DynamicSupervisor` (ADR 0059) | — |
| Start / attach a supervisor | `Workspace startSupervisor:` (BT-1191) | — |
| Get the root app supervisor | `Workspace supervisor` → a `Supervisor` or `nil` | Only the *root* |
| List workspace supervisors | `Workspace supervisors` → flat list | No tree, no children, no foreign procs |
| A supervisor's direct children | `aSupervisor children` → `List(Symbol)` (child **ids**) | Ids only — no pids, no state, no recursion |
| Look up one child | `aSupervisor which: aClass` → `Result(Object, Error)` | Single class, not a walk |
| List live actors | `Workspace actors` → `List(Actor)` | Flat — no supervision relationship |
| Walk the *whole* live tree | **nothing** | This ADR |
| Per-node state (`sys:get_status`) | **nothing** | This ADR |
| Foreign (non-Beamtalk) processes in the tree | **nothing** | This ADR |

So a developer can enumerate *the supervisors they personally attached* and
ask each for its child **ids**, but cannot:

- walk the tree recursively (a supervisor's child that is itself a supervisor),
- get a child's pid, registered name, restart count, or current state,
- see Erlang/OTP processes (gen_servers from packages, runtime infra) that
  share the same tree,
- distinguish "the user's app" from "the workspace's internal plumbing."

### The "split-brain" cost (same shape as ADR 0087)

In a classic Smalltalk image the process browser is a cheap in-image read.
On BEAM the live process structure lives in OTP supervisor processes, not in
the Beamtalk object graph. `supervisor:which_children/1` is the authoritative
source; any Beamtalk surface is a *mirror* that must (a) cross the
Beamtalk→Erlang boundary and (b) decide how to reconcile a structure that
mutates while you walk it. That second point is the design crux: a supervision
tree is not a stable value.

### Constraints

1. **No new runtime mechanism — wrap OTP.** Everything needed is already in
   `supervisor:`, `sys:`, and `erlang:process_info/2`. This ADR adds a thin
   Beamtalk-facing shim, not a new bookkeeping process. (Contrast ADR 0087,
   which *did* need a maintained index because re-parsing source was
   structurally too slow; reading `which_children` on a *static* supervisor is
   cheap, so no index here.) **Caveat — the dynamic case is not cheap.**
   `which_children/1` on a `simple_one_for_one` `DynamicSupervisor` with
   thousands of children (a connection pool, a per-request worker farm) copies
   the entire child list out of the supervisor process in one message, then
   the walk would `process_info` each pid — a real performance cliff OTP's own
   docs warn about. The snapshot therefore **caps** per-supervisor child
   expansion at a configurable limit (default e.g. 200): beyond it, the node
   reports `childCount` (from the cheap `count_children/1`) and a truncation
   marker instead of materialising every child. Full expansion of a large
   dynamic supervisor is opt-in (`(ProcessNavigation from: aSup limit: n)
   unwrap`).
2. **No self-deadlock on the walk.** The introspection object must run as a
   plain value in the calling eval worker, never as a class-side gen_server —
   exactly the lesson `SystemNavigation` documents
   (`stdlib/src/SystemNavigation.bt:18-23`): "routing the iteration through a
   class gen_server would self-deadlock when the walk reaches `SystemNavigation`
   itself." A process walk that reaches the eval worker's own supervisor has
   the same hazard.
3. **`sys:get_status/1` is not safe to call blindly.** It blocks, can time out
   on a busy or wedged process, and is meaningless for non-`sys`-compliant
   processes. State capture must be lazy, timeout-guarded, and optional.
4. **Mixed process kinds.** A workspace runs Beamtalk actors, Beamtalk
   supervisors, *and* foreign OTP processes (package gen_servers, the compiler
   server, the xref index, logger handlers). All appear in the same OTP tree;
   the surface must represent all of them without pretending the foreign ones
   are Beamtalk objects.
5. **OTP logger discipline** (`CLAUDE.md`): the shim uses `?LOG_*` with
   `domain => [beamtalk, runtime]`; no `io:format`.
6. **Structured errors** (`CLAUDE.md`): boundary failures (dead supervisor,
   stale pid) carry a structured `#beamtalk_error{}` reason, surfaced through
   `from:`'s `Result error: (beamtalk_error <reason>)` per the supervisor
   family convention (ADR 0080) — consistent with `whichChild`'s existing
   `stale_handle` kind.

## Decision

Introduce a dedicated value class, **`ProcessNavigation`**, as the dynamic
twin of `SystemNavigation`, returning a **snapshot tree object**
(`SupervisionTree`) of **per-node records** (`SupervisionNode`). The tree is a
high-level navigable object (collection protocol + parent/child/walk) backed by
flat, inspectable node records. State capture (`sys:get_status`) is lazy and
timeout-guarded. Foreign OTP processes appear as first-class nodes with a
distinct `kind`. Change-event streaming is explicitly **out of scope** and
deferred to the Announcements substrate (BT-2193).

### 1. Receiver — a dedicated `ProcessNavigation` value class

```beamtalk
class default                            -> ProcessNavigation               // workspace-scoped, plumbing filtered
class system                            -> ProcessNavigation               // everything, incl. runtime infra
class from: aRoot :: Supervisor | Pid   -> Result(ProcessNavigation, Error) // rooted subtree
```

`default` and `system` cannot fail (they snapshot the live tree), so they
return a `ProcessNavigation` directly and chain fluently — `ProcessNavigation
default tree`. `from:` takes a *user-supplied* root that can be the wrong type
or already dead, so it follows the supervisor family's Result convention (ADR
0080: `supervise`, `whichChild`, `terminate:` all return `Result`) and returns
`Result(ProcessNavigation, Error)`. Callers unwrap to navigate:
`(ProcessNavigation from: aSup) unwrap tree`, or `ifOk:ifError:` for a
recoverable root.

`from:` accepts **either** a Beamtalk `Supervisor` object (pid pulled from its
handle) **or** a raw `Pid` — the latter is required because `system` walks
*foreign* OTP supervisors that have no Beamtalk `Supervisor` wrapper, and a
user drilling into an `#otpSupervisor` node passes `node pid`. A pid that
turns out not to be a supervisor (a plain worker, or one that errors on
`which_children`) is **not** an error — `from:` returns `Result ok:` with a
single-node tree (recursion only descends where `which_children` succeeds).
The `aRoot :: Supervisor | Pid` annotation lets the type checker reject a
wrong-type argument (`ProcessNavigation from: 42`) **statically**, so the
common bug never reaches runtime; the `Result error: (beamtalk_error
type_error)` variant remains as the defensive runtime fallback (e.g. an
untyped `Dynamic` value flowing in via FFI). The Result error channel's
*genuine runtime* failure is an already-dead supervisor (`stale_handle`) —
the only failure the type system cannot catch.

`ProcessNavigation` is a `sealed typed Object subclass` — a value object, not
an actor — exactly like `SystemNavigation`. `default`/`system` return the
navigation directly; `tree` yields the `SupervisionTree` snapshot (see §4).
The class side is just a constructor surface; all query logic is instance-side
so it runs as plain Erlang in the calling eval worker (constraint 2).

A convenience alias lives on the existing facade, paralleling
`Workspace actors`:

```beamtalk
Workspace processes      // == ProcessNavigation default tree
```

**Why a dedicated class rather than rooting at `Beamtalk` or `Workspace`:**
the project already split *static* navigation off `Beamtalk` onto
`SystemNavigation` (BT-2189/2190) precisely so the facade stays small and the
query surface can grow scoped constructors (`SystemNavigation over: aPackage`).
Dynamic navigation gets the same treatment for the same reasons:
discoverability (`ProcessNavigation` autocompletes its whole protocol in one
place), room to grow scoped roots (`from:`), and symmetry that makes the pair
teachable as "Static → `SystemNavigation`, Dynamic → `ProcessNavigation`." The
`Workspace processes` alias preserves the one-liner ergonomics of
`Workspace actors` without bloating the facade.

### 2. The tree object — `SupervisionTree`

A navigable snapshot with the full collection protocol plus tree navigation:

```beamtalk
tree := ProcessNavigation default tree
tree root                         // => a SupervisionNode (the snapshot root)
tree size                         // => total node count
tree do: [:node | ...]            // pre-order walk over every node
tree select: [:node | node isSupervisor]
tree detect: [:node | node registeredName = #DatabasePool]
tree nodesOfKind: #beamtalkActor  // => List(SupervisionNode)
tree findClass: Counter           // => List(SupervisionNode) for Counter actors
```

`SupervisionTree` is a thin wrapper over a flat `List(SupervisionNode)` plus a
parent/child adjacency captured at snapshot time. The flat list **is** the
record API the acceptance criteria asks for; the tree object is the high-level
view layered on top. Both are available — `tree nodes` returns the flat list,
`tree root` returns the navigable head.

### 3. The node — `SupervisionNode`

Each node is an immutable record (a `Value subclass:`) carrying everything the
acceptance criteria enumerates:

```beamtalk
node pid              // => a Pid | nil   (nil for a child mid-restart, see below)
node registeredName  // => Symbol | nil   (registered name if any)
node kind            // => one of #beamtalkSupervisor #beamtalkActor
                     //            #otpSupervisor #otpProcess #restarting
node behaviourClass  // => Class | nil   (the Beamtalk class, nil for foreign)
node strategy        // => Symbol | nil  (#oneForOne … ; supervisors only)
node childCount      // => Integer       (live children; supervisors only)
node maxRestarts     // => Integer | nil (configured budget; supervisors only)
node restartWindow   // => Integer | nil (configured window, seconds; supervisors only)
node restartIntensity // => Dictionary | nil  (convenience: #{maxRestarts, restartWindow})
node childNodes      // => List(SupervisionNode)   (snapshot children)
node parent          // => SupervisionNode | nil
node isSupervisor    // => Boolean
node isBeamtalk      // => Boolean        (kind is one of the #beamtalk* kinds)
node status          // => Dictionary | nil   (LAZY — see §5)
node asSupervisor    // => Supervisor | nil    (live handle; see §3a)
```

The accessor names and shapes are **deliberately identical to the live
`Supervisor`'s** so a supervisor reads the same whether you hold it or
discovered it in a snapshot (see §3a). `childNodes` is the one structural
exception — it returns child *records* (snapshot navigation), as distinct from
the live `Supervisor children` which returns child *ids* and the class-side
`Supervisor class children` which returns child *classes*. Naming it
`childNodes` keeps `children` from meaning three different things.

`kind` is the load-bearing field: it tells a renderer whether to draw a
Beamtalk class badge (`#beamtalkActor`/`#beamtalkSupervisor`, `behaviourClass`
populated) or a foreign-process badge (`#otpProcess`/`#otpSupervisor`,
`behaviourClass` is `nil`, `registeredName`/module identify it).

**Children mid-restart.** `supervisor:which_children/1` returns
`{Id, restarting, Type, Modules}` — the literal atom `restarting`, *not* a pid
— for a child OTP is currently restarting. This is exactly the state a
developer who just witnessed a crash is most likely to catch, so it must be a
first-class outcome, not a crash in the walk. Such a node gets
`kind => #restarting`, `pid => nil`, `status => nil`, and the `Id`/`Modules`
still populate `behaviourClass` where derivable. The snapshot builder must
guard every `process_info`/`is_beamtalk_actor` call against the `restarting`
atom (never call `process_info(restarting, …)`).

```beamtalk
// Find every running Counter and its supervisor's configured restart budget:
(ProcessNavigation default tree findClass: Counter)
  collect: [:n | n parent restartIntensity]   // => #(#{#maxRestarts => 10, ...})

// Foreign processes are visible, just differently tagged:
(ProcessNavigation system tree nodesOfKind: #otpProcess) size   // => 11
```

**On restart *counts* (decided scope boundary).** OTP's *public* supervisor
API exposes the configured restart *budget* (`maxRestarts` / `restartWindow`,
already surfaced by `beamtalk_supervisor` via the class-side
`maxRestarts`/`restartWindow` methods) and live child *counts*
(`count_children`), but **not a per-child restart *history* or cumulative
restart *count*.** The running tally lives in the supervisor's private state
and is only reachable by parsing `sys:get_status/1`'s internal report —
undocumented, version-fragile, and explicitly *not* a public contract.

**Decision:** this ADR surfaces `restartIntensity` (the configured budget,
public and stable) and **per-child restart *history* is out of scope** — it
will not be sourced by parsing private supervisor state, because that couples
the introspection surface to an OTP implementation detail. If a true restart
counter is wanted later, the right shape is a **runtime-maintained tally
hooked into the supervisor's child-restart path** (the same maintained-index
pattern as ADR 0087's xref), which is its own ADR/issue, not a fragile read
bolted onto this snapshot. The `SupervisionNode` record leaves room to add
`restartCount` then without a breaking change. The acceptance criterion
"restart history" is answered as: *budget now; history is a separate,
deliberately-deferred feature with a documented reason.*

### 3a. API consistency with the live `Supervisor`

A `SupervisionNode` and a live `Supervisor` (the object you get from
`MySup supervise unwrap`) describe the *same domain thing* — a supervisor, its
strategy, its restart budget, its children — so their **read** surfaces are
kept name- and shape-identical. The split is by *role*, not by vocabulary:

| Concept | live `Supervisor` (control + self-inspect) | `SupervisionNode` (snapshot fact) |
|---|---|---|
| strategy | `strategy` (class-side config) | `strategy` |
| restart budget | `maxRestarts`, `restartWindow` | `maxRestarts`, `restartWindow` (+ `restartIntensity` dict) |
| running child count | `childCount` | `childCount` |
| is it a supervisor | `isSupervisor` | `isSupervisor` |
| children | `children` → child **ids** (`List(Symbol)`) | `childNodes` → child **records** |

Two alignment changes to the **shipped** live `Supervisor`
(`stdlib/src/Supervisor.bt`) fall out of this and are part of this ADR's scope:

- **Rename `count` → `childCount`** (`Supervisor.bt:135`) for the
  running-children count, updating its call sites and tests in the same change.
  No deprecated alias is kept — pre-1.0, a clean rename is preferred over
  carrying two names. (`count` reads as "my own count" on a non-collection;
  `childCount` is unambiguous and matches the node.)
- No change to `maxRestarts`/`restartWindow`/`strategy`/`children` — the node
  was aligned to *them*, not the reverse.

**Read vs mutate — the rule that makes the split safe.** A snapshot is a frozen
*past* truth and is **read-only**: there are no `terminate:` / `stop` /
`supervise` methods on `SupervisionNode`. To *act* on something you discovered
by walking, cross back to the live object:

```beamtalk
sup := node asSupervisor                    // live handle, or nil for foreign/dead
sup ifNotNil: [:s | s terminate: Worker]    // control goes through the live object

// foreign OTP nodes have no Beamtalk Supervisor — re-root the navigator instead:
deeper := (ProcessNavigation from: node pid) unwrap tree
```

`node asSupervisor` returns the live `Supervisor` for a `#beamtalkSupervisor`
node — by `registeredName` via `Supervisor current` when the node is named, and
by a **pid-based fallback** when it is not (an anonymous Beamtalk supervisor is
still bridged, since its `pid` is live and its class is known). It returns `nil`
only for foreign processes, dead pids, or `#restarting` nodes — so a node with
`kind = #beamtalkSupervisor` and `isSupervisor = true` always bridges while
alive, named or not. This is the only node→live bridge; all mutation stays on
the live object (ADR 0080 family), all discovery stays on the navigator, all
frozen facts stay on the node.

### 4. Snapshot semantics

Construction **snapshots** the tree: `ProcessNavigation default` walks
`which_children` top-down once and freezes the structure (pids, names, kinds,
counts, adjacency) into immutable records. Once frozen, walking the returned
tree never re-enters OTP, so *iteration* is internally consistent — it can
never deadlock, block on a busy process, or observe records mutating mid-walk.

Honest caveat: **construction itself is not atomic.** The snapshot is
assembled from many separate `which_children` calls (one per supervisor), and
the tree can mutate between them — a child can be restarted, or appear/vanish,
between the moment its parent is read and the moment it is read. So the
snapshot is a *best-effort point-in-time view*, not a globally-consistent
cut. This is the same guarantee `:observer` and LiveDashboard provide (BEAM
offers no atomic whole-tree freeze), and it is acceptable precisely because
the model is "snapshot then browse, re-snapshot to refresh" — matching
Pharo's / Squeak's process-browser idiom and the snapshot-friendly ordering
`SystemNavigation` already adopts. A node whose pid died during or after
construction is detected lazily: `node status` returns `nil` rather than
raising.

To observe change, take a new snapshot:

```beamtalk
before := ProcessNavigation default tree
// ... a child crashes and is restarted ...
after  := ProcessNavigation default tree
after root childCount = before root childCount   // => true (restarted)
```

A node's pid may be dead by the time you inspect it (the snapshot is a
*past* truth). `node status` (§5) is the liveness check — it returns `nil`
for a process that has since died, rather than raising.

### 5. Lazy, timeout-guarded state

`node status` is **not** captured at snapshot time (constraint 3). Calling it
issues a guarded `sys:get_status/1` *at call time* against that node's pid:

- alive + `sys`-compliant → a `Dictionary` of the OTP status report;
- dead, timed out, or not `sys`-compliant → `nil` (logged at debug, not raised).

This keeps snapshotting cheap (no blocking call per process) and makes "show me
the state of *this* node" an explicit, bounded action — the renderer fetches
status only for the node the user expanded, never for the whole tree at once.

### 6. Public vs internal scope — and authorization

`ProcessNavigation default` filters out the workspace's own plumbing — the
REPL session supervisor, the ChangeLog supervisor, `beamtalk_xref`, the
compiler server, the subprocess sup, logger handlers — via a deny-list of
registered names/modules owned by the runtime. `ProcessNavigation system`
shows everything. The deny-list lives in the Erlang shim (it knows the runtime
module names) and is the single place that defines "infrastructure." Foreign
*application* processes (a package's gen_server) are **not** filtered — they're
the user's dependencies and belong in `default`.

**Authorization (decided, not deferred).** The two scopes carry different
privilege:

- `ProcessNavigation default` / `Workspace processes` is a **Read** operation
  under ADR 0091's role model — it is scoped exactly like the existing
  `actors` Read op, just adding supervision structure, and is safe to grant
  the Observer role.
- `ProcessNavigation system` is **not** a plain Observer-read op. It exposes
  the whole-node process map (runtime internals, other sessions' supervisors,
  foreign processes) and is a lateral-movement aid in the compromised-Phoenix
  scenario ADR 0091 flags. It is gated to a **privileged role** (at minimum,
  one already holding `eval` — i.e. it grants no reconnaissance a code-running
  caller couldn't already perform via FFI).

The ADR-0091 implementation issue owns the authoritative op list and adds
`processes` (`default`→Read, `system`→privileged) accordingly; this ADR fixes
the *requirement*, that issue records the *wiring*.

### 7. The runtime shim

One new module, `beamtalk_process_navigation`, exporting the snapshot builder
and the guarded status fetch. It delegates entirely to existing OTP calls —
`supervisor:which_children/1`, `supervisor:count_children/1`,
`erlang:process_info/2` (registered name, current function, dictionary), and
`sys:get_status/1` (lazy). No new gen_server, no ETS, no maintained index.

Kind classification reuses machinery the runtime already has:

- **`#beamtalkActor`** — `beamtalk_actor:is_beamtalk_actor/1` already answers
  this: it reads `process_info(Pid, dictionary)` and tests for the
  `'$beamtalk_actor'` marker that every Beamtalk actor's `init/1` plants via
  `erlang:put('$beamtalk_actor', Class)`. The marker's value **is** the
  `behaviourClass`. (Note: the actor's `'$beamtalk_class'` lives in the
  gen_server *state map*, not the readable process dictionary —
  `'$beamtalk_actor'` is the correct, pid-reachable tag.)
- **`#beamtalkSupervisor`** — matched by pid against the supervisor registry
  that `beamtalk_supervisor:get_root/0` and `current:/1` already maintain
  (class ↔ pid), since Beamtalk supervisors run the OTP `supervisor`
  behaviour and do *not* carry an actor process-dict marker.
- **`#otpSupervisor` / `#otpProcess`** — any remaining pid: classified as a
  supervisor when it presents the OTP supervisor behaviour (detectable via
  `process_info(Pid, dictionary)` `$ancestors`/`'$initial_call'` or a guarded
  `supervisor:which_children/1`), otherwise a plain process. `behaviourClass`
  is `nil`; `registeredName` / initial-call module identify it.

### 8. Push vs pull — out of scope

This ADR is **pull/snapshot only.** A LiveView IDE wanting live tree updates
re-snapshots on a timer (cheap) for v1. A genuine *change-event* story —
subscribe to "child added" / "child crashed" — belongs to the **Announcements
event substrate (ADR 0093 / BT-2396)**, not this surface. Supervision deltas
are discrete typed events (`SupervisionChildAdded` / `SupervisionChildCrashed`)
published on `SystemAnnouncer` (ADR 0093 §5); a tool subscribes there. The
seam: `ProcessNavigation` can later expose an `announcer` accessor over those
events, layered on top of this snapshot API without changing it. Designing the
event substrate here would duplicate ADR 0093 and couple introspection to a
notification system that did not yet exist when this ADR was written.

### REPL session

```beamtalk
bt> Workspace processes root
#SupervisionNode<#beamtalkSupervisor AppSup <0.200.0> children: 3>

bt> Workspace processes do: [:n | Transcript showLine: n registeredName printString]
#AppSup
#DatabasePool
#HTTPRouter
#MetricsCollector
nil          "an anonymous worker"

bt> (Workspace processes findClass: HTTPRouter) first status
#{ #state => #running, #queueLength => 0, ... }

bt> ProcessNavigation system tree size
47

bt> ProcessNavigation default tree size
6            "internal plumbing filtered out"
```

### Error behaviour

A wrong-type root is caught **statically** by the `from: aRoot :: Supervisor
| Pid` annotation — it never reaches runtime in typed code:

```beamtalk
bt> ProcessNavigation from: 42
// type error: `from:` expects Supervisor | Pid, got Integer (42)
```

The only failure the type system cannot prevent is a root that *is* the right
type but already dead. That surfaces through the `Result`, rendered as the
supervisor family's shorthand (ADR 0080), `Result error: (beamtalk_error
<reason>)`:

```beamtalk
bt> ProcessNavigation from: aStoppedSupervisor
// => Result error: (beamtalk_error stale_handle)
//      "supervisor <0.200.0> is not alive"

bt> (ProcessNavigation from: aStoppedSupervisor) unwrap
// => raises: supervisor <0.200.0> is not alive

bt> deadNode status
// => nil          "process died since snapshot — not an error"
```

(The runtime still defends the boundary — an untyped `Dynamic` value arriving
via FFI yields `Result error: (beamtalk_error type_error)` rather than
crashing — but typed call sites never see it.)

## Prior Art

| Source | What they do | What we take / leave |
|---|---|---|
| **Pharo / Squeak Process Browser** | Snapshot list of processes, click to inspect state, manual refresh. Process-centric, flat. | **Take:** snapshot-then-browse idiom; manual refresh. **Leave:** flat process list — BEAM's supervision *hierarchy* is the more useful spine. |
| **BEAM `:observer`** (`observer_pro_wx`, app/sup tree) | GUI tree built on `which_children` + `sys:get_status`; mixes all process kinds; lazy state on selection. | **Take:** the exact primitive set; lazy state-on-select; show foreign processes. **Leave:** it's a wx GUI with no programmatic Beamtalk surface — we expose the *data*, the IDE renders it. |
| **Elixir `Supervisor.which_children/1` + `:supervisor.count_children`** | Thin functional wrappers returning tuples/lists; caller composes the tree. | **Take:** thin-wrapper philosophy (no new mechanism). **Leave:** tuples — we return message-passing objects, and we recurse for the caller. |
| **Phoenix LiveDashboard process tree** | LiveView render of the OTP tree with periodic re-poll; lazy per-process detail; filters infra. | **Take:** re-poll snapshots for "live" view; infra filtering (`default` vs `system`); lazy detail. **Confirms:** push-events are *not* required for a usable live pane. |
| **Akka `ActorSelection` / actor paths** | Path-addressed hierarchy navigation. | **Leave:** path strings are a foreign idiom; Beamtalk navigates by object reference and class, not `/user/app/worker` paths. |

The synthesis: **`:observer`'s data model, Pharo's snapshot ergonomics,
Elixir's thin-wrapper restraint, exposed as message-passing objects.**

## User Impact

- **Newcomer.** `Workspace processes` mirrors `Workspace actors` they already
  know; `do:` / `select:` are the same collection messages as everywhere else.
  They can *see* supervision (an abstract OTP concept) as concrete objects —
  a teaching win. Getting it wrong is gentle: `status` on a dead node returns
  `nil`, not a crash.
- **Smalltalk developer.** Recognises the Process Browser immediately;
  snapshot-then-browse is the Pharo muscle memory. The object-returning,
  class-rooted query (`findClass:`) is more Smalltalk than Erlang's tuple
  wrappers. The `SystemNavigation`/`ProcessNavigation` symmetry reads as a
  natural pair.
- **Erlang/BEAM developer.** Sees it's `which_children` + `sys:get_status`
  underneath — no magic, no hidden bookkeeping process, predictable cost.
  Foreign gen_servers from their Erlang packages show up correctly tagged.
  Can still drop to FFI when they want raw tuples.
- **Operator.** A live tree pane (re-snapshotting) gives observer-style insight
  without attaching a wx GUI to a production node — usable over the
  authenticated remote front (ADR 0091). Lazy/guarded `sys:get_status` means
  introspection cannot wedge a busy process by accident.
- **Tooling developer.** A clean, immutable, fully-typed record tree — trivial
  to render in LiveView (ADR 0085) and to diff between snapshots for a
  "what changed" view. The `kind` field drives rendering decisions directly.

## Steelman Analysis

### Alternative A — Flat record API only (caller composes the tree)

- 🧑‍💻 **Newcomer:** "One list of records is less to learn than a tree object
  with its own protocol."
- ⚙️ **BEAM veteran:** "This is exactly what `which_children` gives me — don't
  wrap what's already simple; let me `select:` over a flat list."
- 🎨 **Language designer:** "Smallest surface, fewest classes. The tree is
  derivable; don't ship derived state as API."
- **Tension:** maximally simple, but every consumer (REPL user, LiveView pane,
  test) re-implements parent/child reconstruction and recursion — the
  acceptance criteria explicitly asks for *both*, and "compose it yourself"
  pushes the hard part (adjacency, cycles, ordering) onto every caller.

### Alternative B — Tree-as-object only (no flat record access)

- 🎩 **Smalltalk purist:** "A tree is an object; you talk to it. Exposing the
  backing list leaks representation."
- 🏭 **Operator:** "I only ever want to walk and render; the flat list is noise."
- **Tension:** clean OO, but diffing two snapshots and bulk filtering
  (`nodesOfKind:`) are far easier over a flat list; hiding it forces awkward
  full-walk recomputation for set operations.

*The decision takes the union of A and B — tree object **backed by** an
exposed flat list — because the two steelmen target different real tasks
(render vs. analyze) and neither subsumes the other.*

### Alternative C — Raw FFI to `supervisor:which_children/1`, no Beamtalk surface

- ⚙️ **BEAM veteran:** "I already know the OTP calls; a Beamtalk wrapper is
  ceremony. `(Erlang supervisor) whichChildren: pid` is enough."
- 🎨 **Language designer:** "Zero new classes, zero maintenance."
- **Tension:** abandons every other cohort — newcomers get raw tuples and
  pids, no class tagging, no foreign/Beamtalk distinction, no IDE-renderable
  model, and it violates the project's pattern of giving runtime state a
  message-passing surface (the whole point of `SystemNavigation`,
  `WorkspaceInterface`, `ChangeLog`).

### Alternative D — Gate access behind an `:observer`-equivalent UI primitive only

- 🏭 **Operator:** "Just give me the pane; I don't want an API, I want a screen."
- **Tension:** an ADR-0085 IDE pane *needs a data source* — a UI-only gate has
  nothing underneath it to render, test, or script. This inverts the
  dependency: the API is the substrate, the pane is one consumer.

### Receiver tension — `Beamtalk` vs `Workspace` vs `ProcessNavigation`

- Rooting at `Beamtalk` (system facade) keeps the call short but re-bloats the
  facade the project just *un*-bloated (BT-2189/2190 moved navigation *off*
  `Beamtalk`).
- Rooting only at `Workspace` couples process navigation to the project facade
  and gives scoped roots (`from: aSupervisor`) no natural home.
- A dedicated `ProcessNavigation` + a thin `Workspace processes` alias gets
  both: a growable, discoverable query class **and** the one-liner. This is the
  same resolution `SystemNavigation` reached, and consistency across the two
  introspection axes is itself the strongest argument.

## Alternatives Considered

See Steelman Analysis above for the four surface alternatives (A flat-only,
B tree-only, C raw-FFI, D UI-gate) and the three receiver options.

### Alternative E — Extend the existing `Supervisor` API to recurse (no new classes)

Rather than three new classes, add one method to the existing `Supervisor`
class — `recursiveChildren -> List(SupervisionNode)` — that walks down from a
known Beamtalk supervisor root using the dispatch `which:`/`children` already
provide, and let `Workspace supervisors collect: [:s | s recursiveChildren]`
cover the workspace case.

```beamtalk
(WebApp current) recursiveChildren     // walk just this subtree
Workspace supervisors flatCollect: [:s | s recursiveChildren]
```

This is the genuinely strong lightweight option: one method on an existing
class, **no deny-list** (it only ever sees what the user's own supervisor
hierarchy reaches), and it covers the primary use case — "debug a vanished
actor under a supervisor I defined."

**Why rejected.** It is structurally blind to exactly the things the
acceptance criteria require: it can only start from a *Beamtalk* `Supervisor`
object, so it cannot represent **foreign OTP supervisors** (no Beamtalk
wrapper to call `recursiveChildren` on), cannot show the **root application
tree** below a foreign root, and has no home for a **`system`** view of
runtime infrastructure. It also re-roots the introspection surface back onto
the actor/supervisor domain objects — the opposite of the `SystemNavigation`
split that deliberately moved navigation *off* the domain facade into a
dedicated query class. It is, however, a viable *Phase 1* if scope must
shrink (see Implementation): ship `recursiveChildren` over Beamtalk-only
trees first, then generalise to `ProcessNavigation` for foreign/`system`
coverage. The full design is preferred because the foreign-process and
`system` requirements are first-class in BT-2395, not nice-to-haves.

---

The decision is **tree-object-backed-by-flat-records on a dedicated
`ProcessNavigation` class with a `Workspace processes` alias**, snapshot
semantics, lazy guarded state, foreign processes tagged by `kind`, and
change-events deferred to BT-2193.

## Consequences

### Positive
- First Beamtalk-language window into live process structure; unblocks the
  ADR-0085 Observer pane and REPL-driven debugging of vanished actors.
- Zero new runtime mechanism — pure wrapper over OTP, so no bookkeeping
  process to keep coherent, no hot-reload interaction, predictable cost.
- Symmetric with `SystemNavigation` (static ↔ dynamic), making the pair
  teachable and the codebase consistent.
- Immutable, typed snapshot records are trivial to render and to diff.

### Negative
- Snapshots are stale by construction — a node's pid can be dead before you
  inspect it. Mitigated by lazy `status` returning `nil`, but users must
  understand "snapshot, not live."
- No push events in v1; a "live" IDE pane must poll. Acceptable (LiveDashboard
  does the same) but is a known follow-up dependency on BT-2193.
- `kind` classification leans on the `'$beamtalk_actor'` process-dictionary
  marker and OTP behaviour metadata; an exotic foreign process could be
  mis-tagged `#otpProcess`. Low impact (it still appears, just generically).
- **Information exposure / ADR 0091 role placement.** `ProcessNavigation
  system` enumerates *every* process on the node — runtime internals, other
  sessions' supervisors, foreign package processes — with pids, registered
  names, child counts and (lazily) internal state. ADR 0091's curated Read op
  set is explicitly `info, inspect, bindings, actors, sessions, complete`
  (§Decision 3) — **`processes` is not in it.** Decided (§6): `default` /
  `Workspace processes` is a *Read* op (scoped like `actors`), `system` is
  gated to a **privileged role** (at minimum `eval`-holding), and the
  ADR-0091 implementation issue records the wiring. The asymmetry (`default`
  safe-by-default, `system` privileged) is documented at the call site, not
  just here.
- Per-child restart *history* is not publicly available from OTP (see §3);
  the ADR ships configured restart *budget* only and defers true restart
  counts. Anyone expecting "how many times has this crashed" from v1 will not
  find it.

### Neutral
- Adds up to three stdlib classes (`ProcessNavigation`, `SupervisionTree`,
  `SupervisionNode`) and one Erlang shim — a deliberate, bounded surface,
  shipped flat-list-first (Phase 0) so the tree classes are deferrable.
- The infra deny-list (`default` vs `system`) is a maintained list of runtime
  module names; it grows as the runtime adds internal supervisors. Centralised
  in the shim and pinned by the deny-list parity test (Implementation §4), so
  the cost is contained and a missing entry fails CI rather than leaking.
- `node pid` hands back a `Pid`, but `Pid`'s current protocol is thin
  (`isAlive`, `kill`, `exit:`, equality, printing — `stdlib/src/Pid.bt`).
  Inspecting a node is rich; *acting* on its process from the `Pid` alone is
  limited today. For Beamtalk actors the richer move is `node behaviourClass`
  + the actor object; growing `Pid` (e.g. `send:`) is out of scope here.

## Implementation

High-level, downstream of acceptance (separate issues). Sequenced so the
cheapest thing that proves the design ships first (napkin wire-check, per the
project's Phase-0 convention):

0. **Phase 0 (napkin / wire-check)** — `beamtalk_process_navigation.erl`
   exporting one function that returns the **flat `List(SupervisionNode)`**
   for `default` (Beamtalk-only kinds, infra filtered), plus a minimal
   `SupervisionNode` `Value subclass:` and the `Workspace processes` accessor
   returning the flat list. This alone covers the REPL "debug a vanished
   actor" use case and de-risks kind classification (`'$beamtalk_actor'`
   marker + supervisor registry) and the `restarting`-atom guard before any
   tree object exists. If this step is awkward, the design is wrong.
1. **Runtime shim (full)** — extend Phase 0 with `count_children`, guarded
   `sys:get_status` (lazy `status`), foreign-process classification
   (`#otpSupervisor`/`#otpProcess`), `system` scope, the `simple_one_for_one`
   child cap, and `from: aSupervisor | aPid`. Structured errors; `?LOG_*`
   with `domain => [beamtalk, runtime]`.
2. **Tree object** — `SupervisionTree` (collection protocol + parent/child
   navigation) layered over the flat list, and `ProcessNavigation`
   (`sealed typed Object subclass`, constructors `default` / `system` /
   `from:`). Deferring this behind Phase 0/1 is the cheap escape hatch if
   scope must shrink — flat-list-first remains useful on its own (see
   Alternative E).
3. **Facade alias** — `Workspace processes` returns `ProcessNavigation default
   tree` once the tree object lands.
4. **Tests** — BUnit covering: tree walk; the non-atomic snapshot under
   concurrent mutation (a supervisor stopped mid-walk yields a *partial* tree,
   not a raise); a child in `restarting` state (`kind => #restarting`,
   `pid => nil`); mixed Beamtalk/foreign nodes; a `DynamicSupervisor` past the
   child cap (truncation marker + `childCount`); lazy `status` returning `nil`
   for a dead node; `from:` returning `Result ok:` for a `Supervisor` and a
   `Pid`, and `Result error: (beamtalk_error stale_handle)` for a dead
   supervisor; a **type-checker test** asserting `from:` rejects a non-`Supervisor
   | Pid` argument statically (plus the FFI/`Dynamic` runtime fallback yielding
   `Result error: (beamtalk_error type_error)`).
   **Deny-list parity test:** assert `default`'s node
   set ⊆ `system`'s, and that each currently-known internal supervisor
   (`beamtalk_workspace_changelog`, session sup, `beamtalk_xref`, compiler
   server, subprocess sup, …) is filtered from `default` — this test is the
   ownership mechanism that catches a new runtime supervisor missing from the
   deny-list.
5. **Surfaces** — LSP / MCP / browser consumers (ADR 0085 pane, still
   `Proposed` — this ADR unblocks it but does not depend on it); update
   `docs/development/surface-parity.md`.
6. **ADR 0091 coordination** — the role-map issue places `processes`
   (`default`→Read, `system`→privileged) in the authoritative op list.

Affected components: **runtime** (new shim), **stdlib** (one value class first,
two more later + facade method), **docs/tests**. No parser or codegen changes.

## References
- Related issues: BT-2395 (this ADR), BT-448 (supervision syntax, shipped),
  BT-1387 (OTP supervision tree runtime), BT-1191 (`Workspace startSupervisor:`),
  BT-2193 (Announcements — deferred push-event substrate),
  BT-2228 (SystemNavigation xref epic — parallel pattern),
  BT-2189/BT-2190 (moved navigation off the `Beamtalk` facade)
- Related ADRs: ADR 0059 (Supervision Tree Syntax), ADR 0080 (Supervisor
  Lifecycle Result), ADR 0085 (Editor Live-Image Representation — primary
  consumer), ADR 0087 (Maintained Xref Index for SystemNavigation — static
  counterpart), ADR 0091 (Remote Workspace Access)
- Documentation: `stdlib/src/WorkspaceInterface.bt`,
  `stdlib/src/SystemNavigation.bt`, `stdlib/src/Supervisor.bt`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`
- External: Pharo Process Browser; BEAM `:observer`
  (`erts/lib/observer/src/observer_pro_wx.erl`); Phoenix LiveDashboard process
  tree; Elixir `Supervisor.which_children/1`
