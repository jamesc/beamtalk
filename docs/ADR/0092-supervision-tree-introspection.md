# ADR 0092: Supervision Tree Introspection API (Runtime Query Surface)

## Status
Proposed (2026-06-06)

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
   structurally too slow; reading `which_children` is cheap, so no index here.)
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
   stale pid) surface as `#beamtalk_error{}`, consistent with `whichChild`'s
   existing `stale_handle` kind.

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
ProcessNavigation default     // workspace-scoped, internal plumbing filtered
ProcessNavigation system      // everything, including runtime infra
ProcessNavigation from: aSupervisor   // rooted at one supervisor subtree
```

`ProcessNavigation` is a `sealed typed Object subclass` — a value object, not
an actor — exactly like `SystemNavigation`. Constructors capture a snapshot
(see §4) and hand back a `SupervisionTree`. The class side is just a
constructor surface; all query logic is instance-side so it runs as plain
Erlang in the calling eval worker (constraint 2).

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
node pid              // => a Pid (the live process)
node registeredName  // => Symbol | nil   (registered name if any)
node kind            // => one of #beamtalkSupervisor #beamtalkActor
                     //            #otpSupervisor #otpProcess
node behaviourClass  // => Class | nil   (the Beamtalk class, nil for foreign)
node strategy        // => Symbol | nil  (#oneForOne … ; supervisors only)
node childCount      // => Integer       (live children; supervisors only)
node restartIntensity // => Dictionary | nil  (configured #{maxRestarts, window};
                     //   supervisors only — see note on restart *counts* below)
node children        // => List(SupervisionNode)   (snapshot children)
node parent          // => SupervisionNode | nil
node isSupervisor    // => Boolean
node isBeamtalk      // => Boolean        (kind is one of the #beamtalk* kinds)
node status          // => Dictionary | nil   (LAZY — see §5)
```

`kind` is the load-bearing field: it tells a renderer whether to draw a
Beamtalk class badge (`#beamtalkActor`/`#beamtalkSupervisor`, `behaviourClass`
populated) or a foreign-process badge (`#otpProcess`/`#otpSupervisor`,
`behaviourClass` is `nil`, `registeredName`/module identify it).

```beamtalk
// Find every running Counter and its supervisor's configured restart budget:
(ProcessNavigation default tree findClass: Counter)
  collect: [:n | n parent restartIntensity]   // => #(#{#maxRestarts => 10, ...})

// Foreign processes are visible, just differently tagged:
(ProcessNavigation system tree nodesOfKind: #otpProcess) size   // => 11
```

**On restart *counts* (acceptance-criteria gap, called out honestly).** OTP's
*public* supervisor API exposes the configured restart *budget*
(`maxRestarts` / `restartWindow`, already surfaced by `beamtalk_supervisor`
via the class-side `maxRestarts`/`restartWindow` methods) and live child
*counts* (`count_children`), but **not a per-child restart *history* or
cumulative restart *count*.** The running tally lives in the supervisor's
private state and is only reachable by parsing `sys:get_status/1`'s internal
report — undocumented, version-fragile, and explicitly *not* a public
contract. This ADR therefore surfaces `restartIntensity` (the configured
budget, public and stable) on the node, and treats a true per-child restart
*counter* as an **open question** deferred to implementation: if a stable
source emerges (e.g. a runtime-maintained tally hooked into the
supervisor's child-restart path, paralleling the maintained xref index of
ADR 0087), it can be added as `node restartCount` without breaking the
record. The acceptance criterion "restart history" is answered as: *budget
now, history deferred with a documented reason.*

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

### 6. Public vs internal scope

`ProcessNavigation default` filters out the workspace's own plumbing — the
REPL session supervisor, the ChangeLog supervisor, `beamtalk_xref`, the
compiler server, the subprocess sup, logger handlers — via a deny-list of
registered names/modules owned by the runtime. `ProcessNavigation system`
shows everything. The deny-list lives in the Erlang shim (it knows the runtime
module names) and is the single place that defines "infrastructure." Foreign
*application* processes (a package's gen_server) are **not** filtered — they're
the user's dependencies and belong in `default`.

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
subscribe to "child added" / "child crashed" — is the **Announcements
package's** job (BT-2193), not this surface. The seam: when Announcements
lands, `ProcessNavigation` can expose `announcer` returning a stream of
tree-delta events, layered on top of this snapshot API without changing it.
Designing the event substrate here would duplicate BT-2193 and couple
introspection to a notification system that does not yet exist.

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

```beamtalk
bt> ProcessNavigation from: 42
// => #beamtalk_error{ kind = type_error,
//      message = "ProcessNavigation from: expects a Supervisor, got 42" }

bt> deadNode status
// => nil          "process died since snapshot — not an error"

bt> ProcessNavigation from: aStoppedSupervisor
// => #beamtalk_error{ kind = stale_handle,
//      message = "supervisor <0.200.0> is not alive" }
```

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
B tree-only, C raw-FFI, D UI-gate) and the three receiver options. The
decision is **tree-object-backed-by-flat-records on a dedicated
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
- **Information exposure.** `ProcessNavigation system` enumerates *every*
  process on the node — including runtime internals, other sessions'
  supervisors, and foreign package processes — with pids and registered
  names. Over the authenticated remote front (ADR 0091) this is a
  reconnaissance surface. Mitigation: `default` (the `Workspace processes`
  alias and the IDE pane's source) is *already* scoped to the workspace's own
  application tree with infra filtered; `system` is the privileged, opt-in
  view. The ADR-0091 authenticated boundary gates *who can eval at all*, so
  `system` is no more exposing than the FFI those callers already have — but
  the asymmetry (`default` safe-by-default, `system` privileged) should be
  documented at the call site, not just here.
- Per-child restart *history* is not publicly available from OTP (see §3);
  the ADR ships configured restart *budget* only and defers true restart
  counts. Anyone expecting "how many times has this crashed" from v1 will not
  find it.

### Neutral
- Adds three stdlib classes (`ProcessNavigation`, `SupervisionTree`,
  `SupervisionNode`) and one Erlang shim — a deliberate, bounded surface.
- The infra deny-list (`default` vs `system`) is a maintained list of runtime
  module names; it grows as the runtime adds internal supervisors. Centralised
  in the shim, so the cost is contained.

## Implementation

High-level, downstream of acceptance (separate issues):

1. **Runtime shim** — `beamtalk_process_navigation.erl`: snapshot builder over
   `which_children`/`count_children`/`process_info`, guarded `sys:get_status`,
   kind classification, infra deny-list. Structured errors; `?LOG_*` with
   `domain => [beamtalk, runtime]`.
2. **Value classes** — `SupervisionNode` (`Value subclass:`),
   `SupervisionTree` (collection protocol + tree navigation),
   `ProcessNavigation` (`sealed typed Object subclass`, constructors
   `default` / `system` / `from:`).
3. **Facade alias** — `Workspace processes` on `WorkspaceInterface`.
4. **Tests** — BUnit covering: tree walk, snapshot consistency under
   concurrent mutation, mixed Beamtalk/foreign nodes, `default` vs `system`
   filtering, lazy `status` returning `nil` for a dead node, `from:` type
   errors.
5. **Surfaces** — LSP / MCP / browser consumers (ADR 0085 pane); update
   `docs/development/surface-parity.md`.

Affected components: **runtime** (new shim), **stdlib** (three classes + facade
method), **docs/tests**. No parser or codegen changes.

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
