# ADR 0095: System Browser Data Source — Class/Method Browse API for the LiveView IDE

## Status
Proposed (2026-06-10)

## Context

### Problem

The LiveView IDE (ADR 0017 Phase 3, epic BT-2482) renders a **System Browser** —
the four-pane Smalltalk navigator: *classes → protocols → selectors → method
source*, with a class-definition pane. The UX spike
(`spikes/cockpit-ux-spike/`, `image.js`) is built against a hand-authored data
model that already encodes the contract the real browser needs:

```js
// spikes/cockpit-ux-spike/image.js
{ name: "Counter", superclass: "Actor", category: "Demo-Counting",
  state: ["value = 0", "step = 1"], comment: "...",
  definition: "Actor subclass: Counter\n  state: value = 0\n  state: step = 1",
  methods: [ m("Counter", "instance", "increment", "operations", ..., /*runtime*/ false),
             m("Counter", "class",    "startingAt:", "instance creation", ...) ] }
```

Each method carries `{klass, side, selector, category /*protocol*/, source,
runtime}`. The browser groups selectors by `category` (protocol), splits
instance/class side, fetches `source` on demand, and shows a class `definition`
pane. The `runtime` flag is the spike's stand-in for **image/disk divergence** —
a method present in the live image but not in static source.

There is **no decided API** the LiveView IDE can call to populate these panes
against a real workspace. The spike fakes it. This ADR decides where that data
comes from and the exact op shapes the browser consumes.

### What this ADR is NOT

- **Not the editor representation surface (ADR 0085).** ADR 0085 decides what a
  *VS Code editor buffer* renders (virtual `beamtalk-live://` documents) and how
  edits route to disk (ADR 0082). This ADR decides the **browse/enumeration data
  source** the *LiveView IDE's* System Browser panes consume — the class/protocol/
  selector trees and the read-only source/definition payloads. The two share a
  substrate (live source comes from the same runtime accessors) but answer
  different questions: 0085 is "what does the editor edit", 0095 is "what does the
  browser list".
- **Not a new transport.** It reuses the BT-2399 term-op layer
  (`docs/development/repl-op-term-contract.md`): `beamtalk_repl_ops:dispatch/4`
  returns terms over Erlang distribution to Phoenix; `encode/2` flattens to JSON
  only at the WebSocket edge. No new protocol.
- **Not persistence.** Browse is read-only. Edits route through ADR 0082.

### Current state

The term-op layer already has two browse-adjacent ops, but neither is
System-Browser-shaped:

| Op | Term shape today | Gap for the System Browser |
|----|------------------|----------------------------|
| `list-classes` | `{class_list, [ClassInfo]}` — `name`, `superclass`, `doc`, `sealed`, `abstract`, `internal`, `source_file`, `actor_count` | No **category** (Smalltalk class category for the package/group tree); flat list, no hierarchy edges beyond `superclass` |
| `methods` | `{methods, Methods, StateVars}` — flat instance+class selectors + state vars | No **protocol grouping**; no per-method **side** tagging in a browser-shaped tree; no **source-status** marker (indexed / synthetic / runtime-only) |

The substrate to fill these gaps already exists:

- **`SystemNavigation`** (BT-2201, `stdlib/src/SystemNavigation.bt`) — the
  enumeration backbone: `allClasses`, `actorClasses`, `selectorsForClass:`,
  `selectorsMatching:`. Class-scoped instances (`SystemNavigation over:`,
  `forClasses:`) are designed for in BT-2201.
- **The xref index** (ADR 0087, `beamtalk_xref`) — sub-millisecond
  `{class, side, selector}` rows, each already tagged with
  `source_status :: indexed | unindexed_runtime_fun | synthetic` and a
  `provenance :: class_body | extension | class_builder | put_method`. This is
  precisely the image/disk divergence signal the browser needs, already
  maintained coherently with the live image.
- **`aClass sourceString`** / `(aClass >> #sel) source` (ADR 0085 "Content is a
  message send", BT-2196) — patch-aware class and method source text.
- **`get_class_source/1`** / `beamtalk_runtime_api:superclass/1` etc. — the
  reflection used by `list-classes` today.

### Constraints

1. **Static-first, live-augmented (ADR 0024).** The browser must work against a
   *cold* project (static analysis only, no BEAM) **and** a *live* workspace
   (runtime-augmented). The data source must define both a static answer and how
   the live image augments/diverges from it.
2. **Reuse the term-op layer (BT-2399).** Ops return terms; JSON only at the WS
   edge. The browse ops are `{value, JsonValue}`-tagged so dist clients (Phoenix)
   get typed rows with no inspect-string round-trip (the contract's reason the
   `value` tag exists, distinct from `result`).
3. **Read-capability only (ADR 0091).** Every browse op must be grantable to the
   **Observer** role — it must trigger **no user code** (no user `printOn:` /
   `displayString`; field reflection only, per ADR 0091 Decision 4 / ADR 0035).
   This is a hard gate: the browse data source is on the Observer read-surface.
4. **Surface parity** (`docs/development/surface-parity.md`) — the browse ops
   appear across REPL/MCP/LSP/LiveView and must be equivalent where they appear.
5. **Files are source of truth (Principle #5).** Browse *renders* the image but
   never makes the image authoritative over files; divergence is *surfaced*, not
   silently reconciled.

## Decision

Add a **System Browser browse facade**: four read-only term-ops on the BT-2399 op
layer, sourced **static-first / live-augmented** per ADR 0024, with image/disk
divergence carried as explicit per-row metadata (never silently merged). The four
ops map one-to-one to the four browser panes plus the definition pane:

| Op | Pane | Term shape | Answers |
|----|------|------------|---------|
| `browse-classes` | class tree | `{value, [ClassRow]}` | "what classes exist, with superclass + category" |
| `browse-protocols` | protocol + selector tree | `{value, ProtocolTree}` | "the selectors of a class/side, grouped by protocol" |
| `browse-method-source` | method source pane | `{value, MethodSource}` | "the source of one method (image-accurate)" |
| `browse-class-definition` | class-definition pane | `{value, ClassDefinition}` | "the class's definition + state + comment" |

These are **new browse ops**, not extensions of `list-classes` / `methods`:
`list-classes` (the REPL `:classes` listing) and `methods` (completion-adjacent)
keep their existing, separately-tested shapes (ADR-0091 facade entries, CLAUDE.md
REPL-output rule). The browse ops are the IDE-shaped views the spike implies.

### Static-first / live-augmented, per op

Each op has a **static answer** (from `ProjectIndex` / `ClassHierarchy`, ADR 0024
Tier 1/2 — available with no BEAM) and a **live answer** (from the class registry
+ xref index, Tier 3). The op merges them with the ADR 0024 rule — *static is the
backbone; live augments and is tagged* — and surfaces divergence rather than
hiding it:

- **Cold (no workspace):** the ops run against static analysis. Every row is
  `origin = "static"`. Runtime-only methods, live `>>` patches, and
  `ClassBuilder` classes are absent — correctly, there's no image.
- **Live (workspace attached):** the ops run against the running image
  (registry + xref). The static project source is still consulted to compute
  divergence. Each row carries `origin` so the browser can render the delta.

### Op 1 — `browse-classes` → `{value, [ClassRow]}`

Lists every class in scope with the edges the class tree needs: `superclass`
(hierarchy) and `category` (the Smalltalk class-category group the spike's tree
uses). Extends the `list-classes` field set with `category` and `origin`; it does
**not** replace `list-classes`.

```erlang
%% one ClassRow (JSON-shaped value; dist clients get this as a term, WS edge as JSON)
#{ <<"name">>       => <<"Counter">>,
   <<"superclass">> => <<"Actor">>,          %% null for Object
   <<"category">>   => <<"Demo-Counting">>,  %% class category; null if unset
   <<"comment">>    => <<"The canonical live object.">>,  %% first line; field reflection only
   <<"sealed">>     => false,
   <<"abstract">>   => false,
   <<"internal">>   => false,
   <<"source_file">> => <<"src/Counter.bt">>, %% null for file-less (ClassBuilder)
   <<"origin">>     => <<"both">> }           %% see "origin" below
```

`category` source: the class's declared category (ADR 0070 package / class-
category metadata) read via field reflection — **not** by sending the class a
method. When no category is declared, `null` (the browser groups these under an
"Uncategorized" bucket, matching Pharo).

### Op 2 — `browse-protocols` → `{value, ProtocolTree}`

Given `{class, side}`, returns the class's selectors **grouped by protocol**
(method category), which is the spike's `category` field on each method. This is
the pane the spike groups by (`"operations"`, `"accessing"`, `"instance
creation"`, …).

```erlang
%% browse-protocols {class: "Counter", side: "instance"} →
#{ <<"class">> => <<"Counter">>,
   <<"side">>  => <<"instance">>,
   <<"protocols">> => [
     #{ <<"name">> => <<"accessing">>,
        <<"selectors">> => [
          #{ <<"selector">> => <<"value">>,
             <<"line">>     => 80,
             <<"source_status">> => <<"indexed">>,  %% xref source_status
             <<"origin">>   => <<"both">> },
          #{ <<"selector">> => <<"setTo:">>, <<"line">> => 83,
             <<"source_status">> => <<"indexed">>, <<"origin">> => <<"both">> } ] },
     #{ <<"name">> => <<"operations">>,
        <<"selectors">> => [
          #{ <<"selector">> => <<"increment">>, <<"line">> => 68,
             <<"source_status">> => <<"indexed">>, <<"origin">> => <<"both">> } ] } ] }
```

- **`side`** is `"instance"` | `"class"` — the browser's instance/class toggle.
  One op call per side (the browser caches both); class-side maps to the
  metaclass rows (ADR 0036 / xref class-side keying).
- **`protocols`** is sorted; selectors within each protocol are sorted. Stable
  order so the tree doesn't jump.
- **`source_status`** is the xref tag verbatim: `"indexed"` (compiled from
  source), `"synthetic"` (compiler-generated accessor — included by default per
  ADR 0087, the browser may filter), `"unindexed_runtime_fun"` (sourceless
  runtime method — `register/4`, `put_method/3`). This is how the browser knows a
  selector has no openable source.
- **Protocol of a selector:** the xref row's `provenance` plus the method's
  declared protocol category. Selectors with no declared protocol fall into a
  `"as yet unclassified"` bucket (Pharo convention).

### Op 3 — `browse-method-source` → `{value, MethodSource}`

Fetches one method's source, image-accurate. This is the spike's `S[...]` lookup.

```erlang
%% browse-method-source {class: "Counter", side: "instance", selector: "increment"} →
#{ <<"class">>    => <<"Counter">>,
   <<"side">>     => <<"instance">>,
   <<"selector">> => <<"increment">>,
   <<"source">>   => <<"increment =>\n  self.value := self.value + self.step\n  ...">>,
   <<"source_status">> => <<"indexed">>,
   <<"origin">>   => <<"both">>,
   <<"disk_differs">> => false }   %% image source != static/disk source (live patch)
```

- **Source provider:** `(aClass >> #selector) source` — the patch-aware per-method
  text (BT-2196, ADR 0085 reader form). For class-side, the metaclass method.
- **`disk_differs`** is the per-method image/disk divergence flag: `true` when the
  live (patched) source differs from the static/disk source — a live `>>` patch
  not yet flushed (ADR 0082). The browser shows a "live patch (unflushed)" badge.
  `null` when no static source exists to compare (file-less class).
- **Sourceless methods** (`source_status = unindexed_runtime_fun`): `source` is
  `null` and `source_status` says why; the browser shows "no source (runtime
  method)" rather than an empty pane.

### Op 4 — `browse-class-definition` → `{value, ClassDefinition}`

The class-definition pane: the class header, state slots, and comment — the
spike's `definition` + `state` + `comment`.

```erlang
%% browse-class-definition {class: "Counter"} →
#{ <<"class">>      => <<"Counter">>,
   <<"superclass">> => <<"Actor">>,
   <<"category">>   => <<"Demo-Counting">>,
   <<"definition">> => <<"Actor subclass: Counter\n  state: value = 0\n  state: step = 1">>,
   <<"state">>      => [ #{ <<"name">> => <<"value">>, <<"default">> => <<"0">> },
                         #{ <<"name">> => <<"step">>,  <<"default">> => <<"1">> } ],
   <<"comment">>    => <<"The canonical live object...">>,
   <<"origin">>     => <<"both">>,
   <<"disk_differs">> => false }
```

- **`definition`** comes from `aClass sourceString` (the class skeleton; method
  bodies excluded — the definition pane shows only the class header + slots, per
  Pharo). Patch-aware via the same BT-2196/ADR-0085 path.
- **`state`** is field reflection (ADR 0035) — names + default expressions, no
  user code run.
- **`comment`** is the class doc (full text here, unlike the first-line `comment`
  in `browse-classes`).
- **File-less classes** (ClassBuilder, ADR 0038): `definition` is `null` and
  `origin = "runtime"` — the browser shows "no source (programmatic class)",
  matching ADR 0085's explicit non-goal of serialising file-less classes.

### How divergence is surfaced: the `origin` field

Image/disk divergence is **never silently merged**. Every browse row carries an
`origin` tag — the System-Browser analog of ADR 0024's `TierSource` and ADR 0087's
`source_status`, lifted to the row level so the browser can render the delta:

| `origin` | Meaning | Browser rendering |
|----------|---------|-------------------|
| `"both"` | present in static source **and** live image, identical | normal |
| `"static"` | in disk source, **not** loaded in the image (not yet loaded, or unloaded) | dimmed / "not loaded" |
| `"runtime"` | in the live image, **no** static/disk source (runtime-only method, live `>>`-added selector, `ClassBuilder` class) | "runtime-only" badge (the spike's `runtime: true`) |

`disk_differs` (ops 3/4) is the finer signal for the *both-but-changed* case: same
selector in both views, but the image body was hot-patched and not flushed
(ADR 0082 ChangeLog delta). `origin = "both"` + `disk_differs = true` = "loaded
and edited live". This is the browser's "you are looking at unflushed image state"
cue — the exact gap ADR 0085 identifies, surfaced here as browse metadata rather
than an editor buffer.

**Cold mode** collapses this: with no workspace, every row is `origin = "static"`
and `disk_differs` is `null` (nothing to compare against). The browser renders a
plain static class browser — the ADR 0024 graceful-degradation guarantee.

### Read-only / no-user-code guarantee (ADR 0091 Observer gate)

All four ops are **pure reflection**: class metadata, xref rows, and stored source
text. None sends a user-defined method to a value (no `printString`,
`displayString`, `printOn:`). `comment`/`state`/`category` are field reflection
(ADR 0035). This is the acceptance criterion ADR 0091 Decision 4 defers to the
read-surface: the browse ops are safe to grant the **Observer** role. The
implementation issue (BT-2488) must assert this (no user-code execution path) as a
test, not an assumption.

### LiveView session (illustrative)

```text
// Phoenix LiveView mounts the System Browser against an attached workspace.
// (dist client — consumes terms directly, no JSON)

browse-classes {}
  → [ {name: "Object",  superclass: null,    category: "Kernel-Objects", origin: "both"},
      {name: "Counter", superclass: "Actor", category: "Demo-Counting",  origin: "both"},
      {name: "Sketch",  superclass: "Object", category: null, origin: "runtime"} ]  // ClassBuilder

// user clicks Counter, instance side
browse-protocols {class: "Counter", side: "instance"}
  → protocols: [ {name: "accessing",  selectors: [value, setTo:, step:]},
                 {name: "operations", selectors: [increment, decrement, reset]} ]

// user clicks increment — and another session has hot-patched it
browse-method-source {class: "Counter", side: "instance", selector: "increment"}
  → {source: "increment =>\n  self.value := self.value + 2", source_status: "indexed",
     origin: "both", disk_differs: true}        // ← live patch badge

// user clicks the class name → definition pane
browse-class-definition {class: "Counter"}
  → {definition: "Actor subclass: Counter\n  state: value = 0\n  state: step = 1",
     state: [{name: "value", default: "0"}, {name: "step", default: "1"}],
     comment: "The canonical live object...", origin: "both", disk_differs: false}
```

### Error examples

```text
// Unknown class:
browse-protocols {class: "Nope", side: "instance"}
  → {error, #beamtalk_error{...}}   // not_found, structured (term contract)

// Bad side:
browse-protocols {class: "Counter", side: "klass"}
  → {error, #beamtalk_error{...}}   // invalid_argument: side must be instance|class

// Observer attempts a write op (ADR 0091 RBAC, enforced in Phoenix):
save ...   → 403, never reaches dist. browse-* stay allowed for Observer.
```

## Prior Art

### Pharo / Squeak / Cuis — the System Browser

The four-pane browser (package → class → protocol → method) is the canonical
Smalltalk navigator. Protocols (method categories) group selectors; the
class-definition pane shows `subclass:instanceVariableNames:...`. **Adopted:** the
four-pane shape maps directly to the four ops; protocol grouping is `browse-
protocols`. **Adapted:** in Pharo the browser *is* the image with no static/disk
split — every method has one truth. Beamtalk has the split brain (ADR 0087): the
xref `source_status` and our `origin`/`disk_differs` exist precisely because a
selector can be in the image but not on disk, or patched-but-unflushed. We surface
that delta as first-class browse metadata, which Pharo never needs.

### Pharo `RBBrowser` / Calypso data model

Modern Pharo (Calypso) separates the browser *model* (a queryable scope of
classes/protocols/methods) from the UI. `SystemNavigation` (BT-2201) already
follows that shape on the read side; this ADR is the IDE-facing projection of it.
**Adopted:** scope-as-a-value (`SystemNavigation over:` / `forClasses:`), so a
future "browse this package only" is the same op with a narrower scope.

### ElixirLS / Erlang LS — `documentSymbol` / module reflection

LSP `documentSymbol` returns a class/function tree for one file (static), and
ElixirLS reflects on loaded modules for the live view. **Adopted:** the static
tree is exactly our cold-mode answer (the LSP `documentSymbol` path is the same
`ProjectIndex` data). **Diverged:** they never reconcile static vs. loaded into one
tagged tree — the editor edits files, period. We merge and tag (`origin`) because
the LiveView browser browses the *image*, not a file.

### Glamorous Toolkit (Pharo) — moldable inspector/browser

GT makes every browse pane a queryable, composable view over live objects.
**Observed:** the aspiration that browse views are cheap, composable queries — our
sub-millisecond xref backing (ADR 0087) is what makes the protocol/selector panes
viable for live, hover-driven interaction rather than batch. **Not adopted:** GT's
moldable per-object custom views are an inspector concern, out of scope for the
class/method *structure* browser this ADR defines.

### Livebook (Elixir) — Phoenix over Attach

The transport peer: Phoenix attaches to a BEAM runtime over distribution.
**Adopted:** browse ops return live terms over dist (BT-2399), JSON only at the WS
edge — Livebook's runtime-attach with the term-not-JSON discipline the topology
spike chose.

## User Impact

### Newcomer (from VS Code / Python / JS)
A familiar class browser appears in the IDE: pick a class, see its methods grouped
into sensible buckets, click to read source. The static/live split is invisible in
the common case (everything is `origin: "both"`); the only new concept is the
occasional "runtime-only" or "live patch" badge, which *explains* something
otherwise mysterious (why does this method exist but isn't in my file?) rather than
adding ceremony.

### Smalltalk developer
This is the System Browser they expect — protocols, instance/class toggle, method
source pane, class-definition pane — now over a live BEAM image. The thing Pharo
never had to express (image-vs-disk divergence) is shown explicitly, which a
Smalltalker reading hot-patched code will appreciate: the browser tells them
they're looking at unflushed state, not lies to them.

### Erlang/BEAM developer
The browse ops are the same BT-2399 term layer LSP/MCP already use; backing them
with the maintained xref index (ADR 0087) means the protocol/selector panes are
sub-millisecond, not per-click re-parses. `source_status = unindexed_runtime_fun`
honestly marks the sourceless-fun methods (`register/4`, `put_method/3`) they know
exist but can't open.

### Production operator
Browse is read-only and grantable to the Observer role (ADR 0091): a teaching
audience or a reviewer can navigate the running system's classes and read source
**without** any code-execution capability — provided the no-user-code guarantee
holds (the implementation must verify it, per ADR 0091 Decision 4). The ops use the
same reflection as `observer`/`recon`; no production impact.

### Tooling developer
Four small, JSON-shaped (`{value, …}`) ops with stable row schemas. A clean
contract: `origin` for static/live divergence, `source_status` for source
availability, `disk_differs` for patch-state. One op per pane, so a browser UI is a
thin projection. Reuses `SystemNavigation` scope objects for future narrowing.

## Steelman Analysis

### Alternative A — Extend `list-classes` / `methods` in place (no new ops)

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Fewer ops to learn — one classes op, one methods op." |
| ⚙️ **BEAM veteran** | "`list-classes`/`methods` already exist and are tested; adding fields is cheaper than new handlers." |
| 🎨 **Language designer** | "Don't multiply ops; a System Browser is just a richer methods query." |
| 🏭 **Operator** | "Smaller op surface = smaller RBAC surface to reason about." |

**Why not chosen:** `list-classes` is the REPL `:classes` listing and `methods` is
completion-adjacent — both have stable shapes covered by existing tests and are
named in the ADR 0091 facade. Overloading them with browser-only fields
(protocol trees, `origin`, `disk_differs`) couples the REPL listing to the IDE
browser and risks the CLAUDE.md "don't change REPL output without asking" rule. The
browser panes are a *different question* (grouped, side-split, divergence-tagged)
than "list classes for `:classes`". New ops keep each contract single-purpose;
they still *reuse* the same `SystemNavigation`/xref substrate, so the duplication is
in the op envelope, not the logic.

### Alternative B — One mega-op `browse {class, depth}` returning the whole tree

| Cohort | Strongest argument |
|--------|-------------------|
| 🎨 **Language designer** | "One round-trip fills the whole browser; fewer ops, atomic snapshot." |
| ⚙️ **BEAM veteran** | "Avoids N method-source round-trips." |

**Why not chosen:** the browser is lazy by nature — you don't fetch every method's
source up front (the spike doesn't; `S[...]` is on-demand). A mega-op forces
eager source fetch (expensive, and triggers the most divergence computation) or
re-introduces a `depth` parameter that's really four ops in a trenchcoat. Four
lazy ops match the four-pane interaction (click class → protocols, click selector
→ source) and let each pane refresh independently off the right push channel
(ADR 0093 `class-loaded`/`changed`). Snapshot atomicity isn't needed — per ADR 0087
the xref is per-class atomic, which is exactly the browser's unit of refresh.

### Alternative C — JSON-only browse ops (skip the term layer)

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "The browser only needs JSON anyway — why involve terms?" |
| 🎨 **Language designer** | "Simpler: one encoding." |

**Why not chosen:** the browse rows are already JSON-shaped values, so they use the
`{value, JsonValue}` tag (passed through `encode/2` with identity) — they *are*
the "typed rows, no inspect-string round-trip" case the term contract added the
`value` tag for. But routing them through `dispatch/4` keeps them on the one op seam
every surface shares (BT-2399): dist clients (Phoenix) consume the value term
directly; the WS edge JSON-encodes it. A JSON-only side channel would fork the op
layer the term contract deliberately unified. The cost is zero (identity encode);
the consistency win is real.

### Alternative D — Source browse from the LSP / `ProjectIndex` only (no runtime)

| Cohort | Strongest argument |
|--------|-------------------|
| 🏭 **Operator** | "Pure static = zero runtime dependency, zero execution risk." |
| ⚙️ **BEAM veteran** | "The LSP already has the static tree (`documentSymbol`)." |

**Why not chosen:** it forecloses the entire point — a *live* System Browser that
shows runtime-only methods, `ClassBuilder` classes, and hot-patched bodies. That's
the divergence the spike's `runtime` flag encodes and ADR 0085 calls out. Static-
only *is* the cold-mode answer (and we deliberately reuse `ProjectIndex` for it),
but capping there abandons the live image the LiveView IDE exists to show. ADR 0024
already settled this: static-first, **live-augmented**, not static-only.

### Tension points
- **Op-surface minimalism (A/B) vs. single-purpose contracts:** minimizers want to
  overload existing ops; we accept four new ops to keep each contract clean and the
  REPL listing decoupled from the IDE browser.
- **Operator caution (D) vs. the live-image goal:** pure-static is safest but
  defeats the feature; we get safety instead from the read-only/no-user-code
  guarantee + Observer RBAC, keeping the live view.
- **Eager snapshot (B) vs. lazy panes:** a one-shot tree is atomic but eager; the
  browser is interactively lazy, so four ops fit the interaction and the per-class
  xref refresh unit.

## Alternatives Considered

### Extend `list-classes` / `methods` in place
See steelman A. Rejected: couples the REPL listing / completion ops to the IDE
browser and risks the REPL-output stability rule; new single-purpose ops reuse the
same substrate without overloading existing contracts.

### One mega `browse` op
See steelman B. Rejected: forces eager source fetch or a `depth` param that is four
ops in disguise; four lazy ops match the four-pane lazy interaction and per-class
refresh.

### JSON-only browse side channel
See steelman C. Rejected: forks the unified op seam (BT-2399) for no gain; the
`{value, JsonValue}` tag already carries JSON-shaped rows at identity-encode cost.

### Pure-static (LSP-only) browse
See steelman D. Rejected: forecloses the live image (runtime-only methods,
ClassBuilder classes, hot patches) that motivates the browser; static-only is the
cold-mode answer, not the ceiling (ADR 0024).

### Render source via user `printOn:` / `displayString`
Rejected outright: would make the browse data source an execution vector reachable
by the Observer role (ADR 0091 live-image caveat). Source/state/comment come from
stored text + field reflection (ADR 0035) only.

## Consequences

### Positive
- The LiveView System Browser has a real, decided data source — the spike's faked
  `image.js` model becomes four live ops.
- Static-first/live-augmented: the browser works cold (static) and live (image),
  degrading gracefully (ADR 0024).
- Image/disk divergence is explicit (`origin`, `disk_differs`, `source_status`) —
  the browser can *show* unflushed patches and runtime-only methods instead of
  lying or hiding them. Closes the gap ADR 0085 names, for the LiveView surface.
- Sub-millisecond protocol/selector panes by backing on the xref index (ADR 0087);
  viable for hover-driven, live-refresh interaction.
- Read-only and Observer-grantable (ADR 0091): a non-executing role can browse.
- Reuses the BT-2399 term layer and `SystemNavigation` scope objects — no new
  transport, future package/scope narrowing is the same op.

### Negative
- Four new ops on the term layer + JSON encoders to maintain (one per pane).
- The `origin` / `disk_differs` divergence computation requires consulting *both*
  static source and the live image when a workspace is attached — extra work per
  browse vs. a naive image-only read. Bounded (per-class), but non-zero.
- The no-user-code guarantee is a *property the implementation must hold and test*
  (ADR 0091 Decision 4 acceptance criterion), not a structural impossibility — a
  future careless source-render path could violate it.
- `category` (class category) must be reflectable without sending the class a
  method; if a class declares no category the tree shows an "Uncategorized" bucket
  (acceptable, Pharo-standard).
- File-less (`ClassBuilder`) classes have no `definition`/source — surfaced as
  `origin = "runtime"`, null source — consistent with ADR 0085's explicit non-goal,
  but a known "can't open this" for the user.

### Neutral
- `list-classes` and `methods` are unchanged — the browse ops are additive.
- The push channels (ADR 0093: `class-loaded`/`changed`/`removed`, `bindings`)
  drive browser refresh; this ADR adds no new push channel, it consumes existing
  ones per pane.
- Surface parity: the browse ops are added to `docs/development/surface-parity.md`;
  the LSP's static `documentSymbol` is the cold-mode peer of `browse-protocols`.

### DDD Model Impact

Extends the **Language Service** and **Runtime** bounded contexts
(`docs/beamtalk-ddd-model.md`):

- **New Value Objects:** `ClassRow`, `ProtocolTree`, `MethodSource`,
  `ClassDefinition` — the four browse row/tree shapes (JSON-shaped values on the
  `{value, …}` tag).
- **New Domain Service:** `SystemBrowserQuery` — projects `SystemNavigation` scope +
  xref rows + static `ProjectIndex` into the four browse shapes, computing `origin`/
  `disk_differs`. Belongs to Language Service; reads from Runtime (xref, registry)
  when a workspace is attached (the same controlled Tier-3 dependency ADR 0024's
  `LiveConnector` established).
- **Extended Value Object:** `origin :: static | both | runtime` is the row-level
  analog of ADR 0024's `TierSource`.

## Implementation

The next issue (**BT-2488**) implements the four op shapes. High-level approach:

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_nav.erl` (or a new `beamtalk_repl_ops_browse.erl`) | Four `handle_term/4` clauses: `browse-classes`, `browse-protocols`, `browse-method-source`, `browse-class-definition`; each returns `{value, JsonValue}` |
| `runtime/.../beamtalk_repl_protocol.erl` | Per-op JSON encoders (identity for `{value, …}`, but keep the thin `handle/4` WS wrapper) |
| `stdlib/src/SystemNavigation.bt` | Scope projection helpers if needed (`selectorsForClass:` grouped by protocol); reuse, don't duplicate, the xref-backed gather (ADR 0087) |
| `runtime/.../beamtalk_xref.erl` (read) | Read `source_status` / `provenance` / `line` per `{class, side, selector}` for the protocol/selector rows — already maintained, this is a read |
| `runtime/.../beamtalk_runtime_api.erl` | `category` reflection; `aClass sourceString` (patch-aware, BT-2196/ADR 0085) for definition + method source; state-slot reflection (ADR 0035) |
| `crates/beamtalk-core/src/language_service/` | Cold-mode (static) answers for the four ops from `ProjectIndex` / `ClassHierarchy` (ADR 0024 Tier 1/2), tagged `origin = "static"` |
| `crates/beamtalk-mcp`, `editors/liveview` | Consume the four ops (LiveView is the primary consumer; MCP for parity) |
| `docs/development/surface-parity.md` | Record the four browse ops |

### Phasing

| Phase | Scope | Depends on |
|---|---|---|
| 0 — wire-check | `browse-classes` end-to-end (term → JSON → one LiveView pane) against a live workspace; validate `origin` tagging | BT-2399 (done), ADR 0087 (done) |
| 1 — live browse | All four ops, live mode (registry + xref + patch-aware source); `origin`/`source_status`/`disk_differs` | Phase 0 |
| 2 — cold mode | Static answers from `ProjectIndex` (ADR 0024 Tier 1/2), so the browser works with no workspace; `origin = "static"` | Phase 1 |
| 3 — RBAC sign-off | Assert the no-user-code guarantee (ADR 0091 Decision 4 acceptance criterion); grant Observer | Phase 1 |

Phase 0 is the napkin wire-check (one pane, one op) before broad build, mirroring
ADR 0087's Phase 3 pattern.

## References
- Related issues: BT-2483 (this ADR), BT-2482 (LiveView IDE epic), BT-2488
  (implements the op shapes), BT-2201 (`SystemNavigation` API), BT-2399 (term-op
  contract), BT-2228 (xref index epic)
- Related ADRs:
  - ADR 0024 — Static-First, Live-Augmented IDE Tooling (the static/live model)
  - ADR 0087 — Maintained xref Index (`source_status`/`provenance`, sub-ms selector rows)
  - ADR 0085 — Editor Live-Image Representation (the editor-buffer counterpart; shared source substrate)
  - ADR 0091 — Phoenix Authenticated Front (curated facade, Observer read-surface, no-user-code gate)
  - ADR 0093 — Announcements / Event Substrate (push channels driving browser refresh)
  - ADR 0017 — Browser Connectivity (Phase 3 LiveView IDE)
  - ADR 0082 — Method-Level Edit and Save (ChangeLog delta = `disk_differs`)
  - ADR 0070 — Package Namespaces (class category source)
  - ADR 0036 — Full Metaclass Tower (class-side / metaclass row keying)
  - ADR 0035 — field reflection (state/comment without user code)
  - ADR 0038 — ClassBuilder (file-less classes; null definition)
- Documentation:
  - `docs/development/repl-op-term-contract.md` (BT-2399 op layer)
  - `spikes/cockpit-ux-spike/image.js` (the implied browse contract)
  - `stdlib/src/SystemNavigation.bt`
  - `docs/development/surface-parity.md`
