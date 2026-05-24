# ADR 0085: Editor Live-Image Representation

## Status
Proposed (2026-05-24)

## Context

### Problem

A Beamtalk workspace is a running BEAM image: classes are loaded, methods are
hot-patched live (`>>`, ADR 0066), extension methods are registered at runtime
(ADR 0066), and classes can be built programmatically with no source file at all
(ADR 0038 ClassBuilder). The editor, meanwhile, renders **on-disk `.bt` files**.
These two views drift:

- A `Counter >> increment => ...` live patch changes the in-memory method. The
  disk file is unchanged.
- An extension method or a `ClassBuilder`-created class may have no on-disk
  representation the editor can open at all.
- ADR 0082 (the write/persistence model) deliberately writes to disk **only on
  explicit `Workspace flush`**. So in the window between a live patch and a
  flush, the editor buffer shows the *last-flushed disk* content — not what the
  running system is actually executing.

There is no mechanism that makes the editor **render the live image**. The user
asked the obvious question: how can VS Code show the running image rather than
the stale on-disk format?

### The decisive technical fact

You cannot render pre-flush, in-memory state in the editor *from disk*. There are
exactly two ways to put the live image in front of the user:

1. **Write memory to disk on every patch** (write-through) so the file always
   equals the image — explicitly **rejected by ADR 0082** (Alternative B), and it
   would drag editor workflow back into the persistence decision.
2. **Serve the editor buffer from the image** via a virtual document whose
   content is the running system's source — *not* a file on disk.

This ADR chooses (2). It is therefore a decision about **representation** (what
the editor renders, and from where), strictly separate from **persistence** (how
and when edits reach disk), which ADR 0082 owns.

### Current state

| Capability | Today |
|---|---|
| Read-only virtual docs in VS Code | `StdlibContentProvider` serves `beamtalk-stdlib:///X.bt` via the LSP `beamtalk-lsp/fetchContent` request (reads stdlib files) |
| Live workspace connection | `WorkspaceClient` WebSocket (ADR 0046 / ADR 0017), with a `classes` push channel (`{event: "loaded"}`) |
| Live source of a loaded class | `beamtalk_workspace_meta:get_class_source/1` — the in-memory class source, updated on whole-class **load/reload** (including the REPL loader's `load_recompiled_method` recompile path). **Not** updated by the `beamtalk_extensions:register` patch chokepoint that ADR 0082 hooks; a bare `>>` patch leaves the class source stale. Per-method `>>` body text lives separately in `beamtalk_extension_sources` (BT-2196), surfaced via `CompiledMethod source` |
| Class/method enumeration | `SystemNavigation` (BT-2201) |
| Edit → image → disk | **Not built** — ADR 0082 (Proposed, unimplemented) |
| Editor renders the live image | **Does not exist** |

### Constraints

1. **Principle #4 ("No Image, But Live") and #5 ("Code Lives in Files")** — the
   filesystem is the source of truth; there is no binary image snapshot. This ADR
   must not make the image authoritative over files.
2. **ADR 0082 owns persistence.** This ADR must not re-decide flush semantics,
   the ChangeLog, write-through, or conflict handling. Editor workflow stays *out*
   of the persistence issue. Saves route through ADR 0082's primitives; this ADR
   adds none of its own.
3. **ADR 0082 rejected Alternative F (shadow-overlay)** over "editor/runtime
   schism — two competing views of a method." A live-image editor surface must
   not reintroduce that schism.
4. **ADR 0024 keeps the LSP static-first.** The live surface must use the
   existing `WorkspaceClient` WebSocket transport (ADR 0017/0020), not a new
   LSP→runtime connection. The LSP continues to serve cold files statically.
5. **Surface parity** (`docs/development/surface-parity.md`).

## Decision

**Add an image-backed editor representation surface: virtual documents whose
content is fetched from the running image over the existing `WorkspaceClient`
WebSocket. The editor renders the image as a *buffer*; the disk `.bt` file is the
*last-saved projection*; the delta between them is exactly ADR 0082's ChangeLog.**

The model is the universal editor model — **buffer vs. saved file** — mapped onto
the live system:

```text
   beamtalk-live:///Counter.bt   ──render──►  editor buffer   = the live image (in-memory source)
                                                  │
                                                  │ save  →  Counter compile: #sel source: body   (ADR 0082: memory + ChangeLog)
                                                  ▼
   examples/counter.bt           ◄──flush───   disk file       = last saved (ADR 0082: Workspace flush)
```

When the buffer shows the *same in-memory source the runtime dispatches*, the
"two URIs" are not two truths — they are *current image* and *last saved*, and
their difference is the ChangeLog ADR 0082 already maintains. For read-only L1
this is a single coordinated view, unlike Alternative F's split (runtime
dispatches an overlay while the editor shows the base). Editable L2 must hold an
additional invariant to keep that property; see *Resolving the Alt-F schism*
below.

### Design principle: representation only, never persistence

This ADR decides three things and nothing else:

1. **What the editor renders** — the live image (in-memory source), not disk.
2. **The URI schemes** that name image-backed documents.
3. **That edits route to ADR 0082's `compile:source:`** (memory + ChangeLog) and
   that "save to disk" *is* `Workspace flush`.

It deliberately decides **nothing** about *when* disk is written, conflict
resolution, the ChangeLog format, or source-of-truth. Those are ADR 0082.

### Content is a message send (ADR 0040 alignment)

Following ADR 0040/0082's "the language is the API; tools compose Beamtalk
expressions over `evaluate`," the content provider does **not** introduce a new
transport op. It fetches by evaluating a Beamtalk expression over the existing
`evaluate` path:

| Granularity | Beamtalk binding (backed by) | Used for |
|---|---|---|
| Class | `aClass sourceString` (see below) | `readFile` / `provideTextDocumentContent` |
| Method | `(aClass >> #selector) source` (← `CompiledMethod`, ADR 0066 reader form) | method-level browsing |
| Enumeration | `SystemNavigation allClasses`, `aClass selectors` | `readDirectory` |

`aClass sourceString` is the new Beamtalk-level accessor this ADR needs. It must
return source that reflects **in-memory `>>` patches**, which `get_class_source`
alone does not (see Current state). It therefore depends on one of two runtime
changes — this is a dependency, not a freebie:

1. **Re-stitch** the class skeleton from `get_class_source` with each method body
   overridden by its `CompiledMethod source` (the patch-accurate per-method text,
   BT-2196), **or**
2. **Hook** `set_class_source` into the `beamtalk_extensions:register` install
   path so the class source tracks patches directly. This hook is naturally
   co-located with ADR 0082's planned Phase-1 hook on the same chokepoint
   (0082 captures span/`prev_source` there; 0085 would additionally refresh the
   class source).

**Scope of `sourceString`.** It renders classes that have a stored class source
(loaded or reloaded). Classes with **no source entry** — programmatically built
`ClassBuilder` classes (ADR 0038), anonymous/dynamic classes — are an explicit
**non-goal** for this ADR: rendering them would require serialising a class from
in-memory metadata, which is exactly what ADR 0082 deliberately refused to build
(0082: "avoid needing a class-to-source serialiser; we don't reconstruct from
in-memory metadata"). If file-less class rendering is wanted later, the serialiser
is its own decision and its own ADR.

**Document unit = the class's own methods.** A class document renders only the
methods defined *in that class*. **Extension methods contributed by other
packages are excluded** from the document — they live in the extender's source
file (ADR 0066/0082) and flush there, so folding them into this class's buffer
would split the persistence unit. They may appear read-only with provenance in a
future revision, but the editable unit is one class's own methods in one
document, matching the one-class-per-file convention (ADR 0040).

### Phasing: L1 first, L2 deferred

**L1 (read-only) is the committed deliverable. L2 (editable) is deferred** —
designed here so the decision is coherent, but not committed. We ship L1, learn
from real use, and then decide whether to build L2, adopt a cheaper complement
(Alternatives F/G — overlay or diff), or stop. L2 carries the bulk of the cost,
the hard dependency on the (unbuilt) ADR 0082 write path, and the schism risk, so
it should be gated behind demonstrated demand rather than built on spec.

**L1 — read-only Live Browser.** A `beamtalk-live://` `TextDocumentContentProvider`
(generalizing `StdlibContentProvider`, but sourcing content from the image over
the `WorkspaceClient`, not from the LSP). Opening `beamtalk-live:///Counter.bt`
shows the live class source (reflecting reloads, and unflushed `>>` patches once
the patch-aware source path lands — see *Content is a message send*). The
`classes` push channel — extended with `changed`/`removed` events — drives
`onDidChange`, so open live documents refresh whenever the image changes (another
session patches a method, a class is reloaded). Read-only: editing happens on
disk files via ADR 0082.

**L2 — editable Live Image.** A `FileSystemProvider` registered for an editable
`beamtalk-image:/` workspace:

- `readDirectory` → packages → classes → (optionally) methods, from `SystemNavigation`
- `readFile(Counter.bt)` → `Counter sourceString`
- `writeFile` (save) → `Counter compile: #sel source: body` (ADR 0082: memory +
  ChangeLog); compile errors returned and surfaced as diagnostics on the doc
- `onDidChange` → fired from the `classes` push
- "Save to disk" / "Save All" → `Workspace flush` (ADR 0082)

The two phases are independent deliverables on the same substrate (L1 ships
first; L2 is deferred). Their runtime dependencies, stated honestly, are:

- **L1** needs (a) a patch-aware `aClass sourceString` (re-stitch or the
  `set_class_source`-on-patch hook — see *Content is a message send*) for the
  live body to reflect `>>` patches, and (b) the `classes` push channel to
  **emit `changed`/`removed` events from the patch path** (today it emits only
  `class-loaded`, ADR 0046 — so this is new runtime event-emission work, not
  "free" plumbing). A *reduced* L1 that renders last-loaded source and refreshes
  on `class-loaded` only can ship before (a)/(b), and is already ahead of disk
  for reloaded and source-backed classes.
- **L2** additionally needs ADR 0082's write primitives (`compile:source:`,
  `flush`) and the single-editing-surface invariant (see *Resolving the Alt-F
  schism*).

Neither phase is a prerequisite for the other beyond the shared content-fetch and
push-refresh plumbing.

### Mode, not a global switch

- **No workspace attached** → ordinary file editing, static LSP (today's
  behaviour, unchanged).
- **Workspace attached** → the Live Image surface is *available* as an explicit,
  labelled place (`beamtalk-live://` / `beamtalk-image:/`). The user knows they
  are browsing the running system, exactly as a Pharo developer knows the System
  Browser is the image. Disk-file editing remains available side by side.

The two surfaces are never silently mixed: a `file://` buffer is disk; a
`beamtalk-live://` / `beamtalk-image:/` buffer is the image.

### Resolving the Alt-F schism

ADR 0082 rejected shadow-overlays because they create *two uncoordinated sources*
for one method: the runtime dispatches the overlay body while the editor, opening
the disk file, shows the base body — and LSP hover, go-to-definition, `bt fmt`,
and stack traces each have to guess which is canonical.

The strength of this claim differs by phase, and the ADR is honest about that:

- **L1 (read-only) is schism-free.** A read-only buffer renders the live source;
  there is only one editable surface (the disk `file://`). Static tooling
  disambiguates on the URI scheme: `file://` → static/disk (LSP, ADR 0024);
  `beamtalk-live://` → image, read-only. No tool has to guess, and there is no
  second writer.
- **L2 (editable) is NOT automatically schism-free** and must earn the claim. Once
  the image buffer is editable, a method genuinely has two editable views — disk
  `file://Counter.bt` and image `beamtalk-image:/Counter.bt` — which is
  structurally close to the Alternative F situation. L2 avoids the schism only if
  it holds a hard invariant:

  > **A given class is edited through exactly one surface at a time.** When a
  > workspace is attached and the image buffer is the editing surface, the disk
  > `file://` buffer for the same class is treated as read-only-stale (the editor
  > is told not to offer it as a competing editable view), and *vice versa*.
  > Concurrent edits across the two surfaces fall back to ADR 0082's existing
  > external-edit detection and multi-client last-writer-wins rules at flush.

So L1 *is* the completion of ADR 0082's coherence model (it makes the ChangeLog
delta visible as a read-only buffer). L2 *can* be, but only under the
single-editing-surface invariant above; without it, L2 reintroduces the very
fragmentation ADR 0082 rejected. This is the central reason L1 and L2 are
separable (see *Phasing: L1 first, L2 deferred*) and why L2 carries the design risk.

### Editor session (illustrative)

```text
// Workspace running. User hot-patches at the REPL:
> Counter >> increment => self.value := self.value + 2
=> a CompiledMethod (#increment in Counter)     // memory patched, not flushed

// In VS Code, "Browse Live: Counter" opens beamtalk-live:///Counter.bt
// The buffer shows `increment => self.value := self.value + 2`  ← the LIVE body,
// even though examples/counter.bt on disk still says `+ 1`.
//
// NOTE: this requires `sourceString` to reflect the patch (see "Content is a
// message send"). Today a bare `>>` patch does NOT update get_class_source, so
// the live body shows only after the patch-aware source path lands. Until then,
// L1 faithfully renders the last-LOADED/reloaded source — already ahead of disk
// for reloaded classes, but not for un-reloaded `>>` patches.

// L2: editing the live buffer and saving:
//   save  → Counter compile: #increment source: "self.value := self.value + 3"
//           (ADR 0082: installs in memory, appends ChangeEntry)
//   "Save All to Disk" → Workspace flush  (ADR 0082 splices into counter.bt)
```

### Error / edge examples

```text
// No workspace attached:
open beamtalk-live:///Counter.bt
=> "Live Image view unavailable: no workspace connected. Open the .bt file, or
    start a workspace (beamtalk repl)."

// Live-editing a class with no source file (stdlib / ClassBuilder):
//   L2 save still routes to compile:source: → ADR 0082 logs it flushable:false.
//   The editor shows the standard ADR 0082 "memory-only (not flushable: stdlib)"
//   indication. This ADR adds no new rule; it surfaces ADR 0082's.

// Compile error on save (L2):
save beamtalk-image:/Counter.bt   // body references undefined identifier
=> diagnostic on the live doc; memory unchanged, ChangeLog unchanged (ADR 0082).
```

## Prior Art

### Pharo / Squeak — System Browser
The class browser *is* a view of the image; every method pane edits in-image and
"accept" compiles into the running system. There is no file behind the pane.
**Adopted:** the editor renders the image; editing a method compiles it live.
**Adapted:** our buffer is image-backed but the *durable* artifact is a `.bt`
file (via ADR 0082 flush), not an image snapshot — Principle #4 stands.

### GemStone/S
Code lives in a transactional object repository; source text is a projection.
**Observed:** the "edit a projection of live code, commit to make durable" shape
matches ours (commit ≈ `Workspace flush`). **Rejected:** the repository as source
of truth — ADR 0004/Principle #4 chose files.

### VS Code virtual workspaces — `FileSystemProvider`
The GitHub Repositories extension and `vscode.dev` register a `FileSystemProvider`
so an editable workspace is served from a remote store with no local clone;
`readFile`/`writeFile`/`readDirectory` route to that store. **Adopted directly:**
this is exactly L2 — the "store" is the running image. The pattern is proven,
native, and integrates with the editor's normal open/edit/save UX.

### Unison
Code is content-addressed in a codebase database; on-disk files are scratch
buffers that get added to the codebase. **Observed:** an editor surface over a
non-file code store is mainstream and workable. **Diverged:** Beamtalk keeps
files canonical; the image store is the *live* view, not the system of record.

### ElixirLS / Erlang LS
Reflect on compiled/loaded modules for intelligence, but the editor still edits
**files** — they never render module state as the buffer. **Contrast:** this ADR
goes further (render the image as the buffer) precisely because Beamtalk's live
patching and `ClassBuilder` produce code with no file the editor could open.

### Newspeak / Hopscotch
Live, hyperlinked, image-based tool navigation with no offline mode. **Adopted:**
the aspiration of moving fluidly through the live system. **Rejected:** requiring
the image for basic editing — cold-file editing via the static LSP remains.

## User Impact

### Newcomer (from VS Code / Python / JS)
- A "Browse Live" affordance opens what the system is *actually running* — a
  strong "aha" for understanding hot reload. The URI bar clearly reads
  `beamtalk-live://`, signalling "this is the image, not a file."
- Risk: confusion about why a live buffer differs from the disk file. Mitigated by
  the explicit scheme/label and by ADR 0082's dirty/ChangeLog indication that
  explains the delta.

### Smalltalk developer
- This is the System Browser, in VS Code: edit a method, accept (save) compiles it
  live. Immediately familiar. The fileOut equivalent is `Workspace flush`.

### Erlang / Elixir developer
- Finally a way to *see* hot-loaded code without dumping it at the shell. Stays
  consistent with BEAM's memory-vs-disk reality; durable writes remain an explicit
  step (ADR 0082).

### Production operator
- Read-only L1 is a safe live-inspection tool (browse what a node is running).
  L2/editing inherits ADR 0082's safety (memory-only until flush; flush skips
  out-of-tree). Release nodes (no workspace) are unaffected — surface unavailable.

### Tooling developer
- A clean, scheme-keyed boundary: `file://` is static/LSP; `beamtalk-live://` /
  `beamtalk-image:/` is the image over the `WorkspaceClient`. No new LSP→runtime
  connection (ADR 0024 preserved). The `StdlibContentProvider` pattern generalizes.

## Steelman Analysis

### Option A — Image-backed virtual surface (chosen)
- 🧑‍💻 **Newcomer:** "I can finally *see* what hot reload did — the editor shows the running code."
- 🎩 **Smalltalk purist:** "This is the System Browser. Editing the image is the whole point of a live environment."
- ⚙️ **BEAM veteran:** "It reflects loaded module state honestly, and durable writes stay explicit — matches BEAM."
- 🏭 **Operator:** "Read-only L1 is a safe live lens; editing keeps ADR 0082's guardrails."
- 🎨 **Language designer:** "Buffer = image, save = flush. It reuses the universal editor model and completes ADR 0082 instead of fighting it."

### Option B — Stay on disk files + `applyEdit` (ADR 0082 only)
- 🧑‍💻 **Newcomer:** "One model — I only ever edit files."
- 🏭 **Operator:** "Nothing new can render unflushed memory; least surface."
- 🎨 **Language designer:** "Simplest — no new URI schemes."
- **Why rejected:** it does not solve the problem. `applyEdit` fires on *flush*;
  between patch and flush the editor still shows stale disk, and `ClassBuilder` /
  extension code has no file to open. "Represent the live image" is impossible from
  disk without write-through.

### Option C — Write-through to disk on every patch
- 🧑‍💻 **Newcomer:** "Ctrl-S behaves like any editor; file always matches the image."
- 🎨 **Language designer:** "One source, zero pending-state."
- **Why rejected:** ADR 0082 already rejected this (Alternative B) — every REPL
  one-liner mutates the git tree; constant collision with editors and `git pull`.
  It also drags editor workflow into the persistence decision, which we explicitly
  refuse to do here.

### Option D — Image-authoritative (image is the truth; `fileOut` to export)
- 🎩 **Smalltalk purist:** "Pharo proves the image-as-truth model; files are just an export."
- **Why rejected:** violates Principle #4/#5 and ADR 0004. Files stay canonical;
  the image is the live *view*, not the system of record.

### Option E — Read-only forever (L1 only, never editable)
- 🏭 **Operator:** "Zero write risk; pure inspection."
- 🎩 **Smalltalk purist:** *(hostile)* "A browser you can't edit in isn't a Smalltalk browser."
- **Why not chosen as the ceiling:** acceptable as a *first* deliverable, but
  capping at read-only abandons the live-editing experience that motivates the
  whole direction. L1 ships first by dependency, not as the destination.

### Options F/G/H — Overlay, diff, and sidebar (complements, not substitutes)
- 🏭 **Operator:** "A CodeLens 'live body differs' overlay (F) on the real file is
  the safest possible way to surface drift — no new surface, no write path."
- 🎨 **Language designer:** "A live-vs-disk diff (G) makes the ChangeLog delta
  literal; the sidebar/System Browser (H) reuses ADR 0046's transport and keeps
  live browsing visually distinct from file editing."
- **Why not chosen *instead*:** none of the three lets you *work in* the image
  (F/G are viewers; H reimplements editor affordances a document gets for free),
  and none renders not-yet-saved classes. They are genuinely valuable
  *complements* — F especially is the cheapest 80/20 and could precede L1 — so
  they are recorded in *Alternatives Considered*, not dismissed.

### Tension points
- **Newcomer simplicity (B) vs. the live-image goal (A):** B is one model but
  cannot show the image. We accept a second, clearly-labelled surface to deliver
  the capability.
- **Smalltalk purity (D) vs. Principle #4:** purists want image-as-truth; Beamtalk
  keeps files canonical. A delivers the *experience* of image editing while flush
  keeps files authoritative.
- **Operator caution (E) vs. live editing (A):** addressed by phasing — L1
  (read-only) is the safe lens; L2 inherits ADR 0082's safety model.

## Alternatives Considered

### Option B — Stay on disk files + `applyEdit`
See steelman. Cannot render pre-flush memory or file-less classes; rejected.

### Option C — Write-through on patch
See steelman. Rejected by ADR 0082 (Alt B); also couples editor workflow to
persistence. Rejected.

### Option D — Image-authoritative with `fileOut`
See steelman. Violates Principle #4/#5 and ADR 0004. Rejected.

### Option E — Read-only Live Browser only
See steelman. Adopted as L1 (first deliverable), not as the ceiling.

### Option F — Decoration / CodeLens overlay on the real `file://`
Instead of a virtual buffer, annotate the real on-disk file: a CodeLens or gutter
decoration on a method whose in-memory body differs from disk ("live body differs
— ⌥click to view"), with a peek/hover showing the live source. **Pros:** no new
URI scheme, no schism (one editable buffer — the file), trivially safe, reuses the
file's git/format/trace integration. **Cons:** cannot render file-less or
not-yet-saved classes (there's no file to decorate), and it shows the live body in
a peek rather than letting you *work in* the image. **Disposition:** strong
complement to L1, not a replacement — worth shipping alongside or before L1 as the
lowest-risk "see what hot reload did" affordance. Recorded here as a deliberately
considered, cheaper sibling.

### Option G — Live-vs-disk diff view
Open a two-pane diff: disk `file://` on the left, image source on the right —
directly visualising the ChangeLog delta the ADR keeps invoking. **Pros:** makes
"buffer vs saved" literal and obvious; native diff UX; read-only and safe.
**Cons:** a viewer, not an editing surface; doesn't address file-less classes.
**Disposition:** an excellent presentation of the L1 delta; complementary, not a
substitute for an editable image surface.

### Option H — Extend the ADR 0046 sidebar / a System Browser panel
Put live-image browsing (and eventually editing) in the existing Workspace sidebar
(ADR 0046) — a Pharo-style System Browser panel — rather than minting editor
documents. **Pros:** reuses the panel + transport already built; keeps "live
browsing" visually distinct from file editing; a natural home for class/method
trees. **Cons:** a custom panel reimplements editor affordances (syntax
highlighting, multi-cursor, find, extensions) that VS Code documents get for free;
diverges from the editor users already know. **Disposition:** the panel and the
document surface are complementary — the sidebar is the *navigator*, the
`beamtalk-live://` document is the *editor pane it opens into*. Not mutually
exclusive; this ADR chooses documents for the editing surface and leaves the
navigator to ADR 0046.

## Consequences

### Positive
- The editor can render the live image's source — reflecting reloads and, once
  the patch-aware source path lands, in-memory `>>` patches — which a disk-only
  view cannot.
- Reuses the existing `StdlibContentProvider` pattern, the `WorkspaceClient`
  transport, the `classes` push channel, and `get_class_source`.
- Keeps the LSP static-first (ADR 0024); no second LSP→runtime connection.
- Cleanly separated from persistence: this surface routes writes to ADR 0082 and
  decides nothing about disk.
- L1 (read-only) resolves, rather than reopens, ADR 0082's Alt-F schism (one
  read-only view of the same source; delta = ChangeLog).

### Negative
- A second editor surface to learn (file vs. live); mitigated by explicit URI
  schemes and labels.
- Depends on ADR 0082 for the L2 write path; L2 cannot ship before 0082's
  `compile:source:` exists.
- Requires real runtime additions, not just plumbing: a patch-aware
  `aClass sourceString` (re-stitch or a `set_class_source`-on-patch hook) and
  `changed`/`removed` event emission from the patch path (the `classes` channel
  emits only `class-loaded` today).
- `FileSystemProvider` (L2) is substantially more involved than a content
  provider (caching, stat, watch semantics, save/diagnostics round-trip) and
  carries the schism risk; L1 carries most of the value at a fraction of the cost
  and risk (see *Alternatives* F–H).
- L2 reintroduces a second editable view of a class; safe only under the
  single-editing-surface invariant, which is itself new coordination logic.

### Second-order effects over virtual URIs

Virtual documents (`beamtalk-live://`, `beamtalk-image:/`) are not real files, so
file-assuming tooling behaves differently and the ADR must set expectations:

- **Stack traces / error locations / breakpoints** point at on-disk source
  locations (ADR 0033), not virtual URIs. A live buffer is not a debug target;
  navigating a trace opens the `file://` source, not the image doc. Debugging the
  image doc is out of scope here.
- **`bt fmt`, git blame/history, diff** have no real path to operate on. Format
  may run on buffer text in-memory, but git operations are unavailable on the
  virtual scheme by design (the durable artifact is the flushed `.bt`).
- **Find-references / go-to-definition** must keep returning `file://` locations
  (static, ADR 0024); they must not resolve into `beamtalk-image:/` URIs, or
  navigation fragments across two schemes.
- **Flush still spans against the disk file, not the buffer.** ADR 0082's
  byte-span splice reads and parses the on-disk `.bt` to locate the target span.
  Editing the virtual buffer and then flushing is therefore *not* "write the
  buffer to disk" — it is ADR 0082's `compile:source:` (memory) followed by a
  span-splice computed against disk. The virtual buffer and the disk file are
  reconciled only through ADR 0082's machinery, which is the intended boundary.

### Neutral
- New URI schemes (`beamtalk-live://`, `beamtalk-image:/`) join the existing
  `beamtalk-stdlib://`.
- Method-level granularity (open a single method as a doc) is possible on the same
  substrate but is left to implementation; the class-per-document surface with
  method-level `compile:source:` underneath is the default.
- Does not change cold-file editing or the static LSP at all.

## Implementation

### Affected components

| Layer | Change |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` | Patch-aware source: either refresh class source on the `register` chokepoint (co-located with ADR 0082's Phase-1 hook) or expose per-method source for re-stitch; **emit `changed`/`removed` workspace events** on patch/removal |
| `stdlib` (`Behaviour`/`SystemNavigation`) | `aClass sourceString` accessor (patch-aware, per *Content is a message send*); reuse `SystemNavigation allClasses` / `aClass selectors` for enumeration |
| `runtime/apps/beamtalk_workspace` | Extend the `classes` push channel beyond `class-loaded` to carry `changed`/`removed` |
| `editors/vscode/src` | `LiveImageContentProvider` (L1, generalize `StdlibContentProvider`, source over `WorkspaceClient`); `beamtalk-image:/` `FileSystemProvider` (L2); wire push → `onDidChange`; "Browse Live" entry points; route save → ADR 0082 `compile:source:`, "Save to Disk" → `Workspace flush`; enforce the single-editing-surface invariant |
| `crates/beamtalk-mcp` (parity) | Equivalent live class-source read for surface parity, if applicable |
| `docs/development/surface-parity.md` | Record the live-source read + (L2) live-save routing |

### Phased rollout

| Phase | Scope | Effort | Depends on | Tests |
|---|---|---|---|---|
| **0 — wire-check** | Prove the core assumption end to end: fetch one class's live source over the `WorkspaceClient` and render it in a single read-only VS Code doc. Explicitly validate whether the rendered source reflects a `>>` patch — if not, that confirms the patch-aware-source work is required before L1's "shows patches" promise. No providers, no FileSystemProvider. | S | existing transport | Manual + one VS Code extension e2e asserting rendered text = live source |
| **L1 — read-only browser** | `beamtalk-live://` content provider over `WorkspaceClient`; patch-aware `aClass sourceString`; `classes` push `changed`/`removed` → `onDidChange`; "Browse Live" entry point. (A reduced L1 on last-loaded source + `class-loaded` only can land first.) | M | Phase 0; patch-aware source + event emission (runtime) | Runtime EUnit for `sourceString` patch-accuracy; VS Code e2e for browse + live refresh |
| **L2 — editable image** | `beamtalk-image:/` `FileSystemProvider`; save → `compile:source:`; diagnostics round-trip; "Save to Disk" → `flush`; single-editing-surface invariant | L | L1; **ADR 0082** write primitives (`compile:source:`, `flush`) | VS Code e2e for edit→compile→diagnostics and save-to-disk→flush; conflict/invariant tests |

Phase 0 is scaffolding — its deliverable is *evidence the live source renders
correctly* (including the patch question), not a shippable feature. If it shows
the patch-aware-source work is larger than expected, L1 ships in its reduced form
first. **L1 is the committed deliverable; L2 is deferred** pending a decision
after L1 ships (it carries the extra ADR 0082 dependency and the schism risk).

## Migration Path

Not applicable — this ADR is purely additive. Cold-file editing and the static
LSP are unchanged; the live surface only appears when a workspace is attached and
the user explicitly opens a `beamtalk-live://` / `beamtalk-image:/` document. No
existing behaviour, file, or workflow changes.

## References
- Related issues: BT-2201 (SystemNavigation), BT-2228 (xref index), BT-2259
  (ClassBuilder — programmatic class authoring; note: *rendering* file-less
  ClassBuilder classes is an explicit non-goal here, see *Scope of `sourceString`*),
  BT-2215 / BT-2239 / BT-2240 (runtime-attached navigation — to be refocused onto
  this live surface rather than LSP-delegated editor navigation)
- Related ADRs:
  - ADR 0082 — Method-Level Edit and Save in the Live Workspace (the write/persistence counterpart; this ADR's save path)
  - ADR 0024 — Static-First, Live-Augmented IDE Tooling (LSP stays static; transport reuse)
  - ADR 0017 — Browser Connectivity to Running Workspaces (WebSocket transport)
  - ADR 0046 — VSCode Live Workspace Sidebar (`WorkspaceClient`, `classes` push)
  - ADR 0004 — Persistent Workspace Management (memory-only hot reload)
  - ADR 0040 — Workspace-Native REPL Commands (language-is-the-API; content as message send)
  - ADR 0066 — Open Class Extension Methods (`>>` patch/reader forms)
  - ADR 0038 — Subclass ClassBuilder Protocol (file-less classes that need this surface)
- Documentation:
  - `docs/beamtalk-principles.md` — principles #4 (No Image, But Live), #5 (Code Lives in Files)
  - `editors/vscode/src/extension.ts` — `StdlibContentProvider` (pattern generalized here)
  - `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_meta.erl` — `get_class_source/1`
  - `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` — `>>` patch chokepoint + `beamtalk_extension_sources` (BT-2196)
- External:
  - VS Code `FileSystemProvider` API: <https://code.visualstudio.com/api/references/vscode-api#FileSystemProvider>
  - VS Code `TextDocumentContentProvider` API: <https://code.visualstudio.com/api/extension-guides/virtual-documents>
  - Unison content-addressed codebase (files as scratch over a code store): <https://www.unison-lang.org/docs/the-big-idea/>
