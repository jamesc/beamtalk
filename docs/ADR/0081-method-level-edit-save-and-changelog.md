# ADR 0081: Method-Level Edit and Save in the Live Workspace

## Status
Proposed (2026-05-17)

## Context

### Problem

The runtime already supports live, in-memory method patching via the `>>` operator (principle #11 in `docs/beamtalk-principles.md`, ADR 0066 for the syntax). A browser-based IDE (ADR 0017 Phase 3, the Phoenix LiveView upgrade) needs an additional capability that does not exist today: a user-driven *save* action on a single method that travels back through the runtime and reaches the on-disk `.bt` file in a controlled, observable way.

We do not have a path for this. Today:

- `>>` mutates the in-memory class and never touches disk
- `ClassName reload` (ADR 0040) recompiles the whole `.bt` file *from* disk into memory — one direction only
- `load-source` (browser internal op) compiles a full class source string into memory — also memory-only
- `Counter sourceFile` (Behaviour.bt:287) records the file association per class, so memory→disk is *addressable* but not *implemented*
- No op exists to write a single method, the whole class, or any subset back to its source file

Without an explicit decision, any browser "Save" button has to choose silently between (a) memory-only patching that disappears on workspace restart, (b) write-through that mutates the user's git tree on every keystroke save, or (c) something in between. Each choice has implications for ADR 0004 (memory-only hot reload), ADR 0017 (browser IDE), ADR 0024 (LSP/runtime coherence), and ADR 0046 (VSCode coexistence).

### Current State

| Concern | Today |
|---------|-------|
| Live method patch | `Counter >> increment => self.value := self.value + 1` — works, memory-only |
| Source file association | `Counter sourceFile // => "examples/counter.bt"` — tracked per class |
| Whole-class reload from disk | `Counter reload` — works |
| Whole-class load from string | `load-source` REPL op — works, memory-only |
| Single-method save to disk | **Does not exist** |
| Per-method dirty tracking | **Does not exist** |
| Undo / ChangeLog | **Does not exist** |
| External-edit detection | **Does not exist** |
| LSP write coordination | **Does not exist** (LSP currently only reads) |

### Constraints

1. **ADR 0004** is explicit: "Hot reloaded code is **memory-only**. If the node restarts, it loads code from disk (release files), not the hot-reloaded version. This is a fundamental BEAM characteristic." Any persistence decision here must reconcile with that contract.
2. **Principle #5 ("Code Lives in Files")** declares the filesystem the source of truth and says: "compiler reads from filesystem; tooling writes changes back to files." Tooling writing back is sanctioned; the runtime *unilaterally* mutating user source files is not.
3. **Principle #12 ("Compiler is the Language Service")** mandates a rich AST with trivia preservation. We have the substrate for non-destructive splice into existing source files.
4. **Surface parity** (`docs/development/surface-parity.md`, mandated in CLAUDE.md): an operation reachable from REPL, MCP, LSP, and browser must produce equivalent effects on every surface. Different-rules-per-surface is forbidden.
5. **Production safety**: release nodes (no workspace) must be unaffected. A production triage REPL session must never accidentally mutate code on disk.

## Decision

**Adopt explicit-flush semantics backed by a workspace-local ChangeLog. Live patches mutate memory and append to the ChangeLog; disk writes occur only on explicit `Workspace flush` (or the IDE-level equivalent).**

### Model

```
┌──────────────────────────────────────────────────────────────────────┐
│                          .bt source file                             │
│                       (source of truth on disk)                      │
└────────────────────────────▲─────────────────────────────────────────┘
                             │ flush (explicit)
                             │ — splice via trivia-preserving printer
                             │ — atomic temp-rename
                             │ — workspace/applyEdit notify to LSP
                             │
┌────────────────────────────┴─────────────────────────────────────────┐
│                       ChangeLog (per workspace)                      │
│              append-only log of method-level patches                 │
│              persists across workspace restarts                      │
└────────────────────────────▲─────────────────────────────────────────┘
                             │ append on every live patch
                             │
┌────────────────────────────┴─────────────────────────────────────────┐
│                 In-memory class (hot-reloaded BEAM)                  │
│      Counter >> increment    save-method op    load-source op        │
└──────────────────────────────────────────────────────────────────────┘
```

### Behaviour

- A successful `>>` patch (or new `save-method` REPL op) does two things atomically: installs the new method in memory **and** appends a `ChangeEntry` to the workspace ChangeLog.
- The ChangeLog is the dirty state. A method is "dirty" iff there is an unflushed ChangeEntry whose target is that method, that class, or that file. Granularity is per-method; aggregate views (per-class, per-file) are derived.
- `Workspace flush` walks pending ChangeEntries, groups them by source file, splices each set into the file via the trivia-preserving printer, writes via temp+rename, then prunes the flushed entries from the log.
- `Workspace flush: Counter` flushes only entries targeting that class. Similar for `flush: #{file: "..."}`.
- `Workspace clearChanges` discards the ChangeLog without writing. Memory still holds the latest patched versions until the next workspace restart, when disk wins.
- An `autoflush` workspace setting (boolean, default `false`) inverts the default: live patches flush immediately. This is one switch, applied uniformly across all surfaces — surface parity preserved.

### Surface

| Surface | Binding |
|---------|---------|
| Beamtalk | `Workspace flush`, `Workspace flush: aClass`, `Workspace clearChanges`, `Workspace changes`, `Workspace dirty`, `Workspace dirtyMethods` |
| REPL meta-command | `:flush`, `:flush <Class>`, `:changes`, `:dirty` |
| MCP | `flush`, `list_changes`, `dirty_methods` |
| LSP | client-driven via `workspace/executeCommand` for `flush`; `workspace/applyEdit` consumed *from* runtime on flush so VSCode buffers refresh |
| Browser | per-method "Save" button → save-method op → ChangeLog. Workspace-level "Save All" → flush. Dirty indicators on tab/method tree. |

### REPL session

```beamtalk
> Counter >> increment => self.value := self.value + 1
=> a CompiledMethod (#increment in Counter)        // memory patched
> Workspace dirty
=> true
> Workspace dirtyMethods
=> #{Counter -> #{#increment}}
> Workspace changes
=> a ChangeLog with 1 entry
> Workspace flush
=> flushed 1 method across 1 file
> Workspace dirty
=> false
```

### Error examples

```beamtalk
> Integer >> double => self * 2          // stdlib class, no source file
=> error: cannot patch Integer — stdlib classes are sealed against
         live editing (sourceFile is nil)

> Counter >> bogus => undefinedSym       // compile failure
=> error: undefined identifier 'undefinedSym' in #bogus
         — memory unchanged, ChangeLog unchanged

> Workspace flush                        // external edit collision
=> error: external edit detected in examples/counter.bt
         (mtime advanced; content hash differs).
         Pending: 2 methods. Choose:
           Workspace flush:force          // overwrite disk
           Workspace clearChanges         // discard memory edits
           Workspace diff: counter.bt     // inspect conflict
```

### Cross-cutting decisions

| Concern | Decision |
|---------|----------|
| Splice strategy | Trivia-preserving printer driven from the AST. Replace only the method's source span; leave surrounding comments, blank lines, and ordering intact. No reflow, no `bt fmt` on write. |
| Atomicity | Write `<file>.tmp` → fsync → atomic rename. Memory install precedes disk write; ChangeEntry is only pruned after rename returns. A crash between install and rename leaves the entry pending — retried on next flush. |
| Compile failure on patch | Memory unchanged, ChangeLog unchanged, error surfaced. |
| Compile failure on flush | Should not happen — patches were already compiled into memory. If parse of the *target file* fails (file got corrupted externally), flush aborts and reports the unparseable file. |
| External-edit detection | At flush time, compare per-file `(mtime, content-hash)` against the snapshot captured when the first pending ChangeEntry for that file was logged. Mismatch → conflict; pending entries remain in the log; user chooses force/discard/diff. |
| LSP coordination on flush | Runtime emits `workspace/applyEdit` for each flushed file; VSCode reloads the buffer. If the editor has unsaved changes, VSCode's standard conflict dialog applies. |
| Multi-client (two browsers) | Last-writer-wins on memory install. Both clients' ChangeEntries land in the log; on flush, the second client's entry shadows the first for the same method. Each browser session observes the dirty set and shows "modified by another session" when its local view drifts. |
| ChangeLog format | Append-only JSON-Lines under workspace data dir: `<workspace>/changes.jsonl`. Each entry: `{ts, class, selector, kind: "instance"\|"class", source, prev_source, sourceFile, author}`. Survives workspace restart. |
| ChangeLog growth | Bounded ring of last N=1000 entries by default. Older entries archived to `changes-<timestamp>.jsonl.gz` on flush. |
| Undo | `Workspace revert: aMethod` re-installs `prev_source` from the ChangeLog and appends a new revert entry (revert is itself a patch, not log mutation). |
| Release builds | No-op. Release nodes do not start a workspace; ChangeLog code is in `beamtalk_workspace`, not `beamtalk_runtime`. |
| Dynamic classes (ADR 0038, ClassBuilder) | `sourceFile => nil` ⇒ patches are memory-only and never logged. Same for stdlib classes. |
| Extension methods (ADR 0066) | A class adding extension methods to a foreign class has its own `sourceFile`; the patch is logged against the extender's file, not the extended class's file. |
| `autoflush: true` | Memory install → flush in the same transaction. On flush failure (external-edit conflict, write error), the install is *rolled back* in memory before surfacing the error. This is the only mode where memory ever rolls back. |

## Prior Art

### Pharo / Squeak Smalltalk

Pharo's `.changes` file is the canonical reference. Every method edit appends a chunk to the changes file before the image even commits the change to its method dictionary. The `.changes` log is browsable, replayable, and is what makes "save in place" tolerable — you can always recover an overwrite. Pharo decouples the *log* from the *image snapshot*: log is continuous, snapshot is on-demand.

**Adopted:** append-only log of method-level patches, used both as dirty tracker and as undo store.
**Adapted:** the log persists across workspace restarts (matching Pharo's `.changes` durability) but unlike Pharo there is no image — flush writes the splice into the `.bt` source files instead of into a binary image.
**Rejected:** Pharo's auto-write-on-edit behaviour. Pharo's image-based model means "write" doesn't touch user-visible files. Our files *are* user-visible (and version-controlled), so silent writes on every edit are wrong by default.

### Erlang / Elixir

Erlang's `code:load_binary/3` and Elixir's `Code.compile_string/2` install modules from in-memory source. Neither has a "save back to file" path — production releases ship `.beam` only, and source-editing happens externally in editors. ElixirLS and Erlang LS read files; they never write code back.

**Adopted:** the runtime-is-loader, editor-is-writer split. Our flush operation is the explicit bridge; without it, the runtime stays in the BEAM tradition of memory-only patching.
**Rejected:** the *implicit* split where memory and disk simply never reconcile. We need explicit reconciliation because the IDE story demands it.

### Newspeak / Hopscotch

Newspeak has no global namespace and edits happen inside a Hopscotch browser against an image. The whole notion of "splice into a source file" doesn't apply — the image *is* the source. We diverge because we explicitly rejected the image model in ADR 0004.

### LSP — `workspace/applyEdit`

The LSP spec defines a server-initiated edit message that clients (VSCode, etc.) handle by applying changes to open buffers, prompting on conflict, and refreshing on-disk state. This is the *only* sanctioned protocol for "an external process is about to modify a file the editor may have open."

**Adopted:** flush emits `workspace/applyEdit` per file. VSCode handles the conflict UX for us.

### Git's index vs working tree

The two-stage model (stage with `git add`, commit with `git commit`) is the closest mainstream analogue to our memory + ChangeLog + flush split. The ChangeLog is roughly the index; flush is roughly commit. The conceptual familiarity is useful to lean on in user docs.

## User Impact

### Newcomer (from VSCode / Python / JS)

- "Ctrl-S in the browser editor" is **not** an immediate file write by default. Surprising at first.
- Mitigation: the dirty indicator and a one-click "Save All to Disk" button make the model legible. The newcomer doesn't have to learn the word "flush" — the button does it.
- For users who genuinely want editor semantics: flip `autoflush: true` once in workspace settings.
- Discoverability: `:dirty` and `:flush` are short REPL commands; the browser has visible affordances.

### Smalltalk developer

- Recognises the `.changes` model immediately. The two-step *patch → flush* sequence matches their muscle memory from Pharo.
- `Workspace changes` browser maps directly to Pharo's ChangeLog browser.
- The departure from Pharo: there is no image; flush writes to `.bt` source files. Smalltalkers who learned Pharo's "filesOut" workflow will find this *closer* to filesOut than to image saving.

### Erlang / Elixir developer

- The memory/disk decoupling matches BEAM's hot-reload-is-memory-only contract (ADR 0004).
- Flush gives them an explicit reconciliation step they did not previously have. The alternative (write-through) would surprise them more — Erlang/Elixir tooling has never written code from a running node back to source.
- Production triage on a release node is unaffected: no workspace, no ChangeLog, no flush path.

### Production operator

- ChangeLog is an **audit trail**: every in-memory patch is recorded with timestamp and author (REPL session, MCP tool name, browser session id).
- "Was this fix flushed or is it still in memory?" has a definitive answer (`Workspace dirty`).
- Workspace restart loses unflushed patches — this is *desirable* in production: emergency in-memory fixes do not silently become permanent.

### Tooling developer (LSP/IDE)

- LSP gains a write path via `workspace/executeCommand` for flush and `workspace/applyEdit` *from* the runtime.
- Surface-parity table grows by one operation set; the drift-check binary will catch missing bindings.
- The trivia-preserving printer is a reusable asset for refactorings (rename, extract, inline) — flush is the first user but not the last.

## Steelman Analysis

### Alternative A — Memory-only (status quo + "Export Changes")

- 🧑‍💻 **Newcomer:** "There is zero risk of an IDE save mangling my git tree. I can copy-paste the export when I'm ready."
- 🎩 **Smalltalk purist:** "Image-style; no surprises about file representation."
- ⚙️ **BEAM veteran:** "This is the only option that preserves the existing memory-only invariant *literally*. Anything else is a new contract."
- 🏭 **Operator:** "Best for production triage — patches cannot leak to disk by accident."
- 🎨 **Language designer:** "Smallest surface. The export step is human and intentional, like `:show-codegen` is."
- **Why rejected:** users *will* lose work on workspace restart with no warning; the IDE story still ends in copy-paste; no audit trail for what was changed in memory.

### Alternative B — Write-through (every patch writes immediately)

- 🧑‍💻 **Newcomer:** "Ctrl-S writes the file like every other editor. The model is one model."
- 🎩 **Smalltalk purist:** "Closest to image semantics: the source-of-truth and the running system are always in agreement."
- ⚙️ **BEAM veteran:** "If we are going to write at all, write transactionally. One model is easier to reason about than two."
- 🏭 **Operator:** "Every change is in git history (after the user commits). Audit trail is the git log."
- 🎨 **Language designer:** "Maximally consistent."
- **Why rejected:** every transient `>>` from a REPL one-liner or an MCP agent mutates the user's git tree; collision with VSCode unsaved buffers and `git pull` is constant; rollback on compile failure means rolling back disk too, which is fiddly; violates the safety property operators rely on.

### Alternative D — REPL memory-only, browser-editor write-through

- 🧑‍💻 **Newcomer:** "Each surface behaves like its native idiom — the REPL is a REPL, the editor is an editor."
- 🎩 **Smalltalk purist:** "Surfaces are different tools; different rules are fine."
- ⚙️ **BEAM veteran:** "REPL stays clean for production work."
- 🏭 **Operator:** "I never accidentally write code from a REPL session."
- 🎨 **Language designer:** "Two-surface honesty: don't pretend they're the same."
- **Why rejected:** violates the surface-parity contract in CLAUDE.md (`docs/development/surface-parity.md` line 7: equivalent effects across surfaces unless explicitly `surface-specific`). The same patch from MCP and from the browser would produce different on-disk outcomes — exactly the drift surface-parity exists to prevent. The drift-check binary would have to whitelist this, which we have collectively decided not to do.

### Tension points

- **Newcomer vs principle #4 (No Image, But Live):** newcomers expect editor semantics; the language explicitly rejected the image model that makes editor semantics safe. ChangeLog + flush is the bridge.
- **REPL ergonomics vs operator safety:** every option that makes the REPL feel more "live" makes production safer or less safe. Option C lands on the operator's side by default, gives ergonomics back via `autoflush`.
- **Smalltalk purity vs BEAM idiom:** Smalltalk wants the running system *to be* the source of truth. BEAM wants source files to be the source of truth and memory to be a fast cache. ADR 0004 chose BEAM; this ADR honours that choice while giving back the Pharo-like *log* that made Smalltalk's model tolerable.

## Alternatives Considered

### Alternative A — Memory-only (status quo + "Export Changes")
See steelman above. Smallest surface; loses work; no audit trail; rejected.

### Alternative B — Write-through (every patch writes immediately)
See steelman above. Maximally consistent but operationally dangerous; constant collision with editors and source control; rejected.

### Alternative D — REPL memory-only, browser-editor write-through
See steelman above. Matches per-surface intuition but violates surface parity; rejected.

### Alternative E — Image-style snapshot (revisit ADR 0004)
Drop file-based source entirely; persist the workspace as a binary image. Rejected by ADR 0004 with extensive rationale; this ADR does not revisit that decision.

## Consequences

### Positive

- ADR 0004 ("hot reload is memory-only") is honoured *literally*, with an explicit reconciliation step.
- Per-method dirty tracking, undo, and audit trail fall out of one mechanism.
- Trivia-preserving splice is a reusable asset for future refactorings (rename, extract, inline).
- Production triage remains safe by default; flush is opt-in per patch and opt-out per workspace.
- Capability #8 from the prior "missing IDE features" list (ChangeLog / undo) is delivered as a byproduct.
- Surface parity is preserved across REPL, MCP, LSP, browser.

### Negative

- Two-step save is unusual for editor users; mitigated by `autoflush: true` and visible "Save All" UI.
- Trivia-preserving printer is non-trivial; bugs in the splice corrupt user source. Mitigation: temp+rename atomicity, byte-range fallback if AST splice fails, ChangeLog retains prev_source for revert.
- ChangeLog growth on long-lived workspaces requires pruning policy.
- Multi-client coordination is last-writer-wins; concurrent edits to the same method by two users will lose one — observable but not prevented.

### Neutral

- ChangeLog persistence introduces a new on-disk artifact (`<workspace>/changes.jsonl`). Backup story is "it lives next to the workspace; back it up with the workspace."
- `autoflush: true` collapses the model to Alternative B at the per-workspace level. Users who want write-through can have it; the default does not.
- Stdlib and dynamic classes (no `sourceFile`) silently accept patches as memory-only. This matches today's behaviour for `>>` against `Integer`, but the ChangeLog will not contain entries for them — they are not flushable by definition.

### DDD Model Impact

- **Compilation context** owns the trivia-preserving printer (extends existing AST/printer infrastructure).
- **REPL context** owns the ChangeLog gen_server and the flush op. New module: `beamtalk_repl_changelog.erl`.
- **Workspace context** gains `Workspace flush`, `Workspace changes`, `Workspace dirty`, `Workspace dirtyMethods`, `Workspace revert:`, `Workspace clearChanges`.
- **No language-service changes** — the LSP server consumes `workspace/applyEdit` notifications from the runtime, which is a one-way bridge already supported by the protocol.

## Implementation

### Affected components

| Layer | Change |
|-------|--------|
| `crates/beamtalk-core/src/codegen/` | New trivia-preserving splice printer: given an AST and a target method span, produce a new file body with only that method's span replaced. Reuses existing trivia model from ADR 0044. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_changelog.erl` (new) | Gen_server owning the append-only ChangeLog. ETS for live state, JSON-Lines on disk. Exposed via the `Workspace` facade per ADR 0040. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` | New ops: `save-method`, `flush`, `list-changes`, `dirty`. Integrated alongside existing `load-source` / `reload` ops. |
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` (or wherever `>>` install lives) | Hook the install path to emit a ChangeEntry to the workspace ChangeLog process when one is reachable. Stdlib / dynamic classes (`sourceFile = nil`) skip the emit. |
| `stdlib/src/Workspace.bt` | New facade methods: `flush`, `flush:`, `changes`, `dirty`, `dirtyMethods`, `revert:`, `clearChanges`. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands: `:flush`, `:flush <Class>`, `:changes`, `:dirty`. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `flush`, `list_changes`, `dirty_methods`. |
| `crates/beamtalk-lsp/src/server.rs` | Handle `workspace/executeCommand` for `flush`. Emit `workspace/applyEdit` *to* clients on flush events received from the runtime. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | Per-method dirty indicator, "Save" per method, "Save All to Disk" workspace-level. |
| `docs/development/surface-parity.md` | Add rows for `save-method`, `flush`, `list-changes`, `dirty`. |
| `docs/beamtalk-language-features.md` | Document `Workspace flush` semantics. |

### Phased rollout

| Phase | Scope | Effort |
|-------|-------|--------|
| **0** | ChangeLog gen_server + JSON-Lines persistence + `Workspace changes` / `dirty` (read-only). No flush yet. Validates the log mechanism end-to-end before any disk-write code is written. | S |
| **1** | Trivia-preserving splice printer in `beamtalk-core`. Unit tests covering: replace single method, preserve surrounding comments, preserve blank lines, preserve method order, gracefully fail on un-parseable target file. | M |
| **2** | `Workspace flush` + `flush:` + atomic temp+rename + external-edit detection. ChangeEntry pruning on success. Wire to existing `>>` install path. | M |
| **3** | `save-method` REPL op, MCP tool, LSP `executeCommand`, browser "Save" UI. Surface-parity table updated. | M |
| **4** | `Workspace revert:` + `clearChanges` + `autoflush` setting. ChangeLog browsing UI in the browser workspace. | S |
| **5** | LSP-side `workspace/applyEdit` consumption in VSCode + e2e test that flush refreshes an open buffer. | S |

Total: ~M-L across 6 phases; Phase 0 is independently shippable as a read-only audit log even without the flush story.

## Migration Path

No user code changes required. Existing `>>` patches continue to work identically — they now additionally append to the ChangeLog (silently, until the user looks).

For users who today rely on workspace-restart wiping memory patches (intentional ephemerality): behaviour is preserved. The ChangeLog persists across restart but memory does not; on restart, disk wins, and the ChangeLog contents become orphaned entries (patches whose "memory state" is no longer installed). A `Workspace clearChanges` after restart is the recommended hygiene; phase 4 can auto-prompt.

For ADR 0046 (VSCode sidebar): no migration. The sidebar gains a "Workspace dirty" indicator and a "Flush" command surface as a phase-3 deliverable.

## References

- Related issues: BT-XXX (this ADR; issues to be created via `/plan-adr`)
- Related ADRs:
  - ADR 0004 — Persistent Workspace Management (memory-only hot reload contract)
  - ADR 0017 — Browser Connectivity to Running Workspaces (Phase 3 LiveView IDE that motivates this)
  - ADR 0024 — Static-First, Live-Augmented IDE Tooling (LSP/runtime coherence rules)
  - ADR 0033 — Runtime-Embedded Documentation (source-location tracking precedent)
  - ADR 0040 — Workspace-Native REPL Commands (class-based reload; the read-direction counterpart)
  - ADR 0044 — Comments as First-Class AST Nodes (trivia model used by splice printer)
  - ADR 0046 — VSCode Live Workspace Sidebar (consumer of `workspace/applyEdit`)
  - ADR 0066 — Open Class Extension Methods (`>>` syntax, the patch operator)
- Documentation:
  - `docs/beamtalk-principles.md` — principles #4, #5, #11, #12
  - `docs/development/surface-parity.md` — drift contract this ADR must satisfy
  - Pharo `.changes` file model: <https://books.pharo.org/booklet-PharoToolingHandbook/pdf/2017-02-PharoToolingHandbook.pdf>
  - LSP `workspace/applyEdit`: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>
