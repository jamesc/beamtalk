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

```text
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

**Intent: ephemeral vs durable.** The ChangeLog records *durable* changes (intent-to-keep). Ephemeral exploration — spike a fix, check it works, throw it away — is supported but does not produce log entries. Intent is signalled by the **operation chosen**, not the identity of the caller:

| Operation | Intent | ChangeLog |
|-----------|--------|-----------|
| `Counter >> selector => body` (REPL/browser) | durable — human typed it | logged |
| `save-method` op (REPL/MCP/LSP/browser) | durable | logged |
| `save-class` op (REPL/MCP/LSP/browser) — new class from source | durable | logged (`kind: "new-class"`) |
| `try-method` op (MCP only) | ephemeral — agent exploration | **not** logged |
| `Counter >> #selector` (reader form) | n/a — pure read | not logged |
| `load-source` of an existing class | ephemeral by default (legacy browser internal op) | not logged unless `intent: "save"` parameter set |

`author_kind` (`human` / `agent` / `test`) is recorded on every entry as **audit metadata**, not as a filter. `Workspace dirty` defaults to *all* logged entries regardless of author — an agent that ran `save-class` for ten new test files produces ten visible dirty entries, because those files are the deliverable.

**Method patch flow (`>>` patcher form, `save-method` op).** A successful patch does three things in sequence: (1) reads and parses `Counter sourceFile` to capture the existing method's exact byte span and source body as `prev_source`; (2) installs the new method in memory; (3) appends a `ChangeEntry` to the workspace ChangeLog. All-or-nothing applies between steps 2 and 3 — if memory install succeeds, the ChangeEntry is emitted; if memory install fails, neither happens. Step 1 has a separate downgrade path: if the disk read or parse fails (file deleted, syntax error introduced externally), the patch downgrades to memory-only — step 2 still runs but step 3 is suppressed and the user is warned (see *Disk-read failure on patch* in Cross-cutting decisions). The disk read in step 1 is what makes `revert:` and splice both work; without it, the hook cannot know what the disk currently contains.

**New-class flow (`save-class` op).** Parameters: `(source, targetPath)`, where `targetPath` is required and must lie inside the project source tree. The op (1) compiles and installs the class in memory, (2) writes a `ChangeEntry` of `kind: "new-class"` with `prev_source = nil`, `span = nil`, and the full class source as `source`. Subsequent `>>` patches against this class log additional entries layered on top; at flush time, entries replay in order — the `new-class` entry writes the initial file, then later method patches splice into it. This avoids needing a class-to-source serialiser; we don't reconstruct from in-memory metadata. `targetPath` is rejected if it already exists on disk (use `save-method` or `>>` against the existing class instead).

**Ephemeral patch flow (`try-method` op, MCP only).** Installs in memory exactly like `>>` but emits no ChangeEntry. Agents use this for exploration: spike a candidate fix, run tests via `evaluate`, observe outcome, and either (a) discard by ignoring it (memory diverges from disk, will be lost on restart — fine for spikes) or (b) promote by calling `save-method` with the same source to log it for real. The `try-method` → `save-method` step is the agent's analogue of a human typing `>>` at the REPL: they tried it interactively, now they want to keep it.

A class is **flushable** iff `sourceFile` is non-nil **and** the source file lies inside the current project's source tree (per the active `beamtalk.toml`). Stdlib classes, dynamic classes (ADR 0038), and classes loaded from package dependencies are **not flushable** — patches against them install in memory but emit no ChangeEntry. This guards reproducible builds against accidental writes into the dependency cache. New classes created via `save-class` are flushable by construction (target path is required to be in-project).
- The ChangeLog is the dirty state. A method is "dirty" iff there is an unflushed ChangeEntry whose target is that method, that class, or that file. Granularity is per-method; aggregate views (per-class, per-file) are derived.
- `Workspace flush` walks pending ChangeEntries, groups them by source file, computes a new file body per file by replacing each target method's recorded byte span with its patched source, and writes each via temp+rename. The splice operates on byte spans recorded at hook time — it does not depend on the formatter being able to round-trip the whole file. ChangeEntry pruning happens **per file as each Phase B rename succeeds**: a partial flush failure leaves a mixed state where successfully-renamed files have their entries pruned and reported as completed, while failed files retain their entries for retry. See *Multi-file atomicity* in Cross-cutting decisions for the full Phase A / Phase B protocol.
- `Workspace flush: Counter` flushes only entries targeting that class. Similar for `flush: #{file: "..."}`.
- `Workspace clearChanges` discards the ChangeLog without writing. Memory still holds the latest patched versions until the next workspace restart, when disk wins.
- An `autoflush` workspace setting (boolean, default `false`) inverts the default: live patches flush immediately. This is one switch, applied uniformly across all surfaces — surface parity preserved. On autoflush failure (external-edit conflict, write error), memory and disk diverge and the user is told so explicitly; we do not roll back the BEAM module install because the prior `.beam` binary may already be unloaded and live actors may hold references to the new closures. Autoflush is therefore best-effort consistency, not transactional consistency.

### Surface

| Surface | Binding |
|---------|---------|
| Beamtalk | `Workspace flush`, `Workspace flush: aClass`, `Workspace clearChanges`, `Workspace changes`, `Workspace dirty`, `Workspace dirtyMethods`, `Workspace saveClass:to:` |
| REPL meta-command | `:flush`, `:flush <Class>`, `:changes`, `:dirty` |
| MCP | `save_method`, `save_class`, `try_method` (ephemeral), `flush`, `list_changes`, `dirty_methods` |
| LSP | client-driven via `workspace/executeCommand` for `flush` and `save_class`; `workspace/applyEdit` consumed *from* runtime on flush so VSCode buffers refresh |
| Browser | per-method "Save" button → `save-method` op → ChangeLog. "New File" action → `save-class` op. Workspace-level "Save All" → flush. Dirty indicators on tab/method tree. |

### REPL session (human, patching existing class)

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

### MCP agent session (spike, then commit new test + impl)

```beamtalk
// 1. Agent explores via try_method — installs in memory, no log entry
mcp> try_method(class: "Counter", selector: "doubled", body: "^ self value * 2")
=> a CompiledMethod (memory only, ephemeral)
mcp> evaluate("(Counter new) doubled")
=> 0                                              // works, agent commits

// 2. Agent promotes the spike to a logged change
mcp> save_method(class: "Counter", selector: "doubled", body: "^ self value * 2")
=> ChangeEntry logged (#doubled in Counter)

// 3. Agent creates a new test class for the feature
mcp> save_class(source: "<DoubleCounterTest source>", target: "test/double_counter_test.bt")
=> ChangeEntry logged (kind: new-class)

// 4. Agent creates the impl file for a follow-up class
mcp> save_class(source: "<DoubleCounter source>", target: "src/double_counter.bt")
=> ChangeEntry logged (kind: new-class)

// 5. Human reviews and flushes
> Workspace dirty
=> true
> Workspace dirtyMethods
=> #{Counter -> #{#doubled},
     DoubleCounterTest -> #new-class,
     DoubleCounter -> #new-class}
> Workspace flush
=> flushed 1 method + 2 new files across 3 files
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
| Splice strategy | **Byte-span replacement**, not AST round-trip. At hook time, the disk file is parsed to locate the target method's exact byte span (start..end including the body and trailing newline); that span is stored on the ChangeEntry. At flush time, a new file body is produced by copying bytes verbatim outside the span and substituting the patched source inside it. No reformat, no AST reprint of unchanged content. This sidesteps the open question of whether the formatter can round-trip every `.bt` file losslessly — only the parser's span resolution must be correct, which ADR 0044's trivia model already supports. |
| Single-file atomicity | Write `<file>.tmp` → fsync → atomic rename. Memory install precedes disk write; ChangeEntry is only pruned after rename returns. A crash between install and rename leaves the entry pending — retried on next flush. |
| Multi-file atomicity | Two-phase per flush operation. **Phase A:** parse every target file, validate every recorded byte span still resolves cleanly, write every `<file>.tmp`. **Phase B:** rename each `<file>.tmp` → `<file>` in sequence. If any Phase A step fails, abort the entire flush; no temp files are renamed; **no ChangeEntries are pruned**. If a Phase B rename fails (POSIX guarantees atomicity but the OS may surface I/O errors), the failed file's entries remain in the log alongside any entries for files that haven't yet renamed; already-renamed files are reported as completed. The user sees a per-file status report. This is the strongest atomicity achievable without filesystem transactions, and it ensures the failure mode is *recoverable via re-flush*, not silent data loss. |
| Compile failure on patch | Memory unchanged, ChangeLog unchanged, error surfaced. |
| Disk-read failure on patch | If `sourceFile` cannot be read or parsed at hook time (file deleted, syntax error introduced externally), the patch downgrades to memory-only: memory is patched, but no ChangeEntry is emitted and the user is warned that the patch will not survive workspace restart. |
| Compile failure on flush | Should not happen — patches were already compiled into memory. Splice is purely byte-level. The only flush-time failure modes are external-edit conflicts and I/O errors. |
| External-edit detection | At flush time, compare per-file `(mtime, content-hash)` against the snapshot captured when the first pending ChangeEntry for that file was logged. Mismatch → conflict; pending entries remain in the log; user chooses force/discard/diff. |
| LSP coordination on flush | Runtime emits `workspace/applyEdit` for each flushed file; VSCode reloads the buffer. If the editor has unsaved changes, VSCode's standard conflict dialog applies. |
| Multi-client (two browsers) | Last-writer-wins on memory install. Both clients' ChangeEntries land in the log; on flush, the second client's entry shadows the first for the same method. Each browser session observes the dirty set and shows "modified by another session" when its local view drifts. |
| ChangeLog format | Append-only JSON-Lines under workspace data dir: `<workspace>/changes.jsonl`. Each entry: `{ts, class, selector, kind: "instance"\|"class"\|"new-class", source, prev_source, sourceFile, span: {start, end} \| null, author, author_kind: "human"\|"agent"\|"test", intent: "save"\|"new"}`. `new-class` entries have `span: null` and `prev_source: null`. Survives workspace restart. |
| ChangeLog growth | Bounded ring of last N=1000 entries by default. Older entries archived to `changes-<timestamp>.jsonl.gz` on flush. Entries with `author_kind: "test"` (logged by `Workspace runTests` for its own setup patches) are pruned aggressively (kept N=200) since they're audit-only and never the user's deliverable. `human` and `agent` entries are retained on equal footing — both represent durable intent and are pruned only by the ring bound. |
| Intent vs author | Intent is signalled by the **op chosen**, not the caller: `save-method`/`save-class` always log; `try-method` never logs; the `>>` patcher form logs (it has no ephemeral mode — humans who want an ephemeral patch can `Workspace clearChanges` after). `author_kind` is **audit metadata** — it tells us *who* made a logged change, not whether it counts as dirty. `Workspace dirty` and `Workspace flush` include all logged entries by default; the `kinds:` keyword filters by `author_kind` for selective flushing if needed (e.g., `Workspace flush kinds: #{agent}` to commit an agent batch separately from human changes). |
| New-class flush | When flushing a `new-class` entry, the splice operation is "write `source` to `targetPath`" (no byte-span surgery; the file doesn't exist yet). External-edit detection still applies: if `targetPath` was created externally between the `save-class` call and flush, the conflict surfaces with the same force/discard/diff choice. Subsequent `>>` patches against a not-yet-flushed new class produce additional entries that replay in order at flush — the `new-class` entry writes first, then later method-patch entries splice into the just-written file. |
| Undo | `Workspace revert: aMethod` re-installs `prev_source` from the most recent ChangeEntry for that method and appends a new revert entry (revert is itself a patch, not log mutation). Revert is only possible for flushable classes — ephemeral memory-only patches against stdlib/dependencies are not recorded and therefore not revertible. |
| Release builds | No-op. Release nodes do not start a workspace; ChangeLog code is in `beamtalk_workspace`, not `beamtalk_runtime`. |
| Stdlib classes | `sourceFile => nil` ⇒ patches are memory-only and never logged. |
| Dynamic classes (ADR 0038, ClassBuilder) | `sourceFile => nil` ⇒ patches are memory-only and never logged. |
| Package dependency classes | `sourceFile` resolves to a path **outside** the current project source tree ⇒ patches install in memory but emit no ChangeEntry. Reproducible-build guarantee preserved. |
| Extension methods (ADR 0066) | A class adding extension methods to a foreign class has its own `sourceFile`; the patch is logged against the extender's file, not the extended class's file. |
| `autoflush: true` | Memory install → flush in the same call. On flush failure (external-edit conflict, write error, multi-file partial), memory and disk diverge — we do **not** attempt to roll back the BEAM module install (prior binary may be unloaded; live actors may hold references to the new closures). The error surfaces with a "memory ahead of disk" warning and the ChangeEntry remains in the log for manual flush. Autoflush is best-effort consistency, not transactional. |

## Prior Art

### Pharo / Squeak Smalltalk

Pharo's `.changes` file is the canonical reference. Every method edit appends a chunk to the changes file before the image even commits the change to its method dictionary. The `.changes` log is browsable, replayable, and is what makes "save in place" tolerable — you can always recover an overwrite. Pharo decouples the *log* from the *image snapshot*: log is continuous, snapshot is on-demand.

**Adopted:** append-only log of method-level patches, used both as dirty tracker and as undo store.
**Adapted:** the log persists across workspace restarts (matching Pharo's `.changes` durability) but unlike Pharo there is no image — flush writes the splice into the `.bt` source files instead of into a binary image.
**Rejected:** Pharo's auto-write-on-edit behaviour. Pharo's image-based model means "write" doesn't touch user-visible files. Our files *are* user-visible (and version-controlled), so silent writes on every edit are wrong by default.

### GemStone/S (GemTalk Systems)

GemStone/S is a multi-user, persistent Smalltalk: classes, methods, and all live objects reside in a transactional object repository, not in source files. Edits happen inside a per-session transaction; `System commitTransaction` makes them durable and visible to other sessions, `System abortTransaction` discards them. Concurrent commits to the same method surface as a first-class `TransactionConflict` that the user resolves explicitly. GemStone has run production multi-developer Smalltalk systems at scale for thirty years — it is the canonical reference for the *workflow shape* this ADR adopts.

**Workflow parallel.** The three core steps map one-to-one:

| GemStone/S | Beamtalk (this ADR) |
|------------|---------------------|
| Edit method → in-session install | `>>` patch / `save-method` → memory install + ChangeEntry |
| `System commitTransaction` | `Workspace flush` |
| `System abortTransaction` | `Workspace clearChanges` |
| `TransactionConflict` on commit | External-edit conflict at flush time |
| File-in from topaz / GBS | `save-class` with `targetPath` |
| Per-session transaction isolation | Single shared workspace; multi-client last-writer-wins (simpler point on the same axis) |

**Adopted:** the *explicit-commit, conflict-as-first-class* model. GemStone proves at production scale that a save → commit → conflict-aware workflow is intuitive when the vocabulary is explicit and the conflict surface is part of the contract, not an afterthought. Our `Workspace flush` and external-edit detection inherit this directly. The "commit/abort/conflict" vocabulary is also worth borrowing in user-facing docs — newcomers from any DB-backed system will recognise it.

**Adapted:** GemStone's per-session transaction isolation. They support arbitrarily many concurrent sessions, each with its own pending edits, reconciled via optimistic concurrency at commit time. We don't need that today — multi-client coordination is last-writer-wins on memory install, and conflict surfaces only at flush against the on-disk file. The model is the same; the scope is narrower. If multi-session isolation becomes a need, GemStone's optimistic-concurrency approach is the upgrade path.

**Rejected:** the object repository as the source of truth. ADR 0004 made the opposite architectural choice — files are the source. GemStone solved the persistence problem by making the database authoritative and treating source text as a projection; we solve it by making the filesystem authoritative and treating memory + ChangeLog as a transactional staging area on top. Architecturally opposite; *user-experience-wise* close enough that GemStone is the strongest single piece of prior art we have for the workflow shape, even though the storage model is mirror-image different.

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
- Byte-span splice depends on the parser correctly resolving every method's span against arbitrary `.bt` files. Phase 0 exists to validate this against the stdlib+examples corpus before any flush code is written. If Phase 0 fails, the design pivots.
- ChangeLog growth on long-lived workspaces requires pruning policy. Human and agent entries are pruned equally by the 1000-entry ring; test entries are pruned aggressively (200) since they are audit-only.
- Two MCP tools (`try_method` ephemeral, `save_method` durable) means agents must choose the right one. Documented in MCP tool descriptions; the typical agent flow is "try → evaluate → save," and getting it wrong produces visible drift between `Workspace dirty` and what the agent intended.
- Multi-client coordination is last-writer-wins; concurrent edits to the same method by two users will lose one — observable but not prevented.
- Autoflush is best-effort consistency, not transactional. On flush failure under autoflush, memory and disk diverge and require manual reconciliation. This is documented behaviour, not a bug to fix — the alternative (rolling back the BEAM module install) is unsound when live actors hold references to the new closures.
- Multi-file flush failure leaves a mixed state across files even after the two-phase protocol — Phase B renames are sequential, and a hard I/O error mid-sequence means some files renamed and some did not. The user gets a per-file status report and can re-flush; entries for already-renamed files are pruned, entries for failed files remain.

### Neutral

- ChangeLog persistence introduces a new on-disk artifact (`<workspace>/changes.jsonl`). Backup story is "it lives next to the workspace; back it up with the workspace."
- `autoflush: true` collapses the model to Alternative B at the per-workspace level. Users who want write-through can have it; the default does not.
- Stdlib and dynamic classes (no `sourceFile`) silently accept patches as memory-only. This matches today's behaviour for `>>` against `Integer`, but the ChangeLog will not contain entries for them — they are not flushable by definition.
- Package dependency classes silently accept patches as memory-only for the same reason — their `sourceFile` is outside the project tree. This is a feature, not a limitation: reproducible builds depend on dependency caches being treated as read-only.
- MCP agents finalising new tests or impl files via `save_class` / `save_method` produce visible `dirty` entries — the deliverable is supposed to be visible. Agents that want to spike-without-noise use `try_method`, which installs in memory but does not log. The split is by op intent, not by author identity.

### DDD Model Impact

- **Compilation context** owns the byte-span resolver — given a source string and a target `(class, selector)`, return the exact byte span of the method definition. Reuses the existing parser; no new printer required.
- **Workspace context** owns the ChangeLog gen_server, the flush op, and the `Workspace` facade extensions. New module: `beamtalk_workspace_changelog.erl` (not REPL-scoped — the ChangeLog is consumed cross-surface by REPL, MCP, LSP, and browser, so DDD-correct placement is the workspace, not the REPL).
- **REPL context** gains thin op handlers (`save-method`, `flush`, `list-changes`, `dirty`) that delegate to the workspace ChangeLog.
- **No language-service changes** — the LSP server consumes `workspace/applyEdit` notifications from the runtime, which is a one-way bridge already supported by the protocol.

## Implementation

### Affected components

| Layer | Change |
|-------|--------|
| `crates/beamtalk-core/src/source_analysis/` | New byte-span resolver: given source text and `(class, selector, kind)`, return the byte span of that method's definition. Pure parser-level work; no new printer. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` (new) | Gen_server owning the append-only ChangeLog. ETS for live state, JSON-Lines on disk. Exposed via the `Workspace` facade per ADR 0040. Lives in the workspace context, not REPL, because it's consumed cross-surface. |
| `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` | The `>>` patch install chokepoint (already exists, 259 LOC). Hook the install path to (1) read+parse `sourceFile` to capture span and `prev_source`, (2) install in memory, (3) emit ChangeEntry. Flushability check (project-tree containment) gates the emit. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` | New REPL ops: `save-method`, `save-class`, `try-method`, `flush`, `list-changes`, `dirty`. Thin handlers delegating to `beamtalk_workspace_changelog`. `try-method` skips the ChangeLog emit; `save-class` validates that `targetPath` is in-project and not pre-existing. |
| `stdlib/src/Workspace.bt` | New facade methods: `flush`, `flush:`, `flush kinds:`, `changes`, `dirty`, `dirty:`, `dirtyMethods`, `revert:`, `clearChanges`, `saveClass:to:`. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands: `:flush`, `:flush <Class>`, `:changes`, `:dirty`. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `save_method`, `save_class`, `try_method`, `flush`, `list_changes`, `dirty_methods`. MCP-issued logged patches are auto-tagged `author_kind: agent` for audit. `try_method` is the explicit ephemeral spike path; `save_method`/`save_class` are the durable-commit path. |
| `crates/beamtalk-lsp/src/server.rs` | Handle `workspace/executeCommand` for `flush`. Emit `workspace/applyEdit` *to* clients on flush events received from the runtime. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | Per-method dirty indicator, "Save" per method, "Save All to Disk" workspace-level. |
| `docs/development/surface-parity.md` | Add rows for `save-method`, `flush`, `list-changes`, `dirty`. |
| `docs/beamtalk-language-features.md` | Document `Workspace flush` semantics. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|-------|-------|--------|-------|
| **0** | **Validation spike** (internal scaffolding, not user-facing). Implement the byte-span resolver and prove it against the entire stdlib + examples corpus: parse, locate every method's span, re-serialise file with a no-op span replacement, and assert byte-identical output. This is the load-bearing assumption of the design — validate it before building anything else. | S | Corpus round-trip tests in `crates/beamtalk-core/src/source_analysis/`. |
| **1** | ChangeLog gen_server + JSON-Lines persistence + `Workspace changes` / `dirty` / `dirtyMethods`. Hooks into `beamtalk_extensions.erl`. No flush yet. `author_kind` plumbing through REPL/MCP. | M | EUnit tests for the gen_server; BUnit tests for the `Workspace` facade reads. |
| **2** | `Workspace flush` + `flush:` + single-file atomic temp+rename + multi-file two-phase (Phase A all writes, Phase B all renames) + external-edit detection. Pruning rules implemented. | M | EUnit tests for atomicity (kill the process between phases); BUnit tests for the facade. |
| **3** | `save-method`, `save-class`, `try-method` REPL ops + MCP tools; LSP `executeCommand` for `flush` and `save_class`; browser "Save" / "New File" UI. Surface-parity table updated; surface-drift CI gate passes. | M | Surface-parity tests; browser e2e for Save and New File; MCP integration tests for the try→save promotion flow. |
| **4** | `Workspace revert:` + `clearChanges` + `autoflush` setting. ChangeLog browsing UI in the browser workspace. | S | BUnit tests for revert; e2e for autoflush. |
| **5** | LSP-side `workspace/applyEdit` consumption in VSCode + e2e test that flush refreshes an open buffer. | S | VSCode extension e2e. |

Total: ~M-L across 6 phases. Phase 0 is scaffolding — its deliverable is *evidence that byte-span splice works on real code*, not a shippable feature. If Phase 0 reveals that the parser cannot reliably resolve method spans against arbitrary `.bt` files, the design pivots before phases 1–5 commit to it.

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
