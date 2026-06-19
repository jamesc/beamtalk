# ADR 0082: Method-Level Edit and Save in the Live Workspace

## Status
Accepted (2026-05-17) — **Shipped 2026-05-26** (all phases 0–5 merged to main, BT-2281 through BT-2292).

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
│      Counter compile:source:    Workspace newClass:at:    load-source │
└──────────────────────────────────────────────────────────────────────┘
```

### Behaviour

**Intent: ephemeral vs durable.** The ChangeLog records *durable* changes (intent-to-keep). Ephemeral exploration — spike a fix, check it works, throw it away — is supported but does not produce log entries. Intent is signalled by the **operation chosen**, not the identity of the caller:

| Operation | Intent | ChangeLog |
|-----------|--------|-----------|
| `Counter compile: #selector source: body` (durable, ADR 0066 `>>` desugars to this) | durable — caller wants to keep it | logged |
| `Counter tryCompile: #selector source: body` (ephemeral) | ephemeral — exploration / spike | **not** logged |
| `Workspace newClass: source at: path` (new-class creation) | durable — caller wants a new file | logged (`kind: "new-class"`) |
| `Counter >> #selector` (reader form) | n/a — pure read | not logged |
| `load-source` of an existing class | ephemeral by default (legacy browser internal op) | not logged unless `intent: "save"` parameter set |

`author_kind` (`human` / `agent`) is recorded on every entry as **audit metadata**, not as a filter. The pending change set (`Workspace changes`) includes *all* logged entries regardless of author — an agent that ran `Workspace newClass:at:` for ten new test files produces ten visible entries in `Workspace changes`, because those files are the deliverable.

**No new workspace-side REPL ops.** All operations described below are Beamtalk method calls submitted via the existing `evaluate` REPL op. MCP tools, LSP `executeCommand` handlers, REPL meta-commands, and browser actions are all *client-side* structured wrappers that construct the Beamtalk expression and submit it via `evaluate`. The workspace dispatcher does not learn new op names. See *Implementation* for the rationale.

**Logging principle: every in-memory method mutation produces a ChangeEntry.** Always. The audit trail is exhaustive — `Workspace changes` answers "what has the running workspace mutated relative to disk?" without gaps. Whether an entry is *flushable* and whether the caller *intended* it as durable are two orthogonal flags on the entry; neither controls whether-it-logs.

**Method patch flow (`>>` patcher form, underlying `Behaviour compile:source:`).** A successful patch does three things in sequence: (1) reads and parses `Counter sourceFile` (if non-nil) to capture the existing method's exact byte span and source body as `prev_source`; (2) installs the new method in memory; (3) appends a `ChangeEntry` to the workspace ChangeLog. The entry carries `intent: durable` and `flushable: true` (when `sourceFile` is in-project) or `flushable: false` (stdlib / dependency / dynamic class — see Cross-cutting decisions). All-or-nothing applies between steps 2 and 3 — if memory install succeeds, the ChangeEntry is emitted; if memory install fails, neither happens.

**New-class flow (`Workspace newClass: source at: path`).** `targetPath` is required and must lie inside the project source tree. The method (1) compiles and installs the class in memory, (2) writes a `ChangeEntry` with `kind: "new-class"`, `intent: durable`, `flushable: true`, `prev_source = nil`, `span = nil`, and the full class source. Subsequent `compile:source:` patches against this class log additional entries layered on top; at flush time, entries replay in order — the `new-class` entry writes the initial file, then later method patches splice into it. This avoids needing a class-to-source serialiser; we don't reconstruct from in-memory metadata. `targetPath` is rejected per the validation rules (see Cross-cutting decisions).

**Ephemeral patch flow (`Behaviour tryCompile:source:`).** Installs in memory exactly like `compile:source:` and **also logs a ChangeEntry** — but with `intent: ephemeral`. Agents use this for exploration: spike a candidate fix, run tests via `evaluate`, observe outcome, and either (a) discard by ignoring it (ephemeral entries auto-prune on flush of durable changes and on workspace restart — see Hygiene below) or (b) promote by calling `compile:source:` with the same source to upgrade the intent. The `tryCompile:` → `compile:` step is the agent's analogue of a human typing `>>` at the REPL: they tried it interactively, now they want to keep it. The audit trail still records every `tryCompile:` call — visibility into what the agent tried is part of the value of the ChangeLog, not noise to hide.

**Flushability**: a class is **flushable** iff `sourceFile` is non-nil **and** the source file lies inside the current project's source tree (per the active `beamtalk.toml`). For flushable classes, `intent: durable` entries are written to disk by `Workspace flush`. For non-flushable classes (stdlib, dependency, dynamic — `sourceFile = nil` or out-of-project), patches still install in memory and still log, but `flush` skips them with a status report. New classes created via `Workspace newClass:at:` are flushable by construction.

**`Workspace flush` selection rule:** writes only entries where `intent = durable AND flushable = true`. Other entries — ephemeral entries, non-flushable durable patches — are reported in the flush summary as "skipped: <reason>" (`ephemeral` or `not flushable (stdlib)` or `not flushable (dependency: <path>)`).

**Hygiene for non-deliverable entries.** Ephemeral and non-flushable entries don't accumulate forever:

| Trigger | What it prunes |
|---------|----------------|
| Workspace restart | All ephemeral entries (orphans by definition — memory state gone). Non-flushable durable entries get auto-tagged `orphan` (the patch can never be re-applied without re-running it). |
| `Workspace flush` succeeds | Ephemeral entries from the same session are pruned (configurable; default yes — they're noise after the commit). Non-flushable entries persist for audit. |
| `Workspace changes pruneEphemeral` | Manual sweep of `intent: ephemeral` entries. |
| `Workspace changes pruneOrphans` | Existing orphan cleanup; now also catches non-flushable entries tagged as orphan after restart. |
- The ChangeLog is the dirty state. A method is "dirty" iff there is an unflushed ChangeEntry whose target is that method, that class, or that file. Granularity is per-method; aggregate views (per-class, per-file) are derived.
- `Workspace flush` walks pending ChangeEntries, groups them by source file, computes a new file body per file by replacing each target method's recorded byte span with its patched source, and writes each via temp+rename. The splice operates on byte spans recorded at hook time — it does not depend on the formatter being able to round-trip the whole file. ChangeEntry pruning happens **per file as each Phase B rename succeeds**: a partial flush failure leaves a mixed state where successfully-renamed files have their entries pruned and reported as completed, while failed files retain their entries for retry. See *Multi-file atomicity* in Cross-cutting decisions for the full Phase A / Phase B protocol.
- **Multiple patches to the same method** (same session or across sessions) append multiple ChangeEntries. The ChangeLog is append-only — earlier entries are not mutated. On flush, only the **most recent** entry for each `(class, selector)` is applied to disk; older entries are *shadowed* and remain in the log for audit and `revert:` history.
- `Workspace flush: Counter` flushes only entries targeting that class. Similar for `flush: #{file: "..."}`.
- `Workspace changes clear` discards the ChangeLog without writing. Memory still holds the latest patched versions until the next workspace restart, when disk wins.
- An `autoflush` workspace setting (boolean, default `false`) inverts the default: live patches flush immediately. This is one switch, applied uniformly across all surfaces — surface parity preserved. On autoflush failure (external-edit conflict, write error), memory and disk diverge and the user is told so explicitly; we do not roll back the BEAM module install because the prior `.beam` binary may already be unloaded and live actors may hold references to the new closures. Autoflush is therefore best-effort consistency, not transactional consistency.

### Surface

**Principle (per ADR 0040): every MCP / REPL / LSP / browser tool op is a structured invocation of a Beamtalk-level expression. There are no tool-only operations — every op compiles to something a human could type at the REPL.** The tool surface is a convenience layer; the language is the API.

**Beamtalk language bindings (the methods every tool calls through to):**

| Where | Binding | Used by |
|-------|---------|---------|
| **`Behaviour` metaclass** | `Counter compile: #selector source: "body"` (new, underlying primitive) — durable, logged | `>>` parser desugars to this; MCP `save_method`, browser "Save", REPL editor save call it directly |
| **`Behaviour` metaclass** | `Counter tryCompile: #selector source: "body"` (new, underlying primitive) — ephemeral, no log | MCP `try_method` calls it directly |
| **`Behaviour` metaclass** | `Counter >> selector => body` (existing patcher form, ADR 0066) — parser sugar that desugars to `compile:source:` | Humans typing at the REPL |
| **`Behaviour` metaclass** | `Counter >> #selector` (existing reader form, ADR 0066) — pure read, returns CompiledMethod | tab-completion, inspector |
| **`Workspace`** | `Workspace newClass: source at: path` (new) — durable new-class creation, logged as `kind: "new-class"` | MCP `save_class`, browser "New File", REPL |
| **`Workspace`** | `Workspace flush`, `Workspace flush: aClass` | MCP `flush`, REPL `:flush`, LSP `executeCommand`, browser "Save All" |
| **`Workspace`** | `Workspace changes` — returns the ChangeLog object (gateway for all pending-state queries) | MCP `list_changes`, MCP `dirty`, REPL `:changes`, REPL `:dirty`, browser ChangeLog viewer, browser dirty indicator |
| **`ChangeLog`** (returned by `Workspace changes`) | `size`, `isEmpty`, `notEmpty`, `do:`, `select:`, `dirtyMethods`, `revert:`, `clear`, `flushKinds:` | "Is anything dirty?" is `Workspace changes notEmpty`; "what's dirty?" is `Workspace changes dirtyMethods`; MCP `dirty` ≡ `Workspace changes notEmpty`; MCP `dirty_methods` ≡ `Workspace changes dirtyMethods` |

The `compile:source:` / `tryCompile:source:` distinction matters for implementation: tools take the body as a **value** (a Beamtalk String passed through the eval pipeline), not as a substring concatenated into a `>>` expression. Building a `>>` source string and re-parsing would require escaping the body to be valid Beamtalk source — fragile, breaks on quote chars, multi-line bodies, etc. Calling `compile:source:` directly bypasses the string-roundtrip and passes the body value end-to-end.

**Tool surfaces (each row maps to one or more bindings above):**

| Surface | Op | Compiles to |
|---------|-----|-------------|
| REPL meta-command | `:flush`, `:flush <Class>` | `Workspace flush` / `Workspace flush: aClass` |
| REPL meta-command | `:changes` | `Workspace changes` |
| REPL meta-command | `:dirty` | `Workspace changes notEmpty` |
| MCP | `save_method` | `aClass compile: aSym source: body` |
| MCP | `save_class` | `Workspace newClass: source at: path` |
| MCP | `try_method` | `aClass tryCompile: aSym source: body` |
| MCP | `flush` | `Workspace flush` (or `Workspace flush: aClass`) |
| MCP | `list_changes` | `Workspace changes` (returns serialised log) |
| MCP | `dirty_methods` | `Workspace changes dirtyMethods` |
| LSP | `workspace/executeCommand: flush` | `Workspace flush` |
| LSP | `workspace/executeCommand: save_class` | `Workspace newClass: source at: path` |
| Browser | "Save" (per method) | `aClass compile: aSym source: body` |
| Browser | "New File" | `Workspace newClass: source at: path` |
| Browser | "Save All to Disk" | `Workspace flush` |

**Workspace facade vs ChangeLog object.** The Workspace facade follows Pharo's `Smalltalk changes` idiom and stays minimal: four methods total (`flush`, `flush:`, `changes`, `newClass:at:`). All pending-state queries — *is anything dirty?*, *what's dirty?*, *revert this one*, *clear them all* — live on the ChangeLog returned by `Workspace changes`, which carries the full collection protocol (`size`, `isEmpty`, `notEmpty`, `do:`, `select:`, `dirtyMethods`, `revert:`, `clear`, `flushKinds:`). The previously-proposed convenience method `Workspace dirty` was dropped in favour of `Workspace changes notEmpty` — composes from existing primitives, makes the model explicit (there's a changeset, you're querying it), no capability lost.

### REPL session (human, patching existing class)

```beamtalk
> Counter >> increment => self.value := self.value + 1
=> a CompiledMethod (#increment in Counter)        // memory patched
> Workspace changes
=> a ChangeLog with 1 entry
> Workspace changes notEmpty
=> true
> Workspace changes dirtyMethods
=> #{Counter -> #{#increment}}
> Workspace flush
=> flushed 1 method across 1 file
> Workspace changes isEmpty
=> true
```

### MCP agent session (spike, then commit new test + impl)

Each MCP tool call below is annotated with the Beamtalk expression it compiles to — the tool is a structured invocation, the language is the API.

```beamtalk
// 1. Agent explores via try_method — installs in memory and logs as ephemeral
mcp> try_method(class: "Counter", selector: "doubled", body: "^ self value * 2")
//   ≡ Counter tryCompile: #doubled source: "^ self value * 2"
=> ChangeEntry logged (#doubled in Counter, intent: ephemeral, flushable: true)
mcp> evaluate("(Counter new) doubled")
=> 0                                              // works, agent commits

// 2. Agent promotes the spike — same source, durable intent this time
mcp> save_method(class: "Counter", selector: "doubled", body: "^ self value * 2")
//   ≡ Counter compile: #doubled source: "^ self value * 2"
//   (a human typing `Counter >> doubled => self value * 2` reaches the same method via parser sugar)
=> ChangeEntry logged (#doubled in Counter, intent: durable, flushable: true)
//   The earlier ephemeral entry remains for audit; both shadow-resolve on flush

// 3. Agent creates a new test class for the feature
mcp> save_class(source: "<DoubleCounterTest source>", target: "test/double_counter_test.bt")
//   ≡ Workspace newClass: "<DoubleCounterTest source>" at: "test/double_counter_test.bt"
=> ChangeEntry logged (kind: new-class)

// 4. Agent creates the impl file for a follow-up class
mcp> save_class(source: "<DoubleCounter source>", target: "src/double_counter.bt")
//   ≡ Workspace newClass: "<DoubleCounter source>" at: "src/double_counter.bt"
=> ChangeEntry logged (kind: new-class)

// 5. Human reviews and flushes — same operations, no tool needed
> Workspace changes notEmpty
=> true
> Workspace changes dirtyMethods
=> #{Counter -> #{#doubled},
     DoubleCounterTest -> #new-class,
     DoubleCounter -> #new-class}
> Workspace changes do: [:e | Transcript show: e author_kind]
=> "agent" "agent" "agent"
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
           Workspace changes clear        // discard memory edits
           Workspace changes diff: counter.bt     // inspect conflict
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
| ChangeLog format | Two-part layout under a dedicated workspace subdir: short metadata lines in `<workspace>/changes/changes.jsonl`, source bodies stored as plain `.bt` files in `<workspace>/changes/sources/`. Each `changes.jsonl` entry is small (stays under ~300 chars regardless of method size): `{ts, seq, epoch, class, selector, kind: "instance"\|"class"\|"new-class", source_ref, prev_source_ref \| null, sourceFile \| null, span: {start, end} \| null, intent: "durable"\|"ephemeral", flushable: bool, not_flushable_reason: "stdlib"\|"dynamic"\|"dependency:<path>" \| null, author, author_kind: "human"\|"agent"}`. `source_ref` and `prev_source_ref` are filenames relative to `changes/sources/` (e.g. `"000142-source.bt"`, `"000142-prev.bt"`); `new-class` entries have `span: null` and `prev_source_ref: null`; non-flushable entries may have `sourceFile: null` (stdlib/dynamic) or a path outside the project tree (dependency). The source files themselves are plain Beamtalk source — `cat`, `less`, `bt fmt`, `diff`, syntax highlighting all work without escaping. The `author_kind` and `not_flushable_reason` enums are intentionally open. Survives workspace restart. The dedicated `changes/` subdir keeps the workspace root uncluttered and gives a single backup/exclude target. |
| ChangeLog growth | Bounded ring of last N=1000 entries by default. On rotation, both the metadata segment and the referenced source files are archived: `<workspace>/changes/archive/changes-<timestamp>.jsonl.gz` for metadata, `<workspace>/changes/archive/sources-<timestamp>.tar.gz` for the corresponding source files. `human` and `agent` entries are retained on equal footing — both represent durable intent and are pruned only by the ring bound. Source-file disk usage scales linearly with logged entries (two `.bt` files per logged method patch — source + prev_source; one for new-class entries). If usage becomes a concern, a future revision can switch `source_ref` from seq-numbered filenames to content-addressed hashes for dedup, without changing the on-disk shape. |
| Orphan entries on restart | The ChangeLog persists across workspace restart; the BEAM module state does not. On startup, the workspace assigns a new `epoch` and tags every pre-existing entry as belonging to a prior epoch. Entries whose `prev_source` no longer matches the current on-disk content are tagged `orphan` (the disk advanced via VSCode/git/another flush while the workspace was down). Both prior-epoch and orphan entries are **excluded from the active `Workspace changes` view by default** — their memory state was lost on restart and the patches are no longer installed, so `Workspace changes notEmpty` returns `false` for these alone. They remain in the underlying log for audit and inspection via `Workspace changes includingOrphans` (`select: [:e \| e isOrphan]`); a `Workspace changes pruneOrphans` operation discards them on demand. Auto-prune-on-startup is opt-in via a workspace setting. |
| File relocation / deletion at flush | External-edit detection catches *content* changes via `(mtime, content-hash)`. A *path* change (file moved or deleted between patch and flush) surfaces as a `flush` error with a distinct conflict kind: "source file relocated or deleted." The user chooses: `Workspace changes relocate: aClass to: newPath` to update the entries' `sourceFile`, `Workspace changes clear: aClass` to discard, or `Workspace diff: aClass` to inspect. The entry is not auto-rewritten — relocation requires explicit human confirmation because the new path may be the wrong one. |
| Intent vs author vs flushable | Three orthogonal flags on every ChangeEntry. **`intent`** (`durable` / `ephemeral`) is signalled by the *method called*: `compile:source:` and `newClass:at:` ⇒ durable, `tryCompile:source:` ⇒ ephemeral, `>>` parser form ⇒ durable (desugars to `compile:source:`). **`flushable`** (boolean) is derived from the class: true iff `sourceFile` is in-project. **`author_kind`** (`human`/`agent`) is audit metadata identifying the caller. `Workspace flush` writes `intent = durable AND flushable = true` entries only. `Workspace changes` shows everything; `Workspace changes select: [:e \| e isFlushable]` filters; `Workspace changes select: [:e \| e isDurable]` filters; the `flushKinds:` selector also accepts `author_kind` filters (e.g., `Workspace changes flushKinds: #{agent}` to commit an agent batch separately from human changes). |
| New-class flush | When flushing a `new-class` entry, the splice operation is "write `source` to `targetPath`" (no byte-span surgery; the file doesn't exist yet). External-edit detection still applies: if `targetPath` was created externally between the `newClass:at:` call and flush, the conflict surfaces with the same force/discard/diff choice. Subsequent `compile:source:` patches against a not-yet-flushed new class produce additional entries that replay in order at flush — the `new-class` entry writes first, then later method-patch entries splice into the just-written file. |
| Undo | `Workspace changes revert: aMethod` re-installs `prev_source` from the most recent ChangeEntry for that method and appends a new revert entry (revert is itself a patch, not log mutation). Revert is only possible for flushable classes — ephemeral memory-only patches against stdlib/dependencies are not recorded and therefore not revertible. |
| Release builds | No-op. Release nodes do not start a workspace; ChangeLog code is in `beamtalk_workspace`, not `beamtalk_runtime`. |
| Stdlib classes | `sourceFile => nil` ⇒ patches install in memory and **log a ChangeEntry with `flushable: false`** (reason: `"stdlib"`). `Workspace flush` skips them with a status line. Smalltalk-style live debugging of stdlib (e.g. `Integer compile: #double source: "^ self * 2"`) is supported; the patch is real in memory, recorded in the audit log, and operators can see the drift via `Workspace changes`. Reproducible-build guarantee preserved: flush will not write into the stdlib source tree. |
| Dynamic classes (ADR 0038, ClassBuilder) | `sourceFile => nil` ⇒ same shape as stdlib: install + log with `flushable: false` (reason: `"dynamic"`). |
| Package dependency classes | `sourceFile` outside the current project source tree ⇒ same shape: install + log with `flushable: false` (reason: `"dependency: <path>"`). Reproducible-build guarantee preserved: flush will not write into the dependency cache. |
| `Workspace newClass:` validation | The op raises if: (a) `targetPath` already exists on disk; (b) `targetPath` lies outside the project source tree; (c) `source` parses successfully *but* the declared class name does not match the basename of `targetPath` (one-class-per-file convention per ADR 0040); (d) a class with that name is already loaded in memory (use `compile:source:` against the existing class, or remove it first). All four are loud errors with specific messages — no silent fallback. |
| `tryCompile:source:` and restart | Ephemeral patches log a ChangeEntry (`intent: ephemeral`) so the audit trail is complete, but the patch itself does not survive workspace restart: on restart, memory wins from disk, and ephemeral entries are auto-pruned from the log (they're orphans by definition — the memory state they recorded is gone). Agents wanting to keep a successful spike call `compile:source:` (or `save_method` MCP tool) to upgrade the intent to durable. The log retains pruned-on-restart ephemerals in the rotated archive (`changes/archive/`) if longer-term audit is wanted. |
| Concurrent compile + flush | `Workspace flush` snapshots the set of pending ChangeEntries at flush start (end of Phase A). New `compile:source:` calls that arrive mid-flush append to the log normally and become pending for the next flush; they do not race with the in-progress flush operation. The ChangeLog gen_server serialises log appends and flush-start reads. |
| Extension methods (ADR 0066) | A class adding extension methods to a foreign class has its own `sourceFile`; the patch is logged against the extender's file, not the extended class's file. **Multi-extender ambiguity:** if two packages both extend `String >> shout`, the patch is logged against the file owning the *currently-resolved* extension method (whatever the MRO picked at dispatch time). Conflict resolution between competing extenders is ADR 0066's problem, not this ADR's — we faithfully patch whichever extender was active. |
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
| Edit method → in-session install | `Counter compile: #sel source: body` (or `>>` parser sugar) → memory install + ChangeEntry |
| `System commitTransaction` | `Workspace flush` |
| `System abortTransaction` | `Workspace changes clear` |
| `TransactionConflict` on commit | External-edit conflict at flush time |
| File-in from topaz / GBS | `Workspace newClass: source at: targetPath` |
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
- "Was this fix flushed or is it still in memory?" has a definitive answer (`Workspace changes notEmpty`, or inspect `Workspace changes` for the per-method breakdown).
- Workspace restart loses unflushed patches — this is *desirable* in production: emergency in-memory fixes do not silently become permanent.

### Tooling developer (LSP/IDE)

- LSP gains a write path via `workspace/executeCommand` for flush and `workspace/applyEdit` *from* the runtime.
- Surface-parity table grows by one operation set; the drift-check binary will catch missing bindings.
- The trivia-preserving printer is a reusable asset for refactorings (rename, extract, inline) — flush is the first user but not the last.

## Steelman Analysis

### Alternative A — Memory-only (status quo + "Export Changes")

- 🧑‍💻 **Newcomer:** "There is zero risk of an IDE save mangling my git tree. I can copy-paste the export when I'm ready."
- 🎩 **Smalltalk purist:** "Pharo's `.changes` exists because pure memory *is* insufficient — but it's an internal mechanism, not a user-facing flush. Adopting Pharo's *log* without Pharo's *image* is half a model. Either go all-in on persistent runtime state (an image) or stay honest that the runtime is ephemeral and source files are the only durable artifact. The hybrid is the worst of both worlds."
- ⚙️ **BEAM veteran:** "This is the only option that preserves the existing memory-only invariant *literally*. Anything else is a new contract."
- 🏭 **Operator:** "Best for production triage — patches cannot leak to disk by accident."
- 🎨 **Language designer:** "Smallest surface. The export step is human and intentional, like `:show-codegen` is."
- **Why rejected:** users *will* lose work on workspace restart with no warning; the IDE story still ends in copy-paste; no audit trail for what was changed in memory. The Smalltalk-purist's "half a model" critique is real, but ADR 0004 already chose against the image — option A makes that choice user-visible in a way that maximises pain.

### Alternative B — Write-through (every patch writes immediately)

- 🧑‍💻 **Newcomer:** "Ctrl-S writes the file like every other editor. The model is one model."
- 🎩 **Smalltalk purist:** "Live editing means edits propagate everywhere — memory, disk, IDE views — simultaneously. That's Morphic's deepest promise. A two-step save reintroduces the compile-deploy cycle Smalltalk was invented to abolish; flush is a build step in disguise."
- ⚙️ **BEAM veteran:** "If we are going to write at all, write transactionally. One model is easier to reason about than two."
- 🏭 **Operator:** "Every change is in git history (after the user commits). Audit trail is the git log."
- 🎨 **Language designer:** "Zero impedance between language and tooling. There is no 'pending state' to reason about because pending state cannot exist; the system is either consistent or in an explicit conflict. The two-step model adds a synchronisation question — *is my memory ahead of disk or behind?* — which is a category of bug that doesn't exist with write-through."
- **Why rejected:** every transient `>>` from a REPL one-liner or an MCP agent mutates the user's git tree; collision with VSCode unsaved buffers and `git pull` is constant; rollback on compile failure means rolling back disk too, which is fiddly; violates the safety property operators rely on. The Smalltalk-purist's "flush is a build step in disguise" critique is fair — Option C *does* reintroduce a hint of compile-deploy. We accept that cost because the alternative (silent file mutation from a REPL one-liner) is worse.

### Alternative D — REPL memory-only, browser-editor write-through

- 🧑‍💻 **Newcomer:** "Each surface behaves like its native idiom — the REPL is a REPL, the editor is an editor."
- 🎩 **Smalltalk purist:** "REPL and Browser are different *activities*, not different views of the same thing. Pharo doesn't save when you Do-It in the Workspace, but it does save when you Save-As in the System Browser. The semantics follow the user's intent, which is signalled by which tool they reached for."
- ⚙️ **BEAM veteran:** "REPL stays clean for production work."
- 🏭 **Operator:** "I never accidentally write code from a REPL session."
- 🎨 **Language designer:** "Two-surface honesty: don't pretend they're the same. Each surface has its own contract; trying to unify them produces compromises that satisfy nobody."
- **Why rejected:** violates the surface-parity contract in CLAUDE.md (`docs/development/surface-parity.md` line 7: equivalent effects across surfaces unless explicitly `surface-specific`). The same patch from MCP and from the browser would produce different on-disk outcomes — exactly the drift surface-parity exists to prevent. The drift-check binary would have to whitelist this, which we have collectively decided not to do. The Smalltalk-purist's "different activities, different semantics" critique is genuine — we counter it via the `try_method` / `save_method` split, which encodes the *activity* (explore vs commit) at the op level rather than at the surface level.

### Alternative F — Shadow-file overlay (Monticello-style)

- 🧑‍💻 **Newcomer:** "I can see exactly what's pending in a separate file. No splice machinery means no risk of mangling my real source."
- 🎩 **Smalltalk purist:** "This is literally how Monticello (Pharo's package system) handles deltas — overlay files that compose with originals. Decades of production use. Why reinvent it?"
- ⚙️ **BEAM veteran:** "Loader complexity is small (read original.bt + original.bt.patch, merge); no splice machinery needed; no byte-span resolver risk in Phase 0."
- 🏭 **Operator:** "Pending patches are visible as files on disk — operationally legible, greppable, diff-able."
- 🎨 **Language designer:** "Separates concerns: the *original* file is the user's source-of-truth; the *patch* file is the workspace's pending state. Two artifacts, two responsibilities. Cleaner than splicing into a shared file."
- **Why rejected:** the decisive reason is **editor/runtime schism in a multi-surface world.** In F, VSCode opens `counter.bt` from disk and sees the *base* method body; the runtime dispatches the *overlay* method body. LSP hover, go-to-definition, `bt fmt`, and stack traces each have to choose which source is canonical, and every choice is wrong for the other view. ADR 0024 (static-first, live-augmented) explicitly designed for one runtime + one filesystem (drift between two views of one truth, mediated by LSP→runtime queries); F structurally splits the source into *two files per method*, and the static/live drift becomes a fragmentation problem no single LSP query can fix. C's drift is between memory and disk — one node, one filesystem; F's drift is between editor and runtime — fundamentally harder. We acknowledge F's genuine wins (**restart survival** — overlays on disk re-apply automatically, where C's ChangeLog is orphaned after restart; **crash-safety** — file I/O is more robust than gen_server-managed in-memory state). (F's "operational legibility" advantage is mitigated by C's two-part format: source bodies are plain `.bt` files in `changes/sources/`, also greppable and `cat`-able.) These wins are decisive *if* the editor and runtime are unified (Pharo's image model); they are dominated by the schism cost *in our multi-surface architecture* (VSCode + LSP + LiveView + MCP + REPL). If Beamtalk ever ships a monolithic IDE that owns both editor and runtime, F is the right answer and this decision should be revisited.

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

### Alternative F — Shadow-file overlay (Monticello-style)
See steelman above. Pharo's Monticello uses overlay files for package deltas with decades of production use, and F has genuine wins over C: **overlays survive workspace restart automatically** (the loader re-applies them, no orphaned ChangeLog); **crash-safety is better** (plain file I/O vs in-memory gen_server state); **pending state is greppable / cat-able** (operational legibility). Rejected because in our multi-surface architecture (VSCode + LSP + LiveView + MCP + REPL) the overlay creates two-source-files-per-method, fracturing every external tool's view of "where does this method live?" — a schism ADR 0024's static-first/live-augmented model cannot mediate the way it mediates C's memory/disk drift. Right model for a monolithic IDE (Pharo's image); wrong model when the editor and runtime are separate processes. **Revisit if Beamtalk ever ships a unified IDE that owns both.**

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
- Two MCP tools (`try_method` ephemeral, `save_method` durable) means agents must choose the right one — but the cost of choosing wrong is now small: both log, so the audit trail is intact either way. The difference is what `Workspace flush` will write (durable + flushable only) and what auto-prunes on restart (ephemeral entries). Documented in MCP tool descriptions; the typical agent flow is "try → evaluate → save."
- All in-memory mutations log unconditionally, including ephemeral spikes and patches against non-flushable classes (stdlib, dependencies, dynamic classes). Disk usage in `changes/sources/` grows faster on long agent sessions; mitigated by aggressive ephemeral pruning on flush and on restart, and by the rotated-archive scheme.
- Multi-client coordination is last-writer-wins; concurrent edits to the same method by two users will lose one — observable but not prevented.
- Autoflush is best-effort consistency, not transactional. On flush failure under autoflush, memory and disk diverge and require manual reconciliation. This is documented behaviour, not a bug to fix — the alternative (rolling back the BEAM module install) is unsound when live actors hold references to the new closures.
- Multi-file flush failure leaves a mixed state across files even after the two-phase protocol — Phase B renames are sequential, and a hard I/O error mid-sequence means some files renamed and some did not. The user gets a per-file status report and can re-flush; entries for already-renamed files are pruned, entries for failed files remain.

### Neutral

- ChangeLog persistence introduces a new on-disk subdirectory `<workspace>/changes/` (short metadata in `changes.jsonl`, source bodies as plain `.bt` files in `sources/`, rotated history in `archive/`). Single dir to back up, exclude, or wipe. Backup story is "it lives under the workspace; back it up with the workspace."
- `autoflush: true` collapses the model to Alternative B at the per-workspace level. Users who want write-through can have it; the default does not.
- Stdlib and dynamic classes (no `sourceFile`) silently accept patches as memory-only. This matches today's behaviour for `>>` against `Integer`, but the ChangeLog will not contain entries for them — they are not flushable by definition.
- Package dependency classes silently accept patches as memory-only for the same reason — their `sourceFile` is outside the project tree. This is a feature, not a limitation: reproducible builds depend on dependency caches being treated as read-only.
- MCP agents finalising new tests or impl files via `save_class` / `save_method` produce visible entries in `Workspace changes` — the deliverable is supposed to be visible. Agent spikes via `try_method` also produce visible entries, tagged `intent: ephemeral` — this is intentional: visibility into what the agent tried (and discarded) is part of the audit value, not noise. The distinction is what gets flushed and what auto-prunes, not what gets recorded.
- Stdlib live-patching (Smalltalk-style: `Integer compile: #double source: "^ self * 2"`) is supported as a durable in-memory patch that is logged with `flushable: false`. Operators can see the drift via `Workspace changes`; flush skips it; restart wipes it. This restores a piece of Smalltalk muscle memory that the prior "raise an error on stdlib" rule removed.

### DDD Model Impact

- **Compilation context** owns the byte-span resolver — given a source string and a target `(class, selector)`, return the exact byte span of the method definition. Reuses the existing parser; no new printer required.
- **Workspace context** owns the ChangeLog gen_server, the flush op, and the `Workspace` facade extensions. New module: `beamtalk_workspace_changelog.erl` (not REPL-scoped — the ChangeLog is consumed cross-surface by REPL, MCP, LSP, and browser, so DDD-correct placement is the workspace, not the REPL).
- **REPL context** is unchanged — no new workspace-side ops are registered. All operations are Beamtalk expressions submitted via the existing `evaluate` REPL op. The agent/CLI/LSP/browser layers each construct the appropriate expression client-side.
- **No language-service changes** — the LSP server consumes `workspace/applyEdit` notifications from the runtime, which is a one-way bridge already supported by the protocol.

## Implementation

### Affected components

| Layer | Change |
|-------|--------|
| `crates/beamtalk-core/src/source_analysis/` | New byte-span resolver: given source text and `(class, selector, kind)`, return the byte span of that method's definition. Pure parser-level work; no new printer. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_changelog.erl` (new) | Gen_server owning the append-only ChangeLog. ETS for live state; on disk, JSON-Lines metadata in `changes/changes.jsonl` plus per-entry source files in `changes/sources/`. Exposed via the `Workspace` facade per ADR 0040. Lives in the workspace context, not REPL, because it's consumed cross-surface. |
| `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` | The `>>` patch install chokepoint (already exists, 259 LOC). Hook the install path to (1) read+parse `sourceFile` to capture span and `prev_source`, (2) install in memory, (3) emit ChangeEntry. Flushability check (project-tree containment) gates the emit. |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_load.erl` | **No changes — no new workspace-side ops.** All operations are reached via the existing `evaluate` op, which receives a Beamtalk expression constructed by the calling layer (MCP / LSP / REPL CLI / browser). See *Rationale: why no new REPL ops* below. |
| `stdlib/src/Workspace.bt` | New facade methods: `flush`, `flush:`, `changes` (returns ChangeLog), `newClass:at:`. Four methods total — pending-state queries live on the ChangeLog object (`changes notEmpty`, `changes dirtyMethods`, etc.), matching Pharo's `Smalltalk changes` idiom. |
| `stdlib/src/Behaviour.bt` | Two new class-side methods: `compile: aSym source: aString` (durable, logs) and `tryCompile: aSym source: aString` (ephemeral, no log). `compile:source:` is the underlying primitive that the existing `>>` patcher form desugars to (ADR 0066 parser rule updated). Both share the same compile-and-install path; only `compile:source:` emits a ChangeEntry. MCP tools call these directly with body values, avoiding fragile string-construction of `>>` expressions. |
| `stdlib/src/ChangeLog.bt` (new) | The navigable ChangeLog object: `size`, `isEmpty`, `do:`, `select:`, `dirtyMethods`, `revert:`, `clear`, `flushKinds:`. Backed by `beamtalk_workspace_changelog.erl` via FFI. |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | New meta-commands: `:flush`, `:flush <Class>`, `:changes`, `:dirty`. Each is a CLI-side shortcut that constructs the equivalent Beamtalk expression (e.g. `:flush` → `Workspace flush`) and submits via the existing `evaluate` op — no new workspace-side dispatch. |
| `crates/beamtalk-mcp/src/server.rs` | New tools: `save_method`, `save_class`, `try_method`, `flush`, `list_changes`, `dirty_methods`. Each tool implementation takes typed args, constructs the corresponding Beamtalk expression (see Surface table), and submits it via the existing `evaluate` pathway — there is no workspace-side op to dispatch. The MCP layer is purely a typed front for the language. MCP-issued logged patches are auto-tagged `author_kind: agent` (passed as metadata on the eval submission). |
| `crates/beamtalk-lsp/src/server.rs` | Handle `workspace/executeCommand` for `flush` and `save_class` by constructing the Beamtalk expression and submitting via `evaluate`. Emit `workspace/applyEdit` *to* clients on flush events received from the runtime. |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | Per-method dirty indicator, "Save" per method, "Save All to Disk" workspace-level. |
| `docs/development/surface-parity.md` | No new REPL-op rows. The MCP tools and LSP commands listed in the Surface table compile to `evaluate` of a known Beamtalk expression; the drift checker should treat these as parity-compliant by virtue of the expression being the contract. May need a small drift-checker update to recognise the "MCP tool ≡ Beamtalk expression" pattern. |
| `docs/beamtalk-language-features.md` | Document `Workspace flush` semantics. |

### Phased rollout

| Phase | Scope | Effort | Tests |
|-------|-------|--------|-------|
| **0** | **Validation spike** (internal scaffolding, not user-facing). Implement the byte-span resolver and prove it against the entire stdlib + examples corpus: parse, locate every method's span, re-serialise file with a no-op span replacement, and assert byte-identical output. This is the load-bearing assumption of the design — validate it before building anything else. | S | Corpus round-trip tests in `crates/beamtalk-core/src/source_analysis/`. |
| **1** | ChangeLog gen_server + two-part on-disk persistence (`changes/changes.jsonl` for metadata + `changes/sources/` for source bodies) + `Workspace changes` (returns ChangeLog object) + ChangeLog collection protocol (`isEmpty`, `notEmpty`, `size`, `do:`, `select:`, `dirtyMethods`). Hooks into `beamtalk_extensions.erl`. No flush yet. `author_kind` plumbing through REPL/MCP. | M | EUnit tests for the gen_server (including crash-safety: write metadata + source files atomically); BUnit tests for `Workspace changes` and the ChangeLog collection methods. |
| **2** | `Workspace flush` + `flush:` + single-file atomic temp+rename + multi-file two-phase (Phase A all writes, Phase B all renames) + external-edit detection. Pruning rules implemented. | M | EUnit tests for atomicity (kill the process between phases); BUnit tests for the facade. |
| **3** | MCP tools (`save_method`, `save_class`, `try_method`, `flush`, `list_changes`, `dirty_methods`) implemented as expression-building wrappers over the existing `evaluate` op; LSP `executeCommand` handlers (`flush`, `save_class`) same pattern; REPL meta-commands (`:flush`, `:changes`, `:dirty`) construct expressions CLI-side; browser "Save" / "New File" / "Save All" actions same pattern. **No workspace-side REPL ops added.** Surface-parity table updated to recognise expression-backed tools as parity-compliant. | M | MCP integration tests for try→save promotion; browser e2e for Save and New File; LSP command tests; surface-parity drift check passes. |
| **4** | ChangeLog object operations (`revert:`, `clear`, `flushKinds:`, `do:`, `select:`) + `autoflush` workspace setting. ChangeLog browsing UI in the browser workspace. | S | BUnit tests for revert and ChangeLog navigation; e2e for autoflush. |
| **5** | LSP-side `workspace/applyEdit` consumption in VSCode + e2e test that flush refreshes an open buffer. | S | VSCode extension e2e. |

Total: ~M-L across 6 phases. Phase 0 is scaffolding — its deliverable is *evidence that byte-span splice works on real code*, not a shippable feature. If Phase 0 reveals that the parser cannot reliably resolve method spans against arbitrary `.bt` files, the design pivots before phases 1–5 commit to it.

### Rationale: why no new REPL ops

A previous draft of this ADR registered six new workspace-side REPL ops (`save-method`, `save-class`, `try-method`, `flush`, `list-changes`, `dirty`), each implemented as a thin handler that constructed the equivalent Beamtalk expression and submitted it through the workspace evaluator. That layer was redundant.

The existing `evaluate` REPL op is the universal mechanism: it receives a Beamtalk expression string and returns the result. Every operation this ADR describes is expressible as a Beamtalk expression (per the *Beamtalk language bindings* table in Surface). So the workspace dispatcher does not need to learn new op names — it already knows `evaluate`, which is enough.

The reasons to register *new* ops on the workspace side are narrow:

| Reason | Applies here? |
|--------|---------------|
| Transport mechanism that can't be expressed in-language (e.g. `interrupt`, `complete`, session lifecycle) | No — every operation here is in-language |
| Out-of-band signal (`interrupt` while eval is blocking) | No |
| OS-level concern (`load-project` reads beamtalk.toml from disk before any class is loaded) | No |
| Discoverability for agents | Provided by the MCP tool schema layer, not by the workspace op registry — agents discover via the MCP schema, not by introspecting workspace ops |
| Discoverability for editors | Provided by the LSP `executeCommand` registry, not the workspace |
| Structured returns | Eventually fixable in `evaluate` itself (a `toJson` selector on values, or an eval mode that returns structured data) — out of scope for this ADR but tracked for a future improvement |
| Telemetry / audit (`author_kind`) | Passed as metadata on the eval submission, not as a separate op |

Pharo and GemStone follow the same architecture: there is no "image RPC API" beyond message-send and reflection. The IDE tools (System Browser, Inspector, Test Runner, Monticello) are Smalltalk code that compose message-sends. We mirror that: MCP tools, LSP commands, REPL meta-commands, and browser actions are all *tooling layers* that compose Beamtalk expressions and submit via `evaluate`. The workspace is the runtime; the tools are the IDE.

**Out of scope for this ADR but related:** the existing workspace has ~17 redundant REPL ops (`actors`, `methods`, `list-classes`, `inspect`, `test`, etc.) that predate the `Workspace` / `Beamtalk` facade (ADR 0040) and could likewise be collapsed into `evaluate` of a known expression. That cleanup is opportunistic — touched when those handlers next break or are modified. A future "REPL op consolidation" epic may sweep them out.

## Out of Scope

This ADR covers **patch** (existing method) and **create** (new class file). The following are deliberately deferred to follow-up ADRs so that the persistence model can ship without being held up by destructive-op design:

| Deferred concern | Why deferred | Future ADR |
|------------------|--------------|-----------|
| **Method-level removal** (`aClass removeSelector:`) | The language primitive does not exist yet. Adding it is a separate design question (raise vs no-op on absent selector? cascade to overrides? extension-method handling?) — not bundled with persistence. The runtime can erase a method's `method_signatures` entry (`beamtalk_object_class.erl:640`) but there is no first-class Beamtalk method that calls it. | "Method-level Removal Language Primitive" |
| **Class-level removal flush UX** | `aClass removeFromSystem` already exists (BT-785) for memory removal. What it should mean to *flush* a class removal — deleting a `.bt` file from disk — is irreversibly destructive and wants its own UX: confirmation prompt, `.bt.deleted` tombstone, undo flow. Different concerns than patch/create. | "Destructive Workspace Operations" |
| **Renames** (class rename, method rename, file relocation) | Touches two paths (the old and the new), needs cross-file rename detection in the splice machinery, and benefits from concrete usage data from the patch-and-create case before its UX is locked in. | "Destructive Workspace Operations" |
| **Schema accommodation** | The ChangeLog format reserves the `kind` enum as open (`"instance"`, `"class"`, `"new-class"` today; `"remove-method"`, `"remove-class"`, `"rename"` will slot in later) and `author_kind` as open. Future ADRs extend the enum without breaking the format. No prep-work needed in this ADR's implementation phases. | n/a |

**Implementation order:** ADR 0082 phases 0–3 land first → method-removal language primitive ADR lands in parallel → destructive workspace ops ADR is written *after* phases 1–2 ship and produce real usage signal (i.e., the UX questions are answered by what users actually try to do, not by speculation now).

## Migration Path

No user code changes required. Existing `>>` patches continue to work identically — they now additionally append to the ChangeLog (silently, until the user looks).

For users who today rely on workspace-restart wiping memory patches (intentional ephemerality): behaviour is preserved. The ChangeLog persists across restart but memory does not; on restart, disk wins, and the ChangeLog contents become orphaned entries (patches whose "memory state" is no longer installed). Per *Orphan entries on restart* in Cross-cutting decisions, the workspace assigns a fresh `epoch` on startup and excludes prior-epoch entries from the active `Workspace changes` view automatically — the user does not need to manually clear unless they want the entries pruned from the audit log.

For ADR 0046 (VSCode sidebar): no migration. The sidebar gains a "pending changes" indicator (computed from `Workspace changes notEmpty`) and a "Flush" command surface as a phase-3 deliverable.

## References

- Implementation epic: **BT-2280** (parent), phases tracked under:
  - BT-2281 — Phase 0: byte-span resolver + corpus round-trip validation spike
  - BT-2282 — Phase 1: ChangeLog gen_server + two-part on-disk persistence
  - BT-2283 — Phase 1: `Behaviour compile:source:` / `tryCompile:source:` + install-hook ChangeEntry
  - BT-2284 — Phase 1: `Workspace changes` + `ChangeLog.bt` collection protocol
  - BT-2285 — Phase 1: `Workspace newClass:at:` — new-class creation + validation
  - BT-2286 — Phase 2: `Workspace flush` — splice, atomicity, external-edit detection
  - BT-2287 — Phase 3: REPL meta-commands `:flush` / `:changes` / `:dirty`
  - BT-2288 — Phase 3: MCP tools `save_method` / `save_class` / `try_method` / `flush` / `list_changes` / `dirty_methods`
  - BT-2289 — Phase 3: LSP `executeCommand` (`beamtalk.flush*`, `beamtalk.saveClass`) + `workspace/applyEdit` on flush
  - BT-2290 — Phase 4: `ChangeLog revert:` / `clear` / `flushKinds:` + `autoflush`
  - BT-2291 — Phase 5: e2e full flush / external-edit conflict round-trip btscript
  - BT-2292 — Phase 5: docs + surface-parity audit
- Related ADRs:
  - ADR 0004 — Persistent Workspace Management (memory-only hot reload contract)
  - ADR 0017 — Browser Connectivity to Running Workspaces (Phase 3 LiveView IDE that motivates this)
  - ADR 0024 — Static-First, Live-Augmented IDE Tooling (LSP/runtime coherence rules)
  - ADR 0033 — Runtime-Embedded Documentation (source-location tracking precedent)
  - ADR 0040 — Workspace-Native REPL Commands (class-based reload; the read-direction counterpart)
  - ADR 0044 — Comments as First-Class AST Nodes (trivia model used by splice printer)
  - ADR 0046 — VSCode Live Workspace Sidebar (consumer of `workspace/applyEdit`)
  - ADR 0066 — Open Class Extension Methods (`>>` syntax, the patch operator)
  - ADR 0085 — Editor Live-Image Representation (the read-surface counterpart: renders the in-memory source as the editor buffer and routes saves to this ADR's `compile:source:` / `flush`; the buffer-vs-saved delta *is* this ADR's ChangeLog)
- Documentation:
  - `docs/beamtalk-principles.md` — principles #4, #5, #11, #12
  - `docs/development/surface-parity.md` — drift contract this ADR must satisfy
  - Pharo `.changes` file model: <https://books.pharo.org/booklet-PharoToolingHandbook/pdf/2017-02-PharoToolingHandbook.pdf>
  - LSP `workspace/applyEdit`: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_applyEdit>

## Amendment 1 — Cockpit Positioning: live-first (agents) / git-first (humans)

**Status: Proposed (2026-06-19) — pending decision, tracked in BT-2585.** This
section does not change the accepted decision above; it records a positioning
question that surfaced while building the LiveView IDE ChangeLog UX (BT-2573:
collapse duplicate reverts; net-vs-disk diff / "disappear when clean"; BT-2577:
flush corruption) and proposes an answer for sign-off.

### Problem

The accepted design makes the running image the source of truth between edits,
with the ChangeLog as the dirty-state tracker and undo store and `flush` as the
reconciliation step. Building UX on top of it revealed that **most ChangeLog
features re-implement what files + git already provide for free**:

| ChangeLog feature | Git equivalent |
|-------------------|----------------|
| `Workspace changes` (dirty set) | `git status` |
| `ChangeEntry diff` (net vs disk) | `git diff` |
| `ChangeLog revert:` | `git checkout -- <file>` |
| "disappear when clean" (BT-2575) | git's own unmodified notion |

For a **Smalltalk live-programming** tool this is correct and the divergence
from a file-based editor (VS Code) is the point — you are editing a *running
image*, not files, so the image needs its own dirty/undo/diff model. For a
**"nicer editor for Beamtalk"** audience it is a large novel surface that fights
file+git muscle memory. The risk is building Smalltalk-path features while
reasoning with VS-Code-path instincts — satisfying neither.

### Options

- **(1) Live-programming-first (one model for everyone).** ChangeLog, flush,
  diff, revert are core; autoflush stays off by default; the VS Code gap is
  intentional. Maximises the live story; maximises newcomer friction.
- **(2) Editor-first (one model for everyone).** Lean on files + git: autoflush
  on by default (save means save), surface real `git status`/`git diff` instead
  of a bespoke ChangeLog, and make live-no-flush editing the *special* mode.
  Minimises novelty; discards much of the ChangeLog UX investment.
- **(3) Split by actor (recommended).** **Live-first for agents**,
  **git-first for humans.** Agents (MCP/LiveView automation) benefit most from
  the structured ChangeLog audit trail and machine-readable diff — keep the full
  model for them. Humans default to `autoflush: true` and lean on git for diff /
  revert / history; the ChangeLog remains available but is not the primary
  human-facing surface. This is coherent (each actor gets the model that fits)
  rather than straining one model across both, and it preserves the
  agent-native thesis without imposing image semantics on file-oriented humans.

### Proposed consequences (if option 3 is accepted)

- `autoflush` default becomes actor-dependent: `true` for human sessions,
  `false` for agent sessions (a per-session default, not a global switch — note
  this interacts with the "one switch, applied uniformly across all surfaces"
  statement under *Behaviour* above, which would need to be relaxed to
  per-session).
- BT-2575 (net-vs-disk diff / disappear-when-clean) ships as an
  **agent-facing** capability (and the LiveView ChangeLog viewer for power
  users), not as core human workflow. It is currently **held** pending this
  decision.
- BT-2293 (ChangeLog viewer / per-method Save / Save All) is re-scoped against
  the chosen positioning.
- Surface parity is preserved: the *operations* remain identical across
  surfaces; only the *default* (autoflush) and the *primary human affordance*
  (git vs ChangeLog) differ by actor — analogous to existing surface-specific
  presentation notes.

### Non-goals

This amendment does not revisit ADR 0004 (memory-only hot reload) or the
byte-span splice mechanism. It is purely about which model is the *default,
primary* surface for which *actor*.
