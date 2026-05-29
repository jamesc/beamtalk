# Surface Parity Map

This document tracks which REPL operations are exposed across each Beamtalk surface (CLI, REPL meta-commands, MCP tools, LSP capabilities). It is the single source of truth for identifying gaps and preventing surface drift.

## Parity Contract

Any operation **not** labelled `surface-specific` must produce equivalent output across all surfaces where it appears. "Equivalent" means the same structured data (modulo transport encoding); display formatting may vary per surface.

When adding a new capability to any surface, update this table. If the capability maps to an existing REPL op, add it to the corresponding row. If it is surface-specific, add the reason.

### REPL surface — meta-commands vs. message-sends

The REPL exposes capability through two surfaces:

1. **Message-sends** (`Workspace …`, `Beamtalk …`, or any object) — the primary surface. Object-level capability is reached this way, not through meta-commands.
2. **Meta-commands** (`:cmd`) — reserved for **client-side** concerns (`:exit`, `:help`) or **transport-out-of-band** ops that cannot be expressed as a normal eval (e.g. `:interrupt`, since you cannot send an eval while one is blocking the session).

Historically meta-commands like `:bindings`, `:sync`, `:test` existed to bootstrap the REPL before the object model was rich enough. Now that `Workspace` and `Beamtalk` express most capability, the REPL column in this map cites the message-send (`via Workspace classes`, `via Beamtalk help:`) rather than treating absent meta-commands as gaps.

## Legend

| Symbol | Meaning |
|--------|---------|
| **name** | Binding exists (tool/command/handler name shown) |
| `via X` | Reachable as a message-send on `X` (e.g. `via Workspace classes`) — counts as parity for the REPL surface |
| `--` | Not applicable for this surface |
| `surface-specific: reason` | Intentionally present only on this surface |
| `MISSING` | Should exist but does not yet |

## Inventory Sources

| Surface | Source of truth |
|---------|---------------|
| REPL ops | `describe_ops()` in `beamtalk_repl_ops_dev.erl` + `beamtalk_repl_ops_perf.erl` |
| CLI subcommands | `Command` enum in `crates/beamtalk-cli/src/main.rs` |
| REPL meta-commands | `handle_repl_command()` in `crates/beamtalk-cli/src/commands/repl/mod.rs` |
| MCP tools | `#[tool(...)]` in `crates/beamtalk-mcp/src/server.rs` |
| LSP capabilities | `ServerCapabilities` in `crates/beamtalk-lsp/src/server.rs` |

## Core Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `eval` | -- | *(implicit: any expression)* | `evaluate` | -- | Core evaluation; CLI uses REPL client, LSP has no eval |
| `stdin` | -- | *(implicit: interactive input)* | -- | -- | Provides input to a blocked eval; CLI handles interactively |
| `complete` | -- | *(implicit: tab completion)* | `complete` | `completion` | Autocompletion suggestions |
| `show-codegen` | -- | `:show-codegen` / `:sc` | `show_codegen` | -- | Show generated Core Erlang |
| `load-source` | -- | `surface-specific: browser workspace internal` | -- | -- | Load inline source string |
| `load-project` | -- | `:sync` / `:s` | `load_project` | -- | Sync project files from `beamtalk.toml` |

## Session Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `clear` | -- | `:clear` | `clear` | -- | Clear session locals; meta-cmd retained — session-locals layer has no object-side accessor (BT-2092 considers a first-class session object) |
| `bindings` | -- | `:bindings` / `:b` | `get_bindings` | -- | View session locals merged with workspace globals; meta-cmd retained for the same reason as `clear` |
| `sessions` | -- | `surface-specific: transport handshake` | -- | -- | List active REPL sessions |
| `clone` | -- | `surface-specific: transport handshake` | -- | -- | Create a new session |
| `close` | -- | `:exit` / `:quit` / `:q` | -- | -- | Close session; `:exit` exits the CLI REPL |
| `interrupt` | -- | `:interrupt` / `:int` | `interrupt` | -- | Cancel a running evaluation; out-of-band by definition (BT-2090) |

## Actor Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `actors` | -- | `via Workspace actors` | `list_actors` | -- | List running actors |
| `inspect` | -- | `surface-specific: agent-only typed introspection` | `inspect` | -- | Inspect actor state. Locked: structured-JSON view is for agents; humans use `Transcript show: actorRef` or send messages directly |
| `kill` | -- | `via anActor stop` | -- | -- | Terminate an actor; MCP can `evaluate` the same send |

## Module Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `unload` | -- | `:unload <class>` | `unload` | -- | Unload a class from the workspace |
| `define-class` | -- | *(via `eval`: `Object subclass: …` **or** `Object classBuilder … register`)* | *(via `evaluate`)* | -- | Define/register a class. Both the grammar form and the programmatic `ClassBuilder` cascade (ADR 0038 / ADR 0084) flow through the shared `eval`/`evaluate` surface, so any surface that can evaluate an expression can create a class — no dedicated op. `register` returns the canonical class object (BT-2258). |
| `live-edit-method` | -- | *(via `eval`: `Class >> sel => body` / `Class class >> sel => body`, or `Class compile: #sel source: body`)* | `save_method` (durable; ephemeral counterpart `try_method` is listed in **MCP-Only Tools**) | -- | Live-patch an instance- or class-side method on a registered class (ADR 0066 / ADR 0082 Phase 1 / ADR 0084 / Phase 3 BT-2288). The `>>` patcher form desugars to `compile:source:` (durable); `tryCompile:source:` is the ephemeral variant; both take the body as a String value. Every successful in-memory patch emits a ChangeLog entry. MCP `save_method` ≡ `aClass compile: #sel source: body` (durable); MCP `try_method` ≡ `aClass tryCompile: #sel source: body` (ephemeral). Both are typed front-ends that build the Beamtalk expression and submit via `evaluate` — no dedicated workspace-side op. The class-side path recompiles the class's recorded source. |
| `new-class` | -- | *(via `eval`: `Workspace newClass: source at: path`)* | `save_class` | `executeCommand: beamtalk.saveClass` (BT-2289) | Create a brand-new class from a source String at a target path (ADR 0082 Phase 1, MCP wiring BT-2288, LSP wiring BT-2289). MCP `save_class` ≡ `Workspace newClass: source at: path`. Compiles and installs the class in memory and logs a durable `kind: "new-class"` ChangeLog entry; Phase 1 does not write the file (that is Phase 2 flush). Raises a loud, specific error (no silent fallback) when the target already exists, lies outside the project tree, the declared class name does not match the path basename, or a class of that name is already loaded. The MCP tool and the LSP `beamtalk.saveClass` command are both typed front-ends that build the Beamtalk expression and submit via `evaluate` — no dedicated workspace-side op. LSP arguments: `[source, path]` (positional) or `[{source}, {path}]` (object-wrapped). |
| `flush` | -- | `:flush` / `:flush <arg>` ≡ `Workspace flush` / `Workspace flush: <arg>` (arg is a Class, a Symbol kind, or `#{#file => "path"}`) | `flush` | `executeCommand: beamtalk.flush` / `executeCommand: beamtalk.flush.class` / `executeCommand: beamtalk.flush.file` / `executeCommand: beamtalk.flush.kind` (BT-2289) | Write the pending ChangeLog entries to disk (ADR 0082 Phase 2, MCP wiring BT-2288, LSP wiring BT-2289). MCP `flush` with no argument ≡ `Workspace flush`; the optional `class` / `file` / `kind` parameters (mutually exclusive) build `Workspace flush: ClassName`, `Workspace flush: #{ #file => "path" }`, or `Workspace flush: #'kind'`. The LSP exposes the four shapes as four separate command identifiers (`beamtalk.flush`, `beamtalk.flush.class`, `beamtalk.flush.file`, `beamtalk.flush.kind`) for clean editor-side binding. Splices each durable+flushable entry's body back into its source file via byte-span replacement (no AST reprint), atomically (`<file>.tmp` + atomic rename), with external-edit conflict detection. Multi-file flushes use a two-phase commit: write every `<file>.tmp` first, then rename each in sequence; any Phase A failure aborts the whole flush with no rename happening. Returns a `FlushResult` summary recording what was written (`flushed`, `files`, `newClasses`) and what conflicted (`conflicts`, with `reason ∈ {external_edit, target_exists, span_out_of_range, source_file_unreadable, rename_failed}`). Flushed entries stay in the audit log but drop out of the active `Workspace changes` view. After a flush completes, the runtime broadcasts a `flush_completed` push frame on the `workspace` channel; the LSP server consumes it and emits `workspace/applyEdit` for each touched file that is currently open in the editor, so open buffers refresh against the new on-disk state. The REPL `:flush` meta-command, the MCP `flush` tool, and the LSP `beamtalk.flush*` commands are all CLI/typed-arg-side shortcuts that construct the equivalent Beamtalk expression — no dedicated workspace-side op. |
| `changes` | -- | `:changes` ≡ `Workspace changes` | `list_changes` | -- | Return the workspace ChangeLog — the navigable view of pending in-memory changes (ADR 0082, BT-2284, MCP wiring BT-2288). MCP `list_changes` ≡ `Workspace changes`. Backed by `beamtalk_workspace_changelog:changeLog/0` via FFI. The REPL `:changes` meta-command and the MCP `list_changes` tool are both shortcuts for the underlying message send. All pending-state queries (`size`, `notEmpty`, `dirtyMethods`, `select:`, ...) live on the returned ChangeLog object per Pharo's `Smalltalk changes` idiom — no dedicated workspace-side op. |
| `dirty` | -- | `:dirty` ≡ `Workspace changes dirtyMethods` | `dirty_methods` | -- | Per-class set of dirty selectors (ADR 0082, BT-2284, MCP wiring BT-2288). MCP `dirty_methods` ≡ `Workspace changes dirtyMethods`. The REPL `:dirty` meta-command and the MCP `dirty_methods` tool are both shortcuts that compose from the existing `changes` primitive — no dedicated workspace-side op. Pair with `list_changes` for the full ChangeLog summary or with `flush` to write the durable entries to disk. Returns a Dictionary of `Class -> {selectors}`. |
| `revert` | -- | *(via `eval`: `Workspace changes revert: anEntry`)* | *(via `evaluate`)* | *(via `executeCommand: evaluate`)* | Undo a single pending in-memory patch by re-installing its recorded prior body (ADR 0082 Phase 4, BT-2290). The re-install emits a fresh durable ChangeEntry — the original entry is preserved in the log so the audit history is intact. Lives on the ChangeLog object (per Pharo's `Smalltalk changes` idiom) and is reached through the existing `evaluate` pathway on every surface; no dedicated workspace-side op. Raises a structured error for new-class entries (destructive — deferred to a future "Destructive Workspace Operations" ADR), for methods with no recorded prior body, and for entries with no active log row. |
| `clear` (changes) | -- | *(via `eval`: `Workspace changes clear`)* | *(via `evaluate`)* | *(via `executeCommand: evaluate`)* | Discard every pending ChangeLog entry without writing to disk (ADR 0082 Phase 4, BT-2290). Memory still holds the latest patched method versions until the next workspace restart, when disk wins — matching the ADR's "clear discards the ChangeLog without writing" contract. Idempotent. Distinct from the `clear` REPL op for session locals (Session Operations row), which clears REPL bindings — `Workspace changes clear` operates on the ChangeLog. |
| `flush-kinds` | -- | *(via `eval`: `Workspace changes flushKinds: kinds`)* | *(via `evaluate`)* | *(via `executeCommand: evaluate`)* | Flush only the ChangeEntries whose kind or author_kind is in `kinds` (ADR 0082 Phase 4, BT-2290). Accepts a Set or List of Symbols — entry kinds (`#instance`, `#class`, `#'new-class'`) and/or author kinds (`#human`, `#agent`). When both dimensions are present, an entry must satisfy both (e.g. `#{#agent, #'new-class'}` flushes only agent-authored new-class entries). Unknown symbols and empty sets are rejected with a structured error. Returns the same `FlushResult` shape as `Workspace flush`. Lives on the ChangeLog object; no dedicated workspace-side op. |
| `autoflush` | -- | *(via `eval`: `Workspace autoflush` / `Workspace autoflush: true`)* | *(via `evaluate`)* | *(via `executeCommand: evaluate`)* | Workspace setting that, when enabled, causes every successful durable in-memory patch to immediately trigger `Workspace flush` (ADR 0082 Phase 4, BT-2290). Default `false`. Setting persists across workspace restarts via `metadata.json`. Best-effort: a flush failure (external-edit conflict, write error) leaves the entry pending in the log; the BEAM module install is not rolled back because live actors may hold references to the new closures (per the ADR's autoflush failure semantics). Ephemeral patches via `tryCompile:source:` are never autoflushed. |

## Navigation Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `nav-query` | -- | *(via `eval`: `SystemNavigation default {senders,implementors,references}…`)* | *(via `evaluate`)* | `textDocument/references` / `textDocument/implementation` / `textDocument/prepareCallHierarchy` / `callHierarchy/incomingCalls` / `callHierarchy/outgoingCalls` / `textDocument/prepareTypeHierarchy` / `typeHierarchy/supertypes` / `typeHierarchy/subtypes` | Structured navigation query channel for runtime-attached LSP / MCP (BT-2239). Returns typed `beamtalk_xref` site records as JSON (avoiding the inspect-string round-trip of `eval`-based queries). The op is wired and answered today; LSP nav features call it via `Backend::delegate_nav_query` when `initializationOptions.delegateToRuntime` is on and a workspace is running, falling back to the in-process AST walker otherwise. The Beamtalk `SystemNavigation default sendersOf:` / `implementorsOf:` / `referencesTo:` selectors remain the human-facing surface — `nav-query` is the typed channel for tooling. Per-method LSP children flip individual queries over to the runtime path one at a time and wire their own capabilities: [BT-2240](https://linear.app/beamtalk/issue/BT-2240) closed the declaration-merge gap in `textDocument/references` — when `context.includeDeclaration = true` the LSP overlays method-definition headers (selector cursor: via runtime `implementorsOf` → AST fallback `find_selector_declarations`) and class-declaration name spans (class cursor: AST `find_class_declarations`) onto the runtime `senders`/`references` result; when `false`, declarations are stripped from the cold-file fallback so both paths obey the flag identically. [BT-2241](https://linear.app/beamtalk/issue/BT-2241) ships `textDocument/implementation` (selector → `implementorsOf:` → method-header locations, instance- and class-side both reported). [BT-2243](https://linear.app/beamtalk/issue/BT-2243) adds `textDocument/prepareCallHierarchy` + `callHierarchy/{incomingCalls,outgoingCalls}` (prepare lives under `textDocument/` per the LSP spec because it takes a text-document position; incoming routes through `nav-query` `senders`; outgoing walks the method body's AST in-process via `SystemNavigation messagesSentBy:`'s shared `all_sends_query`). [BT-2242](https://linear.app/beamtalk/issue/BT-2242) adds `textDocument/prepareTypeHierarchy` + `typeHierarchy/{supertypes,subtypes}` via `Behaviour superclassChain` / `allSubclasses`. Type hierarchy stays cold-file (the `nav-query` wire shape is deliberately locked to senders/implementors/references; the `ClassHierarchy` index already covers stdlib + user code and matches the runtime answer on the indexed corpus). |
| `nav-symbols` | -- | *(via `eval`: `Beamtalk allClasses` + per-class introspection)* | *(via `evaluate`)* | `textDocument/documentSymbol` / `workspace/symbol` | Bulk class+method outline channel for runtime-attached LSP / MCP (BT-2244). Sibling of `nav-query` — kept on a separate op so `nav-query`'s wire shape stays locked to selector-shaped navigation (senders / implementors / references). Returns one row per loaded class with its instance- and class-side method headers, sourced from the live class registry + `beamtalk_xref:defined_selectors/2` + `beamtalk_xref:method_info/3`. LSP `document_symbol` and `symbol` call this via `Backend::delegate_nav_symbols` when `initializationOptions.delegateToRuntime` is on and a workspace is running, falling back to the in-process AST/glob walker otherwise. The **headline win**: classes loaded purely at the REPL (no `.bt` file) and methods installed via `Behaviour >>` / `compile:source:` since the last flush appear in `workspace/symbol` here — the AST walker can't see either. Source-less classes attach to the workspace-root URI with a zero-width range and `(no source file)` in the symbol detail so editors render them visibly distinct. `textDocument/documentSymbol` uses `scope = "user"` (source-backed classes only — a URI is the natural lookup key); `workspace/symbol` uses `scope = "all"` (every loaded class, for the headline win). |

## Test Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `test` | `test` | `:test` / `:t` | `test` | -- | Run BUnit tests (single class or file) |
| `test-all` | -- | `via Workspace test` | -- | -- | Run all loaded tests; MCP `test` covers the all-tests case via empty params |

## Dev / Introspection Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `methods` | -- | `via aClass methods` | -- | -- | List methods for a class; reachable on any `Behaviour` |
| `list-classes` | -- | `via Workspace classes` | `list_classes` | -- | List available classes. `workspace/symbol` was the historical LSP binding (BT-2081); BT-2244 moved the LSP source of truth to the `nav-symbols` op so the editor sees REPL-loaded and live-edited classes that `list-classes` does not enumerate as outline children. `list_classes` MCP still answers via this op. |
| `erlang-help` | -- | `surface-specific: REPL completion helper` | -- | -- | Erlang module documentation; MCP coverage tracked in BT-1903 |
| `erlang-complete` | -- | `surface-specific: REPL completion helper` | -- | -- | Erlang module/function completion |

## Performance / Tracing Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `enable-tracing` | -- | `surface-specific: agent-only workflow (BT-1606)` | `enable_tracing` | -- | Enable actor trace capture |
| `disable-tracing` | -- | `surface-specific: agent-only workflow (BT-1606)` | `disable_tracing` | -- | Disable actor trace capture |
| `get-traces` | -- | `surface-specific: agent-only workflow (BT-1606)` | `get_traces` | -- | Retrieve captured traces |
| `actor-stats` | -- | `surface-specific: agent-only workflow (BT-1606)` | `actor_stats` | -- | Per-actor/method aggregate stats |
| `export-traces` | -- | `surface-specific: agent-only workflow (BT-1606)` | `export_traces` | -- | Export traces to JSON file |

## Server Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `describe` | -- | `surface-specific: transport handshake` | `describe` | -- | Capability discovery (list supported ops) |
| `health` | -- | `surface-specific: operator probe` | -- | -- | Workspace health probe |
| `shutdown` | `workspace stop` | `surface-specific: lifecycle command` | -- | -- | Graceful workspace shutdown |

## CLI-Only Commands (no REPL op equivalent)

These CLI subcommands are build/tooling commands that operate offline (no workspace needed) and have no corresponding REPL op.

| CLI subcommand | MCP tool | LSP capability | Notes |
|---------------|----------|----------------|-------|
| `build` | -- | -- | `surface-specific: offline compiler, no workspace` |
| `build-stdlib` | -- | -- | `surface-specific: internal stdlib build step` |
| `run` | -- | -- | `surface-specific: script/service runner` |
| `new` | -- | -- | `surface-specific: project scaffolding` |
| `repl` | -- | -- | `surface-specific: starts the REPL client` |
| `check` | -- | `textDocument/publishDiagnostics` | Offline syntax/type check; LSP provides diagnostics on save |
| `fmt` | -- | `textDocument/formatting` | Format source files; LSP provides document formatting |
| `fmt-check` | -- | -- | `surface-specific: CI formatting check` |
| `lint` | `lint` | -- | Lint checks; MCP exposes for AI-assisted workflows |
| `test-script` | -- | -- | `surface-specific: btscript expression tests (CI)` |
| `test-docs` | -- | -- | `surface-specific: doctest runner (CI)` |
| `doctor` | -- | -- | `surface-specific: environment health check` |
| `deps add` | -- | -- | `surface-specific: package management` |
| `deps list` | -- | -- | `surface-specific: package management` |
| `deps update` | -- | -- | `surface-specific: package management` |
| `generate native` | -- | -- | `surface-specific: code generation` |
| `generate stubs` | -- | -- | `surface-specific: code generation` |
| `doc` | -- | -- | `surface-specific: HTML documentation generator` |
| `type-coverage` | -- | -- | `surface-specific: type inference coverage stats` |
| `workspace list` | -- | -- | `surface-specific: workspace management` |
| `workspace stop` | -- | -- | Maps to `shutdown` REPL op (see Server Operations) |
| `workspace status` | -- | -- | `surface-specific: workspace management` |
| `workspace attach` | -- | -- | `surface-specific: attaches REPL to existing workspace` |
| `workspace transcript` | -- | -- | `surface-specific: streams Transcript output` |
| `workspace logs` | -- | -- | `surface-specific: workspace log viewer` |
| `workspace create` | -- | -- | `surface-specific: workspace management` |

## MCP-Only Tools (no REPL op equivalent)

These MCP tools provide AI-assistant-specific capabilities that have no direct REPL op.

| MCP tool | Notes |
|----------|-------|
| `diagnostic_summary` | `surface-specific: AI-facing diagnostic overview for a file/package` |
| `search_examples` | `surface-specific: offline corpus search for code examples` |
| `search_classes` | `surface-specific: offline class discovery by keyword` |
| `list_packages` | `surface-specific: list loaded Beamtalk packages` |
| `package_classes` | `surface-specific: list classes in a named package` |
| `docs` | Wraps `Beamtalk help: ClassName` (optionally `selector: #sel` for instance- or class-side methods) — REPL `docs` op was hard-removed (BT-2091) |
| `load_file` | Wraps `Workspace load: "path"` — REPL `load-file` op was hard-removed (BT-2091) |
| `reload_class` | Wraps `ClassName reload` — REPL `reload` op was hard-removed (BT-2091) |
| `try_method` | Ephemeral counterpart of `save_method` (ADR 0082 Phase 3, BT-2288). Wraps `aClass tryCompile: #sel source: body` — installs in memory and logs an ephemeral ChangeLog entry that does not flush. Listed here because the `live-edit-method` REPL op row only carries one MCP binding (`save_method`, the durable path); humans typing `>>` at the REPL reach the same install chokepoint without a separate token. Promote a successful spike by calling `save_method` with the same body. |

## LSP-Only Capabilities (no REPL op equivalent)

These LSP capabilities are editor-specific and have no direct REPL op.

| LSP capability | Notes |
|----------------|-------|
| `textDocument/hover` | Class/method documentation in editor hover tooltips. Wires to `Beamtalk help: ClassName` (optionally `selector: #sel`) (BT-2081). The REPL `docs` op was hard-removed in BT-2091; same capability is now reached via the `Beamtalk help:` message-send (which walks both instance and class-side method tables) across CLI/REPL/MCP. |
| `textDocument/signatureHelp` | `surface-specific: editor parameter hints` |
| `textDocument/definition` | `surface-specific: editor go-to-definition; parity-tested (BT-2081) against the user-class set surfaced by MCP list_classes — every LSP-resolved location must point inside the loaded project tree` |
| `textDocument/references` | `surface-specific: editor find-all-references` |
| `textDocument/rangeFormatting` | `surface-specific: editor format-selection` |
| `textDocument/codeAction` | `surface-specific: editor quick-fixes and refactorings` |
| `textDocument/publishDiagnostics` | `surface-specific: editor inline error/warning display` |
| `textDocument/didOpen` | `surface-specific: editor document lifecycle` |
| `textDocument/didChange` | `surface-specific: editor document lifecycle` |
| `textDocument/didClose` | `surface-specific: editor document lifecycle` |
| `textDocument/didSave` | `surface-specific: editor document lifecycle` |
| `workspace/applyEdit` | Server-initiated edit emitted on `flush_completed` runtime push (ADR 0082 Phase 3, BT-2289). For each flushed file open in the editor, the LSP issues a whole-document `TextEdit` so the buffer realigns with the new on-disk content. Closed files are skipped — VSCode reads them fresh on next `did_open`. |

## Drift Check (CI)

The `beamtalk-surface-drift` binary (`crates/beamtalk-surface-drift/`,
BT-2082) enforces this contract automatically. It runs on every PR via
`just check-surface-drift`, wired into the `check` job in
`.github/workflows/ci.yml` and `just ci`.

The check parses this document plus the canonical inventory sources
listed above and fails when:

- A REPL op is registered in `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_*.erl`
  but missing from any operations table here. Add a row, with `--` /
  binding name / `surface-specific: <reason>` per surface.
- A documented `Bound` binding cell has no corresponding code artifact
  (e.g. `:cmd` listed but no dispatch arm in
  `crates/beamtalk-cli/src/commands/repl/mod.rs::handle_repl_command`,
  or an MCP tool name with no `#[tool(...)]` `async fn` of that name in
  `crates/beamtalk-mcp/src/server.rs`, or an LSP capability that is not
  enabled in `ServerCapabilities`).
- An MCP tool is implemented but missing from this doc — add it to the
  matching op row, or to the **MCP-Only Tools** section if it is
  intentionally `surface-specific`.
- A REPL meta-command is dispatched in the CLI but missing from this
  doc — add it to the matching op row or the **REPL Meta-Command
  Reference** table.
- An LSP capability is enabled in `crates/beamtalk-lsp/src/server.rs`
  but missing from this doc — add it to the **LSP-Only Capabilities**
  section (or the matching op row).

When adding a new surface binding, the workflow is: implement on the
surface(s), update this document in the same PR, and let CI confirm the
inventory matches.

## REPL Meta-Command Reference

For completeness, the full list of REPL meta-commands and their corresponding REPL ops:

| Meta-command | Aliases | REPL op |
|-------------|---------|---------|
| `:exit` | `:quit`, `:q` | `close` |
| `:help` | `:h`, `:?` | -- (client-side) |
| `:clear` | -- | `clear` |
| `:bindings` | `:b` | `bindings` |
| `:sync` | `:s` | `load-project` |
| `:unload <class>` | -- | `unload` |
| `:test` | `:t` | `test` / `test-all` |
| `:show-codegen` | `:sc` | `show-codegen` |
| `:interrupt` | `:int` | `interrupt` |
| `:changes` | -- | -- (composes `evaluate` of `Workspace changes`; ADR 0082 Phase 3) |
| `:dirty` | -- | -- (composes `evaluate` of `Workspace changes dirtyMethods`; ADR 0082 Phase 3) |
| `:flush` / `:flush <arg>` | -- | -- (composes `evaluate` of `Workspace flush` / `Workspace flush: <arg>`; ADR 0082 Phase 3) |
