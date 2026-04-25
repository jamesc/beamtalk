# Surface Parity Map

This document tracks which REPL operations are exposed across each Beamtalk surface (CLI, REPL meta-commands, MCP tools, LSP capabilities). It is the single source of truth for identifying gaps and preventing surface drift.

## Parity Contract

Any operation **not** labelled `surface-specific` must produce equivalent output across all surfaces where it appears. "Equivalent" means the same structured data (modulo transport encoding); display formatting may vary per surface.

When adding a new capability to any surface, update this table. If the capability maps to an existing REPL op, add it to the corresponding row. If it is surface-specific, add the reason.

## Legend

| Symbol | Meaning |
|--------|---------|
| **name** | Binding exists (tool/command/handler name shown) |
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
| `load-file` | -- | -- | `load_file` | -- | Load a single `.bt` file (deprecated op, use `Workspace load:`) |
| `load-source` | -- | -- | -- | -- | Load inline source string (used by browser workspace) |
| `load-project` | -- | `:sync` / `:s` | `load_project` | -- | Sync project files from `beamtalk.toml` |
| `reload` | -- | -- | `reload_class` | -- | Hot-reload a class (deprecated op, use `ClassName reload`) |

## Session Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `clear` | -- | `:clear` | `clear` | -- | Clear session bindings (deprecated op) |
| `bindings` | -- | `:bindings` / `:b` | `get_bindings` | -- | List session variable bindings (deprecated op) |
| `sessions` | -- | -- | -- | -- | List active REPL sessions |
| `clone` | -- | -- | -- | -- | Create a new session |
| `close` | -- | `:exit` / `:quit` / `:q` | -- | -- | Close session; `:exit` exits the CLI REPL |
| `interrupt` | -- | -- | `interrupt` | -- | Cancel a running evaluation |

## Actor Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `actors` | -- | -- | `list_actors` | -- | List running actors |
| `inspect` | -- | -- | `inspect` | -- | Inspect actor state |
| `kill` | -- | -- | -- | -- | Terminate an actor |

## Module Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `modules` | -- | -- | -- | -- | List loaded modules (deprecated op, use `Workspace classes`) |
| `unload` | -- | `:unload <class>` | `unload` | -- | Unload a class from the workspace |

## Test Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `test` | `test` | `:test` / `:t` | `test` | -- | Run BUnit tests (single class or file) |
| `test-all` | -- | -- | -- | -- | Run all loaded tests |

## Dev / Introspection Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `docs` | -- | -- | `docs` | -- | Class/method documentation (deprecated op, use `Beamtalk help:`) |
| `methods` | -- | -- | -- | -- | List methods for a class |
| `list-classes` | -- | -- | `list_classes` | -- | List available classes |
| `erlang-help` | -- | -- | -- | -- | Erlang module documentation |
| `erlang-complete` | -- | -- | -- | -- | Erlang module/function completion |

## Performance / Tracing Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `enable-tracing` | -- | -- | `enable_tracing` | -- | Enable actor trace capture |
| `disable-tracing` | -- | -- | `disable_tracing` | -- | Disable actor trace capture |
| `get-traces` | -- | -- | `get_traces` | -- | Retrieve captured traces |
| `actor-stats` | -- | -- | `actor_stats` | -- | Per-actor/method aggregate stats |
| `export-traces` | -- | -- | `export_traces` | -- | Export traces to JSON file |

## Server Operations

| REPL op | CLI subcommand | REPL meta-command | MCP tool | LSP capability | Notes |
|---------|---------------|-------------------|----------|----------------|-------|
| `describe` | -- | -- | `describe` | -- | Capability discovery (list supported ops) |
| `health` | -- | -- | -- | -- | Workspace health probe |
| `shutdown` | `workspace stop` | -- | -- | -- | Graceful workspace shutdown |

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

## LSP-Only Capabilities (no REPL op equivalent)

These LSP capabilities are editor-specific and have no direct REPL op.

| LSP capability | Notes |
|----------------|-------|
| `textDocument/hover` | `surface-specific: editor hover information` |
| `textDocument/signatureHelp` | `surface-specific: editor parameter hints` |
| `textDocument/definition` | `surface-specific: editor go-to-definition` |
| `textDocument/references` | `surface-specific: editor find-all-references` |
| `textDocument/documentSymbol` | `surface-specific: editor outline/breadcrumbs` |
| `textDocument/rangeFormatting` | `surface-specific: editor format-selection` |
| `textDocument/codeAction` | `surface-specific: editor quick-fixes and refactorings` |
| `textDocument/publishDiagnostics` | `surface-specific: editor inline error/warning display` |
| `textDocument/didOpen` | `surface-specific: editor document lifecycle` |
| `textDocument/didChange` | `surface-specific: editor document lifecycle` |
| `textDocument/didClose` | `surface-specific: editor document lifecycle` |
| `textDocument/didSave` | `surface-specific: editor document lifecycle` |

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
