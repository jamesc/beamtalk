# CLAUDE.md - Beamtalk

Beamtalk is a Smalltalk/Newspeak-inspired language compiling to BEAM via Rust. The compiler generates Core Erlang, compiled to BEAM bytecode via erlc.

**Repo:** Owner: `jamesc`, Repo: `beamtalk`

## Essential Rules

- **Verify Beamtalk syntax** before writing any new `.bt` or Erlang FFI code: grep for 3+ existing examples, then confirm against `docs/beamtalk-language-features.md`. Never invent syntax or guess from Smalltalk memory.
- **No duplicate implementations:** Before writing a helper/list/table, grep for an existing implementation of the same rule and import or extract it — never copy. "Module X sits below Y in the dependency graph" is not a reason to duplicate; extract a shared leaf module below both instead. Never write a "keep in sync"/"mirrors" comment without a test that enforces the invariant. A rule crossing the Rust/Erlang boundary needs a shared conformance fixture or code generation, not a comment. See `docs/development/architecture-principles.md` § Duplication & the Shared-Leaf-Module Pattern.
- **Codegen:** All Core Erlang codegen uses the `Document` / `docvec!` + typed-leaf API (`leaf::atom`, `leaf::var`, `leaf::string_lit`, `leaf::int_lit`, `leaf::fname`, `leaf::binary_lit`, …). Never use `format!()` or string concatenation for Core Erlang fragments — not even atoms, arities, or map keys. See `docs/ADR/0089-typed-document-leaves.md`.
- **State-threading codegen:** Loops, conditionals, exception handling (`on:do:`/`ensure:`), list-op accumulators, gen_server actor-state routing, class-var shadow-writes, and Tier 2 stateful-block bodies and NLR boundaries all lower through `ThreadedIr` as the emission input, verified by its `verify()` (`crates/beamtalk-codegen/src/core_erlang/threaded_ir.rs`). General expression codegen stays AST-directed. A violated invariant is a `VerifyError`, reported via the shared `report_threaded_ir_verify_errors` helper — never add a new ad-hoc `debug_assert!` at a state-threading call site. See `docs/development/debugging.md` § ThreadedIr verifier and `docs/ADR/0111-lowered-ir-verifier-for-state-threading.md`.
- **Type annotations:** `::` (double-colon), never single `:`. E.g. `state: x :: Integer = 0`, `param :: Type -> ReturnType =>`.
- **Returns (`^`):** Use `^` only for early/non-local returns, never on the last expression. Inside a block, `^` exits the *enclosing method* immediately (standard Smalltalk semantics, via throw/catch in codegen).
- **Structured errors:** Use `#beamtalk_error{}` for all user-facing/public API errors. Internal helpers may use `{ok, V} | {error, R}` if translated at public boundaries.
- **DNU:** An unrecognised message raises `does_not_understand` (`#beamtalk_error{}`). Use `respondsTo:` to check before calling.
- **Blocks into class methods:** A class method runs in its class's gen_server process, so a block passed into one runs there too. Values and `^` cross that boundary; process-local side effects (process dictionary, `self()`) do not, and messaging the same class back raises `dispatch_error`. See `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods.
- **Erlang logging:** OTP logger macros (`?LOG_ERROR`, …), never `io:format` or `logger:error()`. Set `domain` metadata in `init/1` via `logger:set_process_metadata(#{domain => [beamtalk, runtime]})` (`[beamtalk, stdlib]` for stdlib) so all calls inherit it.
- **License headers:** All source **code** files (`.rs`, `.erl`, `.bt`, `.hrl`) need `Copyright 2026 James Casey` / `SPDX-License-Identifier: Apache-2.0`. Not on `.md` files.
- **Test assertions:** Every expression in test files needs a `// =>` assertion (even `// => _`) — no assertion means no execution.
- **Cross-platform temp paths:** Never hardcode `/tmp/`. Use `File tempDirectory` (BT) / `beamtalk_file:'tempDirectory'()` (Erlang) and concatenate, or prefer relative temp filenames in runtime EUnit tests. See `docs/development/testing-strategy.md`.
- **Generated files:** Before editing `.app.src` or other generated artifacts, check if a build step owns them — `build_stdlib.rs` generates `beamtalk_stdlib.app.src`, so edit the generator.
- **REPL output:** Confirm with the user before changing any REPL display value, prompt format, or output behaviour — it's usually intentional and covered by e2e tests.
- **Surface parity:** When adding/modifying operations on any surface (CLI, REPL, MCP, LSP), update `docs/development/surface-parity.md`. Operations not labelled `surface-specific` must produce equivalent output across surfaces.

## CI Commands

```bash
just ci          # Full CI checks
just build       # Build Rust + Erlang runtime
just test        # Rust + stdlib + BUnit + runtime tests
just test-stdlib # Compiled language feature tests (~14s)
just test-bunit  # BUnit TestCase tests
just verify-threaded-ir  # ThreadedIr verify() over full stdlib + bootstrap-test corpus
just test-repl-protocol  # REPL TCP-protocol tests (~50s)
just fmt         # Format all code
just clippy      # Lints (warnings = errors)
just dialyzer    # Erlang type checking
```

You may run `just`, `cargo`, `rustc`, `rustfmt`, and `git` without asking.

## Test Organization

| Test needs... | Where |
|---|---|
| Bootstrap primitives (arithmetic, booleans, equality, strings) | `stdlib/bootstrap-test/*.btscript` |
| Language features (collections, closures, regex, actors, etc.) | `stdlib/test/*.bt` (BUnit) |
| Stateful tests with setUp/tearDown | `stdlib/test/*.bt` (BUnit) |
| Workspace bindings, REPL commands, variable persistence | `tests/repl-protocol/cases/*.btscript` |

Prefer `stdlib/test/*.bt` (BUnit TestCase) for new tests. Use `stdlib/bootstrap-test/` only for bootstrap-critical primitives.

## Architecture

- **DDD contexts:** Language Service, Compilation, Runtime, REPL.
- **Dependencies flow down only** — `beamtalk-core` never imports `beamtalk-cli` or `beamtalk-lsp`.
- **Never** panic/`unwrap()` on user input, or add deps without justification.
- **Always** return `(Result, Vec<Diagnostic>)` for user-facing operations.

## Workflow

- **Versioning & releases:** `VERSION` at repo root is the single source of truth; everything derives from it at build time. Don't hand-edit derived version strings. Full process: `docs/development/releasing.md`.
- **Issues (Linear):** Project prefix `BT`. Always set labels `agent-state`, `item-area`, `issue-type`, `item-size`, and blocking relationships for dependencies. Include context, acceptance criteria, files to modify, and references.
- **Scope control:** Fix inline — formatting/typos in files you're touching, test failures from your changes. File follow-up issues for unrelated bugs, optimizations, or refactors not required for the task.

## Key References

| Resource | Location |
|----------|----------|
| Full agent guidelines | `docs/agents/expanded.md` |
| Language spec | `docs/beamtalk-language-features.md` |
| Architecture principles | `docs/development/architecture-principles.md` |
| Rust guidelines | `docs/development/rust-guidelines.md` |
| Erlang guidelines | `docs/development/erlang-guidelines.md` |
| Common tasks | `docs/development/common-tasks.md` |
| Debugging | `docs/development/debugging.md` |
| Surface parity | `docs/development/surface-parity.md` |
| Releasing | `docs/development/releasing.md` |
| ADRs | `docs/ADR/` |
