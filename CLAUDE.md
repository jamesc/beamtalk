# CLAUDE.md - Beamtalk

Beamtalk is a Smalltalk/Newspeak-inspired language compiling to BEAM via Rust. The compiler generates Core Erlang, compiled to BEAM bytecode via erlc.

**Repo:** Owner: `jamesc`, Repo: `beamtalk`

## Essential Rules

- **Verify Beamtalk syntax** before writing any new `.bt` or Erlang FFI code: grep for 3+ existing codebase examples of the pattern, then confirm against `docs/beamtalk-language-features.md`. Do not invent syntax or guess from Smalltalk memory.
- **Structured errors:** Use `#beamtalk_error{}` for all user-facing/public API errors. Internal runtime helpers may use `{ok, Value} | {error, Reason}` if translated at public boundaries.
- **Codegen:** All Core Erlang codegen MUST use `Document` / `docvec!` API. **NEVER use `format!()` or string concatenation to produce Core Erlang fragments — not even for "simple" atoms, arities, or map keys.** This rule has been violated repeatedly and required a dedicated cleanup effort (BT-875). Do not reintroduce violations.
- **Erlang logging:** Use OTP logger macros (`?LOG_ERROR`, etc.), never `io:format` or `logger:error()`.
- **License headers:** All source **code** files (`.rs`, `.erl`, `.bt`, `.hrl`) need `Copyright 2026 James Casey` / `SPDX-License-Identifier: Apache-2.0`. Do **not** add license headers to `.md` files.
- **Implicit returns:** Use `^` ONLY for early returns, never on last expression.
- **Block returns:** `^` inside a block returns from the **block only** — it is NOT a non-local return from the enclosing method. `someFlag ifFalse: [^nil]` returns nil from the block; the method continues executing.
- **Type annotations:** Beamtalk uses `::` (double-colon) for type annotations, never single `:`. Examples: `state: x :: Integer = 0`, `param :: Type -> ReturnType =>`.
- **DNU:** `doesNotUnderstand:` returns `false` by default. Unrecognized message sends silently return `false` — not an error. Never treat a `false` result as proof that a method exists and returned correctly.
- **Generated files:** Before editing `.app.src` or other generated artifacts, check if a build step owns them. `build_stdlib.rs` generates `beamtalk_stdlib.app.src` — edit the generator, not the output.
- **Worktree stale builds:** After entering or creating a worktree, run `just build` before running tests. Stale BEAM artifacts from another build cause false failures.
- **REPL output:** Before changing any REPL display value, prompt format, or output behaviour, confirm with the user — existing output is usually intentional and covered by e2e tests. Do not "fix" output that looks wrong without checking first.
- **Test assertions:** Every expression in test files MUST have a `// =>` assertion (even `// => _`). No assertion = no execution.

## CI Commands

```bash
just ci          # Full CI checks
just build       # Build Rust + Erlang runtime
just test        # Fast tests (~10s)
just test-stdlib # Compiled language feature tests (~14s)
just test-e2e    # REPL integration tests (~50s)
just fmt         # Format all code
just clippy      # Lints (warnings = errors)
```

You may run `just`, `cargo`, `rustc`, `rustfmt`, and `git` without asking.

## Test Organization

| Test needs... | Where |
|---|---|
| Bootstrap primitives (arithmetic, booleans, equality, strings) | `stdlib/bootstrap-test/*.bt` |
| Language features (collections, closures, regex, actors, etc.) | `stdlib/test/*.bt` (BUnit) |
| Stateful tests with setUp/tearDown | `stdlib/test/*.bt` (BUnit) |
| Workspace bindings, REPL commands, variable persistence | `tests/e2e/cases/*.bt` |

Prefer `stdlib/test/*.bt` (BUnit TestCase) for new tests. Only use `stdlib/bootstrap-test/` for bootstrap-critical primitives.

## Architecture

- **DDD contexts:** Language Service, Compilation, Runtime, REPL
- **Dependencies flow down only** — `beamtalk-core` never imports `beamtalk-cli` or `beamtalk-lsp`
- **Never** panic on user input, use `unwrap()` on user input, or add deps without justification
- **Always** return `(Result, Vec<Diagnostic>)` for user-facing operations

## Issue Workflow (Linear)

Project prefix: `BT`. Always set labels: `agent-state`, `item-area`, `issue-type`, `item-size`. Establish blocking relationships for dependencies. Include context, acceptance criteria, files to modify, and references in issues.

## Scope Control

Fix inline: formatting/typos in files you're modifying, test failures from your changes. Create follow-up issues for unrelated bugs, optimization opportunities, or refactoring not required for the current task.

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
| ADRs | `docs/ADR/` |
