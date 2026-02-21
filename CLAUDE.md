# CLAUDE.md - Beamtalk

Beamtalk is a Smalltalk/Newspeak-inspired language compiling to BEAM via Rust. The compiler generates Core Erlang, compiled to BEAM bytecode via erlc.

**Repo:** Owner: `jamesc`, Repo: `beamtalk`

## Essential Rules

- **Verify Beamtalk syntax** in `docs/beamtalk-language-features.md`, `examples/`, or `tests/` before using it. Do not invent syntax.
- **Structured errors:** Use `#beamtalk_error{}` for all user-facing/public API errors. Internal runtime helpers may use `{ok, Value} | {error, Reason}` if translated at public boundaries.
- **Codegen:** All Core Erlang codegen MUST use `Document` / `docvec!` API. Never `format!()` or string concatenation.
- **Erlang logging:** Use OTP logger macros (`?LOG_ERROR`, etc.), never `io:format` or `logger:error()`.
- **License headers:** All source files need `Copyright 2026 James Casey` / `SPDX-License-Identifier: Apache-2.0`.
- **Implicit returns:** Use `^` ONLY for early returns, never on last expression.
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
