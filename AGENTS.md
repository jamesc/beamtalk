<!-- Short index for agents. For full details, see docs/agents/expanded.md -->
# AGENTS.md - Beamtalk Agent Quick Reference

This file is a compact one-page index for AI coding agents. For full, detailed guidance, examples, and workflows see [`docs/agents/expanded.md`](docs/agents/expanded.md).

## Essential Rules

- **Repo values (use for API calls):** Owner: `jamesc`, Repo: `beamtalk`.
- **Syntax verification:** Always verify Beamtalk syntax in `docs/beamtalk-language-features.md`, `examples/`, or `tests/` before using it.
- **No duplicate implementations:** Grep for an existing implementation before writing a new helper/list/table; import or extract, don't copy. A "keep in sync"/"mirrors" comment needs a test enforcing it, not just prose. See [expanded doc](docs/agents/expanded.md#duplication--drift-prevention).
- **Structured errors:** Use `#beamtalk_error{}` for all user-facing/public API errors. Internal runtime helpers (`runtime/**/*.erl`) may use `{ok, Value} | {error, Reason}` if translated at public API boundaries.
- **Blocks into class methods:** A class method runs in its class's gen_server process, so a block passed into one runs there too. Values and `^` cross fine; process-local side effects (process dictionary, `self()`) do not, and messaging the same class back raises `dispatch_error`. See [expanded doc](docs/agents/expanded.md#blocks-passed-into-class-methods).
- **CI checklist:** Use `just ci` for full checks; quick commands: `just build`, `just test`, `just test-stdlib`, `just test-repl-protocol`.
- **Agent shortcuts:** You may run `just`, `cargo`, `rustc`, `rustfmt`, and `git` without asking.

## Issue Workflow

Create Linear issues with context and acceptance criteria. Always set labels: `agent-state`, `item-area`, `issue-type`, `item-size`. Establish blocking relationships for dependencies. See [expanded doc](docs/agents/expanded.md#work-tracking) for label lists, GraphQL examples, and issue templates.

## Key Links

| Resource | Location |
|----------|----------|
| Full agent guidelines | [`docs/agents/expanded.md`](docs/agents/expanded.md) |
| Language spec | [`docs/beamtalk-language-features.md`](docs/beamtalk-language-features.md) |
| Architecture principles | [`docs/development/architecture-principles.md`](docs/development/architecture-principles.md) |
| Rust guidelines | [`docs/development/rust-guidelines.md`](docs/development/rust-guidelines.md) |
| Erlang guidelines | [`docs/development/erlang-guidelines.md`](docs/development/erlang-guidelines.md) |
| Common tasks | [`docs/development/common-tasks.md`](docs/development/common-tasks.md) |
| Debugging | [`docs/development/debugging.md`](docs/development/debugging.md) |
| ADRs | [`docs/ADR/`](docs/ADR/) |
