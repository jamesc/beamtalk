<!-- Short index for agents. For full details, see docs/agents/expanded.md -->
# AGENTS.md - Beamtalk Agent Quick Reference

This file is a compact one-page index for AI coding agents. For full, detailed guidance, examples, and workflows see [`docs/agents/expanded.md`](docs/agents/expanded.md).

## Essential Rules

- **Repo values (use for API calls):** Owner: `jamesc`, Repo: `beamtalk`.
- **Syntax verification:** Always verify Beamtalk syntax in `docs/beamtalk-language-features.md`, `examples/`, or `tests/` before using it.
- **Structured errors:** Use `#beamtalk_error{}` for all user-facing/public API errors. Internal runtime helpers (`runtime/**/*.erl`) may use `{ok, Value} | {error, Reason}` if translated at public API boundaries.
- **CI checklist:** Use `just ci` for full checks; quick commands: `just build`, `just test`, `just test-stdlib`, `just test-e2e`.
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
