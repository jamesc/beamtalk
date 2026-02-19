Title: Add global -v/--verbose and map to beamtalk logging for all CLI binaries

> DRAFT: This file is a local draft used to create Linear issue BT-730; consider moving to the official Linear issue tracker or `.github/skills/` for operational workflows.
Team: BT
Assignee: jamesc.000@gmail.com
Agent State: agent-ready
Item Area: cli
Type: Improvement
Size: M
Priority: 3

Context:
Users need an easy, consistent way to enable debug/trace logging across Beamtalk CLI tools. `RUST_LOG` works but is inconvenient; recent changes added `-v` support to `beamtalk` and `beamtalk-mcp` partially, but other CLI binaries and a broad default directive (so `-v` surfaces logs from all beamtalk crates) are missing.

Acceptance Criteria:
- [ ] Add a global `-v/--verbose` flag for CLI binaries (counted) where applicable.
- [ ] Map verbosity to logging directives: `-v` → `beamtalk=debug`, `-vv+` → `beamtalk=trace` by default.
- [ ] Ensure `RUST_LOG` env var continues to override the flag when present.
- [ ] Apply the same behavior to these CLI binaries: `beamtalk` (already updated), `beamtalk-mcp` (already updated), `beamtalk-compiler-port`, `beamtalk-lsp`, and any other top-level CLI crates.
- [ ] Update README/docs for `beamtalk` and `beamtalk-mcp` to document `--verbose` usage and mapping.
- [ ] Add unit/integration tests that validate flag parsing and that the EnvFilter is initialized as expected (where feasible without starting long-running processes).

Files to Modify:
- crates/beamtalk-cli/src/main.rs
- crates/beamtalk-mcp/src/main.rs
- crates/beamtalk-compiler-port/src/main.rs
- crates/beamtalk-lsp/src/main.rs
- crates/beamtalk-mcp/README.md
- crates/beamtalk-cli/README.md

Dependencies: None

References:
- Current PR/diff that added `-v` to beamtalk CLI and beamtalk-mcp (local changes in this branch)
- `tracing_subscriber::EnvFilter` usage patterns in existing crates

Notes:
- Default directive uses `beamtalk=...` to cover logs across crates; if we prefer per-crate directives, adjust accordingly.
- If Linear API integration is configured, create the issue in Linear using the above metadata; otherwise this file serves as the canonical issue payload for manual creation.

JSON payload for Linear (example):
{
  "action": "create",
  "title": "Add global -v/--verbose and map to beamtalk logging for all CLI binaries",
  "team": "BT",
  "assignee": "jamesc.000@gmail.com",
  "body": "Context:\nUsers need an easy, consistent way to enable debug/trace logging across Beamtalk CLI tools...",
  "labels": ["agent-ready", "Improvement", "cli", "M"],
  "priority": 3
}
