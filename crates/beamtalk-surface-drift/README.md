# beamtalk-surface-drift

CI lint that enforces the surface parity contract recorded in
`docs/development/surface-parity.md`.

It compares the inventory of REPL ops (read from the op-table fixture
`repl_op_table.json`, pinned to `beamtalk_repl_op_table`), MCP tools (`#[tool(...)]` attributes), REPL
meta-commands (the `:cmd` literals dispatched by `handle_repl_command`),
and LSP `ServerCapabilities` fields against the parity doc, and fails
when a new binding lands without a matching doc row (or vice-versa).

Run via `just check-surface-drift`. Wired into `just ci` and the
GitHub Actions `check` job.

Tracked in BT-2082.
