# Cross-client conformance fixtures

One JSON file per format that more than one client parses or composes. Each
client's own tests read the same file, so a rule cannot change in one place
without a test failing in the others. Do not write a "keep in sync" comment
instead of a fixture (see `docs/development/architecture-principles.md`).

| Fixture | Format | Consumers |
|---|---|---|
| `attach_sname_conformance.json` | LiveView front's distribution short name, `bt_attach_<suffix>_<os_pid>@localhost` | Rust `beamtalk-desktop-broker` (`sname.rs`), Elixir `BtAttach.Workspace.attach_sname/2` (`workspace_test.exs`) |
| `port_file_conformance.json` | Workspace `port` file, `PORT` then optional `NONCE`; strict, digits only 1-65535 on the first line | Rust `beamtalk_workspace::parse_port_file`, TypeScript `parsePortFileContent` (`editors/vscode/src/__tests__/portFile.test.ts`) |

Other conformance fixtures live next to the code they pin:
`runtime/apps/*/test/fixtures/*_conformance.json` (for example
`beamtalk_root_dir_conformance.json`, read by Rust, Erlang, Elixir and
TypeScript) and `crates/beamtalk-workspace/tests/fixtures/workspace_id_cases.json`
(Rust and TypeScript).
