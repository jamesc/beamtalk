# beamtalk-project

Project/workspace root discovery and manifest parsing shared by Beamtalk
tooling (CLI, LSP, MCP). Walks up from a starting directory looking for
project markers (`.beamtalk`, `beamtalk.toml`, `.git`) and resolves package
source files from a manifest.

It also holds what every tool needs to see a package's dependencies the same
way: `[dependencies]` parsing and package-name validation (`dependencies`,
`package_name`), the offline transitive dependency-graph walk
(`dependency_graph`), and the rule for when dependency extensions are
completely loaded (`dependency_extensions`, ADR 0070 WS3).
