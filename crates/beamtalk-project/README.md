# beamtalk-project

Project/workspace root discovery and manifest parsing shared by Beamtalk
tooling (CLI, LSP, MCP). Walks up from a starting directory looking for
project markers (`.beamtalk`, `beamtalk.toml`, `.git`) and resolves package
source files from a manifest.
