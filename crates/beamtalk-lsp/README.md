# beamtalk-lsp

Language Server Protocol server for the Beamtalk programming language.

Provides IDE features for `.bt` files via the LSP protocol:
- Code completions
- Hover information
- Go to definition
- Find references
- Diagnostics (errors and warnings)
- Document symbols (outline)

Usage: Run the LSP server (beamtalk-lsp). Use `-v`/`--verbose` to increase logging to stderr (one `-v` → debug, `-vv+` → trace).
