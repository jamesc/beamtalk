# Copilot Instructions for sicp

This is a [Beamtalk](https://jamesc.github.io/beamtalk) example workspace implementing a Scheme interpreter following SICP.

## Key Conventions

- Source files use `.bt` extension and live in `src/`
- Tests use BUnit (TestCase subclasses) and live in `test/`
- Build output goes to `_build/` (gitignored)
- Package manifest is `beamtalk.toml`

## Build Commands

```bash
beamtalk build    # Compile the project
beamtalk repl     # Start interactive REPL
beamtalk test     # Run tests
```

## MCP / Live Workspace

`.mcp.json` configures the `beamtalk` MCP server. In Claude Code it starts
automatically. Use the MCP tools (`evaluate`, `load_file`, `reload_module`,
`run_tests`, `docs`, `inspect`) to interact with a live REPL rather than
inferring behaviour from source. See `AGENTS.md` for the full tool list.
