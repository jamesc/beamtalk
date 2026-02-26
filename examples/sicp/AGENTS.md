# sicp — Agent Guide

> **Coming soon.** This workspace will contain a SICP Scheme compiler written in Beamtalk.

## Workspace Structure

```
sicp/
├── beamtalk.toml        # Package manifest
├── src/                 # Source files (to be added)
├── test/                # BUnit tests (to be added)
├── AGENTS.md            # This file
├── .mcp.json            # MCP server config
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Starting the REPL

```bash
cd examples/sicp
beamtalk repl
```

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

| Tool | When to use |
|------|-------------|
| `evaluate` | Test expressions, explore values, prototype code snippets |
| `load_file` | Load a `.bt` file into the workspace before evaluating it |
| `reload_module` | Hot-reload a module after editing — migrates live actors |
| `docs` | Look up stdlib class or method docs — primary stdlib reference |
| `run_tests` | Run BUnit tests (class name, or all) |
