# Beamtalk Examples

Each example is a self-contained **workspace** with `src/`, `test/`, and the
standard project files (`.mcp.json`, `AGENTS.md`, `.github/copilot-instructions.md`).
Open a workspace in Claude Code and the MCP server starts automatically.

**New to Beamtalk?** Start with the [REPL Tutorial](repl-tutorial.md).

## Workspaces

| Workspace | What it demonstrates |
|-----------|---------------------|
| [getting-started/](getting-started/) | Hello world, Counter, Hanoi, closures, inheritance |
| [bank/](bank/) | Actor coordination: accounts, transfers, overdraft protection |
| [chat-room/](chat-room/) | Multi-actor: actors, inheritance, Set/List, message broadcasting |
| [sicp/](sicp/) _(coming soon)_ | SICP Scheme compiler written in Beamtalk |
| [gof-patterns/](gof-patterns/) _(coming soon)_ | Gang of Four patterns in idiomatic Beamtalk |

## Quickstart

```bash
cd examples/getting-started
beamtalk repl
> :load src/hello.bt
> Hello new
```

Each workspace's `README.md` has a full walkthrough.

## Workspace Layout

Every workspace follows the `beamtalk new` convention:

```
workspace/
├── beamtalk.toml        # Package manifest
├── src/                 # Source files (.bt)
├── test/                # BUnit test files
├── AGENTS.md            # AI agent guide (MCP tools, pitfalls)
├── .mcp.json            # beamtalk-mcp config — auto-starts in Claude Code
├── .github/
│   └── copilot-instructions.md
└── .gitignore
```

## Adding a New Workspace

1. Create a directory under `examples/`
2. Run `beamtalk new <name>` inside it, or copy the layout above
3. Add `src/` files, `test/` files, and a `README.md`
4. Include copyright headers in all `.bt` files
5. Update this README

## Getting Help

- Type `:help` in the REPL for command reference
- See [REPL Tutorial](repl-tutorial.md) for a guided tour
- See [Language Features](../docs/beamtalk-language-features.md) for syntax reference
- Report issues at: https://github.com/jamesc/beamtalk/issues
