# beamtalk-mcp

MCP (Model Context Protocol) server for the beamtalk REPL — enabling AI agents to interact with live beamtalk objects.

## Overview

`beamtalk-mcp` wraps the beamtalk REPL's JSON-over-TCP protocol as an MCP server, allowing any MCP-compatible client (GitHub Copilot, Claude Desktop, Cursor, etc.) to:

- **Evaluate** beamtalk expressions in a live workspace
- **Spawn and interact** with actors
- **Inspect** actor state
- **Load** source files and **hot-reload** modules
- **Explore** classes, methods, and documentation

## Quick Start

### 1. Start a beamtalk REPL

```bash
beamtalk repl
```

### 2. Run the MCP server

```bash
# Auto-discover running REPL
beamtalk-mcp

# Or specify port explicitly
beamtalk-mcp --port 9876
```

### 3. Configure your MCP client

#### Claude Desktop / Cursor

Add to your MCP config (`claude_desktop_config.json` or equivalent):

```json
{
  "mcpServers": {
    "beamtalk": {
      "command": "beamtalk-mcp",
      "args": []
    }
  }
}
```

#### GitHub Copilot CLI

The MCP server is automatically available when configured in your MCP settings.

## Available Tools

| Tool | Description |
|------|-------------|
| `evaluate` | Evaluate a beamtalk expression in the live REPL |
| `complete` | Get autocompletion suggestions for partial input |
| `load_file` | Load a `.bt` source file into the workspace |
| `inspect` | Inspect a running actor's state by PID |
| `list_actors` | List all running actors with class and PID |
| `list_modules` | List all loaded modules with source and actor count |
| `get_bindings` | Get current REPL session variable bindings |
| `reload_module` | Hot-reload a module (recompile + migrate actors) |
| `docs` | Get documentation for a class or method |

## Architecture

```text
MCP Client (Copilot, Claude, Cursor)
    ↕ stdio (JSON-RPC / MCP protocol)
beamtalk-mcp server
    ↕ TCP (newline-delimited JSON)
beamtalk REPL server
    ↕ BEAM VM
Live beamtalk objects & actors
```

The MCP server maintains a **persistent TCP connection** to the REPL for the lifetime of the MCP session. This means variable bindings persist across tool calls — you can assign a variable in one `evaluate` call and use it in the next.

## CLI Options

```text
beamtalk-mcp [OPTIONS]

Options:
  -p, --port <PORT>                  REPL server port (overrides workspace discovery)
  -w, --workspace-id <WORKSPACE_ID>  Workspace ID for port discovery
  -h, --help                         Print help
```

### Port Discovery

The server discovers the REPL port in this order:

1. **`--port` flag** — explicit port number
2. **`--workspace-id` flag** — reads port from `~/.beamtalk/workspaces/<id>/port`
3. **Current directory** — generates workspace ID from `$PWD` and reads port file
4. **Any workspace** — scans `~/.beamtalk/workspaces/` for any running REPL

## Development

```bash
# Build
cargo build -p beamtalk-mcp

# Run with logging
RUST_LOG=beamtalk_mcp=debug cargo run -p beamtalk-mcp -- --port 9876

# Test with MCP Inspector
npx @modelcontextprotocol/inspector cargo run -p beamtalk-mcp -- --port 9876
```

## License

Apache-2.0
