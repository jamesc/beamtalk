# MCP Demo: AI Agent Explores a Counter

This walkthrough shows how an AI agent interacts with live beamtalk objects through the MCP server.

## Setup

```bash
# Terminal 1: Start the REPL
beamtalk repl

# Terminal 2: Start the MCP server (auto-discovers REPL)
beamtalk-mcp
```

## Demo Transcript

An AI agent connected via MCP performs the following sequence:

### 1. Load the Counter class

**Tool call:** `load_file`
```json
{ "path": "examples/counter.bt" }
```

**Response:**
```
Loaded classes: Counter
```

### 2. Explore the Counter class

**Tool call:** `docs`
```json
{ "class": "Counter" }
```

**Response:**
```
Counter (Actor)
  Instance Methods:
    increment — Increment the counter
    decrement — Decrement the counter
    value — Get the current value
    reset — Reset to zero
```

### 3. Spawn a Counter actor

**Tool call:** `evaluate`
```json
{ "code": "c := Counter spawn" }
```

**Response:**
```
#Actor<Counter,0.173.0>
```

### 4. Interact with the Counter

**Tool call:** `evaluate`
```json
{ "code": "c increment" }
```

**Response:**
```
1
```

**Tool call:** `evaluate`
```json
{ "code": "3 timesRepeat: [c increment]" }
```

**Response:**
```
nil
```

### 5. Inspect the actor's state

**Tool call:** `inspect`
```json
{ "actor": "<0.173.0>" }
```

**Response:**
```json
{
  "class": "Counter",
  "state": {
    "value": 4
  }
}
```

### 6. List all running actors

**Tool call:** `list_actors`

**Response:**
```
Counter (counter) — pid: <0.173.0>
```

### 7. Hot-reload after code changes

After modifying `examples/counter.bt` to add a new method:

**Tool call:** `reload_module`
```json
{ "module": "counter" }
```

**Response:**
```
Module reloaded successfully
Affected actors: 1
```

### 8. Use the new method

**Tool call:** `evaluate`
```json
{ "code": "c value" }
```

**Response:**
```
4
```

## What This Demonstrates

1. **Live object interaction** — The agent spawns and messages real actors
2. **Session persistence** — Variables (`c`) persist across tool calls
3. **State inspection** — The agent can examine actor internals
4. **Hot code reloading** — Code changes apply to running actors
5. **Discovery** — The agent explores classes via documentation

This is the agent-native development vision: AI agents as first-class participants in a live, interactive programming environment.
