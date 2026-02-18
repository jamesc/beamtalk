# Beamtalk REPL Message Protocol

The Beamtalk REPL uses a JSON-based message protocol over WebSocket for communication between clients (CLI, IDE, web) and the workspace backend. The protocol is inspired by [nREPL](https://nrepl.org/) and [Jupyter](https://jupyter.org/).

## Transport

- **Protocol:** WebSocket (RFC 6455) over TCP
- **Endpoint:** `ws://127.0.0.1:{port}/ws`
- **Encoding:** JSON, one message per WebSocket text frame
- **Binding:** `127.0.0.1` (loopback only, for security)
- **Default port:** OS-assigned (port 0); override with `--port` or `BEAMTALK_REPL_PORT`

### Connection Handshake (ADR 0020)

1. **TCP connect** to `127.0.0.1:{port}`
2. **WebSocket upgrade** to `ws://127.0.0.1:{port}/ws`
3. **Read auth-required** — server sends `{"op":"auth-required"}` (no session yet)
4. **Send auth** — client sends `{"type":"auth","cookie":"<cookie>"}` as first message
5. **Read auth response** — server replies `{"type":"auth_ok"}` on success, or `{"type":"auth_error","message":"..."}` + connection close on failure
6. **Read session-started** — after auth_ok, server sends `{"op":"session-started","session":"<id>"}`
7. **Protocol ready** — client can now send protocol operations

Session creation is deferred until after successful authentication to prevent resource exhaustion from unauthenticated connections.

### Cookie Authentication

The cookie is read from the workspace's `cookie` file at `~/.beamtalk/workspaces/{id}/cookie` (created with `chmod 600` at workspace startup). The server validates it against `erlang:get_cookie()`. This prevents unauthorized access on shared machines.

For standalone BEAM nodes (no workspace), the default `~/.erlang.cookie` is used.

## Message Format

### Request

```json
{
  "op": "eval",
  "id": "msg-001",
  "session": "alice",
  "code": "counter := Counter spawn"
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `op` | string | **yes** | Operation name |
| `id` | string | no | Client-generated correlation ID (echoed in response) |
| `session` | string | no | Session identifier (echoed in response) |
| *params* | varies | per-op | Operation-specific parameters (see below) |

### Response

```json
{
  "id": "msg-001",
  "session": "alice",
  "value": "#Actor<Counter,0.123.0>",
  "status": ["done"]
}
```

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Echoed from request (if provided) |
| `session` | string | Echoed from request (if provided) |
| `status` | string[] | Status flags: `["done"]` for final message. Absent on intermediate streaming messages. |
| `value` | any | Result value (for eval, clear, kill) |
| `output` | string | Captured stdout from evaluation (omitted if empty). Present in the final message for backward compatibility. Streaming clients should prefer `out` messages. |
| `out` | string | Streaming stdout chunk (BT-696). Sent as intermediate messages during eval, before the final `done` message. No `status` field. |
| `error` | string | Error message (when status includes `"error"`) |
| *result fields* | varies | Operation-specific fields (see below) |

### Status Flags

| Flag | Meaning |
|------|---------|
| `done` | Final message for this request |
| `error` | Request failed |
| `need-input` | Eval is waiting for stdin input (BT-698) |

A response with `status: ["done"]` is a successful final response.
A response with `status: ["done", "error"]` is an error final response.
A response with `status: ["need-input"]` means the eval is blocked waiting for user input (see `stdin` operation).

## Operations

### Core Operations

#### `eval` — Evaluate Expression

Evaluate a Beamtalk expression in the current session.

**Request:**
```json
{"op": "eval", "id": "msg-001", "code": "1 + 2"}
```

**Response (success, no stdout):**
```json
{"id": "msg-001", "value": 3, "status": ["done"]}
```

**Streaming response (with stdout output, BT-696):**

When the evaluated expression writes to stdout (e.g., via `Transcript show:`), the server sends intermediate `out` messages as output is produced, followed by a final `done` message:

```json
{"id": "msg-001", "out": "Hello, "}
{"id": "msg-001", "out": "world!\n"}
{"id": "msg-001", "value": "nil", "output": "Hello, world!\n", "status": ["done"]}
```

Intermediate `out` messages have no `status` field. The final `done` message still includes the full `output` field for backward compatibility with clients that don't support streaming.

If the expression produces no stdout, the response is a single message (unchanged from pre-streaming behavior).

**Response (error):**
```json
{"id": "msg-001", "error": "Undefined variable: foo", "status": ["done", "error"]}
```

**Interactive input (stdin, BT-698):**

When the evaluated expression calls `io:get_line` or similar input functions, the server sends a `need-input` status and the client can provide input via the `stdin` operation:

```json
Client → Server:  {"op": "eval", "id": "msg-001", "code": "name := Erlang io get_line: \"Name: \""}
Server → Client:  {"id": "msg-001", "status": ["need-input"], "prompt": "Name: "}
Client → Server:  {"op": "stdin", "id": "msg-001", "value": "Alice\n"}
Server → Client:  {"id": "msg-001", "value": "\"Alice\\n\"", "status": ["done"]}
```

The `need-input` message includes a `prompt` field with the prompt string from the IO request. The eval remains blocked until the client responds with a `stdin` operation or the server-side timeout (30 seconds) expires.

#### `stdin` — Provide Input (BT-698)

Provide stdin input to a running evaluation that has requested it via `need-input` status.

**Request:**
```json
{"op": "stdin", "id": "msg-001", "value": "user input\n"}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `value` | string | **yes** | The input text to provide. Use `"eof"` to signal end-of-file. |

**Behavior:**
- Only valid after receiving a `need-input` status for the same eval
- The eval resumes with the provided input
- Sending `"eof"` as the value signals end-of-file to the IO request
- If no eval is waiting for input, returns an error response
- Multiple `need-input` / `stdin` exchanges can occur during a single eval

#### `complete` — Autocompletion

Get completion suggestions for a partial expression.

**Request:**
```json
{"op": "complete", "id": "msg-002", "code": "Coun"}
```

**Response:**
```json
{"id": "msg-002", "completions": ["Counter"], "status": ["done"]}
```

#### `info` — Symbol Information

Get information about a symbol (class, method, variable).

**Request:**
```json
{"op": "info", "id": "msg-003", "symbol": "Counter"}
```

**Response:**
```json
{"id": "msg-003", "info": {"found": true, "symbol": "Counter", "kind": "class"}, "status": ["done"]}
```

#### `load-file` — Load Source File

Compile and load a `.bt` source file.

**Request:**
```json
{"op": "load-file", "id": "msg-004", "path": "examples/counter.bt"}
```

**Response:**
```json
{"id": "msg-004", "classes": ["Counter"], "status": ["done"]}
```

#### `load-source` — Load Inline Source

Compile and load Beamtalk source from an inline string (no file path needed). Used by the browser workspace Editor pane.

**Request:**
```json
{"op": "load-source", "id": "msg-005", "source": "Object subclass: Counter\n  state: count := 0\n  increment => self.count := self.count + 1\n  count => self.count"}
```

**Response:**
```json
{"id": "msg-005", "classes": ["Counter"], "status": ["done"]}
```

**Error (parse error):**
```json
{"id": "msg-005", "error": "Parse error: unexpected token...", "status": ["done", "error"]}
```

#### `reload` — Hot Reload Module

Reload a previously loaded module (requires `path` parameter).

**Request:**
```json
{"op": "reload", "id": "msg-005", "module": "Counter", "path": "examples/counter.bt"}
```

### Session Operations

#### `clear` — Clear Bindings

Clear all variable bindings in the current session.

**Request:**
```json
{"op": "clear", "id": "msg-010"}
```

**Response:**
```json
{"id": "msg-010", "value": "ok", "status": ["done"]}
```

#### `bindings` — Get Bindings

List current variable bindings.

**Request:**
```json
{"op": "bindings", "id": "msg-011"}
```

**Response:**
```json
{"id": "msg-011", "bindings": {"x": 42, "counter": "#Actor<Counter,0.123.0>"}, "status": ["done"]}
```

#### `sessions` — List Sessions

List all active REPL sessions in the workspace.

**Request:**
```json
{"op": "sessions", "id": "msg-012"}
```

**Response:**
```json
{"id": "msg-012", "sessions": [{"id": "<0.456.0>"}, {"id": "<0.789.0>"}], "status": ["done"]}
```

#### `clone` — Clone Session

Create a new session (returns the new session ID).

**Request:**
```json
{"op": "clone", "id": "msg-013"}
```

**Response:**
```json
{"id": "msg-013", "value": "session_1234_5678", "status": ["done"]}
```

#### `close` — Close Session

Close the current session and clean up resources.

**Request:**
```json
{"op": "close", "id": "msg-014"}
```

**Response:**
```json
{"id": "msg-014", "value": "ok", "status": ["done"]}
```

### Actor Operations

#### `actors` — List Actors

List all running actors in the workspace.

**Request:**
```json
{"op": "actors", "id": "msg-020"}
```

**Response:**
```json
{
  "id": "msg-020",
  "actors": [
    {"pid": "<0.123.0>", "class": "Counter", "module": "counter", "spawned_at": 1234567890}
  ],
  "status": ["done"]
}
```

#### `inspect` — Inspect Actor State

Get the internal state of an actor process.

**Request:**
```json
{"op": "inspect", "id": "msg-021", "actor": "<0.123.0>"}
```

**Response:**
```json
{"id": "msg-021", "state": {"value": 42}, "status": ["done"]}
```

#### `kill` — Kill Actor

Terminate an actor process.

**Request:**
```json
{"op": "kill", "id": "msg-022", "actor": "<0.123.0>"}
```

**Response:**
```json
{"id": "msg-022", "value": "ok", "status": ["done"]}
```

### Module Operations

#### `modules` — List Modules

List loaded modules in the workspace.

**Request:**
```json
{"op": "modules", "id": "msg-030"}
```

**Response:**
```json
{
  "id": "msg-030",
  "modules": [
    {"name": "Counter", "source_file": "counter.bt", "actor_count": 1, "load_time": 0, "time_ago": "2m ago"}
  ],
  "status": ["done"]
}
```

#### `unload` — Unload Module

Unload a module from the workspace. Uses `code:soft_purge/1` and `code:delete/1`.

**Request:**
```json
{"op": "unload", "id": "msg-031", "module": "Counter"}
```

**Response:**
```json
{"id": "msg-031", "status": ["done"]}
```

**Error (module not loaded):**
```json
{"id": "msg-031", "error": "module_not_loaded: Counter", "status": ["error"]}
```

### Server Operations

#### `describe` — Capability Discovery

Returns the list of supported operations with their parameters, protocol version, and server metadata. Enables tooling to dynamically discover server capabilities without hardcoding the op list. Requires no additional authorization beyond the normal cookie-authenticated connection.

**Request:**
```json
{"op": "describe", "id": "msg-040"}
```

**Response (truncated — showing a subset of ops):**
```json
{
  "id": "msg-040",
  "ops": {
    "eval": {"params": ["code"]},
    "complete": {"params": ["code"]},
    "info": {"params": ["symbol"]},
    "docs": {"params": ["class"], "optional": ["selector"]},
    "describe": {"params": []},
    "health": {"params": []}
  },
  "versions": {"protocol": "1.0", "beamtalk": "0.1.0"},
  "status": ["done"]
}
```

The actual response includes all supported operations (e.g., `eval`, `complete`, `info`, `docs`, `load-file`, `load-source`, `reload`, `clear`, `bindings`, `sessions`, `clone`, `close`, `actors`, `inspect`, `kill`, `interrupt`, `modules`, `unload`, `health`, `describe`, `shutdown`). Each entry lists required `params` and any `optional` parameters. The `versions` map includes the protocol version and the Beamtalk runtime version.

#### `health` — Health Probe

Returns workspace identity and a nonce for stale-workspace detection. Requires no additional authorization beyond the normal cookie-authenticated connection.

**Request:**
```json
{"op": "health", "id": "msg-041"}
```

**Response:**
```json
{"id": "msg-041", "workspace_id": "abc123", "nonce": "xyz789", "status": ["done"]}
```

#### `shutdown` — Graceful Shutdown

Initiates graceful OTP supervisor tree shutdown. Requires cookie authentication.

**Request:**
```json
{"op": "shutdown", "id": "msg-042", "cookie": "<node-cookie>"}
```

**Response (success):**
```json
{"id": "msg-042", "value": "ok", "status": ["done"]}
```

**Response (auth failure):**
```json
{"id": "msg-042", "error": "auth_error: Invalid cookie", "status": ["done", "error"]}
```

## Legacy Format (Backward Compatible)

The server also accepts the legacy format for backward compatibility:

**Legacy request:**
```json
{"type": "eval", "expression": "1 + 2"}
```

**Legacy response:**
```json
{"type": "result", "value": 3}
```

Legacy format is auto-detected by the presence of a `type` field instead of `op`.

| Legacy Type | New Op | Notes |
|-------------|--------|-------|
| `eval` | `eval` | `expression` → `code` |
| `clear` | `clear` | |
| `bindings` | `bindings` | |
| `load` | `load-file` | |
| `actors` | `actors` | |
| `modules` | `modules` | |
| `kill` | `kill` | `pid` → `actor` |
| `unload` | `unload` | |

## Error Handling

Errors are reported with structured messages:

```json
{
  "id": "msg-001",
  "error": "does_not_understand: Integer does not understand 'foo'.\n  Hint: Check spelling or use Integer methods to see available methods.",
  "status": ["done", "error"]
}
```

Error types include:
- **Parse errors** — Invalid Beamtalk syntax with source location
- **Runtime errors** — Exceptions during evaluation with formatted `#beamtalk_error{}` messages
- **Protocol errors** — Invalid requests, unknown operations
- **Timeout errors** — Evaluation exceeded time limit

## Implementation

The protocol is implemented in:

| File | Description |
|------|-------------|
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_protocol.erl` | Protocol encoder/decoder (incl. `output` field) |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl` | WebSocket server and request dispatch |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` | Expression evaluation and I/O capture |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_shell.erl` | Session state bridge |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | Rust CLI client |

## References

- [ADR 0004: Persistent Workspace Management](ADR/0004-persistent-workspace-management.md)
- [Clojure nREPL](https://nrepl.org/nrepl/design/overview.html)
- [Jupyter Kernel Protocol](https://jupyter-client.readthedocs.io/en/stable/messaging.html)
