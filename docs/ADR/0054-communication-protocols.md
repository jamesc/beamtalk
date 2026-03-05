# ADR 0054: Communication Protocols

## Status
Implemented (2026-03-05)

## Context

The Beamtalk system comprises several components written in different languages (Rust CLI, Erlang/OTP runtime, browser frontend) that must communicate across process and language boundaries. Over the course of development, three distinct communication protocols emerged, each chosen to fit a specific boundary:

| Boundary | Components | Transport |
|----------|-----------|-----------|
| **Client ↔ Workspace** | CLI, browser, IDE ↔ REPL backend | WebSocket + JSON |
| **Workspace ↔ Compiler** | Erlang runtime ↔ Rust compiler | OTP Port + ETF |
| **Actor ↔ Actor** | Beamtalk actor processes | BEAM `gen_server` calls/casts |

The architectural choice to use different protocols for different channels — rather than a single universal protocol — has never been formally recorded. This ADR documents the rationale.

### Why Not a Single Protocol?

Each boundary has fundamentally different constraints:

- **Client ↔ Workspace** crosses a network boundary (even on localhost) and must support browser clients that can only speak HTTP/WebSocket. JSON is the natural format for browser interop.
- **Workspace ↔ Compiler** crosses a language boundary (Erlang ↔ Rust) within the same machine. Performance matters for REPL responsiveness. ETF (Erlang Term Format) is native on the Erlang side and avoids JSON serialization overhead.
- **Actor ↔ Actor** stays entirely within the BEAM VM. Native Erlang process messages are zero-copy, and `gen_server` provides the supervision, timeout, and back-pressure semantics that actors need.

A single protocol (e.g., JSON-RPC everywhere) would impose unnecessary overhead at the tighter boundaries and lose native guarantees at the loosest one.

## Decision

Use three protocols, each fit-for-purpose at its boundary.

### 1. Client ↔ Workspace: WebSocket + JSON

**Transport:** WebSocket (RFC 6455) over TCP, `ws://127.0.0.1:{port}/ws`
**Format:** JSON, one message per WebSocket text frame
**Security:** Cookie-based authentication handshake (ADR 0020)
**Reference:** `docs/repl-protocol.md` (authoritative)

The REPL protocol is inspired by [nREPL](https://nrepl.org/) and [Jupyter](https://jupyter.org/). It uses an operation-based message format:

Request:
```json
{"op": "eval", "id": "msg-001", "code": "1 + 2"}
```

Response:
```json
{"id": "msg-001", "value": 3, "status": ["done"]}
```

**Why WebSocket + JSON:**
- Browser clients can only communicate via HTTP/WebSocket — raw TCP is not available from JavaScript
- JSON is human-readable, simplifying debugging and tooling development
- WebSocket provides full-duplex communication for streaming output and push notifications (Transcript, actor lifecycle events)
- HTTP upgrade is compatible with standard reverse proxies (Caddy, nginx) for remote access
- nREPL/Jupyter-style operation semantics map cleanly to JSON objects

**Key features:**
- Session isolation (per-connection bindings)
- Streaming eval output via intermediate `out` messages (BT-696)
- Interactive stdin support via `need-input` / `stdin` exchange (BT-698)
- Push messages for Transcript output and actor lifecycle events (BT-690)
- Capability discovery via `describe` operation
- Legacy format auto-detection for backward compatibility

**Evolution:** The REPL protocol originally used raw TCP with newline-delimited JSON. ADR 0020 migrated to WebSocket for browser compatibility and cookie authentication. The operation semantics are unchanged.

### 2. Workspace ↔ Compiler: OTP Port + ETF

**Transport:** OTP Port (stdin/stdout), 4-byte length-prefixed frames (`{packet, 4}`)
**Format:** Erlang External Term Format (ETF)
**Reference:** ADR 0022 (authoritative)

The Rust compiler runs as a standalone executable managed by an OTP supervisor via `open_port/2`. The Erlang side uses `term_to_binary/1` and `binary_to_term/1`; the Rust side uses an ETF library for encode/decode.

```erlang
%% Compile a REPL expression — direct function call
KnownVars = [atom_to_binary(V) || V <- maps:keys(Bindings)],
beamtalk_compiler:compile_expression(Expression, ModuleName, KnownVars).
```

**Why OTP Port + ETF:**
- Zero serialization library needed on the Erlang side (`term_to_binary` is built-in)
- Native support for Erlang maps, binaries, atoms — no lossy JSON round-trip
- Port crash is isolated: the OTP supervisor restarts the compiler automatically without losing actor state or REPL sessions
- stdin/stdout transport works on all platforms (Linux, macOS, Windows) — unlike the previous Unix socket daemon
- ~2ms overhead per call, negligible vs 10-500ms compilation time

**Evolution:** Originally the compiler ran as a separate daemon process communicating via JSON-RPC over Unix domain sockets. ADR 0022 replaced this with an embedded OTP Port, eliminating daemon lifecycle management, stale socket files, and JSON serialization overhead. The daemon code was fully removed in Phase 5.

### 3. Actor ↔ Actor: BEAM Process Messages

**Transport:** BEAM process messages via `gen_server`
**Format:** Erlang terms
**Reference:** `docs/internal/beamtalk-protocols.md`

Every Beamtalk actor is a BEAM process running a `gen_server`. Message sends compile to synchronous calls or asynchronous casts:

```erlang
%% Sync: blocks until result
Result = gen_server:call(ActorPid, {increment, []})

%% Async: returns a future immediately
FuturePid = beamtalk_future:new(),
gen_server:cast(ActorPid, {increment, [], FuturePid})
```

**Message format:**
- Sync: `{Selector, Args}` — dispatched via `handle_call`, blocks until result
- Async: `{Selector, Args, FuturePid}` — dispatched via `handle_cast`, result delivered to future
- Cast (fire-and-forget): `{cast, Selector, Args}` — dispatched via `handle_cast`, result discarded (used by the `!` bang operator)

**Why BEAM process messages:**
- No serialization overhead — Erlang terms stay in native format (small terms are copied between process heaps; large binaries are reference-counted and shared)
- `gen_server` provides supervision, timeouts, back-pressure, and OTP tooling compatibility (`observer`, `sys:get_state`)
- Location transparency — the same message protocol works for local and distributed (cross-node) actor communication
- `doesNotUnderstand:args:` handler enables proxy and delegation patterns (Smalltalk DNU)

**Future resolution** is a sub-protocol within this boundary. Futures are lightweight BEAM processes that transition through `pending → resolved | rejected` states. Waiters register via `{await, Pid}` messages and receive `{future_resolved, FuturePid, Value}` or `{future_rejected, FuturePid, Reason}` notifications. Futures self-terminate after 5 minutes of inactivity.

## Prior Art

### Smalltalk (Pharo, Squeak)
Everything runs in a single image — there are no protocol boundaries between the compiler, runtime, and UI. Message passing between objects is the only "protocol." Beamtalk differs because the compiler is Rust (not Beamtalk) and clients can be external processes (CLI, browser).

### Erlang/OTP
OTP uses multiple protocols by design: distribution protocol for cross-node messaging, Ports for native code integration, and `gen_server` calls/casts for intra-node communication. Beamtalk follows this convention directly. The OTP Port pattern for the compiler aligns with how Erlang itself handles native code (`heart`, `epmd`, `inet_gethost`).

### Elixir + Phoenix
Phoenix uses WebSocket channels for browser communication and native BEAM messaging for backend processes — the same split as Beamtalk. LiveView's server-rendered approach influenced the Workspace UI design (ADR 0017).

### Jupyter
Jupyter's kernel protocol uses ZeroMQ with JSON messages over multiple channels (shell, IOPub, stdin). Beamtalk's REPL protocol is simpler (single WebSocket connection, multiplexed operations) but drew inspiration from Jupyter's operation semantics and streaming output model.

### Language Server Protocol (LSP)
LSP uses JSON-RPC over stdio or TCP. Beamtalk's LSP integration uses standard LSP; the REPL protocol is separate because LSP's request/response model doesn't support streaming eval output, push notifications, or interactive stdin.

## User Impact

### Newcomer
The protocol boundaries are invisible. The CLI connects to the workspace, types expressions, gets results. The browser opens a URL and provides the same experience. No protocol knowledge is needed.

### Smalltalk Developer
Actor messaging via `gen_server` preserves the message-passing semantics they expect. `doesNotUnderstand:args:` works as in Smalltalk. The fact that the compiler uses a different protocol (Port + ETF) is an implementation detail hidden behind `beamtalk_compiler`.

### Erlang/BEAM Developer
All three protocols use standard BEAM patterns: WebSocket via Cowboy, OTP Port for native code, `gen_server` for process communication. They can inspect any layer with standard tools (`observer`, `sys:get_state`, `dbg`).

### Production Operator
One BEAM node to monitor. The compiler port is supervised and auto-restarts on crash. WebSocket connections are authenticated via cookie handshake. Actor messaging stays within the VM — no network exposure. Standard BEAM monitoring tools work at every layer.

### Tooling Developer (LSP, IDE)
The REPL protocol's `describe` operation enables dynamic capability discovery. The JSON format is easy to parse in any language. Push messages (Transcript, actor events) enable real-time UI updates. The `show-codegen` operation supports debugging and learning tools.

## Steelman Analysis

### For a Single Protocol (Rejected)

- **Newcomer**: "One protocol to learn. If I understand the REPL protocol, I understand the whole system. Three protocols means three sets of error handling, three message formats to debug."
- **Smalltalk purist**: "In Pharo, everything is message passing. One protocol would be truer to the Smalltalk ideal of uniform communication."
- **BEAM veteran**: "JSON-RPC is a solved problem with excellent tooling. The 2ms ETF advantage is irrelevant when compilation takes 10-500ms."
- **Operator**: "One protocol means one monitoring approach, one set of metrics, one failure mode to understand."
- **Language designer**: "Protocol uniformity is an architectural virtue. Three protocols is three times the API surface to version, document, and maintain."

**Response:** The simplicity argument is valid for external-facing protocols, but the compiler boundary requires ETF for crash isolation (Port process model) and the actor boundary requires native BEAM messaging for zero-overhead dispatch. Forcing JSON through these boundaries would add latency, lose type fidelity (atoms, binaries), and sacrifice OTP supervision guarantees. The complexity of three protocols is hidden behind clean APIs — users interact with `beamtalk_compiler:compile/2` and `gen_server:call/2`, not raw protocol frames.

### For Unix Sockets Instead of WebSocket (Rejected)

- **Security engineer**: "Unix sockets provide filesystem-based access control. No cookie handshake needed — just `chmod 600` the socket file."
- **BEAM veteran**: "Unix sockets are simpler than WebSocket. No HTTP upgrade, no framing, no Cowboy dependency."
- **Operator**: "Unix sockets eliminate an entire class of network attacks. Loopback TCP is visible to any local process; socket files are protected by filesystem permissions."

**Response:** Unix sockets don't work on Windows (ADR 0027), can't be accessed from browsers (ADR 0017), and are incompatible with standard HTTP reverse proxies for remote access (ADR 0020). The cookie handshake provides equivalent authentication to filesystem permissions on shared machines.

### Tension Points
- Security purists prefer Unix sockets for filesystem ACLs, but cross-platform and browser requirements rule them out
- Performance purists would prefer ETF everywhere, but browser clients can't speak ETF
- Simplicity advocates would prefer one protocol, but the three boundaries have genuinely different constraints
- Operators would prefer one protocol for monitoring uniformity, but the API abstraction layer makes the protocol diversity largely invisible

## Alternatives Considered

### Alternative A: JSON-RPC Everywhere
Use JSON-RPC 2.0 for all three boundaries: client ↔ workspace (WebSocket), workspace ↔ compiler (Unix socket or TCP), and a JSON-based actor messaging layer.

**Rejected because:**
- JSON-RPC over Unix sockets for the compiler was the original design (pre-ADR 0022). It was replaced by OTP Port + ETF for crash isolation, Windows compatibility, and elimination of daemon lifecycle complexity.
- JSON-based actor messaging would add serialization overhead to every message dispatch (currently zero-cost) and lose native Erlang term fidelity.
- JSON cannot represent Erlang atoms, PIDs, or references without lossy encoding.

### Alternative B: ETF Everywhere
Use Erlang Term Format for all communication, including client ↔ workspace.

**Rejected because:**
- Browsers cannot natively encode/decode ETF — would require a JavaScript ETF library
- ETF is a binary format — not human-readable for debugging
- Standard web tooling (browser DevTools, curl, Postman) cannot inspect ETF messages
- JSON is the lingua franca for web APIs; ETF creates an unnecessary barrier for tooling developers

### Alternative C: gRPC / Protocol Buffers
Use gRPC for structured, typed communication across all boundaries.

**Rejected because:**
- Adds a schema definition and code generation step to the build
- Protobuf has no native Erlang term support (atoms, PIDs) — same lossy encoding as JSON
- gRPC requires HTTP/2, adding infrastructure complexity
- No meaningful advantage over ETF for the compiler boundary or BEAM messaging for actors
- Browser gRPC support (gRPC-Web) requires a proxy

## Consequences

### Positive
- **Fit-for-purpose:** Each protocol matches the constraints of its boundary — JSON for browser interop, ETF for Erlang-native performance, BEAM messaging for zero-overhead actor dispatch
- **Standard tooling:** WebSocket is debuggable with browser DevTools, OTP Ports are monitorable with `observer`, `gen_server` works with `sys:get_state` and `dbg`
- **Crash isolation:** Compiler crashes (Port) don't affect actor state; actor crashes don't affect the REPL; client disconnects don't affect the workspace
- **Cross-platform:** All three protocols work on Linux, macOS, and Windows

### Negative
- **Three protocols to document and maintain:** Each has its own message format, error handling, and versioning concerns
- **No single client library:** A tool that spans boundaries (e.g., a test runner that compiles and evaluates) must use multiple protocols
- **Conceptual overhead:** New contributors must understand which protocol applies at which boundary

### Neutral
- **The protocols are largely hidden behind APIs:** Users call `beamtalk_compiler:compile/2` (not Port ETF) and `gen_server:call/2` (not raw messages). Only the REPL protocol is directly visible to external clients.
- **Future evolution:** If the compiler is rewritten in Beamtalk (self-hosting), the OTP Port boundary collapses into direct BEAM messaging. The `beamtalk_compiler` API remains unchanged — only the backend dispatch changes.

## Implementation

This ADR records existing decisions. All protocols are implemented:

| Protocol | Implementation | ADR |
|----------|---------------|-----|
| Client ↔ Workspace (WebSocket + JSON) | `beamtalk_repl_server.erl`, `beamtalk_repl_protocol.erl`, `beamtalk_ws_handler.erl` | ADR 0020 (security), ADR 0017 (browser), ADR 0029 (streaming) |
| Workspace ↔ Compiler (OTP Port + ETF) | `beamtalk_compiler.erl`, `beamtalk_compiler_server.erl`, `beamtalk_compiler_port.erl` | ADR 0022 |
| Actor ↔ Actor (BEAM gen_server) | `beamtalk_actor.erl`, `beamtalk_future.erl` | ADR 0005 (object model), ADR 0043 (sync-by-default) |

No code changes are required.

## References
- Related issues: [BT-306](https://linear.app/beamtalk/issue/BT-306/adr-communication-protocols)
- Related ADRs:
  - [ADR 0004](0004-persistent-workspace-management.md) — Persistent Workspace Management
  - [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) — BEAM Object Model
  - [ADR 0017](0017-browser-connectivity-to-running-workspaces.md) — Browser Connectivity
  - [ADR 0020](0020-connection-security.md) — Connection Security
  - [ADR 0022](0022-embedded-compiler-via-otp-port.md) — Embedded Compiler via OTP Port
  - [ADR 0029](0029-streaming-eval-output.md) — Streaming Eval Output
  - [ADR 0043](0043-sync-by-default-actor-messaging.md) — Sync-by-Default Actor Messaging
- Documentation:
  - `docs/repl-protocol.md` — REPL protocol specification
  - `docs/internal/beamtalk-protocols.md` — Internal protocol details (actor messaging, future resolution)
