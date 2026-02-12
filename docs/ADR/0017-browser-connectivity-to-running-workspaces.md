# ADR 0017: Browser Connectivity to Running Workspaces

## Status
Proposed (2026-02-11)

## Context

### Problem

Beamtalk is an **interactive-first** language (Principle 1), but the workspace is currently accessible only through the CLI REPL over a raw TCP connection bound to `127.0.0.1`. This creates two problems:

1. **No cross-platform access.** The compiler daemon uses Unix sockets (`~/.beamtalk/daemon.sock`), so Beamtalk cannot run on Windows at all. The entire pipeline — daemon startup, IPC, REPL connection — is Linux/macOS only. There is no near-term plan to port the Unix socket layer.

2. **No remote or browser-based access.** The workspace is a long-lived BEAM node (ADR 0004) that persists actors and state across REPL disconnects. But connecting to it requires a local TCP client. Users on remote machines, tablets, or different operating systems cannot interact with a running workspace.

A browser-based connection to the workspace would solve the remote access problem: users on any OS — including Windows — can connect via browser to a workspace running on Linux, in a dev container, or on a cloud VM. **Note:** this does not make Beamtalk run natively on Windows. The workspace and compiler daemon still require a Unix host. Browser connectivity enables *remote access* to a Unix-hosted workspace from any platform.

### Current Architecture

```
┌─────────────┐    TCP (JSON)    ┌──────────────────────┐   Unix Socket   ┌──────────────┐
│ CLI REPL    │ ──────────────→  │ beamtalk_repl_server │ ─────────────→  │ Compiler     │
│ (Rust)      │  localhost:9000  │ (Erlang/OTP)         │  daemon.sock    │ Daemon       │
└─────────────┘                  │                      │                 │ (Rust)       │
                                 │ ┌──────────────────┐ │                 └──────────────┘
                                 │ │ Session (shell)  │ │
                                 │ │ Session (shell)  │ │
                                 │ └──────────────────┘ │
                                 └──────────────────────┘
```

**What already exists (and is reusable):**
- JSON-over-TCP REPL protocol (`docs/repl-protocol.md`) — nREPL-inspired, designed for CLI/IDE/web clients
- Per-connection sessions with isolated bindings
- Operations: `eval`, `complete`, `info`, `load-file`, `reload`, `bindings`, `actors`, `inspect`, `kill`, `modules`, `sessions`
- JSX JSON library already in `rebar.config`
- `beamtalk_repl_protocol.erl` — encoder/decoder for the protocol
- `beamtalk_repl_shell.erl` — session gen_server (transport-agnostic)

**What's missing:**
- HTTP/WebSocket transport layer
- Static file serving for a web frontend
- Authentication for non-localhost access
- A browser frontend (HTML/JS)

### Constraints

1. **Cowboy is the natural choice** — it is the HTTP server underneath Phoenix/LiveView. Any work here feeds directly into the planned Phoenix LiveView IDE (`docs/beamtalk-ide.md`).
2. **The REPL protocol is transport-agnostic** — `beamtalk_repl_shell` speaks in terms of operations and bindings, not TCP frames. A WebSocket handler can reuse the same session infrastructure.
3. **Security model changes** — moving beyond localhost requires authentication. But Phase 1 can stay localhost-only (same security model as the TCP REPL).
4. **Minimal frontend** — the goal is workspace connectivity, not a full IDE. But even Phase 1 should feel like a Smalltalk workspace, not just a textarea.
5. **File loading model** — the CLI `:load` command sends filesystem paths to the server. Browser clients don't have filesystem access, so we need a `load-source` operation that accepts source code inline.

## Decision

Add a **Cowboy WebSocket transport** to the `beamtalk_workspace` OTP application, alongside the existing TCP transport. The WebSocket handler bridges browser connections to `beamtalk_repl_shell` sessions using the same JSON protocol.

The browser experience is a **multi-pane workspace** — not a single REPL textarea. Even in Phase 1, it should embody the Smalltalk principle: multiple tools (Workspace, Transcript, Inspector) looking at the same running system. Each pane connects to the same workspace over a shared WebSocket connection, and the Transcript streams output in real time via push messages.

### Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│ Browser                                                          │
│ ┌─────────────────────┐  ┌───────────────────────────────────┐  │
│ │ Workspace Pane      │  │ Transcript Pane                   │  │
│ │ (eval, bindings)    │  │ (live output stream)              │  │
│ ├─────────────────────┤  ├───────────────────────────────────┤  │
│ │ > c := Counter spawn│  │ 10:42:01 Hello, world!            │  │
│ │ #Actor<Counter,...> │  │ 10:42:03 Counter incremented      │  │
│ │ > c increment       │  │ 10:42:05 value is now 3           │  │
│ │ nil                 │  │                                   │  │
│ ├─────────────────────┤  └───────────────────────────────────┘  │
│ │ [Do It] [Print It]  │  ┌───────────────────────────────────┐  │
│ │ [Inspect It]        │  │ Inspector: Counter<0.150>         │  │
│ └─────────────────────┘  │ state: {value: 3}                 │  │
│                          │ class: Counter                    │  │
│ ┌─────────────────────┐  │ mailbox: (empty)                  │  │
│ │ File Editor Pane    │  │              [Refresh] [Kill] [X] │  │
│ │ (load-source)       │  └───────────────────────────────────┘  │
│ └─────────────────────┘                                         │
└──────────────────────────────────────────────────────────────────┘
         │ single WebSocket
         ▼
┌──────────────────────────────────────┐
│ beamtalk_workspace app               │
│                                      │
│ ┌──────────────────────────────────┐ │
│ │ beamtalk_web_handler.erl        │ │
│ │ (cowboy_websocket)              │ │
│ │                                  │ │
│ │ • request/response (eval, etc.) │ │
│ │ • push messages (transcript)    │ │
│ │ • Transcript subscriber         │ │
│ └────────────┬─────────────────────┘ │
│              │                       │
│ ┌────────────▼─────────────────────┐ │
│ │ beamtalk_repl_shell.erl         │ │   ┌──────────────┐
│ │ (shared session logic)          │ │   │ Compiler     │
│ └──────────────────────────────────┘ │   │ Daemon       │
│                                      │   │ (Rust)       │
│ ┌──────────────────────────────────┐ │   └──────────────┘
│ │ beamtalk_transcript_stream.erl  │ │
│ │ (ring buffer + subscribers)     │ │
│ └──────────────────────────────────┘ │
└──────────────────────────────────────┘
```

Both WebSocket and TCP transports share `beamtalk_repl_shell` — no session logic is duplicated.

### Key Design: Two Message Directions

Unlike the TCP REPL (pure request/response), WebSocket enables **server-initiated push messages**. This is critical for the Transcript:

```
Client → Server:  {"op": "eval", "id": "msg-1", "code": "Transcript show: 'hello'"}
Server → Client:  {"id": "msg-1", "value": "nil", "status": ["done"]}        ← response
Server → Client:  {"push": "transcript", "text": "hello"}                    ← push (async)
```

The WebSocket handler subscribes to the workspace's `Transcript` actor on connect. When any session (even another user's CLI session) writes to Transcript, the browser receives a push message. This is the Smalltalk Transcript experience — a **live shared log** visible to all connected tools.

**Note:** Transcript is a workspace singleton (ADR 0010). All sessions — browser and CLI — share the same Transcript. This is intentional and matches Smalltalk semantics, but will surprise web developers expecting per-session isolation.

**Push message types:**

| Push type | Payload | When | Phase |
|-----------|---------|------|-------|
| `transcript` | `{"push": "transcript", "text": "..."}` | Any `Transcript show:` in any session | 1 |
| `actor-spawned` | `{"push": "actor-spawned", "class": "Counter", "pid": "..."}` | Actor spawned in workspace | 2 (requires new registry notification infrastructure) |
| `actor-stopped` | `{"push": "actor-stopped", "pid": "..."}` | Actor stopped/crashed | 2 (requires new registry notification infrastructure) |

**Message ordering:** Push messages (Transcript) and request responses (`eval` result) are independent streams. A Transcript push from another session may arrive between an eval request and its response. The browser frontend must handle messages by type (`push` vs `id`-correlated response), not by arrival order.

### Session Lifecycle and Reconnection

WebSocket connections die on page refresh, tab close, or network interruption. For an interactive-first language, losing session state on refresh is unacceptable.

**Strategy: Session ID in URL + server-side session retention**

1. On first connect, server returns session ID in the WebSocket welcome message
2. Browser stores session ID in URL hash (`#session=abc123`) or `localStorage`
3. On reconnect, browser sends `{"op": "resume", "session": "abc123"}`
4. Server checks if session is still alive in `beamtalk_session_sup`; if yes, reattaches
5. If session expired, creates new session and returns fresh bindings

This leverages the existing session supervision tree — sessions already survive TCP disconnects (ADR 0004). The WebSocket handler just needs to support reattachment.

**Heartbeat:** Cowboy's `idle_timeout` option handles keepalive. Set to 60 seconds with WebSocket ping/pong frames. Dead connections are detected and cleaned up automatically.

### Compilation Dependency

**Important constraint:** Even with `load-source`, compilation goes through the daemon Unix socket (`~/.beamtalk/daemon.sock`). The browser user doesn't need a local daemon — the workspace's BEAM node connects to *its own* colocated daemon. But the workspace host must have both the BEAM runtime and the Rust compiler daemon running. This is the same setup as the CLI REPL, just accessed remotely.

### WebSocket Handler (Sketch)

```erlang
-module(beamtalk_web_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    %% Create a new REPL session (same as TCP connection)
    SessionId = beamtalk_repl_server:generate_session_id(),
    {ok, SessionPid} = beamtalk_session_sup:start_session(SessionId),
    %% Subscribe to Transcript for live output streaming
    beamtalk_transcript_stream:subscribe(self()),
    {ok, State#{session_id => SessionId, session_pid => SessionPid}}.

websocket_handle({text, Data}, #{session_pid := SessionPid} = State) ->
    case beamtalk_repl_protocol:decode(Data) of
        {ok, Msg} ->
            Response = beamtalk_repl_server:handle_protocol_request(
                         Msg, SessionPid),
            {[{text, Response}], State};
        {error, Reason} ->
            Error = beamtalk_repl_protocol:encode_error(
                      Reason, undefined, fun beamtalk_repl_server:term_to_json/1),
            {[{text, Error}], State}
    end;
websocket_handle(_Data, State) ->
    {[], State}.

%% Transcript push — arrives from beamtalk_transcript_stream subscriber
websocket_info({transcript_output, Text}, State) ->
    Push = jsx:encode(#{<<"push">> => <<"transcript">>,
                        <<"text">> => Text}),
    {[{text, Push}], State};

%% Actor lifecycle push (future: from actor registry)
websocket_info({actor_spawned, Class, Pid}, State) ->
    Push = jsx:encode(#{<<"push">> => <<"actor-spawned">>,
                        <<"class">> => atom_to_binary(Class, utf8),
                        <<"pid">> => list_to_binary(pid_to_list(Pid))}),
    {[{text, Push}], State};

websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Req, #{session_pid := SessionPid}) ->
    beamtalk_transcript_stream:unsubscribe(self()),
    beamtalk_repl_shell:stop(SessionPid),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.
```

### File Loading from the Browser: `load-source` Operation

The CLI `:load` command sends a **filesystem path** to the server:
```json
{"op": "load-file", "path": "examples/counter.bt"}
```

But browser clients don't have access to the server filesystem. We add a new `load-source` operation that accepts **source code as a string**:

```json
{"op": "load-source", "id": "msg-5",
 "source": "Actor subclass: Counter\n  state: value = 0\n  increment => ...",
 "name": "counter.bt"}
```

**Response:**
```json
{"id": "msg-5", "classes": ["Counter"], "status": ["done"]}
```

The `name` field is optional metadata (used for error messages and module tracking). The server compiles the source via the daemon exactly like `load-file`, but without reading from disk.

**Implementation:** The daemon's compile endpoint already accepts `{path, source}` params — `beamtalk_repl_eval:compile_file_via_daemon/4` takes source as a string. The new op simply skips `file:read_file/1` and passes the source directly.

This also enables a **file editor pane** in the browser UI — users can write class definitions in an editor panel and "Accept" them, which sends `load-source` to compile and hot-load the class.

### Static File Handler

```erlang
Dispatch = cowboy_router:compile([
    {'_', [
        {"/ws", beamtalk_web_handler, []},
        {"/", cowboy_static, {priv_file, beamtalk_workspace, "index.html"}},
        {"/[...]", cowboy_static, {priv_dir, beamtalk_workspace, "static"}}
    ]}
]),
cowboy:start_clear(beamtalk_http, [{port, 8080}],
    #{env => #{dispatch => Dispatch}}).
```

### CLI Integration

```bash
# Start workspace with web REPL enabled
beamtalk repl --web              # Opens browser to http://localhost:8080
beamtalk repl --web --port 9090  # Custom port
```

### The Multi-Pane Browser Experience

The frontend is a **single page with multiple panes**, each backed by the same WebSocket:

#### Workspace Pane (REPL)
Evaluates expressions, shows results inline. Supports Smalltalk-style actions:
- **Do It** (Ctrl+D) — evaluate, no output
- **Print It** (Ctrl+P) — evaluate, show result inline after selection
- **Inspect It** (Ctrl+I) — evaluate, open Inspector for the result

```
┌─────────────────────────────────────────────────────────┐
│  Workspace                                               │
│  ─────────────────────────────────────────────────────── │
│  counter := Counter spawn                                │
│  counter increment                                       │
│  counter increment                                       │
│  counter getValue await  ⟹ 2                            │
│                                                          │
│  counter state  ⟹ {value: 2}                            │
│                                                          │
│                              [Do It] [Print It] [Inspect]│
└─────────────────────────────────────────────────────────┘
```

Sends `{"op": "eval", "code": "..."}` over WebSocket.

#### Transcript Pane (Live Output Stream)
A **read-only log** showing all `Transcript show:` output from any session. This is the Smalltalk Transcript — shared across the entire workspace.

```
┌─────────────────────────────────────────────────────────┐
│  Transcript                                    [Clear]   │
│  ─────────────────────────────────────────────────────── │
│  10:42:01  Hello, world!                                 │
│  10:42:03  Counter incremented to 1                      │
│  10:42:05  Counter incremented to 2                      │
│  10:42:07  Connection from session "alice"                │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

Populated by `{"push": "transcript", "text": "..."}` push messages. The WebSocket handler subscribes to `beamtalk_transcript_stream` on connect, so output appears in real time — even when triggered by another user's CLI session or a background actor.

Can be **popped out into a separate browser window** for multi-monitor setups.

#### Inspector Pane
Opened via "Inspect It" or by clicking an actor in the Actors list. Shows live state of an actor.

```
┌─────────────────────────────────────────────────────────┐
│  Inspector: Counter<0.150>            [Refresh] [Kill]   │
│  ─────────────────────────────────────────────────────── │
│  Class:    Counter                                       │
│  State:                                                  │
│    value: 2                                              │
│  Mailbox: (empty)                                        │
│  Methods: increment, decrement, getValue, incrementBy:   │
└─────────────────────────────────────────────────────────┘
```

Sends `{"op": "inspect", "pid": "..."}` to get state. Can auto-refresh on a timer or via future PubSub events.

#### File Editor Pane
A code editor for writing class definitions. "Accept" compiles and hot-loads via `load-source`:

```
┌─────────────────────────────────────────────────────────┐
│  Editor: counter.bt                   [Accept] [Revert]  │
│  ─────────────────────────────────────────────────────── │
│  Actor subclass: Counter                                 │
│    state: value = 0                                      │
│                                                          │
│    increment =>                                          │
│      self.value := self.value + 1                        │
│      Transcript show: 'Counter incremented to '          │
│      Transcript show: self.value                         │
│                                                          │
│    getValue => self.value                                │
└─────────────────────────────────────────────────────────┘
```

On "Accept": sends `{"op": "load-source", "source": "...", "name": "counter.bt"}`. Result appears in Transcript. Running actors get the updated method table via hot reload.

### Pop-Out Windows

Any pane can be **detached into a separate browser window**. All windows share the same WebSocket connection (via `BroadcastChannel` or `SharedWorker`), so:
- Transcript in Window 2 sees output from Workspace in Window 1
- Inspector in Window 2 can inspect actors spawned in Window 1
- Editor in Window 3 can hot-reload code that running actors in Window 1 pick up

This mirrors the Smalltalk experience of having Workspace, Transcript, and Inspector as separate windows — but in the browser.

### Error Example

```
> foo
ERROR: Undefined variable: foo
  Hint: Did you mean to define a variable? Use := for assignment: foo := 42
```

## Prior Art

### Livebook (Elixir)
The closest prior art on BEAM. Livebook is a browser-based interactive notebook for Elixir:
- **Transport:** Phoenix LiveView (WebSocket under the hood via Phoenix Channels)
- **Architecture:** Phoenix app → attached/embedded BEAM runtime → code execution
- **Key insight:** Can attach to remote production nodes via Erlang distribution
- **What we adopt:** WebSocket-based browser connectivity to a running BEAM node
- **What we adapt:** Livebook is a notebook (cell-based); Beamtalk is a REPL/workspace (session-based with persistent bindings). We use the simpler WebSocket-per-session model rather than Phoenix Channels.

### Jupyter
Browser-based notebook with language-agnostic kernel protocol:
- **Transport:** HTTP REST for kernel management + WebSocket for execution (`/api/kernels/{id}/channels`)
- **Architecture:** Server manages kernels; all kernel channels multiplexed over one WebSocket
- **Key insight:** Single WebSocket per kernel, JSON message protocol
- **What we adopt:** The pattern of one WebSocket = one session with JSON messages
- **What differs:** Jupyter uses ZeroMQ internally; we have native BEAM processes

### Pharo/Squeak Smalltalk
Traditional Smalltalk uses a desktop image, not browser connectivity:
- **PharoJS:** Transpiles Smalltalk to JavaScript for browser execution (different goal)
- **Glamorous Toolkit:** Desktop IDE with moldable views, no web transport
- **Key insight:** The Smalltalk community has not solved browser-based live development well — this is an opportunity for Beamtalk to lead

### Try Haskell / Go Playground / Rust Playground
Simple web REPLs for other languages:
- **Architecture:** HTTP POST to server, server compiles/executes, returns result
- **Limitation:** Stateless — no persistent sessions, no actor inspection
- **What we adopt:** The simplicity of a minimal web UI
- **What differs:** Our sessions are stateful (bindings persist, actors survive)

## User Impact

### Newcomer (coming from Python/JS)
**Positive:** Can try Beamtalk without installing anything beyond a browser. Dev container + `beamtalk repl --web` gives instant access. This dramatically lowers the barrier to entry, especially for Windows users who currently cannot use Beamtalk at all.

### Smalltalk Developer
**Positive:** Browser-based workspace is a step toward the Smalltalk-style integrated environment. "Do It" and "Print It" in a browser echoes the Pharo workspace. The live, persistent session model (actors survive page refresh if workspace stays running) is closer to the image feel than a traditional CLI REPL.

### Erlang/BEAM Developer
**Positive:** Cowboy is the standard BEAM HTTP server — idiomatic choice. WebSocket maps naturally to one process per connection (OTP pattern). Reuses existing session infrastructure. They may also appreciate being able to `beamtalk repl --web` on a remote server and connect from their local browser.

### Production Operator
**Neutral for now:** Phase 1 is localhost-only (same security as TCP REPL). Future phases would need authentication for remote access. Cowboy is battle-tested in production (used by Phoenix, RabbitMQ management UI, etc.).

### Tooling Developer
**Positive:** WebSocket transport means any browser-based tool (Monaco editor, xterm.js terminal, custom dashboards) can connect to the workspace. The JSON protocol is already documented (`docs/repl-protocol.md`).

## Steelman Analysis

The steelman for each option presents the **strongest possible argument** from each cohort — the case that would be hardest to refute, even for advocates of the chosen approach.

### Option A: Cowboy WebSocket in BEAM (Recommended)

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Open a URL, start coding. No Rust toolchain, no Erlang install, no PATH configuration — the entire barrier to entry collapses to a browser tab." |
| 🎩 **Smalltalk purist** | "The Transcript streams live. Actors persist across page refreshes. 'Accept' hot-reloads running instances. This is the first browser-based environment that actually feels like a Smalltalk image — and Pharo never shipped one." |
| ⚙️ **BEAM veteran** | "One Cowboy process per WebSocket, supervised by OTP, dispatching to the same gen_server sessions as TCP. This is a textbook BEAM architecture — the WebSocket handler is 50 lines because the platform does all the heavy lifting." |
| 🏭 **Operator** | "Cowboy handles millions of connections in production at WhatsApp and RabbitMQ. We're adding one listener to an existing supervision tree — the risk surface is minimal and well-understood." |
| 🎨 **Language designer** | "Every line of Cowboy handler code survives into the Phoenix LiveView IDE. We're not building scaffolding — we're building the foundation." |

### Option B: Rust HTTP Proxy (axum/tower)

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "The Rust web ecosystem (axum, tower, tokio) is one of the best-documented in any language. Finding help on Stack Overflow or in tutorials is trivial compared to Cowboy." |
| 🎩 **Smalltalk purist** | "The transport is invisible to the user. Whether WebSocket terminates in Rust or Erlang, the workspace experience is identical — what matters is the protocol, not the plumbing." |
| ⚙️ **BEAM veteran** | "Zero Erlang dependency changes. The TCP REPL protocol stays frozen — the proxy is a separate concern that can be replaced, upgraded, or removed without touching runtime code." |
| 🏭 **Operator** | "Rust HTTP servers have predictable latency and memory usage. axum on tokio can handle WebSocket connections with microsecond overhead — the 'extra hop' is localhost loopback, which is effectively free." |
| 🎨 **Language designer** | "The compiler, CLI, LSP, and daemon are all in Rust. Adding the web gateway there means one language for all external interfaces — simpler hiring, simpler debugging, simpler CI." |

**Weakness:** The proxy can't receive push events from the BEAM (Transcript, actor lifecycle) without polling or a reverse channel. This fundamentally limits the live workspace experience. The "extra hop is free" argument breaks down for streaming — every Transcript line would need to traverse Erlang→TCP→Rust→WebSocket instead of Erlang→WebSocket directly.

### Option C: Jump to Phoenix LiveView

| Cohort | Strongest argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Livebook proved that Phoenix LiveView is the gold standard for browser-based BEAM experiences. Why build something worse and migrate later when the right answer is known?" |
| 🎩 **Smalltalk purist** | "LiveView's server-rendered DOM diffing is the closest thing to Morphic on the web. Inspector views that update in real-time without JavaScript — that's the Smalltalk dream, not a hand-rolled JS frontend." |
| ⚙️ **BEAM veteran** | "Phoenix Channels give us PubSub for free. Actor state changes, Transcript streaming, hot reload notifications — all solved by `Phoenix.PubSub.broadcast` instead of building our own notification infrastructure." |
| 🏭 **Operator** | "Phoenix has battle-tested authentication (Guardian/phx.gen.auth), CSRF protection, rate limiting, and HTTPS out of the box. Building auth for bare Cowboy is reinventing solved problems." |
| 🎨 **Language designer** | "Every week spent on a Cowboy+vanilla JS intermediate is a week not spent on the actual IDE. The intermediate doesn't teach us anything — we already know the protocol works over WebSocket." |

**Weakness:** Phoenix adds Elixir and Mix to the build chain — a significant dependency for a project that compiles Rust and Erlang today. The full IDE vision requires language features (method-level hot reload, breakpoints, supervision tree visualization) that don't exist yet. Building the IDE before the language is ready means building on shifting ground.

### Tension Points
- **Newcomers** are split between A (simplest to deploy) and C (best end-user experience)
- **Smalltalk purists** genuinely prefer C — LiveView's server-rendered model is philosophically closer to Morphic than any JS frontend. But A gets workspace feel *now* while C is months away.
- **BEAM veterans** recognize that A's Cowboy handler is throwaway-cheap (50 lines), so "wasted work" is negligible. The real question is whether the vanilla JS frontend in Phase 1-2 becomes legacy debt.
- **Operators** slightly prefer C for its built-in security stack, but acknowledge that localhost-only Phase 1 doesn't need auth yet.
- **Language designers** see A as correct sequencing: validate the workspace experience in the browser *before* committing to a framework. The frontend is a sketch, the protocol is the product — the WebSocket handler, push message format, `load-source` op, and Transcript subscription infrastructure are the durable decisions. The HTML/JS is the napkin drawing that gets thrown away when Phoenix arrives.

## Alternatives Considered

### Alternative: Rust HTTP Proxy (Option B)
Add an HTTP+WebSocket server in `beamtalk-cli` using axum or tower, proxying requests to the existing TCP REPL.

```
Browser ──WebSocket──→ Rust (axum) ──TCP──→ Erlang REPL
```

**Rejected because:**
- Extra network hop adds latency and complexity
- Cannot receive BEAM PubSub events (actor changes, hot reload) — would need a separate event stream
- Doesn't align with the Phoenix LiveView IDE path (which needs Cowboy in the BEAM node)
- Two HTTP servers to maintain (Rust for CLI web, Cowboy for Phoenix IDE later)

### Alternative: Phoenix LiveView Immediately (Option C)
Jump straight to the full IDE described in `docs/beamtalk-ide.md`.

**Rejected because:**
- Scope is months of work; we need browser access now
- Adds Elixir/Phoenix/npm as build dependencies
- Many IDE features depend on language features not yet implemented
- Cowboy is a subset of Phoenix — work done now is directly reusable

### Alternative: Embedded Browser (Tauri/Electron)
Ship a desktop app with an embedded browser instead of a web server.

**Rejected because:**
- Doesn't solve the Windows problem (still needs Unix sockets for daemon)
- Adds massive distribution complexity (native binaries per platform)
- Doesn't enable remote access
- Overkill for a REPL

### Alternative: xterm.js Terminal Emulator
Serve an xterm.js terminal in the browser that connects to the existing CLI REPL via WebSocket-to-TCP proxy.

**Rejected because:**
- Ships faster (days vs weeks) but provides no Smalltalk-style workspace features
- No Transcript pane, no Inspector, no structured eval results — just a raw terminal
- Cannot leverage push messages (Transcript, actor lifecycle) — it's a byte-stream terminal
- Doesn't build toward the Phoenix LiveView IDE — it's a dead end

### Alternative: Server-Sent Events (SSE) + HTTP POST
Use HTTP POST for eval commands and SSE for server-initiated push (Transcript, actor events).

**Rejected because:**
- SSE is unidirectional (server→client only); would need two separate connections (POST + SSE)
- WebSocket is a single bidirectional connection — simpler client code
- Phoenix LiveView uses WebSocket; SSE doesn't align with the migration path
- SSE has browser connection limits (6 per domain in HTTP/1.1)
- **Considered for:** simpler proxy scenarios, but not justified given Cowboy's native WebSocket support

## Consequences

### Positive
- **Remote access from any OS** — users on Windows, tablets, or remote machines connect via browser to a workspace running on Linux/macOS
- **Zero-install trial** — anyone with a browser can try Beamtalk if pointed at a running workspace
- **Remote development** — connect to workspaces on cloud VMs, dev containers, CI environments
- **Feeds into IDE vision** — Cowboy is Phoenix's HTTP layer; all handler code is reusable
- **Transcript streaming** — WebSocket push messages enable real-time Transcript, which TCP can't do
- **Pair programming** — multiple browser tabs can connect to the same workspace (different sessions, shared actors and Transcript)

### Negative
- **New dependency** — Cowboy added to `beamtalk_workspace` app
- **Two transports to maintain** — TCP (for CLI) and WebSocket (for browser). But they share all session logic.
- **Frontend maintenance** — HTML/JS needs updates as protocol evolves (keep minimal; disposable before Phoenix)
- **Security surface** — HTTP server is a new attack surface (mitigated by localhost-only in Phase 1)
- **Not native Windows** — browser access requires a Linux-hosted workspace; does not make Beamtalk run on Windows
- **Shared Transcript may surprise** — web developers expect per-session isolation; Beamtalk's shared Transcript is intentional but unfamiliar

### Neutral
- Existing TCP REPL continues to work unchanged
- CLI workflow is unaffected
- `load-file` (path-based) remains for CLI; `load-source` (inline) is additive
- Future Phoenix migration replaces Cowboy routing and HTML frontend but keeps WebSocket handler pattern

## Implementation

### Phase 0: Wire Check (Size: S)

Prove the WebSocket→eval→response round-trip works before building the full workspace UI. A single HTML file with a `<textarea>`, eval button, and Transcript `<div>` — the "napkin" that validates the protocol over WebSocket.

**Affected components:**

| Component | Change |
|-----------|--------|
| `runtime/rebar.config` | Add `cowboy` dependency |
| `runtime/apps/beamtalk_workspace/src/beamtalk_web_handler.erl` | New — Cowboy WebSocket handler with Transcript subscription |
| `runtime/apps/beamtalk_workspace/src/beamtalk_workspace_sup.erl` | Start Cowboy listener as child (optional, enabled by config) |
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_server.erl` | Extract `handle_protocol_request/2` to be callable from both TCP and WS handlers |
| `runtime/apps/beamtalk_runtime/src/beamtalk_transcript_stream.erl` | Add exported `subscribe/1` and `unsubscribe/1` convenience functions |
| `runtime/apps/beamtalk_workspace/priv/index.html` | New — minimal eval textarea + Transcript div (~100 lines) |

**Validates:**
- Cowboy starts alongside TCP REPL without conflicts
- JSON protocol works identically over WebSocket and TCP
- Transcript push messages arrive in real time
- `eval` round-trip latency is acceptable
- Session creation/cleanup on WebSocket connect/disconnect

**Does not include:** `load-source` op, Inspector, Editor, multi-pane layout, CLI `--web` flag. Those build on top once the wire works.

### Phase 1: WebSocket Workspace (Size: L)

Build the workspace experience on Phase 0's validated transport. The browser must feel like a Smalltalk workspace — Workspace pane, Transcript pane, File Editor with `load-source`, and Inspector.

**Affected components (beyond Phase 0):**

| Component | Change |
|-----------|--------|
| `runtime/apps/beamtalk_workspace/src/beamtalk_repl_eval.erl` | Add `handle_load_source/3` — compile from string, not file path |
| `runtime/apps/beamtalk_workspace/priv/index.html` | Expand to multi-pane workspace (Workspace, Transcript, Inspector, Editor) |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.js` | New — WebSocket client, pane management |
| `runtime/apps/beamtalk_workspace/priv/static/workspace.css` | New — pane layout styles |
| `crates/beamtalk-cli/src/commands/repl/mod.rs` | Add `--web` / `--web-port` flags |
| `docs/repl-protocol.md` | Document WebSocket transport, push messages, `load-source` op |

**Steps:**
1. Add `load-source` operation to `beamtalk_repl_server.erl` / `beamtalk_repl_eval.erl`
2. Build multi-pane `index.html` — Workspace, Transcript, Inspector, Editor panes
3. Add `--web` flag to CLI
4. Tests: load-source, Inspector via `inspect` op, error handling

### Phase 2: Polish + Reconnection (Size: M)

- Session reconnection via `resume` op + session ID in URL
- Syntax highlighting (CodeMirror)
- Keyboard shortcuts (Ctrl+D Do It, Ctrl+P Print It, Ctrl+I Inspect It)
- Tab completion in Workspace pane (using `complete` op)
- Command history (up/down arrow)
- Pop-out window support (BroadcastChannel for cross-window communication)

### Phase 3: Live Updates + Authentication (Size: L)

- Actor registry notification infrastructure (new: subscribe to spawn/stop events)
- `actor-spawned` and `actor-stopped` push messages
- Auto-refreshing Inspector
- Token-based authentication (workspace generates token on startup, printed to stdout)
- HTTPS/WSS support (TLS termination or reverse proxy guidance)
- Bind to `0.0.0.0` when `--web-remote` flag is used

### Phase 4: Phoenix LiveView IDE (Size: XL)

- Migrate to Phoenix (Cowboy handlers become Phoenix channels/LiveView)
- Full IDE from `docs/beamtalk-ide.md`
- Method editor with hot-reload
- Debugger integration (pause, inspect, resume)
- Message Timeline visualization
- PubSub integration for live actor state updates

**Note on Phoenix migration:** Phases 1-2 should keep frontend complexity minimal — avoid building a sophisticated JS SPA that gets rewritten in LiveView. The Cowboy WebSocket handler and protocol extensions carry over directly; the HTML/JS frontend is disposable.

## Migration Path

No migration needed — this is purely additive. The existing TCP REPL, CLI workflow, and protocol are unchanged. The `load-source` op (Phase 2) is a new operation alongside the existing `load-file` op.

## References
- Related ADRs: [ADR 0004 — Persistent Workspace Management](0004-persistent-workspace-management.md), [ADR 0009 — OTP Application Structure](0009-otp-application-structure.md), [ADR 0010 — Global Objects and Singleton Dispatch](0010-global-objects-and-singleton-dispatch.md)
- Documentation: [REPL Protocol](../repl-protocol.md), [IDE Vision](../beamtalk-ide.md), [Design Principles](../beamtalk-principles.md)
- Prior art: [Livebook](https://livebook.dev/), [Jupyter Kernel Gateway](https://jupyter-kernel-gateway.readthedocs.io/), [Cowboy WebSocket](https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_websocket/)
