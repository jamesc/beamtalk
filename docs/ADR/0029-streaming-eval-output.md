# ADR 0029: Streaming Eval Output

## Status
Proposed (2026-02-18)

## Context

### Problem

Beamtalk's REPL protocol currently uses a **buffer-then-send** model: when a client sends an `eval` request, the server captures all stdout during evaluation, then returns a single JSON response with the complete `output` field and the final `value`. This means users get **no feedback** until evaluation completes.

For short expressions (`1 + 2`), this is invisible. But for long-running evaluations — actor loops, collection processing, debugging with `Transcript show:` — the user stares at a blank screen for seconds or minutes before receiving a wall of output all at once.

This contradicts Beamtalk's core design principle: **"Feedback is immediate — no compile-deploy-restart cycle"** (Principle 1: Interactive-First).

### Current Architecture

```
Client sends:  {"op": "eval", "id": "msg-1", "code": "100 timesRepeat: [Transcript show: 'tick']"}
                    ↓
      beamtalk_repl_shell:eval/2  ← BLOCKING gen_server:call (30s timeout)
                    ↓
      Worker process:
        start_io_capture()          ← redirects group_leader to buffer process
        apply(Module, eval, [Bindings])   ← runs code, all stdout → buffer
        stop_io_capture()           ← retrieves complete buffer
                    ↓
      encode_result(Result, Output) ← ONE JSON message
                    ↓
Client receives: {"id": "msg-1", "value": "nil", "output": "tick\ntick\n...(100 lines)", "status": ["done"]}
```

Key bottlenecks:
1. **`gen_server:call/2`** blocks until eval completes (30s timeout)
2. **`io_capture_loop/1`** accumulates a binary buffer, only returned on `stop_io_capture()`
3. **`encode_result/3`** produces exactly one JSON message per eval
4. The Transcript push channel (`{"push": "transcript", ...}`) is **workspace-global** — it broadcasts to all connections, not correlated to a specific eval request

### Constraints

- **Backward compatibility**: Existing CLI and browser clients must continue to work without changes
- **Protocol simplicity**: The JSON-over-WebSocket protocol is intentionally simple; adding complexity must be justified
- **Transcript push coexistence**: Streaming eval output must not conflict with the existing Transcript push mechanism (ADR 0017)
- **BEAM idioms**: Solution should use standard OTP patterns (message passing, monitors), not exotic concurrency

## Decision

Change the eval response model from **single-message** to **multi-message streaming**. An `eval` request may produce zero or more `out` messages before a final `done` message, all correlated by the request `id`.

### Protocol Change

**Streaming eval response (new):**

```
Client → Server:
  {"op": "eval", "id": "msg-1", "code": "3 timesRepeat: [Transcript show: 'tick']"}

Server → Client (incremental, as output is produced):
  {"id": "msg-1", "out": "tick\n"}
  {"id": "msg-1", "out": "tick\n"}
  {"id": "msg-1", "out": "tick\n"}

Server → Client (final):
  {"id": "msg-1", "value": "nil", "status": ["done"]}
```

**Non-streaming eval (unchanged — still works):**

```
Client → Server:
  {"op": "eval", "id": "msg-2", "code": "1 + 2"}

Server → Client:
  {"id": "msg-2", "value": 3, "status": ["done"]}
```

If an eval produces no stdout, the response is identical to today: a single message with `value` and `status: ["done"]`. Clients that ignore messages without `status` will work unchanged.

### Message Types

| Message | Fields | When sent |
|---------|--------|-----------|
| **Output chunk** | `id`, `out` | During eval, as stdout is produced |
| **Error output** | `id`, `err` | During eval, as stderr is produced |
| **Final result** | `id`, `value`, `status: ["done"]` | After eval completes |
| **Final error** | `id`, `error`, `status: ["done", "error"]` | After eval fails |

Rules:
- Messages with `status` containing `"done"` are **always the last message** for a given `id`
- The `output` field in the final message is **removed** — output is delivered incrementally via `out` messages
- Warnings, if present, appear only in the final message
- The `out`/`err` split mirrors nREPL and Jupyter conventions

### REPL Session Example

```
beamtalk> 10 timesRepeat: [Transcript show: 'processing...']
processing...      ← appears immediately (streamed)
processing...      ← appears ~instantly after
processing...
...
=> nil              ← final result after all iterations
```

### Client Opt-In (Future)

A future enhancement could allow clients to opt into streaming via a request parameter:

```json
{"op": "eval", "id": "msg-1", "code": "...", "streaming": true}
```

This ADR does not require opt-in — streaming is the default behavior. Clients that only look for `status: ["done"]` messages are unaffected by intermediate `out` messages.

## Prior Art

### nREPL (Clojure)

nREPL uses multi-message responses for eval. During evaluation, the server sends `{:out "text\n"}` messages as stdout is produced, and `{:err "text\n"}` for stderr. The final message includes `{:value "result" :status #{:done}}`. All messages share the same `:id` for correlation.

**Adopted:** The `out`/`err`/`done` pattern, `id`-based correlation, and "done means last message" convention.

### Jupyter Kernel Protocol

Jupyter uses a separate IOPub channel for streaming output. During cell execution, the kernel sends `stream` messages with `{"name": "stdout", "text": "..."}` over IOPub. The final result comes as `execute_result` on the shell channel.

**Adapted:** We use a single WebSocket channel (not separate channels) but the concept of streaming output messages alongside a final result is the same.

**Not adopted:** Jupyter's separate ZeroMQ channels — WebSocket message correlation via `id` is simpler and sufficient.

### Erlang Shell

The standard Erlang shell writes output directly to the group leader in real-time. There is no buffering — `io:format("~p~n", [X])` appears immediately. This is the expected behavior for any BEAM-based REPL.

**Key insight:** Beamtalk's current buffering is actually *worse* than the standard Erlang shell experience. Streaming restores parity.

### Livebook (Elixir)

Livebook streams cell output in real-time via WebSocket. Each output chunk is sent as it's produced, with a final result message. Livebook also supports rich outputs (images, charts) via the same streaming mechanism.

**Adopted:** Real-time streaming of output during cell/eval execution.

## User Impact

### Newcomer (from Python/JS)
Streaming output matches their expectations — `print()` in Python and `console.log()` in JS produce immediate output. The current buffered behavior would be surprising and frustrating. This change makes the REPL feel responsive.

### Smalltalk Developer
In Pharo/Squeak, `Transcript show:` output appears immediately in the Transcript window. Beamtalk's current batched output is a regression from the Smalltalk experience. Streaming restores the expected live feedback.

### Erlang/BEAM Developer
Erlang's shell prints `io:format` output immediately via the group leader protocol. Streaming aligns with the BEAM convention. The implementation uses standard OTP patterns (message passing from IO capture process to WebSocket handler).

### Production Operator
Streaming output enables real-time monitoring of long-running operations without requiring separate logging infrastructure. Progress output (`Transcript show: 'Processing batch ' , i printString`) becomes immediately visible.

### Tooling Developer
Multi-message responses require clients to handle message correlation (matching `id` fields). However, the protocol remains simple JSON — no new framing or encoding. Clients that only care about the final result can filter for `status: ["done"]` messages.

## Steelman Analysis

### Alternative A: Keep Single-Message (Status Quo)

| Cohort | Best argument |
|--------|--------------|
| 🧑‍💻 **Newcomer** | "One request = one response is the simplest mental model. I don't need to write a state machine in my client." |
| 🎩 **Smalltalk purist** | "The Transcript push channel already provides live output. The eval response should be the *value*, not the output — Smalltalk's Transcript is separate from the return value." |
| ⚙️ **BEAM veteran** | "The current model is simpler to reason about for error handling and timeouts. One call, one response, done." |
| 🏭 **Operator** | "Single-message responses are easier to log, replay, and debug. Multi-message adds ordering concerns." |
| 🎨 **Language designer** | "Separation of concerns: eval returns values, Transcript streams output. Mixing them in the eval response conflates two different concerns." |

### Alternative B: Use Transcript Push for All Output

| Cohort | Best argument |
|--------|--------------|
| 🧑‍💻 **Newcomer** | "I only need to handle two kinds of messages: responses and pushes. No multi-message eval complexity." |
| 🎩 **Smalltalk purist** | "This IS the Smalltalk model! The Transcript is a workspace tool, not an eval artifact. Output goes to Transcript, values come from eval." |
| ⚙️ **BEAM veteran** | "Reuses existing infrastructure. No new message types, no changes to the eval path." |
| 🏭 **Operator** | "One channel for all output, one for results. Clean separation." |

### Tension Points

- **Smalltalk purists** would prefer Transcript-only output (Alternative B), arguing that eval should return values, not output streams. But this breaks when multiple sessions run concurrently — output from one session's eval appears in another session's Transcript, with no way to correlate it.
- **BEAM veterans** would accept status quo (Alternative A) since Erlang's shell doesn't have this problem (it writes directly to the terminal). But Beamtalk operates over a network protocol, not a local terminal.
- **Newcomers and operators** strongly prefer streaming (the proposed decision) because it matches expectations from every other modern REPL (Jupyter, IPython, Node.js REPL).

## Alternatives Considered

### A: Keep Single-Message Response (Status Quo)

Keep the current buffer-then-send model. Output is only available after eval completes.

**Rejected because:** Contradicts the Interactive-First principle. For any eval taking more than ~200ms, the user gets no feedback. This is particularly painful for actor-based workflows where eval may trigger multiple message sends and Transcript writes.

### B: Route All Output Through Transcript Push

Instead of adding `out` messages to eval responses, route all eval stdout through the existing Transcript push channel. The eval response only contains the final value.

```
Client → Server:  {"op": "eval", "id": "msg-1", "code": "Transcript show: 'hello'"}
Server → Client:  {"push": "transcript", "text": "hello"}     ← push (async)
Server → Client:  {"id": "msg-1", "value": "nil", "status": ["done"]}  ← eval result
```

**Rejected because:** Transcript push is workspace-global — all connected clients receive it. With multiple concurrent sessions, there's no way to correlate which eval produced which output. The `out` message with `id` correlation solves this.

**However:** Transcript push remains valuable for its original purpose — workspace-wide visibility. `Transcript show:` should continue to trigger both a Transcript push AND an `out` message on the originating eval. These are complementary, not competing.

### C: Client Opt-In Streaming

Only stream output when the client explicitly requests it via `{"op": "eval", "streaming": true, ...}`. Default to single-message for backward compatibility.

**Deferred, not rejected:** This is a reasonable approach for the transition period but adds protocol complexity. Since clients that ignore `out` messages already work correctly (they just wait for `status: ["done"]`), the opt-in mechanism is not needed initially. Can be added later if backward compatibility becomes a real concern.

## Consequences

### Positive
- Immediate feedback for long-running evaluations — aligns with Interactive-First principle
- Parity with nREPL, Jupyter, and Livebook developer experience
- Enables progress reporting for actor workflows (`Transcript show:` during spawn, message processing)
- Browser workspace (ADR 0017) can show live output without relying solely on Transcript push
- CLI can display output incrementally instead of buffering

### Negative
- Clients must handle multi-message responses (correlation by `id`)
- Testing becomes slightly more complex — must assert on message sequences, not single responses
- The IO capture process gains a new responsibility (forwarding chunks vs buffering)
- Potential for high-frequency `out` messages in tight loops — may need throttling/batching

### Neutral
- Transcript push (ADR 0017) continues to work unchanged — it serves a different purpose (workspace-global visibility)
- The `output` field in single-message responses is removed in favor of `out` messages, but the information is equivalent
- Legacy protocol format is unaffected (it doesn't support `id` correlation anyway)

## Implementation

### Phase 1: Streaming IO Capture (Runtime)

Modify `io_capture_loop/1` in `beamtalk_repl_eval.erl` to **forward output chunks** to a callback process instead of buffering:

```erlang
%% Current: accumulates buffer
io_capture_loop(Buffer) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            io_capture_loop(<<Buffer/binary, Chars/binary>>)
    end.

%% New: forwards chunks to subscriber
io_stream_loop(Subscriber) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            Subscriber ! {eval_output, self(), iolist_to_binary(Chars)},
            io_stream_loop(Subscriber)
    end.
```

### Phase 2: Async Eval Flow (Server)

Change `beamtalk_repl_server` eval handling from synchronous `gen_server:call` to async message passing:

1. Server spawns eval worker, monitors it
2. Worker sends `{eval_output, Ref, Text}` messages during execution
3. Worker sends `{eval_result, Ref, Result}` when complete
4. Server forwards each as a WebSocket message correlated by request `id`

### Phase 3: Protocol and Client Updates

1. Add `out`/`err` message encoding to `beamtalk_repl_protocol.erl`
2. Update CLI (`protocol.rs`) to handle multi-message eval responses
3. Update browser workspace (`workspace.js`) to append streaming output
4. Update `docs/repl-protocol.md` with new message types

### Affected Components

| Component | Change | Effort |
|-----------|--------|--------|
| `beamtalk_repl_eval.erl` | Stream IO chunks instead of buffering | M |
| `beamtalk_repl_shell.erl` | Async eval with message-based result | M |
| `beamtalk_repl_server.erl` | Forward streaming messages to transport | M |
| `beamtalk_ws_handler.erl` | Send `out` messages during eval | S |
| `beamtalk_repl_protocol.erl` | Encode `out`/`err` message types | S |
| `protocol.rs` (CLI) | Handle multi-message eval responses | M |
| `workspace.js` (browser) | Append streaming output to pane | S |
| `docs/repl-protocol.md` | Document new message types | S |

## References
- Related issues: BT-253 (Research: Formal REPL Message Protocol)
- Related ADRs: ADR 0017 (Browser Workspace — Transcript push), ADR 0020 (Connection Security)
- Documentation: `docs/repl-protocol.md`
- Prior art: [nREPL ops](https://nrepl.org/nrepl/ops.html), [Jupyter messaging](https://jupyter-client.readthedocs.io/en/latest/messaging.html)
- Design principle: [Principle 1 — Interactive-First](../beamtalk-principles.md)
