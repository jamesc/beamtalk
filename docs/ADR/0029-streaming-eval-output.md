# ADR 0029: Streaming Eval Output

## Status
Implemented (2026-02-18)

## Context

### Problem

Beamtalk's REPL protocol currently uses a **buffer-then-send** model: when a client sends an `eval` request, the server captures all stdout during evaluation, then returns a single JSON response with the complete `output` field and the final `value`. This means users get **no feedback** until evaluation completes.

For short expressions (`1 + 2`), this is invisible. But for long-running evaluations — actor loops, collection processing, debugging with `Transcript show:` — the user stares at a blank screen for seconds or minutes before receiving a wall of output all at once.

This contradicts Beamtalk's core design principle: **"Feedback is immediate — no compile-deploy-restart cycle"** (Principle 1: Interactive-First).

### Current Architecture

```
Client sends:  {"op": "eval", "id": "msg-1", "code": "100 timesRepeat: [Transcript show: 'tick']"}
                    ↓
      beamtalk_repl_shell:eval/2  ← gen_server:call (30s timeout)
                    ↓
      spawn_monitor(worker)         ← already async internally (BT-666)
        start_io_capture()          ← redirects group_leader to buffer process
        apply(Module, eval, [Bindings])   ← runs code, all stdout → buffer
        stop_io_capture()           ← retrieves complete buffer
        Self ! {eval_result, ...}   ← sends result back to shell
                    ↓
      encode_result(Result, Output) ← ONE JSON message
                    ↓
Client receives: {"id": "msg-1", "value": "nil", "output": "tick\ntick\n...(100 lines)", "status": ["done"]}
```

Note: The eval worker is already spawned asynchronously via `spawn_monitor` (for interrupt support, BT-666). The `gen_server:call` blocks the *caller* but the eval itself runs in a separate process that sends results via message passing. This means the async infrastructure is partially in place — the key change is forwarding IO chunks during execution rather than buffering them.

Key bottlenecks:
1. **`io_capture_loop/1`** accumulates a binary buffer, only returned on `stop_io_capture()`
2. **`encode_result/3`** produces exactly one JSON message per eval — no intermediate messages are sent
3. The Transcript push channel (`{"push": "transcript", ...}`) is **workspace-global** — it broadcasts to all connections, not correlated to a specific eval request

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
| **Output chunk** | `id`, `out` | During eval, as stdout is produced (coalesced) |
| **Final result** | `id`, `value`, `status: ["done"]` | After eval completes |
| **Final error** | `id`, `error`, `status: ["done", "error"]` | After eval fails |

Rules:
- Messages with `status` containing `"done"` are **always the last message** for a given `id`
- The `output` field in the final message is **removed** — output is delivered incrementally via `out` messages
- Warnings, if present, appear only in the final message
- `out` messages for a given `id` are delivered **in order** — the client can concatenate them
- The `out`/`done` pattern mirrors nREPL conventions

### Output Coalescing

To prevent message explosion in tight loops (e.g., `10000 timesRepeat: [Transcript show: 'x']`), the IO stream process **coalesces output within a time window**:

- Output chunks arriving within **50ms** of each other are batched into a single `out` message
- A timer fires after 50ms of inactivity, flushing the accumulated buffer
- This means `10000 timesRepeat:` produces ~20 coalesced `out` messages (one per 50ms of wall time), not 10,000 individual messages
- The coalescing window is a server implementation detail, not a protocol guarantee

This matches nREPL's behavior, which also coalesces rapid output.

### Transcript Interaction

When `Transcript show:` is called during eval, **two things happen**:

1. The output is captured by the eval's group_leader → forwarded as an `out` message (correlated by request `id`)
2. The Transcript push fires independently → sends `{"push": "transcript", "text": "..."}` to all subscribers

These are separate channels serving different purposes:
- `out` messages tell the **originating client** what their eval produced
- Transcript push gives **all connected clients** workspace-wide visibility

Clients should **not deduplicate** — the `out` stream is the definitive eval output; Transcript push is a workspace-level notification. A client may choose to display only one or both.

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

**Adopted:** The `out`/`done` pattern, `id`-based correlation, and "done means last message" convention. Also adopted: nREPL's approach of coalescing rapid output before sending — the server batches output within a time window rather than sending per-`put_chars`.

### Jupyter Kernel Protocol

Jupyter uses a separate IOPub channel for streaming output. During cell execution, the kernel sends `stream` messages with `{"name": "stdout", "text": "..."}` over IOPub. The final result comes as `execute_result` on the shell channel.

**Adapted:** We use a single WebSocket channel (not separate channels) but the concept of streaming output messages alongside a final result is the same.

**Not adopted:** Jupyter's separate ZeroMQ channels — WebSocket message correlation via `id` is simpler and sufficient.

### Erlang Shell

The standard Erlang shell writes output directly to the group leader in real-time. There is no buffering — `io:format("~p~n", [X])` appears immediately. This is the expected behavior for any BEAM-based REPL.

**Key insight:** Beamtalk's current buffering is actually *worse* than the standard Erlang shell experience. However, the Erlang shell writes to a local terminal — there's no network protocol involved. Streaming over WebSocket introduces challenges (batching, ordering, backpressure) that the Erlang shell doesn't face. The goal is to approximate the immediacy of local IO over a network transport.

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

### Tension Points

- **BEAM veterans** would accept status quo (Alternative A) since Erlang's shell doesn't have this problem (it writes directly to the terminal). But Beamtalk operates over a network protocol, not a local terminal.
- **Newcomers and operators** strongly prefer streaming (the proposed decision) because it matches expectations from every other modern REPL (Jupyter, IPython, Node.js REPL).

Note: Routing all output through Transcript push was considered but is the wrong model. Transcript is a workspace pane — one of many future subscription channels (logging, actor announcements, message traces). Eval stdout (`out`) and workspace panes are orthogonal concerns: `out` captures everything the eval wrote to stdout (including `io:format`, logger output, etc.), not just `Transcript show:` calls.

## Alternatives Considered

### A: Keep Single-Message Response (Status Quo)

Keep the current buffer-then-send model. Output is only available after eval completes.

**Rejected because:** Contradicts the Interactive-First principle. For any eval taking more than ~200ms, the user gets no feedback. This is particularly painful for actor-based workflows where eval may trigger multiple message sends and Transcript writes.

### B: Client Opt-In Streaming

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
- E2E test harness (`e2e.rs`) assumes single-message-per-eval — `read_text()` returns the first non-push message, which would be an `out` chunk instead of the final result. Tests must be updated to filter for `status: ["done"]`
- The IO capture process gains a new responsibility (forwarding chunks vs buffering) plus coalescing logic
- Potential for high-frequency `out` messages if the coalescing window is too small — needs tuning

### Neutral
- Transcript push (ADR 0017) continues to work unchanged — it serves a different purpose (workspace-global visibility)
- The `output` field in single-message responses is removed in favor of `out` messages, but the information is equivalent
- Legacy protocol format is unaffected (it doesn't support `id` correlation anyway)

## Implementation

### Phase 0: Wire Check (S)

Minimal proof that streaming works end-to-end before building the full solution:

1. Modify `io_capture_loop` to forward a **single test chunk** mid-eval
2. WebSocket handler sends it as `{"id": "...", "out": "..."}` before the final result
3. Verify CLI receives both messages (manual test)
4. No coalescing, no batching — just prove the message path works

### Phase 1: Streaming IO Capture with Coalescing (Runtime) (M)

Modify `io_capture_loop/1` in `beamtalk_repl_eval.erl` to **forward output chunks** to a callback process with time-based coalescing:

```erlang
%% Current: accumulates buffer
io_capture_loop(Buffer) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            io_capture_loop(<<Buffer/binary, Chars/binary>>)
    end.

%% New: coalesces chunks within 50ms window, then forwards
io_stream_loop(Subscriber, Buffer) ->
    receive
        {io_request, From, ReplyAs, {put_chars, _, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            NewBuffer = <<Buffer/binary, (iolist_to_binary(Chars))/binary>>,
            io_stream_loop(Subscriber, NewBuffer)
    after 50 ->
        case Buffer of
            <<>> -> io_stream_loop(Subscriber, <<>>);
            _ ->
                Subscriber ! {eval_output, self(), Buffer},
                io_stream_loop(Subscriber, <<>>)
        end
    end.
```

### Phase 2: Server and Protocol Updates (M)

1. Add `out` message encoding to `beamtalk_repl_protocol.erl`
2. `beamtalk_repl_shell.erl` forwards `{eval_output, ...}` from worker to the calling process (leveraging existing `spawn_monitor` pattern)
3. `beamtalk_repl_server.erl` / `beamtalk_ws_handler.erl` send `out` messages during eval, correlated by request `id`

### Phase 3: Client Updates (M)

1. Update CLI (`protocol.rs`) to handle multi-message eval responses — loop until `status: ["done"]`, displaying `out` chunks immediately
2. Update browser workspace (`workspace.js`) to append streaming output to workspace pane
3. Update E2E test harness (`e2e.rs`) to filter for final result messages
4. Update `docs/repl-protocol.md` with new message types

### Affected Components

| Component | Change | Effort |
|-----------|--------|--------|
| `beamtalk_repl_eval.erl` | Stream IO chunks with coalescing | M |
| `beamtalk_repl_shell.erl` | Forward eval_output messages from worker | S |
| `beamtalk_repl_server.erl` | Forward streaming messages to transport | S |
| `beamtalk_ws_handler.erl` | Send `out` messages during eval | S |
| `beamtalk_repl_protocol.erl` | Encode `out` message type | S |
| `protocol.rs` (CLI) | Handle multi-message eval responses | M |
| `workspace.js` (browser) | Append streaming output to pane | S |
| `e2e.rs` (tests) | Update `read_text` to wait for `done` status | S |
| `docs/repl-protocol.md` | Document new message types | S |

## Migration Path

### Protocol Compatibility

- **Legacy clients** (using `type` field): Unaffected. Legacy format has no `id` correlation and the encoder checks `Msg#protocol_msg.legacy` — legacy responses continue to use the single-message `{"type": "result", ...}` format with no `out` messages.
- **Modern clients** (using `op` field): Must be updated to handle `out` messages before the final `done` message. Clients that skip all messages without a `status` field will continue to work but will lose streamed output.
- **E2E tests**: The `read_text()` function in `e2e.rs` must be updated to loop until it finds a message with `status: ["done"]`, collecting any `out` messages encountered along the way.

### Deprecation

The `output` field in eval responses is removed. Clients should read `out` messages during eval instead. Since `output` was only present when non-empty, clients that don't check for it are unaffected.

## References
- Related issues: BT-253 (Research: Formal REPL Message Protocol), BT-695 (Epic: nREPL-Inspired Protocol Enhancements), BT-696 (Streaming eval output implementation)
- Related ADRs: ADR 0009 (OTP Application Structure — REPL code in beamtalk_workspace), ADR 0017 (Browser Workspace — Transcript push), ADR 0020 (Connection Security)
- Documentation: `docs/repl-protocol.md`
- Prior art: [nREPL ops](https://nrepl.org/nrepl/ops.html), [Jupyter messaging](https://jupyter-client.readthedocs.io/en/latest/messaging.html)
- Design principle: [Principle 1 — Interactive-First](../beamtalk-principles.md)

### Future Extensions

- **`err` message type**: nREPL and Jupyter distinguish stdout (`out`) from stderr (`err`). Beamtalk currently has no stderr concept during eval (all IO goes through the group_leader). If a distinct error output channel is needed in the future (e.g., for OTP logger output during eval), an `err` message type can be added following the same pattern.
- **Rich output**: If Livebook-style rich outputs (images, structured data) are added (ADR 0017 Phase 3), a `display` message type can be introduced alongside `out`.
