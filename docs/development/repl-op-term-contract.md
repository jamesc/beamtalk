# REPL Op Term Contract

**Status:** Active · **Introduced:** BT-2399 · **Context:** ADR 0017 Phase 3
(Browser Connectivity), ADR 0085 (read-surface), ADR 0082 (write-surface)

This is the cross-surface API boundary for the curated REPL op layer. It lets
the **browser** (WebSocket), **Phoenix LiveView** (Erlang distribution / Attach
topology), and runtime-attached **LSP / MCP** clients share one op contract,
while only the WebSocket edge pays the JSON cost.

## Why terms, not JSON

The op layer (`beamtalk_repl_server:handle_protocol_request/2` →
`beamtalk_repl_ops:dispatch/4`) does the dispatch, validation, and
error-wrapping every surface needs. Historically it also encoded JSON inline,
which makes it the JSON boundary.

Over Erlang distribution, JSON is both overhead and **lossy in the way that
matters**: `Counter spawn` is a live term —
`{beamtalk_object, 'Counter', bt@counter, <0.369.0>}` carrying a real,
messageable remote pid — that JSON flattens to the dead string
`"#Actor<Counter,…>"`. An inspector, "send a message to this object", or
reference-following all need the term, not its `printString`. That liveness is
the whole point of a live-image IDE.

So the op layer is split:

```text
                       ┌─ Phoenix / LSP / MCP (dist): consume terms directly
  dispatch/4 ─ term ───┤
                       └─ encode/2 ─ JSON ─ WebSocket (browser)
```

* `beamtalk_repl_ops:dispatch/4` returns a structured **term** (`op_result()`),
  never JSON.
* `beamtalk_repl_ops:encode/2` encodes that term to the protocol JSON binary
  at the **WebSocket transport edge only**. The browser wire format is
  unchanged.

## The `op_result()` contract

`op_result()` is a small, deliberately stable tagged union — not raw internal
shapes — so the contract does not leak compiler/runtime internals. Phoenix
matches on the `beamtalk_error` record tag, so no shared `.hrl` is required.

| Tag | Shape | Produced by |
|-----|-------|-------------|
| result | `{ok, Value, Output, Warnings}` | `eval` |
| trace | `{trace, Steps, Output, Warnings}` | `eval` (trace mode) |
| actors | `{actors, [ActorMeta]}` | `actors` |
| inspect | `{inspect, map() \| binary()}` | `inspect` |
| status | `{status, ok}` | `kill`, `interrupt` |
| error | `{error, #beamtalk_error{}}` | any op |
| error+io | `{error, #beamtalk_error{}, Output, Warnings}` | `eval` |
| json | `{json, binary()}` | ops not yet ported (see below) |

* `Value` and the `inspect` payload are **live terms** — over distribution they
  retain messageable pids and structure; only `encode/2` flattens them.
* `Output` is captured stdout (`binary()`); `Warnings` is `[binary()]`.
* Errors are the structured `#beamtalk_error{}` record (tag `beamtalk_error`),
  the same record used everywhere else in the runtime for user-facing errors.

## Porting status

`eval` (the canonical core path) and the actor read-surface ops (`actors`,
`inspect`, `kill`, `interrupt`) return native term shapes today.

The remaining ops — `load-source` / `load-project` / `unload`,
`sessions` / `clone` / `close` / `health` / `shutdown`,
`complete` / `describe` / `methods` / `list-classes` / `show-codegen` /
`test` / `test-all` / `erlang-help` / `erlang-complete`, the tracing ops, and
`nav-query` / `nav-symbols` — are still JSON-internally and are surfaced
through the `{json, Binary}` escape tag, so every op still flows through the one
`dispatch/4` → `encode/2` seam with no wire-format change. Porting those to
native term shapes is incremental follow-up work for the LiveView IDE epic
(read-surface ADR 0085, write-surface ADR 0082).

## Live push streams: the subscription facade

`dispatch/4` covers request/response ops. The workspace also pushes **live
streams**. These are owned by `beamtalk_repl_subscriptions`, the stable
subscription facade — rather than casting `{subscribe, self()}` tuples at the
underlying gen_servers, a client calls the facade and the **calling process**
becomes the subscriber. Over distribution a LiveView process subscribes its own
location-transparent pid and receives the push messages natively.

| Stream | Push message to the subscriber |
|--------|--------------------------------|
| `transcript` | `{transcript_output, Text :: binary()}` |
| `actors` | `{actor_spawned, Meta}` / `{actor_stopped, StopInfo}` |
| `classes` | `{class_loaded, ClassName :: atom()}` |
| `bindings` | `{bindings_changed, SessionId :: binary()}` |
| `flush` | `{flush_completed, Files :: [binary()]}` |

```erlang
%% Subscribe the calling process to everything the browser sees:
beamtalk_repl_subscriptions:subscribe_all().
%% …or a single stream:
beamtalk_repl_subscriptions:subscribe(transcript).
```

The browser edge (`beamtalk_ws_handler`) re-encodes these push messages to JSON
push frames; dist clients consume the messages directly.

## Where this lives

| Concern | Module |
|---------|--------|
| Dispatch + term contract + JSON edge | `beamtalk_repl_ops` |
| `eval` term handler | `beamtalk_repl_ops_eval:handle_term/4` |
| Actor read-surface term handlers | `beamtalk_repl_ops_actors:handle_term/4` |
| Subscription facade | `beamtalk_repl_subscriptions` |
| WebSocket transport edge | `beamtalk_repl_server:handle_op/4`, `beamtalk_ws_handler` |

## References

* `docs/research/phoenix-topology-spike.md` — "API shape: return terms, encode
  JSON only at the WebSocket edge".
* ADR 0017 (Browser Connectivity — Phase 3).
* ADR 0085 (read-surface), ADR 0082 (write-surface).
