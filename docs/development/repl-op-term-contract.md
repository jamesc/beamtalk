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
`"Actor(Counter, …)"`. An inspector, "send a message to this object", or
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
| result | `{ok, Value, Output, Warnings}` | `eval`, `unload`, `clone`, tracing read ops |
| trace | `{trace, Steps, Output, Warnings}` | `eval` (trace mode) |
| actors | `{actors, [ActorMeta]}` | `actors` |
| inspect | `{inspect, map() \| binary()}` | `inspect` |
| status | `{status, ok}` | `kill`, `interrupt`, `close`, `shutdown` |
| value | `{value, JsonValue}` | `nav-query`, `nav-symbols`, `export-traces` |
| loaded | `{loaded, Classes, Warnings}` | `load-source` |
| load_project | `{load_project, Classes, Errors, Summary, Warnings}` | `load-project` |
| sessions | `{sessions, [SessionMeta]}` | `sessions` |
| health | `{health, WorkspaceId, Nonce}` | `health` |
| completions | `{completions, [binary()]}` | `complete`, `erlang-complete` |
| docs | `{docs, binary()}` | `erlang-help` |
| codegen | `{codegen, CoreErlang, Warnings}` | `show-codegen` |
| methods | `{methods, Methods, StateVars}` | `methods` |
| class_list | `{class_list, [ClassInfo]}` | `list-classes` |
| test_results | `{test_results, TestResult}` | `test`, `test-all` |
| describe | `{describe, Ops, Versions}` | `describe` |
| error | `{error, #beamtalk_error{}}` | any op |
| error+io | `{error, #beamtalk_error{}, Output, Warnings}` | `eval`, `show-codegen` |

* `Value` and the `inspect` payload are **live terms** — over distribution they
  retain messageable pids and structure; only `encode/2` flattens them.
* The `value` tag is distinct from `result`: its payload is *already* a
  wire-shaped JSON value (a map/list of binaries, integers, booleans, and
  `null`), so `encode/2` passes it through with the identity function rather
  than `term_to_json/1`. This is what lets `nav-query` / `nav-symbols` hand the
  LSP typed rows with no inspect-string round-trip.
* `Output` is captured stdout (`binary()`); `Warnings` is `[binary()]`.
* Errors are the structured `#beamtalk_error{}` record (tag `beamtalk_error`),
  the same record used everywhere else in the runtime for user-facing errors.
  Every handler **returns** `{error, #beamtalk_error{}}` for user-facing
  failures — handlers never raise them across the seam. (The tracing ops'
  internal filter/export helpers raise a structured `#beamtalk_error{}`, but
  `beamtalk_repl_ops_perf:handle_term/4` catches and normalises it into the
  term contract.) Unexpected, non-structured crashes still propagate as
  exceptions — they are bugs, and the WebSocket edge logs them via
  `beamtalk_repl_server:handle_protocol_request/2`.

## Porting status

**Complete (BT-2402).** Every curated protocol op returns a native
`op_result()` term shape: the core path (`eval`), the actor read-surface
(`actors`, `inspect`, `kill`, `interrupt`), the write-surface
(`load-source` / `load-project` / `unload`, ADR 0082), the session ops
(`sessions` / `clone` / `close` / `health` / `shutdown`), the developer
read-surface (`complete` / `describe` / `methods` / `list-classes` /
`show-codegen` / `test` / `test-all` / `erlang-help` / `erlang-complete`,
ADR 0085), the tracing ops, and the navigation ops
(`nav-query` / `nav-symbols`).

The `{json, Binary}` escape tag used during the incremental port (BT-2399) has
been removed. Every op flows through the one `dispatch/4` → `encode/2` seam, and
dist-attached clients consume the live terms directly with no JSON step.

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

### Subscribing a remote pid over RPC (Attach topology)

The `subscribe_all/0` / `subscribe/1` forms register `self()`. That is correct
on the workspace node (the WebSocket handler *is* the consumer), but a
dist-attached client calling over `rpc:call/4` would register the short-lived
RPC proxy rather than its own pid. The explicit-pid forms take the subscriber
pid directly, so a Phoenix LiveView registers its own location-transparent pid:

```erlang
%% Run on the workspace node via rpc:call/4, passing the LiveView pid:
beamtalk_repl_subscriptions:subscribe_all(LiveViewPid).
beamtalk_repl_subscriptions:subscribe(transcript, LiveViewPid).
```

The facade still owns every `gen_server:cast`; clients pass only a `stream()`
name and a pid, never an internal `{subscribe, …}` tuple (BT-2407).

## Where this lives

| Concern | Module |
|---------|--------|
| Dispatch + term contract + JSON edge | `beamtalk_repl_ops` |
| `eval` term handler | `beamtalk_repl_ops_eval:handle_term/4` |
| Actor read-surface term handlers | `beamtalk_repl_ops_actors:handle_term/4` |
| Write-surface term handlers (`load-*`, `unload`) | `beamtalk_repl_ops_load:handle_term/4` |
| Session term handlers | `beamtalk_repl_ops_session:handle_term/4` |
| Developer read-surface term handlers | `beamtalk_repl_ops_dev:handle_term/4` |
| Tracing term handlers | `beamtalk_repl_ops_perf:handle_term/4` |
| Navigation term handlers | `beamtalk_repl_ops_nav:handle_term/4`, `beamtalk_repl_ops_nav_symbols:handle_term/4` |
| Per-tag JSON encoders | `beamtalk_repl_protocol:encode_*` |
| Subscription facade | `beamtalk_repl_subscriptions` |
| WebSocket transport edge | `beamtalk_repl_server:handle_op/4`, `beamtalk_ws_handler` |

Each per-op module keeps a thin `handle/4` =
`beamtalk_repl_ops:encode(handle_term(...), Msg)` wrapper for the WebSocket
transport, so `dispatch/4` (terms, for dist clients) and `handle/4` (JSON, for
the browser) share one implementation.

## References

* `docs/research/phoenix-topology-spike.md` — "API shape: return terms, encode
  JSON only at the WebSocket edge".
* ADR 0017 (Browser Connectivity — Phase 3).
* ADR 0085 (read-surface), ADR 0082 (write-surface).
