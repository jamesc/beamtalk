%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Term-returning op-layer seam for the curated REPL protocol (BT-2399, ADR 0017
Phase 3).

This module is the single dispatch table for protocol ops and the **only**
place JSON is produced for the WebSocket transport. It splits the curated op
layer into two halves:

* `dispatch/4` — routes an op to its handler and returns a structured Erlang
  **term** (`op_result()`), never JSON. This is the contract that
  dist-attached clients (Phoenix LiveView, runtime-attached LSP / MCP) consume
  directly over Erlang distribution, where JSON is both overhead and *lossy*
  (a live `{beamtalk_object, Class, Module, Pid}` carries a real remote pid
  that JSON would flatten to a dead display string).
* `encode/2` — encodes an `op_result()` to the protocol JSON binary at the
  **WebSocket transport edge only**. The browser wire format is unchanged.

So the data flow is:

```
                       ┌─ Phoenix / LSP / MCP (dist): consume terms directly
  dispatch/4 ─ term ───┤
                       └─ encode/2 ─ JSON ─ WebSocket (browser)
```

## The cross-surface contract (`op_result()`)

`op_result()` — together with the `#beamtalk_error{}` record (tagged
`beamtalk_error`, so Phoenix can match on the tag without sharing a `.hrl`) —
is the **documented, stable API boundary** across surfaces. It is deliberately
a small tagged union rather than raw internal shapes, so the contract does not
leak compiler/runtime internals.

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

`Value` and `inspect` payloads are live terms — over distribution they retain
messageable pids and structure; only `encode/2` flattens them to JSON.

## Incremental porting

`eval` (the canonical core path) and the actor read-surface ops (`actors`,
`inspect`, `kill`, `interrupt`) return native term shapes. The remaining ops
(`load-*`, `unload`, `sessions`/`clone`/`close`/`health`/`shutdown`,
`complete`/`describe`/…, tracing, `nav-*`) are still JSON-internally and are
surfaced through the `{json, Binary}` escape tag so every op flows through the
one `dispatch/4` → `encode/2` seam without a wire-format change. Porting those
to native term shapes is tracked as follow-up work for the LiveView IDE epic
(read-surface ADR 0085 / write-surface ADR 0082).

## References

* `docs/development/repl-op-term-contract.md` — the contract reference.
* `docs/research/phoenix-topology-spike.md` — "API shape: return terms, encode
  JSON only at the WebSocket edge".
* ADR 0017 Phase 3 (Browser Connectivity).
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([dispatch/4, encode/2]).

-export_type([op_result/0]).

-type protocol_msg() :: beamtalk_repl_protocol:protocol_msg().

-doc """
The cross-surface op result contract. A dispatched op returns one of these
tagged terms; `encode/2` is the only thing that turns them into JSON.
""".
-type op_result() ::
    {ok, Value :: term(), Output :: binary(), Warnings :: [binary()]}
    | {trace, Steps :: [term()], Output :: binary(), Warnings :: [binary()]}
    | {actors, [map()]}
    | {inspect, map() | binary()}
    | {status, ok}
    | {error, #beamtalk_error{}}
    | {error, #beamtalk_error{}, Output :: binary(), Warnings :: [binary()]}
    | {json, binary()}.

%%% Dispatch — op name → term result

-doc """
Route a protocol op to its handler, returning a structured `op_result()` term.

This is the term-returning entry point for dist-attached clients. It mirrors
the op routing previously inlined in `beamtalk_repl_server:handle_op/4`. Ops
that have been ported to the term contract return native term shapes; the rest
are surfaced via the `{json, Binary}` escape tag.
""".
-spec dispatch(binary(), map(), protocol_msg(), pid()) -> op_result().
dispatch(<<"eval">>, Params, Msg, SessionPid) ->
    beamtalk_repl_ops_eval:handle_term(<<"eval">>, Params, Msg, SessionPid);
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"actors">>;
    Op =:= <<"inspect">>;
    Op =:= <<"kill">>;
    Op =:= <<"interrupt">>
->
    beamtalk_repl_ops_actors:handle_term(Op, Params, Msg, SessionPid);
dispatch(<<"load-source">>, Params, Msg, SessionPid) ->
    {json, beamtalk_repl_ops_load:handle(<<"load-source">>, Params, Msg, SessionPid)};
dispatch(<<"load-project">>, Params, Msg, SessionPid) ->
    {json, beamtalk_repl_ops_load:handle(<<"load-project">>, Params, Msg, SessionPid)};
dispatch(<<"unload">>, Params, Msg, SessionPid) ->
    {json, beamtalk_repl_ops_load:handle(<<"unload">>, Params, Msg, SessionPid)};
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"sessions">>;
    Op =:= <<"clone">>;
    Op =:= <<"close">>;
    Op =:= <<"health">>;
    Op =:= <<"shutdown">>
->
    {json, beamtalk_repl_ops_session:handle(Op, Params, Msg, SessionPid)};
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"complete">>;
    Op =:= <<"describe">>;
    Op =:= <<"methods">>;
    Op =:= <<"list-classes">>;
    Op =:= <<"show-codegen">>;
    Op =:= <<"test">>;
    Op =:= <<"test-all">>;
    Op =:= <<"erlang-help">>;
    Op =:= <<"erlang-complete">>
->
    {json, beamtalk_repl_ops_dev:handle(Op, Params, Msg, SessionPid)};
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"enable-tracing">>;
    Op =:= <<"disable-tracing">>;
    Op =:= <<"get-traces">>;
    Op =:= <<"actor-stats">>;
    Op =:= <<"export-traces">>
->
    {json, beamtalk_repl_ops_perf:handle(Op, Params, Msg, SessionPid)};
dispatch(<<"nav-query">>, Params, Msg, SessionPid) ->
    {json, beamtalk_repl_ops_nav:handle(<<"nav-query">>, Params, Msg, SessionPid)};
dispatch(<<"nav-symbols">>, Params, Msg, SessionPid) ->
    {json, beamtalk_repl_ops_nav_symbols:handle(<<"nav-symbols">>, Params, Msg, SessionPid)};
dispatch(Op, _Params, _Msg, _SessionPid) ->
    Err0 = beamtalk_error:new(unknown_op, 'REPL'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Unknown operation: ">>, Op])
    ),
    {error, Err1}.

%%% Encode — term result → protocol JSON (WebSocket transport edge only)

-doc """
Encode an `op_result()` term to the protocol JSON binary. This is the only
place JSON is produced for the curated op layer; dist-attached clients never
call it.
""".
-spec encode(op_result(), protocol_msg()) -> binary().
encode({ok, Value, Output, Warnings}, Msg) ->
    beamtalk_repl_protocol:encode_result(
        Value, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
    );
encode({trace, Steps, Output, Warnings}, Msg) ->
    beamtalk_repl_protocol:encode_trace_result(
        Steps, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings
    );
encode({actors, Actors}, Msg) ->
    beamtalk_repl_protocol:encode_actors(Actors, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({inspect, Map}, Msg) when is_map(Map) ->
    beamtalk_repl_protocol:encode_inspect(Map, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({inspect, Bin}, Msg) when is_binary(Bin) ->
    beamtalk_repl_protocol:encode_inspect(Bin, Msg);
encode({status, ok}, Msg) ->
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({error, Err}, Msg) ->
    beamtalk_repl_json:encode_error(Err, Msg);
encode({error, Err, Output, Warnings}, Msg) ->
    beamtalk_repl_json:encode_error(Err, Msg, Output, Warnings);
encode({json, Bin}, _Msg) when is_binary(Bin) ->
    Bin.
