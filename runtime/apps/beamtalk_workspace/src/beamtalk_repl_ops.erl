%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Term-returning op-layer seam for the curated REPL protocol (ADR 0017
Phase 3).

This module is the single dispatch entry point for protocol ops (the op table
itself is `beamtalk_repl_op_table`) and the **only**
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
| result | `{ok, Value, Output, Warnings}` | `eval`, `unload`, `clone`, tracing read ops |
| trace | `{trace, Steps, Output, Warnings}` | `eval` (trace mode) |
| script_exit | `{script_exit, Code, Output, Warnings}` | `eval` (connected `Program exit:`) |
| actors | `{actors, [ActorMeta]}` | `actors` |
| inspect | `{inspect, map() \| binary()}` | `inspect` |
| status | `{status, ok}` | `kill`, `interrupt`, `close`, `shutdown` |
| value | `{value, JsonValue}` | `nav-query`, `nav-symbols`, `export-traces`, `list-tests`, `reload-findings` |
| loaded | `{loaded, Classes, Warnings}` | `load-source` |
| load_project | `{load_project, Classes, Errors, Summary, Warnings}` | `load-project` |
| sessions | `{sessions, [SessionMeta]}` | `sessions` |
| health | `{health, WorkspaceId, Nonce}` | `health` |
| completions | `{completions, [binary()]}` | `complete`, `erlang-complete` |
| diagnostics | `{diagnostics, [map()]}` | `diagnostics` |
| docs | `{docs, binary()}` | `erlang-help`, `hover` |
| codegen | `{codegen, CoreErlang, Warnings}` | `show-codegen` |
| methods | `{methods, Methods, StateVars}` | `methods` |
| inherited_methods | `{inherited_methods, Methods}` | `inherited-methods` |
| class_list | `{class_list, [ClassInfo]}` | `list-classes` |
| test_results | `{test_results, TestResult}` | `test`, `test-all` |
| describe | `{describe, Ops, Versions}` | `describe` |
| error | `{error, #beamtalk_error{}}` | any op |
| error+io | `{error, #beamtalk_error{}, Output, Warnings}` | `eval`, `show-codegen` |

`Value` and `inspect` payloads are live terms — over distribution they retain
messageable pids and structure; only `encode/2` flattens them to JSON. The
`value` tag is distinct from `result`: its payload is *already* a wire-shaped
JSON value (a map/list of binaries, integers, booleans, and `null`), so
`encode/2` passes it through with the identity function rather than
`term_to_json/1`.

## Porting status

All curated protocol ops return native `op_result()` term shapes; the
`{json, Binary}` escape tag used during the incremental port has been
removed. Every op flows through the one `dispatch/4` → `encode/2` seam, and
dist-attached clients consume the live terms directly without any JSON step.

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
    {ok, Value :: term(), Output :: binary(), Warnings :: [beamtalk_repl_protocol:warning()]}
    | {trace, Steps :: [term()], Output :: binary(), Warnings :: [beamtalk_repl_protocol:warning()]}
    | {script_exit, Code :: non_neg_integer(), Output :: binary(),
        Warnings :: [beamtalk_repl_protocol:warning()]}
    | {actors, [map()]}
    | {inspect, map() | binary()}
    | {status, ok}
    | {value, JsonValue :: term()}
    | {loaded, Classes :: [map()], Warnings :: [beamtalk_repl_protocol:warning()]}
    | {load_project, Classes :: [binary()], Errors :: [map()], Summary :: binary(),
        Warnings :: [beamtalk_repl_protocol:warning()]}
    | {sessions, [map()]}
    | {health, WorkspaceId :: binary(), Nonce :: binary()}
    | {completions, [binary()]}
    | {diagnostics, [map()]}
    | {docs, binary()}
    | {codegen, CoreErlang :: binary(), Warnings :: [beamtalk_repl_protocol:warning()]}
    | {methods, Methods :: [map()], StateVars :: [binary()]}
    | {inherited_methods, Methods :: [map()]}
    | {class_list, [map()]}
    | {test_results, map()}
    | {describe, Ops :: map(), Versions :: map()}
    | {error, #beamtalk_error{}}
    | {error, #beamtalk_error{}, Output :: binary(),
        Warnings :: [beamtalk_repl_protocol:warning()]}.

%%% Dispatch — op name → term result

-doc """
Route a protocol op to its handler, returning a structured `op_result()` term.

This is the term-returning entry point for dist-attached clients. It mirrors
the op routing previously inlined in `beamtalk_repl_server:handle_op/4`. Every
op returns a native `op_result()` term shape — handlers never raise a
user-facing error, they return `{error, #beamtalk_error{}}`.

Every op is first checked against the node's capabilities
(`beamtalk_capability:check/1`, ADR 0125 §1.5): on a release node, an op that
compiles source is refused with `release_mode_no_compiler` and one that
mutates the workspace with `release_mode_no_workspace`, before its handler
runs.
""".
-spec dispatch(binary(), map(), protocol_msg(), pid()) -> op_result().
dispatch(Op, Params, Msg, SessionPid) ->
    case beamtalk_capability:check(Op) of
        ok -> route(Op, Params, Msg, SessionPid);
        {error, Err} -> {error, Err}
    end.

-doc """
Route `Op` to the module the op table (`beamtalk_repl_op_table`) names for it.
Ops served by the WebSocket transport itself (`beamtalk_ws_handler`) are not
routed here and, like any op the table does not name, are refused as unknown.
""".
-spec route(binary(), map(), protocol_msg(), pid()) -> op_result().
route(Op, Params, Msg, SessionPid) ->
    case beamtalk_repl_op_table:handler(Op) of
        {ok, beamtalk_ws_handler} ->
            unknown_op(Op);
        {ok, Handler} ->
            Handler:handle_term(Op, Params, Msg, SessionPid);
        error ->
            unknown_op(Op)
    end.

-spec unknown_op(binary()) -> op_result().
unknown_op(Op) ->
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
encode({script_exit, Code, Output, Warnings}, Msg) ->
    %% connected-session `Program exit: Code`. Carries the POSIX exit
    %% status in a dedicated `exit_code` field; the session shell has already
    %% terminated by the time this reply is encoded.
    beamtalk_repl_protocol:encode_script_exit(Code, Msg, Output, Warnings);
encode({actors, Actors}, Msg) ->
    beamtalk_repl_protocol:encode_actors(Actors, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({inspect, Map}, Msg) when is_map(Map) ->
    beamtalk_repl_protocol:encode_inspect(Map, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({inspect, Bin}, Msg) when is_binary(Bin) ->
    beamtalk_repl_protocol:encode_inspect(Bin, Msg);
encode({status, ok}, Msg) ->
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
encode({value, JsonValue}, Msg) ->
    %% Already-wire-shaped value (maps/lists/binaries/integers/booleans/null) —
    %% encode with identity so term_to_json does not stringify it.
    beamtalk_repl_protocol:encode_result(JsonValue, Msg, fun(V) -> V end);
encode({loaded, Classes, Warnings}, Msg) ->
    beamtalk_repl_protocol:encode_loaded(
        Classes, Msg, fun beamtalk_repl_json:term_to_json/1, Warnings
    );
encode({load_project, Classes, Errors, Summary, Warnings}, Msg) ->
    beamtalk_repl_protocol:encode_load_project(Classes, Errors, Summary, Warnings, Msg);
encode({sessions, Sessions}, Msg) ->
    beamtalk_repl_protocol:encode_sessions(
        Sessions, Msg, fun beamtalk_repl_json:term_to_json/1
    );
encode({health, WorkspaceId, Nonce}, Msg) ->
    beamtalk_repl_protocol:encode_health(WorkspaceId, Nonce, Msg);
encode({completions, Completions}, Msg) ->
    beamtalk_repl_protocol:encode_completions(Completions, Msg);
encode({diagnostics, Diagnostics}, Msg) ->
    beamtalk_repl_protocol:encode_diagnostics(Diagnostics, Msg);
encode({docs, DocText}, Msg) ->
    beamtalk_repl_protocol:encode_docs(DocText, Msg);
encode({codegen, CoreErlang, Warnings}, Msg) ->
    beamtalk_repl_protocol:encode_codegen(CoreErlang, Warnings, Msg);
encode({methods, Methods, StateVars}, Msg) ->
    beamtalk_repl_protocol:encode_methods(Methods, StateVars, Msg);
encode({inherited_methods, Methods}, Msg) ->
    beamtalk_repl_protocol:encode_inherited_methods(Methods, Msg);
encode({class_list, ClassList}, Msg) ->
    beamtalk_repl_protocol:encode_class_list(ClassList, Msg);
encode({test_results, TestResult}, Msg) ->
    beamtalk_repl_protocol:encode_test_results(TestResult, Msg);
encode({describe, Ops, Versions}, Msg) ->
    beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg);
encode({error, Err}, Msg) ->
    beamtalk_repl_json:encode_error(Err, Msg);
encode({error, Err, Output, Warnings}, Msg) ->
    beamtalk_repl_json:encode_error(Err, Msg, Output, Warnings).
