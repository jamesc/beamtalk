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
| result | `{ok, Value, Output, Warnings}` | `eval`, `unload`, `clone`, tracing read ops |
| trace | `{trace, Steps, Output, Warnings}` | `eval` (trace mode) |
| script_exit | `{script_exit, Code, Output, Warnings}` | `eval` (connected `Program exit:`, BT-2688) |
| actors | `{actors, [ActorMeta]}` | `actors` |
| inspect | `{inspect, map() \| binary()}` | `inspect` |
| status | `{status, ok}` | `kill`, `interrupt`, `close`, `shutdown` |
| value | `{value, JsonValue}` | `nav-query`, `nav-symbols`, `export-traces` |
| loaded | `{loaded, Classes, Warnings}` | `load-source` |
| load_project | `{load_project, Classes, Errors, Summary, Warnings}` | `load-project` |
| sessions | `{sessions, [SessionMeta]}` | `sessions` |
| health | `{health, WorkspaceId, Nonce}` | `health` |
| completions | `{completions, [binary()]}` | `complete`, `erlang-complete` |
| diagnostics | `{diagnostics, [map()]}` | `diagnostics` |
| docs | `{docs, binary()}` | `erlang-help`, `hover` |
| codegen | `{codegen, CoreErlang, Warnings}` | `show-codegen` |
| methods | `{methods, Methods, StateVars}` | `methods` |
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

All curated protocol ops return native `op_result()` term shapes (BT-2402); the
`{json, Binary}` escape tag used during the incremental port (BT-2399) has been
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
    {ok, Value :: term(), Output :: binary(), Warnings :: [binary()]}
    | {trace, Steps :: [term()], Output :: binary(), Warnings :: [binary()]}
    | {script_exit, Code :: non_neg_integer(), Output :: binary(), Warnings :: [binary()]}
    | {actors, [map()]}
    | {inspect, map() | binary()}
    | {status, ok}
    | {value, JsonValue :: term()}
    | {loaded, Classes :: [map()], Warnings :: [binary()]}
    | {load_project, Classes :: [binary()], Errors :: [map()], Summary :: binary(),
        Warnings :: [binary()]}
    | {sessions, [map()]}
    | {health, WorkspaceId :: binary(), Nonce :: binary()}
    | {completions, [binary()]}
    | {diagnostics, [map()]}
    | {docs, binary()}
    | {codegen, CoreErlang :: binary(), Warnings :: [binary()]}
    | {methods, Methods :: [map()], StateVars :: [binary()]}
    | {class_list, [map()]}
    | {test_results, map()}
    | {describe, Ops :: map(), Versions :: map()}
    | {error, #beamtalk_error{}}
    | {error, #beamtalk_error{}, Output :: binary(), Warnings :: [binary()]}.

%%% Dispatch — op name → term result

-doc """
Route a protocol op to its handler, returning a structured `op_result()` term.

This is the term-returning entry point for dist-attached clients. It mirrors
the op routing previously inlined in `beamtalk_repl_server:handle_op/4`. Every
op returns a native `op_result()` term shape (BT-2402) — handlers never raise a
user-facing error, they return `{error, #beamtalk_error{}}`.
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
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"load-source">>;
    Op =:= <<"load-project">>;
    Op =:= <<"unload">>;
    %% BT-2670: edit → compile → reload → write-back for a project-owned native.
    Op =:= <<"save-native-source">>
->
    beamtalk_repl_ops_load:handle_term(Op, Params, Msg, SessionPid);
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"sessions">>;
    Op =:= <<"clone">>;
    Op =:= <<"close">>;
    Op =:= <<"health">>;
    Op =:= <<"shutdown">>
->
    beamtalk_repl_ops_session:handle_term(Op, Params, Msg, SessionPid);
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"complete">>;
    Op =:= <<"hover">>;
    Op =:= <<"diagnostics">>;
    Op =:= <<"describe">>;
    Op =:= <<"methods">>;
    Op =:= <<"list-classes">>;
    Op =:= <<"list-tests">>;
    Op =:= <<"load-tests">>;
    Op =:= <<"show-codegen">>;
    Op =:= <<"test">>;
    Op =:= <<"test-all">>;
    Op =:= <<"erlang-help">>;
    Op =:= <<"erlang-complete">>
->
    beamtalk_repl_ops_dev:handle_term(Op, Params, Msg, SessionPid);
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"enable-tracing">>;
    Op =:= <<"disable-tracing">>;
    Op =:= <<"get-traces">>;
    Op =:= <<"actor-stats">>;
    Op =:= <<"export-traces">>
->
    beamtalk_repl_ops_perf:handle_term(Op, Params, Msg, SessionPid);
dispatch(<<"pid-stats">>, Params, Msg, SessionPid) ->
    %% ADR 0095 §5 / BT-2489 (Cockpit Phase 3): live-Inspector process metrics
    %% read, the request/response companion to the `object` push stream.
    beamtalk_repl_ops_watch:handle_term(<<"pid-stats">>, Params, Msg, SessionPid);
dispatch(<<"nav-query">>, Params, Msg, SessionPid) ->
    beamtalk_repl_ops_nav:handle_term(<<"nav-query">>, Params, Msg, SessionPid);
dispatch(<<"nav-symbols">>, Params, Msg, SessionPid) ->
    beamtalk_repl_ops_nav_symbols:handle_term(<<"nav-symbols">>, Params, Msg, SessionPid);
dispatch(Op, Params, Msg, SessionPid) when
    Op =:= <<"browse-classes">>;
    Op =:= <<"browse-protocols">>;
    Op =:= <<"browse-method-source">>;
    Op =:= <<"browse-class-definition">>;
    %% BT-2578 native pane + BT-2648 native-modules enumeration.
    Op =:= <<"browse-native-source">>;
    Op =:= <<"browse-native-modules">>
->
    %% ADR 0095 (BT-2488): System Browser browse facade — read-only term-ops,
    %% each returning `{value, JsonValue}`.
    beamtalk_repl_ops_browse:handle_term(Op, Params, Msg, SessionPid);
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
encode({script_exit, Code, Output, Warnings}, Msg) ->
    %% BT-2688: connected-session `Program exit: Code`. Carries the POSIX exit
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
