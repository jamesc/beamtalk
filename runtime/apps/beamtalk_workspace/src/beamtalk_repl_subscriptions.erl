%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_subscriptions).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Stable subscription facade for the workspace live push streams (BT-2399, ADR
0017 Phase 3).

The curated op layer (`beamtalk_repl_ops`) covers request/response ops, but the
workspace also pushes **live streams** — Transcript output, actor lifecycle,
class loads, bindings changes, and flush completions. Previously those were
wired only inside `beamtalk_ws_handler`, which called each underlying event
module (`beamtalk_transcript_stream:subscribe/1`, `beamtalk_repl_actors:subscribe/0`,
…) directly. A dist-attached client (Phoenix LiveView, runtime-attached LSP)
had no stable surface and would otherwise cast `{subscribe, self()}` tuples at
the gen_servers — coupling to internal message shapes.

This module is that stable surface: one place that names the streams and owns
the subscribe/unsubscribe calls. The **calling process** becomes the subscriber
(the underlying event servers register `self()`), so over Erlang distribution a
LiveView process subscribes its own location-transparent pid and receives the
push messages natively.

## Subscribing on behalf of a remote pid (Attach topology)

The `subscribe/1` / `subscribe_all/0` forms register `self()`, which is correct
when the caller *is* the long-lived consumer (the WebSocket handler runs on the
workspace node, so `self()` is the handler pid). A **dist-attached** client
(Phoenix LiveView, BT-2407) cannot use those forms over `rpc:call/4`: the RPC
proxy spawned on the workspace node would become the (short-lived) subscriber
instead of the LiveView pid, and its `'DOWN'` would immediately unsubscribe it.

The `subscribe/2` / `subscribe_all/1` forms take an **explicit subscriber pid**.
A LiveView passes its own location-transparent pid; the facade registers that
pid and the workspace pushes messages to it directly over distribution. The
underlying event servers already accept an explicit pid in their subscribe cast,
so this is the same registration the WebSocket edge uses — only the subscriber
identity differs. The facade still owns every cast: clients never cast
`{subscribe, Pid}` tuples at the gen_servers themselves.

## Streams and the messages they push

| Stream | Push message to the subscriber |
|--------|--------------------------------|
| `transcript` | `{transcript_output, Text :: binary()}` |
| `actors` | `{actor_spawned, Meta}` / `{actor_stopped, StopInfo}` |
| `classes` | `{class_loaded, ClassName :: atom()}` |
| `bindings` | `{bindings_changed, SessionId :: binary()}` |
| `flush` | `{flush_completed, Files :: [binary()]}` |

These message shapes are part of the documented push contract; clients pattern
match on them (the browser edge re-encodes them to JSON push frames in
`beamtalk_ws_handler`).

## References

* `docs/development/repl-op-term-contract.md` — the op + subscription contract.
* `docs/research/phoenix-topology-spike.md` — "One genuine gap: push-stream
  subscription".
* ADR 0017 Phase 3 (Browser Connectivity).
""".

-export([
    streams/0,
    subscribe/1,
    subscribe/2,
    unsubscribe/1,
    unsubscribe/2,
    subscribe_all/0,
    subscribe_all/1,
    unsubscribe_all/0,
    unsubscribe_all/1
]).

%% Registered names of the underlying event servers (all local to the workspace
%% node). The facade owns these so the cross-node subscribe cast lives in one
%% place; clients address the streams by their `stream()` name only.
-define(TRANSCRIPT_REF, 'Transcript').
-define(ACTOR_REGISTRY, beamtalk_actor_registry).

-type stream() :: transcript | actors | classes | bindings | flush.

-export_type([stream/0]).

-doc "The ordered list of live push streams a client can subscribe to.".
-spec streams() -> [stream()].
streams() ->
    [transcript, actors, classes, bindings, flush].

-doc """
Subscribe the calling process to a single live push stream. The subscriber
receives the stream's push messages (see the module doc table).
""".
-spec subscribe(stream()) -> ok.
subscribe(transcript) ->
    beamtalk_transcript_stream:subscribe('Transcript');
subscribe(actors) ->
    beamtalk_repl_actors:subscribe();
subscribe(classes) ->
    beamtalk_class_events:subscribe();
subscribe(bindings) ->
    beamtalk_bindings_events:subscribe();
subscribe(flush) ->
    beamtalk_flush_events:subscribe().

-doc "Unsubscribe the calling process from a single live push stream.".
-spec unsubscribe(stream()) -> ok.
unsubscribe(transcript) ->
    beamtalk_transcript_stream:unsubscribe('Transcript');
unsubscribe(actors) ->
    beamtalk_repl_actors:unsubscribe();
unsubscribe(classes) ->
    beamtalk_class_events:unsubscribe();
unsubscribe(bindings) ->
    beamtalk_bindings_events:unsubscribe();
unsubscribe(flush) ->
    beamtalk_flush_events:unsubscribe().

-doc """
Subscribe the calling process to every live push stream. Used by the WebSocket
handler on connect/resume, and the one call a dist-attached client makes to
mirror the browser's live surface.
""".
-spec subscribe_all() -> ok.
subscribe_all() ->
    lists:foreach(fun subscribe/1, streams()).

-doc "Unsubscribe the calling process from every live push stream.".
-spec unsubscribe_all() -> ok.
unsubscribe_all() ->
    lists:foreach(fun unsubscribe/1, streams()).

-doc """
Subscribe an explicit `Pid` to a single live push stream. Used by dist-attached
clients (Phoenix LiveView, Attach topology) that call this over `rpc:call/4` and
must register their own location-transparent pid rather than the short-lived RPC
proxy. The registered pid receives the stream's push messages (see the module
doc table).
""".
-spec subscribe(stream(), pid()) -> ok.
subscribe(transcript, Pid) when is_pid(Pid) ->
    gen_server:cast(?TRANSCRIPT_REF, {subscribe, Pid});
subscribe(actors, Pid) when is_pid(Pid) ->
    gen_server:cast(?ACTOR_REGISTRY, {subscribe_lifecycle, Pid});
subscribe(classes, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_class_events, {subscribe, Pid});
subscribe(bindings, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_bindings_events, {subscribe, Pid});
subscribe(flush, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_flush_events, {subscribe, Pid}).

-doc "Unsubscribe an explicit `Pid` from a single live push stream.".
-spec unsubscribe(stream(), pid()) -> ok.
unsubscribe(transcript, Pid) when is_pid(Pid) ->
    gen_server:cast(?TRANSCRIPT_REF, {unsubscribe, Pid});
unsubscribe(actors, Pid) when is_pid(Pid) ->
    gen_server:cast(?ACTOR_REGISTRY, {unsubscribe_lifecycle, Pid});
unsubscribe(classes, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_class_events, {unsubscribe, Pid});
unsubscribe(bindings, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_bindings_events, {unsubscribe, Pid});
unsubscribe(flush, Pid) when is_pid(Pid) ->
    gen_server:cast(beamtalk_flush_events, {unsubscribe, Pid}).

-doc """
Subscribe an explicit `Pid` to every live push stream. The one call a
dist-attached client makes over RPC to mirror the browser's live surface with
its own pid as the subscriber.
""".
-spec subscribe_all(pid()) -> ok.
subscribe_all(Pid) when is_pid(Pid) ->
    lists:foreach(fun(Stream) -> subscribe(Stream, Pid) end, streams()).

-doc "Unsubscribe an explicit `Pid` from every live push stream.".
-spec unsubscribe_all(pid()) -> ok.
unsubscribe_all(Pid) when is_pid(Pid) ->
    lists:foreach(fun(Stream) -> unsubscribe(Stream, Pid) end, streams()).
