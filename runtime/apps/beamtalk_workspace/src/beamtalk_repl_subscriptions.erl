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

-export([streams/0, subscribe/1, unsubscribe/1, subscribe_all/0, unsubscribe_all/0]).

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
