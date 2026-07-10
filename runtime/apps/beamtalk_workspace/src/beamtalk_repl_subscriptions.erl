%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_subscriptions).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Stable subscription facade for the workspace live push streams (BT-2399, ADR
0017 Phase 3).

The curated op layer (`beamtalk_repl_ops`) covers request/response ops, but the
workspace also pushes **live streams** — Transcript output, actor lifecycle,
class loads, bindings changes, flush completions, and reload-induced
re-check outcomes (ADR 0105, BT-2779). Previously those were
wired only inside `beamtalk_ws_handler`, which called each underlying event
module (`beamtalk_transcript_stream:subscribe/1`, `beamtalk_repl_actors:subscribe/0`,
…) directly. A dist-attached client (Phoenix LiveView, runtime-attached LSP)
had no stable surface and would otherwise cast `{subscribe, self()}` tuples at
the gen_servers — coupling to internal message shapes.

This module is that stable surface: one place that names the streams and owns
the subscribe/unsubscribe calls. The **calling process** (or an explicit pid)
becomes the subscriber, so over Erlang distribution a LiveView process subscribes
its own location-transparent pid and receives the push messages natively.

## Substrate: SystemAnnouncer (ADR 0093, BT-2531)

The actor/class/bindings/flush streams ride the typed Announcements bus
(`SystemAnnouncer`). The facade registers the subscriber pid on the bus
(`beamtalk_announcements:subscribe/4`) against each stream's announcement
class(es) with an **inert handler term** (`?PUSH_HANDLER`); the bus delivers the
native announcement message to the subscriber's mailbox (the inert-handler branch
of `dispatch_one_veneer/5`). The bespoke per-stream gen_servers
(`beamtalk_class_events` / `beamtalk_bindings_events` / `beamtalk_flush_events`
and the actor registry's lifecycle casts) were retired in BT-2531. The
`transcript` stream is *not* on the bus — it is line-rate and stays on
`beamtalk_transcript_stream` (ADR 0093 §5).

`beamtalk_repl_subscriptions` is the only sanctioned entry point (and the future
ADR 0091 RBAC seam): clients never call `beamtalk_announcements:subscribe/4`
themselves. pg plays no role — membership is the bus's workspace-local ETS table
and remote delivery is via explicit-pid registration (ADR 0093, corrected by
BT-2530).

## Stream → announcement class → push message

| Stream | Announcement class(es) | Native push message to the subscriber |
|--------|------------------------|---------------------------------------|
| `transcript` | _(bespoke, not on the bus)_ | `{transcript_output, Text :: binary()}` |
| `actors` | `ActorSpawned`, `ActorStopped` | `{beamtalk_announcement, SubRef, Class, Handler, Event}` |
| `classes` | `ClassLoaded`, `ClassRemoved` | `{beamtalk_announcement, SubRef, Class, Handler, Event}` |
| `bindings` | `BindingChanged` | `{beamtalk_announcement, SubRef, 'BindingChanged', Handler, Event}` |
| `flush` | `FlushCompleted` | `{beamtalk_announcement, SubRef, 'FlushCompleted', Handler, Event}` |
| `reload_check` | `ReloadCheckCompleted` | `{beamtalk_announcement, SubRef, 'ReloadCheckCompleted', Handler, Event}` |

`Class` is the *announced* class atom; `Event` is the typed announcement payload
(a tagged map, e.g. `#{'$beamtalk_class' => 'ActorSpawned', actorClass => …,
pid => …}`). `Handler` is the inert `?PUSH_HANDLER` term and carries no meaning
for these consumers — they discriminate on the class atom. The browser edge
re-encodes these to JSON push frames in `beamtalk_ws_handler`.

## Subscribing on behalf of a remote pid (Attach topology)

The `subscribe/1` / `subscribe_all/0` forms register `self()`, which is correct
when the caller *is* the long-lived consumer (the WebSocket handler runs on the
workspace node, so `self()` is the handler pid). A **dist-attached** client
(Phoenix LiveView, BT-2407) cannot use those forms over `rpc:call/4`: the RPC
proxy spawned on the workspace node would become the (short-lived) subscriber
instead of the LiveView pid. The `subscribe/2` / `subscribe_all/1` forms take an
**explicit subscriber pid**: a LiveView passes its own location-transparent pid
and the bus pushes messages to it directly over distribution.

## Reconnect contract (D5)

No replay. A dist-attached subscriber's rows are pruned by the bus's cross-node
monitor on disconnect (up to ~`net_ticktime` lag), and the client **re-subscribes
on reconnect** — the same rule `beamtalk_object_watch` documents for its per-object
stream.

## Ordering contract (D6)

Dispatch is caller-side (per-announcer), so there is **no cross-announcer ordering
guarantee** — weaker than the retired serialised gen_servers. This is acceptable:
every consumer of these streams is refresh-trigger driven.

## Per-object change subscriptions (ADR 0095 §5, BT-2489)

The streams above are *system-wide* (every transcript line, every class load).
The **live Inspector** (Cockpit Phase 3) needs a narrower, *per-object* stream: a
pane watching one actor's pid wants `{object_changed, Pid, ChangedSlots}` only
for that actor. Because it is parameterised by a target pid it does not fit the
`stream()` enum; `subscribe_object/2` / `unsubscribe_object/2` take the watched
actor pid **and** an explicit subscriber pid (the same dist-attach explicit-pid
form `subscribe/2` uses). It is opt-in — an actor only *publishes* while watched,
so an unwatched actor's dispatch pays just a single message-free `ets:member/2`
membership read and never builds or sends an event (see `beamtalk_object_watch`).

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
    unsubscribe_all/1,
    subscribe_object/2,
    unsubscribe_object/2
]).

%% The Transcript stream stays bespoke (line-rate, ADR 0093 §5) — the facade owns
%% the cast so clients address it by its `stream()` name only.
-define(TRANSCRIPT_REF, 'Transcript').

%% Inert handler term registered for every workspace push-stream subscriber on the
%% SystemAnnouncer bus (BT-2531). `beamtalk_announcements` recognises it as a
%% non-runnable handler and delivers the native `{beamtalk_announcement, …}`
%% message to the subscriber's mailbox instead of trying to run it.
-define(PUSH_HANDLER, repl_push_subscription).

-type stream() :: transcript | actors | classes | bindings | flush | reload_check.

-export_type([stream/0]).

-doc "The ordered list of live push streams a client can subscribe to.".
-spec streams() -> [stream()].
streams() ->
    [transcript, actors, classes, bindings, flush, reload_check].

-doc """
Subscribe the calling process to a single live push stream. The subscriber
receives the stream's push messages (see the module doc table).
""".
-spec subscribe(stream()) -> ok.
subscribe(transcript) ->
    beamtalk_transcript_stream:subscribe(?TRANSCRIPT_REF);
subscribe(Stream) ->
    subscribe_bus(Stream, self()).

-doc "Unsubscribe the calling process from a single live push stream.".
-spec unsubscribe(stream()) -> ok.
unsubscribe(transcript) ->
    beamtalk_transcript_stream:unsubscribe(?TRANSCRIPT_REF);
unsubscribe(Stream) ->
    unsubscribe_bus(Stream, self()).

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
subscribe(Stream, Pid) when is_pid(Pid) ->
    subscribe_bus(Stream, Pid).

-doc "Unsubscribe an explicit `Pid` from a single live push stream.".
-spec unsubscribe(stream(), pid()) -> ok.
unsubscribe(transcript, Pid) when is_pid(Pid) ->
    gen_server:cast(?TRANSCRIPT_REF, {unsubscribe, Pid});
unsubscribe(Stream, Pid) when is_pid(Pid) ->
    unsubscribe_bus(Stream, Pid).

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

%%====================================================================
%% Internal — SystemAnnouncer bus registration (BT-2531)
%%====================================================================

-doc """
The announcement class(es) a bus-backed stream subscribes to (BT-2531). A stream
that fans into more than one class (`actors`, `classes`) registers one
subscription per class so each announced event reaches the subscriber.
""".
-spec announcement_classes(actors | classes | bindings | flush | reload_check) -> [atom(), ...].
announcement_classes(actors) -> ['ActorSpawned', 'ActorStopped'];
announcement_classes(classes) -> ['ClassLoaded', 'ClassRemoved'];
announcement_classes(bindings) -> ['BindingChanged'];
announcement_classes(flush) -> ['FlushCompleted'];
announcement_classes(reload_check) -> ['ReloadCheckCompleted'].

-doc """
Register `Pid` on the SystemAnnouncer bus for every announcement class of
`Stream`, with the inert `?PUSH_HANDLER` term so the bus delivers the native
`{beamtalk_announcement, …}` message to `Pid`'s mailbox (BT-2531).

Idempotent per pid+class: each class is detached (`system_unsubscribe/2`) before
re-subscribing, so a LiveView that re-mounts within `net_ticktime` of a
disconnect — before the bus monitor-prunes its old rows — does not accumulate
duplicate subscriptions (which would deliver the same event twice).

Tolerant of a momentarily-unavailable bus: registration is a `gen_server:call`
to `beamtalk_announcements`, and this is on the WebSocket connect path
(`subscribe_all/0` runs as the handler authenticates). A `noproc`/timeout/error
there must **never** crash the connect — that would fail the workspace's WS health
check — so a missing bus is skipped (the consumer simply gets no pushes until it
re-subscribes) and any mid-call failure is swallowed. This restores the
crash-immunity the legacy `gen_server:cast` channels had.
""".
-spec subscribe_bus(actors | classes | bindings | flush | reload_check, pid()) -> ok.
subscribe_bus(Stream, Pid) ->
    case whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _ ->
            lists:foreach(
                fun(Class) ->
                    try
                        beamtalk_announcements:system_unsubscribe(Class, Pid),
                        beamtalk_announcements:subscribe(Class, Pid, ?PUSH_HANDLER, false)
                    of
                        _ -> ok
                    catch
                        _:_ -> ok
                    end
                end,
                announcement_classes(Stream)
            )
    end.

-doc "Remove `Pid`'s bus subscriptions for every announcement class of `Stream`.".
-spec unsubscribe_bus(actors | classes | bindings | flush | reload_check, pid()) -> ok.
unsubscribe_bus(Stream, Pid) ->
    lists:foreach(
        fun(Class) ->
            try beamtalk_announcements:system_unsubscribe(Class, Pid) of
                _ -> ok
            catch
                _:_ -> ok
            end
        end,
        announcement_classes(Stream)
    ).

-doc """
Subscribe `Subscriber` to committed state-change events on a single actor
`ActorPid` (ADR 0095 §5 / BT-2489). The subscriber receives
`{object_changed, ActorPid, ChangedSlots}` after each state write on the actor.

Parameterised by the watched pid (unlike the system-wide streams), so it takes
both pids explicitly; the LiveView passes its own location-transparent pid as
`Subscriber` over `rpc:call/4`. Delegates to the runtime `beamtalk_object_watch`
server, which keys the opt-in (only watched actors publish).
""".
-spec subscribe_object(pid(), pid()) -> ok.
subscribe_object(ActorPid, Subscriber) when is_pid(ActorPid), is_pid(Subscriber) ->
    beamtalk_object_watch:subscribe(ActorPid, Subscriber).

-doc "Unsubscribe `Subscriber` from state-change events on the actor `ActorPid`.".
-spec unsubscribe_object(pid(), pid()) -> ok.
unsubscribe_object(ActorPid, Subscriber) when is_pid(ActorPid), is_pid(Subscriber) ->
    beamtalk_object_watch:unsubscribe(ActorPid, Subscriber).
