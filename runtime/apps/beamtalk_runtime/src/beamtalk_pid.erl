%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_pid).

%%% **DDD Context:** Runtime Context

-moduledoc """
Remote-safe pid liveness (ADR 0126 §7.3, Phase 0).

`erlang:is_process_alive/1` is local-only: it raises `badarg` for a pid on
another node. Every liveness guard in the runtime needs the same
remote-safe answer, and before this module that guard was duplicated —
`beamtalk_announcements:subscriber_alive/1` grew a `node(Pid) =:= node()`
clause for BT-2530, and `beamtalk_actor.erl` carried six raw
`is_process_alive/1` call sites that still crashed on a remote pid. This is
the single leaf module both route through (CLAUDE.md "No duplicate
implementations"): `beamtalk_announcements:subscriber_alive/1` delegates
here, and every `is_process_alive/1` guard in `beamtalk_actor.erl` calls
`is_alive/1` instead.
""".

-export([is_alive/1, qualify_registered_ref/1]).

-doc """
Is `Pid` alive?

A local pid delegates to `erlang:is_process_alive/1`. A remote pid answers
`true` optimistically: Erlang has no cheap, race-free way to ask another
node "is this pid alive" without a network round trip, and a caller about
to send to it finds out the real answer for free — a dead remote actor's
cast harmlessly lands in the void, and a partitioned or never-connected
node surfaces as `node_down`/`noconnection` from the send itself (mapped to
a structured error by the call sites that use this guard), not from this
check. This mirrors the fix BT-2530 made for
`beamtalk_announcements:subscriber_alive/1`.
""".
-spec is_alive(pid()) -> boolean().
is_alive(Pid) when is_pid(Pid) ->
    case node(Pid) =:= node() of
        true -> is_process_alive(Pid);
        false -> true
    end.

-doc """
Node-qualify an actor reference for a cross-node hop (ADR 0126 §3, §5.1).

A local `{registered, Name}` ref (ADR 0079) resolves via `whereis/1` on
every send, so shipping it unqualified to another node would silently
re-resolve it against *that* node's registry instead of the sender's. This
stamps it with the sender's own node so it keeps resolving against its
origin. A ref already node-qualified (relayed from elsewhere), a
cluster-unique `{global, Name}` ref (ADR 0126 §4 — already node-independent
by construction, `global:whereis_name/1` resolves it the same way from
every connected node, so there is no "origin" to stamp), and an ordinary
pid (already node-qualified natively by BEAM) all pass through unchanged.

The single leaf both cross-node actor-ref paths route through (CLAUDE.md
"No duplicate implementations"): `beamtalk_wire`'s generic term walk
(`encode_beamtalk_object/2`, for an actor ref the walk meets directly) and
`beamtalk_shape_migration`'s `sendable_ref`-tier field packing (for an
Actor-typed field inside a `Value` instance) both call this instead of
each carrying their own copy of the rewrite.
""".
-spec qualify_registered_ref(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()}
) ->
    pid() | {registered, atom(), node()} | {global, atom()}.
qualify_registered_ref({registered, Name}) when is_atom(Name) ->
    {registered, Name, node()};
qualify_registered_ref(Pid) ->
    Pid.
