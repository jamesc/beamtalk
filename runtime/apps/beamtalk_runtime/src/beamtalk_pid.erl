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
the single leaf module both route through (AGENTS.md "No duplicate
implementations"): `beamtalk_announcements:subscriber_alive/1` delegates
here, and every `is_process_alive/1` guard in `beamtalk_actor.erl` calls
`is_alive/1` instead.
""".

-export([is_alive/1, ask_alive/3, qualify_registered_ref/1]).

-include("beamtalk.hrl").

%% How long `ask_alive/3` waits for a remote pid's node to answer (BT-5192):
%% the same bound as a remote lookup (ADR 0126 §3).
-define(REMOTE_ALIVE_TIMEOUT, ?BT_REMOTE_CALL_TIMEOUT).

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

This is the runtime's internal guard, for deciding whether to attempt a send.
The user-facing `isAlive` selector uses `ask_alive/3`, which asks a remote
pid's node instead (BT-5192).
""".
-spec is_alive(pid()) -> boolean().
is_alive(Pid) when is_pid(Pid) ->
    case node(Pid) =:= node() of
        true -> is_process_alive(Pid);
        false -> true
    end.

-doc """
The user-facing `isAlive` answer for `Pid` (actors, supervisors; BT-5192,
ADR 0126 §7.3). `Class` and `Selector` label the error.

A local pid is `erlang:is_process_alive/1`. A remote pid's node is asked
(`erlang:is_process_alive/1` over `erpc`), waiting at most
`?REMOTE_ALIVE_TIMEOUT` ms, so a dead remote actor answers `false`. When the
node cannot tell, the failure is classified by `beamtalk_call_failure`, the
classification a failed send uses: an unreachable node is `node_down` and no
answer in time is `timeout`, never "alive" nor "dead" (across a partition the
actor may well be alive). Any other `erpc` failure, or an answer that is not a
boolean, is a `runtime_error` whose details name the node and what came back.
""".
-spec ask_alive(pid(), atom(), atom()) -> {ok, boolean()} | {error, #beamtalk_error{}}.
ask_alive(Pid, _Class, _Selector) when node(Pid) =:= node() ->
    {ok, is_process_alive(Pid)};
ask_alive(Pid, Class, Selector) ->
    Node = node(Pid),
    case beamtalk_call_failure:erpc(Node, erlang, is_process_alive, [Pid], ?REMOTE_ALIVE_TIMEOUT) of
        {ok, Alive} when is_boolean(Alive) ->
            {ok, Alive};
        {ok, Other} ->
            {error, unexpected_answer_error(Class, Selector, Node, {unexpected_result, Other})};
        {error, {node_down, DownNode}} ->
            {error, beamtalk_error:node_down(Class, Selector, DownNode)};
        {error, timeout} ->
            {error,
                beamtalk_error:timeout(
                    Class,
                    Selector,
                    <<"The actor's node did not answer in time; it may be partitioned or overloaded">>
                )};
        {error, {other, Reason}} ->
            {error, unexpected_answer_error(Class, Selector, Node, Reason)}
    end.

-spec unexpected_answer_error(atom(), atom(), node(), term()) -> #beamtalk_error{}.
unexpected_answer_error(Class, Selector, Node, Reason) ->
    beamtalk_error:with_details(
        beamtalk_error:new(runtime_error, Class, Selector),
        #{node => Node, reason => Reason}
    ).

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

The single leaf both cross-node actor-ref paths route through (AGENTS.md
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
