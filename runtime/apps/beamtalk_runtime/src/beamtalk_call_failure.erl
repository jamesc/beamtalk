%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_call_failure).
-moduledoc """
The one classifier for "a call to another process failed" (BT-4676).

`classify/2` recognises the exit/error shapes an OTP `gen_server:call` (or an
`erpc` call) can fail with and answers one of:

- `dead` — the process does not exist (`noproc`).
- `{unreachable, Why}` — its node could not be reached (`nodedown`,
  `noconnection`, or an `erpc` equivalent). `Why` is `nodedown | noconnection`.
- `timeout` — the call ran out of time.
- `other` — anything else.

**Explicit policy for `shutdown` / `killed` / `normal`.** A callee that exits
*during* a call (`{shutdown, _}`, `{killed, _}`, `{normal, _}`, a custom stop
reason) classifies as `other`: whether that means "the callee is gone" depends
on who the callee is. Each caller maps `other` to its own error:

- An actor send (`beamtalk_actor:sync_call/5`) has only the actor to talk to,
  so any `exit:{_, _}` from the call means the actor is unavailable and
  becomes `actor_dead`.
- A supervisor call (`beamtalk_supervisor:with_live_supervisor/3`) runs child
  code under the supervisor, so a `{shutdown, _}` may come from that code and
  is re-raised unchanged.
""".

-export([classify/2, unreachable_node/2]).

-export_type([classification/0]).

-type classification() ::
    dead | {unreachable, nodedown | noconnection} | timeout | other.

-doc "Classify an exception `Class:Reason` raised by a call to another process.".
-spec classify(error | exit | throw, term()) -> classification().
classify(exit, noproc) -> dead;
classify(exit, {noproc, _}) -> dead;
classify(exit, {{nodedown, _Node}, _}) -> {unreachable, nodedown};
classify(exit, {nodedown, _Node}) -> {unreachable, nodedown};
classify(exit, noconnection) -> {unreachable, noconnection};
classify(exit, {noconnection, _}) -> {unreachable, noconnection};
classify(exit, timeout) -> timeout;
classify(exit, {timeout, _}) -> timeout;
classify(error, {erpc, noconnection}) -> {unreachable, noconnection};
classify(error, {erpc, timeout}) -> timeout;
classify(_Class, _Reason) -> other.

-doc """
The node a `{unreachable, _}` failure names (`{{nodedown, Node}, _}` /
`{nodedown, Node}`), or `Default` when the reason carries none.
""".
-spec unreachable_node(term(), node()) -> node().
unreachable_node({{nodedown, Node}, _}, _Default) when is_atom(Node) -> Node;
unreachable_node({nodedown, Node}, _Default) when is_atom(Node) -> Node;
unreachable_node(_Reason, Default) -> Default.
