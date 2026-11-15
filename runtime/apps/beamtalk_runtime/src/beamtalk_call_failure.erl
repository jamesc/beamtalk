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

-export([classify/2, unreachable_node/2, erpc/5, tolerate_gone/2, tolerate_gone/3]).

-export_type([classification/0, erpc_error/0, gone_opts/0]).

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

-type erpc_error() :: {node_down, node()} | timeout | {other, term()}.

-doc """
`erpc:call/5` with its failure modes folded into one tagged result, so the
remote-call sites supply only their own error builder.

- `{ok, Value}` — the remote call returned `Value`.
- `{error, {node_down, Node}}` — `{erpc, noconnection}`.
- `{error, timeout}` — `{erpc, timeout}`.
- `{error, {other, Reason}}` — every other `erpc` failure: `{erpc, R}` becomes
  `{other, {erpc, R}}`; a remote error or exit (`{exception, R, _}` /
  `exit:{exception, R}`) becomes `{other, R}`.

A remote `throw` is not caught: it propagates, as it did from the inlined
`try`/`catch` blocks this replaces. The caller handles `{ok, Value}` *outside*
this call, so an exception raised while using the value is never mistaken for
a remote failure.
""".
-spec erpc(node(), module(), atom(), [term()], timeout()) ->
    {ok, term()} | {error, erpc_error()}.
erpc(Node, M, F, A, Timeout) ->
    try erpc:call(Node, M, F, A, Timeout) of
        Value -> {ok, Value}
    catch
        error:{erpc, _} = Erpc ->
            case classify(error, Erpc) of
                {unreachable, noconnection} -> {error, {node_down, Node}};
                timeout -> {error, timeout};
                _ -> {error, {other, Erpc}}
            end;
        error:{exception, Reason, _Stack} ->
            {error, {other, Reason}};
        exit:{exception, Reason} ->
            {error, {other, Reason}}
    end.

-type gone_opts() :: #{
    stopping => boolean(),
    timeout => boolean()
}.

-doc "`tolerate_gone/3` with the default policy: only a dead or unreachable callee.".
-spec tolerate_gone(fun(() -> T), D) -> T | D when T :: term(), D :: term().
tolerate_gone(Fun, Default) ->
    tolerate_gone(Fun, Default, #{}).

-doc """
Run `Fun` (a call to another process); if the callee is gone, return `Default`.

The base policy is `classify/2`: `exit` reasons classified `dead` (`noproc`)
or `{unreachable, _}` (`nodedown`, `noconnection`) are tolerated. Everything
else propagates, including every `error:` and `throw:`.

**Contract:** `Fun` must be a call into another process (e.g. `gen_server:call/2,3`,
`gen_server:stop/3`, or a module API that wraps one), never code that runs a
Beamtalk block. The class-variable
catcher audit (ADR 0130) marks this one `catch` `not-applicable-other-process`
on that basis, and the lint cannot see a caller's `fun`.

Options keep deliberate differences explicit:

- `stopping => true` also tolerates the callee exiting normally or shutting
  down during the call (`normal`, `shutdown`, `{normal, _}`, `{shutdown, _}`,
  `{{normal, _}, _}`, `{{shutdown, _}, _}`).
- `timeout => true` also tolerates the call timing out (`classify/2` →
  `timeout`).
""".
-spec tolerate_gone(fun(() -> T), D, gone_opts()) -> T | D when T :: term(), D :: term().
tolerate_gone(Fun, Default, Opts) ->
    %% bt-catcher-audit: not-applicable-other-process - contract (see -doc): Fun is a call
    %% into another process (gen_server:call/stop), never a Beamtalk block
    try
        Fun()
    catch
        exit:Reason:Stack ->
            case is_gone(Reason, Opts) of
                true -> Default;
                false -> erlang:raise(exit, Reason, Stack)
            end
    end.

-spec is_gone(term(), gone_opts()) -> boolean().
is_gone(Reason, Opts) ->
    case classify(exit, Reason) of
        dead -> true;
        {unreachable, _} -> true;
        timeout -> maps:get(timeout, Opts, false);
        other -> maps:get(stopping, Opts, false) andalso is_stopping(Reason)
    end.

-spec is_stopping(term()) -> boolean().
is_stopping(normal) -> true;
is_stopping(shutdown) -> true;
is_stopping({normal, _}) -> true;
is_stopping({shutdown, _}) -> true;
is_stopping({{normal, _}, _}) -> true;
is_stopping({{shutdown, _}, _}) -> true;
is_stopping(_) -> false.
