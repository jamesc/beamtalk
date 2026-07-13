%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_recheck_worker).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Single-worker queue for ADR 0105 Phase 2's shape re-check (BT-2780).

`beamtalk_repl_loader:activate_module/3` fires on *every* ordinary
class-body install — far more often than the method-signature path's
`maybe_trigger_recheck/4`, which only runs on an explicit `>>` patch or
removal (see that function's doc). Running its shape re-check
(`beamtalk_repl_loader:maybe_trigger_shape_recheck/1`, in turn
`beamtalk_recheck:trigger_shape/2`) synchronously on the install's response
path measurably regressed a heavy sequential-reload scenario (the
`repl_protocol` e2e suite): the per-reload caller-cap's up-to-20 compiler
round trips, serialised in front of every subsequent REPL response, was
enough to trip a client-side timeout.

Moving to a bare `spawn/1` per reload fixes *that* regression but trades it
for a subtler one (found in adversarial review): with no cap on concurrently
spawned rechecks, a burst of shape-changing reloads can have many of these
processes hammer `beamtalk_compiler_server` (ADR 0022) at once — the same
single, request-serialising process that also carries the still-synchronous
method-signature recheck and ordinary editor/LSP diagnostic requests. That
does not remove the latency risk, it just relocates it from "this reload's
own response" onto "an unrelated concurrent request sharing the same
compiler port."

This module is the fix for *that*: a single dedicated `gen_server` whose
mailbox **is** the queue. `enqueue/1` is a `cast` — it never blocks the
caller — and `handle_cast/2` runs one reload's recheck to completion before
looking at the next queued message, so at most one shape re-check is ever
mid-flight through the compiler port from this path at a time (the same
order of magnitude as the synchronous method-signature path's own
footprint), no matter how many class-body installs happen back to back.
Reloads simply queue and drain in order; nothing here needs to know about
backpressure or cancellation because there is no unbounded fan-out left to
bound.

**BT-2856 / ADR 0107 Phase A** reuses this same queue for the leaf-change
re-check (`beamtalk_repl_loader:maybe_trigger_leaf_change_recheck/1`,
enqueued via `enqueue_leaf_change/1`) instead of a second dedicated worker:
`trigger_leaf_change/1` is at least as expensive a compiler-port sweep as a
shape re-check (a whole-image recompile, per its own doc), so it needs the
exact same "at most one in flight" guarantee, and the event that triggers
it (a class gaining its first subclass) is rarer still than an ordinary
shape-changing reload — sharing one mailbox/queue is strictly simpler than
running two independent single-worker queues for two ADR 0105 mechanisms
with the identical contention concern.

## Crash isolation

`beamtalk_repl_loader:maybe_trigger_shape_recheck_for_class/1` and
`maybe_trigger_leaf_change_recheck_for_class/1` are both already documented
as self-swallowing end-to-end (store read/compute, the `trigger_*/*` call,
and publish are each wrapped in their own `try/catch`), so this worker
should never see an exception escape either call. `handle_cast/2` wraps
both in `try/catch` anyway, defensively: an uncaught crash here would take
down this `gen_server` (permanent restart under `beamtalk_workspace_sup`,
matching every sibling ADR 0105 store), and because a crashed process's
mailbox is discarded on restart, any *other* reloads' rechecks still
queued behind the crashing one would be silently dropped rather than
retried — a plain `try/catch` costs nothing here and avoids ever exercising
that path.

## Session-only, never persisted

No state beyond OTP's own gen_server bookkeeping — mirrors every other
ADR 0105 store (`beamtalk_workspace_signature_store`,
`beamtalk_workspace_shape_store`, `beamtalk_workspace_findings_store`),
supervised under `beamtalk_workspace_sup`, REPL-mode only (there is no live
class-body reload path in run mode for this to guard).
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, enqueue/1, enqueue_leaf_change/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

-doc "Start the shape re-check worker (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Queue a shape re-check for `Classes` (the same list `activate_module/3`
passes through) and return immediately. See the moduledoc: this is a
`cast`, so the caller never waits on the compiler-port round trips a
genuine shape change triggers, and concurrent `enqueue/1` calls drain
strictly one at a time through this worker's mailbox.
""".
-spec enqueue([map()]) -> ok.
enqueue(Classes) ->
    gen_server:cast(?MODULE, {recheck, Classes}).

-doc """
BT-2856 / ADR 0107 Phase A: queue a leaf-change re-check for
`NewlyNonLeafSuperclasses` (the same list
`beamtalk_repl_loader:superclasses_losing_leaf_status/1` produces) and
return immediately — see the moduledoc's "reuses this same queue" note for
why this shares `enqueue/1`'s worker rather than getting its own.
""".
-spec enqueue_leaf_change([binary()]) -> ok.
enqueue_leaf_change(NewlyNonLeafSuperclasses) ->
    gen_server:cast(?MODULE, {leaf_change, NewlyNonLeafSuperclasses}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    {ok, undefined}.

handle_cast({leaf_change, Superclasses}, State) ->
    try
        beamtalk_repl_loader:maybe_trigger_leaf_change_recheck(Superclasses)
    catch
        %% Defensive only — see the moduledoc's "Crash isolation" section.
        %% maybe_trigger_leaf_change_recheck/1 is already self-swallowing, so
        %% this should be unreachable in practice.
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Leaf-change re-check worker: unexpected crash swallowed (queue continues)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            )
    end,
    {noreply, State};
handle_cast({recheck, Classes}, State) ->
    try
        beamtalk_repl_loader:maybe_trigger_shape_recheck(Classes)
    catch
        %% Defensive only — see the moduledoc's "Crash isolation" section.
        %% maybe_trigger_shape_recheck/1 is already self-swallowing, so this
        %% should be unreachable in practice.
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Shape re-check worker: unexpected crash swallowed (queue continues)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            )
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
