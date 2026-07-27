%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_parallel).

%%% **DDD Context:** Runtime Context

-moduledoc """
Parallel class implementation — block-based fan-out/join combinators (BT-2974).

Beamtalk sends are synchronous by default (ADR 0104): a `.` send blocks the
caller and types as the method's return value. This module gives that same
blocking contract to *fan-out* work, instead of exposing an awaitable
future/promise handle to Beamtalk code (the `Future` stub was deliberately
removed for exactly this reason — BT-1057, superseding the exploration in
BT-507). `all/1`, `all/2`, and `any/1` spawn one process per block, block the
calling process until the combinator is done, and return plain `Result`
values (`beamtalk_result:t()`) built with `beamtalk_result:from_tagged_tuple/1`
— no future handle ever escapes to Beamtalk code.

## Process model

Each block is spawned with `erlang:spawn_opt/2` using **both** `link` and
`monitor` (see `spawn_workers/1`):

- `link` means a worker dies automatically if the calling process dies —
  no orphaned worker processes are left running after the caller crashes,
  even mid-block (BT-2974 acceptance criteria: "no orphans").
- `monitor` lets the caller observe each worker's completion (or an
  unexpected crash that bypassed the worker's own `try`/`catch`) as an
  ordinary message, without a worker's exit signal being able to kill the
  caller.

A worker normally terminates with reason `normal` (it sends its result, then
its function returns) — a `normal` exit never propagates across a link, so
the link is harmless in the common case. The case that needs care is
*deliberately* killing a still-running worker (an `all:timeout:` deadline, or
a losing `any:` competitor): `exit(Pid, kill)` reaches the caller through the
link too (as reason `killed`) unless the link is removed first, so every
deliberate kill in this module calls `erlang:unlink/1` immediately before
`exit(Pid, kill)` (see `kill_pending/1`).

Every block runs inside a `try ... catch Class:Reason:Stack -> ...` in the
worker process (see `run_worker/3`), so a raised exception — including a
stray non-local-return throw from `^`, which cannot cross the process
boundary back into the block's enclosing method — always becomes a `Result
error:` for that slot. It never crashes the caller.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Class methods (canonical colon forms)
-export(['all:'/1, 'all:timeout:'/2, 'any:'/1]).

%% FFI shims for (Erlang beamtalk_parallel) / `self delegate` dispatch
-export([all/1, all/2, any/1]).

-type worker_result() :: beamtalk_result:t().
-type pending() :: #{pos_integer() => {pid(), reference()}}.

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

-doc """
Run each zero-argument block in its own linked+monitored process. Blocks the
caller until every block has finished (successfully or not). Returns one
`Result` per block, in the same order as `Blocks` — a block that raises
yields `Result error:` for its slot; the other blocks still run to
completion.
""".
-spec 'all:'([function()]) -> [worker_result()].
'all:'(Blocks) when is_list(Blocks) ->
    validate_blocks('all:', Blocks),
    all_impl(Blocks, infinity);
'all:'(Blocks) ->
    raise_type_error('all:', <<"Argument must be a List of zero-argument Blocks">>, #{
        got => Blocks
    }).

-doc """
Like `all:`, but with an overall wall-clock deadline (Integer milliseconds
or a Duration) measured from when `all:timeout:` is sent — not per block.

On timeout, every block still running is killed (`unlink` + `exit(Pid,
kill)`, so the kill cannot reach the caller) and its slot becomes `Result
error:` with a `timeout`-kind error; blocks that already finished keep their
real `Result`.
""".
-spec 'all:timeout:'([function()], integer() | beamtalk_duration:t()) -> [worker_result()].
'all:timeout:'(Blocks, #{'$beamtalk_class' := 'Duration'} = D) when is_list(Blocks) ->
    'all:timeout:'(Blocks, beamtalk_duration:'asMilliseconds'(D));
'all:timeout:'(Blocks, TimeoutMs) when
    is_list(Blocks), is_integer(TimeoutMs), TimeoutMs >= 0
->
    validate_blocks('all:timeout:', Blocks),
    all_impl(Blocks, TimeoutMs);
'all:timeout:'(Blocks, TimeoutMs) when is_list(Blocks) ->
    raise_type_error(
        'all:timeout:',
        <<"Timeout must be a non-negative Integer or a non-negative Duration">>,
        #{got => TimeoutMs}
    );
'all:timeout:'(Blocks, _TimeoutMs) ->
    raise_type_error(
        'all:timeout:', <<"Argument must be a List of zero-argument Blocks">>, #{got => Blocks}
    ).

-doc """
Run each zero-argument block in its own linked+monitored process; return the
first successful `Result` (whichever block finishes first with a value).
Every still-running block is killed as soon as a winner is found (`unlink` +
`exit(Pid, kill)`). If every block fails, returns `Result error:` wrapping a
`List` of the individual failure reasons, in input order.

Raises a `type_error` if `Blocks` is empty — "first of nothing" is not a
meaningful call.
""".
-spec 'any:'([function()]) -> worker_result().
'any:'([]) ->
    raise_type_error(
        'any:', <<"Argument must be a non-empty List of zero-argument Blocks">>, #{got => []}
    );
'any:'(Blocks) when is_list(Blocks) ->
    validate_blocks('any:', Blocks),
    any_impl(Blocks);
'any:'(Blocks) ->
    raise_type_error('any:', <<"Argument must be a List of zero-argument Blocks">>, #{
        got => Blocks
    }).

%%% ============================================================================
%%% FFI Shims
%%%
%%% `self delegate` / `(Erlang beamtalk_parallel) ...` dispatch derives the
%%% Erlang function name from the first keyword of the Beamtalk selector
%%% (stripping the trailing colon) — these shims bridge to the canonical
%%% colon-quoted implementations above.
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_parallel) all: blocks`".
-spec all([function()]) -> [worker_result()].
all(Blocks) -> 'all:'(Blocks).

-doc "FFI shim: `(Erlang beamtalk_parallel) all: blocks timeout: ms`".
-spec all([function()], integer() | beamtalk_duration:t()) -> [worker_result()].
all(Blocks, TimeoutMs) -> 'all:timeout:'(Blocks, TimeoutMs).

-doc "FFI shim: `(Erlang beamtalk_parallel) any: blocks`".
-spec any([function()]) -> worker_result().
any(Blocks) -> 'any:'(Blocks).

%%% ============================================================================
%%% Internal — shared worker spawn
%%% ============================================================================

-doc """
Spawn one linked+monitored worker process per block, numbered from 1 in
input order. Returns `[{Index, Pid, MonitorRef}]`.

`CallRef` is a fresh reference (one per `all:`/`all:timeout:`/`any:`
invocation) that every worker stamps onto its result message — see
`run_worker/4` for why.
""".
-spec spawn_workers([function()], reference()) -> [{pos_integer(), pid(), reference()}].
spawn_workers(Blocks, CallRef) ->
    Caller = self(),
    Indexed = lists:zip(lists:seq(1, length(Blocks)), Blocks),
    lists:map(
        fun({Idx, Block}) ->
            {Pid, Ref} = erlang:spawn_opt(
                fun() -> run_worker(Caller, CallRef, Idx, Block) end, [link, monitor]
            ),
            {Idx, Pid, Ref}
        end,
        Indexed
    ).

-doc """
Worker body: evaluate `Block`, catching any exception, and send
`{CallRef, Idx, Result}` back to `Caller`. `Result` is always a fully formed
`beamtalk_result:t()` — the caller never has to interpret a raw exception
from a worker that reached this far (only an unhandled kill/crash that
bypasses this `catch` shows up as a `'DOWN'` instead — see `gather_all/4`
and `gather_any/4`).

Messages are tagged with `CallRef` (rather than sent as `{self(), Idx,
Result}`) so `gather_all/4`/`gather_any/4` can pattern-match on an exact,
per-invocation reference instead of an unbound sender/index pair — the
caller may be a long-lived process (e.g. an actor) with unrelated messages
already in its mailbox that could otherwise coincidentally match a bare
`{_Pid, Idx, Result}` shape.
""".
-spec run_worker(pid(), reference(), pos_integer(), function()) -> ok.
run_worker(Caller, CallRef, Idx, Block) ->
    Result =
        try
            Value = Block(),
            beamtalk_result:from_tagged_tuple({ok, Value})
        catch
            Class:Reason:Stack ->
                ExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack),
                beamtalk_result:from_tagged_tuple({error, ExObj})
        end,
    Caller ! {CallRef, Idx, Result},
    ok.

-doc "Validate that every element of `Blocks` is a zero-argument Block (fun/0).".
-spec validate_blocks(atom(), [term()]) -> ok.
validate_blocks(Selector, Blocks) ->
    case lists:all(fun(B) -> is_function(B, 0) end, Blocks) of
        true ->
            ok;
        false ->
            raise_type_error(
                Selector, <<"Every element must be a zero-argument Block">>, #{got => Blocks}
            )
    end.

-doc """
Build a `Ref -> Idx` reverse map of `Pending`, so a `receive` clause can
guard on `is_map_key(Ref, PendingByRef)` — a real guard BIF — instead of an
unbound `Ref`/`_Pid` that would match (and silently discard) an unrelated
`'DOWN'` message already destined for this process for some other reason
(e.g. a monitor the caller itself set up before calling into `Parallel`).
""".
-spec pending_by_ref(pending()) -> #{reference() => pos_integer()}.
pending_by_ref(Pending) ->
    maps:fold(fun(Idx, {_Pid, Ref}, Acc) -> Acc#{Ref => Idx} end, #{}, Pending).

-doc """
Kill every still-pending worker. Each is unlinked first so the kill signal
cannot reach the caller through the link, then demonitored (flushing any
in-flight `'DOWN'`) before the kill so a race can't deliver a stray message.

Does **not** drain a worker's `{Pid, Idx, Result}` message on its own — a
worker that finished (sent its result) in the instant before being killed
can still have that message sitting in the caller's mailbox. Callers pair
this with `drain_pending_messages/1` so no stray message survives into the
caller's process after `all:timeout:`/`any:` returns.
""".
-spec kill_pending(pending()) -> ok.
kill_pending(Pending) ->
    maps:foreach(
        fun(_Idx, {Pid, Ref}) ->
            erlang:unlink(Pid),
            erlang:demonitor(Ref, [flush]),
            exit(Pid, kill)
        end,
        Pending
    ).

-doc """
Non-blocking drain of any already-in-flight `{CallRef, Idx, Result}` message
for workers in `Pending`. A worker killed at the exact moment it finished may
have already enqueued its result before the kill signal took effect; without
this, that message would sit in the caller's mailbox indefinitely after
`all:timeout:`/`any:` returns — surprising a caller that is itself a
long-lived process (e.g. an actor) with an unexpected message later. Always
called right after `kill_pending/1`, with the same `Pending` map.

**Residual race**: on a multi-scheduler BEAM node, `exit(Pid, kill)` only
enqueues the kill signal — a worker running concurrently on another
scheduler can still execute `Caller ! {CallRef, Idx, Result}` in the window
between that enqueue and the signal actually being processed, i.e. *after*
this drain's `receive ... after 0` has already scanned the mailbox and found
nothing. The result message then lands after the drain returns. `CallRef`
scoping means it can never be misread as belonging to a later `Parallel`
call, so this is not a correctness bug, but a long-lived caller (an actor)
can accumulate a handful of these stray tuples in its mailbox over many
`all:timeout:`/`any:` calls that happen to race a kill this way.
""".
-spec drain_pending_messages(reference(), pending()) -> ok.
drain_pending_messages(CallRef, Pending) ->
    maps:foreach(fun(Idx, {_Pid, _Ref}) -> drain_one_message(CallRef, Idx) end, Pending).

-spec drain_one_message(reference(), pos_integer()) -> ok.
drain_one_message(CallRef, Idx) ->
    receive
        {CallRef, Idx, _Result} -> ok
    after 0 -> ok
    end.

%%% ============================================================================
%%% Internal — all:
%%% ============================================================================

-spec all_impl([function()], timeout()) -> [worker_result()].
all_impl([], _Timeout) ->
    [];
all_impl(Blocks, Timeout) ->
    CallRef = erlang:make_ref(),
    Workers = spawn_workers(Blocks, CallRef),
    Pending = maps:from_list([{Idx, {Pid, Ref}} || {Idx, Pid, Ref} <- Workers]),
    Deadline = deadline_for(Timeout),
    ResultsByIndex = gather_all(CallRef, Pending, Deadline, #{}),
    [maps:get(Idx, ResultsByIndex) || {Idx, _Pid, _Ref} <- Workers].

-spec deadline_for(timeout()) -> infinity | integer().
deadline_for(infinity) -> infinity;
deadline_for(Ms) -> erlang:monotonic_time(millisecond) + Ms.

-doc "Milliseconds left before `Deadline`, clamped to zero — suitable as a `receive ... after` value.".
-spec time_left(infinity | integer()) -> timeout().
time_left(infinity) ->
    infinity;
time_left(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).

-spec gather_all(reference(), pending(), infinity | integer(), #{pos_integer() => worker_result()}) ->
    #{pos_integer() => worker_result()}.
gather_all(_CallRef, Pending, _Deadline, Acc) when map_size(Pending) =:= 0 ->
    Acc;
gather_all(CallRef, Pending, Deadline, Acc) ->
    PendingByRef = pending_by_ref(Pending),
    receive
        {CallRef, Idx, Result} when is_map_key(Idx, Pending) ->
            {_WPid, Ref} = maps:get(Idx, Pending),
            erlang:demonitor(Ref, [flush]),
            gather_all(CallRef, maps:remove(Idx, Pending), Deadline, Acc#{Idx => Result});
        {'DOWN', Ref, process, _Pid, Reason} when is_map_key(Ref, PendingByRef) ->
            %% Worker vanished without sending a result (killed by something
            %% other than us, or an exit signal that bypassed run_worker/4's
            %% own catch-all). The is_map_key/2 guard (rather than an unbound
            %% Ref matched against every 'DOWN') ensures a 'DOWN' belonging to
            %% some other monitor the caller set up is left in the mailbox
            %% untouched, instead of being silently consumed here.
            Idx = maps:get(Ref, PendingByRef),
            ExObj = beamtalk_exception_handler:ensure_wrapped(exit, Reason, []),
            Result = beamtalk_result:from_tagged_tuple({error, ExObj}),
            gather_all(CallRef, maps:remove(Idx, Pending), Deadline, Acc#{Idx => Result})
    after time_left(Deadline) ->
        kill_pending(Pending),
        drain_pending_messages(CallRef, Pending),
        TimedOut = maps:map(fun(_Idx, _WorkerRef) -> make_timeout_result() end, Pending),
        maps:merge(Acc, TimedOut)
    end.

-spec make_timeout_result() -> worker_result().
make_timeout_result() ->
    Error0 = beamtalk_error:new(timeout, 'Parallel'),
    Error1 = beamtalk_error:with_selector(Error0, 'all:timeout:'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<"Increase the timeout, or make the block itself faster/cancellable">>
    ),
    beamtalk_result:from_tagged_tuple({error, Error2}).

%%% ============================================================================
%%% Internal — any:
%%% ============================================================================

-spec any_impl([function()]) -> worker_result().
any_impl(Blocks) ->
    CallRef = erlang:make_ref(),
    Workers = spawn_workers(Blocks, CallRef),
    Pending = maps:from_list([{Idx, {Pid, Ref}} || {Idx, Pid, Ref} <- Workers]),
    IndexOrder = [Idx || {Idx, _Pid, _Ref} <- Workers],
    gather_any(CallRef, Pending, IndexOrder, #{}).

-spec gather_any(reference(), pending(), [pos_integer()], #{pos_integer() => term()}) ->
    worker_result().
gather_any(_CallRef, Pending, IndexOrder, Errors) when map_size(Pending) =:= 0 ->
    %% Every block failed — aggregate the reasons (already-wrapped Exception
    %% objects) into a List, in input order. `makeError:` stores the reason
    %% as-is (unlike `from_tagged_tuple/1`, which would try to re-wrap the
    %% whole List as if it were a single raw exception reason).
    Reasons = [maps:get(Idx, Errors) || Idx <- IndexOrder],
    beamtalk_result:'makeError:'(Reasons);
gather_any(CallRef, Pending, IndexOrder, Errors) ->
    PendingByRef = pending_by_ref(Pending),
    receive
        {CallRef, Idx, #{'isOk' := true} = Result} when is_map_key(Idx, Pending) ->
            {_WPid, Ref} = maps:get(Idx, Pending),
            erlang:demonitor(Ref, [flush]),
            Losers = maps:remove(Idx, Pending),
            kill_pending(Losers),
            drain_pending_messages(CallRef, Losers),
            Result;
        {CallRef, Idx, #{'isOk' := false, 'errReason' := Reason}} when is_map_key(Idx, Pending) ->
            {_WPid, Ref} = maps:get(Idx, Pending),
            erlang:demonitor(Ref, [flush]),
            gather_any(CallRef, maps:remove(Idx, Pending), IndexOrder, Errors#{Idx => Reason});
        {'DOWN', Ref, process, _Pid, Reason} when is_map_key(Ref, PendingByRef) ->
            %% See gather_all/4's matching clause for why this is guarded by
            %% is_map_key/2 instead of matching an unbound Ref.
            Idx = maps:get(Ref, PendingByRef),
            ExObj = beamtalk_exception_handler:ensure_wrapped(exit, Reason, []),
            gather_any(CallRef, maps:remove(Idx, Pending), IndexOrder, Errors#{Idx => ExObj})
    end.

%%% ============================================================================
%%% Internal — errors
%%% ============================================================================

-spec raise_type_error(atom(), binary(), map()) -> no_return().
raise_type_error(Selector, Hint, Details) ->
    Error0 = beamtalk_error:new(type_error, 'Parallel'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    Error3 = beamtalk_error:with_details(Error2, Details),
    beamtalk_error:raise(Error3).
