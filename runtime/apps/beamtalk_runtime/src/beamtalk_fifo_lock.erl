%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_fifo_lock).

%%% **DDD Context:** Actor System Context

-moduledoc """
Node-local FIFO mutex keyed by any term, with a bounded wait (BT-3994).

Built for `beamtalk_supervisor:supervise/2`, which must serialise racing
starts of one supervisor class. It replaces `global:trans/4`, which retries a
held lock with randomised back-off (so a caller can wait seconds past the
release) and waits forever behind a holder whose critical section hangs.

## Mechanism

No server process: a public `ordered_set` ETS table holds one row per caller,
`{{Key, Seq}, Pid, Ref}`, where `Seq` is a monotonic unique integer taken on
arrival. The row with the smallest `Seq` for a key is the holder; the rest
queue behind it in arrival order.

  * **Acquire** inserts the caller's row, then waits until it is the head.
  * **Release** deletes the holder's row and sends `{Ref, lock_released}` to
    the new head, which wakes at once (no back-off).
  * **Holder death**: every waiter monitors the current head. A `'DOWN'`
    deletes exactly that row (`ets:delete_object/2`, so a row the holder
    already released is untouched) and the queue moves on.
  * **Bounded wait**: a waiter that reaches its deadline removes its own row,
    passes the turn on to whoever is now the head (it may have become the
    head in the meantime) and returns `{error, timeout}`.

Waiters also re-check the head every `?POLL_MS`, so a head that changed
without notifying them (it was released to another waiter, who then died) is
re-monitored. The critical section runs in the caller's own process.
""".

-export([with_lock/3, queue/1]).

-define(TABLE, beamtalk_fifo_locks).
-define(POLL_MS, 200).

-type entry() :: {{term(), integer()}, pid(), reference()}.
-type deadline() :: integer() | infinity.
-type watch() :: {entry(), reference()} | undefined.

-doc """
Run `Fun` while holding the node-local lock `Key`.

Callers are served in arrival order. Returns `{ok, Fun()}`, or
`{error, timeout}` when the lock could not be taken within `TimeoutMs`
(`Fun` is then not run). The lock is released when `Fun` returns or raises;
if the caller dies while holding it, the next waiter takes over.
""".
-spec with_lock(term(), timeout(), fun(() -> T)) -> {ok, T} | {error, timeout}.
with_lock(Key, TimeoutMs, Fun) ->
    case acquire(Key, TimeoutMs) of
        {ok, Entry} ->
            try
                {ok, Fun()}
            after
                release(Entry)
            end;
        {error, timeout} = Timeout ->
            Timeout
    end.

-doc "The pids queued on `Key`, holder first (for diagnostics and tests).".
-spec queue(term()) -> [pid()].
queue(Key) ->
    ensure_table(),
    [Pid || {_, Pid, _} <- lists:sort(ets:match_object(?TABLE, {{Key, '_'}, '_', '_'}))].

%%% ============================================================================
%%% Internal
%%% ============================================================================

-spec acquire(term(), timeout()) -> {ok, entry()} | {error, timeout}.
acquire(Key, TimeoutMs) ->
    ensure_table(),
    Entry = {{Key, erlang:unique_integer([monotonic, positive])}, self(), make_ref()},
    true = ets:insert(?TABLE, Entry),
    Deadline =
        case TimeoutMs of
            infinity -> infinity;
            _ -> now_ms() + TimeoutMs
        end,
    wait(Entry, undefined, Deadline).

-spec wait(entry(), watch(), deadline()) -> {ok, entry()} | {error, timeout}.
wait({{Key, _}, _, Ref} = Entry, Watch0, Deadline) ->
    case head(Key) of
        Entry ->
            unwatch(Watch0),
            flush(Ref),
            {ok, Entry};
        none ->
            %% Our own row is gone (the table was recreated): queue again.
            unwatch(Watch0),
            true = ets:insert(?TABLE, Entry),
            wait(Entry, undefined, Deadline);
        Head ->
            {Watched, MRef} = Watch = watch(Head, Watch0),
            receive
                {Ref, lock_released} ->
                    wait(Entry, Watch, Deadline);
                {'DOWN', MRef, process, _, _} ->
                    %% The row we watched belongs to a dead process: drop it.
                    _ = ets:delete_object(?TABLE, Watched),
                    wait(Entry, undefined, Deadline)
            after poll_ms(Deadline) ->
                case expired(Deadline) of
                    false ->
                        wait(Entry, Watch, Deadline);
                    true ->
                        unwatch(Watch),
                        give_up(Entry)
                end
            end
    end.

%% Leave the queue at the deadline. If we became the head after the last
%% check, pass the turn on so the next waiter is not left behind a gap.
-spec give_up(entry()) -> {error, timeout}.
give_up({{Key, _}, _, Ref} = Entry) ->
    _ = ets:delete_object(?TABLE, Entry),
    notify_head(Key),
    flush(Ref),
    {error, timeout}.

-spec release(entry()) -> ok.
release({{Key, _}, _, Ref} = Entry) ->
    _ = ets:delete_object(?TABLE, Entry),
    notify_head(Key),
    flush(Ref).

-spec notify_head(term()) -> ok.
notify_head(Key) ->
    case head(Key) of
        {_, Pid, Ref} ->
            Pid ! {Ref, lock_released},
            ok;
        none ->
            ok
    end.

%% The row with the smallest sequence number for `Key`, or `none`.
-spec head(term()) -> entry() | none.
head(Key) ->
    case ets:next(?TABLE, {Key, 0}) of
        {Key, _} = Slot ->
            case ets:lookup(?TABLE, Slot) of
                [Entry] -> Entry;
                %% Deleted between `next` and `lookup`: look again.
                [] -> head(Key)
            end;
        _ ->
            none
    end.

%% Monitor the head's process, keeping the existing monitor when the head has
%% not changed.
-spec watch(entry(), watch()) -> {entry(), reference()}.
watch(Head, {Head, _} = Watch) ->
    Watch;
watch({_, Pid, _} = Head, Watch0) ->
    unwatch(Watch0),
    {Head, erlang:monitor(process, Pid)}.

-spec unwatch(watch()) -> ok.
unwatch(undefined) ->
    ok;
unwatch({_, MRef}) ->
    erlang:demonitor(MRef, [flush]),
    ok.

-spec flush(reference()) -> ok.
flush(Ref) ->
    receive
        {Ref, lock_released} -> flush(Ref)
    after 0 -> ok
    end.

-spec poll_ms(deadline()) -> non_neg_integer().
poll_ms(infinity) ->
    ?POLL_MS;
poll_ms(Deadline) ->
    max(0, min(?POLL_MS, Deadline - now_ms())).

-spec expired(deadline()) -> boolean().
expired(infinity) -> false;
expired(Deadline) -> now_ms() >= Deadline.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).

%% Public, so any caller can queue and any waiter can drop a dead holder's
%% row; heir'd to `beamtalk_ets_heir` so it outlives whichever process created
%% it.
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined ->
            try
                _ = ets:new(
                    ?TABLE,
                    [named_table, public, ordered_set | beamtalk_ets_heir:heir_option()]
                ),
                ok
            catch
                %% Another caller created it first.
                error:badarg -> ok
            end;
        _ ->
            ok
    end.
