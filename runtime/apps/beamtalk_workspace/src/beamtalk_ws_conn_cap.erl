%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_conn_cap).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Caps on live WebSocket connections (BT-5127, BT-5185).

Ranch's `max_connections` counts a socket only until it is upgraded:
`cowboy_websocket:takeover/7` calls `ranch:remove_connection/1`, so an
upgraded WebSocket no longer counts and the listener-level cap does not bound
WebSockets at all. This module counts them itself, in two budgets:

* **pre-auth** (`ws_max_preauth_connections`, default 32): sockets that have
  not authenticated yet. When it is full, a new socket evicts the *oldest*
  unauthenticated one instead of being refused, so idle unauthenticated
  sockets cannot lock a real client out: an attacker would have to open them
  faster than the client's one-round-trip auth completes. The evicted handler
  gets `{beamtalk_ws_conn_cap, evicted}` and closes.
* **authenticated** (`ws_max_connections`, default 256): a socket moves here
  on successful auth (`promote/0`). Authenticated sockets are never evicted;
  over this cap the socket is refused.

The handler takes a pre-auth slot in `websocket_init/1` and gives back
whatever it holds in `terminate/3` (both run in the process that owns the
socket). Counts are atomic counters (`ets:update_counter/4`), so two sockets
racing for the last slot cannot both lose (or both win). Rows are keyed by
pid; a handler killed without running `terminate/3` would leak its row, so a
full budget first prunes rows whose pid is dead. A handler killed from
outside in the instant between reserving its count and inserting its row
leaves one count with no row, which pruning cannot see; the budget is then one
slot smaller until the table's owner restarts. That needs an external
`exit(Pid, kill)` landing between two consecutive ETS calls, and costs one
slot, so it is accepted rather than reconciled under concurrent reservations.

The table is owned by `beamtalk_repl_server` (`new/0`). If that process
restarts, the table is recreated empty while old handlers may live on, so a
handler monitors the owner (`monitor_owner/0`) and closes when it goes: the
new table then starts consistent.
""".

-export([
    new/0,
    acquire/0,
    promote/0,
    release/0,
    limit/0,
    preauth_limit/0,
    count/0,
    count/1,
    monitor_owner/0
]).

-define(TABLE, beamtalk_ws_conn_cap).
-define(DEFAULT_LIMIT, 256).
-define(DEFAULT_PREAUTH_LIMIT, 32).

-type class() :: pre_auth | authenticated.

-doc "Create the slot table. Call from the process that should own it.".
-spec new() -> ok.
new() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
            true = ets:insert(?TABLE, [{{count, pre_auth}, 0}, {{count, authenticated}, 0}]),
            ok;
        _ ->
            ok
    end.

-doc "Authenticated-connection cap (`beamtalk_workspace` env `ws_max_connections`).".
-spec limit() -> pos_integer().
limit() ->
    env_limit(ws_max_connections, ?DEFAULT_LIMIT).

-doc "Unauthenticated-connection cap (env `ws_max_preauth_connections`).".
-spec preauth_limit() -> pos_integer().
preauth_limit() ->
    env_limit(ws_max_preauth_connections, ?DEFAULT_PREAUTH_LIMIT).

env_limit(Key, Default) ->
    case application:get_env(beamtalk_workspace, Key, Default) of
        N when is_integer(N), N > 0 -> N;
        _ -> Default
    end.

-doc "Live WebSocket connections holding a slot, both budgets.".
-spec count() -> non_neg_integer().
count() ->
    count(pre_auth) + count(authenticated).

-doc "Live WebSocket connections holding a slot in one budget.".
-spec count(class()) -> non_neg_integer().
count(Class) ->
    case ets:whereis(?TABLE) of
        undefined -> 0;
        _ -> ets:lookup_element(?TABLE, {count, Class}, 2, 0)
    end.

-doc """
Take a pre-auth slot for the calling process. When the pre-auth budget is
full this evicts the oldest unauthenticated holder; it is
`{error, at_capacity}` only when there is none to evict.
""".
-spec acquire() -> ok | {error, at_capacity}.
acquire() ->
    case ets:whereis(?TABLE) of
        undefined ->
            %% No listener owns the table (handler used outside the server):
            %% nothing to count against.
            ok;
        _ ->
            case take_slot(pre_auth, preauth_limit()) of
                ok -> ok;
                {error, at_capacity} -> evict_oldest_preauth()
            end
    end.

-doc """
Move the calling process from the pre-auth budget to the authenticated one,
after a successful auth. `{error, at_capacity}` when the authenticated budget
is full; `{error, evicted}` when the slot was taken by an eviction first.
Either way the caller keeps nothing it must release beyond `release/0`.
""".
-spec promote() -> ok | {error, at_capacity | evicted}.
promote() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ok;
        _ ->
            case reserve(authenticated, limit()) of
                {error, at_capacity} ->
                    {error, at_capacity};
                ok ->
                    %% Only a pre-auth row moves; an evictor may have taken it.
                    Self = self(),
                    case
                        ets:select_replace(?TABLE, [
                            {{Self, pre_auth, '$1'}, [], [{{Self, authenticated, '$1'}}]}
                        ])
                    of
                        1 ->
                            _ = ets:update_counter(?TABLE, {count, pre_auth}, -1),
                            ok;
                        0 ->
                            _ = ets:update_counter(?TABLE, {count, authenticated}, -1),
                            {error, evicted}
                    end
            end
    end.

-doc "Give the calling process's slot back, whichever budget (idempotent).".
-spec release() -> ok.
release() ->
    case ets:whereis(?TABLE) of
        undefined -> ok;
        _ -> drop_row(self())
    end.

-doc """
Monitor the table's owner, so a handler can close when the owner (and with
it the table) goes away. `undefined` when there is no table.
""".
-spec monitor_owner() -> reference() | undefined.
monitor_owner() ->
    case ets:whereis(?TABLE) of
        undefined ->
            undefined;
        Tid ->
            case ets:info(Tid, owner) of
                Owner when is_pid(Owner) -> erlang:monitor(process, Owner);
                _ -> undefined
            end
    end.

%%% Internal

%% Reserve a count in `Class` and record the caller's row.
take_slot(Class, Limit) ->
    case reserve(Class, Limit) of
        ok ->
            true = ets:insert(?TABLE, {self(), Class, erlang:monotonic_time()}),
            ok;
        Error ->
            Error
    end.

%% Atomic: the counter is the only arbiter, so two callers racing for the
%% last slot get exactly one `ok`. Over the limit, prune dead holders once
%% and try again.
reserve(Class, Limit) ->
    case try_reserve(Class, Limit) of
        ok ->
            ok;
        full ->
            prune_dead(),
            case try_reserve(Class, Limit) of
                ok -> ok;
                full -> {error, at_capacity}
            end
    end.

try_reserve(Class, Limit) ->
    Key = {count, Class},
    case ets:update_counter(?TABLE, Key, 1, {Key, 0}) of
        N when N =< Limit ->
            ok;
        _ ->
            _ = ets:update_counter(?TABLE, Key, -1),
            full
    end.

%% Remove `Pid`'s row and give its count back. `ets:take/2` is atomic, so a
%% row is counted down exactly once whoever removes it.
drop_row(Pid) ->
    case ets:take(?TABLE, Pid) of
        [{Pid, Class, _Since}] ->
            _ = ets:update_counter(?TABLE, {count, Class}, -1),
            ok;
        [] ->
            ok
    end.

prune_dead() ->
    Holders = ets:select(?TABLE, [{{'$1', '_', '_'}, [{is_pid, '$1'}], ['$1']}]),
    lists:foreach(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> drop_row(Pid)
            end
        end,
        Holders
    ).

%% The pre-auth budget is full: hand the oldest unauthenticated holder's slot
%% to the caller. The conditional delete only matches a row still in
%% `pre_auth`, so a holder that has just authenticated is never evicted, and
%% two evictors never take the same row.
evict_oldest_preauth() ->
    Candidates = lists:sort(
        ets:select(?TABLE, [{{'$1', pre_auth, '$2'}, [{is_pid, '$1'}], [{{'$2', '$1'}}]}])
    ),
    evict_first(Candidates).

evict_first([]) ->
    {error, at_capacity};
evict_first([{_Since, Victim} | Rest]) when Victim =:= self() ->
    evict_first(Rest);
evict_first([{_Since, Victim} | Rest]) ->
    case ets:select_delete(?TABLE, [{{Victim, pre_auth, '_'}, [], [true]}]) of
        1 ->
            %% The count is unchanged: the slot passes from Victim to us.
            true = ets:insert(?TABLE, {self(), pre_auth, erlang:monotonic_time()}),
            Victim ! {?MODULE, evicted},
            ok;
        0 ->
            evict_first(Rest)
    end.
