%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_table).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Session registry backed by a named ETS table.

Single owner of the `beamtalk_sessions` ETS table.  All create, insert,
delete, and lookup operations go through this module so that:

  * The table atom is not scattered across every op handler.
  * Tests can pre-populate the table via `insert/2` without touching ETS
    internals.
  * The common "resolve session ID → live PID" pattern lives in one place.
""".

-export([new/0, insert/2, delete/1, lookup/1, lookup_alive/1, resolve_pid/2]).

-doc """
Create the session ETS table if it does not already exist.
Idempotent — safe to call on supervisor restart or test re-runs.
""".
-spec new() -> ok.
new() ->
    case ets:whereis(beamtalk_sessions) of
        undefined ->
            ets:new(beamtalk_sessions, [named_table, public, {read_concurrency, true}]);
        _ ->
            ok
    end,
    ok.

-doc "Register a session PID under its ID.".
-spec insert(binary(), pid()) -> true.
insert(SessionId, Pid) when is_binary(SessionId), is_pid(Pid) ->
    ets:insert(beamtalk_sessions, {SessionId, Pid}).

-doc "Remove a session from the registry.".
-spec delete(binary()) -> true.
delete(SessionId) when is_binary(SessionId) ->
    ets:delete(beamtalk_sessions, SessionId).

-doc """
Look up a session by ID.
Returns `{ok, Pid}` if the entry exists (the process may have died),
or `error` if not found.
""".
-spec lookup(binary()) -> {ok, pid()} | error.
lookup(SessionId) when is_binary(SessionId) ->
    case ets:lookup(beamtalk_sessions, SessionId) of
        [{_, Pid}] -> {ok, Pid};
        [] -> error
    end.

-doc """
Look up a session by ID, returning the PID only if its process is alive.

BT-2366 (ADR 0081 Phase 3): the liveness-checked variant of `lookup/1`.  Unlike
`lookup/1` (which can return a dead PID) and `resolve_pid/2` (which substitutes
a default), this returns `{ok, Pid}` only when the registered process is alive,
and `error` for not-found, dead-process, or ETS-unavailable cases.  Used by the
factory `withId/1` primitive so a captured `Session` never carries a dead PID.
""".
-spec lookup_alive(binary()) -> {ok, pid()} | error.
lookup_alive(SessionId) when is_binary(SessionId) ->
    try
        case ets:lookup(beamtalk_sessions, SessionId) of
            [{_, Pid}] when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true -> {ok, Pid};
                    false -> error
                end;
            _ ->
                error
        end
    catch
        _:_ -> error
    end.

-doc """
Resolve a session ID to a live PID, falling back to `Default`.

If `IdOrUndefined` is `undefined`, returns `Default` unchanged.
If the session is found and its process is alive, returns that PID.
Otherwise (not found, dead process, ETS unavailable) returns `Default`.
""".
-spec resolve_pid(binary() | undefined, pid()) -> pid().
resolve_pid(undefined, Default) ->
    Default;
resolve_pid(SessionId, Default) when is_binary(SessionId) ->
    try
        case ets:lookup(beamtalk_sessions, SessionId) of
            [{_, Pid}] when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true -> Pid;
                    false -> Default
                end;
            _ ->
                Default
        end
    catch
        _:_ -> Default
    end.
