%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_conn_cap).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Cap on live WebSocket connections (BT-5127).

Ranch's `max_connections` counts a socket only until it is upgraded:
`cowboy_websocket:takeover/7` calls `ranch:remove_connection/1`, so an
upgraded WebSocket no longer counts and the listener-level cap does not bound
WebSockets at all. This module counts them itself. The handler takes a slot in
`websocket_init/1` and gives it back in `terminate/3` (both run in the process
that owns the socket); over the cap the connection is closed with code 1013.

Slots are rows in a public ETS set keyed by the connection pid, owned by
`beamtalk_repl_server` (`new/0`). A handler that is killed without running
`terminate/3` would leak its row, so a refusal first prunes rows whose pid is
dead and only then decides.
""".

-export([new/0, acquire/0, release/0, limit/0, count/0]).

-define(TABLE, beamtalk_ws_conn_cap).
-define(DEFAULT_LIMIT, 256).

-doc "Create the slot table. Call from the process that should own it.".
-spec new() -> ok.
new() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ?TABLE = ets:new(?TABLE, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.

-doc "Configured cap (`beamtalk_workspace` env `ws_max_connections`).".
-spec limit() -> pos_integer().
limit() ->
    case application:get_env(beamtalk_workspace, ws_max_connections, ?DEFAULT_LIMIT) of
        N when is_integer(N), N > 0 -> N;
        _ -> ?DEFAULT_LIMIT
    end.

-doc "Live WebSocket connections currently holding a slot.".
-spec count() -> non_neg_integer().
count() ->
    case ets:whereis(?TABLE) of
        undefined -> 0;
        _ -> ets:info(?TABLE, size)
    end.

-doc "Take a slot for the calling process, or `{error, at_capacity}`.".
-spec acquire() -> ok | {error, at_capacity}.
acquire() ->
    case ets:whereis(?TABLE) of
        undefined ->
            %% No listener owns the table (handler used outside the server):
            %% nothing to count against.
            ok;
        _ ->
            true = ets:insert(?TABLE, {self()}),
            Limit = limit(),
            case ets:info(?TABLE, size) =< Limit orelse prune_and_fits(Limit) of
                true ->
                    ok;
                false ->
                    ets:delete(?TABLE, self()),
                    {error, at_capacity}
            end
    end.

-doc "Give the calling process's slot back (idempotent).".
-spec release() -> ok.
release() ->
    case ets:whereis(?TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete(?TABLE, self()),
            ok
    end.

prune_and_fits(Limit) ->
    lists:foreach(
        fun({Pid}) ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> ets:delete(?TABLE, Pid)
            end
        end,
        ets:tab2list(?TABLE)
    ),
    ets:info(?TABLE, size) =< Limit.
