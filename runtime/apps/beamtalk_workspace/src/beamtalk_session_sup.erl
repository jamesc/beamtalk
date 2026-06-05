%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_session_sup).
-behaviour(supervisor).

%%% **DDD Context:** Workspace Context

-moduledoc """
Session supervisor for workspace

Supervises REPL session shell processes.
Each REPL connection creates one session process that maintains
bindings and evaluation state for that specific connection.

Sessions are ephemeral - they start when a REPL connects and
terminate when the connection closes.
""".

-export([start_link/0, start_session/1, stop_session/1]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%% Public API

-doc "Start the session supervisor.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Start a new session under supervision.
SessionId should be a unique identifier for the session (e.g., alice, bob).
""".
-spec start_session(atom() | binary()) -> {ok, pid()} | {error, term()}.
start_session(SessionId) ->
    supervisor:start_child(?MODULE, [SessionId]).

-doc """
Stop a session previously started by `start_session/1`.

Sessions are `temporary` children of this `simple_one_for_one` supervisor, so a
session process does not terminate just because its owner (e.g. a dist-attached
Phoenix LiveView) exits — the supervisor must be asked to terminate it, or one
orphaned session leaks per mount/reconnect. Best-effort and non-fatal: a pid
that was never a child of this supervisor returns `{error, not_found}`, and a
second stop on a just-terminated child is a harmless `ok` no-op.
""".
-spec stop_session(pid()) -> ok | {error, not_found | simple_one_for_one}.
stop_session(SessionPid) when is_pid(SessionPid) ->
    case supervisor:terminate_child(?MODULE, SessionPid) of
        ok ->
            ok;
        {error, Reason} = Error ->
            ?LOG_DEBUG(
                "stop_session/1 could not terminate ~p: ~p",
                [SessionPid, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            Error
    end.

%%% supervisor callbacks

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    %% Child spec template for sessions
    %% NOTE: This is a placeholder. The actual session implementation
    %% (beamtalk_repl_shell or similar) needs to be created.
    ChildSpec = #{
        id => beamtalk_session,
        start => {beamtalk_repl_shell, start_link, []},
        % Sessions don't restart
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [beamtalk_repl_shell]
    },

    {ok, {SupFlags, [ChildSpec]}}.
