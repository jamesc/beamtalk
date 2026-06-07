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

-export([start_link/0, start_session/1, start_session/2, stop_session/1]).
-export([normalize_kind/1, known_kinds/0]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%% Recognised client surfaces. A session's `kind` is normalised against this
%% allowlist at creation so `Workspace sessions` only ever shows a known label
%% (anything else, including a spoofed or future client string, collapses to
%% `<<"unknown">>`).
-define(KNOWN_KINDS, [
    <<"repl">>, <<"mcp">>, <<"lsp">>, <<"liveview">>, <<"ide">>, <<"attach">>, <<"unknown">>
]).

%%% Public API

-doc "Start the session supervisor.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-doc """
Start a new session under supervision (origin metadata defaults to
`kind => unknown`).
SessionId should be a unique identifier for the session (e.g., alice, bob).
""".
-spec start_session(atom() | binary()) -> {ok, pid()} | {error, term()}.
start_session(SessionId) ->
    start_session(SessionId, #{}).

-doc """
Start a new session carrying origin/debug metadata.

`Meta` is a map recorded on the session and surfaced by `Workspace sessions` /
`Session info`. Its `kind` key (the originating client surface) is normalised
here against `known_kinds/0` so an unknown or spoofed value collapses to
`<<"unknown">>`; other keys (`peer`, `node`, `user`, `connected_at`, …) are
passed through verbatim. This is the single gate every creation path
(WebSocket, `clone`, dist-attached LiveView) funnels through, so kind
normalisation happens exactly once.
""".
-spec start_session(atom() | binary(), map()) -> {ok, pid()} | {error, term()}.
start_session(SessionId, Meta) when is_map(Meta) ->
    NormalisedMeta = Meta#{kind => normalize_kind(maps:get(kind, Meta, <<"unknown">>))},
    supervisor:start_child(?MODULE, [SessionId, NormalisedMeta]).

-doc "The allowlist of recognised client-surface kinds.".
-spec known_kinds() -> [binary()].
known_kinds() ->
    ?KNOWN_KINDS.

-doc """
Normalise a client-surface kind to a known lowercase binary.

Accepts an atom, binary, or string; lowercases it and keeps it only if it is in
`known_kinds/0`. Anything else (including `undefined`, a number, or an
unrecognised string) becomes `<<"unknown">>`.
""".
-spec normalize_kind(term()) -> binary().
normalize_kind(Kind) when is_atom(Kind), Kind =/= undefined ->
    normalize_kind(atom_to_binary(Kind, utf8));
normalize_kind(Kind) when is_list(Kind) ->
    try
        normalize_kind(unicode:characters_to_binary(Kind))
    catch
        _:_ -> <<"unknown">>
    end;
normalize_kind(Kind) when is_binary(Kind) ->
    Lower = string:lowercase(Kind),
    case lists:member(Lower, ?KNOWN_KINDS) of
        true -> Lower;
        false -> <<"unknown">>
    end;
normalize_kind(_Other) ->
    <<"unknown">>.

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
