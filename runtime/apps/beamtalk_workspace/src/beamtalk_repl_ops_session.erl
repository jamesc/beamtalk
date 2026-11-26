%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_session).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for sessions, close, health, and shutdown operations.

Extracted from beamtalk_repl_server.
""".

-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4]).

-doc """
Handle sessions/close/health/shutdown ops for the WebSocket transport —
encodes the term result to JSON at the edge.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for sessions/close/health/shutdown.
Returns `{sessions, [Meta]}` (`Meta` carries `id` and the shell `pid`; the wire encoding emits `id` only), `{status, ok}` (close/shutdown), `{health, WorkspaceId, Nonce}`, or
`{error, #beamtalk_error{}}` — no JSON in this path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"sessions">>, _Params, _Msg, _SessionPid) ->
    case whereis(beamtalk_session_sup) of
        undefined ->
            {sessions, []};
        _Sup ->
            %% Report the protocol session ids — the values `resume` accepts —
            %% not the shell pids (BT-5012).
            Children = supervisor:which_children(beamtalk_session_sup),
            Sessions = lists:filtermap(
                fun
                    ({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                        try beamtalk_repl_shell:get_session_id(Pid, 1000) of
                            {ok, SessionId} -> {true, #{id => SessionId, pid => Pid}}
                        catch
                            exit:_ -> false
                        end;
                    (_) ->
                        false
                end,
                Children
            ),
            {sessions, Sessions}
    end;
handle_term(<<"close">>, _Params, Msg, SessionPid) ->
    %% Stop the session shell; its terminate removes the table row (BT-5012).
    %% Only children of the session supervisor are stopped, so a caller that is
    %% not a session (e.g. an in-process test) is left alone.
    %%
    %% BT-5199: a `session` field names another session to close — VS Code
    %% closes the session of a REPL terminal it started once that terminal
    %% ends. An unknown id closes nothing (the session is already gone); it
    %% never falls back to the caller's own session.
    _ =
        case beamtalk_repl_protocol:get_session(Msg) of
            undefined ->
                stop_session(SessionPid);
            TargetSession ->
                case beamtalk_session_table:lookup(TargetSession) of
                    {ok, TargetPid} -> stop_session(TargetPid);
                    error -> {error, not_found}
                end
        end,
    {status, ok};
handle_term(<<"health">>, _Params, _Msg, _SessionPid) ->
    {ok, Nonce} = beamtalk_repl_server:get_nonce(),
    WorkspaceId =
        case beamtalk_workspace_meta:get_metadata() of
            {ok, Meta} -> maps:get(workspace_id, Meta, <<>>);
            {error, _} -> <<>>
        end,
    {health, WorkspaceId, Nonce};
handle_term(<<"shutdown">>, Params, _Msg, _SessionPid) ->
    ProvidedCookie = maps:get(<<"cookie">>, Params, <<>>),
    ValidCookie = beamtalk_ws_cookie:valid(ProvidedCookie),
    case ValidCookie of
        true ->
            ?LOG_INFO("Shutdown requested via protocol", #{domain => [beamtalk, runtime]}),
            erlang:send_after(100, beamtalk_repl_server, shutdown_requested),
            {status, ok};
        false ->
            ?LOG_WARNING("Shutdown rejected: invalid cookie", #{domain => [beamtalk, runtime]}),
            {error,
                beamtalk_repl_errors:make(
                    auth_error,
                    'REPL',
                    <<"Invalid cookie">>,
                    <<"Provide the correct node cookie for shutdown.">>
                )}
    end.

-spec stop_session(term()) -> ok | {error, term()}.
stop_session(SessionPid) when is_pid(SessionPid) ->
    case whereis(beamtalk_session_sup) of
        undefined -> {error, not_found};
        _ -> beamtalk_session_sup:stop_session(SessionPid)
    end;
stop_session(_) ->
    {error, not_found}.
