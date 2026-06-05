%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_session).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for sessions, clone, close, health, and shutdown operations.

Extracted from beamtalk_repl_server (BT-705).
""".

-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4]).

-doc """
Handle sessions/clone/close/health/shutdown ops for the WebSocket transport —
encodes the term result to JSON at the edge (BT-2402).
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for sessions/clone/close/health/shutdown (BT-2402).
Returns `{sessions, [Meta]}`, `{ok, NewSessionId, Output, Warnings}` (clone),
`{status, ok}` (close/shutdown), `{health, WorkspaceId, Nonce}`, or
`{error, #beamtalk_error{}}` — no JSON in this path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"sessions">>, _Params, _Msg, _SessionPid) ->
    case whereis(beamtalk_session_sup) of
        undefined ->
            {sessions, []};
        _Sup ->
            Children = supervisor:which_children(beamtalk_session_sup),
            Sessions = lists:filtermap(
                fun
                    ({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                        {true, #{id => list_to_binary(pid_to_list(Pid))}};
                    (_) ->
                        false
                end,
                Children
            ),
            {sessions, Sessions}
    end;
handle_term(<<"clone">>, _Params, _Msg, _SessionPid) ->
    NewSessionId = beamtalk_repl_server:generate_session_id(),
    case beamtalk_session_sup:start_session(NewSessionId) of
        {ok, _NewPid} ->
            {ok, NewSessionId, <<>>, []};
        {error, Reason} ->
            Err0 = beamtalk_error:new(session_error, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary([
                    <<"Failed to create session: ">>,
                    io_lib:format("~p", [Reason])
                ])
            ),
            {error, Err1}
    end;
handle_term(<<"close">>, _Params, _Msg, _SessionPid) ->
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
    NodeCookie = atom_to_binary(erlang:get_cookie(), utf8),
    ValidCookie =
        is_binary(ProvidedCookie) andalso
            byte_size(ProvidedCookie) =:= byte_size(NodeCookie) andalso
            crypto:hash_equals(ProvidedCookie, NodeCookie),
    case ValidCookie of
        true ->
            ?LOG_INFO("Shutdown requested via protocol", #{domain => [beamtalk, runtime]}),
            erlang:send_after(100, beamtalk_repl_server, shutdown_requested),
            {status, ok};
        false ->
            ?LOG_WARNING("Shutdown rejected: invalid cookie", #{domain => [beamtalk, runtime]}),
            Err0 = beamtalk_error:new(auth_error, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0, <<"Invalid cookie">>),
            Err2 = beamtalk_error:with_hint(
                Err1, <<"Provide the correct node cookie for shutdown.">>
            ),
            {error, Err2}
    end.
