%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for sessions, clone, close, health, and shutdown operations.
%%%
%%% **DDD Context:** REPL
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_session).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4]).

%% @doc Handle sessions/clone/close/health/shutdown ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"sessions">>, _Params, Msg, _SessionPid) ->
    case whereis(beamtalk_session_sup) of
        undefined ->
            beamtalk_repl_protocol:encode_sessions([], Msg, fun beamtalk_repl_json:term_to_json/1);
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
            beamtalk_repl_protocol:encode_sessions(
                Sessions, Msg, fun beamtalk_repl_json:term_to_json/1
            )
    end;
handle(<<"clone">>, _Params, Msg, _SessionPid) ->
    NewSessionId = beamtalk_repl_server:generate_session_id(),
    case beamtalk_session_sup:start_session(NewSessionId) of
        {ok, _NewPid} ->
            beamtalk_repl_protocol:encode_result(
                NewSessionId, Msg, fun beamtalk_repl_json:term_to_json/1
            );
        {error, Reason} ->
            Err0 = beamtalk_error:new(session_error, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary([
                    <<"Failed to create session: ">>,
                    io_lib:format("~p", [Reason])
                ])
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end;
handle(<<"close">>, _Params, Msg, _SessionPid) ->
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
handle(<<"health">>, _Params, Msg, _SessionPid) ->
    {ok, Nonce} = beamtalk_repl_server:get_nonce(),
    WorkspaceId =
        case beamtalk_workspace_meta:get_metadata() of
            {ok, Meta} -> maps:get(workspace_id, Meta, <<>>);
            {error, _} -> <<>>
        end,
    Base = beamtalk_repl_protocol:base_response(Msg),
    jsx:encode(Base#{
        <<"workspace_id">> => WorkspaceId,
        <<"nonce">> => Nonce,
        <<"status">> => [<<"done">>]
    });
handle(<<"shutdown">>, Params, Msg, _SessionPid) ->
    ProvidedCookie = maps:get(<<"cookie">>, Params, <<>>),
    NodeCookie = atom_to_binary(erlang:get_cookie(), utf8),
    ValidCookie =
        is_binary(ProvidedCookie) andalso
            byte_size(ProvidedCookie) =:= byte_size(NodeCookie) andalso
            crypto:hash_equals(ProvidedCookie, NodeCookie),
    case ValidCookie of
        true ->
            ?LOG_INFO("Shutdown requested via protocol", #{}),
            erlang:send_after(100, beamtalk_repl_server, shutdown_requested),
            beamtalk_repl_protocol:encode_status(
                ok,
                Msg,
                fun beamtalk_repl_json:term_to_json/1
            );
        false ->
            ?LOG_WARNING("Shutdown rejected: invalid cookie", #{}),
            Err0 = beamtalk_error:new(auth_error, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0, <<"Invalid cookie">>),
            Err2 = beamtalk_error:with_hint(
                Err1, <<"Provide the correct node cookie for shutdown.">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err2,
                Msg,
                fun beamtalk_repl_json:format_error_message/1
            )
    end.
