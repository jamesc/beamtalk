%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc WebSocket handler for Beamtalk REPL protocol (ADR 0020).
%%%
%%% **DDD Context:** REPL
%%%
%%% Handles WebSocket connections from CLI, browser, and MCP clients.
%%% Implements cookie-based authentication on first message before
%%% accepting any protocol operations.

-module(beamtalk_ws_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate/3]).

-record(ws_state, {
    session_id :: binary() | undefined,
    session_pid :: pid() | undefined,
    authenticated :: boolean(),
    peer :: term()
}).

%% @doc HTTP upgrade to WebSocket.
init(Req, _Opts) ->
    Peer = cowboy_req:peer(Req),
    {cowboy_websocket, Req, #ws_state{authenticated = false, peer = Peer},
     #{idle_timeout => 300000, max_frame_size => 1048576}}.

%% @doc WebSocket connection initialized — send auth challenge.
%% Session is NOT created yet; deferred until after successful auth (security).
websocket_init(State) ->
    %% Send a minimal welcome so clients know the connection is alive.
    %% Session details are sent after successful authentication.
    Welcome = jsx:encode(#{<<"op">> => <<"auth-required">>}),
    {[{text, Welcome}], State}.

%% @doc Handle incoming WebSocket text frame.
%% First message must be auth; subsequent messages are protocol ops.
websocket_handle({text, Data}, State = #ws_state{authenticated = false}) ->
    handle_auth(Data, State);
websocket_handle({text, Data}, State = #ws_state{authenticated = true, session_pid = SessionPid}) ->
    handle_protocol(Data, SessionPid, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

%% @doc Handle Erlang messages sent to the handler process.
websocket_info(shutdown_requested, State) ->
    ?LOG_INFO("Executing WebSocket-requested shutdown", #{}),
    init:stop(),
    {ok, State};
websocket_info(_Info, State) ->
    {ok, State}.

%% @doc Connection terminated — clean up session.
terminate(_Reason, _Req, #ws_state{session_id = undefined}) ->
    ok;
terminate(_Reason, _Req, #ws_state{session_id = SessionId, session_pid = SessionPid, peer = Peer}) ->
    ?LOG_INFO("WebSocket client disconnected", #{
        session => SessionId,
        session_pid => SessionPid,
        peer => Peer
    }),
    ets:delete(beamtalk_sessions, SessionId),
    case is_pid(SessionPid) andalso is_process_alive(SessionPid) of
        true -> beamtalk_repl_shell:stop(SessionPid);
        false -> ok
    end,
    ok.

%%% Internal — Authentication

%% @private
%% Validate the first WebSocket message as a cookie auth handshake.
%% Expected: {"type":"auth","cookie":"<workspace cookie>"}
handle_auth(Data, State) ->
    try jsx:decode(Data, [return_maps]) of
        #{<<"type">> := <<"auth">>, <<"cookie">> := ProvidedCookie}
                when is_binary(ProvidedCookie) ->
            NodeCookie = atom_to_binary(erlang:get_cookie(), utf8),
            %% Timing-safe comparison to prevent side-channel attacks.
            %% crypto:hash_equals/2 raises badarg on length mismatch,
            %% so we guard with byte_size check first.
            CookieValid = byte_size(ProvidedCookie) =:= byte_size(NodeCookie)
                andalso crypto:hash_equals(ProvidedCookie, NodeCookie),
            case CookieValid of
                true ->
                    %% Auth succeeded — now create the session
                    SessionId = beamtalk_repl_server:generate_session_id(),
                    case beamtalk_session_sup:start_session(SessionId) of
                        {ok, SessionPid} ->
                            ets:insert(beamtalk_sessions, {SessionId, SessionPid}),
                            ?LOG_INFO("WebSocket auth succeeded, session created", #{
                                session => SessionId,
                                session_pid => SessionPid,
                                peer => State#ws_state.peer
                            }),
                            beamtalk_workspace_meta:update_activity(),
                            AuthOk = jsx:encode(#{<<"type">> => <<"auth_ok">>}),
                            SessionMsg = jsx:encode(#{<<"op">> => <<"session-started">>,
                                                      <<"session">> => SessionId}),
                            {[{text, AuthOk}, {text, SessionMsg}],
                             State#ws_state{authenticated = true,
                                            session_id = SessionId,
                                            session_pid = SessionPid}};
                        {error, Reason} ->
                            ?LOG_ERROR("Session creation failed after auth", #{
                                reason => Reason,
                                peer => State#ws_state.peer
                            }),
                            ErrorJson = jsx:encode(#{<<"type">> => <<"auth_error">>,
                                                     <<"message">> => <<"Session creation failed">>}),
                            {[{text, ErrorJson}, {close, 1011, <<"Session creation failed">>}], State}
                    end;
                false ->
                    ?LOG_WARNING("WebSocket auth failed: invalid cookie", #{peer => State#ws_state.peer}),
                    Reply = jsx:encode(#{<<"type">> => <<"auth_error">>,
                                         <<"message">> => <<"Invalid cookie">>}),
                    {[{text, Reply}, {close, 1008, <<"Authentication failed">>}], State}
            end;
        _ ->
            ?LOG_WARNING("WebSocket auth failed: invalid auth message", #{peer => State#ws_state.peer}),
            Reply = jsx:encode(#{<<"type">> => <<"auth_error">>,
                                 <<"message">> => <<"First message must be auth">>}),
            {[{text, Reply}, {close, 1008, <<"Authentication required">>}], State}
    catch
        error:badarg ->
            ?LOG_WARNING("WebSocket auth failed: malformed JSON", #{peer => State#ws_state.peer}),
            Reply = jsx:encode(#{<<"type">> => <<"auth_error">>,
                                 <<"message">> => <<"Malformed auth message">>}),
            {[{text, Reply}, {close, 1008, <<"Authentication required">>}], State}
    end.

%%% Internal — Protocol Dispatch

%% @private
%% Dispatch a protocol message after authentication.
handle_protocol(Data, SessionPid, State) ->
    case beamtalk_repl_protocol:decode(Data) of
        {ok, Msg} ->
            Op = beamtalk_repl_protocol:get_op(Msg),
            case Op of
                <<"shutdown">> ->
                    handle_shutdown(Msg, State);
                _ ->
                    Response = beamtalk_repl_server:handle_protocol_request(Msg, SessionPid),
                    {[{text, Response}], State}
            end;
        {error, DecodeError} ->
            ErrorJson = beamtalk_repl_json:format_error(DecodeError),
            {[{text, ErrorJson}], State}
    end.

%% @private
%% Handle shutdown op — re-validates cookie as an extra security measure.
%% Even though WebSocket is already authenticated, shutdown is a privileged
%% operation that requires explicit cookie authorization in the message.
handle_shutdown(Msg, State) ->
    Params = beamtalk_repl_protocol:get_params(Msg),
    case maps:get(<<"cookie">>, Params, undefined) of
        undefined ->
            ?LOG_WARNING("Shutdown denied: missing cookie in message", #{}),
            ErrorJson = beamtalk_repl_json:format_error(<<"Shutdown requires cookie">>),
            {[{text, ErrorJson}], State};
        ProvidedCookie when is_binary(ProvidedCookie) ->
            NodeCookie = atom_to_binary(erlang:get_cookie(), utf8),
            CookieValid = byte_size(ProvidedCookie) =:= byte_size(NodeCookie)
                andalso crypto:hash_equals(ProvidedCookie, NodeCookie),
            case CookieValid of
                true ->
                    ?LOG_INFO("Shutdown requested via WebSocket protocol", #{}),
                    erlang:send_after(100, self(), shutdown_requested),
                    Reply = beamtalk_repl_protocol:encode_status(ok, Msg,
                        fun beamtalk_repl_json:term_to_json/1),
                    {[{text, Reply}], State};
                false ->
                    ?LOG_WARNING("Shutdown denied: invalid cookie", #{}),
                    ErrorJson = beamtalk_repl_json:format_error(<<"Invalid shutdown cookie">>),
                    {[{text, ErrorJson}], State}
            end;
        _ ->
            ?LOG_WARNING("Shutdown denied: invalid cookie type", #{}),
            ErrorJson = beamtalk_repl_json:format_error(<<"Invalid shutdown cookie">>),
            {[{text, ErrorJson}], State}
    end.
