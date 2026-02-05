%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL session shell (placeholder)
%%%
%%% Maintains per-session state including:
%%% - Variable bindings (counter := Counter spawn)
%%% - Session ID
%%% - Evaluation history (future)
%%%
%%% This is a placeholder for BT-260. Full implementation will come
%%% in later issues as the REPL architecture is refactored for
%%% persistent workspaces.

-module(beamtalk_repl_shell).
-behaviour(gen_server).

%% Public API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    session_id :: atom() | binary(),
    bindings :: map()
}).

%%% Public API

%% @doc Start a REPL shell session.
-spec start_link(atom() | binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link(?MODULE, SessionId, []).

%%% gen_server callbacks

%% @private
init(SessionId) ->
    State = #state{
        session_id = SessionId,
        bindings = #{}
    },
    {ok, State}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
