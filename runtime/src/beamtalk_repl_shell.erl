%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL session shell for workspace
%%%
%%% Maintains per-session state including:
%%% - Variable bindings (counter := Counter spawn)
%%% - Session ID
%%% - Evaluation state and counter
%%%
%%% Each REPL connection gets its own session process, allowing
%%% multiple users to work in the same workspace with independent
%%% bindings while sharing access to actors and loaded modules.

-module(beamtalk_repl_shell).
-behaviour(gen_server).

%% Public API
-export([start_link/1, eval/2, get_bindings/1, clear_bindings/1, load_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Public API

%% @doc Start a REPL shell session.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link(?MODULE, SessionId, []).

%% @doc Evaluate an expression in this session.
-spec eval(pid(), string()) -> {ok, term()} | {error, term()}.
eval(SessionPid, Expression) ->
    gen_server:call(SessionPid, {eval, Expression}, 30000).

%% @doc Get current variable bindings for this session.
-spec get_bindings(pid()) -> {ok, map()}.
get_bindings(SessionPid) ->
    gen_server:call(SessionPid, get_bindings).

%% @doc Clear all variable bindings for this session.
-spec clear_bindings(pid()) -> ok.
clear_bindings(SessionPid) ->
    gen_server:call(SessionPid, clear_bindings).

%% @doc Load a Beamtalk source file in this session.
-spec load_file(pid(), string()) -> {ok, [atom()]} | {error, term()}.
load_file(SessionPid, Path) ->
    gen_server:call(SessionPid, {load_file, Path}, 30000).

%%% gen_server callbacks

%% @private
init(SessionId) ->
    %% Create session-specific REPL state
    %% We use undefined for listen_socket and port since session doesn't own TCP connection
    State0 = beamtalk_repl_state:new(undefined, 0),
    
    %% Get workspace-wide actor registry
    %% The registry is registered globally in the workspace
    RegistryPid = case whereis(beamtalk_actor_registry) of
        undefined ->
            logger:warning("Actor registry not found for session ~p", [SessionId]),
            undefined;
        Pid ->
            Pid
    end,
    State1 = beamtalk_repl_state:set_actor_registry(RegistryPid, State0),
    
    {ok, {SessionId, State1}}.

%% @private
handle_call({eval, Expression}, _From, {SessionId, State}) ->
    case beamtalk_repl_eval:do_eval(Expression, State) of
        {ok, Result, NewState} ->
            {reply, {ok, Result}, {SessionId, NewState}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState}}
    end;

handle_call(get_bindings, _From, {SessionId, State}) ->
    Bindings = beamtalk_repl_state:get_bindings(State),
    {reply, {ok, Bindings}, {SessionId, State}};

handle_call(clear_bindings, _From, {SessionId, State}) ->
    NewState = beamtalk_repl_state:clear_bindings(State),
    {reply, ok, {SessionId, NewState}};

handle_call({load_file, Path}, _From, {SessionId, State}) ->
    case beamtalk_repl_eval:handle_load(Path, State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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
