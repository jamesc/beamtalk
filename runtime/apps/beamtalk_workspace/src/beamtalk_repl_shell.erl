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

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/1, stop/1, eval/2, get_bindings/1, clear_bindings/1, load_file/2,
         get_module_tracker/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Public API

%% @doc Start a REPL shell session.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link(?MODULE, SessionId, []).

%% @doc Stop a REPL shell session.
-spec stop(pid()) -> ok.
stop(SessionPid) ->
    gen_server:stop(SessionPid, normal, 5000).

%% @doc Evaluate an expression in this session.
-spec eval(pid(), string()) -> {ok, term(), binary(), [binary()]} | {error, term(), binary(), [binary()]}.
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
-spec load_file(pid(), string()) -> {ok, [map()]} | {error, term()}.
load_file(SessionPid, Path) ->
    gen_server:call(SessionPid, {load_file, Path}, 30000).

%% @doc Get the module tracker for this session (user-loaded modules only).
-spec get_module_tracker(pid()) -> {ok, beamtalk_repl_modules:module_tracker()}.
get_module_tracker(SessionPid) ->
    gen_server:call(SessionPid, get_module_tracker, 5000).

%%% gen_server callbacks

%% @private
init(SessionId) ->
    %% Create session-specific REPL state
    %% We use undefined for listen_socket and port since session doesn't own TCP connection
    State0 = beamtalk_repl_state:new(undefined, 0),
    
    %% ADR 0019 Phase 3: Inject workspace convenience bindings into session
    %% so that Transcript, Beamtalk, Workspace resolve from bindings map.
    Bindings0 = inject_workspace_bindings(#{}),
    State0b = beamtalk_repl_state:set_bindings(Bindings0, State0),

    %% Get workspace-wide actor registry
    %% The registry is registered globally in the workspace
    RegistryPid = case whereis(beamtalk_actor_registry) of
        undefined ->
            ?LOG_WARNING("Actor registry not found for session ~p", [SessionId]),
            undefined;
        Pid ->
            Pid
    end,
    State1 = beamtalk_repl_state:set_actor_registry(RegistryPid, State0b),
    
    {ok, {SessionId, State1}}.

%% @private
handle_call({eval, Expression}, _From, {SessionId, State}) ->
    case beamtalk_repl_eval:do_eval(Expression, State) of
        {ok, Result, Output, Warnings, NewState} ->
            {reply, {ok, Result, Output, Warnings}, {SessionId, NewState}};
        {error, Reason, Output, Warnings, NewState} ->
            {reply, {error, Reason, Output, Warnings}, {SessionId, NewState}}
    end;

handle_call(get_bindings, _From, {SessionId, State}) ->
    Bindings = beamtalk_repl_state:get_bindings(State),
    {reply, {ok, Bindings}, {SessionId, State}};

handle_call(clear_bindings, _From, {SessionId, State}) ->
    %% ADR 0019 Phase 3: Re-inject workspace bindings after clearing
    %% so that Transcript, Beamtalk, Workspace remain available.
    Bindings = inject_workspace_bindings(#{}),
    NewState = beamtalk_repl_state:set_bindings(Bindings, State),
    {reply, ok, {SessionId, NewState}};

handle_call({load_file, Path}, _From, {SessionId, State}) ->
    case beamtalk_repl_eval:handle_load(Path, State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState}}
    end;

handle_call(get_module_tracker, _From, {SessionId, State}) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    {reply, {ok, Tracker}, {SessionId, State}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, {SessionId, _State}) ->
    ?LOG_INFO("REPL session terminated", #{session => SessionId, reason => Reason}),
    ok;
terminate(Reason, _State) ->
    ?LOG_INFO("REPL session terminated", #{reason => Reason}),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% @private ADR 0019 Phase 4: Inject workspace convenience bindings.
%% Looks up Transcript, Beamtalk, Workspace singletons by registered name
%% and adds them to the bindings map so ClassReference codegen can resolve them.
-include("beamtalk_workspace.hrl").

-define(BINDING_INFO, [
    {'Transcript',  'TranscriptStream',      beamtalk_transcript_stream},
    {'Beamtalk',    'SystemDictionary',       beamtalk_system_dictionary},
    {'Workspace',   'WorkspaceEnvironment',   beamtalk_workspace_actor}
]).

inject_workspace_bindings(Bindings) ->
    lists:foldl(
        fun({Name, ClassName, Module}, Acc) ->
            case erlang:whereis(Name) of
                undefined -> Acc;
                Pid -> maps:put(Name, {beamtalk_object, ClassName, Module, Pid}, Acc)
            end
        end,
        Bindings,
        ?BINDING_INFO
    ).
