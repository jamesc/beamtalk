%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace metadata tracking
%%%
%%% Stores metadata about the workspace including:
%%% - Workspace ID
%%% - Project path
%%% - Creation timestamp
%%% - Last activity timestamp
%%%
%%% This module provides a gen_server that tracks workspace state
%%% and can be queried by other components (e.g., idle monitor).

-module(beamtalk_workspace_meta).
-behaviour(gen_server).

%% Public API
-export([start_link/1, get_metadata/0, update_activity/0, get_last_activity/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    workspace_id :: binary(),
    project_path :: binary(),
    created_at :: integer(),
    last_activity :: integer()
}).

-type metadata() :: #{
    workspace_id => binary(),
    project_path => binary(),
    created_at => integer(),
    last_activity => integer()
}.

-export_type([metadata/0]).

%%% Public API

%% @doc Start the workspace metadata server.
-spec start_link(metadata()) -> {ok, pid()} | {error, term()}.
start_link(InitialMetadata) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, InitialMetadata, []).

%% @doc Get all workspace metadata.
-spec get_metadata() -> {ok, metadata()} | {error, not_started}.
get_metadata() ->
    try
        gen_server:call(?MODULE, get_metadata)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%% @doc Update the last activity timestamp to now.
%% Called by other components when activity occurs (message sent, code loaded, etc.)
-spec update_activity() -> ok.
update_activity() ->
    try
        gen_server:cast(?MODULE, update_activity)
    catch
        exit:{noproc, _} ->
            ok  % Gracefully handle if server not running
    end.

%% @doc Get the last activity timestamp.
%% Returns the timestamp in seconds since epoch, or {error, not_started}.
-spec get_last_activity() -> {ok, integer()} | {error, not_started}.
get_last_activity() ->
    try
        gen_server:call(?MODULE, get_last_activity)
    catch
        exit:{noproc, _} ->
            {error, not_started}
    end.

%%% gen_server callbacks

%% @private
init(InitialMetadata) ->
    WorkspaceId = maps:get(workspace_id, InitialMetadata),
    ProjectPath = maps:get(project_path, InitialMetadata),
    CreatedAt = maps:get(created_at, InitialMetadata),
    Now = erlang:system_time(second),
    
    State = #state{
        workspace_id = WorkspaceId,
        project_path = ProjectPath,
        created_at = CreatedAt,
        last_activity = Now
    },
    
    {ok, State}.

%% @private
handle_call(get_metadata, _From, State) ->
    Metadata = #{
        workspace_id => State#state.workspace_id,
        project_path => State#state.project_path,
        created_at => State#state.created_at,
        last_activity => State#state.last_activity
    },
    {reply, {ok, Metadata}, State};

handle_call(get_last_activity, _From, State) ->
    {reply, {ok, State#state.last_activity}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(update_activity, State) ->
    Now = erlang:system_time(second),
    {noreply, State#state{last_activity = Now}};

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
