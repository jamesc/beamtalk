%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace API for managing workspace-level operations.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This module provides a high-level API for querying workspace state.
%%% It delegates to beamtalk_workspace_meta for workspace metadata.

-module(beamtalk_workspace).

-export([status/0]).

%% @doc Return a summary of the current workspace state.
%% Includes workspace ID, project path, created_at, last_activity, actor count, and loaded modules.
-spec status() -> {ok, map()} | {error, term()}.
status() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, Meta} ->
            SupervisedActors = maps:get(supervised_actors, Meta, []),
            {ok, #{
                workspace_id => maps:get(workspace_id, Meta, undefined),
                project_path => maps:get(project_path, Meta, undefined),
                created_at => maps:get(created_at, Meta, undefined),
                last_activity => maps:get(last_activity, Meta, undefined),
                actor_count => length(SupervisedActors),
                loaded_modules => maps:get(loaded_modules, Meta, [])
            }};
        {error, Reason} ->
            {error, Reason}
    end.
