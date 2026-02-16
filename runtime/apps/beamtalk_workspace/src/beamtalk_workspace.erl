%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Workspace API for managing workspace-level operations.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This module provides a high-level API for querying workspace state.
%%% It delegates to beamtalk_workspace_meta for metadata and
%%% beamtalk_repl_server for connection information.

-module(beamtalk_workspace).

-export([status/0]).

%% @doc Return a summary of the current workspace state.
%% Includes workspace ID, project path, uptime, actor count, and loaded modules.
-spec status() -> {ok, map()} | {error, not_started}.
status() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, Meta} ->
            Actors = case beamtalk_workspace_meta:supervised_actors() of
                {ok, Pids} -> length(Pids);
                {error, _} -> 0
            end,
            Modules = case beamtalk_workspace_meta:loaded_modules() of
                {ok, Mods} -> Mods;
                {error, _} -> []
            end,
            {ok, #{
                workspace_id => maps:get(workspace_id, Meta, undefined),
                project_path => maps:get(project_path, Meta, undefined),
                created_at => maps:get(created_at, Meta, undefined),
                last_activity => maps:get(last_activity, Meta, undefined),
                actor_count => Actors,
                loaded_modules => Modules
            }};
        {error, Reason} ->
            {error, Reason}
    end.
