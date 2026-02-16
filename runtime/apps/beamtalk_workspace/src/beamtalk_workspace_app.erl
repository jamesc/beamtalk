%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Application callback module for Beamtalk workspace management.
%%%
%%% **DDD Context:** Workspace
%%%
%%% This application provides the interactive development environment including:
%%% - REPL evaluation and protocol
%%% - Workspace management and supervision
%%% - Actor and module tracking
%%% - Session management
%%% - Idle monitoring and auto-cleanup
%%%
%%% Dependencies: beamtalk_runtime (core language runtime)
%%%
%%% When started, this application registers itself with the runtime to receive
%%% actor spawn notifications, enabling workspace-wide actor tracking.

-module(beamtalk_workspace_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @private
%% @doc Start the workspace application and register actor spawn callback.
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Register actor spawn callback with runtime
    %% This allows the runtime to notify us when actors spawn, enabling
    %% workspace-wide tracking without creating a compile-time dependency
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_repl_actors),
    
    %% Start the workspace supervisor tree
    beamtalk_workspace_app_sup:start_link().

%% @private
%% @doc Stop the workspace application and unregister actor spawn callback.
-spec stop(term()) -> ok.
stop(_State) ->
    %% Unregister actor spawn callback
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    ok.
