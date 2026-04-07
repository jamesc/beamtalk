%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_app).
-behaviour(application).

%%% **DDD Context:** Workspace Context

-moduledoc """
Application callback module for Beamtalk workspace management.

This application provides the interactive development environment including:
- REPL evaluation and protocol
- Workspace management and supervision
- Actor and module tracking
- Session management
- Idle monitoring and auto-cleanup

Dependencies: beamtalk_runtime (core language runtime)

When started, this application registers itself with the runtime to receive
actor spawn notifications, enabling workspace-wide actor tracking.
""".

-export([start/2, stop/1]).

-doc "Start the workspace application and register actor spawn callback.".
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Register actor spawn callback with runtime
    %% This allows the runtime to notify us when actors spawn, enabling
    %% workspace-wide tracking without creating a compile-time dependency
    application:set_env(beamtalk_runtime, actor_spawn_callback, beamtalk_repl_actors),
    %% Register class load callback with runtime (BT-1020)
    %% Allows the runtime to notify us when classes are loaded/reloaded
    application:set_env(beamtalk_runtime, class_load_callback, beamtalk_class_events),

    %% Start the workspace supervisor tree
    beamtalk_workspace_app_sup:start_link().

-doc "Stop the workspace application and unregister actor spawn callback.".
-spec stop(term()) -> ok.
stop(_State) ->
    %% Unregister actor spawn callback
    application:unset_env(beamtalk_runtime, actor_spawn_callback),
    %% Unregister class load callback (BT-1020)
    application:unset_env(beamtalk_runtime, class_load_callback),
    %% Remove WebSocket log handler (BT-1433)
    _ = logger:remove_handler(beamtalk_ws_log),
    ok.
