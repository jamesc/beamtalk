%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-workspace supervisor
%%%
%%% This supervisor manages all components of a persistent workspace:
%%% - Actor registry (workspace-wide actor tracking)
%%% - REPL TCP server for client connections  
%%% - Idle monitor for auto-cleanup
%%% - Workspace metadata tracking
%%% - Actor supervision (shared across sessions)
%%% - Session supervision (one per REPL connection)
%%%
%%% Architecture (from ADR 0004, implemented in BT-262):
%%% ```
%%% beamtalk_workspace_sup
%%%   ├─ beamtalk_actor_registry   % Workspace-wide actor registry
%%%   ├─ beamtalk_repl_server      % TCP server (session-per-connection)
%%%   ├─ beamtalk_idle_monitor     % Tracks activity, self-terminates if idle
%%%   ├─ beamtalk_workspace_meta   % Metadata (project path, created_at)
%%%   ├─ beamtalk_actor_sup        % Supervises user actors
%%%   └─ beamtalk_session_sup      % Supervises session shell processes
%%% ```

-module(beamtalk_workspace_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-type workspace_config() :: #{
    workspace_id => binary(),
    project_path => binary(),
    tcp_port => inet:port_number(),
    auto_cleanup => boolean()
}.

-export_type([workspace_config/0]).

%% @doc Start the workspace supervisor.
-spec start_link(workspace_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% @private
init(Config) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    %% Extract configuration
    WorkspaceId = maps:get(workspace_id, Config),
    ProjectPath = maps:get(project_path, Config),
    TcpPort = maps:get(tcp_port, Config),
    AutoCleanup = maps:get(auto_cleanup, Config, true),
    
    ChildSpecs = [
        %% Workspace metadata (must start first - others may query it)
        #{
            id => beamtalk_workspace_meta,
            start => {beamtalk_workspace_meta, start_link, [#{
                workspace_id => WorkspaceId,
                project_path => ProjectPath,
                created_at => erlang:system_time(second),
                repl_port => TcpPort
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_meta]
        },
        
        %% Actor registry (workspace-wide, shared across sessions)
        #{
            id => beamtalk_actor_registry,
            start => {beamtalk_repl_actors, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_repl_actors]
        },
        
        %% REPL TCP server (session-per-connection architecture)
        #{
            id => beamtalk_repl_server,
            start => {beamtalk_repl_server, start_link, [TcpPort]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_repl_server]
        },
        
        %% Idle monitor for auto-cleanup (only if enabled)
        #{
            id => beamtalk_idle_monitor,
            start => {beamtalk_idle_monitor, start_link, [#{
                enabled => AutoCleanup,
                max_idle_seconds => 3600 * 4  % 4 hours
            }]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_idle_monitor]
        },
        
        %% Actor supervisor (shared across all sessions)
        #{
            id => beamtalk_actor_sup,
            start => {beamtalk_actor_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Give actors time to shut down gracefully
            type => supervisor,
            modules => [beamtalk_actor_sup]
        },
        
        %% Session supervisor (one child per REPL connection)
        #{
            id => beamtalk_session_sup,
            start => {beamtalk_session_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [beamtalk_session_sup]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
