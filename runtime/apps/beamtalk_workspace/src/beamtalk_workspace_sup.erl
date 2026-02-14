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
%%%   ├─ beamtalk_workspace_meta      % Metadata (project path, created_at)
%%%   ├─ beamtalk_transcript_stream   % Transcript singleton (ADR 0010)
%%%   ├─ beamtalk_system_dictionary   % Beamtalk singleton (ADR 0010)
%%%   ├─ beamtalk_actor_registry      % Workspace-wide actor registry
%%%   ├─ beamtalk_workspace_actor     % Workspace singleton (BT-423)
%%%   ├─ beamtalk_workspace_bootstrap % Class var bootstrap (ADR 0019)
%%%   ├─ beamtalk_repl_server         % TCP server (session-per-connection)
%%%   ├─ beamtalk_idle_monitor        % Tracks activity, self-terminates if idle
%%%   ├─ beamtalk_actor_sup           % Supervises user actors
%%%   └─ beamtalk_session_sup         % Supervises session shell processes
%%% ```

-module(beamtalk_workspace_sup).
-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

-type workspace_config() :: #{
    workspace_id => binary(),
    project_path => binary(),
    tcp_port => inet:port_number(),
    auto_cleanup => boolean(),
    max_idle_seconds => integer()
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
    MaxIdleSeconds = maps:get(max_idle_seconds, Config, 3600 * 4),
    
    %% Set up file logging before children start (they may log during init)
    setup_file_logger(WorkspaceId),
    
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
        
        %% Singleton actors — workspace bindings (ADR 0010 Phase 2)
        %% These assume beamtalk_stdlib has already been started elsewhere in the system.
        %% Each registers itself in init/1: persistent_term + register/2
        #{
            id => beamtalk_transcript_stream,
            start => {beamtalk_transcript_stream, start_link_singleton, [1000]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_transcript_stream]
        },
        #{
            id => beamtalk_system_dictionary,
            start => {beamtalk_system_dictionary, start_link_singleton, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_system_dictionary]
        },
        
        %% Actor registry (workspace-wide, shared across sessions)
        #{
            id => beamtalk_actor_registry,
            start => {beamtalk_repl_actors, start_link, [registered]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_repl_actors]
        },
        
        %% Workspace actor — introspection singleton (BT-423)
        %% Placed after actor_registry since its methods query the registry
        #{
            id => beamtalk_workspace_actor,
            start => {beamtalk_workspace_actor, start_link_singleton, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_actor]
        },
        
        %% Bootstrap worker — sets singleton class variables (ADR 0019 Phase 2)
        %% Must start after all singletons but before REPL server accepts connections.
        %% Monitors singleton PIDs and re-sets class vars on restart.
        #{
            id => beamtalk_workspace_bootstrap,
            start => {beamtalk_workspace_bootstrap, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_bootstrap]
        },
        
        %% REPL TCP server (session-per-connection architecture)
        #{
            id => beamtalk_repl_server,
            start => {beamtalk_repl_server, start_link, [#{port => TcpPort, workspace_id => WorkspaceId}]},
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
                max_idle_seconds => MaxIdleSeconds
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

%%% File Logging

%% @private
%% @doc Set up a file-based logger handler for the workspace node.
%% Writes to ~/.beamtalk/workspaces/{workspace_id}/workspace.log with rotation.
%% Disabled when BEAMTALK_NO_FILE_LOG=1 is set.
-spec setup_file_logger(binary()) -> ok.
setup_file_logger(WorkspaceId) ->
    case os:getenv("BEAMTALK_NO_FILE_LOG") of
        "1" ->
            ok;
        _ ->
            do_setup_file_logger(WorkspaceId)
    end.

%% @private
do_setup_file_logger(WorkspaceId) ->
    case os:getenv("HOME") of
        false ->
            ?LOG_WARNING("HOME not set; skipping file logger for workspace ~p",
                           [WorkspaceId]),
            ok;
        Home ->
            LogFile = filename:join([Home, ".beamtalk", "workspaces",
                                     binary_to_list(WorkspaceId), "workspace.log"]),
            case filelib:ensure_dir(LogFile) of
                ok ->
                    HandlerConfig = #{
                        config => #{
                            file => LogFile,
                            max_no_bytes => 1048576,  % 1 MB per file
                            max_no_files => 5
                        },
                        level => debug,
                        formatter => {logger_formatter, #{
                            template => [time, " [", level, "] ", mfa, " ", msg, "\n"],
                            single_line => true
                        }}
                    },
                    case logger:add_handler(beamtalk_file_log, logger_std_h, HandlerConfig) of
                        ok ->
                            ?LOG_INFO("Workspace log file: ~s", [LogFile]),
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING("Failed to add file logger: ~p", [Reason]),
                            ok
                    end;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to create log directory for ~s: ~p",
                                   [LogFile, Reason]),
                    ok
            end
    end.
