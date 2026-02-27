%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Per-workspace supervisor
%%%
%%% **DDD Context:** Workspace
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
%%%   ├─ bt@stdlib@transcript_stream   % Transcript singleton (ADR 0010)
%%%   ├─ bt@stdlib@beamtalk_interface  % Beamtalk singleton (ADR 0010)
%%%   ├─ beamtalk_actor_registry       % Workspace-wide actor registry
%%%   ├─ bt@stdlib@workspace_interface % Workspace singleton (BT-423)
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
    project_path => binary() | undefined,
    tcp_port => inet:port_number(),
    bind_addr => inet:ip4_address(),
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
    ProjectPath = maps:get(project_path, Config, undefined),
    TcpPort = maps:get(tcp_port, Config),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
    WebPort = maps:get(web_port, Config, undefined),
    AutoCleanup = maps:get(auto_cleanup, Config, true),
    MaxIdleSeconds = maps:get(max_idle_seconds, Config, 3600 * 4),

    %% Set up file logging before children start (they may log during init)
    setup_file_logger(WorkspaceId),

    %% Start the compiler application (ADR 0022)
    %% This is started dynamically rather than as a static app dependency
    %% because the compiler port binary may not be available in all environments
    %% (e.g., unit tests).
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, Reason} -> ?LOG_ERROR("Failed to start beamtalk_compiler app", #{reason => Reason})
    end,

    ChildSpecs =
        [
            %% Workspace metadata (must start first - others may query it)
            #{
                id => beamtalk_workspace_meta,
                start =>
                    {beamtalk_workspace_meta, start_link, [
                        #{
                            workspace_id => WorkspaceId,
                            project_path => ProjectPath,
                            created_at => erlang:system_time(second),
                            repl_port => TcpPort
                        }
                    ]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_workspace_meta]
            }

            %% Singleton actors — workspace singletons (ADR 0010 Phase 2, ADR 0019 Phase 4)
            %% These assume beamtalk_stdlib has already been started elsewhere in the system.
            %% Each registers via gen_server name registration ({local, Name}).
            %% Specs derived from beamtalk_workspace_config:singletons/0.
            %%
            %% Note: actor registry is interleaved between singletons because
            %% WorkspaceInterface's methods query the registry.
        ] ++ singleton_child_specs() ++
            [
                %% Bootstrap worker — sets singleton class variables (ADR 0019 Phase 2)
                %% and activates compiled project modules (BT-739).
                %% Must start after all singletons but before REPL server accepts connections.
                %% Monitors singleton PIDs and re-sets class vars on restart.
                #{
                    id => beamtalk_workspace_bootstrap,
                    start => {beamtalk_workspace_bootstrap, start_link, [ProjectPath]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [beamtalk_workspace_bootstrap]
                },

                %% REPL TCP server (session-per-connection architecture)
                #{
                    id => beamtalk_repl_server,
                    start =>
                        {beamtalk_repl_server, start_link, [
                            #{
                                port => TcpPort,
                                workspace_id => WorkspaceId,
                                bind_addr => BindAddr,
                                web_port => WebPort
                            }
                        ]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [beamtalk_repl_server]
                },

                %% Idle monitor for auto-cleanup (only if enabled)
                #{
                    id => beamtalk_idle_monitor,
                    start =>
                        {beamtalk_idle_monitor, start_link, [
                            #{
                                enabled => AutoCleanup,
                                max_idle_seconds => MaxIdleSeconds
                            }
                        ]},
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
                    % Give actors time to shut down gracefully
                    shutdown => infinity,
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

%%% Singleton Child Specs

%% @private Generate supervisor child specs for workspace singletons.
%% Interleaves the actor registry before WorkspaceInterface, which
%% depends on it.
singleton_child_specs() ->
    Singletons = beamtalk_workspace_config:singletons(),
    {Before, After} = lists:partition(
        fun(#{binding_name := N}) -> N =/= 'Workspace' end,
        Singletons
    ),
    lists:map(fun singleton_to_child_spec/1, Before) ++
        [
            #{
                id => beamtalk_actor_registry,
                start => {beamtalk_repl_actors, start_link, [registered]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_repl_actors]
            }
        ] ++
        lists:map(fun singleton_to_child_spec/1, After).

%% @private Convert a singleton config to a supervisor child spec.
singleton_to_child_spec(#{binding_name := BindingName, module := Module, start_args := Args}) ->
    #{
        id => Module,
        start => {Module, start_link, [{local, BindingName} | Args]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    }.

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
    case beamtalk_platform:home_dir() of
        false ->
            ?LOG_WARNING(
                "HOME/USERPROFILE not set; skipping file logger",
                #{workspace_id => WorkspaceId}
            ),
            ok;
        Home ->
            LogFile = filename:join([
                Home,
                ".beamtalk",
                "workspaces",
                binary_to_list(WorkspaceId),
                "workspace.log"
            ]),
            case filelib:ensure_dir(LogFile) of
                ok ->
                    HandlerConfig = #{
                        config => #{
                            file => LogFile,
                            % 1 MB per file
                            max_no_bytes => 1048576,
                            max_no_files => 5
                        },
                        level => debug,
                        formatter =>
                            {logger_formatter, #{
                                template => [time, " [", level, "] ", mfa, " ", msg, "\n"],
                                single_line => true
                            }}
                    },
                    %% Lower primary logger level to debug so file handler captures all events.
                    %% Console handler keeps its own level filter unchanged.
                    logger:set_primary_config(level, debug),
                    case logger:add_handler(beamtalk_file_log, logger_std_h, HandlerConfig) of
                        ok ->
                            ?LOG_INFO("Workspace log file: ~s", [LogFile]),
                            ok;
                        {error, {already_exist, _}} ->
                            ?LOG_INFO(
                                "Reusing existing workspace file logger",
                                #{path => LogFile}
                            ),
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING(
                                "Failed to add file logger",
                                #{reason => Reason, path => LogFile}
                            ),
                            ok
                    end;
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Failed to create log directory",
                        #{path => LogFile, reason => Reason}
                    ),
                    ok
            end
    end.
