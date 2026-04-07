%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_sup).
-behaviour(supervisor).

%%% **DDD Context:** Workspace Context

-moduledoc """
Per-workspace supervisor

This supervisor manages all components of a persistent workspace:
- Actor registry (workspace-wide actor tracking)
- REPL TCP server for client connections
- Idle monitor for auto-cleanup
- Workspace metadata tracking
- Actor supervision (shared across sessions)
- Session supervision (one per REPL connection)

Architecture (from ADR 0004, implemented in BT-262):
```
beamtalk_workspace_sup
  ├─ beamtalk_workspace_meta      % Metadata (project path, created_at)
  ├─ beamtalk_transcript_stream    % Transcript singleton (ADR 0010, Actor)
  ├─ beamtalk_actor_registry       % Workspace-wide actor registry
  ├─ beamtalk_workspace_bootstrap % Class var bootstrap (ADR 0019)
  │     (also initialises sealed Object singletons: BeamtalkInterface, WorkspaceInterface)
  ├─ beamtalk_repl_server         % TCP server (session-per-connection)
  ├─ beamtalk_idle_monitor        % Tracks activity, self-terminates if idle
  ├─ beamtalk_actor_sup           % Supervises user actors
  └─ beamtalk_session_sup         % Supervises session shell processes
```
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

-type workspace_config() :: #{
    workspace_id => binary(),
    project_path => binary() | undefined,
    repl => boolean(),
    tcp_port => inet:port_number() | undefined,
    bind_addr => inet:ip4_address(),
    auto_cleanup => boolean(),
    max_idle_seconds => integer()
}.

-export_type([workspace_config/0]).

-doc "Start the workspace supervisor.".
-spec start_link(workspace_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% Extract configuration
    WorkspaceId = maps:get(workspace_id, Config),
    ProjectPath = maps:get(project_path, Config, undefined),
    Repl = maps:get(repl, Config, true),
    TcpPort = maps:get(tcp_port, Config, undefined),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
    WebPort = maps:get(web_port, Config, undefined),
    AutoCleanup = maps:get(auto_cleanup, Config, true),
    MaxIdleSeconds = maps:get(max_idle_seconds, Config, 3600 * 4),

    %% Fail fast: REPL mode requires a TCP port — run mode (repl=false) does not.
    case {Repl, TcpPort} of
        {true, undefined} -> erlang:error({bad_config, missing_tcp_port_for_repl});
        _ -> ok
    end,

    %% Set up file logging before children start (they may log during init).
    %% Skipped in run mode — no workspace artifacts should be created on disk.
    case Repl of
        true -> setup_file_logger(WorkspaceId);
        false -> ok
    end,

    %% Set up WebSocket log handler for live log streaming (BT-1433).
    %% Registered in all modes — subscribers opt in per-session.
    setup_ws_log_handler(),

    %% Start the compiler application (ADR 0022)
    %% This is started dynamically rather than as a static app dependency
    %% because the compiler port binary may not be available in all environments
    %% (e.g., unit tests).
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start beamtalk_compiler app", #{
                reason => Reason, domain => [beamtalk, runtime]
            })
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
                            repl_port => TcpPort,
                            repl => Repl
                        }
                    ]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_workspace_meta]
            }

            %% Actor singleton — workspace singletons (ADR 0010 Phase 2, ADR 0019 Phase 4)
            %% These assume beamtalk_stdlib has already been started elsewhere in the system.
            %% Each registers via gen_server name registration ({local, Name}).
            %% Specs derived from beamtalk_workspace_config:singletons/0.
            %% (BeamtalkInterface and WorkspaceInterface are value singletons, bootstrapped
            %% by beamtalk_workspace_bootstrap after the actor registry is started.)
        ] ++ singleton_child_specs() ++
            [
                %% Class-loaded event pub/sub (BT-1020)
                %% Must start before bootstrap so class load events are captured.
                #{
                    id => beamtalk_class_events,
                    start => {beamtalk_class_events, start_link, [registered]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [beamtalk_class_events]
                },

                %% Bindings-changed event pub/sub
                %% Broadcasts to all WS clients after each successful eval so the
                %% VS Code sidebar refreshes regardless of which session ran the eval.
                #{
                    id => beamtalk_bindings_events,
                    start => {beamtalk_bindings_events, start_link, [registered]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [beamtalk_bindings_events]
                },

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

                %% Actor supervisor (shared across all sessions)
                #{
                    id => beamtalk_actor_sup,
                    start => {beamtalk_actor_sup, start_link, []},
                    restart => permanent,
                    % Give actors time to shut down gracefully
                    shutdown => infinity,
                    type => supervisor,
                    modules => [beamtalk_actor_sup]
                }
            ] ++
            repl_child_specs(
                Repl, TcpPort, WorkspaceId, BindAddr, WebPort, AutoCleanup, MaxIdleSeconds
            ),

    {ok, {SupFlags, ChildSpecs}}.

%%% REPL Child Specs

-doc """
Return child specs for REPL-mode children.
When repl=false (run mode), these are omitted: no TCP listener, no idle monitor,
no per-connection session supervisor.
""".
repl_child_specs(false, _TcpPort, _WorkspaceId, _BindAddr, _WebPort, _AutoCleanup, _MaxIdleSeconds) ->
    [];
repl_child_specs(true, TcpPort, WorkspaceId, BindAddr, WebPort, AutoCleanup, MaxIdleSeconds) ->
    [
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

        %% Session supervisor (one child per REPL connection)
        #{
            id => beamtalk_session_sup,
            start => {beamtalk_session_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [beamtalk_session_sup]
        }
    ].

%%% Singleton Child Specs

-doc """
Generate supervisor child specs for actor workspace singletons.
Starts actor singletons from beamtalk_workspace_config:singletons/0, then
the actor registry. Value singletons (BeamtalkInterface, WorkspaceInterface)
are not started here — they are bootstrapped by beamtalk_workspace_bootstrap.
""".
singleton_child_specs() ->
    Singletons = beamtalk_workspace_config:singletons(),
    lists:map(fun singleton_to_child_spec/1, Singletons) ++
        [
            #{
                id => beamtalk_actor_registry,
                start => {beamtalk_repl_actors, start_link, [registered]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_repl_actors]
            }
        ].

-doc "Convert a singleton config to a supervisor child spec.".
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

-doc """
Set up a file-based logger handler for the workspace node.
Writes to ~/.beamtalk/workspaces/{workspace_id}/workspace.log with rotation.
Disabled when BEAMTALK_NO_FILE_LOG=1 is set.
""".
-spec setup_file_logger(binary()) -> ok.
setup_file_logger(WorkspaceId) ->
    case os:getenv("BEAMTALK_NO_FILE_LOG") of
        "1" ->
            ok;
        _ ->
            do_setup_file_logger(WorkspaceId)
    end.

do_setup_file_logger(WorkspaceId) ->
    case beamtalk_platform:home_dir() of
        false ->
            ?LOG_WARNING(
                "HOME/USERPROFILE not set; skipping file logger",
                #{workspace_id => WorkspaceId, domain => [beamtalk, runtime]}
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
                    %% Read log level from app env (set by --log-level CLI flag),
                    %% defaulting to info.
                    Level = application:get_env(beamtalk_runtime, log_level, info),
                    HandlerConfig = #{
                        config => #{
                            file => LogFile,
                            % 10 MB per file
                            max_no_bytes => 10485760,
                            max_no_files => 5
                        },
                        level => Level,
                        formatter =>
                            {beamtalk_json_formatter, #{}}
                    },
                    %% Set primary logger level (from --log-level CLI flag, default info).
                    %% Use `Logger setLevel: #debug` at runtime to change dynamically.
                    logger:set_primary_config(level, Level),
                    case logger:add_handler(beamtalk_file_log, logger_std_h, HandlerConfig) of
                        ok ->
                            %% Suppress redundant proc_lib crash reports for beamtalk
                            %% actors — they just say "process crash: unknown" and add
                            %% nothing beyond the gen_server/supervisor reports.
                            logger:add_handler_filter(
                                beamtalk_file_log,
                                beamtalk_suppress_proc_lib_crash,
                                {fun beamtalk_log_filter:filter_proc_lib_crash/2, []}
                            ),
                            ?LOG_INFO("Workspace log file: ~s", [LogFile], #{
                                domain => [beamtalk, runtime]
                            }),
                            ok;
                        {error, {already_exist, _}} ->
                            ?LOG_INFO(
                                "Reusing existing workspace file logger",
                                #{path => LogFile, domain => [beamtalk, runtime]}
                            ),
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING(
                                "Failed to add file logger",
                                #{reason => Reason, path => LogFile, domain => [beamtalk, runtime]}
                            ),
                            ok
                    end;
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Failed to create log directory",
                        #{path => LogFile, reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    ok
            end
    end.

%%% WebSocket Log Handler (BT-1433)

-doc """
Register the WebSocket log handler with OTP logger.
The handler forwards log events to subscribed WebSocket sessions.
""".
-spec setup_ws_log_handler() -> ok.
setup_ws_log_handler() ->
    case
        logger:add_handler(beamtalk_ws_log, beamtalk_ws_log_handler, #{
            level => debug
        })
    of
        ok ->
            ok;
        {error, {already_exist, _}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to add WebSocket log handler",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            ok
    end.
