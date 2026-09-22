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

Architecture (from ADR 0004):
```
beamtalk_workspace_sup
  ├─ beamtalk_workspace_meta      % Metadata (project path, created_at)
  ├─ beamtalk_workspace_changelog % Append-only ChangeLog (ADR 0082)
  ├─ beamtalk_transcript_stream    % Transcript singleton (ADR 0010, Actor)
  ├─ beamtalk_actor_registry       % Workspace-wide actor registry
  ├─ beamtalk_workspace_bootstrap % Class var bootstrap (ADR 0019)
  │     (also initialises sealed Object singletons: BeamtalkInterface, WorkspaceInterface)
  ├─ beamtalk_actor_sup           % Supervises user actors
  │   -- mode => workspace only below this line --
  ├─ beamtalk_workspace_signature_store % Signature-generation store (ADR 0105)
  ├─ beamtalk_workspace_shape_store % Shape-generation store (ADR 0105)
  ├─ beamtalk_alias_xref          % Alias-name -> dependent-class index (ADR 0108)
  ├─ beamtalk_workspace_shape_recheck_worker % Serialised shape re-check queue (ADR 0105)
  ├─ beamtalk_workspace_findings_store % Reload-induced findings store (ADR 0105)
  │   -- mode => workspace, or mode => release with console => true --
  ├─ beamtalk_session_sup         % Supervises session shell processes (before repl_server)
  ├─ beamtalk_repl_server         % TCP server (session-per-connection)
  │   -- mode => workspace only --
  └─ beamtalk_idle_monitor        % Tracks activity, self-terminates if idle
```

## Modes (ADR 0125 §1.4)

The config's required `mode` key selects the child set:

| | `run` | `workspace` | `release` |
|---|---|---|---|
| bootstrap, `beamtalk_actor_sup`, singletons | ✓ | ✓ | ✓ |
| `beamtalk_compiler` app | ✓ (unless `start_compiler => false`) | ✓ | only with `include_compiler => true` |
| workspace file logger | ✗ | ✓ | ✗ |
| ChangeLog | memory-only | on disk | memory-only |
| ADR 0105 stores + recheck worker, `alias_xref` | ✗ | ✓ | ✗ |
| `beamtalk_session_sup` + `beamtalk_repl_server` | ✗ | ✓ | only with `console => true` |
| `beamtalk_idle_monitor` | ✗ | ✓ | never |

`init/1` also records the node's capabilities (`beamtalk_capability:set/1`),
which is what makes a `release` node refuse compiler and workspace
operations (ADR 0125 §1.5).
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1]).

-type mode() :: beamtalk_capability:mode().

-type workspace_config() :: #{
    workspace_id => binary(),
    project_path => binary() | undefined,
    %% Required: `run` (no REPL), `workspace` (live development) or `release`
    %% (an OTP release) — ADR 0125 §1.4.
    mode := mode(),
    %% `release` mode only: start the REPL listener (default `false`).
    console => boolean(),
    %% `release` mode only: the release bundles the compiler, so start it and
    %% re-enable the compiler-dependent ops (default `false`; ADR 0125 §1.5).
    include_compiler => boolean(),
    %% `run` / `workspace` modes only: `false` skips starting the compiler app
    %% (a packaged escript ships no compiler port). Default `true`.
    start_compiler => boolean(),
    tcp_port => inet:port_number() | undefined,
    bind_addr => inet:ip4_address(),
    auto_cleanup => boolean(),
    %% web_port (the Phase-1 browser HTTP listener) has been removed.
    max_idle_seconds => integer()
}.

-export_type([workspace_config/0, mode/0]).

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
    Mode = config_mode(Config),
    WorkspaceId = maps:get(workspace_id, Config),
    ProjectPath = maps:get(project_path, Config, undefined),
    Console = starts_console(Mode, Config),
    TcpPort = maps:get(tcp_port, Config, undefined),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
    AutoCleanup = maps:get(auto_cleanup, Config, true),
    MaxIdleSeconds = maps:get(max_idle_seconds, Config, 3600 * 4),

    %% Fail fast: a mode that starts the REPL listener requires a TCP port —
    %% run mode, and a release without a console, do not.
    case {Console, TcpPort} of
        {true, undefined} -> erlang:error({bad_config, missing_tcp_port_for_repl});
        _ -> ok
    end,

    %% Record what this node may do before any child (or REPL op) runs, so a
    %% release refuses compiler and workspace operations (ADR 0125 §1.5).
    ok = beamtalk_capability:set(#{
        mode => Mode, include_compiler => maps:get(include_compiler, Config, false)
    }),

    %% Set up file logging before children start (they may log during init).
    %% Workspace mode only — run and release modes create no workspace
    %% artifacts on disk.
    case Mode of
        workspace -> setup_file_logger(WorkspaceId);
        _ -> ok
    end,

    %% Set up WebSocket log handler for live log streaming.
    %% Registered in all modes — subscribers opt in per-session.
    setup_ws_log_handler(),

    %% Start the compiler application (ADR 0022)
    %% This is started dynamically rather than as a static app dependency
    %% because the compiler port binary may not be available in all environments
    %% (e.g., unit tests).
    %%
    %% A packaged escript (ADR 0099 §4) runs precompiled classes and ships no
    %% compiler-port binary, so it sets `start_compiler => false` to skip this —
    %% otherwise the compiler server crashes on the missing port and dumps a
    %% spurious SASL crash report to stderr. A release ships no compiler unless
    %% it was built with `include_compiler` (ADR 0125 §1.5).
    %%
    %% The two-clause match is intentionally fail-loud — an unexpected value is
    %% a programming error, not a runtime input, so it should crash rather than
    %% be silently coerced.
    case starts_compiler(Mode, Config) of
        false ->
            ok;
        true ->
            case application:ensure_all_started(beamtalk_compiler) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to start beamtalk_compiler app", #{
                        reason => Reason, domain => [beamtalk, runtime]
                    })
            end
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
                            mode => Mode
                        }
                    ]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_workspace_meta]
            },

            %% ChangeLog gen_server (ADR 0082 Phase 1).
            %% Append-only log of live in-memory method mutations; dirty-state +
            %% undo store, consumed cross-surface (REPL/MCP/LSP/browser). Owns the
            %% two-part on-disk persistence under <workspace>/changes/. Depends only
            %% on workspace_id, so it can start right after meta. In run and
            %% release modes the id is dropped to undefined so the log stays
            %% memory-only — no workspace artifacts on disk, matching workspace_meta.
            #{
                id => beamtalk_workspace_changelog,
                start =>
                    {beamtalk_workspace_changelog, start_link, [
                        #{workspace_id => changelog_workspace_id(Mode, WorkspaceId)}
                    ]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [beamtalk_workspace_changelog]
            }

            %% Actor singleton — workspace singletons (ADR 0010 Phase 2, ADR 0019 Phase 4)
            %% These assume beamtalk_stdlib has already been started elsewhere in the system.
            %% Each registers via gen_server name registration ({local, Name}).
            %% Specs derived from beamtalk_workspace_config:singletons/0.
            %% (BeamtalkInterface and WorkspaceInterface are value singletons, bootstrapped
            %% by beamtalk_workspace_bootstrap after the actor registry is started.)
        ] ++ singleton_child_specs() ++
            [
                %% The bespoke class-loaded / bindings-changed /
                %% flush-completion pub/sub gen_servers were retired. Those
                %% workspace push streams now ride the SystemAnnouncer bus
                %% (`beamtalk_announcements`, started under `beamtalk_runtime_sup`)
                %% and are subscribed through `beamtalk_repl_subscriptions`.

                %% Bootstrap worker — sets singleton class variables (ADR 0019 Phase 2)
                %% and activates compiled project modules.
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
            repl_child_specs(Mode, #{
                console => Console,
                tcp_port => TcpPort,
                workspace_id => WorkspaceId,
                bind_addr => BindAddr,
                auto_cleanup => AutoCleanup,
                max_idle_seconds => MaxIdleSeconds
            }),

    {ok, {SupFlags, ChildSpecs}}.

%%% Mode

-doc """
Read and validate the required `mode` key. There is no default and no
`repl => boolean()` compatibility clause: a missing, stale or unknown key is
a programming error at the call site, so it fails loudly.
""".
-spec config_mode(map()) -> mode().
config_mode(#{repl := _}) ->
    erlang:error({bad_config, {removed_key, repl, use_mode}});
config_mode(#{mode := Mode}) when Mode =:= run; Mode =:= workspace; Mode =:= release ->
    Mode;
config_mode(#{mode := Mode}) ->
    erlang:error({bad_config, {invalid_mode, Mode}});
config_mode(_Config) ->
    erlang:error({bad_config, missing_mode}).

-doc """
Whether this mode starts the REPL listener (`beamtalk_session_sup` +
`beamtalk_repl_server`): always in workspace mode, never in run mode, and in
release mode only when the config opts in with `console => true`.
""".
-spec starts_console(mode(), map()) -> boolean().
starts_console(run, _Config) -> false;
starts_console(workspace, _Config) -> true;
starts_console(release, Config) -> maps:get(console, Config, false).

-doc """
Whether to start the `beamtalk_compiler` application. Run and workspace modes
start it unless `start_compiler => false` (a packaged escript); a release
ships no compiler unless built with `include_compiler => true`.
""".
-spec starts_compiler(mode(), map()) -> boolean().
starts_compiler(release, Config) -> maps:get(include_compiler, Config, false);
starts_compiler(_Mode, Config) -> maps:get(start_compiler, Config, true).

%%% REPL Child Specs

-doc """
Return the mode-dependent child specs that follow `beamtalk_actor_sup`
(ADR 0125 §1.4):

- `run` — none: no TCP listener, no idle monitor, no session supervisor, no
  ADR 0105 stores.
- `workspace` — the ADR 0105 / ADR 0108 live-development stores, then the
  REPL listener, then the idle monitor.
- `release` — the REPL listener only, and only with `console => true`. Never
  the stores (a release has no live-edit path to feed them) and never the
  idle monitor (it calls `init:stop/0`, which would halt a healthy
  production node that simply had no REPL traffic).

Wherever the listener is started, `beamtalk_session_sup` precedes
`beamtalk_repl_server` (see `console_child_specs/1`).
""".
-spec repl_child_specs(mode(), map()) -> [supervisor:child_spec()].
repl_child_specs(run, _Opts) ->
    [];
repl_child_specs(workspace, Opts) ->
    live_development_child_specs() ++ console_child_specs(Opts) ++ [idle_monitor_child_spec(Opts)];
repl_child_specs(release, #{console := true} = Opts) ->
    console_child_specs(Opts);
repl_child_specs(release, _Opts) ->
    [].

-doc """
The workspace-mode-only live-development children: the ADR 0105 signature /
shape / findings stores and recheck worker, and the ADR 0108 alias xref.
""".
-spec live_development_child_specs() -> [supervisor:child_spec()].
live_development_child_specs() ->
    [
        %% Signature-generation store (ADR 0105 Phase 1).
        %% Per-selector previous-generation method signatures, captured at
        %% patch time so a diff survives the class-state metadata wipe. Only
        %% meaningful in workspace mode — run and release modes execute a
        %% precompiled artifact with no live-edit path (no way to reach
        %% beamtalk_repl_loader:install_method/9 without a compiler and a
        %% working tree), so there is nothing for it to capture there.
        %% Started before session_sup/repl_server so
        %% it's ready before any REPL connection could trigger an install.
        #{
            id => beamtalk_workspace_signature_store,
            start => {beamtalk_workspace_signature_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_signature_store]
        },

        %% Shape-generation store (ADR 0105 Phase 2).
        %% Per-class previous-generation `state:`/`field:` slot sets, captured
        %% around a full class-body reload so a diff survives the module
        %% replacement (companion to the signature store above, for shape
        %% rather than per-method signature changes). Same workspace-mode-only
        %% rationale as the signature store.
        #{
            id => beamtalk_workspace_shape_store,
            start => {beamtalk_workspace_shape_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_shape_store]
        },

        %% Alias-name -> dependent-class index (ADR 0108 hot-reload re-check
        %% trigger). Populated at class-install time from the
        %% compiler port's `referenced_aliases` response field; consulted by
        %% `beamtalk_recheck:trigger_alias_change/1` so a live alias
        %% redefinition re-checks only its recorded dependents instead of
        %% sweeping every live class. Same workspace-mode-only rationale as its
        %% siblings above.
        #{
            id => beamtalk_alias_xref,
            start => {beamtalk_alias_xref, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_alias_xref]
        },

        %% Shape re-check worker (ADR 0105 Phase 2).
        %% Serialises `beamtalk_repl_loader:activate_module/3`'s shape
        %% re-check behind a single gen_server mailbox so a burst of
        %% class-body reloads can't flood `beamtalk_compiler_server` (ADR
        %% 0022) with unbounded concurrent recheck traffic — see that
        %% worker's moduledoc. Started right after the shape store it reads
        %% from (via `beamtalk_repl_loader:maybe_trigger_shape_recheck/1`).
        #{
            id => beamtalk_workspace_shape_recheck_worker,
            start => {beamtalk_workspace_shape_recheck_worker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_shape_recheck_worker]
        },

        %% Reload-induced findings store (ADR 0105 Phase 1).
        %% Live, session-only findings keyed by caller class, published to
        %% every surface (LSP / REPL / workspace UI) via the
        %% `'ReloadCheckCompleted'` system announcement
        %% (`beamtalk_repl_loader:maybe_trigger_recheck/4`). Downstream of the
        %% signature and shape stores (a re-check needs a classified diff
        %% first), so it starts right after them — before any REPL connection
        %% could trigger an install.
        #{
            id => beamtalk_workspace_findings_store,
            start => {beamtalk_workspace_findings_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_workspace_findings_store]
        }
    ].

-doc """
The REPL listener: `beamtalk_session_sup` then `beamtalk_repl_server`, in
that order (see the comment on `beamtalk_session_sup` below).
""".
-spec console_child_specs(map()) -> [supervisor:child_spec()].
console_child_specs(#{tcp_port := TcpPort, workspace_id := WorkspaceId, bind_addr := BindAddr}) ->
    [
        %% Session supervisor (one child per REPL connection).
        %%
        %% MUST start before beamtalk_repl_server: the REPL server's init/1 binds
        %% the cowboy `/ws` listener AND writes the port file (the CLI's readiness
        %% signal) before returning. If session_sup started after repl_server, the
        %% CLI could discover the port and open a `/ws` connection during the window
        %% where session_sup does not yet exist, so beamtalk_ws_handler's
        %% create_session -> supervisor:start_child(beamtalk_session_sup, _) exits
        %% with `noproc`, the handler crashes, and the connection is dropped. The CLI
        %% sees "port accepting TCP but WebSocket health check failed" and retries on
        %% a fresh node (flaky workspace-startup CI failures). Ordering
        %% session_sup first gates the port file behind a ready session tier.
        #{
            id => beamtalk_session_sup,
            start => {beamtalk_session_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [beamtalk_session_sup]
        },

        %% REPL WebSocket server (session-per-connection architecture)
        #{
            id => beamtalk_repl_server,
            start =>
                {beamtalk_repl_server, start_link, [
                    #{
                        port => TcpPort,
                        workspace_id => WorkspaceId,
                        bind_addr => BindAddr
                    }
                ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [beamtalk_repl_server]
        }
    ].

-doc """
The idle monitor (workspace mode only). Its `max_idle_seconds` expiry calls
`init:stop/0` and halts the node, which is why no other mode starts it.
""".
-spec idle_monitor_child_spec(map()) -> supervisor:child_spec().
idle_monitor_child_spec(#{auto_cleanup := AutoCleanup, max_idle_seconds := MaxIdleSeconds}) ->
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
    }.

%%% ChangeLog workspace id

-doc """
Resolve the workspace id passed to the ChangeLog gen_server.
Run and release modes return `undefined` so the log stays memory-only and
writes no artifacts to disk; workspace mode passes the real id through.
""".
-spec changelog_workspace_id(mode(), binary()) -> binary() | undefined.
changelog_workspace_id(run, _WorkspaceId) -> undefined;
changelog_workspace_id(workspace, WorkspaceId) -> WorkspaceId;
changelog_workspace_id(release, _WorkspaceId) -> undefined.

%%% Singleton Child Specs

-doc """
Generate supervisor child specs for actor workspace singletons.
Starts actor singletons from beamtalk_workspace_config:singletons/0, then
the actor registry. Value singletons (BeamtalkInterface, WorkspaceInterface)
are not started here — they are bootstrapped by beamtalk_workspace_bootstrap.
""".
singleton_child_specs() ->
    Singletons = beamtalk_workspace_config:singletons(),
    [singleton_to_child_spec(S) || S <- Singletons] ++
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
                    % elp:fixme W0011 intentional cross-app read
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
                    %% Use `Beamtalk logLevel: #debug` at runtime to change dynamically.
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

%%% WebSocket Log Handler

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
