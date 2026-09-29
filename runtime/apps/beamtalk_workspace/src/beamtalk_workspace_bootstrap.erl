%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_bootstrap).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Bootstrap worker for the workspace (ADR 0019 Phase 2).

Owns the `bind:as:` user-bindings ETS table and activates compiled project
modules at startup. When a project path is provided, first activates dependency classes from `_build/deps/*/ebin/`
and native code paths, then scans `_build/dev/ebin/` for `bt@*.beam` modules
(excluding `bt@stdlib@*`) and calls `register_class/0` on each, making them
visible in the class registry without requiring `:load`.
""".

-export([start_link/0, start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).
-export([activate_project_modules/1, activate_dependency_modules/1]).

%% Backwards-compatible re-exports — callers should migrate to
%% beamtalk_module_activation directly.
-export([find_bt_modules_in_dir/1, sort_modules_by_dependency/2, is_valid_module_name/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-record(state, {}).

-doc "Start the bootstrap worker without project module activation.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(undefined).

-doc """
Start the bootstrap worker.
When ProjectPath is a binary path to a project root, compiled modules from
`{ProjectPath}/_build/dev/ebin/` matching `bt@*` (excluding `bt@stdlib@*`)
are activated during init. Pass `undefined` to skip activation.
""".
-spec start_link(binary() | undefined) -> {ok, pid()} | {error, term()}.
start_link(ProjectPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ProjectPath], []).

init([ProjectPath]) ->
    beamtalk_logging_config:set_domain(runtime),
    %% Create the user-bindings ETS table here so that this long-lived process
    %% owns it. If created inside a short-lived eval worker instead, the table
    %% would be deleted when that worker exits (ETS tables are deleted on owner
    %% process exit unless an heir is specified).
    beamtalk_workspace_interface_primitives:create_bindings_table(),
    %% Ensure beamtalk_interface is loaded so that all its exported
    %% function names (e.g. findClass, allClasses) are in the atom table.
    %% The sealed-Object Beamtalk dispatches via beamtalk_message_dispatch
    %% which uses list_to_existing_atom to resolve selector→function name; if
    %% the module is not yet loaded, the atom won't exist and dispatch fails.
    _ = code:ensure_loaded(beamtalk_interface),
    %% Activate project modules synchronously before returning so that
    %% beamtalk_repl_server (the next child) does not write the port file
    %% until all compiled project classes are registered and visible.
    activate_project_modules(ProjectPath),
    {ok, #state{}}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% Internal functions

-doc """
Activate compiled project modules from _build/dev/ebin/.

Delegates to `beamtalk_module_activation:activate_ebin/2` with a callback
that registers modules in workspace_meta and stores source text for
`ClassName >> method => body` support.
""".
-spec activate_project_modules(binary() | undefined) -> ok.
activate_project_modules(undefined) ->
    ok;
activate_project_modules(ProjectPath) when is_binary(ProjectPath), byte_size(ProjectPath) > 0 ->
    AbsPath = binary_to_list(ProjectPath),
    %% Activate dependency classes BEFORE project classes so that project code
    %% (e.g. Orchestrator.initialize calling config on a dependency class) can
    %% reference them immediately at startup, not only after :sync.
    _ = activate_dependency_modules(AbsPath),
    EbinDir = filename:absname(
        filename:join([AbsPath, "_build", "dev", "ebin"])
    ),
    {ok, Errors} = beamtalk_module_activation:activate_ebin(EbinDir, #{
        on_activate => fun on_project_module_activated/1
    }),
    case Errors of
        [] ->
            ok;
        _ ->
            ?LOG_WARNING(
                "Bootstrap: ~b project module(s) failed to activate",
                [length(Errors)],
                #{errors => Errors, domain => [beamtalk, runtime]}
            )
    end;
activate_project_modules(_Other) ->
    ok.

-doc """
Activate pre-compiled dependency modules from _build/deps/ and native paths.

Delegates to `beamtalk_module_activation:activate_dependencies/2` with a
workspace-specific callback that registers each module in workspace_meta.
Called by both bootstrap init (startup) and `:sync` (repl_ops_load).

Returns a (possibly empty) list of `{Name, Reason}` error pairs, where
`Name` may be either a module name or an application name for `.app` load
failures reported by dependency activation.
""".
-spec activate_dependency_modules(string()) -> [{atom(), term()}].
activate_dependency_modules(ProjectPath) ->
    Opts = #{
        on_activate => fun({Module, SourcePath}) ->
            beamtalk_workspace_meta:register_module(Module, SourcePath)
        end
    },
    DepErrors = beamtalk_module_activation:activate_dependencies(ProjectPath, Opts),
    case DepErrors of
        [] ->
            ok;
        _ ->
            ?LOG_WARNING(
                "Dependency activation: ~b module(s) failed to activate",
                [length(DepErrors)],
                #{errors => DepErrors, domain => [beamtalk, runtime]}
            )
    end,
    DepErrors.

-doc """
Callback for project module activation.
Registers the module in workspace_meta and stores source text.
""".
-spec on_project_module_activated({module(), string() | undefined}) -> ok.
on_project_module_activated({Module, SourcePath}) ->
    beamtalk_workspace_meta:register_module(Module, SourcePath),
    store_bootstrap_class_source(Module, SourcePath).

-doc """
Read source file content and store per-class in workspace_meta so
that `ClassName >> method => body` works for bootstrap-loaded classes.
""".
-spec store_bootstrap_class_source(module(), string() | undefined) -> ok.
store_bootstrap_class_source(_ModuleName, undefined) ->
    ok;
store_bootstrap_class_source(ModuleName, SourcePath) ->
    case file:read_file(SourcePath) of
        {ok, Binary} ->
            Source = binary_to_list(Binary),
            ClassNames = beamtalk_module_activation:extract_class_names(ModuleName),
            lists:foreach(
                fun(ClassName) ->
                    beamtalk_workspace_meta:set_class_source(
                        atom_to_binary(ClassName, utf8), Source
                    )
                end,
                ClassNames
            );
        {error, Reason} ->
            ?LOG_DEBUG(
                "Bootstrap: could not read source file for class source storage",
                #{
                    module => ModuleName,
                    path => SourcePath,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            )
    end.

-doc "Backwards-compatible delegate to beamtalk_module_activation.".
-spec find_bt_modules_in_dir(string()) -> [module()].
find_bt_modules_in_dir(Dir) ->
    beamtalk_module_activation:find_bt_modules_in_dir(Dir).

-doc "Backwards-compatible delegate to beamtalk_module_activation.".
-spec sort_modules_by_dependency(string(), [module()]) -> [module()].
sort_modules_by_dependency(EbinDir, Modules) ->
    beamtalk_module_activation:sort_modules_by_dependency(EbinDir, Modules).

-doc "Backwards-compatible delegate to beamtalk_module_activation.".
-spec is_valid_module_name(string()) -> boolean().
is_valid_module_name(Name) ->
    beamtalk_module_activation:is_valid_module_name(Name).
