%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_interface_primitives).

%%% **DDD Context:** Workspace Context

-moduledoc """
Primitive implementations for the WorkspaceInterface sealed Object.

Implements methods for the WorkspaceInterface class. WorkspaceInterface is
a `sealed Object subclass:` (value type, no gen_server process). Methods
are called via Erlang FFI from the compiled Beamtalk module.

## State management

User-registered bindings (`bind:as:` / `unbind:`) are stored in a public
named ETS table `beamtalk_wi_user_bindings` keyed by `Name` (atom).
Since WorkspaceInterface is a singleton (one per workspace), no per-process
keying is needed. The ETS table is normally created during workspace startup
via `create_bindings_table/0` (called by `beamtalk_workspace_bootstrap`),
which ensures the long-lived bootstrap process owns it. A lazy fallback via
`ensure_bindings_table/0` is also available for cases where the table is
accessed before bootstrap completes. Both paths produce the same singleton
table. The table is accessible from external processes via
`get_user_bindings/0` and `get_session_bindings/0`.

## External API

`get_user_bindings/0` and `get_session_bindings/0` are called by
`beamtalk_repl_eval` and `beamtalk_repl_shell` to inject workspace bindings
into REPL session state. Workspace readiness is detected via
`whereis(beamtalk_workspace_meta)`.

## Methods

| Selector      | Description                                         |
|---------------|-----------------------------------------------------|
| `actors'      | List all live actor object references               |
| `actorAt:'    | Look up actor by pid string                         |
| `classes'     | List all loaded user classes                        |
| `load:'       | Compile and load a .bt file                         |
| `globals'     | Full workspace namespace snapshot (Dictionary)      |
| `sync'        | Incremental project sync (compile changed files)   |
| `bind:as:'    | Register a value in workspace namespace             |
| `unbind:'     | Remove a value from workspace namespace             |
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).
%% Stable external API (called by repl_eval and repl_shell)
-export([get_user_bindings/0, get_session_bindings/0]).
%% Called by beamtalk_workspace_bootstrap to create the ETS table under a
%% long-lived process (prevents table from being deleted when eval workers exit)
-export([create_bindings_table/0]).
%% Direct exports for Erlang FFI calls from sealed Object WorkspaceInterface
-export([actors/0, actorAt/1, classes/0, load/1, globals/0, bind/2, unbind/1, rootSupervisor/0]).
%% Supervisor lifecycle management (BT-1341)
-export([startSupervisor/1, stopSupervisor/1, supervisors/0]).
%% Package reflection (ADR 0070 Phase 5)
-export([dependencies/0]).
%% Project sync (BT-1723)
-export([sync/0]).

%% ETS table name for user workspace bindings
-define(WI_BINDINGS_TABLE, beamtalk_wi_user_bindings).

%%% ============================================================================
%%% dispatch/3 — kept for backward compatibility; delegates to direct functions
%%% ============================================================================

-doc """
Dispatch a primitive method call for WorkspaceInterface.

Retained for backward compatibility. The compiled sealed Object module
uses Erlang FFI calls to the direct exports instead of this dispatch/3.
""".
-spec dispatch(atom(), list(), term()) -> term().
dispatch(actors, [], _Self) ->
    actors();
dispatch('actorAt:', [PidStr], _Self) ->
    actorAt(PidStr);
dispatch(classes, [], _Self) ->
    classes();
dispatch('load:', [Path], _Self) ->
    load(Path);
dispatch(globals, [], _Self) ->
    globals();
dispatch('bind:as:', [Value, Name], _Self) ->
    bind(Value, Name);
dispatch('unbind:', [Name], _Self) ->
    unbind(Name);
dispatch(rootSupervisor, [], _Self) ->
    rootSupervisor();
dispatch('startSupervisor:', [ClassArg], _Self) ->
    startSupervisor(ClassArg);
dispatch('stopSupervisor:', [ClassArg], _Self) ->
    stopSupervisor(ClassArg);
dispatch(supervisors, [], _Self) ->
    supervisors();
dispatch(sync, [], _Self) ->
    sync();
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Err1, <<"To list available selectors, use: Workspace methods">>
        )
    ).

%%% ============================================================================
%%% Direct exports for Erlang FFI (called via ErlangModule proxy)
%%% ============================================================================

-doc """
Return a list of all live actors as beamtalk_object references.
Called via `(Erlang beamtalk_workspace_interface_primitives) actors`.
""".
-spec actors() -> [tuple()].
actors() ->
    handle_actors().

-doc """
Look up a specific actor by pid string.
Called via `(Erlang beamtalk_workspace_interface_primitives) actorAt: pidString`.
""".
-spec actorAt(binary() | list() | term()) -> tuple() | 'nil'.
actorAt(PidStr) ->
    handle_actor_at(PidStr).

-doc """
Return all loaded user classes.
Called via `(Erlang beamtalk_workspace_interface_primitives) classes`.
""".
-spec classes() -> [tuple()].
classes() ->
    handle_classes().

-doc """
Compile and load a .bt file.
Called via `(Erlang beamtalk_workspace_interface_primitives) load: path`.
Returns the loaded class object (or a list of class objects for multi-class files).
""".
-spec load(term()) -> term().
load(Path) ->
    case handle_load(Path) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Return the full workspace namespace snapshot.
Called via `(Erlang beamtalk_workspace_interface_primitives) globals`.
""".
-spec globals() -> map().
globals() ->
    UserBindings = all_user_bindings(),
    handle_globals(UserBindings).

-doc """
Register a value in the workspace namespace under a given atom name.
Called via `(Erlang beamtalk_workspace_interface_primitives) bind: value as: name`.
""".
-spec bind(term(), As :: atom() | term()) -> 'nil'.
bind(Value, Name) ->
    ensure_bindings_table(),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case check_bind_conflicts(AtomName) of
                ok ->
                    ets:insert(?WI_BINDINGS_TABLE, {AtomName, Value}),
                    spawn(fun() -> maybe_warn_loaded_class(AtomName) end),
                    nil;
                {error, Err} ->
                    beamtalk_error:raise(Err)
            end
    end.

-doc """
Remove a registered name from the workspace namespace.
Called via `(Erlang beamtalk_workspace_interface_primitives) unbind: name`.
""".
-spec unbind(atom() | term()) -> 'nil'.
unbind(Name) ->
    ensure_bindings_table(),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case ets:lookup(?WI_BINDINGS_TABLE, AtomName) of
                [] ->
                    Err0 = beamtalk_error:new(name_not_found, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'unbind:'),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Err1,
                            iolist_to_binary([
                                atom_to_binary(AtomName, utf8),
                                <<" is not a registered workspace name">>
                            ])
                        )
                    );
                _ ->
                    ets:delete(?WI_BINDINGS_TABLE, AtomName),
                    nil
            end
    end.

-doc """
Return the OTP application root supervisor, or nil (BT-1191).

Called via `(Erlang beamtalk_workspace_interface_primitives) rootSupervisor`.
Delegates to `beamtalk_supervisor:get_root/0` which reads from the ETS
registry populated by the generated `beamtalk_{appname}_app:start/2`.

Returns the `{beamtalk_supervisor, ClassName, Module, Pid}` tuple when an
OTP application with `[application] supervisor` has started, or the atom
`nil` if no root has been registered.
""".
-spec rootSupervisor() -> tuple() | nil.
rootSupervisor() ->
    beamtalk_supervisor:get_root().

%%% ============================================================================
%%% Supervisor lifecycle management (BT-1341)
%%% ============================================================================

-doc """
Start and attach a user supervisor to the workspace supervision tree.

Called via `(Erlang beamtalk_workspace_interface_primitives) startSupervisor: MySup`.
The class must be a Supervisor or DynamicSupervisor subclass. The supervisor
is started as a dynamic child of `beamtalk_workspace_sup` with `temporary`
restart (user supervisors are not auto-restarted — the user re-attaches
explicitly during iterative development).

If the supervisor is already running under the workspace tree, returns the
existing instance (idempotent).
""".
-spec startSupervisor(term()) -> term().
startSupervisor(ClassArg) ->
    case beamtalk_class_registry:is_class_object(ClassArg) of
        false ->
            raise_start_supervisor_type_error(<<"Expected a Supervisor class">>);
        true ->
            ok
    end,
    ClassPid = element(4, ClassArg),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name(ClassPid),
    case beamtalk_supervisor:is_supervisor(ClassName) of
        false ->
            NameBin = atom_to_binary(ClassName, utf8),
            raise_start_supervisor_type_error(
                iolist_to_binary([NameBin, <<" is not a Supervisor or DynamicSupervisor subclass">>])
            );
        true ->
            ok
    end,
    do_start_supervisor(ClassName, Module).

do_start_supervisor(ClassName, Module) ->
    ChildId = {user_supervisor, ClassName},
    ChildSpec = #{
        id => ChildId,
        start => {Module, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => supervisor,
        modules => [Module]
    },
    case supervisor:start_child(beamtalk_workspace_sup, ChildSpec) of
        {ok, Pid} ->
            {beamtalk_supervisor, ClassName, Module, Pid};
        {error, {already_started, _Pid}} ->
            %% Process is already running. If the child spec is registered
            %% under our workspace supervisor (same ChildId), this is an
            %% idempotent call — return the actual handle from which_children
            %% (not the caller's Module, which may be stale after a reload).
            %% Otherwise, the supervisor was started standalone.
            case workspace_child_handle(ChildId) of
                {ok, Handle} ->
                    Handle;
                not_found ->
                    NameBin = atom_to_binary(ClassName, utf8),
                    raise_start_supervisor_error(
                        iolist_to_binary([
                            NameBin,
                            <<" is already running — stop it first, then attach to the workspace">>
                        ])
                    )
            end;
        {error, already_present} ->
            %% Child spec exists but process is not running (was previously stopped).
            %% Delete the stale spec and re-add to support iterative development.
            _ = supervisor:delete_child(beamtalk_workspace_sup, ChildId),
            case supervisor:start_child(beamtalk_workspace_sup, ChildSpec) of
                {ok, Pid} ->
                    {beamtalk_supervisor, ClassName, Module, Pid};
                {error, Reason} ->
                    raise_start_supervisor_error(Reason)
            end;
        {error, Reason} ->
            raise_start_supervisor_error(Reason)
    end.

-doc """
Return the actual attached handle for a workspace child.
Reads from which_children to get the real module and pid (which may differ
from the caller's ClassArg after a class reload).
""".
-spec workspace_child_handle(term()) -> {ok, tuple()} | not_found.
workspace_child_handle(ChildId = {user_supervisor, ClassName}) ->
    case lists:keyfind(ChildId, 1, supervisor:which_children(beamtalk_workspace_sup)) of
        {ChildId, Pid, supervisor, [RunningModule]} when is_pid(Pid) ->
            {ok, {beamtalk_supervisor, ClassName, RunningModule, Pid}};
        _ ->
            not_found
    end.

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_start_supervisor_type_error/1}).
raise_start_supervisor_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message)).

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_start_supervisor_error/1}).
raise_start_supervisor_error(Message) when is_binary(Message) ->
    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message));
raise_start_supervisor_error(Reason) ->
    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary(io_lib:format("Failed to start supervisor: ~p", [Reason]))
        )
    ).

-doc """
Stop and remove a supervisor from the workspace.

Called via `(Erlang beamtalk_workspace_interface_primitives) stopSupervisor: MySup`.
Works for both workspace-attached supervisors and the root application
supervisor. Cleanly shuts down the supervisor and all its children.
""".
-spec stopSupervisor(term()) -> nil.
stopSupervisor(ClassArg) ->
    case beamtalk_class_registry:is_class_object(ClassArg) of
        false ->
            raise_stop_supervisor_type_error(<<"Expected a Supervisor class">>);
        true ->
            ok
    end,
    ClassPid = element(4, ClassArg),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    case beamtalk_supervisor:is_supervisor(ClassName) of
        false ->
            raise_stop_supervisor_type_error(
                iolist_to_binary([
                    atom_to_binary(ClassName, utf8),
                    <<" is not a Supervisor or DynamicSupervisor subclass">>
                ])
            );
        true ->
            ok
    end,
    do_stop_supervisor(ClassName).

do_stop_supervisor(ClassName) ->
    ChildId = {user_supervisor, ClassName},
    case supervisor:terminate_child(beamtalk_workspace_sup, ChildId) of
        ok ->
            _ = supervisor:delete_child(beamtalk_workspace_sup, ChildId),
            nil;
        {error, not_found} ->
            %% Not a workspace-attached supervisor — check if it's the root
            %% application supervisor. If so, stop it and clear the ETS entry.
            case beamtalk_supervisor:get_root() of
                {beamtalk_supervisor, ClassName, _Module, Pid} ->
                    try
                        gen_server:stop(Pid)
                    catch
                        exit:{noproc, _} -> ok
                    end,
                    beamtalk_supervisor:clear_root(),
                    nil;
                _ ->
                    NameBin = atom_to_binary(ClassName, utf8),
                    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'stopSupervisor:'),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Err1,
                            iolist_to_binary([NameBin, <<" is not attached to the workspace">>])
                        )
                    )
            end
    end.

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_stop_supervisor_type_error/1}).
raise_stop_supervisor_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'stopSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message)).

-doc """
List all supervisors in the workspace supervision tree.

Called via `(Erlang beamtalk_workspace_interface_primitives) supervisors`.
Returns a list of `{beamtalk_supervisor, ClassName, Module, Pid}` tuples
including the root application supervisor (if registered) and all
supervisors attached via `startSupervisor:`.
""".
-spec supervisors() -> [tuple()].
supervisors() ->
    Root =
        case beamtalk_supervisor:get_root() of
            nil -> [];
            RootSup -> [RootSup]
        end,
    Children = supervisor:which_children(beamtalk_workspace_sup),
    UserSups = lists:filtermap(
        fun
            ({{user_supervisor, ClassName}, Pid, supervisor, [Module]}) when is_pid(Pid) ->
                {true, {beamtalk_supervisor, ClassName, Module, Pid}};
            (_) ->
                false
        end,
        Children
    ),
    Root ++ UserSups.

-doc """
Return the direct dependencies of the current workspace package.

Called via `(Erlang beamtalk_workspace_interface_primitives) dependencies`.
Returns a map of `#{PackageName => PackageInfoMap}` for each direct dependency
of the current workspace package. Returns an empty map if the workspace
has no package name or no dependencies.
""".
-spec dependencies() -> #{binary() => map()}.
dependencies() ->
    case beamtalk_workspace_meta:get_package_name() of
        undefined ->
            #{};
        PkgName ->
            DepNames = beamtalk_package:dependencies(PkgName),
            maps:from_list(
                lists:filtermap(
                    fun(DepName) ->
                        try
                            Info = beamtalk_package:named(DepName),
                            {true, {DepName, Info}}
                        catch
                            error:_ -> false
                        end
                    end,
                    DepNames
                )
            )
    end.

%%% ============================================================================
%%% Project sync (BT-1723)
%%% ============================================================================

-doc """
Perform an incremental project sync from the current working directory.

Called via `(Erlang beamtalk_workspace_interface_primitives) sync`.
Delegates to `beamtalk_repl_ops_load:sync_project/2` which handles
incremental compilation (mtime tracking, native .erl files, dependency
ordering) and returns a result map.

Returns a Dictionary with keys:
  - `#summary` — human-readable summary (e.g. "Reloaded 2 of 5 files (3 unchanged)")
  - `#classes` — List of loaded class name Strings
  - `#errors` — List of error Dictionaries (empty on success)
  - `#changedCount` — number of files reloaded
  - `#unchangedCount` — number of unchanged files
  - `#deletedCount` — number of deleted files
""".
-spec sync() -> map().
sync() ->
    case beamtalk_repl_ops_load:sync_project(".", #{}) of
        {ok, Result} ->
            #{
                summary => maps:get(summary, Result),
                classes => maps:get(classes, Result),
                errors => maps:get(errors, Result),
                changedCount => maps:get(changed_count, Result),
                unchangedCount => maps:get(unchanged_count, Result),
                deletedCount => maps:get(deleted_count, Result)
            };
        {error, Err} ->
            beamtalk_error:raise(Err)
    end.

%%% ============================================================================
%%% Stable external API
%%% ============================================================================

-doc """
Return workspace-level user bindings (for REPL session injection).
Called before each eval to merge workspace bindings into session bindings.
Returns #{} when the workspace meta process is not running (workspace down).
""".
-spec get_user_bindings() -> #{atom() => term()}.
get_user_bindings() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            #{};
        _ ->
            all_user_bindings()
    end.

-doc """
Return non-class workspace globals for session binding injection.
Includes singletons (Transcript, Beamtalk, Workspace) and user-registered
bind:as: names. Class objects are excluded.
""".
-spec get_session_bindings() -> #{atom() => term()}.
get_session_bindings() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            #{};
        _ ->
            UserBindings = all_user_bindings(),
            handle_session_bindings(UserBindings)
    end.

%%% ============================================================================
%%% Internal helpers — ETS management
%%% ============================================================================

-doc """
Create the bindings ETS table owned by the calling process.
Must be called from a long-lived process (beamtalk_workspace_bootstrap) so that
the table is not deleted when short-lived eval worker processes exit.
""".
-spec create_bindings_table() -> ok.
create_bindings_table() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            ets:new(?WI_BINDINGS_TABLE, [set, public, named_table]),
            ok;
        _ ->
            ok
    end.

-doc """
Ensure the bindings ETS table exists.
Creates it on first call; safe to call repeatedly.
""".
-spec ensure_bindings_table() -> ok.
ensure_bindings_table() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            try
                ets:new(?WI_BINDINGS_TABLE, [set, public, named_table])
            catch
                error:badarg ->
                    %% Another process created it concurrently — that's fine
                    ok
            end,
            ok;
        _ ->
            ok
    end.

-doc "Read all user bindings as a map.".
-spec all_user_bindings() -> #{atom() => term()}.
all_user_bindings() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            #{};
        _ ->
            maps:from_list(ets:tab2list(?WI_BINDINGS_TABLE))
    end.

%%% ============================================================================
%%% Internal method implementations
%%% ============================================================================

-doc "Get all live actors as beamtalk_object references.".
-spec handle_actors() -> [tuple()].
handle_actors() ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(fun wrap_actor/1, Actors)
    end.

-doc "Look up a specific actor by pid string.".
-spec handle_actor_at(binary() | list()) -> tuple() | 'nil'.
handle_actor_at(PidStr) when is_binary(PidStr) ->
    handle_actor_at(binary_to_list(PidStr));
handle_actor_at(PidStr) when is_list(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case whereis(beamtalk_actor_registry) of
            undefined ->
                nil;
            RegistryPid ->
                case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                    {ok, Metadata} ->
                        case wrap_actor(Metadata) of
                            {true, Obj} -> Obj;
                            false -> nil
                        end;
                    {error, not_found} ->
                        nil
                end
        end
    catch
        error:badarg -> nil
    end;
handle_actor_at(_) ->
    nil.

-doc "Return all loaded user classes (those with a source file recorded).".
-spec handle_classes() -> [tuple()].
handle_classes() ->
    beamtalk_runtime_api:user_classes().

-doc """
Load a .bt file, compiling and registering the class.
On success, returns the loaded class object(s) so the REPL displays what was loaded.
""".
-spec handle_load(term()) -> term() | {error, #beamtalk_error{}}.
handle_load(Path) when is_binary(Path) ->
    handle_load(binary_to_list(Path));
handle_load(Path) when is_list(Path) ->
    %% BT-2091: BT-1719 demand-driven native .erl recompilation. Previously
    %% wired into the deprecated `load-file` op handler; mirror the same
    %% pre-step here so `Workspace load: "path"` keeps native FFI working
    %% for package projects with `native/*.erl` sources.
    %%
    %% Native compile failures are surfaced as a structured `#beamtalk_error{}`
    %% so callers see the FFI break at the `load:` boundary rather than a
    %% silent log + downstream "function undefined" at runtime.
    case
        beamtalk_repl_ops_load:maybe_recompile_native_deps(
            Path, beamtalk_repl_ops_load:find_project_root(Path)
        )
    of
        {ok, _Count} ->
            handle_load_after_native(Path);
        {error, NativeErrors} ->
            ?LOG_ERROR(
                "Workspace load: native .erl compilation failed for ~s: ~p",
                [Path, NativeErrors],
                #{domain => [beamtalk, workspace]}
            ),
            Err0 = beamtalk_error:new(native_compile_failed, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([
                    <<"Native .erl compilation failed for ">>,
                    Path
                ])
            ),
            Err3 = beamtalk_error:with_details(Err2, #{path => Path, errors => NativeErrors}),
            {error, Err3}
    end;
handle_load(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'load:'),
    {error,
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary([<<"load: expects a String path, got ">>, TypeName])
        )}.

%% BT-2091: Path post-step extracted so the native-compile error path
%% short-circuits without falling through to reload_class_file/1.
-spec handle_load_after_native(string()) -> term() | {error, #beamtalk_error{}}.
handle_load_after_native(Path) ->
    case beamtalk_repl_eval:reload_class_file(Path) of
        {ok, ClassNames} ->
            %% BT-2091: record class source so subsequent `Class >> selector => body`
            %% method-patch syntax (which depends on workspace_meta:get_class_source/1)
            %% keeps working. The deprecated `load-file` op's session-aware path
            %% recorded sources via store_file_class_sources/3; the stateless
            %% `Workspace load:` path now mirrors that.
            case file:read_file(Path) of
                {ok, SourceBin} ->
                    SourceStr = binary_to_list(SourceBin),
                    %% Defensive match: skip entries that don't follow the
                    %% `#{name := ...}` shape (loaded_class_objects/1 already
                    %% treats those as recoverable; we shouldn't crash on
                    %% drifted reload payloads either).
                    lists:foreach(
                        fun
                            (#{name := Atom}) when is_atom(Atom) ->
                                beamtalk_workspace_meta:set_class_source(
                                    atom_to_binary(Atom, utf8), SourceStr
                                );
                            (#{name := Bin}) when is_binary(Bin) ->
                                beamtalk_workspace_meta:set_class_source(Bin, SourceStr);
                            (#{name := Str}) when is_list(Str) ->
                                beamtalk_workspace_meta:set_class_source(
                                    list_to_binary(Str), SourceStr
                                );
                            (#{name := _Unsupported}) ->
                                ok;
                            (_Other) ->
                                ok
                        end,
                        ClassNames
                    );
                {error, _} ->
                    ok
            end,
            loaded_class_objects(ClassNames);
        {error, {file_not_found, _}} ->
            Err0 = beamtalk_error:new(file_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"File not found: ">>, Path])
                )};
        {error, Reason} ->
            %% BT-2091: surface structured compile/semantic errors through `Workspace load:`
            %% so e2e callers see specific error reasons (cannot subclass sealed class,
            %% cannot assign to field, etc.) rather than a generic "Failed to load".
            %% The migration target for the deprecated `load-file` op was already running
            %% through `ensure_structured_error/1`; mirror that here.
            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
    end.

-doc "Return the full workspace globals snapshot.".
-spec handle_globals(map()) -> map().
handle_globals(UserBindings) ->
    Base = handle_session_bindings(UserBindings),
    Classes = handle_classes(),
    lists:foldl(
        fun
            (#beamtalk_object{class = ClassTag} = ClassObj, Acc) ->
                ClassName = base_class_name(ClassTag),
                Acc#{ClassName => ClassObj};
            (_, Acc) ->
                Acc
        end,
        Base,
        Classes
    ).

-doc "Return non-class session bindings: singletons + user bind:as: entries.".
-spec handle_session_bindings(map()) -> map().
handle_session_bindings(UserBindings) ->
    Base0 = UserBindings,
    Base1 =
        case resolve_singleton('TranscriptStream') of
            nil -> Base0;
            TranscriptObj -> Base0#{'Transcript' => TranscriptObj}
        end,
    Base2 =
        case resolve_singleton('BeamtalkInterface') of
            nil -> Base1;
            BeamtalkObj -> Base1#{'Beamtalk' => BeamtalkObj}
        end,
    %% Resolve Workspace from singleton state, same as Beamtalk/Transcript.
    %% Falls back to a plain tagged-map if the class var hasn't been wired yet.
    WorkspaceObj =
        case resolve_singleton('WorkspaceInterface') of
            nil -> #{'$beamtalk_class' => 'WorkspaceInterface'};
            Obj -> Obj
        end,
    Base2#{'Workspace' => WorkspaceObj}.

-doc "Convert a name argument to an atom.".
-spec to_atom_name(term()) -> atom() | {error, #beamtalk_error{}}.
to_atom_name(Name) when is_atom(Name) -> Name;
to_atom_name(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    {error,
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Symbol name, got ">>, TypeName])
        )}.

-doc "Check if a name conflicts with Beamtalk system globals.".
-spec check_bind_conflicts(atom()) -> ok | {error, #beamtalk_error{}}.
check_bind_conflicts(AtomName) ->
    case is_protected_name(AtomName) of
        true ->
            Err0 = beamtalk_error:new(name_conflict, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'bind:as:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([
                        atom_to_binary(AtomName, utf8),
                        <<" is a system name and cannot be shadowed">>
                    ])
                )};
        false ->
            ok
    end.

-spec is_protected_name(atom()) -> boolean().
is_protected_name('Transcript') -> true;
is_protected_name('Beamtalk') -> true;
is_protected_name('Workspace') -> true;
is_protected_name(_) -> false.

-doc "Warn if name is an existing loaded class.".
-spec maybe_warn_loaded_class(atom()) -> ok.
maybe_warn_loaded_class(AtomName) ->
    Classes = handle_classes(),
    IsLoadedClass = lists:any(
        fun
            (#beamtalk_object{class = ClassTag}) ->
                base_class_name(ClassTag) =:= AtomName;
            (_) ->
                false
        end,
        Classes
    ),
    case IsLoadedClass of
        true ->
            WarningMsg = iolist_to_binary([
                <<"Warning: ">>,
                atom_to_binary(AtomName, utf8),
                <<" is a loaded class. Use reload instead.">>
            ]),
            ?LOG_WARNING("~s", [WarningMsg], #{domain => [beamtalk, runtime]});
        false ->
            ok
    end.

-doc "Wrap actor metadata into a beamtalk_object tuple.".
-spec wrap_actor(beamtalk_repl_actors:actor_metadata()) -> {true, tuple()} | false.
wrap_actor(#{pid := Pid, class := Class, module := Module}) ->
    case is_process_alive(Pid) of
        true ->
            {true, {beamtalk_object, Class, Module, Pid}};
        false ->
            false
    end.

-doc "Resolve a singleton class instance.".
-spec resolve_singleton(atom()) -> tuple() | 'nil'.
resolve_singleton(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            nil;
        ClassPid ->
            try
                beamtalk_class_dispatch:class_send(ClassPid, current, [])
            catch
                _:_ -> nil
            end
    end.

-doc """
Extract the base class name from a class tag (e.g. 'Counter class' -> 'Counter').
""".
-spec base_class_name(atom()) -> atom().
base_class_name(Tag) ->
    Bin = beamtalk_runtime_api:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> Tag
    end.

-doc """
Resolve loaded ClassNames maps to a Beamtalk List of class objects.
Always returns a List (possibly empty) so callers have a uniform type.
""".
-spec loaded_class_objects([map()]) -> list().
loaded_class_objects(ClassNames) ->
    Objects = lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                try list_to_existing_atom(Name) of
                    Atom ->
                        case beamtalk_runtime_api:whereis_class(Atom) of
                            undefined ->
                                ?LOG_WARNING(
                                    "loaded_class_objects: class ~p not found in registry after load",
                                    [Name],
                                    #{domain => [beamtalk, runtime]}
                                ),
                                false;
                            ClassPid ->
                                Mod = beamtalk_runtime_api:module_name(ClassPid),
                                Tag = beamtalk_runtime_api:class_object_tag(Atom),
                                {true, #beamtalk_object{
                                    class = Tag, class_mod = Mod, pid = ClassPid
                                }}
                        end
                catch
                    error:badarg ->
                        ?LOG_WARNING(
                            "loaded_class_objects: class name ~p is not a known atom",
                            [Name],
                            #{domain => [beamtalk, runtime]}
                        ),
                        false
                end;
            (Entry) ->
                ?LOG_WARNING("loaded_class_objects: unexpected entry shape ~p", [Entry], #{
                    domain => [beamtalk, runtime]
                }),
                false
        end,
        ClassNames
    ),
    Objects.

-doc "Return a human-readable type name for an Erlang/Beamtalk value.".
-spec value_type_name(term()) -> binary().
value_type_name(V) when is_integer(V) -> <<"Integer">>;
value_type_name(V) when is_float(V) -> <<"Float">>;
value_type_name(V) when is_boolean(V) -> <<"Boolean">>;
value_type_name(nil) -> <<"nil">>;
value_type_name(V) when is_atom(V) -> <<"Symbol">>;
value_type_name(V) when is_list(V) -> <<"List">>;
value_type_name(V) when is_map(V) -> <<"Dictionary">>;
value_type_name(#beamtalk_object{}) -> <<"Object">>;
value_type_name(_) -> <<"Unknown">>.
