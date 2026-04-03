%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unified module discovery, sorting, and activation for Beamtalk.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Every code path that loads compiled Beamtalk modules (stdlib init, workspace
%%% bootstrap, REPL load-project) needs the same sequence: discover `bt@*.beam`
%%% files, topologically sort by superclass, load each module, call
%%% `register_class/0`, and optionally load the OTP `.app` metadata so that
%%% `beamtalk_package:all/0` can see the package.
%%%
%%% Previously this logic was duplicated across `beamtalk_stdlib`,
%%% `beamtalk_workspace_bootstrap`, and `beamtalk_repl_ops_load` with slight
%%% variations.  This module is the single implementation.
%%%
%%% ## Callers
%%%
%%% - `beamtalk_stdlib` — stdlib startup (topo_sort + activate_module)
%%% - `beamtalk_workspace_bootstrap` — dependency + project activation at boot and :sync
%%%
%%% Caller-specific side effects (e.g. registering in workspace_meta, storing
%%% source text) are handled via the `on_activate` callback in activation options.
-module(beamtalk_module_activation).

-include_lib("kernel/include/logger.hrl").

-export([
    activate_ebin/1,
    activate_ebin/2,
    activate_dependencies/1,
    activate_dependencies/2,
    activate_module/1,
    activate_module/2,
    find_bt_modules_in_dir/1,
    sort_modules_by_dependency/2,
    topo_sort/1,
    extract_source_path/1,
    extract_class_names/1,
    load_app_from_ebin/1
]).

%% Exported for callers that need validation (e.g. workspace_bootstrap backwards compat).
-export([is_valid_module_name/1]).

%% Maximum number of modules activated per ebin directory.
%% Prevents atom table exhaustion if the directory contains excessive files.
-define(MAX_MODULES, 1000).

%%% ============================================================================
%%% Types
%%% ============================================================================

%% Options map for activate_ebin/2 and activate_module/2.
%%
%% - `on_activate`: Called after each module is successfully loaded and
%%   registered.  Receives `{Module, SourcePath}` where SourcePath may be
%%   `undefined`.  Defaults to no-op.
%%
%% - `log_domain`: Logger domain for `activate_module/2` log messages.
%%   Infrastructure helpers (`load_app_from_ebin`, `find_bt_modules_in_dir`)
%%   always log under `[beamtalk, runtime]`.  Defaults to `[beamtalk, runtime]`.
-type opts() :: #{
    on_activate => fun(({module(), string() | undefined}) -> ok),
    log_domain => [atom()]
}.

-export_type([opts/0]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Activate all Beamtalk modules in an ebin directory.
%%
%% Equivalent to `activate_ebin(EbinDir, #{})`.
-spec activate_ebin(file:filename()) -> {ok, [{module(), term()}]}.
activate_ebin(EbinDir) ->
    activate_ebin(EbinDir, #{}).

%% @doc Activate all Beamtalk modules in an ebin directory with options.
%%
%% 1. Adds the directory to the BEAM code path
%% 2. Loads any `.app` file found (so `beamtalk_package:all/0` sees it)
%% 3. Discovers `bt@*.beam` modules (excluding `bt@stdlib@*`)
%% 4. Topologically sorts by superclass dependency
%% 5. Loads each module and calls `register_class/0`
%% 6. Invokes `on_activate` callback for each successfully activated module
%%
%% Returns `{ok, Errors}` where `Errors` is a (possibly empty) list of
%% `{Module, Reason}` pairs for modules that failed to load or register.
%% Activation is never aborted — all modules are attempted regardless of
%% individual failures.
-spec activate_ebin(file:filename(), opts()) -> {ok, [{module(), term()}]}.
activate_ebin(EbinDir, Opts) ->
    case filelib:is_dir(EbinDir) of
        false ->
            {ok, []};
        true ->
            _ = code:add_pathz(EbinDir),
            {ok, AppErrors} = load_app_from_ebin(EbinDir),
            Modules = find_bt_modules_in_dir(EbinDir),
            Sorted = sort_modules_by_dependency(EbinDir, Modules),
            ModErrors = lists:filtermap(
                fun(Mod) ->
                    case activate_module(Mod, Opts) of
                        ok -> false;
                        {error, Reason} -> {true, {Mod, Reason}}
                    end
                end,
                Sorted
            ),
            {ok, AppErrors ++ ModErrors}
    end.

%% @doc Activate all dependency packages and native ebin paths for a project.
%%
%% Scans `{ProjectPath}/_build/deps/*/ebin/` for compiled dependency packages,
%% activates each via `activate_ebin/2`, and adds native code paths
%% (`_build/dev/native/ebin/` and rebar3 hex deps) to the BEAM code path.
%%
%% Equivalent to `activate_dependencies(ProjectPath, #{})`.
-spec activate_dependencies(file:filename()) -> [{atom(), term()}].
activate_dependencies(ProjectPath) ->
    activate_dependencies(ProjectPath, #{}).

%% @doc Activate all dependency packages with custom activation options.
%%
%% Returns a (possibly empty) list of `{Name, Reason}` error pairs, where
%% `Name` may be a module atom (activation failure) or an application name
%% (`.app` load failure).
-spec activate_dependencies(file:filename(), opts()) -> [{atom(), term()}].
activate_dependencies(ProjectPath, Opts) ->
    DepsDir = filename:join([ProjectPath, "_build", "deps"]),
    DepErrors =
        case filelib:is_dir(DepsDir) of
            false ->
                [];
            true ->
                case file:list_dir(DepsDir) of
                    {ok, DepNames} ->
                        lists:flatmap(
                            fun(DepName) ->
                                EbinDir = filename:join([DepsDir, DepName, "ebin"]),
                                {ok, Errors} = activate_ebin(EbinDir, Opts),
                                Errors
                            end,
                            lists:sort(DepNames)
                        );
                    {error, _} ->
                        []
                end
        end,
    %% Add native ebin paths for FFI modules.
    NativeEbin = filename:join([ProjectPath, "_build", "dev", "native", "ebin"]),
    case filelib:is_dir(NativeEbin) of
        true ->
            _ = code:add_pathz(NativeEbin),
            ok;
        false ->
            ok
    end,
    %% Add rebar3 hex dependency ebin paths (cowboy, gun, etc.).
    Rebar3LibDir = filename:join([ProjectPath, "_build", "dev", "native", "default", "lib"]),
    case filelib:is_dir(Rebar3LibDir) of
        false ->
            ok;
        true ->
            case file:list_dir(Rebar3LibDir) of
                {ok, HexDeps} ->
                    lists:foreach(
                        fun(HexDep) ->
                            HexEbin = filename:join([Rebar3LibDir, HexDep, "ebin"]),
                            case filelib:is_dir(HexEbin) of
                                true ->
                                    _ = code:add_pathz(HexEbin),
                                    ok;
                                false ->
                                    ok
                            end
                        end,
                        HexDeps
                    );
                {error, _} ->
                    ok
            end
    end,
    DepErrors.

%% @doc Activate a single module with default options.
-spec activate_module(module()) -> ok | {error, term()}.
activate_module(Module) ->
    activate_module(Module, #{}).

%% @doc Load a module, call `register_class/0`, and invoke the callback.
%%
%% Returns `ok` on success or `{error, Reason}` if the module could not
%% be loaded or class registration failed.
-spec activate_module(module(), opts()) -> ok | {error, term()}.
activate_module(Module, Opts) ->
    Domain = maps:get(log_domain, Opts, [beamtalk, runtime]),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case try_register_class(Module, Domain) of
                ok ->
                    SourcePath = extract_source_path(Module),
                    case maps:find(on_activate, Opts) of
                        {ok, Callback} ->
                            try
                                Callback({Module, SourcePath})
                            catch
                                Class:Reason:Stacktrace ->
                                    ?LOG_WARNING(
                                        "on_activate callback failed for ~p: ~p:~p",
                                        [Module, Class, Reason],
                                        #{stacktrace => Stacktrace, domain => Domain}
                                    )
                            end;
                        error ->
                            ok
                    end,
                    ?LOG_DEBUG(
                        "Activated module ~p",
                        [Module],
                        #{domain => Domain}
                    ),
                    ok;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to load module ~p: ~p",
                [Module, Reason],
                #{domain => Domain}
            ),
            {error, Reason}
    end.

%% @doc Scan a directory for `bt@*.beam` files, excluding `bt@stdlib@*`.
%%
%% Returns a list of module atoms.  Returns `[]` for missing or unreadable
%% directories.  Capped at 1000 modules to guard against atom table
%% exhaustion.
-spec find_bt_modules_in_dir(file:filename()) -> [module()].
find_bt_modules_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Eligible = lists:sort(lists:filter(fun is_project_beam_file/1, Files)),
            Count = length(Eligible),
            Capped =
                case Count > ?MAX_MODULES of
                    true ->
                        ?LOG_WARNING(
                            "Too many project modules, capping at limit",
                            #{
                                found => Count,
                                limit => ?MAX_MODULES,
                                dir => Dir,
                                domain => [beamtalk, runtime]
                            }
                        ),
                        lists:sublist(Eligible, ?MAX_MODULES);
                    false ->
                        Eligible
                end,
            lists:filtermap(fun beam_file_to_module/1, Capped);
        {error, _} ->
            []
    end.

%% @doc Sort modules by superclass dependency order.
%%
%% Reads the `beamtalk_class` attribute from each BEAM file to determine
%% `{ClassName, SuperclassName}`, then topologically sorts so superclasses
%% are loaded before subclasses.  Modules without the attribute are placed
%% first (no known dependencies).
-spec sort_modules_by_dependency(file:filename(), [module()]) -> [module()].
sort_modules_by_dependency(_EbinDir, []) ->
    [];
sort_modules_by_dependency(EbinDir, Modules) ->
    {WithClass, WithoutClass} = lists:foldl(
        fun(Mod, {WC, WOC}) ->
            case extract_class_info_from_beam(EbinDir, Mod) of
                {ok, ClassName, Superclass} ->
                    {[{Mod, ClassName, Superclass} | WC], WOC};
                error ->
                    {WC, [Mod | WOC]}
            end
        end,
        {[], []},
        Modules
    ),
    Sorted = topo_sort(lists:reverse(WithClass)),
    lists:reverse(WithoutClass) ++ [Mod || {Mod, _, _} <- Sorted].

%% @doc Topological sort of class entries by superclass dependency.
%%
%% Accepts entries in two formats:
%% - `{Module, ClassName, SuperclassName}` tuples (from BEAM attribute scan)
%% - Maps with `name`, `module`, `parent` keys (from `.app` class metadata)
%%
%% Returns entries ordered so that each class's superclass appears before it.
%% On circular or unresolvable dependencies, logs a warning and appends the
%% remaining entries as-is.
-spec topo_sort([Entry]) -> [Entry] when
    Entry :: {module(), atom(), atom()} | map().
topo_sort(Entries) ->
    ClassSet = sets:from_list([entry_class(E) || E <- Entries]),
    topo_sort_waves(Entries, ClassSet, sets:new(), []).

%% @doc Extract the `.bt` source file path from a loaded module's attributes.
%%
%% The compiler embeds `beamtalk_source = ["path/to/file.bt"]` in every user
%% module.  Returns `undefined` for stdlib modules or older compiled modules.
-spec extract_source_path(module()) -> string() | undefined.
extract_source_path(ModuleName) ->
    try
        Attrs = erlang:get_module_info(ModuleName, attributes),
        case proplists:get_value(beamtalk_source, Attrs) of
            [Path] when is_list(Path) -> Path;
            _ -> undefined
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_DEBUG(
                "Failed to extract source path from ~p: ~p:~p",
                [ModuleName, Class, Reason],
                #{stacktrace => Stacktrace, domain => [beamtalk, runtime]}
            ),
            undefined
    end.

%% @doc Extract class name atoms from a loaded module's `beamtalk_class` attribute.
-spec extract_class_names(module()) -> [atom()].
extract_class_names(ModuleName) ->
    try
        Attrs = erlang:get_module_info(ModuleName, attributes),
        Classes = proplists:get_value(beamtalk_class, Attrs, []),
        [ClassName || {ClassName, _Superclass} <- Classes]
    catch
        Class:Reason:Stacktrace ->
            ?LOG_DEBUG(
                "Failed to extract class names from ~p: ~p:~p",
                [ModuleName, Class, Reason],
                #{stacktrace => Stacktrace, domain => [beamtalk, runtime]}
            ),
            []
    end.

%% @doc Load the OTP `.app` file from an ebin directory.
%%
%% Scans the directory for `.app` files and calls `application:load/1` for
%% each one found.  This makes the application's env (including `{classes, [...]}`
%% metadata) visible to `application:loaded_applications/0` and
%% `application:get_env/2`, which `beamtalk_package:all/0` relies on.
%%
%% Returns `{ok, Errors}` where `Errors` is a list of `{AppName, Reason}`
%% pairs for any `.app` files that could not be loaded.
%%
%% Safe to call multiple times — `application:load/1` is idempotent when the
%% application is already loaded.
-spec load_app_from_ebin(file:filename()) -> {ok, [{atom(), term()}]}.
load_app_from_ebin(EbinDir) ->
    case file:list_dir(EbinDir) of
        {ok, Files} ->
            AppFiles = [F || F <- Files, filename:extension(F) =:= ".app"],
            Errors = lists:filtermap(
                fun(AppFile) ->
                    BaseName = filename:rootname(AppFile),
                    case is_valid_module_name(BaseName) of
                        false -> false;
                        true -> load_single_app(list_to_atom(BaseName))
                    end
                end,
                AppFiles
            ),
            {ok, Errors};
        {error, _} ->
            {ok, []}
    end.

%% @private Load a single OTP application by name.
%% Returns false on success, {true, {AppName, Reason}} on failure.
-spec load_single_app(atom()) -> {true, {atom(), term()}} | false.
load_single_app(AppName) ->
    case application:load(AppName) of
        ok ->
            ?LOG_DEBUG(
                "Loaded OTP application metadata: ~p",
                [AppName],
                #{domain => [beamtalk, runtime]}
            ),
            false;
        {error, {already_loaded, _}} ->
            false;
        {error, Reason} ->
            ?LOG_WARNING(
                "Could not load OTP application ~p: ~p",
                [AppName, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            {true, {AppName, Reason}}
    end.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private Call register_class/0 on a module if it exports one.
-spec try_register_class(module(), [atom()]) -> ok | {error, term()}.
try_register_class(Module, Domain) ->
    case erlang:function_exported(Module, register_class, 0) of
        true ->
            try
                case Module:register_class() of
                    ok ->
                        ok;
                    Other ->
                        ?LOG_WARNING(
                            "Unexpected return from register_class/0: ~p",
                            [Other],
                            #{module => Module, domain => Domain}
                        ),
                        ok
                end
            catch
                error:undef ->
                    ?LOG_WARNING(
                        "Module ~p missing register_class/0",
                        [Module],
                        #{domain => Domain}
                    ),
                    ok;
                Class:Reason:Stacktrace ->
                    ?LOG_WARNING(
                        "register_class/0 failed for ~p: ~p:~p",
                        [Module, Class, Reason],
                        #{domain => Domain, stacktrace => Stacktrace}
                    ),
                    {error, {Class, Reason}}
            end;
        false ->
            ok
    end.

%% @private Iteratively emit classes whose superclass dependencies are satisfied.
-spec topo_sort_waves(
    [Entry],
    sets:set(atom()),
    sets:set(atom()),
    [Entry]
) -> [Entry] when
    Entry :: {module(), atom(), atom()} | map().
topo_sort_waves([], _ClassSet, _Emitted, Acc) ->
    lists:reverse(Acc);
topo_sort_waves(Remaining, ClassSet, Emitted, Acc) ->
    {Ready, Deferred} = lists:partition(
        fun(Entry) ->
            Super = entry_parent(Entry),
            (not sets:is_element(Super, ClassSet)) orelse sets:is_element(Super, Emitted)
        end,
        Remaining
    ),
    case Ready of
        [] ->
            ?LOG_WARNING(
                "topo_sort: unresolvable dependencies",
                #{
                    remaining => [entry_class(E) || E <- Deferred],
                    domain => [beamtalk, runtime]
                }
            ),
            lists:reverse(Acc) ++ Deferred;
        _ ->
            NewEmitted = lists:foldl(
                fun(Entry, S) ->
                    sets:add_element(entry_class(Entry), S)
                end,
                Emitted,
                Ready
            ),
            topo_sort_waves(Deferred, ClassSet, NewEmitted, lists:reverse(Ready) ++ Acc)
    end.

%% @private Extract class name from an entry (map or tuple).
-spec entry_class(map() | {module(), atom(), atom()}) -> atom().
entry_class(#{name := Name}) -> Name;
entry_class({_Mod, Name, _Super}) -> Name.

%% @private Extract parent/superclass from an entry (map or tuple).
-spec entry_parent(map() | {module(), atom(), atom()}) -> atom().
entry_parent(#{parent := Parent}) -> Parent;
entry_parent({_Mod, _Name, Parent}) -> Parent.

%% @private Check if a filename is a candidate project beam file (bt@* but not bt@stdlib@*).
%% Does NOT validate the module name — that is deferred to beam_file_to_module/1 so
%% invalid names trigger a warning rather than being silently filtered out.
-spec is_project_beam_file(string()) -> boolean().
is_project_beam_file(File) ->
    case filename:extension(File) of
        ".beam" ->
            ModName = filename:rootname(filename:basename(File)),
            is_user_class_module(ModName);
        _ ->
            false
    end.

%% @private Map a filename to a module atom, or false to skip.
%% Logs a warning for user-class modules with invalid names (e.g. containing
%% dots or spaces) so corrupted .beam files are not silently ignored.
-spec beam_file_to_module(string()) -> {true, module()} | false.
beam_file_to_module(File) ->
    case filename:extension(File) of
        ".beam" ->
            ModName = filename:rootname(filename:basename(File)),
            case is_user_class_module(ModName) of
                true ->
                    case is_valid_module_name(ModName) of
                        true ->
                            {true, list_to_atom(ModName)};
                        false ->
                            ?LOG_WARNING(
                                "Skipping module with invalid name",
                                #{filename => File, domain => [beamtalk, runtime]}
                            ),
                            false
                    end;
                false ->
                    false
            end;
        _ ->
            false
    end.

%% @private Returns true if ModName is a user-defined class module.
-spec is_user_class_module(string()) -> boolean().
is_user_class_module(ModName) ->
    lists:prefix("bt@", ModName) andalso not lists:prefix("bt@stdlib@", ModName).

%% @doc Validate that a module name contains only valid Beamtalk characters.
-spec is_valid_module_name(string()) -> boolean().
is_valid_module_name([]) ->
    false;
is_valid_module_name(Name) ->
    lists:all(fun is_valid_module_char/1, Name).

%% @private
-spec is_valid_module_char(char()) -> boolean().
is_valid_module_char(C) when C >= $a, C =< $z -> true;
is_valid_module_char(C) when C >= $A, C =< $Z -> true;
is_valid_module_char(C) when C >= $0, C =< $9 -> true;
is_valid_module_char($@) -> true;
is_valid_module_char($_) -> true;
is_valid_module_char(_) -> false.

%% @private Extract class name and superclass from a BEAM file's attributes.
-spec extract_class_info_from_beam(file:filename(), module()) -> {ok, atom(), atom()} | error.
extract_class_info_from_beam(EbinDir, ModuleName) ->
    BeamFile = filename:join(EbinDir, atom_to_list(ModuleName) ++ ".beam"),
    case beam_lib:chunks(BeamFile, [attributes]) of
        {ok, {_, [{attributes, Attrs}]}} ->
            case proplists:get_value(beamtalk_class, Attrs) of
                [{ClassName, Superclass}] ->
                    {ok, ClassName, Superclass};
                [{ClassName, Superclass} | _] ->
                    {ok, ClassName, Superclass};
                _ ->
                    error
            end;
        _ ->
            error
    end.
