%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for load-file, load-source, load-project, reload, unload, and modules operations.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_load).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, resolve_class_to_module/1, resolve_module_atoms/2]).

%% BT-1723: Shared sync logic callable from both protocol handler and primitives.
-export([sync_project/2]).

%% BT-1719: Exported for demand-driven native .erl compilation from classReload.
-export([find_project_root/1, maybe_recompile_native_deps/2]).

%% Export internals for white-box testing of load-project helpers.
-ifdef(TEST).
-export([
    find_bt_files/1,
    find_erl_files/1,
    compile_native_erl_files/2,
    extract_bt_class_info/1,
    sort_bt_files_by_deps/1,
    structured_file_errors/2,
    format_collision_warning/3,
    extract_package_from_module/1,
    classify_files_by_change/2,
    get_file_mtime/1,
    extract_native_refs/1,
    find_project_root/1,
    maybe_recompile_native_deps/2,
    activate_dependency_modules/1,
    activate_dep_ebin/1
]).
-endif.

%%% ============================================================================
%%% sync_project/2 — shared incremental sync logic (BT-1723)
%%% ============================================================================

%% @doc Perform an incremental project sync.
%%
%% Scans the project at `Path` for `.bt` and `.erl` files, classifies them
%% as changed/unchanged/deleted by mtime, compiles changed files in dependency
%% order, and returns a result map with summary statistics.
%%
%% `Options` is a map with optional keys:
%%   - `include_tests` (boolean, default false) — include test/ directory
%%   - `force` (boolean, default false) — recompile all files regardless of mtime
%%   - `session_pid` (pid() | undefined) — REPL session for module tracking
%%
%% Returns `{ok, ResultMap}` where ResultMap contains:
%%   - `classes` — list of loaded class name binaries
%%   - `errors` — list of structured error maps
%%   - `summary` — human-readable summary binary
%%   - `changed_count` — number of files reloaded
%%   - `unchanged_count` — number of unchanged files
%%   - `deleted_count` — number of deleted files
%%   - `total_files` — total file count including deleted
%%
%% Returns `{error, #beamtalk_error{}}` if no beamtalk.toml found.
-spec sync_project(string(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
sync_project(Path, Options) ->
    IncludeTests = maps:get(include_tests, Options, false),
    Force = maps:get(force, Options, false),
    SessionPid = maps:get(session_pid, Options, undefined),
    AbsPath = filename:absname(Path),
    ManifestPath = filename:join(AbsPath, "beamtalk.toml"),
    case filelib:is_file(ManifestPath) of
        false ->
            Err0 = beamtalk_error:new(file_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(["No beamtalk.toml found in: ", AbsPath])
            ),
            {error,
                beamtalk_error:with_hint(
                    Err1,
                    <<"Provide a directory path containing beamtalk.toml">>
                )};
        true ->
            do_sync_project(AbsPath, IncludeTests, Force, SessionPid)
    end.

%% @private Core sync logic, called after validating beamtalk.toml exists.
-spec do_sync_project(string(), boolean(), boolean(), pid() | undefined) -> {ok, map()}.
do_sync_project(AbsPath, IncludeTests, Force, SessionPid) ->
    %% Activate pre-compiled dependency modules before loading project files,
    %% so that project classes can reference dependency classes (e.g. HTTPClient).
    activate_dependency_modules(AbsPath),
    SrcFiles = find_bt_files(filename:join(AbsPath, "src")),
    TestFiles =
        case IncludeTests of
            true -> find_bt_files(filename:join(AbsPath, "test"));
            false -> []
        end,
    AllBtFiles = SrcFiles ++ TestFiles,
    NativeDir = filename:join(AbsPath, "native"),
    AllErlFiles = find_erl_files(NativeDir),
    AllFiles = AllErlFiles ++ AllBtFiles,
    PreviousMtimes =
        case beamtalk_workspace_meta:get_file_mtimes() of
            {ok, Mtimes} -> Mtimes;
            {error, _} -> #{}
        end,
    PrevErlMtimes =
        maps:filter(
            fun(P, _Mtime) -> filename:extension(P) =:= ".erl" end,
            PreviousMtimes
        ),
    PrevBtMtimes =
        maps:filter(
            fun(P, _Mtime) -> filename:extension(P) =:= ".bt" end,
            PreviousMtimes
        ),
    {ChangedErl, UnchangedErl, DeletedErl} =
        case Force of
            true ->
                {_, _, DelErl} = classify_files_by_change(AllErlFiles, PrevErlMtimes),
                {AllErlFiles, [], DelErl};
            false ->
                classify_files_by_change(AllErlFiles, PrevErlMtimes)
        end,
    {ChangedBt, UnchangedBt, DeletedBt} =
        case Force of
            true ->
                {_, _, DelBt} = classify_files_by_change(AllBtFiles, PrevBtMtimes),
                {AllBtFiles, [], DelBt};
            false ->
                classify_files_by_change(AllBtFiles, PrevBtMtimes)
        end,
    DeletedBtCount = handle_deleted_files(DeletedBt, SessionPid),
    lists:foreach(
        fun(P) ->
            ?LOG_INFO(
                "sync-project: native .erl file deleted: ~s",
                [P],
                #{domain => [beamtalk, runtime]}
            ),
            %% Unload the native module from the VM before removing the mtime.
            ModName = list_to_atom(filename:basename(P, ".erl")),
            case code:is_loaded(ModName) of
                false ->
                    ok;
                {file, _} ->
                    code:purge(ModName),
                    code:delete(ModName),
                    code:purge(ModName)
            end,
            beamtalk_workspace_meta:remove_file_mtime(P)
        end,
        DeletedErl
    ),
    DeletedCount = DeletedBtCount + length(DeletedErl),
    ErlPreLoadMtimes = snapshot_file_mtimes(ChangedErl),
    {NativeErrors, _NativeCompiledCount} =
        compile_native_erl_files(ChangedErl, AbsPath),
    %% Only record mtimes for .erl files that compiled successfully (no error entry).
    ErlErrorPaths = sets:from_list([
        binary_to_list(maps:get(<<"path">>, E))
     || E <- NativeErrors
    ]),
    SuccessfulErlMtimes = [
        {P, M}
     || {P, M} <- ErlPreLoadMtimes,
        not sets:is_element(P, ErlErrorPaths)
    ],
    record_file_mtimes_from_snapshot(SuccessfulErlMtimes),
    SortedChanged = sort_bt_files_by_deps(ChangedBt),
    PreLoadMtimes = snapshot_file_mtimes(SortedChanged),
    {AllClasses, BtErrors} =
        case SessionPid of
            undefined ->
                load_files_stateless(SortedChanged);
            _ ->
                load_files_sequential(SortedChanged, SessionPid)
        end,
    %% Only record mtimes for .bt files that compiled successfully (no error entry).
    BtErrorPaths = sets:from_list([
        binary_to_list(maps:get(<<"path">>, E))
     || E <- BtErrors
    ]),
    SuccessfulBtMtimes = [
        {P, M}
     || {P, M} <- PreLoadMtimes,
        not sets:is_element(P, BtErrorPaths)
    ],
    record_file_mtimes_from_snapshot(SuccessfulBtMtimes),
    Errors = lists:reverse(NativeErrors) ++ BtErrors,
    ClassNames =
        [
            case maps:get(name, C, "") of
                N when is_binary(N) -> N;
                N -> list_to_binary(N)
            end
         || C <- AllClasses
        ],
    TotalFiles = length(AllFiles) + DeletedCount,
    ChangedCount = length(ChangedBt) + length(ChangedErl),
    UnchangedCount = length(UnchangedBt) + length(UnchangedErl),
    {ok, #{
        classes => ClassNames,
        errors => Errors,
        summary => build_incremental_summary(
            ChangedCount, TotalFiles, UnchangedCount, DeletedCount
        ),
        changed_count => ChangedCount,
        unchanged_count => UnchangedCount,
        deleted_count => DeletedCount,
        total_files => TotalFiles
    }}.

%% @doc Handle load/reload/unload/modules ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"load-project">>, Params, Msg, SessionPid) ->
    PathBin = maps:get(<<"path">>, Params, <<".">>),
    Path = binary_to_list(PathBin),
    IncludeTests = maps:get(<<"include_tests">>, Params, false),
    Force = maps:get(<<"force">>, Params, false),
    Options = #{
        include_tests => IncludeTests,
        force => Force,
        session_pid => SessionPid
    },
    case sync_project(Path, Options) of
        {error, Err} ->
            beamtalk_repl_protocol:encode_error(
                Err, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        {ok, Result} ->
            Base = beamtalk_repl_protocol:base_response(Msg),
            Response = Base#{
                <<"status">> => [<<"done">>],
                <<"classes">> => maps:get(classes, Result),
                <<"errors">> => maps:get(errors, Result),
                <<"summary">> => maps:get(summary, Result)
            },
            iolist_to_binary(json:encode(Response))
    end;
handle(<<"load-file">>, Params, Msg, SessionPid) ->
    Path = binary_to_list(maps:get(<<"path">>, Params, <<>>)),
    %% BT-1719: Demand-driven native .erl compilation on initial load.
    %% Same as do_reload/4 — if the .bt file references native Erlang modules,
    %% compile them before loading the .bt file.
    case maybe_recompile_native_deps(Path, find_project_root(Path)) of
        {ok, _Count} ->
            ok;
        {error, NativeErrors} ->
            ?LOG_ERROR(
                "load-file: native .erl compilation failed for ~s: ~p",
                [Path, NativeErrors],
                #{domain => [beamtalk, runtime]}
            )
    end,
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            Warnings = collect_load_warnings(Classes),
            beamtalk_repl_protocol:encode_loaded(
                Classes, Msg, fun beamtalk_repl_json:term_to_json/1, Warnings
            );
        {error, Reason} ->
            WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end;
handle(<<"load-source">>, Params, Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty source">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter Beamtalk source code to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    Warnings = collect_load_warnings(Classes),
                    beamtalk_repl_protocol:encode_loaded(
                        Classes, Msg, fun beamtalk_repl_json:term_to_json/1, Warnings
                    );
                {error, Reason} ->
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"reload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case maps:get(<<"path">>, Params, undefined) of
        undefined when ModuleBin =/= <<>> ->
            case beamtalk_repl_errors:safe_to_existing_atom(ModuleBin) of
                {ok, ModuleAtom} ->
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
                    case beamtalk_repl_modules:get_module_info(ModuleAtom, Tracker) of
                        {ok, Info} ->
                            case beamtalk_repl_modules:get_source_file(Info) of
                                undefined ->
                                    Err0 = beamtalk_error:new(no_source_file, 'Module'),
                                    Err1 = beamtalk_error:with_message(
                                        Err0,
                                        iolist_to_binary([
                                            <<"No source file recorded for module: ">>,
                                            ModuleBin
                                        ])
                                    ),
                                    Err2 = beamtalk_error:with_hint(
                                        Err1,
                                        <<"Use :load <path> to load it first.">>
                                    ),
                                    beamtalk_repl_protocol:encode_error(
                                        Err2,
                                        Msg,
                                        fun beamtalk_repl_json:format_error_message/1
                                    );
                                SourcePath ->
                                    do_reload(SourcePath, ModuleAtom, Msg, SessionPid)
                            end;
                        {error, not_found} ->
                            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                            Err1 = beamtalk_error:with_message(
                                Err0,
                                iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])
                            ),
                            Err2 = beamtalk_error:with_hint(
                                Err1,
                                <<"Use :load <path> to load it first.">>
                            ),
                            beamtalk_repl_protocol:encode_error(
                                Err2,
                                Msg,
                                fun beamtalk_repl_json:format_error_message/1
                            )
                    end;
                {error, badarg} ->
                    Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                    Err1 = beamtalk_error:with_message(
                        Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])
                    ),
                    Err2 = beamtalk_error:with_hint(
                        Err1,
                        <<"Use :load <path> to load it first.">>
                    ),
                    beamtalk_repl_protocol:encode_error(
                        Err2,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1
                    )
            end;
        undefined ->
            Err0 = beamtalk_error:new(missing_argument, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                <<"Missing module name for reload">>
            ),
            Err2 = beamtalk_error:with_hint(
                Err1,
                <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        Path ->
            PathStr = binary_to_list(Path),
            do_reload(PathStr, undefined, Msg, SessionPid)
    end;
handle(<<"unload">>, Params, Msg, SessionPid) ->
    %% BT-1239: Restore unload op — fully removes class from system (actors, gen_server,
    %% BEAM module, workspace_meta, session tracker).
    ClassNameBin = maps:get(<<"module">>, Params, <<>>),
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {error, badarg} ->
            Err0 = beamtalk_error:new(class_not_found, 'REPL'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary([<<"Class not found: '">>, ClassNameBin, <<"'">>])
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        {ok, ClassName} ->
            case beamtalk_runtime_api:remove_class_from_system(ClassName) of
                {ok, ModuleName} ->
                    %% Clean up workspace-level and session-level tracking.
                    beamtalk_workspace_meta:unregister_module(ModuleName),
                    beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName),
                    beamtalk_repl_protocol:encode_result(
                        iolist_to_binary([
                            <<"Class '">>, ClassNameBin, <<"' removed from system">>
                        ]),
                        Msg,
                        fun beamtalk_repl_json:term_to_json/1
                    );
                {error, Err} ->
                    beamtalk_repl_protocol:encode_error(
                        Err, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"modules">>, _Params, Msg, SessionPid) ->
    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
    %% BT-1239: Filter out stale tracker entries for modules purged via removeFromSystem.
    %% code:is_loaded/1 returns false for modules that have been code:delete'd.
    AllTrackedModules = beamtalk_repl_modules:list_modules(Tracker),
    TrackedModules = lists:filter(
        fun({N, _}) -> code:is_loaded(N) =/= false end, AllTrackedModules
    ),
    RegistryPid = whereis(beamtalk_actor_registry),
    %% Build a module-atom → Beamtalk-class-name map so the UI shows class names,
    %% not BEAM module atoms. Without this, names like 'beamtalk_counter_v1_abc' appear
    %% instead of 'Counter', and the methods op fails because it looks up by class name.
    ModToClass = module_to_class_name_map(),
    ModulesWithInfo = lists:map(
        fun({ModName, ModInfo}) ->
            ActorCount = beamtalk_repl_modules:get_actor_count(ModName, RegistryPid, Tracker),
            Info0 = beamtalk_repl_modules:format_module_info(ModInfo, ActorCount),
            ClassName = maps:get(ModName, ModToClass, maps:get(name, Info0)),
            Info = Info0#{name => ClassName},
            {ModName, Info}
        end,
        TrackedModules
    ),
    %% Merge workspace-level classes (bootstrap-activated) that are not in the
    %% session tracker. These are loaded at startup via beamtalk_workspace_bootstrap
    %% and registered in workspace_meta but never added to any session tracker.
    WorkspaceExtra =
        case beamtalk_workspace_meta:loaded_modules() of
            {ok, WsMods} ->
                TrackedSet = maps:from_list([{N, true} || {N, _} <- TrackedModules]),
                lists:filtermap(
                    fun({ModName, SourcePath}) ->
                        case maps:is_key(ModName, TrackedSet) of
                            true ->
                                false;
                            false ->
                                %% BT-1239: Skip stale workspace_meta entries for modules
                                %% that have been purged (e.g. via removeFromSystem).
                                case code:is_loaded(ModName) of
                                    false ->
                                        false;
                                    _ ->
                                        ClassName = maps:get(
                                            ModName, ModToClass, atom_to_binary(ModName, utf8)
                                        ),
                                        ResolvedPath =
                                            case SourcePath of
                                                undefined -> resolve_source_path(ModName);
                                                _ -> SourcePath
                                            end,
                                        Info = #{
                                            name => ClassName,
                                            source_file => ResolvedPath,
                                            actor_count => 0,
                                            load_time => 0,
                                            time_ago => "startup"
                                        },
                                        {true, {ModName, Info}}
                                end
                        end
                    end,
                    WsMods
                );
            _ ->
                []
        end,
    beamtalk_repl_protocol:encode_modules(
        ModulesWithInfo ++ WorkspaceExtra, Msg, fun beamtalk_repl_json:term_to_json/1
    ).

%%% ============================================================================
%%% Dependency activation — load pre-compiled dependency BEAM modules
%%% ============================================================================

%% @doc Discover and activate pre-compiled dependency modules from _build/deps/.
%%
%% For each dependency package in `_build/deps/{name}/ebin/`, adds the ebin
%% directory to the BEAM code path, then loads each `bt@*.beam` module and
%% calls `register_class/0` so the class is visible in the runtime registry.
%%
%% Also adds native ebin paths (_build/dev/native/ebin/ and rebar3 hex deps)
%% to the code path so that FFI modules are available.
%%
%% Dependency modules may be (re)loaded multiple times; `register_class/0`
%% callbacks are expected to be safe to call more than once.
-spec activate_dependency_modules(string()) -> ok.
activate_dependency_modules(AbsPath) ->
    DepsDir = filename:join([AbsPath, "_build", "deps"]),
    case filelib:is_dir(DepsDir) of
        false ->
            ok;
        true ->
            case file:list_dir(DepsDir) of
                {ok, DepNames} ->
                    lists:foreach(
                        fun(DepName) ->
                            EbinDir = filename:join([DepsDir, DepName, "ebin"]),
                            activate_dep_ebin(EbinDir)
                        end,
                        lists:sort(DepNames)
                    );
                {error, _} ->
                    ok
            end
    end,
    %% Add native ebin paths for FFI modules.
    NativeEbin = filename:join([AbsPath, "_build", "dev", "native", "ebin"]),
    case filelib:is_dir(NativeEbin) of
        true ->
            _ = code:add_pathz(NativeEbin),
            ok;
        false ->
            ok
    end,
    %% Add rebar3 hex dependency ebin paths (cowboy, gun, etc.).
    Rebar3LibDir = filename:join([AbsPath, "_build", "dev", "native", "default", "lib"]),
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
    ok.

%% @private Add a dependency ebin dir to the code path and activate its bt@* modules.
%% Modules are sorted by superclass dependency order before activation,
%% matching the approach used by workspace bootstrap.
-spec activate_dep_ebin(string()) -> ok.
activate_dep_ebin(EbinDir) ->
    case filelib:is_dir(EbinDir) of
        false ->
            ok;
        true ->
            _ = code:add_pathz(EbinDir),
            Modules = beamtalk_workspace_bootstrap:find_bt_modules_in_dir(EbinDir),
            Sorted = beamtalk_workspace_bootstrap:sort_modules_by_dependency(EbinDir, Modules),
            lists:foreach(fun activate_dep_module/1, Sorted)
    end.

%% @private Load a single dependency module, call register_class/0, and register it.
%% If register_class/0 fails, the failure is logged and the module is not
%% registered in workspace_meta (to avoid masking the error).
-spec activate_dep_module(module()) -> ok.
activate_dep_module(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {module, ModuleName} ->
            RegisterResult =
                case erlang:function_exported(ModuleName, register_class, 0) of
                    true ->
                        try
                            ModuleName:register_class(),
                            ok
                        catch
                            Class:Reason:Stacktrace ->
                                ?LOG_WARNING(
                                    "load-project: register_class/0 failed for ~p: ~p:~p",
                                    [ModuleName, Class, Reason],
                                    #{domain => [beamtalk, runtime], stacktrace => Stacktrace}
                                ),
                                {error, {Class, Reason}}
                        end;
                    false ->
                        ok
                end,
            case RegisterResult of
                ok ->
                    SourcePath = extract_dep_source_path(ModuleName),
                    beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
                    ?LOG_DEBUG(
                        "load-project: activated dependency module ~p",
                        [ModuleName],
                        #{domain => [beamtalk, runtime]}
                    );
                {error, _} ->
                    ok
            end;
        {error, Reason} ->
            ?LOG_WARNING(
                "load-project: failed to load dependency module ~p: ~p",
                [ModuleName, Reason],
                #{domain => [beamtalk, runtime]}
            )
    end.

%% @private Extract the source file path for a dependency module.
%% Uses the same beamtalk_source attribute as bootstrap's extract_source_path/1.
-spec extract_dep_source_path(module()) -> string() | undefined.
extract_dep_source_path(ModuleName) ->
    try
        Attrs = erlang:get_module_info(ModuleName, attributes),
        case proplists:get_value(beamtalk_source, Attrs) of
            [Path] when is_list(Path) -> Path;
            _ -> undefined
        end
    catch
        _:_ -> undefined
    end.

%%% Internal helpers

%% @private
%% @doc Recursively find all .bt files in a directory.
-spec find_bt_files(string()) -> [string()].
find_bt_files(Dir) ->
    case filelib:is_dir(Dir) of
        false ->
            [];
        true ->
            filelib:fold_files(Dir, ".*\\.bt$", true, fun(F, Acc) -> [F | Acc] end, [])
    end.

%% @private
%% @doc Find all .erl files in a directory, recursively but skipping test/.
%% Returns an empty list if the directory does not exist.
-spec find_erl_files(string()) -> [string()].
find_erl_files(Dir) ->
    case filelib:is_dir(Dir) of
        false ->
            [];
        true ->
            %% Recursively find all .erl files but skip native/test/ — those are
            %% EUnit tests, not runtime modules that should be loaded into the VM.
            AllErl = filelib:fold_files(Dir, ".*\\.erl$", true, fun(F, Acc) -> [F | Acc] end, []),
            TestPrefix = filename:join(Dir, "test") ++ "/",
            [F || F <- AllErl, not lists:prefix(TestPrefix, F)]
    end.

%% @private
%% @doc Compile a list of native .erl files via compile:file/2.
%% Returns {Errors, CompiledCount} where Errors is a list of structured error maps.
%% Include paths are set to native/include/ if present.
-spec compile_native_erl_files([string()], string()) -> {[map()], non_neg_integer()}.
compile_native_erl_files([], _ProjectRoot) ->
    {[], 0};
compile_native_erl_files(ErlFiles, ProjectRoot) ->
    NativeDir = filename:join(ProjectRoot, "native"),
    IncludeDir = filename:join(NativeDir, "include"),
    BaseOpts = [debug_info, return_errors, return_warnings, binary],
    IncludeOpts =
        case filelib:is_dir(IncludeDir) of
            true -> [{i, IncludeDir} | BaseOpts];
            false -> BaseOpts
        end,
    lists:foldl(
        fun(ErlPath, {ErrsAcc, CountAcc}) ->
            ?LOG_INFO(
                "load-project: compiling native .erl file: ~s",
                [ErlPath],
                #{domain => [beamtalk, runtime]}
            ),
            %% BT-1719: Snapshot the .erl file's mtime before compiling so
            %% is_native_erl_stale/2 can compare against it later.
            ErlMtimeSnapshot = get_file_mtime(ErlPath),
            case compile:file(ErlPath, IncludeOpts) of
                {ok, ModName, Binary, Warnings} ->
                    %% Load the compiled binary into the VM.
                    case code:load_binary(ModName, ErlPath, Binary) of
                        {module, ModName} ->
                            set_native_compile_mtime(ModName, ErlMtimeSnapshot),
                            log_erl_warnings(ErlPath, Warnings),
                            {ErrsAcc, CountAcc + 1};
                        {error, LoadErr} ->
                            ErrMap = #{
                                <<"path">> => list_to_binary(ErlPath),
                                <<"kind">> => <<"load_error">>,
                                <<"message">> => iolist_to_binary(
                                    io_lib:format("Failed to load compiled module: ~p", [LoadErr])
                                )
                            },
                            {[ErrMap | ErrsAcc], CountAcc}
                    end;
                {ok, ModName, Binary} ->
                    case code:load_binary(ModName, ErlPath, Binary) of
                        {module, ModName} ->
                            set_native_compile_mtime(ModName, ErlMtimeSnapshot),
                            {ErrsAcc, CountAcc + 1};
                        {error, LoadErr} ->
                            ErrMap = #{
                                <<"path">> => list_to_binary(ErlPath),
                                <<"kind">> => <<"load_error">>,
                                <<"message">> => iolist_to_binary(
                                    io_lib:format("Failed to load compiled module: ~p", [LoadErr])
                                )
                            },
                            {[ErrMap | ErrsAcc], CountAcc}
                    end;
                {error, CompileErrors, Warnings} ->
                    log_erl_warnings(ErlPath, Warnings),
                    ErrMaps = format_erl_compile_errors(ErlPath, CompileErrors),
                    {ErrMaps ++ ErrsAcc, CountAcc};
                error ->
                    ErrMap = #{
                        <<"path">> => list_to_binary(ErlPath),
                        <<"kind">> => <<"compile_error">>,
                        <<"message">> => <<"Erlang compilation failed">>
                    },
                    {[ErrMap | ErrsAcc], CountAcc}
            end
        end,
        {[], 0},
        ErlFiles
    ).

%% @private
%% @doc Format Erlang compile errors into structured error maps.
-spec format_erl_compile_errors(string(), [{string(), [term()]}]) -> [map()].
format_erl_compile_errors(ErlPath, CompileErrors) ->
    PathBin = list_to_binary(ErlPath),
    lists:flatmap(
        fun({_ErrFile, FileErrors}) ->
            lists:map(
                fun
                    ({Line, Module, Desc}) ->
                        Msg = iolist_to_binary(Module:format_error(Desc)),
                        ErrMap = #{
                            <<"path">> => PathBin,
                            <<"kind">> => <<"compile_error">>,
                            <<"message">> => Msg
                        },
                        case Line of
                            L when is_integer(L), L > 0 -> ErrMap#{<<"line">> => L};
                            _ -> ErrMap
                        end;
                    (Other) ->
                        #{
                            <<"path">> => PathBin,
                            <<"kind">> => <<"compile_error">>,
                            <<"message">> => iolist_to_binary(
                                io_lib:format("~p", [Other])
                            )
                        }
                end,
                FileErrors
            )
        end,
        CompileErrors
    ).

%% @private
%% @doc Log Erlang compilation warnings.
-spec log_erl_warnings(string(), [{string(), [term()]}]) -> ok.
log_erl_warnings(_ErlPath, []) ->
    ok;
log_erl_warnings(ErlPath, Warnings) ->
    lists:foreach(
        fun({_WarnFile, FileWarnings}) ->
            lists:foreach(
                fun
                    ({Line, Module, Desc}) ->
                        Msg = Module:format_error(Desc),
                        ?LOG_WARNING(
                            "load-project: ~s:~p: ~s",
                            [ErlPath, Line, Msg],
                            #{domain => [beamtalk, runtime]}
                        );
                    (_Other) ->
                        ok
                end,
                FileWarnings
            )
        end,
        Warnings
    ).

%% @private
%% @doc Extract the declared class name and superclass from a .bt source file.
%% Returns {ClassName, SuperClass} as binaries, or {undefined, undefined}.
-spec extract_bt_class_info(string()) -> {binary() | undefined, binary() | undefined}.
extract_bt_class_info(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case re:run(Bin, <<"(\\w+)\\s+subclass:\\s+(\\w+)">>, [{capture, [1, 2], binary}]) of
                {match, [Super, Class]} -> {Class, Super};
                nomatch -> {undefined, undefined}
            end;
        {error, _} ->
            {undefined, undefined}
    end.

%% @private
%% @doc Sort .bt files in topological dependency order (superclass before subclass).
%% Files whose superclass is not in the project set come first.
-spec sort_bt_files_by_deps([string()]) -> [string()].
sort_bt_files_by_deps([]) ->
    [];
sort_bt_files_by_deps(Files) ->
    Infos = lists:map(
        fun(Path) ->
            {Class, Super} = extract_bt_class_info(Path),
            #{path => Path, class => Class, super => Super}
        end,
        Files
    ),
    ProjectClasses = sets:from_list([
        C
     || #{class := C} <- Infos, C =/= undefined
    ]),
    {Ready, Pending} = lists:partition(
        fun(#{super := Super}) ->
            Super =:= undefined orelse not sets:is_element(Super, ProjectClasses)
        end,
        Infos
    ),
    topo_sort_loop(Ready, Pending, []).

%% @private
-spec topo_sort_loop([map()], [map()], [string()]) -> [string()].
topo_sort_loop([], [], Acc) ->
    lists:reverse(Acc);
topo_sort_loop([], Pending, Acc) ->
    %% Unresolved entries remain (likely a cycle or unparseable class declaration).
    %% Warn and append them in original order so load proceeds best-effort.
    ?LOG_WARNING(
        "load-project: ~B file(s) have unresolved superclass dependencies "
        "(possible cycle or missing class declaration); loading in original order",
        [length(Pending)],
        #{domain => [beamtalk, runtime]}
    ),
    lists:reverse(Acc) ++ [maps:get(path, I) || I <- Pending];
topo_sort_loop([#{path := Path, class := Class} | Ready], Pending, Acc) ->
    {NowReady, StillPending} = lists:partition(
        fun(#{super := Super}) -> Super =:= Class end,
        Pending
    ),
    topo_sort_loop(Ready ++ NowReady, StillPending, [Path | Acc]).

%% @private
%% @doc Load files one by one, accumulating class maps and per-file error maps.
%% Accumulates in reverse to avoid quadratic ++ and reverses at the end.
%% Per-file errors are returned as structured maps with path, kind, and message
%% so callers can handle partial failures programmatically.
%%
%% BT-1608: Class indexes are rebuilt after each successful file load so that
%% later files in the batch can reference classes loaded earlier (e.g. test
%% files referencing fixture classes). The original BT-1543 optimisation of
%% building indexes once caused "Undefined function" errors when test files
%% were compiled before their fixture dependencies were visible in the index.
-spec load_files_sequential([string()], pid()) -> {[map()], [map()]}.
load_files_sequential(Files, SessionPid) ->
    Indexes0 = beamtalk_repl_compiler:build_class_indexes(),
    {RevClasses, RevErrors, _} =
        lists:foldl(
            fun(Path, {ClassesAcc, ErrorsAcc, Indexes}) ->
                case beamtalk_repl_shell:load_file(SessionPid, Path, Indexes) of
                    {ok, Classes} ->
                        NewIndexes = beamtalk_repl_compiler:build_class_indexes(),
                        {lists:reverse(Classes, ClassesAcc), ErrorsAcc, NewIndexes};
                    {error, Reason} ->
                        ErrMaps = structured_file_errors(Path, Reason),
                        {ClassesAcc, lists:reverse(ErrMaps, ErrorsAcc), Indexes}
                end
            end,
            {[], [], Indexes0},
            Files
        ),
    {lists:reverse(RevClasses), lists:reverse(RevErrors)}.

%% @private
%% @doc Load files without a session (BT-1723).
%% Uses beamtalk_repl_loader:reload_class_file/1 for stateless compilation.
%% Called by sync_project when no SessionPid is available (e.g., from
%% the Workspace sync primitive).
-spec load_files_stateless([string()]) -> {[map()], [map()]}.
load_files_stateless(Files) ->
    {RevClasses, RevErrors} =
        lists:foldl(
            fun(Path, {ClassesAcc, ErrorsAcc}) ->
                case beamtalk_repl_loader:reload_class_file(Path) of
                    {ok, Classes} ->
                        {lists:reverse(Classes, ClassesAcc), ErrorsAcc};
                    {error, Reason} ->
                        ErrMaps = structured_file_errors(Path, Reason),
                        {ClassesAcc, lists:reverse(ErrMaps, ErrorsAcc)}
                end
            end,
            {[], []},
            Files
        ),
    {lists:reverse(RevClasses), lists:reverse(RevErrors)}.

%% @private
%% @doc Build structured error maps for a per-file load failure.
%% For compile errors with a diagnostic list, returns one error map per diagnostic
%% with line numbers preserved. Other errors return a single-element list.
-spec structured_file_errors(string(), term()) -> [map()].
structured_file_errors(Path, {compile_error, Diagnostics}) when
    is_list(Diagnostics), Diagnostics =/= []
->
    PathBin = list_to_binary(Path),
    [diagnostic_to_error_map(PathBin, D) || D <- Diagnostics];
structured_file_errors(Path, Reason) ->
    #beamtalk_error{kind = Kind, message = Msg, hint = Hint} =
        beamtalk_repl_errors:ensure_structured_error(Reason),
    ErrMap = #{
        <<"path">> => list_to_binary(Path),
        <<"kind">> => atom_to_binary(Kind, utf8),
        <<"message">> => Msg
    },
    [
        case Hint of
            undefined -> ErrMap;
            _ -> ErrMap#{<<"hint">> => Hint}
        end
    ].

%% @private
%% @doc Convert a single compiler diagnostic map to a structured error map.
-spec diagnostic_to_error_map(binary(), term()) -> map().
diagnostic_to_error_map(PathBin, D) when is_map(D) ->
    Msg = maps:get(message, D, <<"Unknown error">>),
    ErrMap0 = #{
        <<"path">> => PathBin,
        <<"kind">> => <<"compile_error">>,
        <<"message">> => Msg
    },
    ErrMap1 =
        case maps:find(line, D) of
            {ok, Line} when is_integer(Line) -> ErrMap0#{<<"line">> => Line};
            _ -> ErrMap0
        end,
    case maps:find(hint, D) of
        {ok, Hint} when is_binary(Hint) -> ErrMap1#{<<"hint">> => Hint};
        _ -> ErrMap1
    end;
diagnostic_to_error_map(PathBin, D) when is_binary(D) ->
    #{
        <<"path">> => PathBin,
        <<"kind">> => <<"compile_error">>,
        <<"message">> => D
    };
diagnostic_to_error_map(PathBin, D) ->
    #{
        <<"path">> => PathBin,
        <<"kind">> => <<"compile_error">>,
        <<"message">> => iolist_to_binary(io_lib:format("~p", [D]))
    }.

%% @private
%% @doc Collect collision warnings for the loaded classes after a file load.
%% BT-737: Drains warnings from the ETS table keyed by class name and
%% formats them as human-readable binary strings for the protocol response.
-spec collect_load_warnings([map()]) -> [binary()].
collect_load_warnings(Classes) ->
    %% Use safe_to_existing_atom (not list_to_atom) to avoid leaking atoms for
    %% user-defined class names. Class atoms must already exist after loading.
    ClassNames = lists:filtermap(
        fun
            (#{name := N}) when N =/= "" ->
                case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(N)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            (_) ->
                false
        end,
        Classes
    ),
    Collisions = beamtalk_runtime_api:drain_class_warnings_by_names(ClassNames),
    [
        format_collision_warning(ClassName, OldModule, NewModule)
     || {ClassName, OldModule, NewModule} <- Collisions
    ].

%% @private
%% @doc Collect collision warnings with package-aware draining.
%% BT-742: Uses the module atom to derive the package, then drains only
%% that package's warnings — leaving sibling packages' warnings intact.
%% Falls back to unqualified drain when ModuleAtom is undefined (e.g.,
%% path-based :reload without a known module).
-spec collect_load_warnings_qualified([map()], atom() | undefined) -> [binary()].
collect_load_warnings_qualified(Classes, undefined) ->
    %% No module context — fall back to unqualified drain (drains all packages).
    collect_load_warnings(Classes);
collect_load_warnings_qualified(Classes, ModuleAtom) ->
    Package = beamtalk_class_registry:extract_package_from_module(ModuleAtom),
    ClassNames = lists:filtermap(
        fun
            (#{name := N}) when N =/= "" ->
                case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(N)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            (_) ->
                false
        end,
        Classes
    ),
    QualifiedNames = [{Package, CN} || CN <- ClassNames],
    Collisions = beamtalk_runtime_api:drain_class_warnings_by_qualified_names(QualifiedNames),
    [
        format_collision_warning(ClassName, OldModule, NewModule)
     || {ClassName, OldModule, NewModule} <- Collisions
    ].

%% @private
-spec format_collision_warning(atom(), atom(), atom()) -> binary().
format_collision_warning(ClassName, OldModule, NewModule) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    OldPkg = extract_package_from_module(OldModule),
    NewPkg = extract_package_from_module(NewModule),
    %% BT-1659: When both modules come from packages, add a qualified-name hint.
    QualifiedHint =
        case {OldPkg, NewPkg} of
            {OldP, NewP} when OldP =/= undefined, NewP =/= undefined ->
                iolist_to_binary([
                    ". Use ",
                    OldP,
                    "@",
                    ClassBin,
                    " or ",
                    NewP,
                    "@",
                    ClassBin,
                    " to be explicit"
                ]);
            _ ->
                <<>>
        end,
    iolist_to_binary([
        "Class '",
        ClassBin,
        "' redefined (was ",
        atom_to_binary(OldModule, utf8),
        ", now ",
        atom_to_binary(NewModule, utf8),
        ")",
        QualifiedHint
    ]).

%% @private Extract the package segment from a bt@{pkg}@{class} module name.
%% Returns the package name binary or undefined.
%% Only 3-part names (bt@{pkg}@{class}) have a package; 2-part names (bt@{class})
%% are stdlib/unqualified and return undefined.
-spec extract_package_from_module(atom()) -> binary() | undefined.
extract_package_from_module(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg, _Class | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        _ ->
            undefined
    end.

%% @private
-spec do_reload(string(), atom() | undefined, beamtalk_repl_protocol:protocol_msg(), pid()) ->
    binary().
do_reload(Path, ModuleAtom, Msg, SessionPid) ->
    %% BT-1717: Demand-driven native .erl recompilation on single-file reload.
    %% Before compiling the .bt file, check if it references native Erlang modules
    %% (via native: annotation or (Erlang module) FFI) and recompile them if stale.
    case maybe_recompile_native_deps(Path, find_project_root(Path)) of
        {ok, _Count} ->
            ok;
        {error, NativeErrors} ->
            ?LOG_ERROR(
                "reload: native .erl compilation failed for ~s: ~p",
                [Path, NativeErrors],
                #{domain => [beamtalk, runtime]}
            )
    end,
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            %% BT-742: Use qualified drain for reload — only drain warnings for
            %% this file's package, not sibling packages with same class names.
            Warnings = collect_load_warnings_qualified(Classes, ModuleAtom),
            {ActorCount, MigrationFailures} =
                trigger_actor_code_change(ModuleAtom, Classes),
            beamtalk_repl_json:encode_reloaded(
                Classes, ActorCount, MigrationFailures, Msg, Warnings
            );
        {error, Reason} ->
            WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end.

%% @private
-spec trigger_actor_code_change(atom() | undefined, [map()]) ->
    {non_neg_integer(), [{pid(), term()}]}.
trigger_actor_code_change(ModuleAtom, Classes) ->
    ModuleAtoms = lists:usort(resolve_module_atoms(ModuleAtom, Classes)),
    case whereis(beamtalk_actor_registry) of
        undefined ->
            {0, []};
        RegistryPid ->
            {Count, FailsRev} = lists:foldl(
                fun(Mod, {CountAcc, FailAcc}) ->
                    case beamtalk_repl_actors:get_pids_for_module(RegistryPid, Mod) of
                        {ok, []} ->
                            {CountAcc, FailAcc};
                        {ok, Pids} ->
                            {ok, Upgraded, Failures} =
                                beamtalk_runtime_api:trigger_code_change(Mod, Pids),
                            NewFailAcc = lists:foldl(
                                fun(F, A) -> [F | A] end, FailAcc, Failures
                            ),
                            {CountAcc + Upgraded, NewFailAcc};
                        {error, _} ->
                            {CountAcc, FailAcc}
                    end
                end,
                {0, []},
                ModuleAtoms
            ),
            {Count, lists:reverse(FailsRev)}
    end.

%% @private
-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(
        fun(ClassMap) ->
            case maps:get(name, ClassMap, "") of
                "" ->
                    false;
                Name when is_list(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(Name)) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                Name when is_binary(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(Name) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                _ ->
                    false
            end
        end,
        Classes
    ).

%% @private
-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    resolve_class_to_module(ClassName, ClassPids).

resolve_class_to_module(ClassName, []) ->
    ClassName;
resolve_class_to_module(ClassName, [Pid | Rest]) ->
    try
        case beamtalk_runtime_api:class_name(Pid) of
            ClassName ->
                beamtalk_runtime_api:module_name(Pid);
            _ ->
                resolve_class_to_module(ClassName, Rest)
        end
    catch
        _:_ ->
            resolve_class_to_module(ClassName, Rest)
    end.

%% @private
%% @doc Resolve a source path for a module when workspace_meta has none.
%% Reads the beamtalk_source module attribute embedded by the compiler (BT-845/BT-860).
-spec resolve_source_path(atom()) -> string().
resolve_source_path(ModName) ->
    try
        Attrs = erlang:get_module_info(ModName, attributes),
        case proplists:get_value(beamtalk_source, Attrs) of
            [Path] when is_list(Path), Path =/= "" -> Path;
            _ -> "unknown"
        end
    catch
        _:_ -> "unknown"
    end.

%% @private
%% @doc Build a map from BEAM module atom to Beamtalk class name binary.
%% Queries all registered class processes in a single pass.
-spec module_to_class_name_map() -> #{atom() => binary()}.
module_to_class_name_map() ->
    Pids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    lists:foldl(
        fun(Pid, Acc) ->
            try
                ModAtom = beamtalk_runtime_api:module_name(Pid),
                ClassName = beamtalk_runtime_api:class_name(Pid),
                Acc#{ModAtom => atom_to_binary(ClassName, utf8)}
            catch
                _:_ -> Acc
            end
        end,
        #{},
        Pids
    ).

%%% ===================================================================
%%% BT-1717: Demand-driven native .erl recompilation on single-file reload
%%% ===================================================================

%% @private
%% @doc Extract native module references from a .bt source file.
%% Returns a deduplicated list of Erlang module name binaries referenced via:
%%   - `native: module_name` annotation in the class header
%%   - `(Erlang module_name)` FFI calls in method bodies
-spec extract_native_refs(string()) -> [binary()].
extract_native_refs(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            NativeRefs =
                case
                    re:run(
                        Bin,
                        <<"(?:native:\\s+(\\w+))">>,
                        [{capture, [1], binary}, global]
                    )
                of
                    {match, Matches} -> [M || [M] <- Matches];
                    nomatch -> []
                end,
            FfiRefs =
                case
                    re:run(
                        Bin,
                        <<"\\(Erlang\\s+(\\w+)\\)">>,
                        [{capture, [1], binary}, global]
                    )
                of
                    {match, FfiMatches} -> [M || [M] <- FfiMatches];
                    nomatch -> []
                end,
            lists:usort(NativeRefs ++ FfiRefs);
        {error, _} ->
            []
    end.

%% @private
%% @doc Find the project root by walking up from a file path to find beamtalk.toml.
%% Returns the directory containing beamtalk.toml, or undefined if not found.
-spec find_project_root(string()) -> string() | undefined.
find_project_root(Path) ->
    Dir = filename:dirname(filename:absname(Path)),
    find_project_root_walk(Dir).

%% @private
-spec find_project_root_walk(string()) -> string() | undefined.
find_project_root_walk(Dir) ->
    Parent = filename:dirname(Dir),
    case Parent =:= Dir of
        true ->
            %% Reached filesystem root (/ on Unix, X:/ on Windows)
            undefined;
        false ->
            case filelib:is_file(filename:join(Dir, "beamtalk.toml")) of
                true -> Dir;
                false -> find_project_root_walk(Parent)
            end
    end.

%% @private
%% @doc Check native module references from a .bt file and recompile stale .erl files.
%% For each native module referenced, checks if native/{module}.erl exists in the
%% project and if it's newer than its loaded .beam. If so, recompiles via compile:file/2.
%% Returns {ok, RecompiledCount} or {error, Errors}.
-spec maybe_recompile_native_deps(string(), string() | undefined) ->
    {ok, non_neg_integer()} | {error, [map()]}.
maybe_recompile_native_deps(_Path, undefined) ->
    {ok, 0};
maybe_recompile_native_deps(Path, ProjectRoot) ->
    NativeRefs = extract_native_refs(Path),
    case NativeRefs of
        [] ->
            {ok, 0};
        _ ->
            NativeDir = filename:join(ProjectRoot, "native"),
            StaleFiles = lists:filtermap(
                fun(ModBin) ->
                    ErlFile = filename:join(
                        NativeDir,
                        binary_to_list(ModBin) ++ ".erl"
                    ),
                    case filelib:is_file(ErlFile) of
                        false ->
                            false;
                        true ->
                            case is_native_erl_stale(ErlFile, ModBin) of
                                true -> {true, ErlFile};
                                false -> false
                            end
                    end
                end,
                NativeRefs
            ),
            case StaleFiles of
                [] ->
                    {ok, 0};
                _ ->
                    ?LOG_INFO(
                        "reload: demand-compiling ~B stale native .erl file(s) for ~s",
                        [length(StaleFiles), Path],
                        #{domain => [beamtalk, runtime]}
                    ),
                    {Errors, Count} = compile_native_erl_files(StaleFiles, ProjectRoot),
                    case Errors of
                        [] -> {ok, Count};
                        _ -> {error, Errors}
                    end
            end
    end.

%% @private
%% @doc Check if a native .erl file is newer than its loaded .beam counterpart.
%% Returns true if:
%%   - The module is not loaded at all (needs compilation)
%%   - The .erl file's mtime is newer than the .beam file's mtime
-spec is_native_erl_stale(string(), binary()) -> boolean().
is_native_erl_stale(ErlFile, ModBin) ->
    ModAtom = binary_to_atom(ModBin, utf8),
    case code:is_loaded(ModAtom) of
        false ->
            %% Module not loaded — needs compilation.
            true;
        {file, BeamPath} ->
            ErlMtime = get_file_mtime(ErlFile),
            BeamMtime =
                case is_list(BeamPath) of
                    true ->
                        %% BT-1719: When loaded via code:load_binary/3 the stored
                        %% path is the .erl source, not a .beam file. Comparing
                        %% its mtime against itself always yields "not stale".
                        %% Use the mtime snapshot taken at compile time instead.
                        case filename:extension(BeamPath) of
                            ".erl" ->
                                get_native_compile_mtime(ModAtom);
                            _ ->
                                get_file_mtime(BeamPath)
                        end;
                    false ->
                        %% BeamPath may be 'preloaded' or a cover-compiled atom.
                        %% In those cases, treat as stale to be safe.
                        {{0, 0, 0}, {0, 0, 0}}
                end,
            ErlMtime > BeamMtime
    end.

%% @private
%% @doc Record the .erl file's mtime when a native module is compiled in-memory.
%% Used by is_native_erl_stale/2 to detect changes since last compilation.
-spec set_native_compile_mtime(atom(), calendar:datetime()) -> ok.
set_native_compile_mtime(ModAtom, Mtime) ->
    persistent_term:put({beamtalk_native_mtime, ModAtom}, Mtime),
    ok.

%% @private
%% @doc Retrieve the .erl file's mtime that was recorded at compilation time.
%% Returns epoch if no mtime was recorded (triggers recompilation).
-spec get_native_compile_mtime(atom()) -> calendar:datetime().
get_native_compile_mtime(ModAtom) ->
    try
        persistent_term:get({beamtalk_native_mtime, ModAtom})
    catch
        error:badarg ->
            {{0, 0, 0}, {0, 0, 0}}
    end.

%%% ===================================================================
%%% BT-1685: Incremental load-project helpers
%%% ===================================================================

%% @private
%% @doc Classify files into changed, unchanged, and deleted lists.
%%
%% A file is "changed" if:
%%   - It is new (not in PreviousMtimes)
%%   - Its current mtime differs from the stored mtime
%%
%% A file is "unchanged" if its mtime matches the stored value.
%%
%% "Deleted" files are those present in PreviousMtimes but not in
%% the current file list.
-spec classify_files_by_change([string()], #{string() => calendar:datetime()}) ->
    {Changed :: [string()], Unchanged :: [string()], Deleted :: [string()]}.
classify_files_by_change(CurrentFiles, PreviousMtimes) ->
    CurrentSet = sets:from_list(CurrentFiles),
    {Changed, Unchanged} = lists:partition(
        fun(Path) ->
            case maps:find(Path, PreviousMtimes) of
                {ok, OldMtime} ->
                    CurrentMtime = get_file_mtime(Path),
                    CurrentMtime =/= OldMtime;
                error ->
                    %% New file — always changed
                    true
            end
        end,
        CurrentFiles
    ),
    %% Find deleted files: were in PreviousMtimes but not in current file list.
    Deleted = [
        P
     || P <- maps:keys(PreviousMtimes),
        not sets:is_element(P, CurrentSet)
    ],
    {Changed, Unchanged, Deleted}.

%% @private
%% @doc Get the mtime of a file.
%% Returns {{0,0,0},{0,0,0}} if the file doesn't exist or can't be read.
-spec get_file_mtime(string()) -> calendar:datetime().
get_file_mtime(Path) ->
    case filelib:last_modified(Path) of
        0 -> {{0, 0, 0}, {0, 0, 0}};
        Mtime -> Mtime
    end.

%% @private
%% @doc Snapshot mtimes for a list of files before loading.
%% Returns a list of {Path, Mtime} tuples.
-spec snapshot_file_mtimes([string()]) -> [{string(), calendar:datetime()}].
snapshot_file_mtimes(Files) ->
    [{Path, get_file_mtime(Path)} || Path <- Files].

%% @private
%% @doc Record file mtimes from a pre-load snapshot.
-spec record_file_mtimes_from_snapshot([{string(), calendar:datetime()}]) -> ok.
record_file_mtimes_from_snapshot(Snapshot) ->
    lists:foreach(
        fun({Path, Mtime}) ->
            beamtalk_workspace_meta:set_file_mtime(Path, Mtime)
        end,
        Snapshot
    ).

%% @private
%% @doc Handle files that were previously loaded but have been deleted.
%% Unregisters their classes from the class registry and unloads their BEAM modules.
%% Returns the count of deleted files processed.
-spec handle_deleted_files([string()], pid() | undefined) -> non_neg_integer().
handle_deleted_files([], _SessionPid) ->
    0;
handle_deleted_files(DeletedFiles, SessionPid) ->
    %% Build the module-to-class map once for all deleted files.
    ModToClass = module_to_class_name_map(),
    LoadedModules =
        case beamtalk_workspace_meta:loaded_modules() of
            {ok, Mods} -> Mods;
            {error, _} -> []
        end,
    lists:foreach(
        fun(Path) ->
            ?LOG_INFO(
                "load-project: file deleted, unloading: ~s",
                [Path],
                #{domain => [beamtalk, runtime]}
            ),
            %% Find modules loaded from this path and unregister them.
            unload_modules_for_path(Path, SessionPid, ModToClass, LoadedModules),
            beamtalk_workspace_meta:remove_file_mtime(Path)
        end,
        DeletedFiles
    ),
    length(DeletedFiles).

%% @private
%% @doc Find and unload all modules that were loaded from a given source path.
-spec unload_modules_for_path(string(), pid() | undefined, #{atom() => binary()}, [
    {atom(), string() | undefined}
]) -> ok.
unload_modules_for_path(Path, SessionPid, ModToClass, LoadedModules) ->
    lists:foreach(
        fun({ModuleName, SourcePath}) ->
            case SourcePath =:= Path of
                true ->
                    %% Try to find class name for this module and remove it.
                    case maps:find(ModuleName, ModToClass) of
                        {ok, ClassNameBin} ->
                            case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
                                {ok, ClassName} ->
                                    case beamtalk_runtime_api:remove_class_from_system(ClassName) of
                                        {ok, _} -> ok;
                                        {error, _} -> ok
                                    end;
                                {error, badarg} ->
                                    ok
                            end;
                        error ->
                            ok
                    end,
                    beamtalk_workspace_meta:unregister_module(ModuleName),
                    case SessionPid of
                        undefined -> ok;
                        _ -> beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName)
                    end;
                false ->
                    ok
            end
        end,
        LoadedModules
    ).

%% @private
%% @doc Build the incremental summary message.
%% Format: "Reloaded 2 of 7 files (5 unchanged)" or "Reloaded 2 of 7 files (3 unchanged, 2 deleted)"
-spec build_incremental_summary(
    non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()
) -> binary().
build_incremental_summary(ChangedCount, TotalFiles, UnchangedCount, DeletedCount) ->
    BaseParts = [
        "Reloaded ",
        integer_to_list(ChangedCount),
        " of ",
        integer_to_list(TotalFiles),
        " files"
    ],
    DetailParts =
        case {UnchangedCount, DeletedCount} of
            {0, 0} ->
                [];
            {U, 0} ->
                [" (", integer_to_list(U), " unchanged)"];
            {0, D} ->
                [" (", integer_to_list(D), " deleted)"];
            {U, D} ->
                [" (", integer_to_list(U), " unchanged, ", integer_to_list(D), " deleted)"]
        end,
    iolist_to_binary(BaseParts ++ DetailParts).
