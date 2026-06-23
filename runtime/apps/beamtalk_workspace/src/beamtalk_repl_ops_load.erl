%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_load).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for load-source, load-project, and unload operations.

Extracted from beamtalk_repl_server (BT-705).
The deprecated ops `load-file`, `reload`, and `modules` were removed in
BT-2091 — use `Workspace load:`, `ClassName reload`, and `Workspace classes`
respectively.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, resolve_class_to_module/1, resolve_module_atoms/2]).

%% BT-1723: Shared sync logic callable from both protocol handler and primitives.
-export([sync_project/2]).

%% BT-1719: Exported for demand-driven native .erl compilation from classReload.
-export([find_project_root/1, maybe_recompile_native_deps/2]).

%% Export internals for white-box testing of load-project helpers.
-ifdef(TEST).
-export([
    find_bt_files/1,
    find_erl_files/2,
    compile_native_erl_files/2,
    render_class_header/1,
    regenerate_native_class_header/1,
    native_generated_include_dir/1,
    build_source_class_module_index/1,
    source_module_name/3,
    extract_all_bt_classes/1,
    read_package_name/1,
    extract_bt_class_info/1,
    sort_bt_files_by_deps/1,
    structured_file_errors/2,
    format_collision_warning/3,
    extract_package_from_module/1,
    classify_files_by_change/2,
    get_file_mtime/1,
    extract_native_refs/1,
    filter_mtimes_under_project/2
]).
-endif.

%%% ============================================================================
%%% sync_project/2 — shared incremental sync logic (BT-1723)
%%% ============================================================================

-doc """
Perform an incremental project sync.

Scans the project at `Path` for `.bt` and `.erl` files, classifies them
as changed/unchanged/deleted by mtime, compiles changed files in dependency
order, and returns a result map with summary statistics.

`Options` is a map with optional keys:
  - `include_tests` (boolean, default false) — include test/ directory
  - `force` (boolean, default false) — recompile all files regardless of mtime
  - `session_pid` (pid() | undefined) — REPL session for module tracking

Returns `{ok, ResultMap}` where ResultMap contains:
  - `classes` — list of loaded class name binaries
  - `errors` — list of structured error maps
  - `summary` — human-readable summary binary
  - `changed_count` — number of files reloaded
  - `unchanged_count` — number of unchanged files
  - `deleted_count` — number of deleted files
  - `total_files` — total file count including deleted

Returns `{error, #beamtalk_error{}}` if no beamtalk.toml found.
""".
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
            {error,
                beamtalk_repl_errors:make(
                    file_not_found,
                    'WorkspaceInterface',
                    iolist_to_binary(["No beamtalk.toml found in: ", AbsPath]),
                    <<"Provide a directory path containing beamtalk.toml">>
                )};
        true ->
            do_sync_project(AbsPath, IncludeTests, Force, SessionPid)
    end.

-doc "Core sync logic, called after validating beamtalk.toml exists.".
-spec do_sync_project(string(), boolean(), boolean(), pid() | undefined) -> {ok, map()}.
do_sync_project(AbsPath, IncludeTests, Force, SessionPid) ->
    %% Activate pre-compiled dependency modules before loading project files,
    %% so that project classes can reference dependency classes (e.g. HTTPClient).
    DepErrors = beamtalk_workspace_bootstrap:activate_dependency_modules(AbsPath),
    %% Load the workspace project's own .app metadata so that
    %% beamtalk_package:all/0 and Workspace dependencies can discover it.
    ProjectEbin = filename:join([AbsPath, "_build", "dev", "ebin"]),
    case filelib:is_dir(ProjectEbin) of
        true ->
            _ = code:add_pathz(ProjectEbin),
            case beamtalk_module_activation:load_app_from_ebin(ProjectEbin) of
                {ok, []} ->
                    ok;
                {ok, AppErrors} ->
                    ?LOG_WARNING(
                        "Failed to load project .app metadata: ~p",
                        [AppErrors],
                        #{domain => [beamtalk, runtime]}
                    )
            end;
        false ->
            ok
    end,
    SrcFiles = find_bt_files(filename:join(AbsPath, "src")),
    TestFiles =
        case IncludeTests of
            true -> find_bt_files(filename:join(AbsPath, "test"));
            false -> []
        end,
    AllBtFiles = SrcFiles ++ TestFiles,
    NativeDir = filename:join(AbsPath, "native"),
    %% BT-2653: On the test-load path (include_tests=true), also discover
    %% native/test/ helper modules (e.g. a test server that a `.bt` test drives
    %% via `(Erlang <helper>) <msg>`). Under normal load-project they stay
    %% skipped — they are EUnit helpers, not runtime modules — mirroring how the
    %% `beamtalk test` CLI compiles native/test/ into _build/dev/native/ebin/.
    AllErlFiles = find_erl_files(NativeDir, IncludeTests),
    AllFiles = AllErlFiles ++ AllBtFiles,
    PreviousMtimes =
        case beamtalk_workspace_meta:get_file_mtimes() of
            {ok, Mtimes} -> Mtimes;
            {error, _} -> #{}
        end,
    %% BT-2089: Scope previous-mtime tracking to the project being synced.
    %% The workspace meta table accumulates mtimes for every project ever
    %% loaded into this workspace, so unfiltered classification would treat
    %% every other project's files as "deleted" and unload their classes.
    %% Only files whose path lies under AbsPath belong to this project sync.
    ProjectMtimes = filter_mtimes_under_project(PreviousMtimes, AbsPath),
    %% BT-2089: When include_tests=false, also drop previously-tracked test
    %% files from the baseline. Otherwise an `Op::Load` (which defaults to
    %% include_tests=false) classifies the test files loaded by an earlier
    %% `:test`/include_tests=true sync as "deleted" and unregisters them.
    TestDirPrefix = filename:join(AbsPath, "test") ++ "/",
    BaselineMtimes =
        case IncludeTests of
            true ->
                ProjectMtimes;
            false ->
                maps:filter(
                    fun
                        (P, _M) when is_list(P) ->
                            not lists:prefix(TestDirPrefix, P);
                        (_NonString, _M) ->
                            false
                    end,
                    ProjectMtimes
                )
        end,
    PrevErlMtimes =
        maps:filter(
            fun(P, _Mtime) -> filename:extension(P) =:= ".erl" end,
            BaselineMtimes
        ),
    PrevBtMtimes =
        maps:filter(
            fun(P, _Mtime) -> filename:extension(P) =:= ".bt" end,
            BaselineMtimes
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
            % elp:fixme W0023 intentional atom creation
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
    %% BT-2653: Before compiling any native .erl, regenerate the
    %% beamtalk_classes.hrl header from the live class→module index so native
    %% modules that `-include("beamtalk_classes.hrl")` build against the current
    %% class set. A stale header (or a stale .beam compiled against one) causes a
    %% `spec for undefined function …` cascade that fails the test-load even when
    %% the package's `beamtalk test` CLI is green. The CLI regenerates this header
    %% on every build (build.rs generate_class_header/2); the workspace
    %% incremental path must do the same to stay in sync.
    HeaderChanged = regenerate_native_class_header(AbsPath),
    %% BT-2653: decide which native .erl to (re)compile. The mtime check only
    %% catches .erl edits; it misses the case where the *header* changed (a class
    %% added/moved/renamed) while the .erl is byte-identical — the already-loaded
    %% .beam would stay compiled against the old macro values (stale cascade,
    %% symptom 2). So when the header content changed, recompile every native
    %% file, not just the mtime-changed ones. On the test-load path we also force
    %% a full native rebuild unconditionally so the runner always loads native
    %% modules fresh, exactly as the `beamtalk test` CLI does from a clean build.
    NativeToCompile =
        case IncludeTests orelse HeaderChanged of
            true -> AllErlFiles;
            false -> ChangedErl
        end,
    ErlPreLoadMtimes = snapshot_file_mtimes(NativeToCompile),
    {NativeErrors, _NativeCompiledCount} =
        compile_native_erl_files(NativeToCompile, AbsPath),
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
    Errors = lists:reverse(NativeErrors, BtErrors),
    ClassNames =
        [
            case maps:get(name, C, "") of
                N when is_binary(N) -> N;
                N -> list_to_binary(N)
            end
         || C <- AllClasses
        ],
    %% BT-2089: Drain class collision warnings so cross-project class
    %% redefinitions produce a clear diagnostic instead of silent eviction.
    CollisionWarnings = collect_load_warnings(AllClasses),
    TotalFiles = length(AllFiles) + DeletedCount,
    ChangedCount = length(ChangedBt) + length(ChangedErl),
    UnchangedCount = length(UnchangedBt) + length(UnchangedErl),
    DepErrorMsgs = [format_dep_error(Mod, Reason) || {Mod, Reason} <- DepErrors],
    {ok, #{
        classes => ClassNames,
        errors => Errors,
        dep_errors => DepErrorMsgs,
        warnings => CollisionWarnings,
        summary => build_incremental_summary(
            ChangedCount, TotalFiles, UnchangedCount, DeletedCount
        ),
        changed_count => ChangedCount,
        unchanged_count => UnchangedCount,
        deleted_count => DeletedCount,
        total_files => TotalFiles
    }}.

-doc """
Handle load-source/load-project/unload ops for the WebSocket transport —
encodes the term result to JSON at the edge (BT-2402).
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for load-source/load-project/unload (BT-2402, ADR 0082
write-surface). Returns `{loaded, Classes, Warnings}`,
`{load_project, Classes, Errors, Summary, Warnings}`, `{ok, Value, Output,
Warnings}` (unload), or `{error, #beamtalk_error{}}` — no JSON in this path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"load-project">>, Params, _Msg, SessionPid) ->
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
            {error, Err};
        {ok, Result} ->
            DepErrors = maps:get(dep_errors, Result, []),
            %% BT-2089: Surface collision warnings to the load-project caller
            %% so that cross-project class collisions produce a clear
            %% diagnostic instead of silent eviction.
            Warnings = maps:get(warnings, Result, []),
            {load_project, maps:get(classes, Result), maps:get(errors, Result) ++ DepErrors,
                maps:get(summary, Result), Warnings}
    end;
handle_term(<<"load-source">>, Params, _Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            {error,
                beamtalk_repl_errors:make(
                    empty_expression,
                    'REPL',
                    <<"Empty source">>,
                    <<"Enter Beamtalk source code to compile.">>
                )};
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    Warnings = collect_load_warnings(Classes),
                    {loaded, Classes, Warnings};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end
    end;
handle_term(<<"unload">>, Params, _Msg, SessionPid) ->
    %% BT-1239: Restore unload op — fully removes class from system (actors, gen_server,
    %% BEAM module, workspace_meta, session tracker).
    ClassNameBin = maps:get(<<"module">>, Params, <<>>),
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {error, badarg} ->
            {error,
                beamtalk_repl_errors:make(
                    class_not_found,
                    'REPL',
                    iolist_to_binary([<<"Class not found: '">>, ClassNameBin, <<"'">>])
                )};
        {ok, ClassName} ->
            case beamtalk_runtime_api:remove_class_from_system(ClassName) of
                {ok, ModuleName} ->
                    %% Clean up workspace-level and session-level tracking.
                    beamtalk_workspace_meta:unregister_module(ModuleName),
                    beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName),
                    {ok,
                        iolist_to_binary([
                            <<"Class '">>, ClassNameBin, <<"' removed from system">>
                        ]), <<>>, []};
                {error, Err} ->
                    {error, Err}
            end
    end.

%%% Internal helpers

-doc """
Format a dependency activation error as a structured error map.
Matches the `#{<<"path">> => ..., <<"kind">> => ..., <<"message">> => ...}`
format used by native Erlang and Beamtalk compilation errors.
""".
-spec format_dep_error(module(), term()) -> map().
format_dep_error(Mod, Reason) ->
    #{
        <<"path">> => atom_to_binary(Mod, utf8),
        <<"kind">> => <<"dep_activation_error">>,
        <<"message">> => iolist_to_binary(
            io_lib:format("Failed to activate dependency module ~p: ~p", [Mod, Reason])
        )
    }.

-doc "Recursively find all .bt files in a directory.".
-spec find_bt_files(string()) -> [string()].
find_bt_files(Dir) ->
    case filelib:is_dir(Dir) of
        false ->
            [];
        true ->
            filelib:fold_files(Dir, ".*\\.bt$", true, fun(F, Acc) -> [F | Acc] end, [])
    end.

-doc """
Find all .erl files in a directory recursively.

When `IncludeTests` is false (normal load-project), native/test/ is skipped —
those are EUnit helpers, not runtime modules that should be loaded into the VM.

When `IncludeTests` is true (the test-load path: `load-tests`/`list-tests`),
native/test/ helper modules ARE included so a `.bt` test can drive them via
`(Erlang <helper>) <msg>` — mirroring how the `beamtalk test` CLI compiles
native/test/ into _build/dev/native/ebin/ (BT-2653).

Returns an empty list if the directory does not exist.
""".
-spec find_erl_files(string(), boolean()) -> [string()].
find_erl_files(Dir, IncludeTests) ->
    case filelib:is_dir(Dir) of
        false ->
            [];
        true ->
            AllErl = filelib:fold_files(Dir, ".*\\.erl$", true, fun(F, Acc) -> [F | Acc] end, []),
            case IncludeTests of
                true ->
                    AllErl;
                false ->
                    TestPrefix = filename:join(Dir, "test") ++ "/",
                    [F || F <- AllErl, not lists:prefix(TestPrefix, F)]
            end
    end.

-doc """
Compile a list of native .erl files via compile:file/2.
Returns {Errors, CompiledCount} where Errors is a list of structured error maps.
Include paths are set to native/include/ if present.
""".
-spec compile_native_erl_files([string()], string()) -> {[map()], non_neg_integer()}.
compile_native_erl_files([], _ProjectRoot) ->
    {[], 0};
compile_native_erl_files(ErlFiles, ProjectRoot) ->
    NativeDir = filename:join(ProjectRoot, "native"),
    IncludeDir = filename:join(NativeDir, "include"),
    %% BT-2653: Native modules may `-include("beamtalk_classes.hrl")`, the
    %% generated class→module header. The CLI writes it to
    %% _build/dev/native/include/ and adds that dir to the erlc include path;
    %% the workspace path must do the same (regenerate_native_class_header/1
    %% refreshes it before compilation) so includes resolve against a fresh
    %% header rather than failing or picking up a stale copy.
    %%
    %% The generated dir is listed FIRST so it takes precedence: erlc searches
    %% `-I` dirs in order, and the freshly generated header must win over any
    %% hand-written copy that might sit in the project's native/include/.
    GeneratedIncludeDir = native_generated_include_dir(ProjectRoot),
    BaseOpts = [debug_info, return_errors, return_warnings, binary],
    %% Prepend native/include first, then the generated dir, so the final list
    %% is [generated, native/include | BaseOpts] — generated searched first.
    IncludeOpts0 =
        case filelib:is_dir(IncludeDir) of
            true -> [{i, IncludeDir} | BaseOpts];
            false -> BaseOpts
        end,
    IncludeOpts =
        case filelib:is_dir(GeneratedIncludeDir) of
            true -> [{i, GeneratedIncludeDir} | IncludeOpts0];
            false -> IncludeOpts0
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

-doc "Format Erlang compile errors into structured error maps.".
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

-doc "Log Erlang compilation warnings.".
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

-doc """
The directory holding generated native headers (e.g. beamtalk_classes.hrl):
`<ProjectRoot>/_build/dev/native/include`. Mirrors the CLI's BuildLayout
`native_include_dir/0` so the workspace and CLI builds share one location.
""".
-spec native_generated_include_dir(string()) -> string().
native_generated_include_dir(ProjectRoot) ->
    filename:join([ProjectRoot, "_build", "dev", "native", "include"]).

-doc """
Regenerate the `beamtalk_classes.hrl` header from the live class→module index.

BT-2653: Native `.erl` modules `-include("beamtalk_classes.hrl")` to map class
names to compiled BEAM module atoms via `?BT_CLASS_MODULE_<Class>` macros. The
`beamtalk test` CLI regenerates this header on every build (build.rs
`generate_class_header/2`); the workspace incremental native build must do the
same so it never compiles against a stale header — the root cause of the
`spec for undefined function …` cascade on the LiveView test-load path.

The index is the union of two sources (BT-2671):

  1. The project's own `src/**/*.bt` classes, derived by scanning source for
     `Super subclass: Class` declarations and computing the package-qualified
     module atom (`bt@<pkg>@<relative@path>`) exactly as the CLI does in
     `build.rs` (`build_class_module_index` + `compute_relative_module`). This
     guarantees a *cold* load — where the project's classes are not yet
     registered — still emits a `-define(BT_CLASS_MODULE_<Class>, …)` for every
     class defined in the package being loaded, so a native module that uses
     `?BT_CLASS_MODULE_Foo` for a same-package class `Foo` compiles instead of
     failing on a missing macro (a relocated symptom-2 cascade).

  2. The live class registry
     (`beamtalk_repl_compiler:build_class_module_index/0`), which is the
     canonical source of the *actual* runtime module atoms a native
     `?BT_CLASS_MODULE_*` call must resolve to.

The live registry takes precedence on conflict: on the warm/test-load path the
registry holds the authoritative module atom (including its exact casing), so it
overrides the source-derived guess; the source index only fills in classes the
registry does not yet know about (the cold-load gap). The module atom each
source builds for the same class is identical by construction — both lowercase
the file-stem/path segments via the same snake-case rule
(`beamtalk_repl_loader:to_snake_case/1` mirrors the Rust
`core_erlang:to_module_name/1`) — so the union never produces a casing-divergent
duplicate.

Returns `true` if the header content changed (so callers can force a native
rebuild), `false` if it was already up to date. The write is atomic
(temp file + rename) so a concurrent native compile never reads a half-written
header. The header is written to `_build/dev/native/include/beamtalk_classes.hrl`
— the same location the CLI uses, and the dir added to the erlc include path by
`compile_native_erl_files/2`. Best-effort: a write failure is logged and returns
`false` rather than aborting the load.
""".
-spec regenerate_native_class_header(string()) -> boolean().
regenerate_native_class_header(ProjectRoot) ->
    %% BT-2671: Union the source-AST-derived index (complete on a cold load)
    %% with the live registry (canonical module atoms on the warm path). The
    %% registry wins on conflict via the second arg to maps:merge/2.
    SourceIndex = build_source_class_module_index(ProjectRoot),
    RegistryIndex = beamtalk_repl_compiler:build_class_module_index(),
    Index = maps:merge(SourceIndex, RegistryIndex),
    IncludeDir = native_generated_include_dir(ProjectRoot),
    HrlPath = filename:join(IncludeDir, "beamtalk_classes.hrl"),
    Content = iolist_to_binary(render_class_header(Index)),
    case header_content_matches(HrlPath, Content) of
        true ->
            %% Already up to date — no rewrite, no forced rebuild.
            false;
        false ->
            write_native_class_header(IncludeDir, HrlPath, Content, maps:size(Index))
    end.

-spec write_native_class_header(string(), string(), binary(), non_neg_integer()) -> boolean().
write_native_class_header(IncludeDir, HrlPath, Content, ClassCount) ->
    case filelib:ensure_dir(HrlPath) of
        ok ->
            %% Atomic publish: write to a unique temp file in the same dir, then
            %% rename over the target so a racing compile never sees a partial
            %% header (file:write_file/2 is not atomic).
            TmpPath = HrlPath ++ ".tmp." ++ integer_to_list(erlang:unique_integer([positive])),
            case file:write_file(TmpPath, Content) of
                ok ->
                    case file:rename(TmpPath, HrlPath) of
                        ok ->
                            ?LOG_INFO(
                                "test-load: regenerated ~s (~B classes)",
                                [HrlPath, ClassCount],
                                #{domain => [beamtalk, runtime]}
                            ),
                            true;
                        {error, RenameReason} ->
                            _ = file:delete(TmpPath),
                            ?LOG_WARNING(
                                "test-load: failed to publish ~s: ~p",
                                [HrlPath, RenameReason],
                                #{domain => [beamtalk, runtime]}
                            ),
                            false
                    end;
                {error, WriteReason} ->
                    ?LOG_WARNING(
                        "test-load: failed to write ~s: ~p",
                        [TmpPath, WriteReason],
                        #{domain => [beamtalk, runtime]}
                    ),
                    false
            end;
        {error, DirReason} ->
            ?LOG_WARNING(
                "test-load: failed to create ~s: ~p",
                [IncludeDir, DirReason],
                #{domain => [beamtalk, runtime]}
            ),
            false
    end.

-spec header_content_matches(string(), binary()) -> boolean().
header_content_matches(HrlPath, Content) ->
    case file:read_file(HrlPath) of
        {ok, Existing} -> Existing =:= Content;
        {error, _} -> false
    end.

-doc """
Build a class→module index from the project's `src/**/*.bt` source (BT-2671).

This is the source-AST-derived index that gives the cold-load path full parity
with the CLI: on a clean load the project's own classes are not yet registered,
so the live registry alone (`build_class_module_index/0`) would omit them and a
native module referencing `?BT_CLASS_MODULE_Foo` for a same-package class would
fail to compile. By scanning source we always include every class defined in the
package being loaded.

For each `src/*.bt` file, every `Super subclass: Class` declaration maps to the
package-qualified module atom `bt@<pkg>@<relative@path>`, mirroring the CLI's
`build.rs` (`build_class_module_index` + `compute_relative_module`). The package
name is read from `beamtalk.toml`; subdirectory segments under `src/` become `@`
segments, each snake-cased via `beamtalk_repl_loader:to_snake_case/1` — the same
transform the Rust `core_erlang:to_module_name/1` applies, so the module atom is
byte-identical to the one the CLI build produces (casing parity).

Returns an empty map (no entries, never a crash) when the package name cannot be
determined or `src/` is absent — the caller then falls back to the live registry
alone, exactly the previous behaviour.
""".
-spec build_source_class_module_index(string()) -> #{binary() => binary()}.
build_source_class_module_index(ProjectRoot) ->
    case read_package_name(ProjectRoot) of
        undefined ->
            #{};
        PackageName ->
            SrcDir = filename:join(ProjectRoot, "src"),
            BtFiles = find_bt_files(SrcDir),
            lists:foldl(
                fun(Path, Acc) ->
                    ModuleName = source_module_name(Path, SrcDir, PackageName),
                    Classes = extract_all_bt_classes(Path),
                    lists:foldl(
                        fun(ClassName, InnerAcc) ->
                            InnerAcc#{ClassName => ModuleName}
                        end,
                        Acc,
                        Classes
                    )
                end,
                #{},
                BtFiles
            )
    end.

-doc """
Compute the package-qualified module atom binary for a `src/` .bt file.

Mirrors the CLI's `compute_relative_module` (build.rs): the file's path relative
to `src/` becomes `@`-joined snake-cased segments, prefixed with `bt@<pkg>@`.
E.g. `src/util/http_response.bt` in package `web` → `bt@web@util@http_response`.
""".
-spec source_module_name(string(), string(), binary()) -> binary().
source_module_name(Path, SrcDir, PackageName) ->
    AbsPath = filename:absname(Path),
    AbsSrc = filename:absname(SrcDir),
    SrcParts = filename:split(AbsSrc),
    PathParts = filename:split(AbsPath),
    RelSegments =
        case lists:prefix(SrcParts, PathParts) of
            true ->
                Rel = lists:nthtail(length(SrcParts), PathParts),
                drop_ext_segments(Rel);
            false ->
                %% Defensive: fall back to the bare stem if the file is somehow
                %% not under src/ (find_bt_files only returns files under it).
                [filename:basename(Path, ".bt")]
        end,
    SnakeSegments = [beamtalk_repl_loader:to_snake_case(S) || S <- RelSegments],
    iolist_to_binary(["bt@", PackageName, "@", lists:join("@", SnakeSegments)]).

%% Strip the `.bt` extension from the final path segment.
-spec drop_ext_segments([string()]) -> [string()].
drop_ext_segments([]) ->
    [];
drop_ext_segments(Segments) ->
    Last = lists:last(Segments),
    lists:droplast(Segments) ++ [filename:rootname(Last, ".bt")].

-doc """
Extract all declared class names from a .bt source file (BT-2671).

Unlike `extract_bt_class_info/1` (which returns only the first declaration),
this returns every `Super subclass: Class` class name in the file, so a source
file declaring multiple classes contributes all of them to the cold-load index.
Returns binaries; an unreadable file yields an empty list.
""".
-spec extract_all_bt_classes(string()) -> [binary()].
extract_all_bt_classes(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            case
                re:run(
                    Bin,
                    <<"\\w+\\s+subclass:\\s+(\\w+)">>,
                    [{capture, [1], binary}, global]
                )
            of
                {match, Matches} -> [C || [C] <- Matches];
                nomatch -> []
            end;
        {error, _} ->
            []
    end.

-doc """
Read the `[package] name` field from `<ProjectRoot>/beamtalk.toml`.

Returns the package name binary, or `undefined` if the manifest is missing or
has no package name (TOML uses double-quoted strings; the name must be a bare
`[a-z][a-z0-9_]*` identifier, matching `beamtalk_workspace_meta`'s detection).
""".
-spec read_package_name(string()) -> binary() | undefined.
read_package_name(ProjectRoot) ->
    ManifestPath = filename:join(ProjectRoot, "beamtalk.toml"),
    case file:read_file(ManifestPath) of
        {ok, Content} ->
            case re:run(Content, <<"\\[package\\]">>, [{capture, none}]) of
                match ->
                    case
                        re:run(
                            Content,
                            <<"name\\s*=\\s*\"([a-z][a-z0-9_]*)\"">>,
                            [{capture, [1], binary}]
                        )
                    of
                        {match, [Name]} -> Name;
                        nomatch -> undefined
                    end;
                nomatch ->
                    undefined
            end;
        {error, _} ->
            undefined
    end.

-doc """
Render the `beamtalk_classes.hrl` contents from a class→module index.

Mirrors the CLI's `generate_class_header/2` (build.rs): one
`-define(BT_CLASS_MODULE_<Class>, '<module>').` per class, wrapped in an include
guard, with entries sorted for deterministic output.

Entries whose class or module name is not a safe Erlang identifier/atom segment
are skipped (and logged): injecting an arbitrary name raw into a `-define` macro
identifier or a quoted atom could otherwise emit a `.hrl` that fails to compile
and would break every native module that includes it. Generated class names
(identifiers) and module atoms (`bt@pkg@mod`) always pass; this only guards
against pathological inputs.
""".
-spec render_class_header(#{binary() => binary()}) -> iolist().
render_class_header(Index) ->
    Entries = lists:sort(maps:to_list(Index)),
    Defines = lists:filtermap(
        fun({ClassName, ModuleName}) ->
            case safe_macro_class_name(ClassName) andalso safe_module_atom(ModuleName) of
                true ->
                    {true, [
                        "-define(BT_CLASS_MODULE_",
                        ClassName,
                        ", '",
                        ModuleName,
                        "').\n"
                    ]};
                false ->
                    ?LOG_WARNING(
                        "test-load: skipping unsafe class header entry ~p => ~p",
                        [ClassName, ModuleName],
                        #{domain => [beamtalk, runtime]}
                    ),
                    false
            end
        end,
        Entries
    ),
    [
        "%% Copyright 2026 James Casey\n",
        "%% SPDX-License-Identifier: Apache-2.0\n\n",
        "%% Generated by the Beamtalk workspace test-load - do not edit.\n",
        "%% BT-1730/BT-2653: Maps Beamtalk class names to compiled BEAM module atoms.\n\n",
        "-ifndef(BEAMTALK_CLASSES_HRL).\n",
        "-define(BEAMTALK_CLASSES_HRL, true).\n\n",
        Defines,
        "\n-endif. %% BEAMTALK_CLASSES_HRL\n"
    ].

%% A class name is safe to splice into a `BT_CLASS_MODULE_<Class>` macro
%% identifier when it is a non-empty run of [A-Za-z0-9_] (Erlang macro-name
%% grammar). Generated Beamtalk class names are identifiers, so this rejects
%% only pathological inputs.
-spec safe_macro_class_name(binary()) -> boolean().
safe_macro_class_name(<<>>) ->
    false;
safe_macro_class_name(Name) when is_binary(Name) ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
                (C >= $A andalso C =< $Z) orelse
                (C >= $0 andalso C =< $9) orelse
                C =:= $_
        end,
        binary_to_list(Name)
    );
safe_macro_class_name(_) ->
    false.

%% A module name is safe to splice into a single-quoted atom when it contains
%% neither a single quote nor a backslash (which would break out of the quoted
%% atom). Generated module atoms are `bt@pkg@mod`, which always pass.
-spec safe_module_atom(binary()) -> boolean().
safe_module_atom(<<>>) ->
    false;
safe_module_atom(Name) when is_binary(Name) ->
    binary:match(Name, [<<"'">>, <<"\\">>]) =:= nomatch;
safe_module_atom(_) ->
    false.

-doc """
Extract the declared class name and superclass from a .bt source file.
Returns {ClassName, SuperClass} as binaries, or {undefined, undefined}.
""".
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

-doc """
Sort .bt files in topological dependency order (superclass before subclass).
Files whose superclass is not in the project set come first.
""".
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
    lists:reverse(Acc, [maps:get(path, I) || I <- Pending]);
topo_sort_loop([#{path := Path, class := Class} | Ready], Pending, Acc) ->
    {NowReady, StillPending} = lists:partition(
        fun(#{super := Super}) -> Super =:= Class end,
        Pending
    ),
    topo_sort_loop(Ready ++ NowReady, StillPending, [Path | Acc]).

-doc """
Load files one by one, accumulating class maps and per-file error maps.
Accumulates in reverse to avoid quadratic ++ and reverses at the end.
Per-file errors are returned as structured maps with path, kind, and message
so callers can handle partial failures programmatically.

BT-1608: Class indexes are rebuilt after each successful file load so that
later files in the batch can reference classes loaded earlier (e.g. test
files referencing fixture classes). The original BT-1543 optimisation of
building indexes once caused "Undefined function" errors when test files
were compiled before their fixture dependencies were visible in the index.
""".
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

-doc """
Load files without a session (BT-1723).
Uses beamtalk_repl_loader:reload_class_file/1 for stateless compilation.
Called by sync_project when no SessionPid is available (e.g., from
the Workspace sync primitive).
""".
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

-doc """
Build structured error maps for a per-file load failure.
For compile errors with a diagnostic list, returns one error map per diagnostic
with line numbers preserved. Other errors return a single-element list.
""".
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

-doc "Convert a single compiler diagnostic map to a structured error map.".
-spec diagnostic_to_error_map(binary(), term()) -> map().
diagnostic_to_error_map(PathBin, D) when is_map(D) ->
    Msg = maps:get(message, D, <<"Unknown error">>),
    ErrMap0 = #{
        <<"path">> => PathBin,
        <<"kind">> => <<"compile_error">>,
        <<"message">> => Msg
    },
    ErrMap1 =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(line, D) of
            {ok, Line} when is_integer(Line) -> ErrMap0#{<<"line">> => Line};
            _ -> ErrMap0
        end,
    % elp:fixme W0032 maps:find with complex branch logic
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

-doc """
Collect collision warnings for the loaded classes after a file load.
BT-737: Drains warnings from the ETS table keyed by class name and
formats them as human-readable binary strings for the protocol response.
""".
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

-doc """
Extract the package segment from a bt@{pkg}@{class} module name.
Returns the package name binary or undefined.
Only 3-part names (bt@{pkg}@{class}) have a package; 2-part names (bt@{class})
are stdlib/unqualified and return undefined.
""".
-spec extract_package_from_module(atom()) -> binary() | undefined.
extract_package_from_module(ModuleName) when is_atom(ModuleName) ->
    ModStr = atom_to_list(ModuleName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg, _Class | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        _ ->
            undefined
    end.

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

-doc """
Build a map from BEAM module atom to Beamtalk class name binary.
Queries all registered class processes in a single pass.
""".
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

-doc """
Extract native module references from a .bt source file.
Returns a deduplicated list of Erlang module name binaries referenced via:
  - `native: module_name` annotation in the class header
  - `(Erlang module_name)` FFI calls in method bodies
""".
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

-doc """
Find the project root by walking up from a file path to find beamtalk.toml.
Returns the directory containing beamtalk.toml, or undefined if not found.
""".
-spec find_project_root(string()) -> string() | undefined.
find_project_root(Path) ->
    Dir = filename:dirname(filename:absname(Path)),
    find_project_root_walk(Dir).

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

-doc """
Check native module references from a .bt file and recompile stale .erl files.
For each native module referenced, checks if native/{module}.erl exists in the
project and if it's newer than its loaded .beam. If so, recompiles via compile:file/2.
Returns {ok, RecompiledCount} or {error, Errors}.
""".
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

-doc """
Check if a native .erl file is newer than its loaded .beam counterpart.
Returns true if:
  - The module is not loaded at all (needs compilation)
  - The .erl file's mtime is newer than the .beam file's mtime
""".
-spec is_native_erl_stale(string(), binary()) -> boolean().
is_native_erl_stale(ErlFile, ModBin) ->
    % elp:fixme W0023 intentional atom creation
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

-doc """
Record the .erl file's mtime when a native module is compiled in-memory.
Used by is_native_erl_stale/2 to detect changes since last compilation.
""".
-spec set_native_compile_mtime(atom(), calendar:datetime()) -> ok.
set_native_compile_mtime(ModAtom, Mtime) ->
    persistent_term:put({beamtalk_native_mtime, ModAtom}, Mtime),
    ok.

-doc """
Retrieve the .erl file's mtime that was recorded at compilation time.
Returns epoch if no mtime was recorded (triggers recompilation).
""".
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

-doc """
Classify files into changed, unchanged, and deleted lists.

A file is "changed" if:
  - It is new (not in PreviousMtimes)
  - Its current mtime differs from the stored mtime

A file is "unchanged" if its mtime matches the stored value.

"Deleted" files are those present in PreviousMtimes but not in
the current file list.
""".
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

-doc """
Filter previously-tracked file mtimes to only those under the given project root.

BT-2089: Multiple `load-project` calls against the same workspace should
accumulate, not evict. The workspace meta table accumulates mtimes across
every project loaded into the workspace, so unfiltered "deleted file"
detection treats files from sibling projects as deleted and unloads
their classes. Scoping by project root keeps each sync responsible only
for its own files.

The `ProjectRoot` is expected to be an absolute path. Files under it are
matched by string prefix on the directory boundary; the path
`/p/foo/src/a.bt` matches root `/p/foo` but `/p/foobar/src/a.bt` does not.
""".
-spec filter_mtimes_under_project(
    #{string() => calendar:datetime()}, string()
) -> #{string() => calendar:datetime()}.
filter_mtimes_under_project(Mtimes, ProjectRoot) ->
    %% Normalise the project root so the prefix check is unambiguous.
    %% Trim any trailing separator to avoid double-slash artefacts.
    Root = string:trim(ProjectRoot, trailing, "/"),
    Prefix = Root ++ "/",
    maps:filter(
        fun
            (Path, _Mtime) when is_list(Path) ->
                Path =:= Root orelse lists:prefix(Prefix, Path);
            (_NonString, _Mtime) ->
                false
        end,
        Mtimes
    ).

-doc """
Get the mtime of a file.
Returns {{0,0,0},{0,0,0}} if the file doesn't exist or can't be read.
""".
-spec get_file_mtime(string()) -> calendar:datetime().
get_file_mtime(Path) ->
    case filelib:last_modified(Path) of
        0 -> {{0, 0, 0}, {0, 0, 0}};
        Mtime -> Mtime
    end.

-doc """
Snapshot mtimes for a list of files before loading.
Returns a list of {Path, Mtime} tuples.
""".
-spec snapshot_file_mtimes([string()]) -> [{string(), calendar:datetime()}].
snapshot_file_mtimes(Files) ->
    [{Path, get_file_mtime(Path)} || Path <- Files].

-doc "Record file mtimes from a pre-load snapshot.".
-spec record_file_mtimes_from_snapshot([{string(), calendar:datetime()}]) -> ok.
record_file_mtimes_from_snapshot(Snapshot) ->
    lists:foreach(
        fun({Path, Mtime}) ->
            beamtalk_workspace_meta:set_file_mtime(Path, Mtime)
        end,
        Snapshot
    ).

-doc """
Handle files that were previously loaded but have been deleted.
Unregisters their classes from the class registry and unloads their BEAM modules.
Returns the count of deleted files processed.
""".
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

-doc "Find and unload all modules that were loaded from a given source path.".
-spec unload_modules_for_path(string(), pid() | undefined, #{atom() => binary()}, [
    {atom(), string() | undefined}
]) -> ok.
unload_modules_for_path(Path, SessionPid, ModToClass, LoadedModules) ->
    lists:foreach(
        fun({ModuleName, SourcePath}) ->
            case SourcePath =:= Path of
                true ->
                    %% Try to find class name for this module and remove it.
                    % elp:fixme W0032 maps:find with complex branch logic
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

-doc """
Build the incremental summary message.
Format: "Reloaded 2 of 7 files (5 unchanged)" or "Reloaded 2 of 7 files (3 unchanged, 2 deleted)"
""".
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
