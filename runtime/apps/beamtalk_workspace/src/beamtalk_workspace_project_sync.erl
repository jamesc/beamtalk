%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_project_sync).

%%% **DDD Context:** Workspace Context

-moduledoc """
Incremental project sync and demand-driven native recompilation for a running
workspace: scan a project for `.bt` / `.erl` files, classify them as
changed/unchanged/deleted by mtime, compile changed files in superclass-first
order, compile native `.erl` files (and regenerate their class headers), and
validate the on-disk build stamp (ADR 0098) so a toolchain change forces a full
recompile.


This module holds the build/sync logic only. The REPL protocol op handlers
(`load-project`, `load-source`, `save-section`, `save-native-source`) stay in
`beamtalk_repl_ops_load`, which calls into here; `sync_project/2` is also
the entry point for `Workspace` primitives.


Extracted from beamtalk_repl_ops_load (BT-4706).

""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-include_lib("kernel/include/logger.hrl").

%% Shared sync logic callable from both protocol handler and primitives.
-export([sync_project/2]).

%% Used by the native-source authoring path and the `load-source` op in
%% beamtalk_repl_ops_load (previously TEST-only exports of the same module).
-export([
    collect_load_warnings/1,
    resolve_package_name/2
]).

%% Export internals for white-box testing of load-project helpers.
-ifdef(TEST).

-export([
    sort_bt_files_by_deps/1,
    structured_file_errors/2,
    format_collision_warning/3,
    classify_files_by_change/2,
    filter_mtimes_under_project/2,
    %% White-box coverage for the pure load-result-shape helpers
    %% behind sync_project/2 and handle_term/4's `load-source` clause.
    build_incremental_summary/4
]).

-endif.

%%% ============================================================================
%%% sync_project/2 — shared incremental sync logic
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
  - `package_name` (binary()) — the project's package name, as supplied by the
    caller (the `load-project` request's optional `package_name` field). When
    absent it falls back to the `root_package` runtime fact if `Path` is the
    launcher's project root (see `resolve_package_name/2`).

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
    PackageName = resolve_package_name(AbsPath, maps:get(package_name, Options, undefined)),
    ManifestPath = filename:join(AbsPath, "beamtalk.toml"),
    case filelib:is_file(ManifestPath) of
        false ->
            {error,
                beamtalk_repl_errors:make(
                    file_not_found,
                    'Workspace',
                    iolist_to_binary(["No beamtalk.toml found in: ", AbsPath]),
                    <<"Provide a directory path containing beamtalk.toml">>
                )};
        true ->
            do_sync_project(AbsPath, IncludeTests, Force, SessionPid, PackageName)
    end.

-doc "Core sync logic, called after validating beamtalk.toml exists.".
-spec do_sync_project(string(), boolean(), boolean(), pid() | undefined, binary() | undefined) ->
    {ok, map()} | {error, #beamtalk_error{}}.
do_sync_project(AbsPath, IncludeTests, Force0, SessionPid, PackageName) ->
    %% ADR 0098 Phase 4: provenance gate, evaluated BEFORE any module is loaded so
    %% a failure leaves the code server in its pre-attach state (no partial loads
    %% to roll back, per ADR §4).
    %%
    %% Project scope: a stamp produced by a different toolchain forces a full
    %% in-memory recompile from source (generalizing the header-content
    %% force-rebuild). Dependency scope: the workspace recompiles project sources
    %% only, so a stale dep cannot self-heal here — per ADR §4 we FAIL the attach
    %% with a directed message rather than activating stale dep `.beam`, which
    %% would re-open the beamtalk-http mojibake/stale-doc failure mode.
    ProvenanceStale = beamtalk_workspace_build_provenance:project_provenance_stale(AbsPath),
    case beamtalk_workspace_build_provenance:collect_stale_dep_provenance(AbsPath) of
        [] ->
            do_sync_project_clean(
                AbsPath, IncludeTests, Force0 orelse ProvenanceStale, SessionPid, PackageName
            );
        StaleDeps ->
            beamtalk_workspace_build_provenance:fail_stale_dep_attach(StaleDeps)
    end.

-spec do_sync_project_clean(
    string(), boolean(), boolean(), pid() | undefined, binary() | undefined
) -> {ok, map()}.
do_sync_project_clean(AbsPath, IncludeTests, Force, SessionPid, PackageName) ->
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
    SrcFiles = beamtalk_workspace_project_files:find_bt_files(filename:join(AbsPath, "src")),
    TestFiles =
        case IncludeTests of
            true -> beamtalk_workspace_project_files:find_bt_files(filename:join(AbsPath, "test"));
            false -> []
        end,
    AllBtFiles = SrcFiles ++ TestFiles,
    NativeDir = filename:join(AbsPath, "native"),
    %% On the test-load path (include_tests=true), also discover
    %% native/test/ helper modules (e.g. a test server that a `.bt` test drives
    %% via `(Erlang <helper>) <msg>`). Under normal load-project they stay
    %% skipped — they are EUnit helpers, not runtime modules — mirroring how the
    %% `beamtalk test` CLI compiles native/test/ into _build/dev/native/ebin/.
    AllErlFiles = beamtalk_workspace_project_files:find_erl_files(NativeDir, IncludeTests),
    AllFiles = AllErlFiles ++ AllBtFiles,
    PreviousMtimes =
        case beamtalk_workspace_meta:get_file_mtimes() of
            {ok, Mtimes} -> Mtimes;
            {error, _} -> #{}
        end,
    %% Scope previous-mtime tracking to the project being synced.
    %% The workspace meta table accumulates mtimes for every project ever
    %% loaded into this workspace, so unfiltered classification would treat
    %% every other project's files as "deleted" and unload their classes.
    %% Only files whose path lies under AbsPath belong to this project sync.
    ProjectMtimes = filter_mtimes_under_project(PreviousMtimes, AbsPath),
    %% When include_tests=false, also drop previously-tracked test
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
            %% Erase the compile-time mtime persistent_term entry along
            %% with the module — otherwise it outlives the module forever (one
            %% entry per ever-deleted native module, each `put` also scanning
            %% every scheduler for global GC).
            beamtalk_workspace_native_build:erase_native_compile_mtime(ModName),
            beamtalk_workspace_meta:remove_file_mtime(P)
        end,
        DeletedErl
    ),
    DeletedCount = DeletedBtCount + length(DeletedErl),
    %% Before compiling any native .erl, regenerate the
    %% beamtalk_classes.hrl header from the live class→module index so native
    %% modules that `-include("beamtalk_classes.hrl")` build against the current
    %% class set. A stale header (or a stale .beam compiled against one) causes a
    %% `spec for undefined function …` cascade that fails the test-load even when
    %% the package's `beamtalk test` CLI is green. The CLI regenerates this header
    %% on every build (build.rs generate_class_header/2); the workspace
    %% incremental path must do the same to stay in sync.
    HeaderChanged = beamtalk_workspace_native_build:regenerate_native_class_header(
        AbsPath, PackageName
    ),
    %% Decide which native .erl to (re)compile. The mtime check only
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
        beamtalk_workspace_native_build:compile_native_erl_files(NativeToCompile, AbsPath),
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
    {AllClasses, BtErrors, LoadWarnings} =
        case SessionPid of
            undefined ->
                {StatelessClasses, StatelessErrors} = load_files_stateless(SortedChanged),
                {StatelessClasses, StatelessErrors, []};
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
    %% BT-3662: `beamtalk build` is the only writer of the `.app` class list, so
    %% classes loaded here (or defined in the REPL) would stay invisible to
    %% `Package classes` until the next build. Refresh the loaded `.app`'s
    %% `classes` env from the live registry — the in-memory equivalent of
    %% regenerating it.
    ok = refresh_package_classes(PackageName),
    ClassNames =
        [
            case maps:get(name, C, "") of
                N when is_binary(N) -> N;
                N -> list_to_binary(N)
            end
         || C <- AllClasses
        ],
    %% Drain class collision warnings so cross-project class
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
        %% BT-5060: one-hop extension warnings come back with each file's load.
        warnings => CollisionWarnings ++ beamtalk_runtime_api:dedupe_warnings(LoadWarnings),
        summary => build_incremental_summary(
            ChangedCount, TotalFiles, UnchangedCount, DeletedCount
        ),
        changed_count => ChangedCount,
        unchanged_count => UnchangedCount,
        deleted_count => DeletedCount,
        total_files => TotalFiles
    }}.

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
        <<"message">> => beamtalk_error:chars_to_binary(
            io_lib:format("Failed to activate dependency module ~tp: ~tp", [Mod, Reason])
        )
    }.

-doc """
The package name to use for `ProjectRoot`'s source class index.

Resolution order (no `beamtalk.toml` parsing in Erlang; Rust owns the manifest):

  1. `Requested`, when the caller supplied one (the `load-project` request's
     `package_name` field, or the `package_name` sync option).
  2. The `root_package` runtime fact recorded by the launcher
     (`beamtalk_package:root_package_name/0`), when `ProjectRoot` is the
     launcher's project root (or the workspace does not know its project path).
  3. `undefined` - the source index is then empty and the class header relies on
     the live registry alone.
""".
-spec resolve_package_name(string(), binary() | undefined) -> binary() | undefined.
resolve_package_name(_ProjectRoot, Requested) when is_binary(Requested), Requested =/= <<>> ->
    Requested;
resolve_package_name(ProjectRoot, _Requested) ->
    case beamtalk_package:root_package_name() of
        undefined ->
            undefined;
        RootName ->
            case launcher_project_path() of
                undefined ->
                    RootName;
                LauncherRoot ->
                    case same_dir(LauncherRoot, ProjectRoot) of
                        true -> RootName;
                        false -> undefined
                    end
            end
    end.

-doc "Refresh the root package's loaded `.app` class list (BT-3662).".
-spec refresh_package_classes(binary() | undefined) -> ok.
refresh_package_classes(undefined) ->
    ok;
refresh_package_classes(PackageName) ->
    _ = beamtalk_package:refresh_app_classes(PackageName),
    ok.

-spec launcher_project_path() -> string() | undefined.
launcher_project_path() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := P}} when is_binary(P) -> binary_to_list(P);
        _ -> undefined
    end.

-spec same_dir(string(), string()) -> boolean().
same_dir(A, B) ->
    string:trim(filename:absname(A), trailing, "/") =:=
        string:trim(filename:absname(B), trailing, "/").

-doc """
Sort .bt files in topological dependency order (superclass before subclass).
A file declaring several classes is ordered after every other project file
that defines a superclass of any of its classes; a superclass declared in the
same file never counts as a dependency. Files whose superclasses are not in the
project set come first, and independent files keep their original order.
""".
-spec sort_bt_files_by_deps([string()]) -> [string()].
sort_bt_files_by_deps([]) ->
    [];
sort_bt_files_by_deps(Files) ->
    Infos = lists:map(
        fun(Path) ->
            Decls = beamtalk_workspace_project_files:extract_bt_class_infos(Path),
            Defined = [C || {C, _} <- Decls],
            Needs = lists:usort([
                S
             || {_, S} <- Decls, S =/= undefined, not lists:member(S, Defined)
            ]),
            #{path => Path, defined => Defined, needs => Needs}
        end,
        Files
    ),
    ProjectClasses = sets:from_list([C || #{defined := Defined} <- Infos, C <- Defined]),
    topo_sort_loop(Infos, ProjectClasses, sets:new(), []).

-spec topo_sort_loop([map()], sets:set(binary()), sets:set(binary()), [string()]) ->
    [string()].
topo_sort_loop([], _ProjectClasses, _Loaded, Acc) ->
    lists:reverse(Acc);
topo_sort_loop(Pending, ProjectClasses, Loaded, Acc) ->
    IsReady = fun(#{needs := Needs}) ->
        lists:all(
            fun(S) ->
                not sets:is_element(S, ProjectClasses) orelse sets:is_element(S, Loaded)
            end,
            Needs
        )
    end,
    case lists:partition(IsReady, Pending) of
        {[], _} ->
            %% Unresolved entries remain (likely a cycle or an unparseable class declaration).
            %% Warn and append them in original order so load proceeds best-effort.
            ?LOG_WARNING(
                "load-project: ~B file(s) have unresolved superclass dependencies "
                "(possible cycle or missing class declaration); loading in original order",
                [length(Pending)],
                #{domain => [beamtalk, runtime]}
            ),
            lists:reverse(Acc, [maps:get(path, I) || I <- Pending]);
        {Ready, StillPending} ->
            Loaded1 = lists:foldl(
                fun(#{defined := Defined}, L) ->
                    lists:foldl(fun sets:add_element/2, L, Defined)
                end,
                Loaded,
                Ready
            ),
            topo_sort_loop(
                StillPending,
                ProjectClasses,
                Loaded1,
                lists:reverse([maps:get(path, I) || I <- Ready], Acc)
            )
    end.

-doc """
Load files one by one, accumulating class maps and per-file error maps.
Accumulates in reverse to avoid quadratic ++ and reverses at the end.
Per-file errors are returned as structured maps with path, kind, and message
so callers can handle partial failures programmatically.

Class indexes are rebuilt after each successful file load so that
later files in the batch can reference classes loaded earlier (e.g. test
files referencing fixture classes). The original optimisation of
building indexes once caused "Undefined function" errors when test files
were compiled before their fixture dependencies were visible in the index.
""".
-spec load_files_sequential([string()], pid()) -> {[map()], [map()], [binary()]}.
load_files_sequential(Files, SessionPid) ->
    %% BT-4378: every compile of this batch carries the same `project_batch'
    %% token (merged into the compile options with the indexes), so the
    %% compiler port verifies the project scope once per load, not per file.
    Batch = new_project_batch(),
    Indexes0 = maps:merge(beamtalk_repl_compiler:build_class_indexes(), Batch),
    {RevClasses, RevErrors, RevWarnings, _} =
        lists:foldl(
            fun(Path, {ClassesAcc, ErrorsAcc, WarningsAcc, Indexes}) ->
                {Loaded, Warnings} =
                    beamtalk_repl_shell:load_file_with_warnings(SessionPid, Path, Indexes),
                NewWarningsAcc = lists:reverse(Warnings, WarningsAcc),
                case Loaded of
                    {ok, Classes} ->
                        NewIndexes = maps:merge(
                            beamtalk_repl_compiler:build_class_indexes(), Batch
                        ),
                        {lists:reverse(Classes, ClassesAcc), ErrorsAcc, NewWarningsAcc, NewIndexes};
                    {error, Reason} ->
                        ErrMaps = structured_file_errors(Path, Reason),
                        {ClassesAcc, lists:reverse(ErrMaps, ErrorsAcc), NewWarningsAcc, Indexes}
                end
            end,
            {[], [], [], Indexes0},
            Files
        ),
    {
        lists:reverse(RevClasses),
        lists:reverse(RevErrors),
        lists:reverse(RevWarnings)
    }.

-doc """
Load files without a session.
Uses beamtalk_repl_loader_reload:reload_class_file/3 for stateless compilation.
Called by sync_project when no SessionPid is available (e.g., from
the Workspace sync primitive).

Like `load_files_sequential/2', every compile of the load — each file's own
and any protocol-reload fan-out recompile it triggers — carries one
`project_batch' token (BT-4394), so the compiler port verifies the project
scope once per load rather than after every fan-out.
""".
-spec load_files_stateless([string()]) -> {[map()], [map()]}.
load_files_stateless(Files) ->
    Batch = new_project_batch(),
    {RevClasses, RevErrors} =
        lists:foldl(
            fun(Path, {ClassesAcc, ErrorsAcc}) ->
                case beamtalk_repl_loader_reload:reload_class_file(Path, undefined, Batch) of
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

%% A fresh `project_batch' compile option for one `load_project' (BT-4378):
%% the compiler port verifies a project's scope on the first compile tagged
%% with a given token and reuses it for the rest of that batch.
-spec new_project_batch() -> #{project_batch := binary()}.
new_project_batch() ->
    #{project_batch => integer_to_binary(erlang:unique_integer([positive]))}.

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
    %% BT-4332: #beamtalk_error{} messages/hints are Beamtalk Strings
    %% ({beamtalk_string, Bin}); route through normalize_diagnostic so the
    %% load-project wire encoder only ever sees binaries.
    D0 = #{message => Msg},
    D1 =
        case Hint of
            undefined -> D0;
            _ -> D0#{hint => Hint}
        end,
    Norm = beamtalk_repl_errors:normalize_diagnostic(D1),
    Base = #{
        <<"path">> => list_to_binary(Path),
        <<"kind">> => atom_to_binary(Kind, utf8),
        <<"message">> => maps:get(message, Norm)
    },
    [
        case maps:find(hint, Norm) of
            {ok, NormHint} -> Base#{<<"hint">> => NormHint};
            error -> Base
        end
    ].

-doc "Convert a single compiler diagnostic map to a structured error map.".
-spec diagnostic_to_error_map(binary(), term()) -> map().
diagnostic_to_error_map(PathBin, D) ->
    Norm = beamtalk_repl_errors:normalize_diagnostic(D),
    Base = #{
        <<"path">> => PathBin,
        <<"kind">> => <<"compile_error">>,
        <<"message">> => maps:get(message, Norm)
    },
    Base1 =
        case maps:find(line, Norm) of
            {ok, Line} -> Base#{<<"line">> => Line};
            error -> Base
        end,
    case maps:find(hint, Norm) of
        {ok, Hint} -> Base1#{<<"hint">> => Hint};
        error -> Base1
    end.

-doc """
Collect collision warnings for the loaded classes after a file load.
Drains warnings from the ETS table keyed by class name and
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
    OldPkg = beamtalk_module_name:package_of(OldModule),
    NewPkg = beamtalk_module_name:package_of(NewModule),
    %% When both modules come from packages, add a qualified-name hint.
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
%%% Incremental load-project helpers
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
                    CurrentMtime = beamtalk_workspace_project_files:get_file_mtime(Path),
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

Multiple `load-project` calls against the same workspace should
accumulate, not evict. The workspace meta table accumulates mtimes across
every project loaded into the workspace, so unfiltered "deleted file"
detection treats files from sibling projects as deleted and unloads
their classes. Scoping by project root keeps each sync responsible only
for its own files.

The `ProjectRoot` is expected to be an absolute path. Files under it are
matched component-wise (`beamtalk_workspace_file:path_inside/2`); the path
`/p/foo/src/a.bt` matches root `/p/foo` (or `/p/foo/`) but
`/p/foobar/src/a.bt` does not.
""".
-spec filter_mtimes_under_project(
    #{string() => calendar:datetime()}, string()
) -> #{string() => calendar:datetime()}.
filter_mtimes_under_project(Mtimes, ProjectRoot) ->
    maps:filter(
        fun
            (Path, _Mtime) when is_list(Path) ->
                beamtalk_workspace_file:path_inside(ProjectRoot, Path);
            (_NonString, _Mtime) ->
                false
        end,
        Mtimes
    ).

-doc """
Snapshot mtimes for a list of files before loading.
Returns a list of {Path, Mtime} tuples.
""".
-spec snapshot_file_mtimes([string()]) -> [{string(), calendar:datetime()}].
snapshot_file_mtimes(Files) ->
    [{Path, beamtalk_workspace_project_files:get_file_mtime(Path)} || Path <- Files].

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
                    Removed =
                        case maps:find(ModuleName, ModToClass) of
                            {ok, ClassNameBin} ->
                                case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
                                    {ok, ClassName} ->
                                        case
                                            beamtalk_runtime_api:remove_class_from_system(ClassName)
                                        of
                                            {ok, _} ->
                                                true;
                                            {error, #beamtalk_error{kind = class_not_found}} ->
                                                %% Already gone: nothing left to keep registered.
                                                true;
                                            {error, Reason} ->
                                                %% BT-4936: a refusal (e.g. the class is
                                                %% supervised) leaves the class loaded, so
                                                %% keep the module registered and tracked.
                                                ?LOG_WARNING(
                                                    "Class removal refused during sync; the class stays loaded until removed by hand",
                                                    #{
                                                        class => ClassName,
                                                        reason => Reason,
                                                        domain => [beamtalk, runtime]
                                                    }
                                                ),
                                                false
                                        end;
                                    {error, badarg} ->
                                        true
                                end;
                            error ->
                                true
                        end,
                    case Removed of
                        true ->
                            beamtalk_workspace_meta:unregister_module(ModuleName),
                            case SessionPid of
                                undefined -> ok;
                                _ -> beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName)
                            end;
                        false ->
                            ok
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
