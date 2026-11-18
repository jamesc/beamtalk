%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_native_build).

%%% **DDD Context:** Workspace Context

-moduledoc """
Native `.erl` compilation for a project loaded into the workspace: compiling `native/` modules, generating the `beamtalk_classes.hrl` class-to-module header, and the demand-driven recompile of stale native dependencies when a single `.bt` file is reloaded.

Split out of `beamtalk_workspace_project_sync` (BT-5048). Sits above `beamtalk_workspace_project_files` and below `beamtalk_workspace_project_sync`.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    compile_native_erl_files/2,
    regenerate_native_class_header/2,
    build_source_class_module_index/2,
    find_project_root/1,
    maybe_recompile_native_deps/2,
    erase_native_compile_mtime/1
]).

%% Exported for white-box tests only.
-ifdef(TEST).
-export([
    native_generated_include_dir/1,
    relative_bt_path/2,
    render_class_header/1,
    extract_native_refs/1
]).
-endif.

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
    %% Native modules may `-include("beamtalk_classes.hrl")`, the
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
            %% Snapshot the .erl file's mtime before compiling so
            %% is_native_erl_stale/2 can compare against it later.
            ErlMtimeSnapshot = beamtalk_workspace_project_files:get_file_mtime(ErlPath),
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
                                <<"message">> => beamtalk_error:chars_to_binary(
                                    io_lib:format("Failed to load compiled module: ~tp", [LoadErr])
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
                                <<"message">> => beamtalk_error:chars_to_binary(
                                    io_lib:format("Failed to load compiled module: ~tp", [LoadErr])
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
                            <<"message">> => beamtalk_error:chars_to_binary(
                                io_lib:format("~tp", [Other])
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

Native `.erl` modules `-include("beamtalk_classes.hrl")` to map class
names to compiled BEAM module atoms via `?BT_CLASS_MODULE_<Class>` macros. The
`beamtalk test` CLI regenerates this header on every build (build.rs
`generate_class_header/2`); the workspace incremental native build must do the
same so it never compiles against a stale header — the root cause of the
`spec for undefined function …` cascade on the LiveView test-load path.

The index is the union of two sources:

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
(`beamtalk_repl_loader_support:to_snake_case/1` mirrors the Rust
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
-spec regenerate_native_class_header(string(), binary() | undefined) -> boolean().
regenerate_native_class_header(ProjectRoot, PackageName) ->
    %% Union the source-AST-derived index (complete on a cold load)
    %% with the live registry (canonical module atoms on the warm path). The
    %% registry wins on conflict via the second arg to maps:merge/2.
    SourceIndex = build_source_class_module_index(ProjectRoot, PackageName),
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
Build a class→module index from the project's `src/**/*.bt` source.

This is the source-AST-derived index that gives the cold-load path full parity
with the CLI: on a clean load the project's own classes are not yet registered,
so the live registry alone (`build_class_module_index/0`) would omit them and a
native module referencing `?BT_CLASS_MODULE_Foo` for a same-package class would
fail to compile. By scanning source we always include every class defined in the
package being loaded.

For each `src/*.bt` file, `beamtalk_compiler:build_class_module_index_in_source/3`
parses the file with the real grammar (every declared class, not just the first
`Super subclass: Class` a regex happens to match) and computes the
package-qualified module atom `bt@<pkg>@<relative@path>` via the shared
`relative_module_segments` leaf the CLI's own `build_class_module_index`
(`build.rs`) uses — so this index and a `beamtalk build` of the same project can
never diverge on either class extraction or module-name casing (see
AGENTS.md's "No duplicate implementations" rule). The package name is read from
`PackageName` argument (never re-parsed from `beamtalk.toml` here; see
`resolve_package_name/2`).

Returns an empty map (no entries, never a crash) when the package name cannot be
determined or `src/` is absent — the caller then falls back to the live registry
alone, exactly the previous behaviour. A single file that fails to index (an
`invalid_path_segment` from a filename outside `[A-Za-z0-9_]`,
`reserved_test_directory` for a `src/test/` file, or a transport failure) is skipped with a logged warning rather than failing the whole index —
the same best-effort contract the previous implementation had.
""".
-spec build_source_class_module_index(string(), binary() | undefined) ->
    #{binary() => binary()}.
build_source_class_module_index(ProjectRoot, PackageName0) ->
    case PackageName0 of
        undefined ->
            #{};
        PackageName ->
            SrcDir = filename:join(ProjectRoot, "src"),
            BtFiles = beamtalk_workspace_project_files:find_bt_files(SrcDir),
            lists:foldl(
                fun(Path, Acc) -> index_bt_file(Path, SrcDir, PackageName, Acc) end,
                #{},
                BtFiles
            )
    end.

%% Index one `src/**/*.bt` file's classes into `Acc` via the compiler port
%% Best-effort: an unreadable file, an indexing failure, or a
%% compiler-port transport failure is logged and skipped rather than failing
%% the whole cold-load index.
-spec index_bt_file(string(), string(), binary(), #{binary() => binary()}) ->
    #{binary() => binary()}.
index_bt_file(Path, SrcDir, PackageName, Acc) ->
    case file:read_file(Path) of
        {error, Reason} ->
            ?LOG_WARNING(
                "build-source-class-module-index: could not read ~s: ~p",
                [Path, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            Acc;
        {ok, Source} ->
            RelativePath = relative_bt_path(Path, SrcDir),
            case
                beamtalk_compiler:build_class_module_index_in_source(
                    Source, RelativePath, PackageName
                )
            of
                {ok, ModuleName, Classes} ->
                    lists:foldl(
                        fun(ClassName, InnerAcc) -> InnerAcc#{ClassName => ModuleName} end,
                        Acc,
                        Classes
                    );
                {error, Reason, Message} ->
                    ?LOG_WARNING(
                        "build-source-class-module-index: skipping ~s (~p): ~s",
                        [Path, Reason, Message],
                        #{domain => [beamtalk, runtime]}
                    ),
                    Acc
            end
    end.

-doc """
`Path`'s file-system path relative to `SrcDir`, `/`-joined with the file
extension left intact (e.g. `src/util/http_response.bt` under `src/` →
`<<"util/http_response.bt">>`) — the `relative_path` the compiler port's
`build_class_module_index_in_source` uses to derive the module-name segments.
Only locates the relative path; the module-name casing rule itself
lives entirely on the Rust side (`relative_module_segments`), never
re-derived here.
""".
-spec relative_bt_path(string(), string()) -> binary().
relative_bt_path(Path, SrcDir) ->
    AbsPath = filename:absname(Path),
    AbsSrc = filename:absname(SrcDir),
    RelSegments =
        case beamtalk_workspace_file:relative_parts(AbsSrc, AbsPath) of
            {ok, Parts} ->
                Parts;
            false ->
                %% Defensive: fall back to the bare basename if the file is
                %% somehow not under src/ (find_bt_files only returns files
                %% under it).
                [filename:basename(Path)]
        end,
    iolist_to_binary(lists:join("/", RelSegments)).

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
        "%% Maps Beamtalk class names to compiled BEAM module atoms.\n\n",
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

%%% ===================================================================
%%% Demand-driven native .erl recompilation on single-file reload
%%% ===================================================================

-doc """
Extract native module references from a .bt source file.
Returns a sorted, deduplicated list of Erlang module name binaries referenced via:
  - `native: module_name` annotation in the class header
  - `(Erlang module_name)` FFI calls in method bodies
Taken from the parsed AST, so occurrences inside comments and string literals
are not references (BT-4861).
""".
-spec extract_native_refs(string()) -> [binary()].
extract_native_refs(Path) ->
    case beamtalk_workspace_project_files:scan_bt_file(Path) of
        {ok, #{native_refs := Refs}} -> lists:usort(Refs);
        error -> []
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
            ErlMtime = beamtalk_workspace_project_files:get_file_mtime(ErlFile),
            BeamMtime =
                case is_list(BeamPath) of
                    true ->
                        %% When loaded via code:load_binary/3 the stored
                        %% path is the .erl source, not a .beam file. Comparing
                        %% its mtime against itself always yields "not stale".
                        %% Use the mtime snapshot taken at compile time instead.
                        case filename:extension(BeamPath) of
                            ".erl" ->
                                get_native_compile_mtime(ModAtom);
                            _ ->
                                beamtalk_workspace_project_files:get_file_mtime(BeamPath)
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

-doc """
Erase the recorded compile-time mtime for `ModAtom`, if any.

Called when a native module's .erl source is deleted (the module is unloaded
from the VM and its mtime tracking should go with it) so the
`{beamtalk_native_mtime, ModAtom}` `persistent_term` entry does not linger
forever — a missing entry already degrades safely to `get_native_compile_mtime/1`
returning the epoch (forced recompile), so this is purely hygiene, not a
correctness fix. A no-op if nothing was recorded for `ModAtom`.
""".
-spec erase_native_compile_mtime(atom()) -> ok.
erase_native_compile_mtime(ModAtom) ->
    _ = persistent_term:erase({beamtalk_native_mtime, ModAtom}),
    ok.
