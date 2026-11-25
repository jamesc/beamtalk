%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_project_files).

%%% **DDD Context:** Workspace Context

-moduledoc """
File-level helpers for load-project: finding `.bt`/`.erl` files under a project, scanning a `.bt` file's class declarations and native references with the compiler port, and reading file mtimes.

Split out of `beamtalk_workspace_project_sync` (BT-5048) as the leaf below `beamtalk_workspace_native_build` and `beamtalk_workspace_project_sync`.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    find_bt_files/1,
    find_erl_files/2,
    scan_bt_file/1,
    extract_bt_class_infos/1,
    get_file_mtime/1
]).

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
native/test/ into _build/dev/native_test/ebin/.

Returns an empty list if the directory does not exist.
""".
-spec find_erl_files(string(), boolean()) -> [string()].
find_erl_files(Dir, IncludeTests) ->
    case filelib:is_dir(Dir) of
        false ->
            [];
        true ->
            AllErl0 = filelib:fold_files(Dir, ".*\\.erl$", true, fun(F, Acc) -> [F | Acc] end, []),
            %% BT-5017: `save-native-source` stages its compile in a
            %% `.bt-staging.*` directory under native/; a crash can leave one
            %% behind and it is never a project module.
            AllErl = [F || F <- AllErl0, not in_staging_dir(Dir, F)],
            case IncludeTests of
                true ->
                    AllErl;
                false ->
                    TestPrefix = filename:join(Dir, "test") ++ "/",
                    [F || F <- AllErl, not lists:prefix(TestPrefix, F)]
            end
    end.

%% True iff `File` lies under a `.bt-staging.*` directory below `Dir`
%% (`beamtalk_workspace_file:staging_dir_name/1`'s naming).
-spec in_staging_dir(string(), string()) -> boolean().
in_staging_dir(Dir, File) ->
    case beamtalk_workspace_file:relative_parts(Dir, File) of
        {ok, Parts} ->
            lists:any(fun beamtalk_workspace_file:is_staging_dir_name/1, lists:droplast(Parts));
        false ->
            false
    end.

-doc """
Parse a .bt source file with the compiler port's real grammar and report its
class declarations and native-module references (BT-4861).

Replaces whole-file regexes that matched inside `///` doc comments and string
literals and saw only one class per file. Best-effort: an unreadable file or a
compiler-port failure is logged (the latter) and yields no information, so
load-project falls back to original file order for that file rather than
failing.
""".
-spec scan_bt_file(string()) ->
    {ok, #{classes := [map()], native_refs := [binary()]}} | error.
scan_bt_file(Path) ->
    case file:read_file(Path) of
        {ok, Source} ->
            case beamtalk_compiler:scan_source_dependencies(Source) of
                {ok, _} = Ok ->
                    Ok;
                {error, Reason, Message} ->
                    ?LOG_WARNING(
                        "scan-source-dependencies: could not scan ~s (~p): ~s",
                        [Path, Reason, Message],
                        #{domain => [beamtalk, runtime]}
                    ),
                    error
            end;
        {error, _} ->
            error
    end.

-doc """
Every class declared in a .bt source file, in source order, as
`{ClassName, SuperClass | undefined}` binaries. `SuperClass` is `undefined`
for a root class or a package-qualified superclass. Returns `[]` for a file
with no class, an unreadable file, or when the compiler port is unavailable.
""".
-spec extract_bt_class_infos(string()) -> [{binary(), binary() | undefined}].
extract_bt_class_infos(Path) ->
    case scan_bt_file(Path) of
        {ok, #{classes := Classes}} ->
            [{maps:get(name, C), maps:get(superclass, C, undefined)} || C <- Classes];
        error ->
            []
    end.

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
