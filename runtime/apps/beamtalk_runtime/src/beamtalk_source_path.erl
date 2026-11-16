%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_source_path).

-moduledoc """
Resolves the source path a compiled module embeds (BT-4940).

The compiler embeds a path relative to the package root (the directory
holding `beamtalk.toml`), with `/` separators, so the same project built in
two directories gives byte-identical `.beam` files. That string is exactly
what a stack frame should show. Code that needs the file on disk (reload,
flush, test discovery, the language service) calls `resolve/2` to turn it
back into an absolute path at run time.

The module's own root is decided first, and when it is known it is the only
root tried (a file missing under it is reported there, never silently taken
from another project):

1. the root named by a `.beamtalk-source-root` marker beside the module's
   `.beam` (written for a path dependency, whose beams live under the
   consumer's `_build/deps/` and outside any package);
2. else the package root above the module's `.beam` (`code:which/1`) — a built
   project, or a git dependency checked out with its manifest.

Only when the module has no root of its own (compiled in memory by the REPL or
`load_project`, or a release layout) are these tried, first existing file wins:

3. the workspace's project root (`workspace_project_root` application env);
4. the package root above the current directory.

An already-absolute path is returned as is (modules built before BT-4940).
When nothing exists on disk the path is joined to the first root found; with no
root at all it is returned unchanged, still relative. Callers that open the file
treat that as "not found".
""".

-export([find_package_root/1, resolve/2, is_absolute/1]).

-doc """
Walk up from `Path` (a file or directory) to the first directory holding a
`beamtalk.toml`; `undefined` when there is none.
""".
-spec find_package_root(file:filename_all()) -> file:filename_all() | undefined.
find_package_root(Path) ->
    walk(filename:dirname(filename:absname(Path))).

-doc "True for an absolute path in either Unix or Windows spelling.".
-spec is_absolute(file:filename_all()) -> boolean().
is_absolute(Path) ->
    filename:pathtype(Path) =/= relative.

-doc """
Resolve the embedded `Path` of `Module` to an absolute path. See the module
doc for the search order.
""".
-spec resolve(module() | undefined, file:filename_all()) -> file:filename_all().
resolve(Module, Path) ->
    case is_absolute(Path) of
        true ->
            Path;
        false ->
            Roots = candidate_roots(Module),
            case [J || R <- Roots, J <- [filename:join(R, Path)], filelib:is_file(J)] of
                [Found | _] -> Found;
                [] when Roots =/= [] -> filename:join(hd(Roots), Path);
                [] -> Path
            end
    end.

-spec candidate_roots(module() | undefined) -> [file:filename_all()].
candidate_roots(Module) ->
    case beam_root(Module) of
        undefined ->
            dedupe([R || R <- [workspace_root(), cwd_root()], R =/= undefined]);
        Root ->
            [Root]
    end.

dedupe(L) -> dedupe(L, []).
dedupe([], Acc) ->
    lists:reverse(Acc);
dedupe([H | T], Acc) ->
    case lists:member(H, Acc) of
        true -> dedupe(T, Acc);
        false -> dedupe(T, [H | Acc])
    end.

-spec beam_root(module() | undefined) -> file:filename_all() | undefined.
beam_root(undefined) ->
    undefined;
beam_root(Module) ->
    case code:which(Module) of
        Beam when is_list(Beam), Beam =/= [] ->
            case read_marker(filename:dirname(Beam)) of
                undefined -> find_package_root(Beam);
                Root -> Root
            end;
        _ ->
            undefined
    end.

-spec read_marker(file:filename_all()) -> file:filename_all() | undefined.
read_marker(EbinDir) ->
    case file:read_file(filename:join(EbinDir, ".beamtalk-source-root")) of
        {ok, Bin} ->
            case string:trim(unicode:characters_to_list(Bin)) of
                [] -> undefined;
                Root -> Root
            end;
        {error, _} ->
            undefined
    end.

-spec workspace_root() -> file:filename_all() | undefined.
workspace_root() ->
    case application:get_env(beamtalk_runtime, workspace_project_root) of
        {ok, Root} when is_binary(Root) -> unicode:characters_to_list(Root);
        {ok, Root} when is_list(Root) -> Root;
        _ -> undefined
    end.

-spec cwd_root() -> file:filename_all() | undefined.
cwd_root() ->
    case file:get_cwd() of
        {ok, Cwd} -> walk(Cwd);
        _ -> undefined
    end.

-spec walk(file:filename_all()) -> file:filename_all() | undefined.
walk(Dir) ->
    case filelib:is_file(filename:join(Dir, "beamtalk.toml")) of
        true ->
            Dir;
        false ->
            Parent = filename:dirname(Dir),
            case Parent =:= Dir of
                true -> undefined;
                false -> walk(Parent)
            end
    end.
