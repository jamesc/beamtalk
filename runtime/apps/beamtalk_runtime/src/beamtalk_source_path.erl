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
   consumer's `_build/deps/` and outside any package). Line 1 is the root
   relative to the marker's directory, so a consumer + dependency tree moved
   together (BT-4957) still resolves; line 2 is the absolute root, used when
   the relative one holds no package (a dependency outside the moved tree);
2. else the package root above the module's `.beam` (`code:which/1`) — a built
   project, or a git dependency checked out with its manifest.

Only when the module has no root of its own (compiled in memory by the REPL or
`load_project`, or a release layout) are these tried, first existing file wins:

3. the workspace's project root (`workspace_project_root` application env);
4. the package root above the current directory.

A path that exists relative to the current directory is answered as embedded,
before any of these (a REPL `:load tests/x.bt`, as before BT-4940).

An already-absolute path is returned as is (modules built before BT-4940).
With a module root, the result is that root joined with the path (it may not
exist). Without one, a path that exists under no candidate is joined to the
workspace project root when there is one, else returned unchanged, still
relative; callers that open the file treat that as "not found".
""".

-export([find_package_root/1, resolve/2, resolve_strict/2, is_absolute/1]).

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
    case resolve_strict(Module, Path) of
        {ok, Resolved} -> Resolved;
        %% For display only; callers that open the file use `resolve_strict/2`.
        unresolvable -> Path
    end.

-doc """
Like `resolve/2`, but `unresolvable` when the module's root is recorded and
cannot be read as text (a non-UTF-8 dependency path, BT-4957). The path must
then not be handed to code that opens it: relative to the node's cwd it could
name another project's file.
""".
-spec resolve_strict(module() | undefined, file:filename_all()) ->
    {ok, file:filename_all()} | unresolvable.
resolve_strict(Module, Path) ->
    case is_absolute(Path) of
        true ->
            {ok, Path};
        false ->
            case beam_root(Module) of
                %% The module's own root decides: a file missing under it is
                %% reported there, never taken from another project.
                undefined -> {ok, resolve_without_root(Path)};
                %% A root is recorded but cannot be read as text. Never fall
                %% back to another project's root.
                unreadable -> unresolvable;
                Root -> {ok, filename:join(Root, Path)}
            end
    end.

%% A module with no root of its own (compiled in memory, or outside any
%% package). First existing file wins; a path that exists nowhere is returned
%% unchanged, still relative, so callers see "not found" and a cwd-relative
%% path keeps meaning what it always did.
-spec resolve_without_root(file:filename_all()) -> file:filename_all().
resolve_without_root(Path) ->
    case filelib:is_file(Path) of
        %% Exists relative to the current directory: answer it as embedded,
        %% exactly as before BT-4940 (a REPL `:load tests/x.bt`).
        true ->
            Path;
        false ->
            Roots = dedupe([R || R <- [workspace_root(), cwd_root()], R =/= undefined]),
            case [J || R <- Roots, J <- [filename:join(R, Path)], filelib:is_file(J)] of
                [Found | _] ->
                    Found;
                [] ->
                    %% Nothing on disk. In a workspace the embedded path of an
                    %% in-memory module is relative to its project, so a file
                    %% deleted since load is still reported under that project
                    %% (not mistaken for a dependency's). No workspace: as is.
                    case workspace_root() of
                        undefined -> Path;
                        Ws -> filename:join(Ws, Path)
                    end
            end
    end.

dedupe(L) -> dedupe(L, []).
dedupe([], Acc) ->
    lists:reverse(Acc);
dedupe([H | T], Acc) ->
    case lists:member(H, Acc) of
        true -> dedupe(T, Acc);
        false -> dedupe(T, [H | Acc])
    end.

-spec beam_root(module() | undefined) -> file:filename_all() | unreadable | undefined.
beam_root(undefined) ->
    undefined;
beam_root(Module) ->
    case code:which(Module) of
        Beam when is_list(Beam), Beam =/= [] ->
            case read_marker(filename:dirname(Beam)) of
                absent -> find_package_root(Beam);
                Found -> Found
            end;
        _ ->
            undefined
    end.

%% `absent`: no marker. `unreadable`: a marker names a root that cannot be
%% decoded as text, so the package walk (which would find the consumer's root
%% for a path dependency) must not run. Otherwise the root as a string: line 1
%% (relative to `EbinDir`) when it holds a package, else line 2 (absolute),
%% else whichever is present.
-spec read_marker(file:filename_all()) -> file:filename_all() | unreadable | absent.
read_marker(EbinDir) ->
    case file:read_file(filename:join(EbinDir, ".beamtalk-source-root")) of
        {ok, Bin} ->
            case unicode:characters_to_list(Bin) of
                Decoded when is_list(Decoded) ->
                    marker_root(EbinDir, [string:trim(L) || L <- string:split(Decoded, "\n", all)]);
                _ ->
                    unreadable
            end;
        {error, _} ->
            absent
    end.

marker_root(EbinDir, Lines) ->
    case [L || L <- Lines, L =/= []] of
        [] ->
            absent;
        [Only] ->
            marker_candidate(EbinDir, Only);
        [Rel, Abs | _] ->
            Cand = marker_candidate(EbinDir, Rel),
            case filelib:is_file(filename:join(Cand, "beamtalk.toml")) of
                true -> Cand;
                false -> Abs
            end
    end.

%% A relative root is relative to the marker's directory.
marker_candidate(EbinDir, Root) ->
    case is_absolute(Root) of
        true -> Root;
        false -> normalize(filename:absname(Root, EbinDir))
    end.

%% Collapse `.` and `..` segments lexically (the ebin dir is already absolute).
normalize(Path) ->
    Parts = filename:split(Path),
    filename:join(lists:reverse(lists:foldl(fun norm_seg/2, [], Parts))).

norm_seg(".", Acc) -> Acc;
norm_seg("..", [Prev | Rest]) when Prev =/= "/" -> Rest;
norm_seg("..", Acc) -> Acc;
norm_seg(Seg, Acc) -> [Seg | Acc].

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
