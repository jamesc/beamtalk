%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_file).

%%% **DDD Context:** Workspace Context

-moduledoc """
Small filesystem helpers shared by the workspace's file writers and path
checks (BT-4683).

  * `path_inside/2` and `relative_parts/2`: component-wise path containment
    (`"/a/bc"` is not inside `"/a/b"`), used wherever the workspace asks
    "is this file under that directory?".
  * `atomic_write/3` and `write_temp/2`: write through a temp file in the
    target's directory, then rename over the target. The temp name is unique
    per call (`erlang:unique_integer/1`), never a fixed `<file>.tmp`, so two
    writers aiming at the same path can never write into one shared temp file
    and rename each other's partial content into place.

A leaf module: it calls only OTP.
""".

-export([
    path_inside/2,
    relative_parts/2,
    temp_path/1,
    write_temp/2,
    atomic_write/3
]).

-doc """
True iff `Path` is `Root` itself or lies beneath it.

Compares `filename:split/1` components, so a sibling sharing a string prefix
(`"/proj-other"` against `"/proj"`) is not inside, and a trailing or doubled
separator on either side does not matter. Neither path is made absolute;
callers pass paths in the same form (normally both from
`filename:absname/1`).
""".
-spec path_inside(file:filename(), file:filename()) -> boolean().
path_inside(Root, Path) ->
    relative_parts(Root, Path) =/= false.

-doc """
`Path`'s components below `Root` (`[]` when `Path` is `Root` itself), or
`false` when `Path` is not inside `Root`. Same comparison as `path_inside/2`.
""".
-spec relative_parts(file:filename(), file:filename()) -> {ok, [file:filename()]} | false.
relative_parts(Root, Path) ->
    RootParts = filename:split(Root),
    PathParts = filename:split(Path),
    case lists:prefix(RootParts, PathParts) of
        true -> {ok, lists:nthtail(length(RootParts), PathParts)};
        false -> false
    end.

-doc """
A fresh temp path next to `Path`: `<Path>.<unique>.tmp`. Unique per call, so
concurrent writers to the same `Path` never share a temp file.
""".
-spec temp_path(string()) -> string().
temp_path(Path) ->
    Path ++ "." ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp".

-doc """
Write `Data` to a fresh `temp_path/1` next to `Path` and return the temp
path, for a caller that renames it into place later (a staged two-phase
commit such as `beamtalk_workspace_flush`). On a write failure the partial
temp file is deleted.
""".
-spec write_temp(string(), iodata()) -> {ok, string()} | {error, {write, term()}}.
write_temp(Path, Data) ->
    Tmp = temp_path(Path),
    case file:write_file(Tmp, Data) of
        ok ->
            {ok, Tmp};
        {error, Reason} ->
            _ = file:delete(Tmp),
            {error, {write, Reason}}
    end.

-doc """
Atomically replace `Path` with `Data`: write a `write_temp/2` temp file, then
`file:rename/2` it over `Path` (atomic on POSIX), so a reader never sees a
half-written file. The temp file is deleted on any failure.

`ExpectedCurrent`, when not `undefined`, is byte-compared against `Path`'s
current contents just before the rename. A mismatch aborts with
`{error, {recheck, changed_on_disk}}` and writes nothing, which closes the
window between a caller's own earlier read and this rename.

Errors are tagged with the step that failed: `{write, Reason}`,
`{recheck, Reason}` or `{rename, Reason}`.
""".
-spec atomic_write(string(), iodata(), binary() | undefined) ->
    ok | {error, {write | recheck | rename, term()}}.
atomic_write(Path, Data, ExpectedCurrent) ->
    case write_temp(Path, Data) of
        {ok, Tmp} ->
            case recheck_unchanged(Path, ExpectedCurrent) of
                ok ->
                    case file:rename(Tmp, Path) of
                        ok ->
                            ok;
                        {error, RenameReason} ->
                            _ = file:delete(Tmp),
                            {error, {rename, RenameReason}}
                    end;
                {error, RecheckReason} ->
                    _ = file:delete(Tmp),
                    {error, {recheck, RecheckReason}}
            end;
        {error, _} = WriteErr ->
            WriteErr
    end.

%% `ok` when `ExpectedCurrent` is `undefined` (no recheck requested) or
%% byte-matches `Path`'s current contents; `{error, changed_on_disk}` on a
%% mismatch (a concurrent writer landed since the caller's own read);
%% `{error, Reason}` if `Path` can no longer be read at all.
-spec recheck_unchanged(string(), binary() | undefined) -> ok | {error, term()}.
recheck_unchanged(_Path, undefined) ->
    ok;
recheck_unchanged(Path, ExpectedCurrent) ->
    case file:read_file(Path) of
        {ok, ExpectedCurrent} -> ok;
        {ok, _Other} -> {error, changed_on_disk};
        {error, Reason} -> {error, Reason}
    end.
