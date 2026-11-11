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
  * `sweep_stale_temps/2` (one file's) and `sweep_stale_temps_in/2` (a whole
    directory's): delete `temp_path/1` leftovers of a crashed staged write
    once they are old enough that no live writer can own them.

A leaf module: it calls only OTP.
""".

-include_lib("kernel/include/file.hrl").

-export([
    path_inside/2,
    relative_parts/2,
    temp_path/1,
    write_temp/2,
    atomic_write/3,
    sweep_stale_temps/1,
    sweep_stale_temps/2,
    sweep_stale_temps_in/1,
    sweep_stale_temps_in/2
]).

%% How old a `temp_path/1` leftover must be before a sweep deletes it. A live
%% writer renames its temp file within moments of writing it, so a file this
%% old can only be a crashed writer's. The one value every workspace sweep uses.
-define(STALE_TEMP_SECONDS, 3600).

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

-doc """
Delete stale `temp_path/1` leftovers next to `Path`: siblings named
`<basename>.<digits>.tmp` whose last modification is more than `MaxAgeSeconds`
ago. Returns the paths it deleted.

A staged writer (flush Phase A then Phase B) that crashes between its two
phases leaves its unique temp file behind, and nothing else ever reads or
overwrites it. Only age tells a crashed writer's file from a live one's: a
live writer renames its temp file within moments, so anything older than
`MaxAgeSeconds` is abandoned. A fresh temp file is never touched, so this is
safe to run while another writer is staging the same `Path`. Best effort:
files that cannot be inspected or deleted are skipped.
""".
-spec sweep_stale_temps(string(), non_neg_integer()) -> [string()].
sweep_stale_temps(Path, MaxAgeSeconds) ->
    Prefix = filename:basename(Path) ++ ".",
    sweep(filename:dirname(Path), fun(Name) -> is_temp_name(Prefix, Name) end, MaxAgeSeconds).

-doc """
Delete every stale `temp_path/1` leftover directly in `Dir`: any
`<name>.<digits>.tmp` whose last modification is more than `MaxAgeSeconds`
ago, whatever file it was staging. Returns the paths it deleted.

For a writer that does not sweep per file, such as the ChangeLog at startup.
The same age rule as `sweep_stale_temps/2` keeps it safe while other writers
are staging files in `Dir`.
""".
-spec sweep_stale_temps_in(file:filename(), non_neg_integer()) -> [string()].
sweep_stale_temps_in(Dir, MaxAgeSeconds) ->
    sweep(Dir, fun is_any_temp_name/1, MaxAgeSeconds).

-spec sweep(file:filename(), fun((string()) -> boolean()), non_neg_integer()) -> [string()].
sweep(Dir, IsTemp, MaxAgeSeconds) ->
    Now = erlang:system_time(second),
    Names =
        case file:list_dir(Dir) of
            {ok, Ns} -> Ns;
            {error, _} -> []
        end,
    lists:filtermap(
        fun(Name) ->
            Candidate = filename:join(Dir, Name),
            case IsTemp(Name) andalso older_than(Candidate, Now, MaxAgeSeconds) of
                true -> file:delete(Candidate) =:= ok andalso {true, Candidate};
                false -> false
            end
        end,
        Names
    ).

%% `Name` is `<base>.<digits>.tmp` for some non-empty `<base>`.
-spec is_any_temp_name(string()) -> boolean().
is_any_temp_name(Name) ->
    case string:split(Name, ".", trailing) of
        [Stem, "tmp"] ->
            case string:split(Stem, ".", trailing) of
                [Base, _Digits] when Base =/= [] -> is_temp_name(Base ++ ".", Name);
                _ -> false
            end;
        _ ->
            false
    end.

%% `Name` is `Prefix ++ Digits ++ ".tmp"` with at least one digit: exactly
%% what `temp_path/1` produces for the file `Prefix` was built from.
-spec is_temp_name(string(), string()) -> boolean().
is_temp_name(Prefix, Name) ->
    case lists:prefix(Prefix, Name) of
        true ->
            %% Match ".tmp" on what follows the prefix, so a name where the
            %% prefix's own trailing "." would also start ".tmp" (the legacy
            %% `<basename>.tmp`) is not mistaken for one.
            case lists:reverse(lists:nthtail(length(Prefix), Name)) of
                "pmt." ++ RevDigits when RevDigits =/= [] ->
                    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, RevDigits);
                _ ->
                    false
            end;
        false ->
            false
    end.

-spec older_than(string(), integer(), non_neg_integer()) -> boolean().
older_than(File, Now, MaxAgeSeconds) ->
    case file:read_file_info(File, [{time, posix}]) of
        {ok, #file_info{type = regular, mtime = MTime}} -> Now - MTime > MaxAgeSeconds;
        _ -> false
    end.

-doc "`sweep_stale_temps/2` with the workspace's one staleness threshold (an hour).".
-spec sweep_stale_temps(string()) -> [string()].
sweep_stale_temps(Path) ->
    sweep_stale_temps(Path, ?STALE_TEMP_SECONDS).

-doc "`sweep_stale_temps_in/2` with the workspace's one staleness threshold (an hour).".
-spec sweep_stale_temps_in(file:filename()) -> [string()].
sweep_stale_temps_in(Dir) ->
    sweep_stale_temps_in(Dir, ?STALE_TEMP_SECONDS).
