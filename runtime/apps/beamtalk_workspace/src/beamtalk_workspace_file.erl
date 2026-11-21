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
    and rename each other's partial content into place. It also carries a
    `.bt-staging.` marker, so a sweep never mistakes another tool's
    `<file>.<digits>.tmp` in the user's source tree for one of ours.
  * `sweep_stale_temps/2` (one file's) and `sweep_stale_temps_in/2` (a whole
    directory's): delete `temp_path/1` leftovers of a crashed staged write
    once they are old enough that no live writer can own them. A fixed-name
    `<file>.tmp` left by a crash under a build from before BT-4683 is never
    swept: nothing reads it back, so it is harmless, and without the marker
    it cannot be told apart from another tool's file.

Durability (BT-5017): `write_temp/2` `fsync`s the staged bytes before
`atomic_write/3` renames them, so a power loss cannot leave the target
renamed over an empty file. The containing directory is not fsynced: OTP has
no portable directory sync, and the worst outcome of skipping it is that the
rename is lost and the previous, whole file is still there. This helper is
the one place the workspace syncs; its callers (flush, ChangeLog bodies,
`metadata.json`, native saves) add no sync of their own.

A leaf module: it calls only OTP.
""".

-include_lib("kernel/include/file.hrl").

-export([
    path_inside/2,
    relative_parts/2,
    temp_path/1,
    write_temp/2,
    staging_dir/1,
    is_staging_dir_name/1,
    atomic_write/3,
    sweep_stale_temps/1, sweep_stale_temps/2,
    sweep_stale_temps_in/1, sweep_stale_temps_in/2,
    sweep_stale_staging_dirs/1
]).

%% How old a `temp_path/1` leftover must be before a sweep deletes it. A live
%% writer renames its temp file within moments of writing it, so a file this
%% old can only be a crashed writer's. The one value every workspace sweep uses.
-define(STALE_TEMP_SECONDS, 3600).

%% Marks a `temp_path/1` name as the workspace's own. Sweeps delete only names
%% carrying it, never a bare `<file>.<digits>.tmp` some editor or other tool
%% left beside a source file.
-define(TEMP_MARKER, ".bt-staging.").

-doc """
True iff `Path` is `Root` itself or lies beneath it.

Compares `filename:split/1` components, so a sibling sharing a string prefix
(`"/proj-other"` against `"/proj"`) is not inside, and a trailing or doubled
separator on either side does not matter. Neither path is made absolute;
callers pass paths in the same form (normally both from
`filename:absname/1`).

A path with a `..` component is never inside (BT-5070): `filename:absname/1`
does not collapse it, so `/proj/src/../../x/Foo.bt` would otherwise pass a
component-prefix test. Symlinks are resolved on both sides before comparing
(each existing component is read with `file:read_link_all/1`), so a link
inside the root that points outside it is not inside either. A component that
does not exist yet is taken as written, so a file about to be created is
judged by its real parent.
""".
-spec path_inside(file:filename(), file:filename()) -> boolean().
path_inside(Root, Path) ->
    case {to_list(Root), to_list(Path)} of
        {{ok, R}, {ok, P}} ->
            case has_dotdot(R) orelse has_dotdot(P) of
                true -> false;
                false -> relative_parts(real_path(R), real_path(P)) =/= false
            end;
        _ ->
            %% Not valid unicode: cannot be judged, so not inside.
            false
    end.

-doc """
`Path`'s components below `Root` (`[]` when `Path` is `Root` itself), or
`false` when `Path` is not inside `Root`. A purely lexical comparison like
`path_inside/2`'s, except that it never resolves symlinks; a `..` component in
either path gives `false`.
""".
-spec relative_parts(file:filename(), file:filename()) -> {ok, [file:filename()]} | false.
relative_parts(Root, Path) ->
    case has_dotdot(Root) orelse has_dotdot(Path) of
        true ->
            false;
        false ->
            RootParts = filename:split(Root),
            PathParts = filename:split(Path),
            case lists:prefix(RootParts, PathParts) of
                true -> {ok, lists:nthtail(length(RootParts), PathParts)};
                false -> false
            end
    end.

-spec has_dotdot(file:filename()) -> boolean().
has_dotdot(Path) ->
    Parts = filename:split(Path),
    lists:member("..", Parts) orelse lists:member(<<"..">>, Parts).

-spec to_list(file:filename()) -> {ok, string()} | error.
to_list(Path) ->
    case unicode:characters_to_list(Path) of
        L when is_list(L) -> {ok, L};
        _ -> error
    end.

%% Symlink links followed before giving up and taking the rest as written.
-define(MAX_LINK_HOPS, 40).

%% `Path` with every existing symlink component replaced by its target.
-spec real_path(file:filename()) -> file:filename().
real_path(Path) ->
    real_path(filename:split(Path), "", 0).

-spec real_path([file:filename()], string(), non_neg_integer()) -> file:filename().
real_path([], Acc, _Hops) ->
    Acc;
real_path(Rest, Acc, Hops) when Hops > ?MAX_LINK_HOPS ->
    filename:join([Acc | Rest]);
real_path(["." | Rest], Acc, Hops) ->
    real_path(Rest, Acc, Hops);
real_path([".." | Rest], Acc, Hops) ->
    %% Only reachable from a link target; Acc is already a real path.
    real_path(Rest, filename:dirname(Acc), Hops);
real_path([C | Rest], Acc, Hops) ->
    Cur =
        case Acc of
            "" -> C;
            _ -> filename:join(Acc, C)
        end,
    case file:read_link_all(Cur) of
        {ok, Target} ->
            Abs = filename:absname(Target, filename:dirname(Cur)),
            real_path(filename:split(Abs) ++ Rest, "", Hops + 1);
        {error, _} ->
            real_path(Rest, Cur, Hops)
    end.

-doc """
A fresh temp path next to `Path`: `<Path>.bt-staging.<unique>.tmp`. Unique per
call, so concurrent writers to the same `Path` never share a temp file.
""".
-spec temp_path(string()) -> string().
temp_path(Path) ->
    Path ++ ?TEMP_MARKER ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp".

-doc """
A fresh scratch directory path next to `Path`: `<dirname>/.bt-staging.<unique>`.
For a caller that must hand a tool a file with the real name (`erlc` wants the
module's own `<module>.erl`) without ever putting a second copy where the
build looks for sources. It is not created. `is_staging_dir_name/1` recognises
the name so source scanners can skip a leftover one.
""".
-spec staging_dir(string()) -> string().
staging_dir(Path) ->
    filename:join(
        filename:dirname(Path),
        ?TEMP_MARKER ++ integer_to_list(erlang:unique_integer([positive]))
    ).

-doc "True for a directory name `staging_dir/1` produced.".
-spec is_staging_dir_name(file:filename()) -> boolean().
is_staging_dir_name(Name) ->
    lists:prefix(?TEMP_MARKER, Name).

-doc """
Write `Data` to a fresh `temp_path/1` next to `Path` and return the temp
path, for a caller that renames it into place later (a staged two-phase
commit such as `beamtalk_workspace_flush`). On a write failure the partial
temp file is deleted.
""".
-spec write_temp(string(), iodata()) -> {ok, string()} | {error, {write, term()}}.
write_temp(Path, Data) ->
    Tmp = temp_path(Path),
    case write_synced(Tmp, Data) of
        ok ->
            {ok, Tmp};
        {error, Reason} ->
            _ = file:delete(Tmp),
            {error, {write, Reason}}
    end.

%% Write `Data` to `Tmp` and `fsync` it before closing (BT-5017). A rename is
%% atomic with respect to other processes, but not durable on its own: after a
%% power loss the new name can point at a file whose data never reached disk,
%% i.e. an empty `metadata.json` or source body. Syncing the temp file first
%% closes that. The directory entry itself is not synced; see the moduledoc.
-spec write_synced(string(), iodata()) -> ok | {error, term()}.
write_synced(Tmp, Data) ->
    case file:open(Tmp, [write, binary, raw]) of
        {ok, Fd} ->
            Result =
                case file:write(Fd, Data) of
                    ok -> file:sync(Fd);
                    {error, _} = Err -> Err
                end,
            case {Result, file:close(Fd)} of
                {ok, ok} -> ok;
                {ok, {error, _} = CloseErr} -> CloseErr;
                {{error, _} = WriteErr, _} -> WriteErr
            end;
        {error, _} = OpenErr ->
            OpenErr
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
`<basename>.bt-staging.<digits>.tmp` whose last modification is more than `MaxAgeSeconds`
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
    Prefix = filename:basename(Path) ++ ?TEMP_MARKER,
    sweep(filename:dirname(Path), fun(Name) -> is_temp_name(Prefix, Name) end, MaxAgeSeconds).

-doc """
Delete every stale `temp_path/1` leftover directly in `Dir`: any
`<name>.bt-staging.<digits>.tmp` whose last modification is more than `MaxAgeSeconds`
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

%% `Name` is `<base>.bt-staging.<digits>.tmp` for some non-empty `<base>`.
-spec is_any_temp_name(string()) -> boolean().
is_any_temp_name(Name) ->
    case string:split(Name, ?TEMP_MARKER, trailing) of
        [Base, _Rest] when Base =/= [] -> is_temp_name(Base ++ ?TEMP_MARKER, Name);
        _ -> false
    end.

%% `Name` is `Prefix ++ Digits ++ ".tmp"` with at least one digit: exactly
%% what `temp_path/1` produces for the file `Prefix` was built from.
-spec is_temp_name(string(), string()) -> boolean().
is_temp_name(Prefix, Name) ->
    case lists:prefix(Prefix, Name) of
        true ->
            %% Match ".tmp" on what follows the prefix, so the prefix's own
            %% trailing "." can never double as the start of ".tmp".
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

-doc """
Delete every `.bt-staging.<digits>` scratch directory (`staging_dir/1`) directly
in `Dir` whose last modification is more than the workspace's staleness
threshold (an hour) ago, and return the paths it deleted (BT-5084). A writer that
crashed between creating its directory and removing it leaves the directory
behind and nothing else ever reads it; a fresh directory (a live writer's) is
never touched.
""".
-spec sweep_stale_staging_dirs(file:filename()) -> [string()].
sweep_stale_staging_dirs(Dir) ->
    Now = erlang:system_time(second),
    Names =
        case file:list_dir(Dir) of
            {ok, Ns} -> Ns;
            {error, _} -> []
        end,
    lists:filtermap(
        fun(Name) ->
            Candidate = filename:join(Dir, Name),
            case is_staging_dir_name(Name) andalso stale_dir(Candidate, Now) of
                true -> file:del_dir_r(Candidate) =:= ok andalso {true, Candidate};
                false -> false
            end
        end,
        Names
    ).

-spec stale_dir(string(), integer()) -> boolean().
stale_dir(Dir, Now) ->
    case file:read_file_info(Dir, [{time, posix}]) of
        {ok, #file_info{type = directory, mtime = MTime}} -> Now - MTime > ?STALE_TEMP_SECONDS;
        _ -> false
    end.

-doc "`sweep_stale_temps_in/2` with the workspace's one staleness threshold (an hour).".
-spec sweep_stale_temps_in(file:filename()) -> [string()].
sweep_stale_temps_in(Dir) ->
    sweep_stale_temps_in(Dir, ?STALE_TEMP_SECONDS).
