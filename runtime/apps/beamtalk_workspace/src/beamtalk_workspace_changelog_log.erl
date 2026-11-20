%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_changelog_log).

%%% **DDD Context:** Workspace Context

-moduledoc """
ChangeLog persistence: the `changes.jsonl` append/load path, transactional rotation into `archive/`, source-body file bookkeeping, stale-temp sweeping and atomic writes (ADR 0082, 'ChangeLog growth'). Moved verbatim out of `beamtalk_workspace_changelog` (BT-4907); the gen_server there owns the ETS table and calls into this module.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_changelog.hrl").
-include("beamtalk_workspace_changelog_state.hrl").

-export([
    changes_dir/1,
    load_from_disk/1,
    log_path/1,
    repair_tail/1,
    persist_append/4,
    rewrite_log/2,
    rotate_transactional/3,
    truncate_to/2,
    source_ref_filename/2,
    sweep_stale_temps/1,
    truncate_log/1,
    warn_if_persistence_skipped/2,
    write_file_atomic/2
]).

%% Crash-safe persistence ordering: write the body files first (atomically via
%% temp+rename), then append the metadata line. A crash between the two leaves
%% orphaned body files (harmless — pruned on rotation) but never a metadata line
%% pointing at a missing body. In run mode (no changes_dir) this is a no-op and
%% the entry lives in ETS only.
-spec persist_append(#entry{}, binary() | undefined, binary() | undefined, #state{}) ->
    ok | {error, term()}.
persist_append(_Entry, _Source, _PrevSource, #state{changes_dir = undefined}) ->
    ok;
persist_append(Entry, Source, PrevSource, State) ->
    SourcesDir = filename:join(State#state.changes_dir, "sources"),
    case filelib:ensure_path(SourcesDir) of
        ok ->
            case write_optional_source(SourcesDir, Entry#entry.source_ref, Source) of
                ok ->
                    case
                        write_optional_source(SourcesDir, Entry#entry.prev_source_ref, PrevSource)
                    of
                        ok -> append_metadata_line(Entry, State);
                        Err -> Err
                    end;
                Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {ensure_path, SourcesDir, Reason}}
    end.

%% Write `Body` to `sources/<Ref>` when `Ref` is present. `Ref` is `undefined`
%% for a `'remove-method'` entry's `source_ref` (no new body — ADR 0112) and,
%% independently, for `prev_source_ref` on any entry with no recorded prior
%% body (e.g. a `new-class` entry, or a patch that added a brand-new method) —
%% both are legitimate no-ops, not persistence failures.
-spec write_optional_source(string(), binary() | undefined, binary() | undefined) ->
    ok | {error, term()}.
write_optional_source(_SourcesDir, undefined, _Body) ->
    ok;
write_optional_source(SourcesDir, Ref, Body) ->
    Path = filename:join(SourcesDir, binary_to_list(Ref)),
    write_file_atomic(Path, Body).

%% Append `Data` to `Path` and fsync before returning (BT-5084): the entry is only
%% reported recorded once it is on stable storage, like the atomic temp+rename
%% writes (`write_file_atomic/2`) used for every other ChangeLog file.
-spec append_synced(string(), iodata()) -> ok | {error, term()}.
append_synced(Path, Data) ->
    case file:open(Path, [append, raw, binary]) of
        {ok, Fd} ->
            append_synced_open(Path, Fd, Data);
        {error, _} = Err ->
            Err
    end.

-spec append_synced_open(string(), file:fd(), iodata()) -> ok | {error, term()}.
append_synced_open(Path, Fd, Data) ->
    case file:position(Fd, eof) of
        {ok, Before} ->
            Result =
                case file:write(Fd, Data) of
                    ok -> file:sync(Fd);
                    {error, _} = Err -> Err
                end,
            _ = file:close(Fd),
            %% A failed write or fsync may leave a whole line on disk that the
            %% caller is told was not recorded; the next append would reuse its
            %% seq and one entry would overwrite the other at load. Cut the file
            %% back to where it was (BT-5084).
            case Result of
                ok ->
                    ok;
                {error, _} ->
                    _ = truncate_to(Path, Before),
                    Result
            end;
        {error, _} = Err ->
            _ = file:close(Fd),
            Err
    end.

-spec truncate_to(string(), non_neg_integer()) -> ok | {error, term()}.
truncate_to(Path, Size) ->
    case file:open(Path, [read, write, raw, binary]) of
        {ok, Fd} ->
            Result =
                case file:position(Fd, Size) of
                    {ok, _} -> file:truncate(Fd);
                    {error, _} = Err -> Err
                end,
            _ = file:close(Fd),
            Result;
        {error, _} = Err ->
            Err
    end.

-spec append_metadata_line(#entry{}, #state{}) -> ok | {error, term()}.
append_metadata_line(Entry, State) ->
    Line = [beamtalk_workspace_changelog_codec:entry_to_json(Entry), $\n],
    case append_synced(State#state.log_path, Line) of
        ok ->
            ok;
        {error, Reason} ->
            %% A failed append (disk full, I/O error) may have written part of
            %% the line. Cut that fragment off now, or the next successful
            %% append would be glued onto it and both lost at load.
            _ = repair_tail(State#state.log_path),
            {error, {write_log, Reason}}
    end.

%% BT-5017: bring `changes.jsonl` back to whole lines after a torn write.
%%
%% Every entry is written as `<json>\n`, so a file that does not end in a
%% newline ends in a partial write. The reader's rule for that last line:
%%   * it decodes as an entry (the crash fell between the JSON and its
%%     newline): keep it and restore the newline;
%%   * it does not decode (cut off mid-entry): drop it, truncating the file
%%     back to the last newline.
%% Earlier lines are never touched, and a file that already ends in a newline
%% is not rewritten. The rewrite is atomic, so repair cannot itself tear the log.
-spec repair_tail(string()) -> ok | {error, term()}.
repair_tail(LogPath) ->
    case file:read_file(LogPath) of
        {ok, Bin} ->
            _ = repair_tail_binary(LogPath, Bin),
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Returns the repaired contents (what `LogPath` holds afterwards, or would
%% hold had the rewrite not failed), so the loader parses the repaired view.
-spec repair_tail_binary(string(), binary()) -> binary().
repair_tail_binary(_LogPath, <<>>) ->
    <<>>;
repair_tail_binary(LogPath, Bin) ->
    case binary:last(Bin) of
        $\n ->
            Bin;
        _ ->
            {Whole, Tail} = split_last_line(Bin),
            Repaired =
                case is_entry_line(Tail) of
                    true ->
                        ?LOG_WARNING(
                            "ChangeLog ~ts: last entry was missing its newline; restored",
                            [LogPath]
                        ),
                        <<Whole/binary, Tail/binary, $\n>>;
                    false ->
                        ?LOG_WARNING(
                            "ChangeLog ~ts: dropping a partial last entry (~b bytes) left "
                            "by an interrupted write",
                            [LogPath, byte_size(Tail)]
                        ),
                        Whole
                end,
            case write_file_atomic(LogPath, Repaired) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("ChangeLog ~ts: could not repair torn tail: ~p", [
                        LogPath, Reason
                    ])
            end,
            Repaired
    end.

%% `{Through the last newline, after it}`.
-spec split_last_line(binary()) -> {binary(), binary()}.
split_last_line(Bin) ->
    case binary:matches(Bin, <<"\n">>) of
        [] ->
            {<<>>, Bin};
        Matches ->
            {Pos, 1} = lists:last(Matches),
            {binary:part(Bin, 0, Pos + 1), binary:part(Bin, Pos + 1, byte_size(Bin) - Pos - 1)}
    end.

-spec is_entry_line(binary()) -> boolean().
is_entry_line(Line) ->
    try beamtalk_workspace_changelog_codec:entry_from_json(Line) of
        _ -> true
    catch
        _:_ -> false
    end.

%%% ----------------------------------------------------------------------------
%%% Load on startup (restart semantics)
%%% ----------------------------------------------------------------------------

%% Read changes.jsonl, rebuild ETS, assign a fresh epoch, and tag every
%% pre-existing entry as prior-epoch + (if prev_source no longer matches disk)
%% orphan. In run mode (no changes_dir) there is nothing on disk — start clean.
-spec load_from_disk(#state{}) -> #state{}.
load_from_disk(#state{changes_dir = undefined} = State) ->
    State#state{next_seq = 0, epoch = 0};
load_from_disk(State) ->
    LogPath = State#state.log_path,
    case file:read_file(LogPath) of
        {ok, Bin0} ->
            Bin = repair_tail_binary(LogPath, Bin0),
            Entries = parse_log(Bin),
            PriorEpochMax = max_epoch(Entries),
            NextSeq = max_seq(Entries) + 1,
            FreshEpoch = PriorEpochMax + 1,
            SourcesDir = filename:join(State#state.changes_dir, "sources"),
            Tagged = [tag_prior(E, SourcesDir) || E <- Entries],
            lists:foreach(fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, Tagged),
            State#state{next_seq = NextSeq, epoch = FreshEpoch};
        {error, enoent} ->
            State#state{next_seq = 0, epoch = 1};
        {error, Reason} ->
            ?LOG_WARNING("Failed to read ChangeLog at ~ts: ~p", [LogPath, Reason]),
            State#state{next_seq = 0, epoch = 1}
    end.

%% Pre-existing entry → prior epoch; orphan iff its recorded prev_source no
%% longer matches the current on-disk content of its sourceFile.
-spec tag_prior(#entry{}, string()) -> #entry{}.
tag_prior(Entry, SourcesDir) ->
    Entry#entry{prior_epoch = true, orphan = is_orphan(Entry, SourcesDir)}.

-spec is_orphan(#entry{}, string()) -> boolean().
is_orphan(#entry{source_file = undefined}, _SourcesDir) ->
    %% Non-flushable (stdlib/dynamic) entries have no disk file to compare —
    %% their memory state is gone on restart, but they are not "orphaned"
    %% against disk content. Excluded from the active view via prior_epoch.
    false;
is_orphan(#entry{prev_source_ref = undefined}, _SourcesDir) ->
    %% new-class entries: the file should not have existed at append time. If it
    %% exists now, the active view excludes it via prior_epoch regardless; we do
    %% not mark new-class entries orphan here (relocation/conflict is Phase 2).
    false;
is_orphan(#entry{source_file = SourceFile, span = Span, prev_source_ref = PrevRef}, SourcesDir) ->
    PrevPath = filename:join(SourcesDir, binary_to_list(PrevRef)),
    case {file:read_file(binary_to_list(SourceFile)), file:read_file(PrevPath)} of
        {{ok, DiskBin}, {ok, PrevBin}} ->
            not beamtalk_workspace_span:matches_span(DiskBin, Span, PrevBin);
        _ ->
            %% Source file or recorded prev body unreadable → treat as orphaned:
            %% the patch can no longer be safely reconciled against disk.
            true
    end.

-spec parse_log(binary()) -> [#entry{}].
parse_log(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
    lists:filtermap(
        fun(Line) ->
            try
                {true, beamtalk_workspace_changelog_codec:entry_from_json(Line)}
            catch
                Class:Reason ->
                    ?LOG_WARNING("Skipping malformed ChangeLog line: ~p:~p", [Class, Reason]),
                    false
            end
        end,
        Lines
    ).

-spec max_seq([#entry{}]) -> integer().
max_seq([]) -> -1;
max_seq(Entries) -> lists:max([E#entry.seq || E <- Entries]).

-spec max_epoch([#entry{}]) -> integer().
max_epoch([]) -> 0;
max_epoch(Entries) -> lists:max([E#entry.epoch || E <- Entries]).

%% Perform the rotation only if every disk step succeeds. Order:
%%   1. archive the overflow segment (metadata + sources) to archive/
%%   2. rewrite changes.jsonl with exactly the retained entries
%% Both are crash-safe (atomic temp+rename). Only once both succeed do we prune
%% the archived source bodies and swap ETS to the retained set. On any failure we
%% return State unchanged (ETS and the live log keep all entries) and log it.
-spec rotate_transactional([#entry{}], [#entry{}], #state{}) -> #state{}.
rotate_transactional(ToArchive, ToKeep, State) ->
    case archive_segment(ToArchive, State) of
        {ok, ArchivedMembers} ->
            case rewrite_log(ToKeep, State) of
                ok ->
                    %% Both disk steps committed — now it is safe to drop the
                    %% archived body files and swap ETS to the retained set.
                    prune_source_members(ArchivedMembers),
                    ets:delete_all_objects(?ETS_TABLE),
                    lists:foreach(
                        fun(E) -> ets:insert(?ETS_TABLE, {E#entry.seq, E}) end, ToKeep
                    ),
                    State;
                {error, Reason} ->
                    %% Archive succeeded but the live-log rewrite failed. Leave
                    %% ETS + log untouched; the (harmless) extra archive segment
                    %% will be superseded on the next successful rotation.
                    ?LOG_ERROR(
                        "ChangeLog rotation aborted: failed to rewrite live log",
                        #{reason => Reason, domain => [beamtalk, runtime]}
                    ),
                    State
            end;
        {error, Reason} ->
            ?LOG_ERROR(
                "ChangeLog rotation aborted: failed to archive overflow segment",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            State
    end.

%% Archive the overflow segment. Returns `{ok, Members}` (the source-body files
%% that were tarred, so the caller can delete them after the whole rotation
%% commits) or `{error, Reason}` if any disk step fails. Source bodies are NOT
%% deleted here — deletion is deferred until rewrite_log/2 also succeeds.
-spec archive_segment([#entry{}], #state{}) ->
    {ok, [{string(), string()}]} | {error, term()}.
archive_segment(Entries, State) ->
    ArchiveDir = filename:join(State#state.changes_dir, "archive"),
    Ts = archive_suffix(),
    case filelib:ensure_path(ArchiveDir) of
        ok ->
            case archive_metadata(Entries, ArchiveDir, Ts) of
                ok ->
                    archive_sources(Entries, ArchiveDir, Ts, State);
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {ensure_path, ArchiveDir, Reason}}
    end.

%% Unique, monotonic, collision-free archive filename suffix. A millisecond
%% timestamp can still collide when two rotations land in the same millisecond
%% (e.g. a single overflowing batch), so we append a strictly-increasing unique
%% integer. Format: "<ms>-<unique>".
-spec archive_suffix() -> string().
archive_suffix() ->
    Ms = integer_to_list(erlang:system_time(millisecond)),
    Unique = integer_to_list(erlang:unique_integer([positive, monotonic])),
    Ms ++ "-" ++ Unique.

-spec archive_metadata([#entry{}], string(), string()) -> ok | {error, term()}.
archive_metadata(Entries, ArchiveDir, Ts) ->
    Path = filename:join(ArchiveDir, "changes-" ++ Ts ++ ".jsonl.gz"),
    Lines = [[beamtalk_workspace_changelog_codec:entry_to_json(E), $\n] || E <- Entries],
    Gz = zlib:gzip(iolist_to_binary(Lines)),
    case write_file_atomic(Path, Gz) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {archive_metadata, Path, Reason}}
    end.

%% Tar the referenced source bodies into archive/. Returns `{ok, Members}` with
%% the body files that were archived (deleted later, once the rotation commits)
%% or `{error, Reason}`. An empty member set is a successful no-op.
-spec archive_sources([#entry{}], string(), string(), #state{}) ->
    {ok, [{string(), string()}]} | {error, term()}.
archive_sources(Entries, ArchiveDir, Ts, State) ->
    SourcesDir = filename:join(State#state.changes_dir, "sources"),
    Refs = source_refs(Entries),
    Members = collect_source_members(Refs, SourcesDir),
    Path = filename:join(ArchiveDir, "sources-" ++ Ts ++ ".tar.gz"),
    case Members of
        [] ->
            {ok, []};
        _ ->
            case erl_tar:create(Path, Members, [compressed]) of
                ok ->
                    {ok, Members};
                {error, Reason} ->
                    {error, {archive_sources, Path, Reason}}
            end
    end.

%% Delete the source-body files that were safely archived. Called only after the
%% whole rotation has committed (archive + log rewrite both succeeded).
-spec prune_source_members([{string(), string()}]) -> ok.
prune_source_members(Members) ->
    lists:foreach(fun({_Name, AbsPath}) -> _ = file:delete(AbsPath) end, Members),
    ok.

-spec source_refs([#entry{}]) -> [binary()].
source_refs(Entries) ->
    lists:flatten([refs_of(E) || E <- Entries]).

%% Both refs are optional (`source_ref` is `undefined` for a `'remove-method'`
%% entry; `prev_source_ref` is `undefined` for e.g. a `new-class` entry or a
%% brand-new-method patch) — only the ones actually present are archived.
-spec refs_of(#entry{}) -> [binary()].
refs_of(#entry{source_ref = SR, prev_source_ref = PR}) ->
    [Ref || Ref <- [SR, PR], Ref =/= undefined].

%% Build erl_tar member list {NameInArchive, AbsolutePath} for refs that exist.
-spec collect_source_members([binary()], string()) -> [{string(), string()}].
collect_source_members(Refs, SourcesDir) ->
    lists:filtermap(
        fun(Ref) ->
            Name = binary_to_list(Ref),
            Abs = filename:join(SourcesDir, Name),
            case filelib:is_regular(Abs) of
                true -> {true, {Name, Abs}};
                false -> false
            end
        end,
        Refs
    ).

%%% ----------------------------------------------------------------------------
%%% On-disk helpers
%%% ----------------------------------------------------------------------------

%% Rewrite changes.jsonl from scratch with exactly Entries (used after rotation
%% and never on the hot append path). Atomic temp+rename so a crash mid-rewrite
%% cannot truncate the live log.
-spec rewrite_log([#entry{}], #state{}) -> ok | {error, term()}.
rewrite_log(_Entries, #state{log_path = undefined}) ->
    ok;
rewrite_log(Entries, State) ->
    Lines = [[beamtalk_workspace_changelog_codec:entry_to_json(E), $\n] || E <- Entries],
    write_file_atomic(State#state.log_path, iolist_to_binary(Lines)).

-spec truncate_log(#state{}) -> ok.
truncate_log(#state{log_path = undefined}) ->
    ok;
truncate_log(State) ->
    _ = write_file_atomic(State#state.log_path, <<>>),
    ok.

%% Delete temp files a crashed `write_file_atomic/2` left in `changes/` or
%% `changes/sources/`. Unique temp names mean nothing else ever overwrites or
%% removes them. Best effort; a missing directory is fine.
-spec sweep_stale_temps(string() | undefined) -> ok.
sweep_stale_temps(undefined) ->
    ok;
sweep_stale_temps(ChangesDir) ->
    _ = beamtalk_workspace_file:sweep_stale_temps_in(ChangesDir),
    _ = beamtalk_workspace_file:sweep_stale_temps_in(filename:join(ChangesDir, "sources")),
    ok.

%% Write Data to Path via a sibling temp file + atomic rename so readers never
%% observe a partially written file (`beamtalk_workspace_file:atomic_write/3`:
%% a unique temp name per call, so a writer outside this gen_server — e.g.
%% `store_site_body/1`, which runs in the caller — can never share a temp file
%% with another writer). Errors are `{write, Reason}` / `{rename, Reason}`.
-spec write_file_atomic(string(), iodata()) -> ok | {error, term()}.
write_file_atomic(Path, Data) ->
    _ = filelib:ensure_dir(Path),
    beamtalk_workspace_file:atomic_write(Path, Data, undefined).

%% sources/<seq6>-source.bt / sources/<seq6>-prev.bt
-spec source_ref_filename(non_neg_integer(), source | prev) -> binary().
source_ref_filename(Seq, Which) ->
    Padded = io_lib:format("~6..0b", [Seq]),
    Suffix =
        case Which of
            source -> "-source.bt";
            prev -> "-prev.bt"
        end,
    iolist_to_binary([Padded, Suffix]).

-doc """
Return the absolute `changes/` directory for a workspace, or `undefined` when
there is no workspace (run mode) or no home directory (BT-3680; persistence is
then off, as in run mode). Resolves via `beamtalk_platform:workspace_dir/1`:
`<home>/.beamtalk/workspaces/<id>/changes`. Exported for tests.
""".
-spec changes_dir(binary() | undefined) -> string() | undefined.
changes_dir(undefined) ->
    undefined;
changes_dir(WorkspaceId) when is_binary(WorkspaceId) ->
    case beamtalk_platform:workspace_dir(WorkspaceId) of
        undefined -> undefined;
        WsDir -> filename:join(WsDir, "changes")
    end.

%% BT-3685: a durable (non-run-mode) workspace whose home dir cannot be resolved
%% silently loses persistence; say so once at start-up (run mode is intentional).
-spec warn_if_persistence_skipped(binary() | undefined, string() | undefined) -> ok.
warn_if_persistence_skipped(WorkspaceId, undefined) when is_binary(WorkspaceId) ->
    ?LOG_WARNING(
        "No home directory; skipping ChangeLog persistence for workspace ~p",
        [WorkspaceId],
        #{domain => [beamtalk, runtime]}
    ),
    ok;
warn_if_persistence_skipped(_WorkspaceId, _ChangesDir) ->
    ok.

-spec log_path(string() | undefined) -> string() | undefined.
log_path(undefined) -> undefined;
log_path(ChangesDir) -> filename:join(ChangesDir, "changes.jsonl").
