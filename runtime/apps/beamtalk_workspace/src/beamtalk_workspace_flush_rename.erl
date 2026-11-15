%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush_rename).

%%% **DDD Context:** Workspace Context

-moduledoc """
Rename-class / move-class and rename-method preparation for flush (ADR 0114):
the Phase A file-collision checks, new-path derivation (`resolve_new_path/1`),
and the staging of renamed/moved files and multi-site rewrites. Moved verbatim
out of `beamtalk_workspace_flush` (BT-4907); the orchestrator calls the
`check_*` and `phase_a_*` entry points, this module calls the splice/site-unit
engine in `beamtalk_workspace_flush_sites`.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_flush.hrl").

-export([
    check_no_rename_file_collisions/2,
    check_no_rename_method_file_collisions/3,
    phase_a_rename_methods/1,
    phase_a_renames/1,
    rename_entry_all_files/1,
    rename_expected_files_map/1,
    resolve_new_path/1,
    rename_method_expected_files_map/1
]).

%%% ----------------------------------------------------------------------------
%%% Class rename (Tier 2) — multi-file staged move (ADR 0114)
%%% ----------------------------------------------------------------------------

-doc """
Cross-pipeline collision guard (ADR 0114): abort the whole flush, before any
Phase A I/O runs, if a `'rename-class'` entry's target files (`old_path`,
`new_path`, and every site's `sourceFile`) overlap any file an *ordinary*
entry in the same batch is about to splice. Mirrors `prepare_file/2`'s
`mixed_remove_class_and_splice` defensive guard for the identical reason —
reordering "write a patch into this file" and "this file is moving/being
deleted" within one Phase A/B pass is undesigned, so it is refused rather
than silently racing.

Two independent `'rename-class'` entries touching the same file (e.g. two
unrelated renames each rewriting a reference in the same third file) are NOT
a collision here — `group_units_by_file/1` (inside `phase_a_renames/1`)
already merges their sites into one combined splice, the same way
`group_by_file/1` already merges multiple ordinary entries against one file.
""".
-spec check_no_rename_file_collisions([term()], [{binary(), [term()]}]) ->
    ok | {conflict, [map()]}.
check_no_rename_file_collisions([], _OrdinaryGroups) ->
    ok;
check_no_rename_file_collisions(RenameEntries, OrdinaryGroups) ->
    OrdinaryFiles = sets:from_list([F || {F, _} <- OrdinaryGroups], [{version, 2}]),
    OrdinaryConflicts = lists:filtermap(
        fun(Entry) -> rename_entry_ordinary_collision(Entry, OrdinaryFiles, OrdinaryGroups) end,
        RenameEntries
    ),
    RenameConflicts = rename_entry_rename_collisions(RenameEntries),
    case OrdinaryConflicts ++ RenameConflicts of
        [] -> ok;
        Conflicts -> {conflict, Conflicts}
    end.

-spec rename_entry_ordinary_collision(term(), sets:set(binary()), [{binary(), [term()]}]) ->
    {true, map()} | false.
rename_entry_ordinary_collision(Entry, OrdinaryFiles, OrdinaryGroups) ->
    case [F || F <- rename_entry_all_files(Entry), sets:is_element(F, OrdinaryFiles)] of
        [] ->
            false;
        [Collided | _] ->
            {Collided, OrdEntries} = lists:keyfind(Collided, 1, OrdinaryGroups),
            {true,
                beamtalk_workspace_flush_support:conflict_map(
                    Collided,
                    <<"mixed_rename_and_ordinary_edit">>,
                    [Entry | OrdEntries],
                    iolist_to_binary([
                        <<"Cannot flush a rename-class entry (">>,
                        beamtalk_workspace_changelog:entry_old_class(Entry),
                        <<" -> ">>,
                        beamtalk_workspace_changelog:entry_class(Entry),
                        <<
                            ") alongside a pending ordinary patch against the same file "
                            "in the same operation; flush or discard the other pending "
                            "entry first, then re-flush the rename"
                        >>
                    ])
                )}
    end.

-doc """
`rename_entry_ordinary_collision/3` only
guards a rename-class entry's touched files against *ordinary* pending
entries — two rename-class entries whose own files intersect (e.g. a future
`new_path`-supplying producer, or two unrelated classes whose
`derive_new_path/3` styles coincide — see the moduledoc's "Atomicity (class
rename)" section) went unguarded, risking one rename's `.tmp` silently
clobbering the other's. Unreachable today (`classRenameTo/2`'s own
`ensure_rename_collision_free/2` refuses a second rename targeting a class
name already live, which is what would be needed to co-pend two renames
colliding on the SAME `new_path`) but defended proactively rather than left
as a latent gap for the next producer.

Compares each entry's FULL touched-file set (`rename_entry_all_files/1` —
`old_path` included, not just write-targets) against the other's, on BOTH
sides — a first fix here compared write-targets to write-targets only and
missed the more dangerous shape review round 2 found: entry A's `new_path`
equal to entry B's `old_path`. Since Phase B's `move` commit unlinks
`old_path` only AFTER its own `new_path.tmp` rename succeeds, whichever of
A/B commits second would delete the file the OTHER just wrote there —
silently destroying a renamed class's content while both entries report as
cleanly flushed. Using the full set on both sides catches this (and the
symmetric case, B's `new_path` equal to A's `old_path`) in one check, the
same way `rename_entry_ordinary_collision/3` already uses the full set for
the rename side of its own (asymmetric, since an ordinary patch has no
separate "old"/"new" path) guard.

Known, accepted limitation: since `old_path` is only ever refreshed
by a recompile (`maybe_reload_renamed_class_source/1`, itself only triggered
by a flush COMMIT), renaming the SAME class twice before ever flushing
(`Foo renameTo: #Bar` then, with no flush in between, `Bar renameTo: #Baz`)
produces two entries that both compute `old_path = foo.bt` — genuinely
sharing a file, so this guard correctly refuses the whole batch as a
collision rather than the two-hop rename it actually is. Safe (clean abort,
no data loss, both entries stay pending); collapsing the chain into an
effective single-hop rename was deliberately rejected as too much
atomicity risk for a UX nicety (a deliberate design decision). What this function DOES
do for that shape is report it accurately: `same_class_rename_chain/2`
recognises when a colliding pair is entirely attributable to one entry's
`old_class` tracing back to the other's `class` (a genuine rename chain,
not two unrelated classes) and `rename_pair_conflict/3` emits a distinct
`same_class_rename_chain_needs_flush` reason/message for it instead of the
generic `mixed_rename_and_rename_edit`, which would otherwise wrongly imply
an unrelated-class collision.

Checked pairwise, each unordered pair exactly once (`Rest` only ever holds
entries *after* `Entry` in list order) — mirrors `rename_entry_ordinary_
collision/3`'s one-conflict-per-entry granularity, just against a peer
rename instead of an ordinary group.
""".
-spec rename_entry_rename_collisions([term()]) -> [map()].
rename_entry_rename_collisions([]) ->
    [];
rename_entry_rename_collisions([Entry | Rest]) ->
    Files = sets:from_list(rename_entry_all_files(Entry), [{version, 2}]),
    case rename_entry_rename_collision_with(Entry, Files, Rest) of
        {true, Conflict} -> [Conflict | rename_entry_rename_collisions(Rest)];
        false -> rename_entry_rename_collisions(Rest)
    end.

-spec rename_entry_rename_collision_with(term(), sets:set(binary()), [term()]) ->
    {true, map()} | false.
rename_entry_rename_collision_with(_Entry, _Files, []) ->
    false;
rename_entry_rename_collision_with(Entry, Files, [Other | Rest]) ->
    OtherFiles = sets:from_list(rename_entry_all_files(Other), [{version, 2}]),
    case sets:to_list(sets:intersection(Files, OtherFiles)) of
        [] ->
            rename_entry_rename_collision_with(Entry, Files, Rest);
        [Collided | _] ->
            {true, rename_pair_conflict(Collided, Entry, Other)}
    end.

-doc """
Builds the conflict map for one colliding rename-vs-rename pair.
Distinguishes two shapes that both surface as a file collision here:

* A genuine same-class rename CHAIN — `Other` picks up where `Entry` left
  off (pair order from `rename_entry_rename_collisions/1` is chronological,
  `Entry` always before `Other` — see `same_class_rename_chain/2`'s own doc
  for why only that one direction is safe to check), detected via
  `same_class_rename_chain/2`. Reported as `same_class_rename_chain_needs_
  flush` with a message naming the actual two/three-class chain and asking
  for a flush between renames — NOT the generic wording below, which would
  wrongly imply two unrelated classes collided.
* A genuine cross-class collision (e.g. `derive_new_path/3` coincidence, or
  the `new_path`-equals-other's-`old_path` shape) — reported as
  `mixed_rename_and_rename_edit`, unchanged from before this fix.
""".
-spec rename_pair_conflict(binary(), term(), term()) -> map().
rename_pair_conflict(Collided, Entry, Other) ->
    case same_class_rename_chain(Entry, Other) of
        true ->
            beamtalk_workspace_flush_support:conflict_map(
                Collided,
                <<"same_class_rename_chain_needs_flush">>,
                [Entry, Other],
                iolist_to_binary([
                    <<"Cannot flush two renames of the same class (">>,
                    beamtalk_workspace_changelog:entry_old_class(Entry),
                    <<" -> ">>,
                    beamtalk_workspace_changelog:entry_class(Entry),
                    <<" and ">>,
                    beamtalk_workspace_changelog:entry_old_class(Other),
                    <<" -> ">>,
                    beamtalk_workspace_changelog:entry_class(Other),
                    <<
                        ") in the same operation; the class was renamed more "
                        "than once before flushing, so flush between renames "
                        "of the same class, then perform the next rename"
                    >>
                ])
            );
        false ->
            beamtalk_workspace_flush_support:conflict_map(
                Collided,
                <<"mixed_rename_and_rename_edit">>,
                [Entry, Other],
                iolist_to_binary([
                    <<"Cannot flush two rename-class entries (">>,
                    beamtalk_workspace_changelog:entry_old_class(Entry),
                    <<" -> ">>,
                    beamtalk_workspace_changelog:entry_class(Entry),
                    <<" and ">>,
                    beamtalk_workspace_changelog:entry_old_class(Other),
                    <<" -> ">>,
                    beamtalk_workspace_changelog:entry_class(Other),
                    <<
                        ") whose target files collide in the same operation; "
                        "flush or discard one of the pending entries first, "
                        "then re-flush the other"
                    >>
                ])
            )
    end.

-doc """
True when `Entry` and `Other`'s file collision is entirely attributable to
them being the SAME underlying class renamed twice before any intervening
flush — `Other`'s `old_class` traces back to `Entry`'s `class` —
rather than two genuinely unrelated classes.

Checked in ONE direction only, deliberately: pair order out of
`rename_entry_rename_collisions/1` is chronological (`Entry` always before
`Other` in append/seq order), and class names are unique in the live
registry, so `Other`'s `old_class` can only equal `Entry`'s `class` by
actually being the same class instance `Entry` just renamed — there is no
independent class that could coincidentally share that name at that
moment. The REVERSE direction (`Entry`'s `old_class` equal to `Other`'s
`class`) is NOT safe to also check, despite looking symmetric: `Entry`'s
rename vacates its old name, and a later, wholly UNRELATED class can
legitimately be renamed to reuse that now-free name (e.g. `Foo renameTo:
#Bar` frees `Foo`, then some unrelated `X renameTo: #Foo` legitimately
reclaims it) — if `X`'s derived `new_path` happens to collide with
`Entry`'s vacated `old_path`, that IS a genuine cross-class collision
(`X`'s incoming file vs. `Entry`'s outgoing one) needing the generic
`mixed_rename_and_rename_edit` reason, not this one. Checking the reverse
direction would misreport that real collision as a same-class chain.

`entry_old_class/1` is `binary() | undefined` (a rename entry always sets
it in practice, but the type allows `undefined`); guarding against
`undefined` avoids matching two entries that coincidentally both lack it.
""".
-spec same_class_rename_chain(term(), term()) -> boolean().
same_class_rename_chain(Entry, Other) ->
    EntryClass = beamtalk_workspace_changelog:entry_class(Entry),
    OtherOldClass = beamtalk_workspace_changelog:entry_old_class(Other),
    OtherOldClass =/= undefined andalso EntryClass =:= OtherOldClass.

%% Every file `Entry`'s rename touches in any way — read (`old_path`),
%% written (`new_path` and every other site's file). Used only for the
%% cross-pipeline collision check above; `rename_entry_all_files_to_write/1`
%% (excludes `old_path`) is the one that matters for "which files must
%% commit for this entry to count as flushed" (`rename_expected_files_map/1`).
-spec rename_entry_all_files(term()) -> [binary()].
rename_entry_all_files(Entry) ->
    OldPath = beamtalk_workspace_changelog:entry_old_path(Entry),
    lists:usort([F || F <- [OldPath | rename_entry_all_files_to_write(Entry)], F =/= undefined]).

-spec rename_entry_all_files_to_write(term()) -> [binary()].
rename_entry_all_files_to_write(Entry) ->
    NewPath = resolve_new_path(Entry),
    SiteFiles = [maps:get(source_file, S) || S <- other_sites(Entry)],
    lists:usort([F || F <- [NewPath | SiteFiles], F =/= undefined]).

%% The `sites` entries whose own file is NOT the file being moved — an
%% ordinary reference site in a different file. See `move_sites/1` for the
%% complementary set (sites whose file IS `old_path`, folded into the move's
%% own splice instead — ADR 0114 § "Atomicity (class rename)").
-spec other_sites(term()) -> [beamtalk_workspace_changelog:site()].
other_sites(Entry) ->
    OldPath = beamtalk_workspace_changelog:entry_old_path(Entry),
    [
        S
     || S <- beamtalk_workspace_changelog:entry_sites(Entry),
        S =/= undefined,
        maps:get(source_file, S) =/= OldPath
    ].

%% The `sites` entries whose own file IS `old_path` — the declaration site
%% itself (`sites[0]`, always this) plus any OTHER reference the class makes
%% to its own name inside its own file (a self-send/self-reference).
%% Multiple entries here must be merged into ONE splice against `old_path`,
%% never independent ones, or the second would silently discard the first's
%% edit (mirrors `beamtalk_repl_loader:group_sites_by_class/1`'s identical
%% same-class merge rule for the in-memory half of this mechanism).
-spec move_sites(term()) -> [beamtalk_workspace_changelog:site()].
move_sites(Entry) ->
    OldPath = beamtalk_workspace_changelog:entry_old_path(Entry),
    [
        S
     || S <- beamtalk_workspace_changelog:entry_sites(Entry),
        S =/= undefined,
        maps:get(source_file, S) =:= OldPath
    ].

-doc """
Resolve `Entry`'s post-rename path, deriving one when the entry itself did
not record `new_path` (ADR 0114 § ChangeLog schema documents `new_path` as
"basename derived from new_class, same directory as old_path" as a RULE, not
necessarily something the writer must have already computed — today's only
producer, `classRenameTo/2`, always logs `new_path => undefined`, per its own
`old_path => ..., new_path => undefined` ChangeLog spec). Preserves whichever
of the project's two established file-naming conventions `old_path` itself
used (`beamtalk_repl_loader`'s own `newClass:at:` basename check accepts
either an exact `Counter.bt` match or a `to_snake_case/1` `greeter.bt`
match) rather than forcing one style on the renamed file.
""".
-spec resolve_new_path(term()) -> binary() | undefined.
resolve_new_path(Entry) ->
    case beamtalk_workspace_changelog:entry_new_path(Entry) of
        undefined ->
            derive_new_path(
                beamtalk_workspace_changelog:entry_old_path(Entry),
                beamtalk_workspace_changelog:entry_old_class(Entry),
                beamtalk_workspace_changelog:entry_class(Entry)
            );
        Path ->
            Path
    end.

-spec derive_new_path(binary() | undefined, binary() | undefined, binary()) ->
    binary() | undefined.
derive_new_path(undefined, _OldClassBin, _NewClassBin) ->
    undefined;
derive_new_path(OldPath, OldClassBin, NewClassBin) ->
    % NOTE: `raw_dirname/1`, not `filename:dirname/1` — the stdlib version
    % always returns a `/`-only path, even when given a `\`-delimited
    % Windows path (`filename:dirname("C:\\proj\\src\\Counter.bt")` comes
    % back as `"C:/proj/src"`, per its own implementation and doc example).
    % That silently desyncs Dir from the un-normalized paths used everywhere
    % else (the original `beamtalk_source` attribute, ChangeLog
    % `old_path`/`new_path` strings), breaking `sourceFile` equality checks
    % on Windows whenever `old_path` was backslash-delimited.
    Dir = raw_dirname(binary_to_list(OldPath)),
    % NOTE: `raw_basename/2`, not `filename:basename/2` — the stdlib version
    % only recognizes `\` as a separator when `os:type/0` reports `win32`,
    % so it silently fails to split a `\`-delimited path apart on any other
    % host (harmless when this code actually runs on Windows, since it then
    % sees real backslashes and os:type() to match, but it makes the same
    % path-splitting logic behave differently depending on which host runs
    % it — including in tests, which is why the two now share raw_dirname/1
    % and raw_basename/2's OS-independent notion of "separator" instead).
    OldBase = raw_basename(binary_to_list(OldPath), ".bt"),
    NewStem =
        case OldClassBin =/= undefined andalso list_to_binary(OldBase) =:= OldClassBin of
            true -> binary_to_list(NewClassBin);
            false -> beamtalk_repl_loader_support:to_snake_case(binary_to_list(NewClassBin))
        end,
    % NOTE: deliberately not filename:join/2 — on win32 it lower-cases a
    % leading drive letter (e.g. "C:" -> "c:") while raw_dirname/1 above
    % does not, so the two would disagree on the drive-letter case and
    % desync from the un-normalized paths used elsewhere (the original
    % `beamtalk_source` attribute, and ChangeLog `old_path`/`new_path`
    % strings), breaking `sourceFile` equality checks on Windows.
    NewPath =
        case Dir of
            "." ->
                NewStem ++ ".bt";
            _ ->
                case lists:suffix("/", Dir) of
                    true -> Dir ++ NewStem ++ ".bt";
                    false -> Dir ++ "/" ++ NewStem ++ ".bt"
                end
        end,
    list_to_binary(NewPath).

-doc """
Like `filename:dirname/1`, but treats BOTH `/` and `\\` as path separators
regardless of host OS, and never normalizes the separator it finds — the
returned prefix is a byte-for-byte slice of `Path` up to (excluding) the
last separator character. `filename:dirname/1` always rewrites the result
to use `/` (see `derive_new_path/3`'s note above), which loses whichever
separator convention `Path` actually used.

Not OS-gated (unlike `filename:dirname/1`, which only recognizes `\\` when
`os:type/0` reports `win32`): the paths this function is applied to already
mix conventions on a single host (a project root in native OS form, with
`/`-joined suffixes appended by callers), so both separators must always be
recognized everywhere this runs, not just on Windows.
""".
-spec raw_dirname(string()) -> string().
raw_dirname(Path) ->
    case last_separator_pos(Path, 1, 0) of
        0 -> ".";
        1 -> string:slice(Path, 0, 1);
        Pos -> string:slice(Path, 0, Pos - 1)
    end.

-spec last_separator_pos(string(), pos_integer(), non_neg_integer()) -> non_neg_integer().
last_separator_pos([], _Idx, Last) ->
    Last;
last_separator_pos([C | Rest], Idx, _Last) when C =:= $/; C =:= $\\ ->
    last_separator_pos(Rest, Idx + 1, Idx);
last_separator_pos([_ | Rest], Idx, Last) ->
    last_separator_pos(Rest, Idx + 1, Last).

-doc """
Like `filename:basename/2`, but recognizes BOTH `/` and `\\` as path
separators regardless of host OS — see `raw_dirname/1`'s doc for why this
module needs that independence. `Ext` is stripped from the end of the
final path component when present, exactly like `filename:basename/2`'s
own `Ext` argument.
""".
-spec raw_basename(string(), string()) -> string().
raw_basename(Path, Ext) ->
    Base =
        case last_separator_pos(Path, 1, 0) of
            0 -> Path;
            Pos -> string:slice(Path, Pos)
        end,
    strip_suffix(Base, Ext).

-spec strip_suffix(string(), string()) -> string().
strip_suffix(Str, Suffix) ->
    case lists:suffix(Suffix, Str) of
        true -> lists:sublist(Str, length(Str) - length(Suffix));
        false -> Str
    end.

-doc """
Per-entry expected file set: every file that must appear in a Phase B
commit for `Entry`'s `'rename-class'` to be considered fully flushed
(`multi_site_entry_fully_committed/3`). Keyed by `seq` since `#prepared{}`
records carry the raw `entry()`, not a convenient lookup key.
""".
-spec rename_expected_files_map([term()]) -> #{non_neg_integer() => sets:set(binary())}.
rename_expected_files_map(RenameEntries) ->
    lists:foldl(
        fun(Entry, Acc) ->
            Seq = beamtalk_workspace_changelog:entry_seq(Entry),
            Files = sets:from_list(rename_entry_all_files_to_write(Entry), [{version, 2}]),
            Acc#{Seq => Files}
        end,
        #{},
        RenameEntries
    ).

-doc """
Phase A for every pending `'rename-class'` entry in this flush, combined
across entries the same way `phase_a/1` combines ordinary file groups:
build every entry's move task plus every OTHER-site file group (merged
across entries, ADR 0114 § "Atomicity (class rename)"), prepare each in
turn, and abort-with-cleanup on the first conflict or hard error exactly
like `phase_a_loop/3` does for ordinary groups.
""".
-spec phase_a_renames([term()]) ->
    {ok, [#prepared{}]} | {error, #beamtalk_error{}} | {conflict, [map()]}.
phase_a_renames([]) ->
    {ok, []};
phase_a_renames(RenameEntries) ->
    {MoveTasks, SiteGroups} = build_rename_tasks(RenameEntries),
    Tasks = [{move, T} || T <- MoveTasks] ++ [{site_group, G} || G <- SiteGroups],
    phase_a_renames_loop(Tasks, [], []).

phase_a_renames_loop([], Prepared, []) ->
    {ok, lists:reverse(Prepared)};
phase_a_renames_loop([], Prepared, Conflicts) ->
    beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
    {conflict, lists:reverse(Conflicts)};
phase_a_renames_loop([{move, {Entry, MoveUnits}} | Rest], Prepared, Conflicts) ->
    case prepare_rename_move(Entry, MoveUnits) of
        {ok, Rec} ->
            phase_a_renames_loop(Rest, [Rec | Prepared], Conflicts);
        {conflict, C} ->
            phase_a_renames_loop(Rest, Prepared, [C | Conflicts]);
        {error, _} = Err ->
            beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
            Err
    end;
phase_a_renames_loop([{site_group, {File, Units}} | Rest], Prepared, Conflicts) ->
    case prepare_rename_site_group(File, Units) of
        {ok, Rec} ->
            phase_a_renames_loop(Rest, [Rec | Prepared], Conflicts);
        {conflict, C} ->
            phase_a_renames_loop(Rest, Prepared, [C | Conflicts]);
        {error, _} = Err ->
            beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
            Err
    end.

%% Fan `RenameEntries` out into `{MoveTasks, SiteGroups}`:
%%   - `MoveTasks`: one `{Entry, MoveUnits}` per entry (`move_sites/1` — the
%%     declaration plus any self-reference in the same file).
%%   - `SiteGroups`: `{File, [{Site, OwnerEntry}]}` merged ACROSS every
%%     entry's `other_sites/1`, keyed by file, mirroring `group_by_file/1`'s
%%     merge for ordinary entries — two different renames touching the same
%%     referencing file compose into one splice, not two racing writes.
-spec build_rename_tasks([term()]) ->
    {[{term(), [beamtalk_workspace_changelog:site()]}], [
        {binary(), [{beamtalk_workspace_changelog:site(), term()}]}
    ]}.
build_rename_tasks(RenameEntries) ->
    {MoveTasksRev, OtherUnitsRev} = lists:foldl(
        fun(Entry, {MoveAcc, OtherAcc}) ->
            MoveUnits = move_sites(Entry),
            OtherUnits = [{S, Entry} || S <- other_sites(Entry)],
            {[{Entry, MoveUnits} | MoveAcc], lists:reverse(OtherUnits) ++ OtherAcc}
        end,
        {[], []},
        RenameEntries
    ),
    {lists:reverse(MoveTasksRev), group_units_by_file(lists:reverse(OtherUnitsRev))}.

-spec group_units_by_file([{beamtalk_workspace_changelog:site(), term()}]) ->
    [{binary(), [{beamtalk_workspace_changelog:site(), term()}]}].
group_units_by_file(Units) ->
    lists:foldr(
        fun({Site, _Entry} = Unit, Acc) ->
            File = maps:get(source_file, Site),
            case lists:keyfind(File, 1, Acc) of
                {File, Us} -> lists:keyreplace(File, 1, Acc, {File, [Unit | Us]});
                false -> [{File, [Unit]} | Acc]
            end
        end,
        [],
        Units
    ).

-doc """
Stage a `'rename-class'` entry's move: splice `MoveUnits` (the declaration
line, plus any self-reference sites in the same file) against `old_path`'s
current content and write the result to `<new_path>.tmp`.

Three outcomes, matching the moduledoc's "Atomicity (class rename)" table:
`old_path` exists (the ordinary case — read, splice, stage); `old_path` is
gone but `new_path` already carries every unit's expected new text (a prior
attempt's move already completed — `op = move_noop`, no I/O); or neither
resolves (a hard, unrecoverable conflict).
""".
-spec prepare_rename_move(term(), [beamtalk_workspace_changelog:site()]) ->
    {ok, #prepared{}} | {conflict, map()} | {error, #beamtalk_error{}}.
prepare_rename_move(Entry, MoveUnits) ->
    OldPath = beamtalk_workspace_changelog:entry_old_path(Entry),
    NewPath = resolve_new_path(Entry),
    Units = [{Site, Entry} || Site <- MoveUnits],
    case file:read_file(binary_to_list(OldPath)) of
        {ok, OldBody} ->
            case beamtalk_workspace_flush_sites:apply_site_units(OldBody, Units) of
                {ok, NewFileBody} ->
                    case check_rename_target_safe(NewPath, NewFileBody, Entry) of
                        ok ->
                            case
                                beamtalk_workspace_flush_support:write_tmp(
                                    binary_to_list(NewPath), NewFileBody
                                )
                            of
                                {ok, Tmp} ->
                                    {ok, #prepared{
                                        file = NewPath,
                                        old_file = OldPath,
                                        tmp = Tmp,
                                        entries = [Entry],
                                        pre_existing = true,
                                        op = move
                                    }};
                                {error, _} = Err ->
                                    beamtalk_workspace_flush_support:wrap_io_error(Err, NewPath)
                            end;
                        {conflict, _} = C ->
                            C
                    end;
                {conflict, _} = C ->
                    C;
                {error, _} = Err ->
                    Err
            end;
        {error, enoent} ->
            resolve_missing_rename_source(Entry, OldPath, NewPath, Units);
        {error, Reason} ->
            beamtalk_workspace_flush_support:wrap_io_error({error, Reason}, OldPath)
    end.

-doc """
Refuse to stage a move over an unrelated pre-existing file at `NewPath`
(review round 4 on PR #3526): `old_path` still existing here means Phase B
has not yet renamed `<new_path>.tmp` into place for THIS attempt, so
`NewPath` already existing can only mean one of two things — an earlier
attempt's own Phase B rename already succeeded but its subsequent unlink of
`old_path` failed (the documented "unlink itself fails" recovery case,
`commit/1`'s `op = move` doc), in which case `NewPath` already holds
EXACTLY `NewFileBody`; or an unrelated file that happens to already occupy
the derived target path, which this rename must never silently overwrite —
mirrors `prepare_new_class/3`'s `target_exists` guard for the identical
reason (`'new-class'` already refuses this; a rename creating a file at a
brand-new path is not fundamentally different).

Byte-for-byte equality against the ALREADY-COMPUTED `NewFileBody` (not just
"a file exists") is what tells the two cases apart without a false
positive on the legitimate resume — content merely existing doesn't mean
this is ours.
""".
-spec check_rename_target_safe(binary(), binary(), term()) -> ok | {conflict, map()}.
check_rename_target_safe(NewPath, NewFileBody, Entry) ->
    AbsNewPath = binary_to_list(NewPath),
    case file:read_file(AbsNewPath) of
        {ok, NewFileBody} ->
            ok;
        {error, enoent} ->
            ok;
        _NotAlreadyOurs ->
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    NewPath,
                    <<"rename_target_exists">>,
                    [Entry],
                    iolist_to_binary([
                        <<"Cannot rename to ">>,
                        NewPath,
                        <<
                            "; a file already exists at that path with "
                            "different content; move or remove it first, "
                            "then re-flush the rename"
                        >>
                    ])
                )}
    end.

%% See moduledoc's "Atomicity (class rename)" — `old_path` gone disambiguates
%% into "already fully moved" (op = move_noop) vs. an unresolvable conflict.
-spec resolve_missing_rename_source(
    term(), binary(), binary() | undefined, [{beamtalk_workspace_changelog:site(), term()}]
) ->
    {ok, #prepared{}} | {conflict, map()} | {error, #beamtalk_error{}}.
resolve_missing_rename_source(Entry, OldPath, undefined, _Units) ->
    {conflict,
        beamtalk_workspace_flush_support:conflict_map(
            OldPath,
            <<"rename_target_unresolvable">>,
            [Entry],
            <<
                "Could not determine the renamed class's target path (new_path was not "
                "recorded and could not be derived); the rename cannot be flushed"
            >>
        )};
resolve_missing_rename_source(Entry, OldPath, NewPath, Units) ->
    case file:read_file(binary_to_list(NewPath)) of
        {ok, NewBody} ->
            case beamtalk_workspace_flush_sites:all_units_already_applied(NewBody, Units) of
                true ->
                    {ok, #prepared{
                        file = NewPath,
                        old_file = undefined,
                        tmp = undefined,
                        entries = [Entry],
                        pre_existing = false,
                        op = move_noop
                    }};
                false ->
                    {conflict,
                        beamtalk_workspace_flush_support:conflict_map(
                            OldPath,
                            <<"rename_target_mismatch">>,
                            [Entry],
                            iolist_to_binary([
                                <<"Neither ">>,
                                OldPath,
                                <<" (old path) exists, nor does ">>,
                                NewPath,
                                <<
                                    " (new path) already carry the expected renamed text; "
                                    "the rename cannot be resumed automatically. Re-issue "
                                    "the rename or resolve the target manually."
                                >>
                            ])
                        )}
            end;
        {error, enoent} ->
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    OldPath,
                    <<"source_missing">>,
                    [Entry],
                    iolist_to_binary([
                        <<"Neither the old path (">>,
                        OldPath,
                        <<") nor the new path (">>,
                        NewPath,
                        <<
                            ") exists; the rename cannot be completed automatically. "
                            "Something external deleted both."
                        >>
                    ])
                )};
        {error, Reason} ->
            beamtalk_workspace_flush_support:wrap_io_error({error, Reason}, NewPath)
    end.

-doc """
Stage one OTHER-site file group's splice: read `File`, resolve every unit's
splice against it (idempotent — see `resolve_site_unit/2`), write
`<File>.tmp`. Ordinary Tier-1-shaped splice, just sourced from `site()`
maps carrying their own `span`/`source_ref`/`prev_source_ref` instead of an
entry's own top-level fields.
""".
-spec prepare_rename_site_group(binary(), [{beamtalk_workspace_changelog:site(), term()}]) ->
    {ok, #prepared{}} | {conflict, map()} | {error, #beamtalk_error{}}.
prepare_rename_site_group(File, Units) ->
    AbsPath = binary_to_list(File),
    case file:read_file(AbsPath) of
        {ok, Disk} ->
            case beamtalk_workspace_flush_sites:apply_site_units(Disk, Units) of
                {ok, NewBody} ->
                    case beamtalk_workspace_flush_support:write_tmp(AbsPath, NewBody) of
                        {ok, Tmp} ->
                            OwnerEntries = lists:usort(
                                fun(A, B) ->
                                    beamtalk_workspace_changelog:entry_seq(A) =<
                                        beamtalk_workspace_changelog:entry_seq(B)
                                end,
                                [E || {_S, E} <- Units]
                            ),
                            {ok, #prepared{
                                file = File,
                                tmp = Tmp,
                                entries = OwnerEntries,
                                pre_existing = true
                            }};
                        {error, _} = Err ->
                            beamtalk_workspace_flush_support:wrap_io_error(Err, File)
                    end;
                {conflict, _} = C ->
                    C;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    File,
                    <<"source_file_unreadable">>,
                    [E || {_S, E} <- Units],
                    iolist_to_binary([
                        <<"Could not read source file: ">>, atom_to_binary(Reason, utf8)
                    ])
                )}
    end.

%%% ----------------------------------------------------------------------------
%%% Method rename (Tier 2) — multi-file staged splice (ADR 0114)
%%% ----------------------------------------------------------------------------

-doc """
Cross-pipeline collision guard for `'rename-method'` (ADR 0114):
abort the whole flush, before any Phase A I/O runs, if a `'rename-method'`
entry's confirmed-site files overlap any file an ordinary pending patch or a
pending `'rename-class'` entry is about to touch in the same batch (`OtherFiles`,
built by the caller as the union of both). Mirrors `check_no_rename_file_
collisions/2`'s identical defensive posture for the identical reason —
reordering two different splice mechanisms against one file within a single
Phase A/B pass is undesigned, so it is refused rather than silently raced.

Two `'rename-method'` entries sharing a file are deliberately NOT a collision
here (unlike class rename, which guards rename-vs-rename because a move can
race another move's own unlink) — their confirmed sites simply merge into one
splice via `group_units_by_file/1`, the same way multiple ordinary patches
against one file already merge via `group_by_file/1`. There is no move/unlink
step for a method rename to race.
""".
-spec check_no_rename_method_file_collisions([term()], sets:set(binary()), [{binary(), [term()]}]) ->
    ok | {conflict, [map()]}.
check_no_rename_method_file_collisions([], _OtherFiles, _OrdinaryGroups) ->
    ok;
check_no_rename_method_file_collisions(RenameMethodEntries, OtherFiles, OrdinaryGroups) ->
    Conflicts = lists:filtermap(
        fun(Entry) -> rename_method_entry_collision(Entry, OtherFiles, OrdinaryGroups) end,
        RenameMethodEntries
    ),
    case Conflicts of
        [] -> ok;
        _ -> {conflict, Conflicts}
    end.

-spec rename_method_entry_collision(term(), sets:set(binary()), [{binary(), [term()]}]) ->
    {true, map()} | false.
rename_method_entry_collision(Entry, OtherFiles, OrdinaryGroups) ->
    case [F || F <- rename_method_entry_all_files(Entry), sets:is_element(F, OtherFiles)] of
        [] ->
            false;
        [Collided | _] ->
            %% The colliding file may belong to a pending ordinary group (in
            %% which case its entries are worth reporting alongside this
            %% one) or to a `'rename-class'` entry's own touched files (no
            %% convenient group to look up here — reporting just this
            %% entry's own seq is still an accurate, actionable conflict).
            OrdEntries =
                case lists:keyfind(Collided, 1, OrdinaryGroups) of
                    {Collided, Es} -> Es;
                    false -> []
                end,
            {true,
                beamtalk_workspace_flush_support:conflict_map(
                    Collided,
                    <<"mixed_rename_method_and_pending_edit">>,
                    [Entry | OrdEntries],
                    iolist_to_binary([
                        <<"Cannot flush a rename-method entry (">>,
                        beamtalk_workspace_changelog:entry_class(Entry),
                        <<" ">>,
                        display_selector(beamtalk_workspace_changelog:entry_old_selector(Entry)),
                        <<" -> ">>,
                        display_selector(beamtalk_workspace_changelog:entry_selector(Entry)),
                        <<
                            ") alongside a pending ordinary patch or another pending "
                            "rename against the same file in the same operation; flush "
                            "or discard the other pending entry first, then re-flush "
                            "this rename"
                        >>
                    ])
                )}
    end.

-spec display_selector(binary() | undefined) -> binary().
display_selector(undefined) -> <<"?">>;
display_selector(Sel) when is_binary(Sel) -> Sel.

%% Every file a `'rename-method'` entry's CONFIRMED sites touch — the
%% definition (`sites[0]`) plus every confirmed self/super sender
%% (`sites[1..]`). `candidate_sites` are never consulted here (ADR 0114: they
%% are never staged, written, or otherwise touched by flush under any
%% circumstance).
-spec rename_method_entry_all_files(term()) -> [binary()].
rename_method_entry_all_files(Entry) ->
    lists:usort([
        maps:get(source_file, S)
     || S <- entry_confirmed_sites(Entry), S =/= undefined
    ]).

-spec entry_confirmed_sites(term()) -> [beamtalk_workspace_changelog:site()].
entry_confirmed_sites(Entry) ->
    case beamtalk_workspace_changelog:entry_sites(Entry) of
        undefined -> [];
        Sites -> Sites
    end.

-doc """
Per-entry expected file set for a `'rename-method'` entry: every file that
must appear in a Phase B commit for it to be considered fully flushed
(`multi_site_entry_fully_committed/3`). Mirrors `rename_expected_files_map/1`
for class rename; keyed by `seq` for the same reason (`#prepared{}` records
carry the raw `entry()`, not a convenient lookup key).
""".
-spec rename_method_expected_files_map([term()]) -> #{non_neg_integer() => sets:set(binary())}.
rename_method_expected_files_map(RenameMethodEntries) ->
    lists:foldl(
        fun(Entry, Acc) ->
            Seq = beamtalk_workspace_changelog:entry_seq(Entry),
            Files = sets:from_list(rename_method_entry_all_files(Entry), [{version, 2}]),
            Acc#{Seq => Files}
        end,
        #{},
        RenameMethodEntries
    ).

-doc """
Phase A for every pending `'rename-method'` entry in this flush (ADR 0114):
build the union of every entry's CONFIRMED `sites` (never
`candidate_sites`) as `{Site, OwnerEntry}` units — sites from different
entries that land in the same file merge, via `group_units_by_file/1`,
exactly the way class rename's own OTHER-site references already merge
across entries — then stage each file group with the same generic
`prepare_rename_site_group/2` class rename's reference-site splices use.
A `'rename-method'` entry has no file-move component (renaming a selector
never changes which file a method's class lives in), so every one of its
sites, including the definition site, is an ordinary in-place splice; abort-
with-cleanup on the first conflict or hard error exactly like
`phase_a_loop/3`/`phase_a_renames_loop/3` do for their own groups.
""".
-spec phase_a_rename_methods([term()]) ->
    {ok, [#prepared{}]} | {error, #beamtalk_error{}} | {conflict, [map()]}.
phase_a_rename_methods([]) ->
    {ok, []};
phase_a_rename_methods(RenameMethodEntries) ->
    Units = lists:flatmap(
        fun(Entry) ->
            [{Site, Entry} || Site <- entry_confirmed_sites(Entry), Site =/= undefined]
        end,
        RenameMethodEntries
    ),
    Groups = group_units_by_file(Units),
    phase_a_rename_methods_loop(Groups, [], []).

phase_a_rename_methods_loop([], Prepared, []) ->
    {ok, lists:reverse(Prepared)};
phase_a_rename_methods_loop([], Prepared, Conflicts) ->
    beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
    {conflict, lists:reverse(Conflicts)};
phase_a_rename_methods_loop([{File, Units} | Rest], Prepared, Conflicts) ->
    case prepare_rename_site_group(File, Units) of
        {ok, Rec} ->
            phase_a_rename_methods_loop(Rest, [Rec | Prepared], Conflicts);
        {conflict, C} ->
            phase_a_rename_methods_loop(Rest, Prepared, [C | Conflicts]);
        {error, _} = Err ->
            beamtalk_workspace_flush_support:cleanup_tmps(Prepared),
            Err
    end.
