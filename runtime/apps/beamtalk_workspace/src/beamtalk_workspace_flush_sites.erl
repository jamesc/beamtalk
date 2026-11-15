%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush_sites).

%%% **DDD Context:** Workspace Context

-moduledoc """
The flush splice and site-unit engine: reads a file's bytes, applies each
entry's byte-span patch (or appends a brand-new method), and resolves multi-site
rename units against the on-disk body, reporting span conflicts and
already-applied detection. Moved verbatim out of `beamtalk_workspace_flush`
(BT-4907); the orchestrator and `beamtalk_workspace_flush_rename` call it.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace_flush.hrl").

-export([
    all_units_already_applied/2,
    apply_site_units/2,
    multi_site_entry_fully_committed/3,
    prepare_splice/2
]).

%% Apply every `{Site, OwnerEntry}` unit's splice to `Body`, rightmost-span-
%% first (mirrors `apply_splices/2`/`beamtalk_repl_loader:apply_site_splices/2`'s
%% identical tie-break: a later, higher-offset splice must land before an
%% earlier one so a length-changing replacement never invalidates a
%% not-yet-applied earlier span's recorded byte offsets).
-spec apply_site_units(binary(), [{beamtalk_workspace_changelog:site(), term()}]) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
apply_site_units(Body, Units) ->
    Sorted = lists:sort(
        fun(
            {#{span := #{start := A, 'end' := AEnd}}, _},
            {#{span := #{start := B, 'end' := BEnd}}, _}
        ) ->
            {A, AEnd} >= {B, BEnd}
        end,
        Units
    ),
    apply_site_units_loop(Body, Sorted).

apply_site_units_loop(Body, []) ->
    {ok, Body};
apply_site_units_loop(Body, [Unit | Rest]) ->
    case resolve_site_unit(Body, Unit) of
        {ok, NewBody} -> apply_site_units_loop(NewBody, Rest);
        Other -> Other
    end.

-doc """
Idempotent three-way splice check for one rewrite site (ADR 0114 §
"Atomicity (class rename)"): the recorded span either still holds the prior
text (splice normally), already holds the new text (a previously-committed
site from a partially-failed flush attempt — a no-op, not a conflict), or
neither (a genuine external edit or invalid span). The middle case is what
lets a re-issued destructive flush retry only the sites that never
committed instead of permanently misreporting an already-correct site as an
external edit.
""".
-spec resolve_site_unit(binary(), {beamtalk_workspace_changelog:site(), term()}) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
resolve_site_unit(Body, {Site, OwnerEntry}) ->
    #{source_file := File, span := #{start := Start, 'end' := End}} = Site,
    NewRef = maps:get(source_ref, Site, undefined),
    PrevRef = maps:get(prev_source_ref, Site, undefined),
    case beamtalk_workspace_changelog:read_site_body(NewRef) of
        {ok, NewText} ->
            case beamtalk_workspace_changelog:read_site_body(PrevRef) of
                {ok, PrevText} ->
                    resolve_site_unit_2(Body, File, Start, End, PrevText, NewText, OwnerEntry);
                {error, Reason} ->
                    {error, beamtalk_workspace_flush_support:prev_source_error(File, Reason)}
            end;
        {error, Reason} ->
            {error, beamtalk_workspace_flush_support:source_body_error(File, Reason)}
    end.

resolve_site_unit_2(Body, File, Start, End, PrevText, NewText, OwnerEntry) ->
    case beamtalk_workspace_span:check_span(Body, {Start, End}, PrevText) of
        ok ->
            {ok, beamtalk_workspace_span:splice(Body, {Start, End}, NewText)};
        {conflict, external_edit = Reason} ->
            case beamtalk_workspace_span:matches_at(Body, Start, NewText) of
                true ->
                    {ok, Body};
                false ->
                    {conflict,
                        span_conflict(File, Reason, OwnerEntry, <<
                            "the rewrite's recorded prev_source, and do not already carry "
                            "the rewritten text either"
                        >>)}
            end;
        {conflict, Reason} ->
            {conflict, span_conflict(File, Reason, OwnerEntry, <<>>)}
    end.

-doc """
Build the flush conflict for a failed `beamtalk_workspace_span:check_span/3`
(shared by method splices and rewrite-site splices). `Expected` describes what
the span was expected to hold, for the `external_edit` detail text (unused
for `span_out_of_range`).
""".
-spec span_conflict(
    binary() | undefined,
    beamtalk_workspace_span:conflict_reason(),
    term(),
    binary()
) -> map().
span_conflict(File, {span_out_of_range, Start, End, Size}, Entry, _Expected) ->
    beamtalk_workspace_flush_support:conflict_map(
        File,
        <<"span_out_of_range">>,
        [Entry],
        iolist_to_binary([
            <<"Recorded byte span ">>,
            integer_to_binary(Start),
            <<"..">>,
            integer_to_binary(End),
            <<" is outside the current ">>,
            integer_to_binary(Size),
            <<"-byte file; the file changed externally">>
        ])
    );
span_conflict(File, external_edit, Entry, Expected) ->
    beamtalk_workspace_flush_support:conflict_map(
        File,
        <<"external_edit">>,
        [Entry],
        <<
            "External edit detected: the bytes at the recorded span no longer match ",
            Expected/binary,
            ". Re-flush after reconciling, or use `Workspace changes clear` to "
            "discard the pending entries"
        >>
    ).

-doc """
Verify every unit's expected new text already sits at its correct position
in `Body` — the "old_path already gone" recovery check `resolve_missing_
rename_source/4` uses to tell a genuinely completed move apart from an
unresolvable conflict.

**Cannot just check each unit's own RECORDED `span.start` against `Body`**
(review round 5 on PR #3526): that offset is relative to the ORIGINAL,
pre-splice source — valid input to `apply_site_units/2`'s own progressive,
rightmost-first application (each not-yet-applied lower-offset unit's
recorded start is still correct at the moment ITS splice runs, since only
content strictly after it has shifted so far), but NOT valid as a direct
lookup into the FINAL, fully-spliced `Body` this function is handed instead
— there is no original body to progressively splice against here (`old_path`
is gone, which is the whole reason this recovery path exists). Once `move_
sites/1` has more than one unit in the same file (a declaration plus a
same-file self-reference, `rename_class_self_reference_folds_into_move/1`'s
own fixture shape) and the rename changes the class name's byte length (the
overwhelmingly common case), every unit at a HIGHER original offset than an
earlier one has shifted in the final body by that earlier unit's own
length delta — checking its stale, unshifted offset misses genuinely
correct content and reports a live, working rename as an unresolvable
`rename_target_mismatch`, contradicting this PR's whole idempotent-retry
guarantee (fails safe, not silently, but still wrong).

Fixed by walking units in ascending original-offset order and threading a
running `Shift` — the sum of `byte_size(NewText) - byte_size(PrevText)`
for every unit already processed (all of which sit at LOWER original
offsets, hence entirely before this one in the file) — added to each
unit's own recorded `start` before checking. This mirrors, in reverse
order, exactly how `apply_site_units/2`'s rightmost-first application
would have shifted this unit's true position, without needing the
original pre-splice body at all — every input it needs (`prev_source_ref`
alongside the already-used `source_ref`) is already recorded on the
site.
""".
-spec all_units_already_applied(binary(), [{beamtalk_workspace_changelog:site(), term()}]) ->
    boolean().
all_units_already_applied(Body, Units) ->
    Sorted = lists:sort(
        fun(
            {#{span := #{start := A, 'end' := AEnd}}, _},
            {#{span := #{start := B, 'end' := BEnd}}, _}
        ) ->
            {A, AEnd} =< {B, BEnd}
        end,
        Units
    ),
    all_units_already_applied_loop(Body, Sorted, 0).

-spec all_units_already_applied_loop(
    binary(), [{beamtalk_workspace_changelog:site(), term()}], integer()
) -> boolean().
all_units_already_applied_loop(_Body, [], _Shift) ->
    true;
all_units_already_applied_loop(Body, [{Site, _Entry} | Rest], Shift) ->
    #{span := #{start := Start}} = Site,
    NewRef = maps:get(source_ref, Site, undefined),
    PrevRef = maps:get(prev_source_ref, Site, undefined),
    case
        {
            beamtalk_workspace_changelog:read_site_body(NewRef),
            beamtalk_workspace_changelog:read_site_body(PrevRef)
        }
    of
        {{ok, NewText}, {ok, PrevText}} ->
            case beamtalk_workspace_span:matches_at(Body, Start + Shift, NewText) of
                true ->
                    NextShift = Shift + (byte_size(NewText) - byte_size(PrevText)),
                    all_units_already_applied_loop(Body, Rest, NextShift);
                false ->
                    false
            end;
        _ ->
            false
    end.

%% A `'rename-class'`/`'rename-method'` entry is only considered fully
%% flushed once EVERY file it touches (`rename_entry_all_files_to_write/1` /
%% `rename_method_entry_all_files/1`, folded together into the single
%% `RenameExpectedFiles` map `run_flush/2` builds, keyed by seq — globally
%% unique across every entry regardless of kind, so the two kinds' entries
%% never collide in one map) has committed in the SAME Phase B pass — every
%% other kind targets exactly one file, so it trivially satisfies this the
%% moment it appears in `Committed` at all.
-spec multi_site_entry_fully_committed(
    term(), #{non_neg_integer() => sets:set(binary())}, sets:set(binary())
) -> boolean().
multi_site_entry_fully_committed(Entry, RenameExpectedFiles, CommittedFilesSet) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        Kind when Kind =:= 'rename-class'; Kind =:= 'rename-method' ->
            Seq = beamtalk_workspace_changelog:entry_seq(Entry),
            Expected = maps:get(Seq, RenameExpectedFiles, sets:new([{version, 2}])),
            sets:is_subset(Expected, CommittedFilesSet);
        _ ->
            true
    end.

%%% ----------------------------------------------------------------------------
%%% Method splice
%%% ----------------------------------------------------------------------------

prepare_splice(File, Entries) ->
    AbsPath = binary_to_list(File),
    case file:read_file(AbsPath) of
        {ok, Disk} ->
            case apply_splices(Disk, Entries) of
                {ok, NewBody} ->
                    case beamtalk_workspace_flush_support:write_tmp(AbsPath, NewBody) of
                        {ok, Tmp} ->
                            {ok, #prepared{
                                file = File,
                                tmp = Tmp,
                                entries = Entries,
                                pre_existing = true
                            }};
                        {error, _} = Err ->
                            beamtalk_workspace_flush_support:wrap_io_error(Err, File)
                    end;
                {conflict, ConflictMap} ->
                    {conflict, ConflictMap};
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {conflict,
                beamtalk_workspace_flush_support:conflict_map(
                    File,
                    <<"source_file_unreadable">>,
                    Entries,
                    iolist_to_binary([
                        <<"Could not read source file: ">>, atom_to_binary(Reason, utf8)
                    ])
                )}
    end.

%% Apply each entry's splice to Body. Entries are applied in *descending* span
%% order so earlier byte offsets are not shifted by later replacements. An
%% external-edit conflict on any entry short-circuits with `{conflict, ...}`.
-spec apply_splices(binary(), [term()]) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
apply_splices(Body, Entries) ->
    %% Sort by span start descending so later splices (closer to end of file)
    %% are applied first, leaving earlier spans' byte offsets intact for the
    %% next iteration.
    Sorted = lists:sort(
        fun(A, B) ->
            beamtalk_workspace_flush_support:span_start(A) >
                beamtalk_workspace_flush_support:span_start(B)
        end,
        Entries
    ),
    apply_splices_loop(Body, Sorted).

apply_splices_loop(Body, []) ->
    {ok, Body};
apply_splices_loop(Body, [Entry | Rest]) ->
    case splice_one(Body, Entry) of
        {ok, NewBody} -> apply_splices_loop(NewBody, Rest);
        Other -> Other
    end.

-spec splice_one(binary(), term()) ->
    {ok, binary()} | {conflict, map()} | {error, #beamtalk_error{}}.
splice_one(Body, Entry) ->
    case beamtalk_workspace_changelog:entry_span(Entry) of
        undefined ->
            %% A splice entry with no span: the install hook recorded
            %% `selector_not_found` (a brand-new method added live, the file
            %% has no prior definition to overwrite). Append the body after
            %% a separating newline if the file does not already end in one.
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, NewSrc} ->
                    {ok, append_method(Body, NewSrc)};
                {error, Reason} ->
                    {error,
                        beamtalk_workspace_flush_support:source_body_error(
                            beamtalk_workspace_changelog:entry_source_file(Entry), Reason
                        )}
            end;
        #{start := Start, 'end' := End} ->
            case beamtalk_workspace_changelog:read_prev_source_body(Entry) of
                {ok, PrevBody} ->
                    splice_with_prev(Body, Entry, Start, End, PrevBody);
                {error, Reason} ->
                    {error,
                        beamtalk_workspace_flush_support:prev_source_error(
                            beamtalk_workspace_changelog:entry_source_file(Entry), Reason
                        )}
            end
    end.

splice_with_prev(Body, Entry, Start, End, PrevBody) ->
    File = beamtalk_workspace_changelog:entry_source_file(Entry),
    case beamtalk_workspace_span:check_span(Body, {Start, End}, PrevBody) of
        ok ->
            replacement_for(Entry, Body, Start, End, File);
        {conflict, Reason} ->
            {conflict, span_conflict(File, Reason, Entry, <<"the patch's recorded prev_source">>)}
    end.

%% The bytes to splice into `{Start, End}` once the external-edit check has
%% passed. A `'remove-method'` entry (ADR 0113 Phase 2: Tier 1, no gate — see
%% module doc) has no `source_ref` — nothing replaces the excised text, so the
%% replacement is empty, mechanically identical to a patch whose new body
%% happens to be `<<>>`. Every other kind reads its recorded `source_ref`.
-spec replacement_for(term(), binary(), non_neg_integer(), non_neg_integer(), binary() | undefined) ->
    {ok, binary()} | {error, #beamtalk_error{}}.
replacement_for(Entry, Body, Start, End, File) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'remove-method' ->
            {ok, beamtalk_workspace_span:splice(Body, {Start, End}, <<>>)};
        _ ->
            case beamtalk_workspace_changelog:read_source_body(Entry) of
                {ok, NewSrc} ->
                    %% The stored `source` is already the on-disk
                    %% byte-span shape (`source_ref == disk[span]` by
                    %% construction — the install hook reshaped the
                    %% compiler's canonical body to the span's base
                    %% indentation via `reindent_method_source`). The splice
                    %% is a verbatim byte replacement; no reshaping here.
                    {ok, beamtalk_workspace_span:splice(Body, {Start, End}, NewSrc)};
                {error, Reason} ->
                    {error, beamtalk_workspace_flush_support:source_body_error(File, Reason)}
            end
    end.

%% Append a method body (which itself may or may not end in a newline) after a
%% blank-line separator. Used for `selector_not_found` entries — a brand-new
%% method added live whose disk source does not yet have it. Ensures the disk
%% file ends in exactly one trailing newline before appending.
-spec append_method(binary(), binary()) -> binary().
append_method(Body, NewSrc) ->
    Trimmed = beamtalk_workspace_reshape:strip_trailing_newlines(Body),
    NewSrcWithNl = beamtalk_workspace_reshape:ensure_trailing_newline(NewSrc),
    <<Trimmed/binary, "\n\n", NewSrcWithNl/binary>>.
