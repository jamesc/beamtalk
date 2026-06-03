%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_flush_tests).

-moduledoc """
Unit tests for `beamtalk_workspace_flush` (ADR 0082 Phase 2, BT-2286).

Covers:
- single-method splice writes via `<file>.tmp` + atomic rename
- multi-method splice in seq order; descending-span application keeps offsets stable
- new-class entries write the full source to a non-existing target
- external-edit conflict detection (prev_source no longer matches the recorded span)
- target-already-exists conflict for new-class entries
- per-file isolation: a conflict on one file leaves siblings unwritten
- `flush:` filter by class and by file
- entries are marked flushed after a successful flush (excluded from active view)
- pure `splice/3` helper round-trip
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

flush_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun empty_log_returns_empty_summary/1,
        fun single_method_splice_writes_atomically/1,
        fun shadowed_entry_is_marked_flushed/1,
        fun multi_method_splice_in_one_file/1,
        fun multiple_files_each_flushed/1,
        fun new_class_writes_file/1,
        fun new_class_target_exists_is_conflict/1,
        fun external_edit_is_conflict/1,
        fun conflict_leaves_no_tmp_files/1,
        fun conflict_does_not_mark_entries_flushed/1,
        fun mark_flushed_excludes_from_active/1,
        fun filter_by_class/1,
        fun filter_by_file/1,
        fun filter_by_new_class_selector/1,
        %% ADR 0082 Phase 4 (BT-2290): flushKinds: surface
        fun flush_kinds_by_entry_kind/1,
        fun flush_kinds_by_author_kind/1,
        fun flush_kinds_combined_dimensions_intersect/1,
        fun flush_kinds_empty_set_is_rejected/1,
        fun flush_kinds_unknown_kind_is_rejected/1,
        %% Additional coverage (BT runtime coverage push)
        fun selector_not_found_appends_method/1,
        fun selector_not_found_appends_to_file_without_trailing_newline/1,
        fun source_file_unreadable_is_conflict/1,
        fun filter_by_class_object/1,
        fun filter_by_class_object_non_class_is_error/1,
        fun filter_by_selector_symbol/1,
        fun flush_kinds_by_class_entry_kind/1,
        fun flush_kinds_by_human_author_kind/1,
        fun filter_dict_with_list_file/1,
        fun filter_dict_missing_file_key_is_error/1,
        fun filter_non_class_object_atom_lowercase_is_selector/1,
        fun bad_filter_type_is_error/1,
        fun flush_kinds_non_list_is_error/1,
        fun flush_kinds_non_symbol_element_is_error/1,
        fun filter_by_binary_class_name/1,
        fun append_into_empty_file/1,
        fun mixed_span_and_appended_method_in_one_file/1,
        fun missing_source_body_is_hard_error/1,
        fun missing_prev_source_body_is_hard_error/1
    ]}.

unit_test_() ->
    [
        fun splice_is_pure_byte_replace/0,
        fun splice_at_start_of_file/0,
        fun splice_at_end_of_file/0,
        fun group_by_file_preserves_seq_order/0,
        fun filter_shadowed_keeps_only_renamed_survivors/0,
        fun filter_shadowed_drops_unrenamed_survivors/0
    ].

new_class_directory_target_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun new_class_target_is_directory_is_conflict/1}.

mark_flushed_failure_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun mark_flushed_failure_is_reported/1}.

setup() ->
    {WorkspaceId, TmpHome, OldHome} = fresh_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    ProjDir = filename:join(TmpHome, "proj"),
    ok = filelib:ensure_path(filename:join(ProjDir, "src")),
    #{
        pid => Pid,
        workspace_id => WorkspaceId,
        tmp_home => TmpHome,
        old_home => OldHome,
        proj_dir => ProjDir
    }.

cleanup(#{pid := Pid, tmp_home := TmpHome, old_home := OldHome}) ->
    stop(Pid),
    restore_home(OldHome),
    del_tree(TmpHome),
    ok.

%%====================================================================
%% Tests — flush()
%%====================================================================

empty_log_returns_empty_summary(_Ctx) ->
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual([], maps:get(files, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary))
    ].

single_method_splice_writes_atomically(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    %% Span covers `value => 0\n` — find it dynamically.
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    NewBody = <<"value => 42\n">>,
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, NewBody, OldBody, list_to_binary(File), Start, End
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    Expected = <<"Object subclass: Counter\n  value => 42\nend\n">>,
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual([list_to_binary(File)], maps:get(files, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary)),
        ?_assertEqual(Expected, Final),
        %% No .tmp left over after rename.
        ?_assertEqual(false, filelib:is_regular(File ++ ".tmp"))
    ].

shadowed_entry_is_marked_flushed(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    %% Two appends to the same (Counter, value): first 42, then 99.
    {ok, Seq0} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 42\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    {ok, Seq1} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 99\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    %% After flush both seqs are marked flushed; the active view is empty.
    Active = beamtalk_workspace_changelog:active_entries(),
    [
        %% Only one *applied* entry counts (the survivor); the shadow drops out.
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(<<"Object subclass: Counter\n  value => 99\nend\n">>, Final),
        ?_assertEqual([], Active),
        %% Both entries are recorded flushed on disk.
        ?_assertEqual(
            true,
            entry_flushed(Seq0) andalso entry_flushed(Seq1)
        )
    ].

multi_method_splice_in_one_file(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<
        "Object subclass: Counter\n"
        "  value => 0\n"
        "  step => 1\n"
        "end\n"
    >>,
    ok = file:write_file(File, Original),
    {S0, E0, B0} = locate(Original, <<"value => 0\n">>),
    {S1, E1, B1} = locate(Original, <<"step => 1\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 42\n">>, B0, list_to_binary(File), S0, E0
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"step">>, <<"step => 7\n">>, B1, list_to_binary(File), S1, E1
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    Expected = <<
        "Object subclass: Counter\n"
        "  value => 42\n"
        "  step => 7\n"
        "end\n"
    >>,
    [
        ?_assertEqual(2, maps:get(flushed, Summary)),
        ?_assertEqual([list_to_binary(File)], maps:get(files, Summary)),
        ?_assertEqual(Expected, Final)
    ].

multiple_files_each_flushed(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Source1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Source2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Source1),
    ok = file:write_file(File2, Source2),
    {Start1, End1, Old1} = locate(Source1, <<"value => 0\n">>),
    {Start2, End2, Old2} = locate(Source2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            Old1,
            list_to_binary(File1),
            Start1,
            End1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>,
            <<"render">>,
            <<"render => 'x'\n">>,
            Old2,
            list_to_binary(File2),
            Start2,
            End2
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final1} = file:read_file(File1),
    {ok, Final2} = file:read_file(File2),
    Files = maps:get(files, Summary),
    [
        ?_assertEqual(2, maps:get(flushed, Summary)),
        ?_assertEqual(2, length(Files)),
        ?_assert(lists:member(list_to_binary(File1), Files)),
        ?_assert(lists:member(list_to_binary(File2), Files)),
        ?_assertEqual(<<"Object subclass: Counter\n  value => 1\nend\n">>, Final1),
        ?_assertEqual(<<"Object subclass: Widget\n  render => 'x'\nend\n">>, Final2)
    ].

new_class_writes_file(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "greeter.bt"]),
    NewSource = <<"Object subclass: Greeter\n  hi => 'hello'\nend\n">>,
    {ok, _} = beamtalk_workspace_changelog:append(
        new_class_input(
            <<"Greeter">>, NewSource, list_to_binary(File)
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(1, maps:get(newClasses, Summary)),
        ?_assertEqual(NewSource, Final)
    ].

new_class_target_exists_is_conflict(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "greeter.bt"]),
    ok = file:write_file(File, <<"Object subclass: Greeter\n  // external\nend\n">>),
    NewSource = <<"Object subclass: Greeter\n  hi => 'hello'\nend\n">>,
    {ok, Seq} = beamtalk_workspace_changelog:append(
        new_class_input(
            <<"Greeter">>, NewSource, list_to_binary(File)
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    Conflicts = maps:get(conflicts, Summary),
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual(1, length(Conflicts)),
        ?_assertEqual(<<"target_exists">>, maps:get(reason, hd(Conflicts))),
        ?_assert(lists:member(Seq, maps:get(seqs, hd(Conflicts)))),
        %% Disk unchanged.
        ?_assertEqual(
            {ok, <<"Object subclass: Greeter\n  // external\nend\n">>}, file:read_file(File)
        )
    ].

external_edit_is_conflict(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 42\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    %% Simulate an external edit: someone changed the bytes inside the span
    %% out from under us.
    Edited = <<"Object subclass: Counter\n  value => 9\nend\n">>,
    ok = file:write_file(File, Edited),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    Conflicts = maps:get(conflicts, Summary),
    %% Conflict is detected either as external_edit (when sizes still match)
    %% or as span_out_of_range (when the edit changed the span's length).
    Reasons = [maps:get(reason, C) || C <- Conflicts],
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual(1, length(Conflicts)),
        ?_assert(
            lists:member(<<"external_edit">>, Reasons) orelse
                lists:member(<<"span_out_of_range">>, Reasons)
        ),
        %% Disk unchanged.
        ?_assertEqual({ok, Edited}, file:read_file(File))
    ].

conflict_leaves_no_tmp_files(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            B1,
            list_to_binary(File1),
            S1,
            E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>,
            <<"render">>,
            <<"render => 'x'\n">>,
            B2,
            list_to_binary(File2),
            S2,
            E2
        )
    ),
    %% Edit File2 externally so its splice fails Phase A.
    ok = file:write_file(File2, <<"Object subclass: Widget\n  render => '!'\nend\n">>),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    [
        ?_assertEqual(false, filelib:is_regular(File1 ++ ".tmp")),
        ?_assertEqual(false, filelib:is_regular(File2 ++ ".tmp")),
        %% File1 must be unchanged because the *whole flush* aborted in Phase A.
        ?_assertEqual({ok, Original1}, file:read_file(File1))
    ].

conflict_does_not_mark_entries_flushed(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    {ok, _Seq} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    %% External edit so flush conflicts.
    ok = file:write_file(File, <<"Object subclass: Counter\n  value => 9\nend\n">>),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    Active = beamtalk_workspace_changelog:active_entries(),
    [
        ?_assertEqual(1, length(Active))
    ].

mark_flushed_excludes_from_active(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 42\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    BeforeActive = length(beamtalk_workspace_changelog:active_entries()),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    AfterActive = length(beamtalk_workspace_changelog:active_entries()),
    AllEntries = length(beamtalk_workspace_changelog:entries()),
    [
        ?_assertEqual(1, BeforeActive),
        ?_assertEqual(0, AfterActive),
        %% Entry stays in the log for audit.
        ?_assertEqual(1, AllEntries)
    ].

filter_by_class(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, B1, list_to_binary(File1), S1, E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>, <<"render">>, <<"render => 'x'\n">>, B2, list_to_binary(File2), S2, E2
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush('Counter'),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        %% Counter is flushed; Widget remains pending.
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File1)
        ),
        ?_assertEqual({ok, Original2}, file:read_file(File2))
    ].

filter_by_file(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, B1, list_to_binary(File1), S1, E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>, <<"render">>, <<"render => 'x'\n">>, B2, list_to_binary(File2), S2, E2
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(#{file => list_to_binary(File2)}),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual({ok, Original1}, file:read_file(File1)),
        ?_assertEqual(
            {ok, <<"Object subclass: Widget\n  render => 'x'\nend\n">>}, file:read_file(File2)
        )
    ].

filter_by_new_class_selector(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "greeter.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File1, Original1),
    {Start, End, OldBody} = locate(Original1, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            OldBody,
            list_to_binary(File1),
            Start,
            End
        )
    ),
    NewSource = <<"Object subclass: Greeter\n  hi => 'hi'\nend\n">>,
    {ok, _} = beamtalk_workspace_changelog:append(
        new_class_input(
            <<"Greeter">>, NewSource, list_to_binary(File2)
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush('new-class'),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(1, maps:get(newClasses, Summary)),
        %% Counter is left untouched.
        ?_assertEqual({ok, Original1}, file:read_file(File1)),
        ?_assertEqual({ok, NewSource}, file:read_file(File2))
    ].

%%====================================================================
%% Tests — substantive Copilot fixes (PR #2325, BT-2286)
%%====================================================================

%% Fix #2: target path that already exists *as a directory* must surface as
%% a `target_exists` conflict during Phase A, not as a `rename_failed` later
%% on. Mirrors the BT-2285 fix in beamtalk_repl_loader:validate_target_path/1.
new_class_target_is_directory_is_conflict(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "greeter.bt"]),
    %% Create a directory *at the target path*. filelib:is_regular/1 would
    %% return false (so the old code would proceed and later fail rename);
    %% file:read_file_info/1 returns {ok, FileInfo} so we catch it up front.
    ok = filelib:ensure_path(File),
    ?assertEqual(true, filelib:is_dir(File)),
    NewSource = <<"Object subclass: Greeter\n  hi => 'hello'\nend\n">>,
    {ok, Seq} = beamtalk_workspace_changelog:append(
        new_class_input(<<"Greeter">>, NewSource, list_to_binary(File))
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    Conflicts = maps:get(conflicts, Summary),
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual(1, length(Conflicts)),
        ?_assertEqual(<<"target_exists">>, maps:get(reason, hd(Conflicts))),
        ?_assert(lists:member(Seq, maps:get(seqs, hd(Conflicts)))),
        %% Directory remains as-is — no .tmp left behind either.
        ?_assertEqual(true, filelib:is_dir(File)),
        ?_assertEqual(false, filelib:is_regular(File ++ ".tmp"))
    ].

%% Fix #3: when mark_flushed/1 returns {error, _} after successful renames,
%% the success summary must surface that failure (as a conflict-like entry)
%% rather than silently swallowing it. Otherwise disk has moved on but the
%% ChangeLog still shows the entries as pending — next flush hits
%% external_edit on its own writes.
mark_flushed_failure_is_reported(#{proj_dir := ProjDir, pid := Pid}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 42\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    %% Simulate a marker failure by routing through the exported
    %% complete_flush/4 helper with seqs that the (live) ChangeLog server
    %% cannot mark — pass a manually-crafted scenario by killing the
    %% ChangeLog gen_server before calling mark_flushed.
    stop(Pid),
    %% Without a live ChangeLog server, mark_flushed/1 errors via the
    %% gen_server:call mechanism. complete_flush/4 must wrap that into the
    %% summary as a flush_marker_failed conflict.
    Files = [list_to_binary(File)],
    Renamed = [],
    Failed = [],
    Seqs = [1],
    %% Trap exits so the failed gen_server:call does not nuke this test
    %% process — complete_flush/4 should catch the exit and turn it into
    %% the marker conflict.
    process_flag(trap_exit, true),
    Result =
        try
            beamtalk_workspace_flush:complete_flush(Files, Renamed, Failed, Seqs)
        catch
            exit:_ -> caller_did_not_handle
        end,
    [
        ?_assertMatch({ok, _}, Result),
        ?_assert(
            case Result of
                {ok, S} ->
                    Conflicts = maps:get(conflicts, S),
                    lists:any(
                        fun(C) ->
                            maps:get(reason, C, undefined) =:= <<"flush_marker_failed">>
                        end,
                        Conflicts
                    );
                _ ->
                    false
            end
        )
    ].

%% Fix #1: when Phase B aborts mid-loop, shadowed entries whose survivor
%% never reached disk must be excluded from the mark-flushed set. The pure
%% helpers `renamed_target_keys/1` and `filter_shadowed_by_survivor/2` are
%% the seam — exercise them directly with synthetic entries.
filter_shadowed_keeps_only_renamed_survivors() ->
    %% Set up via the real changelog so the entries have the right shape.
    {WorkspaceId, TmpHome, OldHome} = fresh_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    try
        %% Two entries for (Counter, value): the survivor (newer) and a shadow.
        %% One unrelated entry for (Widget, render) that never gets renamed.
        File1 = <<"/proj/src/counter.bt">>,
        File2 = <<"/proj/src/widget.bt">>,
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Counter">>, <<"value">>, <<"new1">>, <<"old1">>, File1, 0, 4
            )
        ),
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Counter">>, <<"value">>, <<"new2">>, <<"old2">>, File1, 0, 4
            )
        ),
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Widget">>, <<"render">>, <<"new3">>, <<"old3">>, File2, 0, 4
            )
        ),
        AllPending = beamtalk_workspace_changelog:flushable_pending(),
        %% Survivor for (Counter, value) is the newest one. Build "renamed
        %% survivor" set as if File1 was renamed but File2 was not.
        CounterEntries = [
            E
         || E <- AllPending,
            beamtalk_workspace_changelog:entry_class(E) =:= <<"Counter">>
        ],
        RenamedSurvivor =
            hd(
                lists:sort(
                    fun(A, B) ->
                        beamtalk_workspace_changelog:entry_seq(A) >
                            beamtalk_workspace_changelog:entry_seq(B)
                    end,
                    CounterEntries
                )
            ),
        Shadowed = [
            E
         || E <- CounterEntries,
            beamtalk_workspace_changelog:entry_seq(E) =/=
                beamtalk_workspace_changelog:entry_seq(RenamedSurvivor)
        ],
        ?assertEqual(1, length(Shadowed)),
        Keys = beamtalk_workspace_flush:renamed_target_keys([RenamedSurvivor]),
        Kept = beamtalk_workspace_flush:filter_shadowed_by_survivor(Shadowed, Keys),
        ?assertEqual(1, length(Kept))
    after
        stop(Pid),
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

filter_shadowed_drops_unrenamed_survivors() ->
    {WorkspaceId, TmpHome, OldHome} = fresh_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    try
        File2 = <<"/proj/src/widget.bt">>,
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Widget">>, <<"render">>, <<"new1">>, <<"old1">>, File2, 0, 4
            )
        ),
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Widget">>, <<"render">>, <<"new2">>, <<"old2">>, File2, 0, 4
            )
        ),
        AllPending = beamtalk_workspace_changelog:flushable_pending(),
        Sorted = lists:sort(
            fun(A, B) ->
                beamtalk_workspace_changelog:entry_seq(A) >
                    beamtalk_workspace_changelog:entry_seq(B)
            end,
            AllPending
        ),
        [_Survivor | Shadowed] = Sorted,
        ?assertEqual(1, length(Shadowed)),
        %% No survivor was renamed: the empty set must exclude the shadow.
        EmptyKeys = beamtalk_workspace_flush:renamed_target_keys([]),
        Kept = beamtalk_workspace_flush:filter_shadowed_by_survivor(Shadowed, EmptyKeys),
        ?assertEqual([], Kept)
    after
        stop(Pid),
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

%%====================================================================
%% ADR 0082 Phase 4 (BT-2290): flushKinds: filter
%%====================================================================
%% Each test sets up multiple ChangeEntries with mixed kinds and author_kinds,
%% calls `flush_kinds/1` with a Symbol set, and asserts that exactly the
%% matching entries were written to disk and the others remain pending.

flush_kinds_by_entry_kind(#{proj_dir := ProjDir}) ->
    %% One method-patch entry (`instance` kind) + one new-class entry.
    %% `flushKinds: #{#instance}` must write only the method patch.
    InstanceFile = filename:join([ProjDir, "src", "counter.bt"]),
    NewClassFile = filename:join([ProjDir, "src", "greeter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(InstanceFile, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            Old,
            list_to_binary(InstanceFile),
            S,
            E
        )
    ),
    NewSource = <<"Object subclass: Greeter\n  hi => 'hi'\nend\n">>,
    {ok, _} = beamtalk_workspace_changelog:append(
        new_class_input(<<"Greeter">>, NewSource, list_to_binary(NewClassFile))
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush_kinds([instance]),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        %% The new-class file is NOT written because its kind is `'new-class'`.
        ?_assertEqual(false, filelib:is_regular(NewClassFile)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>},
            file:read_file(InstanceFile)
        ),
        %% The new-class entry remains pending after a partial flush.
        ?_assertEqual(1, length(beamtalk_workspace_changelog:active_entries()))
    ].

flush_kinds_by_author_kind(#{proj_dir := ProjDir}) ->
    %% Two method patches against different files, one human-authored, one
    %% agent-authored. `flushKinds: #{#agent}` must write only the agent entry.
    HumanFile = filename:join([ProjDir, "src", "counter.bt"]),
    AgentFile = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(HumanFile, Original1),
    ok = file:write_file(AgentFile, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            B1,
            list_to_binary(HumanFile),
            S1,
            E1
        )
    ),
    AgentEntry = (method_input(
        <<"Widget">>,
        <<"render">>,
        <<"render => 'x'\n">>,
        B2,
        list_to_binary(AgentFile),
        S2,
        E2
    ))#{
        author_kind => agent, author => <<"agent-1">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(AgentEntry),
    {ok, Summary} = beamtalk_workspace_flush:flush_kinds([agent]),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual({ok, Original1}, file:read_file(HumanFile)),
        ?_assertEqual(
            {ok, <<"Object subclass: Widget\n  render => 'x'\nend\n">>},
            file:read_file(AgentFile)
        )
    ].

flush_kinds_combined_dimensions_intersect(#{proj_dir := ProjDir}) ->
    %% `flushKinds: #{#agent, #'new-class'}` must select only entries that are
    %% BOTH agent-authored AND new-class — a per-dimension AND of constraints.
    AgentInstanceFile = filename:join([ProjDir, "src", "counter.bt"]),
    HumanNewClassFile = filename:join([ProjDir, "src", "widget.bt"]),
    AgentNewClassFile = filename:join([ProjDir, "src", "greeter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(AgentInstanceFile, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    %% (1) Agent + instance — fails the new-class dimension.
    AgentInstance = (method_input(
        <<"Counter">>,
        <<"value">>,
        <<"value => 1\n">>,
        Old,
        list_to_binary(AgentInstanceFile),
        S,
        E
    ))#{
        author_kind => agent, author => <<"agent">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(AgentInstance),
    %% (2) Human + new-class — fails the author-kind dimension.
    HumanNewSrc = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    HumanNewClass = (new_class_input(
        <<"Widget">>, HumanNewSrc, list_to_binary(HumanNewClassFile)
    ))#{
        author_kind => human, author => <<"human">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(HumanNewClass),
    %% (3) Agent + new-class — passes both dimensions.
    AgentNewSrc = <<"Object subclass: Greeter\n  hi => 'hi'\nend\n">>,
    AgentNewClass = (new_class_input(
        <<"Greeter">>, AgentNewSrc, list_to_binary(AgentNewClassFile)
    ))#{
        author_kind => agent, author => <<"agent">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(AgentNewClass),
    {ok, Summary} = beamtalk_workspace_flush:flush_kinds([agent, 'new-class']),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(1, maps:get(newClasses, Summary)),
        ?_assertEqual({ok, Original}, file:read_file(AgentInstanceFile)),
        ?_assertEqual(false, filelib:is_regular(HumanNewClassFile)),
        ?_assertEqual({ok, AgentNewSrc}, file:read_file(AgentNewClassFile))
    ].

flush_kinds_empty_set_is_rejected(_Ctx) ->
    Result = beamtalk_workspace_flush:flush_kinds([]),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

flush_kinds_unknown_kind_is_rejected(_Ctx) ->
    Result = beamtalk_workspace_flush:flush_kinds([bogus, also_bogus]),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%%====================================================================
%% Additional coverage: selector_not_found append path + filter shapes
%%====================================================================

%% A splice entry with span = undefined (the install hook recorded
%% `selector_not_found` — a brand-new method added live). flush must append the
%% new body after a blank-line separator rather than overwrite a span.
selector_not_found_appends_method(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    %% File already ends in a single trailing newline.
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    %% No span, no prev_source ⇒ append path.
    {ok, _} = beamtalk_workspace_changelog:append(
        append_method_input(
            <<"Counter">>, <<"step">>, <<"step => 1\n">>, list_to_binary(File)
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary)),
        %% Trailing newlines collapse to one, then a blank line, then the body.
        ?_assertEqual(
            <<"Object subclass: Counter\n  value => 0\nend\n\nstep => 1\n">>, Final
        )
    ].

%% Same append path, but the on-disk file has NO trailing newline — exercises
%% the strip/ensure-trailing-newline branches differently.
selector_not_found_appends_to_file_without_trailing_newline(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend">>,
    ok = file:write_file(File, Original),
    %% New body also has no trailing newline ⇒ ensure_trailing_newline adds one.
    {ok, _} = beamtalk_workspace_changelog:append(
        append_method_input(
            <<"Counter">>, <<"step">>, <<"step => 1">>, list_to_binary(File)
        )
    ),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    [
        ?_assertEqual(
            <<"Object subclass: Counter\n  value => 0\nend\n\nstep => 1\n">>, Final
        )
    ].

%% prepare_splice on a file that does not exist on disk ⇒ source_file_unreadable
%% conflict (the read fails with enoent).
source_file_unreadable_is_conflict(#{proj_dir := ProjDir}) ->
    %% Reference a file that was never written to disk.
    File = filename:join([ProjDir, "src", "missing.bt"]),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            <<"value => 0\n">>,
            list_to_binary(File),
            0,
            11
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    Conflicts = maps:get(conflicts, Summary),
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual(1, length(Conflicts)),
        ?_assertEqual(<<"source_file_unreadable">>, maps:get(reason, hd(Conflicts)))
    ].

%% Filter by a class *object* (`#beamtalk_object{class = 'Counter class'}`):
%% normalise_filter strips the " class" suffix and matches the entry's class.
filter_by_class_object(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, B1, list_to_binary(File1), S1, E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>, <<"render">>, <<"render => 'x'\n">>, B2, list_to_binary(File2), S2, E2
        )
    ),
    %% The 'Counter class' atom must exist for class_display_name to resolve.
    ClassObj = #beamtalk_object{
        class = binary_to_atom(<<"Counter class">>, utf8),
        class_mod = counter,
        pid = self()
    },
    {ok, Summary} = beamtalk_workspace_flush:flush(ClassObj),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File1)
        ),
        ?_assertEqual({ok, Original2}, file:read_file(File2))
    ].

%% A `#beamtalk_object{}` whose class atom is NOT a class-object name (no
%% " class" suffix) is rejected with a structured error.
filter_by_class_object_non_class_is_error(_Ctx) ->
    NonClass = #beamtalk_object{class = plain_atom, class_mod = m, pid = self()},
    Result = beamtalk_workspace_flush:flush(NonClass),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%% Filter by a selector Symbol (a lowercase atom): selects entries whose
%% selector matches, regardless of class.
filter_by_selector_symbol(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, B1, list_to_binary(File1), S1, E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>, <<"render">>, <<"render => 'x'\n">>, B2, list_to_binary(File2), S2, E2
        )
    ),
    %% `render` is a lowercase atom ⇒ treated as a selector filter.
    {ok, Summary} = beamtalk_workspace_flush:flush(render),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual({ok, Original1}, file:read_file(File1)),
        ?_assertEqual(
            {ok, <<"Object subclass: Widget\n  render => 'x'\nend\n">>}, file:read_file(File2)
        )
    ].

%% classify_kind(class) -> entry: exercise the `#class` entry-kind branch.
flush_kinds_by_class_entry_kind(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    %% A class-side method patch (kind => class).
    ClassEntry = (method_input(
        <<"Counter">>, <<"value">>, <<"value => 1\n">>, Old, list_to_binary(File), S, E
    ))#{
        kind => class
    },
    {ok, _} = beamtalk_workspace_changelog:append(ClassEntry),
    {ok, Summary} = beamtalk_workspace_flush:flush_kinds([class]),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File)
        )
    ].

%% classify_kind(human) -> author: exercise the `#human` author-kind branch.
flush_kinds_by_human_author_kind(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    %% method_input defaults author_kind => human.
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, Old, list_to_binary(File), S, E
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush_kinds([human]),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File)
        )
    ].

%% Dictionary filter where the file value is a *list* (string), not a binary.
filter_dict_with_list_file(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, Old, list_to_binary(File), S, E
        )
    ),
    %% file => File as a *string* (list of chars), not a binary.
    {ok, Summary} = beamtalk_workspace_flush:flush(#{file => File}),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File)
        )
    ].

%% Dictionary filter missing the `file` key is rejected.
filter_dict_missing_file_key_is_error(_Ctx) ->
    Result = beamtalk_workspace_flush:flush(#{not_file => <<"x">>}),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%% A lowercase atom that is not a class name is treated as a selector — with no
%% matching entries the flush is an empty (but successful) no-op.
filter_non_class_object_atom_lowercase_is_selector(_Ctx) ->
    {ok, Summary} = beamtalk_workspace_flush:flush(someselector),
    [
        ?_assertEqual(0, maps:get(flushed, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary))
    ].

%% A filter that is neither class/symbol/binary/map/object is rejected.
bad_filter_type_is_error(_Ctx) ->
    Result = beamtalk_workspace_flush:flush(12345),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%% flush_kinds/1 with a non-list argument is rejected.
flush_kinds_non_list_is_error(_Ctx) ->
    Result = beamtalk_workspace_flush:flush_kinds(not_a_list),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%% flush_kinds/1 with a non-Symbol element (e.g. a binary) is rejected.
flush_kinds_non_symbol_element_is_error(_Ctx) ->
    Result = beamtalk_workspace_flush:flush_kinds([<<"instance">>]),
    [
        ?_assertMatch({error, #beamtalk_error{kind = type_error}}, Result)
    ].

%% Filter by a binary class name (the `normalise_filter(Bin)` clause).
filter_by_binary_class_name(#{proj_dir := ProjDir}) ->
    File1 = filename:join([ProjDir, "src", "counter.bt"]),
    File2 = filename:join([ProjDir, "src", "widget.bt"]),
    Original1 = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    Original2 = <<"Object subclass: Widget\n  render => nil\nend\n">>,
    ok = file:write_file(File1, Original1),
    ok = file:write_file(File2, Original2),
    {S1, E1, B1} = locate(Original1, <<"value => 0\n">>),
    {S2, E2, B2} = locate(Original2, <<"render => nil\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 1\n">>, B1, list_to_binary(File1), S1, E1
        )
    ),
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Widget">>, <<"render">>, <<"render => 'x'\n">>, B2, list_to_binary(File2), S2, E2
        )
    ),
    %% Pass the class name as a binary directly.
    {ok, Summary} = beamtalk_workspace_flush:flush(<<"Counter">>),
    [
        ?_assertEqual(1, maps:get(flushed, Summary)),
        ?_assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 1\nend\n">>}, file:read_file(File1)
        ),
        ?_assertEqual({ok, Original2}, file:read_file(File2))
    ].

%% Appending a method to an empty (zero-byte) file exercises the empty-binary
%% branches of strip_trailing_newlines/1 and ensure_trailing_newline/1.
append_into_empty_file(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "empty.bt"]),
    ok = file:write_file(File, <<>>),
    {ok, _} = beamtalk_workspace_changelog:append(
        append_method_input(
            <<"Empty">>, <<"go">>, <<"go => 1\n">>, list_to_binary(File)
        )
    ),
    {ok, _Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    [
        %% Empty body trimmed to <<>>, then "\n\n" separator, then the appended body.
        ?_assertEqual(<<"\n\ngo => 1\n">>, Final)
    ].

%% One file with both a span-based splice entry AND a span=undefined appended
%% method entry. apply_splices sorts the two, exercising span_start/1 for the
%% undefined case (returns -1) so it sorts before the real span.
mixed_span_and_appended_method_in_one_file(#{proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {S, E, Old} = locate(Original, <<"value => 0\n">>),
    %% Splice entry (has a span).
    {ok, _} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>, <<"value">>, <<"value => 9\n">>, Old, list_to_binary(File), S, E
        )
    ),
    %% Appended-method entry against the same file (no span).
    {ok, _} = beamtalk_workspace_changelog:append(
        append_method_input(
            <<"Counter">>, <<"step">>, <<"step => 1\n">>, list_to_binary(File)
        )
    ),
    {ok, Summary} = beamtalk_workspace_flush:flush(),
    {ok, Final} = file:read_file(File),
    [
        ?_assertEqual(2, maps:get(flushed, Summary)),
        ?_assertEqual([], maps:get(conflicts, Summary)),
        %% The span splice replaced `value => 0`, and the appended method lands
        %% after a blank-line separator at the end.
        ?_assertEqual(
            <<"Object subclass: Counter\n  value => 9\nend\n\nstep => 1\n">>, Final
        )
    ].

%% When the recorded patch body in sources/ is unreadable (deleted out from
%% under us), flush surfaces a hard {error, #beamtalk_error{}} via
%% source_body_error/2 + wrap_io_error/2. Exercise the append (span=undefined)
%% path so read_source_body is the first read that fails.
missing_source_body_is_hard_error(#{workspace_id := WsId, proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    ok = file:write_file(File, <<"Object subclass: Counter\nend\n">>),
    {ok, Seq} = beamtalk_workspace_changelog:append(
        append_method_input(
            <<"Counter">>, <<"step">>, <<"step => 1\n">>, list_to_binary(File)
        )
    ),
    %% Delete the recorded source body so read_source_body/1 fails with enoent.
    ChangesDir = beamtalk_workspace_changelog:changes_dir(WsId),
    SourceBody = filename:join([ChangesDir, "sources", source_ref_name(Seq, "-source.bt")]),
    ok = file:delete(SourceBody),
    Result = beamtalk_workspace_flush:flush(),
    [
        ?_assertMatch({error, #beamtalk_error{kind = source_body_unreadable}}, Result)
    ].

%% When the recorded *prior* body in sources/ is unreadable, a span splice
%% surfaces a hard {error, #beamtalk_error{}} via prev_source_error/2.
missing_prev_source_body_is_hard_error(#{workspace_id := WsId, proj_dir := ProjDir}) ->
    File = filename:join([ProjDir, "src", "counter.bt"]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, End, OldBody} = locate(Original, <<"value => 0\n">>),
    {ok, Seq} = beamtalk_workspace_changelog:append(
        method_input(
            <<"Counter">>,
            <<"value">>,
            <<"value => 1\n">>,
            OldBody,
            list_to_binary(File),
            Start,
            End
        )
    ),
    %% Delete the recorded prev body so read_prev_source_body/1 fails.
    ChangesDir = beamtalk_workspace_changelog:changes_dir(WsId),
    PrevBody = filename:join([ChangesDir, "sources", source_ref_name(Seq, "-prev.bt")]),
    ok = file:delete(PrevBody),
    Result = beamtalk_workspace_flush:flush(),
    [
        ?_assertMatch({error, #beamtalk_error{kind = prev_source_unreadable}}, Result)
    ].

%%====================================================================
%% Pure helpers
%%====================================================================

splice_is_pure_byte_replace() ->
    Body = <<"hello world">>,
    Result = beamtalk_workspace_flush:splice(Body, {6, 11}, <<"there">>),
    ?assertEqual(<<"hello there">>, Result).

splice_at_start_of_file() ->
    Body = <<"abcdef">>,
    Result = beamtalk_workspace_flush:splice(Body, {0, 3}, <<"XYZ">>),
    ?assertEqual(<<"XYZdef">>, Result).

splice_at_end_of_file() ->
    Body = <<"abcdef">>,
    Result = beamtalk_workspace_flush:splice(Body, {3, 6}, <<"XYZ">>),
    ?assertEqual(<<"abcXYZ">>, Result).

group_by_file_preserves_seq_order() ->
    %% Append two entries against File1, one against File2; after grouping the
    %% File1 group should list them in seq-ascending order.
    {WorkspaceId, TmpHome, OldHome} = fresh_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    try
        File1 = <<"/proj/src/counter.bt">>,
        File2 = <<"/proj/src/widget.bt">>,
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Counter">>, <<"inc">>, <<"new1">>, <<"old1">>, File1, 0, 4
            )
        ),
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Widget">>, <<"render">>, <<"new2">>, <<"old2">>, File2, 0, 4
            )
        ),
        {ok, _} = beamtalk_workspace_changelog:append(
            method_input(
                <<"Counter">>, <<"dec">>, <<"new3">>, <<"old3">>, File1, 5, 9
            )
        ),
        Entries = beamtalk_workspace_changelog:flushable_pending(),
        Groups = beamtalk_workspace_flush:group_by_file(Entries),
        {value, {_, F1Entries}} = lists:search(
            fun({F, _}) -> F =:= File1 end, Groups
        ),
        Selectors = [beamtalk_workspace_changelog:entry_selector(E) || E <- F1Entries],
        ?assertEqual([<<"inc">>, <<"dec">>], Selectors)
    after
        stop(Pid),
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

%%====================================================================
%% Helpers
%%====================================================================

method_input(Class, Selector, NewBody, OldBody, File, Start, End) ->
    #{
        class => Class,
        selector => Selector,
        kind => instance,
        source => NewBody,
        prev_source => OldBody,
        intent => durable,
        flushable => true,
        author => <<"sess-test">>,
        author_kind => human,
        source_file => File,
        span => #{start => Start, 'end' => End}
    }.

%% A method entry recorded by the install hook as `selector_not_found`: a
%% brand-new method added live, with no span and no prev_source. flush appends
%% it to the file rather than splicing over an existing definition.
append_method_input(Class, Selector, NewBody, File) ->
    #{
        class => Class,
        selector => Selector,
        kind => instance,
        source => NewBody,
        intent => durable,
        flushable => true,
        author => <<"sess-test">>,
        author_kind => human,
        source_file => File
    }.

new_class_input(ClassName, Source, File) ->
    #{
        class => ClassName,
        kind => 'new-class',
        source => Source,
        intent => durable,
        flushable => true,
        author => <<"sess-test">>,
        author_kind => agent,
        source_file => File
    }.

%% Build the sources/ filename for a seq (mirrors the changelog's zero-padded
%% naming): "000042-source.bt" / "000042-prev.bt".
source_ref_name(Seq, Suffix) ->
    lists:flatten(io_lib:format("~6..0b~s", [Seq, Suffix])).

%% Find Needle in Haystack; return {Start, End, ActualBytes}. Crashes if not
%% found so the test fails loudly rather than silently miscomputing offsets.
locate(Haystack, Needle) ->
    {Start, Len} = binary:match(Haystack, Needle),
    {Start, Start + Len, Needle}.

%% Look up an entry by seq and report its flushed flag.
entry_flushed(Seq) ->
    [Entry] = [
        E
     || E <- beamtalk_workspace_changelog:entries(),
        beamtalk_workspace_changelog:entry_seq(E) =:= Seq
    ],
    beamtalk_workspace_changelog:entry_flushed(Entry).

fresh_workspace() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("test-ws-flush-" ++ Unique),
    Tmp = filename:join(temp_dir(), "bt-flush-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    {WorkspaceId, Tmp, OldHome}.

restore_home(false) -> os:unsetenv("HOME");
restore_home(OldHome) -> os:putenv("HOME", OldHome).

temp_dir() ->
    unicode:characters_to_list(beamtalk_file:'tempDirectory'()).

stop(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end.

del_tree(Path) ->
    case filelib:is_dir(Path) of
        true -> _ = file:del_dir_r(Path);
        false -> _ = file:delete(Path)
    end,
    ok.
