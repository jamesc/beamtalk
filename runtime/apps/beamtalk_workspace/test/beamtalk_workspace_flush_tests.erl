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
        fun filter_by_new_class_selector/1
    ]}.

unit_test_() ->
    [
        fun splice_is_pure_byte_replace/0,
        fun splice_at_start_of_file/0,
        fun splice_at_end_of_file/0,
        fun group_by_file_preserves_seq_order/0
    ].

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
