%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_changelog_tests).

-moduledoc """
Unit tests for beamtalk_workspace_changelog (ADR 0082 Phase 1, BT-2282).

Covers:
- append + sequence assignment + ETS state
- two-part on-disk layout (changes.jsonl metadata + sources/*.bt bodies)
- crash-safety (no .tmp leftovers; metadata references existing bodies)
- JSON round-trip of the ChangeEntry schema
- restart semantics: fresh epoch, prior-epoch tagging, orphan tagging
- bounded ring rotation with archive segments
- run-mode (no workspace_id): memory-only, no disk artifacts
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

%% Each test runs against a fresh temp workspace dir so disk state is isolated.
%% We drive the gen_server with a synthetic workspace_id and point HOME at the
%% temp dir, mirroring how a real workspace resolves <home>/.beamtalk/...
changelog_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun append_assigns_sequential_seqs/1,
        fun append_writes_two_part_layout/1,
        fun append_metadata_line_bounded_regardless_of_body_size/1,
        fun append_no_tmp_leftovers/1,
        fun append_new_class_has_no_prev_or_span/1,
        fun json_round_trip/1,
        fun active_excludes_nothing_in_fresh_session/1,
        fun change_entries_builds_tagged_maps/1,
        fun change_log_wraps_all_entries/1,
        fun change_entries_marks_active_flag/1,
        fun dirty_methods_groups_active_by_class/1,
        fun dirty_methods_uses_new_class_placeholder/1,
        fun append_returns_error_on_unwritable_dir/1,
        fun clear_empties_log/1,
        %% ADR 0082 Phase 4 (BT-2290)
        fun find_revert_target_returns_prev_body/1,
        fun find_revert_target_no_entry_when_unknown/1,
        fun find_revert_target_picks_most_recent_entry/1,
        fun find_revert_target_skips_flushed_entries/1,
        fun find_revert_target_rejects_new_class/1
    ]}.

%% FFI surface with no gen_server started: must degrade to empty, not crash.
ffi_no_server_test_() ->
    [
        fun change_entries_empty_when_table_absent/0,
        fun change_log_empty_when_table_absent/0,
        fun dirty_methods_empty_when_table_absent/0
    ].

%% These run with no gen_server started so they exercise the table-absent guards.
no_server_test_() ->
    [
        fun entries_returns_empty_when_table_absent/0,
        fun size_returns_zero_when_table_absent/0
    ].

setup() ->
    {WorkspaceId, TmpHome, OldHome} = fresh_workspace(),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    #{pid => Pid, workspace_id => WorkspaceId, tmp_home => TmpHome, old_home => OldHome}.

cleanup(#{pid := Pid, tmp_home := TmpHome, old_home := OldHome}) ->
    stop(Pid),
    restore_home(OldHome),
    del_tree(TmpHome),
    ok.

%%====================================================================
%% append
%%====================================================================

append_assigns_sequential_seqs(_Ctx) ->
    {ok, S0} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    {ok, S1} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
    {ok, S2} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"reset">>)),
    [
        ?_assertEqual(0, S0),
        ?_assertEqual(1, S1),
        ?_assertEqual(2, S2),
        ?_assertEqual(3, beamtalk_workspace_changelog:size())
    ].

append_writes_two_part_layout(#{workspace_id := WsId}) ->
    {ok, Seq} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    LogPath = filename:join(Dir, "changes.jsonl"),
    SourcePath = filename:join([Dir, "sources", seq_file(Seq, "-source.bt")]),
    PrevPath = filename:join([Dir, "sources", seq_file(Seq, "-prev.bt")]),
    {ok, LogBin} = file:read_file(LogPath),
    [Line | _] = binary:split(LogBin, <<"\n">>, [global, trim_all]),
    Decoded = json:decode(Line),
    [
        ?_assert(filelib:is_regular(SourcePath)),
        ?_assert(filelib:is_regular(PrevPath)),
        ?_assertEqual({ok, <<"^ self value + 1">>}, file:read_file(SourcePath)),
        ?_assertEqual({ok, <<"^ self value">>}, file:read_file(PrevPath)),
        ?_assertEqual(<<"Counter">>, maps:get(<<"class">>, Decoded)),
        ?_assertEqual(<<"inc">>, maps:get(<<"selector">>, Decoded)),
        ?_assertEqual(<<"instance">>, maps:get(<<"kind">>, Decoded)),
        ?_assertEqual(<<"durable">>, maps:get(<<"intent">>, Decoded)),
        ?_assertEqual(true, maps:get(<<"flushable">>, Decoded)),
        ?_assertEqual(<<"human">>, maps:get(<<"author_kind">>, Decoded))
    ].

%% The two-part layout's core invariant: the metadata line size is independent
%% of the method body size (bodies live in sources/, not in the JSON line). A
%% 50 KB body must not bloat the metadata line.
append_metadata_line_bounded_regardless_of_body_size(#{workspace_id := WsId}) ->
    HugeBody = binary:copy(<<"x">>, 50000),
    Input = (durable_input(<<"Counter">>, <<"inc">>))#{source => HugeBody},
    {ok, _} = beamtalk_workspace_changelog:append(Input),
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    {ok, LogBin} = file:read_file(filename:join(Dir, "changes.jsonl")),
    [Line | _] = binary:split(LogBin, <<"\n">>, [global, trim_all]),
    SourcePath = filename:join([Dir, "sources", seq_file(0, "-source.bt")]),
    [
        %% Metadata line stays small even with a 50 KB method body.
        ?_assert(byte_size(Line) < 500),
        %% ...and the full body is preserved in sources/.
        ?_assertEqual({ok, HugeBody}, file:read_file(SourcePath))
    ].

append_no_tmp_leftovers(#{workspace_id := WsId}) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    {ok, SourceFiles} = file:list_dir(filename:join(Dir, "sources")),
    Tmps = [F || F <- SourceFiles, lists:suffix(".tmp", F)],
    [?_assertEqual([], Tmps)].

append_new_class_has_no_prev_or_span(#{workspace_id := WsId}) ->
    Input = #{
        class => <<"NewThing">>,
        kind => 'new-class',
        source => <<"class NewThing\nend">>,
        intent => durable,
        flushable => true,
        author => <<"sess-1">>,
        author_kind => human,
        source_file => <<"/proj/src/new_thing.bt">>
    },
    {ok, Seq} = beamtalk_workspace_changelog:append(Input),
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    PrevPath = filename:join([Dir, "sources", seq_file(Seq, "-prev.bt")]),
    {ok, LogBin} = file:read_file(filename:join(Dir, "changes.jsonl")),
    [Line | _] = binary:split(LogBin, <<"\n">>, [global, trim_all]),
    Decoded = json:decode(Line),
    [
        ?_assertNot(filelib:is_regular(PrevPath)),
        ?_assertEqual(null, maps:get(<<"prev_source_ref">>, Decoded)),
        ?_assertEqual(null, maps:get(<<"span">>, Decoded)),
        ?_assertEqual(null, maps:get(<<"selector">>, Decoded)),
        ?_assertEqual(<<"new-class">>, maps:get(<<"kind">>, Decoded))
    ].

%%====================================================================
%% JSON round-trip
%%====================================================================

json_round_trip(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    [Entry] = beamtalk_workspace_changelog:entries(),
    Json = beamtalk_workspace_changelog:entry_to_json(Entry),
    Decoded = beamtalk_workspace_changelog:entry_from_json(Json),
    Reencoded = beamtalk_workspace_changelog:entry_to_json(Decoded),
    [?_assertEqual(Json, Reencoded)].

%%====================================================================
%% active vs all
%%====================================================================

active_excludes_nothing_in_fresh_session(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
    [
        ?_assertEqual(2, length(beamtalk_workspace_changelog:active_entries())),
        ?_assertEqual(2, length(beamtalk_workspace_changelog:entries()))
    ].

%%====================================================================
%% Beamtalk FFI surface (ADR 0082 Phase 1, BT-2284)
%%====================================================================

change_entries_builds_tagged_maps(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    [Entry] = beamtalk_workspace_changelog:change_entries(),
    [
        ?_assertEqual('ChangeEntry', maps:get('$beamtalk_class', Entry)),
        ?_assertEqual('Counter', maps:get(className, Entry)),
        ?_assertEqual(inc, maps:get(selector, Entry)),
        ?_assertEqual(instance, maps:get(kind, Entry)),
        ?_assertEqual(durable, maps:get(intent, Entry)),
        ?_assertEqual(true, maps:get(flushable, Entry)),
        ?_assertEqual(human, maps:get(authorKind, Entry)),
        ?_assertEqual(<<"/proj/src/counter.bt">>, maps:get(sourceFile, Entry)),
        ?_assertEqual(false, maps:get(orphan, Entry)),
        ?_assertEqual(false, maps:get(priorEpoch, Entry)),
        ?_assertEqual(true, maps:get(active, Entry))
    ].

change_log_wraps_all_entries(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
    Log = beamtalk_workspace_changelog:changeLog(),
    Entries = maps:get(entries, Log),
    [
        ?_assertEqual('ChangeLog', maps:get('$beamtalk_class', Log)),
        ?_assertEqual(2, length(Entries)),
        %% Oldest-first ordering preserved.
        ?_assertEqual(inc, maps:get(selector, hd(Entries)))
    ].

%% new-class entries surface a nil selector (not undefined) so Beamtalk reads it
%% as the nil object.
change_entries_marks_active_flag(_Ctx) ->
    NewClassInput = #{
        class => <<"NewThing">>,
        kind => 'new-class',
        source => <<"class NewThing\nend">>,
        intent => durable,
        flushable => true,
        author => <<"sess-1">>,
        author_kind => agent,
        source_file => <<"/proj/src/new_thing.bt">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(NewClassInput),
    [Entry] = beamtalk_workspace_changelog:change_entries(),
    [
        ?_assertEqual(nil, maps:get(selector, Entry)),
        ?_assertEqual('new-class', maps:get(kind, Entry)),
        ?_assertEqual(agent, maps:get(authorKind, Entry)),
        ?_assertEqual(true, maps:get(active, Entry))
    ].

dirty_methods_groups_active_by_class(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Widget">>, <<"render">>)),
    Dirty = beamtalk_workspace_changelog:dirtyMethods(),
    CounterSet = maps:get('Counter', Dirty),
    WidgetSet = maps:get('Widget', Dirty),
    [
        ?_assertEqual(2, map_size(Dirty)),
        %% Each value is a Beamtalk Set (tagged map with ordset elements).
        ?_assertEqual('Set', maps:get('$beamtalk_class', CounterSet)),
        ?_assertEqual([dec, inc], maps:get(elements, CounterSet)),
        ?_assertEqual([render], maps:get(elements, WidgetSet))
    ].

dirty_methods_uses_new_class_placeholder(_Ctx) ->
    NewClassInput = #{
        class => <<"DoubleCounter">>,
        kind => 'new-class',
        source => <<"class DoubleCounter\nend">>,
        intent => durable,
        flushable => true,
        author => <<"sess-1">>,
        author_kind => agent,
        source_file => <<"/proj/src/double_counter.bt">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(NewClassInput),
    Dirty = beamtalk_workspace_changelog:dirtyMethods(),
    Set = maps:get('DoubleCounter', Dirty),
    [
        ?_assertEqual(['new-class'], maps:get(elements, Set))
    ].

change_entries_empty_when_table_absent() ->
    ok = ensure_no_changelog_table(),
    ?assertEqual([], beamtalk_workspace_changelog:change_entries()).

change_log_empty_when_table_absent() ->
    ok = ensure_no_changelog_table(),
    Log = beamtalk_workspace_changelog:changeLog(),
    ?assertEqual('ChangeLog', maps:get('$beamtalk_class', Log)),
    ?assertEqual([], maps:get(entries, Log)).

dirty_methods_empty_when_table_absent() ->
    ok = ensure_no_changelog_table(),
    ?assertEqual(#{}, beamtalk_workspace_changelog:dirtyMethods()).

%%====================================================================
%% error path
%%====================================================================

append_returns_error_on_unwritable_dir(#{pid := Pid, workspace_id := WsId}) ->
    %% Make the sources dir a regular file so ensure_path fails.
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    SourcesPath = filename:join(Dir, "sources"),
    ok = file:write_file(SourcesPath, <<"not a dir">>),
    Result = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    %% Server survives the error.
    Alive = is_process_alive(Pid),
    [
        ?_assertMatch({error, #beamtalk_error{kind = changelog_write_error}}, Result),
        ?_assert(Alive)
    ].

clear_empties_log(#{workspace_id := WsId}) ->
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
    {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
    ok = beamtalk_workspace_changelog:clear(),
    Dir = beamtalk_workspace_changelog:changes_dir(WsId),
    {ok, LogBin} = file:read_file(filename:join(Dir, "changes.jsonl")),
    [
        ?_assertEqual(0, beamtalk_workspace_changelog:size()),
        ?_assertEqual(<<>>, LogBin)
    ].

%%====================================================================
%% find_revert_target/2 (ADR 0082 Phase 4, BT-2290)
%%====================================================================
%% Used by `ChangeLog>>revert:` to recover the prior body for a method.
%% The lookup walks the active entries (current epoch, not orphaned, not
%% already flushed), keeps only entries for the requested `(class, selector)`,
%% picks the highest-seq survivor, and reads its recorded prev_source body.

find_revert_target_returns_prev_body(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(
        durable_input(<<"Counter">>, <<"inc">>)
    ),
    Result = beamtalk_workspace_changelog:find_revert_target(<<"Counter">>, inc),
    [
        ?_assertMatch({ok, <<"^ self value">>, _Entry}, Result)
    ].

find_revert_target_no_entry_when_unknown(_Ctx) ->
    {ok, _} = beamtalk_workspace_changelog:append(
        durable_input(<<"Counter">>, <<"inc">>)
    ),
    %% Class matches but selector does not.
    NoSel = beamtalk_workspace_changelog:find_revert_target(<<"Counter">>, never),
    %% Selector matches but class does not.
    NoClass = beamtalk_workspace_changelog:find_revert_target(<<"Other">>, inc),
    [
        ?_assertEqual({error, no_entry}, NoSel),
        ?_assertEqual({error, no_entry}, NoClass)
    ].

find_revert_target_picks_most_recent_entry(_Ctx) ->
    %% Two patches against the same method — revert must return the most recent
    %% prev body, because that is the state immediately before the latest patch.
    %% In our fixture every append uses `prev_source = <<"^ self value">>`, so
    %% we vary the body via a custom input to make the comparison meaningful.
    Input1 = (durable_input(<<"Counter">>, <<"inc">>))#{
        source => <<"^ 1">>,
        prev_source => <<"^ 0">>
    },
    Input2 = (durable_input(<<"Counter">>, <<"inc">>))#{
        source => <<"^ 2">>,
        prev_source => <<"^ 1">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(Input1),
    {ok, _} = beamtalk_workspace_changelog:append(Input2),
    Result = beamtalk_workspace_changelog:find_revert_target(<<"Counter">>, inc),
    [
        ?_assertMatch({ok, <<"^ 1">>, _}, Result)
    ].

find_revert_target_skips_flushed_entries(_Ctx) ->
    %% A flushed entry must not be reverted — it has been written to disk and
    %% is therefore not part of the active dirty set any more.
    {ok, Seq} = beamtalk_workspace_changelog:append(
        durable_input(<<"Counter">>, <<"inc">>)
    ),
    ok = beamtalk_workspace_changelog:mark_flushed([Seq]),
    Result = beamtalk_workspace_changelog:find_revert_target(<<"Counter">>, inc),
    [
        ?_assertEqual({error, no_entry}, Result)
    ].

find_revert_target_rejects_new_class(_Ctx) ->
    %% A new-class entry has `selector = undefined`, so the regular
    %% `find_revert_target(Class, SomeSelector)` lookup never matches it —
    %% the rejection of new-class reverts is enforced at the FFI boundary
    %% (`extract_revert_target_from_map/1`) rather than here. From the
    %% perspective of this lower-level API, a method-selector lookup on a
    %% new-class-only entry returns `{error, no_entry}`.
    NewClassInput = #{
        class => <<"NewThing">>,
        kind => 'new-class',
        source => <<"Object subclass: NewThing\nend\n">>,
        intent => durable,
        flushable => true,
        author => <<"sess-1">>,
        author_kind => human,
        source_file => <<"/proj/src/new_thing.bt">>
    },
    {ok, _} = beamtalk_workspace_changelog:append(NewClassInput),
    NoEntry = beamtalk_workspace_changelog:find_revert_target(<<"NewThing">>, anything),
    [
        ?_assertEqual({error, no_entry}, NoEntry)
    ].

%%====================================================================
%% Restart semantics (separate fixture: stop + restart same workspace)
%%====================================================================

restart_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, [
        fun restart_bumps_epoch_and_tags_prior/0,
        fun restart_tags_orphan_when_disk_advanced/0,
        fun restart_keeps_prior_when_disk_matches/0
    ]}.

restart_bumps_epoch_and_tags_prior() ->
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        {ok, P1} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        ?assertEqual(1, beamtalk_workspace_changelog:epoch()),
        {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
        stop(P1),

        %% Restart: epoch must advance, the pre-existing entry must be prior-epoch
        %% and therefore excluded from the active view, but still present overall.
        {ok, P2} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        ?assertEqual(2, beamtalk_workspace_changelog:epoch()),
        ?assertEqual(1, beamtalk_workspace_changelog:size()),
        ?assertEqual([], beamtalk_workspace_changelog:active_entries()),
        %% A new append in the fresh epoch is active.
        {ok, Seq} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"dec">>)),
        ?assertEqual(1, Seq),
        ?assertEqual(1, length(beamtalk_workspace_changelog:active_entries())),
        stop(P2)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

restart_tags_orphan_when_disk_advanced() ->
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        %% A real on-disk source file whose method body the entry recorded.
        SrcFile = filename:join(TmpHome, "counter.bt"),
        ok = file:write_file(SrcFile, <<"class Counter\n  inc => ^ self value\nend\n">>),
        %% span covers "^ self value" within the file
        Span = span_of(<<"class Counter\n  inc => ^ self value\nend\n">>, <<"^ self value">>),
        {ok, P1} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        Input = (durable_input(<<"Counter">>, <<"inc">>))#{
            source_file => list_to_binary(SrcFile),
            span => Span,
            prev_source => <<"^ self value">>
        },
        {ok, _} = beamtalk_workspace_changelog:append(Input),
        stop(P1),

        %% Disk advances under us (someone edited the file).
        ok = file:write_file(SrcFile, <<"class Counter\n  inc => ^ 999\nend\n">>),

        {ok, P2} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        [Entry] = beamtalk_workspace_changelog:entries(),
        ?assert(is_orphan_flag(Entry)),
        ?assertEqual([], beamtalk_workspace_changelog:active_entries()),
        stop(P2)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

restart_keeps_prior_when_disk_matches() ->
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        Content = <<"class Counter\n  inc => ^ self value\nend\n">>,
        SrcFile = filename:join(TmpHome, "counter.bt"),
        ok = file:write_file(SrcFile, Content),
        Span = span_of(Content, <<"^ self value">>),
        {ok, P1} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        Input = (durable_input(<<"Counter">>, <<"inc">>))#{
            source_file => list_to_binary(SrcFile),
            span => Span,
            prev_source => <<"^ self value">>
        },
        {ok, _} = beamtalk_workspace_changelog:append(Input),
        stop(P1),

        %% Disk unchanged → entry is prior-epoch but NOT orphan.
        {ok, P2} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        [Entry] = beamtalk_workspace_changelog:entries(),
        ?assertNot(is_orphan_flag(Entry)),
        stop(P2)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

%%====================================================================
%% Bounded ring rotation
%%====================================================================

rotation_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, [
        fun rotation_archives_overflow/0,
        fun rotation_keeps_state_when_archive_fails/0,
        fun archive_filenames_unique_within_same_second/0,
        fun load_enforces_ring_bound_when_disk_exceeds_max/0
    ]}.

rotation_archives_overflow() ->
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        %% Append MAX_ENTRIES + 3 entries; the 3 oldest must be archived.
        Total = 1003,
        lists:foreach(
            fun(N) ->
                Sel = list_to_binary("m" ++ integer_to_list(N)),
                {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, Sel))
            end,
            lists:seq(1, Total)
        ),
        Dir = beamtalk_workspace_changelog:changes_dir(WsId),
        ArchiveDir = filename:join(Dir, "archive"),
        {ok, ArchiveFiles} = file:list_dir(ArchiveDir),
        MetaArchives = [F || F <- ArchiveFiles, lists:suffix(".jsonl.gz", F)],
        SrcArchives = [F || F <- ArchiveFiles, lists:suffix(".tar.gz", F)],
        ?assertEqual(1000, beamtalk_workspace_changelog:size()),
        ?assert(length(MetaArchives) >= 1),
        ?assert(length(SrcArchives) >= 1),
        %% The live log on disk holds exactly the retained 1000 entries.
        {ok, LogBin} = file:read_file(filename:join(Dir, "changes.jsonl")),
        Lines = binary:split(LogBin, <<"\n">>, [global, trim_all]),
        ?assertEqual(1000, length(Lines)),
        stop(Pid)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

rotation_keeps_state_when_archive_fails() ->
    %% Transactional rotation: if archiving the overflow segment fails, the live
    %% ETS and on-disk log must be left untouched (no history loss).
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        Total = 1003,
        lists:foreach(
            fun(N) ->
                Sel = list_to_binary("m" ++ integer_to_list(N)),
                {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, Sel))
            end,
            lists:seq(1, Total)
        ),
        Dir = beamtalk_workspace_changelog:changes_dir(WsId),
        %% After a healthy rotation the ring is bounded and an archive exists.
        ?assertEqual(1000, beamtalk_workspace_changelog:size()),
        %% Now sabotage the archive dir: make it a regular file so the next
        %% rotation's archive write fails. Append enough to trigger a rotation.
        ArchiveDir = filename:join(Dir, "archive"),
        ok = file:del_dir_r(ArchiveDir),
        ok = file:write_file(ArchiveDir, <<"not a dir">>),
        SizeBefore = beamtalk_workspace_changelog:size(),
        {ok, LogBefore} = file:read_file(filename:join(Dir, "changes.jsonl")),
        %% This append overflows the ring and attempts a rotation that must fail.
        {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"boom">>)),
        SizeAfter = beamtalk_workspace_changelog:size(),
        {ok, LogAfter} = file:read_file(filename:join(Dir, "changes.jsonl")),
        %% The new entry was appended (persist_append succeeded); rotation was
        %% attempted, failed, and left the ring intact rather than dropping
        %% the oldest 1 entry. So size grew by exactly the one append.
        ?assertEqual(SizeBefore + 1, SizeAfter),
        %% The on-disk log was NOT rewritten/truncated by the failed rotation —
        %% it still contains everything it had before (plus the new line).
        ?assert(byte_size(LogAfter) >= byte_size(LogBefore)),
        ?assert(is_process_alive(Pid)),
        stop(Pid)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

archive_filenames_unique_within_same_second() ->
    %% Two rotations in the same wall-clock second must not collide.
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        %% Drive enough appends to force at least two separate rotations quickly.
        lists:foreach(
            fun(N) ->
                Sel = list_to_binary("m" ++ integer_to_list(N)),
                {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, Sel))
            end,
            lists:seq(1, 1002)
        ),
        %% First rotation done. Append more to trigger a second rotation in the
        %% (very likely) same second.
        lists:foreach(
            fun(N) ->
                Sel = list_to_binary("n" ++ integer_to_list(N)),
                {ok, _} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, Sel))
            end,
            lists:seq(1, 5)
        ),
        Dir = beamtalk_workspace_changelog:changes_dir(WsId),
        ArchiveDir = filename:join(Dir, "archive"),
        {ok, ArchiveFiles} = file:list_dir(ArchiveDir),
        MetaArchives = [F || F <- ArchiveFiles, lists:suffix(".jsonl.gz", F)],
        %% Both rotations produced distinct archive files (no overwrite).
        ?assert(length(MetaArchives) >= 2),
        ?assertEqual(length(MetaArchives), length(lists:usort(MetaArchives))),
        stop(Pid)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

load_enforces_ring_bound_when_disk_exceeds_max() ->
    %% A changes.jsonl persisted with > MAX_ENTRIES must be trimmed to the ring
    %% bound on startup, not left unbounded until the next append.
    {WsId, TmpHome, OldHome} = fresh_workspace(),
    try
        Dir = beamtalk_workspace_changelog:changes_dir(WsId),
        ok = filelib:ensure_path(Dir),
        %% Hand-write 1005 metadata lines straight to disk (no source bodies
        %% needed for this test — entries with source_file=null are non-orphan).
        Lines = [
            begin
                Entry = beamtalk_workspace_changelog:entry_from_json(
                    line_json(N)
                ),
                [beamtalk_workspace_changelog:entry_to_json(Entry), $\n]
            end
         || N <- lists:seq(0, 1004)
        ],
        ok = file:write_file(
            filename:join(Dir, "changes.jsonl"), iolist_to_binary(Lines)
        ),
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WsId}),
        Size = beamtalk_workspace_changelog:size(),
        {ok, LogBin} = file:read_file(filename:join(Dir, "changes.jsonl")),
        LogLines = binary:split(LogBin, <<"\n">>, [global, trim_all]),
        ?assertEqual(1000, Size),
        ?assertEqual(1000, length(LogLines)),
        stop(Pid)
    after
        restore_home(OldHome),
        del_tree(TmpHome)
    end.

%%====================================================================
%% Forward-compatibility: unknown enum values must not drop the line
%%====================================================================

forward_compat_test_() ->
    [
        fun unknown_kind_preserved_not_dropped/0,
        fun known_kinds_decode_to_atoms/0
    ].

unknown_kind_preserved_not_dropped() ->
    %% A newer writer may emit a kind this beam doesn't know. Decoding must keep
    %% the line (map unknown to 'unknown') rather than throw and drop history.
    Json = line_json_with(#{<<"kind">> => <<"future-kind">>}),
    Entry = beamtalk_workspace_changelog:entry_from_json(Json),
    ?assertEqual(unknown, beamtalk_workspace_changelog:entry_kind(Entry)).

known_kinds_decode_to_atoms() ->
    Instance = beamtalk_workspace_changelog:entry_from_json(
        line_json_with(#{<<"kind">> => <<"instance">>})
    ),
    Class = beamtalk_workspace_changelog:entry_from_json(
        line_json_with(#{<<"kind">> => <<"class">>})
    ),
    NewClass = beamtalk_workspace_changelog:entry_from_json(
        line_json_with(#{<<"kind">> => <<"new-class">>})
    ),
    ?assertEqual(instance, beamtalk_workspace_changelog:entry_kind(Instance)),
    ?assertEqual(class, beamtalk_workspace_changelog:entry_kind(Class)),
    ?assertEqual('new-class', beamtalk_workspace_changelog:entry_kind(NewClass)).

%%====================================================================
%% Table-absent guards (no gen_server started)
%%====================================================================

entries_returns_empty_when_table_absent() ->
    ok = ensure_no_changelog_table(),
    ?assertEqual([], beamtalk_workspace_changelog:entries()).

size_returns_zero_when_table_absent() ->
    ok = ensure_no_changelog_table(),
    ?assertEqual(0, beamtalk_workspace_changelog:size()).

%%====================================================================
%% Run mode (no workspace_id): memory-only
%%====================================================================

run_mode_test_() ->
    {setup, fun() -> ok end, fun(_) -> ok end, [
        fun run_mode_is_memory_only/0
    ]}.

run_mode_is_memory_only() ->
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => undefined}),
    try
        {ok, Seq} = beamtalk_workspace_changelog:append(durable_input(<<"Counter">>, <<"inc">>)),
        ?assertEqual(0, Seq),
        ?assertEqual(1, beamtalk_workspace_changelog:size()),
        ?assertEqual(undefined, beamtalk_workspace_changelog:changes_dir(undefined))
    after
        stop(Pid)
    end.

%%====================================================================
%% Helpers
%%====================================================================

durable_input(Class, Selector) ->
    #{
        class => Class,
        selector => Selector,
        kind => instance,
        source => <<"^ self value + 1">>,
        prev_source => <<"^ self value">>,
        intent => durable,
        flushable => true,
        author => <<"sess-1">>,
        author_kind => human,
        source_file => <<"/proj/src/counter.bt">>,
        span => #{start => 0, 'end' => 10}
    }.

%% Create an isolated workspace: a unique id + a temp HOME so the changelog
%% resolves its changes/ dir under our temp tree. Returns the prior HOME so it
%% can be restored.
fresh_workspace() ->
    Unique = integer_to_list(erlang:unique_integer([positive])),
    WorkspaceId = list_to_binary("test-ws-" ++ Unique),
    Tmp = filename:join(temp_dir(), "bt-changelog-" ++ Unique),
    ok = filelib:ensure_path(Tmp),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Tmp),
    {WorkspaceId, Tmp, OldHome}.

restore_home(false) -> os:unsetenv("HOME");
restore_home(OldHome) -> os:putenv("HOME", OldHome).

%% Cross-platform absolute temp dir (CLAUDE.md: never hardcode /tmp in tests).
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

seq_file(Seq, Suffix) ->
    lists:flatten(io_lib:format("~6..0b~s", [Seq, Suffix])).

%% Find the byte span of Needle within Haystack (0-based start, exclusive end).
span_of(Haystack, Needle) ->
    {Start, Len} = binary:match(Haystack, Needle),
    #{start => Start, 'end' => Start + Len}.

is_orphan_flag(Entry) ->
    beamtalk_workspace_changelog:entry_is_orphan(Entry).

%% A minimal, valid changes.jsonl line (non-orphan: sourceFile=null) for tests
%% that hand-write the log. Seq/ts derived from N so lines are distinct.
line_json(N) ->
    line_json_with(#{<<"seq">> => N, <<"ts">> => N}).

%% Build a changes.jsonl line from a base map merged with Overrides.
line_json_with(Overrides) ->
    Base = #{
        <<"ts">> => 0,
        <<"seq">> => 0,
        <<"epoch">> => 1,
        <<"class">> => <<"Counter">>,
        <<"selector">> => <<"inc">>,
        <<"kind">> => <<"instance">>,
        <<"source_ref">> => <<"000000-source.bt">>,
        <<"prev_source_ref">> => null,
        <<"sourceFile">> => null,
        <<"span">> => null,
        <<"intent">> => <<"durable">>,
        <<"flushable">> => true,
        <<"not_flushable_reason">> => null,
        <<"author">> => <<"sess-1">>,
        <<"author_kind">> => <<"human">>
    },
    iolist_to_binary(json:encode(maps:merge(Base, Overrides))).

%% Ensure the changelog ETS table does not exist (table-absent guard tests).
ensure_no_changelog_table() ->
    case ets:whereis(beamtalk_changelog_entries) of
        undefined ->
            ok;
        Tid ->
            ets:delete(Tid),
            ok
    end.
