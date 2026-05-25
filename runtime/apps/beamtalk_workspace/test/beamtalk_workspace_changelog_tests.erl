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
        fun append_returns_error_on_unwritable_dir/1,
        fun clear_empties_log/1
    ]}.

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
        fun rotation_archives_overflow/0
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
