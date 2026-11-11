%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_file_tests).

-moduledoc """
Unit tests for `beamtalk_workspace_file` (BT-4683): component-wise path
containment and the unique-temp atomic write shared by
`beamtalk_repl_ops_load`, `beamtalk_workspace_changelog` and
`beamtalk_workspace_flush`.

The last two groups pin the BT-4683 behaviour change for ChangeLog and
flush writes: they used to stage through a fixed `<file>.tmp`, so a second
writer's staging file at that name was overwritten and renamed into place.
They now stage through a unique name and leave a foreign `<file>.tmp` alone.
A successful flush commit sweeps only stale unique staging files.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%%====================================================================
%% path_inside/2, relative_parts/2
%%====================================================================

path_inside_test_() ->
    [
        ?_assert(beamtalk_workspace_file:path_inside("/proj", "/proj/src/a.bt")),
        ?_assert(beamtalk_workspace_file:path_inside("/proj", "/proj")),
        ?_assert(beamtalk_workspace_file:path_inside("/proj/", "/proj/src/a.bt")),
        ?_assert(beamtalk_workspace_file:path_inside("/proj", "/proj//src/a.bt")),
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj", "/proj-other/a.bt")),
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj/src", "/proj")),
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj", "/elsewhere/a.bt"))
    ].

relative_parts_test_() ->
    [
        ?_assertEqual(
            {ok, ["src", "a.bt"]},
            beamtalk_workspace_file:relative_parts("/proj", "/proj/src/a.bt")
        ),
        ?_assertEqual({ok, []}, beamtalk_workspace_file:relative_parts("/proj", "/proj")),
        ?_assertEqual(false, beamtalk_workspace_file:relative_parts("/proj", "/projx/a.bt"))
    ].

%%====================================================================
%% write_temp/2, atomic_write/3
%%====================================================================

write_temp_uses_a_fresh_name_per_call_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        {ok, T1} = beamtalk_workspace_file:write_temp(Path, <<"one">>),
        {ok, T2} = beamtalk_workspace_file:write_temp(Path, <<"two">>),
        ?assertNotEqual(T1, T2),
        ?assertNotEqual(Path ++ ".tmp", T1),
        ?assertEqual(Dir, filename:dirname(T1)),
        %% The second staging write did not touch the first one's file.
        ?assertEqual({ok, <<"one">>}, file:read_file(T1)),
        ?assertEqual({ok, <<"two">>}, file:read_file(T2))
    end).

write_temp_failure_is_tagged_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join([Dir, "missing_dir", "target.bt"]),
        ?assertEqual({error, {write, enoent}}, beamtalk_workspace_file:write_temp(Path, <<"x">>))
    end).

atomic_write_replaces_and_leaves_no_temp_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        ok = file:write_file(Path, <<"old">>),
        ?assertEqual(ok, beamtalk_workspace_file:atomic_write(Path, <<"new">>, undefined)),
        ?assertEqual({ok, <<"new">>}, file:read_file(Path)),
        ?assertEqual([], tmp_files(Dir))
    end).

atomic_write_recheck_mismatch_writes_nothing_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        ok = file:write_file(Path, <<"changed by someone else">>),
        ?assertEqual(
            {error, {recheck, changed_on_disk}},
            beamtalk_workspace_file:atomic_write(Path, <<"mine">>, <<"what I read">>)
        ),
        ?assertEqual({ok, <<"changed by someone else">>}, file:read_file(Path)),
        ?assertEqual([], tmp_files(Dir))
    end).

atomic_write_rename_failure_cleans_up_test() ->
    with_dir(fun(Dir) ->
        %% A non-empty directory at the target path makes the rename fail.
        Path = filename:join(Dir, "target.bt"),
        ok = filelib:ensure_path(filename:join(Path, "child")),
        ?assertMatch(
            {error, {rename, _}}, beamtalk_workspace_file:atomic_write(Path, <<"x">>, undefined)
        ),
        ?assertEqual([], tmp_files(Dir))
    end).

atomic_write_concurrent_writers_never_mix_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        Contents = [binary:copy(<<C>>, 100000) || C <- "ABCDEFGH"],
        Parent = self(),
        Pids = [
            spawn(fun() ->
                Parent ! {done, self(), beamtalk_workspace_file:atomic_write(Path, C, undefined)}
            end)
         || C <- Contents
        ],
        Results = [
            receive
                {done, Pid, R} -> R
            after 10000 -> timeout
            end
         || Pid <- Pids
        ],
        ?assertEqual([ok || _ <- Pids], Results),
        {ok, Final} = file:read_file(Path),
        ?assert(lists:member(Final, Contents)),
        ?assertEqual([], tmp_files(Dir))
    end).

%%====================================================================
%% sweep_stale_temps/2
%%====================================================================

sweep_stale_temps_deletes_only_old_unique_temps_of_that_file_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "a.bt"),
        Old = Path ++ ".17.tmp",
        Young = Path ++ ".18.tmp",
        Legacy = Path ++ ".tmp",
        NotDigits = Path ++ ".x1.tmp",
        Sibling = filename:join(Dir, "b.bt.17.tmp"),
        Prefixed = filename:join(Dir, "a.bt2.17.tmp"),
        Kept = [Young, Legacy, NotDigits, Sibling, Prefixed],
        lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, [Old | Kept]),
        lists:foreach(fun(F) -> ok = set_age(F, 7200) end, [
            Old, Legacy, NotDigits, Sibling, Prefixed
        ]),
        ?assertEqual([Old], beamtalk_workspace_file:sweep_stale_temps(Path, 3600)),
        ?assertNot(filelib:is_regular(Old)),
        lists:foreach(fun(F) -> ?assert(filelib:is_regular(F)) end, Kept),
        %% A missing directory is not an error.
        ?assertEqual(
            [], beamtalk_workspace_file:sweep_stale_temps(filename:join([Dir, "no", "x.bt"]), 0)
        )
    end).

sweep_stale_temps_in_deletes_any_old_unique_temp_in_the_dir_test() ->
    with_dir(fun(Dir) ->
        Old1 = filename:join(Dir, "changes.jsonl.5.tmp"),
        Old2 = filename:join(Dir, "000001-source.bt.77.tmp"),
        Young = filename:join(Dir, "changes.jsonl.6.tmp"),
        Legacy = filename:join(Dir, "changes.jsonl.tmp"),
        NoBase = filename:join(Dir, ".5.tmp"),
        Plain = filename:join(Dir, "000001-source.bt"),
        Kept = [Young, Legacy, NoBase, Plain],
        lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, [Old1, Old2 | Kept]),
        lists:foreach(fun(F) -> ok = set_age(F, 7200) end, [Old1, Old2, Legacy, NoBase, Plain]),
        ?assertEqual(
            lists:sort([Old1, Old2]),
            lists:sort(beamtalk_workspace_file:sweep_stale_temps_in(Dir, 3600))
        ),
        lists:foreach(fun(F) -> ?assert(filelib:is_regular(F)) end, Kept)
    end).

%%====================================================================
%% beamtalk_workspace_flush stages through a unique temp name
%%====================================================================

flush_does_not_touch_a_foreign_fixed_tmp_test() ->
    with_changelog(fun(_WorkspaceId, Home) ->
        ProjDir = filename:join([Home, "proj", "src"]),
        ok = filelib:ensure_path(ProjDir),
        File = filename:join(ProjDir, "counter.bt"),
        Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
        ok = file:write_file(File, Original),
        %% Another writer's in-flight staging file at the old fixed name.
        Foreign = File ++ ".tmp",
        ok = file:write_file(Foreign, <<"another writer's half-written body">>),
        OldBody = <<"value => 0\n">>,
        {Start, Len} = binary:match(Original, OldBody),
        {ok, _} = beamtalk_workspace_changelog:append(#{
            class => <<"Counter">>,
            selector => <<"value">>,
            kind => instance,
            source => <<"value => 42\n">>,
            prev_source => OldBody,
            intent => durable,
            flushable => true,
            author => <<"sess-test">>,
            author_kind => human,
            source_file => list_to_binary(File),
            span => #{start => Start, 'end' => Start + Len}
        }),
        {ok, Summary} = beamtalk_workspace_flush:flush(),
        ?assertEqual(1, maps:get(flushed, Summary)),
        ?assertEqual(
            {ok, <<"Object subclass: Counter\n  value => 42\nend\n">>}, file:read_file(File)
        ),
        ?assertEqual({ok, <<"another writer's half-written body">>}, file:read_file(Foreign)),
        %% Only the foreign file is left; flush's own staging file is gone.
        ?assertEqual(["counter.bt.tmp"], tmp_files(ProjDir))
    end).

%% A crashed earlier flush's unique staging file is swept by the next
%% successful commit to the same file once it is stale; a fresh one (which
%% could be another flush in flight) is left alone.
flush_sweeps_a_stale_unique_tmp_but_not_a_fresh_one_test() ->
    with_changelog(fun(_WorkspaceId, Home) ->
        ProjDir = filename:join([Home, "proj", "src"]),
        ok = filelib:ensure_path(ProjDir),
        File = filename:join(ProjDir, "counter.bt"),
        Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
        ok = file:write_file(File, Original),
        Stale = File ++ ".111.tmp",
        Fresh = File ++ ".222.tmp",
        ok = file:write_file(Stale, <<"crashed attempt">>),
        ok = file:write_file(Fresh, <<"in flight">>),
        ok = set_age(Stale, 2 * 3600),
        OldBody = <<"value => 0\n">>,
        {Start, Len} = binary:match(Original, OldBody),
        {ok, _} = beamtalk_workspace_changelog:append(#{
            class => <<"Counter">>,
            selector => <<"value">>,
            kind => instance,
            source => <<"value => 42\n">>,
            prev_source => OldBody,
            intent => durable,
            flushable => true,
            author => <<"sess-test">>,
            author_kind => human,
            source_file => list_to_binary(File),
            span => #{start => Start, 'end' => Start + Len}
        }),
        {ok, Summary} = beamtalk_workspace_flush:flush(),
        ?assertEqual(1, maps:get(flushed, Summary)),
        ?assertEqual(["counter.bt.222.tmp"], tmp_files(ProjDir))
    end).

%%====================================================================
%% beamtalk_workspace_changelog writes through a unique temp name
%%====================================================================

changelog_log_rewrite_does_not_touch_a_foreign_fixed_tmp_test() ->
    with_changelog(fun(WorkspaceId, _Home) ->
        ChangesDir = beamtalk_workspace_changelog:changes_dir(WorkspaceId),
        ok = filelib:ensure_path(ChangesDir),
        Foreign = filename:join(ChangesDir, "changes.jsonl.tmp"),
        ok = file:write_file(Foreign, <<"foreign">>),
        %% `clear/0` truncates changes.jsonl through write_file_atomic/2.
        ok = beamtalk_workspace_changelog:clear(),
        ?assertEqual({ok, <<>>}, file:read_file(filename:join(ChangesDir, "changes.jsonl"))),
        ?assertEqual({ok, <<"foreign">>}, file:read_file(Foreign))
    end).

%% A crashed write's stale temp file under changes/ or changes/sources/ is
%% swept when the ChangeLog starts; a fresh one is left alone.
changelog_start_sweeps_stale_temps_test() ->
    WorkspaceId = list_to_binary("test-ws-file-sweep-" ++ beamtalk_test_unique:id()),
    with_home(fun(_Home) ->
        ChangesDir = beamtalk_workspace_changelog:changes_dir(WorkspaceId),
        SourcesDir = filename:join(ChangesDir, "sources"),
        ok = filelib:ensure_path(SourcesDir),
        StaleLog = filename:join(ChangesDir, "changes.jsonl.11.tmp"),
        StaleSource = filename:join(SourcesDir, "000003-source.bt.12.tmp"),
        Fresh = filename:join(ChangesDir, "changes.jsonl.13.tmp"),
        lists:foreach(
            fun(F) -> ok = file:write_file(F, <<>>) end, [StaleLog, StaleSource, Fresh]
        ),
        ok = set_age(StaleLog, 7200),
        ok = set_age(StaleSource, 7200),
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
        stop(Pid),
        ?assertNot(filelib:is_regular(StaleLog)),
        ?assertNot(filelib:is_regular(StaleSource)),
        ?assert(filelib:is_regular(Fresh))
    end).

%%====================================================================
%% Helpers
%%====================================================================

with_dir(Fun) ->
    Dir = filename:join(temp_dir(), "bt-wsfile-" ++ beamtalk_test_unique:id()),
    ok = filelib:ensure_path(Dir),
    try
        Fun(Dir)
    after
        _ = file:del_dir_r(Dir)
    end.

%% A `beamtalk_workspace_changelog` server for a fresh workspace under a
%% throwaway `HOME`, as `beamtalk_workspace_flush_tests` sets one up.
with_changelog(Fun) ->
    WorkspaceId = list_to_binary("test-ws-file-" ++ beamtalk_test_unique:id()),
    with_home(fun(Home) ->
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
        try
            Fun(WorkspaceId, Home)
        after
            stop(Pid)
        end
    end).

%% Run `Fun(Home)` with `HOME` pointing at a fresh throwaway directory, then
%% restore `HOME` and delete the directory.
with_home(Fun) ->
    Home = filename:join(temp_dir(), "bt-wsfile-home-" ++ beamtalk_test_unique:id()),
    ok = filelib:ensure_path(Home),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Home),
    try
        Fun(Home)
    after
        case OldHome of
            false -> os:unsetenv("HOME");
            _ -> os:putenv("HOME", OldHome)
        end,
        _ = file:del_dir_r(Home)
    end.

stop(Pid) ->
    Ref = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> ok
    end.

%% Backdate `File`'s modification time by `Seconds`.
set_age(File, Seconds) ->
    MTime = erlang:system_time(second) - Seconds,
    file:write_file_info(File, #file_info{mtime = MTime}, [{time, posix}]).

tmp_files(Dir) ->
    {ok, Names} = file:list_dir(Dir),
    lists:sort([N || N <- Names, lists:suffix(".tmp", N)]).

temp_dir() ->
    unicode:characters_to_list(element(2, beamtalk_file:'tempDirectory'())).
