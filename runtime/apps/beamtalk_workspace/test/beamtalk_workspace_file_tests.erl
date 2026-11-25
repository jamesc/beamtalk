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

%% BT-5070: `filename:absname/1` leaves `..` in place, so a component-prefix
%% test alone passed `/proj/src/../../x/Foo.bt`.
path_inside_dotdot_test_() ->
    [
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj", "/proj/src/../../x/Foo.bt")),
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj", "/proj/../proj/a.bt")),
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj/../proj", "/proj/a.bt")),
        ?_assertEqual(false, beamtalk_workspace_file:relative_parts("/proj", "/proj/src/../a.bt")),
        ?_assertNot(
            beamtalk_workspace_file:path_inside(<<"/proj">>, <<"/proj/src/../../x/Foo.bt">>)
        ),
        ?_assert(beamtalk_workspace_file:path_inside(<<"/proj">>, <<"/proj/src/a.bt">>)),
        %% Invalid UTF-8 cannot be judged: not inside, never a crash.
        ?_assertNot(beamtalk_workspace_file:path_inside("/proj", <<"/proj/", 255, ".bt">>))
    ].

%% BT-5070: a symlink under the root that points outside it is not inside.
path_inside_symlink_test_() ->
    {setup, fun() -> make_link_fixture() end, fun(Dir) -> beamtalk_test_fs:rm_rf(Dir) end, fun(Dir) ->
        Root = long_root(Dir),
        RootLink = filename:join(Dir, "rootlink"),
        [
            ?_assert(beamtalk_workspace_file:path_inside(Root, filename:join(Root, "src/a.bt"))),
            ?_assertNot(
                beamtalk_workspace_file:path_inside(Root, filename:join(Root, "link/Foo.bt"))
            ),
            ?_assertNot(
                beamtalk_workspace_file:path_inside(Root, filename:join(Root, "link"))
            ),
            %% A link that stays inside the root is fine.
            ?_assert(
                beamtalk_workspace_file:path_inside(Root, filename:join(Root, "inner/a.bt"))
            ),
            %% A root reached through a link compares equal to its target.
            %% On failure, show the raw link target (BT-5122).
            ?_assertEqual(
                {true, file:read_link_all(RootLink)},
                {
                    beamtalk_workspace_file:path_inside(
                        RootLink, filename:join(Root, "src/a.bt")
                    ),
                    file:read_link_all(RootLink)
                }
            )
        ] ++ windows_case_cases(RootLink, Root)
    end}.

%% The project root in the spelling Windows reports for a link to it (BT-5122).
%% `TMP` on a Windows runner holds an 8.3 short name (`RUNNER~1`) that
%% `path_inside/2` cannot expand, while `file:read_link_all/1` answers the long
%% name. Building the root from the link target keeps this test about links,
%% not 8.3 names. On other systems the target is the root as created.
long_root(Dir) ->
    {ok, Target} = file:read_link_all(filename:join(Dir, "rootlink")),
    filename:join(filename:dirname(Target), "proj").

%% BT-5122: Windows compares paths case-insensitively, and reports a link
%% target in its own spelling (`c:/Users/...`), so a root reached through a
%% link still contains the same file written in another case. Elsewhere case
%% is significant: there is nothing to check.
windows_case_cases(RootLink, Root) ->
    case os:type() of
        {win32, _} ->
            [
                ?_assert(
                    beamtalk_workspace_file:path_inside(
                        RootLink, string:uppercase(filename:join(Root, "src/a.bt"))
                    )
                )
            ];
        _ ->
            []
    end.

%% BT-5122: the components `path_inside/2` compares. Windows folds case;
%% other systems keep each component as written.
comparable_parts_test_() ->
    Parts = fun beamtalk_workspace_file:comparable_parts/2,
    Win = {win32, nt},
    Unix = {unix, linux},
    [
        ?_assertEqual(Parts("c:/users/proj", Win), Parts("C:/Users/PROJ", Win)),
        ?_assertEqual(Parts("c:/users/runneradmin", Win), Parts("C:/Users/RunnerAdmin", Win)),
        %% A different name is still different, whatever its case.
        ?_assertNotEqual(Parts("C:/Users/proj", Win), Parts("C:/Users/proj-other", Win)),
        %% Case is significant elsewhere.
        ?_assertNotEqual(Parts("/home/Proj", Unix), Parts("/home/proj", Unix)),
        ?_assertEqual(["/", "home", "Proj"], Parts("/home/Proj", Unix))
    ].

make_link_fixture() ->
    Dir = beamtalk_test_fs:tmp_dir("bt5070_path_inside"),
    Root = filename:join(Dir, "proj"),
    ok = filelib:ensure_path(filename:join(Root, "src")),
    ok = filelib:ensure_path(filename:join(Dir, "outside")),
    ok = file:make_symlink(filename:join(Dir, "outside"), filename:join(Root, "link")),
    ok = file:make_symlink("src", filename:join(Root, "inner")),
    ok = file:make_symlink(Root, filename:join(Dir, "rootlink")),
    Dir.

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

%% BT-5017: the staged bytes are fsynced before the caller renames them into
%% place. Without it a power loss can leave the renamed file empty. Observed by
%% call-tracing `file:sync/1` and `file:rename/2` (beamtalk_test_trace), so the
%% test needs no timing.
write_temp_syncs_before_returning_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        {{ok, Tmp}, Calls} = beamtalk_test_trace:calls_during(
            fun() -> beamtalk_workspace_file:write_temp(Path, <<"body">>) end,
            [{file, sync, 1}]
        ),
        ?assertEqual({ok, <<"body">>}, file:read_file(Tmp)),
        ?assertMatch([{file, sync, _}], Calls)
    end).

atomic_write_syncs_before_rename_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        {ok, Calls} = beamtalk_test_trace:calls_during(
            fun() -> beamtalk_workspace_file:atomic_write(Path, <<"body">>, undefined) end,
            [{file, sync, 1}, {file, rename, 2}]
        ),
        ?assertEqual([sync, rename], [F || {file, F, _} <- Calls]),
        ?assertEqual({ok, <<"body">>}, file:read_file(Path))
    end).

write_temp_uses_a_fresh_name_per_call_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        {ok, T1} = beamtalk_workspace_file:write_temp(Path, <<"one">>),
        {ok, T2} = beamtalk_workspace_file:write_temp(Path, <<"two">>),
        ?assertNotEqual(T1, T2),
        ?assertNotEqual(Path ++ ".tmp", T1),
        ?assert(lists:prefix(Path ++ ".bt-staging.", T1)),
        ?assertEqual(Dir, filename:dirname(T1)),
        %% The second staging write did not touch the first one's file.
        ?assertEqual({ok, <<"one">>}, file:read_file(T1)),
        ?assertEqual({ok, <<"two">>}, file:read_file(T2))
    end).

write_temp_is_private_and_exclusive_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        {ok, T} = beamtalk_workspace_file:write_temp(Path, <<"x">>, #{private => true}),
        {ok, #file_info{mode = Mode}} = file:read_file_info(T),
        ?assertEqual(8#600, Mode band 8#777),
        %% Unpredictable: a random suffix beyond the counter.
        ?assertMatch([_, _, _ | _], string:split(T, ".", all))
    end).

atomic_write_keeps_existing_mode_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "target.bt"),
        ok = file:write_file(Path, <<"old">>),
        ok = file:change_mode(Path, 8#640),
        ok = beamtalk_workspace_file:atomic_write(Path, <<"new">>, undefined),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
        ?assertEqual(8#640, Mode band 8#777),
        New = filename:join(Dir, "fresh.bt"),
        ok = beamtalk_workspace_file:atomic_write(New, <<"n">>, undefined, #{private => true}),
        {ok, #file_info{mode = M2}} = file:read_file_info(New),
        ?assertEqual(8#600, M2 band 8#777),
        %% A staged (flush-style) temp for an existing file takes its mode.
        {ok, T} = beamtalk_workspace_file:write_temp(Path, <<"s">>),
        {ok, #file_info{mode = M3}} = file:read_file_info(T),
        ?assertEqual(8#640, M3 band 8#777)
    end).

atomic_write_private_tightens_existing_0644_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "metadata.json"),
        ok = file:write_file(Path, <<"old">>),
        ok = file:change_mode(Path, 8#644),
        ok = beamtalk_workspace_file:atomic_write(Path, <<"new">>, undefined, #{private => true}),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
        ?assertEqual(8#600, Mode band 8#777),
        ?assertEqual({ok, <<"new">>}, file:read_file(Path))
    end).

ensure_private_dir_is_0700_test() ->
    with_dir(fun(Dir) ->
        Sub = filename:join([Dir, "a", "ws"]),
        ok = beamtalk_workspace_file:ensure_private_dir(Sub),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Sub),
        ?assertEqual(8#700, Mode band 8#777)
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
        Old = Path ++ ".bt-staging.17.tmp",
        Young = Path ++ ".bt-staging.18.tmp",
        Legacy = Path ++ ".tmp",
        %% Another tool's `<file>.<digits>.tmp`: no `.bt-staging.` marker.
        Unmarked = Path ++ ".17.tmp",
        NotDigits = Path ++ ".bt-staging.x1.tmp",
        Sibling = filename:join(Dir, "b.bt.bt-staging.17.tmp"),
        Prefixed = filename:join(Dir, "a.bt2.bt-staging.17.tmp"),
        OldKept = [Legacy, Unmarked, NotDigits, Sibling, Prefixed],
        Kept = [Young | OldKept],
        lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, [Old | Kept]),
        lists:foreach(fun(F) -> ok = set_age(F, 7200) end, [Old | OldKept]),
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
        Old1 = filename:join(Dir, "changes.jsonl.bt-staging.5.tmp"),
        Old2 = filename:join(Dir, "000001-source.bt.bt-staging.77.tmp"),
        Young = filename:join(Dir, "changes.jsonl.bt-staging.6.tmp"),
        Legacy = filename:join(Dir, "changes.jsonl.tmp"),
        Unmarked = filename:join(Dir, "changes.jsonl.5.tmp"),
        NoBase = filename:join(Dir, ".bt-staging.5.tmp"),
        Plain = filename:join(Dir, "000001-source.bt"),
        Kept = [Young, Legacy, Unmarked, NoBase, Plain],
        lists:foreach(fun(F) -> ok = file:write_file(F, <<>>) end, [Old1, Old2 | Kept]),
        lists:foreach(fun(F) -> ok = set_age(F, 7200) end, [
            Old1, Old2, Legacy, Unmarked, NoBase, Plain
        ]),
        ?assertEqual(
            lists:sort([Old1, Old2]),
            lists:sort(beamtalk_workspace_file:sweep_stale_temps_in(Dir, 3600))
        ),
        lists:foreach(fun(F) -> ?assert(filelib:is_regular(F)) end, Kept)
    end).

%% `beamtalk_repl_ops_load`'s direct source writes (save-native-source,
%% section dividers) sweep a crashed earlier write's stale staging file too.
ops_load_atomic_write_sweeps_stale_temps_test() ->
    with_dir(fun(Dir) ->
        Path = filename:join(Dir, "a.bt"),
        Stale = Path ++ ".bt-staging.5.tmp",
        Unmarked = Path ++ ".6.tmp",
        ok = file:write_file(Stale, <<>>),
        ok = file:write_file(Unmarked, <<>>),
        ok = set_age(Stale, 7200),
        ok = set_age(Unmarked, 7200),
        ok = beamtalk_repl_ops_load:atomic_write_file(Path, <<"new">>),
        ?assertEqual({ok, <<"new">>}, file:read_file(Path)),
        ?assertNot(filelib:is_regular(Stale)),
        ?assert(filelib:is_regular(Unmarked))
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
        Stale = File ++ ".bt-staging.111.tmp",
        Fresh = File ++ ".bt-staging.222.tmp",
        %% An old `<file>.<digits>.tmp` some other tool left: not ours.
        Unmarked = File ++ ".333.tmp",
        ok = file:write_file(Stale, <<"crashed attempt">>),
        ok = file:write_file(Fresh, <<"in flight">>),
        ok = file:write_file(Unmarked, <<"another tool's file">>),
        ok = set_age(Stale, 2 * 3600),
        ok = set_age(Unmarked, 2 * 3600),
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
        ?assertEqual(["counter.bt.333.tmp", "counter.bt.bt-staging.222.tmp"], tmp_files(ProjDir))
    end).

%%====================================================================
%% beamtalk_workspace_changelog writes through a unique temp name
%%====================================================================

changelog_log_rewrite_does_not_touch_a_foreign_fixed_tmp_test() ->
    with_changelog(fun(WorkspaceId, _Home) ->
        ChangesDir = beamtalk_workspace_changelog_log:changes_dir(WorkspaceId),
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
    beamtalk_test_fs:with_tmp_home(fun(_Home) ->
        ChangesDir = beamtalk_workspace_changelog_log:changes_dir(WorkspaceId),
        SourcesDir = filename:join(ChangesDir, "sources"),
        ok = filelib:ensure_path(SourcesDir),
        StaleLog = filename:join(ChangesDir, "changes.jsonl.bt-staging.11.tmp"),
        StaleSource = filename:join(SourcesDir, "000003-source.bt.bt-staging.12.tmp"),
        Fresh = filename:join(ChangesDir, "changes.jsonl.bt-staging.13.tmp"),
        lists:foreach(
            fun(F) -> ok = file:write_file(F, <<>>) end, [StaleLog, StaleSource, Fresh]
        ),
        ok = set_age(StaleLog, 7200),
        ok = set_age(StaleSource, 7200),
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
        beamtalk_test_proc:shutdown(Pid),
        ?assertNot(filelib:is_regular(StaleLog)),
        ?assertNot(filelib:is_regular(StaleSource)),
        ?assert(filelib:is_regular(Fresh))
    end).

%%====================================================================
%% Helpers
%%====================================================================

with_dir(Fun) ->
    Dir = filename:join(beamtalk_test_fs:base_dir(), "bt-wsfile-" ++ beamtalk_test_unique:id()),
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
    beamtalk_test_fs:with_tmp_home(fun(Home) ->
        {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
        try
            Fun(WorkspaceId, Home)
        after
            beamtalk_test_proc:shutdown(Pid)
        end
    end).

%% Backdate `File`'s modification time by `Seconds`.
set_age(File, Seconds) ->
    MTime = erlang:system_time(second) - Seconds,
    file:write_file_info(File, #file_info{mtime = MTime}, [{time, posix}]).

tmp_files(Dir) ->
    {ok, Names} = file:list_dir(Dir),
    lists:sort([N || N <- Names, lists:suffix(".tmp", N)]).
