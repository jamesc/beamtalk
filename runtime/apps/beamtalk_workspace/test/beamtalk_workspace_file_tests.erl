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
""".

-include_lib("eunit/include/eunit.hrl").

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
    Unique = beamtalk_test_unique:id(),
    WorkspaceId = list_to_binary("test-ws-file-" ++ Unique),
    Home = filename:join(temp_dir(), "bt-wsfile-home-" ++ Unique),
    ok = filelib:ensure_path(Home),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Home),
    {ok, Pid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    try
        Fun(WorkspaceId, Home)
    after
        stop(Pid),
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

tmp_files(Dir) ->
    {ok, Names} = file:list_dir(Dir),
    lists:sort([N || N <- Names, lists:suffix(".tmp", N)]).

temp_dir() ->
    unicode:characters_to_list(element(2, beamtalk_file:'tempDirectory'())).
