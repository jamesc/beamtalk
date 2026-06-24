%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_git_tests).

-moduledoc """
Unit tests for beamtalk_git porcelain/log parsing (ADR 0082, BT-2586).

These exercise the pure parsing seam (fixture porcelain → typed maps) without
spawning git. The fixtures mirror real `git status --porcelain=v2 -b -z` and
`git log --format=...` output, including paths with spaces and unicode, which
the `-z` NUL-delimited parsing must handle without whitespace splitting.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Status classification (XY → {index, worktree})
%%% ============================================================================

classify_modified_worktree_test() ->
    ?assertEqual({unmodified, modified}, beamtalk_git:classify_xy(<<".M">>)).

classify_staged_modification_test() ->
    ?assertEqual({modified, unmodified}, beamtalk_git:classify_xy(<<"M.">>)).

classify_added_then_modified_test() ->
    ?assertEqual({added, modified}, beamtalk_git:classify_xy(<<"AM">>)).

classify_deleted_test() ->
    ?assertEqual({deleted, unmodified}, beamtalk_git:classify_xy(<<"D.">>)).

classify_unknown_char_is_unmodified_test() ->
    ?assertEqual({unmodified, unmodified}, beamtalk_git:classify_xy(<<"XY">>)).

classify_type_changed_worktree_test() ->
    ?assertEqual({unmodified, type_changed}, beamtalk_git:classify_xy(<<".T">>)).

classify_type_changed_index_test() ->
    ?assertEqual({type_changed, unmodified}, beamtalk_git:classify_xy(<<"T.">>)).

%%% ============================================================================
%%% Status parsing (git status --porcelain=v2 -b -z)
%%% ============================================================================

%% A clean repo on `main` with an upstream and no changed files.
status_clean_branch_test() ->
    Bin = nul_join([
        <<"# branch.oid abc123">>,
        <<"# branch.head main">>,
        <<"# branch.upstream origin/main">>,
        <<"# branch.ab +0 -0">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(<<"main">>, maps:get(branch, Status)),
    ?assertEqual(<<"origin/main">>, maps:get(upstream, Status)),
    ?assertEqual(0, maps:get(ahead, Status)),
    ?assertEqual(0, maps:get(behind, Status)),
    ?assertEqual([], maps:get(files, Status)).

%% Ahead/behind counts are parsed from `# branch.ab +A -B`.
status_ahead_behind_test() ->
    Bin = nul_join([
        <<"# branch.head feature">>,
        <<"# branch.ab +3 -2">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(3, maps:get(ahead, Status)),
    ?assertEqual(2, maps:get(behind, Status)).

%% Detached HEAD reports a nil branch rather than the literal "(detached)".
status_detached_head_test() ->
    Bin = nul_join([<<"# branch.head (detached)">>]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(nil, maps:get(branch, Status)).

%% A modified-in-worktree ordinary entry (`1 .M ... <path>`).
status_modified_file_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 .M N... 100644 100644 100644 aaa bbb src/foo.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"src/foo.bt">>, index => unmodified, worktree => modified}],
        maps:get(files, Status)
    ).

%% A staged-and-then-modified entry classifies both columns (`1 AM`).
status_staged_and_modified_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 AM N... 100644 100644 100644 aaa bbb src/new.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"src/new.bt">>, index => added, worktree => modified}],
        maps:get(files, Status)
    ).

%% Untracked entries (`? <path>`) are surfaced with worktree => untracked.
status_untracked_file_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"? notes.txt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"notes.txt">>, index => unmodified, worktree => untracked}],
        maps:get(files, Status)
    ).

%% Paths with spaces survive because `-z` is NUL-delimited, not whitespace-split.
status_path_with_spaces_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 .M N... 100644 100644 100644 aaa bbb my dir/has spaces.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"my dir/has spaces.bt">>, index => unmodified, worktree => modified}],
        maps:get(files, Status)
    ).

%% Unicode paths round-trip unchanged.
status_unicode_path_test() ->
    Path = <<"src/café.bt"/utf8>>,
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 .M N... 100644 100644 100644 aaa bbb ", Path/binary>>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => Path, index => unmodified, worktree => modified}],
        maps:get(files, Status)
    ).

%% A rename entry (`2 ...`) emits an extra NUL record for the original path,
%% which must be skipped so it is not mis-parsed as a separate entry. The new
%% path is kept verbatim.
status_rename_skips_orig_path_test() ->
    Bin = nul_join_with_orig(
        <<"# branch.head main">>,
        <<"2 R. N... 100644 100644 100644 aaa bbb R100 new/name.bt">>,
        <<"old/name.bt">>
    ),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"new/name.bt">>, index => renamed, worktree => unmodified}],
        maps:get(files, Status)
    ).

%% A copy entry (`2 C...`) is classified `copied` and its orig-path record skipped.
status_copy_skips_orig_path_test() ->
    Bin = nul_join_with_orig(
        <<"# branch.head main">>,
        <<"2 C. N... 100644 100644 100644 aaa bbb C100 dup.bt">>,
        <<"orig.bt">>
    ),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"dup.bt">>, index => copied, worktree => unmodified}],
        maps:get(files, Status)
    ).

%% An ordinary (`1`) entry whose path contains a literal tab must NOT be
%% truncated (regression: tab-splitting once corrupted the path, feeding the
%% wrong path to stage/revert/diff — BT-2586 review C1).
status_path_with_tab_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 .M N... 100644 100644 100644 aaa bbb weird\tname.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"weird\tname.bt">>, index => unmodified, worktree => modified}],
        maps:get(files, Status)
    ).

%% A merge-conflict entry (`u UU ...`) must surface as an unmerged file, not be
%% silently dropped (BT-2586 review M2). The `u` layout has 9 metadata fields
%% before the path.
status_unmerged_conflict_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"u UU N... 100644 100644 100644 100644 aaa bbb ccc conflict.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<"conflict.bt">>, index => unmerged, worktree => unmerged}],
        maps:get(files, Status)
    ).

%% Mixed entries preserve source order (modified, then untracked).
status_preserves_order_test() ->
    Bin = nul_join([
        <<"# branch.head main">>,
        <<"1 .M N... 100644 100644 100644 aaa bbb a.bt">>,
        <<"? z.bt">>
    ]),
    Status = beamtalk_git:parse_status(Bin),
    [F1, F2] = maps:get(files, Status),
    ?assertEqual(<<"a.bt">>, maps:get(path, F1)),
    ?assertEqual(<<"z.bt">>, maps:get(path, F2)).

%% Empty output (no header, no files) degrades to defaults, not a crash.
status_empty_test() ->
    Status = beamtalk_git:parse_status(<<>>),
    ?assertEqual(nil, maps:get(branch, Status)),
    ?assertEqual([], maps:get(files, Status)).

%% The `! <path>` clause maps ignored-file entries to worktree => ignored. This
%% is forward-looking/defensive parser coverage: `git_status/0` invokes
%% `git status --porcelain=v2 -b -z` *without* `--ignored`, so git never emits
%% `!` entries in production today. The test documents the parser contract for
%% when/if `--ignored` is added, not current `git_status/0` behaviour.
status_ignored_file_test() ->
    Bin = nul_join([<<"# branch.head main">>, <<"! .env">>]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(
        [#{path => <<".env">>, index => unmodified, worktree => ignored}],
        maps:get(files, Status)
    ).

%% A `# branch.ab` line that does not match `+A -B` degrades to {0, 0}.
status_malformed_ahead_behind_defaults_to_zero_test() ->
    Bin = nul_join([<<"# branch.head main">>, <<"# branch.ab malformed">>]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(0, maps:get(ahead, Status)),
    ?assertEqual(0, maps:get(behind, Status)).

%% Non-integer values in `+A -B` reach the to_int error:badarg arm and fall back
%% to 0, not a crash.
status_non_integer_ahead_behind_defaults_to_zero_test() ->
    Bin = nul_join([<<"# branch.head main">>, <<"# branch.ab +xyz -abc">>]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual(0, maps:get(ahead, Status)),
    ?assertEqual(0, maps:get(behind, Status)).

%% An entry whose prefix does not match any known type (`1`, `2`, `u`, `?`, `!`,
%% `#`) is silently dropped — `fold_status` matches on multi-byte prefixes (e.g.
%% `<<"1 ">>`), so `<<"X some-unknown-entry">>` falls through to the catch-all.
status_unknown_entry_is_dropped_test() ->
    Bin = nul_join([<<"# branch.head main">>, <<"X some-unknown-entry">>]),
    Status = beamtalk_git:parse_status(Bin),
    ?assertEqual([], maps:get(files, Status)).

%%% ============================================================================
%%% Log parsing (git log --format=%H<FS>%h<FS>%an<FS>%aI<FS>%s<RS>)
%%% ============================================================================

log_single_commit_test() ->
    Bin = log_record(
        <<"abc123def456">>,
        <<"abc123d">>,
        <<"Jane Dev">>,
        <<"2026-06-19T10:00:00+00:00">>,
        <<"feat: add git panel">>
    ),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(<<"abc123def456">>, maps:get(sha, Commit)),
    ?assertEqual(<<"abc123d">>, maps:get(short_sha, Commit)),
    ?assertEqual(<<"feat: add git panel">>, maps:get(subject, Commit)),
    ?assertEqual(<<"Jane Dev">>, maps:get(author, Commit)),
    ?assertEqual(<<"2026-06-19T10:00:00+00:00">>, maps:get(ts, Commit)).

log_multiple_commits_in_order_test() ->
    Bin = <<
        (log_record(<<"sha1">>, <<"s1">>, <<"A">>, <<"t1">>, <<"first">>))/binary,
        (log_record(<<"sha2">>, <<"s2">>, <<"B">>, <<"t2">>, <<"second">>))/binary
    >>,
    Commits = beamtalk_git:parse_log(Bin),
    ?assertEqual(2, length(Commits)),
    ?assertEqual([<<"first">>, <<"second">>], [maps:get(subject, C) || C <- Commits]).

%% A subject containing the field separator's neighbours (spaces, colons) parses
%% intact because fields are delimited by US, not whitespace.
log_subject_with_punctuation_test() ->
    Bin = log_record(
        <<"sha">>, <<"s">>, <<"Dev">>, <<"t">>, <<"fix: handle a, b; c (edge: case)">>
    ),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(<<"fix: handle a, b; c (edge: case)">>, maps:get(subject, Commit)).

%% A subject containing the field-separator byte (US, 0x1F) must not drop the
%% commit: because subject is the last field, trailing parts rejoin into it
%% (BT-2586 review M3).
log_subject_with_field_separator_test() ->
    FS = 16#1f,
    Subject = <<"weird", FS, "subject">>,
    Bin = log_record(<<"sha">>, <<"s">>, <<"Dev">>, <<"t">>, Subject),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(Subject, maps:get(subject, Commit)),
    ?assertEqual(<<"sha">>, maps:get(sha, Commit)).

log_empty_test() ->
    ?assertEqual([], beamtalk_git:parse_log(<<>>)).

%% A record with only 4 fields (missing subject) does not match the 5-field
%% pattern and is silently dropped rather than crashing.
log_too_few_fields_is_dropped_test() ->
    FS = 16#1f,
    RS = 16#1e,
    Bin = <<"sha", FS, "short", FS, "Author", FS, "ts", RS>>,
    ?assertEqual([], beamtalk_git:parse_log(Bin)).

%%% ============================================================================
%%% Argument validation
%%% ============================================================================

git_commit_rejects_empty_message_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_commit(<<>>)).

%% A whitespace-only message must not slip past the guard into `git commit`.
git_commit_rejects_whitespace_only_message_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_commit(<<"   ">>)).

git_commit_rejects_non_binary_message_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_commit(42)).

git_diff_rejects_non_binary_path_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_diff(42)).

git_stage_rejects_non_binary_path_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_stage(42)).

git_unstage_rejects_non_binary_path_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_unstage(42)).

git_revert_file_rejects_non_binary_path_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_revert_file(42)).

git_log_rejects_zero_count_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_log(0)).

git_log_rejects_negative_count_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_log(-1)).

git_log_rejects_non_integer_test() ->
    ?assertMatch({error, _}, beamtalk_git:git_log(<<"3">>)).

%%% ============================================================================
%%% Subdirectory-project cwd/pathspec consistency (BT-2608)
%%%
%%% These spawn a real `git` against a throwaway repo whose project root is a
%%% *subdirectory* of the repo toplevel. They prove that the path `git status`
%%% reports (repo-root-relative) is the same path that revert/stage/unstage
%%% accept — the bug was that mutating ops ran with cwd = project subdir, so the
%%% repo-root-relative pathspec was re-prefixed and matched nothing.
%%% ============================================================================

%% `git rev-parse --show-toplevel` from a project subdir resolves to the repo
%% toplevel, not the subdir.
repo_toplevel_resolves_to_top_from_subdir_test() ->
    with_subdir_repo(fun(Top, ProjectDir, _RelFile) ->
        {ok, Resolved} = beamtalk_git:repo_toplevel(git_status, ProjectDir),
        ?assertEqual(canonical(Top), canonical(Resolved))
    end).

%% The end-to-end fix: a file modified in a subdir project is reported by
%% `git status` with a repo-root-relative path, and `git restore` with that
%% exact path (run at the toplevel) succeeds and discards the change.
revert_in_subdir_project_succeeds_test() ->
    with_subdir_repo(fun(_Top, ProjectDir, RelFile) ->
        {ok, Top} = beamtalk_git:repo_toplevel(git_revert_file, ProjectDir),
        %% Status reports the file under its repo-root-relative path.
        {ok, StatusOut, 0} = beamtalk_git:run_git_in(
            git_status, [<<"status">>, <<"--porcelain=v2">>, <<"-b">>, <<"-z">>], Top
        ),
        Status = beamtalk_git:parse_status(StatusOut),
        Paths = [maps:get(path, F) || F <- maps:get(files, Status)],
        ?assert(lists:member(RelFile, Paths)),
        %% Revert with that exact repo-root-relative path, at the toplevel.
        {ok, _, RevCode} = beamtalk_git:run_git_in(
            git_revert_file, [<<"restore">>, <<"--">>, RelFile], Top
        ),
        ?assertEqual(0, RevCode),
        %% The working tree is clean again for that path.
        {ok, AfterOut, 0} = beamtalk_git:run_git_in(
            git_status, [<<"status">>, <<"--porcelain=v2">>, <<"-b">>, <<"-z">>], Top
        ),
        AfterStatus = beamtalk_git:parse_status(AfterOut),
        AfterPaths = [maps:get(path, F) || F <- maps:get(files, AfterStatus)],
        ?assertNot(lists:member(RelFile, AfterPaths))
    end).

%% Stage then unstage a subdir-project file using its repo-root-relative path,
%% mirroring git_stage/1 and git_unstage/1 which share the same cwd assumption.
stage_and_unstage_in_subdir_project_succeeds_test() ->
    with_subdir_repo(fun(_Top, ProjectDir, RelFile) ->
        {ok, Top} = beamtalk_git:repo_toplevel(git_stage, ProjectDir),
        {ok, _, AddCode} = beamtalk_git:run_git_in(
            git_stage, [<<"add">>, <<"--">>, RelFile], Top
        ),
        ?assertEqual(0, AddCode),
        StagedStatus = beamtalk_git:parse_status(
            element(
                2,
                beamtalk_git:run_git_in(
                    git_status, [<<"status">>, <<"--porcelain=v2">>, <<"-b">>, <<"-z">>], Top
                )
            )
        ),
        ?assert(
            lists:any(
                fun(F) ->
                    maps:get(path, F) =:= RelFile andalso
                        maps:get(index, F) =/= unmodified
                end,
                maps:get(files, StagedStatus)
            )
        ),
        {ok, _, UnstageCode} = beamtalk_git:run_git_in(
            git_unstage, [<<"restore">>, <<"--staged">>, <<"--">>, RelFile], Top
        ),
        ?assertEqual(0, UnstageCode)
    end).

%% Regression guard: when the project root *is* the repo toplevel, resolution is
%% a no-op and revert still works (the common case must not regress).
revert_when_project_is_repo_root_succeeds_test() ->
    with_repo_root_project(fun(Top, RelFile) ->
        {ok, Resolved} = beamtalk_git:repo_toplevel(git_revert_file, Top),
        ?assertEqual(canonical(Top), canonical(Resolved)),
        {ok, _, RevCode} = beamtalk_git:run_git_in(
            git_revert_file, [<<"restore">>, <<"--">>, RelFile], Top
        ),
        ?assertEqual(0, RevCode)
    end).

%% `git_diff/1` shared the same cwd contract as the mutating ops and had the
%% same latent subdir bug: diffing a repo-root-relative path from the subdir
%% silently returned empty. At the toplevel it now produces a real diff
%% (BT-2608 review: lock the diff path-base in too).
diff_in_subdir_project_returns_nonempty_test() ->
    with_subdir_repo(fun(_Top, ProjectDir, RelFile) ->
        {ok, Top} = beamtalk_git:repo_toplevel(git_diff, ProjectDir),
        {ok, Diff, 0} = beamtalk_git:run_git_in(
            git_diff, [<<"diff">>, <<"--">>, RelFile], Top
        ),
        %% Non-empty unified diff mentioning the file means the pathspec matched.
        ?assertNotEqual(<<>>, Diff),
        ?assertNotEqual(nomatch, binary:match(Diff, RelFile))
    end).

%% repo_toplevel/2 surfaces the structured not-a-git-repository error when the
%% project dir is outside any git repo, rather than crashing (covers the
%% not_a_repo_error branch end to end).
repo_toplevel_outside_repo_errors_test() ->
    Dir = make_temp_dir(),
    try
        ?assertMatch(
            {error, _},
            beamtalk_git:repo_toplevel(git_status, list_to_binary(Dir))
        )
    after
        rm_rf(Dir)
    end.

%%% ============================================================================
%%% Repo toplevel caching through beamtalk_workspace_meta (BT-2621)
%%%
%%% These route real git ops through run_git/2 (which reads project_path from the
%%% workspace_meta gen_server) to prove the toplevel is resolved once and cached,
%%% with no behavioural regression vs BT-2608 (subdir status still correct).
%%% ============================================================================

%% A git op populates the per-project-path toplevel cache and subsequent ops
%% reuse it, while status still reports the modified subdir file under its
%% repo-root-relative path.
git_status_populates_toplevel_cache_test() ->
    with_subdir_repo(fun(Top, ProjectDir, RelFile) ->
        stop_meta_if_running(),
        {ok, Pid} = beamtalk_workspace_meta:start_link(#{
            workspace_id => <<"git_cache_test">>,
            project_path => ProjectDir,
            created_at => erlang:system_time(second),
            repl => false
        }),
        try
            %% Cold cache: nothing stored for this project path yet.
            ?assertEqual(miss, beamtalk_workspace_meta:get_git_toplevel(ProjectDir)),

            %% First status resolves + caches the toplevel and still lists the
            %% modified file under its repo-root-relative path (BT-2608 contract).
            {ok, Status1} = beamtalk_git:git_status(),
            Paths1 = [maps:get(path, F) || F <- maps:get(files, Status1)],
            ?assert(lists:member(RelFile, Paths1)),

            %% Barrier: a synchronous call flushes the set_git_toplevel cast that
            %% run_git/2 emitted (sent before this call from the same process).
            {ok, _} = beamtalk_workspace_meta:get_metadata(),

            %% The cache now holds the resolved repo toplevel.
            ?assertEqual(
                {ok, canonical(Top)},
                cache_canonical(beamtalk_workspace_meta:get_git_toplevel(ProjectDir))
            ),

            %% A second status reuses the cache and behaves identically.
            {ok, Status2} = beamtalk_git:git_status(),
            Paths2 = [maps:get(path, F) || F <- maps:get(files, Status2)],
            ?assert(lists:member(RelFile, Paths2))
        after
            gen_server:stop(Pid)
        end
    end).

stop_meta_if_running() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            timer:sleep(10)
    end.

%% Canonicalise the toplevel inside an {ok, _} cache result so it compares equal
%% to canonical(Top) across symlinked temp dirs (macOS /tmp → /private/tmp).
cache_canonical({ok, Top}) -> {ok, canonical(Top)};
cache_canonical(Other) -> Other.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% Resolve symlinks so macOS `/tmp` → `/private/tmp` compares equal to the path
%% `git rev-parse --show-toplevel` reports. `pwd -P` follows every symlink.
canonical(Path) when is_binary(Path) ->
    list_to_binary(canonical(binary_to_list(Path)));
canonical(Path) ->
    string:trim(os:cmd("cd " ++ shell_quote(Path) ++ " && pwd -P")).

%% Create a throwaway git repo with a committed file inside a *subdirectory*
%% project, modify that file, run Fun(Toplevel, ProjectDir, RepoRelFile), then
%% clean up. RepoRelFile is the path as `git status` reports it (repo-root
%% relative, e.g. <<"sub/proj/src/counter.bt">>).
with_subdir_repo(Fun) ->
    Top = make_temp_repo(),
    try
        ProjectRel = "sub/proj",
        FileRel = ProjectRel ++ "/src/counter.bt",
        ok = filelib:ensure_dir(filename:join(Top, FileRel)),
        ok = file:write_file(filename:join(Top, FileRel), <<"original\n">>),
        git_in(Top, ["add", "-A"]),
        commit(Top),
        ok = file:write_file(filename:join(Top, FileRel), <<"modified\n">>),
        Fun(
            list_to_binary(Top),
            list_to_binary(filename:join(Top, ProjectRel)),
            list_to_binary(FileRel)
        )
    after
        rm_rf(Top)
    end.

%% As above but the project root *is* the repo toplevel (no subdir).
with_repo_root_project(Fun) ->
    Top = make_temp_repo(),
    try
        FileRel = "src/counter.bt",
        ok = filelib:ensure_dir(filename:join(Top, FileRel)),
        ok = file:write_file(filename:join(Top, FileRel), <<"original\n">>),
        git_in(Top, ["add", "-A"]),
        commit(Top),
        ok = file:write_file(filename:join(Top, FileRel), <<"modified\n">>),
        Fun(list_to_binary(Top), list_to_binary(FileRel))
    after
        rm_rf(Top)
    end.

%% Create a unique empty temp dir (not a git repo).
make_temp_dir() ->
    Base = filename:basedir(user_cache, "beamtalk_git_test"),
    ok = filelib:ensure_dir(filename:join(Base, "x")),
    Unique = lists:flatten(
        io_lib:format("repo_~p_~p", [erlang:unique_integer([positive]), os:getpid()])
    ),
    Dir = filename:join(Base, Unique),
    ok = file:make_dir(Dir),
    Dir.

%% Create a unique temp dir and `git init` it with a deterministic identity.
make_temp_repo() ->
    Dir = make_temp_dir(),
    git_in(Dir, ["init", "-q"]),
    git_in(Dir, ["config", "user.email", "test@example.com"]),
    git_in(Dir, ["config", "user.name", "Test"]),
    git_in(Dir, ["config", "commit.gpgsign", "false"]),
    Dir.

commit(Dir) ->
    git_in(Dir, ["commit", "-q", "-m", "init"]).

%% Run git synchronously in Dir via os:cmd, asserting it does not blow up. Used
%% only for test *setup* (the code under test is beamtalk_git's own runner).
git_in(Dir, Args) ->
    Cmd =
        "git -C " ++ shell_quote(Dir) ++ " " ++
            string:join([shell_quote(A) || A <- Args], " "),
    _ = os:cmd(Cmd),
    ok.

shell_quote(S) ->
    "'" ++ lists:flatten(string:replace(S, "'", "'\\''", all)) ++ "'".

rm_rf(Dir) ->
    _ = os:cmd("rm -rf " ++ shell_quote(Dir)),
    ok.

%% Join records with a trailing NUL after each, matching `-z` output.
nul_join(Records) ->
    iolist_to_binary([[R, 0] || R <- Records]).

%% A `2` (rename) entry followed by its original-path NUL record, plus a header.
nul_join_with_orig(Header, Entry, Orig) ->
    iolist_to_binary([Header, 0, Entry, 0, Orig, 0]).

%% Build one log record terminated by RS, mirroring the custom --format. Field
%% order matches git_log/1: %H, %h, %an, %aI, %s (subject LAST).
log_record(Sha, Short, Author, Ts, Subject) ->
    FS = 16#1f,
    RS = 16#1e,
    <<Sha/binary, FS, Short/binary, FS, Author/binary, FS, Ts/binary, FS, Subject/binary, RS>>.
