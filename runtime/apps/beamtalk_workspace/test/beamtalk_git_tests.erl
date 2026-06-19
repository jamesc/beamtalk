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

%%% ============================================================================
%%% Log parsing (git log --format=%H<FS>%h<FS>%an<FS>%aI<FS>%s<RS>)
%%% ============================================================================

log_single_commit_test() ->
    Bin = log_record(
        <<"abc123def456">>,
        <<"abc123d">>,
        <<"feat: add git panel">>,
        <<"Jane Dev">>,
        <<"2026-06-19T10:00:00+00:00">>
    ),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(<<"abc123def456">>, maps:get(sha, Commit)),
    ?assertEqual(<<"abc123d">>, maps:get(short_sha, Commit)),
    ?assertEqual(<<"feat: add git panel">>, maps:get(subject, Commit)),
    ?assertEqual(<<"Jane Dev">>, maps:get(author, Commit)),
    ?assertEqual(<<"2026-06-19T10:00:00+00:00">>, maps:get(ts, Commit)).

log_multiple_commits_in_order_test() ->
    Bin = <<
        (log_record(<<"sha1">>, <<"s1">>, <<"first">>, <<"A">>, <<"t1">>))/binary,
        (log_record(<<"sha2">>, <<"s2">>, <<"second">>, <<"B">>, <<"t2">>))/binary
    >>,
    Commits = beamtalk_git:parse_log(Bin),
    ?assertEqual(2, length(Commits)),
    ?assertEqual([<<"first">>, <<"second">>], [maps:get(subject, C) || C <- Commits]).

%% A subject containing the field separator's neighbours (spaces, colons) parses
%% intact because fields are delimited by US, not whitespace.
log_subject_with_punctuation_test() ->
    Bin = log_record(
        <<"sha">>, <<"s">>, <<"fix: handle a, b; c (edge: case)">>, <<"Dev">>, <<"t">>
    ),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(<<"fix: handle a, b; c (edge: case)">>, maps:get(subject, Commit)).

%% A subject containing the field-separator byte (US, 0x1F) must not drop the
%% commit: because subject is the last field, trailing parts rejoin into it
%% (BT-2586 review M3).
log_subject_with_field_separator_test() ->
    FS = 16#1f,
    Subject = <<"weird", FS, "subject">>,
    Bin = log_record(<<"sha">>, <<"s">>, Subject, <<"Dev">>, <<"t">>),
    [Commit] = beamtalk_git:parse_log(Bin),
    ?assertEqual(Subject, maps:get(subject, Commit)),
    ?assertEqual(<<"sha">>, maps:get(sha, Commit)).

log_empty_test() ->
    ?assertEqual([], beamtalk_git:parse_log(<<>>)).

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

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% Join records with a trailing NUL after each, matching `-z` output.
nul_join(Records) ->
    iolist_to_binary([[R, 0] || R <- Records]).

%% A `2` (rename) entry followed by its original-path NUL record, plus a header.
nul_join_with_orig(Header, Entry, Orig) ->
    iolist_to_binary([Header, 0, Entry, 0, Orig, 0]).

%% Build one log record terminated by RS, mirroring the custom --format. Field
%% order matches git_log/1: %H, %h, %an, %aI, %s (subject LAST).
log_record(Sha, Short, Subject, Author, Ts) ->
    FS = 16#1f,
    RS = 16#1e,
    <<Sha/binary, FS, Short/binary, FS, Author/binary, FS, Ts/binary, FS, Subject/binary, RS>>.
