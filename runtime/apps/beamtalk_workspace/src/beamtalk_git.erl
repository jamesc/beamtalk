%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_git).

%%% **DDD Context:** Workspace Context

-moduledoc """
Git operations on the workspace project root (ADR 0082 Amendment 1, BT-2586).

The cockpit's post-flush, human-facing VCS surface. Beamtalk's `.bt` files
*are* the git working tree (ADR 0004), so this is a thin shell-out to the
system `git` binary rather than a libgit2 NIF: a subprocess isolates failures
to an OS process (preserving the BEAM fault-isolation thesis) and mutating ops
honour the user's hooks, GPG signing, credential helpers and `git config` for
free.

Runs on the workspace node (where the `.bt` working tree lives). The git
working tree is resolved from `beamtalk_workspace_meta:get_metadata/0` →
`project_path`.

== Operations ==

Read ops parse machine formats into typed Erlang maps at this seam:

* `git_status/0` → `#{branch, upstream, ahead, behind, files => [#{path, index, worktree}]}`
  (parsed from `git status --porcelain=v2 -b -z`).
* `git_diff/1` (Path) → `#{worktree, staged}` unified-diff binaries (verbatim).
* `git_log/1` (Count) → `[#{sha, short_sha, subject, author, ts}]`
  (parsed from `git log -n <count> --format=...`).

Mutating ops invoke system `git` so hooks/signing/credentials/config apply:

* `git_stage/1` (Path), `git_unstage/1` (Path), `git_commit/1` (Message),
  `git_revert_file/1` (Path) — the human counterpart to the agent ChangeLog
  `revert:`.

Every function returns `{ok, Term} | {error, #beamtalk_error{}}`. A missing
project path, a directory that is not a git repository, or `git` absent from
`$PATH` all produce a structured error for graceful degradation — the node is
never crashed.

== References ==

* ADR 0082 Amendment 1 (BT-2585) — surface split: cockpit = git-first (humans)
* ADR 0004 — `.bt` files are the source of truth (the git working tree)
* ADR 0051 — subprocess execution (the `beamtalk_exec` port reused here)
* ADR 0097 — workspace node seam
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Read ops (Observer-visible)
-export([git_status/0, git_diff/1, git_log/1]).
%% Mutating ops (Owner-gated)
-export([git_stage/1, git_unstage/1, git_commit/1, git_revert_file/1]).

-ifdef(TEST).
-export([
    parse_status/1,
    parse_log/1,
    classify_xy/1
]).
-endif.

%% Field separator (US) and record separator (RS) used in the log format.
-define(FS, 16#1f).
-define(RS, 16#1e).

%% Upper bound on how long a single git invocation may run before we give up
%% and return a structured timeout error rather than blocking the caller.
-define(GIT_TIMEOUT_MS, 30000).

%%% ============================================================================
%%% Read operations (Observer-visible)
%%% ============================================================================

-doc """
Working-tree status as a typed map.

Returns `{ok, #{branch, upstream, ahead, behind, files}}` where `files` is a
list of `#{path, index, worktree}`. `index`/`worktree` are short status
classifications (e.g. `modified`, `added`, `untracked`, `unmodified`).

Parsed from `git status --porcelain=v2 -b -z`; NUL-delimited so paths with
spaces or unicode are handled without whitespace splitting.
""".
-spec git_status() -> {ok, map()} | {error, #beamtalk_error{}}.
git_status() ->
    case run_git(git_status, [<<"status">>, <<"--porcelain=v2">>, <<"-b">>, <<"-z">>]) of
        {ok, Stdout, 0} ->
            {ok, parse_status(Stdout)};
        {ok, _Stdout, Code} ->
            {error, exit_error(git_status, Code, <<"git status failed">>)};
        {error, _} = Err ->
            Err
    end.

-doc """
Unified diff for a single path: working-tree-vs-HEAD and staged-vs-HEAD.

Returns `{ok, #{worktree => Binary, staged => Binary}}`, the diff text passed
through verbatim. An empty binary means no change on that side.
""".
-spec git_diff(binary()) -> {ok, map()} | {error, #beamtalk_error{}}.
git_diff(Path) when is_binary(Path) ->
    case run_git(git_diff, [<<"diff">>, <<"--">>, Path]) of
        {ok, WorktreeDiff, 0} ->
            case run_git(git_diff, [<<"diff">>, <<"--staged">>, <<"--">>, Path]) of
                {ok, StagedDiff, 0} ->
                    {ok, #{worktree => WorktreeDiff, staged => StagedDiff}};
                {ok, _, Code} ->
                    {error, exit_error(git_diff, Code, <<"git diff --staged failed">>)};
                {error, _} = Err ->
                    Err
            end;
        {ok, _, Code} ->
            {error, exit_error(git_diff, Code, <<"git diff failed">>)};
        {error, _} = Err ->
            Err
    end;
git_diff(_Path) ->
    {error, arg_error(git_diff, <<"path must be a String">>)}.

-doc """
The most recent `Count` commits.

Returns `{ok, [#{sha, short_sha, subject, author, ts}]}` parsed from
`git log` with a NUL/US/RS-delimited custom format so subjects with arbitrary
punctuation parse cleanly.
""".
-spec git_log(integer()) -> {ok, [map()]} | {error, #beamtalk_error{}}.
git_log(Count) when is_integer(Count), Count > 0 ->
    %% Subject (%s) is arbitrary user text and is placed LAST so that, even if it
    %% contains a literal US byte, the trailing fields rejoin unambiguously into
    %% the subject (parse_log_record reassembles everything past the 4th field).
    Format = <<"--format=%H", ?FS, "%h", ?FS, "%an", ?FS, "%aI", ?FS, "%s", ?RS>>,
    Args = [<<"log">>, <<"-n">>, integer_to_binary(Count), Format],
    case run_git(git_log, Args) of
        {ok, Stdout, 0} ->
            {ok, parse_log(Stdout)};
        {ok, _, Code} ->
            {error, exit_error(git_log, Code, <<"git log failed">>)};
        {error, _} = Err ->
            Err
    end;
git_log(_Count) ->
    {error, arg_error(git_log, <<"count must be a positive Integer">>)}.

%%% ============================================================================
%%% Mutating operations (Owner-gated)
%%% ============================================================================

-doc "Stage a single path (`git add -- <path>`). Returns `{ok, nil}`.".
-spec git_stage(binary()) -> {ok, nil} | {error, #beamtalk_error{}}.
git_stage(Path) when is_binary(Path) ->
    mutate(git_stage, [<<"add">>, <<"--">>, Path], <<"git add failed">>);
git_stage(_Path) ->
    {error, arg_error(git_stage, <<"path must be a String">>)}.

-doc "Unstage a single path (`git restore --staged -- <path>`). Returns `{ok, nil}`.".
-spec git_unstage(binary()) -> {ok, nil} | {error, #beamtalk_error{}}.
git_unstage(Path) when is_binary(Path) ->
    mutate(
        git_unstage,
        [<<"restore">>, <<"--staged">>, <<"--">>, Path],
        <<"git restore --staged failed">>
    );
git_unstage(_Path) ->
    {error, arg_error(git_unstage, <<"path must be a String">>)}.

-doc """
Commit the staged index with `Message` (`git commit -m <message>`).

Invokes system `git` so commit hooks, GPG signing and `user.name`/`user.email`
config apply. Returns `{ok, nil}`.
""".
-spec git_commit(binary()) -> {ok, nil} | {error, #beamtalk_error{}}.
git_commit(Message) when is_binary(Message), Message =/= <<>> ->
    mutate(git_commit, [<<"commit">>, <<"-m">>, Message], <<"git commit failed">>);
git_commit(Message) when is_binary(Message) ->
    {error, arg_error(git_commit, <<"commit message must not be empty">>)};
git_commit(_Message) ->
    {error, arg_error(git_commit, <<"commit message must be a String">>)}.

-doc """
Discard a working-tree change for a single path (`git restore -- <path>`).

The human counterpart to the agent ChangeLog `revert:`. Returns `{ok, nil}`.
""".
-spec git_revert_file(binary()) -> {ok, nil} | {error, #beamtalk_error{}}.
git_revert_file(Path) when is_binary(Path) ->
    mutate(git_revert_file, [<<"restore">>, <<"--">>, Path], <<"git restore failed">>);
git_revert_file(_Path) ->
    {error, arg_error(git_revert_file, <<"path must be a String">>)}.

%%% ============================================================================
%%% Mutating helper
%%% ============================================================================

-spec mutate(atom(), [binary()], binary()) -> {ok, nil} | {error, #beamtalk_error{}}.
mutate(Selector, Args, FailMsg) ->
    case run_git(Selector, Args) of
        {ok, _Stdout, 0} ->
            {ok, nil};
        {ok, Stdout, Code} ->
            {error, exit_error(Selector, Code, with_stderr(FailMsg, Stdout))};
        {error, _} = Err ->
            Err
    end.

%%% ============================================================================
%%% Subprocess execution (ADR 0051 — reuses the beamtalk_exec port)
%%% ============================================================================

%% Run `git` with the given argument list in the workspace project directory,
%% blocking until the child exits. Returns the combined stdout (verbatim
%% bytes — NULs and newlines preserved) plus the exit code, or a structured
%% error when the project path is missing, `git` is absent, or the call times
%% out. stderr is captured and folded into error messages.
-spec run_git(atom(), [binary()]) ->
    {ok, binary(), non_neg_integer()} | {error, #beamtalk_error{}}.
run_git(Selector, Args) ->
    case project_dir() of
        {ok, Dir} ->
            run_git_in(Selector, Args, Dir);
        {error, _} = Err ->
            Err
    end.

-spec run_git_in(atom(), [binary()], binary()) ->
    {ok, binary(), non_neg_integer()} | {error, #beamtalk_error{}}.
run_git_in(Selector, Args, Dir) ->
    ChildId = 0,
    try beamtalk_exec_port:open() of
        Port ->
            try
                beamtalk_exec_port:spawn_child(Port, ChildId, <<"git">>, Args, #{dir => Dir}),
                collect(Selector, Port, ChildId)
            after
                catch beamtalk_exec_port:close(Port),
                %% Drain any trailing {Port, _} messages (e.g. the one-shot exec
                %% binary's own {exit_status} delivered after the child's {exit},
                %% or messages from a timed-out child) so they never linger in the
                %% caller's mailbox. Harmless on the short-lived rpc proxy, but
                %% keeps the helper safe to call from a long-lived process too.
                flush_port(Port)
            end
    catch
        error:{exec_not_found, _} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(
                        runtime_error,
                        'Git',
                        Selector,
                        <<"the git subprocess runner is unavailable">>
                    ),
                    <<"the beamtalk-exec helper binary was not found">>
                )};
        Kind:Reason ->
            ?LOG_ERROR("git subprocess failed to start", #{
                selector => Selector,
                error => {Kind, Reason},
                domain => [beamtalk, runtime]
            }),
            {error,
                beamtalk_error:new(
                    runtime_error, 'Git', Selector, <<"failed to start git subprocess">>
                )}
    end.

%% Receive-loop accumulating raw stdout/stderr bytes until the child exits.
%% The beamtalk-exec binary joins its reader threads before sending the exit
%% event (BT-1148), so by the time we see {exit, ...} all output has arrived.
-spec collect(atom(), port(), non_neg_integer()) ->
    {ok, binary(), non_neg_integer()} | {error, #beamtalk_error{}}.
collect(Selector, Port, ChildId) ->
    collect(Selector, Port, ChildId, <<>>, <<>>).

-spec collect(atom(), port(), non_neg_integer(), binary(), binary()) ->
    {ok, binary(), non_neg_integer()} | {error, #beamtalk_error{}}.
collect(Selector, Port, ChildId, Stdout, Stderr) ->
    receive
        {Port, {data, Packet}} ->
            case erlang:binary_to_term(Packet, [safe]) of
                {stdout, ChildId, Data} ->
                    collect(Selector, Port, ChildId, <<Stdout/binary, Data/binary>>, Stderr);
                {stderr, ChildId, Data} ->
                    collect(Selector, Port, ChildId, Stdout, <<Stderr/binary, Data/binary>>);
                {exit, ChildId, Code} when is_integer(Code) ->
                    finish(Selector, Code, Stdout, Stderr);
                _Other ->
                    collect(Selector, Port, ChildId, Stdout, Stderr)
            end;
        {Port, {exit_status, _N}} ->
            %% The exec binary itself exited before sending an {exit} event.
            {error,
                beamtalk_error:new(
                    runtime_error, 'Git', Selector, <<"git subprocess terminated unexpectedly">>
                )}
    after ?GIT_TIMEOUT_MS ->
        ?LOG_WARNING("git subprocess timed out", #{
            selector => Selector, timeout_ms => ?GIT_TIMEOUT_MS, domain => [beamtalk, runtime]
        }),
        {error, beamtalk_error:new(timeout, 'Git', Selector, <<"git command timed out">>)}
    end.

%% Non-blockingly drain any remaining messages from this port out of the mailbox.
-spec flush_port(port()) -> ok.
flush_port(Port) ->
    receive
        {Port, _} -> flush_port(Port)
    after 0 ->
        ok
    end.

%% On a clean (zero) exit return stdout. On a non-zero exit, surface stderr in
%% the error so callers can show git's own message (e.g. "not a git repository").
-spec finish(atom(), integer(), binary(), binary()) ->
    {ok, binary(), non_neg_integer()} | {error, #beamtalk_error{}}.
finish(_Selector, 0, Stdout, _Stderr) ->
    {ok, Stdout, 0};
finish(Selector, Code, Stdout, Stderr) ->
    case is_not_a_repo(Stderr) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(
                        file_not_found,
                        'Git',
                        Selector,
                        <<"the workspace project is not a git repository">>
                    ),
                    <<"run `git init` in the project root to enable the git panel">>
                )};
        false ->
            %% Return the combined stdout/stderr so the non-zero path still
            %% carries git's own message; callers fold it into the error.
            {ok, with_stderr(Stdout, Stderr), Code}
    end.

%%% ============================================================================
%%% Status parsing (git status --porcelain=v2 -b -z)
%%% ============================================================================

%% Porcelain v2 with `-b` emits header lines (`# branch.oid`, `# branch.head`,
%% `# branch.upstream`, `# branch.ab`) then one entry per changed path. With
%% `-z`, records are NUL-terminated. Ordinary/changed entries (`1`/`2`) carry an
%% `<XY>` two-char status field; untracked (`?`) and ignored (`!`) entries carry
%% only a path.
-spec parse_status(binary()) -> map().
parse_status(Bin) ->
    Records = split_nul(Bin),
    fold_status(Records, #{
        branch => nil,
        upstream => nil,
        ahead => 0,
        behind => 0,
        files => []
    }).

-spec fold_status([binary()], map()) -> map().
fold_status([], Acc) ->
    %% Files were prepended for O(1) accumulation; restore source order.
    Acc#{files => lists:reverse(maps:get(files, Acc))};
fold_status([<<"# branch.head ", Head/binary>> | Rest], Acc) ->
    Branch =
        case Head of
            <<"(detached)">> -> nil;
            _ -> Head
        end,
    fold_status(Rest, Acc#{branch => Branch});
fold_status([<<"# branch.upstream ", Up/binary>> | Rest], Acc) ->
    fold_status(Rest, Acc#{upstream => Up});
fold_status([<<"# branch.ab ", AB/binary>> | Rest], Acc) ->
    {Ahead, Behind} = parse_ahead_behind(AB),
    fold_status(Rest, Acc#{ahead => Ahead, behind => Behind});
fold_status([<<"# ", _/binary>> | Rest], Acc) ->
    %% Other header lines (branch.oid, etc.) — ignored.
    fold_status(Rest, Acc);
fold_status([<<"? ", Path/binary>> | Rest], Acc) ->
    add_file(Rest, Acc, #{path => Path, index => unmodified, worktree => untracked});
fold_status([<<"! ", Path/binary>> | Rest], Acc) ->
    add_file(Rest, Acc, #{path => Path, index => unmodified, worktree => ignored});
fold_status([<<"1 ", Entry/binary>> | Rest], Acc) ->
    %% Ordinary entry: 6 metadata fields precede the path.
    add_ordinary(Rest, Acc, Entry, 6);
fold_status([<<"2 ", Entry/binary>> | Rest], Acc) ->
    %% Rename/copy entry: 7 metadata fields precede the path (the extra field is
    %% the `<X><score>` rename/copy score). Under `-z` the *original* path is
    %% emitted as a separate NUL record (never tab-joined to the new path), which
    %% we skip — the new path stays verbatim, tabs and all.
    Rest1 =
        case Rest of
            [_OrigPath | RestAfterOrig] -> RestAfterOrig;
            [] -> []
        end,
    add_ordinary(Rest1, Acc, Entry, 7);
fold_status([<<"u ", Entry/binary>> | Rest], Acc) ->
    %% Unmerged (merge-conflict) entry. After the `xy` pair, 8 metadata fields
    %% precede the path — "<xy> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>"
    %% (4 modes + 3 hashes + sub). The XY pair carries the conflict columns
    %% (e.g. `UU`), which classify_xy maps to `unmerged`.
    add_ordinary(Rest, Acc, Entry, 8);
fold_status([_Unknown | Rest], Acc) ->
    fold_status(Rest, Acc).

%% Ordinary/changed entry body. For `1`:
%%   "<XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>"          (6 fields before path)
%% For `2` (rename/copy):
%%   "<XY> <sub> <mH> <mI> <mW> <hH> <hI> <Xscore> <path>" (7 fields before path)
%% For `u` (unmerged):
%%   "<xy> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>" (8 fields before path)
%% We only need XY and the path, both space-delimited up to the path. Under `-z`
%% the path is the final field and is taken verbatim (it may contain spaces and
%% tabs), so nothing past `drop_fields` ever splits it.
-spec add_ordinary([binary()], map(), binary(), non_neg_integer()) -> map().
add_ordinary(Rest, Acc, Entry, MetaFields) ->
    case parse_ordinary(Entry, MetaFields) of
        {ok, XY, Path} ->
            {Index, Worktree} = classify_xy(XY),
            add_file(Rest, Acc, #{path => Path, index => Index, worktree => Worktree});
        error ->
            fold_status(Rest, Acc)
    end.

-spec parse_ordinary(binary(), non_neg_integer()) -> {ok, binary(), binary()} | error.
parse_ordinary(Entry, MetaFields) ->
    case binary:split(Entry, <<" ">>) of
        [XY, RestFields] ->
            %% After splitting off XY, drop the remaining metadata fields so only
            %% the path (verbatim — spaces and tabs preserved) is left.
            case drop_fields(RestFields, MetaFields) of
                {ok, Path} -> {ok, XY, Path};
                error -> error
            end;
        _ ->
            error
    end.

%% Drop N leading space-delimited fields, returning the remainder (the path,
%% which may itself contain spaces and is taken verbatim).
-spec drop_fields(binary(), non_neg_integer()) -> {ok, binary()} | error.
drop_fields(Bin, 0) ->
    {ok, Bin};
drop_fields(Bin, N) when N > 0 ->
    case binary:split(Bin, <<" ">>) of
        [_Field, Rest] -> drop_fields(Rest, N - 1);
        _ -> error
    end.

-spec add_file([binary()], map(), map()) -> map().
add_file(Rest, Acc, File) ->
    Files = maps:get(files, Acc),
    fold_status(Rest, Acc#{files => [File | Files]}).

%% "# branch.ab +A -B" → {A, B}.
-spec parse_ahead_behind(binary()) -> {integer(), integer()}.
parse_ahead_behind(AB) ->
    case binary:split(AB, <<" ">>) of
        [<<"+", A/binary>>, <<"-", B/binary>>] ->
            {to_int(A, 0), to_int(B, 0)};
        _ ->
            {0, 0}
    end.

%% Map a porcelain XY status pair to {IndexState, WorktreeState}.
%% X = index/staged column, Y = working-tree column.
-spec classify_xy(binary()) -> {atom(), atom()}.
classify_xy(<<X, Y>>) ->
    {classify_char(X), classify_char(Y)};
classify_xy(_) ->
    {unmodified, unmodified}.

-spec classify_char(char()) -> atom().
classify_char($.) -> unmodified;
classify_char($M) -> modified;
classify_char($A) -> added;
classify_char($D) -> deleted;
classify_char($R) -> renamed;
classify_char($C) -> copied;
classify_char($U) -> unmerged;
classify_char($T) -> type_changed;
classify_char(_) -> unmodified.

%%% ============================================================================
%%% Log parsing (git log --format=%H<FS>%h<FS>%an<FS>%aI<FS>%s<RS>)
%%% ============================================================================

-spec parse_log(binary()) -> [map()].
parse_log(Bin) ->
    Records = binary:split(Bin, <<?RS>>, [global]),
    lists:filtermap(fun parse_log_record/1, Records).

-spec parse_log_record(binary()) -> {true, map()} | false.
parse_log_record(Record) ->
    %% Records are separated by RS; git emits a newline between records too,
    %% so trim leading whitespace before parsing the fields.
    case string:trim(Record, leading) of
        <<>> ->
            false;
        Trimmed ->
            %% Fields: %H, %h, %an, %aI, %s. Subject is last, so rejoin any
            %% trailing fields (a subject containing US) back into the subject.
            case binary:split(Trimmed, <<?FS>>, [global]) of
                [Sha, Short, Author, Ts | SubjectParts] when SubjectParts =/= [] ->
                    {true, #{
                        sha => Sha,
                        short_sha => Short,
                        subject => iolist_to_binary(lists:join(<<?FS>>, SubjectParts)),
                        author => Author,
                        ts => Ts
                    }};
                _ ->
                    false
            end
    end.

%%% ============================================================================
%%% Project directory resolution
%%% ============================================================================

-spec project_dir() -> {ok, binary()} | {error, #beamtalk_error{}}.
project_dir() ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := Path}} when is_binary(Path), Path =/= <<>> ->
            {ok, Path};
        {ok, _} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(
                        file_not_found, 'Git', git, <<"the workspace has no project path">>
                    ),
                    <<"open a project directory to use the git panel">>
                )};
        {error, not_started} ->
            {error,
                beamtalk_error:new(
                    runtime_error, 'Git', git, <<"the workspace is not running">>
                )}
    end.

%%% ============================================================================
%%% Error/byte helpers
%%% ============================================================================

-spec arg_error(atom(), binary()) -> #beamtalk_error{}.
arg_error(Selector, Hint) ->
    beamtalk_error:with_hint(
        beamtalk_error:new(type_error, 'Git', Selector, <<"invalid git argument">>),
        Hint
    ).

-spec exit_error(atom(), integer(), binary()) -> #beamtalk_error{}.
exit_error(Selector, Code, Message) ->
    beamtalk_error:with_details(
        beamtalk_error:new(runtime_error, 'Git', Selector, Message),
        #{exit_code => Code}
    ).

%% True when git's stderr indicates the directory is not a repository. Anchored
%% to git's actual `fatal: not a git repository` message so a path/content that
%% merely mentions the phrase is not misclassified.
-spec is_not_a_repo(binary()) -> boolean().
is_not_a_repo(Stderr) ->
    nomatch =/= binary:match(Stderr, <<"fatal: not a git repository">>).

%% Append trimmed stderr to a base message when present.
-spec with_stderr(binary(), binary()) -> binary().
with_stderr(Base, Extra) ->
    case string:trim(Extra) of
        <<>> -> Base;
        Trimmed -> <<Base/binary, ": ", Trimmed/binary>>
    end.

%% Split NUL-terminated records, dropping empty records.
-spec split_nul(binary()) -> [binary()].
split_nul(Bin) ->
    [R || R <- binary:split(Bin, <<0>>, [global]), R =/= <<>>].

-spec to_int(binary(), integer()) -> integer().
to_int(Bin, Default) ->
    try
        binary_to_integer(Bin)
    catch
        error:badarg -> Default
    end.
