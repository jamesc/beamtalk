%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_git).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for the git working tree: `git-status`, `git-diff`, `git-log`,
`git-stage`, `git-unstage`, `git-commit` and `git-revert-file` (BT-5205).

Each is the one implementation of a capability the LiveView Git panel used to
reach by calling `beamtalk_git` directly. `beamtalk_git` is the chokepoint: a
shell-out to system `git` (never through a shell; every argument is one argv
entry), so hooks, signing and config apply to the mutating ops.

All answer `{value, Map}` (`handle_term/4`), wire-shaped by
`beamtalk_repl_wire:project/1`: binary keys, `null` for an absent value,
status atoms as their names. `handle/4` is the WebSocket-edge JSON wrapper.
They ignore `SessionPid`: the working tree is workspace-global. A failure is
`{error, #beamtalk_error{}}`; a missing project path, a directory that is not
a repository and a `git` that is not installed all arrive that way.

## Paths and limits

A `path` is checked by the shared project-path policy
(`beamtalk_repl_paths:check_relative/1`) before git runs: relative, no `..`
segment, no NUL byte. It is then passed to git as a literal pathspec, so
`:(top)`-style magic or a glob in a name cannot reach outside it. A refused
path is an `invalid_argument` error and git is not started.

Output is bounded: `git-diff` returns at most 1 MiB per side
(`truncated` is `true` when it cut), `git-log` at most 500 commits (the
`limit` it applied is in the reply) and `git-status` at most 5000 files
(`truncated`). Diff text is always valid UTF-8; bytes that are not are read
as Latin-1. A commit message is at most 64 KiB.

## Results

| Op | Params | `{value, Map}` |
|----|--------|----------------|
| `git-status` | none | `branch`, `upstream`, `ahead`, `behind`, `files` (`path`, `index`, `worktree`), `truncated` |
| `git-diff` | `path` | `path`, `worktree`, `staged`, `truncated` |
| `git-log` | optional `count` (default 20) | `commits` (`sha`, `short_sha`, `subject`, `author`, `ts`), `limit` |
| `git-stage`, `git-unstage`, `git-revert-file` | `path` | `path` |
| `git-commit` | `message` | `committed` (`true`) |
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

-define(MAX_DIFF_BYTES, 1048576).
-define(MAX_LOG_COUNT, 500).
-define(DEFAULT_LOG_COUNT, 20).
-define(MAX_STATUS_FILES, 5000).
-define(MAX_MESSAGE_BYTES, 65536).

-doc "WebSocket-edge wrapper: encodes the term result to JSON.".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc "Term-returning handler for the git ops.".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"git-status">>, _Params, _Msg, _SessionPid) ->
    case beamtalk_git:git_status() of
        {ok, Status} -> {value, status(Status)};
        {error, Reason} -> {error, structured(Reason)}
    end;
handle_term(<<"git-diff">>, Params, _Msg, _SessionPid) ->
    with_path(<<"git-diff">>, Params, fun(Path) ->
        case beamtalk_git:git_diff(Path) of
            {ok, #{worktree := Worktree, staged := Staged}} ->
                {WorktreeText, WorktreeCut} = bounded_text(Worktree),
                {StagedText, StagedCut} = bounded_text(Staged),
                {value, #{
                    <<"path">> => Path,
                    <<"worktree">> => WorktreeText,
                    <<"staged">> => StagedText,
                    <<"truncated">> => WorktreeCut orelse StagedCut
                }};
            {error, Reason} ->
                {error, structured(Reason)}
        end
    end);
handle_term(<<"git-log">>, Params, _Msg, _SessionPid) ->
    case log_limit(maps:get(<<"count">>, Params, undefined)) of
        {ok, Limit} ->
            case beamtalk_git:git_log(Limit) of
                {ok, Commits} ->
                    {value, #{
                        <<"commits">> => beamtalk_repl_wire:project(Commits),
                        <<"limit">> => Limit
                    }};
                {error, Reason} ->
                    {error, structured(Reason)}
            end;
        {error, _} = Err ->
            Err
    end;
handle_term(<<"git-stage">>, Params, _Msg, _SessionPid) ->
    mutate_path(<<"git-stage">>, Params, fun beamtalk_git:git_stage/1);
handle_term(<<"git-unstage">>, Params, _Msg, _SessionPid) ->
    mutate_path(<<"git-unstage">>, Params, fun beamtalk_git:git_unstage/1);
handle_term(<<"git-revert-file">>, Params, _Msg, _SessionPid) ->
    mutate_path(<<"git-revert-file">>, Params, fun beamtalk_git:git_revert_file/1);
handle_term(<<"git-commit">>, Params, _Msg, _SessionPid) ->
    case maps:get(<<"message">>, Params, undefined) of
        Message when is_binary(Message), byte_size(Message) =< ?MAX_MESSAGE_BYTES ->
            case beamtalk_git:git_commit(Message) of
                {ok, _} -> {value, #{<<"committed">> => true}};
                {error, Reason} -> {error, structured(Reason)}
            end;
        Message when is_binary(Message) ->
            {error,
                invalid(
                    <<"git-commit: message is longer than 64 KiB">>,
                    <<"Shorten the commit message.">>
                )};
        _ ->
            {error,
                invalid(
                    <<"git-commit: message must be a String">>,
                    <<"Pass the commit message as the `message` param.">>
                )}
    end.

%%====================================================================
%% Params
%%====================================================================

-spec with_path(binary(), map(), fun((binary()) -> beamtalk_repl_ops:op_result())) ->
    beamtalk_repl_ops:op_result().
with_path(Op, Params, Fun) ->
    Path = maps:get(<<"path">>, Params, undefined),
    case beamtalk_repl_paths:check_relative(Path) of
        ok -> Fun(Path);
        {error, Refusal} -> {error, refused_path(Op, Refusal)}
    end.

-spec mutate_path(binary(), map(), fun((binary()) -> {ok, term()} | {error, term()})) ->
    beamtalk_repl_ops:op_result().
mutate_path(Op, Params, Git) ->
    with_path(Op, Params, fun(Path) ->
        case Git(Path) of
            {ok, _} -> {value, #{<<"path">> => Path}};
            {error, Reason} -> {error, structured(Reason)}
        end
    end).

%% `count` is absent (the default) or a positive integer, clamped to the cap;
%% anything else is refused rather than read as the default.
-spec log_limit(term()) -> {ok, pos_integer()} | {error, #beamtalk_error{}}.
log_limit(undefined) ->
    {ok, ?DEFAULT_LOG_COUNT};
log_limit(Count) when is_integer(Count), Count > 0 ->
    {ok, min(Count, ?MAX_LOG_COUNT)};
log_limit(_Other) ->
    {error,
        invalid(
            <<"git-log: count must be a positive Integer">>,
            <<"Pass count as a whole number, or omit it for the last 20 commits.">>
        )}.

-spec refused_path(binary(), beamtalk_repl_paths:refusal()) -> #beamtalk_error{}.
refused_path(Op, Refusal) ->
    invalid(
        iolist_to_binary([Op, <<": ">>, refusal_text(Refusal)]),
        <<"Pass a path relative to the project root, such as src/Counter.bt, without '..'.">>
    ).

-spec refusal_text(beamtalk_repl_paths:refusal()) -> binary().
refusal_text(not_a_string) -> <<"path must be a String">>;
refusal_text(empty) -> <<"path must not be empty">>;
refusal_text(nul) -> <<"path must not contain a NUL byte">>;
refusal_text(absolute) -> <<"path must be relative to the project root">>;
refusal_text(parent_segment) -> <<"path must not contain '..'">>.

-spec invalid(binary(), binary()) -> #beamtalk_error{}.
invalid(Message, Hint) ->
    beamtalk_repl_errors:make(invalid_argument, 'Workspace', Message, Hint).

-spec structured(term()) -> #beamtalk_error{}.
structured(Reason) ->
    beamtalk_repl_errors:ensure_structured_error(Reason).

%%====================================================================
%% Results
%%====================================================================

-spec status(map()) -> map().
status(#{files := Files} = Status) ->
    {Kept, Truncated} =
        case length(Files) > ?MAX_STATUS_FILES of
            true -> {lists:sublist(Files, ?MAX_STATUS_FILES), true};
            false -> {Files, false}
        end,
    (beamtalk_repl_wire:project(maps:without([files], Status)))#{
        <<"files">> => beamtalk_repl_wire:project(Kept),
        <<"truncated">> => Truncated
    }.

%% One side of a diff, cut at the cap and made valid UTF-8 (the wire is JSON).
-spec bounded_text(binary()) -> {binary(), boolean()}.
bounded_text(Bin) when byte_size(Bin) > ?MAX_DIFF_BYTES ->
    {utf8(binary:part(Bin, 0, ?MAX_DIFF_BYTES)), true};
bounded_text(Bin) ->
    {utf8(Bin), false}.

-spec utf8(binary()) -> binary().
utf8(Bin) ->
    case unicode:characters_to_binary(Bin, utf8, utf8) of
        Valid when is_binary(Valid) ->
            Valid;
        %% Cut inside a multi-byte character: keep the whole characters.
        {incomplete, Valid, _Rest} ->
            Valid;
        {error, _Valid, _Rest} ->
            unicode:characters_to_binary(Bin, latin1, utf8)
    end.

%%====================================================================
%% Op descriptors for dynamic describe
%%====================================================================

-doc "Return op descriptors for the git ops.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).
