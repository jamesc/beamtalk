%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_git_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The git ops (BT-5205): `git-status`, `git-diff`, `git-log`, `git-stage`,
`git-unstage`, `git-commit` and `git-revert-file`.

Driven through `beamtalk_repl_ops:request/3`, against a real repository the
workspace's project path points at. The path-refusal tests assert that git did
not run: the working tree is unchanged after each refused call.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

git_ops_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(Ctx) -> {"git-status reports the branch and changed files", fun() -> status(Ctx) end} end,
        fun(Ctx) -> {"git-diff returns both sides for a path", fun() -> diff(Ctx) end} end,
        fun(Ctx) -> {"git-log lists commits and applies its limit", fun() -> log(Ctx) end} end,
        fun(Ctx) -> {"git-log clamps and validates count", fun() -> log_count(Ctx) end} end,
        fun(Ctx) -> {"stage, unstage and revert change the tree", fun() -> mutations(Ctx) end} end,
        fun(Ctx) -> {"git-commit commits the index", fun() -> commit(Ctx) end} end,
        fun(Ctx) ->
            {"a commit message is one argument, never shell text", fun() ->
                commit_message_is_literal(Ctx)
            end}
        end,
        fun(Ctx) -> {"git-commit refuses a bad message", fun() -> commit_bad_message(Ctx) end} end,
        fun(Ctx) ->
            {"every path op refuses a bad path without running git", fun() ->
                refuses_bad_paths(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"a glob or pathspec magic names a file, never a set", fun() ->
                paths_are_literal(Ctx)
            end}
        end,
        fun(Ctx) -> {"git-diff output is bounded", fun() -> diff_is_bounded(Ctx) end} end,
        fun(Ctx) ->
            {"git-diff text is valid UTF-8 whatever the file holds", fun() ->
                diff_is_utf8(Ctx)
            end}
        end,
        fun(Ctx) -> {"results encode as JSON at the edge", fun() -> encodes_as_json(Ctx) end} end,
        fun(_Ctx) -> {"a workspace that is not a repository errors", fun not_a_repo/0} end
    ]}.

setup() ->
    Top = make_repo(),
    ok = filelib:ensure_dir(filename:join(Top, "src/x")),
    ok = file:write_file(filename:join(Top, "src/counter.bt"), <<"original\n">>),
    git(Top, ["add", "-A"]),
    git(Top, ["commit", "-q", "-m", "init"]),
    beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
    {ok, MetaPid} = start_meta(Top),
    #{top => Top, meta => MetaPid}.

cleanup(#{top := Top, meta := MetaPid}) ->
    beamtalk_test_proc:shutdown(MetaPid),
    beamtalk_test_fs:rm_rf(Top),
    ok.

start_meta(Dir) ->
    beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"git-ops-test">>,
        project_path => list_to_binary(Dir),
        created_at => erlang:system_time(second),
        mode => run
    }).

request(Op, Params) ->
    beamtalk_repl_ops:request(Op, Params, self()).

modify(Top) ->
    ok = file:write_file(filename:join(Top, "src/counter.bt"), <<"modified\n">>).

read(Top) ->
    {ok, Bin} = file:read_file(filename:join(Top, "src/counter.bt")),
    Bin.

status_files() ->
    {value, #{<<"files">> := Files}} = request(<<"git-status">>, #{}),
    Files.

%%% reads

status(#{top := Top}) ->
    {value, Clean} = request(<<"git-status">>, #{}),
    ?assertEqual([], maps:get(<<"files">>, Clean)),
    ?assertEqual(false, maps:get(<<"truncated">>, Clean)),
    modify(Top),
    {value, Dirty} = request(<<"git-status">>, #{}),
    ?assertEqual(
        [
            #{
                <<"path">> => <<"src/counter.bt">>,
                <<"index">> => <<"unmodified">>,
                <<"worktree">> => <<"modified">>
            }
        ],
        maps:get(<<"files">>, Dirty)
    ),
    ?assert(maps:is_key(<<"branch">>, Dirty)),
    ?assert(maps:is_key(<<"ahead">>, Dirty)).

diff(#{top := Top}) ->
    modify(Top),
    {value, Diff} = request(<<"git-diff">>, #{<<"path">> => <<"src/counter.bt">>}),
    ?assertEqual(<<"src/counter.bt">>, maps:get(<<"path">>, Diff)),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"worktree">>, Diff), <<"+modified">>)),
    ?assertEqual(<<>>, maps:get(<<"staged">>, Diff)),
    ?assertEqual(false, maps:get(<<"truncated">>, Diff)),
    ?assertMatch({error, #beamtalk_error{}}, request(<<"git-diff">>, #{})).

log(#{top := Top}) ->
    ok = file:write_file(filename:join(Top, "src/b.bt"), <<"b\n">>),
    git(Top, ["add", "-A"]),
    git(Top, ["commit", "-q", "-m", "second"]),
    {value, All} = request(<<"git-log">>, #{}),
    ?assertEqual(20, maps:get(<<"limit">>, All)),
    ?assertMatch(
        [
            #{<<"subject">> := <<"second">>, <<"sha">> := _, <<"short_sha">> := _, <<"ts">> := _},
            #{<<"subject">> := <<"init">>, <<"author">> := <<"Test">>}
        ],
        maps:get(<<"commits">>, All)
    ),
    {value, One} = request(<<"git-log">>, #{<<"count">> => 1}),
    ?assertMatch([#{<<"subject">> := <<"second">>}], maps:get(<<"commits">>, One)).

log_count(_Ctx) ->
    {value, Big} = request(<<"git-log">>, #{<<"count">> => 100000}),
    ?assertEqual(500, maps:get(<<"limit">>, Big)),
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"git-log">>, #{<<"count">> => Bad})
            )
        end,
        [0, -3, <<"5">>, 1.5, null]
    ).

%%% mutations

mutations(#{top := Top}) ->
    modify(Top),
    ?assertEqual(
        {value, #{<<"path">> => <<"src/counter.bt">>}},
        request(<<"git-stage">>, #{<<"path">> => <<"src/counter.bt">>})
    ),
    ?assertMatch([#{<<"index">> := <<"modified">>}], status_files()),
    ?assertEqual(
        {value, #{<<"path">> => <<"src/counter.bt">>}},
        request(<<"git-unstage">>, #{<<"path">> => <<"src/counter.bt">>})
    ),
    ?assertMatch(
        [#{<<"index">> := <<"unmodified">>, <<"worktree">> := <<"modified">>}], status_files()
    ),
    ?assertEqual(
        {value, #{<<"path">> => <<"src/counter.bt">>}},
        request(<<"git-revert-file">>, #{<<"path">> => <<"src/counter.bt">>})
    ),
    ?assertEqual(<<"original\n">>, read(Top)),
    ?assertEqual([], status_files()).

commit(#{top := Top}) ->
    modify(Top),
    {value, _} = request(<<"git-stage">>, #{<<"path">> => <<"src/counter.bt">>}),
    ?assertEqual(
        {value, #{<<"committed">> => true}},
        request(<<"git-commit">>, #{<<"message">> => <<"change counter">>})
    ),
    ?assertEqual([], status_files()),
    {value, #{<<"commits">> := [#{<<"subject">> := Subject} | _]}} = request(<<"git-log">>, #{}),
    ?assertEqual(<<"change counter">>, Subject).

commit_message_is_literal(#{top := Top}) ->
    modify(Top),
    {value, _} = request(<<"git-stage">>, #{<<"path">> => <<"src/counter.bt">>}),
    Marker = filename:join(Top, "pwned"),
    Message = iolist_to_binary([<<"x\"; touch ">>, Marker, <<"; $(touch ">>, Marker, <<") `touch ">>, Marker, <<"` -n">>]),
    ?assertEqual({value, #{<<"committed">> => true}}, request(<<"git-commit">>, #{<<"message">> => Message})),
    ?assertNot(filelib:is_file(Marker)),
    {value, #{<<"commits">> := [#{<<"subject">> := Subject} | _]}} = request(<<"git-log">>, #{}),
    ?assertEqual(Message, Subject).

commit_bad_message(#{top := Top}) ->
    modify(Top),
    {value, _} = request(<<"git-stage">>, #{<<"path">> => <<"src/counter.bt">>}),
    lists:foreach(
        fun(Params) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"git-commit">>, Params)
            )
        end,
        [
            #{},
            #{<<"message">> => 5},
            #{<<"message">> => null},
            #{<<"message">> => binary:copy(<<"a">>, 65537)}
        ]
    ),
    %% A blank subject is refused by `beamtalk_git` itself, before git runs.
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error}},
        request(<<"git-commit">>, #{<<"message">> => <<"   ">>})
    ),
    {value, #{<<"commits">> := Commits}} = request(<<"git-log">>, #{}),
    ?assertEqual(1, length(Commits)).

%%% paths

refuses_bad_paths(#{top := Top}) ->
    modify(Top),
    {ok, Outside} = file:get_cwd(),
    Bad = [
        {<<"/etc/passwd">>, absolute},
        {list_to_binary(Outside), absolute},
        {<<"\\windows\\system32">>, absolute},
        {<<"C:\\temp\\x.bt">>, absolute},
        {<<"../outside.bt">>, parent},
        {<<"src/../../outside.bt">>, parent},
        {<<"src\\..\\..\\outside.bt">>, parent},
        {<<"..">>, parent},
        {<<>>, empty},
        {<<"src/a", 0, "b.bt">>, nul},
        {5, type},
        {null, type},
        {[<<"src/counter.bt">>], type}
    ],
    Ops = [
        <<"git-diff">>, <<"git-stage">>, <<"git-unstage">>, <<"git-revert-file">>
    ],
    lists:foreach(
        fun(Op) ->
            lists:foreach(
                fun({Path, Why}) ->
                    Result = request(Op, #{<<"path">> => Path}),
                    ?assertMatch(
                        {error, #beamtalk_error{kind = invalid_argument}},
                        Result,
                        {Op, Path, Why}
                    ),
                    %% Git did not run: the edit is still there and nothing is staged.
                    ?assertEqual(<<"modified\n">>, read(Top), {Op, Path}),
                    ?assertMatch([#{<<"index">> := <<"unmodified">>}], status_files())
                end,
                Bad
            )
        end,
        Ops
    ),
    %% A missing path is the same refusal.
    lists:foreach(
        fun(Op) ->
            ?assertMatch({error, #beamtalk_error{kind = invalid_argument}}, request(Op, #{}))
        end,
        Ops
    ).

paths_are_literal(#{top := Top}) ->
    modify(Top),
    %% `*.bt` is a glob, `:(top)` is pathspec magic: neither is a file here, so
    %% neither may select `src/counter.bt`.
    lists:foreach(
        fun(Path) ->
            {error, _} = request(<<"git-revert-file">>, #{<<"path">> => Path}),
            {error, _} = request(<<"git-stage">>, #{<<"path">> => Path}),
            ?assertEqual(<<"modified\n">>, read(Top), Path),
            ?assertMatch([#{<<"index">> := <<"unmodified">>}], status_files())
        end,
        [<<"src/*.bt">>, <<":(top)src/counter.bt">>, <<":/src/counter.bt">>]
    ).

%%% bounds and encoding

diff_is_bounded(#{top := Top}) ->
    Big = binary:copy(<<"line of changed text\n">>, 100000),
    ok = file:write_file(filename:join(Top, "src/counter.bt"), Big),
    {value, Diff} = request(<<"git-diff">>, #{<<"path">> => <<"src/counter.bt">>}),
    ?assertEqual(true, maps:get(<<"truncated">>, Diff)),
    ?assert(byte_size(maps:get(<<"worktree">>, Diff)) =< 1048576),
    ?assertNotEqual(<<>>, maps:get(<<"worktree">>, Diff)),
    ?assertMatch(Bin when is_binary(Bin), unicode:characters_to_binary(maps:get(<<"worktree">>, Diff))).

diff_is_utf8(#{top := Top}) ->
    %% A Latin-1 byte, then a cut that falls inside a multi-byte character.
    ok = file:write_file(filename:join(Top, "src/counter.bt"), <<"caf", 233, "\n">>),
    {value, Latin} = request(<<"git-diff">>, #{<<"path">> => <<"src/counter.bt">>}),
    ?assert(is_binary(unicode:characters_to_binary(maps:get(<<"worktree">>, Latin)))),
    Wide = binary:copy(<<"é"/utf8>>, 700000),
    ok = file:write_file(filename:join(Top, "src/counter.bt"), Wide),
    {value, Cut} = request(<<"git-diff">>, #{<<"path">> => <<"src/counter.bt">>}),
    ?assertEqual(true, maps:get(<<"truncated">>, Cut)),
    ?assert(is_binary(unicode:characters_to_binary(maps:get(<<"worktree">>, Cut)))).

encodes_as_json(#{top := Top}) ->
    modify(Top),
    Msg = {protocol_msg, <<"git-status">>, <<"m1">>, <<"s1">>, #{}},
    Json = beamtalk_repl_ops_git:handle(<<"git-status">>, #{}, Msg, self()),
    Decoded = json:decode(Json),
    Value = maps:get(<<"value">>, Decoded),
    ?assertMatch([#{<<"path">> := <<"src/counter.bt">>}], maps:get(<<"files">>, Value)),
    ErrMsg = {protocol_msg, <<"git-diff">>, <<"m2">>, <<"s1">>, #{}},
    ErrJson = beamtalk_repl_ops_git:handle(<<"git-diff">>, #{<<"path">> => <<"/etc/hosts">>}, ErrMsg, self()),
    ?assertNotEqual(nomatch, binary:match(ErrJson, <<"project root">>)).

not_a_repo() ->
    Dir = make_dir(),
    try
        beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
        {ok, Meta} = start_meta(Dir),
        try
            ?assertMatch({error, #beamtalk_error{}}, request(<<"git-status">>, #{})),
            ?assertMatch({error, #beamtalk_error{}}, request(<<"git-log">>, #{}))
        after
            beamtalk_test_proc:shutdown(Meta)
        end
    after
        beamtalk_test_fs:rm_rf(Dir)
    end.

%%% Fixtures

make_dir() ->
    Unique = lists:flatten(io_lib:format("gitops_~s", [beamtalk_test_unique:id()])),
    Dir = filename:join(binary_to_list(element(2, beamtalk_file:'tempDirectory'())), Unique),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

make_repo() ->
    Dir = make_dir(),
    git(Dir, ["init", "-q"]),
    git(Dir, ["config", "user.email", "test@example.com"]),
    git(Dir, ["config", "user.name", "Test"]),
    git(Dir, ["config", "commit.gpgsign", "false"]),
    Dir.

%% Test setup only: the code under test runs git through beamtalk_git.
git(Dir, Args) ->
    Cmd = "git -C " ++ quote(Dir) ++ " " ++ string:join([quote(A) || A <- Args], " "),
    _ = os:cmd(Cmd),
    ok.

quote(S) ->
    case os:type() of
        {win32, _} -> "\"" ++ lists:flatten(string:replace(S, "\"", "\\\"", all)) ++ "\"";
        _ -> "'" ++ lists:flatten(string:replace(S, "'", "'\\''", all)) ++ "'"
    end.
