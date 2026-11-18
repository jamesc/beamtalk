%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_fs).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit filesystem helpers (BT-4677): unique temp directories,
recursive delete, and a `HOME` override that is always restored.

Temp directories are rooted at `beamtalk_file:'tempDirectory'/0` (never a
hardcoded `/tmp`) and named with `beamtalk_test_unique:id/0`, so separate
`rebar3 eunit` invocations cannot collide on a path.
""".

-include_lib("kernel/include/file.hrl").

-export([
    with_tmp_home/1,
    tmp_dir/1,
    rm_rf/1,
    base_dir/0,
    restore_home/1,
    changelog_workspace/0,
    release_changelog_workspace/1,
    start_changelog_workspace/0,
    stop_changelog_workspace/1
]).

-doc """
Create (and return) a fresh unique directory under the platform temp dir.
`Label` is folded into the name for debuggability.
""".
-spec tmp_dir(string()) -> file:filename().
tmp_dir(Label) ->
    Dir = filename:join(base_dir(), "bt_" ++ Label ++ "_" ++ beamtalk_test_unique:id()),
    ok = filelib:ensure_path(Dir),
    Dir.

-doc """
Run `Fun` (arity 0, or arity 1 receiving the directory) with `HOME` pointing
at a fresh temp directory; restore the previous `HOME` (or unset it) and
delete the directory afterwards, whatever `Fun` does.
""".
-spec with_tmp_home(fun(() -> T) | fun((file:filename()) -> T)) -> T when T :: term().
with_tmp_home(Fun) ->
    Dir = tmp_dir("home"),
    Old = os:getenv("HOME"),
    os:putenv("HOME", Dir),
    try
        case erlang:fun_info(Fun, arity) of
            {arity, 0} -> Fun();
            {arity, 1} -> Fun(Dir)
        end
    after
        restore_home(Old),
        rm_rf(Dir)
    end.

-doc """
Set up an isolated ChangeLog workspace for a `setup`/`cleanup` pair: a
unique workspace id plus a unique temp `HOME`, so the changelog resolves its
`changes/` directory under our own tree. Returns `{WorkspaceId, Home,
OldHome}`; hand it to `release_changelog_workspace/1` to restore `HOME` and
delete the tree.

The id and directory use `beamtalk_test_unique:id/0`, never a bare
`erlang:unique_integer/1`: that counter resets on every fresh `rebar3 eunit`
invocation, and `beamtalk_workspace_changelog`'s own `load_from_disk` would
otherwise restore a prior run's leftover `changes.jsonl` entries into this
run's ETS table.
""".
-spec changelog_workspace() -> {binary(), file:filename(), string() | false}.
changelog_workspace() ->
    Unique = beamtalk_test_unique:id(),
    WorkspaceId = list_to_binary("test-ws-" ++ Unique),
    Home = tmp_dir("changelog"),
    OldHome = os:getenv("HOME"),
    true = os:putenv("HOME", Home),
    {WorkspaceId, Home, OldHome}.

-doc "Undo `changelog_workspace/0`: restore `HOME` and delete the temp tree.".
-spec release_changelog_workspace({binary(), file:filename(), string() | false}) -> ok.
release_changelog_workspace({_WorkspaceId, Home, OldHome}) ->
    restore_home(OldHome),
    rm_rf(Home).

-doc """
`changelog_workspace/0` plus a freshly started `beamtalk_workspace_changelog`
(any running one is stopped first) on its workspace id — the `setup` half of
a fixture whose tests append real, persisted ChangeLog entries. Returns the
handle for `stop_changelog_workspace/1`.
""".
-spec start_changelog_workspace() -> {binary(), file:filename(), string() | false}.
start_changelog_workspace() ->
    ok = beamtalk_test_proc:stop_registered(beamtalk_workspace_changelog),
    {WorkspaceId, _Home, _OldHome} = Handle = changelog_workspace(),
    {ok, _} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    Handle.

-doc "Undo `start_changelog_workspace/0`: stop the changelog, restore `HOME`, delete the tree.".
-spec stop_changelog_workspace({binary(), file:filename(), string() | false}) -> ok.
stop_changelog_workspace(Handle) ->
    ok = beamtalk_test_proc:stop_registered(beamtalk_workspace_changelog),
    release_changelog_workspace(Handle).

-doc "Cross-platform absolute temp directory (never a hardcoded `/tmp`).".
-spec base_dir() -> file:filename().
base_dir() ->
    unicode:characters_to_list(element(2, beamtalk_file:'tempDirectory'())).

-doc """
Restore `HOME` from a previous `os:getenv("HOME")` result: unset when it was
`false`, otherwise put the old value back. For fixtures whose setup returns
the old value to a separate teardown; prefer `with_tmp_home/1` otherwise.
""".
-spec restore_home(string() | false) -> true | ok.
restore_home(false) -> os:unsetenv("HOME");
restore_home(Old) -> os:putenv("HOME", Old).

-doc "Recursively delete a file or directory tree. Missing paths are ignored.".
-spec rm_rf(file:filename_all()) -> ok.
rm_rf(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} ->
            {ok, Entries} = file:list_dir(Path),
            [rm_rf(filename:join(Path, E)) || E <- Entries],
            _ = file:del_dir(Path),
            ok;
        {ok, _} ->
            _ = file:delete(Path),
            ok;
        {error, _} ->
            ok
    end.
