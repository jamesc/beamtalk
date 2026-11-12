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

-export([with_tmp_home/1, tmp_dir/1, rm_rf/1]).

-doc """
Create (and return) a fresh unique directory under the platform temp dir.
`Label` is folded into the name for debuggability.
""".
-spec tmp_dir(string()) -> file:filename().
tmp_dir(Label) ->
    Base = binary_to_list(element(2, beamtalk_file:'tempDirectory'())),
    Dir = filename:join(Base, "bt_" ++ Label ++ "_" ++ beamtalk_test_unique:id()),
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
