%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_exec_port_tests).

%%% **DDD Context:** stdlib

-moduledoc """
EUnit tests for beamtalk_exec_port binary-finder helpers (BT-2521).

This file lives in beamtalk_stdlib/test/ so that the coverage recipe
(`rebar3 eunit --dir=apps/beamtalk_stdlib/test`) instruments and records
coverage for beamtalk_exec_port.erl (in beamtalk_stdlib/src/).

The --app discovery gotcha means tests in beamtalk_runtime/test/ do not
contribute to the beamtalk_stdlib coverage badge even though the module
lives there (see docs/development/testing-strategy.md).

Covers: find_exec_binary/0 env-var paths (lines 149, 151-153),
        find_in_project/1 release path (lines 194-197) and error arm (line 201),
        find_project_root/0 happy path and filesystem-root error (line 213).

Port I/O tests (open/spawn_child/write_stdin/kill_child) are integration-
shaped and kept in runtime/apps/beamtalk_runtime/test/beamtalk_exec_port_tests.erl.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Returns the OS temp directory as a list string (cross-platform, no /tmp hardcode).
-spec tmp_dir() -> string().
tmp_dir() ->
    binary_to_list(beamtalk_file:'tempDirectory'()).

%% Save the current value of an env var so it can be restored after a test.
-spec save_env(string()) -> string() | false.
save_env(Var) ->
    os:getenv(Var).

%% Restore an env var to the saved value (false means unset it).
-spec restore_env(string(), string() | false) -> ok.
restore_env(Var, false) ->
    os:unsetenv(Var),
    ok;
restore_env(Var, Val) ->
    os:putenv(Var, Val),
    ok.

%%% ============================================================================
%%% find_project_root/0 — happy path
%%% ============================================================================

find_project_root_finds_cargo_toml_test() ->
    Root = beamtalk_exec_port:find_project_root(),
    ?assert(filelib:is_regular(filename:join(Root, "Cargo.toml"))).

%%% ============================================================================
%%% find_project_root/0 — filesystem-root error (line 213)
%%% ============================================================================

find_project_root_no_cargo_toml_raises_test() ->
    %% Change CWD to a fresh temp directory that has no Cargo.toml ancestor.
    %% Walking up to the filesystem root triggers line 213:
    %%   error({exec_not_found, "could not locate project root..."}).
    {ok, Cwd} = file:get_cwd(),
    Unique = erlang:unique_integer([positive, monotonic]),
    TmpDir = filename:join(tmp_dir(), io_lib:format("bt_no_cargo_~p", [Unique])),
    ok = file:make_dir(TmpDir),
    try
        ok = file:set_cwd(TmpDir),
        ?assertError({exec_not_found, _}, beamtalk_exec_port:find_project_root())
    after
        ok = file:set_cwd(Cwd),
        file:del_dir(TmpDir)
    end.

%%% ============================================================================
%%% find_in_project/1 — nonexistent binary (line 201)
%%% ============================================================================

find_in_project_nonexistent_returns_error_test() ->
    %% A name that will never exist in either debug or release build dirs.
    %% find_in_project/1 finds the project root, tries both paths (false for each),
    %% and returns 'error' from the false->error arm (line 201).
    ?assertEqual(error, beamtalk_exec_port:find_in_project("no-such-binary-bt2521")).

%%% ============================================================================
%%% find_in_project/1 — release-only binary (lines 194–197)
%%% ============================================================================

find_in_project_release_path_test() ->
    %% Build a minimal fake project root in a temp directory:
    %%   {FakeRoot}/Cargo.toml               — marks the project root
    %%   {FakeRoot}/target/release/bt-fake   — the "release build"
    %% No target/debug/bt-fake exists, so find_in_project/1 steps through the
    %% debug check (false) and picks up the release binary (lines 194-197).
    {ok, Cwd} = file:get_cwd(),
    Unique = erlang:unique_integer([positive, monotonic]),
    FakeRoot = filename:join(tmp_dir(), io_lib:format("bt_fake_proj_~p", [Unique])),
    ReleaseDir = filename:join([FakeRoot, "target", "release"]),
    FakeBin = filename:join(ReleaseDir, "bt-fake-exec"),
    ok = filelib:ensure_dir(FakeBin),
    ok = file:write_file(FakeBin, <<>>),
    ok = file:write_file(filename:join(FakeRoot, "Cargo.toml"), <<>>),
    try
        ok = file:set_cwd(FakeRoot),
        {ok, RelPath} = beamtalk_exec_port:find_in_project("bt-fake-exec"),
        ?assert(filelib:is_regular(RelPath))
    after
        ok = file:set_cwd(Cwd),
        file:del_dir_r(FakeRoot)
    end.

%%% ============================================================================
%%% find_exec_binary/0 — BEAMTALK_EXEC_PORT_BIN env-var paths
%%% ============================================================================

find_exec_binary_env_var_empty_string_test() ->
    %% BEAMTALK_EXEC_PORT_BIN="" → empty-string arm (line 149) → falls back to dev binary.
    Prev = save_env("BEAMTALK_EXEC_PORT_BIN"),
    os:putenv("BEAMTALK_EXEC_PORT_BIN", ""),
    try
        Result = beamtalk_exec_port:find_exec_binary(),
        ?assert(filelib:is_regular(Result))
    after
        restore_env("BEAMTALK_EXEC_PORT_BIN", Prev)
    end.

find_exec_binary_env_var_valid_path_test() ->
    %% BEAMTALK_EXEC_PORT_BIN = existing file → returned directly (lines 151-152).
    %% eunit.beam is always present when running EUnit tests.
    RealPath = code:which(eunit),
    Prev = save_env("BEAMTALK_EXEC_PORT_BIN"),
    os:putenv("BEAMTALK_EXEC_PORT_BIN", RealPath),
    try
        ?assertEqual(RealPath, beamtalk_exec_port:find_exec_binary())
    after
        restore_env("BEAMTALK_EXEC_PORT_BIN", Prev)
    end.

find_exec_binary_env_var_invalid_path_test() ->
    %% BEAMTALK_EXEC_PORT_BIN = non-existent path → invalid-path arm (line 153) → falls back.
    Prev = save_env("BEAMTALK_EXEC_PORT_BIN"),
    os:putenv("BEAMTALK_EXEC_PORT_BIN", "/does/not/exist/beamtalk-exec-bt2521"),
    try
        Result = beamtalk_exec_port:find_exec_binary(),
        ?assert(filelib:is_regular(Result))
    after
        restore_env("BEAMTALK_EXEC_PORT_BIN", Prev)
    end.
