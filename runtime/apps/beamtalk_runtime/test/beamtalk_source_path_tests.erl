%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_source_path_tests).

-include_lib("eunit/include/eunit.hrl").

%% BT-4940: embedded paths are package-relative; resolve/2 maps them back to
%% a file on disk.

absolute_path_is_returned_unchanged_test() ->
    ?assertEqual("/a/b/c.bt", beamtalk_source_path:resolve(undefined, "/a/b/c.bt")).

resolves_against_cwd_package_root_test() ->
    Root = filename:absname("bt4940_resolve_cwd"),
    ok = filelib:ensure_path(filename:join(Root, "src")),
    ok = file:write_file(filename:join(Root, "beamtalk.toml"), <<"[package]\n">>),
    File = filename:join([Root, "src", "foo.bt"]),
    ok = file:write_file(File, <<>>),
    {ok, Old} = file:get_cwd(),
    try
        ok = file:set_cwd(filename:join(Root, "src")),
        ?assertEqual(File, beamtalk_source_path:resolve(undefined, "src/foo.bt"))
    after
        file:set_cwd(Old),
        file:del_dir_r(Root)
    end.

resolves_against_workspace_root_test() ->
    Root = filename:absname("bt4940_resolve_ws"),
    ok = filelib:ensure_path(filename:join(Root, "src")),
    File = filename:join([Root, "src", "bar.bt"]),
    ok = file:write_file(File, <<>>),
    application:set_env(beamtalk_runtime, workspace_project_root, list_to_binary(Root)),
    try
        ?assertEqual(File, beamtalk_source_path:resolve(undefined, "src/bar.bt"))
    after
        application:unset_env(beamtalk_runtime, workspace_project_root),
        file:del_dir_r(Root)
    end.

unresolvable_path_is_returned_unchanged_test() ->
    ?assertEqual("nope/zzz.bt", beamtalk_source_path:resolve(undefined, "nope/zzz.bt")).

%% A beam in a directory with a `.beamtalk-source-root` marker (a path
%% dependency under the consumer's `_build/deps/`) resolves against the root
%% the marker names, never the consumer's, even when the consumer has a file
%% at the same relative path.
marker_root_wins_over_enclosing_package_test() ->
    Base = filename:absname("bt4940_marker"),
    App = filename:join(Base, "app"),
    Dep = filename:join(Base, "dep"),
    Ebin = filename:join([App, "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = filelib:ensure_path(filename:join(App, "src")),
    ok = filelib:ensure_path(filename:join(Dep, "src")),
    ok = file:write_file(filename:join(App, "beamtalk.toml"), <<"[package]\n">>),
    ok = file:write_file(filename:join(App, "src/x.bt"), <<>>),
    DepFile = filename:join([Dep, "src", "x.bt"]),
    ok = file:write_file(DepFile, <<>>),
    ok = file:write_file(filename:join(Ebin, ".beamtalk-source-root"), list_to_binary(Dep)),
    Mod = bt4940_marker_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(Ebin, "bt4940_marker_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(DepFile, beamtalk_source_path:resolve(Mod, "src/x.bt"))
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% With a module root known, a missing file is reported under that root, not
%% quietly found under another project's root.
missing_file_is_not_taken_from_another_root_test() ->
    Base = filename:absname("bt4940_noleak"),
    Ws = filename:join(Base, "ws"),
    Own = filename:join(Base, "own"),
    ok = filelib:ensure_path(filename:join(Ws, "src")),
    ok = filelib:ensure_path(filename:join(Own, "ebin")),
    ok = file:write_file(filename:join(Ws, "src/y.bt"), <<>>),
    ok = file:write_file(filename:join(Own, "beamtalk.toml"), <<"[package]\n">>),
    Mod = bt4940_noleak_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join([Own, "ebin", "bt4940_noleak_mod.beam"]),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    application:set_env(beamtalk_runtime, workspace_project_root, list_to_binary(Ws)),
    try
        ?assertEqual(
            filename:join(Own, "src/y.bt"), beamtalk_source_path:resolve(Mod, "src/y.bt")
        )
    after
        application:unset_env(beamtalk_runtime, workspace_project_root),
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% A module with no root of its own and a path relative to the current
%% directory (a REPL `:load tests/x.bt` outside any package) is answered as
%% embedded,
%% even when the workspace has a project root that lacks the file.
cwd_relative_path_resolves_without_a_package_test() ->
    Base = filename:absname("bt4940_cwdrel"),
    Ws = filename:join(Base, "ws"),
    Cwd = filename:join(Base, "cwd"),
    ok = filelib:ensure_path(Ws),
    ok = filelib:ensure_path(filename:join(Cwd, "fixtures")),
    File = filename:join([Cwd, "fixtures", "z.bt"]),
    ok = file:write_file(File, <<>>),
    {ok, Old} = file:get_cwd(),
    application:set_env(beamtalk_runtime, workspace_project_root, list_to_binary(Ws)),
    try
        ok = file:set_cwd(Cwd),
        %% Answered as embedded (relative), as before BT-4940.
        ?assertEqual("fixtures/z.bt", beamtalk_source_path:resolve(undefined, "fixtures/z.bt")),
        ?assert(filelib:is_file(File))
    after
        file:set_cwd(Old),
        application:unset_env(beamtalk_runtime, workspace_project_root),
        file:del_dir_r(Base)
    end.

%% In a workspace, an in-memory module's file deleted since load is still
%% reported under the workspace project, not as a bare relative path.
missing_file_in_workspace_resolves_under_the_project_test() ->
    application:set_env(beamtalk_runtime, workspace_project_root, <<"/ws/proj">>),
    try
        ?assertEqual(
            "/ws/proj/src/gone.bt", beamtalk_source_path:resolve(undefined, "src/gone.bt")
        )
    after
        application:unset_env(beamtalk_runtime, workspace_project_root)
    end.
