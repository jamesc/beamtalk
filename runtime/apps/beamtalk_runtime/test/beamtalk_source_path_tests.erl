%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_source_path_tests).

-include_lib("eunit/include/eunit.hrl").

%% The lenient `resolve/2` is gone (BT-4966): tests go through the strict form.
resolve(Module, Path) ->
    {ok, Resolved} = beamtalk_source_path:resolve_strict(Module, Path),
    Resolved.

%% BT-4966: no production caller wanted the lenient result, so the export is
%% gone and a future caller cannot open a cwd-relative path by mistake.
lenient_resolve_is_not_exported_test() ->
    ?assertNot(erlang:function_exported(beamtalk_source_path, resolve, 2)).

%% BT-4940: embedded paths are package-relative; resolve/2 maps them back to
%% a file on disk.

absolute_path_is_returned_unchanged_test() ->
    ?assertEqual("/a/b/c.bt", resolve(undefined, "/a/b/c.bt")).

resolves_against_cwd_package_root_test() ->
    Root = filename:absname("bt4940_resolve_cwd"),
    ok = filelib:ensure_path(filename:join(Root, "src")),
    ok = file:write_file(filename:join(Root, "beamtalk.toml"), <<"[package]\n">>),
    File = filename:join([Root, "src", "foo.bt"]),
    ok = file:write_file(File, <<>>),
    {ok, Old} = file:get_cwd(),
    try
        ok = file:set_cwd(filename:join(Root, "src")),
        ?assertEqual(File, resolve(undefined, "src/foo.bt"))
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
        ?assertEqual(File, resolve(undefined, "src/bar.bt"))
    after
        application:unset_env(beamtalk_runtime, workspace_project_root),
        file:del_dir_r(Root)
    end.

unresolvable_path_is_returned_unchanged_test() ->
    ?assertEqual("nope/zzz.bt", resolve(undefined, "nope/zzz.bt")).

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
        ?assertEqual(DepFile, resolve(Mod, "src/x.bt"))
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
            filename:join(Own, "src/y.bt"), resolve(Mod, "src/y.bt")
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
        ?assertEqual("fixtures/z.bt", resolve(undefined, "fixtures/z.bt")),
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
            "/ws/proj/src/gone.bt", resolve(undefined, "src/gone.bt")
        )
    after
        application:unset_env(beamtalk_runtime, workspace_project_root)
    end.

%% A marker that is not valid UTF-8 leaves the path unresolved: the package
%% walk must NOT run, because for a path dependency it finds the consumer's
%% root (and a same-named consumer file would be reloaded in its place).
non_utf8_marker_never_resolves_against_the_consumer_test() ->
    Base = filename:absname("bt4940_badmarker"),
    Ebin = filename:join([Base, "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = filelib:ensure_path(filename:join(Base, "src")),
    ok = file:write_file(filename:join(Base, "beamtalk.toml"), <<"[package]\n">>),
    ok = file:write_file(filename:join(Base, "src/a.bt"), <<>>),
    ok = file:write_file(filename:join(Ebin, ".beamtalk-source-root"), <<255, 254, 253>>),
    Mod = bt4940_badmarker_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(Ebin, "bt4940_badmarker_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(unresolvable, beamtalk_source_path:resolve_strict(Mod, "src/a.bt"))
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% BT-4957: the marker records the root relative to the ebin dir, so a
%% consumer + dependency tree moved together (`mv ws ws2`, a CI cache restore)
%% still resolves to the dependency's file without a rebuild.
relative_marker_survives_a_moved_tree_test() ->
    Base = filename:absname("bt4957_moved_a"),
    Moved = filename:absname("bt4957_moved_b"),
    Ebin = filename:join([Base, "app", "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = filelib:ensure_path(filename:join([Base, "app", "src"])),
    ok = filelib:ensure_path(filename:join([Base, "dep", "src"])),
    ok = file:write_file(filename:join([Base, "app", "beamtalk.toml"]), <<"[package]\n">>),
    ok = file:write_file(filename:join([Base, "app", "src", "x.bt"]), <<>>),
    ok = file:write_file(filename:join([Base, "dep", "beamtalk.toml"]), <<"[package]\n">>),
    ok = file:write_file(filename:join([Base, "dep", "src", "x.bt"]), <<>>),
    %% Line 1: relative to the ebin dir; line 2: absolute (the old location).
    ok = file:write_file(
        filename:join(Ebin, ".beamtalk-source-root"),
        iolist_to_binary(["../../../../../dep\n", filename:join(Base, "dep")])
    ),
    ok = file:rename(Base, Moved),
    MovedEbin = filename:join([Moved, "app", "_build", "deps", "dep", "ebin"]),
    Mod = bt4957_moved_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(MovedEbin, "bt4957_moved_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(
            filename:join([Moved, "dep", "src", "x.bt"]),
            resolve(Mod, "src/x.bt")
        )
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base),
        file:del_dir_r(Moved)
    end.

%% A dependency outside the moved tree (or a relative line that holds no
%% package): the recorded absolute root (line 2) is used.
absolute_marker_line_covers_a_dependency_outside_the_moved_tree_test() ->
    Base = filename:absname("bt4957_outside"),
    Dep = filename:join(Base, "dep"),
    Ebin = filename:join([Base, "app", "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = filelib:ensure_path(filename:join(Dep, "src")),
    ok = file:write_file(filename:join(Dep, "beamtalk.toml"), <<"[package]\n">>),
    ok = file:write_file(filename:join([Dep, "src", "x.bt"]), <<>>),
    %% Relative line points at a directory with no package.
    ok = file:write_file(
        filename:join(Ebin, ".beamtalk-source-root"),
        iolist_to_binary(["../../../../nowhere\n", Dep])
    ),
    Mod = bt4957_outside_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(Ebin, "bt4957_outside_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(
            filename:join([Dep, "src", "x.bt"]), resolve(Mod, "src/x.bt")
        )
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% BT-4957: an unreadable marker is `unresolvable` for callers that open the
%% file, and `source_file_from_module/1` is nil (so `reload` raises
%% `no_source_file`), never a relative path.
non_utf8_marker_is_unresolvable_and_source_file_is_nil_test() ->
    Base = filename:absname("bt4957_badmarker"),
    Ebin = filename:join([Base, "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = file:write_file(filename:join(Base, "beamtalk.toml"), <<"[package]\n">>),
    ok = file:write_file(filename:join(Ebin, ".beamtalk-source-root"), <<255, 254, 253>>),
    Mod = bt4957_badmarker_mod,
    {ok, Mod, Bin} = compile:forms([
        {attribute, 1, module, Mod},
        {attribute, 2, export, []},
        {attribute, 3, beamtalk_source, ["src/a.bt"]}
    ]),
    BeamFile = filename:join(Ebin, "bt4957_badmarker_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(unresolvable, beamtalk_source_path:resolve_strict(Mod, "src/a.bt")),
        ?assertEqual(nil, beamtalk_reflection:source_file_from_module(Mod))
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% Only the consumer moved next to a different package at the same relative
%% spot: the still-valid absolute root wins over the relative line.
absolute_root_wins_over_a_lookalike_relative_package_test() ->
    Base = filename:absname("bt4957_lookalike"),
    Dep = filename:join(Base, "realdep"),
    Other = filename:join(Base, "dep"),
    Ebin = filename:join([Base, "app", "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    [
        begin
            ok = filelib:ensure_path(filename:join(D, "src")),
            ok = file:write_file(filename:join(D, "beamtalk.toml"), <<"[package]\n">>),
            ok = file:write_file(filename:join([D, "src", "x.bt"]), <<>>)
        end
     || D <- [Dep, Other]
    ],
    ok = file:write_file(
        filename:join(Ebin, ".beamtalk-source-root"),
        iolist_to_binary(["../../../../../dep\n", Dep])
    ),
    Mod = bt4957_lookalike_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(Ebin, "bt4957_lookalike_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(
            filename:join([Dep, "src", "x.bt"]), resolve(Mod, "src/x.bt")
        )
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% A stack frame of a module whose marker is unreadable has no snippet source,
%% even when the consumer has a file at the same relative path.
unreadable_marker_frame_has_no_source_path_test() ->
    Base = filename:absname("bt4957_frame"),
    Ebin = filename:join([Base, "_build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(Ebin),
    ok = filelib:ensure_path(filename:join(Base, "src")),
    ok = file:write_file(filename:join(Base, "beamtalk.toml"), <<"[package]\n">>),
    ok = file:write_file(filename:join([Base, "src", "a.bt"]), <<>>),
    ok = file:write_file(filename:join(Ebin, ".beamtalk-source-root"), <<255, 254, 253>>),
    Mod = bt4957_frame_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join(Ebin, "bt4957_frame_mod.beam"),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(
            nil, beamtalk_stack_frame:frame_source_path(#{module => Mod, file => <<"src/a.bt">>})
        )
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% BT-4966: the Rust writer computes line 1 between canonical paths, so the
%% runtime must apply it to the canonical ebin dir. `_build` is a symlink to a
%% cache of a different depth (a CI cache mount); applied lexically, line 1
%% climbs the wrong number of levels and misses `dep`.
symlinked_build_dir_resolves_the_relative_line_from_the_real_path_test() ->
    Base = filename:absname("bt4966_symlink"),
    Dep = filename:join(Base, "dep"),
    RealEbin = filename:join([Base, "cache", "x", "build", "deps", "dep", "ebin"]),
    ok = filelib:ensure_path(RealEbin),
    [
        begin
            ok = filelib:ensure_path(filename:join(D, "src")),
            ok = file:write_file(filename:join(D, "beamtalk.toml"), <<"[package]\n">>),
            ok = file:write_file(filename:join([D, "src", "x.bt"]), <<>>)
        end
     || D <- [Dep]
    ],
    ok = filelib:ensure_path(filename:join(Base, "app")),
    ok = file:make_symlink(
        filename:join([Base, "cache", "x", "build"]), filename:join([Base, "app", "_build"])
    ),
    %% Line 1 from the canonical ebin dir; line 2 (old location) is gone.
    ok = file:write_file(
        filename:join(RealEbin, ".beamtalk-source-root"),
        iolist_to_binary(["../../../../../../dep\n", filename:join(Base, "gone")])
    ),
    Mod = bt4966_symlink_mod,
    {ok, Mod, Bin} = compile:forms([{attribute, 1, module, Mod}, {attribute, 2, export, []}]),
    BeamFile = filename:join([
        Base, "app", "_build", "deps", "dep", "ebin", "bt4966_symlink_mod.beam"
    ]),
    ok = file:write_file(BeamFile, Bin),
    {module, Mod} = code:load_abs(filename:rootname(BeamFile)),
    try
        ?assertEqual(filename:join([Dep, "src", "x.bt"]), resolve(Mod, "src/x.bt"))
    after
        code:purge(Mod),
        code:delete(Mod),
        file:del_dir_r(Base)
    end.

%% BT-4966: `..` never climbs above the root, on any platform. The root is the
%% first element of `filename:split/1` (`/` or a drive like `C:/`), not a
%% literal "/".
normalize_clamps_excess_parent_segments_at_the_root_test() ->
    [Root | _] = filename:split(filename:absname("")),
    Deep = filename:join([Root, "a", "b", "..", "..", "..", "..", "c"]),
    ?assertEqual(filename:join(Root, "c"), beamtalk_source_path:normalize(Deep)),
    ?assertEqual(
        filename:join([Root, "a", "c"]),
        beamtalk_source_path:normalize(filename:join([Root, "a", ".", "b", "..", "c"]))
    ).
