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
