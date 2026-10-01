%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Workspace Context

-module(beamtalk_workspace_dir_sites_tests).

-moduledoc """
BT-3680: every site that derives a path under `<home>/.beamtalk/workspaces/<id>`
must resolve through `beamtalk_platform:workspace_dir/1`. These tests pin that
the sites agree under a fake HOME, under a BEAMTALK_HOME override, and under no
home at all (where they all skip persistence — no user-cache fallback).
""".

-include_lib("eunit/include/eunit.hrl").

-define(WS, <<"dir-sites-ws">>).

with_env(Env, Fun) ->
    %% BEAMTALK_NO_FILE_LOG is set by `just test-runtime`; unset so the file-logger site runs.
    Vars = ["BEAMTALK_HOME", "HOME", "USERPROFILE", "BEAMTALK_NO_FILE_LOG"],
    Orig = [{V, os:getenv(V)} || V <- Vars],
    lists:foreach(fun(V) -> os:unsetenv(V) end, Vars),
    maps:foreach(fun(K, V) -> os:putenv(K, V) end, Env),
    try
        Fun()
    after
        lists:foreach(
            fun
                ({V, false}) -> os:unsetenv(V);
                ({V, Val}) -> os:putenv(V, Val)
            end,
            Orig
        )
    end.

tmp_home() ->
    application:ensure_all_started(beamtalk_stdlib),
    Dir = filename:join(
        binary_to_list(beamtalk_file:'tempDirectory'()),
        "bt3680_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    Dir.

stop_meta() ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%% Exercise every Erlang site once and return the resolved paths. The file
%% logger site mutates VM-global logger state (primary level and the
%% `beamtalk_file_log` handler), so snapshot it, start from no handler (a
%% leftover one would make setup_file_logger/1 reuse it and write no file), and
%% restore both afterwards.
site_paths() ->
    PrimaryConfig = logger:get_primary_config(),
    OldHandler =
        case logger:get_handler_config(beamtalk_file_log) of
            {ok, Cfg} -> Cfg;
            {error, _} -> undefined
        end,
    _ = logger:remove_handler(beamtalk_file_log),
    try
        site_paths_inner()
    after
        _ = logger:remove_handler(beamtalk_file_log),
        case OldHandler of
            undefined ->
                ok;
            #{module := Mod} ->
                _ = logger:add_handler(
                    beamtalk_file_log, Mod, maps:remove(id, maps:remove(module, OldHandler))
                )
        end,
        _ = logger:set_primary_config(maps:get(level, PrimaryConfig))
    end.

site_paths_inner() ->
    ok = beamtalk_repl_server:write_port_file(?WS, 4242, <<"noncehex01234567">>),
    ok = beamtalk_workspace_sup:setup_file_logger(?WS),
    stop_meta(),
    {ok, Meta} = beamtalk_workspace_meta:start_link(#{
        workspace_id => ?WS,
        project_path => <<"/bt_test/ws">>,
        created_at => erlang:system_time(second)
    }),
    Signal = beamtalk_logging_config:mcp_signal_path(),
    gen_server:stop(Meta),
    #{
        changes => beamtalk_workspace_changelog:changes_dir(?WS),
        signal => Signal
    }.

assert_files_under(WsDir) ->
    ?assert(filelib:is_regular(filename:join(WsDir, "port"))),
    ?assert(filelib:is_regular(filename:join(WsDir, "workspace.log"))),
    ?assert(filelib:is_regular(filename:join(WsDir, "metadata.json"))).

sites_agree_under_fake_home_test() ->
    Home = tmp_home(),
    try
        with_env(#{"HOME" => Home}, fun() ->
            WsDir = filename:join([Home, ".beamtalk", "workspaces", "dir-sites-ws"]),
            ?assertEqual(WsDir, beamtalk_platform:workspace_dir(?WS)),
            #{changes := Changes, signal := Signal} = site_paths(),
            ?assertEqual(filename:join(WsDir, "changes"), Changes),
            ?assertEqual({ok, filename:join(WsDir, "mcp_debug_enabled")}, Signal),
            assert_files_under(WsDir)
        end)
    after
        _ = file:del_dir_r(Home)
    end.

sites_honour_beamtalk_home_override_test() ->
    Home = tmp_home(),
    try
        with_env(#{"BEAMTALK_HOME" => Home, "HOME" => "/nonexistent-bt3680"}, fun() ->
            WsDir = filename:join([Home, "workspaces", "dir-sites-ws"]),
            #{changes := Changes, signal := Signal} = site_paths(),
            ?assertEqual(filename:join(WsDir, "changes"), Changes),
            ?assertEqual({ok, filename:join(WsDir, "mcp_debug_enabled")}, Signal),
            assert_files_under(WsDir)
        end)
    after
        _ = file:del_dir_r(Home)
    end.

sites_skip_without_home_test() ->
    %% Computed before HOME is unset: user_cache derives from HOME on Linux.
    Cache = filename:join(filename:basedir(user_cache, "beamtalk"), "workspaces"),
    with_env(#{}, fun() ->
        #{changes := Changes, signal := Signal} = site_paths(),
        ?assertEqual(undefined, Changes),
        ?assertEqual({error, no_home_dir}, Signal),
        %% No user-cache fallback dir may have been created for the workspace.
        ?assertNot(filelib:is_dir(filename:join(Cache, "dir-sites-ws")))
    end).
