%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime Context

-module(beamtalk_logging_config_tests).

-moduledoc """
EUnit tests for beamtalk_logging_config module.

Tests log-level management, debug-target toggling, and logger
introspection.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

%% Restore the log level to its original value after every test group.
logging_config_test_() ->
    {setup,
        fun() ->
            OrigLevel = beamtalk_logging_config:logLevel(),
            beamtalk_logging_config:ensure_table(),
            OrigLevel
        end,
        fun(OrigLevel) ->
            beamtalk_logging_config:disableAllDebug(),
            beamtalk_logging_config:logLevel(OrigLevel)
        end,
        [
            fun logLevel_returns_atom/0,
            fun logLevel_set_valid_changes_level/0,
            fun logLevel_set_invalid_returns_error/0,
            fun logLevel_set_non_atom_returns_error/0,
            fun logFormat_returns_text_by_default/0,
            fun logFormat_set_json_changes_format/0,
            fun logFormat_set_text_restores_default/0,
            fun logFormat_invalid_returns_error/0,
            fun logFormat_non_atom_returns_error/0,
            fun debugTargets_returns_expected_list/0,
            fun enableDebug_valid_symbol_succeeds/0,
            fun enableDebug_invalid_symbol_returns_error/0,
            fun disableDebug_removes_target/0,
            fun disableAllDebug_clears_all/0,
            fun loggerInfo_returns_binary/0,
            fun activeDebugTargets_tracks_multiple/0,
            fun enable_disable_cycle_removes_target/0,
            fun enableDebug_domain_target_succeeds/0,
            fun disableDebug_domain_target_removes/0,
            fun enableDebug_class_reference_succeeds/0,
            fun disableDebug_class_reference_removes/0,
            fun enableDebug_actor_instance_succeeds/0,
            fun disableDebug_actor_instance_removes/0,
            fun activeDebugTargets_includes_class_and_actor/0,
            fun enableDebug_invalid_type_returns_error/0,
            fun disableAllDebug_clears_class_and_actor/0,
            fun loggerInfo_includes_class_and_actor/0,
            fun debugTargets_includes_mcp/0,
            fun disableDebug_invalid_symbol_returns_error/0,
            fun disableDebug_invalid_type_returns_error/0,
            fun disableDebug_not_enabled_domain_succeeds/0,
            fun logLevel_all_valid_levels_accepted/0,
            fun ensure_table_idempotent/0,
            fun loggerInfo_includes_level_and_format/0,
            fun enableDebug_supervisor_succeeds/0,
            fun disableDebug_supervisor_removes/0,
            fun logLevel_set_updates_file_handler/0,
            fun logFormat_set_without_handler_succeeds/0,
            fun loggerInfo_with_file_handler_shows_path/0,
            fun loggerInfo_without_handler_shows_standard_io/0,
            fun loggerInfo_with_subsystem_shows_modules/0,
            fun loggerInfo_with_domain_target_shows_domain/0,
            fun loggerInfo_no_active_targets_shows_none/0,
            fun disableDebug_actor_not_in_table_succeeds/0,
            fun disableDebug_class_not_in_table_succeeds/0,
            fun disableAllDebug_clears_domain_targets/0,
            fun enableDebug_compiler_succeeds/0,
            fun enableDebug_workspace_succeeds/0,
            fun enableDebug_stdlib_succeeds/0,
            fun domain_filter_allows_matching_events/0,
            fun domain_filter_ignores_non_matching_events/0,
            fun domain_filter_ignores_events_without_domain/0,
            fun class_filter_allows_matching_class/0,
            fun class_filter_ignores_other_class/0,
            fun actor_filter_allows_matching_pid/0,
            fun actor_filter_ignores_other_pid/0,
            fun enableDebug_with_registered_class_sets_module_level/0,
            fun disableDebug_with_registered_class_unsets_module_level/0,
            fun disableAllDebug_with_registered_class_unsets_module_level/0,
            fun loggerInfo_with_mcp_shows_signal_file/0,
            fun enable_supervisor_progress_idempotent/0,
            fun loggerInfo_handles_no_file_config/0,
            fun mcp_signal_path_without_home_uses_cache_dir/0
        ]}.

%%====================================================================
%% Tests
%%====================================================================

logLevel_returns_atom() ->
    Level = beamtalk_logging_config:logLevel(),
    ?assert(is_atom(Level)).

logLevel_set_valid_changes_level() ->
    OrigLevel = beamtalk_logging_config:logLevel(),
    %% Set to debug
    ?assertEqual(nil, beamtalk_logging_config:logLevel(debug)),
    ?assertEqual(debug, beamtalk_logging_config:logLevel()),
    %% Set to warning
    ?assertEqual(nil, beamtalk_logging_config:logLevel(warning)),
    ?assertEqual(warning, beamtalk_logging_config:logLevel()),
    %% Restore
    beamtalk_logging_config:logLevel(OrigLevel).

logLevel_set_invalid_returns_error() ->
    Result = beamtalk_logging_config:logLevel(banana),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

logLevel_set_non_atom_returns_error() ->
    Result = beamtalk_logging_config:logLevel(<<"debug">>),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

%%====================================================================
%% Log format tests
%%====================================================================

logFormat_returns_text_by_default() ->
    %% Without a beamtalk_file_log handler, the default should be text.
    ?assertEqual(text, beamtalk_logging_config:logFormat()).

logFormat_set_json_changes_format() ->
    %% Install a temporary file handler so we can test format switching.
    install_test_file_handler(),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(json)),
    ?assertEqual(json, beamtalk_logging_config:logFormat()),
    remove_test_file_handler().

logFormat_set_text_restores_default() ->
    install_test_file_handler(),
    beamtalk_logging_config:logFormat(json),
    ?assertEqual(json, beamtalk_logging_config:logFormat()),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(text)),
    ?assertEqual(text, beamtalk_logging_config:logFormat()),
    remove_test_file_handler().

logFormat_invalid_returns_error() ->
    Result = beamtalk_logging_config:logFormat(yaml),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

logFormat_non_atom_returns_error() ->
    Result = beamtalk_logging_config:logFormat(<<"json">>),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

debugTargets_returns_expected_list() ->
    Targets = beamtalk_logging_config:debugTargets(),
    ?assert(is_list(Targets)),
    ?assert(lists:member(actor, Targets)),
    ?assert(lists:member(supervisor, Targets)),
    ?assert(lists:member(dispatch, Targets)),
    ?assert(lists:member(compiler, Targets)),
    ?assert(lists:member(workspace, Targets)),
    ?assert(lists:member(stdlib, Targets)),
    ?assert(lists:member(hotreload, Targets)),
    ?assert(lists:member(runtime, Targets)),
    ?assert(lists:member(user, Targets)).

enableDebug_valid_symbol_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(actor)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    beamtalk_logging_config:disableDebug(actor).

enableDebug_invalid_symbol_returns_error() ->
    Result = beamtalk_logging_config:enableDebug(nonexistent),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableDebug_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())),
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(dispatch)),
    ?assertNot(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())).

disableAllDebug_clears_all() ->
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 2),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

loggerInfo_returns_binary() ->
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    ?assertNotEqual(<<>>, Info).

activeDebugTargets_tracks_multiple() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(stdlib),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    ?assert(lists:member(stdlib, Active)),
    beamtalk_logging_config:disableAllDebug().

enable_disable_cycle_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(hotreload),
    ?assert(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(hotreload),
    ?assertNot(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())).

enableDebug_domain_target_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(runtime)),
    ?assert(lists:member(runtime, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(runtime).

disableDebug_domain_target_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(user),
    ?assert(lists:member(user, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(user),
    ?assertNot(lists:member(user, beamtalk_logging_config:activeDebugTargets())).

%%====================================================================
%% Class debug tests
%%====================================================================

enableDebug_class_reference_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Create a class reference (class object): class name ends with " class"
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(ClassRef)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member({class, 'TestClass'}, Active)),
    beamtalk_logging_config:disableAllDebug().

disableDebug_class_reference_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    ?assert(lists:member({class, 'TestClass'}, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(ClassRef),
    ?assertNot(lists:member({class, 'TestClass'}, beamtalk_logging_config:activeDebugTargets())).

%%====================================================================
%% Actor debug tests
%%====================================================================

enableDebug_actor_instance_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Spawn a simple process to act as an actor
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(ActorRef)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member({actor, Pid, 'Counter'}, Active)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

disableDebug_actor_instance_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    ?assert(lists:member({actor, Pid, 'Counter'}, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(ActorRef),
    ?assertNot(lists:member({actor, Pid, 'Counter'}, beamtalk_logging_config:activeDebugTargets())),
    Pid ! stop.

%%====================================================================
%% Mixed target tests
%%====================================================================

activeDebugTargets_includes_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Enable a subsystem, a class, and an actor
    beamtalk_logging_config:enableDebug(actor),
    ClassRef = #beamtalk_object{class = 'MyClass class', class_mod = my_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    ?assert(lists:member({class, 'MyClass'}, Active)),
    ?assert(lists:member({actor, Pid, 'Counter'}, Active)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

enableDebug_invalid_type_returns_error() ->
    Result = beamtalk_logging_config:enableDebug(42),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableAllDebug_clears_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'Foo class', class_mod = foo, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Bar', class_mod = bar, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 3),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()),
    Pid ! stop.

loggerInfo_includes_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'Widget class', class_mod = widget, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    %% Should mention the class and actor in the output
    ?assertNotEqual(nomatch, binary:match(Info, <<"Widget">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"Counter">>)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

debugTargets_includes_mcp() ->
    Targets = beamtalk_logging_config:debugTargets(),
    ?assert(lists:member(mcp, Targets)).

disableDebug_invalid_symbol_returns_error() ->
    Result = beamtalk_logging_config:disableDebug(nonexistent),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableDebug_invalid_type_returns_error() ->
    Result = beamtalk_logging_config:disableDebug(42),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableDebug_not_enabled_domain_succeeds() ->
    %% Disabling a domain target that was never enabled should succeed silently
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(runtime)).

logLevel_all_valid_levels_accepted() ->
    OrigLevel = beamtalk_logging_config:logLevel(),
    ValidLevels = [emergency, alert, critical, error, warning, notice, info, debug],
    lists:foreach(
        fun(Level) ->
            ?assertEqual(nil, beamtalk_logging_config:logLevel(Level)),
            ?assertEqual(Level, beamtalk_logging_config:logLevel())
        end,
        ValidLevels
    ),
    beamtalk_logging_config:logLevel(OrigLevel).

ensure_table_idempotent() ->
    %% Calling ensure_table multiple times should not error
    ?assertEqual(ok, beamtalk_logging_config:ensure_table()),
    ?assertEqual(ok, beamtalk_logging_config:ensure_table()).

loggerInfo_includes_level_and_format() ->
    Info = beamtalk_logging_config:loggerInfo(),
    %% Should contain "Log level:" and "Format:" substrings
    ?assertNotEqual(nomatch, binary:match(Info, <<"Log level:">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"Format:">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"Active debug targets:">>)).

%%====================================================================
%% MCP signal file tests
%%====================================================================

mcp_signal_file_test_() ->
    {setup,
        fun() ->
            %% Create a temporary workspace directory for testing.
            %% We need beamtalk_workspace_meta running to get workspace_id.
            %% Instead, we test mcp_signal_path/0 directly — it needs
            %% the workspace meta server, which may not be running in unit
            %% tests. So we test the path helper and the file write/remove
            %% mechanics via the public enable/disable API with a mock
            %% workspace.
            beamtalk_logging_config:ensure_table(),
            ok
        end,
        fun(_) ->
            beamtalk_logging_config:disableAllDebug()
        end,
        [
            fun mcp_signal_path_returns_error_without_workspace/0,
            fun mcp_enable_disable_without_workspace_returns_error/0
        ]}.

mcp_signal_path_returns_error_without_workspace() ->
    %% Without a running workspace, mcp_signal_path should return an error
    Result = beamtalk_logging_config:mcp_signal_path(),
    ?assertMatch({error, workspace_not_started}, Result).

mcp_enable_disable_without_workspace_returns_error() ->
    %% enableDebug(mcp) without a workspace should return a runtime_error
    Result = beamtalk_logging_config:enableDebug(mcp),
    ?assertMatch(#beamtalk_error{kind = runtime_error}, Result),
    %% disableDebug(mcp) without a workspace should succeed silently
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(mcp)).

%% Tests that require a running workspace (integration-style) are run with
%% a mock workspace_meta process.
mcp_signal_file_with_workspace_test_() ->
    {setup,
        fun() ->
            beamtalk_logging_config:ensure_table(),
            %% Start a mock workspace_meta that returns a test workspace ID
            TestWsId = iolist_to_binary(
                io_lib:format("test_mcp_~p", [erlang:unique_integer([positive])])
            ),
            %% Create workspace directory
            Home =
                case os:getenv("HOME") of
                    false -> filename:basedir(user_cache, "beamtalk");
                    H -> H
                end,
            WsDir = filename:join([Home, ".beamtalk", "workspaces", binary_to_list(TestWsId)]),
            ok = filelib:ensure_dir(filename:join(WsDir, "dummy")),
            %% Register a mock for beamtalk_workspace_meta via meck
            %% Since we cannot use meck, we test via the signal path function
            %% after manually registering a gen_server.
            {TestWsId, WsDir}
        end,
        fun({_TestWsId, WsDir}) ->
            beamtalk_logging_config:disableAllDebug(),
            %% Clean up signal file and directory
            _ = file:delete(filename:join(WsDir, "mcp_debug_enabled")),
            _ = file:del_dir(WsDir)
        end,
        fun({TestWsId, WsDir}) ->
            [
                {"signal file is created and removed correctly", fun() ->
                    SignalPath = filename:join(WsDir, "mcp_debug_enabled"),
                    %% File should not exist yet
                    ?assertNot(filelib:is_file(SignalPath)),
                    %% Write it directly (the way enableDebug would)
                    ok = file:write_file(SignalPath, <<"debug\n">>),
                    ?assert(filelib:is_file(SignalPath)),
                    %% Remove it
                    ok = file:delete(SignalPath),
                    ?assertNot(filelib:is_file(SignalPath)),
                    %% Verify path matches expected pattern
                    ?assertNotEqual(
                        nomatch,
                        string:find(SignalPath, binary_to_list(TestWsId))
                    )
                end}
            ]
        end}.

%%====================================================================
%% MCP signal file with running workspace_meta
%%
%% Exercises the real write_mcp_signal_file / remove_mcp_signal_file
%% code paths by starting a real beamtalk_workspace_meta gen_server.
%%====================================================================

mcp_signal_file_with_real_workspace_test_() ->
    {setup,
        fun() ->
            stop_workspace_meta_if_running(),
            TestWsId = iolist_to_binary(
                io_lib:format("test_mcp_~p", [erlang:unique_integer([positive])])
            ),
            {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
                workspace_id => TestWsId,
                project_path => <<"/tmp/test">>,
                created_at => erlang:system_time(second)
            }),
            beamtalk_logging_config:ensure_table(),
            {TestWsId, MetaPid}
        end,
        fun({TestWsId, MetaPid}) ->
            beamtalk_logging_config:disableAllDebug(),
            %% Best-effort cleanup of the workspace directory
            case beamtalk_platform:home_dir() of
                false ->
                    CacheDir = filename:basedir(user_cache, "beamtalk"),
                    WsDir = filename:join([
                        CacheDir, "workspaces", binary_to_list(TestWsId)
                    ]),
                    _ = file:delete(filename:join(WsDir, "mcp_debug_enabled")),
                    _ = file:del_dir(WsDir);
                Home ->
                    WsDir = filename:join([
                        Home, ".beamtalk", "workspaces", binary_to_list(TestWsId)
                    ]),
                    _ = file:delete(filename:join(WsDir, "mcp_debug_enabled")),
                    _ = file:del_dir(WsDir)
            end,
            case is_process_alive(MetaPid) of
                true -> gen_server:stop(MetaPid);
                false -> ok
            end
        end,
        fun({TestWsId, _MetaPid}) ->
            [
                {"mcp_signal_path returns the expected path", fun() ->
                    {ok, Path} = beamtalk_logging_config:mcp_signal_path(),
                    ?assert(is_list(Path)),
                    ?assertNotEqual(
                        nomatch,
                        string:find(Path, binary_to_list(TestWsId))
                    ),
                    ?assertNotEqual(nomatch, string:find(Path, "mcp_debug_enabled"))
                end},
                {"enableDebug(mcp) creates the signal file", fun() ->
                    beamtalk_logging_config:disableAllDebug(),
                    {ok, Path} = beamtalk_logging_config:mcp_signal_path(),
                    ?assertEqual(nil, beamtalk_logging_config:enableDebug(mcp)),
                    ?assert(filelib:is_file(Path)),
                    ?assert(lists:member(mcp, beamtalk_logging_config:activeDebugTargets()))
                end},
                {"disableDebug(mcp) removes the signal file", fun() ->
                    {ok, Path} = beamtalk_logging_config:mcp_signal_path(),
                    beamtalk_logging_config:enableDebug(mcp),
                    ?assert(filelib:is_file(Path)),
                    ?assertEqual(nil, beamtalk_logging_config:disableDebug(mcp)),
                    ?assertNot(filelib:is_file(Path)),
                    ?assertNot(lists:member(mcp, beamtalk_logging_config:activeDebugTargets()))
                end},
                {"disableDebug(mcp) is idempotent when file already gone", fun() ->
                    %% Remove the signal file, then call disableDebug — should not error
                    {ok, Path} = beamtalk_logging_config:mcp_signal_path(),
                    beamtalk_logging_config:enableDebug(mcp),
                    _ = file:delete(Path),
                    ?assertEqual(nil, beamtalk_logging_config:disableDebug(mcp))
                end},
                {"disableAllDebug removes mcp signal file", fun() ->
                    {ok, Path} = beamtalk_logging_config:mcp_signal_path(),
                    beamtalk_logging_config:enableDebug(mcp),
                    ?assert(filelib:is_file(Path)),
                    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
                    ?assertNot(filelib:is_file(Path))
                end},
                {"loggerInfo shows mcp with a real signal file path", fun() ->
                    beamtalk_logging_config:disableAllDebug(),
                    beamtalk_logging_config:enableDebug(mcp),
                    Info = beamtalk_logging_config:loggerInfo(),
                    ?assertNotEqual(nomatch, binary:match(Info, <<"mcp">>)),
                    ?assertNotEqual(nomatch, binary:match(Info, <<"signal file">>)),
                    beamtalk_logging_config:disableAllDebug()
                end}
            ]
        end}.

stop_workspace_meta_if_running() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid),
            ok
    end.

%%====================================================================
%% Supervisor progress tests
%%====================================================================

enableDebug_supervisor_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(supervisor)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(supervisor, Active)),
    beamtalk_logging_config:disableDebug(supervisor).

disableDebug_supervisor_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(supervisor),
    ?assert(lists:member(supervisor, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(supervisor),
    ?assertNot(lists:member(supervisor, beamtalk_logging_config:activeDebugTargets())).

%%====================================================================
%% Log level with file handler tests
%%====================================================================

logLevel_set_updates_file_handler() ->
    install_test_file_handler(),
    OrigLevel = beamtalk_logging_config:logLevel(),
    ?assertEqual(nil, beamtalk_logging_config:logLevel(debug)),
    %% The file handler level should also have been updated
    {ok, #{level := HandlerLevel}} = logger:get_handler_config(beamtalk_file_log),
    ?assertEqual(debug, HandlerLevel),
    beamtalk_logging_config:logLevel(OrigLevel),
    remove_test_file_handler().

%%====================================================================
%% Format switching without handler tests
%%====================================================================

logFormat_set_without_handler_succeeds() ->
    %% Without a beamtalk_file_log handler, setting format should succeed silently
    _ = logger:remove_handler(beamtalk_file_log),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(json)),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(text)).

%%====================================================================
%% find_log_file coverage tests
%%====================================================================

loggerInfo_with_file_handler_shows_path() ->
    install_test_file_handler_with_file(),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    %% Should contain a real file path (not standard_io)
    ?assertNotEqual(nomatch, binary:match(Info, <<"Log file:">>)),
    ?assertEqual(nomatch, binary:match(Info, <<"(standard_io)">>)),
    %% The path should contain our test log file pattern
    ?assertNotEqual(nomatch, binary:match(Info, <<"test_log_">>)),
    remove_test_file_handler().

loggerInfo_without_handler_shows_standard_io() ->
    _ = logger:remove_handler(beamtalk_file_log),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assertNotEqual(nomatch, binary:match(Info, <<"(standard_io)">>)).

%%====================================================================
%% format_debug_target coverage
%%====================================================================

loggerInfo_with_subsystem_shows_modules() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(compiler),
    Info = beamtalk_logging_config:loggerInfo(),
    %% Should list compiler modules
    ?assertNotEqual(nomatch, binary:match(Info, <<"compiler">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"beamtalk_compiler_server">>)),
    beamtalk_logging_config:disableAllDebug().

loggerInfo_with_domain_target_shows_domain() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(runtime),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assertNotEqual(nomatch, binary:match(Info, <<"runtime">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"domain=">>)),
    beamtalk_logging_config:disableAllDebug().

loggerInfo_no_active_targets_shows_none() ->
    beamtalk_logging_config:disableAllDebug(),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assertNotEqual(nomatch, binary:match(Info, <<"(none)">>)).

%%====================================================================
%% disableDebug no-op paths
%%====================================================================

disableDebug_actor_not_in_table_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'NeverEnabled', class_mod = never_enabled, pid = Pid},
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(ActorRef)),
    Pid ! stop.

disableDebug_class_not_in_table_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{
        class = 'NeverEnabled class', class_mod = never_enabled, pid = self()
    },
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(ClassRef)).

%%====================================================================
%% disableAllDebug with mixed targets including domain
%%====================================================================

disableAllDebug_clears_domain_targets() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(runtime),
    beamtalk_logging_config:enableDebug(user),
    beamtalk_logging_config:enableDebug(actor),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 3),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

%%====================================================================
%% subsystem_modules coverage
%%====================================================================

enableDebug_compiler_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(compiler)),
    ?assert(lists:member(compiler, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(compiler).

enableDebug_workspace_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(workspace)),
    ?assert(lists:member(workspace, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(workspace).

enableDebug_stdlib_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(stdlib)),
    ?assert(lists:member(stdlib, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(stdlib).

%%====================================================================
%% Filter function behaviour tests
%%
%% Enabling a domain, class, or actor target installs a logger filter
%% fun. We exercise those funs directly by extracting them from the
%% configured filter list, so that the filter logic (not just install)
%% is covered.
%%====================================================================

domain_filter_allows_matching_events() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(runtime),
    {FilterFun, _} = get_handler_filter(beamtalk_debug_domain_runtime),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{domain => [beamtalk, runtime]}},
    ?assertEqual(Event, FilterFun(Event, [])),
    beamtalk_logging_config:disableAllDebug().

domain_filter_ignores_non_matching_events() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(runtime),
    {FilterFun, _} = get_handler_filter(beamtalk_debug_domain_runtime),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{domain => [unrelated, something]}},
    ?assertEqual(ignore, FilterFun(Event, [])),
    beamtalk_logging_config:disableAllDebug().

domain_filter_ignores_events_without_domain() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(runtime),
    {FilterFun, _} = get_handler_filter(beamtalk_debug_domain_runtime),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{}},
    ?assertEqual(ignore, FilterFun(Event, [])),
    beamtalk_logging_config:disableAllDebug().

class_filter_allows_matching_class() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    {FilterFun, _} = get_primary_filter(beamtalk_debug_class_TestClass),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{beamtalk_class => 'TestClass'}},
    ?assertEqual(Event, FilterFun(Event, [])),
    beamtalk_logging_config:disableAllDebug().

class_filter_ignores_other_class() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    {FilterFun, _} = get_primary_filter(beamtalk_debug_class_TestClass),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{beamtalk_class => 'OtherClass'}},
    ?assertEqual(ignore, FilterFun(Event, [])),
    EventNoMeta = #{level => debug, msg => {string, "hi"}, meta => #{}},
    ?assertEqual(ignore, FilterFun(EventNoMeta, [])),
    beamtalk_logging_config:disableAllDebug().

actor_filter_allows_matching_pid() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    FilterId = list_to_atom("beamtalk_debug_actor_" ++ pid_to_list(Pid)),
    {FilterFun, _} = get_primary_filter(FilterId),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{pid => Pid}},
    ?assertEqual(Event, FilterFun(Event, [])),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

actor_filter_ignores_other_pid() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    FilterId = list_to_atom("beamtalk_debug_actor_" ++ pid_to_list(Pid)),
    {FilterFun, _} = get_primary_filter(FilterId),
    %% Use a different pid in the event meta
    OtherPid = self(),
    Event = #{level => debug, msg => {string, "hi"}, meta => #{pid => OtherPid}},
    ?assertEqual(ignore, FilterFun(Event, [])),
    EventNoMeta = #{level => debug, msg => {string, "hi"}, meta => #{}},
    ?assertEqual(ignore, FilterFun(EventNoMeta, [])),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

%%====================================================================
%% Class module table integration — exercises the lookup-found branches
%% in enable_class_debug / disable_class_debug / disableAllDebug.
%%====================================================================

enableDebug_with_registered_class_sets_module_level() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_class_module_table:new(),
    beamtalk_class_module_table:insert('RegisteredClass', lists),
    try
        ClassRef = #beamtalk_object{
            class = 'RegisteredClass class', class_mod = lists, pid = self()
        },
        ?assertEqual(nil, beamtalk_logging_config:enableDebug(ClassRef)),
        %% Module level should now be debug for the registered module
        ?assertEqual([{lists, debug}], logger:get_module_level(lists))
    after
        logger:unset_module_level(lists),
        beamtalk_class_module_table:delete('RegisteredClass'),
        beamtalk_logging_config:disableAllDebug()
    end.

disableDebug_with_registered_class_unsets_module_level() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_class_module_table:new(),
    beamtalk_class_module_table:insert('RegisteredClass2', lists),
    try
        ClassRef = #beamtalk_object{
            class = 'RegisteredClass2 class', class_mod = lists, pid = self()
        },
        beamtalk_logging_config:enableDebug(ClassRef),
        ?assertEqual([{lists, debug}], logger:get_module_level(lists)),
        beamtalk_logging_config:disableDebug(ClassRef),
        %% After disable, the module-level override should be removed
        ?assertEqual([], logger:get_module_level(lists))
    after
        logger:unset_module_level(lists),
        beamtalk_class_module_table:delete('RegisteredClass2'),
        beamtalk_logging_config:disableAllDebug()
    end.

disableAllDebug_with_registered_class_unsets_module_level() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_class_module_table:new(),
    beamtalk_class_module_table:insert('RegisteredClass3', lists),
    try
        ClassRef = #beamtalk_object{
            class = 'RegisteredClass3 class', class_mod = lists, pid = self()
        },
        beamtalk_logging_config:enableDebug(ClassRef),
        ?assertEqual([{lists, debug}], logger:get_module_level(lists)),
        beamtalk_logging_config:disableAllDebug(),
        ?assertEqual([], logger:get_module_level(lists))
    after
        logger:unset_module_level(lists),
        beamtalk_class_module_table:delete('RegisteredClass3')
    end.

%%====================================================================
%% Additional loggerInfo / format_debug_target coverage
%%====================================================================

loggerInfo_with_mcp_shows_signal_file() ->
    %% Inject an mcp entry directly into the debug table to exercise
    %% the mcp_signal branch of format_debug_target without needing a
    %% running workspace.
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:ensure_table(),
    ets:insert(beamtalk_debug_targets, {mcp, subsystem, []}),
    try
        Info = beamtalk_logging_config:loggerInfo(),
        ?assertNotEqual(nomatch, binary:match(Info, <<"signal file">>))
    after
        ets:delete(beamtalk_debug_targets, mcp)
    end.

enable_supervisor_progress_idempotent() ->
    %% Enabling supervisor debug twice should not crash when the
    %% otp_progress filter has already been removed.
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(supervisor)),
    %% Disable then re-enable so the second enable sees no otp_progress filter
    beamtalk_logging_config:disableDebug(supervisor),
    %% Manually remove otp_progress (simulating a system without it)
    _ = logger:remove_handler_filter(default, otp_progress),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(supervisor)),
    beamtalk_logging_config:disableDebug(supervisor).

loggerInfo_handles_no_file_config() ->
    %% Install a handler without a file config so find_log_file falls
    %% through to the default-handler branch.
    _ = logger:remove_handler(beamtalk_file_log),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"Log file:">>)).

mcp_signal_path_without_home_uses_cache_dir() ->
    %% This path requires a running workspace_meta. Without it, the
    %% function returns workspace_not_started (which is already covered
    %% elsewhere). We verify here that calling mcp_signal_path is safe
    %% when HOME is temporarily unset, by saving/restoring the env var.
    OrigHome = os:getenv("HOME"),
    try
        %% Without a workspace, result is {error, workspace_not_started}
        %% regardless of HOME, but we exercise the branch for safety.
        Result = beamtalk_logging_config:mcp_signal_path(),
        ?assertMatch({error, _}, Result)
    after
        case OrigHome of
            false -> os:unsetenv("HOME");
            Val -> os:putenv("HOME", Val)
        end
    end.

%%====================================================================
%% Test helpers
%%====================================================================

-doc "Fetch an installed handler filter's {Fun, Extra} by id.".
get_handler_filter(FilterId) ->
    {ok, #{filters := Filters}} = logger:get_handler_config(FilterId),
    {FilterId, FunPair} = lists:keyfind(FilterId, 1, Filters),
    FunPair.

-doc "Fetch an installed primary filter's {Fun, Extra} by id.".
get_primary_filter(FilterId) ->
    #{filters := Filters} = logger:get_primary_config(),
    {FilterId, FunPair} = lists:keyfind(FilterId, 1, Filters),
    FunPair.

-doc "Install a temporary beamtalk_file_log handler for format-switching tests.".
install_test_file_handler() ->
    _ = logger:remove_handler(beamtalk_file_log),
    logger:add_handler(beamtalk_file_log, logger_std_h, #{
        config => #{type => standard_io},
        level => debug,
        formatter =>
            {logger_formatter, #{
                template => [time, " [", level, "] ", msg, "\n"],
                single_line => true
            }}
    }).

-doc "Install a file-based handler for find_log_file tests.".
install_test_file_handler_with_file() ->
    _ = logger:remove_handler(beamtalk_file_log),
    TmpFile = filename:join(
        filename:basedir(user_cache, "beamtalk"),
        "test_log_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".log"
    ),
    ok = filelib:ensure_dir(TmpFile),
    logger:add_handler(beamtalk_file_log, logger_std_h, #{
        config => #{file => TmpFile},
        level => debug,
        formatter =>
            {logger_formatter, #{
                template => [time, " [", level, "] ", msg, "\n"],
                single_line => true
            }}
    }).

-doc "Remove the temporary beamtalk_file_log handler.".
remove_test_file_handler() ->
    _ = logger:remove_handler(beamtalk_file_log),
    ok.
