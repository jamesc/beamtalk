%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_modules_tests).

-include_lib("eunit/include/eunit.hrl").

%%% Tests for beamtalk_repl_modules module tracker

%% ===================================================================
%% new/0 tests
%% ===================================================================

new_creates_empty_tracker_test() ->
    Tracker = beamtalk_repl_modules:new(),
    ?assertEqual(#{}, Tracker).

%% ===================================================================
%% add_module/3 tests
%% ===================================================================

add_module_with_source_file_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path/to/file.bt", Tracker),
    ?assert(beamtalk_repl_modules:module_exists(my_module, Tracker2)).

add_module_with_undefined_source_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, undefined, Tracker),
    ?assert(beamtalk_repl_modules:module_exists(my_module, Tracker2)).

add_multiple_modules_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(module1, "/path/1.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:add_module(module2, "/path/2.bt", Tracker2),
    Tracker4 = beamtalk_repl_modules:add_module(module3, "/path/3.bt", Tracker3),
    ?assert(beamtalk_repl_modules:module_exists(module1, Tracker4)),
    ?assert(beamtalk_repl_modules:module_exists(module2, Tracker4)),
    ?assert(beamtalk_repl_modules:module_exists(module3, Tracker4)).

add_module_overwrites_existing_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/old/path.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:add_module(my_module, "/new/path.bt", Tracker2),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker3),
    Formatted = beamtalk_repl_modules:format_module_info(Info, 0),
    ?assertEqual("/new/path.bt", maps:get(source_file, Formatted)).

%% ===================================================================
%% remove_module/2 tests
%% ===================================================================

remove_module_existing_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:remove_module(my_module, Tracker2),
    ?assertNot(beamtalk_repl_modules:module_exists(my_module, Tracker3)).

remove_module_nonexistent_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:remove_module(nonexistent, Tracker),
    ?assertEqual(Tracker, Tracker2).

remove_module_from_multiple_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(module1, "/path/1.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:add_module(module2, "/path/2.bt", Tracker2),
    Tracker4 = beamtalk_repl_modules:add_module(module3, "/path/3.bt", Tracker3),
    Tracker5 = beamtalk_repl_modules:remove_module(module2, Tracker4),
    ?assert(beamtalk_repl_modules:module_exists(module1, Tracker5)),
    ?assertNot(beamtalk_repl_modules:module_exists(module2, Tracker5)),
    ?assert(beamtalk_repl_modules:module_exists(module3, Tracker5)).

%% ===================================================================
%% get_module_info/2 tests
%% ===================================================================

get_module_info_existing_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    Result = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    ?assertMatch({ok, _}, Result).

get_module_info_nonexistent_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Result = beamtalk_repl_modules:get_module_info(nonexistent, Tracker),
    ?assertEqual({error, not_found}, Result).

%% ===================================================================
%% list_modules/1 tests
%% ===================================================================

list_modules_empty_test() ->
    Tracker = beamtalk_repl_modules:new(),
    List = beamtalk_repl_modules:list_modules(Tracker),
    ?assertEqual([], List).

list_modules_single_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    List = beamtalk_repl_modules:list_modules(Tracker2),
    ?assertEqual(1, length(List)),
    [{Name, _Info}] = List,
    ?assertEqual(my_module, Name).

list_modules_multiple_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(module1, "/path/1.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:add_module(module2, "/path/2.bt", Tracker2),
    Tracker4 = beamtalk_repl_modules:add_module(module3, "/path/3.bt", Tracker3),
    List = beamtalk_repl_modules:list_modules(Tracker4),
    ?assertEqual(3, length(List)),
    Names = [Name || {Name, _Info} <- List],
    ?assert(lists:member(module1, Names)),
    ?assert(lists:member(module2, Names)),
    ?assert(lists:member(module3, Names)).

%% ===================================================================
%% module_exists/2 tests
%% ===================================================================

module_exists_true_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    ?assert(beamtalk_repl_modules:module_exists(my_module, Tracker2)).

module_exists_false_test() ->
    Tracker = beamtalk_repl_modules:new(),
    ?assertNot(beamtalk_repl_modules:module_exists(nonexistent, Tracker)).

module_exists_after_removal_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    Tracker3 = beamtalk_repl_modules:remove_module(my_module, Tracker2),
    ?assertNot(beamtalk_repl_modules:module_exists(my_module, Tracker3)).

%% ===================================================================
%% get_actor_count/3 tests
%% ===================================================================

get_actor_count_no_registry_pid_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    Count = beamtalk_repl_modules:get_actor_count(my_module, undefined, Tracker2),
    ?assertEqual(0, Count).

get_actor_count_module_not_tracked_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Count = beamtalk_repl_modules:get_actor_count(nonexistent, self(), Tracker),
    ?assertEqual(0, Count).

get_actor_count_with_dead_registry_test() ->
    %% This test verifies the function handles registry errors gracefully
    %% We spawn a process and immediately exit it to have a dead pid
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    DeadPid = spawn(fun() -> ok end),
    %% Ensure process exits
    timer:sleep(10),
    %% Function should handle error gracefully and return 0
    Count = beamtalk_repl_modules:get_actor_count(my_module, DeadPid, Tracker2),
    ?assertEqual(0, Count).

%% ===================================================================
%% format_module_info/2 tests
%% ===================================================================

format_module_info_with_source_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path/to/file.bt", Tracker),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    Formatted = beamtalk_repl_modules:format_module_info(Info, 5),
    ?assertEqual(<<"my_module">>, maps:get(name, Formatted)),
    ?assertEqual("/path/to/file.bt", maps:get(source_file, Formatted)),
    ?assertEqual(5, maps:get(actor_count, Formatted)),
    ?assert(is_integer(maps:get(load_time, Formatted))),
    ?assert(is_list(maps:get(time_ago, Formatted))).

format_module_info_without_source_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, undefined, Tracker),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    Formatted = beamtalk_repl_modules:format_module_info(Info, 0),
    ?assertEqual("unknown", maps:get(source_file, Formatted)).

format_module_info_time_ago_seconds_test() ->
    %% Create a module and immediately format - should be "Ns ago"
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path.bt", Tracker),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    Formatted = beamtalk_repl_modules:format_module_info(Info, 0),
    TimeAgo = maps:get(time_ago, Formatted),
    %% Should be "0s ago" or similar
    ?assert(lists:suffix("s ago", TimeAgo)).

%% ===================================================================
%% get_source_file/1 tests
%% ===================================================================

get_source_file_with_path_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, "/path/to/file.bt", Tracker),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    ?assertEqual("/path/to/file.bt", beamtalk_repl_modules:get_source_file(Info)).

get_source_file_undefined_test() ->
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(my_module, undefined, Tracker),
    {ok, Info} = beamtalk_repl_modules:get_module_info(my_module, Tracker2),
    ?assertEqual(undefined, beamtalk_repl_modules:get_source_file(Info)).

%% ===================================================================
%% get_actor_count/3 with live registry
%% ===================================================================

get_actor_count_with_live_registry_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(test_counter, "/path.bt", Tracker),
    %% No actors registered yet
    Count = beamtalk_repl_modules:get_actor_count(test_counter, RegistryPid, Tracker2),
    ?assertEqual(0, Count),
    gen_server:stop(RegistryPid).

get_actor_count_with_registered_actors_test() ->
    {ok, RegistryPid} = gen_server:start_link(beamtalk_repl_actors, [], []),
    {ok, ActorPid} = test_counter:start_link(0),
    ok = beamtalk_repl_actors:register_actor(RegistryPid, ActorPid, 'Counter', test_counter),
    Tracker = beamtalk_repl_modules:new(),
    Tracker2 = beamtalk_repl_modules:add_module(test_counter, "/path.bt", Tracker),
    Count = beamtalk_repl_modules:get_actor_count(test_counter, RegistryPid, Tracker2),
    ?assertEqual(1, Count),
    gen_server:stop(ActorPid),
    gen_server:stop(RegistryPid).

%% ===================================================================
%% Integration tests
%% ===================================================================

full_lifecycle_test() ->
    %% Create tracker
    Tracker = beamtalk_repl_modules:new(),
    ?assertEqual([], beamtalk_repl_modules:list_modules(Tracker)),

    %% Add first module
    Tracker2 = beamtalk_repl_modules:add_module(counter, "/lib/Counter.bt", Tracker),
    ?assert(beamtalk_repl_modules:module_exists(counter, Tracker2)),

    %% Add second module
    Tracker3 = beamtalk_repl_modules:add_module(worker, "/lib/Worker.bt", Tracker2),
    ?assertEqual(2, length(beamtalk_repl_modules:list_modules(Tracker3))),

    %% Get info and format
    {ok, Info} = beamtalk_repl_modules:get_module_info(counter, Tracker3),
    Formatted = beamtalk_repl_modules:format_module_info(Info, 3),
    ?assertEqual(<<"counter">>, maps:get(name, Formatted)),
    ?assertEqual(3, maps:get(actor_count, Formatted)),

    %% Remove first module
    Tracker4 = beamtalk_repl_modules:remove_module(counter, Tracker3),
    ?assertNot(beamtalk_repl_modules:module_exists(counter, Tracker4)),
    ?assert(beamtalk_repl_modules:module_exists(worker, Tracker4)),

    %% Remove second module
    Tracker5 = beamtalk_repl_modules:remove_module(worker, Tracker4),
    ?assertEqual([], beamtalk_repl_modules:list_modules(Tracker5)).

%% ===================================================================
%% format_time_ago/1 tests (BT-627)
%% ===================================================================

format_time_ago_seconds_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(30)),
    ?assert(lists:suffix("s ago", Result)).

format_time_ago_minutes_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(120)),
    ?assert(lists:suffix("m ago", Result)).

format_time_ago_hours_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(7200)),
    ?assert(lists:suffix("h ago", Result)).

format_time_ago_days_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(172800)),
    ?assert(lists:suffix("d ago", Result)).

format_time_ago_zero_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(0)),
    ?assertEqual("0s ago", Result).

format_time_ago_boundary_59_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(59)),
    ?assert(lists:suffix("s ago", Result)).

format_time_ago_boundary_60_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(60)),
    ?assert(lists:suffix("m ago", Result)).

format_time_ago_boundary_3599_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(3599)),
    ?assert(lists:suffix("m ago", Result)).

format_time_ago_boundary_3600_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(3600)),
    ?assert(lists:suffix("h ago", Result)).

format_time_ago_boundary_86399_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(86399)),
    ?assert(lists:suffix("h ago", Result)).

format_time_ago_boundary_86400_test() ->
    Result = lists:flatten(beamtalk_repl_modules:format_time_ago(86400)),
    ?assert(lists:suffix("d ago", Result)).
