%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_workspace_sup module
%%%
%%% Tests workspace supervisor behavior, child specifications, and startup.

-module(beamtalk_workspace_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Test helper

test_config() ->
    #{
        workspace_id => <<"test123">>,
        project_path => <<"/tmp/test">>,
        tcp_port => 49152,
        auto_cleanup => false  % Disable for testing
    }.

%%% Supervisor flags tests

supervisor_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Should use one_for_one strategy
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

supervisor_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Should allow 5 restarts in 10 seconds
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(10, maps:get(period, SupFlags)).

%%% Child specification tests

children_count_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Should have 6 children: workspace_meta, actor_registry, repl_server, idle_monitor, actor_sup, session_sup
    ?assertEqual(6, length(ChildSpecs)).

children_ids_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Verify all expected children are present
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(beamtalk_workspace_meta, Ids)),
    ?assert(lists:member(beamtalk_actor_registry, Ids)),
    ?assert(lists:member(beamtalk_repl_server, Ids)),
    ?assert(lists:member(beamtalk_idle_monitor, Ids)),
    ?assert(lists:member(beamtalk_actor_sup, Ids)),
    ?assert(lists:member(beamtalk_session_sup, Ids)).

workspace_meta_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Find workspace_meta child spec
    [MetaSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_workspace_meta],
    
    %% Should be a worker, permanent, with correct module
    ?assertEqual(worker, maps:get(type, MetaSpec)),
    ?assertEqual(permanent, maps:get(restart, MetaSpec)),
    
    %% Start function should use beamtalk_workspace_meta:start_link with config map
    {Mod, Fun, [Config]} = maps:get(start, MetaSpec),
    ?assertEqual(beamtalk_workspace_meta, Mod),
    ?assertEqual(start_link, Fun),
    ?assert(maps:is_key(workspace_id, Config)).

beamtalk_repl_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Find actor_registry child spec
    [RegistrySpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_actor_registry],
    ?assertEqual(worker, maps:get(type, RegistrySpec)),
    ?assertEqual(permanent, maps:get(restart, RegistrySpec)),
    ?assertEqual({beamtalk_repl_actors, start_link, [registered]}, 
                 maps:get(start, RegistrySpec)),
    
    %% Find repl_server child spec
    [ReplSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_repl_server],
    ?assertEqual(worker, maps:get(type, ReplSpec)),
    ?assertEqual(permanent, maps:get(restart, ReplSpec)),
    ?assertEqual({beamtalk_repl_server, start_link, [49152]}, 
                 maps:get(start, ReplSpec)).

idle_monitor_spec_test() ->
    Config = test_config(),
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(Config),
    
    %% Find idle_monitor child specs
    MonitorSpecs = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_idle_monitor],
    
    %% With auto_cleanup => false, idle_monitor should still be in child specs
    %% (supervisor filters at runtime, not init time)
    ?assertEqual(1, length(MonitorSpecs)),
    
    [MonitorSpec] = MonitorSpecs,
    ?assertEqual(worker, maps:get(type, MonitorSpec)),
    ?assertEqual(permanent, maps:get(restart, MonitorSpec)).

actor_sup_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Find actor_sup child spec
    [ActorSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_actor_sup],
    
    %% Should be a supervisor, permanent
    ?assertEqual(supervisor, maps:get(type, ActorSupSpec)),
    ?assertEqual(permanent, maps:get(restart, ActorSupSpec)),
    ?assertEqual({beamtalk_actor_sup, start_link, []}, maps:get(start, ActorSupSpec)).

session_sup_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_workspace_sup:init(test_config()),
    
    %% Find session_sup child spec
    [SessionSupSpec] = [S || S <- ChildSpecs, maps:get(id, S) == beamtalk_session_sup],
    
    %% Should be a supervisor, permanent
    ?assertEqual(supervisor, maps:get(type, SessionSupSpec)),
    ?assertEqual(permanent, maps:get(restart, SessionSupSpec)),
    ?assertEqual({beamtalk_session_sup, start_link, []}, maps:get(start, SessionSupSpec)).
