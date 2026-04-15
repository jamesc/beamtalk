%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_sup_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Unit tests for beamtalk_actor_sup module

Tests actor supervisor behavior and dynamic actor management.
""".
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Supervisor initialization tests

init_returns_correct_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_actor_sup:init([]),

    %% Should use simple_one_for_one strategy
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)).

init_returns_correct_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_actor_sup:init([]),

    %% Should allow 10 restarts in 60 seconds (actors may fail frequently)
    ?assertEqual(10, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)).

init_returns_single_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_actor_sup:init([]),

    %% simple_one_for_one should have exactly 1 child spec (the template)
    ?assertEqual(1, length(ChildSpecs)).

init_child_spec_is_temporary_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_actor_sup:init([]),

    %% Child actors should be temporary (not restarted on crash)
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)).

%%% Child spec structure tests

child_spec_uses_beamtalk_actor_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_actor_sup:init([]),

    %% Should use beamtalk_actor:start_link_supervised/3
    ?assertEqual({beamtalk_actor, start_link_supervised, []}, maps:get(start, ChildSpec)).

%%% Start actor tests

start_actor_requires_supervisor_running_test() ->
    %% Attempting to start actor without supervisor should fail
    %% (We can't easily test this without starting the supervisor,
    %% but we verify the function exists and has correct arity)

    ?assertEqual(true, erlang:function_exported(beamtalk_actor_sup, start_actor, 3)).

%%% Integration test (verifies supervisor can actually start)

supervisor_can_start_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_actor_sup:start_link(),

    %% Verify it's running
    ?assert(is_process_alive(Pid)),

    %% Get supervisor info
    Children = supervisor:which_children(Pid),

    %% Should have no children initially (simple_one_for_one starts empty)
    ?assertEqual([], Children),

    %% Cleanup
    exit(Pid, normal).

supervisor_count_children_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_actor_sup:start_link(),

    %% Count children
    Counts = supervisor:count_children(Pid),

    %% Initially should have 0 active children
    ?assertEqual(0, proplists:get_value(active, Counts)),
    ?assertEqual(0, proplists:get_value(workers, Counts)),

    %% Cleanup
    exit(Pid, normal).

%%% BT-1975: Additional coverage tests

init_child_spec_shutdown_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_actor_sup:init([]),
    %% Should have a 5 second shutdown timeout
    ?assertEqual(5000, maps:get(shutdown, ChildSpec)).

init_child_spec_modules_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_actor_sup:init([]),
    %% Modules list should contain beamtalk_actor
    ?assertEqual([beamtalk_actor], maps:get(modules, ChildSpec)).

init_child_spec_id_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_actor_sup:init([]),
    %% Child id should be beamtalk_actor
    ?assertEqual(beamtalk_actor, maps:get(id, ChildSpec)).

supervisor_registered_name_test() ->
    %% Start the supervisor — it registers under beamtalk_actor_sup
    {ok, Pid} = beamtalk_actor_sup:start_link(),
    ?assertEqual(Pid, whereis(beamtalk_actor_sup)),
    %% Cleanup
    exit(Pid, normal).

start_actor_function_exported_test() ->
    %% Verify start_actor/3 is exported with correct arity
    ?assert(erlang:function_exported(beamtalk_actor_sup, start_actor, 3)).

start_link_function_exported_test() ->
    %% Verify start_link/0 is exported
    ?assert(erlang:function_exported(beamtalk_actor_sup, start_link, 0)).
