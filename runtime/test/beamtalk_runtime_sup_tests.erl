%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_runtime_sup module
%%%
%%% Tests supervisor behavior, child specifications, and restart strategies.

-module(beamtalk_runtime_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Supervisor flags tests

supervisor_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% Should use one_for_one strategy
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

supervisor_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% Should allow 5 restarts in 10 seconds
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(10, maps:get(period, SupFlags)).

%%% Child specification tests

children_count_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% Should have exactly 2 children
    ?assertEqual(2, length(ChildSpecs)).

children_ids_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% Children should be beamtalk_classes and beamtalk_instances
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(beamtalk_classes, Ids)),
    ?assert(lists:member(beamtalk_instances, Ids)).

children_are_workers_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% All children should be workers
    Types = [maps:get(type, Spec) || Spec <- ChildSpecs],
    ?assertEqual([worker, worker], Types).

children_are_permanent_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% All children should have permanent restart
    RestartTypes = [maps:get(restart, Spec) || Spec <- ChildSpecs],
    ?assertEqual([permanent, permanent], RestartTypes).

%%% Child ordering tests

classes_before_instances_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    %% Children start in order, classes should be first
    [Child1, Child2] = ChildSpecs,
    ?assertEqual(beamtalk_classes, maps:get(id, Child1)),
    ?assertEqual(beamtalk_instances, maps:get(id, Child2)).

%%% Child specifications tests

classes_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    [ClassesSpec | _] = ChildSpecs,
    ?assertEqual(beamtalk_classes, maps:get(id, ClassesSpec)),
    ?assertEqual({beamtalk_classes, start_link, []}, maps:get(start, ClassesSpec)),
    ?assertEqual(permanent, maps:get(restart, ClassesSpec)),
    ?assertEqual(5000, maps:get(shutdown, ClassesSpec)),
    ?assertEqual(worker, maps:get(type, ClassesSpec)),
    ?assertEqual([beamtalk_classes], maps:get(modules, ClassesSpec)).

instances_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),
    
    [_, InstancesSpec] = ChildSpecs,
    ?assertEqual(beamtalk_instances, maps:get(id, InstancesSpec)),
    ?assertEqual({beamtalk_instances, start_link, []}, maps:get(start, InstancesSpec)),
    ?assertEqual(permanent, maps:get(restart, InstancesSpec)),
    ?assertEqual(5000, maps:get(shutdown, InstancesSpec)),
    ?assertEqual(worker, maps:get(type, InstancesSpec)),
    ?assertEqual([beamtalk_instances], maps:get(modules, InstancesSpec)).

%%% init/1 tests

init_returns_proper_format_test() ->
    Result = beamtalk_runtime_sup:init([]),
    ?assertMatch({ok, {#{strategy := one_for_one}, [_, _]}}, Result).

%%% Behavioral tests

behaviour_implementation_test() ->
    Behaviors = proplists:get_value(behaviour, beamtalk_runtime_sup:module_info(attributes), []),
    ?assert(lists:member(supervisor, Behaviors)).

exports_required_callbacks_test() ->
    Exports = beamtalk_runtime_sup:module_info(exports),
    ?assert(lists:member({start_link, 0}, Exports)),
    ?assert(lists:member({init, 1}, Exports)).
