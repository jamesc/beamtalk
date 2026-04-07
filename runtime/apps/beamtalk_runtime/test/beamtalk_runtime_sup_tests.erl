%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_runtime_sup_tests).

-moduledoc """
Unit tests for beamtalk_runtime_sup module

Tests supervisor behavior, child specifications, and restart strategies.
""".
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

    %% Should have exactly 6 children: bootstrap, stdlib, object_instances, subprocess_sup, reactive_subprocess_sup, trace_store
    ?assertEqual(6, length(ChildSpecs)).

children_ids_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    %% Children should be bootstrap, stdlib, object_instances, subprocess_sup, and reactive_subprocess_sup
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(beamtalk_bootstrap, Ids)),
    ?assert(lists:member(beamtalk_stdlib, Ids)),
    ?assert(lists:member(beamtalk_object_instances, Ids)),
    ?assert(lists:member(beamtalk_subprocess_sup, Ids)),
    ?assert(lists:member(beamtalk_reactive_subprocess_sup, Ids)),
    ?assert(lists:member(beamtalk_trace_store, Ids)),
    ?assertNot(lists:member(beamtalk_classes, Ids)).

children_are_workers_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    %% First three children are workers; subprocess_sup and reactive_subprocess_sup are supervisors; trace_store is worker
    Types = [maps:get(type, Spec) || Spec <- ChildSpecs],
    ?assertEqual([worker, worker, worker, supervisor, supervisor, worker], Types).

children_are_permanent_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    %% All children should have permanent restart
    RestartTypes = [maps:get(restart, Spec) || Spec <- ChildSpecs],
    ?assertEqual([permanent, permanent, permanent, permanent, permanent, permanent], RestartTypes).

%%% Child ordering tests
%%% Order matters: bootstrap must start before stdlib (which registers classes),
%%% and stdlib must complete before instances tracking starts.

children_ordered_correctly_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    %% Verify ordering: bootstrap -> stdlib -> instances -> subprocess_sup -> reactive_subprocess_sup -> trace_store
    Ids = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assertEqual(
        [
            beamtalk_bootstrap,
            beamtalk_stdlib,
            beamtalk_object_instances,
            beamtalk_subprocess_sup,
            beamtalk_reactive_subprocess_sup,
            beamtalk_trace_store
        ],
        Ids
    ).

%%% Child specifications tests

bootstrap_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    [
        BootstrapSpec,
        _StdlibSpec,
        _InstancesSpec,
        _SubprocessSupSpec,
        _ReactiveSupSpec,
        _TraceStoreSpec
    ] = ChildSpecs,
    ?assertEqual(beamtalk_bootstrap, maps:get(id, BootstrapSpec)),
    ?assertEqual({beamtalk_bootstrap, start_link, []}, maps:get(start, BootstrapSpec)),
    ?assertEqual(permanent, maps:get(restart, BootstrapSpec)),
    ?assertEqual(5000, maps:get(shutdown, BootstrapSpec)),
    ?assertEqual(worker, maps:get(type, BootstrapSpec)),
    ?assertEqual([beamtalk_bootstrap], maps:get(modules, BootstrapSpec)).

instances_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    [
        _BootstrapSpec,
        _StdlibSpec,
        InstancesSpec,
        _SubprocessSupSpec,
        _ReactiveSupSpec,
        _TraceStoreSpec
    ] = ChildSpecs,
    ?assertEqual(beamtalk_object_instances, maps:get(id, InstancesSpec)),
    ?assertEqual({beamtalk_object_instances, start_link, []}, maps:get(start, InstancesSpec)),
    ?assertEqual(permanent, maps:get(restart, InstancesSpec)),
    ?assertEqual(5000, maps:get(shutdown, InstancesSpec)),
    ?assertEqual(worker, maps:get(type, InstancesSpec)),
    ?assertEqual([beamtalk_object_instances], maps:get(modules, InstancesSpec)).

stdlib_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    [
        _BootstrapSpec,
        StdlibSpec,
        _InstancesSpec,
        _SubprocessSupSpec,
        _ReactiveSupSpec,
        _TraceStoreSpec
    ] = ChildSpecs,
    ?assertEqual(beamtalk_stdlib, maps:get(id, StdlibSpec)),
    ?assertEqual({beamtalk_stdlib, start_link, []}, maps:get(start, StdlibSpec)),
    ?assertEqual(permanent, maps:get(restart, StdlibSpec)),
    ?assertEqual(5000, maps:get(shutdown, StdlibSpec)),
    ?assertEqual(worker, maps:get(type, StdlibSpec)),
    ?assertEqual([beamtalk_stdlib], maps:get(modules, StdlibSpec)).

subprocess_sup_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    [
        _BootstrapSpec,
        _StdlibSpec,
        _InstancesSpec,
        SubprocessSupSpec,
        _ReactiveSupSpec,
        _TraceStoreSpec
    ] = ChildSpecs,
    ?assertEqual(beamtalk_subprocess_sup, maps:get(id, SubprocessSupSpec)),
    ?assertEqual({beamtalk_subprocess_sup, start_link, []}, maps:get(start, SubprocessSupSpec)),
    ?assertEqual(permanent, maps:get(restart, SubprocessSupSpec)),
    ?assertEqual(5000, maps:get(shutdown, SubprocessSupSpec)),
    ?assertEqual(supervisor, maps:get(type, SubprocessSupSpec)),
    ?assertEqual([beamtalk_subprocess_sup], maps:get(modules, SubprocessSupSpec)).

reactive_subprocess_sup_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_runtime_sup:init([]),

    [
        _BootstrapSpec,
        _StdlibSpec,
        _InstancesSpec,
        _SubprocessSupSpec,
        ReactiveSupSpec,
        _TraceStoreSpec
    ] = ChildSpecs,
    ?assertEqual(beamtalk_reactive_subprocess_sup, maps:get(id, ReactiveSupSpec)),
    ?assertEqual(
        {beamtalk_reactive_subprocess_sup, start_link, []}, maps:get(start, ReactiveSupSpec)
    ),
    ?assertEqual(permanent, maps:get(restart, ReactiveSupSpec)),
    ?assertEqual(5000, maps:get(shutdown, ReactiveSupSpec)),
    ?assertEqual(supervisor, maps:get(type, ReactiveSupSpec)),
    ?assertEqual([beamtalk_reactive_subprocess_sup], maps:get(modules, ReactiveSupSpec)).

%%% init/1 tests

init_returns_proper_format_test() ->
    Result = beamtalk_runtime_sup:init([]),
    ?assertMatch({ok, {#{strategy := one_for_one}, [_, _, _, _, _, _]}}, Result).

%%% Behavioral tests

behaviour_implementation_test() ->
    Behaviors = proplists:get_value(behaviour, beamtalk_runtime_sup:module_info(attributes), []),
    ?assert(lists:member(supervisor, Behaviors)).

exports_required_callbacks_test() ->
    Exports = beamtalk_runtime_sup:module_info(exports),
    ?assert(lists:member({start_link, 0}, Exports)),
    ?assert(lists:member({init, 1}, Exports)).

%%% Startup smoke tests

all_children_alive_test() ->
    %% Set trap_exit before start_link to avoid dying if supervisor crashes early
    OldTrap = process_flag(trap_exit, true),

    %% Handle already-started supervisor (registered name)
    {Sup, WeStarted} =
        case beamtalk_runtime_sup:start_link() of
            {ok, Pid} -> {Pid, true};
            {error, {already_started, Pid}} -> {Pid, false}
        end,

    try
        %% Get all children
        Children = supervisor:which_children(Sup),

        %% Verify each child has correct ID and is alive
        ExpectedIds = [
            beamtalk_bootstrap,
            beamtalk_stdlib,
            beamtalk_object_instances,
            beamtalk_subprocess_sup,
            beamtalk_reactive_subprocess_sup,
            beamtalk_trace_store
        ],
        ActualIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
        ?assertEqual(lists:sort(ExpectedIds), lists:sort(ActualIds)),

        %% Verify each child process is alive
        lists:foreach(
            fun({_Id, ChildPid, _Type, _Modules}) ->
                ?assert(is_process_alive(ChildPid))
            end,
            Children
        )
    after
        %% Only shut down supervisor if we started it
        case WeStarted of
            true ->
                exit(Sup, shutdown),
                receive
                    {'EXIT', Sup, _} -> ok
                after 1000 -> ok
                end;
            false ->
                ok
        end,
        process_flag(trap_exit, OldTrap)
    end.
