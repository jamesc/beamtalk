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

%%% ============================================================================
%%% BT-1979: Child start/stop lifecycle coverage
%%% ============================================================================

start_actor_success_starts_child_test() ->
    %% Start the supervisor, then spawn a child actor through start_actor/3
    {ok, SupPid} = beamtalk_actor_sup:start_link(),
    try
        %% test_counter:init/1 expects an initial value; start_link_supervised/3
        %% forwards Args to the actor module's init/1.
        Result = beamtalk_actor_sup:start_actor(test_counter, start_link, [42]),
        ?assertMatch({ok, _}, Result),
        {ok, ChildPid} = Result,
        ?assert(is_process_alive(ChildPid)),
        %% Supervisor should now report one active child
        Counts = supervisor:count_children(SupPid),
        ?assertEqual(1, proplists:get_value(active, Counts)),
        %% The child should be functional — sync call should return the seed
        ?assertEqual(42, gen_server:call(ChildPid, {getValue, []}))
    after
        exit(SupPid, normal),
        timer:sleep(10)
    end.

start_actor_error_path_logs_and_returns_test() ->
    %% When the module doesn't exist, start_link_supervised/3 -> start_link/3
    %% will fail and supervisor:start_child returns {error, Reason}. This
    %% exercises the {error, Reason} branch of start_actor/3 (including its
    %% ?LOG_ERROR line).
    {ok, SupPid} = beamtalk_actor_sup:start_link(),
    try
        Result = beamtalk_actor_sup:start_actor(nonexistent_module, start_link, [0]),
        ?assertMatch({error, _}, Result)
    after
        exit(SupPid, normal),
        timer:sleep(10)
    end.

start_link_logs_success_test() ->
    %% Covers the {ok, Pid} log path in start_link/0 (the case where
    %% supervisor:start_link returns ok). The previous tests all start the
    %% supervisor too but we assert on the behaviour end-to-end.
    {ok, Pid} = beamtalk_actor_sup:start_link(),
    ?assert(is_process_alive(Pid)),
    %% Registered under the module name
    ?assertEqual(Pid, whereis(beamtalk_actor_sup)),
    exit(Pid, normal),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(beamtalk_actor_sup)).

start_actor_multiple_children_test() ->
    %% Exercise starting several children under the supervisor
    {ok, SupPid} = beamtalk_actor_sup:start_link(),
    try
        {ok, C1} = beamtalk_actor_sup:start_actor(test_counter, start_link, [1]),
        {ok, C2} = beamtalk_actor_sup:start_actor(test_counter, start_link, [2]),
        {ok, C3} = beamtalk_actor_sup:start_actor(test_counter, start_link, [3]),
        ?assert(is_process_alive(C1)),
        ?assert(is_process_alive(C2)),
        ?assert(is_process_alive(C3)),
        Counts = supervisor:count_children(SupPid),
        ?assertEqual(3, proplists:get_value(active, Counts)),
        ?assertEqual(3, proplists:get_value(workers, Counts)),
        %% Values should be distinct (seeded per start)
        ?assertEqual(1, gen_server:call(C1, {getValue, []})),
        ?assertEqual(2, gen_server:call(C2, {getValue, []})),
        ?assertEqual(3, gen_server:call(C3, {getValue, []}))
    after
        exit(SupPid, normal),
        timer:sleep(10)
    end.

start_link_already_started_returns_error_test() ->
    %% Covers the non-{ok,Pid} branch at line 33-34 in start_link/0 (the
    %% `_ -> ok` clause in the log case). When the supervisor is already
    %% registered, a second start_link returns {error, {already_started, Pid}}.
    {ok, FirstPid} = beamtalk_actor_sup:start_link(),
    try
        Result = beamtalk_actor_sup:start_link(),
        ?assertMatch({error, {already_started, _}}, Result)
    after
        exit(FirstPid, normal),
        timer:sleep(10)
    end.

temporary_child_not_restarted_on_crash_test() ->
    %% Verifies the `temporary` restart strategy: crashed actors are NOT
    %% automatically restarted by the supervisor.
    {ok, SupPid} = beamtalk_actor_sup:start_link(),
    try
        {ok, ChildPid} = beamtalk_actor_sup:start_actor(test_counter, start_link, [7]),
        %% Monitor and kill the child
        Ref = erlang:monitor(process, ChildPid),
        exit(ChildPid, kill),
        receive
            {'DOWN', Ref, process, ChildPid, _} -> ok
        after 1000 ->
            ?assert(false)
        end,
        %% After crash, active count should be 0 (temporary child not restarted)
        Counts = supervisor:count_children(SupPid),
        ?assertEqual(0, proplists:get_value(active, Counts))
    after
        exit(SupPid, normal),
        timer:sleep(10)
    end.
