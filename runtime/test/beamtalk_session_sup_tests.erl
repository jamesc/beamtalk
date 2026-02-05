%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_session_sup module
%%%
%%% Tests session supervisor behavior.

-module(beamtalk_session_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%%% Supervisor initialization tests

init_returns_correct_strategy_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),
    
    %% Should use simple_one_for_one strategy
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)).

init_returns_correct_intensity_test() ->
    {ok, {SupFlags, _ChildSpecs}} = beamtalk_session_sup:init([]),
    
    %% Should allow 10 restarts in 60 seconds
    ?assertEqual(10, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)).

init_returns_single_child_spec_test() ->
    {ok, {_SupFlags, ChildSpecs}} = beamtalk_session_sup:init([]),
    
    %% simple_one_for_one should have exactly 1 child spec (the template)
    ?assertEqual(1, length(ChildSpecs)).

init_child_spec_is_temporary_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),
    
    %% Sessions should be temporary (not restarted on disconnect)
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)).

%%% Child spec structure tests

child_spec_uses_repl_shell_test() ->
    {ok, {_SupFlags, [ChildSpec]}} = beamtalk_session_sup:init([]),
    
    %% Should use beamtalk_repl_shell:start_link/1
    ?assertEqual({beamtalk_repl_shell, start_link, []}, maps:get(start, ChildSpec)).

%%% Integration test

supervisor_can_start_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),
    
    %% Verify it's running
    ?assert(is_process_alive(Pid)),
    
    %% Get supervisor info
    Children = supervisor:which_children(Pid),
    
    %% Should have no children initially
    ?assertEqual([], Children),
    
    %% Cleanup
    exit(Pid, normal).

supervisor_count_children_test() ->
    %% Start the supervisor
    {ok, Pid} = beamtalk_session_sup:start_link(),
    
    %% Count children
    Counts = supervisor:count_children(Pid),
    
    %% Initially should have 0 active children
    ?assertEqual(0, proplists:get_value(active, Counts)),
    ?assertEqual(0, proplists:get_value(workers, Counts)),
    
    %% Cleanup
    exit(Pid, normal).
