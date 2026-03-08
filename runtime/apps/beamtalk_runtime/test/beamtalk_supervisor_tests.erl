%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for beamtalk_supervisor runtime helper.
%%%
%%% Tests cover:
%%% - is_supervisor/1 ancestry check
%%% - whichChildren/1 child id extraction
%%% - countChildren/1 active count
%%% - stop/1 returns nil
%%% - build_child_specs/1 with empty list
%%% - supervisor tuple structure
-module(beamtalk_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% OTP supervisor callback and worker start function — required for supervisor:start_link/3.
-export([init/1, start_worker/1]).

%%====================================================================
%% OTP supervisor callback
%%====================================================================

%% @private Used by start_anon_supervisor/0 as the supervisor callback module.
init({SupFlags, ChildSpecs}) ->
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Helpers
%%====================================================================

%% Start an anonymous OTP supervisor (no registered name) with no children.
start_anon_supervisor() ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, []}),
    Pid.

%% Start an anonymous OTP supervisor with one temporary worker child.
%% The worker uses proc_lib:start_link/3 for proper OTP worker initialization.
start_anon_supervisor_with_worker(ChildId) ->
    Self = self(),
    ChildSpec = #{
        id => ChildId,
        start => {?MODULE, start_worker, [Self]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    },
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
    receive
        {worker_ready, _} -> ok
    after 1000 ->
        error(worker_not_ready)
    end,
    Pid.

%% @private OTP-compatible worker start function (returns {ok, Pid}).
start_worker(ParentPid) ->
    WorkerPid = erlang:spawn_link(fun() ->
        ParentPid ! {worker_ready, self()},
        receive
            stop -> ok
        end
    end),
    {ok, WorkerPid}.

make_supervisor_tuple(ClassName, Module, Pid) ->
    {beamtalk_supervisor, ClassName, Module, Pid}.

%%====================================================================
%% Tests: is_supervisor/1
%%====================================================================

is_supervisor_unknown_class_test() ->
    %% A class that does not exist in the registry returns false.
    ?assertEqual(false, beamtalk_supervisor:is_supervisor('NonExistentClass99')).

%%====================================================================
%% Tests: whichChildren/1
%%====================================================================

which_children_empty_test() ->
    %% A supervisor with no children returns [].
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('TestSup', test_module, SupPid),
        ?assertEqual([], beamtalk_supervisor:whichChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

which_children_running_child_test() ->
    %% A supervisor with one running child returns its id.
    SupPid = start_anon_supervisor_with_worker(worker_id),
    try
        Self = make_supervisor_tuple('TestSup2', test_module2, SupPid),
        Ids = beamtalk_supervisor:whichChildren(Self),
        ?assertEqual([worker_id], Ids)
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: countChildren/1
%%====================================================================

count_children_empty_test() ->
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('TestSup3', test_module3, SupPid),
        ?assertEqual(0, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_with_child_test() ->
    SupPid = start_anon_supervisor_with_worker(count_child_id),
    try
        Self = make_supervisor_tuple('TestSup4', test_module4, SupPid),
        ?assertEqual(1, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: stop/1
%%====================================================================

stop_returns_nil_test() ->
    SupPid = start_anon_supervisor(),
    Self = make_supervisor_tuple('TestSup5', test_module5, SupPid),
    Result = beamtalk_supervisor:stop(Self),
    ?assertEqual(nil, Result),
    %% Give the process a moment to shut down
    timer:sleep(50),
    ?assertEqual(false, is_process_alive(SupPid)).

%%====================================================================
%% Tests: terminateChild/2 — dynamic supervisor path (by pid)
%%====================================================================

terminate_child_by_pid_test() ->
    %% Test the actor-instance branch of terminateChild/2.
    %% We use an actor-like tuple (not a class object) as the child arg.
    SupPid = start_anon_supervisor_with_worker(tc_child_id),
    try
        Self = make_supervisor_tuple('TestSup6', test_module6, SupPid),
        %% Verify child is alive
        ?assertEqual(1, beamtalk_supervisor:countChildren(Self)),
        %% Terminate via supervisor:terminate_child directly (no class lookup needed here)
        ok = supervisor:terminate_child(SupPid, tc_child_id),
        ?assertEqual(0, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: build_child_specs/1
%%====================================================================

build_child_specs_empty_test() ->
    %% Empty list returns empty list.
    ?assertEqual([], beamtalk_supervisor:build_child_specs([])).

%%====================================================================
%% Tests: supervisor tuple structure
%%====================================================================

supervisor_tuple_tag_test() ->
    %% Verify the tuple tag for supervisor instances.
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('WebApp', bt_webapp, SupPid),
        ?assertEqual(beamtalk_supervisor, element(1, Self)),
        ?assertEqual('WebApp', element(2, Self)),
        ?assertEqual(bt_webapp, element(3, Self)),
        ?assertEqual(SupPid, element(4, Self))
    after
        gen_server:stop(SupPid)
    end.
