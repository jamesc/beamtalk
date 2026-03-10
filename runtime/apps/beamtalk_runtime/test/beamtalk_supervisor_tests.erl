%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context
%%%
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

%% BT-1285: Fake class methods used by the hierarchy-walk test.
%% These are called directly by call_class_method_direct / call_inherited_class_method_direct
%% when the test populates the module ETS table with this module as the "parent" class module.
-export([class_children/2, class_strategy/2, class_maxRestarts/2, class_restartWindow/2]).

%%====================================================================
%% OTP supervisor callback
%%====================================================================

%% @private Used as the supervisor callback module.
%% {simple_one_for_one, ChildSpec}: dynamic supervisor with one child template.
%% {SupFlags, ChildSpecs}: static (one_for_one) supervisor.
init({simple_one_for_one, ChildSpec}) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    {ok, {SupFlags, [ChildSpec]}};
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
%% The worker uses erlang:spawn_link and signals readiness via message.
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

%% Start a simple_one_for_one dynamic supervisor (no children at start).
start_dynamic_supervisor() ->
    ChildSpec = #{
        id => dynamic_worker,
        start => {?MODULE, start_worker, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    },
    {ok, Pid} = supervisor:start_link(?MODULE, {simple_one_for_one, ChildSpec}),
    Pid.

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
    %% Test the actor-instance branch of terminateChild/2 (DynamicSupervisor path).
    %% Uses a simple_one_for_one supervisor so OTP accepts terminate_child by pid.
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('TestSup6', test_module6, SupPid),
        %% Start a dynamic child; start_worker/1 expects a parent pid to signal readiness.
        Parent = self(),
        {ok, ChildPid} = supervisor:start_child(SupPid, [Parent]),
        receive
            {worker_ready, _} -> ok
        after 1000 ->
            error(worker_not_ready)
        end,
        ?assert(is_process_alive(ChildPid)),
        %% terminateChild/2 DynamicSupervisor path: Arg is an actor tuple (not a class object).
        %% is_class_object returns false (no " class" suffix in element 2), so the pid branch runs.
        ChildArg = {beamtalk_object, 'SomeActor', some_mod, ChildPid},
        Result = beamtalk_supervisor:terminateChild(Self, ChildArg),
        ?assertEqual(nil, Result),
        timer:sleep(50),
        ?assertEqual(false, is_process_alive(ChildPid))
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

%%====================================================================
%% Tests: stale-handle error translation (noproc → beamtalk_error)
%%====================================================================

stale_handle_whichChildren_test() ->
    %% whichChildren/1 on a dead supervisor raises a structured runtime_error.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup', stale_mod, SupPid),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
        beamtalk_supervisor:whichChildren(Self)
    ).

stale_handle_countChildren_test() ->
    %% countChildren/1 on a dead supervisor raises a structured runtime_error.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup2', stale_mod2, SupPid),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
        beamtalk_supervisor:countChildren(Self)
    ).

stale_handle_stop_test() ->
    %% stop/1 on a dead supervisor raises a structured runtime_error.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup3', stale_mod3, SupPid),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
        beamtalk_supervisor:stop(Self)
    ).

root_registry_returns_nil_when_empty_test() ->
    %% get_root/0 returns nil when no root has been registered.
    %% Delete the table first so we start from a clean state.
    catch ets:delete(beamtalk_root_supervisor),
    ?assertEqual(nil, beamtalk_supervisor:get_root()).

root_registry_roundtrip_test() ->
    %% register_root/1 followed by get_root/0 returns the same tuple.
    catch ets:delete(beamtalk_root_supervisor),
    FakePid = self(),
    SupTuple = {beamtalk_supervisor, 'AppSup', 'bt@my_app@app_sup', FakePid},
    ok = beamtalk_supervisor:register_root(SupTuple),
    ?assertEqual(SupTuple, beamtalk_supervisor:get_root()).

root_registry_overwrite_test() ->
    %% A second register_root/1 call replaces the previous entry.
    catch ets:delete(beamtalk_root_supervisor),
    Pid1 = self(),
    Pid2 = spawn(fun() -> ok end),
    beamtalk_supervisor:register_root({beamtalk_supervisor, 'Old', old_mod, Pid1}),
    beamtalk_supervisor:register_root({beamtalk_supervisor, 'New', new_mod, Pid2}),
    ?assertMatch({beamtalk_supervisor, 'New', new_mod, _}, beamtalk_supervisor:get_root()).

%%====================================================================
%% Tests: BT-1285 — hierarchy walk uses ETS, not gen_server:call
%%====================================================================

%% @doc Verify that static_init/2 can walk a two-level Supervisor subclass
%% hierarchy when the ancestor's class methods are in a module looked up
%% via ETS — without any gen_server being registered for those classes.
%%
%% In the old code, call_inherited_class_method_direct called
%% beamtalk_object_class:module_name/1 (gen_server:call).  If the ancestor
%% class gen_server is blocked inside startLink/1, that call deadlocks.
%% The new code reads from beamtalk_class_module_table (ETS) instead.
%%
%% We simulate the deadlock scenario by populating ETS for a fake two-level
%% hierarchy where NO gen_server processes are registered for either class.
%% The old code would crash (gen_server:call to undefined); the new code
%% resolves the module from ETS and succeeds.
static_init_walks_hierarchy_via_ets_not_genserver_test() ->
    %% Ensure ETS tables exist.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),

    %% Set up a two-level hierarchy in ETS:
    %%   'BT1285Child' extends 'BT1285Parent' extends none
    %% Neither has a registered class gen_server process — this simulates
    %% the scenario where 'BT1285Parent' gen_server is blocked in startLink/1.
    beamtalk_class_hierarchy_table:insert('BT1285Child', 'BT1285Parent'),
    beamtalk_class_hierarchy_table:insert('BT1285Parent', none),

    %% Register module for 'BT1285Parent' only — 'BT1285Child' uses a module
    %% (erlang) that does NOT export class_children/2, forcing the walk upward.
    beamtalk_class_module_table:insert('BT1285Parent', ?MODULE),

    %% static_init/2 takes (Module, ClassName) where Module is the child's
    %% BEAM module.  We use 'erlang' as the child module so none of the
    %% class_* functions are exported there — the walk must climb to 'BT1285Parent'.
    %% 'BT1285Child' is not registered with whereis so ClassPid = undefined;
    %% that is fine since ClassSelf is only used as a value passed to class methods.
    Result = beamtalk_supervisor:static_init(erlang, 'BT1285Child'),

    %% Clean up ETS entries to avoid polluting state for other tests.
    beamtalk_class_hierarchy_table:delete('BT1285Child'),
    beamtalk_class_hierarchy_table:delete('BT1285Parent'),
    beamtalk_class_module_table:delete('BT1285Parent'),

    %% The walk found class_children/2, class_strategy/2, class_maxRestarts/2,
    %% and class_restartWindow/2 in ?MODULE (this test module) via the ETS lookup.
    %% It should succeed with zero children and one_for_one strategy.
    ?assertMatch(
        {ok, {#{strategy := one_for_one, intensity := 3, period := 5}, []}},
        Result
    ).

%%====================================================================
%% Fake class methods for BT-1285 hierarchy-walk test
%%====================================================================

class_children(_ClassSelf, _ClassVars) -> [].
class_strategy(_ClassSelf, _ClassVars) -> oneForOne.
class_maxRestarts(_ClassSelf, _ClassVars) -> 3.
class_restartWindow(_ClassSelf, _ClassVars) -> 5.
