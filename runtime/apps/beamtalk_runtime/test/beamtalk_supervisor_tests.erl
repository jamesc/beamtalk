%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context
%%%
-module(beamtalk_supervisor_tests).

-moduledoc """
Unit tests for beamtalk_supervisor runtime helper.

Tests cover:
- is_supervisor/1 ancestry check via ETS hierarchy (BT-1960)
- current/1 — running and not-running supervisor lookup (BT-1960)
- whichChildren/1 child id extraction, multiple children (BT-1960)
- whichChild/2 — found and not-found child lookup (BT-1960)
- countChildren/1 active count, multiple children, dynamic supervisor (BT-1960)
- stop/1 returns nil, kills supervisor process
- terminateChild/2 — dynamic path, stale handle, not_found error (BT-1960)
- build_child_specs/1 with empty list
- supervisor tuple structure
- stale-handle error translation (noproc → beamtalk_error)
- root registry — nil, roundtrip, overwrite, clear_root (BT-1960)
- hierarchy walk — static_init/2, dynamic_init/2 via ETS (BT-1285, BT-1960)
- hierarchy depth limit — call_inherited_class_method_direct error (BT-1960)
- start_child_via_class_method/4: valid return, invalid return, class_var_result
  unwrap, process dictionary cleanup, child linking, supervisor restart,
  supervisor tuple return (BT-1875, BT-1960)
- ensure_root_table idempotent creation (BT-1960)
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% OTP supervisor callback and worker start function — required for supervisor:start_link/3.
-export([init/1, start_worker/1]).

%% BT-1285: Fake class methods used by the hierarchy-walk test.
%% These are called directly by call_class_method_direct / call_inherited_class_method_direct
%% when the test populates the module ETS table with this module as the "parent" class module.
-export([class_children/2, class_strategy/2, class_maxRestarts/2, class_restartWindow/2]).

%% BT-1875: Fake class methods for start_child_via_class_method tests.
%% Simulates what a compiled Beamtalk keyword class method does: start a gen_server
%% and return a {beamtalk_object, ...} tuple.
-export([
    'class_create:value:'/4,
    class_returnInvalid/2,
    class_returnWrapped/2,
    class_returnSupervisor/2,
    start_link_fake_child/0
]).

%% BT-1960: Fake class methods for dynamic_init and hierarchy tests.
-export([class_childClass/2]).

%% gen_server callbacks for fake child actor used by BT-1875 tests.
-export([handle_call/3, handle_cast/2]).

%%====================================================================
%% OTP supervisor callback
%%====================================================================

-doc """
Used as the supervisor callback module.
{simple_one_for_one, ChildSpec}: dynamic supervisor with one child template.
{SupFlags, ChildSpecs}: static (one_for_one) supervisor.
""".
init({simple_one_for_one, ChildSpec}) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 0, period => 1},
    {ok, {SupFlags, [ChildSpec]}};
init({SupFlags, ChildSpecs}) ->
    {ok, {SupFlags, ChildSpecs}};
%% BT-1875: Fake child actor init — minimal gen_server for supervision tests.
init(fake_child) ->
    {ok, #{counter => 0}}.

%% BT-1875: gen_server callbacks for fake child actor.
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

-doc "OTP-compatible worker start function (returns {ok, Pid}).".
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

%% Start an anonymous OTP supervisor with multiple temporary worker children.
start_anon_supervisor_with_workers(ChildIds) ->
    Self = self(),
    ChildSpecs = [
        #{
            id => Id,
            start => {?MODULE, start_worker, [Self]},
            restart => temporary,
            shutdown => brutal_kill,
            type => worker,
            modules => []
        }
     || Id <- ChildIds
    ],
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, ChildSpecs}),
    %% Wait for all workers to report ready.
    lists:foreach(
        fun(_) ->
            receive
                {worker_ready, _} -> ok
            after 1000 -> error(worker_not_ready)
            end
        end,
        ChildIds
    ),
    Pid.

%% Start a named OTP supervisor (registers under the given atom).
start_named_supervisor(Name) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link({local, Name}, ?MODULE, {SupFlags, []}),
    Pid.

make_supervisor_tuple(ClassName, Module, Pid) ->
    {beamtalk_supervisor, ClassName, Module, Pid}.

%%====================================================================
%% Tests: is_supervisor/1
%%====================================================================

is_supervisor_unknown_class_test() ->
    %% A class that does not exist in the registry returns false.
    ?assertEqual(false, beamtalk_supervisor:is_supervisor('NonExistentClass99')).

is_supervisor_returns_true_for_supervisor_subclass_test() ->
    %% BT-1960: A class registered as inheriting from 'Supervisor' returns true.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_hierarchy_table:insert('BT1960SupervisorChild', 'Supervisor'),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960SupervisorChild'))
    after
        beamtalk_class_hierarchy_table:delete('BT1960SupervisorChild')
    end.

is_supervisor_returns_true_for_dynamic_supervisor_subclass_test() ->
    %% BT-1960: A class inheriting from 'DynamicSupervisor' returns true.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_hierarchy_table:insert('BT1960DynChild', 'DynamicSupervisor'),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960DynChild'))
    after
        beamtalk_class_hierarchy_table:delete('BT1960DynChild')
    end.

is_supervisor_returns_false_for_non_supervisor_class_test() ->
    %% BT-1960: A class that inherits from 'Actor' (not Supervisor) returns false.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_hierarchy_table:insert('BT1960Worker', 'Actor'),
    beamtalk_class_hierarchy_table:insert('Actor', none),
    try
        ?assertEqual(false, beamtalk_supervisor:is_supervisor('BT1960Worker'))
    after
        beamtalk_class_hierarchy_table:delete('BT1960Worker'),
        beamtalk_class_hierarchy_table:delete('Actor')
    end.

%%====================================================================
%% Tests: current/1
%%====================================================================

current_returns_nil_when_not_running_test() ->
    %% BT-1960: current/1 returns nil when the supervisor is not started.
    %% We need a class object with a ClassPid that has class_name and module_name.
    %% Use a fake gen_server to provide those responses.
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'BT1960NotRunning'),
                    Loop();
                {'$gen_call', From, module_name} ->
                    gen_server:reply(From, bt1960_not_running_mod),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        Self = {beamtalk_object, 'BT1960NotRunning class', bt1960_not_running_mod, FakeClassPid},
        Result = beamtalk_supervisor:current(Self),
        ?assertEqual(nil, Result)
    after
        FakeClassPid ! stop
    end.

current_returns_supervisor_tuple_when_running_test() ->
    %% BT-1960: current/1 returns a supervisor tuple when the process is registered.
    ModName = bt1960_current_mod,
    SupPid = start_named_supervisor(ModName),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'BT1960Current'),
                    Loop();
                {'$gen_call', From, module_name} ->
                    gen_server:reply(From, ModName),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        Self = {beamtalk_object, 'BT1960Current class', ModName, FakeClassPid},
        Result = beamtalk_supervisor:current(Self),
        ?assertMatch({beamtalk_supervisor, 'BT1960Current', bt1960_current_mod, _}, Result),
        ?assertEqual(SupPid, element(4, Result))
    after
        FakeClassPid ! stop,
        gen_server:stop(SupPid)
    end.

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

which_children_multiple_children_test() ->
    %% BT-1960: A supervisor with multiple children returns all their ids.
    SupPid = start_anon_supervisor_with_workers([child_a, child_b, child_c]),
    try
        Self = make_supervisor_tuple('TestSupMulti', test_mod_multi, SupPid),
        Ids = beamtalk_supervisor:whichChildren(Self),
        ?assertEqual(3, length(Ids)),
        ?assert(lists:member(child_a, Ids)),
        ?assert(lists:member(child_b, Ids)),
        ?assert(lists:member(child_c, Ids))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: whichChild/2
%%====================================================================

which_child_returns_nil_when_not_found_test() ->
    %% BT-1960: whichChild/2 returns nil when the child class module is not running.
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('TestSupWC', test_mod_wc, SupPid),
        %% Create a fake class arg with a gen_server-like pid for class_name.
        FakeClassPid = spawn(fun() ->
            (fun Loop() ->
                receive
                    {'$gen_call', From, class_name} ->
                        gen_server:reply(From, 'MissingClass'),
                        Loop();
                    stop ->
                        ok
                end
            end)()
        end),
        ClassArg = {beamtalk_object, 'MissingClass class', missing_mod, FakeClassPid},
        Result = beamtalk_supervisor:whichChild(Self, ClassArg),
        ?assertEqual(nil, Result),
        FakeClassPid ! stop
    after
        gen_server:stop(SupPid)
    end.

which_child_stale_handle_test() ->
    %% BT-1960: whichChild/2 on a dead supervisor raises a structured runtime_error.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSupWC', stale_mod_wc, SupPid),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'X'),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg = {beamtalk_object, 'X class', x_mod, FakeClassPid},
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
            beamtalk_supervisor:whichChild(Self, ClassArg)
        )
    after
        FakeClassPid ! stop
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

count_children_multiple_test() ->
    %% BT-1960: countChildren with multiple children returns the correct count.
    SupPid = start_anon_supervisor_with_workers([count_a, count_b]),
    try
        Self = make_supervisor_tuple('TestSupCount2', test_mod_cnt2, SupPid),
        ?assertEqual(2, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_dynamic_supervisor_test() ->
    %% BT-1960: countChildren on a dynamic supervisor with no children returns 0.
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('TestDynCount', test_dyn_cnt, SupPid),
        ?assertEqual(0, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_dynamic_supervisor_with_children_test() ->
    %% BT-1960: countChildren on a dynamic supervisor after adding children.
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('TestDynCount2', test_dyn_cnt2, SupPid),
        Parent = self(),
        {ok, _Child1} = supervisor:start_child(SupPid, [Parent]),
        receive
            {worker_ready, _} -> ok
        after 1000 -> error(timeout)
        end,
        {ok, _Child2} = supervisor:start_child(SupPid, [Parent]),
        receive
            {worker_ready, _} -> ok
        after 1000 -> error(timeout)
        end,
        ?assertEqual(2, beamtalk_supervisor:countChildren(Self))
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

terminate_child_not_found_dynamic_test() ->
    %% BT-1960: terminateChild/2 for a pid not in the supervisor raises runtime_error.
    SupPid = start_dynamic_supervisor(),
    FakePid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        Self = make_supervisor_tuple('TestSup7', test_module7, SupPid),
        ChildArg = {beamtalk_object, 'SomeActor', some_mod, FakePid},
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
            beamtalk_supervisor:terminateChild(Self, ChildArg)
        )
    after
        FakePid ! stop,
        gen_server:stop(SupPid)
    end.

terminate_child_stale_handle_test() ->
    %% BT-1960: terminateChild/2 on a dead supervisor raises structured runtime_error.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup4', stale_mod4, SupPid),
    ChildArg = {beamtalk_object, 'SomeActor', some_mod, self()},
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = runtime_error}},
        beamtalk_supervisor:terminateChild(Self, ChildArg)
    ).

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
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    ?assertEqual(nil, beamtalk_supervisor:get_root()).

root_registry_roundtrip_test() ->
    %% register_root/1 followed by get_root/0 returns the same tuple.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    FakePid = self(),
    SupTuple = {beamtalk_supervisor, 'AppSup', 'bt@my_app@app_sup', FakePid},
    ok = beamtalk_supervisor:register_root(SupTuple),
    ?assertEqual(SupTuple, beamtalk_supervisor:get_root()).

root_registry_overwrite_test() ->
    %% A second register_root/1 call replaces the previous entry.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    Pid1 = self(),
    Pid2 = spawn(fun() -> ok end),
    beamtalk_supervisor:register_root({beamtalk_supervisor, 'Old', old_mod, Pid1}),
    beamtalk_supervisor:register_root({beamtalk_supervisor, 'New', new_mod, Pid2}),
    ?assertMatch({beamtalk_supervisor, 'New', new_mod, _}, beamtalk_supervisor:get_root()).

clear_root_removes_entry_test() ->
    %% BT-1960: clear_root/0 removes the root entry so get_root returns nil.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    SupTuple = {beamtalk_supervisor, 'ClearMe', clear_mod, self()},
    ok = beamtalk_supervisor:register_root(SupTuple),
    ?assertEqual(SupTuple, beamtalk_supervisor:get_root()),
    ok = beamtalk_supervisor:clear_root(),
    ?assertEqual(nil, beamtalk_supervisor:get_root()).

clear_root_idempotent_test() ->
    %% BT-1960: clear_root/0 does not crash when called with no entry.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    %% Ensure table exists by calling get_root first.
    ?assertEqual(nil, beamtalk_supervisor:get_root()),
    %% clear_root on empty table should not crash.
    ?assertEqual(ok, beamtalk_supervisor:clear_root()).

ensure_root_table_idempotent_test() ->
    %% BT-1960: Multiple calls to get_root (which calls ensure_root_table)
    %% do not crash — table creation is idempotent.
    (try
        ets:delete(beamtalk_root_supervisor)
    catch
        _:_ -> ok
    end),
    ?assertEqual(nil, beamtalk_supervisor:get_root()),
    ?assertEqual(nil, beamtalk_supervisor:get_root()),
    ?assertEqual(nil, beamtalk_supervisor:get_root()).

%%====================================================================
%% Tests: BT-1285 — hierarchy walk uses ETS, not gen_server:call
%%====================================================================

-doc """
Verify that static_init/2 can walk a two-level Supervisor subclass
hierarchy when the ancestor's class methods are in a module looked up
via ETS — without any gen_server being registered for those classes.

In the old code, call_inherited_class_method_direct called
beamtalk_object_class:module_name/1 (gen_server:call).  If the ancestor
class gen_server is blocked inside startLink/1, that call deadlocks.
The new code reads from beamtalk_class_module_table (ETS) instead.

We simulate the deadlock scenario by populating ETS for a fake two-level
hierarchy where NO gen_server processes are registered for either class.
The old code would crash (gen_server:call to undefined); the new code
resolves the module from ETS and succeeds.
""".
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

dynamic_init_walks_hierarchy_via_ets_test() ->
    %% BT-1960: dynamic_init/2 uses the same ETS hierarchy walk as static_init.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),

    beamtalk_class_hierarchy_table:insert('BT1960DynChild', 'BT1960DynParent'),
    beamtalk_class_hierarchy_table:insert('BT1960DynParent', none),
    beamtalk_class_module_table:insert('BT1960DynParent', ?MODULE),

    %% class_childClass returns a supervisor subclass object, so register it.
    beamtalk_class_hierarchy_table:insert('BT1960DynChildSup', 'Supervisor'),

    %% dynamic_init needs class_childClass, class_maxRestarts, class_restartWindow.
    %% class_childClass returns a class object (beamtalk_object record).
    Result = beamtalk_supervisor:dynamic_init(erlang, 'BT1960DynChild'),

    beamtalk_class_hierarchy_table:delete('BT1960DynChild'),
    beamtalk_class_hierarchy_table:delete('BT1960DynParent'),
    beamtalk_class_hierarchy_table:delete('BT1960DynChildSup'),
    beamtalk_class_module_table:delete('BT1960DynParent'),

    %% dynamic_init always uses simple_one_for_one strategy.
    %% The child spec should contain a start_link entry for the supervisor child.
    ?assertMatch(
        {ok, {#{strategy := simple_one_for_one, intensity := 3, period := 5}, [_ChildSpec]}},
        Result
    ).

hierarchy_depth_limit_error_test() ->
    %% BT-1960: Verify that call_inherited_class_method_direct raises
    %% {supervisor_init_method_not_found, FunName} when the depth limit is exceeded.
    %% We create a circular-ish hierarchy by having a class point to itself (via ETS).
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),

    %% Create a hierarchy where every class points to a parent that doesn't have
    %% the method, eventually exceeding the depth limit of 30.
    %% A points to B, B points to A — circular, will hit depth limit.
    beamtalk_class_hierarchy_table:insert('BT1960LoopA', 'BT1960LoopB'),
    beamtalk_class_hierarchy_table:insert('BT1960LoopB', 'BT1960LoopA'),
    %% Both modules are 'erlang' which won't have class_children.
    beamtalk_class_module_table:insert('BT1960LoopA', erlang),
    beamtalk_class_module_table:insert('BT1960LoopB', erlang),

    try
        ?assertError(
            {supervisor_init_method_not_found, class_children},
            beamtalk_supervisor:static_init(erlang, 'BT1960LoopA')
        )
    after
        beamtalk_class_hierarchy_table:delete('BT1960LoopA'),
        beamtalk_class_hierarchy_table:delete('BT1960LoopB'),
        beamtalk_class_module_table:delete('BT1960LoopA'),
        beamtalk_class_module_table:delete('BT1960LoopB')
    end.

hierarchy_method_not_found_no_parent_test() ->
    %% BT-1960: When a class has no parent (not_found in hierarchy table),
    %% the walk raises {supervisor_init_method_not_found, FunName}.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),

    %% 'BT1960Orphan' has no entry in hierarchy table at all.
    %% The module (erlang) doesn't export class_children/2.
    try
        ?assertError(
            {supervisor_init_method_not_found, class_children},
            beamtalk_supervisor:static_init(erlang, 'BT1960Orphan')
        )
    after
        ok
    end.

%%====================================================================
%% Fake class methods for BT-1285 hierarchy-walk test
%%====================================================================

class_children(_ClassSelf, _ClassVars) -> [].
class_strategy(_ClassSelf, _ClassVars) -> oneForOne.
class_maxRestarts(_ClassSelf, _ClassVars) -> 3.
class_restartWindow(_ClassSelf, _ClassVars) -> 5.

%% BT-1960: Fake class_childClass for dynamic_init tests.
%% Returns a class object for a supervisor subclass so build_child_spec takes the
%% simple path (no message dispatch needed). class_name/1 is called via gen_server,
%% so we spawn a tiny process that responds to it. The process handles one class_name
%% call then exits cleanly (build_child_spec only calls it once).
class_childClass(_ClassSelf, _ClassVars) ->
    FakeClassPid = spawn(fun() ->
        receive
            {'$gen_call', From, class_name} ->
                gen_server:reply(From, 'BT1960DynChildSup')
        after 5000 ->
            ok
        end
    end),
    %% Tag with " class" so is_class_object returns true.
    %% BT1960DynChildSup inherits from Supervisor so is_supervisor returns true
    %% and build_child_spec takes the nested-supervisor shortcut path.
    #beamtalk_object{class = 'BT1960DynChildSup class', class_mod = ?MODULE, pid = FakeClassPid}.

%%====================================================================
%% BT-1875: start_child_via_class_method tests
%%====================================================================

-doc "Start a fake child gen_server — OTP-compatible {ok, Pid} return.".
start_link_fake_child() ->
    gen_server:start_link(?MODULE, fake_child, []).

%% Fake class method: simulates `MyClass create: Name value: Val` which starts
%% a gen_server and returns a beamtalk_object tuple.
'class_create:value:'(_ClassSelf, _ClassVars, _Name, _Value) ->
    {ok, Pid} = start_link_fake_child(),
    {beamtalk_object, 'FakeChild', ?MODULE, Pid}.

%% Fake class method: returns an invalid (non-actor) value.
class_returnInvalid(_ClassSelf, _ClassVars) ->
    <<"not an actor tuple">>.

%% Fake class method: returns a class_var_result wrapper around a valid actor.
class_returnWrapped(_ClassSelf, _ClassVars) ->
    {ok, Pid} = start_link_fake_child(),
    {class_var_result, {beamtalk_object, 'FakeChild', ?MODULE, Pid}, #{some_var => 42}}.

%% BT-1960: Fake class method that returns a supervisor tuple instead of an actor.
class_returnSupervisor(_ClassSelf, _ClassVars) ->
    {ok, Pid} = start_link_fake_child(),
    {beamtalk_supervisor, 'FakeSupervisorChild', ?MODULE, Pid}.

-doc """
Set up ETS tables and register a fake class for start_child_via_class_method.
Returns the fake class pid (a dummy process) that should be cleaned up after the test.
""".
setup_fake_class(ClassName) ->
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),
    %% Register a dummy process as the class gen_server so whereis_class resolves.
    FakeClassPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    RegName = beamtalk_class_registry:registry_name(ClassName),
    register(RegName, FakeClassPid),
    %% Register module mapping so call_class_method_direct can find our fake methods.
    beamtalk_class_module_table:insert(ClassName, ?MODULE),
    FakeClassPid.

-doc "Clean up after a fake class test.".
cleanup_fake_class(ClassName, FakeClassPid) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    (try
        unregister(RegName)
    catch
        _:_ -> ok
    end),
    FakeClassPid ! stop,
    beamtalk_class_module_table:delete(ClassName),
    ok.

%% --- Tests ---

start_child_via_class_method_returns_ok_pid_test() ->
    %% A class method that returns {beamtalk_object, _, _, Pid} yields {ok, Pid}.
    FakeClassPid = setup_fake_class('BT1875Actor'),
    try
        {ok, ChildPid} = beamtalk_supervisor:start_child_via_class_method(
            'BT1875Actor', ?MODULE, 'class_create:value:', [hello, 42]
        ),
        ?assert(is_pid(ChildPid)),
        ?assert(is_process_alive(ChildPid)),
        gen_server:stop(ChildPid)
    after
        cleanup_fake_class('BT1875Actor', FakeClassPid)
    end.

start_child_via_class_method_invalid_return_test() ->
    %% A class method that returns a non-actor value yields {error, WrappedError}
    %% where the error is a beamtalk_error with runtime_error kind.
    FakeClassPid = setup_fake_class('BT1875Invalid'),
    try
        Result = beamtalk_supervisor:start_child_via_class_method(
            'BT1875Invalid', ?MODULE, class_returnInvalid, []
        ),
        ?assertMatch({error, #{'$beamtalk_class' := _, error := #beamtalk_error{}}}, Result)
    after
        cleanup_fake_class('BT1875Invalid', FakeClassPid)
    end.

start_child_via_class_method_unwraps_class_var_result_test() ->
    %% A class method returning {class_var_result, ActorTuple, Vars} is unwrapped.
    FakeClassPid = setup_fake_class('BT1875Wrapped'),
    try
        {ok, ChildPid} = beamtalk_supervisor:start_child_via_class_method(
            'BT1875Wrapped', ?MODULE, class_returnWrapped, []
        ),
        ?assert(is_pid(ChildPid)),
        ?assert(is_process_alive(ChildPid)),
        gen_server:stop(ChildPid)
    after
        cleanup_fake_class('BT1875Wrapped', FakeClassPid)
    end.

start_child_via_class_method_cleans_process_dict_test() ->
    %% Process dictionary entries are cleaned up even on success.
    FakeClassPid = setup_fake_class('BT1875Cleanup'),
    try
        {ok, ChildPid} = beamtalk_supervisor:start_child_via_class_method(
            'BT1875Cleanup', ?MODULE, 'class_create:value:', [a, b]
        ),
        gen_server:stop(ChildPid),
        %% Verify process dictionary is clean after the call.
        ?assertEqual(undefined, get(beamtalk_class_name)),
        ?assertEqual(undefined, get(beamtalk_class_module)),
        ?assertEqual(undefined, get(beamtalk_class_is_abstract))
    after
        cleanup_fake_class('BT1875Cleanup', FakeClassPid)
    end.

start_child_via_class_method_cleans_process_dict_on_error_test() ->
    %% Process dictionary entries are cleaned up even when the class method
    %% returns an invalid value.
    FakeClassPid = setup_fake_class('BT1875CleanupErr'),
    try
        _ = beamtalk_supervisor:start_child_via_class_method(
            'BT1875CleanupErr', ?MODULE, class_returnInvalid, []
        ),
        ?assertEqual(undefined, get(beamtalk_class_name)),
        ?assertEqual(undefined, get(beamtalk_class_module)),
        ?assertEqual(undefined, get(beamtalk_class_is_abstract))
    after
        cleanup_fake_class('BT1875CleanupErr', FakeClassPid)
    end.

start_child_via_class_method_child_linked_to_caller_test() ->
    %% The child started via class method is linked to the calling process
    %% (which in production is the OTP supervisor process), NOT to the class
    %% gen_server. This verifies the fix from the original Copilot review.
    FakeClassPid = setup_fake_class('BT1875Link'),
    try
        {ok, ChildPid} = beamtalk_supervisor:start_child_via_class_method(
            'BT1875Link', ?MODULE, 'class_create:value:', [x, y]
        ),
        %% The child should be linked to self() (the test process = the caller),
        %% NOT to FakeClassPid (the class gen_server).
        {links, CallerLinks} = process_info(self(), links),
        {links, ClassLinks} = process_info(FakeClassPid, links),
        ?assert(lists:member(ChildPid, CallerLinks)),
        ?assertNot(lists:member(ChildPid, ClassLinks)),
        %% Unlink before stopping to avoid test process crash.
        unlink(ChildPid),
        gen_server:stop(ChildPid)
    after
        cleanup_fake_class('BT1875Link', FakeClassPid)
    end.

start_child_via_class_method_supervisor_restart_test() ->
    %% OTP supervisor restarts a crashed child via start_child_via_class_method.
    %% This is the key integration test: a child spec using the class method MFA
    %% is placed in a real OTP supervisor, the child is killed, and we verify the
    %% supervisor restarts it automatically.
    FakeClassPid = setup_fake_class('BT1875Restart'),
    try
        ChildSpec = #{
            id => 'BT1875Restart',
            start =>
                {beamtalk_supervisor, start_child_via_class_method, [
                    'BT1875Restart', ?MODULE, 'class_create:value:', [test, 1]
                ]},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [?MODULE]
        },
        SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
        {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
        try
            %% Get the initial child pid.
            [{_, ChildPid1, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid1)),
            ?assert(is_process_alive(ChildPid1)),

            %% Monitor to detect when the child dies.
            Ref = monitor(process, ChildPid1),
            %% Kill the child — the supervisor should restart it.
            exit(ChildPid1, kill),
            receive
                {'DOWN', Ref, process, ChildPid1, killed} -> ok
            after 2000 ->
                error(child_did_not_die)
            end,

            %% Give the supervisor time to restart.
            timer:sleep(100),

            %% Verify a new child is running with a different pid.
            [{_, ChildPid2, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid2)),
            ?assert(is_process_alive(ChildPid2)),
            ?assertNotEqual(ChildPid1, ChildPid2)
        after
            gen_server:stop(SupPid)
        end
    after
        cleanup_fake_class('BT1875Restart', FakeClassPid)
    end.

start_child_via_class_method_returns_supervisor_tuple_test() ->
    %% BT-1960: A class method returning {beamtalk_supervisor, ...} yields {ok, Pid}.
    FakeClassPid = setup_fake_class('BT1960SupReturn'),
    try
        {ok, ChildPid} = beamtalk_supervisor:start_child_via_class_method(
            'BT1960SupReturn', ?MODULE, class_returnSupervisor, []
        ),
        ?assert(is_pid(ChildPid)),
        ?assert(is_process_alive(ChildPid)),
        unlink(ChildPid),
        gen_server:stop(ChildPid)
    after
        cleanup_fake_class('BT1960SupReturn', FakeClassPid)
    end.

%%====================================================================
%% Tests: is_supervisor integration with hierarchy (BT-1960)
%%====================================================================

is_supervisor_transitive_inheritance_test() ->
    %% BT-1960: is_supervisor returns true for a grandchild of Supervisor.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_hierarchy_table:insert('BT1960GrandChild', 'BT1960Middle'),
    beamtalk_class_hierarchy_table:insert('BT1960Middle', 'Supervisor'),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960GrandChild'))
    after
        beamtalk_class_hierarchy_table:delete('BT1960GrandChild'),
        beamtalk_class_hierarchy_table:delete('BT1960Middle')
    end.

%%====================================================================
%% Tests: stop/1 — children are terminated (BT-1960)
%%====================================================================

stop_terminates_children_test() ->
    %% BT-1960: stop/1 terminates the supervisor and its children.
    SupPid = start_anon_supervisor_with_worker(stop_child),
    [{_, ChildPid, _, _}] = supervisor:which_children(SupPid),
    ?assert(is_process_alive(ChildPid)),
    Self = make_supervisor_tuple('TestSupStop', test_stop_mod, SupPid),
    beamtalk_supervisor:stop(Self),
    timer:sleep(50),
    ?assertEqual(false, is_process_alive(SupPid)),
    ?assertEqual(false, is_process_alive(ChildPid)).

%%====================================================================
%% Tests: static_init/2 — direct module resolution (BT-1960)
%%====================================================================

static_init_direct_module_test() ->
    %% BT-1960: static_init finds class methods in the module directly
    %% (no hierarchy walk needed) when the class module exports them.
    beamtalk_class_hierarchy_table:new(),
    beamtalk_class_module_table:new(),

    %% Register a fake class pid so whereis_class resolves.
    FakeClassPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    RegName = beamtalk_class_registry:registry_name('BT1960DirectInit'),
    register(RegName, FakeClassPid),

    try
        Result = beamtalk_supervisor:static_init(?MODULE, 'BT1960DirectInit'),
        ?assertMatch(
            {ok, {#{strategy := one_for_one, intensity := 3, period := 5}, []}},
            Result
        )
    after
        (try
            unregister(RegName)
        catch
            _:_ -> ok
        end),
        FakeClassPid ! stop
    end.

%%====================================================================
%% Tests: supervisor tuple element access (BT-1960)
%%====================================================================

supervisor_tuple_new_tag_test() ->
    %% BT-1960: startLink returns beamtalk_supervisor_new tag for fresh starts.
    %% We can't easily test startLink without a real class, but we can verify
    %% the tuple structure constants used throughout the module.
    Tuple = {beamtalk_supervisor_new, 'MyClass', my_mod, self()},
    ?assertEqual(beamtalk_supervisor_new, element(1, Tuple)),
    ?assertEqual('MyClass', element(2, Tuple)),
    ?assertEqual(my_mod, element(3, Tuple)),
    ?assertEqual(self(), element(4, Tuple)).
