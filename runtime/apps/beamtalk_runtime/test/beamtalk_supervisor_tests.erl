%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context
%%%
-module(beamtalk_supervisor_tests).

-moduledoc """
Unit tests for beamtalk_supervisor runtime helper.

Tests cover:
- is_supervisor/1 ancestry check via ETS hierarchy
- current/1 — running and not-running supervisor lookup
- whichChildren/1 live child objects in declaration order (BT-3966)
- childInfos/1, childWithId/2, terminateChildId/2, restartChild/2,
  deleteChild/2, dynamicChildren/1, SupervisorChild mapping, ambiguous
  which:/terminate: (BT-3966)
- whichChild/2 — found and not-found child lookup
- countChildren/1 active count, multiple children, dynamic supervisor
- stop/1 returns nil, kills supervisor process
- terminateChild/2 — dynamic path, stale handle, not_found error
- build_child_specs/1 with empty list
- supervisor tuple structure
- stale-handle error translation (noproc → beamtalk_error with stale_handle kind)
- root registry — nil, roundtrip, overwrite, clear_root
- static_init/3, dynamic_init/3 — instance-side configuration, start args,
  calling_self hint, validator errors (ADR 0135 §2)
- start_child_via_class_method/4: valid return, invalid return, read-only snapshot reads, write raises class_state_read_only
  unwrap, process dictionary cleanup, child linking, supervisor restart,
  supervisor tuple return
- start_child_via_class_method/4 supervisor restart through the REAL compiled
  `self spawn`/`self spawnWith:` and `self spawnAs:`/`self spawnWith:as:`
  call chains (not a gen_server:start_link stand-in)
- ensure_root_table idempotent creation
- startLink/1 success, already_started, and error paths
- terminateChild/2 class path + terminateChild:class:/:child: aliases
- terminateChild/2 Result-shaped returns: idempotent not_found → {ok, nil}
  on BOTH static and dynamic paths, {error, terminate_failed} for other
  failures, {error, stale_handle} for dead supervisor
- startChild/1 and /2 success + error paths
- run_initialize/1 sends instance-side initialize; a raise stops the supervisor
- to_otp_strategy/1 oneForAll, restForOne, pass-through
- wrap_child/3 supervisor and actor branches via whichChild
- build_child_specs/1 nested-supervisor OTP spec
- ensure_root_table concurrent creation safety
- stale_handle startChild/1 and /2 coverage
- startChild/1,2 returns {ok, ...} / {error, #beamtalk_error{}}
- with_live_supervisor/3 returns {error, ...} on stale handle
- class_dispatch hook: initialize runs only on fresh start, not already_started or error
- start_child_via_class_method/4 rejects an abstract class's `self new`,
  resolved by class name rather than a fabricated process-dictionary flag
- maxChildren atomic enforcement inside the supervisor: concurrent starters
  never overshoot, one start_child call per startChild, terminateChild frees
  the slot synchronously, failing children release theirs (BT-4068)
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% OTP supervisor callback and worker start function — required for supervisor:start_link/3.
-export([init/1, start_worker/1]).

%% Fake class methods for start_child_via_class_method tests.
%% Simulates what a compiled Beamtalk keyword class method does: start a gen_server
%% and return a {beamtalk_object, ...} tuple.
-export([
    'class_create:value:'/3,
    class_returnInvalid/1,
    class_returnSupervisor/1,
    class_writeVar/1,
    class_readVar/1,
    class_readThenActor/1,
    start_link_fake_child/0,
    start_link_fake_child/1,
    start_link_fake_child/2
]).

%% Fake class methods that route through
%% the REAL compiled `self spawn`/`self spawnWith:`/`self spawnAs:`/
%% `self spawnWith:as:` runtime entry points (beamtalk_class_instantiation:
%% class_self_spawn/3, class_self_spawn_as/4) instead of a `gen_server:start_link`
%% stand-in — see start_child_via_class_method_real_self_spawn_supervisor_restart_test/0
%% and start_child_via_class_method_real_self_spawn_as_supervisor_restart_test/0.
-export([
    'class_createViaSpawn:value:'/3,
    'class_createNamedViaSpawn:value:'/3
]).

%% Fake class method that self-sends `new`, mirroring the codegen
%% shape of `self new` inside a compiled class method body.
-export([class_selfNew/1]).

%% Fake class-method and helper exports for added coverage tests.
%% gen_server callbacks for the fake child actor used above.
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
%% BT-4068: a dynamic supervisor with full enforcement state, installed the
%% way `beamtalk_supervisor:dynamic_init/3` does — through the same
%% production helpers, not a reimplementation. This clause must precede the
%% plain-limit clause below, which would otherwise match first.
init({simple_one_for_one, ChildSpec, {capped, MaxChildren}}) ->
    put('$beamtalk_max_children', MaxChildren),
    Capacity = beamtalk_supervisor:init_child_capacity(MaxChildren),
    {CapTid, CapMon} = Capacity,
    put('$beamtalk_max_children_state', {CapTid, CapMon}),
    Wrapped = beamtalk_supervisor:wrap_capped_child_spec(ChildSpec, MaxChildren, Capacity),
    %% Restarts allowed (unlike the intensity-0 default below): the restart
    %% replay test needs them, and temporary children never restart anyway.
    SupFlags = #{strategy => simple_one_for_one, intensity => 5, period => 5},
    {ok, {SupFlags, [Wrapped]}};
%% BT-3964: a dynamic supervisor carrying a maxChildren limit, stashed the way
%% `beamtalk_supervisor:dynamic_init/2` does (dynamic_init_walks_hierarchy_via_ets_test
%% pins that the key matches).
init({simple_one_for_one, ChildSpec, MaxChildren}) ->
    put('$beamtalk_max_children', MaxChildren),
    init({simple_one_for_one, ChildSpec});
init({SupFlags, ChildSpecs}) ->
    put_pending_child_class(),
    {ok, {SupFlags, ChildSpecs}};
%% Fake child actor init — minimal gen_server for supervision tests.
init(fake_child) ->
    {ok, #{counter => 0}}.

%% gen_server callbacks for fake child actor.
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

%% BT-3966: an anonymous supervisor whose workers are declared as
%% `{Id, Restart, Modules}`. Returns `{SupPid, WorkerPids}`, the worker pids
%% in declaration order.
start_anon_supervisor_with_specs(Specs) ->
    Self = self(),
    ChildSpecs = [
        #{
            id => Id,
            start => {?MODULE, start_worker, [Self]},
            restart => Restart,
            shutdown => brutal_kill,
            type => worker,
            modules => Mods
        }
     || {Id, Restart, Mods} <- Specs
    ],
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, ChildSpecs}),
    %% Drain the readiness messages; their arrival order is NOT start order
    %% (each comes from a different worker process), so the pids are looked
    %% up by id.
    [
        receive
            {worker_ready, _} -> ok
        after 1000 -> error(worker_not_ready)
        end
     || _ <- Specs
    ],
    Children = supervisor:which_children(Pid),
    Workers = [element(2, lists:keyfind(Id, 1, Children)) || {Id, _, _} <- Specs],
    {Pid, Workers}.

%% Start a named OTP supervisor (registers under the given atom).
start_named_supervisor(Name) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link({local, Name}, ?MODULE, {SupFlags, []}),
    Pid.

%% Named supervisor with one running worker (BT-3720).
start_named_supervisor_with_worker(Name, ChildId) ->
    ChildSpec = #{
        id => ChildId,
        start => {?MODULE, start_worker, [self()]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    },
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link({local, Name}, ?MODULE, {SupFlags, [ChildSpec]}),
    Pid.

wait_for_worker() ->
    receive
        {worker_ready, WorkerPid} -> {ok, WorkerPid}
    after 1000 -> error(worker_not_ready)
    end.

make_supervisor_tuple(ClassName, Module, Pid) ->
    {beamtalk_supervisor, ClassName, Module, Pid}.

%%====================================================================
%% Tests: is_supervisor/1
%%====================================================================

is_supervisor_unknown_class_test() ->
    %% A class that does not exist in the registry returns false.
    ?assertEqual(false, beamtalk_supervisor:is_supervisor('NonExistentClass99')).

is_supervisor_returns_true_for_supervisor_subclass_test() ->
    %% A class registered as inheriting from 'Supervisor' returns true.
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert(
        'BT1960SupervisorChild', undefined, undefined, 'Supervisor', undefined
    ),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960SupervisorChild'))
    after
        beamtalk_class_metadata:delete('BT1960SupervisorChild')
    end.

is_supervisor_returns_true_for_dynamic_supervisor_subclass_test() ->
    %% A class inheriting from 'DynamicSupervisor' returns true.
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert(
        'BT1960DynChild', undefined, undefined, 'DynamicSupervisor', undefined
    ),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960DynChild'))
    after
        beamtalk_class_metadata:delete('BT1960DynChild')
    end.

is_supervisor_returns_false_for_non_supervisor_class_test() ->
    %% A class that inherits from 'Actor' (not Supervisor) returns false.
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert('BT1960Worker', undefined, undefined, 'Actor', undefined),
    beamtalk_class_metadata:insert('Actor', undefined, undefined, none, undefined),
    try
        ?assertEqual(false, beamtalk_supervisor:is_supervisor('BT1960Worker'))
    after
        beamtalk_class_metadata:delete('BT1960Worker'),
        beamtalk_class_metadata:delete('Actor')
    end.

%%====================================================================
%% Tests: current/1
%%====================================================================

current_returns_nil_when_not_running_test() ->
    %% current/1 returns nil when the supervisor is not started.
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
    %% current/1 returns a supervisor tuple when the process is registered.
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
    %% whichChildren/1 returns {ok, []} on empty supervisor.
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('TestSup', test_module, SupPid),
        ?assertEqual({ok, []}, beamtalk_supervisor:whichChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

which_children_running_child_test() ->
    %% BT-3966: whichChildren/1 returns the live children, not their ids. A
    %% child whose module is not a Beamtalk class comes back as its pid.
    {SupPid, [W]} = start_anon_supervisor_with_specs([{worker_id, temporary, []}]),
    try
        Self = make_supervisor_tuple('TestSup2', test_module2, SupPid),
        ?assertEqual({ok, [W]}, beamtalk_supervisor:whichChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

which_children_multiple_children_test() ->
    %% BT-3966: declaration order (OTP lists children newest first), and a
    %% terminated child is left out.
    {SupPid, [A, _B, C]} = start_anon_supervisor_with_specs([
        {child_a, permanent, []}, {child_b, permanent, []}, {child_c, permanent, []}
    ]),
    try
        Self = make_supervisor_tuple('TestSupMulti', test_mod_multi, SupPid),
        {ok, [A, B, C]} = beamtalk_supervisor:whichChildren(Self),
        ?assert(is_pid(B)),
        ok = supervisor:terminate_child(SupPid, child_b),
        ?assertEqual({ok, [A, C]}, beamtalk_supervisor:whichChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: BT-3966 child introspection and control by id
%%====================================================================

supervisor_child_entry_running_test() ->
    Pid = self(),
    ?assertEqual(
        #{
            '$beamtalk_class' => 'SupervisorChild',
            id => a1,
            child => Pid,
            status => running,
            type => worker,
            childClass => nil
        },
        beamtalk_supervisor:supervisor_child_entry({a1, Pid, worker, [no_such_mod]}, #{})
    ).

supervisor_child_entry_restarting_test() ->
    %% ADR 0092 §3 precedent: a child mid-restart never raises; child is nil.
    ?assertMatch(
        #{id := a2, child := nil, status := restarting, type := supervisor},
        beamtalk_supervisor:supervisor_child_entry({a2, restarting, supervisor, [m]}, #{})
    ).

supervisor_child_entry_undefined_is_stopped_test() ->
    ?assertMatch(
        #{id := 3, child := nil, status := stopped, childClass := nil},
        beamtalk_supervisor:supervisor_child_entry({3, undefined, worker, []}, #{})
    ).

supervisor_child_entries_reverses_to_declaration_order_test() ->
    Entries = beamtalk_supervisor:supervisor_child_entries([
        {c, undefined, worker, []}, {b, restarting, worker, []}, {a, undefined, worker, []}
    ]),
    ?assertEqual([a, b, c], [Id || #{id := Id} <- Entries]).

child_infos_lists_every_spec_test() ->
    {SupPid, [A, _B]} = start_anon_supervisor_with_specs([
        {a1, permanent, []}, {a2, permanent, []}
    ]),
    try
        Self = make_supervisor_tuple('BT3966Infos', bt3966_infos_mod, SupPid),
        ok = supervisor:terminate_child(SupPid, a2),
        {ok, [I1, I2]} = beamtalk_supervisor:childInfos(Self),
        ?assertMatch(#{id := a1, status := running}, I1),
        ?assertEqual(A, maps:get(child, I1)),
        ?assertMatch(#{id := a2, status := stopped, child := nil}, I2)
    after
        gen_server:stop(SupPid)
    end.

child_with_id_test() ->
    {SupPid, [A, _B]} = start_anon_supervisor_with_specs([
        {a1, permanent, []}, {a2, permanent, []}
    ]),
    try
        Self = make_supervisor_tuple('BT3966WithId', bt3966_with_id_mod, SupPid),
        ?assertEqual({ok, A}, beamtalk_supervisor:childWithId(Self, a1)),
        ok = supervisor:terminate_child(SupPid, a2),
        ?assertEqual({ok, nil}, beamtalk_supervisor:childWithId(Self, a2)),
        {error, E} = beamtalk_supervisor:childWithId(Self, nope),
        ?assertEqual(child_not_found, E#beamtalk_error.kind),
        ?assertEqual('childWithId:', E#beamtalk_error.selector),
        ?assertEqual(#{id => nope}, E#beamtalk_error.details)
    after
        gen_server:stop(SupPid)
    end.

child_id_type_error_test() ->
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('BT3966TypeErr', bt3966_type_err_mod, SupPid),
        lists:foreach(
            fun(F) ->
                ?assertMatch({error, #beamtalk_error{kind = type_error}}, F(<<"a1">>))
            end,
            [
                fun(Id) -> beamtalk_supervisor:childWithId(Self, Id) end,
                fun(Id) -> beamtalk_supervisor:terminateChildId(Self, Id) end,
                fun(Id) -> beamtalk_supervisor:restartChild(Self, Id) end,
                fun(Id) -> beamtalk_supervisor:deleteChild(Self, Id) end
            ]
        )
    after
        gen_server:stop(SupPid)
    end.

terminate_restart_delete_by_id_test() ->
    {SupPid, [_A, B]} = start_anon_supervisor_with_specs([
        {a1, permanent, []}, {a2, permanent, []}
    ]),
    try
        Self = make_supervisor_tuple('BT3966Ctl', bt3966_ctl_mod, SupPid),
        %% running -> child_running for restart and delete
        {error, R0} = beamtalk_supervisor:restartChild(Self, a2),
        ?assertEqual(child_running, R0#beamtalk_error.kind),
        ?assertEqual(#{id => a2, status => running}, R0#beamtalk_error.details),
        ?assertMatch(
            {error, #beamtalk_error{kind = child_running}},
            beamtalk_supervisor:deleteChild(Self, a2)
        ),
        %% terminate, idempotently
        ?assertEqual({ok, nil}, beamtalk_supervisor:terminateChildId(Self, a2)),
        ?assertEqual(false, is_process_alive(B)),
        ?assertEqual({ok, nil}, beamtalk_supervisor:terminateChildId(Self, a2)),
        ?assertEqual({ok, nil}, beamtalk_supervisor:terminateChildId(Self, nope)),
        %% restart gives a new child
        {ok, B2} = beamtalk_supervisor:restartChild(Self, a2),
        ?assert(is_pid(B2)),
        ?assertNotEqual(B, B2),
        receive
            {worker_ready, B2} -> ok
        after 1000 -> error(worker_not_ready)
        end,
        %% delete after terminate
        {ok, nil} = beamtalk_supervisor:terminateChildId(Self, a2),
        ?assertEqual({ok, nil}, beamtalk_supervisor:deleteChild(Self, a2)),
        ?assertMatch(
            {error, #beamtalk_error{kind = child_not_found, details = #{id := a2}}},
            beamtalk_supervisor:restartChild(Self, a2)
        ),
        ?assertMatch(
            {error, #beamtalk_error{kind = child_not_found}},
            beamtalk_supervisor:deleteChild(Self, a2)
        )
    after
        gen_server:stop(SupPid)
    end.

by_id_stale_handle_test() ->
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    Self = make_supervisor_tuple('BT3966Stale', bt3966_stale_mod, SupPid),
    lists:foreach(
        fun(R) -> ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, R) end,
        [
            beamtalk_supervisor:childInfos(Self),
            beamtalk_supervisor:whichChildren(Self),
            beamtalk_supervisor:childWithId(Self, a1),
            beamtalk_supervisor:terminateChildId(Self, a1),
            beamtalk_supervisor:restartChild(Self, a1),
            beamtalk_supervisor:deleteChild(Self, a1),
            beamtalk_supervisor:dynamicChildren(Self)
        ]
    ).

%% BT-3984: a handle to a supervisor on a node this VM cannot reach. Every
%% handle call returns `node_down` (not `stale_handle`, and not a raw
%% `{nodedown, _}` exit into the caller) through `with_live_supervisor/3`;
%% `args` (an `erpc` read of the supervisor's dictionary) too.
unreachable_node_handle_calls_are_node_down_test() ->
    Pid = unreachable_remote_pid(),
    Self = make_supervisor_tuple('BT3984RemoteSup', bt3984_remote_mod, Pid),
    ClassArg = {beamtalk_object, 'X class', x_mod, self()},
    Child = {beamtalk_object, 'X', x_mod, self()},
    Calls = [
        {children, fun() -> beamtalk_supervisor:whichChildren(Self) end},
        {whichChildren, fun() -> beamtalk_supervisor:childInfos(Self) end},
        {'childWithId:', fun() -> beamtalk_supervisor:childWithId(Self, a1) end},
        {'terminateChild:', fun() -> beamtalk_supervisor:terminateChildId(Self, a1) end},
        {'restartChild:', fun() -> beamtalk_supervisor:restartChild(Self, a1) end},
        {'deleteChild:', fun() -> beamtalk_supervisor:deleteChild(Self, a1) end},
        {children, fun() -> beamtalk_supervisor:dynamicChildren(Self) end},
        {'which:', fun() -> beamtalk_supervisor:whichChild(Self, ClassArg) end},
        {'terminateChild:', fun() -> beamtalk_supervisor:terminateChild(Self, ClassArg) end},
        {'terminateChild:', fun() -> beamtalk_supervisor:terminateChild(Self, Child) end},
        {startChild, fun() -> beamtalk_supervisor:startChild(Self) end},
        {'startChild:', fun() -> beamtalk_supervisor:startChild(Self, #{}) end},
        {'startChild:name:', fun() -> beamtalk_supervisor:startChild(Self, #{}, bt3984_n) end},
        {count, fun() -> beamtalk_supervisor:countChildren(Self) end},
        {stop, fun() -> beamtalk_supervisor:stop(Self) end},
        {args, fun() -> beamtalk_supervisor:args(Self) end}
    ],
    lists:foreach(
        fun({Selector, Call}) ->
            Result = Call(),
            ?assertMatch(
                {error, #beamtalk_error{
                    kind = node_down,
                    class = 'BT3984RemoteSup',
                    selector = Selector,
                    details = #{node := 'bt3984_ghost@nohost', reason := _}
                }},
                Result,
                Selector
            )
        end,
        Calls
    ).

unreachable_node_is_alive_is_optimistic_test() ->
    %% As for a remote actor (`beamtalk_pid:is_alive/1`): no network round
    %% trip, and no `badarg` from `erlang:is_process_alive/1` on a remote pid.
    Self = make_supervisor_tuple('BT3984RemoteSup', bt3984_remote_mod, unreachable_remote_pid()),
    ?assert(beamtalk_supervisor:isAlive(Self)).

classify_call_failure_test() ->
    %% The exit/error shapes a supervisor call can fail with. A timeout (an
    %% `erpc` read of a slow node's supervisor) is unreachable, never `dead`.
    MFA = {gen_server, call, [self(), which_children, infinity]},
    lists:foreach(
        fun({Class, Reason, Expected}) ->
            ?assertEqual(
                Expected, beamtalk_supervisor:classify_call_failure(Class, Reason), Reason
            )
        end,
        [
            {exit, noproc, dead},
            {exit, {noproc, MFA}, dead},
            {exit, {{nodedown, n@h}, MFA}, {unreachable, nodedown}},
            {exit, {nodedown, n@h}, {unreachable, nodedown}},
            {exit, noconnection, {unreachable, noconnection}},
            {exit, {noconnection, MFA}, {unreachable, noconnection}},
            {exit, timeout, {unreachable, timeout}},
            {exit, {timeout, MFA}, {unreachable, timeout}},
            {error, {erpc, noconnection}, {unreachable, noconnection}},
            {error, {erpc, timeout}, {unreachable, timeout}},
            {error, badarg, other},
            {exit, {shutdown, MFA}, other},
            {throw, noproc, other}
        ]
    ).

local_supervisor_timeout_is_not_node_down_test() ->
    %% A timeout under a local supervisor's handle came from some other call
    %% (the supervisor's node is this one), so it propagates unmapped.
    Self = make_supervisor_tuple('BT3984LocalSup', bt3984_local_mod, self()),
    Timeout = {timeout, {gen_server, call, [self(), class_name]}},
    ?assertExit(
        Timeout,
        beamtalk_supervisor:with_live_supervisor(Self, count, fun() -> exit(Timeout) end)
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = stale_handle}},
        beamtalk_supervisor:with_live_supervisor(Self, count, fun() ->
            exit({noproc, {gen_server, call, []}})
        end)
    ).

remote_supervisor_failure_of_another_call_propagates_test() ->
    %% Under a remote handle, a failure whose OTP call names another process
    %% (a class process lookup, say) is not blamed on the supervisor.
    Self = make_supervisor_tuple('BT3984RemoteSup', bt3984_remote_mod, unreachable_remote_pid()),
    lists:foreach(
        fun(Reason) ->
            ?assertExit(
                Reason,
                beamtalk_supervisor:with_live_supervisor(Self, count, fun() -> exit(Reason) end)
            )
        end,
        [
            {timeout, {gen_server, call, [self(), class_name]}},
            {noproc, {gen_server, call, [self(), class_name]}}
        ]
    ),
    %% The same shape naming the supervisor is mapped.
    SupTimeout = {timeout, {gen_server, call, [element(4, Self), which_children]}},
    ?assertMatch(
        {error, #beamtalk_error{kind = node_down, details = #{reason := timeout}}},
        beamtalk_supervisor:with_live_supervisor(Self, count, fun() -> exit(SupTimeout) end)
    ).

args_remote_erpc_timeout_is_node_down_test() ->
    %% The reported bug: an `erpc` timeout reading a slow remote supervisor's
    %% args was `stale_handle`.
    meck:new(erpc, [unstick, passthrough]),
    try
        meck:expect(erpc, call, fun(_Node, erlang, process_info, _Args, _Timeout) ->
            error({erpc, timeout})
        end),
        Self = make_supervisor_tuple(
            'BT3984RemoteSup', bt3984_remote_mod, unreachable_remote_pid()
        ),
        ?assertMatch(
            {error, #beamtalk_error{
                kind = node_down, selector = args, details = #{reason := timeout}
            }},
            beamtalk_supervisor:args(Self)
        )
    after
        meck:unload(erpc)
    end.

%% A pid on a node this VM is not connected to (`NEW_PID_EXT`).
unreachable_remote_pid() ->
    NodeBin = atom_to_binary('bt3984_ghost@nohost', utf8),
    binary_to_term(<<131, 88, 118, (byte_size(NodeBin)):16, NodeBin/binary, 1:32, 0:32, 0:32>>).

which_and_terminate_by_class_ambiguous_test() ->
    %% Several children share the class's module: which:/terminate: refuse.
    {SupPid, _} = start_anon_supervisor_with_specs([
        {a1, permanent, [bt3966_amb_mod]}, {a2, permanent, [bt3966_amb_mod]}
    ]),
    try
        Self = make_supervisor_tuple('BT3966Amb', bt3966_amb_sup_mod, SupPid),
        ClassArg = {beamtalk_object, 'Amb class', bt3966_amb_mod, self()},
        {error, E} = beamtalk_supervisor:whichChild(Self, ClassArg),
        ?assertEqual(ambiguous_child, E#beamtalk_error.kind),
        ?assertEqual(#{ids => [a1, a2]}, E#beamtalk_error.details),
        ?assertMatch(
            {error, #beamtalk_error{kind = ambiguous_child, selector = 'terminate:'}},
            beamtalk_supervisor:terminateChild(Self, ClassArg)
        ),
        ?assertEqual({ok, 2}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

declaration_order_survives_one_for_all_restart_test() ->
    %% The reversal in supervisor_child_entries/1 relies on OTP keeping its
    %% newest-first child list; a one_for_all / rest_for_one restart rebuilds
    %% that list, so pin that the order still comes out as declared.
    Self = self(),
    lists:foreach(
        fun(Strategy) ->
            ChildSpecs = [
                #{
                    id => Id,
                    start => {?MODULE, start_worker, [Self]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => []
                }
             || Id <- [o1, o2, o3]
            ],
            SupFlags = #{strategy => Strategy, intensity => 5, period => 5},
            {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, ChildSpecs}),
            [
                receive
                    {worker_ready, _} -> ok
                after 1000 -> error(worker_not_ready)
                end
             || _ <- ChildSpecs
            ],
            {o2, W2, _, _} = lists:keyfind(o2, 1, supervisor:which_children(SupPid)),
            try
                Sup = make_supervisor_tuple('BT3966Order', bt3966_order_mod, SupPid),
                exit(W2, kill),
                %% one_for_all restarts all three, rest_for_one o2 and o3.
                NRestarted =
                    case Strategy of
                        one_for_all -> 3;
                        rest_for_one -> 2
                    end,
                [
                    receive
                        {worker_ready, _} -> ok
                    after 1000 -> error(worker_not_restarted)
                    end
                 || _ <- lists:seq(1, NRestarted)
                ],
                {ok, Infos} = beamtalk_supervisor:childInfos(Sup),
                ?assertEqual([o1, o2, o3], [Id || #{id := Id} <- Infos]),
                {ok, Kids} = beamtalk_supervisor:whichChildren(Sup),
                ?assertEqual(
                    [P || {_, P, _, _} <- lists:reverse(supervisor:which_children(SupPid))],
                    Kids
                )
            after
                unlink(SupPid),
                gen_server:stop(SupPid)
            end
        end,
        [one_for_all, rest_for_one]
    ).

dynamic_terminate_child_non_child_arg_is_type_error_test() ->
    %% A Symbol sent to DynamicSupervisor>>terminateChild: used to crash with
    %% badarg in element/2; it is a type_error Result now.
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('BT3966DynTerm', bt3966_dyn_term_mod, SupPid),
        ?assertMatch(
            {error, #beamtalk_error{kind = type_error}},
            beamtalk_supervisor:terminateChild(Self, a1)
        )
    after
        gen_server:stop(SupPid)
    end.

dynamic_children_empty_test() ->
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('BT3966Dyn', bt3966_dyn_mod, SupPid),
        %% Empty: no child class lookup needed.
        ?assertEqual({ok, []}, beamtalk_supervisor:dynamicChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: whichChild/2
%%====================================================================

which_child_returns_nil_when_not_found_test() ->
    %% whichChild/2 returns {ok, nil} when no child matches.
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
        ?assertEqual({ok, nil}, Result),
        FakeClassPid ! stop
    after
        gen_server:stop(SupPid)
    end.

which_child_stale_handle_test() ->
    %% whichChild/2 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}}.
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
        Result = beamtalk_supervisor:whichChild(Self, ClassArg),
        ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result)
    after
        FakeClassPid ! stop
    end.

%%====================================================================
%% Tests: countChildren/1
%%====================================================================

count_children_empty_test() ->
    %% countChildren/1 returns {ok, Count}.
    SupPid = start_anon_supervisor(),
    try
        Self = make_supervisor_tuple('TestSup3', test_module3, SupPid),
        ?assertEqual({ok, 0}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_with_child_test() ->
    %% countChildren/1 returns {ok, 1} with a running child.
    SupPid = start_anon_supervisor_with_worker(count_child_id),
    try
        Self = make_supervisor_tuple('TestSup4', test_module4, SupPid),
        ?assertEqual({ok, 1}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_multiple_test() ->
    %% countChildren returns {ok, N} with multiple children.
    SupPid = start_anon_supervisor_with_workers([count_a, count_b]),
    try
        Self = make_supervisor_tuple('TestSupCount2', test_mod_cnt2, SupPid),
        ?assertEqual({ok, 2}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_dynamic_supervisor_test() ->
    %% dynamic supervisor with no children → {ok, 0}.
    SupPid = start_dynamic_supervisor(),
    try
        Self = make_supervisor_tuple('TestDynCount', test_dyn_cnt, SupPid),
        ?assertEqual({ok, 0}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

count_children_dynamic_supervisor_with_children_test() ->
    %% dynamic supervisor count after adding children.
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
        ?assertEqual({ok, 2}, beamtalk_supervisor:countChildren(Self))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: stop/1
%%====================================================================

stop_returns_ok_nil_test() ->
    %% stop/1 returns {ok, nil} on a live supervisor.
    SupPid = start_anon_supervisor(),
    Self = make_supervisor_tuple('TestSup5', test_module5, SupPid),
    Result = beamtalk_supervisor:stop(Self),
    ?assertEqual({ok, nil}, Result),
    %% Give the process a moment to shut down
    timer:sleep(50),
    ?assertEqual(false, is_process_alive(SupPid)).

%%====================================================================
%% Tests: terminateChild/2 — dynamic supervisor path (by pid)
%%====================================================================

terminate_child_by_pid_test() ->
    %% terminateChild/2 returns {ok, nil} on success (dynamic path).
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
        ?assertEqual({ok, nil}, Result),
        timer:sleep(50),
        ?assertEqual(false, is_process_alive(ChildPid))
    after
        gen_server:stop(SupPid)
    end.

terminate_child_not_found_dynamic_test() ->
    %% ADR 0080 Phase 1: terminateChild/2 on a pid not in the
    %% supervisor returns {ok, nil} (idempotent — "child is already gone"
    %% is the goal state, so this is success).
    SupPid = start_dynamic_supervisor(),
    FakePid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        Self = make_supervisor_tuple('TestSup7', test_module7, SupPid),
        ChildArg = {beamtalk_object, 'SomeActor', some_mod, FakePid},
        ?assertEqual(
            {ok, nil},
            beamtalk_supervisor:terminateChild(Self, ChildArg)
        )
    after
        FakePid ! stop,
        gen_server:stop(SupPid)
    end.

terminate_child_stale_handle_test() ->
    %% terminateChild/2 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle, ...}}.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup4', stale_mod4, SupPid),
    ChildArg = {beamtalk_object, 'SomeActor', some_mod, self()},
    ?assertMatch(
        {error, #beamtalk_error{kind = stale_handle}},
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
    %% whichChildren/1 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}}.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup', stale_mod, SupPid),
    Result = beamtalk_supervisor:whichChildren(Self),
    ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result),
    {error, BtError} = Result,
    ?assertEqual('StaleSup', BtError#beamtalk_error.class),
    ?assertEqual(children, BtError#beamtalk_error.selector).

stale_handle_countChildren_test() ->
    %% countChildren/1 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}}.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup2', stale_mod2, SupPid),
    Result = beamtalk_supervisor:countChildren(Self),
    ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result),
    {error, BtError} = Result,
    ?assertEqual(count, BtError#beamtalk_error.selector).

stale_handle_stop_test() ->
    %% stop/1 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}}. gen_server:stop/1 raises
    %% `exit:noproc` (bare atom) rather than `exit:{noproc, MFA}`, so this
    %% also exercises the second clause of with_live_supervisor/3.
    SupPid = start_anon_supervisor(),
    gen_server:stop(SupPid),
    timer:sleep(20),
    Self = make_supervisor_tuple('StaleSup3', stale_mod3, SupPid),
    Result = beamtalk_supervisor:stop(Self),
    ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result),
    {error, BtError} = Result,
    ?assertEqual(stop, BtError#beamtalk_error.selector).

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
    %% clear_root/0 removes the root entry so get_root returns nil.
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
    %% clear_root/0 does not crash when called with no entry.
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
    %% Multiple calls to get_root (which calls ensure_root_table)
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
%% Tests: instance-side configuration read in init/1 (ADR 0135 §2, BT-3963)
%%====================================================================

%% Register a real class process for one of the fixture's class names, with
%% the fixture as its module, answering the instance methods in `Config`.
with_bt3963_class(ClassName, Config, Fun) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_supervisor_test_helper:ensure_lock_table(),
    beamtalk_class_metadata:new(),
    ok = beamtalk_bt3963_sup_fixture:configure(Config),
    {ok, ClassPid} = beamtalk_object_class:start_link(ClassName, #{
        superclass => none, module => beamtalk_bt3963_sup_fixture
    }),
    unlink(ClassPid),
    try
        Fun()
    after
        gen_server:stop(ClassPid, normal, 5000),
        beamtalk_bt3963_sup_fixture:unconfigure()
    end.

bt3963_sup(ClassName, Pid) ->
    {beamtalk_supervisor, ClassName, beamtalk_bt3963_sup_fixture, Pid}.

stop_linked(Pid) ->
    unlink(Pid),
    gen_server:stop(Pid).

%% Start the fixture expecting `init/1` to fail; return the exit reason.
start_failing(Args) ->
    Trapping = process_flag(trap_exit, true),
    try
        {error, Reason} = beamtalk_bt3963_sup_fixture:start_link(Args),
        receive
            {'EXIT', _, _} -> ok
        after 100 -> ok
        end,
        Reason
    after
        process_flag(trap_exit, Trapping)
    end.

bt3963_static_config(Overrides) ->
    maps:merge(
        #{
            kind => static,
            childSpecs => [],
            strategy => oneForOne,
            maxRestarts => 10,
            restartWindow => 60,
            %% The stdlib default (a real class inherits it): a top-level
            %% start sends `initialize` (ADR 0135 §1).
            initialize => nil
        },
        Overrides
    ).

static_init_reads_config_from_instance_with_start_args_test() ->
    Test = self(),
    Config = bt3963_static_config(#{
        childSpecs => fun(Self) ->
            Test ! {configured, Self, beamtalk_supervisor:args(Self)},
            []
        end,
        strategy => oneForAll,
        maxRestarts => 4,
        restartWindow => 9
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        {ok, Pid} = beamtalk_bt3963_sup_fixture:start_link(#{port => 80}),
        try
            Sup = bt3963_sup('BT3963Sup', Pid),
            %% `self` in a configuration method is the supervisor being
            %% started, and `self args` its start args.
            receive
                {configured, Self, ArgsInInit} ->
                    ?assertEqual(Sup, Self),
                    ?assertEqual({ok, #{port => 80}}, ArgsInInit)
            after 2000 -> error(not_configured)
            end,
            ?assertEqual({ok, #{port => 80}}, beamtalk_supervisor:args(Sup)),
            ?assertEqual(
                {ok, #{strategy => oneForAll, maxRestarts => 4, restartWindow => 9}},
                beamtalk_supervisor:configured_flags(Pid)
            ),
            ?assertEqual(
                {oneForAll, #{maxRestarts => 4, window => 9}},
                beamtalk_process_navigation:supervisor_config(beamtalkSupervisor, Pid)
            )
        after
            stop_linked(Pid)
        end
    end).

static_init_args_nil_for_registered_start_and_stale_after_stop_test() ->
    with_bt3963_class('BT3963Sup', bt3963_static_config(#{}), fun() ->
        %% start_link/0 is what `supervise` runs: registered, args nil.
        {ok, Pid} = beamtalk_bt3963_sup_fixture:start_link(),
        Sup = bt3963_sup('BT3963Sup', Pid),
        ?assertEqual(Pid, whereis(beamtalk_bt3963_sup_fixture)),
        ?assertEqual({ok, nil}, beamtalk_supervisor:args(Sup)),
        stop_linked(Pid),
        ?assertMatch(
            {error, #beamtalk_error{kind = stale_handle, selector = args}},
            beamtalk_supervisor:args(Sup)
        )
    end).

static_init_old_two_argument_form_passes_nil_args_test() ->
    %% ADR 0125: modules compiled before ADR 0135 call static_init/2.
    with_bt3963_class('BT3963Sup', bt3963_static_config(#{}), fun() ->
        ?assertMatch(
            {ok, {#{strategy := one_for_one, intensity := 10, period := 60}, []}},
            beamtalk_supervisor:static_init(beamtalk_bt3963_sup_fixture, 'BT3963Sup')
        ),
        ?assertEqual(nil, erase('$beamtalk_sup_args')),
        erase('$beamtalk_sup_flags')
    end).

static_init_config_messaging_self_fails_with_hint_test() ->
    %% A configuration method that sends to its own supervisor gets OTP's
    %% calling_self exit; the start fails with the ADR 0135 hint.
    Config = bt3963_static_config(#{
        childSpecs => fun(Self) ->
            {ok, Count} = beamtalk_supervisor:countChildren(Self),
            [Count]
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        Reason = start_failing(nil),
        ?assertMatch(
            #beamtalk_error{kind = supervisor_start_failed, selector = childSpecs}, Reason
        ),
        #beamtalk_error{hint = Hint} = Reason,
        ?assertNotEqual(
            nomatch, binary:match(element(2, Hint), <<"do not send messages to `self`">>)
        )
    end).

static_init_config_raise_fails_start_with_structured_error_test() ->
    Config = bt3963_static_config(#{strategy => fun(_Self) -> error(badarg) end}),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        ?assertMatch(#beamtalk_error{}, start_failing(nil))
    end).

static_init_invalid_strategy_and_intensity_fail_with_type_error_test() ->
    lists:foreach(
        fun({Overrides, Selector}) ->
            with_bt3963_class('BT3963Sup', bt3963_static_config(Overrides), fun() ->
                ?assertMatch(
                    #beamtalk_error{kind = type_error, selector = Selector}, start_failing(nil)
                )
            end)
        end,
        [
            {#{strategy => oneForSome}, strategy},
            {#{maxRestarts => -1}, maxRestarts},
            {#{restartWindow => 0}, restartWindow}
        ]
    ).

start_failed_error_uses_cause_hint_message_or_kind_test() ->
    WithHint = beamtalk_error:new(type_error, 'X', maxChildren, <<"the hint">>),
    NoHint = (beamtalk_error:new(does_not_understand, 'X', childSpecs))#beamtalk_error{
        message = {beamtalk_string, <<"the message">>}, hint = undefined
    },
    Bare = (beamtalk_error:new(boom, 'X', childSpecs))#beamtalk_error{
        message = {beamtalk_string, <<>>}, hint = undefined
    },
    Hint = fun(Reason) ->
        (beamtalk_supervisor:start_failed_error(supervisor_start_failed, 'Sup', supervise, Reason))#beamtalk_error.hint
    end,
    ?assertEqual({beamtalk_string, <<"the hint">>}, Hint(WithHint)),
    ?assertEqual({beamtalk_string, <<"the message">>}, Hint(NoHint)),
    ?assertEqual({beamtalk_string, <<"boom">>}, Hint(Bare)),
    %% A static parent reports a nested supervisor's init failure wrapped by
    %% OTP; the nested error's hint still reaches the caller.
    ?assertEqual(
        {beamtalk_string, <<"the hint">>},
        Hint({shutdown, {failed_to_start_child, 'Inner', WithHint}})
    ),
    %% ... at any depth (Root -> MidSup -> Pool).
    ?assertEqual(
        {beamtalk_string, <<"the hint">>},
        Hint(
            {shutdown,
                {failed_to_start_child, 'Mid',
                    {shutdown, {failed_to_start_child, 'Pool', WithHint}}}}
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(element(2, Hint({shutdown, {failed_to_start_child, 'Mid', boom}})), <<"boom">>)
    ),
    ?assertMatch(
        #beamtalk_error{details = #{cause := WithHint}},
        beamtalk_supervisor:start_failed_error(supervisor_start_failed, 'Sup', supervise, WithHint)
    ).

static_init_two_argument_form_rejects_pre_adr_0135_module_test() ->
    %% A module without dispatch/4 (compiled before ADR 0135) fails with a
    %% "recompile" hint instead of an inherited subclassResponsibility.
    Reason =
        try beamtalk_supervisor:static_init(lists, 'BT3963Old') of
            Other -> {no_exit, Other}
        catch
            exit:R -> R
        end,
    ?assertMatch(#beamtalk_error{kind = supervisor_start_failed}, Reason),
    #beamtalk_error{hint = Hint} = Reason,
    ?assertNotEqual(nomatch, binary:match(element(2, Hint), <<"recompile">>)).

static_init_config_class_var_write_raises_read_only_test() ->
    %% ADR 0130 §5: configuration still runs in a read-only snapshot region of
    %% the supervisor's class; a class-variable write fails the start.
    Config = bt3963_static_config(#{
        childSpecs => fun(_Self) ->
            ClassSelf =
                {beamtalk_object, 'BT3963Sup class', beamtalk_bt3963_sup_fixture,
                    beamtalk_class_registry:whereis_class('BT3963Sup')},
            beamtalk_class_vars:put(ClassSelf, n, 1)
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        ?assertMatch(#beamtalk_error{kind = class_state_read_only}, start_failing(nil))
    end).

%% A fake class object for a supervisor child class.
bt3963_supervisor_child_class(Name) ->
    fake_supervisor_class(Name, bt3963_child_mod).

%% A fake Supervisor-subclass class process (ADR 0135 §3). A bare class in a
%% child list goes through `Class supervisionSpec` -> `childSpec`: the fake
%% answers `supervisionSpec` with its own class object and `childSpec` with
%% the default Beamtalk child-spec dict `SupervisionSpec childSpec` builds for
%% a supervisor (`#spawn`, no args, `#permanent`, shutdown `#infinity`).
fake_supervisor_class(Name, Module) ->
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert(Name, undefined, undefined, 'Supervisor', undefined),
    Tag = list_to_atom(atom_to_list(Name) ++ " class"),
    Parent = self(),
    Ref = make_ref(),
    ClassPid = spawn(fun() ->
        ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = self()},
        Parent ! {Ref, ClassObj},
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, Name),
                    Loop();
                {'$gen_call', From, {class_method_call, supervisionSpec, [], _}} ->
                    gen_server:reply(From, {ok, ClassObj}),
                    Loop();
                {'$gen_call', From, {class_method_call, childSpec, [], _}} ->
                    gen_server:reply(
                        From,
                        {ok, #{
                            id => Name,
                            start => #{
                                '$beamtalk_class' => 'Array',
                                data => #{0 => ClassObj, 1 => spawn, 2 => []}
                            },
                            restart => permanent,
                            shutdown => infinity,
                            type => supervisor
                        }}
                    ),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    receive
        {Ref, ClassObj} -> {ClassPid, ClassObj}
    after 5000 -> error(fake_supervisor_class_timeout)
    end.

bt3963_dynamic_config(ChildClass, Overrides) ->
    maps:merge(
        #{
            kind => dynamic,
            childClass => ChildClass,
            maxRestarts => 3,
            restartWindow => 5,
            maxChildren => infinity,
            childShutdown => nil
        },
        Overrides
    ).

dynamic_init_reads_per_instance_config_test() ->
    {ChildPid, ChildClass} = bt3963_supervisor_child_class('BT3963DynChildSup'),
    Config = bt3963_dynamic_config(ChildClass, #{
        %% Per-instance values, computed from `self args`.
        maxChildren => fun(Self) ->
            {ok, #{max := Max}} = beamtalk_supervisor:args(Self),
            Max
        end,
        childShutdown => fun(Self) ->
            {ok, #{shutdown := Shutdown}} = beamtalk_supervisor:args(Self),
            Shutdown
        end
    }),
    try
        with_bt3963_class('BT3963Pool', Config, fun() ->
            {ok, P1} = beamtalk_bt3963_sup_fixture:start_link(#{max => 7, shutdown => 1234}),
            {ok, P2} = beamtalk_bt3963_sup_fixture:start_link(#{max => 2, shutdown => infinity}),
            try
                ?assertEqual(
                    {{dictionary, '$beamtalk_max_children'}, 7},
                    erlang:process_info(P1, {dictionary, '$beamtalk_max_children'})
                ),
                ?assertEqual(
                    {{dictionary, '$beamtalk_max_children'}, 2},
                    erlang:process_info(P2, {dictionary, '$beamtalk_max_children'})
                ),
                ?assertEqual(
                    {
                        {dictionary, '$beamtalk_child_class'},
                        {'BT3963DynChildSup', bt3963_child_mod}
                    },
                    erlang:process_info(P1, {dictionary, '$beamtalk_child_class'})
                ),
                ?assertEqual(
                    {ok, #{strategy => simpleOneForOne, maxRestarts => 3, restartWindow => 5}},
                    beamtalk_supervisor:configured_flags(P1)
                ),
                %% childShutdown replaces the template's shutdown, per instance.
                {ok, Spec1} = supervisor:get_childspec(P1, 'BT3963DynChildSup'),
                ?assertEqual(1234, maps:get(shutdown, Spec1)),
                {ok, Spec2} = supervisor:get_childspec(P2, 'BT3963DynChildSup'),
                ?assertEqual(infinity, maps:get(shutdown, Spec2))
            after
                stop_linked(P1),
                stop_linked(P2)
            end
        end)
    after
        ChildPid ! stop,
        beamtalk_class_metadata:delete('BT3963DynChildSup')
    end.

dynamic_init_invalid_values_fail_start_with_validator_error_test() ->
    {ChildPid, ChildClass} = bt3963_supervisor_child_class('BT3963DynChildSup2'),
    try
        lists:foreach(
            fun({Overrides, Selector}) ->
                with_bt3963_class(
                    'BT3963Pool', bt3963_dynamic_config(ChildClass, Overrides), fun() ->
                        ?assertMatch(
                            #beamtalk_error{kind = type_error, selector = Selector},
                            start_failing(nil)
                        )
                    end
                )
            end,
            [
                {#{maxChildren => -1}, maxChildren},
                {#{maxChildren => lots}, maxChildren},
                {#{childShutdown => soon}, childShutdown},
                {#{childClass => 42}, childClass}
            ]
        )
    after
        ChildPid ! stop,
        beamtalk_class_metadata:delete('BT3963DynChildSup2')
    end.

%%====================================================================
%% start_child_via_class_method tests
%%====================================================================

-doc "Start a fake child gen_server — OTP-compatible {ok, Pid} return.".
start_link_fake_child() ->
    gen_server:start_link(?MODULE, fake_child, []).

%% BT-4068: arities the wrapped template lands on for `startChild: args`
%% (`[Args]`) and `startChild:name:` (`[Args, Name]`). The received args are
%% recorded for the arg-threading test, then the child starts as usual.
start_link_fake_child(Args) ->
    record_fake_child_args([Args]),
    start_link_fake_child().

start_link_fake_child(Args, Name) ->
    record_fake_child_args([Args, Name]),
    start_link_fake_child().

record_fake_child_args(Args) ->
    case ets:info(bt4068_fake_child_args, id) of
        undefined ->
            ets:new(bt4068_fake_child_args, [named_table, public, set]);
        _ ->
            ok
    end,
    ets:insert(bt4068_fake_child_args, {last, Args}),
    ok.

last_fake_child_args() ->
    [{last, Args}] = ets:lookup(bt4068_fake_child_args, last),
    Args.

%% Fake class method: simulates `MyClass create: Name value: Val` which starts
%% a gen_server and returns a beamtalk_object tuple.
'class_create:value:'(_ClassSelf, _Name, _Value) ->
    {ok, Pid} = start_link_fake_child(),
    {beamtalk_object, 'FakeChild', ?MODULE, Pid}.

%% Fake class method: returns an invalid (non-actor) value.
class_returnInvalid(_ClassSelf) ->
    <<"not an actor tuple">>.

%% ADR 0130 §5: class-variable access from a runtime-owned snapshot region.
%% Writes raise class_state_read_only; reads see the ETS mirror.
class_writeVar(ClassSelf) ->
    beamtalk_class_vars:put(ClassSelf, n, 1).

class_readVar(ClassSelf) ->
    beamtalk_class_vars:get(ClassSelf, n).

%% Reads a class variable (must see the mirror value), then returns an actor.
class_readThenActor(ClassSelf) ->
    7 = beamtalk_class_vars:get(ClassSelf, n),
    {ok, Pid} = start_link_fake_child(),
    {beamtalk_object, 'FakeChild', ?MODULE, Pid}.

%% Fake class method that returns a supervisor tuple instead of an actor.
class_returnSupervisor(_ClassSelf) ->
    {ok, Pid} = start_link_fake_child(),
    {beamtalk_supervisor, 'FakeSupervisorChild', ?MODULE, Pid}.

%% Fake class method that calls the REAL
%% compiled `self spawn`/`self spawnWith:` entry point
%% (beamtalk_class_instantiation:class_self_spawn/3 — exactly what codegen
%% emits for those forms inside a class method body), targeting
%% `test_class_actor` — a generated-actor-shaped fixture whose own `spawn/0,1`
%% delegates to `beamtalk_actor:safe_spawn/2`, the function this PR's fix
%% touches. Unlike `'class_create:value:'/4` above (a `gen_server:start_link`
%% stand-in), this exercises the actual production call chain end to end.
'class_createViaSpawn:value:'(_ClassSelf, _Name, _Value) ->
    beamtalk_class_instantiation:class_self_spawn('BT3243RealSpawnChild', test_class_actor, []).

%% Same idea, but for the named-spawn
%% path — the REAL `self spawnAs:` entry point
%% (beamtalk_class_instantiation:class_self_spawn_as/4). `class_self_spawn_as/4`
%% returns a Result tagged map (matching the `Result(Self, Error)` return
%% type real `startAs:`-style factories declare — see
%% stdlib/test/fixtures/named_factory_actor.bt), so — exactly like a real
%% compiled factory calling `.unwrap` on it before returning — this unwraps
%% the ok value to hand `start_child_via_class_method/4` the bare actor
%% object it expects.
'class_createNamedViaSpawn:value:'(_ClassSelf, Name, _Value) ->
    #{'isOk' := true, 'okValue' := Obj} =
        beamtalk_class_instantiation:class_self_spawn_as(
            'BT3243RealNamedSpawnChild', test_class_actor, false, Name
        ),
    Obj.

%% Fake class method: self-sends `new`, exactly as codegen's
%% `try_instantiation_intrinsic` lowers `self new` inside a class method body —
%% `beamtalk_class_instantiation:class_self_new(ClassName, Module, Args)` with
%% `ClassName` taken from `ClassSelf`. Used to prove the abstract-instantiation
%% guard is enforced when this runs in the supervisor process (via
%% `start_child_via_class_method/4`), not just in the class's own gen_server.
class_selfNew(ClassSelf) ->
    ClassTag = element(2, ClassSelf),
    ClassName = beamtalk_primitive:class_name_from_tag(ClassTag),
    beamtalk_class_instantiation:class_self_new(ClassName, ?MODULE, []).

-doc """
Set up ETS tables and register a fake class for start_child_via_class_method.
Returns the fake class pid (a dummy process) that should be cleaned up after the test.
""".
setup_fake_class(ClassName) ->
    setup_fake_class(ClassName, undefined).

-doc """
Same as `setup_fake_class/1`, with an explicit `is_abstract` metadata value
— used to build classes that must be rejected by the
abstract-instantiation guard regardless of which process resolves them.
""".
setup_fake_class(ClassName, IsAbstract) ->
    beamtalk_class_metadata:new(),
    %% Register a dummy process as the class gen_server so whereis_class resolves.
    FakeClassPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    RegName = beamtalk_class_registry:registry_name(ClassName),
    register(RegName, FakeClassPid),
    %% Register module mapping so call_class_method_direct can find our fake methods.
    beamtalk_class_metadata:insert(ClassName, ?MODULE, undefined, undefined, IsAbstract),
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
    beamtalk_class_metadata:delete(ClassName),
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

%% ADR 0130 §5: the withClassMethod: factory runs in a read-only snapshot
%% region. Reads see the ETS mirror; a write raises class_state_read_only
%% instead of being silently discarded.
start_child_via_class_method_reads_mirror_test() ->
    FakeClassPid = setup_fake_class('BT3708Read'),
    beamtalk_class_registry:record_class_state_snapshot(FakeClassPid, #{n => 7}),
    try
        ClassSelf = #beamtalk_object{
            class = beamtalk_class_registry:class_object_tag('BT3708Read'),
            class_mod = ?MODULE,
            pid = FakeClassPid
        },
        ?assertEqual(
            7,
            beamtalk_class_vars:with_snapshot(ClassSelf, fun() -> class_readVar(ClassSelf) end)
        ),
        %% The factory reads `n` through the mirror and then returns an actor.
        ?assertMatch(
            {ok, Pid} when is_pid(Pid),
            beamtalk_supervisor:start_child_via_class_method(
                'BT3708Read', ?MODULE, class_readThenActor, []
            )
        ),
        ?assertEqual(undefined, get(beamtalk_class_vars:key('BT3708Read')))
    after
        beamtalk_class_registry:forget_class_state_snapshot(FakeClassPid),
        cleanup_fake_class('BT3708Read', FakeClassPid)
    end.

start_child_via_class_method_write_raises_test() ->
    FakeClassPid = setup_fake_class('BT3708Write'),
    beamtalk_class_registry:record_class_state_snapshot(FakeClassPid, #{n => 7}),
    try
        ?assertMatch(
            #beamtalk_error{kind = class_state_read_only},
            raised_beamtalk_error(fun() ->
                beamtalk_supervisor:start_child_via_class_method(
                    'BT3708Write', ?MODULE, class_writeVar, []
                )
            end)
        ),
        %% The snapshot is erased even though the method raised.
        ?assertEqual(undefined, get(beamtalk_class_vars:key('BT3708Write')))
    after
        beamtalk_class_registry:forget_class_state_snapshot(FakeClassPid),
        cleanup_fake_class('BT3708Write', FakeClassPid)
    end.

raised_beamtalk_error(Fun) ->
    try Fun() of
        Other -> {no_error, Other}
    catch
        error:#{error := #beamtalk_error{} = E} -> E;
        error:#beamtalk_error{} = E -> E
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

start_child_via_class_method_real_self_spawn_supervisor_restart_test() ->
    %% Closes a gap:
    %% start_child_via_class_method_supervisor_restart_test/0 above only
    %% proves restart works when the fake class method calls
    %% gen_server:start_link directly — a stand-in, not the real `self
    %% spawn`/`self spawnWith:` path. This test instead routes through the
    %% ACTUAL compiled runtime call chain: 'class_createViaSpawn:value:'/4 →
    %% beamtalk_class_instantiation:class_self_spawn/3 →
    %% beamtalk_class_instantiation:handle_spawn/4 →
    %% test_class_actor:spawn/0 → beamtalk_actor:safe_spawn/2 — the exact
    %% function this PR's fix touches. Before the
    %% ?BT_SUPERVISOR_SPAWN_CONTEXT_KEY fix, safe_spawn/2 always spawned
    %% unlinked, so the supervisor below would never observe the killed
    %% child's exit and this test would fail (ChildPid2 =:= ChildPid1,
    %% or the child simply never restarts).
    FakeClassPid = setup_fake_class('BT3243RealSpawnRestart'),
    try
        ChildSpec = #{
            id => 'BT3243RealSpawnRestart',
            start =>
                {beamtalk_supervisor, start_child_via_class_method, [
                    'BT3243RealSpawnRestart', ?MODULE, 'class_createViaSpawn:value:', [test, 1]
                ]},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [?MODULE]
        },
        SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
        {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
        try
            [{_, ChildPid1, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid1)),
            ?assert(is_process_alive(ChildPid1)),

            Ref = monitor(process, ChildPid1),
            exit(ChildPid1, kill),
            receive
                {'DOWN', Ref, process, ChildPid1, killed} -> ok
            after 2000 ->
                error(child_did_not_die)
            end,

            timer:sleep(100),

            [{_, ChildPid2, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid2)),
            ?assert(is_process_alive(ChildPid2)),
            ?assertNotEqual(ChildPid1, ChildPid2)
        after
            gen_server:stop(SupPid)
        end
    after
        cleanup_fake_class('BT3243RealSpawnRestart', FakeClassPid)
    end.

start_child_via_class_method_real_self_spawn_as_supervisor_restart_test() ->
    %% The test above only covers `self spawn`/`self spawnWith:`; this
    %% covers the named-spawn sibling path —
    %% `self spawnAs:`/`self spawnWith:as:` inside a `withClassMethod:`
    %% factory. Routes through the ACTUAL compiled call chain:
    %% 'class_createNamedViaSpawn:value:'/4 →
    %% beamtalk_class_instantiation:class_self_spawn_as/4 →
    %% do_class_self_named_spawn/6 → beamtalk_actor:'spawnAs'/3
    %% (safe_spawn_named). Before the fix, do_class_self_named_spawn/6
    %% unconditionally unlinked regardless of caller context, so this
    %% supervisor would never see the killed child's exit and the test
    %% would fail.
    FakeClassPid = setup_fake_class('BT3243RealNamedSpawnRestart'),
    try
        ChildSpec = #{
            id => 'BT3243RealNamedSpawnRestart',
            start =>
                {beamtalk_supervisor, start_child_via_class_method, [
                    'BT3243RealNamedSpawnRestart',
                    ?MODULE,
                    'class_createNamedViaSpawn:value:',
                    [
                        bt3243_real_named_spawn_restart_child, 1
                    ]
                ]},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [?MODULE]
        },
        SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
        {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
        try
            [{_, ChildPid1, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid1)),
            ?assert(is_process_alive(ChildPid1)),

            Ref = monitor(process, ChildPid1),
            exit(ChildPid1, kill),
            receive
                {'DOWN', Ref, process, ChildPid1, killed} -> ok
            after 2000 ->
                error(child_did_not_die)
            end,

            timer:sleep(100),

            [{_, ChildPid2, _, _}] = supervisor:which_children(SupPid),
            ?assert(is_pid(ChildPid2)),
            ?assert(is_process_alive(ChildPid2)),
            ?assertNotEqual(ChildPid1, ChildPid2)
        after
            gen_server:stop(SupPid)
        end
    after
        cleanup_fake_class('BT3243RealNamedSpawnRestart', FakeClassPid)
    end.

start_child_via_class_method_returns_supervisor_tuple_test() ->
    %% A class method returning {beamtalk_supervisor, ...} yields {ok, Pid}.
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

start_child_via_class_method_rejects_abstract_class_test() ->
    %% an abstract class's `self new` — dispatched, via a class method
    %% run through start_child_via_class_method/4, in the SUPERVISOR process
    %% rather than the class's own gen_server — must still raise the
    %% abstract-instantiation error. Before the fix, start_child_via_class_method
    %% seeded a hard-coded `beamtalk_class_is_abstract = false` into the
    %% supervisor's process dictionary, and the generic `new` path
    %% (`handle_new_generic/2`) trusted that PD flag, so a genuinely abstract
    %% class would have instantiated successfully instead of raising.
    FakeClassPid = setup_fake_class('BT3106Abstract', true),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
            beamtalk_supervisor:start_child_via_class_method(
                'BT3106Abstract', ?MODULE, class_selfNew, []
            )
        ),
        %% The fabricated PD entry must not linger either way.
        ?assertEqual(undefined, get(beamtalk_class_is_abstract))
    after
        cleanup_fake_class('BT3106Abstract', FakeClassPid)
    end.

%%====================================================================
%% Tests: is_supervisor integration with hierarchy
%%====================================================================

is_supervisor_transitive_inheritance_test() ->
    %% is_supervisor returns true for a grandchild of Supervisor.
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert(
        'BT1960GrandChild', undefined, undefined, 'BT1960Middle', undefined
    ),
    beamtalk_class_metadata:insert('BT1960Middle', undefined, undefined, 'Supervisor', undefined),
    try
        ?assertEqual(true, beamtalk_supervisor:is_supervisor('BT1960GrandChild'))
    after
        beamtalk_class_metadata:delete('BT1960GrandChild'),
        beamtalk_class_metadata:delete('BT1960Middle')
    end.

%%====================================================================
%% Tests: stop/1 — children are terminated
%%====================================================================

stop_terminates_children_test() ->
    %% stop/1 terminates the supervisor and its children.
    SupPid = start_anon_supervisor_with_worker(stop_child),
    [{_, ChildPid, _, _}] = supervisor:which_children(SupPid),
    ?assert(is_process_alive(ChildPid)),
    Self = make_supervisor_tuple('TestSupStop', test_stop_mod, SupPid),
    beamtalk_supervisor:stop(Self),
    timer:sleep(50),
    ?assertEqual(false, is_process_alive(SupPid)),
    ?assertEqual(false, is_process_alive(ChildPid)).

%%====================================================================
%% ADR 0135 §1 (BT-3962): `supervise` and the spawn family
%%====================================================================

-define(BT3963_MOD, beamtalk_bt3963_sup_fixture).

%% The registered class object for a class started by `with_bt3963_class/3`.
bt3962_class_self(ClassName) ->
    beamtalk_class_registry:class_object_from_pid(
        beamtalk_class_registry:whereis_class(ClassName)
    ).

linked_to_self(Pid) ->
    {links, Links} = erlang:process_info(self(), links),
    lists:member(Pid, Links).

%% Poll (yielding) until `Pred()` holds, or fail after ~2 s.
wait_until(Pred) ->
    wait_until(Pred, 400).

wait_until(Pred, 0) ->
    ?assert(Pred());
wait_until(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(5),
            wait_until(Pred, N - 1)
    end.

supervise_starts_registered_unlinked_and_runs_initialize_once_test() ->
    Test = self(),
    Config = bt3963_static_config(#{
        initialize => fun(Self) ->
            Test ! {initialized, Self, self()},
            nil
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        {ok, Sup} = beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD),
        Pid = element(4, Sup),
        try
            ?assertEqual(bt3963_sup('BT3963Sup', Pid), Sup),
            ?assertEqual(Pid, whereis(?BT3963_MOD)),
            %% Unlinked from the caller (and so from any class process).
            ?assertNot(linked_to_self(Pid)),
            receive
                {initialized, Sup, Runner} -> ?assertEqual(Test, Runner)
            after 2000 -> error(not_initialized)
            end,
            %% Idempotent: the running instance, and no second initialize.
            ?assertEqual({ok, Sup}, beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)),
            receive
                {initialized, _, _} -> error(initialized_twice)
            after 100 -> ok
            end
        after
            gen_server:stop(Pid)
        end
    end).

supervise_start_failure_returns_supervisor_start_failed_test() ->
    Config = bt3963_static_config(#{childSpecs => fun(_) -> error(boom) end}),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        ?assertMatch(
            {error, #beamtalk_error{
                kind = supervisor_start_failed, class = 'BT3963Sup', selector = supervise
            }},
            beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)
        ),
        ?assertEqual(undefined, whereis(?BT3963_MOD))
    end).

supervise_callers_racing_during_start_get_one_instance_test() ->
    %% Two `supervise` callers race while the first is still inside
    %% `start_link` (its `childSpecs` is blocked): the second waits, then
    %% answers the same instance; `initialize` runs once.
    Test = self(),
    Config = bt3963_static_config(#{
        childSpecs => fun(_) ->
            Test ! {in_start, self()},
            receive
                go -> []
            after 5000 -> []
            end
        end,
        initialize => fun(_) ->
            Test ! initialized,
            nil
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        Caller = fun(Tag) ->
            spawn(fun() ->
                Test ! {Tag, beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)}
            end)
        end,
        Caller(first),
        SupPid =
            receive
                {in_start, P} -> P
            after 5000 -> error(not_started)
            end,
        Caller(second),
        timer:sleep(50),
        SupPid ! go,
        First =
            receive
                {first, F} -> F
            after 10000 -> error(first_timeout)
            end,
        Second =
            receive
                {second, S} -> S
            after 10000 -> error(second_timeout)
            end,
        try
            ?assertEqual({ok, bt3963_sup('BT3963Sup', SupPid)}, First),
            ?assertEqual(First, Second),
            receive
                initialized -> ok
            after 2000 -> error(not_initialized)
            end,
            receive
                initialized -> error(initialized_twice)
            after 100 -> ok
            end
        after
            gen_server:stop(SupPid)
        end
    end).

start_honours_exit_signals_from_existing_links_test() ->
    %% The start traps exits in the caller; a non-trapping caller whose
    %% existing link dies meanwhile must still die once the start returns.
    Test = self(),
    Config = bt3963_static_config(#{
        childSpecs => fun(_) ->
            Test ! {in_start, self()},
            receive
                go -> []
            after 5000 -> []
            end
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        {Caller, Ref} = spawn_monitor(fun() ->
            Helper = spawn_link(fun() ->
                receive
                    stop -> ok
                end
            end),
            Test ! {helper, Helper},
            _ = beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, spawn, []),
            Test ! survived,
            receive
                stop -> ok
            end
        end),
        Helper =
            receive
                {helper, H} -> H
            after 2000 -> error(no_helper)
            end,
        SupPid =
            receive
                {in_start, P} -> P
            after 5000 -> error(not_started)
            end,
        %% Kill the caller's link while the caller is inside the start.
        exit(Helper, boom),
        timer:sleep(20),
        SupPid ! go,
        try
            receive
                {'DOWN', Ref, process, Caller, Reason} -> ?assertEqual(boom, Reason)
            after 5000 -> error(caller_survived_linked_exit)
            end,
            receive
                survived -> error(caller_survived_linked_exit)
            after 0 -> ok
            end
        after
            catch gen_server:stop(SupPid)
        end
    end).

spawn_starts_independent_unlinked_listed_instances_test() ->
    Test = self(),
    Config = bt3963_static_config(#{
        initialize => fun(Self) ->
            Test ! {initialized, Self},
            nil
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        A = beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, spawn, []),
        B = beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, 'spawnWith:', [#{n => 2}]),
        PA = element(4, A),
        PB = element(4, B),
        try
            ?assertNotEqual(PA, PB),
            ?assertNot(linked_to_self(PA)),
            ?assertNot(linked_to_self(PB)),
            ?assertEqual(undefined, whereis(?BT3963_MOD)),
            ?assertEqual({ok, nil}, beamtalk_supervisor:args(A)),
            ?assertEqual({ok, #{n => 2}}, beamtalk_supervisor:args(B)),
            %% initialize ran once per instance, in the caller.
            receive
                {initialized, A} -> ok
            after 2000 -> error(a_not_initialized)
            end,
            receive
                {initialized, B} -> ok
            after 2000 -> error(b_not_initialized)
            end,
            %% Both are listed as top-level instances (`pg`) ...
            ?assert(lists:member(PA, beamtalk_supervisor:instance_pids())),
            ?assert(lists:member(B, beamtalk_supervisor:instances())),
            gen_server:stop(PA),
            ?assert(is_process_alive(PB)),
            %% ... and `pg` drops a stopped one.
            wait_until(fun() -> not lists:member(PA, beamtalk_supervisor:instance_pids()) end)
        after
            catch gen_server:stop(PA),
            catch gen_server:stop(PB)
        end
    end).

spawn_start_failure_raises_supervisor_start_failed_test() ->
    Config = bt3963_static_config(#{childSpecs => fun(_) -> error(boom) end}),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        ?assertError(
            #{error := #beamtalk_error{kind = supervisor_start_failed, selector = spawn}},
            beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, spawn, [])
        )
    end).

%% BT-3994: a raising `initialize` stops the instance; the `Result` forms
%% (`supervise`, `spawnAs:`, `spawnWith:as:`) answer `Result error:`
%% `#supervisor_start_failed` carrying the error (as `Actor spawnAs:` does),
%% while `spawn` re-raises it.
initialize_raise_answers_result_error_for_result_forms_test() ->
    Name = bt3994_init_raise_sup,
    Config = bt3963_static_config(#{
        initialize => fun(_Self) ->
            beamtalk_error:raise(beamtalk_error:new(bt3994_init_failed, 'BT3963Sup'))
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        ResultError = fun(Selector, Args) ->
            R = beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, Selector, Args),
            ?assertMatch(#{isOk := false, errReason := #{error := #beamtalk_error{}}}, R),
            #{errReason := #{error := E}} = R,
            E
        end,
        lists:foreach(
            fun({Selector, Args}) ->
                E = ResultError(Selector, Args),
                ?assertMatch(
                    #beamtalk_error{
                        kind = supervisor_start_failed,
                        class = 'BT3963Sup',
                        selector = Selector,
                        details = #{cause := #beamtalk_error{kind = bt3994_init_failed}}
                    },
                    E
                )
            end,
            [{supervise, []}, {'spawnAs:', [Name]}, {'spawnWith:as:', [#{n => 1}, Name]}]
        ),
        %% Each failed instance was stopped: no registration is left behind.
        ?assertEqual(undefined, whereis(?BT3963_MOD)),
        ?assertEqual(undefined, whereis(Name)),
        ?assertError(
            #{error := #beamtalk_error{kind = bt3994_init_failed}},
            beamtalk_supervisor:class_start('BT3963Sup', ?BT3963_MOD, spawn, [])
        )
    end).

%% BT-3994: `supervise` serialises racing starts with a node-local FIFO lock
%% and a bounded wait. A caller stuck behind a start that does not finish gets
%% `#supervisor_start_failed` after `init_wait_timeout_ms`, instead of waiting
%% for good; once the start finishes, the next caller is served at once.
supervise_waiter_behind_hung_start_gets_bounded_error_test() ->
    Test = self(),
    Config = bt3963_static_config(#{
        childSpecs => fun(_) ->
            Test ! {in_start, self()},
            receive
                go -> []
            after 10000 -> []
            end
        end
    }),
    application:set_env(beamtalk_runtime, init_wait_timeout_ms, 300),
    try
        with_bt3963_class('BT3963Sup', Config, fun() ->
            spawn(fun() ->
                Test ! {first, beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)}
            end),
            SupPid =
                receive
                    {in_start, P} -> P
                after 5000 -> error(not_started)
                end,
            try
                T0 = erlang:monotonic_time(millisecond),
                ?assertMatch(
                    {error, #beamtalk_error{
                        kind = supervisor_start_failed, class = 'BT3963Sup', selector = supervise
                    }},
                    beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)
                ),
                Waited = erlang:monotonic_time(millisecond) - T0,
                ?assert(Waited >= 300),
                ?assert(Waited < 3000),
                %% The hung start is still the only one in progress.
                ?assertEqual(SupPid, whereis(?BT3963_MOD)),
                SupPid ! go,
                receive
                    {first, First} ->
                        ?assertEqual({ok, bt3963_sup('BT3963Sup', SupPid)}, First)
                after 5000 -> error(first_caller_timeout)
                end,
                %% No lock left held: the next caller answers immediately.
                ?assertEqual(
                    {ok, bt3963_sup('BT3963Sup', SupPid)},
                    beamtalk_supervisor:supervise('BT3963Sup', ?BT3963_MOD)
                )
            after
                catch gen_server:stop(SupPid)
            end
        end)
    after
        application:unset_env(beamtalk_runtime, init_wait_timeout_ms)
    end.

spawn_as_registers_and_named_finds_it_test() ->
    Name = bt3962_named_sup,
    with_bt3963_class('BT3963Sup', bt3963_static_config(#{}), fun() ->
        R1 = beamtalk_supervisor:class_start(
            'BT3963Sup', ?BT3963_MOD, 'spawnWith:as:', [#{n => 1}, Name]
        ),
        ?assertMatch(
            #{isOk := true, okValue := {beamtalk_supervisor, 'BT3963Sup', ?BT3963_MOD, _}}, R1
        ),
        #{okValue := Sup} = R1,
        Pid = element(4, Sup),
        try
            ?assertEqual(Pid, whereis(Name)),
            ?assertNot(linked_to_self(Pid)),
            ?assertEqual({ok, #{n => 1}}, beamtalk_supervisor:args(Sup)),
            ClassSelf = bt3962_class_self('BT3963Sup'),
            ?assertEqual({ok, Sup}, beamtalk_supervisor:named(ClassSelf, Name)),
            ?assertMatch(
                {error, #beamtalk_error{kind = name_registered, selector = 'spawnAs:'}},
                beamtalk_supervisor:doSpawnAs(ClassSelf, Name)
            ),
            ?assertMatch(
                {error, #beamtalk_error{kind = reserved_name}},
                beamtalk_supervisor:doSpawnAs(ClassSelf, beamtalk_bt3962_reserved)
            ),
            ?assertMatch(
                {error, #beamtalk_error{kind = name_not_registered}},
                beamtalk_supervisor:named(ClassSelf, bt3962_no_such_sup)
            ),
            ?assertMatch(
                {error, #beamtalk_error{kind = type_error}},
                beamtalk_supervisor:named(ClassSelf, <<"bt3962">>)
            ),
            Other = spawn(fun() ->
                receive
                    stop -> ok
                end
            end),
            register(bt3962_not_a_sup, Other),
            try
                ?assertMatch(
                    {error, #beamtalk_error{kind = wrong_class}},
                    beamtalk_supervisor:named(ClassSelf, bt3962_not_a_sup)
                )
            after
                Other ! stop
            end
        after
            gen_server:stop(Pid)
        end
    end).

%%====================================================================
%% ADR 0135 §3 (BT-3962): nested supervisor children
%%====================================================================

%% A Beamtalk child-spec dict for a supervisor child, as `SupervisionSpec
%% childSpec` builds it.
bt3962_bt_spec(ClassObj, Id, StartFn, StartArgs) ->
    #{
        id => Id,
        start => #{
            '$beamtalk_class' => 'Array',
            data => #{0 => ClassObj, 1 => StartFn, 2 => StartArgs}
        },
        restart => permanent,
        shutdown => infinity,
        type => supervisor
    }.

spec_to_otp_supervisor_child_mapping_test() ->
    M = bt3962_map_sup_mod,
    {FakePid, ClassObj} = fake_supervisor_class('BT3962MapSupCls', M),
    Start = fun(StartFn, StartArgs, Mode) ->
        maps:get(
            start,
            beamtalk_supervisor:spec_to_otp(
                bt3962_bt_spec(ClassObj, 1, StartFn, StartArgs), Mode
            )
        )
    end,
    try
        ?assertEqual(
            {beamtalk_supervisor, start_supervisor_child, [M, nil]}, Start(spawn, [], static)
        ),
        ?assertEqual(
            {beamtalk_supervisor, start_supervisor_child, [M, #{x => 1}]},
            Start('spawnWith:', [#{x => 1}], static)
        ),
        ?assertEqual(
            {beamtalk_supervisor, start_supervisor_child, [M, nil, n1]},
            Start('spawnAs:', [n1], static)
        ),
        ?assertEqual(
            {beamtalk_supervisor, start_supervisor_child, [M, #{x => 1}, n1]},
            Start('spawnWith:as:', [#{x => 1}, n1], static)
        ),
        ?assertEqual(
            {beamtalk_supervisor, start_dynamic_supervisor_child, [M, nil]},
            Start(spawn, [], dynamic)
        ),
        ?assertEqual(
            {beamtalk_supervisor, start_dynamic_supervisor_child, [M, #{x => 1}]},
            Start('spawnWith:', [#{x => 1}], dynamic)
        ),
        %% Integer ids pass through untouched.
        ?assertEqual(
            1,
            maps:get(id, beamtalk_supervisor:spec_to_otp(bt3962_bt_spec(ClassObj, 1, spawn, [])))
        ),
        %% withClassMethod: and scope: #global are not supported.
        ?assertError(
            #{error := #beamtalk_error{kind = type_error}},
            Start(classMethod, [foo, []], static)
        ),
        ?assertError(
            #{error := #beamtalk_error{kind = type_error}},
            Start('spawnAs:scope:', [n1], static)
        )
    after
        FakePid ! stop,
        beamtalk_class_metadata:delete('BT3962MapSupCls')
    end.

start_supervisor_child_passes_args_and_name_test() ->
    Test = self(),
    Config = bt3963_static_config(#{
        childSpecs => fun(Self) ->
            Test ! {child_args, beamtalk_supervisor:args(Self)},
            []
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        {ok, P1} = beamtalk_supervisor:start_supervisor_child(?BT3963_MOD, #{n => 1}),
        {ok, P2} = beamtalk_supervisor:start_supervisor_child(
            ?BT3963_MOD, #{n => 2}, bt3962_trampoline_named
        ),
        try
            receive
                {child_args, A1} -> ?assertEqual({ok, #{n => 1}}, A1)
            after 2000 -> error(no_args)
            end,
            receive
                {child_args, A2} -> ?assertEqual({ok, #{n => 2}}, A2)
            after 2000 -> error(no_args)
            end,
            ?assertEqual(P2, whereis(bt3962_trampoline_named)),
            ?assertEqual(undefined, whereis(?BT3963_MOD)),
            %% A pool's template: arity 2/3/4 = startChild / startChild: / startChild:name:.
            {ok, P3} = beamtalk_supervisor:start_dynamic_supervisor_child(?BT3963_MOD, nil),
            stop_linked(P3),
            ?assertMatch(
                {error, #beamtalk_error{kind = type_error}},
                beamtalk_supervisor:start_dynamic_supervisor_child(?BT3963_MOD, nil, nil, <<"n">>)
            ),
            ?assertMatch(
                {error, #beamtalk_error{kind = reserved_name}},
                beamtalk_supervisor:start_supervisor_child(?BT3963_MOD, nil, beamtalk_reserved)
            )
        after
            stop_linked(P1),
            stop_linked(P2)
        end
    end).

start_supervisor_child_falls_back_to_start_link_0_for_old_modules_test() ->
    Old = beamtalk_bt3962_old_sup_fixture,
    {ok, P1} = beamtalk_supervisor:start_supervisor_child(Old, #{ignored => true}),
    ?assertEqual(P1, whereis(Old)),
    stop_linked(P1),
    {ok, P2} = beamtalk_supervisor:start_supervisor_child(Old, nil, bt3962_unused_name),
    ?assertEqual(P2, whereis(Old)),
    stop_linked(P2).

nested_old_module_child_starts_through_the_trampoline_test() ->
    %% A parent built after ADR 0135 nests a child supervisor module that
    %% exports only start_link/0 (ADR 0125).
    Old = beamtalk_bt3962_old_sup_fixture,
    {FakePid, ClassObj} = fake_supervisor_class('BT3962OldSupCls', Old),
    try
        [Spec] = beamtalk_supervisor:build_child_specs([ClassObj]),
        SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
        {ok, Parent} = supervisor:start_link(?MODULE, {SupFlags, [Spec]}),
        try
            [{'BT3962OldSupCls', Child, supervisor, [Old]}] = supervisor:which_children(Parent),
            ?assertEqual(Child, whereis(Old))
        after
            stop_linked(Parent)
        end
    after
        FakePid ! stop,
        beamtalk_class_metadata:delete('BT3962OldSupCls')
    end.

nested_supervisor_children_with_integer_ids_restart_independently_test() ->
    with_bt3963_class('BT3963Sup', bt3963_static_config(#{}), fun() ->
        {FakePid, ClassObj} = fake_supervisor_class('BT3962PairLeafCls', ?BT3963_MOD),
        try
            Specs = [
                beamtalk_supervisor:spec_to_otp(bt3962_bt_spec(ClassObj, 1, spawn, [])),
                beamtalk_supervisor:spec_to_otp(
                    bt3962_bt_spec(ClassObj, 2, 'spawnWith:', [#{x => 2}])
                )
            ],
            SupFlags = #{strategy => one_for_one, intensity => 3, period => 5},
            {ok, Parent} = supervisor:start_link(?MODULE, {SupFlags, Specs}),
            try
                Children = fun() ->
                    maps:from_list([{Id, P} || {Id, P, _, _} <- supervisor:which_children(Parent)])
                end,
                #{1 := C1, 2 := C2} = Children(),
                ?assertNotEqual(C1, C2),
                ?assertEqual(
                    {ok, #{x => 2}}, beamtalk_supervisor:args(bt3963_sup('BT3963Sup', C2))
                ),
                exit(C1, kill),
                wait_until(fun() ->
                    case Children() of
                        #{1 := New1} when is_pid(New1), New1 =/= C1 -> true;
                        _ -> false
                    end
                end),
                ?assertEqual(C2, maps:get(2, Children()))
            after
                stop_linked(Parent)
            end
        after
            FakePid ! stop,
            beamtalk_class_metadata:delete('BT3962PairLeafCls')
        end
    end).

%%====================================================================
%% terminateChild/2 — static supervisor (by class name) path
%%====================================================================

terminate_child_by_class_test() ->
    %% terminateChild with a class object arg resolves the child id by module
    %% (BT-3966), so a child whose id is not the class name (`withId:`) is
    %% terminated too, and returns {ok, nil} on success (static path).
    {SupPid, [W]} = start_anon_supervisor_with_specs([
        {custom_id, temporary, [bt1980_class_child_mod]}
    ]),
    Self = make_supervisor_tuple('BT1980TermClass', bt1980_term_mod, SupPid),
    %% Build a class object arg whose class_name/1 returns the child id atom.
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, bt1980_class_child),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg =
            {beamtalk_object, 'BT1980ClassChild class', bt1980_class_child_mod, FakeClassPid},
        Result = beamtalk_supervisor:terminateChild(Self, ClassArg),
        ?assertEqual({ok, nil}, Result),
        ?assertEqual(false, is_process_alive(W)),
        ?assertEqual({ok, 0}, beamtalk_supervisor:countChildren(Self))
    after
        FakeClassPid ! stop,
        (try
            gen_server:stop(SupPid)
        catch
            _:_ -> ok
        end)
    end.

terminate_child_by_class_not_found_test() ->
    %% ADR 0080 Phase 1 — BEHAVIOR CHANGE:
    %% terminateChild/2 (static path) used to raise on {error, not_found}.
    %% It now returns {ok, nil} (idempotent), matching the dynamic-path
    %% convention. Any caller relying on the static path raising for a
    %% missing child loses that signal. See ADR 0080 §Decision.
    SupPid = start_anon_supervisor(),
    Self = make_supervisor_tuple('BT1980TermClassNF', bt1980_term_nf_mod, SupPid),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, bt1980_missing_child),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg = {beamtalk_object, 'BT1980MissingChild class', bt1980_missing_mod, FakeClassPid},
        ?assertEqual(
            {ok, nil},
            beamtalk_supervisor:terminateChild(Self, ClassArg)
        )
    after
        FakeClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% startChild/1 and startChild/2 — DynamicSupervisor paths
%%====================================================================

%% Fake class object for startChild tests: returns a class object whose
%% class gen_server replies class_name=<worker id atom>, and whose element(3)
%% points at this test module (which exports start_worker/1).
make_child_class_for_startchild(ChildClassName) ->
    ChildClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, ChildClassName),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    {ChildClassPid,
        {beamtalk_object, list_to_atom(atom_to_list(ChildClassName) ++ " class"), ?MODULE,
            ChildClassPid}}.

%% A DynamicSupervisor for startChild tests; the test records its child class
%% with set_child_class/2.
start_dynamic_supervisor_for_startchild() ->
    %% We reuse start_dynamic_supervisor/0 which wires ?MODULE:start_worker/1
    %% as the dynamic child template.
    start_dynamic_supervisor().

%% Start a simple_one_for_one supervisor whose dynamic template is
%% {?MODULE, start_link_fake_child, []}. start_link_fake_child/0 calls
%% gen_server:start_link and returns {ok, Pid} — so supervisor:start_child(Sup, [])
%% succeeds and exercises the success branch of startChild/1.
start_dynamic_supervisor_with_nullary_child() ->
    ChildSpec = #{
        id => fake_child_template,
        start => {?MODULE, start_link_fake_child, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    {ok, SupPid} = supervisor:start_link(?MODULE, {simple_one_for_one, ChildSpec}),
    SupPid.

%% ADR 0135: startChild/1,2,3 read a pool's child class from the pool's own
%% process dictionary (`'$beamtalk_child_class'`, written by `dynamic_init/3`
%% from the instance's `childClass`). These tests start plain OTP supervisors,
%% so they record the entry themselves: `set_child_class/2` writes it into a
%% running supervisor (sys:replace_state runs the fun in that process), and
%% `set_child_class/1` stores it for the next supervisor this module's
%% `init/1` starts.
set_child_class(SupPid, Obj) ->
    Entry = child_class_entry(Obj),
    _ = sys:replace_state(SupPid, fun(S) ->
        put('$beamtalk_child_class', Entry),
        S
    end),
    ok.

set_child_class(Obj) ->
    case ets:info(beamtalk_supervisor_tests_child_cell, id) of
        undefined ->
            ets:new(beamtalk_supervisor_tests_child_cell, [named_table, public, set]);
        _ ->
            ok
    end,
    ets:insert(beamtalk_supervisor_tests_child_cell, {current, child_class_entry(Obj)}),
    ok.

child_class_entry(Obj) ->
    {beamtalk_object_class:class_name_safe(element(4, Obj)), element(3, Obj)}.

%% Called from this module's supervisor init/1: plant the child-class entry a
%% test stored with set_child_class/1, as dynamic_init/3 would.
put_pending_child_class() ->
    case ets:info(beamtalk_supervisor_tests_child_cell, id) of
        undefined ->
            ok;
        _ ->
            case ets:take(beamtalk_supervisor_tests_child_cell, current) of
                [{current, Entry}] -> put('$beamtalk_child_class', Entry);
                [] -> ok
            end
    end,
    ok.

startChild_arity1_success_test() ->
    %% startChild/1 success path returns
    %% {ok, {beamtalk_object, ChildClass, Module, Pid}} for non-supervisor
    %% children.
    SupPid = start_dynamic_supervisor_with_nullary_child(),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT1980DynWorker'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT1980DynSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self),
        ?assertMatch({ok, {beamtalk_object, 'BT1980DynWorker', ?MODULE, _}}, Result),
        {ok, Child} = Result,
        ChildPid = element(4, Child),
        ?assert(is_process_alive(ChildPid))
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_announces_child_added_test() ->
    %% ADR 0093 §2: a successful startChild/1 publishes a
    %% SupervisionChildAdded system announcement carrying the supervisor and
    %% child class. Subscribe a fun handler on the bus, start a child, and assert
    %% delivery with the right payload.
    case whereis(beamtalk_announcements) of
        undefined -> {ok, _} = beamtalk_announcements:start_link();
        _ -> ok
    end,
    Collector = self(),
    Handler = fun(Event) -> Collector ! {child_added, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe(
        'SupervisionChildAdded', self(), Handler, false
    ),
    SupPid = start_dynamic_supervisor_with_nullary_child(),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT2445DynWorker'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT2445DynSup', ?MODULE, SupPid},
        {ok, _Child} = beamtalk_supervisor:startChild(Self),
        receive
            {child_added, Event} ->
                ?assertEqual('SupervisionChildAdded', maps:get('$beamtalk_class', Event)),
                ?assertEqual('BT2445DynSup', maps:get(supervisor, Event)),
                ?assertEqual('BT2445DynWorker', maps:get(childClass, Event))
        after 1000 ->
            ?assert(false)
        end
    after
        beamtalk_announcements:unsubscribe(SubRef),
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_arity2_success_test() ->
    %% startChild/2 appends [Args] to the simple_one_for_one
    %% template. The template MFA is {?MODULE, start_worker, []}; with
    %% Args=self() OTP supervisor calls start_worker(self()) (arity-1), which
    %% returns {ok, Pid}. Verify the wrapped result is {ok, {beamtalk_object, …}}.
    SupPid = start_dynamic_supervisor_for_startchild(),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1980DynArgs'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT1980DynArgsSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self, self()),
        %% Drain the worker_ready message start_worker/1 sends us.
        receive
            {worker_ready, _} -> ok
        after 1000 -> error(no_worker_ready)
        end,
        ?assertMatch({ok, {beamtalk_object, 'BT1980DynArgs', ?MODULE, _}}, Result)
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% run_initialize/1 — instance-side initialize lifecycle hook (ADR 0135 §1)
%%====================================================================

run_initialize_sends_instance_initialize_test() ->
    %% The hook is sent to the new supervisor by instance dispatch, in the
    %% caller: `self` is the supervisor, which can be messaged.
    Test = self(),
    Config = bt3963_static_config(#{
        initialize => fun(Self) ->
            {ok, Count} = beamtalk_supervisor:countChildren(Self),
            Test ! {initialized, Self, Count, self()},
            nil
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        {ok, Pid} = beamtalk_bt3963_sup_fixture:start_link(nil),
        try
            Sup = bt3963_sup('BT3963Sup', Pid),
            ?assertEqual(ok, beamtalk_supervisor:run_initialize(Sup)),
            receive
                {initialized, Self, Count, Runner} ->
                    ?assertEqual(Sup, Self),
                    ?assertEqual(0, Count),
                    ?assertEqual(Test, Runner)
            after 2000 -> error(not_initialized)
            end
        after
            stop_linked(Pid)
        end
    end).

run_initialize_raise_stops_supervisor_test() ->
    %% BT-3720: if `initialize` raises, the caller never receives the
    %% supervisor, so it is stopped (name freed, child stopped) before the
    %% error propagates.
    Config = bt3963_static_config(#{
        initialize => fun(_Self) ->
            beamtalk_error:raise(beamtalk_error:new(runtime_error, 'BT3963Sup', initialize))
        end
    }),
    with_bt3963_class('BT3963Sup', Config, fun() ->
        SupName = bt3720_half_started_sup,
        SupPid = start_named_supervisor_with_worker(SupName, bt3720_child),
        unlink(SupPid),
        {ok, WorkerPid} = wait_for_worker(),
        SupRef = erlang:monitor(process, SupPid),
        WorkerRef = erlang:monitor(process, WorkerPid),
        Sup = bt3963_sup('BT3963Sup', SupPid),
        ?assertMatch(
            #beamtalk_error{kind = runtime_error},
            raised_beamtalk_error(fun() -> beamtalk_supervisor:run_initialize(Sup) end)
        ),
        receive
            {'DOWN', SupRef, process, SupPid, _} -> ok
        after 2000 -> error(supervisor_leaked)
        end,
        receive
            {'DOWN', WorkerRef, process, WorkerPid, _} -> ok
        after 2000 -> error(child_orphaned)
        end,
        ?assertEqual(undefined, whereis(SupName))
    end).

%%====================================================================
%% to_otp_strategy/1 — all strategy variants
%%====================================================================

to_otp_strategy_oneForOne_test() ->
    ?assertEqual(one_for_one, beamtalk_supervisor:to_otp_strategy(oneForOne)).

to_otp_strategy_oneForAll_test() ->
    %% oneForAll maps to one_for_all.
    ?assertEqual(one_for_all, beamtalk_supervisor:to_otp_strategy(oneForAll)).

to_otp_strategy_restForOne_test() ->
    %% restForOne maps to rest_for_one.
    ?assertEqual(rest_for_one, beamtalk_supervisor:to_otp_strategy(restForOne)).

to_otp_strategy_unknown_passes_through_test() ->
    %% Unknown strategy passes through unchanged so OTP reports it.
    ?assertEqual(mystery, beamtalk_supervisor:to_otp_strategy(mystery)).

%%====================================================================
%% wrap_child/3 — via whichChild on supervisor and actor children
%%====================================================================

wrap_child_actor_returns_beamtalk_object_test() ->
    %% whichChild returns {beamtalk_object, ...} for a non-supervisor
    %% child. ClassArg's element(3) must be listed in the child spec's modules
    %% list so lists:member succeeds inside whichChild.
    Parent = self(),
    ChildSpec = #{
        id => bt1980_wrap_worker,
        start => {?MODULE, start_worker, [Parent]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(worker_not_ready)
    end,
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'BT1980NonSupChild'),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        Self = make_supervisor_tuple('BT1980WrapSup', bt1980_wrap_sup_mod, SupPid),
        ClassArg = {beamtalk_object, 'BT1980NonSupChild class', ?MODULE, FakeClassPid},
        Result = beamtalk_supervisor:whichChild(Self, ClassArg),
        ?assertMatch({ok, {beamtalk_object, 'BT1980NonSupChild', ?MODULE, _}}, Result)
    after
        FakeClassPid ! stop,
        gen_server:stop(SupPid)
    end.

wrap_child_supervisor_returns_beamtalk_supervisor_test() ->
    %% whichChild returns {beamtalk_supervisor, ...} when the child
    %% class inherits from Supervisor.
    beamtalk_class_metadata:new(),
    beamtalk_class_metadata:insert(
        'BT1980WrapSupChildCls', undefined, undefined, 'Supervisor', undefined
    ),
    Parent = self(),
    ChildSpec = #{
        id => bt1980_wrap_sup_child,
        start => {?MODULE, start_worker, [Parent]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(worker_not_ready)
    end,
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'BT1980WrapSupChildCls'),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        Self = make_supervisor_tuple('BT1980WrapSup2', sup2_mod, SupPid),
        ClassArg = {beamtalk_object, 'BT1980WrapSupChildCls class', ?MODULE, FakeClassPid},
        Result = beamtalk_supervisor:whichChild(Self, ClassArg),
        ?assertMatch({ok, {beamtalk_supervisor, 'BT1980WrapSupChildCls', ?MODULE, _}}, Result)
    after
        FakeClassPid ! stop,
        gen_server:stop(SupPid),
        beamtalk_class_metadata:delete('BT1980WrapSupChildCls')
    end.

%%====================================================================
%% build_child_specs/1 — nested-supervisor path
%%====================================================================

build_child_specs_nested_supervisor_test() ->
    %% ADR 0135 §3: a bare Supervisor-subclass class goes through
    %% `supervisionSpec` -> `childSpec` like any child, and starts through the
    %% `start_supervisor_child` trampoline (anonymous, args nil), type
    %% supervisor, shutdown infinity, restart permanent.
    {FakeClassPid, ClassObj} = fake_supervisor_class('BT1980NestedSupCls', bt1980_nested_sup_mod),
    try
        [Spec] = beamtalk_supervisor:build_child_specs([ClassObj]),
        ?assertEqual('BT1980NestedSupCls', maps:get(id, Spec)),
        ?assertEqual(
            {beamtalk_supervisor, start_supervisor_child, [bt1980_nested_sup_mod, nil]},
            maps:get(start, Spec)
        ),
        ?assertEqual(infinity, maps:get(shutdown, Spec)),
        ?assertEqual(permanent, maps:get(restart, Spec)),
        ?assertEqual(supervisor, maps:get(type, Spec)),
        ?assertEqual([bt1980_nested_sup_mod], maps:get(modules, Spec))
    after
        FakeClassPid ! stop,
        beamtalk_class_metadata:delete('BT1980NestedSupCls')
    end.

%%====================================================================
%% ensure_root_table — idempotent concurrent creation
%%====================================================================

%%====================================================================
%% terminateChild:class:/2 and terminateChild:child:/2 aliases
%%====================================================================

terminateChild_class_alias_delegates_test() ->
    %% 'terminateChild:class:'/2 delegates to terminateChild/2.
    SupPid = start_anon_supervisor_with_worker(bt1980_alias_cls_child),
    Self = make_supervisor_tuple('BT1980AliasCls', bt1980_alias_cls_mod, SupPid),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, bt1980_alias_cls_child),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg =
            {beamtalk_object, 'BT1980AliasClsChild class', bt1980_alias_cls_child_mod,
                FakeClassPid},
        Result = beamtalk_supervisor:'terminateChild:class:'(Self, ClassArg),
        ?assertEqual({ok, nil}, Result)
    after
        FakeClassPid ! stop,
        (try
            gen_server:stop(SupPid)
        catch
            _:_ -> ok
        end)
    end.

terminateChild_child_alias_delegates_test() ->
    %% 'terminateChild:child:'/2 delegates to terminateChild/2
    %% (DynamicSupervisor path: arg is an actor instance, not a class object).
    SupPid = start_dynamic_supervisor(),
    Parent = self(),
    {ok, ChildPid} = supervisor:start_child(SupPid, [Parent]),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(no_worker)
    end,
    Self = make_supervisor_tuple('BT1980AliasChild', bt1980_alias_child_mod, SupPid),
    ChildArg = {beamtalk_object, 'SomeActor', some_mod, ChildPid},
    try
        Result = beamtalk_supervisor:'terminateChild:child:'(Self, ChildArg),
        ?assertEqual({ok, nil}, Result)
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% terminateChild/2 Result-shaped returns (ADR 0080 Phase 1)
%%====================================================================

terminate_child_static_idempotent_double_call_test() ->
    %% A second `terminateChild` call after
    %% the child is gone returns {ok, nil} on the static path. Asserts
    %% the idempotent behavior explicitly.
    SupPid = start_anon_supervisor_with_worker(bt1998_idemp_child),
    Self = make_supervisor_tuple('BT1998Idemp', bt1998_idemp_mod, SupPid),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, bt1998_idemp_child),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg =
            {beamtalk_object, 'BT1998IdempChild class', bt1998_idemp_child_mod, FakeClassPid},
        %% First call — child is running, should return {ok, nil}.
        ?assertEqual(
            {ok, nil},
            beamtalk_supervisor:terminateChild(Self, ClassArg)
        ),
        %% Second call — child is already gone. Previously this would
        %% raise {error, not_found}; now it returns {ok, nil} (idempotent).
        ?assertEqual(
            {ok, nil},
            beamtalk_supervisor:terminateChild(Self, ClassArg)
        )
    after
        FakeClassPid ! stop,
        (try
            gen_server:stop(SupPid)
        catch
            _:_ -> ok
        end)
    end.

terminate_child_static_terminate_failed_test() ->
    %% A non-`not_found` error from supervisor:terminate_child/2 maps to
    %% {error, #beamtalk_error{kind = terminate_failed}}.
    %%
    %% Trigger: terminate by id on a simple_one_for_one supervisor, whose
    %% children have no ids: OTP returns {error, simple_one_for_one}. The
    %% class path no longer reaches this (BT-3966: `undefined` ids never
    %% match a class), so it finds nothing and leaves the child running.
    {ok, SupPid} = supervisor:start_link(
        ?MODULE,
        {simple_one_for_one, #{
            id => dynamic_worker,
            start => {?MODULE, start_worker, []},
            restart => temporary,
            shutdown => brutal_kill,
            type => worker,
            modules => [bt1998_fail_child_mod]
        }}
    ),
    {ok, Child} = supervisor:start_child(SupPid, [self()]),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(worker_not_ready)
    end,
    Self = make_supervisor_tuple('BT1998StaticFail', bt1998_static_fail_mod, SupPid),
    try
        Result = beamtalk_supervisor:terminateChildId(Self, some_id),
        ?assertMatch({error, #beamtalk_error{kind = terminate_failed}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT1998StaticFail', BtError#beamtalk_error.class),
        ?assertEqual('terminateChild:', BtError#beamtalk_error.selector),
        ClassArg = {beamtalk_object, 'BT1998StaticFailChild class', bt1998_fail_child_mod, self()},
        ?assertEqual({ok, nil}, beamtalk_supervisor:terminateChild(Self, ClassArg)),
        ?assert(is_process_alive(Child))
    after
        gen_server:stop(SupPid)
    end.

terminate_child_static_alias_idempotent_test() ->
    %% 'terminateChild:class:'/2 alias also honours the
    %% idempotent not_found → {ok, nil} convention on the static path.
    SupPid = start_anon_supervisor(),
    Self = make_supervisor_tuple('BT1998AliasIdemp', bt1998_alias_idemp_mod, SupPid),
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, bt1998_alias_missing),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    try
        ClassArg =
            {beamtalk_object, 'BT1998AliasMissing class', bt1998_alias_missing_mod, FakeClassPid},
        ?assertEqual(
            {ok, nil},
            beamtalk_supervisor:'terminateChild:class:'(Self, ClassArg)
        )
    after
        FakeClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% startChild/1 error path — returns Result error
%%====================================================================

startChild_arity1_error_returns_child_start_failed_test() ->
    %% When supervisor:start_child returns {error, _}, startChild/1
    %% returns {error, #beamtalk_error{kind = child_start_failed}} instead of
    %% raising. We use start_dynamic_supervisor/0 whose template is
    %% {?MODULE, start_worker, []} — calling start_worker/0 does not exist,
    %% so OTP returns {error, ...}.
    SupPid = start_dynamic_supervisor(),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1980DynErr'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT1980DynErrSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self),
        ?assertMatch({error, #beamtalk_error{kind = child_start_failed}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT1980DynErrSup', BtError#beamtalk_error.class),
        ?assertEqual(startChild, BtError#beamtalk_error.selector)
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% BT-3964: maxChildren limit
%%====================================================================

check_max_children_test() ->
    ?assertEqual({ok, infinity}, beamtalk_supervisor:check_max_children('X', infinity)),
    ?assertEqual({ok, 0}, beamtalk_supervisor:check_max_children('X', 0)),
    ?assertEqual({ok, 3}, beamtalk_supervisor:check_max_children('X', 3)),
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error, class = 'X', selector = maxChildren}},
        beamtalk_supervisor:check_max_children('X', -1)
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error, details = #{value := lots}}},
        beamtalk_supervisor:check_max_children('X', lots)
    ).

%%====================================================================
%% BT-3965: shutdown timeouts
%%====================================================================

to_otp_shutdown_test() ->
    ?assertEqual(infinity, beamtalk_supervisor:to_otp_shutdown('X', shutdown, infinity)),
    ?assertEqual(brutal_kill, beamtalk_supervisor:to_otp_shutdown('X', shutdown, brutalKill)),
    ?assertEqual(0, beamtalk_supervisor:to_otp_shutdown('X', shutdown, 0)),
    ?assertEqual(1500, beamtalk_supervisor:to_otp_shutdown('X', shutdown, 1500)),
    ?assertEqual(
        16#FFFFFFFF, beamtalk_supervisor:to_otp_shutdown('X', shutdown, 16#FFFFFFFF)
    ),
    %% Above the `receive ... after` limit a static supervisor would crash
    %% while shutting its children down.
    ?assertMatch(
        #beamtalk_error{kind = type_error},
        raised_beamtalk_error(fun() ->
            beamtalk_supervisor:to_otp_shutdown('X', shutdown, 16#100000000)
        end)
    ),
    E1 = raised_beamtalk_error(fun() ->
        beamtalk_supervisor:to_otp_shutdown('X', childShutdown, -1)
    end),
    ?assertMatch(#beamtalk_error{kind = type_error, class = 'X', selector = childShutdown}, E1),
    %% Only the Beamtalk spelling is accepted — not OTP's raw atom.
    E2 = raised_beamtalk_error(fun() ->
        beamtalk_supervisor:to_otp_shutdown('X', shutdown, brutal_kill)
    end),
    ?assertMatch(#beamtalk_error{kind = type_error, details = #{value := brutal_kill}}, E2).

spec_to_otp_translates_brutal_kill_test() ->
    %% A Beamtalk `#brutalKill` shutdown reaches OTP as `brutal_kill`.
    ClassObj = bt1990_make_counter_class_obj(),
    try
        BtSpec = #{
            id => 'TestCounter',
            start => #{
                '$beamtalk_class' => 'Array',
                data => #{0 => ClassObj, 1 => spawn, 2 => []}
            },
            restart => temporary,
            shutdown => brutalKill,
            type => worker
        },
        ?assertMatch(#{shutdown := brutal_kill}, beamtalk_supervisor:spec_to_otp(BtSpec)),
        ?assertMatch(
            #{shutdown := infinity},
            beamtalk_supervisor:spec_to_otp(BtSpec#{shutdown => infinity})
        ),
        Err = raised_beamtalk_error(fun() ->
            beamtalk_supervisor:spec_to_otp(BtSpec#{shutdown => soon})
        end),
        ?assertMatch(
            #beamtalk_error{kind = type_error, class = 'SupervisionSpec', selector = shutdown},
            Err
        )
    after
        bt1990_cleanup_class_obj(ClassObj)
    end.

start_capped_dynamic_supervisor(Max) ->
    ChildSpec = #{
        id => fake_child_template,
        start => {?MODULE, start_link_fake_child, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    {ok, SupPid} = supervisor:start_link(?MODULE, {simple_one_for_one, ChildSpec, Max}),
    SupPid.

startChild_max_children_test() ->
    %% At the limit startChild/1 returns {error, max_children} carrying the
    %% limit in details, without starting a child; freeing a slot lets the
    %% next start succeed.
    SupPid = start_capped_dynamic_supervisor(2),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT3964DynWorker'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3964CapSup', ?MODULE, SupPid},
        {ok, C1} = beamtalk_supervisor:startChild(Self),
        {ok, _C2} = beamtalk_supervisor:startChild(Self),
        Result = beamtalk_supervisor:startChild(Self),
        ?assertMatch({error, #beamtalk_error{kind = max_children}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT3964CapSup', BtError#beamtalk_error.class),
        ?assertEqual(startChild, BtError#beamtalk_error.selector),
        ?assertEqual(#{max_children => 2}, BtError#beamtalk_error.details),
        ?assertEqual({ok, 2}, beamtalk_supervisor:countChildren(Self)),
        ok = supervisor:terminate_child(SupPid, element(4, C1)),
        ?assertMatch({ok, _}, beamtalk_supervisor:startChild(Self))
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_max_children_zero_test() ->
    %% maxChildren = 0 refuses every start, including startChild/2 and /3.
    SupPid = start_capped_dynamic_supervisor(0),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT3964DynZero'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3964ZeroSup', ?MODULE, SupPid},
        ?assertMatch(
            {error, #beamtalk_error{kind = max_children, selector = 'startChild:'}},
            beamtalk_supervisor:startChild(Self, self())
        ),
        ?assertMatch(
            {error, #beamtalk_error{kind = max_children, selector = 'startChild:name:'}},
            beamtalk_supervisor:startChild(Self, self(), bt3964_never_registered)
        ),
        ?assertEqual(undefined, whereis(bt3964_never_registered))
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% BT-4068: atomic maxChildren enforcement inside the supervisor
%%====================================================================

start_enforced_capped_dynamic_supervisor(Max) ->
    start_enforced_capped_dynamic_supervisor(Max, temporary).

start_enforced_capped_dynamic_supervisor(Max, Restart) ->
    ChildSpec = #{
        id => fake_child_template,
        start => {?MODULE, start_link_fake_child, []},
        restart => Restart,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    {ok, SupPid} = supervisor:start_link(?MODULE, {simple_one_for_one, ChildSpec, {capped, Max}}),
    SupPid.

%% Poll a pool's children for one outside `Exclude`, e.g. a restarted child.
await_pool_child(SupPid, Exclude) ->
    await_pool_child(SupPid, Exclude, 200).

await_pool_child(_SupPid, _Exclude, 0) ->
    error(pool_child_timeout);
await_pool_child(SupPid, Exclude, N) ->
    Pids = [
        P
     || {_, P, _, _} <- supervisor:which_children(SupPid),
        is_pid(P),
        not lists:member(P, Exclude)
    ],
    case Pids of
        [New | _] ->
            New;
        [] ->
            timer:sleep(10),
            await_pool_child(SupPid, Exclude, N - 1)
    end.

%% Poll a capacity table until its counter reads `Expected` (a death's DOWN
%% is processed asynchronously, so the count settles a moment after the
%% restart it overlaps).
await_capacity_count(Tid, Expected) ->
    await_capacity_count(Tid, Expected, 200).

await_capacity_count(_Tid, Expected, 0) ->
    error({capacity_count_timeout, Expected});
await_capacity_count(Tid, Expected, N) ->
    case ets:lookup_element(Tid, count, 2) of
        Expected ->
            ok;
        _ ->
            timer:sleep(10),
            await_capacity_count(Tid, Expected, N - 1)
    end.

%% Trace `supervisor:count_children/1` and `supervisor:start_child/2` calls
%% (global patterns; only explicitly traced processes report). Callers must
%% pair `trace_sup_start_calls/0` with `untrace_sup_start_calls/0`.
trace_sup_start_calls() ->
    1 = erlang:trace_pattern({supervisor, count_children, 1}, true, [global]),
    1 = erlang:trace_pattern({supervisor, start_child, 2}, true, [global]),
    %% Drop any stale trace messages an earlier failed test left behind, so
    %% the drain below only sees this test's calls.
    _ = drain_sup_start_calls(),
    ok.

untrace_sup_start_calls() ->
    _ = erlang:trace_pattern({supervisor, count_children, 1}, false, [global]),
    _ = erlang:trace_pattern({supervisor, start_child, 2}, false, [global]),
    ok.

%% Spawn a gated starter: it runs `Fun` once released with `go`, then
%% reports to `Test`. A crashing starter is reported as a failure, not a
%% hung collect.
spawn_gated_starter(Test, Fun) ->
    spawn_monitor(fun() ->
        receive
            go -> ok
        end,
        Test ! {starter_done, self(), Fun()}
    end).

%% A gated starter additionally traced for supervisor calls (the patterns
%% from `trace_sup_start_calls/0` must be armed first).
spawn_traced_starter(Test, Fun) ->
    {Pid, _Ref} = Gated = spawn_gated_starter(Test, Fun),
    1 = erlang:trace(Pid, true, [call, {tracer, Test}]),
    Gated.

release_and_collect_starters(Starters) ->
    [P ! go || {P, _} <- Starters],
    [
        receive
            {starter_done, P, R} ->
                _ = erlang:demonitor(Ref, [flush]),
                R;
            {'DOWN', Ref, process, P, Reason} ->
                error({starter_crashed, P, Reason})
        after 30000 ->
            error({starter_timeout, P})
        end
     || {P, Ref} <- Starters
    ].

%% Drain supervisor-call trace messages until 200 ms of quiet; returns
%% `{CountChildrenCalls, StartChildCalls}`.
drain_sup_start_calls() ->
    drain_sup_start_calls(0, 0).

drain_sup_start_calls(CountChildren, StartChild) ->
    receive
        {trace, _, call, {supervisor, count_children, _}} ->
            drain_sup_start_calls(CountChildren + 1, StartChild);
        {trace, _, call, {supervisor, start_child, _}} ->
            drain_sup_start_calls(CountChildren, StartChild + 1)
    after 200 ->
        {CountChildren, StartChild}
    end.

startChild_max_children_concurrent_test() ->
    %% N parallel startChild/1 calls admit exactly Max; the rest get
    %% max_children. The atomic in-supervisor check can never overshoot.
    Max = 8,
    N = 64,
    SupPid = start_enforced_capped_dynamic_supervisor(Max),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynWorker'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT4068CapSup', ?MODULE, SupPid},
        Starters = [
            spawn_gated_starter(self(), fun() -> beamtalk_supervisor:startChild(Self) end)
         || _ <- lists:seq(1, N)
        ],
        Results = release_and_collect_starters(Starters),
        Ok = [R || {ok, _} = R <- Results],
        Refused = [R || {error, #beamtalk_error{kind = max_children}} = R <- Results],
        ?assertEqual(Max, length(Ok)),
        ?assertEqual(N - Max, length(Refused)),
        ?assertEqual(length(Results), length(Ok) + length(Refused)),
        [?assertEqual(#{max_children => Max}, E#beamtalk_error.details) || {error, E} <- Refused],
        ?assertEqual({ok, Max}, beamtalk_supervisor:countChildren(Self)),
        Counts = supervisor:count_children(SupPid),
        ?assertEqual(Max, proplists:get_value(workers, Counts, 0))
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_max_children_single_call_test() ->
    %% Each startChild on a capped pool is exactly one call into the
    %% supervisor (start_child), with no count_children round trip — for
    %% admitted and refused starts alike, on every arity.
    Max = 2,
    SupPid = start_enforced_capped_dynamic_supervisor(Max),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynCalls'),
    set_child_class(SupPid, ChildClassObj),
    trace_sup_start_calls(),
    try
        Self = {beamtalk_supervisor, 'BT4068CallSup', ?MODULE, SupPid},
        Starters = [
            spawn_traced_starter(self(), fun() -> beamtalk_supervisor:startChild(Self) end),
            spawn_traced_starter(self(), fun() -> beamtalk_supervisor:startChild(Self) end),
            spawn_traced_starter(self(), fun() -> beamtalk_supervisor:startChild(Self) end),
            spawn_traced_starter(
                self(), fun() -> beamtalk_supervisor:startChild(Self, #{n => 1}) end
            ),
            spawn_traced_starter(
                self(), fun() -> beamtalk_supervisor:startChild(Self, #{n => 2}, bt4068_traced) end
            )
        ],
        %% Phased release: the first two fill the pool, the rest refuse.
        {Admit, Refuse} = lists:split(2, Starters),
        [First, Second] = release_and_collect_starters(Admit),
        ?assertMatch({ok, _}, First),
        ?assertMatch({ok, _}, Second),
        [Third, Fourth, Fifth] = release_and_collect_starters(Refuse),
        ?assertMatch({error, #beamtalk_error{kind = max_children}}, Third),
        ?assertMatch(
            {error, #beamtalk_error{kind = max_children, selector = 'startChild:'}}, Fourth
        ),
        ?assertMatch(
            {error, #beamtalk_error{kind = max_children, selector = 'startChild:name:'}}, Fifth
        ),
        %% Five starts, five start_child calls, zero count_children calls.
        ?assertEqual({0, 5}, drain_sup_start_calls())
    after
        untrace_sup_start_calls(),
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_legacy_cap_still_counts_test() ->
    %% A capped pool WITHOUT enforcement state (started before BT-4068, or a
    %% fixture that only stashes the limit) keeps the BT-3964 caller-side
    %% check: startChild calls supervisor:count_children/1 first.
    SupPid = start_capped_dynamic_supervisor(2),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynLegacy'),
    set_child_class(SupPid, ChildClassObj),
    trace_sup_start_calls(),
    try
        Self = {beamtalk_supervisor, 'BT4068LegacySup', ?MODULE, SupPid},
        Starter = spawn_traced_starter(self(), fun() -> beamtalk_supervisor:startChild(Self) end),
        [Result] = release_and_collect_starters([Starter]),
        ?assertMatch({ok, _}, Result),
        ?assertEqual({1, 1}, drain_sup_start_calls())
    after
        untrace_sup_start_calls(),
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_max_children_arg_threading_test() ->
    %% The wrapper strips the capacity token and threads the caller's args to
    %% the template untouched: startChild: delivers [Args], startChild:name:
    %% delivers [Args, Name].
    SupPid = start_enforced_capped_dynamic_supervisor(2),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynArgs'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT4068ArgsSup', ?MODULE, SupPid},
        ?assertMatch({ok, _}, beamtalk_supervisor:startChild(Self, #{n => 1})),
        ?assertEqual([#{n => 1}], last_fake_child_args()),
        ?assertMatch({ok, _}, beamtalk_supervisor:startChild(Self, #{n => 2}, bt4068_named)),
        ?assertEqual([#{n => 2}, bt4068_named], last_fake_child_args())
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_max_children_terminate_releases_slot_test() ->
    %% terminateChild: frees the slot synchronously: a startChild immediately
    %% after always succeeds, with no async-DOWN window to flake on.
    %% Repeated rounds so a missing sync release would almost surely flake.
    Max = 2,
    SupPid = start_enforced_capped_dynamic_supervisor(Max),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynTerm'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT4068TermSup', ?MODULE, SupPid},
        {ok, C1} = beamtalk_supervisor:startChild(Self),
        {ok, _C2} = beamtalk_supervisor:startChild(Self),
        ?assertMatch(
            {error, #beamtalk_error{kind = max_children}}, beamtalk_supervisor:startChild(Self)
        ),
        lists:foreach(
            fun(_) ->
                {ok, nil} = beamtalk_supervisor:terminateChild(Self, C1),
                {ok, Next} = beamtalk_supervisor:startChild(Self),
                %% The pool is full again immediately: the freed slot went to
                %% exactly one start.
                ?assertMatch(
                    {error, #beamtalk_error{kind = max_children}},
                    beamtalk_supervisor:startChild(Self)
                ),
                {ok, nil} = beamtalk_supervisor:terminateChild(Self, Next)
            end,
            lists:seq(1, 20)
        )
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

startChild_max_children_restart_replays_original_args_test() ->
    %% An OTP restart replays the tokened start args; the wrapper recognises
    %% the consumed token, strips it, and restarts the child with its
    %% ORIGINAL args — the token must never leak into the child's start.
    SupPid = start_enforced_capped_dynamic_supervisor(2, permanent),
    {ChildClassPid, ChildClassObj} = make_child_class_for_startchild('BT4068DynRestart'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT4068RestartSup', ?MODULE, SupPid},
        {ok, C1} = beamtalk_supervisor:startChild(Self),
        {ok, C2} = beamtalk_supervisor:startChild(Self, #{n => 7}),
        ?assertEqual([#{n => 7}], last_fake_child_args()),
        C1Pid = element(4, C1),
        C2Pid = element(4, C2),
        %% The no-args child restarts through the nullary start (recording
        %% nothing): the recorded args are untouched.
        kill_and_await_down(C1Pid),
        C1New = await_pool_child(SupPid, [C1Pid, C2Pid]),
        ?assertEqual([#{n => 7}], last_fake_child_args()),
        %% The with-args child restarts with its original args.
        kill_and_await_down(C2Pid),
        _C2New = await_pool_child(SupPid, [C1Pid, C2Pid, C1New]),
        ?assertEqual([#{n => 7}], last_fake_child_args()),
        ?assertEqual({ok, 2}, beamtalk_supervisor:countChildren(Self)),
        {{dictionary, '$beamtalk_max_children_state'}, {Tid, _MonPid}} =
            erlang:process_info(SupPid, {dictionary, '$beamtalk_max_children_state'}),
        ok = await_capacity_count(Tid, 2)
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

kill_and_await_down(Pid) ->
    Mon = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, _} ->
            demonitor(Mon, [flush]),
            ok
    after 5000 ->
        error(child_not_dead)
    end.

dynamic_init_installs_capacity_state_test() ->
    %% A pool started through the real dynamic_init/3 carries enforcement
    %% state and a wrapped template; with maxChildren = 0 every startChild
    %% arity refuses without starting anything (the child module does not
    %% even exist — refusal happens before the template MFA runs).
    {ChildPid, ChildClass} = bt3963_supervisor_child_class('BT4068DynChildSup'),
    Config = bt3963_dynamic_config(ChildClass, #{maxChildren => 0}),
    try
        with_bt3963_class('BT3963Pool', Config, fun() ->
            {ok, P} = beamtalk_bt3963_sup_fixture:start_link(nil),
            try
                {{dictionary, '$beamtalk_max_children_state'}, {Tid, MonPid}} =
                    erlang:process_info(P, {dictionary, '$beamtalk_max_children_state'}),
                ?assert(is_pid(MonPid)),
                ?assert(is_process_alive(MonPid)),
                ?assertEqual(0, ets:lookup_element(Tid, count, 2)),
                {ok, Spec} = supervisor:get_childspec(P, 'BT4068DynChildSup'),
                ?assertMatch(
                    {beamtalk_supervisor, checked_dynamic_start, [_]},
                    maps:get(start, Spec)
                ),
                Self = bt3963_sup('BT3963Pool', P),
                ?assertMatch(
                    {error, #beamtalk_error{kind = max_children, selector = startChild}},
                    beamtalk_supervisor:startChild(Self)
                ),
                ?assertMatch(
                    {error, #beamtalk_error{kind = max_children, selector = 'startChild:'}},
                    beamtalk_supervisor:startChild(Self, #{n => 1})
                ),
                ?assertMatch(
                    {error, #beamtalk_error{kind = max_children, selector = 'startChild:name:'}},
                    beamtalk_supervisor:startChild(Self, #{n => 1}, bt4068_never)
                ),
                ?assertEqual(0, ets:lookup_element(Tid, count, 2)),
                ?assertEqual(undefined, whereis(bt4068_never))
            after
                stop_linked(P)
            end
        end)
    after
        ChildPid ! stop,
        beamtalk_class_metadata:delete('BT4068DynChildSup')
    end.

dynamic_init_failing_child_releases_slot_test() ->
    %% A start whose child fails to start releases its slot: repeated failing
    %% starts never fill the pool, and never report max_children.
    {ChildPid, ChildClass} = bt3963_supervisor_child_class('BT4068DynFailSup'),
    Config = bt3963_dynamic_config(ChildClass, #{maxChildren => 1}),
    try
        with_bt3963_class('BT3963Pool', Config, fun() ->
            {ok, P} = beamtalk_bt3963_sup_fixture:start_link(nil),
            try
                {{dictionary, '$beamtalk_max_children_state'}, {Tid, _MonPid}} =
                    erlang:process_info(P, {dictionary, '$beamtalk_max_children_state'}),
                Self = bt3963_sup('BT3963Pool', P),
                %% bt3963_child_mod does not exist, so the child start fails.
                ?assertMatch(
                    {error, #beamtalk_error{kind = child_start_failed}},
                    beamtalk_supervisor:startChild(Self)
                ),
                ?assertEqual(0, ets:lookup_element(Tid, count, 2)),
                ?assertMatch(
                    {error, #beamtalk_error{kind = child_start_failed}},
                    beamtalk_supervisor:startChild(Self)
                ),
                ?assertEqual(0, ets:lookup_element(Tid, count, 2))
            after
                stop_linked(P)
            end
        end)
    after
        ChildPid ! stop,
        beamtalk_class_metadata:delete('BT4068DynFailSup')
    end.

%%====================================================================
%% startChild/1,2 stale-handle Result error
%%====================================================================

startChild_arity1_stale_handle_test() ->
    %% startChild/1 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}} via with_live_supervisor/3.
    SupPid = start_dynamic_supervisor(),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1997StaleStart'),
    set_child_class(SupPid, ChildClassObj),
    gen_server:stop(SupPid),
    timer:sleep(20),
    try
        Self = {beamtalk_supervisor, 'BT1997StaleStartSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self),
        ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT1997StaleStartSup', BtError#beamtalk_error.class),
        ?assertEqual(startChild, BtError#beamtalk_error.selector)
    after
        ChildClassPid ! stop
    end.

startChild_arity2_stale_handle_test() ->
    %% startChild/2 on a dead supervisor returns
    %% {error, #beamtalk_error{kind = stale_handle}}.
    SupPid = start_dynamic_supervisor(),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1997StaleStart2'),
    set_child_class(SupPid, ChildClassObj),
    gen_server:stop(SupPid),
    timer:sleep(20),
    try
        Self = {beamtalk_supervisor, 'BT1997StaleStart2Sup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self, self()),
        ?assertMatch({error, #beamtalk_error{kind = stale_handle}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT1997StaleStart2Sup', BtError#beamtalk_error.class),
        ?assertEqual('startChild:', BtError#beamtalk_error.selector)
    after
        ChildClassPid ! stop
    end.

startChild_arity2_error_returns_child_start_failed_test() ->
    %% startChild/2 propagates an OTP start failure as
    %% {error, #beamtalk_error{kind = child_start_failed}}. We build a
    %% simple_one_for_one supervisor whose template is
    %% {?MODULE, start_worker, []} (arity-0 does not exist — only
    %% start_worker/1 is defined). Supplying Args = self() would call
    %% start_worker(self()) and succeed, so we instead use a template with
    %% a function that does not exist in the test module.
    ChildSpec = #{
        id => bt1997_missing_start,
        start => {?MODULE, this_function_does_not_exist, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    },
    {ok, SupPid} = supervisor:start_link(?MODULE, {simple_one_for_one, ChildSpec}),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1997ChildErr'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT1997ChildErrSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self, self()),
        ?assertMatch({error, #beamtalk_error{kind = child_start_failed}}, Result),
        {error, BtError} = Result,
        ?assertEqual('BT1997ChildErrSup', BtError#beamtalk_error.class),
        ?assertEqual('startChild:', BtError#beamtalk_error.selector)
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% with_live_supervisor/3 contract — inner fun Result passthrough
%%====================================================================

with_live_supervisor_passes_through_ok_test() ->
    %% when the inner fun returns {ok, V}, with_live_supervisor
    %% returns it unchanged.
    SupPid = start_anon_supervisor(),
    try
        Result = beamtalk_supervisor:whichChildren(
            make_supervisor_tuple('BT1997Pass', test_pass_mod, SupPid)
        ),
        ?assertMatch({ok, _}, Result),
        {ok, Ids} = Result,
        ?assertEqual([], Ids)
    after
        gen_server:stop(SupPid)
    end.

with_live_supervisor_passes_through_error_test() ->
    %% when supervisor:start_child returns {error, Reason}, the
    %% inner fun builds {error, BtError} and with_live_supervisor returns it
    %% unchanged (no wrapping, no double-tagging).
    SupPid = start_dynamic_supervisor(),
    {ChildClassPid, ChildClassObj} =
        make_child_class_for_startchild('BT1997Pass2'),
    set_child_class(SupPid, ChildClassObj),
    try
        Self = {beamtalk_supervisor, 'BT1997Pass2Sup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self),
        %% Unwrap one layer — should be a {error, BtError} tagged tuple,
        %% not a doubly-wrapped {error, {error, ...}} or similar.
        ?assertMatch({error, #beamtalk_error{}}, Result)
    after
        ChildClassPid ! stop,
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% ensure_root_table concurrent creation
%%====================================================================

%% concurrent get_root/0 safety is exercised implicitly by the other
%% supervisor tests in this module. A dedicated concurrent test that deletes
%% the root ETS table and races readers is removed because get_root/0 is not
%% designed to handle a deleted table — ets:lookup_element raises badarg by
%% design and the test proved flaky under CI load.

%%====================================================================
%% Helpers for class_dispatch hook tests
%%====================================================================

%% Minimal runtime setup for class_dispatch tests that use beamtalk_object_class.
%% Ensures ETS tables and registry are available.
setup_class_dispatch_runtime() ->
    beamtalk_class_metadata:new(),
    %% Return an opaque context for teardown.
    ok.

teardown_class_dispatch_runtime(_Ctx) ->
    ok.

%%====================================================================
%% ADR 0079 Phase 3: named child specs + restart survival
%%====================================================================

%% Helper: poll until erlang:whereis(Name) is a pid (the supervisor has
%% restarted the child) or the deadline expires.
bt1990_wait_for_registration(Name, Except, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    bt1990_wait_for_registration_loop(Name, Except, Deadline).

bt1990_wait_for_registration_loop(Name, Except, Deadline) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid), Pid =/= Except ->
            Pid;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    error({registration_timeout, Name, Except});
                false ->
                    timer:sleep(5),
                    bt1990_wait_for_registration_loop(Name, Except, Deadline)
            end
    end.

bt1990_cleanup_name(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            ok;
        _ ->
            catch erlang:unregister(Name),
            ok
    end.

spec_to_otp_spawnAs_translates_to_beamtalk_actor_spawnAs_test() ->
    %% ADR 0079: `SupervisionSpec withName:` emits a Beamtalk
    %% childSpec with startFn = #spawnAs: and startArgs = #(Name). The
    %% supervisor runtime must translate that into the MFA
    %% `{beamtalk_supervisor, start_actor_child, [Module, #{}, {local, Name}]}`
    %% (BT-4033: the awaited start, through `beamtalk_actor:spawnAs/3`) so OTP restart re-
    %% registers the name atomically. We construct the dispatchable
    %% Beamtalk spec by hand (equivalent to what Supervisor childSpec
    %% returns) and run it through build_child_specs/1.
    Name = bt1990_spec_spawnas,
    bt1990_cleanup_name(Name),
    %% Drive build_child_specs via a real Beamtalk message send — the
    %% easiest way to get the SupervisionSpec>>childSpec shape without
    %% reaching into private helpers is to call spec_to_otp through a
    %% synthesised tagged-map that responds to `childSpec` by returning a
    %% ready-made Beamtalk child-spec dict. We emulate that by posting
    %% the pre-computed child spec directly through build_child_specs/1
    %% using the hand-built Beamtalk Array shape.
    ClassObj = bt1990_make_counter_class_obj(),
    try
        StartArray = #{
            '$beamtalk_class' => 'Array',
            data => #{0 => ClassObj, 1 => 'spawnAs:', 2 => [Name]}
        },
        BtSpec = #{
            id => 'TestCounter',
            start => StartArray,
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        %% The `dispatch` path calls SpecMap childSpec — but when the
        %% receiver is already the dict we want, we can skip that by
        %% calling the internal flow through a carrier map that
        %% auto-responds to childSpec via a dispatcher shim.
        ChildSpecList = bt1990_build_from_bt_spec(BtSpec),
        [OtpSpec] = ChildSpecList,
        ?assertMatch(
            #{
                start :=
                    {beamtalk_supervisor, start_actor_child, [test_counter, #{}, {local, Name}]}
            },
            OtpSpec
        )
    after
        bt1990_cleanup_class_obj(ClassObj)
    end.

spec_to_otp_spawnWithAs_translates_to_beamtalk_actor_spawnAs_arity3_test() ->
    %% Mirror of the above for the `#spawnWith:as:` path — startArgs are
    %% `#(args, name)` → Erlang list `[Args, Name]`, translated to
    %% `{beamtalk_supervisor, start_actor_child, [Module, Args, {local, Name}]}`.
    Name = bt1990_spec_spawnwithas,
    bt1990_cleanup_name(Name),
    ClassObj = bt1990_make_counter_class_obj(),
    try
        InitArgs = #{value => 42},
        StartArray = #{
            '$beamtalk_class' => 'Array',
            data => #{0 => ClassObj, 1 => 'spawnWith:as:', 2 => [InitArgs, Name]}
        },
        BtSpec = #{
            id => 'TestCounter',
            start => StartArray,
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        [OtpSpec] = bt1990_build_from_bt_spec(BtSpec),
        ?assertMatch(
            #{
                start :=
                    {beamtalk_supervisor, start_actor_child, [
                        test_counter, InitArgs, {local, Name}
                    ]}
            },
            OtpSpec
        )
    after
        bt1990_cleanup_class_obj(ClassObj)
    end.

%%====================================================================
%% spec_to_otp/2 dynamic-mode #spawn — zero-static-arg MFA
%%====================================================================

spec_to_otp_dynamic_spawn_uses_zero_static_arg_mfa_test() ->
    %% for a DynamicSupervisor's simple_one_for_one template
    %% (Mode = dynamic), the default #spawn startFn must NOT bake `[#{}]`
    %% directly into `{ChildModule, start_link, _}` — supervisor:start_child/2
    %% appends whatever extra args the caller passed on top of that fixed
    %% list, so a baked one-element list plus an appended `startChild: args`
    %% element produced a 2-arity call that OTP misread as the named-
    %% registration start_link/4 form and crashed with badarg. Instead the
    %% MFA must route through the zero-static-arg start_dynamic_child/2,3
    %% indirection.
    ClassObj = bt1990_make_counter_class_obj(),
    try
        StartArray = #{
            '$beamtalk_class' => 'Array',
            data => #{0 => ClassObj, 1 => spawn, 2 => []}
        },
        BtSpec = #{
            id => 'TestCounter',
            start => StartArray,
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        OtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec, dynamic),
        ?assertMatch(
            #{start := {beamtalk_supervisor, start_dynamic_child, [test_counter, #{}]}},
            OtpSpec
        ),
        %% Mode defaults to `static` (spec_to_otp/1): a baked arg, since static
        %% Supervisor children start once at supervisor-init time and never
        %% have extra args appended; the awaited start (BT-4033).
        StaticOtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec),
        ?assertMatch(
            #{start := {beamtalk_supervisor, start_actor_child, [test_counter, #{}]}},
            StaticOtpSpec
        )
    after
        bt1990_cleanup_class_obj(ClassObj)
    end.

spec_to_otp_dynamic_spawnWith_uses_zero_static_arg_mfa_test() ->
    %% A DynamicSupervisor childClass can override
    %% `class supervisionSpec` to bake default args via `withArgs:`, which
    %% also compiles to startFn #spawnWith: (not just the plain #spawn
    %% case above). That hits the exact same arity-mismatch badarg once
    %% `startChild: args` appends its own args on top of the baked ones, so
    %% it needs the same zero-static-arg indirection — using the baked map
    %% as start_dynamic_child's DefaultArgs instead of #{}.
    ClassObj = bt1990_make_counter_class_obj(),
    try
        BakedArgs = #{value => 7},
        StartArray = #{
            '$beamtalk_class' => 'Array',
            data => #{0 => ClassObj, 1 => 'spawnWith:', 2 => [BakedArgs]}
        },
        BtSpec = #{
            id => 'TestCounter',
            start => StartArray,
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        OtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec, dynamic),
        ?assertMatch(
            #{start := {beamtalk_supervisor, start_dynamic_child, [test_counter, BakedArgs]}},
            OtpSpec
        ),
        %% Mode defaults to `static` and stays on the original shape.
        StaticOtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec),
        ?assertMatch(
            #{start := {beamtalk_supervisor, start_actor_child, [test_counter, BakedArgs]}},
            StaticOtpSpec
        )
    after
        bt1990_cleanup_class_obj(ClassObj)
    end.

startChild_arity2_with_dynamic_spawnWith_spec_delivers_args_test() ->
    %% Regression: startChild: args on a real
    %% simple_one_for_one supervisor built from a dynamic-mode #spawnWith:
    %% spec (baked default args) must start the child with the caller's
    %% args, not the baked default, and not crash with badarg.
    ClassObj = bt1990_make_counter_class_obj(),
    set_child_class(ClassObj),
    %% test_counter:init/1 stores its single arg directly as `value` (unlike
    %% a real compiled actor's spawnWith:, which merges an args map into
    %% default state) — so the baked default here is the bare term 7, not a
    %% map, matching how the other startChild_arity2_* tests in this file
    %% pass a bare term (e.g. self()) through the same path.
    BakedArgs = 7,
    StartArray = #{
        '$beamtalk_class' => 'Array',
        data => #{0 => ClassObj, 1 => 'spawnWith:', 2 => [BakedArgs]}
    },
    BtSpec = #{
        id => 'TestCounter',
        start => StartArray,
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    OtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec, dynamic),
    SupFlags = #{strategy => simple_one_for_one, intensity => 3, period => 5},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [OtpSpec]}),
    try
        Self = {beamtalk_supervisor, 'BT3365SpawnWithSup', ?MODULE, SupPid},

        %% No-arg startChild uses the baked default (7).
        {ok, DefaultChild} = beamtalk_supervisor:startChild(Self),
        DefaultPid = element(4, DefaultChild),
        DefaultProxy = #beamtalk_object{
            class = 'Counter', class_mod = test_counter, pid = DefaultPid
        },
        ?assertEqual(7, beamtalk_message_dispatch:send(DefaultProxy, getValue, [])),

        %% startChild: args uses the caller's args (99), not the baked
        %% default and not a badarg crash.
        Result = beamtalk_supervisor:startChild(Self, 99),
        ?assertMatch({ok, {beamtalk_object, 'Counter', test_counter, _}}, Result),
        {ok, Child} = Result,
        ChildPid = element(4, Child),
        Proxy = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = ChildPid},
        ?assertEqual(99, beamtalk_message_dispatch:send(Proxy, getValue, []))
    after
        bt1990_cleanup_class_obj(ClassObj),
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% DynamicSupervisor(C)>>startChild: with a child needing init args
%%====================================================================

%% Build a real simple_one_for_one supervisor whose single child template is
%% the OTP spec `build_child_specs/2` (Mode = dynamic) would produce for a
%% plain (#spawn) worker child pointing at test_counter — i.e. exactly what
%% `dynamic_init/3` wires up for a live `DynamicSupervisor(Counter)`.
%% Also records ClassObj as the pool's child class (set_child_class/1, planted
%% by this module's init/1 the way dynamic_init/3 stores it), which
%% startChild/1,2 read back through pool_child_class/1.
bt3365_start_dynamic_supervisor(ClassObj) ->
    set_child_class(ClassObj),
    StartArray = #{
        '$beamtalk_class' => 'Array',
        data => #{0 => ClassObj, 1 => spawn, 2 => []}
    },
    BtSpec = #{
        id => 'TestCounter',
        start => StartArray,
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    OtpSpec = beamtalk_supervisor:spec_to_otp(BtSpec, dynamic),
    SupFlags = #{strategy => simple_one_for_one, intensity => 3, period => 5},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [OtpSpec]}),
    SupPid.

startChild_arity2_with_real_dynamic_spec_delivers_args_test() ->
    %% Regression: startChild: args on a real simple_one_for_one
    %% supervisor built from the dynamic-mode spec must start the child
    %% with exactly the caller's args, not crash with badarg.
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3365DynSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self, 42),
        ?assertMatch({ok, {beamtalk_object, 'Counter', test_counter, _}}, Result),
        {ok, Child} = Result,
        ChildPid = element(4, Child),
        Proxy = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = ChildPid},
        ?assertEqual(42, beamtalk_message_dispatch:send(Proxy, getValue, []))
    after
        bt1990_cleanup_class_obj(ClassObj),
        gen_server:stop(SupPid)
    end.

startChild_arity1_with_real_dynamic_spec_still_works_test() ->
    %% Regression guard: the no-arg startChild path must keep
    %% working once #spawn routes through the zero-static-arg
    %% start_dynamic_child/2,3 indirection instead of a baked [#{}].
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3365DynSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self),
        ?assertMatch({ok, {beamtalk_object, 'Counter', test_counter, _}}, Result),
        {ok, Child} = Result,
        ChildPid = element(4, Child),
        ?assert(is_process_alive(ChildPid))
    after
        bt1990_cleanup_class_obj(ClassObj),
        gen_server:stop(SupPid)
    end.

startChild_arity2_restart_replays_original_call_args_test() ->
    %% verify (and pin down) OTP's actual simple_one_for_one restart
    %% semantics for a child started via startChild: args. OTP's supervisor
    %% stores, per dynamically-started child, the *exact* args used at start
    %% time (`StaticArgs ++ EArgs` from the `start_child/2` call, see
    %% `supervisor:dyn_store/3` and `find_child_and_args/2` in OTP's
    %% supervisor.erl) and replays those same args verbatim on automatic
    %% restart — not just the bare static template. So a child started with
    %% `startChild: 42` restarts as `start_dynamic_child(ChildModule, #{}, 42)`
    %% (arity 3, `Args = 42` wins) again, coming back with the SAME 42, not a
    %% blank default. (A child started via the plain no-arg `startChild`
    %% restarts via the arity-2 clause instead, since its stored EArgs was
    %% `[]` — that one *does* come back with DefaultArgs, because that is
    %% what it was started with in the first place.)
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3365DynSup', ?MODULE, SupPid},
        {ok, Child} = beamtalk_supervisor:startChild(Self, 42),
        ChildPid1 = element(4, Child),
        Proxy1 = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = ChildPid1},
        ?assertEqual(42, beamtalk_message_dispatch:send(Proxy1, getValue, [])),

        exit(ChildPid1, kill),
        ChildPid2 = bt3365_wait_for_restart(SupPid, ChildPid1, 2000),
        ?assertNotEqual(ChildPid1, ChildPid2),
        Proxy2 = #beamtalk_object{class = 'Counter', class_mod = test_counter, pid = ChildPid2},
        %% Restarted with the original startChild: 42 call's args, not a
        %% blank default — OTP's own simple_one_for_one bookkeeping.
        ?assertEqual(42, beamtalk_message_dispatch:send(Proxy2, getValue, []))
    after
        bt1990_cleanup_class_obj(ClassObj),
        gen_server:stop(SupPid)
    end.

bt3365_wait_for_restart(SupPid, OldPid, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    bt3365_wait_for_restart_loop(SupPid, OldPid, Deadline).

bt3365_wait_for_restart_loop(SupPid, OldPid, Deadline) ->
    case [Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid), is_pid(Pid)] of
        [NewPid] when NewPid =/= OldPid ->
            NewPid;
        _ ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true ->
                    error(restart_timeout);
                false ->
                    timer:sleep(5),
                    bt3365_wait_for_restart_loop(SupPid, OldPid, Deadline)
            end
    end.

%%====================================================================
%% ADR 0079 amendment: DynamicSupervisor>>startChild:name:
%%====================================================================

startChild_arity3_with_real_dynamic_spec_registers_name_test() ->
    %% startChild: args name: aName on a real simple_one_for_one
    %% supervisor must start the child registered under aName — reusing
    %% the exact same dynamic-mode spec the arity-2 tests above use, since
    %% start_dynamic_child/4 shares the same zero-static-arg MFA template.
    Name = bt3376_dyn_name,
    bt1990_cleanup_name(Name),
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3376DynSup', ?MODULE, SupPid},
        Result = beamtalk_supervisor:startChild(Self, 42, Name),
        ?assertMatch({ok, {beamtalk_object, 'Counter', test_counter, _}}, Result),
        {ok, Child} = Result,
        ChildPid = element(4, Child),
        ?assertEqual(ChildPid, erlang:whereis(Name)),
        Proxy = #beamtalk_object{
            class = 'Counter', class_mod = test_counter, pid = {registered, Name}
        },
        ?assertEqual(42, beamtalk_message_dispatch:send(Proxy, getValue, []))
    after
        bt1990_cleanup_class_obj(ClassObj),
        bt1990_cleanup_name(Name),
        gen_server:stop(SupPid)
    end.

startChild_arity3_restart_reregisters_same_name_test() ->
    %% The load-bearing regression test (mirrors
    %% `supervisor_restart_re_registers_name_test/0` for the dynamic case):
    %% a child started via `startChild: args name: aName` that later
    %% crashes must come back under a DIFFERENT pid but the SAME registered
    %% name — proving `DynamicSupervisor` children can now survive an
    %% OTP-driven automatic restart with their identity intact, closing the
    %% gap `Actor class>>named:` alone couldn't close without this.
    Name = bt3376_dyn_restart_name,
    bt1990_cleanup_name(Name),
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3376DynSup', ?MODULE, SupPid},
        {ok, Child} = beamtalk_supervisor:startChild(Self, 7, Name),
        Pid1 = element(4, Child),
        ?assertEqual(Pid1, erlang:whereis(Name)),

        %% Kill the child directly (not via terminateChild:) — OTP's own
        %% #permanent restart must bring it back, re-registered under the
        %% same name, with no supervisor-side bookkeeping on our part.
        exit(Pid1, kill),
        Pid2 = bt1990_wait_for_registration(Name, Pid1, 2000),
        ?assertNotEqual(Pid1, Pid2),

        %% simple_one_for_one also replays this call's own args,
        %% so the restarted child comes back with the SAME 7, not blank.
        Proxy = #beamtalk_object{
            class = 'Counter', class_mod = test_counter, pid = {registered, Name}
        },
        ?assertEqual(7, beamtalk_message_dispatch:send(Proxy, getValue, []))
    after
        bt1990_cleanup_class_obj(ClassObj),
        bt1990_cleanup_name(Name),
        gen_server:stop(SupPid)
    end.

startChild_arity3_duplicate_name_surfaces_structured_error_test() ->
    %% a name collision must surface the same structured
    %% `#beamtalk_error{kind = name_registered}` `'spawnAs'/3` already
    %% gives static named children (ADR 0079), re-attributed to
    %% `startChild:name:` rather than `spawnAs`.
    Name = bt3376_dyn_dup_name,
    bt1990_cleanup_name(Name),
    ClassObj = bt1990_make_counter_class_obj(),
    SupPid = bt3365_start_dynamic_supervisor(ClassObj),
    try
        Self = {beamtalk_supervisor, 'BT3376DynSup', ?MODULE, SupPid},
        {ok, _Child} = beamtalk_supervisor:startChild(Self, 1, Name),
        Result = beamtalk_supervisor:startChild(Self, 2, Name),
        ?assertMatch(
            {error, #beamtalk_error{
                kind = name_registered,
                class = 'BT3376DynSup',
                selector = 'startChild:name:'
            }},
            Result
        )
    after
        bt1990_cleanup_class_obj(ClassObj),
        bt1990_cleanup_name(Name),
        gen_server:stop(SupPid)
    end.

supervisor_restart_re_registers_name_test() ->
    %% The load-bearing restart-survival integration test.
    %%
    %% Wire a real OTP supervisor with an atomically-named child (start MFA
    %% uses `beamtalk_actor:spawnAs/3`). Hold a name-resolving proxy
    %% (`{registered, Name}`), then kill the child. After the supervisor
    %% restarts the child, the held proxy must resolve to the new pid —
    %% proving that name-based dispatch survives restarts.
    Name = bt1990_sup_restart_name,
    bt1990_cleanup_name(Name),
    ChildSpec = #{
        id => 'BT1990Restart',
        start => {beamtalk_actor, 'spawnAs', [Name, test_counter, 0]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_counter]
    },
    SupFlags = #{strategy => rest_for_one, intensity => 5, period => 10},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
    try
        Pid1 = erlang:whereis(Name),
        ?assert(is_pid(Pid1)),
        Proxy = #beamtalk_object{
            class = 'Counter',
            class_mod = test_counter,
            pid = {registered, Name}
        },
        %% Proxy send routes through the registered name.
        ?assertEqual(0, beamtalk_message_dispatch:send(Proxy, getValue, [])),
        beamtalk_message_dispatch:send(Proxy, increment, []),
        ?assertEqual(1, beamtalk_message_dispatch:send(Proxy, getValue, [])),

        %% Kill the child: the supervisor must restart it under the same name.
        exit(Pid1, kill),
        Pid2 = bt1990_wait_for_registration(Name, Pid1, 2000),
        ?assertNotEqual(Pid1, Pid2),

        %% State is reset (permanent restart), but the proxy still works.
        ?assertEqual(0, beamtalk_message_dispatch:send(Proxy, getValue, []))
    after
        gen_server:stop(SupPid),
        bt1990_cleanup_name(Name)
    end.

supervisor_restart_survival_via_named_proxy_from_outside_tree_test() ->
    %% Cross-tree consumer: acquire a proxy via `Actor named:` from outside
    %% the supervisor, then exercise the same restart path. This is the
    %% primary motivating use case from ADR 0079 — a cross-tree caller that
    %% would otherwise have to route through `which:`.
    case erlang:whereis(beamtalk_class_Counter) of
        undefined ->
            ?assertEqual(undefined, erlang:whereis(beamtalk_class_Counter));
        ClassPid when is_pid(ClassPid) ->
            Name = bt1990_sup_crosstree,
            bt1990_cleanup_name(Name),
            ChildSpec = #{
                id => 'BT1990CrossTree',
                start => {beamtalk_actor, 'spawnAs', [Name, test_counter, 5]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [test_counter]
            },
            SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
            {ok, SupPid} = supervisor:start_link(
                ?MODULE, {SupFlags, [ChildSpec]}
            ),
            try
                Pid1 = erlang:whereis(Name),
                %% Look up the proxy from outside the supervisor tree.
                Self = #beamtalk_object{
                    class = 'Counter class',
                    class_mod = counter,
                    pid = ClassPid
                },
                {ok, Proxy} = beamtalk_actor:named(Self, Name),
                ?assertMatch(
                    #beamtalk_object{pid = {registered, Name}}, Proxy
                ),

                %% Crash the child; the proxy must re-resolve to the fresh pid.
                exit(Pid1, kill),
                Pid2 = bt1990_wait_for_registration(Name, Pid1, 2000),
                ?assertNotEqual(Pid1, Pid2),
                ?assertEqual(5, beamtalk_message_dispatch:send(Proxy, getValue, []))
            after
                gen_server:stop(SupPid),
                bt1990_cleanup_name(Name)
            end
    end.

%%% Helpers for the named-child-spec tests above.

%% Build a minimal class-object tuple pointing at test_counter. The fake
%% class gen_server only needs to answer `class_name` and `module_name`
%% so that `build_child_spec` can read the module from the tuple.
bt1990_make_counter_class_obj() ->
    FakeClassPid = spawn(fun() ->
        (fun Loop() ->
            receive
                {'$gen_call', From, class_name} ->
                    gen_server:reply(From, 'Counter'),
                    Loop();
                {'$gen_call', From, module_name} ->
                    gen_server:reply(From, test_counter),
                    Loop();
                stop ->
                    ok
            end
        end)()
    end),
    {beamtalk_object, 'Counter class', test_counter, FakeClassPid}.

bt1990_cleanup_class_obj({beamtalk_object, _Class, _Mod, FakeClassPid}) ->
    FakeClassPid ! stop,
    ok.

%% Exercise the spec_to_otp/1 translation directly. The full build_child_spec
%% path funnels through `send(BtSpec, childSpec, [])` to derive the spec map
%% — for these unit tests we hand in the already-built childSpec map and
%% assert only on the OTP translation.
bt1990_build_from_bt_spec(BtSpec) ->
    [beamtalk_supervisor:spec_to_otp(BtSpec)].

%%====================================================================
%% BT-3759: concurrent `supervise` while the first caller's initialize: runs
%%====================================================================

%% Drives the real `supervise` path (`class_start/4` -> `supervise/2`, in each
%% caller's process, ADR 0135 §1) with a real named supervisor. The first
%% caller's `initialize` hook blocks on a message barrier, so the second caller
%% deterministically arrives mid-hook.

concurrent_supervise_initialize_test_() ->
    {setup, fun setup_class_dispatch_runtime/0, fun teardown_class_dispatch_runtime/1, fun(_) ->
        [
            {"initialize: raises: second caller sees the structured error, not a dead pid", fun() ->
                    concurrent_supervise(fail)
                end},
            {"initialize: succeeds: second caller sees a fully initialised supervisor", fun() ->
                concurrent_supervise(ok)
            end}
        ]
    end}.

concurrent_supervise(HookOutcome) ->
    ClassName = 'BT3759ConcurrentSupervise',
    Helper = beamtalk_supervisor_test_helper,
    Tab = beamtalk_supervisor_test_helper_tab,
    SupName = bt3759_named_sup,
    beamtalk_supervisor_test_helper:reset(),
    beamtalk_supervisor_test_helper:ensure_lock_table(),
    beamtalk_supervisor_test_helper:set_mode(named, SupName),
    ets:insert(Tab, {coordinator, self()}),
    ClassInfo = #{
        superclass => none,
        module => Helper,
        class_state => #{}
    },
    %% The class process serves instance dispatch of `initialize`.
    {ok, ClassPid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    Parent = self(),
    Caller = fun(Tag) ->
        spawn_link(fun() ->
            Outcome =
                try
                    {returned, beamtalk_supervisor:class_start(ClassName, Helper, supervise, [])}
                catch
                    Class:Reason -> {raised, Class, Reason}
                end,
            Parent ! {Tag, Outcome}
        end)
    end,
    try
        Caller(first),
        %% Barrier: the first caller is now inside the initialize: hook.
        HookPid =
            receive
                {init_entered, P} -> P
            after 5000 -> error(hook_not_entered)
            end,
        Caller(second),
        %% The second caller must be parked behind the hook and must not have
        %% been handed the supervisor yet.
        case await_second_caller(5000) of
            waiting ->
                ok;
            {early, Early} ->
                SupPid0 = whereis(SupName),
                error(
                    {second_caller_returned_before_initialize_finished, Early,
                        {sup_alive, is_pid(SupPid0) andalso is_process_alive(SupPid0)}}
                )
        end,
        SupPid = whereis(SupName),
        SupRef = erlang:monitor(process, SupPid),
        HookPid ! {hook, HookOutcome},
        First =
            receive
                {first, F} -> F
            after 5000 -> error(first_caller_timeout)
            end,
        Second =
            receive
                {second, S} -> S
            after 5000 -> error(second_caller_timeout)
            end,
        case HookOutcome of
            fail ->
                %% BT-3994: `supervise` answers a failed initialize in its
                %% `Result` (both callers), instead of raising.
                {returned, FirstResult} = First,
                ?assertMatch(
                    #{
                        isOk := false,
                        errReason := #{error := #beamtalk_error{kind = supervisor_start_failed}}
                    },
                    FirstResult
                ),
                receive
                    {'DOWN', SupRef, process, SupPid, _} -> ok
                after 2000 -> error(supervisor_not_stopped)
                end,
                {returned, SecondResult} = Second,
                #{isOk := false, errReason := SecondReason} = SecondResult,
                ?assertMatch(
                    #beamtalk_error{kind = supervisor_start_failed},
                    unwrap_raised(SecondReason)
                );
            ok ->
                ?assertMatch({returned, _}, First),
                {returned, SecondResult} = Second,
                %% The hook had finished before the second caller returned.
                ?assertEqual([{initialized, true}], ets:lookup(Tab, initialized)),
                ?assertMatch(
                    #{okValue := {beamtalk_supervisor, ClassName, Helper, SupPid}},
                    SecondResult
                ),
                ?assert(is_process_alive(SupPid)),
                erlang:demonitor(SupRef, [flush])
        end
    after
        case whereis(SupName) of
            undefined ->
                ok;
            Live ->
                try
                    gen_server:stop(Live)
                catch
                    _:_ -> ok
                end
        end,
        catch gen_server:stop(ClassPid, normal, 5000),
        beamtalk_supervisor_test_helper:reset()
    end.

%% Polls (yielding, no fixed sleep) until the second caller is parked on the
%% initializing gate, or it has already returned/raised.
await_second_caller(TimeoutMs) ->
    await_second_caller_loop(erlang:monotonic_time(millisecond) + TimeoutMs).

await_second_caller_loop(Deadline) ->
    receive
        {second, Early} -> {early, Early}
    after 0 ->
        case init_gate_waiters() of
            [_ | _] ->
                waiting;
            [] ->
                case erlang:monotonic_time(millisecond) >= Deadline of
                    true ->
                        error(second_caller_neither_waiting_nor_returned);
                    false ->
                        erlang:yield(),
                        await_second_caller_loop(Deadline)
                end
        end
    end.

init_gate_waiters() ->
    case ets:info(beamtalk_supervisor_init_gates, id) of
        undefined -> [];
        _ -> [W || {_, waiter, _, _} = W <- ets:tab2list(beamtalk_supervisor_init_gates)]
    end.

unwrap_raised(#{error := #beamtalk_error{} = E}) -> E;
unwrap_raised(#beamtalk_error{} = E) -> E;
unwrap_raised(Other) -> Other.

%%% BT-3759 review: an armed gate that nobody claims must not wedge `supervise`.

orphaned_init_gate_is_reclaimed_test() ->
    beamtalk_supervisor:ensure_init_gate_table(),
    application:set_env(beamtalk_runtime, init_wait_timeout_ms, 300),
    %% A proc_lib process stands in for the never-initialised supervisor so
    %% that stop_failed_supervisor/1 (proc_lib:stop) can stop it.
    {ok, Sup} = gen_event:start(),
    ets:insert(beamtalk_supervisor_init_gates, {Sup, gate}),
    try
        SupTuple = {beamtalk_supervisor, 'BT3759Orphan', undefined, Sup},
        ?assertError(
            #{error := #beamtalk_error{kind = supervisor_start_failed}},
            beamtalk_supervisor:await_initialized(SupTuple)
        ),
        %% Gate rows are gone, so the next caller is not blocked.
        ?assertEqual([], ets:lookup(beamtalk_supervisor_init_gates, Sup)),
        ?assertNot(is_process_alive(Sup)),
        ?assertEqual(ok, beamtalk_supervisor:await_initialized(SupTuple))
    after
        application:unset_env(beamtalk_runtime, init_wait_timeout_ms),
        ets:delete(beamtalk_supervisor_init_gates, Sup),
        catch exit(Sup, kill)
    end.

%% BT-3759 review: waiter rows left after the gate was released must neither
%% block a caller nor get a live, initialised supervisor stopped.
stray_waiter_rows_do_not_hold_gate_test() ->
    beamtalk_supervisor:ensure_init_gate_table(),
    application:set_env(beamtalk_runtime, init_wait_timeout_ms, 300),
    {ok, Sup} = gen_event:start(),
    Stray = {Sup, waiter, self(), make_ref()},
    ets:insert(beamtalk_supervisor_init_gates, Stray),
    try
        SupTuple = {beamtalk_supervisor, 'BT3759Stray', undefined, Sup},
        ?assertEqual(ok, beamtalk_supervisor:await_initialized(SupTuple)),
        ?assert(is_process_alive(Sup))
    after
        application:unset_env(beamtalk_runtime, init_wait_timeout_ms),
        ets:delete(beamtalk_supervisor_init_gates, Sup),
        catch gen_event:stop(Sup)
    end.

%%====================================================================
%% BT-4033: awaited start of supervised actor children
%%====================================================================

%% A static child whose `initialize` fails fails the supervisor's start
%% (OTP `init/1` returning `{stop, Reason}`), instead of `start_link`
%% answering `{ok, _}` and the child crash-looping afterwards.
bt4033_static_child_failing_initialize_fails_supervisor_start_test() ->
    OldTrap = process_flag(trap_exit, true),
    ChildSpec = #{
        id => bt4033_crash,
        start => {beamtalk_supervisor, start_actor_child, [test_init_ack_actor, #{mode => crash}]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_init_ack_actor]
    },
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    try
        ?assertMatch(
            {error, {shutdown, {failed_to_start_child, bt4033_crash, {error, function_clause}}}},
            supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]})
        )
    after
        receive
            {'EXIT', _, _} -> ok
        after 100 -> ok
        end,
        process_flag(trap_exit, OldTrap)
    end.

%% `start_actor_child/2` answers only once `initialize` has returned, and the
%% child stays linked to the caller (the supervisor's restart mechanism).
bt4033_start_actor_child_waits_for_initialize_and_links_test() ->
    {ok, Pid} = beamtalk_supervisor:start_actor_child(
        test_init_ack_actor, #{mode => slow_initialize, delay => 100}
    ),
    try
        {links, Links} = erlang:process_info(self(), links),
        ?assert(lists:member(Pid, Links)),
        %% `initialize` has already returned: the actor answers at once.
        ?assertMatch(#{mode := slow_initialize}, gen_server:call(Pid, get_state, 100))
    after
        unlink(Pid),
        exit(Pid, kill)
    end.

%% An OTP restart whose `initialize` fails is a failed restart: it counts
%% against the restart intensity, and the supervisor stops once that is
%% exceeded (no endless crash loop).
bt4033_failing_initialize_on_restart_counts_against_intensity_test() ->
    OldTrap = process_flag(trap_exit, true),
    Starts = counters:new(1, []),
    ChildSpec = #{
        id => bt4033_restart,
        start =>
            {beamtalk_supervisor, start_actor_child, [
                test_init_ack_actor, #{mode => fail_after_first, starts => Starts}
            ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_init_ack_actor]
    },
    SupFlags = #{strategy => one_for_one, intensity => 2, period => 10},
    {ok, SupPid} = supervisor:start_link(?MODULE, {SupFlags, [ChildSpec]}),
    try
        ?assertEqual(1, counters:get(Starts, 1)),
        [{bt4033_restart, ChildPid, worker, _}] = supervisor:which_children(SupPid),
        MonRef = erlang:monitor(process, SupPid),
        exit(ChildPid, kill),
        receive
            {'DOWN', MonRef, process, SupPid, Reason} -> ?assertEqual(shutdown, Reason)
        after 5000 -> ?assert(false)
        end,
        %% The kill and each failed restart count against the intensity
        %% (2): the first start, then two restarts that fail their
        %% `initialize`; the third restart event exceeds the intensity, so
        %% the supervisor shuts down instead of trying again.
        ?assertEqual(3, counters:get(Starts, 1))
    after
        catch exit(SupPid, kill),
        receive
            {'EXIT', SupPid, _} -> ok
        after 1000 -> ok
        end,
        process_flag(trap_exit, OldTrap)
    end.
