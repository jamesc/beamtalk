%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_process_navigation_tests).

-moduledoc """
Unit tests for `beamtalk_process_navigation` — the ADR 0092 supervision-tree
introspection shim.

Phase 1 (BT-2426) coverage:
- snapshot of a known small tree (root + worker, adjacency via parent_pid)
- a Beamtalk actor child classified `#beamtalkActor` with its behaviour class
- a deny-listed process excluded from `default` but present in `system`
- a `restarting` child represented (`kind => restarting`, `pid => nil`) without
  calling `process_info` on the `restarting` atom
- a worker pid (not a supervisor) walked without crashing
- the infra deny-list contents and `is_infra/1` predicate

Phase 2 (BT-2428) coverage:
- foreign supervisor `#otpSupervisor` / foreign worker `#otpProcess`
- the dynamic-supervisor child cap (truncation marker + opt-in full expansion)
- `from/1,2` accepting a pid and a Supervisor handle, a non-supervisor pid
  yielding a single-node result, and structured `stale_handle` / `type_error`
- lazy guarded `status/1` (nil for a dead pid, a map for a live sys process)
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% OTP supervisor callback + worker/child start functions.
-export([init/1, start_worker/1, start_actor_worker/2, start_named_worker/2]).

%%====================================================================
%% Supervisor callback / worker start functions
%%====================================================================

%% Generic supervisor: init arg is {SupFlags, ChildSpecs}.
init({SupFlags, ChildSpecs}) ->
    {ok, {SupFlags, ChildSpecs}}.

%% Plain worker: links, signals readiness, then idles.
start_worker(ParentPid) ->
    Pid = erlang:spawn_link(fun() ->
        ParentPid ! {worker_ready, self()},
        idle()
    end),
    {ok, Pid}.

%% Worker that plants the `'$beamtalk_actor'` marker so it classifies as a
%% Beamtalk actor of class `ClassName`.
start_actor_worker(ParentPid, ClassName) ->
    Pid = erlang:spawn_link(fun() ->
        erlang:put('$beamtalk_actor', ClassName),
        ParentPid ! {worker_ready, self()},
        idle()
    end),
    {ok, Pid}.

%% Worker that registers itself under `Name` before idling.
start_named_worker(ParentPid, Name) ->
    Pid = erlang:spawn_link(fun() ->
        erlang:register(Name, self()),
        ParentPid ! {worker_ready, self()},
        idle()
    end),
    {ok, Pid}.

idle() ->
    receive
        stop -> ok
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% Start an anonymous one_for_one supervisor with the given child specs and wait
%% for each child to report ready.
start_supervisor(ChildSpecs) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    {ok, Pid} = supervisor:start_link(?MODULE, {SupFlags, ChildSpecs}),
    lists:foreach(
        fun(_) ->
            receive
                {worker_ready, _} -> ok
            after 1000 -> error(worker_not_ready)
            end
        end,
        ChildSpecs
    ),
    Pid.

worker_spec(Id, StartFn, Args) ->
    #{
        id => Id,
        start => {?MODULE, StartFn, Args},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    }.

%% A fake supervisor process that answers `supervisor:which_children/1`
%% (a plain `gen_server:call(Pid, which_children, infinity)`) with a fixed
%% child list — used to deterministically exercise the `restarting` branch.
spawn_fake_supervisor(Children) ->
    spawn(fun() ->
        %% Plant the OTP supervisor marker so the shim's message-free probe
        %% (`looks_like_supervisor/1`) recognises this process and issues the
        %% `which_children` call it answers below.
        erlang:put('$initial_call', {supervisor, fake_sup, 1}),
        fake_sup_loop(Children)
    end).

fake_sup_loop(Children) ->
    receive
        {'$gen_call', From, which_children} ->
            gen_server:reply(From, Children),
            fake_sup_loop(Children);
        {'$gen_call', From, count_children} ->
            Active = length([P || {_Id, P, _T, _M} <- Children, is_pid(P)]),
            gen_server:reply(From, [
                {specs, length(Children)}, {active, Active}, {supervisors, 0}, {workers, Active}
            ]),
            fake_sup_loop(Children);
        stop ->
            ok
    end.

node_with_pid(Nodes, Pid) ->
    case lists:filter(fun(N) -> maps:get(pid, N) =:= Pid end, Nodes) of
        [Node] -> Node;
        [] -> not_found
    end.

%%====================================================================
%% Tests: flat snapshot of a known small tree
%%====================================================================

snapshot_small_tree_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    SupPid = start_supervisor([worker_spec(w1, start_worker, [Self])]),
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([SupPid], default),
        %% Root supervisor + one worker child.
        ?assertEqual(2, length(Nodes)),
        %% Pre-order: the root comes first.
        [Root | _] = Nodes,
        ?assertEqual(SupPid, maps:get(pid, Root)),
        ?assertEqual(nil, maps:get(parent_pid, Root)),
        ?assertEqual(otpSupervisor, maps:get(kind, Root)),
        ?assertEqual(1, maps:get(childCount, Root)),
        ?assertEqual('SupervisionNode', maps:get('$beamtalk_class', Root)),
        %% The worker child links back to the root via parent_pid.
        WorkerNodes = [N || N <- Nodes, maps:get(parent_pid, N) =:= SupPid],
        ?assertEqual(1, length(WorkerNodes)),
        [Worker] = WorkerNodes,
        ?assertEqual(otpProcess, maps:get(kind, Worker)),
        ?assertEqual(0, maps:get(childCount, Worker))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: Beamtalk actor classification
%%====================================================================

beamtalk_actor_classified_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    %% 'Integer' is a registered stdlib class, so behaviourClass resolves to a
    %% real class object.
    SupPid = start_supervisor([worker_spec(a1, start_actor_worker, [Self, 'Integer'])]),
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([SupPid], default),
        [Actor] = [N || N <- Nodes, maps:get(parent_pid, N) =:= SupPid],
        ?assertEqual(beamtalkActor, maps:get(kind, Actor)),
        BClass = maps:get(behaviourClass, Actor),
        ?assert(beamtalk_class_registry:is_class_object(BClass))
    after
        gen_server:stop(SupPid)
    end.

beamtalk_actor_unknown_class_has_nil_behaviour_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    SupPid = start_supervisor([
        worker_spec(a1, start_actor_worker, [Self, 'NotARegisteredClassXYZ'])
    ]),
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([SupPid], default),
        [Actor] = [N || N <- Nodes, maps:get(parent_pid, N) =:= SupPid],
        ?assertEqual(beamtalkActor, maps:get(kind, Actor)),
        ?assertEqual(nil, maps:get(behaviourClass, Actor))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: infra deny-list
%%====================================================================

infra_deny_list_contents_test() ->
    DenyList = beamtalk_process_navigation:infra_deny_list(),
    ?assert(sets:is_element(beamtalk_xref, DenyList)),
    ?assert(sets:is_element(beamtalk_workspace_changelog, DenyList)),
    ?assert(sets:is_element(beamtalk_runtime_sup, DenyList)),
    ?assertNot(sets:is_element(some_user_actor, DenyList)).

is_infra_predicate_test() ->
    %% An unregistered pid is never infra.
    ?assertNot(beamtalk_process_navigation:is_infra(self())),
    ?assertNot(beamtalk_process_navigation:is_infra(not_a_pid)).

deny_listed_process_excluded_from_default_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    %% beamtalk_workspace_changelog lives in the (unstarted) workspace app, so
    %% its registered name is free in the runtime-only EUnit harness. Register a
    %% child under it: `default` must exclude it, `system` must include it.
    DenyName = beamtalk_workspace_changelog,
    case erlang:whereis(DenyName) of
        undefined ->
            SupPid = start_supervisor([
                worker_spec(plain, start_worker, [Self]),
                worker_spec(infra, start_named_worker, [Self, DenyName])
            ]),
            try
                DefaultNodes = beamtalk_process_navigation:snapshot_from_pids(
                    [SupPid], default
                ),
                SystemNodes = beamtalk_process_navigation:snapshot_from_pids(
                    [SupPid], system
                ),
                InfraPid = erlang:whereis(DenyName),
                ?assertEqual(not_found, node_with_pid(DefaultNodes, InfraPid)),
                ?assertNotEqual(not_found, node_with_pid(SystemNodes, InfraPid)),
                %% default is a strict subset here (root + plain worker).
                ?assertEqual(2, length(DefaultNodes)),
                ?assertEqual(3, length(SystemNodes))
            after
                gen_server:stop(SupPid)
            end;
        _Running ->
            %% Name already taken in this environment — skip rather than fail.
            ok
    end.

%%====================================================================
%% Tests: restarting child (ADR 0092 §3)
%%====================================================================

restarting_child_represented_test() ->
    FakeSup = spawn_fake_supervisor([{child_a, restarting, worker, [some_mod]}]),
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([FakeSup], system),
        %% Root (the fake supervisor) + the restarting child.
        Restarting = [N || N <- Nodes, maps:get(kind, N) =:= restarting],
        ?assertEqual(1, length(Restarting)),
        [Node] = Restarting,
        ?assertEqual(nil, maps:get(pid, Node)),
        ?assertEqual(nil, maps:get(registeredName, Node)),
        ?assertEqual(FakeSup, maps:get(parent_pid, Node)),
        ?assertEqual(0, maps:get(childCount, Node))
    after
        FakeSup ! stop
    end.

%%====================================================================
%% Tests: a plain worker pid does not crash the walk
%%====================================================================

worker_root_does_not_crash_test() ->
    Self = self(),
    {ok, WorkerPid} = start_worker(Self),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(worker_not_ready)
    end,
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([WorkerPid], system),
        ?assertEqual(1, length(Nodes)),
        [Node] = Nodes,
        ?assertEqual(otpProcess, maps:get(kind, Node)),
        ?assertEqual(0, maps:get(childCount, Node))
    after
        WorkerPid ! stop
    end.

%%====================================================================
%% Tests: foreign classification (BT-2428)
%%====================================================================

foreign_supervisor_and_worker_classified_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    %% A raw OTP supervisor (no Beamtalk class) with a plain worker child.
    SupPid = start_supervisor([worker_spec(w1, start_worker, [Self])]),
    try
        Nodes = beamtalk_process_navigation:snapshot_from_pids([SupPid], system),
        [Root | _] = Nodes,
        %% Foreign supervisor: #otpSupervisor, no Beamtalk class, no strategy.
        ?assertEqual(otpSupervisor, maps:get(kind, Root)),
        ?assertEqual(nil, maps:get(behaviourClass, Root)),
        ?assertEqual(nil, maps:get(strategy, Root)),
        ?assertEqual(nil, maps:get(restartIntensity, Root)),
        ?assertEqual(false, maps:get(truncated, Root)),
        %% Foreign worker: #otpProcess.
        [Worker] = [N || N <- Nodes, maps:get(parent_pid, N) =:= SupPid],
        ?assertEqual(otpProcess, maps:get(kind, Worker))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: dynamic-supervisor child cap (BT-2428)
%%====================================================================

child_cap_truncates_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    SupPid = start_supervisor([
        worker_spec(w1, start_worker, [Self]),
        worker_spec(w2, start_worker, [Self])
    ]),
    try
        %% Limit 1 with 2 live children: the supervisor is reported truncated and
        %% its children are NOT materialised — only the root node appears.
        Nodes = beamtalk_process_navigation:snapshot_from_pids([SupPid], system, 1),
        ?assertEqual(1, length(Nodes)),
        [Root] = Nodes,
        ?assertEqual(true, maps:get(truncated, Root)),
        ?assertEqual(2, maps:get(childCount, Root)),
        %% A generous limit fully materialises the children (opt-in expansion).
        Full = beamtalk_process_navigation:snapshot_from_pids([SupPid], system, 100),
        ?assertEqual(3, length(Full)),
        [FullRoot | _] = Full,
        ?assertEqual(false, maps:get(truncated, FullRoot))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: from/1,2 (BT-2428)
%%====================================================================

from_accepts_pid_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    SupPid = start_supervisor([worker_spec(w1, start_worker, [Self])]),
    try
        ?assertMatch({ok, [_ | _]}, beamtalk_process_navigation:from(SupPid))
    after
        gen_server:stop(SupPid)
    end.

from_accepts_supervisor_handle_test() ->
    application:ensure_all_started(beamtalk_runtime),
    Self = self(),
    SupPid = start_supervisor([worker_spec(w1, start_worker, [Self])]),
    Handle = {beamtalk_supervisor, 'FakeSup', fake_mod, SupPid},
    try
        {ok, Nodes} = beamtalk_process_navigation:from(Handle),
        ?assertEqual(SupPid, maps:get(pid, hd(Nodes)))
    after
        gen_server:stop(SupPid)
    end.

from_non_supervisor_pid_is_single_node_test() ->
    Self = self(),
    {ok, WorkerPid} = start_worker(Self),
    receive
        {worker_ready, _} -> ok
    after 1000 -> error(worker_not_ready)
    end,
    try
        {ok, Nodes} = beamtalk_process_navigation:from(WorkerPid),
        ?assertEqual(1, length(Nodes)),
        ?assertEqual(otpProcess, maps:get(kind, hd(Nodes)))
    after
        WorkerPid ! stop
    end.

from_dead_supervisor_is_stale_handle_test() ->
    DeadPid = spawn(fun() -> ok end),
    %% Ensure it is dead before the call.
    _ = sys_wait_dead(DeadPid),
    {error, Err} = beamtalk_process_navigation:from(DeadPid),
    ?assertEqual(stale_handle, Err#beamtalk_error.kind),
    ?assertEqual('ProcessNavigation', Err#beamtalk_error.class).

from_wrong_type_is_type_error_test() ->
    {error, Err} = beamtalk_process_navigation:from(42),
    ?assertEqual(type_error, Err#beamtalk_error.kind),
    ?assertEqual('ProcessNavigation', Err#beamtalk_error.class).

sys_wait_dead(Pid) ->
    case erlang:is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(5),
            sys_wait_dead(Pid)
    end.

%%====================================================================
%% Tests: lazy guarded status (BT-2428)
%%====================================================================

status_nil_for_dead_pid_test() ->
    DeadPid = spawn(fun() -> ok end),
    _ = sys_wait_dead(DeadPid),
    ?assertEqual(nil, beamtalk_process_navigation:status(DeadPid)).

status_nil_for_non_pid_test() ->
    ?assertEqual(nil, beamtalk_process_navigation:status(not_a_pid)).

status_map_for_live_sys_process_test() ->
    %% A supervisor is a `sys`-compliant gen_server, so get_status returns a map.
    SupPid = start_supervisor([]),
    try
        Status = beamtalk_process_navigation:status(SupPid),
        ?assert(is_map(Status)),
        ?assertEqual(running, maps:get(sysState, Status)),
        ?assert(maps:is_key(module, Status))
    after
        gen_server:stop(SupPid)
    end.

%%====================================================================
%% Tests: node / tree accessors (BT-2429)
%%====================================================================

%% A hand-built lite node map (the shape the shim mints), for testing the
%% adjacency accessors without a live tree.
lite_node(Pid, ParentPid, Kind) ->
    #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => Pid,
        registeredName => nil,
        kind => Kind,
        behaviourClass => nil,
        childCount => 0,
        strategy => nil,
        restartIntensity => nil,
        truncated => false,
        parent_pid => ParentPid
    }.

%% A small fixed tree: root <- childA, childB; childA <- grandchild.
sample_flat_tree() ->
    Root = self(),
    ChildA = list_to_pid("<0.1.0>"),
    ChildB = list_to_pid("<0.2.0>"),
    Grand = list_to_pid("<0.3.0>"),
    [
        lite_node(Root, nil, otpSupervisor),
        lite_node(ChildA, Root, otpSupervisor),
        lite_node(ChildB, Root, otpProcess),
        lite_node(Grand, ChildA, otpProcess)
    ].

root_of_returns_parentless_node_test() ->
    Flat = sample_flat_tree(),
    Root = beamtalk_process_navigation:rootOf(Flat),
    ?assertEqual(self(), maps:get(pid, Root)),
    ?assertEqual(nil, maps:get(parent_pid, Root)),
    %% The returned root is enriched (carries the sibling set) so it navigates.
    ?assert(maps:is_key(siblings, Root)).

root_of_empty_is_nil_test() ->
    ?assertEqual(nil, beamtalk_process_navigation:rootOf([])).

enrich_attaches_siblings_test() ->
    Flat = sample_flat_tree(),
    Enriched = beamtalk_process_navigation:enrich(Flat),
    ?assertEqual(length(Flat), length(Enriched)),
    ?assert(lists:all(fun(N) -> maps:is_key(siblings, N) end, Enriched)).

children_of_returns_direct_children_test() ->
    Flat = sample_flat_tree(),
    Root = beamtalk_process_navigation:rootOf(Flat),
    Children = beamtalk_process_navigation:childrenOf(Root),
    ChildPids = lists:sort([maps:get(pid, C) || C <- Children]),
    Expected = lists:sort([list_to_pid("<0.1.0>"), list_to_pid("<0.2.0>")]),
    ?assertEqual(Expected, ChildPids),
    %% Children are enriched, so navigation chains (childA has a grandchild).
    [GrandParent] = [C || C <- Children, maps:get(pid, C) =:= list_to_pid("<0.1.0>")],
    Grandchildren = beamtalk_process_navigation:childrenOf(GrandParent),
    ?assertEqual([list_to_pid("<0.3.0>")], [maps:get(pid, G) || G <- Grandchildren]).

children_of_leaf_is_empty_test() ->
    Flat = sample_flat_tree(),
    Enriched = beamtalk_process_navigation:enrich(Flat),
    [Leaf] = [N || N <- Enriched, maps:get(pid, N) =:= list_to_pid("<0.2.0>")],
    ?assertEqual([], beamtalk_process_navigation:childrenOf(Leaf)).

parent_of_returns_parent_test() ->
    Flat = sample_flat_tree(),
    Enriched = beamtalk_process_navigation:enrich(Flat),
    [ChildA] = [N || N <- Enriched, maps:get(pid, N) =:= list_to_pid("<0.1.0>")],
    Parent = beamtalk_process_navigation:parentOf(ChildA),
    ?assertEqual(self(), maps:get(pid, Parent)).

parent_of_root_is_nil_test() ->
    Flat = sample_flat_tree(),
    Root = beamtalk_process_navigation:rootOf(Flat),
    ?assertEqual(nil, beamtalk_process_navigation:parentOf(Root)).

node_field_accessors_test() ->
    Node = #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => self(),
        registeredName => nil,
        kind => beamtalkSupervisor,
        behaviourClass => nil,
        childCount => 2,
        strategy => oneForOne,
        restartIntensity => #{maxRestarts => 10, window => 60},
        truncated => true,
        parent_pid => nil
    },
    ?assertEqual(oneForOne, beamtalk_process_navigation:strategyOf(Node)),
    ?assertEqual(
        #{maxRestarts => 10, window => 60}, beamtalk_process_navigation:restartIntensityOf(Node)
    ),
    ?assertEqual(true, beamtalk_process_navigation:truncatedOf(Node)).
