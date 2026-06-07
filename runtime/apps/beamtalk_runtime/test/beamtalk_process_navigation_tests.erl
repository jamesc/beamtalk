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
