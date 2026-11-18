%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_scan_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Tests for `beamtalk_actor_scan` (ADR 0150, BT-4944): compiled, supervised,
named and `native:` actors are found by the process scan; a process that dies
mid-scan is skipped; read-your-writes is pinned (sync spawn visible, async
spawn not guaranteed); a cost guard bounds the time per process.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% proc_lib entry point for the fake `native:` backing process.
-export([init/1]).

%%% ---------------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------------

pids(Entries) -> lists:sort([P || {P, _, _} <- Entries]).

%% Marker-carrying process, like a generated `init/1` leaves behind.
marked(Class, StartedAt) ->
    Parent = self(),
    Pid = spawn(fun() ->
        erlang:put('$beamtalk_actor', Class),
        case StartedAt of
            undefined -> ok;
            _ -> erlang:put('$beamtalk_actor_started_at', StartedAt)
        end,
        Parent ! {ready, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {ready, Pid} -> Pid
    end.

%%% ---------------------------------------------------------------------
%%% One test per kind of actor
%%% ---------------------------------------------------------------------

compiled_actor_found_test() ->
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        [{Pid, Class, StartedAt}] = [
            E
         || {P, _, _} = E <- beamtalk_actor_scan:scan_actors(), P =:= Pid
        ],
        ?assertEqual('Counter', Class),
        ?assert(is_integer(StartedAt)),
        ?assertEqual([{Pid, Class, StartedAt}], beamtalk_actor_scan:info(Pid)),
        ?assert(lists:member({Pid, StartedAt}, beamtalk_actor_scan:scan_class('Counter')))
    after
        beamtalk_test_proc:kill_and_wait(Pid)
    end.

named_actor_found_test() ->
    Name = bt4944_named_counter,
    {ok, Pid} = beamtalk_actor_registry:'spawnAs'(Name, test_counter, 0),
    try
        ?assertMatch([{Pid, 'Counter', _}], beamtalk_actor_scan:info(Pid)),
        ?assert(lists:keymember(Pid, 1, beamtalk_actor_scan:scan_class('Counter')))
    after
        beamtalk_test_proc:kill_and_wait(Pid)
    end.

supervised_actor_found_test() ->
    {ok, Sup} = supervisor:start_link(
        {local, bt4944_sup},
        beamtalk_actor_scan_tests_sup,
        []
    ),
    try
        [{_, Child, _, _}] = supervisor:which_children(Sup),
        ?assertMatch([{Child, 'Counter', _}], beamtalk_actor_scan:info(Child)),
        ?assert(lists:member(Child, pids(beamtalk_actor_scan:scan_actors())))
    after
        unlink(Sup),
        MonRef = erlang:monitor(process, Sup),
        exit(Sup, shutdown),
        ?awaitMessage({'DOWN', MonRef, process, Sup, _})
    end.

%% A `native:` backing gen_server carries no marker; the scan matches its
%% `'$initial_call'` through the backing-module index.
init(_) ->
    native_loop().

native_loop() ->
    receive
        stop ->
            ok;
        {'$gen_call', From, superclass} ->
            gen_server:reply(From, none),
            native_loop()
    end.

native_actor_found_test() ->
    beamtalk_class_registry:ensure_backing_module_index_table(),
    Class = 'Bt4944NativeClass',
    ClassPid = spawn(fun native_loop/0),
    register(beamtalk_class_registry:registry_name(Class), ClassPid),
    Backing = ?MODULE,
    Pid = proc_lib:spawn(?MODULE, init, [x]),
    Helper = proc_lib:spawn(fun native_loop/0),
    ok = beamtalk_test_wait:until(fun() ->
        {dictionary, D} = erlang:process_info(Pid, dictionary),
        lists:keymember('$initial_call', 1, D)
    end),
    ets:insert(beamtalk_backing_module_index, {Backing, Class, ClassPid}),
    try
        ?assertEqual([{Pid, Class, undefined}], beamtalk_actor_scan:info(Pid)),
        All = beamtalk_actor_scan:scan_actors(),
        ?assert(lists:member({Pid, Class, undefined}, All)),
        ?assertEqual([{Pid, undefined}], beamtalk_actor_scan:scan_class(Class)),
        %% A helper process is not the backing gen_server.
        ?assertEqual([], beamtalk_actor_scan:info(Helper)),
        ?assertNot(lists:member(Helper, pids(All)))
    after
        ets:match_delete(beamtalk_backing_module_index, {Backing, '_', '_'}),
        unregister(beamtalk_class_registry:registry_name(Class)),
        ClassPid ! stop,
        Pid ! stop,
        Helper ! stop
    end.

%%% ---------------------------------------------------------------------
%%% scan_class / scan_classes
%%% ---------------------------------------------------------------------

scan_class_filters_and_reports_start_time_test() ->
    A = marked('Bt4944A', 11),
    B = marked('Bt4944A', undefined),
    C = marked('Bt4944B', 33),
    try
        ?assertEqual(
            lists:sort([{A, 11}, {B, undefined}]),
            lists:sort(beamtalk_actor_scan:scan_class('Bt4944A'))
        ),
        ?assertEqual([{C, 33}], beamtalk_actor_scan:scan_class('Bt4944B')),
        ?assertEqual([], beamtalk_actor_scan:scan_class('Bt4944Absent'))
    after
        [P ! stop || P <- [A, B, C]]
    end.

scan_classes_groups_in_one_pass_test() ->
    A = marked('Bt4944G1', 1),
    B = marked('Bt4944G2', 2),
    Other = marked('Bt4944G3', 3),
    try
        Grouped = beamtalk_actor_scan:scan_classes(['Bt4944G1', 'Bt4944G2', 'Bt4944Empty']),
        ?assertEqual(['Bt4944Empty', 'Bt4944G1', 'Bt4944G2'], lists:sort(maps:keys(Grouped))),
        ?assertEqual([{A, 1}], maps:get('Bt4944G1', Grouped)),
        ?assertEqual([{B, 2}], maps:get('Bt4944G2', Grouped)),
        ?assertEqual([], maps:get('Bt4944Empty', Grouped))
    after
        [P ! stop || P <- [A, B, Other]]
    end.

%%% ---------------------------------------------------------------------
%%% Dead processes
%%% ---------------------------------------------------------------------

dead_process_is_skipped_test() ->
    Pid = marked('Bt4944Dead', 1),
    beamtalk_test_proc:kill_and_wait(Pid),
    ?assertEqual([], beamtalk_actor_scan:info(Pid)),
    ?assertNot(lists:member(Pid, pids(beamtalk_actor_scan:scan_actors()))).

processes_dying_during_scan_do_not_crash_test() ->
    Victims = [marked('Bt4944Victim', I) || I <- lists:seq(1, 200)],
    Killer = spawn_link(fun() -> [exit(V, kill) || V <- Victims] end),
    %% Scan repeatedly while the victims are being killed.
    lists:foreach(
        fun(_) ->
            Entries = beamtalk_actor_scan:scan_actors(),
            ?assert(is_list(Entries))
        end,
        lists:seq(1, 20)
    ),
    MonRef = erlang:monitor(process, Killer),
    ?awaitMessage({'DOWN', MonRef, process, Killer, _}),
    ok = beamtalk_test_wait:until(fun() -> not lists:any(fun erlang:is_process_alive/1, Victims) end),
    ?assertEqual([], beamtalk_actor_scan:scan_class('Bt4944Victim')).

%%% ---------------------------------------------------------------------
%%% Read-your-writes (ADR 0150)
%%% ---------------------------------------------------------------------

sync_spawn_is_visible_to_the_next_scan_test() ->
    %% `safe_spawn_linked/2` returns after `init/1` has run, which wrote the
    %% marker, so the very next scan sees the actor.
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        ?assert(lists:member(Pid, pids(beamtalk_actor_scan:scan_actors())))
    after
        beamtalk_test_proc:kill_and_wait(Pid)
    end.

async_spawn_is_not_guaranteed_visible_test() ->
    %% `safe_spawn_monitored_async/2` returns before `init/1` has run, so an
    %% immediate scan MAY miss the actor (ADR 0150: weaker read-your-writes).
    %% The test asserts only what is guaranteed: once `init/1` has run (the
    %% actor answers a call) the scan sees it.
    {ok, {Pid, MonRef}} = beamtalk_actor:safe_spawn_monitored_async(test_counter, 0),
    try
        _MayMiss = lists:member(Pid, pids(beamtalk_actor_scan:scan_actors())),
        _ = gen_server:call(Pid, {getValue, []}),
        ?assert(lists:member(Pid, pids(beamtalk_actor_scan:scan_actors())))
    after
        erlang:demonitor(MonRef, [flush]),
        beamtalk_test_proc:kill_and_wait(Pid)
    end.

%%% ---------------------------------------------------------------------
%%% Cost guard
%%% ---------------------------------------------------------------------

cost_per_process_is_bounded_test_() ->
    {timeout, 60, fun() ->
        N = 20000,
        Idle = [
            spawn(fun() ->
                receive
                    stop -> ok
                end
            end)
         || _ <- lists:seq(1, N)
        ],
        try
            Total = erlang:system_info(process_count),
            {Micros, _} = timer:tc(fun beamtalk_actor_scan:scan_actors/0),
            PerProcess = Micros / Total,
            %% Measured 2.5-4.5 us/process (ADR 0150); a loose bound that only
            %% catches an accidental O(n^2) or full-dictionary copy.
            ?assert(PerProcess < 100.0)
        after
            [P ! stop || P <- Idle]
        end
    end}.
