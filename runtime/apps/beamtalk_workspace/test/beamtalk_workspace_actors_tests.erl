%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_actors_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Tests for `beamtalk_workspace_actors` (ADR 0150, BT-4950): the workspace actor
lists are one process scan per call, `:actors` sees a synchronous spawn
immediately, and the snapshot-plus-feed protocol loses and duplicates nothing.
""".

-include_lib("eunit/include/eunit.hrl").

%% proc_lib entry point for the fake unmarked `native:` backing process.
-export([init/1]).

%% A `native:` backing gen_server (ADR 0056) carries no marker.
init(_) ->
    native_loop().

native_loop() ->
    receive
        stop -> ok
    end.

stop(Pid) ->
    unlink(Pid),
    MonRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    end.

%% A marker-carrying process, like a generated `init/1` leaves behind.
marked(Class) ->
    Parent = self(),
    Pid = spawn(fun() ->
        erlang:put('$beamtalk_actor', Class),
        erlang:put('$beamtalk_actor_started_at', erlang:system_time(microsecond)),
        Parent ! {ready, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {ready, Pid} -> Pid
    end.

pids(Metas) -> lists:sort([P || #{pid := P} <- Metas]).

%%% ---------------------------------------------------------------------
%%% list/0, lookup/1, count_for_module/1
%%% ---------------------------------------------------------------------

sync_spawn_visible_to_next_list_test() ->
    %% A synchronous spawn returns after init/1 wrote the marker, so `:actors`
    %% right after it lists the actor. (An async spawn returns before init/1
    %% and may be missed by one scan: the ADR accepts this brief invisibility.)
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        ?assertMatch(
            [#{pid := Pid, class := 'Counter', module := _, spawned_at := _}],
            [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid]
        )
    after
        stop(Pid)
    end.

list_metadata_shape_test() ->
    Pid = marked('Shape'),
    try
        [#{pid := Pid, class := 'Shape', module := Module, spawned_at := At}] =
            [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid],
        %% `module` is a resolved module or the atom `undefined` (the wire form
        %% the registry produced); `spawned_at` is seconds, as before.
        ?assert(is_atom(Module)),
        ?assert(is_integer(At)),
        ?assert(abs(At - erlang:system_time(second)) < 5)
    after
        stop(Pid)
    end.

list_missing_start_time_is_null_test() ->
    Parent = self(),
    Pid = spawn(fun() ->
        erlang:put('$beamtalk_actor', 'NoClock'),
        Parent ! {ready, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {ready, Pid} -> ok
    end,
    try
        ?assertMatch(
            [#{spawned_at := null}],
            [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid]
        )
    after
        stop(Pid)
    end.

list_drops_dead_actor_test() ->
    Pid = marked('Gone'),
    stop(Pid),
    ?assertNot(lists:member(Pid, pids(beamtalk_workspace_actors:list()))).

%% BT-4960 (owner decision, 16 Nov 2026): the workspace lists include `native:`
%% actors, the same set `Node current actors` reports. An unmarked `native:`
%% backing process is listed with its class, and `lookup/1` finds it.
unmarked_native_process_is_listed_test() ->
    Class = 'Bt4960NativeClass',
    beamtalk_test_fake_native:with_native([Class], fun(Pid) ->
        ?assertMatch(
            [#{pid := Pid, class := Class, module := _, spawned_at := null, system := false}],
            [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid]
        ),
        ?assertMatch(
            {ok, #{pid := Pid, class := Class, system := false}},
            beamtalk_workspace_actors:lookup(Pid)
        )
    end).

%% A backing module shared by several native classes: one entry per pid, and
%% the class is the alphabetically first of the classes that share the module
%% (deterministic, and the same rule `Node current actors` uses).
shared_native_module_is_listed_once_per_pid_test() ->
    beamtalk_test_fake_native:with_native(['Bt4960Zed', 'Bt4960Alpha'], fun(Pid) ->
        ?assertMatch(
            [#{pid := Pid, class := 'Bt4960Alpha'}],
            [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid]
        ),
        ?assertMatch({ok, #{class := 'Bt4960Alpha'}}, beamtalk_workspace_actors:lookup(Pid))
    end).

%% System actors (children of the workspace supervisor, like `TranscriptStream`)
%% are listed and flagged; user actors are not flagged.
system_actor_is_flagged_test() ->
    Class = 'Bt4960SystemClass',
    User = marked('Bt4960User'),
    try
        beamtalk_test_fake_native:with_system_native([Class], fun(Pid) ->
            ?assertMatch(
                [#{pid := Pid, class := Class, system := true}],
                [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= Pid]
            ),
            ?assertMatch({ok, #{system := true}}, beamtalk_workspace_actors:lookup(Pid)),
            ?assertMatch(
                [#{system := false}],
                [M || #{pid := P} = M <- beamtalk_workspace_actors:list(), P =:= User]
            )
        end)
    after
        stop(User)
    end.

%% The workspace list and `Node current actors` report the same set, with the
%% same class for every pid (including a shared native backing module).
workspace_list_and_node_actors_agree_test() ->
    User = marked('Bt4960AgreeUser'),
    try
        beamtalk_test_fake_native:with_native(['Bt4960AgreeB', 'Bt4960AgreeA'], fun(_Native) ->
            Workspace = lists:sort([
                {P, C}
             || #{pid := P, class := C} <- beamtalk_workspace_actors:list()
            ]),
            Node = lists:sort(
                [{P, C} || {beamtalk_object, C, _Mod, P} <- beamtalk_node:local_actors()]
            ),
            %% `Node current actors` only returns actors whose class is loaded;
            %% the marked fake above has no class, so compare on that common set.
            Loaded = [E || {_, C} = E <- Workspace, lists:keymember(C, 2, Node)],
            ?assertEqual(Node, lists:sort(Loaded)),
            ?assert(lists:keymember('Bt4960AgreeA', 2, Node))
        end)
    after
        stop(User)
    end.

lookup_known_and_unknown_test() ->
    Pid = marked('Look'),
    try
        ?assertMatch({ok, #{pid := Pid, class := 'Look'}}, beamtalk_workspace_actors:lookup(Pid)),
        ?assertEqual(
            {error, not_found}, beamtalk_workspace_actors:lookup(beamtalk_test_proc:spawn_idle())
        )
    after
        stop(Pid)
    end.

lookup_remote_pid_is_not_found_test() ->
    %% Pids parsed from user input may name another node; never crash on one.
    Name = atom_to_binary('nohost@nohost', utf8),
    Remote = binary_to_term(
        <<131, 88, 119, (byte_size(Name)), Name/binary, 0:32, 1:32, 1:32>>
    ),
    ?assertNotEqual(node(), node(Remote)),
    ?assertEqual({error, not_found}, beamtalk_workspace_actors:lookup(Remote)).

count_for_module_test() ->
    {ok, A} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    {ok, B} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        Mod = module_of(A),
        ?assert(beamtalk_workspace_actors:count_for_module(Mod) >= 2),
        ?assertEqual(0, beamtalk_workspace_actors:count_for_module(no_such_module_bt4950))
    after
        stop(A),
        stop(B)
    end.

module_of(Pid) ->
    [#{module := M}] = [X || #{pid := P} = X <- beamtalk_workspace_actors:list(), P =:= Pid],
    M.

%%% ---------------------------------------------------------------------
%%% Snapshot plus feed (subscribe first, then scan, then drop what the scan
%%% already contains)
%%% ---------------------------------------------------------------------

spawned(Pid, Class) ->
    #{'$beamtalk_class' => 'ActorSpawned', actorClass => Class, pid => Pid}.

stopped(Pid, Class) ->
    #{'$beamtalk_class' => 'ActorStopped', actorClass => Class, pid => Pid, reason => normal}.

feed_drops_spawn_already_in_snapshot_test() ->
    S0 = beamtalk_workspace_actors:feed_new([#{pid => self(), class => 'A'}]),
    {drop, S1} = beamtalk_workspace_actors:feed_event(spawned(self(), 'A'), S0),
    %% Only the first announcement is the duplicate; a later respawn of the
    %% same pid (impossible in practice) would be emitted.
    ?assertMatch({emit, _}, beamtalk_workspace_actors:feed_event(spawned(self(), 'A'), S1)).

feed_emits_spawn_after_snapshot_test() ->
    S0 = beamtalk_workspace_actors:feed_new([]),
    ?assertMatch({emit, _}, beamtalk_workspace_actors:feed_event(spawned(self(), 'A'), S0)).

feed_stop_of_snapshot_actor_is_emitted_and_clears_test() ->
    S0 = beamtalk_workspace_actors:feed_new([#{pid => self(), class => 'A'}]),
    {emit, S1} = beamtalk_workspace_actors:feed_event(stopped(self(), 'A'), S0),
    %% After the stop the snapshot no longer shadows a fresh spawn.
    ?assertMatch({emit, _}, beamtalk_workspace_actors:feed_event(spawned(self(), 'A'), S1)).

feed_non_actor_event_is_emitted_unchanged_test() ->
    S0 = beamtalk_workspace_actors:feed_new([]),
    ?assertEqual({emit, S0}, beamtalk_workspace_actors:feed_event(#{other => 1}, S0)).

%% BT-4970: the contract holds for a `native:` actor, end to end on the real
%% announcement bus. A native backing process runs no lifecycle telemetry, so the
%% runtime announces for it (`beamtalk_actor:native_started/1`): subscribe, take
%% the snapshot, and the spawn that the snapshot already holds is dropped; the
%% stop is emitted and clears the pid.
native_actor_snapshot_plus_feed_test() ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, Event} end,
    {ok, S1} = beamtalk_announcements:subscribe('ActorSpawned', Collector, Handler, false),
    {ok, S2} = beamtalk_announcements:subscribe('ActorStopped', Collector, Handler, false),
    try
        beamtalk_test_fake_native:with_native(['Bt4970Native'], fun(Pid) ->
            %% Started after subscribing, before the snapshot: in both.
            ok = beamtalk_actor:native_started(Pid),
            Snapshot = beamtalk_workspace_actors:list(),
            ?assert(lists:member(Pid, pids(Snapshot))),
            Feed0 = beamtalk_workspace_actors:feed_new(Snapshot),
            Spawned = receive_event('ActorSpawned', Pid),
            ?assertMatch(#{actorClass := 'Bt4970Native'}, Spawned),
            {drop, Feed1} = beamtalk_workspace_actors:feed_event(Spawned, Feed0),
            %% Its stop is announced from outside the backing process.
            exit(Pid, kill),
            Stopped = receive_event('ActorStopped', Pid),
            ?assertMatch(#{actorClass := 'Bt4970Native', reason := crashed}, Stopped),
            ?assertMatch({emit, _}, beamtalk_workspace_actors:feed_event(Stopped, Feed1))
        end)
    after
        beamtalk_announcements:unsubscribe(S1),
        beamtalk_announcements:unsubscribe(S2)
    end.

receive_event(Class, Pid) ->
    receive
        {got, #{'$beamtalk_class' := Class, pid := Pid} = Event} -> Event
    after 2000 -> error({no_announcement, Class, Pid})
    end.

%% No actor spawned between subscribe and scan is lost or duplicated: every
%% pid that exists after the race appears exactly once in snapshot + emitted
%% spawns, whichever side of the scan it landed on.
subscribe_scan_race_loses_and_duplicates_nothing_test() ->
    Class = 'Bt4950Race',
    Self = self(),
    Spawner = spawn_link(fun() -> spawner_loop(Self, Class, 200) end),
    %% Scan while the spawner is mid-flight: some events precede the scan,
    %% the rest follow it.
    ok = wait_queue(20),
    Snapshot = [M || #{class := C} = M <- beamtalk_workspace_actors:list(), C =:= Class],
    Feed0 = beamtalk_workspace_actors:feed_new(Snapshot),
    Spawned = collect_spawns(200, []),
    {Emitted, _} = lists:foldl(
        fun(Pid, {Acc, Feed}) ->
            case beamtalk_workspace_actors:feed_event(spawned(Pid, Class), Feed) of
                {emit, Feed1} -> {[Pid | Acc], Feed1};
                {drop, Feed1} -> {Acc, Feed1}
            end
        end,
        {[], Feed0},
        Spawned
    ),
    Seen = lists:sort(pids(Snapshot) ++ Emitted),
    Expected = lists:sort(Spawned),
    unlink(Spawner),
    [stop(P) || P <- Spawned],
    ?assertEqual(Expected, Seen),
    ?assertEqual(length(Seen), length(lists:usort(Seen))).

spawner_loop(_Parent, _Class, 0) ->
    ok;
spawner_loop(Parent, Class, N) ->
    Pid = marked(Class),
    Parent ! {spawned, Pid},
    spawner_loop(Parent, Class, N - 1).

collect_spawns(0, Acc) ->
    Acc;
collect_spawns(N, Acc) ->
    receive
        {spawned, Pid} -> collect_spawns(N - 1, [Pid | Acc])
    after 5000 -> error({missing_spawns, N})
    end.

wait_queue(N) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    case Len >= N of
        true ->
            ok;
        false ->
            timer:sleep(1),
            wait_queue(N)
    end.
