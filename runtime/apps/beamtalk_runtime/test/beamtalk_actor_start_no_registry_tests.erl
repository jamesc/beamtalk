%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_start_no_registry_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4951 (ADR 0150, option D): starting an actor sends no message to, and
writes no table of, either registry (`beamtalk_object_instances` and the actor
list `beamtalk_repl_actors`, registered `beamtalk_actor_registry`).

The first group starts an actor down every start path and asserts, with a
`receive` trace on both registry processes, that nothing arrived that names
the new pid, and that the three registry tables did not grow. The second
group is the open-loop flood the ADR asks for: many spawners that never
drain, with clean stops and `exit(kill)`, leave both mailboxes empty and the
tables the size they were. It asserts mailboxes and table sizes, not the
process count (a spawn flood legitimately queues runnable actors).
""".

-include_lib("eunit/include/eunit.hrl").

-define(INSTANCE_TABLES, [beamtalk_instance_registry, beamtalk_instance_class]).
-define(SPAWNERS, 8).
-define(PER_SPAWNER, 250).

%%% ---------------------------------------------------------------------
%%% Fixture
%%% ---------------------------------------------------------------------

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok = beamtalk_test_class:ensure_counter_loaded(),
    %% Both registries are runtime-owned until BT-4953 deletes them; the test
    %% is only meaningful while they are there to be (not) called.
    ?assert(is_pid(whereis(beamtalk_object_instances))),
    ?assert(is_pid(whereis(beamtalk_actor_registry))),
    ok.

cleanup(_) ->
    ok.

start_paths_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"compiled spawn/0", fun() ->
            no_registration(fun() -> {ok, spawned(bt@counter:spawn())} end)
        end},
        {"compiled spawn/1", fun() ->
            no_registration(fun() -> {ok, spawned(bt@counter:spawn(#{}))} end)
        end},
        {"compiled spawn_monitored/0", fun() ->
            no_registration(fun() ->
                {Actor, MonRef} = bt@counter:spawn_monitored(),
                true = erlang:demonitor(MonRef, [flush]),
                {ok, spawned(Actor)}
            end)
        end},
        {"compiled spawn_monitored_async/0", fun() ->
            no_registration(fun() ->
                {Actor, MonRef} = bt@counter:spawn_monitored_async(),
                true = erlang:demonitor(MonRef, [flush]),
                {ok, spawned(Actor)}
            end)
        end},
        {"runtime safe_spawn_linked (supervised child path)", fun() ->
            no_registration(fun() ->
                {ok, Pid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
                {ok, Pid}
            end)
        end},
        {"runtime safe_spawn (unlinked)", fun() ->
            no_registration(fun() ->
                {ok, Pid} = beamtalk_actor:safe_spawn(test_counter, 0),
                {ok, Pid}
            end)
        end},
        {"the lifecycle start event still announces ActorSpawned", fun announces_spawned/0}
    ]}.

spawned({beamtalk_object, _Class, _Mod, Pid}) -> Pid.

%% Start one actor with `Start`, wait until it is up, and assert that neither
%% registry received a message naming it and that no table grew. The actor is
%% stopped afterwards.
no_registration(Start) ->
    InstPid = whereis(beamtalk_object_instances),
    ListPid = whereis(beamtalk_actor_registry),
    SizesBefore = table_sizes(),
    1 = erlang:trace(InstPid, true, ['receive']),
    1 = erlang:trace(ListPid, true, ['receive']),
    {ok, Pid} = Start(),
    %% A synchronous round trip: `init/1` (and everything after it that could
    %% register) has run.
    pong = sync(Pid),
    %% Let the registries drain whatever they were sent before asking.
    _ = sys:get_state(InstPid),
    _ = sys:get_state(ListPid),
    Traced = collect_traced(),
    erlang:trace(InstPid, false, ['receive']),
    erlang:trace(ListPid, false, ['receive']),
    stop(Pid),
    ?assertEqual([], [M || {_To, M} <- Traced, mentions(M, Pid)]),
    ?assertEqual(SizesBefore, table_sizes()).

sync(Pid) ->
    _ = sys:get_state(Pid),
    pong.

collect_traced() ->
    receive
        {trace, To, 'receive', Msg} -> [{To, Msg} | collect_traced()]
    after 0 -> []
    end.

%% Does `Pid` occur anywhere in `Term`?
mentions(Pid, Pid) -> true;
mentions(Tuple, Pid) when is_tuple(Tuple) -> mentions(tuple_to_list(Tuple), Pid);
mentions([H | T], Pid) -> mentions(H, Pid) orelse mentions(T, Pid);
mentions(Map, Pid) when is_map(Map) -> mentions(maps:to_list(Map), Pid);
mentions(_, _) -> false.

announces_spawned() ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe('ActorSpawned', Collector, Handler, false),
    try
        {ok, Pid} = beamtalk_actor:safe_spawn(test_counter, 0),
        try
            receive
                {got, #{pid := Pid}} -> ok
            after 2000 -> ?assert(false)
            end
        after
            stop(Pid)
        end
    after
        beamtalk_announcements:unsubscribe(SubRef)
    end.

%%% ---------------------------------------------------------------------
%%% The open-loop flood
%%% ---------------------------------------------------------------------

flood_test_() ->
    {setup, fun setup/0, fun cleanup/1, {timeout, 120, fun flood_leaves_no_registry_state/0}}.

flood_leaves_no_registry_state() ->
    InstPid = whereis(beamtalk_object_instances),
    ListPid = whereis(beamtalk_actor_registry),
    SizesBefore = table_sizes(),
    Parent = self(),
    Sampler = spawn_link(fun() -> sample(InstPid, ListPid, Parent, 0) end),
    Spawners = [
        spawn_monitor(fun() -> exit({done, spawn_loop(?PER_SPAWNER)}) end)
     || _ <- lists:seq(1, ?SPAWNERS)
    ],
    Left = lists:append([await_spawner(Ref, Pid) || {Pid, Ref} <- Spawners]),
    %% The flood is over; the survivors are still alive, never drained.
    ?assert(length(Left) > 0),
    Sampler ! {stop, Parent},
    MaxQueue =
        receive
            {max_queue, Q} -> Q
        after 5000 -> error(sampler_timeout)
        end,
    ?assertEqual(0, MaxQueue),
    ?assertEqual(0, queue_len(InstPid)),
    ?assertEqual(0, queue_len(ListPid)),
    ?assertEqual(SizesBefore, table_sizes()),
    lists:foreach(fun stop/1, Left),
    ?assertEqual(0, queue_len(InstPid)),
    ?assertEqual(0, queue_len(ListPid)),
    ?assertEqual(SizesBefore, table_sizes()).

%% Start `N` actors without waiting for any of them: a quarter are stopped
%% cleanly, a quarter are killed, half of the rest are left running and
%% returned, and the others are killed too.
spawn_loop(N) -> spawn_loop(N, []).

spawn_loop(0, Left) ->
    Left;
spawn_loop(N, Left) ->
    {Actor, MonRef} = bt@counter:spawn_monitored_async(),
    Pid = spawned(Actor),
    true = erlang:demonitor(MonRef, [flush]),
    case N rem 4 of
        0 ->
            %% Clean stop: the actor may not have reached `init/1` yet, so the
            %% stop is a request, not a wait.
            spawn(fun() ->
                try
                    gen_server:stop(Pid)
                catch
                    _:_ -> ok
                end
            end),
            spawn_loop(N - 1, Left);
        1 ->
            exit(Pid, kill),
            spawn_loop(N - 1, Left);
        2 ->
            spawn_loop(N - 1, [Pid | Left]);
        3 ->
            exit(Pid, kill),
            spawn_loop(N - 1, Left)
    end.

await_spawner(Ref, Pid) ->
    receive
        {'DOWN', Ref, process, Pid, {done, Left}} -> Left;
        {'DOWN', Ref, process, Pid, Reason} -> error({spawner_failed, Reason})
    after 60000 -> error(spawner_timeout)
    end.

%% Sample both registries' mailboxes while the flood runs; report the maximum.
sample(InstPid, ListPid, Parent, Max) ->
    Seen = max(Max, max(queue_len(InstPid), queue_len(ListPid))),
    receive
        {stop, Parent} -> Parent ! {max_queue, Seen}
    after 5 -> sample(InstPid, ListPid, Parent, Seen)
    end.

queue_len(Pid) ->
    {message_queue_len, N} = process_info(Pid, message_queue_len),
    N.

%%% ---------------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------------

table_sizes() ->
    [{T, ets:info(T, size)} || T <- ?INSTANCE_TABLES] ++
        [{beamtalk_actor_registry, registry_actor_count()}].

%% The actor list server keeps a map, not a table: its size is the length of
%% its answer to `list_actors`.
registry_actor_count() ->
    length(beamtalk_repl_actors:list_actors(whereis(beamtalk_actor_registry))).

stop(Pid) ->
    unlink(Pid),
    MonRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    after 5000 -> error({stop_timeout, Pid})
    end.
