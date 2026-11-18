%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_scan_readers_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Reader tests for ADR 0150 / BT-4947: `allInstances`, `Node current actors` /
`actorAt:`, and the trace store's `bottlenecks` / `actor_health` read the
process scan, not a registry. Every actor
here is *never registered anywhere*: a reader that still consulted a table
would not see it.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% proc_lib entry point for the fake `native:` backing process.
-export([init/1]).

%%% ---------------------------------------------------------------------
%%% Helpers
%%% ---------------------------------------------------------------------

%% A marker-carrying process, like a generated `init/1` leaves behind, that is
%% registered anywhere.
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

bin(Pid) -> {beamtalk_string, list_to_binary(pid_to_list(Pid))}.

%% `Bt4947Obj` registered as a class backed by `test_counter`, so the scanned
%% actors resolve to a `#beamtalk_object{}`.
with_obj_class(Fun) ->
    Pid = beamtalk_test_class:start('Bt4947Obj', #{
        name => 'Bt4947Obj',
        module => test_counter,
        superclass => 'Actor',
        instance_methods => #{},
        class_methods => #{}
    }),
    beamtalk_test_class:with_class(Pid, Fun).

%%% ---------------------------------------------------------------------
%%% allInstances / instanceCount
%%% ---------------------------------------------------------------------

all_instances_reads_the_scan_test() ->
    A = marked('Bt4947Scan'),
    B = marked('Bt4947Scan'),
    Other = marked('Bt4947Other'),
    try
        ?assertEqual(
            lists:sort([A, B]), lists:sort(beamtalk_runtime_api:all_instances('Bt4947Scan'))
        ),
        ?assertEqual([Other], beamtalk_runtime_api:all_instances('Bt4947Other')),
        ?assertEqual([], beamtalk_runtime_api:all_instances('Bt4947None'))
    after
        beamtalk_test_proc:kill_and_wait(A),
        beamtalk_test_proc:kill_and_wait(B),
        beamtalk_test_proc:kill_and_wait(Other)
    end.

all_instances_drops_dead_actors_test() ->
    A = marked('Bt4947Dead'),
    ?assertEqual([A], beamtalk_runtime_api:all_instances('Bt4947Dead')),
    beamtalk_test_proc:kill_and_wait(A),
    ?assertEqual([], beamtalk_runtime_api:all_instances('Bt4947Dead')).

all_instances_includes_named_and_supervised_test() ->
    {ok, Named} = beamtalk_actor_registry:'spawnAs'(bt4947_named_counter, test_counter, 0),
    {ok, Sup} = supervisor:start_link(beamtalk_actor_scan_tests_sup, []),
    try
        [{_, Child, _, _}] = supervisor:which_children(Sup),
        Pids = beamtalk_runtime_api:all_instances('Counter'),
        ?assert(lists:member(Named, Pids)),
        ?assert(lists:member(Child, Pids))
    after
        beamtalk_test_proc:kill_and_wait(Named),
        unlink(Sup),
        MonRef = erlang:monitor(process, Sup),
        exit(Sup, shutdown),
        ?awaitMessage({'DOWN', MonRef, process, Sup, _})
    end.

%%% ---------------------------------------------------------------------
%%% Node current actors / actorAt:
%%% ---------------------------------------------------------------------

local_actors_scans_without_the_actor_list_test() ->
    with_obj_class(fun() ->
        A = marked('Bt4947Obj'),
        B = marked('Bt4947Obj'),
        try
            Objs = beamtalk_node:local_actors(),
            Mine = [O || {beamtalk_object, _, _, P} = O <- Objs, lists:member(P, [A, B])],
            ?assertEqual(
                lists:sort([
                    {beamtalk_object, 'Bt4947Obj', test_counter, A},
                    {beamtalk_object, 'Bt4947Obj', test_counter, B}
                ]),
                lists:sort(Mine)
            ),
            %% a pid is listed once
            Pids = [P || {beamtalk_object, _, _, P} <- Objs],
            ?assertEqual(length(Pids), length(lists:usort(Pids)))
        after
            beamtalk_test_proc:kill_and_wait(A),
            beamtalk_test_proc:kill_and_wait(B)
        end
    end).

local_actors_filters_dead_test() ->
    with_obj_class(fun() ->
        A = marked('Bt4947Obj'),
        B = marked('Bt4947Obj'),
        beamtalk_test_proc:kill_and_wait(A),
        try
            Pids = [P || {beamtalk_object, _, _, P} <- beamtalk_node:local_actors()],
            ?assertNot(lists:member(A, Pids)),
            ?assert(lists:member(B, Pids))
        after
            beamtalk_test_proc:kill_and_wait(B)
        end
    end).

local_actor_at_uses_info_test() ->
    with_obj_class(fun() ->
        A = marked('Bt4947Obj'),
        PidBin = list_to_binary(pid_to_list(A)),
        Expected = {beamtalk_object, 'Bt4947Obj', test_counter, A},
        try
            ?assertEqual(Expected, beamtalk_node:local_actor_at(PidBin)),
            ?assertEqual(Expected, beamtalk_node:local_actor_at(pid_to_list(A))),
            ?assertEqual(Expected, beamtalk_node:local_actor_at({beamtalk_string, PidBin}))
        after
            beamtalk_test_proc:kill_and_wait(A)
        end,
        %% dead
        ?assertEqual(nil, beamtalk_node:local_actor_at(PidBin)),
        %% not a pid / not a string
        ?assertEqual(nil, beamtalk_node:local_actor_at(<<"not-a-pid">>)),
        ?assertEqual(nil, beamtalk_node:local_actor_at(<<"">>)),
        ?assertEqual(nil, beamtalk_node:local_actor_at(42)),
        %% unknown pid, and a live process that is not an actor
        ?assertEqual(nil, beamtalk_node:local_actor_at(<<"<0.99999.0>">>)),
        ?assertEqual(nil, beamtalk_node:local_actor_at(list_to_binary(pid_to_list(self()))))
    end).

%%% ---------------------------------------------------------------------
%%% Destructive readers see marked actors only
%%% ---------------------------------------------------------------------

%% proc_lib entry point for the fake `native:` backing process.
init(_) ->
    receive
        stop -> ok
    end.

%% Classes A and B share one `native:` backing module and a bare OTP process
%% runs it too. `allInstances` (listing) sees all of them; the readers that kill
%% or migrate (`removeFromSystem`, hot reload) see none of them, and
%% `A removeFromSystem` leaves B's instance and the bare process alive.
native_fallback_is_listing_only_test() ->
    beamtalk_class_registry:ensure_backing_module_index_table(),
    ModAtom = bt4947_removable_mod,
    ok = beamtalk_test_module:load(ModAtom, [
        {attribute, 1, module, ModAtom}, {attribute, 2, export, []}
    ]),
    ClassA = 'Bt4947NativeA',
    ClassB = 'Bt4947NativeB',
    PidA = beamtalk_test_class:start(ClassA, #{
        name => ClassA,
        module => ModAtom,
        superclass => none,
        instance_methods => #{},
        class_methods => #{}
    }),
    PidB = beamtalk_test_class:start(ClassB, #{
        name => ClassB,
        module => ?MODULE,
        superclass => none,
        instance_methods => #{},
        class_methods => #{}
    }),
    Shared = proc_lib:spawn(?MODULE, init, [x]),
    Bare = proc_lib:spawn(?MODULE, init, [x]),
    ok = beamtalk_test_wait:until(fun() ->
        lists:all(
            fun(P) ->
                {dictionary, D} = erlang:process_info(P, dictionary),
                lists:keymember('$initial_call', 1, D)
            end,
            [Shared, Bare]
        )
    end),
    ets:insert(beamtalk_backing_module_index, [
        {?MODULE, ClassA, PidA}, {?MODULE, ClassB, PidB}
    ]),
    try
        %% listing: every process running the backing module, under each class
        ?assert(lists:member(Shared, beamtalk_runtime_api:all_instances(ClassA))),
        ?assert(lists:member(Bare, beamtalk_runtime_api:all_instances(ClassB))),
        %% destructive readers: none
        ?assertEqual([], beamtalk_runtime_api:marked_instances(ClassA)),
        ?assertEqual([], beamtalk_runtime_api:marked_instances(ClassB)),
        ?assertEqual(nil, beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassA)),
        ?assert(is_process_alive(Shared)),
        ?assert(is_process_alive(Bare))
    after
        ets:match_delete(beamtalk_backing_module_index, {?MODULE, '_', '_'}),
        Shared ! stop,
        Bare ! stop,
        beamtalk_test_class:stop(PidB),
        beamtalk_test_class:stop(PidA)
    end.

%%% ---------------------------------------------------------------------
%%% removeFromSystem from one of the class's own instances
%%% ---------------------------------------------------------------------

%% `Foo removeFromSystem` run by a `Foo` actor would kill the caller part-way
%% (after the class monitor was unwatched, before the class process, module and
%% registries were cleaned up). It is refused up front instead; nothing is
%% stopped or removed.
remove_from_system_refuses_from_own_instance_test() ->
    Class = 'Bt4947SelfClass',
    ClassPid = beamtalk_test_class:start(Class, #{
        name => Class,
        module => ?MODULE,
        superclass => none,
        instance_methods => #{},
        class_methods => #{}
    }),
    Parent = self(),
    Instance = spawn(fun() ->
        erlang:put('$beamtalk_actor', Class),
        Result =
            try
                beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(Class)
            catch
                error:Err -> {error, Err}
            end,
        Parent ! {result, self(), Result},
        receive
            stop -> ok
        end
    end),
    try
        ?awaitMessage({result, Instance, Result}),
        ?assertMatch({error, #{error := #beamtalk_error{kind = runtime_error}}}, Result),
        ?assert(is_process_alive(Instance)),
        ?assertEqual(ClassPid, beamtalk_class_registry:whereis_class(Class))
    after
        Instance ! stop,
        beamtalk_test_class:stop(ClassPid)
    end.
