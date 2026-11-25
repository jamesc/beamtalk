%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_native_actor_start_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4577: the Beamtalk start paths (`safe_spawn_linked/2`, `'spawnAs'/3`,
`'spawnAsGlobal'/3`, `spawn_named_unlinked/3`) start a `native:` actor's
backing module through `start_link/1` when they are handed its facade, which
has no `init/1`. Covers anonymous, local and global named starts, a taken
name (refused before the backing `init/1` runs), an unlinked start, a
non-map config, and that a compiled-actor module is not mistaken for one.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-define(FACADE, beamtalk_test_native_counter_facade).

get_value(Pid) ->
    gen_server:call(Pid, {getValue, []}).

%% Stop a linked test process without taking the test process down.
backing_module_test() ->
    ?assertEqual(
        beamtalk_test_native_counter, beamtalk_actor_start:native_backing_module(?FACADE)
    ),
    %% A hand-written gen_server has no `__beamtalk_meta/0`.
    ?assertEqual(none, beamtalk_actor_start:native_backing_module(beamtalk_test_native_counter)).

anonymous_linked_start_test() ->
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 3}),
    {links, Links} = process_info(self(), links),
    ?assert(lists:member(Pid, Links)),
    ?assertEqual({ok, 3}, get_value(Pid)),
    beamtalk_test_proc:kill_and_wait(Pid).

local_named_start_test() ->
    Name = bt4577_eunit_local,
    {ok, Pid} = beamtalk_actor_registry:'spawnAs'(Name, ?FACADE, #{initial => 4}),
    ?assertEqual(Pid, whereis(Name)),
    ?assertEqual({ok, 4}, get_value(Name)),
    beamtalk_test_proc:kill_and_wait(Pid).

global_named_start_test() ->
    Name = bt4577_eunit_global,
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, ?FACADE, #{initial => 5}),
    ?assertEqual(Pid, global:whereis_name(Name)),
    ?assertEqual({ok, 5}, get_value(Pid)),
    beamtalk_test_proc:kill_and_wait(Pid).

%% BT-4956: a `native:` actor registered `#global` has a start time, written by
%% the runtime's registration path (the backing module has no `init/1` of ours),
%% so a name conflict keeps the older one instead of falling back to
%% `random_exit_name/3`.
global_native_actor_records_start_time_test() ->
    Before = erlang:system_time(microsecond),
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(
        bt4956_eunit_start_time, ?FACADE, #{initial => 1}
    ),
    After = erlang:system_time(microsecond),
    StartedAt = beamtalk_actor_registry:started_at_from_dictionary(Pid),
    ?assert(is_integer(StartedAt)),
    ?assert(StartedAt >= Before andalso StartedAt =< After),
    beamtalk_test_proc:kill_and_wait(Pid).

local_and_anonymous_native_actors_record_no_start_time_test() ->
    {ok, Anon} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
    ?assertEqual(undefined, beamtalk_actor_registry:started_at_from_dictionary(Anon)),
    {ok, Local} = beamtalk_actor_registry:'spawnAs'(bt4956_eunit_local, ?FACADE, #{}),
    ?assertEqual(undefined, beamtalk_actor_registry:started_at_from_dictionary(Local)),
    beamtalk_test_proc:kill_and_wait(Anon),
    beamtalk_test_proc:kill_and_wait(Local).

global_native_conflict_keeps_the_older_test() ->
    {ok, Older} = beamtalk_actor_registry:'spawnAsGlobal'(bt4956_eunit_older, ?FACADE, #{}),
    ok = await_clock_past_start(Older),
    {ok, Younger1} = beamtalk_actor_registry:'spawnAsGlobal'(bt4956_eunit_younger1, ?FACADE, #{}),
    ok = await_clock_past_start(Younger1),
    {ok, Younger2} = beamtalk_actor_registry:'spawnAsGlobal'(bt4956_eunit_younger2, ?FACADE, #{}),
    %% The resolver stops the loser with `{shutdown, global_name_conflict}`,
    %% which would take a linked test process down with it.
    [unlink(P) || P <- [Older, Younger1, Younger2]],
    Mon1 = erlang:monitor(process, Younger1),
    Mon2 = erlang:monitor(process, Younger2),
    %% Untimed, the one-node tie-break would keep the lower pid, so only a
    %% start-time rule keeps the older whichever order the pids arrive in.
    ?assertEqual(Older, beamtalk_actor_registry:resolve_global_conflict(name, Older, Younger1)),
    ?assertEqual(Older, beamtalk_actor_registry:resolve_global_conflict(name, Younger2, Older)),
    ?awaitMessage({'DOWN', Mon1, process, Younger1, {shutdown, global_name_conflict}}),
    ?awaitMessage({'DOWN', Mon2, process, Younger2, {shutdown, global_name_conflict}}),
    ?assert(is_process_alive(Older)),
    beamtalk_test_proc:kill_and_wait(Older).

taken_name_is_refused_before_start_test() ->
    Name = bt4577_eunit_taken,
    Holder = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    true = register(Name, Holder),
    ?assertMatch(
        {error, #beamtalk_error{kind = name_registered}},
        beamtalk_actor_registry:'spawnAs'(Name, ?FACADE, #{})
    ),
    ?assertEqual(Holder, whereis(Name)),
    Holder ! stop.

unlinked_named_start_test() ->
    Name = bt4577_eunit_unlinked,
    {ok, Pid} = beamtalk_actor_registry:spawn_named_unlinked(Name, ?FACADE, #{}),
    {links, Links} = process_info(self(), links),
    ?assertNot(lists:member(Pid, Links)),
    ?assertEqual({ok, 0}, get_value(Pid)),
    %% No stray 'EXIT' from the removed start link.
    receive
        {'EXIT', Pid, _} -> ?assert(false)
    after 0 -> ok
    end,
    beamtalk_test_proc:kill_and_wait(Pid).

non_map_config_is_type_error_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error}},
        beamtalk_actor:safe_spawn_linked(?FACADE, [not_a_map])
    ).

%% Wait for the clock to move past `Pid`'s recorded start time, so the next
%% start is strictly younger (start times are microsecond stamps).
await_clock_past_start(Pid) ->
    StartedAt = beamtalk_actor_registry:started_at_from_dictionary(Pid),
    ?assert(is_integer(StartedAt)),
    beamtalk_test_wait:until(fun() -> erlang:system_time(microsecond) > StartedAt end).
