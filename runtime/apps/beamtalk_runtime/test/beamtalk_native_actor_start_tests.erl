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

-define(FACADE, beamtalk_test_native_counter_facade).

get_value(Pid) ->
    gen_server:call(Pid, {getValue, []}).

%% Stop a linked test process without taking the test process down.
stop(Pid) ->
    unlink(Pid),
    MonRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    end.

backing_module_test() ->
    ?assertEqual(
        beamtalk_test_native_counter, beamtalk_actor:native_backing_module(?FACADE)
    ),
    %% A hand-written gen_server has no `__beamtalk_meta/0`.
    ?assertEqual(none, beamtalk_actor:native_backing_module(beamtalk_test_native_counter)).

anonymous_linked_start_test() ->
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 3}),
    {links, Links} = process_info(self(), links),
    ?assert(lists:member(Pid, Links)),
    ?assertEqual({ok, 3}, get_value(Pid)),
    stop(Pid).

local_named_start_test() ->
    Name = bt4577_eunit_local,
    {ok, Pid} = beamtalk_actor_registry:'spawnAs'(Name, ?FACADE, #{initial => 4}),
    ?assertEqual(Pid, whereis(Name)),
    ?assertEqual({ok, 4}, get_value(Name)),
    stop(Pid).

global_named_start_test() ->
    Name = bt4577_eunit_global,
    {ok, Pid} = beamtalk_actor_registry:'spawnAsGlobal'(Name, ?FACADE, #{initial => 5}),
    ?assertEqual(Pid, global:whereis_name(Name)),
    ?assertEqual({ok, 5}, get_value(Pid)),
    stop(Pid).

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
    stop(Pid).

non_map_config_is_type_error_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = type_error}},
        beamtalk_actor:safe_spawn_linked(?FACADE, [not_a_map])
    ).
