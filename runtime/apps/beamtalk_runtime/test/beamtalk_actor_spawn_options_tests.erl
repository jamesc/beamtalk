%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_spawn_options_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4997: the `{beamtalk_runtime, actor_min_heap_size}` setting.

Unset (the default) leaves every spawn call as it was: no spawn options are
added (`spawn_options/0` and `gen_start_options/0` answer `[]`, so the
`gen_server:start*` option list is `[]` and `proc_lib:spawn_opt/2` gets
`[monitor]`), and an actor's heap is the VM default. Set to a positive number
of words, a compiled actor started by each spawn path gets at least that heap.
The tests use a real compiled actor (`bt@box_actor`).
""".

-include_lib("eunit/include/eunit.hrl").

-define(CLASS_MOD, 'bt@box_actor').
-define(WORDS, 6000).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    application:unset_env(beamtalk_runtime, actor_min_heap_size),
    ok = beamtalk_actor:refresh_spawn_options(),
    ok.

cleanup(_) ->
    application:unset_env(beamtalk_runtime, actor_min_heap_size),
    ok = beamtalk_actor:refresh_spawn_options(),
    ok.

spawn_options_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"unset: no spawn options are added", fun unset_adds_nothing/0},
        {"unset: an actor gets the default heap on every spawn path", fun unset_default_heap/0},
        {"set: the options are min_heap_size only", fun set_gives_option/0},
        {"set: every compiled spawn path applies it", fun set_applies_on_every_path/0},
        {"changing the env needs a refresh, and refresh undoes it", fun refresh_follows_env/0},
        {"an invalid value is ignored", fun invalid_value_is_ignored/0},
        {"the largest accepted value is applied", fun largest_value_is_accepted/0},
        {"actors already running keep their heap", fun running_actors_keep_heap/0}
    ]}.

%%% ---------------------------------------------------------------------

unset_adds_nothing() ->
    ?assertEqual([], beamtalk_actor:spawn_options()),
    ?assertEqual([], beamtalk_actor:gen_start_options()).

unset_default_heap() ->
    Default = default_min_heap(),
    lists:foreach(
        fun({Path, Spawn}) ->
            {Pid, Stop} = Spawn(),
            try
                ?assertEqual({Path, Default}, {Path, min_heap(Pid)})
            after
                Stop()
            end
        end,
        spawn_paths()
    ).

set_gives_option() ->
    with_words(?WORDS, fun() ->
        ?assertEqual([{min_heap_size, ?WORDS}], beamtalk_actor:spawn_options()),
        ?assertEqual(
            [{spawn_opt, [{min_heap_size, ?WORDS}]}], beamtalk_actor:gen_start_options()
        )
    end).

set_applies_on_every_path() ->
    with_words(?WORDS, fun() ->
        lists:foreach(
            fun({Path, Spawn}) ->
                {Pid, Stop} = Spawn(),
                try
                    Heap = min_heap(Pid),
                    ?assertEqual({Path, true}, {Path, Heap >= ?WORDS})
                after
                    Stop()
                end
            end,
            spawn_paths()
        )
    end).

refresh_follows_env() ->
    application:set_env(beamtalk_runtime, actor_min_heap_size, ?WORDS),
    try
        %% Read at startup and on refresh, not on every spawn.
        ?assertEqual([], beamtalk_actor:spawn_options()),
        ok = beamtalk_actor:refresh_spawn_options(),
        ?assertEqual([{min_heap_size, ?WORDS}], beamtalk_actor:spawn_options())
    after
        application:unset_env(beamtalk_runtime, actor_min_heap_size),
        ok = beamtalk_actor:refresh_spawn_options()
    end,
    ?assertEqual([], beamtalk_actor:spawn_options()).

invalid_value_is_ignored() ->
    lists:foreach(
        fun(Bad) ->
            application:set_env(beamtalk_runtime, actor_min_heap_size, Bad),
            try
                ok = beamtalk_actor:refresh_spawn_options(),
                ?assertEqual({Bad, []}, {Bad, beamtalk_actor:spawn_options()})
            after
                application:unset_env(beamtalk_runtime, actor_min_heap_size),
                ok = beamtalk_actor:refresh_spawn_options()
            end
        end,
        [0, -5, 1.5, big, <<"2000">>, 1048577, 20000000, 1 bsl 70]
    ).

largest_value_is_accepted() ->
    with_words(1048576, fun() ->
        ?assertEqual([{min_heap_size, 1048576}], beamtalk_actor:spawn_options())
    end).

running_actors_keep_heap() ->
    {Pid, Stop} = spawn_unlinked(),
    try
        Before = min_heap(Pid),
        with_words(?WORDS, fun() -> ?assertEqual(Before, min_heap(Pid)) end)
    after
        Stop()
    end.

%%% ---------------------------------------------------------------------
%%% Helpers

with_words(Words, Fun) ->
    application:set_env(beamtalk_runtime, actor_min_heap_size, Words),
    ok = beamtalk_actor:refresh_spawn_options(),
    try
        Fun()
    after
        application:unset_env(beamtalk_runtime, actor_min_heap_size),
        ok = beamtalk_actor:refresh_spawn_options()
    end.

min_heap(Pid) ->
    {garbage_collection, GC} = erlang:process_info(Pid, garbage_collection),
    proplists:get_value(min_heap_size, GC).

%% The minimum heap of a process spawned with no options.
default_min_heap() ->
    Self = self(),
    Pid = erlang:spawn(fun() ->
        Self ! {min_heap, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {min_heap, Pid} -> ok
    end,
    Heap = min_heap(Pid),
    Pid ! stop,
    Heap.

%% {Name, fun(() -> {Pid, StopFun})} for each spawn path a compiled actor uses.
spawn_paths() ->
    [
        {safe_spawn, fun spawn_unlinked/0},
        {safe_spawn_monitored, fun() ->
            {ok, {Pid, MonRef}} = beamtalk_actor:safe_spawn_monitored(?CLASS_MOD, #{}),
            {Pid, fun() -> stop(Pid, MonRef) end}
        end},
        {safe_spawn_monitored_async, fun() ->
            {ok, {Pid, MonRef}} = beamtalk_actor:safe_spawn_monitored_async(?CLASS_MOD, #{}),
            %% Wait until init has run so the process is an actor.
            _ = sys:get_state(Pid),
            {Pid, fun() -> stop(Pid, MonRef) end}
        end},
        {safe_spawn_linked, fun() ->
            {ok, Pid} = beamtalk_actor:safe_spawn_linked(?CLASS_MOD, #{}),
            {Pid, fun() -> stop_linked(Pid) end}
        end},
        {safe_spawn_named_global, fun() ->
            Name = beamtalk_test_unique:atom("bt4997_heap_global"),
            {ok, Pid} = beamtalk_actor:safe_spawn_named(global, unlinked, Name, ?CLASS_MOD, #{}),
            {Pid, fun() ->
                global:unregister_name(Name),
                stop_linked(Pid)
            end}
        end},
        {safe_spawn_named_local, fun() ->
            Name = beamtalk_test_unique:atom("bt4997_heap"),
            {ok, Pid} = beamtalk_actor:safe_spawn_named(local, unlinked, Name, ?CLASS_MOD, #{}),
            {Pid, fun() -> stop_linked(Pid) end}
        end}
    ].

spawn_unlinked() ->
    {ok, Pid} = beamtalk_actor:safe_spawn(?CLASS_MOD, #{}),
    {Pid, fun() -> stop_linked(Pid) end}.

stop(Pid, MonRef) ->
    catch gen_server:stop(Pid),
    erlang:demonitor(MonRef, [flush]).

stop_linked(Pid) ->
    unlink(Pid),
    catch gen_server:stop(Pid).
