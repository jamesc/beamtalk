%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_start_no_registry_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4951 / BT-4953 (ADR 0150, option D): there is no actor registry. Starting an
actor sends no message to, and writes no table of, a registry, because the
modules `beamtalk_object_instances` and the actor list `beamtalk_repl_actors`
(registered `beamtalk_actor_registry`) are deleted, with their two tables
(`beamtalk_instance_registry`, `beamtalk_instance_class`).

Groups:

- a fresh node carries none of the deleted processes, supervisor children or
  tables (an `ets:all()` / `whereis/1` check), and the modules are gone;
- an actor started down every start path leaves the table set and the
  registered-name set unchanged, and still announces `ActorSpawned`;
- compiled code from an older compiler, which calls
  `beamtalk_object_instances:register/2` inside `try ... catch`, still runs:
  the catch absorbs the `undef`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-define(GONE_MODULES, [
    beamtalk_object_instances, beamtalk_repl_actors, beamtalk_test_actor_registry
]).
-define(GONE_TABLES, [beamtalk_instance_registry, beamtalk_instance_class]).
-define(GONE_NAMES, [beamtalk_object_instances, beamtalk_actor_registry]).

%%% ---------------------------------------------------------------------
%%% Fixture
%%% ---------------------------------------------------------------------

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok = beamtalk_test_class:ensure_counter_loaded(),
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

%% Start one actor with `Start`, wait until it is up, and assert that no table
%% was created and no process registered under a deleted name. The actor is
%% stopped afterwards.
no_registration(Start) ->
    TablesBefore = lists:sort(ets:all()),
    {ok, Pid} = Start(),
    %% A synchronous round trip: `init/1` (and everything after it that could
    %% register) has run.
    pong = sync(Pid),
    beamtalk_test_proc:kill_and_wait(Pid),
    ?assertEqual(TablesBefore, lists:sort(ets:all())),
    ?assertEqual([], [N || N <- ?GONE_NAMES, whereis(N) =/= undefined]).

sync(Pid) ->
    _ = sys:get_state(Pid),
    pong.

announces_spawned() ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe('ActorSpawned', Collector, Handler, false),
    try
        {ok, Pid} = beamtalk_actor:safe_spawn(test_counter, 0),
        try
            ?awaitMessage({got, #{pid := Pid}}, 2000)
        after
            beamtalk_test_proc:kill_and_wait(Pid)
        end
    after
        beamtalk_announcements:unsubscribe(SubRef)
    end.

%%% ---------------------------------------------------------------------
%%% Nothing of the registries is left
%%% ---------------------------------------------------------------------

gone_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"the registry modules are deleted", fun modules_are_gone/0},
        {"a fresh node has none of their processes, children or tables", fun fresh_node_is_clean/0}
    ]}.

modules_are_gone() ->
    [?assertEqual({M, non_existing}, {M, code:which(M)}) || M <- ?GONE_MODULES].

fresh_node_is_clean() ->
    ?assertEqual([], [N || N <- ?GONE_NAMES, whereis(N) =/= undefined]),
    Tables = [T || T <- ets:all(), is_atom(T)],
    ?assertEqual([], [T || T <- ?GONE_TABLES, lists:member(T, Tables)]),
    ?assertEqual([], [T || T <- ?GONE_TABLES, ets:whereis(T) =/= undefined]),
    %% The runtime supervisor has no child for either.
    Ids = [Id || {Id, _, _, _} <- supervisor:which_children(beamtalk_runtime_sup)],
    ?assertEqual([], [I || I <- ?GONE_NAMES, lists:member(I, Ids)]).

%%% ---------------------------------------------------------------------
%%% Code from an older compiler
%%% ---------------------------------------------------------------------

%% Before BT-4951 every generated `spawn*` ran, after the actor started,
%%   try call 'beamtalk_object_instances':'register'(Class, Pid)
%%   of _RegOk -> _RegOk catch <_RegT, _RegE, _RegS> -> 'ok'
%% A `.beam` built by that compiler may still be loaded into a runtime that no
%% longer has the module: the call raises `undef`, and the catch absorbs it.
%% This compiles that exact Core Erlang and runs it.
old_compiler_registration_is_absorbed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Mod = bt4953_old_compiler_spawn,
        Src = old_compiler_core(Mod),
        {ok, Scanned, _} = core_scan:string(Src),
        {ok, Core} = core_parse:parse(Scanned),
        {ok, Mod, Bin} = compile:noenv_forms(Core, [from_core, binary, return_errors]),
        {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Bin),
        try
            %% The old registration, on its own: no module, no crash.
            ?assertEqual(ok, Mod:register_only(self())),
            %% The old spawn: it started the actor, then ran the registration.
            {ok, Pid} = Mod:spawn_like(0),
            try
                ?assertEqual(0, beamtalk_actor:sync_send(Pid, getValue, []))
            after
                beamtalk_test_proc:kill_and_wait(Pid)
            end
        after
            code:purge(Mod),
            code:delete(Mod),
            code:purge(Mod)
        end
    end}.

old_compiler_core(Mod) ->
    lists:flatten([
        "module '",
        atom_to_list(Mod),
        "' ['register_only'/1, 'spawn_like'/1]\n"
        "    attributes []\n"
        "'register_only'/1 = fun (Pid) ->\n"
        "    let _InstReg = try call 'beamtalk_object_instances':'register'('Counter', Pid)\n"
        "        of _RegOk -> _RegOk\n"
        "        catch <_RegT, _RegE, _RegS> -> 'ok'\n"
        "    in _InstReg\n"
        "'spawn_like'/1 = fun (Arg) ->\n"
        "    case call 'beamtalk_actor':'safe_spawn'('test_counter', Arg) of\n"
        "        <{'ok', Pid}> when 'true' ->\n"
        "            let _InstReg = try call 'beamtalk_object_instances':'register'('Counter', Pid)\n"
        "                of _RegOk -> _RegOk\n"
        "                catch <_RegT, _RegE, _RegS> -> 'ok'\n"
        "            in {'ok', Pid}\n"
        "        <Other> when 'true' -> Other\n"
        "    end\n"
        "end\n"
    ]).
