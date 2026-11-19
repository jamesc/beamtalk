%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_logging_debug_state_tests).

-moduledoc """
EUnit tests for the lifetime of `Logger enableDebug:` state (BT-5022).

The debug-target table must outlive the (usually short-lived) process that
first enabled debug, the actor filter must not leak when an actor dies, and
enabling debug for N actors must cost one filter and no per-pid atoms.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-define(TABLE, beamtalk_debug_targets).
-define(ACTOR_FILTER, beamtalk_debug_actors).

debug_state_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"the table is created at app start and names the heir", fun table_names_heir/0},
        {"debug enabled by an exited process can still be disabled",
            fun disable_all_after_enabling_process_exits/0},
        {"a dead actor's filter and row are removed", fun dead_actor_is_pruned/0},
        {"a stale unwatch keeps the monitor of a re-enabled actor",
            fun stale_unwatch_keeps_monitor/0},
        {"disableAllDebug leaves the watcher no monitor", fun disable_all_demonitors/0},
        {"the actor filter goes only with the last actor", fun filter_outlives_other_actors/0},
        {"N actors cost one filter and no per-pid atoms", fun n_actors_one_filter_no_atoms/0},
        {"registered filters are external funs", fun filters_are_external_funs/0}
    ]}.

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    beamtalk_logging_config:disableAllDebug(),
    ok.

cleanup(_) ->
    beamtalk_logging_config:disableAllDebug().

table_names_heir() ->
    ?assertNotEqual(undefined, ets:whereis(?TABLE)),
    ?assertEqual(whereis(beamtalk_ets_heir), ets:info(?TABLE, heir)).

disable_all_after_enabling_process_exits() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'Bt5022Class class', class_mod = bt5022, pid = self()},
    %% A short-lived process (like a REPL eval) enables debug, then exits.
    nil = beamtalk_test_proc:in_fresh_process(fun() ->
        beamtalk_logging_config:enableDebug(ClassRef)
    end),
    ?assertEqual([{class, 'Bt5022Class'}], beamtalk_logging_config:activeDebugTargets()),
    ?assert(has_primary_filter(beamtalk_debug_class_Bt5022Class)),
    nil = beamtalk_logging_config:disableAllDebug(),
    ?assertNot(has_primary_filter(beamtalk_debug_class_Bt5022Class)),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

dead_actor_is_pruned() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = beamtalk_test_proc:spawn_idle(),
    nil = beamtalk_logging_config:enableDebug(actor_ref(Pid)),
    ?assert(has_primary_filter(?ACTOR_FILTER)),
    ?assertMatch([{actor, Pid, 'Counter'}], beamtalk_logging_config:activeDebugTargets()),
    beamtalk_test_proc:kill_and_wait(Pid),
    beamtalk_test_wait:until(fun() -> ets:lookup(?TABLE, Pid) =:= [] end),
    beamtalk_test_wait:until(fun() -> not has_primary_filter(?ACTOR_FILTER) end),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

stale_unwatch_keeps_monitor() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = beamtalk_test_proc:spawn_idle(),
    nil = beamtalk_logging_config:enableDebug(actor_ref(Pid)),
    %% A concurrent disable deleted the row, a re-enable re-inserted it, and
    %% only then did the disable's unwatch cast arrive.
    ets:delete(?TABLE, Pid),
    ets:insert(?TABLE, {Pid, actor_instance, 'Counter'}),
    beamtalk_debug_actor_watcher:unwatch(Pid),
    beamtalk_test_wait:sync(beamtalk_debug_actor_watcher),
    beamtalk_test_proc:kill_and_wait(Pid),
    beamtalk_test_wait:until(fun() -> ets:lookup(?TABLE, Pid) =:= [] end),
    beamtalk_test_wait:until(fun() -> not has_primary_filter(?ACTOR_FILTER) end).

disable_all_demonitors() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = beamtalk_test_proc:spawn_idle(),
    nil = beamtalk_logging_config:enableDebug(actor_ref(Pid)),
    ?assert(maps:is_key(Pid, sys:get_state(beamtalk_debug_actor_watcher))),
    nil = beamtalk_logging_config:disableAllDebug(),
    %% unwatch is a cast; sys:get_state is a call, so it is handled after it.
    ?assertNot(maps:is_key(Pid, sys:get_state(beamtalk_debug_actor_watcher))),
    beamtalk_test_proc:stop(Pid).

filter_outlives_other_actors() ->
    beamtalk_logging_config:disableAllDebug(),
    A = beamtalk_test_proc:spawn_idle(),
    B = beamtalk_test_proc:spawn_idle(),
    nil = beamtalk_logging_config:enableDebug(actor_ref(A)),
    nil = beamtalk_logging_config:enableDebug(actor_ref(B)),
    beamtalk_test_proc:kill_and_wait(A),
    beamtalk_test_wait:until(fun() -> ets:lookup(?TABLE, A) =:= [] end),
    ?assert(has_primary_filter(?ACTOR_FILTER)),
    %% The filter still passes B's events and ignores A's.
    {Fun, Extra} = primary_filter(?ACTOR_FILTER),
    EventB = #{level => debug, msg => {string, "hi"}, meta => #{pid => B}},
    EventA = #{level => debug, msg => {string, "hi"}, meta => #{pid => A}},
    ?assertEqual(EventB, Fun(EventB, Extra)),
    ?assertEqual(ignore, Fun(EventA, Extra)),
    nil = beamtalk_logging_config:disableDebug(actor_ref(B)),
    ?assertNot(has_primary_filter(?ACTOR_FILTER)),
    beamtalk_test_proc:stop(B).

n_actors_one_filter_no_atoms() ->
    beamtalk_logging_config:disableAllDebug(),
    N = 50,
    %% Warm up so one-off atoms (the filter id, table keys) are not counted.
    Warm = beamtalk_test_proc:spawn_idle(),
    nil = beamtalk_logging_config:enableDebug(actor_ref(Warm)),
    Pids = [beamtalk_test_proc:spawn_idle() || _ <- lists:seq(1, N)],
    Before = erlang:system_info(atom_count),
    lists:foreach(fun(P) -> nil = beamtalk_logging_config:enableDebug(actor_ref(P)) end, Pids),
    After = erlang:system_info(atom_count),
    ?assert(After - Before < N div 2),
    ?assertEqual(1, length(actor_filter_ids())),
    lists:foreach(fun beamtalk_test_proc:stop/1, [Warm | Pids]).

filters_are_external_funs() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = beamtalk_test_proc:spawn_idle(),
    ClassRef = #beamtalk_object{class = 'Bt5022Class class', class_mod = bt5022, pid = self()},
    nil = beamtalk_logging_config:enableDebug(actor_ref(Pid)),
    nil = beamtalk_logging_config:enableDebug(ClassRef),
    nil = beamtalk_logging_config:enableDebug(runtime),
    {ActorFun, _} = primary_filter(?ACTOR_FILTER),
    {ClassFun, _} = primary_filter(beamtalk_debug_class_Bt5022Class),
    {ok, #{filters := HFilters}} = logger:get_handler_config(beamtalk_debug_domain_runtime),
    {_, {DomainFun, _}} = lists:keyfind(beamtalk_debug_domain_runtime, 1, HFilters),
    lists:foreach(
        fun(F) -> ?assertEqual({type, external}, erlang:fun_info(F, type)) end,
        [ActorFun, ClassFun, DomainFun]
    ),
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_test_proc:stop(Pid).

actor_ref(Pid) ->
    #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid}.

primary_filter(Id) ->
    #{filters := Filters} = logger:get_primary_config(),
    {Id, Pair} = lists:keyfind(Id, 1, Filters),
    Pair.

has_primary_filter(Id) ->
    #{filters := Filters} = logger:get_primary_config(),
    lists:keymember(Id, 1, Filters).

actor_filter_ids() ->
    #{filters := Filters} = logger:get_primary_config(),
    [Id || {Id, _} <- Filters, lists:prefix("beamtalk_debug_actor", atom_to_list(Id))].
