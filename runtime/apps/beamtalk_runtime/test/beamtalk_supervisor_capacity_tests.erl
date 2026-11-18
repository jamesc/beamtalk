%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_capacity_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
Unit tests for edge-case paths in `beamtalk_supervisor_capacity` that were
at 0% in the nightly coverage report (run 44100426984, 2026-11-17).

There are no other EUnit tests for this module; this file adds the first set.
Covers only paths testable without a live distributed node:

- `init_child_capacity/1` reinit branches: monitor died (lines 335),
  ETS table gone with pid monitor (lines 332-333), ETS table gone with
  non-pid monitor (lines 365-373 via `stop_child_capacity_monitor/1`)
- `checked_dynamic_start_n/2` untokened Extra path (line 90)
- `run_capped_start/5` failure arms via `checked_dynamic_start_n/2`:
  `ignore` (lines 126-128), `{error,_}` (lines 129-131), other (lines 132-134)
- `child_capacity_monitor_loop` catch-all receive arm (line 415)

Remote-pid paths in `max_children/1` and `child_capacity_state/1` require a
live remote node and are not covered here.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%%% -----------------------------------------------------------------------
%%% init_child_capacity/1 — reinit branches
%%%
%%% EUnit runs each test function in a separate process, so process-dictionary
%%% writes in one test do not bleed into another.
%%% -----------------------------------------------------------------------

%% Table + monitor both alive: existing state returned unchanged (baseline).
init_child_capacity_alive_returns_existing_state_test() ->
    {Tid, MonPid} = beamtalk_supervisor_capacity:init_child_capacity(5),
    put(?MAX_CHILDREN_STATE_KEY, {Tid, MonPid}),
    Result = beamtalk_supervisor_capacity:init_child_capacity(5),
    ?assertEqual({Tid, MonPid}, Result),
    exit(MonPid, kill),
    ets:delete(Tid).

%% Monitor died, table is alive: table reused, fresh monitor spawned (line 335).
init_child_capacity_dead_monitor_reuses_table_test() ->
    Tid = ets:new(beamtalk_sup_child_capacity, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 7}),
    {DeadMon, MRef} = spawn_monitor(fun() -> ok end),
    ?awaitMessage({'DOWN', MRef, process, DeadMon, _}),
    put(?MAX_CHILDREN_STATE_KEY, {Tid, DeadMon}),
    {NewTid, NewMonPid} = beamtalk_supervisor_capacity:init_child_capacity(10),
    ?assertEqual(Tid, NewTid),
    ?assertNotEqual(DeadMon, NewMonPid),
    ?assert(is_process_alive(NewMonPid)),
    %% count preserved across reinit
    ?assertEqual([{count, 7}], ets:lookup(Tid, count)),
    exit(NewMonPid, kill),
    ets:delete(Tid).

%% ETS table gone, monitor is a live pid: fresh state, old monitor killed
%% (lines 332-333 + `stop_child_capacity_monitor/1` pid clause).
init_child_capacity_dead_table_pid_monitor_creates_fresh_state_test() ->
    Tid = ets:new(beamtalk_sup_child_capacity, [public, set]),
    MonPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    ets:delete(Tid),
    put(?MAX_CHILDREN_STATE_KEY, {Tid, MonPid}),
    {NewTid, NewMonPid} = beamtalk_supervisor_capacity:init_child_capacity(10),
    ?assertNotEqual(Tid, NewTid),
    ?assert(is_pid(NewMonPid)),
    ?assert(is_process_alive(NewMonPid)),
    %% old monitor was killed by stop_child_capacity_monitor/1
    ?assertNot(is_process_alive(MonPid)),
    exit(NewMonPid, kill),
    ets:delete(NewTid).

%% ETS table gone, monitor is not a pid: exercises `stop_child_capacity_monitor/1`
%% non-pid clause (lines 365-373) then fresh_child_capacity (lines 332-333).
init_child_capacity_dead_table_non_pid_monitor_creates_fresh_state_test() ->
    Tid = ets:new(beamtalk_sup_child_capacity, [public, set]),
    ets:delete(Tid),
    put(?MAX_CHILDREN_STATE_KEY, {Tid, not_a_pid}),
    {NewTid, NewMonPid} = beamtalk_supervisor_capacity:init_child_capacity(10),
    ?assertNotEqual(Tid, NewTid),
    ?assert(is_pid(NewMonPid)),
    ?assert(is_process_alive(NewMonPid)),
    exit(NewMonPid, kill),
    ets:delete(NewTid).

%%% -----------------------------------------------------------------------
%%% checked_dynamic_start_n/2 — untokened path + run_capped_start/5 arms
%%%
%%% `run_capped_start/5` is not exported; it is exercised through the exported
%%% `checked_dynamic_start_n/2`.  Passing Extra = [] (no token) hits the
%%% untokened arm (line 90) and always admits the start.
%%% -----------------------------------------------------------------------

%% Build a params map whose MFA calls ReturnFun/0.
make_params(Tid, Max, MonPid, ReturnFun) ->
    #{
        tid => Tid,
        max => Max,
        monitor => MonPid,
        m => erlang,
        f => apply,
        args => [ReturnFun, []]
    }.

%% Untokened Extra (line 90) + {ok, Pid} return: slot counted, child tracked.
checked_dynamic_start_untokened_ok_test() ->
    Tid = ets:new(test_sup_cap_ok, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 0}),
    MonPid = spawn(fun() ->
        receive
            {beamtalk_sup_child_started, _} -> ok
        end
    end),
    ChildPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Params = make_params(Tid, 5, MonPid, fun() -> {ok, ChildPid} end),
    Result = beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, []),
    ?assertEqual({ok, ChildPid}, Result),
    ?assertEqual([{count, 1}], ets:lookup(Tid, count)),
    exit(ChildPid, kill),
    ets:delete(Tid).

%% run_capped_start: start returns `ignore`, slot released (lines 126-128).
run_capped_start_ignore_decrements_count_test() ->
    Tid = ets:new(test_sup_cap_ign, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 0}),
    MonPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Params = make_params(Tid, 5, MonPid, fun() -> ignore end),
    Result = beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, []),
    ?assertEqual(ignore, Result),
    ?assertEqual([{count, 0}], ets:lookup(Tid, count)),
    exit(MonPid, kill),
    ets:delete(Tid).

%% run_capped_start: start returns {error, _}, slot released (lines 129-131).
run_capped_start_error_decrements_count_test() ->
    Tid = ets:new(test_sup_cap_err, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 0}),
    MonPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Params = make_params(Tid, 5, MonPid, fun() -> {error, some_reason} end),
    Result = beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, []),
    ?assertEqual({error, some_reason}, Result),
    ?assertEqual([{count, 0}], ets:lookup(Tid, count)),
    exit(MonPid, kill),
    ets:delete(Tid).

%% run_capped_start: start returns unexpected value, slot released (lines 132-134).
run_capped_start_other_return_decrements_count_test() ->
    Tid = ets:new(test_sup_cap_oth, [public, set, {write_concurrency, true}]),
    true = ets:insert(Tid, {count, 0}),
    MonPid = spawn(fun() ->
        receive
            _ -> ok
        end
    end),
    Params = make_params(Tid, 5, MonPid, fun() -> unexpected_return end),
    Result = beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, []),
    ?assertEqual(unexpected_return, Result),
    ?assertEqual([{count, 0}], ets:lookup(Tid, count)),
    exit(MonPid, kill),
    ets:delete(Tid).

%%% -----------------------------------------------------------------------
%%% child_capacity_monitor_loop — catch-all receive arm (line 415)
%%% -----------------------------------------------------------------------

%% Unknown messages must be silently discarded; the monitor stays alive.
child_capacity_monitor_ignores_unknown_messages_test() ->
    {Tid, MonPid} = beamtalk_supervisor_capacity:init_child_capacity(5),
    MonRef = erlang:monitor(process, MonPid),
    MonPid ! unexpected_message_one,
    MonPid ! {also_unexpected, 42},
    %% Give the monitor a moment to process both; it should not crash.
    receive
        {'DOWN', MonRef, process, MonPid, _Reason} ->
            ?assert(false)
    after 200 ->
        ?assert(is_process_alive(MonPid))
    end,
    exit(MonPid, kill),
    ets:delete(Tid).
