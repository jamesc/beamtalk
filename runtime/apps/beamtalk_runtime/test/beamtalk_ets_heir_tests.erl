%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ets_heir_tests).

-moduledoc """
EUnit tests for `beamtalk_ets_heir` (BT-3787).

The headline property: when a table owner dies, its table survives under
`beamtalk_ets_heir` and `beamtalk_runtime_sup` receives no `'ETS-TRANSFER'`
message (an OTP supervisor logs every such message as an error). Also covers
the shared helpers (`heir_option/0`, `maybe_set_heir/1`, `adopt_owned_tables/0`,
`ensure_table/2`), the `reclaim/1` hand-back, and the real restartable owners
(`beamtalk_trace_store`, `beamtalk_announcements`) reclaiming their tables after
a crash.
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, bt3787_heir_test_table).

%%====================================================================
%% Fixture
%%====================================================================

heir_test_() ->
    {setup, fun setup/0, fun cleanup/1,
        {foreach, fun delete_test_table/0, fun cleanup/1, [
            {"heir_option/0 names the running heir", fun heir_option_names_heir/0},
            {"owner death: table survives under the heir, supervisor gets no message",
                fun owner_death_survives_without_supervisor_message/0},
            {"reclaim/1 hands an inherited table back", fun reclaim_hands_table_back/0},
            {"reclaim/1 refuses a table the heir does not own", fun reclaim_not_owner/0},
            {"maybe_set_heir/1 sets the heir on a caller-owned table",
                fun maybe_set_heir_sets_heir/0},
            {"maybe_set_heir/1 leaves another process's table alone",
                fun maybe_set_heir_skips_foreign_table/0},
            {"adopt_owned_tables/0 sets the heir on every caller-owned table",
                fun adopt_owned_tables_sets_heir/0},
            {"ensure_table/2 creates, then reclaims after the owner dies",
                fun ensure_table_creates_then_reclaims/0},
            {"ensure_table/2 on a caller-owned table does not call the heir",
                fun ensure_table_owned_skips_heir_call/0},
            {"trace store crash: tables survive and are reclaimed by the new store",
                fun trace_store_reclaims_after_crash/0},
            {"announcements crash: tables survive and are reclaimed by the new bus",
                fun announcements_reclaims_after_crash/0},
            {"boot-time tables name the heir", fun boot_tables_name_heir/0}
        ]}}.

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    delete_test_table(),
    ok.

cleanup(_) ->
    delete_test_table(),
    ok.

%%====================================================================
%% Tests
%%====================================================================

heir_option_names_heir() ->
    HeirPid = whereis(beamtalk_ets_heir),
    ?assert(is_pid(HeirPid)),
    ?assertEqual([{heir, HeirPid, undefined}], beamtalk_ets_heir:heir_option()).

owner_death_survives_without_supervisor_message() ->
    HeirPid = whereis(beamtalk_ets_heir),
    with_supervisor_receive_trace(fun() ->
        Owner = spawn_table_owner(?TABLE, beamtalk_ets_heir:heir_option()),
        ets:insert(?TABLE, {key, value}),
        kill_and_wait(Owner),
        sync_heir(),
        ?assertEqual(HeirPid, ets:info(?TABLE, owner)),
        ?assertEqual([{key, value}], ets:lookup(?TABLE, key))
    end),
    delete_test_table().

reclaim_hands_table_back() ->
    HeirPid = whereis(beamtalk_ets_heir),
    Owner = spawn_table_owner(?TABLE, beamtalk_ets_heir:heir_option()),
    kill_and_wait(Owner),
    sync_heir(),
    ?assertEqual(HeirPid, ets:info(?TABLE, owner)),
    ?assertEqual(ok, beamtalk_ets_heir:reclaim(?TABLE)),
    ?assertEqual(self(), ets:info(?TABLE, owner)),
    %% The heir option is a table property: it survives the hand-back.
    ?assertEqual(HeirPid, ets:info(?TABLE, heir)),
    receive
        {'ETS-TRANSFER', ?TABLE, HeirPid, reclaimed} -> ok
    after 1000 ->
        ?assert(false)
    end,
    delete_test_table().

reclaim_not_owner() ->
    _ = ets:new(?TABLE, [named_table, public]),
    ?assertEqual({error, not_owner}, beamtalk_ets_heir:reclaim(?TABLE)),
    ?assertEqual({error, not_owner}, beamtalk_ets_heir:reclaim(bt3787_no_such_table)),
    delete_test_table().

maybe_set_heir_sets_heir() ->
    HeirPid = whereis(beamtalk_ets_heir),
    _ = ets:new(?TABLE, [named_table, public]),
    ?assertEqual(none, ets:info(?TABLE, heir)),
    ok = beamtalk_ets_heir:maybe_set_heir(?TABLE),
    ?assertEqual(HeirPid, ets:info(?TABLE, heir)),
    %% Idempotent.
    ok = beamtalk_ets_heir:maybe_set_heir(?TABLE),
    ?assertEqual(HeirPid, ets:info(?TABLE, heir)),
    delete_test_table().

maybe_set_heir_skips_foreign_table() ->
    Owner = spawn_table_owner(?TABLE, []),
    ok = beamtalk_ets_heir:maybe_set_heir(?TABLE),
    ?assertEqual(none, ets:info(?TABLE, heir)),
    kill_and_wait(Owner),
    ?assertEqual(undefined, ets:info(?TABLE)).

adopt_owned_tables_sets_heir() ->
    HeirPid = whereis(beamtalk_ets_heir),
    Self = self(),
    Parent = self(),
    %% Run in a fresh process so it owns only the tables made here.
    Worker = spawn(fun() ->
        _ = ets:new(?TABLE, [named_table, public]),
        Anon = ets:new(bt3787_anon, [public]),
        ok = beamtalk_ets_heir:adopt_owned_tables(),
        Parent ! {adopted, Anon, ets:info(?TABLE, heir), ets:info(Anon, heir)},
        receive
            stop -> ok
        end
    end),
    receive
        {adopted, Anon, NamedHeir, AnonHeir} ->
            ?assertEqual(HeirPid, NamedHeir),
            ?assertEqual(HeirPid, AnonHeir),
            %% Tables this process owns are untouched by another's adoption.
            ?assertNotEqual(Self, ets:info(?TABLE, owner)),
            kill_and_wait(Worker),
            sync_heir(),
            ?assertEqual(HeirPid, ets:info(?TABLE, owner)),
            ?assertEqual(HeirPid, ets:info(Anon, owner)),
            ets:delete(Anon)
    after 1000 ->
        ?assert(false)
    end,
    delete_test_table().

ensure_table_owned_skips_heir_call() ->
    HeirPid = whereis(beamtalk_ets_heir),
    ?TABLE = ets:new(?TABLE, [named_table, public, set]),
    ok = sys:suspend(HeirPid),
    try
        %% A suspended heir cannot answer reclaim/1; the owner check must
        %% short-circuit before any gen_server:call, else this exits on timeout.
        ?assertEqual(ok, beamtalk_ets_heir:ensure_table(?TABLE, [named_table, public, set])),
        ?assertEqual(self(), ets:info(?TABLE, owner))
    after
        ok = sys:resume(HeirPid),
        ets:delete(?TABLE)
    end.

ensure_table_creates_then_reclaims() ->
    HeirPid = whereis(beamtalk_ets_heir),
    Parent = self(),
    Run = fun() ->
        spawn(fun() ->
            ok = beamtalk_ets_heir:ensure_table(?TABLE, [named_table, public, set]),
            Parent ! {owned, self(), ets:info(?TABLE, owner), ets:info(?TABLE, heir)},
            receive
                stop -> ok
            end
        end)
    end,
    First = Run(),
    receive
        {owned, First, Owner1, Heir1} ->
            ?assertEqual(First, Owner1),
            ?assertEqual(HeirPid, Heir1)
    after 1000 ->
        ?assert(false)
    end,
    ets:insert(?TABLE, {survives, crash}),
    with_supervisor_receive_trace(fun() ->
        kill_and_wait(First),
        sync_heir()
    end),
    ?assertEqual(HeirPid, ets:info(?TABLE, owner)),
    Second = Run(),
    receive
        {owned, Second, Owner2, Heir2} ->
            ?assertEqual(Second, Owner2),
            ?assertEqual(HeirPid, Heir2)
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual([{survives, crash}], ets:lookup(?TABLE, survives)),
    kill_and_wait(Second),
    delete_test_table().

trace_store_reclaims_after_crash() ->
    owner_restart_reclaims(
        beamtalk_trace_store,
        [
            beamtalk_agg_class_stats,
            beamtalk_agg_pid,
            beamtalk_agg_pid_keys,
            beamtalk_agg_free_slots,
            beamtalk_trace_events
        ]
    ).

announcements_reclaims_after_crash() ->
    owner_restart_reclaims(
        beamtalk_announcements,
        [beamtalk_announcement_subs, beamtalk_announcement_by_class]
    ).

boot_tables_name_heir() ->
    HeirPid = whereis(beamtalk_ets_heir),
    %% beamtalk_class_state_snapshot is created by beamtalk_runtime_app:start/2
    %% (owned by the application master), not by the first class gen_server,
    %% and adopt_owned_tables/0 made the heir its heir. (Other boot tables
    %% are recreated by many test fixtures, so they are not asserted here.)
    Owner = ets:info(beamtalk_class_state_snapshot, owner),
    ?assertEqual(
        {initial_call, {application_master, start_it, 4}},
        erlang:process_info(Owner, initial_call)
    ),
    ?assertEqual(HeirPid, ets:info(beamtalk_class_state_snapshot, heir)).

%%====================================================================
%% Helpers
%%====================================================================

owner_restart_reclaims(Name, Tables) ->
    lists:foreach(fun(T) -> ?assertNotEqual(undefined, ets:info(T)) end, Tables),
    OldPid = whereis(Name),
    ?assert(is_pid(OldPid)),
    with_supervisor_receive_trace(fun() ->
        kill_and_wait(OldPid),
        NewPid = wait_for_restart(Name, OldPid, 50),
        lists:foreach(
            fun(T) ->
                ?assertEqual({T, NewPid}, {T, ets:info(T, owner)}),
                ?assertEqual({T, whereis(beamtalk_ets_heir)}, {T, ets:info(T, heir)})
            end,
            Tables
        )
    end).

%% Run `Fun` while tracing every message `beamtalk_runtime_sup` receives, then
%% assert none of them was an `'ETS-TRANSFER'`.
with_supervisor_receive_trace(Fun) ->
    SupPid = whereis(beamtalk_runtime_sup),
    ?assert(is_pid(SupPid)),
    1 = erlang:trace(SupPid, true, ['receive', {tracer, self()}]),
    try
        Fun()
    after
        erlang:trace(SupPid, false, ['receive'])
    end,
    %% A trace message for a delivery that happened before trace-off is in
    %% our mailbox once the trace delivery is flushed.
    Ref = erlang:trace_delivered(SupPid),
    receive
        {trace_delivered, SupPid, Ref} -> ok
    after 5000 ->
        ?assert(false)
    end,
    ?assertEqual([], drain_transfer_traces(SupPid, [])).

drain_transfer_traces(SupPid, Acc) ->
    receive
        {trace, SupPid, 'receive', {'ETS-TRANSFER', _, _, _} = Msg} ->
            drain_transfer_traces(SupPid, [Msg | Acc]);
        {trace, SupPid, 'receive', _Other} ->
            drain_transfer_traces(SupPid, Acc)
    after 0 ->
        lists:reverse(Acc)
    end.

spawn_table_owner(Table, HeirOpts) ->
    Parent = self(),
    Pid = spawn(fun() ->
        _ = ets:new(Table, [named_table, public, set | HeirOpts]),
        Parent ! {table_created, self()},
        receive
            stop -> ok
        end
    end),
    receive
        {table_created, Pid} -> Pid
    after 1000 ->
        error(table_owner_timeout)
    end.

kill_and_wait(Pid) ->
    MRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 ->
        error(kill_timeout)
    end.

%% A synchronous call is processed after the 'ETS-TRANSFER' already queued.
sync_heir() ->
    _ = sys:get_state(beamtalk_ets_heir),
    ok.

wait_for_restart(_Name, _OldPid, 0) ->
    error(restart_timeout);
wait_for_restart(Name, OldPid, N) ->
    case whereis(Name) of
        Pid when is_pid(Pid), Pid =/= OldPid ->
            %% init/1 has returned once the process answers a sys call.
            _ = sys:get_state(Pid),
            Pid;
        _ ->
            timer:sleep(20),
            wait_for_restart(Name, OldPid, N - 1)
    end.

delete_test_table() ->
    case ets:info(?TABLE) of
        undefined -> ok;
        _ -> ets:delete(?TABLE)
    end,
    ok.
