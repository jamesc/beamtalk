%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_lifecycle_telemetry_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4997: the actor `[beamtalk, actor, lifecycle, start | stop]` telemetry
events are opt-in, as the dispatch events are (BT-4113, ADR 0069).

The tests run against a real compiled actor (`bt@box_actor`, from
`test_fixtures/`), so they cover the generated `init/1` / `terminate/2`, not a
stand-in. They pin:

- with the events off (the default) a start and a stop emit nothing;
- with them on, the events are exactly what they were: names, measurements
  and metadata;
- the `ActorSpawned` / `ActorStopped` announcements and the trace-store
  aggregates and trace events are the same in both modes;
- what turns the events on and off, and that doing so at run time reaches
  actors that start afterwards.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-define(START, [beamtalk, actor, lifecycle, start]).
-define(STOP, [beamtalk, actor, lifecycle, stop]).
-define(CLASS, 'BoxActor').

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {module, telemetry} = code:ensure_loaded(telemetry),
    ok = beamtalk_telemetry:disable_lifecycle_events(),
    ok.

cleanup(_) ->
    ok = beamtalk_telemetry:disable_lifecycle_events(),
    application:unset_env(beamtalk_runtime, lifecycle_telemetry),
    ok = beamtalk_telemetry:refresh_lifecycle_events(),
    beamtalk_trace_store:disable(),
    beamtalk_trace_store:clear(),
    ok.

lifecycle_telemetry_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"the events are off by default", fun off_by_default/0},
        {"off: a start and a stop emit no telemetry event", fun off_emits_nothing/0},
        {"on: the events carry the same names, measurements and metadata",
            fun on_events_are_unchanged/0},
        {"on: a crash stop carries the raw reason", fun on_crash_reason_is_raw/0},
        {"announcements are the same with the events off and on",
            fun announcements_same_in_both_modes/0},
        {"aggregates and trace events are recorded with the events off",
            fun trace_store_records_with_events_off/0},
        {"aggregates and trace events are the same with the events on",
            fun trace_store_records_with_events_on/0},
        {"enabling at run time reaches actors that start afterwards",
            fun run_time_enable_reaches_later_starts/0},
        {"a stop is emitted when the flag is on at stop time", fun flag_is_read_at_stop_time/0},
        {"the kill event is still always emitted", fun kill_is_always_emitted/0},
        {"the app env turns the events on at the next refresh", fun app_env_turns_on/0},
        {"a Beamtalk Telemetry handler turns them on and detach turns them off",
            fun beamtalk_handler_turns_on_and_off/0},
        {"a Beamtalk handler on a dispatch event leaves them off",
            fun dispatch_handler_leaves_off/0},
        {"disable leaves another source on", fun disable_keeps_other_source/0},
        {"the trace store's retired start/stop handler ids stay reserved",
            fun retired_ids_stay_reserved/0}
    ]}.

%%% ---------------------------------------------------------------------

off_by_default() ->
    ?assertNot(beamtalk_telemetry:lifecycle_events_enabled()).

off_emits_nothing() ->
    {_, Detach} = attach_start_stop(),
    try
        Pid = spawn_box(),
        gen_server:stop(Pid),
        ?assertEqual(none, next_event(300))
    after
        Detach()
    end.

on_events_are_unchanged() ->
    ok = beamtalk_telemetry:enable_lifecycle_events(),
    {_, Detach} = attach_start_stop(),
    try
        Pid = spawn_box(),
        ?assertEqual({?START, #{}, #{pid => Pid, class => ?CLASS}}, expect_event(?START)),
        gen_server:stop(Pid),
        ?assertEqual(
            {?STOP, #{}, #{pid => Pid, class => ?CLASS, reason => normal}}, expect_event(?STOP)
        )
    after
        Detach(),
        ok = beamtalk_telemetry:disable_lifecycle_events()
    end.

on_crash_reason_is_raw() ->
    ok = beamtalk_telemetry:enable_lifecycle_events(),
    {_, Detach} = attach_start_stop(),
    try
        Pid = spawn_box(),
        expect_event(?START),
        Reason = {bt4997, boom},
        MRef = monitor(process, Pid),
        unlink(Pid),
        try
            gen_server:stop(Pid, Reason, 5000)
        catch
            exit:_ -> ok
        end,
        ?awaitMessage({'DOWN', MRef, process, Pid, _}, 2000),
        ?assertEqual(
            {?STOP, #{}, #{pid => Pid, class => ?CLASS, reason => Reason}}, expect_event(?STOP)
        )
    after
        Detach(),
        ok = beamtalk_telemetry:disable_lifecycle_events()
    end.

announcements_same_in_both_modes() ->
    Off = announced_in_mode(off),
    On = announced_in_mode(on),
    ?assertEqual(Off, On),
    ?assertMatch(
        [{'ActorSpawned', ?CLASS, undefined}, {'ActorStopped', ?CLASS, normal}], Off
    ).

trace_store_records_with_events_off() ->
    ?assertEqual(
        #{
            start => 1,
            stop => 1,
            trace_events => [{<<"start">>, <<"ok">>}, {<<"stop">>, <<"ok">>}]
        },
        trace_store_view(off)
    ).

trace_store_records_with_events_on() ->
    ?assertEqual(trace_store_view(off), trace_store_view(on)).

run_time_enable_reaches_later_starts() ->
    {_, Detach} = attach_start_stop(),
    try
        P1 = spawn_box(),
        ?assertEqual(none, next_event(200)),
        ok = beamtalk_telemetry:enable_lifecycle_events(),
        P2 = spawn_box(),
        ?assertMatch({?START, #{}, #{pid := P2}}, expect_event(?START)),
        gen_server:stop(P1),
        %% P1 started while the events were off; its stop is emitted because
        %% the flag is on now.
        ?assertMatch({?STOP, #{}, #{pid := P1}}, expect_event(?STOP)),
        gen_server:stop(P2),
        ?assertMatch({?STOP, #{}, #{pid := P2}}, expect_event(?STOP))
    after
        Detach(),
        ok = beamtalk_telemetry:disable_lifecycle_events()
    end.

flag_is_read_at_stop_time() ->
    ok = beamtalk_telemetry:enable_lifecycle_events(),
    {_, Detach} = attach_start_stop(),
    try
        Pid = spawn_box(),
        expect_event(?START),
        ok = beamtalk_telemetry:disable_lifecycle_events(),
        gen_server:stop(Pid),
        ?assertEqual(none, next_event(300))
    after
        Detach(),
        ok = beamtalk_telemetry:disable_lifecycle_events()
    end.

kill_is_always_emitted() ->
    Kill = [beamtalk, actor, lifecycle, kill],
    Id = {?MODULE, kill, make_ref()},
    Self = self(),
    ok = telemetry:attach(
        Id, Kill, fun(E, M, Md, _) -> Self ! {event, E, M, Md} end, #{}
    ),
    try
        Pid = spawn_box(),
        unlink(Pid),
        _ = beamtalk_actor:sync_send(Pid, kill, []),
        ?assertMatch({Kill, #{}, #{pid := Pid}}, expect_event(Kill))
    after
        telemetry:detach(Id)
    end.

app_env_turns_on() ->
    application:set_env(beamtalk_runtime, lifecycle_telemetry, true),
    try
        ok = beamtalk_telemetry:refresh_lifecycle_events(),
        ?assert(beamtalk_telemetry:lifecycle_events_enabled())
    after
        application:unset_env(beamtalk_runtime, lifecycle_telemetry)
    end,
    ok = beamtalk_telemetry:refresh_lifecycle_events(),
    ?assertNot(beamtalk_telemetry:lifecycle_events_enabled()).

beamtalk_handler_turns_on_and_off() ->
    Id = bt4997_handler,
    Block = fun(_Event, _Measurements, _Metadata) -> nil end,
    ?assertNot(beamtalk_telemetry:lifecycle_events_enabled()),
    _ = beamtalk_telemetry:attach(Id, ?STOP, Block),
    try
        ?assert(beamtalk_telemetry:lifecycle_events_enabled())
    after
        nil = beamtalk_telemetry:detach(Id)
    end,
    ?assertNot(beamtalk_telemetry:lifecycle_events_enabled()).

dispatch_handler_leaves_off() ->
    Id = bt4997_dispatch_handler,
    Block = fun(_Event, _Measurements, _Metadata) -> nil end,
    _ = beamtalk_telemetry:attach(Id, [beamtalk, actor, dispatch, stop], Block),
    try
        ?assertNot(beamtalk_telemetry:lifecycle_events_enabled())
    after
        nil = beamtalk_telemetry:detach(Id),
        ok = beamtalk_telemetry:refresh_dispatch_events()
    end.

disable_keeps_other_source() ->
    Id = bt4997_handler2,
    Block = fun(_Event, _Measurements, _Metadata) -> nil end,
    ok = beamtalk_telemetry:enable_lifecycle_events(),
    _ = beamtalk_telemetry:attach(Id, ?START, Block),
    try
        ok = beamtalk_telemetry:disable_lifecycle_events(),
        ?assert(beamtalk_telemetry:lifecycle_events_enabled())
    after
        nil = beamtalk_telemetry:detach(Id)
    end,
    ?assertNot(beamtalk_telemetry:lifecycle_events_enabled()).

retired_ids_stay_reserved() ->
    Reserved = beamtalk_trace_store:telemetry_handler_ids(),
    ?assert(lists:member(beamtalk_trace_store_lifecycle_start, Reserved)),
    ?assert(lists:member(beamtalk_trace_store_lifecycle_stop, Reserved)),
    %% And the trace store really has no start/stop handler any more.
    ?assertNot(
        lists:any(
            fun(#{id := Id}) -> Id =:= beamtalk_trace_store_lifecycle_start end,
            telemetry:list_handlers(?START)
        )
    ),
    ?assertNot(
        lists:any(
            fun(#{id := Id}) -> Id =:= beamtalk_trace_store_lifecycle_stop end,
            telemetry:list_handlers(?STOP)
        )
    ).

%%% ---------------------------------------------------------------------
%%% Helpers

spawn_box() ->
    element(4, 'bt@box_actor':spawn()).

attach_start_stop() ->
    Self = self(),
    Id = {?MODULE, make_ref()},
    ok = telemetry:attach_many(
        Id,
        [?START, ?STOP],
        fun(Event, Measurements, Metadata, _) -> Self ! {event, Event, Measurements, Metadata} end,
        #{}
    ),
    {[Id], fun() ->
        telemetry:detach(Id),
        flush()
    end}.

next_event(Timeout) ->
    receive
        {event, Event, Measurements, Metadata} -> {Event, Measurements, Metadata}
    after Timeout -> none
    end.

expect_event(Event) ->
    ?awaitMessage({event, Event, Measurements, Metadata}, 2000),
    {Event, Measurements, Metadata}.

flush() ->
    receive
        {event, _, _, _} -> flush();
        {got, _} -> flush()
    after 0 -> ok
    end.

%% The announcements one actor's start and stop produce, as
%% `{Class, ActorClass, Reason}` in arrival order.
announced_in_mode(Mode) ->
    set_mode(Mode),
    Self = self(),
    Handler = fun(Event) -> Self ! {got, Event} end,
    {ok, S1} = beamtalk_announcements:subscribe('ActorSpawned', Self, Handler, false),
    {ok, S2} = beamtalk_announcements:subscribe('ActorStopped', Self, Handler, false),
    try
        Pid = spawn_box(),
        gen_server:stop(Pid),
        [announcement(Pid), announcement(Pid)]
    after
        beamtalk_announcements:unsubscribe(S1),
        beamtalk_announcements:unsubscribe(S2),
        flush(),
        set_mode(off)
    end.

announcement(Pid) ->
    ?awaitMessage(
        {got, #{'$beamtalk_class' := Class, pid := Pid, actorClass := ActorClass} = Event}, 2000
    ),
    {Class, ActorClass, maps:get(reason, Event, undefined)}.

%% The trace store's view of one actor's start and stop: the `{Class, start}`
%% and `{Class, stop}` aggregate call counts and the lifecycle trace events
%% (`{Action, Outcome}`), tracing on.
trace_store_view(Mode) ->
    set_mode(Mode),
    beamtalk_trace_store:clear(),
    beamtalk_trace_store:enable(),
    try
        Pid = spawn_box(),
        gen_server:stop(Pid),
        Rows = beamtalk_trace_store:hot_methods(10000),
        Count = fun(Action) ->
            lists:sum([
                field(R, <<"calls">>)
             || R <- Rows,
                field(R, <<"class">>) =:= <<"BoxActor">>,
                field(R, <<"selector">>) =:= Action
            ])
        end,
        Mine = list_to_binary(pid_to_list(Pid)),
        Events = [
            {field(E, <<"selector">>), field(E, <<"outcome">>)}
         || E <- beamtalk_trace_store:get_traces(),
            field(E, <<"mode">>) =:= <<"lifecycle">>,
            field(E, <<"actor">>) =:= Mine
        ],
        #{
            start => Count(<<"start">>),
            stop => Count(<<"stop">>),
            trace_events => lists:sort(Events)
        }
    after
        beamtalk_trace_store:disable(),
        beamtalk_trace_store:clear(),
        set_mode(off)
    end.

%% A field of a Beamtalk-facing result map (String keys), String values
%% flattened to binaries.
field(Map, Name) ->
    case maps:get({beamtalk_string, Name}, Map, undefined) of
        {beamtalk_string, B} -> B;
        A when is_atom(A), A =/= undefined -> atom_to_binary(A, utf8);
        Other -> Other
    end.

set_mode(on) -> ok = beamtalk_telemetry:enable_lifecycle_events();
set_mode(off) -> ok = beamtalk_telemetry:disable_lifecycle_events().
