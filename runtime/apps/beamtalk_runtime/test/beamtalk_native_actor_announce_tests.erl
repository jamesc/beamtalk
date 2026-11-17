%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_native_actor_announce_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4970: a `native:` actor's start and stop are announced as `ActorSpawned` /
`ActorStopped`, as a compiled actor's are, so a client holding the connect
snapshot (WebSocket, LiveView, VS Code) stays current.

The backing gen_server (`beamtalk_subprocess`, a user `native:` module) runs no
lifecycle telemetry of its own, so the runtime announces from outside it: the
start path announces `ActorSpawned` and hands the pid to a short-lived observer
process, which announces `ActorStopped` when the actor goes down. Nothing is
registered and no table is written (ADR 0150), and a compiled actor's start
path is untouched.
""".

-include_lib("eunit/include/eunit.hrl").

-define(FACADE, beamtalk_test_native_counter_facade).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

cleanup(_) ->
    ok.

announce_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"an anonymous start announces ActorSpawned, a stop ActorStopped",
            fun anonymous_start_and_stop/0},
        {"a crash is announced as ActorStopped with reason crashed", fun crash_is_announced/0},
        {"a named start announces once", fun named_start_announces_once/0},
        {"a failed start announces nothing", fun failed_start_announces_nothing/0},
        {"the observer registers no name and creates no table", fun observer_leaves_no_trace/0}
    ]}.

%%% ---------------------------------------------------------------------

anonymous_start_and_stop() ->
    with_subscription(fun() ->
        {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
        Class = scan_class(Pid),
        ?assertMatch(#{pid := Pid, actorClass := Class}, expect('ActorSpawned', Pid)),
        stop(Pid),
        ?assertMatch(
            #{pid := Pid, actorClass := Class, reason := _}, expect('ActorStopped', Pid)
        )
    end).

crash_is_announced() ->
    with_subscription(fun() ->
        {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
        expect('ActorSpawned', Pid),
        stop(Pid),
        ?assertMatch(#{reason := crashed}, expect('ActorStopped', Pid))
    end).

named_start_announces_once() ->
    with_subscription(fun() ->
        {ok, Pid} = beamtalk_actor_registry:'spawnAs'(bt4970_eunit_named, ?FACADE, #{initial => 2}),
        expect('ActorSpawned', Pid),
        stop(Pid),
        expect('ActorStopped', Pid),
        ?assertEqual(none, next_for(Pid, 200))
    end).

failed_start_announces_nothing() ->
    with_subscription(fun() ->
        %% A non-map config is refused before the backing module starts.
        {error, _} = beamtalk_actor:safe_spawn_linked(?FACADE, not_a_map),
        ?assertEqual(none, next_any(200))
    end).

observer_leaves_no_trace() ->
    TablesBefore = lists:sort(ets:all()),
    NamesBefore = lists:sort(registered()),
    {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
    ?assertEqual(TablesBefore, lists:sort(ets:all())),
    ?assertEqual(NamesBefore, lists:sort(registered())),
    stop(Pid).

%%% ---------------------------------------------------------------------

with_subscription(Fun) ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, Event} end,
    {ok, S1} = beamtalk_announcements:subscribe('ActorSpawned', Collector, Handler, false),
    {ok, S2} = beamtalk_announcements:subscribe('ActorStopped', Collector, Handler, false),
    try
        Fun()
    after
        beamtalk_announcements:unsubscribe(S1),
        beamtalk_announcements:unsubscribe(S2),
        flush()
    end.

%% The class the process scan reports for `Pid`: the rule every actor list uses.
scan_class(Pid) ->
    case beamtalk_actor_scan:one_per_pid(beamtalk_actor_scan:info(Pid)) of
        [{Pid, Class, _}] -> Class;
        [] -> unknown
    end.

%% The next announcement of `Class` for `Pid`, ignoring other pids' events.
expect(Class, Pid) ->
    receive
        {got, #{'$beamtalk_class' := Class, pid := Pid} = Event} -> Event
    after 2000 -> error({no_announcement, Class, Pid})
    end.

next_for(Pid, Timeout) ->
    receive
        {got, #{pid := Pid} = Event} -> Event
    after Timeout -> none
    end.

next_any(Timeout) ->
    receive
        {got, Event} -> Event
    after Timeout -> none
    end.

flush() ->
    receive
        {got, _} -> flush()
    after 0 -> ok
    end.

%% Kill a linked test process without taking the test process down.
stop(Pid) ->
    unlink(Pid),
    MonRef = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    end.
