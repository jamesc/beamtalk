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
start path announces `ActorSpawned` and hands the pid to an observer process that
lives as long as the actor and announces `ActorStopped` when it goes down. Nothing is
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
        {"a failing class lookup does not fail the start", fun failing_lookup_does_not_fail/0},
        {"an actor already gone when observed is announced as crashed",
            fun already_gone_is_announced_as_crashed/0},
        {"a non-pid start result is ignored", fun non_pid_start_result_is_ignored/0},
        {"the observer hibernates and still announces the stop",
            fun observer_hibernates_and_wakes/0},
        {"a stray message does not leave the observer awake", fun stray_message_rehibernates/0},
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

%% Announcing is best-effort and must never fail or delay a start: the class
%% lookup (`beamtalk_actor_scan:info/1` raises `function_clause` for a pid on
%% another node) falls back to `unknown` instead of raising after the backing
%% process has already started.
failing_lookup_does_not_fail() ->
    with_subscription(fun() ->
        RemotePid = remote_pid(),
        ?assertNotEqual(node(), node(RemotePid)),
        ?assertEqual(ok, beamtalk_actor:native_started(RemotePid)),
        ?assertMatch(
            #{pid := RemotePid, actorClass := unknown}, expect('ActorSpawned', RemotePid)
        )
    end).

%% The observer sets its monitor after `native_started/1` returns. An actor that
%% stopped in that window gives `'DOWN'` with reason `noproc`, whose real exit
%% reason is not observable without a wait on the start path. It is announced as
%% `crashed`. That also reports a prompt clean `stop` in that window as a crash
%% (a named start is reachable by its name before the observer is scheduled): a
%% false alarm is cheaper than a hidden crash.
already_gone_is_announced_as_crashed() ->
    with_subscription(fun() ->
        {Pid, MonRef} = spawn_monitor(fun() -> ok end),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        end,
        ok = beamtalk_actor:native_started(Pid),
        expect('ActorSpawned', Pid),
        ?assertMatch(#{reason := crashed}, expect('ActorStopped', Pid))
    end).

%% A backing `start_link/1` that answers `{ok, NotAPid}` already started its
%% process; announcing is best-effort and must not turn that into a raw error.
non_pid_start_result_is_ignored() ->
    ?assertEqual(ok, beamtalk_actor:native_started(not_a_pid)).

%% A pid on a node that is not this one and not connected: NEW_PID_EXT
%% (tag 88), node name as an ATOM_UTF8_EXT, then id, serial and creation.
remote_pid() ->
    Node = <<"bt4970_fake@nowhere">>,
    binary_to_term(<<131, 88, 119, (byte_size(Node)), Node/binary, 1:32, 0:32, 1:32>>).

%% The observer sits in `erlang:hibernate/3` into an exported function, so it
%% holds no stack or fun into the module code it started in: a code purge of
%% `beamtalk_actor` cannot kill it (which would lose the stop), and it keeps a
%% minimal heap for the actor's whole life.
observer_hibernates_and_wakes() ->
    with_subscription(fun() ->
        {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
        expect('ActorSpawned', Pid),
        {monitored_by, Monitors} = process_info(Pid, monitored_by),
        ?assertEqual(ok, wait_until_one_hibernates(Monitors, 100)),
        stop(Pid),
        ?assertMatch(#{reason := crashed}, expect('ActorStopped', Pid))
    end).

%% A message that is not the actor's `'DOWN'` wakes the observer out of
%% hibernation. It must drop it and hibernate again, or it would sit in a
%% `receive` inside the module's code, where two reloads of that module kill it.
stray_message_rehibernates() ->
    with_subscription(fun() ->
        {ok, Pid} = beamtalk_actor:safe_spawn_linked(?FACADE, #{initial => 1}),
        expect('ActorSpawned', Pid),
        {monitored_by, Monitors} = process_info(Pid, monitored_by),
        ?assertEqual(ok, wait_until_one_hibernates(Monitors, 100)),
        [Observer | _] = [M || M <- Monitors, is_hibernated(M)],
        Observer ! stray,
        %% It woke to handle the message; wait for it to settle again.
        timer:sleep(50),
        ?assertEqual(ok, wait_until_one_hibernates([Observer], 100)),
        stop(Pid),
        ?assertMatch(#{reason := crashed}, expect('ActorStopped', Pid))
    end).

is_hibernated(Pid) ->
    is_pid(Pid) andalso
        process_info(Pid, current_function) =:= {current_function, {erlang, hibernate, 3}}.

wait_until_one_hibernates(_Monitors, 0) ->
    timeout;
wait_until_one_hibernates(Monitors, N) ->
    Hibernated = [
        M
     || M <- Monitors,
        is_pid(M),
        process_info(M, current_function) =:= {current_function, {erlang, hibernate, 3}}
    ],
    case Hibernated of
        [_ | _] ->
            ok;
        [] ->
            timer:sleep(20),
            wait_until_one_hibernates(Monitors, N - 1)
    end.

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
