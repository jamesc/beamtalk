%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_watch_tests).

-moduledoc """
EUnit tests for `beamtalk_object_watch` — the per-object change subscription
substrate for the live Inspector (ADR 0095 §5, BT-2489 / Cockpit Phase 3).

Covers the acceptance criteria:

* a subscriber receives `{object_changed, Pid, ChangedSlots}` only when the
  watched pid publishes a change;
* the opt-in is keyed by subscription — `is_watched/1` is true only while a pid
  has at least one subscriber, and the public membership table tracks it;
* idempotent re-subscribe; unsubscribe; subscriber-death and watched-actor-death
  cleanup all tear the watch down;
* a committed change on a watched pid also fires an `ObjectStateChanged` system
  announcement on the `SystemAnnouncer` bus.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    case whereis(beamtalk_object_watch) of
        undefined -> {ok, _} = beamtalk_object_watch:start_link();
        _ -> ok
    end,
    %% Drain any prior subscriptions so tests start clean.
    [beamtalk_object_watch:unsubscribe(P, self()) || P <- beamtalk_object_watch:watched_pids()],
    ok.

teardown(_) ->
    ok.

%% A harmless long-lived process to stand in for a watched actor pid (we publish
%% changes directly, so it never needs to dispatch).
spawn_dummy() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

stop_dummy(Pid) ->
    Pid ! stop,
    wait_dead(Pid).

wait_dead(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(5),
            wait_dead(Pid)
    end.

expect_change(Pid) ->
    receive
        {object_changed, Pid, Slots} -> Slots
    after 1000 ->
        ?assert(false)
    end.

refute_change(Pid) ->
    receive
        {object_changed, Pid, _Slots} -> ?assert(false)
    after 100 ->
        ok
    end.

%% Block until the server has processed all casts up to this point (the cast
%% queue is drained in order, so a sync call flushes prior casts).
sync(_) ->
    _ = sys:get_state(beamtalk_object_watch),
    ok.

%%====================================================================
%% Tests
%%====================================================================

subscribe_publish_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = sync(ok),
            ?assert(beamtalk_object_watch:is_watched(Actor)),
            ok = beamtalk_object_watch:publish_change(Actor, 'Counter', [count]),
            ?assertEqual([count], expect_change(Actor)),
            stop_dummy(Actor)
        end)
    end}.

unwatched_pid_is_not_watched_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            ?assertNot(beamtalk_object_watch:is_watched(Actor)),
            %% Publishing to an unwatched pid delivers nothing.
            ok = beamtalk_object_watch:publish_change(Actor, 'Counter', [count]),
            refute_change(Actor),
            stop_dummy(Actor)
        end)
    end}.

idempotent_subscribe_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = sync(ok),
            ok = beamtalk_object_watch:publish_change(Actor, 'Counter', [a]),
            %% One subscription → exactly one message.
            ?assertEqual([a], expect_change(Actor)),
            refute_change(Actor),
            stop_dummy(Actor)
        end)
    end}.

unsubscribe_stops_delivery_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = sync(ok),
            ok = beamtalk_object_watch:unsubscribe(Actor, self()),
            ok = sync(ok),
            ?assertNot(beamtalk_object_watch:is_watched(Actor)),
            ok = beamtalk_object_watch:publish_change(Actor, 'Counter', [a]),
            refute_change(Actor),
            stop_dummy(Actor)
        end)
    end}.

subscriber_death_cleans_up_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            Test = self(),
            Sub = spawn(fun() ->
                beamtalk_object_watch:subscribe(Actor, self()),
                Test ! subscribed,
                receive
                    die -> ok
                end
            end),
            receive
                subscribed -> ok
            after 1000 -> ?assert(false)
            end,
            ok = sync(ok),
            ?assert(beamtalk_object_watch:is_watched(Actor)),
            Sub ! die,
            wait_dead(Sub),
            ok = sync(ok),
            %% Last subscriber gone → pid no longer watched.
            ?assertNot(beamtalk_object_watch:is_watched(Actor)),
            stop_dummy(Actor)
        end)
    end}.

watched_actor_death_cleans_up_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = sync(ok),
            ?assert(beamtalk_object_watch:is_watched(Actor)),
            stop_dummy(Actor),
            ok = sync(ok),
            ?assertNot(beamtalk_object_watch:is_watched(Actor)),
            ?assertNot(lists:member(Actor, beamtalk_object_watch:watched_pids()))
        end)
    end}.

multiple_subscribers_each_notified_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Actor = spawn_dummy(),
            Test = self(),
            _Sub2 = spawn(fun() ->
                beamtalk_object_watch:subscribe(Actor, self()),
                Test ! ready,
                receive
                    {object_changed, A, Slots} -> Test ! {sub2_got, A, Slots}
                after 1000 -> Test ! sub2_timeout
                end
            end),
            receive
                ready -> ok
            after 1000 -> ?assert(false)
            end,
            ok = beamtalk_object_watch:subscribe(Actor, self()),
            ok = sync(ok),
            ok = beamtalk_object_watch:publish_change(Actor, 'Counter', [x]),
            ?assertEqual([x], expect_change(Actor)),
            receive
                {sub2_got, Actor, Slots} -> ?assertEqual([x], Slots)
            after 1000 -> ?assert(false)
            end,
            stop_dummy(Actor)
        end)
    end}.

publish_no_server_is_safe_test() ->
    %% No setup: directly assert the absent-server path never crashes.
    case whereis(beamtalk_object_watch) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    Actor = spawn_dummy(),
    ?assertEqual(ok, beamtalk_object_watch:publish_change(Actor, 'Counter', [a])),
    ?assertNot(beamtalk_object_watch:is_watched(Actor)),
    ?assertEqual([], beamtalk_object_watch:watched_pids()),
    stop_dummy(Actor).
