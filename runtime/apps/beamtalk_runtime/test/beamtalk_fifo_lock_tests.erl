%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_fifo_lock_tests).

-moduledoc """
EUnit tests for `beamtalk_fifo_lock` (BT-3994): waiters are served in arrival
order and promptly on release, a dead holder hands the lock on, a raising
critical section releases it, and a bounded wait gives up with
`{error, timeout}` without wedging the queue.
""".

-include_lib("eunit/include/eunit.hrl").

%% Every test runs with the table owned by a process that outlives it, as the
%% runtime app arranges at start.
fifo_lock_test_() ->
    {setup, fun beamtalk_supervisor_test_helper:ensure_lock_table/0, [
        fun uncontended_runs_fun/0,
        fun waiters_are_served_in_arrival_order_and_promptly/0,
        fun dead_holder_hands_the_lock_on/0,
        fun dead_head_waiter_is_skipped/0,
        fun raising_critical_section_releases/0,
        fun bounded_wait_times_out_and_leaves_the_queue/0,
        fun timed_out_waiter_does_not_strand_the_next/0,
        fun recursive_lock_raises/0
    ]}.

%% Without a long-lived owner the table goes when its creator exits; a waiter
%% then answers `{error, lost}` instead of crashing or running unprotected.
lost_table_is_an_error_test() ->
    Key = key(),
    Test = self(),
    case ets:whereis(beamtalk_fifo_locks) of
        undefined -> ok;
        Tid -> ets:delete(Tid)
    end,
    Holder = spawn(fun() ->
        beamtalk_fifo_lock:with_lock(Key, 1000, fun() ->
            Test ! {holding, self()},
            receive
                release -> ok
            end
        end)
    end),
    receive
        {holding, Holder} -> ok
    after 2000 -> error(holder_not_holding)
    end,
    Waiter = spawn(fun() ->
        Test ! {waiter, beamtalk_fifo_lock:with_lock(Key, 5000, fun() -> ran end)}
    end),
    wait_until(fun() -> lists:member(Waiter, beamtalk_fifo_lock:queue(Key)) end),
    Holder ! release,
    try
        receive
            {waiter, R} -> ?assertEqual({error, lost}, R)
        after 2000 -> error(waiter_did_not_answer)
        end
    after
        beamtalk_supervisor_test_helper:ensure_lock_table()
    end.

uncontended_runs_fun() ->
    ?assertEqual({ok, 42}, beamtalk_fifo_lock:with_lock(key(), 1000, fun() -> 42 end)).

waiters_are_served_in_arrival_order_and_promptly() ->
    Key = key(),
    Test = self(),
    Holder = hold(Key),
    Waiters = [
        begin
            W = spawn_link(fun() ->
                {ok, ok} = beamtalk_fifo_lock:with_lock(Key, 5000, fun() ->
                    Test ! {acquired, self(), now_ms()},
                    ok
                end)
            end),
            %% Queued before the next one arrives.
            wait_until(fun() -> lists:member(W, beamtalk_fifo_lock:queue(Key)) end),
            W
        end
     || _ <- lists:seq(1, 3)
    ],
    ?assertEqual([Holder | Waiters], beamtalk_fifo_lock:queue(Key)),
    Released = now_ms(),
    Holder ! release,
    Order = [
        receive
            {acquired, W, At} -> {W, At}
        after 2000 -> error({not_acquired, W})
        end
     || W <- Waiters
    ],
    ?assertEqual(Waiters, [W || {W, _} <- Order]),
    %% Served on notification, not after a poll interval or back-off.
    [{_, FirstAt} | _] = Order,
    ?assert(FirstAt - Released < 150),
    wait_until(fun() -> beamtalk_fifo_lock:queue(Key) =:= [] end).

dead_holder_hands_the_lock_on() ->
    Key = key(),
    Holder = hold(Key),
    unlink(Holder),
    Test = self(),
    spawn_link(fun() ->
        Test ! {result, beamtalk_fifo_lock:with_lock(Key, 5000, fun() -> got_it end)}
    end),
    wait_until(fun() -> length(beamtalk_fifo_lock:queue(Key)) =:= 2 end),
    exit(Holder, kill),
    receive
        {result, R} -> ?assertEqual({ok, got_it}, R)
    after 2000 -> error(not_handed_on)
    end.

%% A waiter killed while queued is skipped when it reaches the head.
dead_head_waiter_is_skipped() ->
    Key = key(),
    Test = self(),
    Holder = hold(Key),
    Doomed = spawn(fun() -> beamtalk_fifo_lock:with_lock(Key, 5000, fun() -> ok end) end),
    wait_until(fun() -> lists:member(Doomed, beamtalk_fifo_lock:queue(Key)) end),
    spawn_link(fun() ->
        Test ! {result, beamtalk_fifo_lock:with_lock(Key, 5000, fun() -> got_it end)}
    end),
    wait_until(fun() -> length(beamtalk_fifo_lock:queue(Key)) =:= 3 end),
    exit(Doomed, kill),
    Holder ! release,
    receive
        {result, R} -> ?assertEqual({ok, got_it}, R)
    after 2000 -> error(not_handed_on)
    end.

recursive_lock_raises() ->
    Key = key(),
    ?assertError(
        {recursive_lock, Key},
        beamtalk_fifo_lock:with_lock(Key, 1000, fun() ->
            beamtalk_fifo_lock:with_lock(Key, 1000, fun() -> ok end)
        end)
    ),
    ?assertEqual([], beamtalk_fifo_lock:queue(Key)).

raising_critical_section_releases() ->
    Key = key(),
    ?assertError(boom, beamtalk_fifo_lock:with_lock(Key, 1000, fun() -> error(boom) end)),
    ?assertEqual([], beamtalk_fifo_lock:queue(Key)),
    ?assertEqual({ok, ok}, beamtalk_fifo_lock:with_lock(Key, 1000, fun() -> ok end)).

bounded_wait_times_out_and_leaves_the_queue() ->
    Key = key(),
    Holder = hold(Key),
    Start = now_ms(),
    ?assertEqual(
        {error, timeout}, beamtalk_fifo_lock:with_lock(Key, 150, fun() -> error(ran) end)
    ),
    Waited = now_ms() - Start,
    ?assert(Waited >= 150),
    ?assert(Waited < 1000),
    %% The timed-out waiter is gone; the holder still holds the lock.
    ?assertEqual([Holder], beamtalk_fifo_lock:queue(Key)),
    Holder ! release,
    wait_until(fun() -> beamtalk_fifo_lock:queue(Key) =:= [] end).

timed_out_waiter_does_not_strand_the_next() ->
    %% Holder, then a short-wait caller, then a long-wait caller: when the
    %% short one gives up and the holder releases, the long one still gets in.
    Key = key(),
    Test = self(),
    Holder = hold(Key),
    Short = spawn_link(fun() ->
        Test ! {short, beamtalk_fifo_lock:with_lock(Key, 100, fun() -> short end)}
    end),
    wait_until(fun() -> lists:member(Short, beamtalk_fifo_lock:queue(Key)) end),
    spawn_link(fun() ->
        Test ! {long, beamtalk_fifo_lock:with_lock(Key, 5000, fun() -> long end)}
    end),
    receive
        {short, S} -> ?assertEqual({error, timeout}, S)
    after 2000 -> error(short_did_not_time_out)
    end,
    Holder ! release,
    receive
        {long, L} -> ?assertEqual({ok, long}, L)
    after 2000 -> error(long_not_served)
    end.

%%% Helpers

key() ->
    {?MODULE, make_ref()}.

%% A process that takes `Key` and holds it until sent `release`.
hold(Key) ->
    Test = self(),
    Pid = spawn_link(fun() ->
        beamtalk_fifo_lock:with_lock(Key, 1000, fun() ->
            Test ! {holding, self()},
            receive
                release -> ok
            end
        end)
    end),
    receive
        {holding, Pid} -> Pid
    after 2000 -> error(holder_not_holding)
    end.

now_ms() ->
    erlang:monotonic_time(millisecond).

wait_until(Pred) ->
    wait_until(Pred, 400).

wait_until(Pred, 0) ->
    ?assert(Pred());
wait_until(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(5),
            wait_until(Pred, N - 1)
    end.
