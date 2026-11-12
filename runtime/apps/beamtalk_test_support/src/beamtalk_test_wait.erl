%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_wait).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit waiters (BT-4677): poll for a condition, wait for a process to
die by monitor, wait for a class to register, or sync with a gen_server.

Replaces the ~70 per-file `wait_until/1,2` / `wait_for_class/2` copies, each
with different tries and sleeps. Budgets are expressed in **milliseconds**,
not retries, so the worst case of a wait is explicit and a test's EUnit
timeout can be checked against it directly (see `default_timeout/0`).

Every waiter returns `ok` or raises `error({wait_timeout, Reason})`; none
returns an atom like `timeout` for the caller to forget to check.
""".

-export([
    idle/1, until/1, until/2, until/3, dead/1, dead/2, class/1, class/2, sync/1, default_timeout/0
]).

-define(DEFAULT_TIMEOUT_MS, 6000).
-define(DEFAULT_INTERVAL_MS, 5).

-doc "Default wait budget in milliseconds (6 s, the largest budget any per-file waiter used); stay below the EUnit test timeout.".
-spec default_timeout() -> pos_integer().
default_timeout() -> ?DEFAULT_TIMEOUT_MS.

-doc "`until/2` with the default budget (`default_timeout/0`).".
-spec until(fun(() -> boolean())) -> ok.
until(Pred) ->
    until(Pred, ?DEFAULT_TIMEOUT_MS).

-doc "Poll `Pred` every 5 ms until it returns `true` or `TimeoutMs` elapses.".
-spec until(fun(() -> boolean()), timeout()) -> ok.
until(Pred, TimeoutMs) ->
    until(Pred, TimeoutMs, ?DEFAULT_INTERVAL_MS).

-doc """
Poll `Pred` every `IntervalMs` until it returns `true`. Raises
`error({wait_timeout, {predicate, LastResult}})` after `TimeoutMs`. The
predicate is evaluated one final time at the deadline, so a condition that
becomes true just as the budget ends still passes.
""".
-spec until(fun(() -> boolean()), timeout(), pos_integer()) -> ok.
until(Pred, TimeoutMs, IntervalMs) when is_function(Pred, 0) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    poll(Pred, Deadline, IntervalMs).

poll(Pred, Deadline, IntervalMs) ->
    case Pred() of
        true ->
            ok;
        Other ->
            Now = erlang:monotonic_time(millisecond),
            case Now >= Deadline of
                true ->
                    error({wait_timeout, {predicate, Other}});
                false ->
                    timer:sleep(min(IntervalMs, Deadline - Now)),
                    poll(Pred, Deadline, IntervalMs)
            end
    end.

-doc "`dead/2` with the default budget.".
-spec dead(pid() | [pid()]) -> ok.
dead(PidOrPids) -> dead(PidOrPids, ?DEFAULT_TIMEOUT_MS).

-doc """
Block until every pid is dead, using a monitor and its `'DOWN'` message (no
sleep, no polling). An already-dead pid returns immediately. Raises
`error({wait_timeout, {alive, Pids}})` on timeout.
""".
-spec dead(pid() | [pid()], timeout()) -> ok.
dead(Pid, TimeoutMs) when is_pid(Pid) ->
    dead([Pid], TimeoutMs);
dead(Pids, TimeoutMs) when is_list(Pids) ->
    Refs = [{monitor(process, P), P} || P <- Pids],
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_down(Refs, Deadline).

await_down([], _Deadline) ->
    ok;
await_down([{Ref, Pid} | Rest] = All, Deadline) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {'DOWN', Ref, process, Pid, _} -> await_down(Rest, Deadline)
    after Left ->
        [demonitor(R, [flush]) || {R, _} <- All],
        error({wait_timeout, {alive, [P || {_, P} <- All]}})
    end.

-doc "`class/2` with the default budget.".
-spec class(atom()) -> ok.
class(ClassName) -> class(ClassName, ?DEFAULT_TIMEOUT_MS).

-doc "Block until `ClassName` is registered as a live class process.".
-spec class(atom(), timeout()) -> ok.
class(ClassName, TimeoutMs) ->
    try
        until(
            fun() -> beamtalk_class_registry:whereis_class(ClassName) =/= undefined end,
            TimeoutMs
        )
    catch
        error:{wait_timeout, _} -> error({class_not_registered, ClassName})
    end.

-doc """
Synchronise with a process: return once it has handled every message sent to
it before this call. Takes a pid or registered name; uses `sys:get_state/2`
so it works for any OTP process and fails (crashes the test) if it is dead
or unresponsive. Returns `ok`.
""".
-spec sync(pid() | atom()) -> ok.
sync(PidOrName) ->
    _ = sys:get_state(PidOrName, ?DEFAULT_TIMEOUT_MS),
    ok.

-doc """
Synchronise with a plain (non-OTP) process, where `sync/1` cannot work:
return once `Pid` is blocked in a `receive` with an empty message queue, i.e.
it has consumed everything sent to it so far. Returns `ok` immediately if the
process is dead. Only meaningful for messages already sent by the caller.
""".
-spec idle(pid()) -> ok.
idle(Pid) ->
    until(
        fun() ->
            case process_info(Pid, [status, message_queue_len]) of
                undefined -> true;
                [{status, waiting}, {message_queue_len, 0}] -> true;
                _ -> false
            end
        end
    ).
