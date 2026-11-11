%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stop).

%%% **DDD Context:** Runtime Context — shared leaf

-moduledoc """
Stop an OTP process and wait a bounded time for it to exit: the contract of
`gen_server:stop/3` (`proc_lib:stop/3`), without its short-wait bug (BT-4494).

`proc_lib:stop/3` asks the process to terminate with `sys:terminate/3`. The
process's `sys` loop replies `ok` *before* it starts shutting down. The caller
then waits for the `'DOWN'` message with `Timeout` minus the time already
spent, rounded up to whole milliseconds. When that reply arrives at or after
the deadline (very likely for a wait of 1-2 ms, possible for 0 ms), the
remaining wait is negative and `receive ... after` raises
`error:timeout_value`, not `exit:timeout`. That path also never removes the
monitor, so a stray `'DOWN'` message later reaches the caller's mailbox.
Seen in OTP 28.5 (`stdlib-7.3`).

`stop/3` keeps `proc_lib:stop/3`'s results but clamps the remaining wait at
zero and removes its monitor on every path. Every bounded stop of a Beamtalk
actor or supervisor (`Actor stopWith:timeout:`, `Supervisor stopWithin:`)
goes through it.
""".

-export([stop/3]).

-doc """
Stop `Pid` with `Reason`, waiting up to `Timeout` ms (or `infinity`) for it
to exit.

Returns `ok` once `Pid` has exited with `Reason`. Otherwise exits, with the
same reasons `proc_lib:stop/3` uses:
* `noproc` when `Pid` is not alive;
* `timeout` when `Pid` has not exited within `Timeout` (it may still be
  shutting down in the background, since the terminate request was sent);
* the exit reason of `Pid` when it exited with some other reason;
* the reason `sys:terminate/3` failed with, for anything else (for example
  `{{nodedown, Node}, {sys, terminate, _}}`).

Never raises `error:timeout_value`, and leaves no `'DOWN'` message for its
monitor in the caller's mailbox.
""".
-spec stop(pid(), term(), timeout()) -> ok.
stop(Pid, Reason, Timeout) ->
    Mref = erlang:monitor(process, Pid),
    T0 = erlang:monotonic_time(millisecond),
    try sys:terminate(Pid, Reason, Timeout) of
        ok ->
            await_down(Mref, Reason, remaining(Timeout, T0))
    catch
        exit:{noproc, {sys, terminate, _}} ->
            erlang:demonitor(Mref, [flush]),
            exit(noproc);
        exit:{timeout, {sys, terminate, _}} ->
            erlang:demonitor(Mref, [flush]),
            exit(timeout);
        exit:{Reason, {sys, terminate, _}} ->
            %% The process exited with `Reason` before replying: its 'DOWN'
            %% is already on the way (OTP 28 counts this as stopped too).
            await_down(Mref, Reason, remaining(Timeout, T0));
        exit:Other ->
            erlang:demonitor(Mref, [flush]),
            exit(Other)
    end.

%% The part of `Timeout` not yet spent since `T0`, never below zero.
-spec remaining(timeout(), integer()) -> timeout().
remaining(infinity, _T0) ->
    infinity;
remaining(Timeout, T0) ->
    max(0, Timeout - (erlang:monotonic_time(millisecond) - T0)).

-spec await_down(reference(), term(), timeout()) -> ok.
await_down(Mref, Reason, Remaining) ->
    receive
        {'DOWN', Mref, _, _, Reason} ->
            ok;
        {'DOWN', Mref, _, _, Other} ->
            exit(Other)
    after Remaining ->
        erlang:demonitor(Mref, [flush]),
        exit(timeout)
    end.
