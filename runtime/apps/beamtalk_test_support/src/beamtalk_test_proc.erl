%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_proc).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit process helpers (BT-4677): dummy processes and deterministic
stop/kill. Every stop waits on a monitor `'DOWN'`, never a `timer:sleep`.
""".

-export([spawn_idle/0, kill_and_wait/1, stop/1, await_down/2]).

-doc "Spawn an unlinked process that idles until it is killed or sent `stop`.".
-spec spawn_idle() -> pid().
spawn_idle() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

-doc "Kill `Pid` (`kill` reason) and block until it is dead. No-op on a dead pid.".
-spec kill_and_wait(pid()) -> ok.
kill_and_wait(Pid) ->
    MRef = monitor(process, Pid),
    exit(Pid, kill),
    await_down(MRef, Pid).

-doc """
Stop `Pid` gracefully: `gen_server:stop/1` for an OTP process, falling back
to a `stop` message for a plain process (see `spawn_idle/0`). Blocks until
dead; ignores non-pids and dead pids.
""".
-spec stop(term()) -> ok.
stop(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            MRef = monitor(process, Pid),
            case is_otp_process(Pid) of
                true ->
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end;
                false ->
                    Pid ! stop
            end,
            await_down(MRef, Pid)
    end;
stop(_) ->
    ok.

%% proc_lib-started processes (gen_server, supervisor, ...) record `$ancestors`.
is_otp_process(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} -> lists:keymember('$ancestors', 1, Dict);
        undefined -> false
    end.

-doc """
Wait for the `'DOWN'` of an existing monitor `MRef` on `Pid` (5 s budget);
raises `error({wait_timeout, {alive, [Pid]}})` otherwise.
""".
-spec await_down(reference(), pid()) -> ok.
await_down(MRef, Pid) ->
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 ->
        demonitor(MRef, [flush]),
        error({wait_timeout, {alive, [Pid]}})
    end.
