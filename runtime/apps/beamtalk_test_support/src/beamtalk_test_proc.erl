%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_proc).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit process helpers (BT-4677): dummy processes and deterministic
stop/kill. Every stop waits on a monitor `'DOWN'`, never a `timer:sleep`.
""".

-export([
    spawn_idle/0,
    dead_pid/0,
    kill_and_wait/1,
    stop/1,
    shutdown/1,
    await_down/2,
    stop_registered/1,
    stop_if_bare/1,
    restart/1,
    in_fresh_process/1
]).

-doc "Spawn an unlinked process that idles until it is killed or sent `stop`.".
-spec spawn_idle() -> pid().
spawn_idle() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

-doc "Return the pid of a process that has already exited.".
-spec dead_pid() -> pid().
dead_pid() ->
    {Pid, MRef} = spawn_monitor(fun() -> ok end),
    ok = await_down(MRef, Pid),
    Pid.

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

-doc """
Unlink `Pid`, exit it with `shutdown`, and wait (5 s budget, then give up
silently) for it to die. Ignores non-pids and dead pids. For test processes
linked to the test process, where `stop/1`'s `gen_server:stop/3` would race
the link.
""".
-spec shutdown(term()) -> ok.
shutdown(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            MRef = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 5000 -> ok
            end;
        false ->
            ok
    end;
shutdown(_) ->
    ok.

-doc """
Stop the process registered as `Name` (via `stop/1`), if any. A no-op when
nothing is registered under `Name`. Replaces the hand-written
`case whereis(Name) of undefined -> ok; Pid -> gen_server:stop(Pid) end`.
""".
-spec stop_registered(atom()) -> ok.
stop_registered(Name) ->
    stop(whereis(Name)).

-doc """
Stop the process registered as `Name` only when a test started it bare: its
`$ancestors` start with a pid (a bare `start_link/0` from a test or fixture
process), not with a supervisor's registered name. A supervised child of the
runtime is left alone. For a fixture's cleanup half of "start it if it is not
running": a bare-started singleton outlives the test (a normal exit signal does not
kill a process that is not trapping exits) and makes the next module's
`ensure_all_started(beamtalk_runtime)` fail with `already_started` (BT-4962).
""".
-spec stop_if_bare(atom()) -> ok.
stop_if_bare(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            case process_info(Pid, dictionary) of
                {dictionary, Dict} ->
                    case proplists:get_value('$ancestors', Dict) of
                        [Starter | _] when is_pid(Starter) -> stop(Pid);
                        _ -> ok
                    end;
                undefined ->
                    ok
            end
    end.

-doc """
Stop the singleton registered as `Name` (if any), then start a fresh one with
`Name:start_link/0` — the module is assumed to share its registered name, as
every Beamtalk workspace singleton does. Returns what `start_link/0` returns.
""".
-spec restart(module()) -> {ok, pid()} | {error, term()} | ignore.
restart(Name) ->
    ok = stop_registered(Name),
    Name:start_link().

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

-doc """
Run `Fun()` in a short-lived process and return its result (a crash is
re-raised in the caller). For a test that calls an actor's `init/1` inline:
`init/1` writes the `'$beamtalk_actor'` marker into the calling process's
dictionary, and a marker left on a shared test process makes that process an
"actor" to every later scan (ADR 0150), listing it in actor lists.
""".
-spec in_fresh_process(fun(() -> T)) -> T when T :: term().
in_fresh_process(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, MonRef} = spawn_monitor(fun() ->
        %% bt-catcher-audit: not-applicable-reraises - test helper; the failure is
        %% sent to the caller, which re-raises it with the original stacktrace
        Parent !
            {Ref,
                try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack -> {raised, Class, Reason, Stack}
                end}
    end),
    receive
        {Ref, {ok, Result}} ->
            erlang:demonitor(MonRef, [flush]),
            Result;
        {Ref, {raised, Class, Reason, Stack}} ->
            erlang:demonitor(MonRef, [flush]),
            erlang:raise(Class, Reason, Stack);
        {'DOWN', MonRef, process, Pid, Why} ->
            erlang:error({in_fresh_process_died, Why})
    end.
