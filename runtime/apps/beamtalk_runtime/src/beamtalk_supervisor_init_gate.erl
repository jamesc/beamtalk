%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_init_gate).

%%% **DDD Context:** Runtime Context

-moduledoc """
The init gate behind `initialize` hooks (BT-3759): the ETS bag of `initializing` gates,
the claim/release protocol around a supervisor's `initialize`, and the bounded
wait an `already_started` caller makes for a concurrent initializer.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    arm_init_gate/1,
    await_initialized/1,
    ensure_init_gate_table/0,
    init_wait_timeout/0,
    run_initialize/1
]).

%% BT-3759: bag of `initializing` gates (see run_initialize/1).
-define(INIT_GATE_TABLE, beamtalk_supervisor_init_gates).

%% Upper bound on how long an already_started caller waits for a concurrent
%% initialize hook (covers an initializer that dies before it claims the gate).
-define(INIT_WAIT_TIMEOUT, 60000).

%% Poll interval for a waiting caller (see wait_loop/8).
-define(INIT_POLL_MS, 200).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================
-doc """
Run the instance-side `initialize` lifecycle hook on a freshly started
supervisor (ADR 0135 §1).

Called by `supervise/2` and the spawn family (`spawn_instance/4`,
`spawn_named_instance/5`) right after the start, in the caller's process — never the class gen_server — so messages
to the supervisor itself work (it is past `init/1`) and `initialize` may send
to its own class. `initialize` is sent to the new supervisor by ordinary
instance dispatch (`beamtalk_message_dispatch:send/3`), so inside it `self`
is the supervisor.

## Concurrent `supervise` callers (BT-3759)

Start -> initialize -> publish is made atomic for `supervise` callers with an
`initializing` gate:

  * `supervise/2` arms a gate for a freshly started supervisor pid while it
    still holds the per-class start lock, so no other `supervise` caller can
    see the pid before the gate exists.
  * This function claims the gate (records the caller as owner) and releases it
    once the hook has finished or failed; on failure the supervisor is stopped
    BEFORE the gate is released.
  * A second `supervise` caller gets `{already_started, Pid}` and calls
    `await_initialized/1`, which blocks it until the gate is released. It then
    sees either the initialised supervisor or a structured
    `supervisor_start_failed` error, never an uninitialised supervisor or a
    dead pid.

Only `supervise` waits on the gate; the spawn family starts a new instance
that no other caller can see yet. A gate that is armed but never claimed (the
first caller died before reaching this function) is reclaimed by the first
waiter to hit the deadline: the orphan supervisor is stopped and the gate
released so the next `supervise` starts afresh.

Known limit: `current` called while a hook is still running can return the
supervisor before it is initialised.

Re-entrancy: a hook that itself calls `supervise` on the same class runs in the
gate owner, which `await_initialized/1` lets straight through. A different
process that the hook waits on and that calls `supervise` would wait on the
gate until `?INIT_WAIT_TIMEOUT` and then get the structured error.
""".
-spec run_initialize(term()) -> ok.
run_initialize({beamtalk_supervisor, _ClassName, _Module, Pid} = SupTuple) ->
    claim_init_gate(Pid),
    %% The supervisor is already started (and unlinked from the caller) by the time the hook runs.
    %% If the hook raises, the caller never receives the supervisor, so stop
    %% it before re-raising: no half-started supervisor, registered name or
    %% orphaned children are left behind (BT-3720). The gate is released only
    %% after the stop so waiters observe a dead supervisor and raise.
    try
        beamtalk_message_dispatch:send(SupTuple, initialize, [])
    catch
        Class:Reason:Stacktrace ->
            stop_failed_supervisor(Pid),
            release_init_gate(Pid),
            erlang:raise(Class, Reason, Stacktrace)
    end,
    release_init_gate(Pid),
    ok.

-doc """
Block until any in-flight `initialize` hook for this supervisor has finished
(BT-3759). Returns `ok` immediately when no hook is running or when the caller
is the hook's own process. Raises a structured `supervisor_start_failed` error
if the hook failed (supervisor stopped) or the wait timed out.
""".
-spec await_initialized(term()) -> ok.
await_initialized({beamtalk_supervisor, ClassName, _Module, Pid}) when is_pid(Pid) ->
    ensure_init_gate_table(),
    Self = self(),
    Rows = ets:lookup(?INIT_GATE_TABLE, Pid),
    %% Only the `gate` row means a hook is in flight; leftover waiter rows alone
    %% never do.
    case gate_held(Pid, Rows) andalso not lists:member({Pid, owner, Self}, Rows) of
        true -> wait_for_init_gate(ClassName, Pid);
        false -> ok
    end;
await_initialized(_) ->
    ok.

-spec wait_for_init_gate(atom(), pid()) -> ok.

wait_for_init_gate(ClassName, Pid) ->
    Ref = make_ref(),
    Waiter = {Pid, waiter, self(), Ref},
    ets:insert(?INIT_GATE_TABLE, Waiter),
    %% Re-check after registering: if the gate was released in between we will
    %% not be notified.
    Rows = ets:lookup(?INIT_GATE_TABLE, Pid),
    case gate_held(Pid, Rows) of
        false ->
            gate_released(ClassName, Pid, Waiter, Ref);
        true ->
            SupMon = erlang:monitor(process, Pid),
            Deadline = erlang:monotonic_time(millisecond) + init_wait_timeout(),
            wait_loop(ClassName, Pid, Waiter, Ref, SupMon, undefined, Rows, Deadline)
    end.

%% The gate is released when its `gate` row is gone; stray waiter rows (inserted
%% after the release took the gate) do not hold it.
-spec gate_held(pid(), list()) -> boolean().

gate_held(Pid, Rows) ->
    lists:member({Pid, gate}, Rows).

-spec gate_released(atom(), pid(), tuple(), reference()) -> ok.

gate_released(ClassName, Pid, Waiter, Ref) ->
    ets:delete_object(?INIT_GATE_TABLE, Waiter),
    flush_init_notice(Ref),
    check_initialized(ClassName, Pid).

%% Poll every ?INIT_POLL_MS so a waiter that registered before the owner
%% claimed the gate (no owner row yet) still starts monitoring the owner once it
%% appears, and so a gate that is armed but never claimed is detected at the
%% deadline instead of leaving `supervise` stuck for good.
-spec wait_loop(
    atom(), pid(), tuple(), reference(), reference(), reference() | undefined, list(), integer()
) -> ok.

wait_loop(ClassName, Pid, Waiter, Ref, SupMon, OwnerMon0, Rows, Deadline) ->
    OwnerMon =
        case {OwnerMon0, [O || {_, owner, O} <- Rows]} of
            {undefined, [Owner | _]} -> erlang:monitor(process, Owner);
            _ -> OwnerMon0
        end,
    receive
        {Ref, initialized} ->
            demonitor_all([SupMon, OwnerMon]),
            check_initialized(ClassName, Pid);
        {'DOWN', SupMon, process, Pid, _} ->
            %% Supervisor gone: drop its gate rows (also covers a gate that was
            %% armed but never claimed).
            demonitor_all([OwnerMon]),
            release_init_gate(Pid),
            flush_init_notice(Ref),
            init_wait_failed(ClassName);
        {'DOWN', OwnerMon, process, _, _} when OwnerMon =/= undefined ->
            %% The hook's process died without finishing: nobody will
            %% release the gate or publish the supervisor.
            demonitor_all([SupMon]),
            abandon_init_gate(ClassName, Pid, Ref)
    after ?INIT_POLL_MS ->
        NewRows = ets:lookup(?INIT_GATE_TABLE, Pid),
        case gate_held(Pid, NewRows) of
            false ->
                demonitor_all([SupMon, OwnerMon]),
                gate_released(ClassName, Pid, Waiter, Ref);
            true ->
                case erlang:monotonic_time(millisecond) >= Deadline of
                    false ->
                        wait_loop(ClassName, Pid, Waiter, Ref, SupMon, OwnerMon, NewRows, Deadline);
                    true ->
                        demonitor_all([SupMon, OwnerMon]),
                        case [O || {_, owner, O} <- NewRows] of
                            [] ->
                                %% Gate armed but never claimed: the first caller
                                %% never reached run_initialize/1. Reclaim the
                                %% orphan so the next supervise starts afresh.
                                abandon_init_gate(ClassName, Pid, Ref);
                            _ ->
                                ets:delete_object(?INIT_GATE_TABLE, Waiter),
                                init_wait_failed(ClassName)
                        end
                end
        end
    end.

-spec abandon_init_gate(atom(), pid(), reference()) -> no_return().

abandon_init_gate(ClassName, Pid, Ref) ->
    stop_failed_supervisor(Pid),
    release_init_gate(Pid),
    flush_init_notice(Ref),
    init_wait_failed(ClassName).

-spec flush_init_notice(reference()) -> ok.

flush_init_notice(Ref) ->
    receive
        {Ref, initialized} -> ok
    after 0 -> ok
    end.

-spec demonitor_all([reference() | undefined]) -> ok.

demonitor_all(Refs) ->
    lists:foreach(
        fun
            (undefined) -> ok;
            (R) -> erlang:demonitor(R, [flush])
        end,
        Refs
    ).

-spec check_initialized(atom(), pid()) -> ok.

check_initialized(ClassName, Pid) ->
    case is_process_alive(Pid) of
        true -> ok;
        false -> init_wait_failed(ClassName)
    end.

-spec init_wait_failed(atom()) -> no_return().

init_wait_failed(ClassName) ->
    beamtalk_error:raise(
        beamtalk_error:new(
            supervisor_start_failed,
            ClassName,
            supervise,
            <<"supervisor initialize did not complete for a concurrent supervise call">>
        )
    ).

-doc "Create the `initializing` gate table if needed (public bag, runtime-owned).".
-spec ensure_init_gate_table() -> ok.

ensure_init_gate_table() ->
    case ets:info(?INIT_GATE_TABLE, id) of
        undefined ->
            try
                ets:new(?INIT_GATE_TABLE, [named_table, public, bag])
            catch
                error:badarg -> ok
            end,
            ok;
        _ ->
            ok
    end.

-spec arm_init_gate(pid()) -> ok.

arm_init_gate(Pid) ->
    ensure_init_gate_table(),
    %% Drop rows left by supervisors that died before their gate was claimed.
    lists:foreach(
        fun(Dead) -> ets:delete(?INIT_GATE_TABLE, Dead) end,
        [
            P
         || P <- lists:usort([element(1, R) || R <- ets:tab2list(?INIT_GATE_TABLE)]),
            not is_process_alive(P)
        ]
    ),
    ets:insert(?INIT_GATE_TABLE, {Pid, gate}),
    ok.

-spec claim_init_gate(pid()) -> ok.

claim_init_gate(Pid) ->
    ensure_init_gate_table(),
    ets:insert(?INIT_GATE_TABLE, [{Pid, gate}, {Pid, owner, self()}]),
    ok.

-spec release_init_gate(pid()) -> ok.

release_init_gate(Pid) ->
    ensure_init_gate_table(),
    lists:foreach(
        fun
            ({_, waiter, WaiterPid, Ref}) -> WaiterPid ! {Ref, initialized};
            (_) -> ok
        end,
        ets:take(?INIT_GATE_TABLE, Pid)
    ),
    ok.

-doc """
Stop a supervisor whose `initialize` hook raised. Best effort: the process may
already be gone, and the original error is the one the caller must see.
""".
-spec stop_failed_supervisor(term()) -> ok.
stop_failed_supervisor(Pid) when is_pid(Pid), Pid =/= self() ->
    try
        gen_server:stop(Pid)
    catch
        _:_ -> ok
    end,
    ok;
stop_failed_supervisor(_) ->
    ok.

%% Overridable (application env) so tests can exercise the timeout paths.
-spec init_wait_timeout() -> non_neg_integer().

init_wait_timeout() ->
    application:get_env(beamtalk_runtime, init_wait_timeout_ms, ?INIT_WAIT_TIMEOUT).
