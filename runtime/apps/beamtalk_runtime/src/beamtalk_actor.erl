%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor).
-behaviour(gen_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
Beamtalk Actor Runtime (gen_server wrapper)

Every Beamtalk actor is a BEAM process running a gen_server.
This module provides the actor behavior template and message dispatch.

## Actor Lifecycle

Actors support lifecycle methods:
- `pid` - Returns the raw Erlang PID backing the actor
- `isAlive` - Returns true/false depending on whether the actor process is running
- `monitor` - Creates an Erlang monitor on the actor process
- `onExit:` - Monitors the actor and calls a block with the exit reason when it dies
- `stop` - Gracefully stops the actor process

These methods are handled at the SEND site (via async_send/4 and sync_send/3)
rather than inside the actor, because a dead actor can't process messages.

WARNING: isAlive check-then-act is inherently racy. The actor could die
between the isAlive check and a subsequent message send. For robust
lifecycle management, use monitors instead of isAlive polling:

```
%% Racy pattern (avoid):
counter isAlive ifTrue: [counter increment]

%% Robust pattern (preferred):
ref := counter monitor
counter increment   %% handle 'DOWN' message if actor dies
```

## Actor State Structure

Each actor maintains state in a map:
```erlang
#{
  '$beamtalk_class' => 'Counter',
  '__methods__' => #{
    increment => fun handle_increment/2,
    getValue => fun handle_getValue/2
  },
  %% User-defined state fields
  value => 0
}
```

## Message Protocol

Generated actors send messages in one of three forms:

**Async (cast)** - Returns a future:
```erlang
FuturePid = beamtalk_future:new(),
gen_server:cast(ActorPid, {Selector, Args, FuturePid}),
FuturePid
```

**Fire-and-forget (cast)** - No return value:
```erlang
gen_server:cast(ActorPid, {cast, Selector, Args}),
ok
```

**Sync (call)** - Blocks until result:
```erlang
gen_server:call(ActorPid, {Selector, Args})
```

## Message Dispatch

The dispatch/4 function looks up the method in the `__methods__` map:
- If found, calls the method function with Args, Self, and State
- If not found, calls doesNotUnderstand handler if defined
- If no doesNotUnderstand handler, returns {error, {unknown_message, Selector}, State}

## Spawn Architecture (BT-411, BT-1417)

There are multiple paths that create actor processes. The `initialize`
hook (if defined) is called inside the generated `init/1` callback,
so it runs for ALL spawn paths — direct, supervised, and named.

| Path | Entry | Context | Initialize? |
|------|-------|---------|-------------|
| Module:spawn/0,1 | gen_server:start_link → init/1 | Batch/tests | Yes |
| REPL spawn | Module:spawn/0,1 + register_spawned/4 | REPL | Yes |
| class_send → spawn | erlang:apply(Module, spawn, Args) | Runtime | Yes |
| Supervisor child | start_link/1 → gen_server:start_link → init/1 | Supervised | Yes |
| dynamic_object | gen_server:start_link(?MODULE, ...) | Internal | No (by design) |

Key invariant: `initialize` dispatch lives in generated `init/1`, not
in `spawn/0,1`. This ensures supervised children also run initialize.
The `register_spawned/4` function handles REPL actor registry
integration as a separate concern after spawn completes.

## Code Hot Reload

The code_change/3 callback supports state migration during hot reload.
By default, it preserves existing state. Generated actors can override
this to migrate state schemas.

## Example Generated Actor

```erlang
-module(beamtalk_counter).
-behaviour(gen_server).

%% Public API
start_link(Args) ->
    beamtalk_actor:start_link(?MODULE, Args).

%% Callbacks delegated to beamtalk_actor
init(Args) ->
    beamtalk_actor:init(#{
        '$beamtalk_class' => 'Counter',
        '__methods__' => #{
            increment => fun handle_increment/2,
            getValue => fun handle_getValue/2
        },
        value => proplists:get_value(initial, Args, 0)
    }).

handle_cast(Msg, State) ->
    beamtalk_actor:handle_cast(Msg, State).

handle_call(Msg, From, State) ->
    beamtalk_actor:handle_call(Msg, From, State).

handle_info(Msg, State) ->
    beamtalk_actor:handle_info(Msg, State).

code_change(OldVsn, State, Extra) ->
    beamtalk_actor:code_change(OldVsn, State, Extra).

terminate(Reason, State) ->
    beamtalk_actor:terminate(Reason, State).

%% Method implementations
handle_increment([], State) ->
    Value = maps:get(value, State),
    NewValue = Value + 1,
    NewState = maps:put(value, NewValue, State),
    {noreply, NewState}.

handle_getValue([], State) ->
    Value = maps:get(value, State),
    {reply, Value, State}.
```
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/2, start_link/3, start_link_supervised/3, register_spawned/4]).
-export([await_initialize/1, safe_spawn/2]).

%% Message send helpers (lifecycle-aware wrappers)
-export([async_send/4, sync_send/3, sync_send/4, cast_send/3]).

%% Propagated context (ADR 0069 Phase 2b)
-export([get_propagated_ctx/0, restore_propagated_ctx/1]).

%% Application-level trace context (BT-1625)
-export([set_trace_context/1, get_trace_context/0, clear_trace_context/0]).

%% Causal trace context (BT-1633)
-export([get_causal_ctx/0]).

%% gen_server callbacks (for generated actors to delegate to)
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Internal dispatch
-export([dispatch/4, make_self/1]).

%% Named registration (ADR 0079, BT-1987)
-export([
    is_beamtalk_actor/1,
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    all_registered/0,
    'spawnAs'/2,
    'spawnAs'/3,
    reserved_name/1
]).

%% Beamtalk stdlib FFI shims for Actor.bt named registration (ADR 0079, BT-1988)
%% Selectors are re-derived by beamtalk_erlang_proxy from the first keyword;
%% these names deliberately differ from the runtime `spawnAs/2,3` entry points
%% so they do not collide at the FFI dispatch layer.
-export([
    doSpawnAs/2,
    doSpawnWith/3,
    registerAs/2,
    unregister/1,
    registeredName/1,
    isRegistered/1,
    named/2,
    allRegistered/1
]).

%% Lifecycle telemetry (BT-1638: called from compiled actor init/terminate)
-export([maybe_execute_telemetry/3]).

%%% Public API

-doc """
Start an actor as part of a supervision tree.
Module should implement the beamtalk actor pattern.
Args are passed to the module's init/1 callback.
""".
-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Args) ->
    gen_server:start_link(Module, Args, []).

-doc """
Start a registered actor as part of a supervision tree.
Name can be {local, Name}, {global, GlobalName}, or {via, Module, ViaName}.
""".
-spec start_link(term(), module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Name, Module, Args) ->
    gen_server:start_link(Name, Module, Args, []).

-doc """
Start an actor under the workspace actor supervisor.
This is used by beamtalk_actor_sup with simple_one_for_one strategy.
Module:Function should spawn the actor and return {ok, Pid}.
""".
-spec start_link_supervised(module(), atom(), list()) -> {ok, pid()} | {error, term()}.
start_link_supervised(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

-doc """
Register an already-spawned actor with the REPL actor registry.
Called by generated REPL code after Module:spawn() returns.
This separates spawn lifecycle (handled by Module:spawn) from
REPL tracking (handled here).

Replaced the former spawn_with_registry/3,4 functions which both
spawned and registered actors. BT-1417: initialize now runs inside
init/1, so all spawn paths (including supervised) call it automatically.
""".
-spec register_spawned(pid(), pid(), atom(), module()) -> ok | {error, term()}.
register_spawned(RegistryPid, ActorPid, ClassName, Module) ->
    case application:get_env(beamtalk_runtime, actor_spawn_callback) of
        {ok, CallbackMod} ->
            try
                CallbackMod:on_actor_spawned(RegistryPid, ActorPid, ClassName, Module)
            catch
                error:undef ->
                    %% Callback module doesn't implement on_actor_spawned/4
                    ?LOG_WARNING("Actor spawn callback not implemented", #{
                        callback => CallbackMod,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {callback_undef, CallbackMod}};
                error:#beamtalk_error{} = BtError ->
                    %% Structured beamtalk error from callback
                    ?LOG_ERROR("Actor spawn callback failed with beamtalk error", #{
                        callback => CallbackMod,
                        error => BtError,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {beamtalk_error, BtError}};
                Kind:Reason:Stacktrace ->
                    %% Unexpected failure in callback
                    ?LOG_ERROR("Actor spawn callback failed", #{
                        callback => CallbackMod,
                        kind => Kind,
                        reason => Reason,
                        stacktrace => Stacktrace,
                        registry_pid => RegistryPid,
                        actor_pid => ActorPid,
                        class => ClassName,
                        domain => [beamtalk, runtime]
                    }),
                    {error, {Kind, Reason}}
            end;
        undefined ->
            ok
    end.

-doc """
BT-1541: Wait for handle_continue to finish after start_link.

When an actor has an `initialize` method, init/1 returns
{ok, State, {continue, initialize}} and the actual dispatch happens
in handle_continue/2. This means start_link returns {ok, Pid} BEFORE
initialize has run. This function synchronizes by sending a system
message (sys:get_state) which OTP guarantees is processed after
handle_continue.

Returns `ok` if the process is still alive after initialization,
or `{error, Reason}` if it crashed during handle_continue.
""".
-spec await_initialize(pid()) -> ok | {error, term()}.
await_initialize(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    try sys:get_state(Pid, 5000) of
        _State ->
            erlang:demonitor(MonRef, [flush]),
            ok
    catch
        exit:{noproc, _} ->
            %% Process already dead — wait for the DOWN message unconditionally.
            %% The monitor was set before sys:get_state, so DOWN is guaranteed.
            receive
                {'DOWN', MonRef, process, Pid, Reason} -> {error, Reason}
            end;
        exit:{Reason, _} ->
            %% gen_server stop reason wrapped by sys: {StopReason, {sys, get_state, ...}}
            %% Reason is the full stop reason (e.g. {error, function_clause}).
            erlang:demonitor(MonRef, [flush]),
            {error, Reason}
    end.

-doc """
BT-1541: Spawn an actor with trap_exit + initialize synchronization.

Handles the full spawn sequence:
1. Trap exits so a failed start_link or handle_continue doesn't kill caller
2. Call gen_server:start_link
3. If start_link succeeds, wait for handle_continue (initialize) to complete
4. Restore trap_exit and return {ok, Pid} or {error, Reason}

This consolidates the trap_exit/await_initialize logic so generated spawn
functions stay simple.
""".
-spec safe_spawn(module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn(Module, InitArgs) ->
    safe_spawn_internal(undefined, Module, InitArgs).

-doc """
BT-1987: Spawn a named actor with trap_exit + initialize synchronization.

Same contract as `safe_spawn/2` but registers the actor under `Name`
via `gen_server:start_link({local, Name}, ...)`. Reuses the same
initialize-synchronization machinery so callers see `{ok, Pid}` only
after `handle_continue(initialize, _)` has completed.
""".
-spec safe_spawn_named(atom(), module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn_named(Name, Module, InitArgs) when is_atom(Name) ->
    safe_spawn_internal({local, Name}, Module, InitArgs).

-spec safe_spawn_internal(undefined | {local, atom()}, module(), term()) ->
    {ok, pid()} | {error, term()}.
safe_spawn_internal(ServerName, Module, InitArgs) ->
    OldTrap = erlang:process_flag(trap_exit, true),
    Result =
        case ServerName of
            undefined ->
                gen_server:start_link(Module, InitArgs, []);
            {local, _} = SN ->
                gen_server:start_link(SN, Module, InitArgs, [])
        end,
    case Result of
        {ok, Pid} ->
            case await_initialize(Pid) of
                ok ->
                    erlang:process_flag(trap_exit, OldTrap),
                    {ok, Pid};
                {error, Reason} ->
                    %% Kill process if still alive (timeout case: initialize
                    %% still running but we gave up waiting). Then flush EXIT
                    %% BEFORE restoring trap_exit to avoid a fatal signal.
                    exit(Pid, kill),
                    %% Wait unconditionally — EXIT is guaranteed after kill
                    receive
                        {'EXIT', Pid, _} -> ok
                    end,
                    erlang:process_flag(trap_exit, OldTrap),
                    {error, Reason}
            end;
        {error, Reason} ->
            erlang:process_flag(trap_exit, OldTrap),
            {error, Reason};
        ignore ->
            erlang:process_flag(trap_exit, OldTrap),
            {error, ignore}
    end.

%%% Message Send Helpers
%%%
%%% These functions wrap gen_server:cast/call with lifecycle-aware behavior:
%%% - `isAlive`, `monitor`, and `stop` are handled locally (no gen_server message needed)
%%% - Dead actor detection rejects futures / returns errors immediately
%%%
%%% WARNING: Race condition! is_process_alive/1 is a snapshot check.
%%% The actor could die between the alive check and the gen_server:cast.
%%% For robust lifecycle management, use monitors instead of isAlive polling.

-doc """
Send an asynchronous message to an actor, with lifecycle handling.

Handles lifecycle methods locally without involving the actor process:
- `isAlive` - checks if process is alive, resolves Future with boolean
- `monitor` - creates a monitor reference, resolves Future with ref
- `stop` - gracefully stops the actor process, resolves Future with ok

For all other messages, checks if the actor is alive first:
- If alive, sends via gen_server:cast (normal async path)
- If dead, rejects the Future with an `actor_dead` error
""".
-spec async_send(pid(), atom(), list(), pid()) -> ok.
async_send(ActorPid, isAlive, [], FuturePid) ->
    %% isAlive is handled locally - no message to the actor
    Result = is_process_alive(ActorPid),
    beamtalk_future:resolve(FuturePid, Result),
    ok;
async_send(ActorPid, stop, [], FuturePid) ->
    %% stop is handled locally - gracefully stops the actor process
    %% BT-1629: No send-site telemetry for stop — terminate/2 handles it
    %% to avoid double-counting (request + termination).
    try
        gen_server:stop(ActorPid, normal, 5000),
        beamtalk_future:resolve(FuturePid, ok)
    catch
        exit:noproc ->
            %% Actor already stopped (bare atom exit) - treat as successful stop
            beamtalk_future:resolve(FuturePid, ok);
        exit:{noproc, _} ->
            %% Actor already stopped (tuple exit) - treat as successful stop
            beamtalk_future:resolve(FuturePid, ok);
        exit:Reason ->
            %% Other stop failures (e.g., timeout) - reject Future deterministically
            Error = beamtalk_error:new(
                actor_dead,
                unknown,
                stop,
                iolist_to_binary(io_lib:format("Actor stop failed: ~p", [Reason]))
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(ActorPid, kill, [], FuturePid) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Monitor BEFORE sending the kill signal to close the TOCTOU window:
    %% if we resolved the future immediately after exit/2 (which is async),
    %% a caller doing `actor kill. actor isAlive` could still see true because
    %% the BEAM scheduler had not yet processed the kill signal.
    %% Waiting for the DOWN message guarantees the process is gone before ok
    %% is delivered.  If the process is already dead, the DOWN arrives instantly.
    %% BT-1629: Emit lifecycle telemetry event for async kill request.
    Class = lookup_class(ActorPid),
    maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, kill],
        #{},
        #{pid => ActorPid, class => Class}
    ),
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    case
        receive
            {'DOWN', Ref, process, ActorPid, _Reason} -> ok
        after 5000 ->
            erlang:demonitor(Ref, [flush]),
            timeout
        end
    of
        ok ->
            beamtalk_future:resolve(FuturePid, ok);
        timeout ->
            %% We did not observe a DOWN within 5 s; cannot guarantee the actor
            %% is gone, so reject the future rather than silently claiming success.
            Error = beamtalk_error:new(
                timeout,
                unknown,
                kill,
                <<"Actor kill timed out: did not receive DOWN within 5000ms">>
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(_ActorPid, delegate, [], FuturePid) ->
    %% BT-1208: Non-native Actors do not have a backing Erlang module.
    Error = beamtalk_error:new(signal, unknown, delegate),
    Error1 = Error#beamtalk_error{
        message = <<"delegate called on a non-native Actor">>
    },
    beamtalk_future:reject(FuturePid, Error1),
    ok;
async_send(ActorPid, pid, [], FuturePid) ->
    %% BT-1442: pid returns the raw Erlang PID backing the actor
    beamtalk_future:resolve(FuturePid, ActorPid),
    ok;
async_send(ActorPid, monitor, [], FuturePid) ->
    %% monitor is handled locally - creates an Erlang monitor
    Ref = erlang:monitor(process, ActorPid),
    beamtalk_future:resolve(FuturePid, Ref),
    ok;
async_send(ActorPid, 'onExit:', [Block], FuturePid) ->
    %% BT-1442: onExit: monitors the actor and calls block with reason on exit.
    %% Synchronize with the watcher to ensure the monitor is set up before resolving.
    %% Use a unique ref token to correlate the ready message (prevents stale/parallel confusion).
    Caller = self(),
    Token = make_ref(),
    WatcherPid = spawn(fun() ->
        Ref = erlang:monitor(process, ActorPid),
        Caller ! {onExit_ready, Token},
        receive
            {'DOWN', Ref, process, ActorPid, Reason} ->
                try
                    Block(Reason)
                catch
                    Class:Err:Stack ->
                        ?LOG_WARNING("Error in onExit: callback", #{
                            actor_pid => ActorPid,
                            exit_reason => Reason,
                            error_class => Class,
                            error => Err,
                            stacktrace => Stack
                        })
                end
        end
    end),
    case
        receive
            {onExit_ready, Token} -> ok
        after 5000 ->
            %% Kill the watcher to prevent ghost callbacks and flush stale token.
            exit(WatcherPid, kill),
            receive
                {onExit_ready, Token} -> ok
            after 0 -> ok
            end,
            timeout
        end
    of
        ok ->
            beamtalk_future:resolve(FuturePid, ok);
        timeout ->
            Error = beamtalk_error:new(
                timeout,
                unknown,
                'onExit:',
                <<"onExit: watcher did not start within 5000ms">>
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(ActorPid, Selector, Args, FuturePid) ->
    %% BT-1603: Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures dispatch-to-mailbox time for async sends.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => async},
    %% BT-886: Check liveness before sending, and spawn a watcher to detect
    %% actor death during message processing. The watcher monitors both the
    %% actor and future processes: if the actor dies before the future is
    %% resolved, the watcher rejects the future with a structured error.
    maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
        case is_process_alive(ActorPid) of
            true ->
                PropCtx = get_propagated_ctx(),
                gen_server:cast(ActorPid, {Selector, Args, FuturePid, PropCtx}),
                spawn_future_watcher(ActorPid, FuturePid, Selector),
                {ok, Metadata#{outcome => ok}};
            false ->
                {error, Error} = actor_dead_error(Selector),
                beamtalk_future:reject(FuturePid, Error),
                {ok, Metadata#{outcome => error}}
        end
    end),
    ok.

-doc """
Send a fire-and-forget message to an actor (no future, no return value).

Checks if the actor is alive before sending. If dead, silently returns ok
(fire-and-forget semantics — the caller does not expect a reply).

WARNING: Race condition! is_process_alive/1 is a snapshot check.
The actor could die between the alive check and the gen_server:cast.
""".
-spec cast_send(pid(), atom(), list()) -> ok.
cast_send(ActorPid, Selector, Args) ->
    %% BT-1603: Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures dispatch-to-mailbox time for cast sends.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => cast},
    maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
        case is_process_alive(ActorPid) of
            true ->
                PropCtx = get_propagated_ctx(),
                gen_server:cast(ActorPid, {cast, Selector, Args, PropCtx}),
                {ok, Metadata#{outcome => cast}};
            false ->
                {ok, Metadata#{outcome => cast}}
        end
    end),
    ok.

-doc """
Send a synchronous message to an actor, with lifecycle handling.

Handles lifecycle methods locally without involving the actor process:
- `pid` - returns the raw Erlang PID backing the actor
- `isAlive` - checks if process is alive, returns boolean
- `monitor` - creates a monitor reference, returns ref
- `onExit:` - monitors actor and calls block on exit
- `stop` - gracefully stops the actor process, returns ok

For all other messages, checks if the actor is alive first:
- If alive, sends via gen_server:call and unwraps the result
- If dead, raises `#beamtalk_error{kind = actor_dead}`
- If timeout, raises `#beamtalk_error{kind = timeout}`
""".
-spec sync_send(pid(), atom(), list()) -> term().
sync_send(ActorPid, isAlive, []) ->
    is_process_alive(ActorPid);
sync_send(ActorPid, stop, []) ->
    %% stop is handled locally - gracefully stops the actor process
    %% BT-1629: No send-site telemetry for stop — terminate/2 handles it
    %% to avoid double-counting (request + termination).
    try
        gen_server:stop(ActorPid, normal, 5000)
    catch
        exit:noproc ->
            %% Idempotent: actor already stopped (bare atom exit)
            ok;
        exit:{noproc, _} ->
            %% Idempotent: actor already stopped (tuple exit)
            ok;
        exit:_Other ->
            %% Preserve sync_send/3 contract: translate exits to structured errors
            actor_dead_error(stop)
    end;
sync_send(ActorPid, kill, []) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Same monitor-before-kill pattern as async_send/4: wait for the DOWN
    %% message so that is_process_alive returns false immediately after kill.
    %% BT-1629: Emit lifecycle telemetry event for kill request.
    Class = lookup_class(ActorPid),
    maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, kill],
        #{},
        #{pid => ActorPid, class => Class}
    ),
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    case
        receive
            {'DOWN', Ref, process, ActorPid, _Reason} -> ok
        after 5000 ->
            erlang:demonitor(Ref, [flush]),
            timeout
        end
    of
        ok -> ok;
        timeout -> raise_timeout(kill)
    end;
sync_send(_ActorPid, delegate, []) ->
    %% BT-1208: Non-native Actors do not have a backing Erlang module.
    %% Native Actors will override this at the codegen level.
    Error = beamtalk_error:new(signal, unknown, delegate),
    Error1 = Error#beamtalk_error{
        message = <<"delegate called on a non-native Actor">>
    },
    error(beamtalk_exception_handler:ensure_wrapped(Error1));
sync_send(ActorPid, pid, []) ->
    %% BT-1442: pid returns the raw Erlang PID backing the actor
    ActorPid;
sync_send(ActorPid, monitor, []) ->
    erlang:monitor(process, ActorPid);
sync_send(ActorPid, 'onExit:', [Block]) ->
    %% BT-1442: onExit: monitors the actor and calls block with reason on exit.
    %% Synchronize with the watcher to ensure the monitor is set up before returning.
    %% Use a unique ref token to correlate the ready message.
    Caller = self(),
    Token = make_ref(),
    WatcherPid = spawn(fun() ->
        Ref = erlang:monitor(process, ActorPid),
        Caller ! {onExit_ready, Token},
        receive
            {'DOWN', Ref, process, ActorPid, Reason} ->
                try
                    Block(Reason)
                catch
                    Class:Err:Stack ->
                        ?LOG_WARNING("Error in onExit: callback", #{
                            actor_pid => ActorPid,
                            exit_reason => Reason,
                            error_class => Class,
                            error => Err,
                            stacktrace => Stack
                        })
                end
        end
    end),
    case
        receive
            {onExit_ready, Token} -> ok
        after 5000 ->
            %% Kill the watcher to prevent ghost callbacks and flush stale token.
            exit(WatcherPid, kill),
            receive
                {onExit_ready, Token} -> ok
            after 0 -> ok
            end,
            timeout
        end
    of
        ok -> ok;
        timeout -> raise_timeout('onExit:')
    end;
sync_send(ActorPid, Selector, Args) ->
    %% BT-1325 Layer 1: Fast-path for self-sends.
    %% If ActorPid is our own process AND we have stashed state (meaning we're
    %% inside a handle_call/handle_cast dispatch), dispatch directly to avoid
    %% gen_server:call deadlock. This catches aliased self-sends like:
    %%   other := self. other fieldNames
    case ActorPid =:= self() andalso get('$bt_actor_state') =/= undefined of
        true ->
            self_dispatch(Selector, Args);
        false ->
            sync_send_remote(ActorPid, Selector, Args)
    end.

-doc """
Remote sync-send via gen_server:call (the normal path).
Factored out of sync_send/3 for BT-1325 Layer 1 self-send fast-path.
""".
-spec sync_send_remote(pid(), atom(), list()) -> term().
sync_send_remote(ActorPid, Selector, Args) ->
    %% BT-1603: Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures caller-perspective round-trip time for sync sends.
    %% telemetry:span/3 emits start/stop/exception events automatically.
    %% On exception, span catches it, emits the exception event, and re-raises
    %% via erlang:raise/3 — preserving the original exit class and reason for
    %% the outer try/catch to convert to structured beamtalk_error records.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
    try
        maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
            case is_process_alive(ActorPid) of
                true ->
                    %% BT-918: Generated handle_call/3 wraps replies as {ok, Result} or {error, Error}.
                    %% Unwrap here so callers receive the value directly.
                    %%
                    %% The {error, Error} case has three sub-forms due to the safe_dispatch layer:
                    %%   1. Error = {ErlType, Value, Stacktrace} — BT-1822: with captured stacktrace
                    %%   2. Error = {ErlType, Value} — backward compat: without stacktrace
                    %%   3. Error = other term — e.g. #beamtalk_error{} from dispatch_user_method
                    %% We re-raise all forms as Erlang exceptions so the caller sees them correctly.
                    PropCtx = get_sync_propagated_ctx(),
                    %% BT-1325 Layer 2: Check for transitive cycles before gen_server:call.
                    %% If ActorPid is already in the call stack, this send would deadlock.
                    check_call_stack(ActorPid, Selector),
                    %% BT-1190: TimeoutProxy manages its own timeout on the inner
                    %% (proxy→target) hop. The outer (caller→proxy) hop must use
                    %% infinity so it doesn't time out before the proxy's configured
                    %% timeout expires. For all other actors, use gen_server:call/2
                    %% which defaults to 5000ms.
                    CallResult =
                        case Class of
                            'TimeoutProxy' ->
                                gen_server:call(ActorPid, {Selector, Args, PropCtx}, infinity);
                            _ ->
                                gen_server:call(ActorPid, {Selector, Args, PropCtx})
                        end,
                    case CallResult of
                        {ok, Result} ->
                            {Result, Metadata#{outcome => ok}};
                        {error, {ErlType, ErrorValue, Stacktrace}} ->
                            %% BT-1822: safe_dispatch caught an Erlang exception with stacktrace;
                            %% re-raise with full type/stacktrace context.
                            %% Guard prevents false-match on non-stacktrace 3-tuples.
                            error(
                                beamtalk_exception_handler:ensure_wrapped(
                                    ErlType, ErrorValue, Stacktrace
                                )
                            );
                        {error, {ErlType, ErrorValue}} ->
                            %% Backward compat: safe_dispatch without stacktrace —
                            %% still pass exception class to preserve error kind
                            error(
                                beamtalk_exception_handler:ensure_wrapped(ErlType, ErrorValue, [])
                            );
                        {error, Error} ->
                            error(beamtalk_exception_handler:ensure_wrapped(Error));
                        DirectValue ->
                            %% Backward compat: actors using beamtalk_actor:handle_call/3 directly
                            %% (rather than the generated handle_call) return values unwrapped.
                            {DirectValue, Metadata#{outcome => ok}}
                    end;
                false ->
                    raise_actor_dead(Selector)
            end
        end)
    catch
        exit:{noproc, _} ->
            raise_actor_dead(Selector);
        exit:{normal, _} ->
            raise_actor_dead(Selector);
        exit:{shutdown, _} ->
            raise_actor_dead(Selector);
        exit:{timeout, _} ->
            raise_timeout(Selector);
        exit:{_Reason, _} ->
            %% Catch-all for other exit reasons: {shutdown, Term}, killed,
            %% custom stop reasons, etc. All indicate the actor is unavailable.
            raise_actor_dead(Selector)
    end.

-doc """
Sync-send with explicit timeout (BT-1190).

Same as sync_send/3 but passes the given Timeout to gen_server:call/3.
Timeout is a non-negative integer (milliseconds) or the atom `infinity`.
Used by TimeoutProxy to forward messages with a custom timeout.
""".
-spec sync_send(pid(), atom(), list(), timeout()) -> term().
sync_send(ActorPid, Selector, Args, Timeout) when
    is_integer(Timeout), Timeout >= 0;
    Timeout =:= infinity
->
    %% BT-1325 Layer 1: Fast-path for self-sends (same as sync_send/3).
    case ActorPid =:= self() andalso get('$bt_actor_state') =/= undefined of
        true ->
            self_dispatch(Selector, Args);
        false ->
            Class = lookup_class(ActorPid),
            Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
            try
                maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
                    case is_process_alive(ActorPid) of
                        true ->
                            PropCtx = get_sync_propagated_ctx(),
                            %% BT-1325 Layer 2: Check for transitive cycles.
                            check_call_stack(ActorPid, Selector),
                            case gen_server:call(ActorPid, {Selector, Args, PropCtx}, Timeout) of
                                {ok, Result} ->
                                    {Result, Metadata#{outcome => ok}};
                                {error, {ErlType, ErrorValue, Stacktrace}} ->
                                    %% BT-1822: safe_dispatch with stacktrace
                                    error(
                                        beamtalk_exception_handler:ensure_wrapped(
                                            ErlType, ErrorValue, Stacktrace
                                        )
                                    );
                                {error, {ErlType, ErrorValue}} ->
                                    %% Backward compat: preserve exception class
                                    error(
                                        beamtalk_exception_handler:ensure_wrapped(
                                            ErlType, ErrorValue, []
                                        )
                                    );
                                {error, Error} ->
                                    error(beamtalk_exception_handler:ensure_wrapped(Error));
                                DirectValue ->
                                    {DirectValue, Metadata#{outcome => ok}}
                            end;
                        false ->
                            raise_actor_dead(Selector)
                    end
                end)
            catch
                exit:{noproc, _} ->
                    raise_actor_dead(Selector);
                exit:{normal, _} ->
                    raise_actor_dead(Selector);
                exit:{shutdown, _} ->
                    raise_actor_dead(Selector);
                exit:{timeout, _} ->
                    raise_timeout(Selector);
                exit:{_Reason, _} ->
                    raise_actor_dead(Selector)
            end
    end;
sync_send(_ActorPid, Selector, _Args, _InvalidTimeout) ->
    Error = beamtalk_error:new(
        type_error,
        unknown,
        Selector,
        <<"Timeout must be a non-negative integer (milliseconds) or #infinity">>
    ),
    error(beamtalk_exception_handler:ensure_wrapped(Error)).

-doc """
Direct self-dispatch for re-entrant self-sends (BT-1325 Layer 1).

When sync_send detects ActorPid == self(), we dispatch directly using
the State stashed in the process dictionary by handle_call/handle_cast.
This avoids the gen_server:call deadlock for aliased self-sends like
`other := self. other fieldNames`.

Returns the unwrapped result (same as sync_send would return), or
raises an error exception on dispatch failure.
""".
-spec self_dispatch(atom(), list()) -> term().
self_dispatch(Selector, Args) ->
    State = get('$bt_actor_state'),
    %% BT-1325: Compiled actors have their own safe_dispatch/3 in the generated
    %% module. Use __class_mod__ from State to call the right dispatch function.
    %% If __class_mod__ is missing (runtime-only actors), fall back to
    %% beamtalk_actor:dispatch/4.
    DispatchResult =
        case maps:get('__class_mod__', State, undefined) of
            undefined ->
                Self = make_self(State),
                dispatch(Selector, Args, Self, State);
            ClassMod ->
                ClassMod:safe_dispatch(Selector, Args, State)
        end,
    unwrap_dispatch_result(DispatchResult).

-doc """
Unwrap a dispatch result, update pdict state, and return the value.
Used by self_dispatch/2 to avoid repeating the put/error pattern.
""".
-spec unwrap_dispatch_result(
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}
) -> term().
unwrap_dispatch_result({reply, Result, NewState}) ->
    put('$bt_actor_state', NewState),
    Result;
unwrap_dispatch_result({noreply, NewState}) ->
    put('$bt_actor_state', NewState),
    nil;
unwrap_dispatch_result({error, {ErlType, ErrorValue, Stacktrace}, NewState}) ->
    %% BT-1822: safe_dispatch with stacktrace
    put('$bt_actor_state', NewState),
    error(beamtalk_exception_handler:ensure_wrapped(ErlType, ErrorValue, Stacktrace));
unwrap_dispatch_result({error, {ErlType, ErrorValue}, NewState}) ->
    %% Backward compat: preserve exception class without stacktrace
    put('$bt_actor_state', NewState),
    error(beamtalk_exception_handler:ensure_wrapped(ErlType, ErrorValue, []));
unwrap_dispatch_result({error, Reason, NewState}) ->
    put('$bt_actor_state', NewState),
    error(beamtalk_exception_handler:ensure_wrapped(Reason)).

-doc """
Restore pdict entries after a dispatch completes (BT-1325).
Called in the `after` block of handle_call/handle_cast to clean up
both the stashed actor state and the call stack.
""".
-spec restore_dispatch_pdict(term()) -> ok.
restore_dispatch_pdict(OldState) ->
    case OldState of
        undefined -> erase('$bt_actor_state');
        _ -> put('$bt_actor_state', OldState)
    end,
    erase('$bt_call_stack'),
    ok.

-doc """
Check call stack for transitive cycles (BT-1325 Layer 2).

Before gen_server:call, verify that the target ActorPid is not already
in the call chain. If it is, a sync send would deadlock (A->B->C->A).
Raises a structured deadlock_detected error with the cycle path.
""".
-spec check_call_stack(pid(), atom()) -> ok.
check_call_stack(ActorPid, Selector) ->
    case get('$bt_call_stack') of
        undefined ->
            ok;
        CallStack ->
            case lists:member(ActorPid, CallStack) of
                false ->
                    ok;
                true ->
                    Class = lookup_class(ActorPid),
                    Error = beamtalk_error:new(
                        deadlock_detected,
                        Class,
                        Selector,
                        <<"Sync send would deadlock: target actor is already in the call chain">>
                    ),
                    Error1 = beamtalk_error:with_details(Error, #{
                        call_stack => CallStack,
                        target_pid => ActorPid
                    }),
                    error(beamtalk_exception_handler:ensure_wrapped(Error1))
            end
    end.

-doc """
Raise a structured actor_dead error as an Erlang exception.

Used by sync_send/3 to signal that the actor process is not available.
The caller will see a beamtalk_error{kind = actor_dead} exception.
""".
-spec raise_actor_dead(atom()) -> no_return().
raise_actor_dead(Selector) ->
    Error = beamtalk_error:new(
        actor_dead,
        unknown,
        Selector,
        <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
    ),
    error(beamtalk_exception_handler:ensure_wrapped(Error)).

-doc """
Raise a structured timeout error as an Erlang exception.

Used by sync_send/3 when gen_server:call exceeds the timeout (default 5000ms).
A timeout means the actor didn't respond in time, NOT that it's dead —
it may be slow, overloaded, or deadlocked.
""".
-spec raise_timeout(atom()) -> no_return().
raise_timeout(Selector) ->
    Error = beamtalk_error:new(
        timeout,
        unknown,
        Selector,
        <<
            "Actor did not respond within the timeout period. "
            "The actor may be slow, overloaded, or deadlocked"
        >>
    ),
    error(beamtalk_exception_handler:ensure_wrapped(Error)).

-doc "Construct a structured actor_dead error for the given selector.".
-spec actor_dead_error(atom()) -> {error, #beamtalk_error{}}.
actor_dead_error(Selector) ->
    Error = beamtalk_error:new(
        actor_dead,
        unknown,
        Selector,
        <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
    ),
    {error, Error}.

-doc """
Wrap a function in telemetry:span/3 if telemetry is available, else run directly.
BUnit tests don't load telemetry, so we must gracefully degrade.
BT-1633: When tracing is enabled, generates a span_id and sets trace_id
(root span if none exists) in the process dictionary for causal linking.
""".
-spec maybe_span(list(), map(), fun(() -> {term(), map()})) -> term().
maybe_span(EventPrefix, Metadata, Fun) ->
    %% BT-1633: Generate causal trace IDs when tracing is enabled.
    %% Only incurs atomics cost (~10ns) when trace capture is active.
    case beamtalk_trace_store:is_enabled() of
        true ->
            SpanId = beamtalk_trace_store:next_span_id(),
            %% If no trace_id exists, this is a root span
            case get('$beamtalk_trace_id') of
                undefined ->
                    put('$beamtalk_trace_id', SpanId),
                    put('$beamtalk_span_id', SpanId);
                _ExistingTraceId ->
                    put('$beamtalk_span_id', SpanId)
            end;
        false ->
            ok
    end,
    case erlang:function_exported(telemetry, span, 3) of
        true ->
            telemetry:span(EventPrefix, Metadata, Fun);
        false ->
            {Result, _UpdatedMeta} = Fun(),
            Result
    end.

-doc """
Emit a telemetry:execute/3 event if telemetry is available (BT-1629).
Used for lifecycle events (start, stop, kill) which are instantaneous —
no duration/span needed. Gracefully degrades when telemetry is not loaded.
""".
-spec maybe_execute_telemetry(list(), map(), map()) -> ok.
maybe_execute_telemetry(EventName, Measurements, Metadata) ->
    case erlang:function_exported(telemetry, execute, 3) of
        true ->
            telemetry:execute(EventName, Measurements, Metadata);
        false ->
            ok
    end.

-doc """
Resolve actor class name via beamtalk_object_instances reverse lookup.
Returns the class atom if found, or 'unknown' for non-Beamtalk PIDs.
Uses ets:match on the bag table — single ETS read (~50-100ns).
""".
-spec lookup_class(pid()) -> atom().
lookup_class(Pid) ->
    try ets:match(beamtalk_instance_registry, {'$1', Pid}) of
        [[Class] | _] -> Class;
        [] -> unknown
    catch
        error:badarg -> unknown
    end.

-doc """
Set application-level trace context key-value pairs (BT-1625).

Stores the provided map in the process dictionary under '$beamtalk_trace_ctx'.
Also merges the keys into OTP logger process metadata so that ?LOG_ERROR,
?LOG_INFO etc. automatically include fields like workflowId without extra work.

Example: set_trace_context(#{workflowId => <<"wf-123">>, activityId => <<"a-1">>})

Cost: ~20-30ns (one put/2 + one logger:update_process_metadata/1).
""".
-spec set_trace_context(map()) -> ok.
set_trace_context(Ctx) when is_map(Ctx) ->
    OldCtx = get_trace_context(),
    Merged = maps:merge(OldCtx, Ctx),
    put('$beamtalk_trace_ctx', Merged),
    logger:update_process_metadata(Merged),
    ok.

-doc """
Get the current application-level trace context (BT-1625).

Returns the map stored by set_trace_context/1, or #{} if none set.
Cost: ~10ns (one get/1).
""".
-spec get_trace_context() -> map().
get_trace_context() ->
    case get('$beamtalk_trace_ctx') of
        undefined -> #{};
        Ctx when is_map(Ctx) -> Ctx;
        _Other -> #{}
    end.

-doc """
Clear the application-level trace context and remove its keys from logger metadata.

Used when restoring an empty propagated trace context to prevent stale metadata
from leaking across unrelated messages in the same actor process.
""".
-spec clear_trace_context() -> ok.
clear_trace_context() ->
    OldCtx = get_trace_context(),
    erase('$beamtalk_trace_ctx'),
    %% Remove the trace context keys from logger metadata
    case logger:get_process_metadata() of
        undefined ->
            ok;
        LogMeta ->
            Cleaned = maps:without(maps:keys(OldCtx), LogMeta),
            logger:set_process_metadata(Cleaned)
    end,
    ok.

-doc """
Get the current causal trace context from the process dictionary (BT-1633).

Returns a map with trace_id, span_id, and parent_span_id keys when
causal tracing is active. Returns #{} when no causal context exists.
Called by handle_dispatch_stop to merge causal IDs into trace event metadata.
""".
-spec get_causal_ctx() -> map().
get_causal_ctx() ->
    case get('$beamtalk_trace_id') of
        undefined ->
            #{};
        TraceId ->
            Base = #{trace_id => TraceId},
            WithSpan =
                case get('$beamtalk_span_id') of
                    undefined -> Base;
                    SpanId -> Base#{span_id => SpanId}
                end,
            case get('$beamtalk_parent_span_id') of
                undefined -> WithSpan;
                ParentSpanId -> WithSpan#{parent_span_id => ParentSpanId}
            end
    end.

-doc """
Build a propagated context map for cross-actor message sends (ADR 0069 Phase 2b).

Returns an extensible map containing context that should flow across actor
boundaries. Currently captures:
- OTel trace context (when otel_ctx module is loaded)
- Application-level trace context (BT-1625, set via set_trace_context/1)
Future keys (request_id, deadline, causality) will be added here.

Cost: ~20ns when OTel not loaded (function_exported check + get/1 + map construction),
~50ns when OTel loaded (+ otel_ctx:get_current/0 call).
""".
-spec get_propagated_ctx() -> map().
get_propagated_ctx() ->
    Base = #{otel => get_otel_ctx(), trace_ctx => get_trace_context()},
    %% BT-1633: Include causal trace IDs for cross-actor linking.
    %% Only present when tracing is enabled and maybe_span has run.
    WithCausal =
        case get('$beamtalk_trace_id') of
            undefined ->
                Base;
            TraceId ->
                SpanId = get('$beamtalk_span_id'),
                Base#{causal => #{trace_id => TraceId, span_id => SpanId}}
        end,
    WithCausal.

-doc """
Like get_propagated_ctx/0 but includes call_stack for cycle detection.
Only used by sync_send/3,4 — async/cast sends must NOT carry call_stack
because the sender is not blocked, so B calling A back is not a deadlock.
""".
-spec get_sync_propagated_ctx() -> map().
get_sync_propagated_ctx() ->
    ExistingStack =
        case get('$bt_call_stack') of
            undefined -> [];
            CS -> CS
        end,
    (get_propagated_ctx())#{call_stack => [self() | ExistingStack]}.

-doc """
Get current OTel trace context if otel_ctx module is available.
Returns 'undefined' when OpenTelemetry is not loaded — no compile-time dependency.
""".
-spec get_otel_ctx() -> term().
get_otel_ctx() ->
    case erlang:function_exported(otel_ctx, get_current, 0) of
        true -> erlang:apply(otel_ctx, get_current, []);
        false -> undefined
    end.

-doc """
Restore propagated context on the receiving actor side (ADR 0069 Phase 2b).

Called by generated handle_call/handle_cast to restore context from the
propagated context map. Restores two kinds of context:
- OTel trace context: attaches to otel_ctx so spans are linked to caller's trace
- Application-level trace context (BT-1625): restores key-value pairs set via
  set_trace_context/1 and updates OTP logger process metadata
No-op when context is not present or dependencies are not loaded.
""".
-spec restore_propagated_ctx(map()) -> ok.
restore_propagated_ctx(PropCtx) when is_map(PropCtx) ->
    %% Restore OTel context if present
    case maps:get(otel, PropCtx, undefined) of
        undefined ->
            ok;
        OtelCtx ->
            case erlang:function_exported(otel_ctx, attach, 1) of
                true ->
                    erlang:apply(otel_ctx, attach, [OtelCtx]),
                    ok;
                false ->
                    ok
            end
    end,
    %% Restore application-level trace context if present (BT-1625).
    %% When trace_ctx is an empty map, we must still clear any previously
    %% restored context to prevent stale metadata leaking across messages.
    case maps:get(trace_ctx, PropCtx, undefined) of
        undefined ->
            ok;
        TraceCtx when is_map(TraceCtx), map_size(TraceCtx) > 0 ->
            clear_trace_context(),
            set_trace_context(TraceCtx);
        TraceCtx when is_map(TraceCtx) ->
            %% Empty map: clear any previously restored trace context
            clear_trace_context();
        _ ->
            ok
    end,
    %% BT-1633: Restore causal trace context for parent-child linking.
    %% The incoming span_id becomes our parent_span_id. We inherit the trace_id.
    case maps:get(causal, PropCtx, undefined) of
        #{trace_id := InTraceId, span_id := InSpanId} ->
            put('$beamtalk_trace_id', InTraceId),
            put('$beamtalk_parent_span_id', InSpanId);
        _ ->
            %% No causal context in this message — clear stale IDs from
            %% a previous traced message so they don't leak into untraced
            %% dispatches or get_propagated_ctx/0.
            erase('$beamtalk_trace_id'),
            erase('$beamtalk_span_id'),
            erase('$beamtalk_parent_span_id')
    end,
    %% BT-1325 Layer 2: Restore call stack for transitive cycle detection.
    %% The incoming call_stack lists all actors already in the sync call chain.
    %% sync_send checks this before gen_server:call to detect A->B->C->A cycles.
    case maps:get(call_stack, PropCtx, undefined) of
        undefined ->
            erase('$bt_call_stack');
        CallStack when is_list(CallStack) ->
            put('$bt_call_stack', CallStack);
        _ ->
            %% Ignore invalid values from untrusted/forward-compatible payloads
            erase('$bt_call_stack')
    end,
    ok;
restore_propagated_ctx(_) ->
    ok.

-doc """
Spawn a lightweight watcher that monitors both the actor and future.
BT-886: Closes the TOCTOU race in async_send — if the actor dies during
message processing (after the cast but before the future is resolved),
the watcher rejects the future with a structured actor_dead error.
The watcher also monitors the future process so it can clean up promptly
when the future completes normally. Times out after 30s as a safety net.
""".
-spec spawn_future_watcher(pid(), pid(), atom()) -> pid().
spawn_future_watcher(ActorPid, FuturePid, Selector) ->
    spawn(fun() ->
        ActorRef = erlang:monitor(process, ActorPid),
        FutureRef = erlang:monitor(process, FuturePid),
        receive
            {'DOWN', ActorRef, process, ActorPid, _Reason} ->
                %% Actor died — reject future (no-op if already resolved)
                {error, Error} = actor_dead_error(Selector),
                beamtalk_future:reject(FuturePid, Error),
                erlang:demonitor(FutureRef, [flush]);
            {'DOWN', FutureRef, process, FuturePid, _Reason} ->
                %% Future completed and its process ended — clean up
                erlang:demonitor(ActorRef, [flush])
        after 30000 ->
            %% Safety cleanup: stop watching after 30s
            erlang:demonitor(ActorRef, [flush]),
            erlang:demonitor(FutureRef, [flush])
        end
    end).

%%% gen_server callbacks

-doc """
Initialize actor with state map.
State must be a map containing '$beamtalk_class' and '__methods__' keys.
""".
-spec init(map()) -> {ok, map()} | {stop, term()}.
init(State) when is_map(State) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    %% Validate required keys
    ClassKey = beamtalk_tagged_map:class_key(),
    case maps:find(ClassKey, State) of
        {ok, Class} when is_atom(Class) ->
            case maps:is_key('__methods__', State) of
                true ->
                    %% BT-1987 / ADR 0079: process-dict marker identifies
                    %% every Beamtalk actor process so tooling and
                    %% `all_registered/0` can filter them out of the
                    %% flat OTP registry without needing a separate table.
                    erlang:put('$beamtalk_actor', Class),
                    StateKeys = [
                        K
                     || K <- maps:keys(State),
                        K =/= '__methods__',
                        K =/= ClassKey,
                        K =/= '__class_mod__'
                    ],
                    ?LOG_INFO("Actor started", #{
                        class => Class,
                        pid => self(),
                        state_keys => StateKeys,
                        domain => [beamtalk, runtime]
                    }),
                    %% BT-1629: Emit lifecycle telemetry event for actor start.
                    %% Recorded in the shared trace ring buffer with mode => lifecycle.
                    maybe_execute_telemetry(
                        [beamtalk, actor, lifecycle, start],
                        #{},
                        #{pid => self(), class => Class}
                    ),
                    {ok, State};
                false ->
                    {stop, {missing_key, '__methods__'}}
            end;
        {ok, _NonAtom} ->
            {stop, {invalid_value, ClassKey}};
        error ->
            {stop, {missing_key, ClassKey}}
    end;
init(_NonMapState) ->
    {stop, {invalid_state, not_a_map}}.

-doc """
Handle asynchronous messages (cast).
Accepts two wire formats:
  {cast, Selector, Args}       - Fire-and-forget (no future, result discarded)
  {Selector, Args, FuturePid}  - Async with future (backward-compatible)
Errors in fire-and-forget are logged but do not crash the actor.
Errors in async-with-future are communicated via future rejection.
""".
-spec handle_cast(term(), map()) -> {noreply, map()}.
%% BT-1604: Fire-and-forget cast with propagated context (ADR 0069 Phase 2b)
handle_cast({cast, Selector, Args, PropCtx}, State) when
    is_atom(Selector), is_list(Args), is_map(PropCtx)
->
    restore_propagated_ctx(PropCtx),
    handle_cast({cast, Selector, Args}, State);
handle_cast({cast, Selector, Args}, State) when is_atom(Selector), is_list(Args) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (cast fire-and-forget)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        mode => cast,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    %% BT-1325: Stash State for re-entrant self-sends (Layer 1)
    OldState = get('$bt_actor_state'),
    put('$bt_actor_state', State),
    try
        case dispatch(Selector, Args, Self, State) of
            {reply, _Result, NewState} ->
                %% Fire-and-forget: result is discarded
                log_dispatch_complete(State, NewState, Selector, cast, T0),
                {noreply, NewState};
            {noreply, NewState} ->
                log_dispatch_complete(State, NewState, Selector, cast, T0),
                {noreply, NewState};
            {error, Reason, NewState} ->
                %% Log error but don't crash — caller expects no reply
                T1 = erlang:monotonic_time(microsecond),
                ?LOG_WARNING("Fire-and-forget dispatch error", #{
                    selector => Selector,
                    reason => Reason,
                    duration_us => T1 - T0,
                    domain => [beamtalk, runtime]
                }),
                {noreply, NewState}
        end
    after
        restore_dispatch_pdict(OldState)
    end;
%% BT-1604: Async send with propagated context (ADR 0069 Phase 2b)
handle_cast({Selector, Args, FuturePid, PropCtx}, State) when is_map(PropCtx) ->
    restore_propagated_ctx(PropCtx),
    handle_cast({Selector, Args, FuturePid}, State);
handle_cast({Selector, Args, FuturePid}, State) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (async)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        caller_pid => FuturePid,
        mode => async,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    %% BT-1325: Stash State for re-entrant self-sends (Layer 1)
    OldState = get('$bt_actor_state'),
    put('$bt_actor_state', State),
    try
        case dispatch(Selector, Args, Self, State) of
            {reply, Result, NewState} ->
                %% Resolve the future with the result
                log_dispatch_complete(State, NewState, Selector, async, T0),
                beamtalk_future:resolve(FuturePid, Result),
                {noreply, NewState};
            {noreply, NewState} ->
                %% Method didn't return a value, resolve with nil
                log_dispatch_complete(State, NewState, Selector, async, T0),
                beamtalk_future:resolve(FuturePid, nil),
                {noreply, NewState};
            {error, Reason, NewState} ->
                %% Method failed, reject the future
                log_dispatch_complete(State, NewState, Selector, async, T0),
                beamtalk_future:reject(FuturePid, Reason),
                {noreply, NewState}
        end
    after
        restore_dispatch_pdict(OldState)
    end;
handle_cast(Msg, State) ->
    %% Unknown cast message format - log and ignore
    ?LOG_WARNING("Unknown cast message", #{message => Msg, domain => [beamtalk, runtime]}),
    {noreply, State}.

-doc """
Handle synchronous messages (call).
Message format: {Selector, Args} or {Selector, Args, PropCtx} (ADR 0069 Phase 2b)
Dispatches to method and returns result immediately.
""".
-spec handle_call(term(), term(), map()) -> {reply, term(), map()}.
%% BT-1604: Sync call with propagated context (ADR 0069 Phase 2b)
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);
handle_call({Selector, Args}, From, State) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (sync)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        caller_pid => element(1, From),
        mode => sync,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    %% BT-1325: Stash State in pdict so re-entrant self-sends (Layer 1)
    %% can dispatch directly without going through gen_server:call.
    OldState = get('$bt_actor_state'),
    put('$bt_actor_state', State),
    try
        case dispatch(Selector, Args, Self, State) of
            {reply, Result, NewState} ->
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, Result, NewState};
            {noreply, NewState} ->
                %% Method didn't return a value, return nil
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, nil, NewState};
            {error, Reason, NewState} ->
                %% Method failed, return error tuple
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, {error, Reason}, NewState}
        end
    after
        restore_dispatch_pdict(OldState)
    end;
handle_call(Msg, _From, State) ->
    %% Unknown call message format
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_details(Error0, #{raw_message => Msg}),
    Error = beamtalk_error:with_hint(Error1, <<"Expected {Selector, Args} tuple">>),
    {reply, {error, Error}, State}.

-doc """
Handle out-of-band messages (info).
By default, unknown messages are ignored.
Generated actors can override this to handle custom messages.
""".
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Msg, State) ->
    %% Ignore unknown info messages by default
    {noreply, State}.

-doc """
Handle hot code reload.
By default, preserves existing state unchanged.
Generated actors can override this to migrate state schemas.
""".
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

-doc """
Clean up when actor is stopping.
By default, does nothing.
Generated actors can override this for cleanup.
""".
-spec terminate(term(), map()) -> ok.
terminate(Reason, State) ->
    Class = beamtalk_tagged_map:class_of(State, unknown),
    ?LOG_INFO("Actor stopped", #{
        class => Class,
        pid => self(),
        reason => Reason,
        domain => [beamtalk, runtime]
    }),
    %% BT-1629: Emit lifecycle telemetry event for actor stop.
    %% Reason distinguishes normal shutdown from crash.
    maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, stop],
        #{},
        #{pid => self(), class => Class, reason => Reason}
    ),
    ok.

%%% Helper Functions

-doc "Log dispatch completion with timing and state mutation info.".
-spec log_dispatch_complete(map(), map(), atom(), atom(), integer()) -> ok.
log_dispatch_complete(OldState, NewState, Selector, Mode, T0) ->
    T1 = erlang:monotonic_time(microsecond),
    Duration = T1 - T0,
    ChangedKeys = changed_state_keys(OldState, NewState),
    case ChangedKeys of
        [] ->
            ?LOG_DEBUG("Actor dispatch complete", #{
                selector => Selector,
                mode => Mode,
                duration_us => Duration,
                domain => [beamtalk, runtime]
            });
        _ ->
            ?LOG_DEBUG("Actor dispatch complete", #{
                selector => Selector,
                mode => Mode,
                duration_us => Duration,
                changed_keys => ChangedKeys,
                domain => [beamtalk, runtime]
            })
    end,
    ok.

-doc """
Compute which user-visible state keys changed between two state maps.
Excludes internal keys (__methods__, $beamtalk_class, __class_mod__).
""".
-spec changed_state_keys(map(), map()) -> [atom()].
changed_state_keys(OldState, NewState) ->
    InternalKeys = ['__methods__', beamtalk_tagged_map:class_key(), '__class_mod__'],
    AllKeys = lists:usort(maps:keys(OldState) ++ maps:keys(NewState)),
    UserKeys = AllKeys -- InternalKeys,
    [
        K
     || K <- UserKeys, maps:get(K, OldState, '__absent__') =/= maps:get(K, NewState, '__absent__')
    ].

-doc """
Construct a Self reference (#beamtalk_object{}) from the actor's state.
Self contains class metadata and the actor's pid, enabling reflection
and self-sends within methods.
""".
-spec make_self(map()) -> #beamtalk_object{}.
make_self(State) ->
    #beamtalk_object{
        class = beamtalk_tagged_map:class_of(State),
        class_mod = maps:get('__class_mod__', State, undefined),
        pid = self()
    }.

%%% Message Dispatch

-doc """
Dispatch a message to the appropriate method (dispatch/4 version).
This is the new signature that passes Self as a separate parameter.
Looks up the selector in the __methods__ map and calls the method function.
If not found, attempts to call doesNotUnderstand handler.
Returns one of:
  {reply, Result, NewState} - Method returned a value
  {noreply, NewState} - Method didn't return a value
  {error, Reason, State} - Method or dispatch failed
""".
-spec dispatch(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch(Selector, Args, Self, State) ->
    %% BT-427: Actor-specific methods that can't be in Object:
    %% - isAlive: checks process liveness
    %% - perform:/perform:withArguments:: re-dispatches through actor's own dispatch
    %% - respondsTo:: must check __methods__ map AND hierarchy
    %% All other Object methods (describe, inspect, fieldNames, hash, etc.)
    %% are discovered via hierarchy walk to Object.
    case Selector of
        isAlive when Args =:= [] ->
            %% Actor is alive if it's processing this message
            {reply, true, State};
        delegate when Args =:= [] ->
            %% BT-1208: Non-native Actors do not have a backing Erlang module.
            %% This path is reached via perform:/perform:withArguments: which
            %% bypass the send site and re-dispatch through dispatch/4.
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error = beamtalk_error:new(signal, ClassName, delegate),
            Error1 = Error#beamtalk_error{
                message = <<"delegate called on a non-native Actor">>
            },
            {error, Error1, State};
        'respondsTo:' when length(Args) =:= 1 ->
            %% Check user-defined methods first, then actor built-ins, then hierarchy
            [CheckSelector] = Args,
            Methods = maps:get('__methods__', State),
            case maps:is_key(CheckSelector, Methods) of
                true ->
                    {reply, true, State};
                false when CheckSelector =:= isAlive ->
                    %% isAlive is handled by actor dispatch, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= stop ->
                    %% stop is handled at send site, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= pid ->
                    %% BT-1442: pid is handled at send site, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= monitor ->
                    %% monitor is handled by actor lifecycle machinery, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= 'onExit:' ->
                    %% BT-1442: onExit: is handled at send site, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= delegate ->
                    %% delegate is handled by actor dispatch, not in __methods__
                    {reply, true, State};
                false ->
                    %% Check inherited methods via hierarchy walk
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Result =
                        try beamtalk_dispatch:responds_to(CheckSelector, ClassName) of
                            true ->
                                true;
                            false ->
                                %% Class may not be registered — check Object directly
                                beamtalk_object_ops:has_method(CheckSelector)
                        catch
                            _:_ -> beamtalk_object_ops:has_method(CheckSelector)
                        end,
                    {reply, Result, State}
            end;
        'perform:' when length(Args) =:= 1 ->
            %% Dynamic message send: obj perform: #increment => obj increment
            [TargetSelector] = Args,
            case is_atom(TargetSelector) of
                true ->
                    dispatch(TargetSelector, [], Self, State);
                false ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error = beamtalk_error:new(type_error, ClassName, 'perform:'),
                    {error, Error, State}
            end;
        'perform:withArguments:' when length(Args) =:= 2 ->
            %% Dynamic message send: obj perform: #'at:put:' withArguments: #(1, 'x')
            [TargetSelector, ArgList] = Args,
            case is_atom(TargetSelector) andalso is_list(ArgList) of
                true ->
                    dispatch(TargetSelector, ArgList, Self, State);
                false ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error = beamtalk_error:new(type_error, ClassName, 'perform:withArguments:'),
                    {error, Error, State}
            end;
        'perform:withArguments:timeout:' when length(Args) =:= 3 ->
            %% BT-1190: Dynamic message send with explicit timeout.
            %% For self-sends (inside an actor's handle_call), the timeout is
            %% irrelevant — just dispatch locally like perform:withArguments:.
            %% Cross-actor sends are intercepted in beamtalk_message_dispatch:send/3
            %% which rewrites to send/4 with the custom timeout.
            [TargetSelector, ArgList, Timeout] = Args,
            case
                is_atom(TargetSelector) andalso is_list(ArgList) andalso
                    (is_integer(Timeout) andalso Timeout >= 0 orelse Timeout =:= infinity)
            of
                true ->
                    dispatch(TargetSelector, ArgList, Self, State);
                false ->
                    ClassName = beamtalk_tagged_map:class_of(State, unknown),
                    Error = beamtalk_error:new(
                        type_error,
                        ClassName,
                        'perform:withArguments:timeout:',
                        <<"Expected atom selector, list of arguments, and non-negative integer or #infinity timeout">>
                    ),
                    {error, Error, State}
            end;
        _ ->
            %% Check user-defined methods, then delegate to hierarchy walk
            dispatch_user_method(Selector, Args, Self, State)
    end.

-doc "Dispatch to user-defined methods after built-in checks.".
-spec dispatch_user_method(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch_user_method(Selector, Args, Self, State) ->
    case Selector of
        initialize ->
            ?LOG_DEBUG("Initialize hook invoked", #{
                class => beamtalk_tagged_map:class_of(State, unknown),
                pid => self(),
                domain => [beamtalk, runtime]
            });
        _ ->
            ok
    end,
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} when is_function(Fun, 4) ->
            %% New-style method: Fun(Selector, Args, Self, State)
            try
                Fun(Selector, Args, Self, State)
            catch
                error:#beamtalk_error{} = BtError:_Stacktrace ->
                    %% Preserve structured beamtalk errors from method implementations
                    {error, BtError, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, State, Class, Reason, Stacktrace)
            end;
        {ok, Fun} when is_function(Fun, 2) ->
            %% Old-style method: Fun(Args, State) - for backward compatibility
            try
                Fun(Args, State)
            catch
                error:#beamtalk_error{} = BtError:_Stacktrace ->
                    %% Preserve structured beamtalk errors from method implementations
                    {error, BtError, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, State, Class, Reason, Stacktrace)
            end;
        {ok, _NotAFunction} ->
            %% Method value is not a function
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error = beamtalk_error:new(type_error, ClassName, Selector),
            {error, Error, State};
        error ->
            %% Method not found - try doesNotUnderstand
            handle_dnu(Selector, Args, Self, State)
    end.

-doc """
Handle messages for unknown selectors.
Attempts to call the doesNotUnderstand:args: handler if defined.
Otherwise, returns an error.
""".
-spec handle_dnu(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
handle_dnu(Selector, Args, Self, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find('doesNotUnderstand:args:', Methods) of
        {ok, DnuFun} when is_function(DnuFun, 3) ->
            call_dnu_handler(DnuFun, [Selector, Args], Self, State, 3);
        {ok, DnuFun} when is_function(DnuFun, 2) ->
            call_dnu_handler(DnuFun, [Selector, Args], Self, State, 2);
        _ ->
            dispatch_via_hierarchy(Selector, Args, Self, State)
    end.

-doc "Call a doesNotUnderstand handler with error wrapping.".
-spec call_dnu_handler(function(), list(), #beamtalk_object{}, map(), 2 | 3) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
call_dnu_handler(DnuFun, DnuArgs, Self, State, 3) ->
    try
        DnuFun(DnuArgs, Self, State)
    catch
        error:#beamtalk_error{} = BtError:_ ->
            {error, BtError, State};
        Class:Reason:Stacktrace ->
            wrap_dnu_handler_error(hd(DnuArgs), State, Class, Reason, Stacktrace)
    end;
call_dnu_handler(DnuFun, DnuArgs, _Self, State, 2) ->
    try
        DnuFun(DnuArgs, State)
    catch
        error:#beamtalk_error{} = BtError:_ ->
            {error, BtError, State};
        Class:Reason:Stacktrace ->
            wrap_dnu_handler_error(hd(DnuArgs), State, Class, Reason, Stacktrace)
    end.

-doc "Dispatch via class hierarchy, falling back to Object on any failure.".
-spec dispatch_via_hierarchy(atom(), list(), #beamtalk_object{}, map()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
dispatch_via_hierarchy(Selector, Args, Self, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    try beamtalk_dispatch:lookup(Selector, Args, Self, State, ClassName) of
        {reply, Result, NewState} ->
            {reply, Result, NewState};
        {error, #beamtalk_error{kind = does_not_understand}} ->
            object_fallback(Selector, Args, Self, State, ClassName);
        {error, #beamtalk_error{kind = class_not_found}} ->
            object_fallback(Selector, Args, Self, State, ClassName);
        {error, Error} ->
            {error, Error, State}
    catch
        exit:{noproc, _} ->
            %% Class registry not available (e.g., in unit tests without bootstrap)
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{normal, _} ->
            %% Class process terminated normally
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{timeout, _} ->
            ?LOG_WARNING("Class dispatch lookup timed out", #{
                selector => Selector,
                class => ClassName,
                domain => [beamtalk, runtime]
            }),
            object_fallback(Selector, Args, Self, State, ClassName);
        exit:{Reason, _}:Stack ->
            ?LOG_WARNING("Class dispatch lookup failed", #{
                selector => Selector,
                class => ClassName,
                reason => Reason,
                stacktrace => Stack,
                domain => [beamtalk, runtime]
            }),
            object_fallback(Selector, Args, Self, State, ClassName)
    end.

-doc "Create a does_not_understand error result.".
-spec make_dnu_error(atom(), atom(), map()) -> {error, term(), map()}.
make_dnu_error(Selector, ClassName, State) ->
    ?LOG_WARNING("doesNotUnderstand", #{
        class => ClassName,
        selector => Selector,
        pid => self(),
        domain => [beamtalk, runtime]
    }),
    Error = beamtalk_error:new(
        does_not_understand,
        ClassName,
        Selector,
        <<"Check spelling or use 'respondsTo:' to verify method exists">>
    ),
    {error, Error, State}.

-doc "Wrap method dispatch exceptions as type_error with source exception details.".
-spec wrap_method_error(atom(), map(), term(), term(), list()) -> {error, term(), map()}.
wrap_method_error(Selector, State, Class, Reason, Stacktrace) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Message = format_method_error_message(ClassName, Selector, Class, Reason, Stacktrace),
    ?LOG_ERROR("Error in method", #{
        selector => Selector,
        class => Class,
        reason => Reason,
        stacktrace => Stacktrace,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(runtime_error, ClassName, Selector),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error = beamtalk_error:with_details(Error1, #{
        original_class => Class,
        original_reason => Reason,
        erlang_stacktrace => Stacktrace
    }),
    {error, Error, State}.

-doc "Format a human-readable error message from a method dispatch failure.".
-spec format_method_error_message(atom(), atom(), term(), term(), list()) -> binary().
format_method_error_message(ClassName, Selector, error, function_clause, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "No matching clause in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarg, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Bad argument in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, undef, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Undefined function in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarith, [{M, F, A, Loc} | _]) ->
    iolist_to_binary(
        io_lib:format(
            "Arithmetic error in ~s>>~s (called ~s:~s/~s~s)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, Class, Reason, _Stacktrace) ->
    iolist_to_binary(
        io_lib:format(
            "~p:~p in ~s>>~s",
            [Class, Reason, ClassName, Selector]
        )
    ).

format_arity(A) when is_list(A) -> integer_to_list(length(A));
format_arity(A) when is_integer(A) -> integer_to_list(A).

format_location(Loc) when is_list(Loc) ->
    case proplists:get_value(line, Loc) of
        undefined ->
            "";
        Line ->
            case proplists:get_value(file, Loc) of
                undefined -> io_lib:format(" at line ~B", [Line]);
                File -> io_lib:format(" at ~s:~B", [File, Line])
            end
    end.

-doc "Wrap doesNotUnderstand handler exceptions consistently for both arities.".
-spec wrap_dnu_handler_error(atom(), map(), term(), term(), list()) -> {error, term(), map()}.
wrap_dnu_handler_error(Selector, State, Class, Reason, Stacktrace) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Message = format_method_error_message(ClassName, Selector, Class, Reason, Stacktrace),
    ?LOG_ERROR("Error in doesNotUnderstand handler", #{
        selector => Selector,
        class => Class,
        reason => Reason,
        stacktrace => Stacktrace,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(runtime_error, ClassName, 'doesNotUnderstand:args:'),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error = beamtalk_error:with_details(Error1, #{
        original_class => Class,
        original_reason => Reason,
        erlang_stacktrace => Stacktrace
    }),
    {error, Error, State}.

-doc """
Fall back to Object dispatch for inherited base methods.
Used when hierarchy walk fails (class not registered) or class registry unavailable.
""".
-spec object_fallback(atom(), list(), #beamtalk_object{}, map(), atom()) ->
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}.
object_fallback(Selector, Args, Self, State, ClassName) ->
    case beamtalk_object_ops:dispatch(Selector, Args, Self, State) of
        {error, _, _} ->
            make_dnu_error(Selector, ClassName, State);
        Result ->
            Result
    end.

%%% =====================================================================
%%% Named Actor Registration (ADR 0079, BT-1987)
%%% =====================================================================
%%%
%%% These intrinsics wire Beamtalk actors into OTP's local process
%%% registry (`erlang:register/2`). The `'$beamtalk_actor'` process
%%% dictionary marker set in `init/1` lets us distinguish Beamtalk
%%% actors from raw OTP processes when filtering `erlang:registered/0`.
%%%
%%% Errors use `#beamtalk_error{}` with kinds `name_registered`,
%%% `type_error`, and `reserved_name` — these are translated to
%%% `Result error: ...` at the stdlib boundary per ADR 0060/0076.

-doc """
Check whether a pid is a Beamtalk actor process.

Returns `true` if the process has the `'$beamtalk_actor'` marker
set in its process dictionary (set by `init/1` for every Beamtalk
actor). Returns `false` for raw OTP processes (including kernel
processes, gen_servers not built on beamtalk_actor, etc.) and for
dead processes.
""".
-spec is_beamtalk_actor(term()) -> boolean().
is_beamtalk_actor(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            lists:keymember('$beamtalk_actor', 1, Dict);
        undefined ->
            %% Process is dead
            false
    end;
is_beamtalk_actor(_) ->
    false.

-doc """
Register a pid under a name in the local atom registry.

Returns `{ok, Name}` on success or `{error, #beamtalk_error{}}` on
failure with `kind` one of:
  - `type_error` — `Name` is not an atom or `Pid` is not a pid, or
    the `Pid` is not registerable (dead, remote, or already has a
    registered name).
  - `reserved_name` — `Name` is in the reserved-name blocklist
  - `name_registered` — another process is already registered
    under `Name`.

Note: `erlang:register/2` raises `badarg` for multiple reasons —
duplicate name, dead pid, remote pid, pid already registered
under another name. We disambiguate by checking
`erlang:whereis(Name)` after `badarg`: if taken, it's a duplicate
name; otherwise the Pid itself is unregisterable.
""".
-spec register_name(term(), term()) -> {ok, atom()} | {error, #beamtalk_error{}}.
register_name(Name, Pid) when is_atom(Name), is_pid(Pid) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', registerAs),
                    iolist_to_binary(
                        io_lib:format(
                            "Cannot register actor under reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try erlang:register(Name, Pid) of
                true -> {ok, Name}
            catch
                error:badarg ->
                    case erlang:whereis(Name) =/= undefined of
                        true ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(
                                        name_registered, 'Actor', registerAs
                                    ),
                                    iolist_to_binary(
                                        io_lib:format(
                                            "Name '~ts' is already registered", [Name]
                                        )
                                    )
                                )};
                        false ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(
                                        type_error, 'Actor', registerAs
                                    ),
                                    iolist_to_binary(
                                        io_lib:format(
                                            "Pid ~tp is not registerable "
                                            "(dead, remote, or already "
                                            "registered under another name)",
                                            [Pid]
                                        )
                                    )
                                )}
                    end
            end
    end;
register_name(Name, Pid) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', registerAs),
            iolist_to_binary(
                io_lib:format(
                    "register_name/2 expects (atom, pid), got (~tp, ~tp)", [Name, Pid]
                )
            )
        )}.

-doc """
Unregister a name previously registered via `register_name/2`.

Returns `ok` on success or `{error, #beamtalk_error{}}` if `Name`
is not currently registered. Idempotent-friendly callers should
check `whereis_name/1` first if they want "unregister-if-present"
semantics.
""".
-spec unregister_name(term()) -> ok | {error, #beamtalk_error{}}.
unregister_name(Name) when is_atom(Name) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', unregister),
                    iolist_to_binary(
                        io_lib:format(
                            "Cannot unregister reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try erlang:unregister(Name) of
                true -> ok
            catch
                error:badarg ->
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(
                                name_registered, 'Actor', unregister
                            ),
                            iolist_to_binary(
                                io_lib:format(
                                    "Name '~ts' is not registered", [Name]
                                )
                            )
                        )}
            end
    end;
unregister_name(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', unregister),
            iolist_to_binary(
                io_lib:format(
                    "unregister_name/1 expects atom, got ~tp", [Name]
                )
            )
        )}.

-doc """
Look up the pid registered under `Name`.

Returns `{ok, Pid}` if a process is registered, or `undefined` if
no process is registered under that name. Non-atom input returns
`{error, #beamtalk_error{kind = type_error}}`.
""".
-spec whereis_name(term()) -> {ok, pid()} | undefined | {error, #beamtalk_error{}}.
whereis_name(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid when is_pid(Pid) -> {ok, Pid}
    end;
whereis_name(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', whereisName),
            iolist_to_binary(
                io_lib:format(
                    "whereis_name/1 expects atom, got ~tp", [Name]
                )
            )
        )}.

-doc """
List all names currently registered to Beamtalk actors.

Filters `erlang:registered/0` down to processes that carry the
`'$beamtalk_actor'` process-dictionary marker, so kernel processes,
gen_servers outside the Beamtalk runtime, and other raw OTP
processes are excluded.
""".
-spec all_registered() -> [atom()].
all_registered() ->
    [
        Name
     || Name <- erlang:registered(),
        case erlang:whereis(Name) of
            undefined -> false;
            Pid -> is_beamtalk_actor(Pid)
        end
    ].

-doc """
Spawn an actor under a registered name (arity 2).

Equivalent to `start_link({local, Name}, Module, [])`. Returns
`{ok, Pid}` on success or `{error, #beamtalk_error{}}` on
name-related failures. Other errors (crash in init, etc.) pass
through from `gen_server:start_link/4` as `{error, Reason}`.
""".
-spec 'spawnAs'(term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAs'(Name, Module) ->
    'spawnAs'(Name, Module, []).

-doc """
Spawn an actor under a registered name (arity 3).

Delegates to `safe_spawn_named/3` (which wraps
`gen_server:start_link({local, Name}, ...)` with trap_exit and
`initialize` synchronization) after enforcing the reserved-name
blocklist and the atom type-check. On `{already_started, _}` or
`badarg` from the registry, returns a structured `#beamtalk_error{}`
with kind `name_registered` so the stdlib boundary can translate
to `Result error: ...`.
""".
-spec 'spawnAs'(term(), term(), term()) -> {ok, pid()} | {error, term()}.
'spawnAs'(Name, Module, Args) when is_atom(Name), is_atom(Module) ->
    case reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(reserved_name, 'Actor', spawnAs),
                    iolist_to_binary(
                        io_lib:format(
                            "Cannot spawn actor under reserved name '~ts'", [Name]
                        )
                    )
                )};
        false ->
            try safe_spawn_named(Name, Module, Args) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_started, _Other}} ->
                    name_registered_error(Name);
                {error, Reason} ->
                    {error, Reason}
            catch
                %% gen_server:start_link can raise badarg from the local
                %% registry (e.g., name already taken at the moment of
                %% registration). Translate to the structured error.
                error:badarg ->
                    name_registered_error(Name)
            end
    end;
'spawnAs'(Name, Module, Args) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', spawnAs),
            iolist_to_binary(
                io_lib:format(
                    "spawnAs/3 expects (atom, module, term), got (~tp, ~tp, ~tp)",
                    [Name, Module, Args]
                )
            )
        )}.

%%% ============================================================================
%%% Beamtalk stdlib FFI shims (ADR 0079, BT-1988)
%%%
%%% These functions back `stdlib/src/Actor.bt`'s named-registration API via
%%% `(Erlang beamtalk_actor)` FFI calls. They take the class object (or actor
%%% instance) as the first argument and translate runtime `{ok, _} | {error, _}`
%%% tuples into the shape ADR 0076 auto-converts to `Result` at the FFI
%%% boundary.
%%% ============================================================================

-doc """
FFI shim for `class spawnAs: name :: Symbol -> Result(Self, Error)`.

`Self` is the class object `{beamtalk_object, 'ClassName class', Module, ClassPid}`.
Delegates to `'spawnAs'/3` with an empty args map, then wraps the returned pid
into a `#beamtalk_object{}` carrying the receiver class.
""".
-spec doSpawnAs(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAs(Self, Name) ->
    doSpawnWith(Self, #{}, Name).

-doc """
FFI shim for `class spawnWith: initArgs :: Object as: name :: Symbol -> Result(Self, Error)`.

Atomically spawns an actor registered under `Name`. Returns a tagged
`{ok, ActorObject} | {error, #beamtalk_error{}}` tuple.
""".
-spec doSpawnWith(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWith(Self, InitArgs, Name) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case 'spawnAs'(Name, Module, InitArgs) of
                {ok, Pid} ->
                    {ok, #beamtalk_object{
                        class = ClassName,
                        class_mod = Module,
                        pid = Pid
                    }};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{
                        class = ClassName,
                        selector = 'spawnAs:'
                    }};
                {error, Reason} ->
                    {error, generic_spawn_error(ClassName, 'spawnAs:', Reason)}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'spawnAs:')}
    end.

-doc """
FFI shim for `registerAs: name :: Symbol -> Result(Self, Error)`.

On success returns the receiver (the actor object) wrapped in `{ok, _}` so
the stdlib boundary materialises it as `Result ok: self` — matching ADR 0079's
fluent-chain contract.
""".
-spec registerAs(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
registerAs(Self, Name) when is_record(Self, beamtalk_object) ->
    Pid = Self#beamtalk_object.pid,
    ClassName = Self#beamtalk_object.class,
    case register_name(Name, Pid) of
        {ok, _} ->
            {ok, Self};
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{
                class = ClassName,
                selector = 'registerAs:'
            }}
    end;
registerAs(Self, _Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', registerAs),
            iolist_to_binary(
                io_lib:format(
                    "registerAs: expects an Actor receiver, got ~tp", [Self]
                )
            )
        )}.

-doc """
FFI shim for `unregister -> Symbol`.

Idempotent: returns `#ok` even if the receiver was not registered or was
already unregistered. Only raises on real failures (reserved-name, type error).
""".
-spec unregister(#beamtalk_object{}) -> ok.
unregister(Self) when is_record(Self, beamtalk_object) ->
    Pid = Self#beamtalk_object.pid,
    case registered_name_for_pid(Pid) of
        undefined ->
            ok;
        Name ->
            case unregister_name(Name) of
                ok ->
                    ok;
                {error, #beamtalk_error{kind = name_registered}} ->
                    %% Raced with another unregister — still idempotent.
                    ok;
                {error, #beamtalk_error{} = Err0} ->
                    Err1 = beamtalk_error:with_selector(Err0, unregister),
                    beamtalk_error:raise(
                        Err1#beamtalk_error{class = Self#beamtalk_object.class}
                    )
            end
    end;
unregister(Self) ->
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', unregister),
            iolist_to_binary(
                io_lib:format(
                    "unregister expects an Actor receiver, got ~tp", [Self]
                )
            )
        )
    ).

-doc """
FFI shim for `registeredName -> Symbol | Nil`.

Returns the atom the actor is registered under, or `nil` when the actor has no
registered name (or has terminated).
""".
-spec registeredName(#beamtalk_object{}) -> atom() | nil.
registeredName(Self) when is_record(Self, beamtalk_object) ->
    case registered_name_for_pid(Self#beamtalk_object.pid) of
        undefined -> nil;
        Name -> Name
    end;
registeredName(_) ->
    nil.

-doc """
FFI shim for `isRegistered -> Boolean`.
""".
-spec isRegistered(#beamtalk_object{}) -> boolean().
isRegistered(Self) when is_record(Self, beamtalk_object) ->
    registered_name_for_pid(Self#beamtalk_object.pid) =/= undefined;
isRegistered(_) ->
    false.

-doc """
FFI shim for `class named: name :: Symbol -> Result(Self, Error)`.

Looks up the pid registered under `Name`, validates the registered actor's
class is (or descends from) the receiver class via the `'$beamtalk_actor'`
process-dictionary marker, and returns a `#beamtalk_object{}` narrowed to the
receiver class. Returns structured errors for the `name_not_registered`,
`wrong_class`, and `type_error` cases per ADR 0079.
""".
-spec named(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
named(Self, Name) when is_atom(Name) ->
    case class_self_to_name_and_module(Self) of
        {ok, ReceiverClass, _ReceiverModule} ->
            %% Hoisted out so error cases below can reference it uniformly.
            _ = ReceiverClass,
            case erlang:whereis(Name) of
                undefined ->
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(
                                name_not_registered, ReceiverClass, 'named:'
                            ),
                            iolist_to_binary(
                                io_lib:format(
                                    "No actor is registered under name '~ts'",
                                    [Name]
                                )
                            )
                        )};
                Pid when is_pid(Pid) ->
                    case pid_class_name(Pid) of
                        undefined ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(
                                        wrong_class, ReceiverClass, 'named:'
                                    ),
                                    iolist_to_binary(
                                        io_lib:format(
                                            "Name '~ts' is registered to a non-Beamtalk "
                                            "process",
                                            [Name]
                                        )
                                    )
                                )};
                        ActualClass ->
                            case class_matches(ActualClass, ReceiverClass) of
                                true ->
                                    case class_mod_for(ActualClass) of
                                        {ok, ActualModule} ->
                                            {ok, #beamtalk_object{
                                                class = ActualClass,
                                                class_mod = ActualModule,
                                                pid = Pid
                                            }};
                                        not_found ->
                                            {error,
                                                beamtalk_error:with_hint(
                                                    beamtalk_error:new(
                                                        wrong_class,
                                                        ReceiverClass,
                                                        'named:'
                                                    ),
                                                    iolist_to_binary(
                                                        io_lib:format(
                                                            "Registered actor class "
                                                            "'~ts' has no loaded module",
                                                            [ActualClass]
                                                        )
                                                    )
                                                )}
                                    end;
                                false ->
                                    {error,
                                        beamtalk_error:with_hint(
                                            beamtalk_error:new(
                                                wrong_class, ReceiverClass, 'named:'
                                            ),
                                            iolist_to_binary(
                                                io_lib:format(
                                                    "Name '~ts' is registered to a "
                                                    "~ts, not a ~ts",
                                                    [Name, ActualClass, ReceiverClass]
                                                )
                                            )
                                        )}
                            end
                    end
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'named:')}
    end;
named(_Self, Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'named:'),
            iolist_to_binary(
                io_lib:format(
                    "named: expects a Symbol, got ~tp", [Name]
                )
            )
        )}.

-doc """
FFI shim for `class allRegistered -> Array(Actor)`.

Returns a list of `#beamtalk_object{}` proxies for every currently-registered
Beamtalk actor (those carrying the `'$beamtalk_actor'` marker). Kernel
processes and raw OTP-registered processes are excluded.
""".
-spec allRegistered(#beamtalk_object{}) -> [#beamtalk_object{}].
allRegistered(_Self) ->
    lists:filtermap(
        fun(Name) ->
            case erlang:whereis(Name) of
                undefined ->
                    false;
                Pid when is_pid(Pid) ->
                    case pid_class_name(Pid) of
                        undefined ->
                            false;
                        ClassName ->
                            case class_mod_for(ClassName) of
                                {ok, Module} ->
                                    {true, #beamtalk_object{
                                        class = ClassName,
                                        class_mod = Module,
                                        pid = Pid
                                    }};
                                not_found ->
                                    false
                            end
                    end
            end
        end,
        all_registered()
    ).

%%% Internal helpers for the FFI shims above.

-spec class_self_to_name_and_module(term()) ->
    {ok, atom(), module()} | {error, #beamtalk_error{}}.
class_self_to_name_and_module(#beamtalk_object{pid = ClassPid}) when is_pid(ClassPid) ->
    try
        ClassName = beamtalk_object_class:class_name(ClassPid),
        Module = beamtalk_object_class:module_name(ClassPid),
        {ok, ClassName, Module}
    catch
        exit:_ ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, 'Actor'),
                    <<"Class object is not reachable">>
                )}
    end;
class_self_to_name_and_module(Other) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor'),
            iolist_to_binary(
                io_lib:format(
                    "Expected a class receiver, got ~tp", [Other]
                )
            )
        )}.

-spec registered_name_for_pid(pid()) -> atom() | undefined.
registered_name_for_pid(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        [] -> undefined;
        undefined -> undefined
    end.

-spec pid_class_name(pid()) -> atom() | undefined.
pid_class_name(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor', 1, Dict) of
                {'$beamtalk_actor', Class} when is_atom(Class) -> Class;
                _ -> undefined
            end;
        _ ->
            undefined
    end.

-spec class_matches(atom(), atom()) -> boolean().
class_matches(ActualClass, ReceiverClass) when ActualClass =:= ReceiverClass ->
    true;
class_matches(ActualClass, ReceiverClass) ->
    beamtalk_behaviour_intrinsics:walk_hierarchy(
        ActualClass,
        fun(CN, _CPid, _Acc) ->
            case CN of
                ReceiverClass -> {halt, true};
                _ -> {cont, false}
            end
        end,
        false
    ).

-spec class_mod_for(atom()) -> {ok, module()} | not_found.
class_mod_for(ClassName) ->
    case beamtalk_class_module_table:lookup(ClassName) of
        {ok, Module} -> {ok, Module};
        not_found -> not_found
    end.

-spec generic_spawn_error(atom(), atom(), term()) -> #beamtalk_error{}.
generic_spawn_error(ClassName, Selector, Reason) ->
    beamtalk_error:with_hint(
        beamtalk_error:with_selector(
            beamtalk_error:new(instantiation_error, ClassName),
            Selector
        ),
        iolist_to_binary(io_lib:format("spawn failed: ~tp", [Reason]))
    ).

-spec name_registered_error(atom()) -> {error, #beamtalk_error{}}.
name_registered_error(Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(name_registered, 'Actor', spawnAs),
            iolist_to_binary(
                io_lib:format(
                    "Name '~ts' is already registered", [Name]
                )
            )
        )}.

-doc """
Check whether `Name` is on the reserved-name blocklist.

Reserved names are:
  - OTP kernel / stdlib registered process names that a user atom
    collision with would be catastrophic (application_controller,
    code_server, erl_prim_loader, erl_signal_server, error_logger,
    erts_code_purger, file_server_2, global_group, global_group_check,
    global_name_server, inet_db, init, kernel_refc, kernel_safe_sup,
    kernel_sup, logger, logger_handler_watcher, logger_proxy,
    logger_std_h_default, logger_sup, net_kernel, net_sup, rex,
    socket_registry, standard_error, standard_error_sup,
    standard_error_writer, user, user_drv, user_drv_reader,
    user_drv_writer).
  - Any atom whose textual form is prefixed `beamtalk_` — reserved
    for internal runtime / supervision use.

See ADR 0079 ("Reserved-name policy") for rationale.
""".
-spec reserved_name(atom()) -> boolean().
reserved_name(Name) when is_atom(Name) ->
    case is_kernel_reserved(Name) of
        true ->
            true;
        false ->
            NameStr = atom_to_list(Name),
            lists:prefix("beamtalk_", NameStr)
    end;
reserved_name(_) ->
    false.

%% Static blocklist of OTP kernel / stdlib registered names.
%% Kept as a function (not a macro) so dialyzer sees the pattern match.
-spec is_kernel_reserved(atom()) -> boolean().
is_kernel_reserved(application_controller) -> true;
is_kernel_reserved(code_server) -> true;
is_kernel_reserved(erl_prim_loader) -> true;
is_kernel_reserved(erl_signal_server) -> true;
is_kernel_reserved(error_logger) -> true;
is_kernel_reserved(erts_code_purger) -> true;
is_kernel_reserved(file_server_2) -> true;
is_kernel_reserved(global_group) -> true;
is_kernel_reserved(global_group_check) -> true;
is_kernel_reserved(global_name_server) -> true;
is_kernel_reserved(inet_db) -> true;
is_kernel_reserved(init) -> true;
is_kernel_reserved(kernel_refc) -> true;
is_kernel_reserved(kernel_safe_sup) -> true;
is_kernel_reserved(kernel_sup) -> true;
is_kernel_reserved(logger) -> true;
is_kernel_reserved(logger_handler_watcher) -> true;
is_kernel_reserved(logger_proxy) -> true;
is_kernel_reserved(logger_std_h_default) -> true;
is_kernel_reserved(logger_sup) -> true;
is_kernel_reserved(net_kernel) -> true;
is_kernel_reserved(net_sup) -> true;
is_kernel_reserved(rex) -> true;
is_kernel_reserved(socket_registry) -> true;
is_kernel_reserved(standard_error) -> true;
is_kernel_reserved(standard_error_sup) -> true;
is_kernel_reserved(standard_error_writer) -> true;
is_kernel_reserved(user) -> true;
is_kernel_reserved(user_drv) -> true;
is_kernel_reserved(user_drv_reader) -> true;
is_kernel_reserved(user_drv_writer) -> true;
is_kernel_reserved(_) -> false.
