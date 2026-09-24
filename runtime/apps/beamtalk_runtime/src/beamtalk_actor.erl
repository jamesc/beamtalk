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

WARNING (remote actors, ADR 0126 §7.3): for an actor on another node,
`isAlive` answers `true` optimistically without probing the peer, and
keeps answering `true` even after that actor has genuinely died — it
never becomes `false` through this selector. This mirrors the internal
liveness guard's "let the send fail with its real reason" policy, not
a liveness probe. Use a monitor's `'DOWN'` message to detect a dead
remote actor.

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

## Spawn Architecture

There are multiple paths that create actor processes. The `initialize`
hook (if defined) is called inside the generated `init/1` callback,
so it runs for ALL spawn paths — direct, supervised, and named.

| Path | Entry | Context | Initialize? |
|------|-------|---------|-------------|
| Module:spawn/0,1 | gen_server:start → init/1 (unlinked) | Batch/tests | Yes |
| REPL spawn | Module:spawn/0,1 + register_spawned/4 | REPL | Yes |
| class_send → spawn | erlang:apply(Module, spawn, Args) | Runtime | Yes |
| self spawn/spawnWith: | safe_spawn/2, unlinked (or linked if inside a `withClassMethod:` supervisor factory) | Class method | Yes |
| self spawnAs:/spawnWith:as: | safe_spawn_named/3, unlinked after (or stays linked, same exception) | Class method | Yes |
| withName: supervisor child | safe_spawn_named/3 → gen_server:start_link → init/1 | Supervised | Yes |
| Supervisor child (unnamed, plain spawn/spawnWith:) | start_link/1 → gen_server:start_link → init/1 | Supervised | Yes |
| withClassMethod: supervisor child | start_child_via_class_method/4 → factory's `self spawn`/`spawnWith:`/`spawnAs:`/`spawnWith:as:` → safe_spawn/2 or safe_spawn_named/3, linked | Supervised | Yes |
| dynamic_object | gen_server:start_link(?MODULE, ...) | Internal | No (by design) |

unnamed `Module:spawn/0,1` (via `beamtalk_actor:safe_spawn/2`)
spawns unlinked in the common case — the caller (which can be a class
gen_server for dynamic dispatch, or a class method body for `self spawn`)
is never linked to the actor it creates, so killing the actor cannot take
the caller down with it. `safe_spawn_named/3` (`spawnAs:`/`spawnWith:as:`)
stays linked, because it doubles as the real OTP supervisor child MFA for
`SupervisionSpec withName:` children (ADR 0079) — that link is the
restart mechanism, not a bug. Its own `self`-send risk (a class method's
`self spawnAs:` running inside the class gen_server) is fixed by unlinking
right after the spawn succeeds — see
`beamtalk_class_instantiation:do_class_self_named_spawn/6`. A plain
(non-`withClassMethod:`) unnamed `Supervisor child` — real OTP supervision
via `beamtalk_supervisor:spec_to_otp/1` calling `Module:start_link/1`
directly — links to the supervisor without ever going through
`safe_spawn/2`, so it was never part of this bug.

`SupervisionSpec withClassMethod:`
children are the exception — their factory method runs directly
inside the real supervisor process
(`beamtalk_supervisor:start_child_via_class_method/4`), so a plain
`self spawn`/`self spawnWith:`/`self spawnAs:`/`self spawnWith:as:` call
inside that factory *does* need the link, or OTP restart silently breaks
(the supervisor never sees the child exit). `start_child_via_class_method/4`
marks that context in the process dictionary
(`?BT_SUPERVISOR_SPAWN_CONTEXT_KEY`) so `safe_spawn/2` and
`do_class_self_named_spawn/6` can tell it apart from the ordinary
class-gen-server case and keep the link instead of severing it.

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

%% ADR 0126 §3: timeout for the `erpc:call/5` a remote spawn/lookup/list
%% makes to the target node — matches `gen_server:call/2`'s own 5000ms
%% default so a stuck remote op fails on the same order of magnitude as a
%% stuck local one, rather than hanging indefinitely (erpc's own default).
-define(BT_REMOTE_CALL_TIMEOUT, 5000).

%% Public API
-export([start_link/2, start_link/3, start_link_supervised/3, register_spawned/4]).
-export([await_initialize/1, safe_spawn/2]).

%% Message send helpers (lifecycle-aware wrappers)
-export([async_send/4, sync_send/3, sync_send/4, cast_send/3]).

%% Propagated context (ADR 0069 Phase 2b)
-export([get_propagated_ctx/0, restore_propagated_ctx/1]).

%% Application-level trace context
-export([set_trace_context/1, get_trace_context/0, clear_trace_context/0]).

%% Causal trace context
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

%% BT-3596: non-parent 'EXIT' interception for trapping actors — called
%% from generated handle_info/2 (both Server-subclass and plain-Actor paths)
-export([handle_linked_exit/2]).

%% Internal dispatch
-export([dispatch/4, make_self/1]).

%% exported so generated self-dispatch error breadcrumbs
%% (dispatch_codegen.rs's generate_self_dispatch_error_clause) can resolve
%% the actor's actual runtime class at the error-wrap boundary, matching
%% sync_send_remote/3's cross-actor Context construction.
-export([lookup_class/1]).

%% ADR 0126 §7.3 (Phase 0): let a caller that already holds the actor's
%% class — from a #beamtalk_object{} record — hand it to lookup_class/1 as
%% a fallback for a remote pid, whose class the local-only
%% beamtalk_instance_registry cannot resolve. beamtalk_message_dispatch is
%% the one call site that has the class handy; it stashes it for the
%% duration of a single send.
-export([stash_known_class/2, clear_known_class/0]).

%% per-object change publish hook, called from compiled actor
%% gen_server callbacks after a method commits new state.
%% strip_local_temps/1 cleans codegen-internal `__local__` threading
%% temporaries from committed state before the reply is emitted.
-export([notify_state_change/2, strip_local_temps/1]).

%% ADR 0126 §5.1/§5.5 (BT-3613): the `'$beamtalk_wire'`-envelope
%% recognition/decode helpers shared by this module's own handle_call/3 and
%% handle_cast/2 clauses AND every compiled actor's generated
%% handle_call/3 / handle_cast/2 (crates/beamtalk-codegen/.../gen_server/
%% callbacks.rs) — the one place that logic lives (CLAUDE.md "No duplicate
%% implementations"). encode_reply_for/3 and
%% maybe_reclassify_compiled_dispatch_error/2 close the same gap on the
%% reply/error side for generated dispatch.
-export([
    decode_wire_call/1,
    decode_wire_cast/1,
    encode_reply_for/3,
    encode_reply_for_tagged/3,
    maybe_reclassify_compiled_dispatch_error/2
]).

%% Shared error constructor used by beamtalk_class_instantiation to avoid
%% duplicating the same instantiation_error construction logic.
-export([generic_spawn_error/3]).

%% Named registration (ADR 0079)
-export([
    is_beamtalk_actor/1,
    pid_class_name/1,
    registered_name_for_pid/1,
    register_name/2,
    unregister_name/1,
    whereis_name/1,
    all_registered/0,
    'spawnAs'/2,
    'spawnAs'/3,
    reserved_name/1
]).

%% Beamtalk stdlib FFI shims for actor.bt named registration (ADR 0079)
%% Selectors are re-derived by beamtalk_erlang_proxy from the first keyword;
%% these names deliberately differ from the runtime `spawnAs/2,3` entry points
%% so they do not collide at the FFI dispatch layer.
%%
%% doSpawn/1 and doSpawnWith/2 back the lifted `Actor>>spawn` /
%% `Actor>>spawnWith:` bodies (sibling of doSpawnAs/2 / doSpawnWith/3 above,
%% which already back the named-registration variants). They coexist with
%% doSpawnWith/3 as separate-arity clauses of the same function name.
-export([
    doSpawn/1,
    doSpawnAs/2,
    doSpawnWith/2,
    doSpawnWith/3,
    registerAs/2,
    unregister/1,
    registeredName/1,
    isRegistered/1,
    named/2,
    allRegistered/1
]).

%% Remote spawn and lookup (ADR 0126 §3, Phase 2, BT-3599). `remote_spawn/4`
%% and `remote_named/3` are the origin-node entry points (erpc caller);
%% `remote_spawn_target/2` and `remote_named_target/2` are exported so
%% `erpc:call/5` can reach them — they run ON the target node, in the
%% temporary process `erpc` spawns to execute the call.
-export([
    remote_spawn/4,
    remote_spawn_target/2,
    remote_spawn_with/5,
    remote_spawn_with_target/3,
    remote_named/3,
    remote_named_target/2,
    remote_all_registered/1
]).

%% Beamtalk stdlib FFI shims for actor.bt remote spawn/lookup (ADR 0126 §3,
%% Phase 2, BT-3599) — mirrors the doSpawnAs/2 etc. shims above.
%% doSpawnWithOn/3 and doSpawnWithAsOn/4 back `spawnWith:on:`/
%% `spawnWith:as:on:` (ADR 0126 §5.1, Phase 3b, BT-3601) — the Value-carrying
%% remote-spawn selectors deferred at Phase 2 until the wire encoder landed.
-export([
    doSpawnOn/2,
    doSpawnAsOn/3,
    doSpawnWithOn/3,
    doSpawnWithAsOn/4,
    doNamedOn/3,
    doAllRegisteredOn/2
]).

%% Lifecycle telemetry (called from compiled actor init/terminate)
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
spawned and registered actors. initialize now runs inside
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
Wait for handle_continue to finish after start_link.

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
Spawn an actor, with initialize synchronization.

Handles the full spawn sequence:
1. Choose linked vs unlinked start (see the rationale below)
2. If start succeeds, wait for handle_continue (initialize) to complete
3. Return {ok, Pid} or {error, Reason}

Deliberately uses `gen_server:start/3` (unlinked), not
`gen_server:start_link/3`, in the common case. `spawn`/`spawnWith:` can run
from inside a class gen_server's `{spawn, _}` handler (dynamic dispatch,
`beamtalk_class_instantiation:handle_spawn/4`) or a class method body
executing in the class's own process (`self spawn` /
`class_self_spawn/3,4`) — either way, this function's caller is whatever
process is evaluating the spawn expression, which linking would tie the
new actor's lifetime to. A link there meant `exit(ActorPid, kill)` — used
by REPL actor cleanup and `classRemoveFromSystemByName`'s actor teardown —
took the caller down with it (e.g. the class gen_server, silently losing
class vars and hot-patched methods). Lifecycle tracking that needs to know
when an actor dies (the REPL actor registry, `onExit:`, etc.) already uses
`erlang:monitor/2`, never the link, so dropping the link changes nothing
for those consumers.

There IS a competing linked use case, though:
`SupervisionSpec withClassMethod:` children. A
`withClassMethod:` factory's body runs directly inside the real OTP
supervisor process via `beamtalk_supervisor:start_child_via_class_method/4`
→ `call_class_method_direct` → `erlang:apply/3` (no process boundary), so if
the factory calls plain `self spawn`/`self spawnWith:`, this function's
caller really is the supervisor — unlinking there would silently defeat OTP
restart. `start_child_via_class_method/4` marks that context by setting
`?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` in the process dictionary before invoking
the factory; since every hop in between is a plain synchronous call in the
same process (no `spawn`/`erlang:apply` to a different process, no message
send-and-receive), the key is still visible here. When it is set, this
delegates to `safe_spawn_linked/2` instead, staying linked exactly like
`safe_spawn_named/3` already does for the named-child case.

This consolidates the await_initialize logic so generated spawn
functions stay simple.
""".
-spec safe_spawn(module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn(Module, InitArgs) ->
    case get(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY) of
        true ->
            safe_spawn_linked(Module, InitArgs);
        _ ->
            case gen_server:start(Module, InitArgs, []) of
                {ok, Pid} -> await_initialize_or_kill_unlinked(Pid);
                {error, Reason} -> {error, Reason};
                ignore -> {error, ignore}
            end
    end.

-doc """
Spawn an actor **linked**, with
trap_exit + initialize synchronization, for the one `safe_spawn/2` case that
needs the link — a `withClassMethod:` supervisor child's factory calling
plain `self spawn`/`self spawnWith:` (see `safe_spawn/2`'s doc). Shares the
trap_exit dance with `safe_spawn_named/3` via `start_link_and_await/1`.
""".
-spec safe_spawn_linked(module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn_linked(Module, InitArgs) ->
    start_link_and_await(fun() -> gen_server:start_link(Module, InitArgs, []) end).

-doc """
Spawn a named actor with trap_exit + initialize synchronization.

Handles the full spawn sequence:
1. Trap exits so a failed start_link or handle_continue doesn't kill caller
2. Call gen_server:start_link({local, Name}, ...)
3. If start_link succeeds, wait for handle_continue (initialize) to complete
4. Restore trap_exit and return {ok, Pid} or {error, Reason}

Unlike `safe_spawn/2` (unnamed) in its common case, this stays
**linked** (`gen_server:start_link/4`, with the original trap_exit dance) —
it is the shared implementation behind both `spawnAs:`/`spawnWith:as:` *and*
the real OTP supervisor child MFA that `beamtalk_supervisor:spec_to_otp/1`
builds for `SupervisionSpec withName:` children (ADR 0079:
`{beamtalk_actor, spawnAs, [Name, Module]}` / `[Name, Module, InitArgs]`
literally names this function as the start callback). That link is not a
bug — it is exactly the OTP contract a real supervisor relies on to
detect the child's exit and restart it; removing it would silently break
supervised named-actor restart (see `beamtalk_supervisor_tests:supervisor_restart_re_registers_name_test/0`).

The risk this shares — a class method's `self spawnAs:` /
`self spawnWith:as:` running inside the class gen_server and linking the
new actor to it — is fixed at that specific call site instead: see
`beamtalk_class_instantiation:do_class_self_named_spawn/6`, which unlinks
immediately after a successful spawn *unless* it detects the
`withClassMethod:`-supervisor context via
`?BT_SUPERVISOR_SPAWN_CONTEXT_KEY`, in which case it stays linked for the
same reason `safe_spawn_linked/2` above does.
""".
-spec safe_spawn_named(atom(), module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn_named(Name, Module, InitArgs) when is_atom(Name) ->
    start_link_and_await(fun() -> gen_server:start_link({local, Name}, Module, InitArgs, []) end).

-doc """
Shared linked-start + trap_exit + await_initialize dance for
`safe_spawn_named/3` and `safe_spawn_linked/2`. `StartFun` performs the
actual `gen_server:start_link/3,4` call (parametrized so the two callers can
each pass their own registration shape) and is invoked exactly once, with
`trap_exit` already set, so a crash during `initialize` delivers an `'EXIT'`
message here instead of taking this process down.
""".
-spec start_link_and_await(fun(() -> {ok, pid()} | {error, term()} | ignore)) ->
    {ok, pid()} | {error, term()}.
start_link_and_await(StartFun) ->
    OldTrap = erlang:process_flag(trap_exit, true),
    Result = StartFun(),
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

-doc """
Shared await_initialize + force-kill-on-timeout tail for the unlinked
`safe_spawn/2` path. No link exists to fall back on for an
'EXIT' message on the force-kill path, so this monitors explicitly to
deterministically observe termination before returning.
""".
-spec await_initialize_or_kill_unlinked(pid()) -> {ok, pid()} | {error, term()}.
await_initialize_or_kill_unlinked(Pid) ->
    case await_initialize(Pid) of
        ok ->
            {ok, Pid};
        {error, Reason} ->
            MonRef = erlang:monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', MonRef, process, Pid, _} -> ok
            end,
            {error, Reason}
    end.

%%% Message Send Helpers
%%%
%%% These functions wrap gen_server:cast/call with lifecycle-aware behavior:
%%% - `isAlive`, `monitor`, and `stop` are handled locally (no gen_server message needed)
%%% - Dead actor detection rejects futures / returns errors immediately
%%%
%%% WARNING: Race condition! beamtalk_pid:is_alive/1 is a snapshot check
%%% (and answers `true` optimistically for a remote pid — ADR 0126 §7.3).
%%% The actor could die between the alive check and the gen_server:cast.
%%% For robust lifecycle management, use monitors instead of isAlive polling.

-doc """
Send an asynchronous message to an actor, with lifecycle handling.

Handles lifecycle methods locally without involving the actor process:
- `isAlive` - checks if process is alive, resolves Future with boolean
  (remote pid: always `true`, ADR 0126 §7.3 — see moduledoc)
- `monitor` - creates a monitor reference, resolves Future with ref
- `stop` - gracefully stops the actor process, resolves Future with ok

For all other messages, checks if the actor is alive first:
- If alive, sends via gen_server:cast (normal async path)
- If dead, rejects the Future with an `actor_dead` error
""".
-spec async_send(
    pid() | {registered, atom()} | {registered, atom(), node()}, atom(), list(), pid()
) ->
    ok.
%% ADR 0079: name-resolving proxy fan-out. Name-only selectors
%% answer from the proxy itself; other selectors resolve the name to the
%% currently-registered pid and re-enter the pid-based clauses below.
async_send({registered, Name}, isAlive, [], FuturePid) when is_atom(Name) ->
    Result = erlang:whereis(Name) =/= undefined,
    beamtalk_future:resolve(FuturePid, Result),
    ok;
async_send({registered, _Name}, isRegistered, [], FuturePid) ->
    beamtalk_future:resolve(FuturePid, true),
    ok;
async_send({registered, Name}, registeredName, [], FuturePid) when is_atom(Name) ->
    beamtalk_future:resolve(FuturePid, Name),
    ok;
async_send({registered, Name} = Ref, Selector, Args, FuturePid) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            Error = no_such_process_error(Ref, Selector),
            beamtalk_future:reject(FuturePid, Error),
            ok;
        Pid when is_pid(Pid) ->
            async_send(Pid, Selector, Args, FuturePid)
    end;
%% ADR 0126 §3: node-qualified proxy fan-out (`named:on:`/`allRegisteredOn:`,
%% or a local `{registered, Name}` rewritten crossing a node boundary) —
%% mirrors the two-tuple clauses above, resolving via `resolve_remote_registered/2`
%% (a remote `erpc` `whereis/1` when `Node` is not this node) instead of a
%% bare local `whereis/1`.
async_send({registered, Name, Node}, isAlive, [], FuturePid) when is_atom(Name), is_atom(Node) ->
    Result = is_pid(resolve_remote_registered(Name, Node)),
    beamtalk_future:resolve(FuturePid, Result),
    ok;
async_send({registered, _Name, _Node}, isRegistered, [], FuturePid) ->
    beamtalk_future:resolve(FuturePid, true),
    ok;
async_send({registered, Name, _Node}, registeredName, [], FuturePid) when is_atom(Name) ->
    beamtalk_future:resolve(FuturePid, Name),
    ok;
async_send({registered, Name, Node} = Ref, Selector, Args, FuturePid) when
    is_atom(Name), is_atom(Node)
->
    case resolve_remote_registered(Name, Node) of
        Pid when is_pid(Pid) ->
            async_send(Pid, Selector, Args, FuturePid);
        undefined ->
            Error = no_such_process_error(Ref, Selector),
            beamtalk_future:reject(FuturePid, Error),
            ok;
        node_down ->
            beamtalk_future:reject(FuturePid, node_down_error_record(Node, Selector)),
            ok
    end;
async_send(ActorPid, isAlive, [], FuturePid) ->
    %% isAlive is handled locally - no message to the actor
    Result = beamtalk_pid:is_alive(ActorPid),
    beamtalk_future:resolve(FuturePid, Result),
    ok;
async_send(ActorPid, isRemote, [], FuturePid) ->
    %% isRemote must compare against the *caller's* node, so it cannot run
    %% in the actor — see is_remote/2.
    try sync_send(ActorPid, isRemote, []) of
        Result -> beamtalk_future:resolve(FuturePid, Result)
    catch
        error:#beamtalk_error{} = Error ->
            beamtalk_future:reject(FuturePid, Error);
        error:#{error := #beamtalk_error{} = Error} ->
            beamtalk_future:reject(FuturePid, Error);
        Class:Reason ->
            Error = beamtalk_error:with_details(
                beamtalk_error:new(runtime_error, unknown, isRemote),
                #{original_class => Class, original_reason => Reason}
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(ActorPid, stop, [], FuturePid) ->
    %% stop is handled locally - gracefully stops the actor process
    %% No send-site telemetry for stop — terminate/2 handles it
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
    %% Telemetry, monitor-before-kill, and DOWN-wait are in kill_and_wait/1.
    case kill_and_wait(ActorPid) of
        ok ->
            beamtalk_future:resolve(FuturePid, ok);
        {error, Error} ->
            %% We did not observe a DOWN within 5 s; cannot guarantee the actor
            %% is gone, so reject the future rather than silently claiming success.
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok;
async_send(_ActorPid, delegate, [], FuturePid) ->
    %% Non-native Actors do not have a backing Erlang module.
    beamtalk_future:reject(FuturePid, delegate_error(unknown)),
    ok;
async_send(ActorPid, pid, [], FuturePid) ->
    %% pid returns the raw Erlang PID backing the actor
    beamtalk_future:resolve(FuturePid, ActorPid),
    ok;
async_send(ActorPid, monitor, [], FuturePid) ->
    %% monitor is handled locally - creates an Erlang monitor
    Ref = erlang:monitor(process, ActorPid),
    beamtalk_future:resolve(FuturePid, Ref),
    ok;
async_send(ActorPid, 'onExit:', [Block], FuturePid) ->
    %% onExit: monitors the actor and calls block with reason on exit.
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
    %% Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures dispatch-to-mailbox time for async sends.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => async},
    %% Check liveness before sending, and spawn a watcher to detect
    %% actor death during message processing. The watcher monitors both the
    %% actor and future processes: if the actor dies before the future is
    %% resolved, the watcher rejects the future with a structured error.
    maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
        case beamtalk_pid:is_alive(ActorPid) of
            true ->
                PropCtx = get_propagated_ctx(),
                %% ADR 0126 §5.1: the only added cost for a local send is this
                %% one node/1 comparison — a remote target wire-encodes Args
                %% and tags the cast so the receiver's handle_cast prelude
                %% recognises it.
                case node(ActorPid) =/= node() of
                    true ->
                        case beamtalk_wire:encode(Args) of
                            {ok, WireArgs} ->
                                gen_server:cast(
                                    ActorPid,
                                    {'$beamtalk_wire', ?BT_WIRE_VERSION, async, Selector, WireArgs,
                                        FuturePid, PropCtx}
                                ),
                                spawn_future_watcher(ActorPid, FuturePid, Selector),
                                {ok, Metadata#{outcome => ok}};
                            {error, EncErr} ->
                                %% Sender-side encode failure (request direction,
                                %% ADR 0126 §5.2) — never reaches the network.
                                beamtalk_future:reject(
                                    FuturePid, EncErr#beamtalk_error{selector = Selector}
                                ),
                                {ok, Metadata#{outcome => error}}
                        end;
                    false ->
                        gen_server:cast(ActorPid, {Selector, Args, FuturePid, PropCtx}),
                        spawn_future_watcher(ActorPid, FuturePid, Selector),
                        {ok, Metadata#{outcome => ok}}
                end;
            false ->
                beamtalk_future:reject(FuturePid, actor_dead_error_record(Selector)),
                {ok, Metadata#{outcome => error}}
        end
    end),
    ok.

-doc """
Send a fire-and-forget message to an actor (no future, no return value).

Checks if the actor is alive before sending. If dead, silently returns ok
(fire-and-forget semantics — the caller does not expect a reply).

WARNING: Race condition! beamtalk_pid:is_alive/1 is a snapshot check.
The actor could die between the alive check and the gen_server:cast.
""".
-spec cast_send(pid() | {registered, atom()} | {registered, atom(), node()}, atom(), list()) -> ok.
%% ADR 0079: name-resolving proxy fan-out for fire-and-forget
%% sends. If the name is not currently registered, silently drop the cast
%% (consistent with cast_send's existing `actor dead -> ok` semantics).
cast_send({registered, Name}, Selector, Args) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> ok;
        Pid when is_pid(Pid) -> cast_send(Pid, Selector, Args)
    end;
%% ADR 0126 §3: node-qualified proxy — see async_send/4's matching clause.
%% A `node_down` here is silently dropped too, matching the "actor dead ->
%% ok" fire-and-forget contract this function already has for a stale name.
cast_send({registered, Name, Node}, Selector, Args) when is_atom(Name), is_atom(Node) ->
    case resolve_remote_registered(Name, Node) of
        Pid when is_pid(Pid) -> cast_send(Pid, Selector, Args);
        undefined -> ok;
        node_down -> ok
    end;
cast_send(ActorPid, Selector, Args) ->
    %% Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures dispatch-to-mailbox time for cast sends.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => cast},
    maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
        case beamtalk_pid:is_alive(ActorPid) of
            true ->
                PropCtx = get_propagated_ctx(),
                case node(ActorPid) =/= node() of
                    true ->
                        case beamtalk_wire:encode(Args) of
                            {ok, WireArgs} ->
                                gen_server:cast(
                                    ActorPid,
                                    {'$beamtalk_wire', ?BT_WIRE_VERSION, cast, Selector, WireArgs,
                                        PropCtx}
                                ),
                                {ok, Metadata#{outcome => cast}};
                            {error, Error} ->
                                %% Fire-and-forget: no caller to tell (mirrors
                                %% the receiver-side cast-skew handling, ADR
                                %% 0126 §5.2) — log and drop, matching Erlang's
                                %% fire-and-forget semantics.
                                ?LOG_WARNING("Cast argument not serialisable for remote send", #{
                                    selector => Selector,
                                    pid => ActorPid,
                                    error => Error,
                                    domain => [beamtalk, runtime, dist]
                                }),
                                {ok, Metadata#{outcome => cast}}
                        end;
                    false ->
                        gen_server:cast(ActorPid, {cast, Selector, Args, PropCtx}),
                        {ok, Metadata#{outcome => cast}}
                end;
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
  (remote pid: always `true`, ADR 0126 §7.3 — see moduledoc)
- `isRemote` - asks the actor for its `node` and compares it with the
  caller's node (ADR 0126 §2 — see is_remote/2)
- `monitor` - creates a monitor reference, returns ref
- `onExit:` - monitors actor and calls block on exit
- `stop` - gracefully stops the actor process, returns ok

For all other messages, checks if the actor is alive first:
- If alive, sends via gen_server:call and unwraps the result
- If dead, raises `#beamtalk_error{kind = actor_dead}`
- If timeout, raises `#beamtalk_error{kind = timeout}`
""".
-spec sync_send(pid() | {registered, atom()} | {registered, atom(), node()}, atom(), list()) ->
    term().
%% ADR 0079: name-resolving proxy fan-out. Name-only methods
%% answer from the proxy itself; other methods resolve to the currently-
%% registered pid, raising `no_such_process` if the name has vanished.
sync_send({registered, Name}, isAlive, []) when is_atom(Name) ->
    erlang:whereis(Name) =/= undefined;
sync_send({registered, _Name}, isRegistered, []) ->
    true;
sync_send({registered, Name}, registeredName, []) when is_atom(Name) ->
    Name;
sync_send({registered, Name} = Ref, Selector, Args) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            raise_no_such_process(Ref, Selector);
        Pid when is_pid(Pid) ->
            sync_send(Pid, Selector, Args)
    end;
%% ADR 0126 §3: node-qualified proxy fan-out — see async_send/4's matching
%% clause for the shared `resolve_remote_registered/2` helper. Answers
%% `isAlive`/`isRegistered`/`registeredName` without a network hop except
%% for `isAlive`, which genuinely needs to know whether the name still
%% resolves on `Node`.
sync_send({registered, Name, Node}, isAlive, []) when is_atom(Name), is_atom(Node) ->
    is_pid(resolve_remote_registered(Name, Node));
sync_send({registered, _Name, _Node}, isRegistered, []) ->
    true;
sync_send({registered, Name, _Node}, registeredName, []) when is_atom(Name) ->
    Name;
sync_send({registered, Name, Node} = Ref, Selector, Args) when is_atom(Name), is_atom(Node) ->
    case resolve_remote_registered(Name, Node) of
        undefined ->
            raise_no_such_process(Ref, Selector);
        node_down ->
            raise_node_down(Node, Selector);
        Pid when is_pid(Pid) ->
            sync_send(Pid, Selector, Args)
    end;
sync_send(ActorPid, isAlive, []) ->
    beamtalk_pid:is_alive(ActorPid);
sync_send(ActorPid, isRemote, []) ->
    is_remote(ActorPid, fun() -> sync_send(ActorPid, node, []) end);
sync_send(ActorPid, stop, []) ->
    %% stop is handled locally - gracefully stops the actor process
    %% No send-site telemetry for stop — terminate/2 handles it
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
        exit:Other ->
            %% Preserve sync_send/3 contract: translate exits to structured errors
            beamtalk_exception_handler:reraise(
                beamtalk_error:with_details(actor_dead_error_record(stop), #{exit_reason => Other})
            )
    end;
sync_send(ActorPid, kill, []) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Telemetry, monitor-before-kill, and DOWN-wait are in kill_and_wait/1.
    case kill_and_wait(ActorPid) of
        ok -> ok;
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end;
sync_send(_ActorPid, delegate, []) ->
    %% Non-native Actors do not have a backing Erlang module.
    %% Native Actors will override this at the codegen level.
    beamtalk_exception_handler:reraise(delegate_error(unknown));
sync_send(ActorPid, pid, []) ->
    %% pid returns the raw Erlang PID backing the actor
    ActorPid;
sync_send(ActorPid, monitor, []) ->
    erlang:monitor(process, ActorPid);
sync_send(ActorPid, 'onExit:', [Block]) ->
    %% onExit: monitors the actor and calls block with reason on exit.
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
    %% Layer 1: Fast-path for self-sends.
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
Factored out of sync_send/3 for Layer 1 self-send fast-path.
""".
-spec sync_send_remote(pid(), atom(), list()) -> term().
sync_send_remote(ActorPid, Selector, Args) ->
    %% Instrument with telemetry:span/3 (ADR 0069 Phase 2a).
    %% Measures caller-perspective round-trip time for sync sends.
    %% telemetry:span/3 emits start/stop/exception events automatically.
    %% On exception, span catches it, emits the exception event, and re-raises
    %% via erlang:raise/3 — preserving the original exit class and reason for
    %% the outer try/catch to convert to structured beamtalk_error records.
    Class = lookup_class(ActorPid),
    Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
    try
        maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
            case beamtalk_pid:is_alive(ActorPid) of
                true ->
                    %% Generated handle_call/3 wraps replies as {ok, Result} or {error, Error}.
                    %% Unwrap here so callers receive the value directly.
                    %%
                    %% The {error, Error} case has three sub-forms due to the safe_dispatch layer:
                    %%   1. Error = {ErlType, Value, Stacktrace} — with captured stacktrace
                    %%   2. Error = {ErlType, Value} — backward compat: without stacktrace
                    %%   3. Error = other term — e.g. #beamtalk_error{} from dispatch_user_method
                    %% We re-raise all forms as Erlang exceptions so the caller sees them correctly.
                    PropCtx = get_sync_propagated_ctx(),
                    %% Layer 2: Check for transitive cycles before gen_server:call.
                    %% If ActorPid is already in the call stack, this send would deadlock.
                    check_call_stack(ActorPid, Selector),
                    %% TimeoutProxy manages its own timeout on the inner
                    %% (proxy→target) hop. The outer (caller→proxy) hop must use
                    %% infinity so it doesn't time out before the proxy's configured
                    %% timeout expires. For all other actors, use gen_server:call/2
                    %% which defaults to 5000ms.
                    CallTimeout =
                        case Class of
                            'TimeoutProxy' -> infinity;
                            _ -> default
                        end,
                    CallResult = wire_sync_call(ActorPid, Selector, Args, PropCtx, CallTimeout),
                    case CallResult of
                        {ok, Result} ->
                            {Result, Metadata#{outcome => ok}};
                        {error, {ErlType, ErrorValue, Stacktrace}} ->
                            %% safe_dispatch caught an Erlang exception with stacktrace;
                            %% re-raise with full type/stacktrace context.
                            %% Guard prevents false-match on non-stacktrace 3-tuples.
                            %% attach the active selector/class breadcrumb so a raw
                            %% error escaping the method is classified *and* located.
                            beamtalk_exception_handler:reraise(ErlType, ErrorValue, Stacktrace, #{
                                selector => Selector, class => Class
                            });
                        {error, {ErlType, ErrorValue}} ->
                            %% Backward compat: safe_dispatch without stacktrace —
                            %% still pass exception class to preserve error kind
                            beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
                        {error, Nlr} when ?IS_NLR(Nlr) ->
                            %% A `^` inside a block invoked on ActorPid on our
                            %% behalf (BT-3582). The matching catch frame is in
                            %% *our* process — ActorPid had no frame holding
                            %% that token, so its reply is the only way the
                            %% signal gets back here. Re-throw so the enclosing
                            %% method unwinds as it would have without the
                            %% process hop — same relay shape as
                            %% beamtalk_class_dispatch.erl's class-method hops.
                            throw(Nlr);
                        {error, Error} ->
                            beamtalk_exception_handler:reraise(Error);
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
        exit:{{nodedown, Node}, _} ->
            %% ADR 0126 §7.1: a partitioned or never-connected node —
            %% the actor may be alive on the far side. Checked before the
            %% catch-all so it isn't misreported as actor_dead. Verified
            %% empirically: gen_server:call/2,3 exits with exactly this
            %% shape (`{{nodedown, Node}, {gen_server, call, [...]}}`) for
            %% both a mid-call partition and a never-connected node.
            raise_node_down(Node, Selector);
        exit:{noconnection, _} ->
            %% Defensive: not observed from gen_server:call in practice
            %% (that always wraps as {nodedown, Node} above), but this is
            %% the DOWN reason erlang:monitor/2 uses for the same
            %% condition, and ADR 0126 §7.1 names it explicitly.
            raise_node_down(node(ActorPid), Selector);
        exit:noconnection ->
            raise_node_down(node(ActorPid), Selector);
        exit:{_Reason, _} ->
            %% Catch-all for other exit reasons: {shutdown, Term}, killed,
            %% custom stop reasons, etc. All indicate the actor is unavailable.
            raise_actor_dead(Selector)
    end.

-doc """
Sync `gen_server:call` for both the local and remote (ADR 0126 §5.1) paths,
shared by `sync_send_remote/3` and `sync_send/4`. `CallTimeout` is `default`
(use `gen_server:call/2`'s own 5000ms default), `infinity`, or a
non-negative integer millisecond timeout — mirroring the two call shapes the
two callers already had before this helper existed.

Remote is decided by `node(ActorPid) =/= node()` — one comparison, the only
added cost for a local call (ADR 0126 §5.1 constraint 2). A remote call
wire-encodes `Args` and tags the message so the receiver's `handle_call`
prelude recognises it, then decodes the raw reply that comes back through
the ordinary `gen_server:call` reply path — `decode_call_result/2` reraises
on a reply-direction decode failure (§5.2) rather than returning a
half-decoded term. An encode failure on `Args` is a request-direction
failure (§5.2): it never reaches the network, and is raised here in the
caller, exactly as `beamtalk_wire:encode/1`'s own doc promises.
""".
-spec wire_sync_call(pid(), atom(), list(), map(), default | timeout()) -> term().
wire_sync_call(ActorPid, Selector, Args, PropCtx, CallTimeout) ->
    case node(ActorPid) =/= node() of
        true ->
            case beamtalk_wire:encode(Args) of
                {ok, WireArgs} ->
                    Msg = {'$beamtalk_wire', ?BT_WIRE_VERSION, call, Selector, WireArgs, PropCtx},
                    RawResult = local_call(ActorPid, Msg, CallTimeout),
                    decode_call_result(RawResult, Selector);
                {error, EncErr} ->
                    beamtalk_exception_handler:reraise(EncErr#beamtalk_error{selector = Selector})
            end;
        false ->
            local_call(ActorPid, {Selector, Args, PropCtx}, CallTimeout)
    end.

-spec local_call(pid(), term(), default | timeout()) -> term().
local_call(ActorPid, Msg, default) ->
    gen_server:call(ActorPid, Msg);
local_call(ActorPid, Msg, Timeout) ->
    gen_server:call(ActorPid, Msg, Timeout).

-doc """
Decode a raw reply that came back from a remote `gen_server:call` (ADR 0126
§5.1's "Result path" table). A decode failure here is reply-direction skew
(§5.2): the method has already run on the callee and its state has changed,
so this raises — `direction => reply` is merged into the error's `details`
so it reads distinctly from the identically-kinded request-direction
failure, which never reaches a method body at all.
""".
-spec decode_call_result(term(), atom()) -> term().
decode_call_result(RawResult, Selector) ->
    case beamtalk_wire:decode(RawResult) of
        {ok, Decoded} ->
            Decoded;
        {error, #beamtalk_error{details = Details} = Err} ->
            beamtalk_exception_handler:reraise(Err#beamtalk_error{
                selector = Selector, details = Details#{direction => reply}
            })
    end.

-doc """
Sync-send with explicit timeout.

Same as sync_send/3 but passes the given Timeout to gen_server:call/3.
Timeout is a non-negative integer (milliseconds) or the atom `infinity`.
Used by TimeoutProxy to forward messages with a custom timeout.
""".
-spec sync_send(
    pid() | {registered, atom()} | {registered, atom(), node()}, atom(), list(), timeout()
) ->
    term().
%% ADR 0079: name-resolving proxy fan-out for the explicit-
%% timeout path. Mirror sync_send/3's name-only handling and resolution.
sync_send({registered, Name}, isAlive, [], _Timeout) when is_atom(Name) ->
    erlang:whereis(Name) =/= undefined;
sync_send({registered, _Name}, isRegistered, [], _Timeout) ->
    true;
sync_send({registered, Name}, registeredName, [], _Timeout) when is_atom(Name) ->
    Name;
sync_send({registered, Name} = Ref, Selector, Args, Timeout) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            raise_no_such_process(Ref, Selector);
        Pid when is_pid(Pid) ->
            sync_send(Pid, Selector, Args, Timeout)
    end;
%% ADR 0126 §3: node-qualified proxy fan-out for the explicit-timeout path —
%% see sync_send/3's matching clause.
sync_send({registered, Name, Node}, isAlive, [], _Timeout) when is_atom(Name), is_atom(Node) ->
    is_pid(resolve_remote_registered(Name, Node));
sync_send({registered, _Name, _Node}, isRegistered, [], _Timeout) ->
    true;
sync_send({registered, Name, _Node}, registeredName, [], _Timeout) when is_atom(Name) ->
    Name;
sync_send({registered, Name, Node} = Ref, Selector, Args, Timeout) when
    is_atom(Name), is_atom(Node)
->
    case resolve_remote_registered(Name, Node) of
        undefined ->
            raise_no_such_process(Ref, Selector);
        node_down ->
            raise_node_down(Node, Selector);
        Pid when is_pid(Pid) ->
            sync_send(Pid, Selector, Args, Timeout)
    end;
sync_send(ActorPid, isRemote, [], Timeout) ->
    is_remote(ActorPid, fun() -> sync_send(ActorPid, node, [], Timeout) end);
sync_send(ActorPid, Selector, Args, Timeout) when
    is_integer(Timeout), Timeout >= 0;
    Timeout =:= infinity
->
    %% Layer 1: Fast-path for self-sends (same as sync_send/3).
    case ActorPid =:= self() andalso get('$bt_actor_state') =/= undefined of
        true ->
            self_dispatch(Selector, Args);
        false ->
            Class = lookup_class(ActorPid),
            Metadata = #{pid => ActorPid, class => Class, selector => Selector, mode => sync},
            try
                maybe_span([beamtalk, actor, dispatch], Metadata, fun() ->
                    case beamtalk_pid:is_alive(ActorPid) of
                        true ->
                            PropCtx = get_sync_propagated_ctx(),
                            %% Layer 2: Check for transitive cycles.
                            check_call_stack(ActorPid, Selector),
                            case wire_sync_call(ActorPid, Selector, Args, PropCtx, Timeout) of
                                {ok, Result} ->
                                    {Result, Metadata#{outcome => ok}};
                                {error, {ErlType, ErrorValue, Stacktrace}} ->
                                    %% safe_dispatch with stacktrace
                                    %% attach selector/class breadcrumb (see sync_send/3).
                                    beamtalk_exception_handler:reraise(
                                        ErlType, ErrorValue, Stacktrace, #{
                                            selector => Selector, class => Class
                                        }
                                    );
                                {error, {ErlType, ErrorValue}} ->
                                    %% Backward compat: preserve exception class
                                    beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
                                {error, Nlr} when ?IS_NLR(Nlr) ->
                                    %% Relay a foreign `^` (BT-3582) — see sync_send/3.
                                    throw(Nlr);
                                {error, Error} ->
                                    beamtalk_exception_handler:reraise(Error);
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
                exit:{{nodedown, Node}, _} ->
                    %% ADR 0126 §7.1 (see sync_send_remote/3's clause for
                    %% the empirical basis for this exit shape).
                    raise_node_down(Node, Selector);
                exit:{noconnection, _} ->
                    raise_node_down(node(ActorPid), Selector);
                exit:noconnection ->
                    raise_node_down(node(ActorPid), Selector);
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
    beamtalk_exception_handler:reraise(Error).

-doc """
Direct self-dispatch for re-entrant self-sends (Layer 1).

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
    %% Compiled actors have their own safe_dispatch/3 in the generated
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
    %% safe_dispatch with stacktrace
    put('$bt_actor_state', NewState),
    beamtalk_exception_handler:reraise(ErlType, ErrorValue, Stacktrace);
unwrap_dispatch_result({error, {ErlType, ErrorValue}, NewState}) ->
    %% Backward compat: preserve exception class without stacktrace
    put('$bt_actor_state', NewState),
    beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
unwrap_dispatch_result({error, Reason, NewState}) ->
    put('$bt_actor_state', NewState),
    beamtalk_exception_handler:reraise(Reason).

-doc """
Restore pdict entries after a dispatch completes.
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
Check call stack for transitive cycles (Layer 2).

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
                    beamtalk_exception_handler:reraise(Error1)
            end
    end.

-doc """
Raise a structured actor_dead error as an Erlang exception.

Used by sync_send/3 to signal that the actor process is not available.
The caller will see a beamtalk_error{kind = actor_dead} exception.
""".
-spec raise_actor_dead(atom()) -> no_return().
raise_actor_dead(Selector) ->
    beamtalk_exception_handler:reraise(actor_dead_error_record(Selector)).

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
    beamtalk_exception_handler:reraise(Error).

-doc "Construct a structured actor_dead error record for the given selector.".
-spec actor_dead_error_record(atom()) -> #beamtalk_error{}.
actor_dead_error_record(Selector) ->
    beamtalk_error:new(
        actor_dead,
        unknown,
        Selector,
        <<"Use 'isAlive' to check, or use monitors for lifecycle events">>
    ).

-doc """
Raise a structured node_down error as an Erlang exception (ADR 0126 §7.1).

Used by sync_send_remote/3 and sync_send/4 when gen_server:call exits with
`{nodedown, Node}` (partitioned or never-connected node), and by
spawn_future_watcher/3 for the equivalent monitor DOWN reason. Distinct
from actor_dead: the node is unreachable, but the actor may be alive and
well on the far side once the partition heals — retry logic needs to be
able to tell the two apart.
""".
-spec raise_node_down(node(), atom()) -> no_return().
raise_node_down(Node, Selector) ->
    beamtalk_exception_handler:reraise(node_down_error_record(Node, Selector)).

-doc """
Answer an actor's `isRemote` (ADR 0126 §2) from the `Node` its `node` send
returned: `true` iff that node is not the *caller's*. This is why `isRemote`
is intercepted in `sync_send/3,4` / `async_send/4` rather than dispatched to
the actor — inside the actor, `node()` is always the actor's own node. Going
through the `node` send (not `node(ActorPid)`) lets a `TimeoutProxy` report
its target's node (§6).

Falls back to the pid's own node when the actor cannot answer `node` with a
`Node` — a user class that overrides `node` for its own meaning (a graph
walker's current node, say), or a `native:` actor whose backing module does
not implement it — so `isRemote` never crashes on a well-formed actor.
""".
-spec is_remote(pid(), fun(() -> term())) -> boolean().
is_remote(ActorPid, AskNode) ->
    NodeName =
        try AskNode() of
            #{'$beamtalk_class' := 'Node', name := Name} when is_atom(Name) -> Name;
            _NotANode -> node(ActorPid)
        catch
            error:#beamtalk_error{kind = does_not_understand} ->
                node(ActorPid);
            error:#{error := #beamtalk_error{kind = does_not_understand}} ->
                node(ActorPid)
        end,
    NodeName =/= node().

-doc "Construct a structured node_down error record for the given node and selector.".
-spec node_down_error_record(node(), atom()) -> #beamtalk_error{}.
node_down_error_record(Node, Selector) ->
    Error = beamtalk_error:new(
        node_down,
        unknown,
        Selector,
        <<"The node may come back; retry, or use monitors to detect when it does">>
    ),
    beamtalk_error:with_details(Error, #{node => Node}).

-doc """
ADR 0126 §3: resolve a `{registered, Name, Node}` proxy's live pid.

`Node =:= node()` degrades to the ordinary local `erlang:whereis/1` the
two-tuple `{registered, Name}` clauses already use — a node-qualified ref
that happens to name the caller's own node (e.g. `named:on:` targeting the
caller's node) needs no network hop. Otherwise resolves via `erpc:call/5`,
which also surfaces a genuinely unreachable `Node` as `node_down` rather
than letting the caller misreport it as `no_such_process` (the name may well
be registered there — the node just cannot be asked).
""".
-spec resolve_remote_registered(atom(), node()) -> pid() | undefined | node_down.
resolve_remote_registered(Name, Node) when Node =:= node() ->
    erlang:whereis(Name);
resolve_remote_registered(Name, Node) ->
    try erpc:call(Node, erlang, whereis, [Name], ?BT_REMOTE_CALL_TIMEOUT) of
        Pid when is_pid(Pid) -> Pid;
        undefined -> undefined
    catch
        %% Every erpc-level failure (an unusual reason, or the remote
        %% erlang:whereis/1 itself somehow raising) collapses to the same
        %% node_down sentinel this function already returns for the common
        %% noconnection/timeout cases: this is a best-effort existence
        %% check, not a Result-returning API, so there is no richer outcome
        %% to report — "could not determine" and "node unreachable" are the
        %% same actionable fact to every call site here.
        error:{erpc, _ErpcReason} -> node_down;
        error:{exception, _Reason, _Stack} -> node_down;
        exit:{exception, _Reason} -> node_down
    end.

-doc """
ADR 0079: raise a `no_such_process` error for sends through a
name-resolving proxy when the registered name no longer points at any
process. Distinct from `actor_dead`, which fires when a held pid points
at a dead process — `no_such_process` says the *name* failed to resolve.
""".
-spec raise_no_such_process({registered, atom()} | {registered, atom(), node()}, atom()) ->
    no_return().
raise_no_such_process({registered, Name}, Selector) ->
    beamtalk_exception_handler:reraise(no_such_process_error_record(Name, Selector));
raise_no_such_process({registered, Name, _Node}, Selector) ->
    beamtalk_exception_handler:reraise(no_such_process_error_record(Name, Selector)).

-doc "Construct a structured `no_such_process` error for the given proxy ref.".
-spec no_such_process_error({registered, atom()} | {registered, atom(), node()}, atom()) ->
    #beamtalk_error{}.
no_such_process_error({registered, Name}, Selector) ->
    no_such_process_error_record(Name, Selector);
no_such_process_error({registered, Name, _Node}, Selector) ->
    no_such_process_error_record(Name, Selector).

-spec no_such_process_error_record(atom(), atom()) -> #beamtalk_error{}.
no_such_process_error_record(Name, Selector) ->
    Error = beamtalk_error:new(
        no_such_process,
        unknown,
        Selector,
        iolist_to_binary(
            io_lib:format(
                "No process is currently registered under name '~p'", [Name]
            )
        )
    ),
    beamtalk_error:with_details(Error, #{name => Name}).

-doc """
Emit lifecycle telemetry, monitor `ActorPid`, send `exit(kill)`, then wait
for the `'DOWN'` message with a 5 s timeout.

Monitoring before sending the kill signal closes the TOCTOU window: if we
returned immediately after `exit/2` (which is asynchronous), a racing
`isAlive` check could still return `true` because the BEAM scheduler had
not yet processed the signal.  Waiting for `'DOWN'` guarantees the process
is gone before this function returns.  If the process is already dead, the
`'DOWN'` message arrives instantly.

Returns `ok` on success, or `{error, Error}` if the `'DOWN'` message is not
received within 5 000 ms.  The error is a structured `#beamtalk_error{}` with
kind `timeout` and the message `"Actor kill timed out: did not receive DOWN
within 5000ms"`.  Building it here keeps the message in one place: callers
pass the error directly to `beamtalk_future:reject/2` (async path) or wrap it
with `beamtalk_exception_handler:ensure_wrapped/1` before raising (sync path).

Called by both `async_send/4` and `sync_send/3` for the `kill` selector.
""".
-spec kill_and_wait(pid()) -> ok | {error, #beamtalk_error{}}.
kill_and_wait(ActorPid) ->
    %% Emit lifecycle telemetry event for kill request.
    Class = lookup_class(ActorPid),
    maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, kill],
        #{},
        #{pid => ActorPid, class => Class}
    ),
    Ref = erlang:monitor(process, ActorPid),
    exit(ActorPid, kill),
    receive
        {'DOWN', Ref, process, ActorPid, _Reason} -> ok
    after 5000 ->
        erlang:demonitor(Ref, [flush]),
        {error,
            beamtalk_error:new(
                timeout,
                unknown,
                kill,
                <<"Actor kill timed out: did not receive DOWN within 5000ms">>
            )}
    end.

-doc """
Construct a structured `signal` error for the `delegate` selector sent to a
non-native Actor.

Used by `async_send/4`, `sync_send/3`, and the internal dispatch path that
is reached via `perform:`/`perform:withArguments:`.  The `ClassName`
argument is `unknown` at the send sites (where the class is not easily
available) and the actual class name inside the dispatch loop.
""".
-spec delegate_error(atom()) -> #beamtalk_error{}.
delegate_error(ClassName) ->
    Error = beamtalk_error:new(signal, ClassName, delegate),
    Error#beamtalk_error{message = <<"delegate called on a non-native Actor">>}.

-doc """
Wrap a function in telemetry:span/3 if telemetry is available, else run directly.
BUnit tests don't load telemetry, so we must gracefully degrade.
When tracing is enabled, generates a span_id and sets trace_id
(root span if none exists) in the process dictionary for causal linking.
""".
-spec maybe_span(list(), map(), fun(() -> {term(), map()})) -> term().
maybe_span(EventPrefix, Metadata, Fun) ->
    %% Generate causal trace IDs when tracing is enabled.
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
Emit a telemetry:execute/3 event if telemetry is available.
Used for lifecycle events (start, stop, kill) which are instantaneous —
no duration/span needed. Gracefully degrades when telemetry is not loaded.
""".
-spec maybe_execute_telemetry(list(), map(), map()) -> ok.
maybe_execute_telemetry(EventName, Measurements, Metadata) ->
    %% ADR 0093 §2: the lifecycle start/stop telemetry call is the one
    %% universal hook every actor-spawn path runs through — generated `init/1`
    %% emits `[beamtalk, actor, lifecycle, start]` (codegen) and `terminate/2`
    %% emits `[..., stop]`. Mirror those into ActorSpawned/ActorStopped system
    %% announcements here so the announce is published from every spawn/stop path
    %% without a codegen change (ADR 0069 §4: a spawn can produce both a telemetry
    %% counter and an announcement). Best-effort and fault-isolated.
    maybe_announce_lifecycle(EventName, Metadata),
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

`beamtalk_instance_registry` is populated only on the node that spawned
the actor, so this always misses for a remote pid. ADR 0126 §7.3: fall
back to a class stashed via `stash_known_class/2` before returning
'unknown', so a caller that already held the class from a
`#beamtalk_object{}` record (`beamtalk_message_dispatch`, the one call
site with it) doesn't lose it to telemetry and error breadcrumbs. Local
pids are unaffected — the registry match above already resolves them, so
the fallback is only ever consulted for a remote (or truly unknown) pid.
""".
-spec lookup_class(pid()) -> atom().
lookup_class(Pid) ->
    try ets:match(beamtalk_instance_registry, {'$1', Pid}) of
        [[Class] | _] -> Class;
        [] -> known_class_hint(Pid)
    catch
        error:badarg -> known_class_hint(Pid)
    end.

-doc """
Stash `Class` as the answer `lookup_class/1` should fall back to for
`Pid`, for the duration of a single send. No-op for a local pid — the
instance registry is already authoritative there, and stashing would only
risk masking a genuine 'unknown' (e.g. a non-Beamtalk pid). Callers
(`beamtalk_message_dispatch`) always pair this with `clear_known_class/0`
in an `after` block so the hint cannot outlive the one send it was
computed for, or leak into an unrelated pid's lookup on the same process.
""".
-spec stash_known_class(pid(), atom()) -> ok.
stash_known_class(Pid, Class) when is_pid(Pid), is_atom(Class), node(Pid) =/= node() ->
    put('$bt_known_remote_class', {Pid, Class}),
    ok;
stash_known_class(_Pid, _Class) ->
    ok.

-doc "Clear the hint set by stash_known_class/2.".
-spec clear_known_class() -> ok.
clear_known_class() ->
    erase('$bt_known_remote_class'),
    ok.

-doc """
Read back the hint set by stash_known_class/2, keyed by `Pid` so a stale
or mismatched hint (e.g. left over from a differently-shaped call on the
same process) is never applied to the wrong pid.
""".
-spec known_class_hint(pid()) -> atom().
known_class_hint(Pid) ->
    case get('$bt_known_remote_class') of
        {Pid, Class} -> Class;
        _ -> unknown
    end.

-doc """
Set application-level trace context key-value pairs.

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
Get the current application-level trace context.

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
Get the current causal trace context from the process dictionary.

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
- Application-level trace context (set via set_trace_context/1)
Future keys (request_id, deadline, causality) will be added here.

Cost: ~20ns when OTel not loaded (function_exported check + get/1 + map construction),
~50ns when OTel loaded (+ otel_ctx:get_current/0 call).
""".
-spec get_propagated_ctx() -> map().
get_propagated_ctx() ->
    Base = #{otel => get_otel_ctx(), trace_ctx => get_trace_context()},
    %% Include causal trace IDs for cross-actor linking.
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
- Application-level trace context: restores key-value pairs set via
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
    %% Restore application-level trace context if present.
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
    %% Restore causal trace context for parent-child linking.
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
    %% Layer 2: Restore call stack for transitive cycle detection.
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
Closes the TOCTOU race in async_send — if the actor dies during
message processing (after the cast but before the future is resolved),
the watcher rejects the future with a structured error. ADR 0126 §7.1:
a `noconnection` DOWN reason (the actor's node partitioned or was never
connected — verified empirically, see erlang:monitor/2 docs) rejects with
`node_down` instead of `actor_dead`, matching the sync/timeout paths'
exit mapping. The watcher also monitors the future process so it can
clean up promptly when the future completes normally. Times out after
30s as a safety net.
""".
-spec spawn_future_watcher(pid(), pid(), atom()) -> pid().
spawn_future_watcher(ActorPid, FuturePid, Selector) ->
    spawn(fun() ->
        ActorRef = erlang:monitor(process, ActorPid),
        FutureRef = erlang:monitor(process, FuturePid),
        receive
            {'DOWN', ActorRef, process, ActorPid, Reason} ->
                %% Actor died (or its node went away) — reject future
                %% (no-op if already resolved).
                Error =
                    case Reason of
                        noconnection -> node_down_error_record(node(ActorPid), Selector);
                        _ -> actor_dead_error_record(Selector)
                    end,
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
    beamtalk_logging_config:set_domain(runtime),
    %% Validate required keys
    ClassKey = beamtalk_tagged_map:class_key(),
    case maps:find(ClassKey, State) of
        {ok, Class} when is_atom(Class) ->
            case maps:is_key('__methods__', State) of
                true ->
                    %% ADR 0079: process-dict marker identifies
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
                    %% Emit lifecycle telemetry event for actor start.
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
%% Wire-tagged remote sends (ADR 0126 §5.1) — decode_wire_cast/1 recognises
%% every `'$beamtalk_wire'` cast/async shape (the one place that logic
%% lives, shared with generated compiled-actor handle_cast/2 — see its own
%% doc and decode_wire_cast/1's) and either hands back a plain local
%% message to re-dispatch through the ordinary clauses below, or has
%% already logged/rejected a decode or version failure itself (cast
%% direction, §5.2: "there is no caller to tell").
%%
%% The `kind` element is matched as the literal atom `cast`/`async` here
%% (mirroring the generated compiled-actor clause) rather than as a
%% wildcard: decode_wire_cast/1's catch-all echoes back any message it
%% doesn't recognise unchanged, so a wildcard `kind` here would let a
%% wire-shaped message of an unexpected kind (or a future wire version's
%% new kind) round-trip through decode_wire_cast/1 unchanged and re-enter
%% this same clause with the identical term — a self-tail-call BEAM's LCO
%% turns into a livelock, not a crash. Restricting `kind` here means any
%% such message instead falls through to the ordinary "unknown cast
%% message" catch-all below (log and ignore).
handle_cast({'$beamtalk_wire', _, cast, _, _, _} = Msg, State) ->
    case decode_wire_cast(Msg) of
        {redispatch, Msg2} -> handle_cast(Msg2, State);
        noreply -> {noreply, State}
    end;
handle_cast({'$beamtalk_wire', _, async, _, _, _, _} = Msg, State) ->
    case decode_wire_cast(Msg) of
        {redispatch, Msg2} -> handle_cast(Msg2, State);
        noreply -> {noreply, State}
    end;
%% Fire-and-forget cast with propagated context (ADR 0069 Phase 2b)
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
    %% Stash State for re-entrant self-sends (Layer 1)
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
%% Async send with propagated context (ADR 0069 Phase 2b)
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
    %% Stash State for re-entrant self-sends (Layer 1)
    OldState = get('$bt_actor_state'),
    put('$bt_actor_state', State),
    try
        case dispatch(Selector, Args, Self, State) of
            {reply, Result, NewState} ->
                %% Resolve the future with the result
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_resolve_future(FuturePid, Result),
                {noreply, NewState};
            {noreply, NewState} ->
                %% Method didn't return a value, resolve with nil
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_resolve_future(FuturePid, nil),
                {noreply, NewState};
            {error, Reason, NewState} ->
                %% Method failed, reject the future
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_reject_future(FuturePid, Reason),
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
%% Wire-tagged remote sends (ADR 0126 §5.1) — decode_wire_call/1 recognises
%% every `'$beamtalk_wire'` call shape (the one place that logic lives,
%% shared with generated compiled-actor handle_call/3 — see its own doc and
%% decode_wire_call/1's) and either hands back a plain local message to
%% re-dispatch through the ordinary clause below, or a ready-made reply for
%% a decode/version failure — request-direction skew (§5.2): the reply is
%% an error and this actor's state is untouched, since it never reaches
%% dispatch/4.
%%
%% The `kind` element is matched as the literal atom `call` here (mirroring
%% the generated compiled-actor clause), not as a wildcard — see the
%% matching comment on the `handle_cast/2` wire clauses above for why: a
%% wildcard `kind` would let decode_wire_call/1's catch-all echo an
%% unexpected-kind message back unchanged, re-entering this same clause
%% with the identical term and livelocking (worse here, since the caller
%% also blocks forever in `gen_server:call` waiting on a reply).
handle_call({'$beamtalk_wire', _, call, _, _, _} = Msg, From, State) ->
    case decode_wire_call(Msg) of
        {redispatch, Msg2} -> handle_call(Msg2, From, State);
        {reply, ReplyVal} -> {reply, ReplyVal, State}
    end;
%% Sync call with propagated context (ADR 0069 Phase 2b)
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
    %% Stash State in pdict so re-entrant self-sends (Layer 1)
    %% can dispatch directly without going through gen_server:call.
    OldState = get('$bt_actor_state'),
    put('$bt_actor_state', State),
    try
        case dispatch(Selector, Args, Self, State) of
            {reply, Result, NewState} ->
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, encode_reply_for(From, Selector, Result), NewState};
            {noreply, NewState} ->
                %% Method didn't return a value, return nil
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, encode_reply_for(From, Selector, nil), NewState};
            {error, Reason, NewState} ->
                %% Method failed, return error tuple
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, encode_reply_for(From, Selector, {error, Reason}), NewState}
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
Wire-encode a sync reply payload (`Result`, `nil`, or `{error, Reason}`) when
the caller (`From`'s pid) is on another node (ADR 0126 §5.1's "Result path"
table). Local replies are returned unchanged — the `node/1` comparison is
the only added cost. An encode failure never crashes the callee: it replaces
the reply with `{error, #beamtalk_error{kind = not_serialisable}}` instead;
the method's state change (the caller's `NewState`) stands regardless, since
this only substitutes the second element of the `{reply, _, NewState}`
tuple the caller already built.
""".
-spec encode_reply_for(term(), atom(), term()) -> term().
encode_reply_for(From, Selector, ReplyPayload) ->
    case encode_reply_for_tagged(From, Selector, ReplyPayload) of
        {ok, Encoded} -> Encoded;
        {error, EncErr} -> {error, EncErr}
    end.

-doc """
Like `encode_reply_for/3`, but always outer-tagged `{ok, _} | {error, _}` —
`ok` for both the remote-encoded-success case AND the local no-op-passthrough
case, `error` only for a genuine remote encode failure. `encode_reply_for/3`
(the hand-written `handle_call/3` path's own reply, never branched on by its
caller) can use the untagged passthrough safely; `handle_call_dispatch_case`
(the generated compiled-actor path, BT-3613) cannot — its success arm has to
choose the outer `{'reply', {'ok', _}, _}` vs `{'reply', {'error', _}, _}`
shape *based on* whether encoding failed, and a plain shape-match against
`encode_reply_for/3`'s untagged local-passthrough result is ambiguous: a
method can legitimately return a raw `Tuple` shaped like `{error, X}` (a
normal, first-class BeamTalk value — see `stdlib/src/tuple.bt`), which a
local call passes through unchanged and would then be indistinguishable from
a genuine encode failure. This function's own outer wrapper is added
unconditionally by this function, never by user code, so matching its outer
tag is unambiguous regardless of what `ReplyPayload` itself looks like.
""".
-spec encode_reply_for_tagged(term(), atom(), term()) -> {ok, term()} | {error, #beamtalk_error{}}.
encode_reply_for_tagged({FromPid, _Tag}, Selector, ReplyPayload) when
    is_pid(FromPid), node(FromPid) =/= node()
->
    case beamtalk_wire:encode(ReplyPayload) of
        {ok, Encoded} -> {ok, Encoded};
        {error, EncErr} -> {error, EncErr#beamtalk_error{selector = Selector}}
    end;
encode_reply_for_tagged(_From, _Selector, ReplyPayload) ->
    {ok, ReplyPayload}.

-doc """
Resolve a future with a value, wire-encoding it first when the future lives
on another node (ADR 0126 §5.1). The encoded value is tagged
`{'\$beamtalk_wire_reply', Version, Encoded}` so `beamtalk_future`'s state
machine — which also serves plain, never-encoded local resolutions — can
tell the two apart on receipt and decode only the tagged ones. An encode
failure rejects the future instead of resolving it (callee-side encode
failures never crash the callee, ADR 0126 §5.1).
""".
-spec maybe_resolve_future(pid(), term()) -> ok.
maybe_resolve_future(FuturePid, Value) when is_pid(FuturePid), node(FuturePid) =/= node() ->
    case beamtalk_wire:encode(Value) of
        {ok, Encoded} ->
            beamtalk_future:resolve(FuturePid, {'$beamtalk_wire_reply', ?BT_WIRE_VERSION, Encoded});
        {error, EncErr} ->
            beamtalk_future:reject(FuturePid, EncErr)
    end,
    ok;
maybe_resolve_future(FuturePid, Value) ->
    beamtalk_future:resolve(FuturePid, Value).

-doc "Reject a future, wire-encoding the reason first when remote — see `maybe_resolve_future/2`.".
-spec maybe_reject_future(pid(), term()) -> ok.
maybe_reject_future(FuturePid, Reason) when is_pid(FuturePid), node(FuturePid) =/= node() ->
    case beamtalk_wire:encode(Reason) of
        {ok, Encoded} ->
            beamtalk_future:reject(FuturePid, {'$beamtalk_wire_reply', ?BT_WIRE_VERSION, Encoded});
        {error, EncErr} ->
            beamtalk_future:reject(FuturePid, EncErr)
    end,
    ok;
maybe_reject_future(FuturePid, Reason) ->
    beamtalk_future:reject(FuturePid, Reason).

-doc """
Construct `#beamtalk_error{kind = wire_version_unsupported}` for a
`'\$beamtalk_wire'` envelope whose leading version this node does not
recognise (ADR 0126 §5.1).
""".
-spec wire_version_unsupported_error(atom(), term()) -> #beamtalk_error{}.
wire_version_unsupported_error(Selector, SentVersion) ->
    beamtalk_error:with_details(
        beamtalk_error:with_hint(
            beamtalk_error:new(wire_version_unsupported, unknown, Selector),
            iolist_to_binary(
                io_lib:format(
                    "wire envelope version ~p is not supported by this node (known version ~p)",
                    [SentVersion, ?BT_WIRE_VERSION]
                )
            )
        ),
        #{sent => SentVersion, known => ?BT_WIRE_VERSION, node => node()}
    ).

-doc """
Recognise and decode a `'\$beamtalk_wire'` call envelope (ADR 0126 §5.1) —
the single place this decode/version-check logic lives (CLAUDE.md "No
duplicate implementations"), called both by this module's own
`handle_call/3` wire clause and by every compiled actor's generated
`handle_call/3` (`crates/beamtalk-codegen/src/core_erlang/gen_server/
callbacks.rs`, `generate_handle_call`).

Returns `{redispatch, {Selector, Args, PropCtx}}` on a successful decode —
the caller re-dispatches that plain 3-tuple through its own ordinary local
clause, exactly the shape `handle_call/3`'s own next clause already
matches. Returns `{reply, ReplyVal}` for a decode failure or an unsupported
wire version (request-direction skew, §5.2): the caller replies `ReplyVal`
immediately with its pre-call `State` untouched — `dispatch/4` never runs.
Any other message (not a wire envelope) passes through unchanged as
`{redispatch, Msg}`, so a caller can route *every* `handle_call/3` message
through this function uniformly if it chooses to.
""".
-spec decode_wire_call(term()) -> {redispatch, term()} | {reply, term()}.
decode_wire_call({'$beamtalk_wire', ?BT_WIRE_VERSION, call, Selector, WireArgs, PropCtx}) ->
    case beamtalk_wire:decode(WireArgs) of
        {ok, Args} ->
            {redispatch, {Selector, Args, PropCtx}};
        {error, Error} ->
            {reply, {error, Error#beamtalk_error{selector = Selector}}}
    end;
decode_wire_call({'$beamtalk_wire', SentVersion, call, Selector, _WireArgs, _PropCtx}) ->
    {reply, {error, wire_version_unsupported_error(Selector, SentVersion)}};
decode_wire_call(Msg) ->
    {redispatch, Msg}.

-doc """
Log-and-drop a wire-tagged cast rejected by version skew or an unsupported
wire version (ADR 0126 §5.2, cast direction: "there is no one to tell").
""".
-spec log_wire_cast_rejected(atom(), #beamtalk_error{}) -> ok.
log_wire_cast_rejected(Selector, Error) ->
    ?LOG_WARNING("Wire-tagged cast rejected", #{
        selector => Selector,
        error => Error,
        domain => [beamtalk, runtime, dist]
    }),
    maybe_execute_telemetry([beamtalk, dist, wire_rejected], #{count => 1}, #{
        selector => Selector, error => Error
    }).

-doc """
Recognise and decode a `'\$beamtalk_wire'` cast/async envelope (ADR 0126
§5.1) — shared the same way decode_wire_call/1 is (see its doc), by this
module's own `handle_cast/2` wire clauses and every compiled actor's
generated `handle_cast/2`.

Returns `{redispatch, Msg}` with a plain local cast/async 4-tuple on a
successful decode, or `noreply` once a decode/version failure has already
been logged and dropped (cast direction, §5.2) or the pending future
rejected. Any other message (not a wire envelope) passes through unchanged
as `{redispatch, Msg}`.

The async-with-future shape (`kind = async`) is decoded here too, for a
hand-written `__methods__` actor's own `handle_cast/2` — but no compiled
`.bt` construct reaches it: a genuinely compiled actor's generated
`handle_cast/2` only ever pattern-matches the 6-element `cast`-kind wire
envelope, so a 7-element `async`-kind one sent to a compiled actor simply
never reaches this function at all and falls through the generated
callback's own catch-all to `{noreply, State}` — exactly like an
un-wire-tagged async cast to a compiled actor already does today
(`beamtalk_codegen_simulation_tests.erl`'s documented gap).
""".
-spec decode_wire_cast(term()) -> {redispatch, term()} | noreply.
decode_wire_cast(
    {'$beamtalk_wire', ?BT_WIRE_VERSION, async, Selector, WireArgs, FuturePid, PropCtx}
) ->
    case beamtalk_wire:decode(WireArgs) of
        {ok, Args} ->
            {redispatch, {Selector, Args, FuturePid, PropCtx}};
        {error, Error} ->
            beamtalk_future:reject(FuturePid, Error#beamtalk_error{selector = Selector}),
            noreply
    end;
decode_wire_cast(
    {'$beamtalk_wire', SentVersion, async, Selector, _WireArgs, FuturePid, _PropCtx}
) ->
    beamtalk_future:reject(FuturePid, wire_version_unsupported_error(Selector, SentVersion)),
    noreply;
decode_wire_cast({'$beamtalk_wire', ?BT_WIRE_VERSION, cast, Selector, WireArgs, PropCtx}) ->
    case beamtalk_wire:decode(WireArgs) of
        {ok, Args} ->
            {redispatch, {cast, Selector, Args, PropCtx}};
        {error, Error} ->
            log_wire_cast_rejected(Selector, Error),
            noreply
    end;
decode_wire_cast({'$beamtalk_wire', SentVersion, cast, Selector, _WireArgs, _PropCtx}) ->
    log_wire_cast_rejected(Selector, wire_version_unsupported_error(Selector, SentVersion)),
    noreply;
decode_wire_cast(Msg) ->
    {redispatch, Msg}.

-doc """
Handle out-of-band messages (info).
By default, unknown messages are ignored, after first checking whether
`Msg` is a non-parent `'EXIT'` this trapping actor needs to react to
(BT-3596, see `handle_linked_exit/2`).
Generated actors can override this to handle custom messages.
""".
-spec handle_info(term(), map()) -> {noreply, map()} | {stop, term(), map()}.
handle_info(Msg, State) ->
    case handle_linked_exit(Msg, State) of
        pass ->
            %% Ignore unknown info messages by default
            {noreply, State};
        Result ->
            Result
    end.

-doc """
BT-3596: Non-parent `'EXIT'` handling for an actor that traps exits.

Actors only trap exits (`erlang:process_flag(trap_exit, true)`, set in
generated `init/1`) when their class, or an ancestor, overrides
`terminate:` — see `beamtalk_codegen`'s
`class_or_ancestor_overrides_terminate`. Without trapping, a supervisor's
`exit(Pid, shutdown)` (used by both `supervisor:terminate_child/2` and a
`rest_for_one`/`one_for_all` cascade) kills the actor immediately and
`terminate/2` never runs.

Once an actor traps exits, `'EXIT'` from its OTP *parent* (the process
that `proc_lib:start_link`-started it — i.e. the ordinary
supervisor/spawner shutdown path) is intercepted by the `gen_server`
loop itself before `handle_info/2` is ever called: it already invokes
`terminate/2` and exits. So any `{'EXIT', _From, _Reason}` that reaches
`handle_info/2` is from some *other* link — typically a child the actor
itself spawned and linked — and must keep the crash-propagation
semantics an untrapped actor already has: a non-`normal` reason stops
this actor with that same reason (so its own supervisor sees it die and
applies its restart strategy, exactly as if this actor were not
trapping), while a `normal` exit is ignored, mirroring how a `normal`
exit signal is never even delivered to a non-trapping linked process.

Both the generated Server-subclass `handle_info/2` (dispatches
`handleInfo:`) and the default ignore-all `handle_info/2` above call this
first and fall through to their own behavior on `pass` — this is the one
place BT-3596's EXIT semantics are implemented, so neither codegen nor
this module duplicates the reason-branching logic.
""".
-spec handle_linked_exit(term(), map()) -> {stop, term(), map()} | {noreply, map()} | pass.
handle_linked_exit({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_linked_exit({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};
handle_linked_exit(_Msg, _State) ->
    pass.

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
    %% Emit lifecycle telemetry event for actor stop.
    %% Reason distinguishes normal shutdown from crash.
    maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, stop],
        #{},
        #{pid => self(), class => Class, reason => Reason}
    ),
    ok.

%%% Helper Functions

-doc """
Normalise an OTP terminate reason to a stable Symbol for the `ActorStopped`
event payload: `normal`/`shutdown` are clean stops, anything else is a
`crashed`. Keeps the typed `reason :: Symbol` field flat (the raw reason term is
still available in the telemetry stop event for diagnostics).
""".
-spec normalize_stop_reason(term()) -> normal | shutdown | crashed.
normalize_stop_reason(normal) -> normal;
normalize_stop_reason(shutdown) -> shutdown;
normalize_stop_reason({shutdown, _}) -> shutdown;
normalize_stop_reason(_Other) -> crashed.

-doc """
Mirror a lifecycle telemetry event into an ActorSpawned/ActorStopped system
announcement (ADR 0093 §2).

Matches only the two lifecycle telemetry event names that every actor
spawn/stop path emits — `[beamtalk, actor, lifecycle, start]` and `[..., stop]`
— and ignores all other telemetry (`kill`, dispatch spans, …). The actor class,
pid, and (for stop) terminate reason are read from the telemetry `Metadata` map.
Best-effort and fault-isolated via `do_announce_actor_lifecycle/2`.
""".
-spec maybe_announce_lifecycle(list(), map()) -> ok.
maybe_announce_lifecycle([beamtalk, actor, lifecycle, start], Metadata) ->
    Class = maps:get(class, Metadata, unknown),
    Pid = maps:get(pid, Metadata, self()),
    do_announce_actor_lifecycle('ActorSpawned', #{actorClass => Class, pid => Pid});
maybe_announce_lifecycle([beamtalk, actor, lifecycle, stop], Metadata) ->
    Class = maps:get(class, Metadata, unknown),
    Pid = maps:get(pid, Metadata, self()),
    Reason = normalize_stop_reason(maps:get(reason, Metadata, normal)),
    do_announce_actor_lifecycle('ActorStopped', #{
        actorClass => Class, pid => Pid, reason => Reason
    });
maybe_announce_lifecycle(_EventName, _Metadata) ->
    ok.

-doc """
Publish an actor-lifecycle system event on the `SystemAnnouncer` bus.

Guarded by a `whereis` check so a spawn/stop never *starts* the bus (it is a
supervised worker brought up at boot, ADR 0093 §1; only when it is already
running is there any subscriber to deliver to), and wrapped in try/catch:
announcing is a best-effort observability side effect that must never fail or
delay actor start/teardown.
""".
-spec do_announce_actor_lifecycle(atom(), map()) -> ok.
do_announce_actor_lifecycle(EventClass, Fields) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _Pid ->
            try
                beamtalk_announcements:system_announce(EventClass, Fields)
            catch
                _:_ -> ok
            end
    end,
    ok.

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
    maybe_publish_state_change(NewState, ChangedKeys),
    ok.

-doc """
Publish a committed state change to the per-object change subscription substrate
(`beamtalk_object_watch`, ADR 0095 §5), but only when this actor is
*watched*. The opt-in keeps the common path cheap: when `ChangedKeys` is empty
nothing is published, and otherwise a single message-free `ets:member/2` read
short-circuits for the (usual) unwatched actor — only a watched actor with an
actual state change reaches `publish_change/3`.

The actor class is read **here**, from the just-committed `NewState`
(`beamtalk_tagged_map:class_of/2`, cheap), and passed through — *not* re-derived
later in the watch server. The watch server runs the publish off the actor's own
message loop, by which point the actor has finished dispatch and cleared its
`$bt_actor_state` pdict entry, so reading the class from afar there would observe
`nil` in the common case (and would deep-copy the whole pdict). Capturing it at
the source keeps `actorClass` accurate and avoids that copy.
""".
-spec maybe_publish_state_change(map(), [atom()]) -> ok.
maybe_publish_state_change(_NewState, []) ->
    ok;
maybe_publish_state_change(NewState, ChangedKeys) ->
    Self = self(),
    case beamtalk_object_watch:is_watched(Self) of
        true ->
            ActorClass = beamtalk_tagged_map:class_of(NewState, nil),
            beamtalk_object_watch:publish_change(Self, ActorClass, ChangedKeys);
        false ->
            ok
    end.

-doc """
Per-object change publish hook for **compiled** actors.

The runtime `beamtalk_actor` gen_server callbacks publish state changes via
`log_dispatch_complete/5`. Compiled actor classes generate their *own*
`handle_call/3` / `handle_cast/2` (codegen), which dispatch through
`safe_dispatch/3` and never reach `log_dispatch_complete`, so without this hook a
state write on a compiled actor was invisible to the live Inspector
(`{object_changed, …}` never fired). The generated callbacks call this after a
method commits `NewState`.

Cheap on the common path: a single message-free `is_watched/1` ETS read
short-circuits before any state diff, so an unwatched actor (the usual case)
pays only that read — the `changed_state_keys/2` diff runs only for a watched
actor.
""".
-spec notify_state_change(map(), map()) -> ok.
notify_state_change(OldState, NewState) ->
    case beamtalk_object_watch:is_watched(self()) of
        true ->
            maybe_publish_state_change(NewState, changed_state_keys(OldState, NewState));
        false ->
            ok
    end.

-doc """
Compute which user-visible state keys changed between two state maps.

Excludes internal keys (__methods__, $beamtalk_class, __class_mod__) and
`__local__`-prefixed control-flow threading temporaries: those are
codegen-internal locals packed into the state map while threading an outer local
through a desugared loop/conditional, never user-observable fields. Belt-and-braces
alongside the codegen-side `strip_local_temps/1` so a stray `__local__` key from any
leak path never surfaces as a watched-actor changed slot.
""".
-spec changed_state_keys(map(), map()) -> [atom()].
changed_state_keys(OldState, NewState) ->
    InternalKeys = ['__methods__', beamtalk_tagged_map:class_key(), '__class_mod__'],
    AllKeys = lists:usort(maps:keys(OldState) ++ maps:keys(NewState)),
    UserKeys = [K || K <- AllKeys -- InternalKeys, not is_local_temp_key(K)],
    [
        K
     || K <- UserKeys, maps:get(K, OldState, '__absent__') =/= maps:get(K, NewState, '__absent__')
    ].

-doc """
Strip codegen-internal `__local__`-prefixed control-flow threading temporaries from
an actor state map before it is persisted or its changes published.

When an actor method threads an outer local through a desugared control-flow
construct (`eachWithIndex:`, `do:separatedBy:`, conditionals, counted loops) the
local is packed into the gen_server `State` map under a `__local__<name>` key. Those
keys are a purely internal threading mechanism — they must not leak into the
persisted actor state or be mistaken for user-observable fields by the watch
notification path. Compiled actor `handle_call`/`handle_cast` callbacks call this on
the committed `NewState` before replying, so the persisted state stays clean
regardless of which codegen path packed the temporary.

Returns the map unchanged (no rebuild) when it carries no such keys — the common
case for a method that threads nothing.
""".
-spec strip_local_temps(map()) -> map().
strip_local_temps(State) ->
    case [K || K <- maps:keys(State), is_local_temp_key(K)] of
        [] -> State;
        LocalKeys -> maps:without(LocalKeys, State)
    end.

-spec is_local_temp_key(term()) -> boolean().
is_local_temp_key(Key) when is_atom(Key) ->
    case atom_to_binary(Key, utf8) of
        <<"__local__", _/binary>> -> true;
        _ -> false
    end;
is_local_temp_key(_Key) ->
    false.

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
    %% Actor-specific methods that can't be in Object:
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
            %% Non-native Actors do not have a backing Erlang module.
            %% This path is reached via perform:/perform:withArguments: which
            %% bypass the send site and re-dispatch through dispatch/4.
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            {error, delegate_error(ClassName), State};
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
                    %% pid is handled at send site, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= monitor ->
                    %% monitor is handled by actor lifecycle machinery, not in __methods__
                    {reply, true, State};
                false when CheckSelector =:= 'onExit:' ->
                    %% onExit: is handled at send site, not in __methods__
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
            %% Dynamic message send with explicit timeout.
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
                throw:Nlr:_Stacktrace when ?IS_NLR(Nlr) ->
                    %% A `^` inside a block this method ran on behalf of a
                    %% caller in another process (e.g. a block invoked here via
                    %% an actor-to-actor send). The matching catch frame lives
                    %% in that caller's process, not ours, so relay the NLR
                    %% tuple as our reply instead of letting the catch-all
                    %% below misclassify it as a runtime_error — mirrors
                    %% beamtalk_class_dispatch.erl's class-method-hop relay
                    %% (ADR 0110), extended to instance actor dispatch (BT-3582).
                    {error, Nlr, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, Args, State, Class, Reason, Stacktrace)
            end;
        {ok, Fun} when is_function(Fun, 2) ->
            %% Old-style method: Fun(Args, State) - for backward compatibility
            try
                Fun(Args, State)
            catch
                error:#beamtalk_error{} = BtError:_Stacktrace ->
                    %% Preserve structured beamtalk errors from method implementations
                    {error, BtError, State};
                throw:Nlr:_Stacktrace when ?IS_NLR(Nlr) ->
                    %% See the arity-4 clause above — same relay, old-style methods.
                    {error, Nlr, State};
                Class:Reason:Stacktrace ->
                    wrap_method_error(Selector, Args, State, Class, Reason, Stacktrace)
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

-doc """
Wrap method dispatch exceptions as type_error with source exception details.

ADR 0126 §5.5 / BT-3579 Phase 0.5 finding (b) is checked first: a block
invoked during dispatch whose defining module is stale or missing on this
node raises `badfun`/`undef`, mapped to `remote_code_mismatch` instead of
the generic classifier's `runtime_error` — see `maybe_remote_code_mismatch/3`
for the asymmetric identification the spike found necessary.
""".
-spec wrap_method_error(atom(), list(), map(), term(), term(), list()) -> {error, term(), map()}.
wrap_method_error(Selector, Args, State, Class, Reason, Stacktrace) ->
    case maybe_remote_code_mismatch(Args, Class, Reason) of
        {ok, Error} ->
            ?LOG_ERROR("Remote code mismatch invoking block", #{
                selector => Selector,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            {error, Error, State};
        error ->
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Message = format_method_error_message(ClassName, Selector, Class, Reason, Stacktrace),
            ?LOG_ERROR("Error in method", #{
                selector => Selector,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace,
                domain => [beamtalk, runtime]
            }),
            Error0 = beamtalk_error:new(method_error_kind(Class, Reason), ClassName, Selector),
            Error1 = beamtalk_error:with_message(Error0, Message),
            Error = beamtalk_error:with_details(Error1, #{
                original_class => Class,
                original_reason => Reason,
                erlang_stacktrace => Stacktrace
            }),
            {error, Error, State}
    end.

-doc """
ADR 0126 §5.5 / BT-3579 Phase 0.5 finding (b): the two reasons a stale block
raises are not symmetric. `{badfun, Fun}` carries the fun value itself, so
`erlang:fun_info/2` on the *reason* identifies the module directly. The bare
`undef` atom carries nothing to identify with — the fallback is the first
function-valued term in the dispatched method's own `Args`, the block
argument the dispatch layer was about to invoke, which it still holds.
Returns `error` (not `remote_code_mismatch`) for every other reason,
including a bare `undef`/`badfun` from a method that never touched a block
argument at all — the identification only makes sense when one is present.
""".
-spec maybe_remote_code_mismatch(list(), atom(), term()) -> {ok, #beamtalk_error{}} | error.
maybe_remote_code_mismatch(_Args, error, {badfun, Fun}) when is_function(Fun) ->
    {ok, remote_code_mismatch_error(Fun)};
maybe_remote_code_mismatch(Args, error, undef) ->
    case lists:search(fun(A) -> is_function(A) end, Args) of
        {value, Fun} -> {ok, remote_code_mismatch_error(Fun)};
        false -> error
    end;
maybe_remote_code_mismatch(_Args, _Class, _Reason) ->
    error.

-doc """
Reclassify a genuinely compiled actor's raw `safe_dispatch/3` dispatch
error (`generate_safe_dispatch`'s catch, `{Type, Reason, Stacktrace}`) as
`remote_code_mismatch` when it is exactly the badfun/undef-invoking-a-
stale-block shape `maybe_remote_code_mismatch/3` already identifies for the
hand-written `__methods__` dispatch path (`wrap_method_error/6`, ADR 0126
§5.5) — reuses that one check rather than re-deriving the same badfun/undef
identification a second time for generated dispatch (CLAUDE.md "No
duplicate implementations"). `Args` is the dispatched method's own argument
list, exactly as `wrap_method_error/6` receives it.

Every error shape compiled dispatch's catch can otherwise produce — any
`{Type, Reason, Stacktrace}` triple that isn't this one case, an NLR relay
tuple, or an already-wrapped `#beamtalk_error{}` from a method that raised
one directly — passes through completely unchanged: this only ever
substitutes the one case compiled dispatch does not otherwise classify,
never compiled dispatch's other (intentionally raw, reraised-as-is) error
wrapping.
""".
-spec maybe_reclassify_compiled_dispatch_error(list(), term()) -> term().
maybe_reclassify_compiled_dispatch_error(Args, {Class, Reason, _Stacktrace} = Error) ->
    case maybe_remote_code_mismatch(Args, Class, Reason) of
        {ok, WrappedError} -> WrappedError;
        error -> Error
    end;
maybe_reclassify_compiled_dispatch_error(_Args, Error) ->
    Error.

-spec remote_code_mismatch_error(fun()) -> #beamtalk_error{}.
remote_code_mismatch_error(Fun) ->
    Module =
        case erlang:fun_info(Fun, module) of
            {module, M} -> M;
            _ -> unknown
        end,
    beamtalk_error:with_details(
        beamtalk_error:with_hint(
            beamtalk_error:new(remote_code_mismatch, 'Block'),
            <<"the block's defining class is not loaded at the same version on this node">>
        ),
        #{module => Module, node => node()}
    ).

%% Classify the kind for a raw method-dispatch failure on the runtime-only
%% (`__methods__`) path, mirroring the compiled path: error-class
%% reasons run through the shared classifier so a raw error surfaces as the same
%% kind whether the method was compiled or runtime-defined. exit/throw stay
%% runtime_error here. The richer MFA message is kept regardless.
-spec method_error_kind(atom(), term()) -> atom().
method_error_kind(error, Reason) -> beamtalk_exception_handler:classify_kind(Reason);
method_error_kind(_Class, _Reason) -> runtime_error.

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
    Error0 = beamtalk_error:new(
        method_error_kind(Class, Reason), ClassName, 'doesNotUnderstand:args:'
    ),
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
%%% Named Actor Registration (ADR 0079)
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
    %% Beamtalk `init/1` expects a state map (possibly empty) — matching the
    %% shape used everywhere else in the runtime (`doSpawnAs/2` passes `#{}`,
    %% the `spawn`/`spawnWith:` callers pass state maps). Defaulting to the
    %% empty list here would crash supervised named starts in `init/1` with
    %% `{badmap, []}`. Keep the default aligned with the rest of the runtime.
    'spawnAs'(Name, Module, #{}).

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
%%% Beamtalk stdlib FFI shims (ADR 0079)
%%%
%%% These functions back `stdlib/src/actor.bt`'s `spawn`/`spawnWith:` and
%%% named-registration API via `(Erlang beamtalk_actor)` FFI calls. They take
%%% the class object (or actor instance) as the first argument.
%%%
%%% `doSpawn/1` / `doSpawnWith/2` raise on failure, matching `spawn`'s
%%% declared `-> Self` return type (no `Result` wrapping). The named variants
%%% (`doSpawnAs/2`, `doSpawnWith/3`) translate runtime `{ok, _} | {error, _}`
%%% tuples into the shape ADR 0076 auto-converts to `Result` at the FFI
%%% boundary, matching their declared `Result(Self, Error)` return type.
%%% ============================================================================

-doc """
FFI shim for `class sealed spawn -> Self`.

`Self` is the class object `{beamtalk_object, 'ClassName class', Module, ClassPid}`.
Reuses `beamtalk_class_instantiation:class_self_spawn/4` — the same helper
the compiler's `self spawn` intrinsic (`try_instantiation_intrinsic` /
ADR 0109 amendment) calls — so behaviour (safe_spawn's trap_exit +
`initialize` sync, hot-reload instance registration via the compiled
`Module:spawn/0`, abstract-class rejection, `#beamtalk_object{}` wrapping) is
byte-for-byte identical to the per-class `spawn/0` export. Unlike
`doSpawnAs/2` / `doSpawnWith/3`, this raises on failure rather than
returning `{error, _}` — `spawn`'s declared return type is `Self`, not
`Result(Self, Error)`.
""".
-spec doSpawn(#beamtalk_object{}) -> #beamtalk_object{}.
doSpawn(Self) ->
    do_spawn_or_raise(Self, [], spawn).

-doc """
FFI shim for `class sealed spawnWith: initArgs -> Self`.

Same contract as `doSpawn/1` plus the initialisation arguments passed to
the actor's `init/1` callback.
""".
-spec doSpawnWith(#beamtalk_object{}, term()) -> #beamtalk_object{}.
doSpawnWith(Self, InitArgs) ->
    do_spawn_or_raise(Self, [InitArgs], 'spawnWith:').

%% Shared implementation for doSpawn/1 and doSpawnWith/2. Resolves ClassName/
%% Module from Self, resolves the abstract-class flag by name (the
%% metadata-lookup pattern — no process-dictionary dependency, safe to call
%% from any process), then delegates to `class_self_spawn/4`, which raises a
%% structured `instantiation_error` on failure.
-spec do_spawn_or_raise(#beamtalk_object{}, list(), atom()) -> #beamtalk_object{}.
do_spawn_or_raise(Self, Args, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            IsAbstract = beamtalk_class_instantiation:resolve_is_abstract_or_raise(
                ClassName, Selector
            ),
            beamtalk_class_instantiation:class_self_spawn(ClassName, Module, IsAbstract, Args);
        {error, #beamtalk_error{} = Err} ->
            beamtalk_error:raise(beamtalk_error:with_selector(Err, Selector))
    end.

-doc """
FFI shim for `class spawnAs: name :: Symbol -> Result(Self, Error)`.

`Self` is the class object `{beamtalk_object, 'ClassName class', Module, ClassPid}`.
Delegates to `'spawnAs'/3` with an empty args map, then wraps the returned pid
into a `#beamtalk_object{}` carrying the receiver class.
""".
-spec doSpawnAs(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAs(Self, Name) ->
    do_spawn_with_selector(Self, #{}, Name, 'spawnAs:').

-doc """
FFI shim for `class spawnWith: initArgs as: name :: Symbol -> Result(Self, Error)`.

Atomically spawns an actor registered under `Name`. Returns a tagged
`{ok, ActorObject} | {error, #beamtalk_error{}}` tuple.
""".
-spec doSpawnWith(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWith(Self, InitArgs, Name) ->
    do_spawn_with_selector(Self, InitArgs, Name, 'spawnWith:as:').

%% Shared implementation for doSpawnAs/2 and doSpawnWith/3. The `Selector`
%% parameter controls which public-facing Beamtalk selector is threaded into
%% returned structured errors so error messages match the method the user
%% called.
%%
%% `spawnAs:`/`spawnWith:as:` are `class` methods (ADR 0079 atomic naming),
%% so an EXTERNAL send (`Logger spawnAs: #foo`) reaches this function via the
%% normal class dispatch `gen_server:call` into the receiver class's own
%% singleton gen_server process (`dispatch_codegen.rs`: "a class send is a
%% gen_server:call into the singleton class process") — this function then
%% executes INSIDE that class process, which is therefore the one
%% `'spawnAs'/3`'s `gen_server:start_link` links to. `class_self_spawn_as`/
%% `do_class_self_named_spawn` (`beamtalk_class_instantiation.erl`) is a
%% SEPARATE deadlock-avoiding shortcut for the same selector reached only via
%% a same-class `self spawnAs:`/`self spawnWith:as:` send (`self`-dispatch
%% inside that class's own gen_server call handler would deadlock on a
%% second `gen_server:call` to itself) — it already unlinks, mirrored below.
%% This path did not, leaving the class's own gen_server process
%% permanently linked to every actor it spawns this way. BT-3596 made that
%% asymmetry newly dangerous: once the spawned actor traps exits (because it
%% overrides `terminate:`), an untrapped normal exit of its own class
%% process — previously inert, since a non-trapping linked partner ignores a
%% `normal` reason — now tears the actor down via `terminate/2` regardless
%% of reason, via the same built-in gen_server "parent EXIT" handling this
%% PR relies on for supervisor cascades. Sever it immediately.
%%
%% Guarded on `?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` for consistency with the
%% self-send unlink site and `safe_spawn/2`, though it can never actually be
%% set here in practice: `beamtalk_supervisor:start_child_via_class_method/4`
%% (the only place that sets it) deliberately runs a `withClassMethod:`
%% factory via `call_class_method_direct`/`erlang:apply/3` — staying in the
%% real supervisor process precisely so a plain `self spawn`/`self spawnAs:`
%% inside it links to the supervisor, not the class gen_server — and never
%% through `class_send`'s `gen_server:call`, which is the only way this
%% function is ever reached. A future caller that did reach here from a
%% context where the key is visible would still get the correct behavior.
-spec do_spawn_with_selector(#beamtalk_object{}, term(), term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_spawn_with_selector(Self, InitArgs, Name, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case 'spawnAs'(Name, Module, InitArgs) of
                {ok, Pid} ->
                    case get(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY) of
                        true -> ok;
                        _ -> unlink(Pid)
                    end,
                    {ok, #beamtalk_object{
                        class = ClassName,
                        class_mod = Module,
                        pid = Pid
                    }};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{
                        class = ClassName,
                        selector = Selector
                    }};
                {error, Reason} ->
                    {error, generic_spawn_error(ClassName, Selector, Reason)}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
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
    ClassName = Self#beamtalk_object.class,
    case proxy_pid(Self, 'registerAs:') of
        {ok, Pid} ->
            case register_name(Name, Pid) of
                {ok, _} ->
                    {ok, Self};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{
                        class = ClassName,
                        selector = 'registerAs:'
                    }}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{
                class = ClassName,
                selector = 'registerAs:'
            }}
    end;
registerAs(Self, _Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'registerAs:'),
            iolist_to_binary(
                io_lib:format(
                    "registerAs: expects an Actor receiver, got ~tp", [Self]
                )
            )
        )}.

-doc """
FFI shim for `unregister -> Symbol`.

Idempotent: returns `#ok` even if the receiver was not registered or was
already unregistered. Only raises on real failures (reserved-name, type
error) — and, for a node-qualified proxy pointing at a genuinely remote
node (ADR 0126 §3), `node_down`/`timeout` when that node cannot be reached
to find out, since the name may still be registered there.
""".
-spec unregister(#beamtalk_object{}) -> ok.
unregister(#beamtalk_object{pid = {registered, Name0, Node0}} = Self) when
    is_atom(Name0), is_atom(Node0), Node0 =/= node()
->
    %% ADR 0126 §3: a genuinely remote node-qualified proxy. `erlang:
    %% process_info/2` (`registered_name_for_pid/1`'s TOCTOU guard below,
    %% via unregister_resolved/2) only accepts *local* pids — badarg for a
    %% pid on another node — so the whole read-check-unregister sequence
    %% must run on `Node0` itself, via a plain two-tuple recursive call to
    %% this same function there, not here.
    %% Unlike the sibling remote ops (remote_spawn/4, remote_named/3,
    %% remote_all_registered/1), an unreachable node here is NOT coerced to
    %% a quiet `ok`: `unregister`'s "only raises on real failures" contract
    %% means idempotent success is reserved for "already unregistered",
    %% never for "couldn't find out" — the actor may still be registered on
    %% Node0. Raise the same node_down/timeout error the siblings surface.
    try
        erpc:call(
            Node0,
            ?MODULE,
            unregister,
            [Self#beamtalk_object{pid = {registered, Name0}}],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        ok -> ok
    catch
        error:{erpc, noconnection} ->
            beamtalk_error:raise(
                (node_down_error_record(Node0, unregister))#beamtalk_error{
                    class = Self#beamtalk_object.class
                }
            );
        error:{erpc, timeout} ->
            beamtalk_error:raise(
                (remote_timeout_error_record(Self#beamtalk_object.class, unregister, true))
            );
        error:{erpc, ErpcReason} ->
            beamtalk_error:raise(
                generic_remote_error(Self#beamtalk_object.class, unregister, {erpc, ErpcReason})
            );
        error:{exception, Reason, _Stack} ->
            beamtalk_error:raise(remote_unregister_error(Self#beamtalk_object.class, Reason));
        exit:{exception, Reason} ->
            beamtalk_error:raise(remote_unregister_error(Self#beamtalk_object.class, Reason))
    end;
unregister(Self) when is_record(Self, beamtalk_object) ->
    %% ADR 0079: name-resolving proxies (`pid = {registered, N}`, including
    %% a node-qualified ref whose Node is *this* node) derive the pid via
    %% `whereis/1`. If the name has gone, treat as idempotent — there is
    %% nothing to unregister.
    Pid =
        case Self#beamtalk_object.pid of
            P when is_pid(P) ->
                P;
            {registered, Name0} when is_atom(Name0) ->
                case erlang:whereis(Name0) of
                    undefined -> dead;
                    Live when is_pid(Live) -> Live
                end;
            {registered, Name0, Node0} when is_atom(Name0), Node0 =:= node() ->
                case erlang:whereis(Name0) of
                    undefined -> dead;
                    Live when is_pid(Live) -> Live
                end
        end,
    case Pid of
        dead ->
            ok;
        _ ->
            unregister_resolved(Self, Pid)
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

-spec unregister_resolved(#beamtalk_object{}, pid()) -> ok.
unregister_resolved(Self, Pid) ->
    case registered_name_for_pid(Pid) of
        nil ->
            ok;
        Name ->
            %% TOCTOU guard: between `registered_name_for_pid/1` above and
            %% `unregister_name/1` below, another process can claim the freed
            %% name. Only unregister when the registry still points at our
            %% pid; otherwise treat as idempotent (someone else beat us to it,
            %% possibly with a replacement actor of the same name).
            case erlang:whereis(Name) of
                Pid ->
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
                    end;
                _Other ->
                    %% Name is gone or held by another pid — nothing to do.
                    ok
            end
    end.

-doc """
FFI shim for `registeredName -> Symbol | Nil`.

Returns the atom the actor is registered under, or `nil` when the actor has no
registered name (or has terminated).
""".
-spec registeredName(#beamtalk_object{}) -> atom() | nil.
registeredName(Self) when is_record(Self, beamtalk_object) ->
    case Self#beamtalk_object.pid of
        {registered, Name} when is_atom(Name) ->
            %% ADR 0079: name-resolving proxy answers from the
            %% identity slot directly — survives the registered actor
            %% being restarted under the same name.
            Name;
        {registered, Name, _Node} when is_atom(Name) ->
            %% ADR 0126 §3: node-qualified proxy — same, answers from the
            %% identity slot (the Symbol is node-independent).
            Name;
        Pid when is_pid(Pid) ->
            registered_name_for_pid(Pid)
    end;
registeredName(_) ->
    nil.

-doc """
FFI shim for `isRegistered -> Boolean`.
""".
-spec isRegistered(#beamtalk_object{}) -> boolean().
isRegistered(Self) when is_record(Self, beamtalk_object) ->
    case Self#beamtalk_object.pid of
        {registered, _Name} ->
            %% ADR 0079: a name-resolving proxy is by
            %% construction registered. The proxy stays "registered"
            %% across restarts because the supervisor re-registers
            %% the name on each restart.
            true;
        {registered, _Name, _Node} ->
            %% ADR 0126 §3: node-qualified proxy — same reasoning.
            true;
        Pid when is_pid(Pid) ->
            registered_name_for_pid(Pid) =/= nil
    end;
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
            named_lookup(ReceiverClass, Name, 'named:');
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
Shared implementation behind `named/2` (local `named:`) and
`remote_named_target/2` (`named:on:`, ADR 0126 §3, BT-3599) — the actual
by-name lookup + class check, parameterised on `ReceiverClass` (rather than
a `#beamtalk_object{}` self to resolve it from) so the remote path can pass
the class name shipped over `erpc` directly, and on `Selector` so error
records name whichever public selector the caller actually used.
""".
-spec named_lookup(atom(), atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
named_lookup(ReceiverClass, Name, Selector) ->
    case erlang:whereis(Name) of
        undefined ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(name_not_registered, ReceiverClass, Selector),
                    iolist_to_binary(
                        io_lib:format(
                            "No actor is registered under name '~ts'",
                            [Name]
                        )
                    )
                )};
        Pid when is_pid(Pid) ->
            case pid_class_name(Pid) of
                nil ->
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(wrong_class, ReceiverClass, Selector),
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
                                    %% ADR 0079: return a
                                    %% name-resolving proxy whose
                                    %% identity slot is `{registered,
                                    %% Name}`. The send-site re-
                                    %% resolves the name on every
                                    %% message, so the reference
                                    %% survives restarts. `remote_named/3`
                                    %% (ADR 0126 §3) node-qualifies this
                                    %% to `{registered, Name, Node}` for the
                                    %% `named:on:` caller.
                                    _ = Pid,
                                    {ok, #beamtalk_object{
                                        class = ActualClass,
                                        class_mod = ActualModule,
                                        pid = {registered, Name}
                                    }};
                                not_found ->
                                    {error,
                                        beamtalk_error:with_hint(
                                            beamtalk_error:new(
                                                wrong_class,
                                                ReceiverClass,
                                                Selector
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
                                    beamtalk_error:new(wrong_class, ReceiverClass, Selector),
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
    end.

-doc """
FFI shim for `class allRegistered -> List(Actor)`.

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
                        nil ->
                            false;
                        ClassName ->
                            case class_mod_for(ClassName) of
                                {ok, Module} ->
                                    %% ADR 0079: enumerate as
                                    %% name-resolving proxies so subsequent
                                    %% sends survive restarts of the
                                    %% supervised actor under that name.
                                    _ = Pid,
                                    {true, #beamtalk_object{
                                        class = ClassName,
                                        class_mod = Module,
                                        pid = {registered, Name}
                                    }};
                                not_found ->
                                    false
                            end
                    end
            end
        end,
        all_registered()
    ).

%%% ============================================================================
%%% Remote spawn and lookup (ADR 0126 §3, Phase 2, BT-3599)
%%%
%%% Backs `stdlib/src/actor.bt`'s `spawnOn:`, `spawnAs:on:`, `named:on:`, and
%%% `allRegisteredOn:` — the `on:` (argument-free) variant of every local
%%% spawn/lookup selector above. `Node`'s host policy (ADR 0126 §9 item 2) is
%%% applied via `beamtalk_node:connect_policy/2` before any `erpc` call, per
%%% that function's own doc ("later remote spawn/lookup selectors ... apply
%%% the same rule rather than re-deriving it").
%%%
%%% Every entry point below is a pair: an origin-side function (`remote_*`,
%%% called locally by the `do*On` FFI shims) that applies the host policy,
%%% makes the `erpc:call/5`, and maps `erpc`/connection failures to
%%% `#beamtalk_error{}`; and, for spawn/lookup, a `*_target` function
%%% (`-export`ed so `erpc` can reach it) that actually runs ON the target
%%% node, in the temporary process `erpc` spawns to execute the call.
%%% ============================================================================

-doc """
Origin-side entry point for `spawnOn:`/`spawnAs:on:` (ADR 0126 §3).

`ClassName` is resolved **on the target node** by `remote_spawn_target/2` —
the caller's own compiled module is never shipped (ADR 0126 §3
"Implementation shape"), only the class name, so a class loaded under a
different module name (or a different version) on the target node still
resolves there. `NameOrUndefined` is `undefined` for an anonymous `spawnOn:`
or the requested name (already reserved-name-checked, on the target node,
by `'spawnAs'/3`) for `spawnAs:on:`.

**Not idempotent.** A `timeout` here does not mean the spawn failed — the
`erpc` call may have completed on the target node after the local timeout
fired. Retrying an anonymous `spawnOn:` risks a duplicate actor; a named
`spawnAs:on:` retried after `name_registered` can be resolved with
`named:on:` instead of retried blindly (ADR 0126 §3).
""".
-spec remote_spawn(node(), atom(), atom() | undefined, atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn(Node, ClassName, NameOrUndefined, Selector) ->
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = ClassName, selector = Selector}};
        ok ->
            try
                erpc:call(
                    Node,
                    ?MODULE,
                    remote_spawn_target,
                    [ClassName, NameOrUndefined],
                    ?BT_REMOTE_CALL_TIMEOUT
                )
            of
                {ok, Pid} ->
                    {ok, Pid};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}};
                {error, Reason} ->
                    {error, generic_spawn_error(ClassName, Selector, Reason)}
            catch
                error:{erpc, noconnection} ->
                    {error, node_down_error_record(Node, Selector)};
                error:{erpc, timeout} ->
                    {error, remote_timeout_error_record(ClassName, Selector, false)};
                error:{erpc, ErpcReason} ->
                    {error, generic_spawn_error(ClassName, Selector, {erpc, ErpcReason})};
                error:{exception, Reason, _Stack} ->
                    {error, generic_spawn_error(ClassName, Selector, Reason)};
                exit:{exception, Reason} ->
                    {error, generic_spawn_error(ClassName, Selector, Reason)}
            end
    end.

-doc """
Runs ON the target node, in the `erpc` worker process `remote_spawn/4`'s
call spawns there (ADR 0126 §3).

Resolves `ClassName` to its module via `class_mod_for/1` — the *target*
node's own class metadata, matching "the class is resolved by name on the
target node" — answering `class_not_found` (an existing kind, now reachable
remotely) when it is not loaded there, exactly as the local instantiation
path already does before ever reaching a spawn primitive.

An anonymous spawn (`NameOrUndefined =:= undefined`) delegates to
`safe_spawn/2`, which (outside a supervisor context, never the case for an
`erpc` worker) uses `gen_server:start/3` — already unlinked, so no
unlink-after-start dance is needed there. A named spawn delegates to the
public `'spawnAs'/3` wrapper (reserved-name check + `safe_spawn_named/3`,
BT-3579's Phase 0.5 correction to this ADR — `safe_spawn_named/3` itself is
not `-export`ed) and **unlinks immediately** after a successful start: the
calling `erpc` worker process exits with a non-`normal` reason once this
function returns, and a still-linked actor would die right along with it
(mirrors `beamtalk_class_instantiation:do_class_self_named_spawn/6`'s own
unlink for the local class-method `self spawnAs:` path).
""".
-spec remote_spawn_target(atom(), atom() | undefined) ->
    {ok, pid()} | {error, term()}.
remote_spawn_target(ClassName, NameOrUndefined) ->
    case class_mod_for(ClassName) of
        not_found ->
            {error, beamtalk_error:new(class_not_found, ClassName)};
        {ok, Module} ->
            case NameOrUndefined of
                undefined ->
                    safe_spawn(Module, #{});
                Name when is_atom(Name) ->
                    case 'spawnAs'(Name, Module, #{}) of
                        {ok, Pid} ->
                            unlink(Pid),
                            {ok, Pid};
                        {error, _} = Err ->
                            Err
                    end
            end
    end.

-doc """
Origin-side entry point for `spawnWith:on:`/`spawnWith:as:on:` (ADR 0126
§5.1, Phase 3b, BT-3601) — `remote_spawn/4`'s sibling for the Value-carrying
spawn selectors deferred at Phase 2 (BT-3599) until the wire encoder landed.

`InitArgs` is wire-encoded before the `erpc` call — a remote spawn never
ships raw, unversioned args, exactly like every other cross-node send
(§5.1). An encode failure (e.g. `initArgs` holds a node-scoped `Ets`) is a
request-direction failure: it never reaches the network, and is returned
here in the caller, matching `wire_sync_call/5`'s send-side behaviour for
ordinary sends. Otherwise shares `remote_spawn/4`'s connect-policy check and
`erpc`/connection failure mapping verbatim.
""".
-spec remote_spawn_with(node(), atom(), atom() | undefined, term(), atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn_with(Node, ClassName, NameOrUndefined, InitArgs, Selector) ->
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = ClassName, selector = Selector}};
        ok ->
            case beamtalk_wire:encode(InitArgs) of
                {ok, WireInitArgs} ->
                    do_remote_spawn_with_erpc(
                        Node, ClassName, NameOrUndefined, WireInitArgs, Selector
                    );
                {error, #beamtalk_error{} = EncErr} ->
                    {error, EncErr#beamtalk_error{class = ClassName, selector = Selector}}
            end
    end.

-spec do_remote_spawn_with_erpc(node(), atom(), atom() | undefined, term(), atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
do_remote_spawn_with_erpc(Node, ClassName, NameOrUndefined, WireInitArgs, Selector) ->
    try
        erpc:call(
            Node,
            ?MODULE,
            remote_spawn_with_target,
            [ClassName, NameOrUndefined, WireInitArgs],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        {ok, Pid} ->
            {ok, Pid};
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{class = ClassName, selector = Selector}};
        {error, Reason} ->
            {error, generic_spawn_error(ClassName, Selector, Reason)}
    catch
        error:{erpc, noconnection} ->
            {error, node_down_error_record(Node, Selector)};
        error:{erpc, timeout} ->
            {error, remote_timeout_error_record(ClassName, Selector, false)};
        error:{erpc, ErpcReason} ->
            {error, generic_spawn_error(ClassName, Selector, {erpc, ErpcReason})};
        error:{exception, Reason, _Stack} ->
            {error, generic_spawn_error(ClassName, Selector, Reason)};
        exit:{exception, Reason} ->
            {error, generic_spawn_error(ClassName, Selector, Reason)}
    end.

-doc """
Runs ON the target node, in the `erpc` worker process `remote_spawn_with/5`'s
call spawns there (ADR 0126 §5.1) — decodes `WireInitArgs` (this node's own
class registry resolves any Value envelope or class ref it carries), then
otherwise mirrors `remote_spawn_target/2` exactly, including the named-spawn
unlink.
""".
-spec remote_spawn_with_target(atom(), atom() | undefined, term()) ->
    {ok, pid()} | {error, term()}.
remote_spawn_with_target(ClassName, NameOrUndefined, WireInitArgs) ->
    case beamtalk_wire:decode(WireInitArgs) of
        {ok, InitArgs} ->
            case class_mod_for(ClassName) of
                not_found ->
                    {error, beamtalk_error:new(class_not_found, ClassName)};
                {ok, Module} ->
                    case NameOrUndefined of
                        undefined ->
                            safe_spawn(Module, InitArgs);
                        Name when is_atom(Name) ->
                            case 'spawnAs'(Name, Module, InitArgs) of
                                {ok, Pid} ->
                                    unlink(Pid),
                                    {ok, Pid};
                                {error, _} = Err ->
                                    Err
                            end
                    end
            end;
        {error, #beamtalk_error{}} = Err ->
            Err
    end.

-doc """
Origin-side entry point for `named:on:` (ADR 0126 §3).

Runs the actual lookup (`named_lookup/3`) on the target node via `erpc` —
the `'$beamtalk_actor'` class-marker read it needs is local-only
(`process_info/2`) — then node-qualifies the returned proxy's identity slot
to `{registered, Name, Node}` (`qualify_registered_ref/2`) so it keeps
resolving against `Node`, its origin, rather than whichever node next reads
it (ADR 0126 §3 "Registered refs are node-qualified on the wire").
""".
-spec remote_named(node(), atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
remote_named(Node, ReceiverClass, Name) ->
    Selector = 'named:on:',
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = ReceiverClass, selector = Selector}};
        ok ->
            try
                erpc:call(
                    Node,
                    ?MODULE,
                    remote_named_target,
                    [ReceiverClass, Name],
                    ?BT_REMOTE_CALL_TIMEOUT
                )
            of
                {ok, Obj} ->
                    {ok, qualify_registered_ref(Obj, Node)};
                {error, #beamtalk_error{}} = Err ->
                    Err
            catch
                error:{erpc, noconnection} ->
                    {error, node_down_error_record(Node, Selector)};
                error:{erpc, timeout} ->
                    {error, remote_timeout_error_record(ReceiverClass, Selector, true)};
                error:{erpc, ErpcReason} ->
                    {error, generic_remote_error(ReceiverClass, Selector, {erpc, ErpcReason})};
                error:{exception, Reason, _Stack} ->
                    {error, generic_remote_error(ReceiverClass, Selector, Reason)};
                exit:{exception, Reason} ->
                    {error, generic_remote_error(ReceiverClass, Selector, Reason)}
            end
    end.

-doc """
Runs ON the target node via `erpc` (`named:on:`, ADR 0126 §3) — thin wrapper
around `named_lookup/3` (shared with the local `named/2`) so the class check
(`'$beamtalk_actor'` marker via `process_info/2`) runs where the actor
actually lives.
""".
-spec remote_named_target(atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
remote_named_target(ReceiverClass, Name) ->
    named_lookup(ReceiverClass, Name, 'named:on:').

-doc """
Origin-side entry point for `allRegisteredOn:` (ADR 0126 §3).

Reuses the public `allRegistered/1` FFI shim as the `erpc` target directly
(its `Self` argument is already ignored — `allRegistered(_Self) -> ...` —
so any placeholder value works), then node-qualifies every returned proxy
the same way `remote_named/3` does, for the same reason.
""".
-spec remote_all_registered(node()) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
remote_all_registered(Node) ->
    Selector = 'allRegisteredOn:',
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = 'Actor', selector = Selector}};
        ok ->
            try erpc:call(Node, ?MODULE, allRegistered, [undefined], ?BT_REMOTE_CALL_TIMEOUT) of
                List when is_list(List) ->
                    {ok, [qualify_registered_ref(Obj, Node) || Obj <- List]}
            catch
                error:{erpc, noconnection} ->
                    {error, node_down_error_record(Node, Selector)};
                error:{erpc, timeout} ->
                    {error, remote_timeout_error_record('Actor', Selector, true)};
                error:{erpc, ErpcReason} ->
                    {error, generic_remote_error('Actor', Selector, {erpc, ErpcReason})};
                error:{exception, Reason, _Stack} ->
                    {error, generic_remote_error('Actor', Selector, Reason)};
                exit:{exception, Reason} ->
                    {error, generic_remote_error('Actor', Selector, Reason)}
            end
    end.

-doc """
Node-qualify a `#beamtalk_object{}` proxy returned by a remote lookup
(`remote_named/3`, `remote_all_registered/1`): rewrites a local
`{registered, Name}` identity slot to `{registered, Name, Node}` so it keeps
resolving against `Node` — its origin — wherever it is read next (ADR 0126
§3). A pid identity (never produced by `named/2`/`allRegistered/1` today,
but harmless if it ever were) passes through unchanged — a pid already
carries its node.
""".
-spec qualify_registered_ref(#beamtalk_object{}, node()) -> #beamtalk_object{}.
qualify_registered_ref(#beamtalk_object{pid = {registered, Name}} = Obj, Node) when is_atom(Name) ->
    Obj#beamtalk_object{pid = {registered, Name, Node}};
qualify_registered_ref(Obj, _Node) ->
    Obj.

-doc """
Construct a structured `timeout` error for a remote spawn/lookup/list/
unregister op (ADR 0126 §3). `Idempotent` picks the hint: `spawnOn:`/
`spawnAs:on:` are genuinely non-idempotent (a timeout may mean the far side
already spawned), while `named:on:`/`allRegisteredOn:` (read-only lookups)
and `unregister` (already idempotent locally) are always safe to retry —
the generic "not guaranteed to be safe" wording would be actively
misleading for those.
""".
-spec remote_timeout_error_record(atom(), atom(), boolean()) -> #beamtalk_error{}.
remote_timeout_error_record(ClassName, Selector, false) ->
    beamtalk_error:new(
        timeout,
        ClassName,
        Selector,
        <<
            "Remote operation did not complete within the timeout; the target node may or "
            "may not have applied it; a retry is not guaranteed to be safe (see the ADR "
            "0126 idempotency note)"
        >>
    );
remote_timeout_error_record(ClassName, Selector, true) ->
    beamtalk_error:new(
        timeout,
        ClassName,
        Selector,
        <<"Remote operation did not complete within the timeout; safe to retry">>
    ).

-doc "Resolve a `node :: Node` FFI argument to its underlying node atom.".
-spec node_arg_to_atom(term()) -> {ok, node()} | {error, #beamtalk_error{}}.
node_arg_to_atom(#{'$beamtalk_class' := 'Node', name := Name}) when is_atom(Name) ->
    {ok, Name};
node_arg_to_atom(Other) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor'),
            iolist_to_binary(io_lib:format("Expected a Node value, got ~tp", [Other]))
        )}.

-doc """
FFI shim for `class spawnOn: node :: Node -> Result(Self, Error)` (ADR 0126
§3, Phase 2, BT-3599).

Anonymous remote spawn: same contract as `spawn`, but the actor process
starts on `node` instead of here. **Not idempotent** — see `remote_spawn/4`'s
doc. **No links, no local tracking**: the spawned actor is never linked to
this caller (a link across a node boundary would turn a partition into a
crash here) and this workspace does not track it — `ActorSpawned` fires on
`node`, not here, so the actor outlives this session unless something else
stops it.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnOn: worker) unwrap
c node        // => Node(worker@localhost)
```
""".
-spec doSpawnOn(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnOn(Self, NodeArg) ->
    do_remote_spawn(Self, undefined, NodeArg, 'spawnOn:').

-doc """
FFI shim for `class spawnAs: name :: Symbol on: node :: Node ->
Result(Self, Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `spawnAs:`, plus the target `node`: the reserved-name check
and the atomic name registration both run on `node`, through the same
public `'spawnAs'/3` the local selector uses. **Not idempotent** — a timeout
after the far-side spawn succeeded leaves an unowned, named actor running;
retry with `named:on:` instead of calling `spawnAs:on:` again (see
`remote_spawn/4`'s doc). **No links, no local tracking** — see `spawnOn:`.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Counter spawnAs: #hits on: worker) unwrap
hits := (Counter named: #hits on: worker) unwrap
```
""".
-spec doSpawnAsOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAsOn(Self, Name, NodeArg) when is_atom(Name) ->
    do_remote_spawn(Self, Name, NodeArg, 'spawnAs:on:');
doSpawnAsOn(Self, Name, _NodeArg) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, _Module} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, ClassName, 'spawnAs:on:'),
                    iolist_to_binary(
                        io_lib:format("spawnAs:on: expects a Symbol name, got ~tp", [Name])
                    )
                )};
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'spawnAs:on:')}
    end.

%% Shared implementation for doSpawnOn/2 and doSpawnAsOn/3.
-spec do_remote_spawn(#beamtalk_object{}, atom() | undefined, term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_remote_spawn(Self, NameOrUndefined, NodeArg, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    case remote_spawn(Node, ClassName, NameOrUndefined, Selector) of
                        {ok, Pid} ->
                            {ok, #beamtalk_object{class = ClassName, class_mod = Module, pid = Pid}};
                        {error, #beamtalk_error{} = Err} ->
                            {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
                    end;
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
    end.

-doc """
FFI shim for `class spawnWith: initArgs :: Object on: node :: Node ->
Result(Self, Error)` (ADR 0126 §5.1, Phase 3b, BT-3601).

Same contract as `spawnOn:`, plus the initialisation arguments passed to the
actor's `init/1` callback on `node` — `initArgs` is wire-encoded before it
crosses the node boundary (see `remote_spawn_with/5`'s doc). **Not
idempotent**, **no links, no local tracking** — see `spawnOn:`.

Error cases: as `spawnOn:`, plus `not_serialisable` (`initArgs` holds a
node-scoped handle).

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnWith: #{#count => 10} on: worker) unwrap
```
""".
-spec doSpawnWithOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWithOn(Self, InitArgs, NodeArg) ->
    do_remote_spawn_with(Self, InitArgs, undefined, NodeArg, 'spawnWith:on:').

-doc """
FFI shim for `class spawnWith: initArgs :: Object as: name :: Symbol on:
node :: Node -> Result(Self, Error)` (ADR 0126 §5.1, Phase 3b, BT-3601).

Same contract as `spawnAs:on:`, plus the initialisation arguments — see
`spawnWith:on:` for the wire-encoding note.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Counter spawnWith: #{#count => 10} as: #hits on: worker) unwrap
```
""".
-spec doSpawnWithAsOn(#beamtalk_object{}, term(), term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWithAsOn(Self, InitArgs, Name, NodeArg) when is_atom(Name) ->
    do_remote_spawn_with(Self, InitArgs, Name, NodeArg, 'spawnWith:as:on:');
doSpawnWithAsOn(Self, _InitArgs, Name, _NodeArg) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, _Module} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, ClassName, 'spawnWith:as:on:'),
                    iolist_to_binary(
                        io_lib:format("spawnWith:as:on: expects a Symbol name, got ~tp", [Name])
                    )
                )};
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'spawnWith:as:on:')}
    end.

%% Shared implementation for doSpawnWithOn/3 and doSpawnWithAsOn/4.
-spec do_remote_spawn_with(#beamtalk_object{}, term(), atom() | undefined, term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_remote_spawn_with(Self, InitArgs, NameOrUndefined, NodeArg, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    case remote_spawn_with(Node, ClassName, NameOrUndefined, InitArgs, Selector) of
                        {ok, Pid} ->
                            {ok, #beamtalk_object{class = ClassName, class_mod = Module, pid = Pid}};
                        {error, #beamtalk_error{} = Err} ->
                            {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
                    end;
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
    end.

-doc """
FFI shim for `class named: name :: Symbol on: node :: Node ->
Result(Self, Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `named:`, run against `node`'s registry instead of this
node's. Returns a proxy whose identity slot is node-qualified
(`{registered, name, node}`) — see `remote_named/3`'s doc.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
hits := (Counter named: #hits on: worker) unwrap
hits increment   // => 1 — sent to worker, not resolved against this node
```
""".
-spec doNamedOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doNamedOn(Self, Name, NodeArg) when is_atom(Name) ->
    case class_self_to_name_and_module(Self) of
        {ok, ReceiverClass, _Module} ->
            case node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    remote_named(Node, ReceiverClass, Name);
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ReceiverClass, selector = 'named:on:'}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'named:on:')}
    end;
doNamedOn(_Self, Name, _NodeArg) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'named:on:'),
            iolist_to_binary(
                io_lib:format("named:on: expects a Symbol, got ~tp", [Name])
            )
        )}.

-doc """
FFI shim for `class allRegisteredOn: node :: Node -> Result(List(Actor),
Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `allRegistered`, listing `node`'s registered actors instead
of this node's, wrapped in a `Result` (unlike the local, network-free
`allRegistered`, ADR 0060: reaching another node is an expected-failure
operation). Returns node-qualified proxies — see `remote_all_registered/1`'s
doc.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Actor allRegisteredOn: worker) unwrap  // => #(an Actor(Counter), ...)
```
""".
-spec doAllRegisteredOn(#beamtalk_object{}, term()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
doAllRegisteredOn(_Self, NodeArg) ->
    case node_arg_to_atom(NodeArg) of
        {ok, Node} ->
            remote_all_registered(Node);
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{class = 'Actor', selector = 'allRegisteredOn:'}}
    end.

%%% Internal helpers for the FFI shims above.

-spec class_self_to_name_and_module(term()) ->
    {ok, atom(), module()} | {error, #beamtalk_error{}}.
class_self_to_name_and_module(#beamtalk_object{pid = ClassPid}) when is_pid(ClassPid) ->
    try
        ClassName = beamtalk_object_class:class_name(ClassPid),
        Module = beamtalk_object_class:module_name_safe(ClassPid),
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

-doc """
ADR 0079: derive a current pid from a `#beamtalk_object{}`
identity slot. Pid identities pass through unchanged; name-resolving
proxies look up the current pid via `whereis/1`. Returns a structured
`no_such_process` error when a proxy's name is not currently registered.
""".
-spec proxy_pid(#beamtalk_object{}, atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
%% The final clause is a defensive fallback for malformed identity slots
%% constructed outside the type system (e.g. from FFI). Dialyzer proves it
%% unreachable given the record spec, but keep it to raise a structured
%% error instead of `function_clause` when the invariant is violated.
-dialyzer({no_match, proxy_pid/2}).
proxy_pid(#beamtalk_object{pid = Pid}, _Selector) when is_pid(Pid) ->
    {ok, Pid};
proxy_pid(#beamtalk_object{pid = {registered, Name}}, Selector) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, no_such_process_error_record(Name, Selector)};
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end;
proxy_pid(#beamtalk_object{pid = {registered, Name, Node}}, Selector) when
    is_atom(Name), is_atom(Node)
->
    %% ADR 0126 §3: node-qualified proxy — see resolve_remote_registered/2.
    case resolve_remote_registered(Name, Node) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            {error, no_such_process_error_record(Name, Selector)};
        node_down ->
            {error, node_down_error_record(Node, Selector)}
    end;
proxy_pid(#beamtalk_object{class = ClassName, pid = Other}, Selector) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, ClassName, Selector),
            iolist_to_binary(
                io_lib:format(
                    "proxy_pid/2 expects pid() or {registered, atom()} in "
                    "#beamtalk_object.pid, got ~tp",
                    [Other]
                )
            )
        )}.

%% The registered name of a pid, or the Beamtalk `nil` atom when unregistered
%% or dead. Canonical implementation: previously duplicated (with a
%% disagreeing `undefined` sentinel in this module and a `nil` sentinel in
%% beamtalk_process_navigation.erl's now-removed `registered_name/1`).
%% Beamtalk-facing surfaces speak Beamtalk's null concept, not raw Erlang
%% `undefined`, so `nil` is the one true sentinel; callers that need Erlang's
%% `undefined` for an internal `=/=`/`case` guard pattern-match on `nil` now
%% instead.
-spec registered_name_for_pid(pid()) -> atom() | nil.
registered_name_for_pid(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} when is_atom(Name) -> Name;
        _ -> nil
    end.

%% The Beamtalk class name for an actor pid, read from the `'$beamtalk_actor'`
%% process-dictionary marker planted by every actor's `init/1`. Canonical
%% implementation: previously duplicated as `pid_class_name/1` here
%% (sentinel `undefined`), `beamtalk_inspector:actor_class/1` and
%% `beamtalk_process_navigation:actor_class_name/1` (both sentinel `nil`).
%% `nil` wins as the single sentinel — see `registered_name_for_pid/1` above.
-spec pid_class_name(pid()) -> atom() | nil.
pid_class_name(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor', 1, Dict) of
                {'$beamtalk_actor', Class} when is_atom(Class) -> Class;
                _ -> nil
            end;
        _ ->
            nil
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
    case beamtalk_class_metadata:lookup_module(ClassName) of
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

-doc """
Catch-all for an unusual `erpc`/remote-side failure on a non-spawn remote op
(`named:on:`, `allRegisteredOn:`, remote `unregister`) — `generic_spawn_error/3`'s
`kind = instantiation_error`/"spawn failed" wording is specific to spawn and
would misdescribe a failed lookup or unregister, so those get this neutral
`runtime_error` instead (ADR 0126 §3).
""".
-spec generic_remote_error(atom(), atom(), term()) -> #beamtalk_error{}.
generic_remote_error(ClassName, Selector, Reason) ->
    beamtalk_error:with_hint(
        beamtalk_error:with_selector(
            beamtalk_error:new(runtime_error, ClassName),
            Selector
        ),
        iolist_to_binary(io_lib:format("remote operation failed: ~tp", [Reason]))
    ).

-doc """
`erpc:call/5` re-surfaces a remote `beamtalk_error:raise/1` as
`error:{exception, Reason, Stacktrace}` at the origin, where `Reason` is
`raise/1`'s own wrap: `#{'$beamtalk_class' => _, error := #beamtalk_error{}}`
(`beamtalk_exception_handler:wrap/1`). When the remote `unregister/1` call
(via `unregister_resolved/2`) raised a genuine structured error this way —
`name_registered` on a lost TOCTOU race, a reserved-name/type error, etc. —
unwrap and re-raise *that* error, with this node's own view of the class
and `unregister` selector, so the caller sees the real `kind`/`hint` instead
of a generic wrapper. Any other exception shape falls back to
`generic_remote_error/3`.
""".
-spec remote_unregister_error(atom(), term()) -> #beamtalk_error{}.
remote_unregister_error(ClassName, #{error := #beamtalk_error{} = Err}) ->
    Err#beamtalk_error{class = ClassName, selector = unregister};
remote_unregister_error(ClassName, Reason) ->
    generic_remote_error(ClassName, unregister, Reason).

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
