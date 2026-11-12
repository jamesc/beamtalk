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
| self spawnAs:/spawnWith:as: | safe_spawn_named/5, unlinked after (or stays linked, same exception) | Class method | Yes |
| withName: supervisor child | safe_spawn_named/5 → gen_server:start_link → init/1 | Supervised | Yes |
| Supervisor child (unnamed, plain spawn/spawnWith:) | start_link/1 → gen_server:start_link → init/1 | Supervised | Yes |
| withClassMethod: supervisor child | start_child_via_class_method/4 → factory's `self spawn`/`spawnWith:`/`spawnAs:`/`spawnWith:as:` → safe_spawn/2 or safe_spawn_named/5, linked | Supervised | Yes |
| dynamic_object | gen_server:start_link(?MODULE, ...) | Internal | No (by design) |

A `native:` actor's facade module (ADR 0056) has no `init/1`. Its own
`spawn/0,1` calls `BackingModule:start_link/1`. When one of the paths above
is handed the facade instead (a supervised child, `spawnAs:`),
`safe_spawn_linked/2` and `safe_spawn_named/5` start the backing module the
same way (`safe_start_native/4`, BT-4577). There is no Beamtalk
`initialize` to await.

unnamed `Module:spawn/0,1` (via `beamtalk_actor:safe_spawn/2`)
spawns unlinked in the common case — the caller (which can be a class
gen_server for dynamic dispatch, or a class method body for `self spawn`)
is never linked to the actor it creates, so killing the actor cannot take
the caller down with it. `safe_spawn_named/5` (`spawnAs:`/`spawnWith:as:`)
stays linked, because it doubles as the real OTP supervisor child MFA for
`SupervisionSpec withName:` children (ADR 0079) — that link is the
restart mechanism, not a bug. Its own `self`-send risk (a class method's
`self spawnAs:` running inside the class gen_server) is fixed by unlinking
right after the spawn succeeds — see
`beamtalk_class_instantiation:do_class_self_named_spawn/6`. A plain
(non-`withClassMethod:`) unnamed `Supervisor child` — real OTP supervision
via `beamtalk_supervisor:spec_to_otp/1`, whose start MFA calls
`beamtalk_supervisor:start_actor_child/2` → `safe_spawn_linked/2`
(BT-4033) — links to the supervisor without ever going through
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

handle_continue(Continue, State) ->
    beamtalk_actor:handle_continue(Continue, State).

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

%% BT-4090: the `ReplyTo` handle (runtime-minted, `'$beamtalk_class'`-tagged).
-type reply_to() :: #{'$beamtalk_class' := 'ReplyTo', from := term()}.

%% Default `gen_server:stop/3` timeout for `stop` / `stopWith:` (ms).
-define(STOP_TIMEOUT, 5000).

%% BT-4676: name-resolving proxy refs (ADR 0079 / 0126) accepted by the send
%% functions; resolved by resolve_proxy/1.
-type proxy_ref() :: {registered, atom()} | {registered, atom(), node()} | {global, atom()}.
-define(IS_PROXY_REF(R),
    (is_tuple(R) andalso
        ((tuple_size(R) =:= 2 andalso is_atom(element(2, R)) andalso
            (element(1, R) =:= registered orelse element(1, R) =:= global)) orelse
            (tuple_size(R) =:= 3 andalso element(1, R) =:= registered andalso
                is_atom(element(2, R)) andalso is_atom(element(3, R)))))
).

%% BT-3967: process-dictionary key holding a deferred self-stop request
%% (`self stopWith: Reason` from inside a method), consumed by
%% apply_pending_actions/1 once the current gen_server callback returns.
-define(PENDING_STOP_KEY, '$bt_pending_stop').

%% BT-4113: marker written alongside either pending-action key above, so the
%% per-callback clear_pending_actions/0 and apply_pending_actions/1 read one
%% key, not two, in the common case where a method requested neither.
-define(PENDING_MARKER_KEY, '$bt_pending').

%% BT-4090: process-dictionary key holding the `From` of the sync call being
%% dispatched (installed around dispatch by handle_call, see
%% install_call_from/1). `self deferReply` (takeDeferredCall/1) rewrites it to
%% `{deferred, From}`; uninstall_call_from/0 reports that to the success arm,
%% which then answers `{noreply, State}` instead of replying. One put/erase
%% per sync call, nothing otherwise.
-define(CALL_FROM_KEY, '$bt_call_from').

%% BT-3992: start-args key carrying the starter's `{StarterPid, AckRef}`
%% initialize-ack target (see with_init_ack/1 and await_initialize/3), and
%% the process-dictionary key the actor keeps it under between init/1 and
%% the `initialize` step of handle_continue/2.
-define(BT_INIT_ACK_KEY, '$bt_init_ack').

%% BT-3992: how long a starter waits for `initialize` to finish —
%% `?BT_INITIALIZE_TIMEOUT` lives in beamtalk.hrl (BT-4033: shared with
%% `beamtalk_supervisor`'s init-timeout hint).

%% BT-3968: the continuation-request selectors (`Actor>>continueWith:` /
%% `continueWith:withArguments:`).
-define(IS_CONTINUATION_SELECTOR(S),
    (S =:= 'continueWith:' orelse S =:= 'continueWith:withArguments:')
).

%% The `perform:` forms restricted_target/2 unwraps, and (BT-4265) every
%% selector restricted_target/2 can answer other than `none` for — the
%% continuation selectors, `deferReply`, and a `perform:` naming either.
-define(IS_PERFORM_SELECTOR(S),
    (S =:= 'perform:' orelse S =:= 'perform:withArguments:' orelse
        S =:= 'perform:withArguments:timeout:')
).
-define(IS_RESTRICTED_OR_PERFORM_SELECTOR(S),
    (?IS_CONTINUATION_SELECTOR(S) orelse S =:= deferReply orelse ?IS_PERFORM_SELECTOR(S))
).

%% BT-4265: the helpers an actor send (caller side) and a received message
%% (actor side) run every time are inlined, so a send pays for the work, not
%% for a chain of local calls to reach it.
-compile(
    {inline, [
        self_send_kind/1,
        sync_send_remote/4,
        resolve_class/2,
        get_sync_propagated_ctx/0,
        get_propagated_ctx/0,
        get_otel_ctx/0,
        check_call_stack/2,
        wire_sync_call/5,
        local_call/3,
        clear_trace_context_if_set/0
    ]}
).

%% ADR 0126 §3: timeout for the `erpc:call/5` a remote spawn/lookup/list
%% makes to the target node — matches `gen_server:call/2`'s own 5000ms
%% default so a stuck remote op fails on the same order of magnitude as a
%% stuck local one, rather than hanging indefinitely (erpc's own default).
%% `?BT_REMOTE_CALL_TIMEOUT` itself now lives in beamtalk.hrl — shared with
%% `beamtalk_node`/`beamtalk_node_monitor` (Phase 4) rather than a second
%% literal (AGENTS.md "No duplicate implementations").

%% Public API
-export([start_link/2, start_link/3, start_link_supervised/3, register_spawned/4]).
-export([await_initialize/1, await_initialize/2, await_initialize/3, safe_spawn/2]).
%% BT-4033: the awaited linked start of an anonymous supervised actor child.
-export([safe_spawn_linked/2]).
%% BT-4577: the backing module of a `native:` actor's facade, or `none`.
-export([native_backing_module/1]).
%% BT-4104: an anonymous start that leaves the caller monitoring the actor,
%% with the monitor created atomically with the process.
-export([safe_spawn_monitored/2]).
%% BT-4477: the same atomic spawn+monitor, but the caller does not wait for
%% `init/1` or `initialize`.
-export([safe_spawn_monitored_async/2]).

-type link_mode() :: linked | unlinked.
%% What a successful initialize wait does with the start monitor: `release`
%% removes it (every plain spawn), `keep` hands it to the caller
%% (safe_spawn_monitored/2, BT-4104).
-type monitor_mode() :: release | keep.

%% BT-3992: initialize acknowledgement, called from generated init/1 and
%% handle_continue/2 (and this module's own init/1).
-export([take_init_ack/2, ack_initialized/2]).

%% Message send helpers (lifecycle-aware wrappers)
-export([async_send/4, sync_send/3, sync_send/4, cast_send/3]).

%% BT-4113: the same sends for a caller that already holds the actor's class
%% (`beamtalk_message_dispatch`, from the `#beamtalk_object{}` record), so the
%% send path never has to look it up.
-export([sync_send_known/4, sync_send_known/5, cast_send_known/4]).

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
    handle_continue/2,
    code_change/3,
    terminate/2
]).

%% BT-3596: non-parent 'EXIT' interception for trapping actors — called
%% from generated handle_info/2 (both Server-subclass and plain-Actor paths)
-export([handle_linked_exit/2, handle_linked_exit/3]).

%% Internal dispatch
-export([dispatch/4, make_self/1]).

%% BT-3967: `Actor`'s quoted `@intrinsic` bodies (`actorStop`,
%% `actorStopWith`, ...) compile to `bt@stdlib@actor:dispatch/3`, which
%% delegates here. Reached only when the send was not intercepted at the
%% send site — i.e. a self-send from inside one of the actor's own methods.
-export([dispatch/3]).
-ifdef(TEST).
-export([intrinsic_selector/1, request_continuation/4]).
%% BT-4525: the self-dispatch unwrap, driven directly on a `$bt_nlr` result.
-export([unwrap_dispatch_result/1]).
-endif.

%% BT-3967 / BT-3968: deferred pending actions. Every gen_server callback
%% that runs a Beamtalk dispatch (this module's own and every compiled
%% actor's generated handle_continue/handle_call/handle_cast/handle_info)
%% passes its return value through apply_pending_actions/1, which turns it
%% into a `{stop, ...}` return when the dispatch requested `self stopWith:`,
%% or adds a `{continue, ...}` action when it requested
%% `self continueWith:`; clear_pending_actions/0 runs first so a request can
%% never leak in from anywhere else. seed_pending_continuations/1 (from
%% handle_continue) keeps an already-queued chain ahead of new requests, and
%% discard_pending_continuations/0 (from the replying dispatch-error arms)
%% drops the continuations a failed dispatch requested.
-export([
    install_call_from/1,
    uninstall_call_from/0,
    clear_pending_actions/0,
    apply_pending_actions/1,
    seed_pending_continuations/1,
    discard_pending_continuations/0
]).

%% BT-3968: the mailbox-boundary guard against another process scheduling a
%% continuation — called by this module's own handle_call/handle_cast and by
%% every compiled actor's generated ones (see mailbox_continuation_check/2).
-export([mailbox_continuation_check/2, performed_selector/2]).

%% exported so generated self-dispatch error breadcrumbs
%% (dispatch_codegen.rs's generate_self_dispatch_error_clause) can resolve
%% the actor's actual runtime class at the error-wrap boundary, matching
%% sync_call/5's cross-actor Context construction.
-export([lookup_class/1]).

%% per-object change publish hook, called from compiled actor
%% gen_server callbacks after a method commits new state.
%% strip_local_temps/1 cleans codegen-internal `__local__` threading
%% temporaries from committed state before the reply is emitted.
-export([notify_state_change/2, strip_local_temps/1]).

%% ADR 0126 §5.1/§5.5 (BT-3613): the `'$beamtalk_wire'`-envelope
%% recognition/decode helpers shared by this module's own handle_call/3 and
%% handle_cast/2 clauses AND every compiled actor's generated
%% handle_call/3 / handle_cast/2 (crates/beamtalk-codegen/.../gen_server/
%% callbacks.rs) — the one place that logic lives (AGENTS.md "No duplicate
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

%% BT-4701: the pieces of `beamtalk_actor_registry` / `_remote` / `_ffi` (split out of
%% this module) that still call back into it. Exported for the split only; they
%% were private helpers shared with the send paths.
-export([
    safe_spawn_named/5,
    node_down_error_record/2,
    no_such_process_error_record/2,
    resolve_remote_registered/2
]).

%% BT-4701: delegates kept under the old module name for at least one release —
%% `erpc` targets, the `global` resolve function (registered by `safe_spawn_named`)
%% and any supervisor child MFA recorded by an earlier release name this module,
%% so a mixed-version cluster keeps working
%% (see the moduledocs of `beamtalk_actor_registry` / `_remote` / `_ffi`).
-export([
    'spawnAs'/2,
    'spawnAs'/3,
    'spawnAsGlobal'/2,
    'spawnAsGlobal'/3,
    resolve_global_conflict/3,
    started_at_from_dictionary/1,
    pid_class_candidates/1,
    pid_class_name/1,
    remote_spawn_target/2,
    remote_spawn_with_target/3,
    remote_named_target/2,
    'allRegistered'/1,
    unregister/1
]).
-export_type([link_mode/0]).

%% Deferred-reply FFI shims for stdlib/src/actor.bt and reply_to.bt. They read
%% this module's private call-from machinery, so they stay here (BT-4701).
-export([takeDeferredCall/1, replyToDeferred/2, replyErrorToDeferred/2]).

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
            %% bt-catcher-audit: not-applicable-no-block - Erlang workspace callback, not a
            %% Beamtalk block
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
Wait for a started actor's `initialize` to finish — **sys-based fallback**.

BT-3992: Beamtalk's own spawn paths no longer use this: they pass an ack
target in the start args (with_init_ack/1) and wait with
await_initialize/3, which returns as soon as `initialize` has returned —
without waiting for any `continueWith:` continuations it requested (OTP
`handle_continue` semantics). This form is kept for a pid that was started
without an ack target, such as a hand-written gen_server, and for external
callers.

It synchronizes by sending a system message (`sys:get_state/2`), which OTP
handles only between callbacks and never while a `{continue, _}` action is
pending — so it returns only after `initialize` *and every continuation
chained from it* have run (5 s timeout over the whole chain).

Returns `ok` if the process is still alive after initialization,
or `{error, Reason}` if it crashed during handle_continue.
""".
-spec await_initialize(pid()) -> ok | {error, term()}.
await_initialize(Pid) ->
    await_initialize(Pid, erlang:monitor(process, Pid)).

-doc """
Like `await_initialize/1` (the sys-based fallback), but with a monitor the
caller established *atomically with the spawn* (`gen_server:start_monitor/3`).

BT-3677: `await_initialize/1` monitors AFTER `gen_server:start/3` returned.
`handle_continue` runs concurrently with the caller, so under scheduler load
an actor whose `initialize` fails (e.g. the post-initialize
uninitialized-state check) can already be dead before the caller reaches
`erlang:monitor/2`. A monitor on an already-dead process reports the
reason `noproc`, NOT the real stop reason, so the structured
`UninitializedStateError` (with its field/type hint) was intermittently
replaced by a bare `noproc` hint. A monitor created together with the
process always reports the true exit reason.

`MonRef` is consumed (demonitored or received) by this function.
""".
-spec await_initialize(pid(), reference()) -> ok | {error, term()}.
await_initialize(Pid, MonRef) ->
    await_initialize_sys(
        Pid, MonRef, fun(P) -> sys:get_state(P, ?BT_INITIALIZE_TIMEOUT) end, release
    ).

%% The sys-based wait behind await_initialize/2: `SysCall` is a `sys` call
%% (handled only between callbacks) whose exit shapes are sys:get_state/2's.
%% `Mode` says what success does with `MonRef` (settle_monitor/2); every
%% failure consumes it.
-spec await_initialize_sys(pid(), reference(), fun((pid()) -> term()), monitor_mode()) ->
    ok | {error, term()}.
await_initialize_sys(Pid, MonRef, SysCall, Mode) ->
    %% bt-catcher-audit: not-applicable-no-block - SysCall is a local fun
    %% wrapping a sys:get_state/replace_state call, never a Beamtalk block
    try SysCall(Pid) of
        _State ->
            settle_monitor(Mode, MonRef)
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
BT-3992: wait until a just-started actor's `initialize` has returned,
using the ack target `with_init_ack/1` put in its start args.

Startup ends when `initialize` returns (OTP semantics: `init/1` returning
`{ok, State, {continue, _}}` lets `start_link` return before
`handle_continue` runs). Continuations `initialize` requested with
`continueWith:` (and any they chain) run *after* this returns — still
before any message sent to the actor after it — so a slow first step never
blocks the starter or a supervisor, and never counts against the timeout.

Must be called after the `gen_server` start call returned `{ok, Pid}`. The
actor's `init/1` (take_init_ack/2) has then already sent, before
`proc_lib`'s start reply and so already in this mailbox:

- `{AckRef, initialized}` — no `initialize` step is pending; done.
- `{AckRef, initializing}` — `initialize` is pending in `handle_continue/2`;
  wait for `{AckRef, initialized}` (sent by ack_initialized/2 after it
  succeeded and passed the post-initialize uninitialized-state check) or
  the `'DOWN'` for `MonRef` (`{error, Reason}`), at most 5 s
  (`{error, timeout}`; the caller kills the actor).
- neither — the process never consumed the ack target (a hand-written
  gen_server, or an actor module compiled before BT-3992): fall back to the
  sys-based wait, which also strips the target from a map state.

`MonRef` is consumed (demonitored or received) by this function. A late
`{AckRef, _}` left after a timeout is the caller's to flush
(flush_init_ack/1) once the actor is dead.
""".
-spec await_initialize(pid(), reference(), reference() | none) -> ok | {error, term()}.
await_initialize(Pid, MonRef, AckRef) ->
    await_initialize(Pid, MonRef, AckRef, release).

%% await_initialize/3 with the success-side handling of `MonRef` chosen by
%% `Mode` (BT-4104): `release` demonitors it, `keep` leaves it live for the
%% caller, whose later `'DOWN'` then carries the actor's real exit reason.
%% Every failure consumes `MonRef` in both modes.
-spec await_initialize(pid(), reference(), reference() | none, monitor_mode()) ->
    ok | {error, term()}.
await_initialize(Pid, MonRef, none, Mode) ->
    await_initialize_sys(
        Pid, MonRef, fun(P) -> sys:get_state(P, ?BT_INITIALIZE_TIMEOUT) end, Mode
    );
await_initialize(Pid, MonRef, AckRef, Mode) ->
    receive
        {AckRef, initialized} ->
            settle_monitor(Mode, MonRef);
        {AckRef, initializing} ->
            receive
                {AckRef, initialized} ->
                    settle_monitor(Mode, MonRef);
                {'DOWN', MonRef, process, Pid, Reason} ->
                    {error, Reason}
            after ?BT_INITIALIZE_TIMEOUT ->
                erlang:demonitor(MonRef, [flush]),
                {error, timeout}
            end
    after 0 ->
        %% The actor never consumed the target: a hand-written gen_server, or
        %% an actor module compiled before BT-3992 whose init/1 merges its
        %% start args into its state. Wait the sys way, and strip the target
        %% from a map state so it does not stay there for good.
        await_initialize_sys(
            Pid,
            MonRef,
            fun(P) ->
                sys:replace_state(P, fun strip_init_ack_key/1, ?BT_INITIALIZE_TIMEOUT)
            end,
            Mode
        )
    end.

%% The success side of an initialize wait: `release` drops the start monitor,
%% `keep` leaves it for the caller (BT-4104).
-spec settle_monitor(monitor_mode(), reference()) -> ok.
settle_monitor(release, MonRef) ->
    erlang:demonitor(MonRef, [flush]),
    ok;
settle_monitor(keep, _MonRef) ->
    ok.

-spec strip_init_ack_key(term()) -> term().
strip_init_ack_key(State) when is_map(State) ->
    maps:remove(?BT_INIT_ACK_KEY, State);
strip_init_ack_key(State) ->
    State.

-doc """
BT-3992: add the initialize-ack target `{self(), AckRef}` to a spawn's start
args. Every Beamtalk start path (`safe_spawn/2`, `safe_spawn_linked/2`,
`safe_spawn_named/5`) goes through this and then await_initialize/3; the
actor strips the key from its state in `init/1` (take_init_ack/2). Start
args that are not a map (a hand-written gen_server's own shape) carry no
target (`none`); await_initialize/3 then takes the sys-based fallback.
""".
-spec with_init_ack(Args) -> {Args, reference() | none} when Args :: term().
with_init_ack(InitArgs) when is_map(InitArgs) ->
    AckRef = make_ref(),
    {InitArgs#{?BT_INIT_ACK_KEY => {self(), AckRef}}, AckRef};
with_init_ack(InitArgs) ->
    {InitArgs, none}.

-doc """
BT-3992: drop any initialize ack still in the mailbox for `AckRef` — after a
failed or timed-out start, once the actor is known dead (so nothing more
can arrive).
""".
-spec flush_init_ack(reference() | none) -> ok.
flush_init_ack(none) ->
    ok;
flush_init_ack(AckRef) ->
    receive
        {AckRef, _} -> flush_init_ack(AckRef)
    after 0 -> ok
    end.

-doc """
BT-3992: called by an actor's `init/1` on its initial state (generated
`init/1`, outermost call only — never as a parent state helper — and this
module's own init/1). Removes the starter's ack target
(`'$bt_init_ack'`, see with_init_ack/1) from the state, and tells the
starter where startup stands:

- `initialized` — no `initialize` step follows; startup is complete.
- `initializing` — `init/1` returns `{continue, initialize}`; the target is
  kept in the process dictionary until ack_initialized/2 sends the final
  `{AckRef, initialized}`.

The message is sent before `init/1` returns, hence before `proc_lib`'s start
reply, so the starter finds it as soon as its start call returns. A state
with no ack target (the actor was started some other way) is returned
unchanged and nothing is sent.
""".
-spec take_init_ack(map(), initializing | initialized) -> map().
take_init_ack(State, Phase) when is_map(State) ->
    case maps:take(?BT_INIT_ACK_KEY, State) of
        {{From, AckRef} = Target, State1} when is_pid(From), is_reference(AckRef) ->
            From ! {AckRef, Phase},
            case Phase of
                initializing -> put(?BT_INIT_ACK_KEY, Target);
                initialized -> ok
            end,
            State1;
        {_Malformed, State1} ->
            State1;
        error ->
            State
    end.

-doc """
BT-3992: called with the final return value of every `handle_continue/2`
step (generated actors). For the `initialize` step it sends the starter
`{AckRef, initialized}` when the step succeeded — `initialize` returned,
the post-initialize uninitialized-state check passed, and no
`self stopWith:` was requested — i.e. the result is a `{noreply, ...}`,
possibly carrying the `{continue, _}` action for the continuations
`initialize` requested. Those run after this callback returns, so the
starter is released before them. A failing step sends nothing: the actor
stops, and the starter sees its `'DOWN'`. The ack target is erased either
way. Returns `Result` unchanged.
""".
-spec ack_initialized(term(), Result) -> Result when Result :: term().
ack_initialized(initialize, Result) ->
    case erase(?BT_INIT_ACK_KEY) of
        {From, AckRef} when
            is_tuple(Result),
            element(1, Result) =:= noreply,
            (tuple_size(Result) =:= 2 orelse tuple_size(Result) =:= 3)
        ->
            From ! {AckRef, initialized};
        _ ->
            ok
    end,
    Result;
ack_initialized(_Continue, Result) ->
    Result.

-doc """
Spawn an actor, with initialize synchronization.

Handles the full spawn sequence:
1. Choose linked vs unlinked start (see the rationale below)
2. If start succeeds, wait until `initialize` has returned (await_initialize/3 — not for
   the continuations it requested, BT-3992)
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
`safe_spawn_named/5` already does for the named-child case.

This consolidates the await_initialize logic so generated spawn
functions stay simple.
""".
-spec safe_spawn(module(), map()) -> {ok, pid()} | {error, term()}.
safe_spawn(Module, InitArgs) ->
    case get(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY) of
        true ->
            safe_spawn_linked(Module, InitArgs);
        _ ->
            %% start_monitor (not start + monitor): the monitor must exist before
            %% the actor can possibly die, or a fast-failing initialize reports
            %% `noproc` instead of its real stop reason (BT-3677).
            case start_monitor_and_await(Module, InitArgs, release) of
                {ok, {Pid, _ReleasedRef}} -> {ok, Pid};
                {error, Reason} -> {error, Reason}
            end
    end.

-doc """
BT-4104: spawn an anonymous actor **monitored by the caller**, with
initialize synchronization. Answers `{ok, {Pid, MonRef}}` once the actor's
`initialize` has returned (not the continuations it requested, BT-3992), or
`{error, Reason}` when it failed or timed out.

`MonRef` is the monitor `gen_server:start_monitor/3` created together with
the process, so it is live from the actor's first instant: an actor that
finishes straight after `initialize` (a `continueWith:` step ending in
`self stopWith: #normal`) delivers `{'DOWN', MonRef, process, Pid, normal}`
to the caller, never the `noproc` a separate `spawn` + `monitor` reports
when the actor is already gone by the time `monitor` runs.

The monitor belongs to the process that calls this — the generated
`Module:spawn_monitored/0,1` runs in whatever process evaluates the spawn
expression. On failure the actor is killed and the monitor consumed, so the
caller's mailbox holds no `'DOWN'` for it and no initialize ack.

Never links, not even in a `withClassMethod:` supervisor factory (where
`safe_spawn/2` links): a monitored start is for a caller that observes the
actor's exit itself, which is not what a supervisor child needs.
""".
-spec safe_spawn_monitored(module(), map()) ->
    {ok, {pid(), reference()}} | {error, term()}.
safe_spawn_monitored(Module, InitArgs) ->
    start_monitor_and_await(Module, InitArgs, keep).

-doc """
BT-4477: spawn an anonymous actor **monitored by the caller** without waiting
for it to start. Answers `{ok, {Pid, MonRef}}` as soon as the process exists:
the caller does not wait for `init/1` or for `initialize` (no init-ack target
is passed, so `take_init_ack/2` and `ack_initialized/2` are no-ops).

`MonRef` is created atomically with the process (`proc_lib:spawn_opt/2` with
the `monitor` option), so an actor that stops straight away delivers
`{'DOWN', MonRef, process, Pid, Reason}` with its real reason, never `noproc`.

A failure in `init/1` or `initialize` cannot be raised to the caller, which has
already moved on: the actor exits and the failure arrives as the `'DOWN'`
reason. Messages sent to `Pid` before `initialize` finishes queue in the
mailbox and run after it.

The process is started with `proc_lib` and enters the `gen_server` loop with
`gen_server:enter_loop/5` after `Module:init/1`. It carries the same
`$initial_call` (`{Module, init, 1}`) as a `gen_server:start*` actor and is its
own OTP parent, as an unlinked `gen_server:start*` actor is, so `sys`, observer
and the crash-report secret redaction treat it like any other actor. Its
`$ancestors` has the actor itself prepended (a deliberate side effect of
making it its own parent), which shows as a duplicate in crash reports. Not linked; the monitor belongs to the process that calls this.
`init/1` results are honoured as `gen_server` does: `{ok, State}`,
`{ok, State, Timeout | hibernate | {continue, _}}`, `{stop, Reason}` (exit
with `Reason`) and `ignore` (exit `normal`).
""".
-spec safe_spawn_monitored_async(module(), map()) -> {ok, {pid(), reference()}}.
safe_spawn_monitored_async(Module, InitArgs) ->
    {Pid, MonRef} = proc_lib:spawn_opt(
        fun() -> async_init_and_loop(Module, InitArgs) end,
        [monitor]
    ),
    {ok, {Pid, MonRef}}.

%% Body of the process started by safe_spawn_monitored_async/2: run the
%% module's `init/1` here (not in the caller), then become its gen_server.
%% `gen_server:enter_loop/5` must be called from the process that ran `init/1`.
-spec async_init_and_loop(module(), term()) -> no_return().
async_init_and_loop(Module, InitArgs) ->
    %% `proc_lib:spawn_opt/2` with a fun records the fun's own MFA as the
    %% initial call. Set the one `gen_server:start*` sets, because the
    %% crash-report secret redaction (BT-4666, `beamtalk_actor_redaction`),
    %% observer and `proc_lib:initial_call/1` identify the actor by
    %% `{Module, init, _}`.
    put('$initial_call', {Module, init, 1}),
    %% An unlinked `gen_server:start*` actor is its own OTP parent
    %% (`gen:do_spawn` passes `self`), whereas `enter_loop/5` takes the head of
    %% `$ancestors` — here the spawner. Make the actor its own parent so an
    %% exit-trapping actor linked to its spawner later treats that exit as a
    %% normal link exit (BT-3596), not parent shutdown, exactly as a
    %% `spawnMonitored` actor does. The side effect is a duplicate self entry
    %% in `$ancestors` (crash reports, and inherited by `proc_lib` children);
    %% `enter_loop` has no public way to pass the parent, so this is deliberate.
    put('$ancestors', [self() | get('$ancestors')]),
    case Module:init(InitArgs) of
        {ok, State} ->
            gen_server:enter_loop(Module, [], State, self(), infinity);
        {ok, State, Action} ->
            gen_server:enter_loop(Module, [], State, self(), Action);
        {stop, Reason} ->
            exit(Reason);
        ignore ->
            exit(normal);
        Other ->
            exit({bad_return_value, Other})
    end.

%% Shared unlinked start for safe_spawn/2 and safe_spawn_monitored/2:
%% start_monitor (not start + monitor), so the monitor exists before the
%% actor can possibly die, or a fast-failing initialize reports `noproc`
%% instead of its real stop reason (BT-3677). `Mode` decides whether that
%% monitor survives a successful start (BT-4104).
-spec start_monitor_and_await(module(), term(), monitor_mode()) ->
    {ok, {pid(), reference()}} | {error, term()}.
start_monitor_and_await(Module, InitArgs, Mode) ->
    {AckArgs, AckRef} = with_init_ack(InitArgs),
    case gen_server:start_monitor(Module, AckArgs, []) of
        {ok, {Pid, MonRef}} -> await_initialize_or_kill_unlinked(Pid, MonRef, AckRef, Mode);
        {error, Reason} -> {error, Reason};
        ignore -> {error, ignore}
    end.

-doc """
Spawn an actor **linked** to the caller, with trap_exit + initialize
synchronization: answers `{ok, Pid}` once the actor's `initialize` has
returned (not the continuations it requested), or `{error, Reason}` when it
failed or timed out — never `ignore`.

The start of every anonymous supervised actor child (BT-4033): a static
`Supervisor` child's and a `DynamicSupervisor` pool child's start MFA goes
through `beamtalk_supervisor:start_actor_child/2,3`, which calls this, so a
failing `initialize` fails the supervisor's start or `startChild`, as OTP
`init/1` returning `{stop, Reason}` does. It is also the `safe_spawn/2` case
that needs the link — a `withClassMethod:` supervisor child's factory
calling plain `self spawn`/`self spawnWith:` (see `safe_spawn/2`'s doc).
Shares the trap_exit dance with `safe_spawn_named/5` via
`start_link_and_await/3`. `InitArgs` that are not a map (a hand-written
gen_server's own start args) carry no init-ack target and fall back to the
sys-based wait (`await_initialize/3`).
""".
-spec safe_spawn_linked(module(), term()) -> {ok, pid()} | {error, term()}.
safe_spawn_linked(Module, InitArgs) ->
    case native_backing_module(Module) of
        none ->
            start_link_and_await(
                fun(Args) -> gen_server:start_link(Module, Args, []) end, InitArgs, linked
            );
        Backing ->
            safe_start_native(Backing, InitArgs, none, linked)
    end.

-doc """
Spawn a named actor with trap_exit + initialize synchronization, per
`Scope` (`local`/ADR 0079, or `global`/ADR 0126 §4, Phase 5, BT-3603).

Handles the full spawn sequence:
1. Trap exits so a failed start_link or handle_continue doesn't kill caller
2. Start the actor, registered per `Scope` (see below)
3. If the start succeeds, wait until `initialize` has returned (await_initialize/3 — not for
   the continuations it requested, BT-3992)
4. Restore trap_exit and return {ok, Pid} or {error, Reason}

Unlike `safe_spawn/2` (unnamed) in its common case, this stays
**linked** — it is the shared implementation behind both
`spawnAs:`/`spawnWith:as:`/`spawnAs:scope:`/`spawnWith:as:scope:` *and*
the real OTP supervisor child MFA that `beamtalk_supervisor:spec_to_otp/1`
builds for `SupervisionSpec withName:`/`withName:scope:` children (ADR
0079: `{beamtalk_actor, spawnAs, [Name, Module]}` / `[Name, Module,
InitArgs]`, or, for `global`, `{beamtalk_actor, spawnAsGlobal, ...}` —
either way this function is the start callback). That link is not a
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

`local` keeps the original `{local, Name}` registration — a single atomic
`gen_server:start_link({local, Name}, ...)`.

`global` cannot reuse the same atomic form — OTP's `{global, Name}` start
always binds the *default* resolver (`global:random_exit_name/3`, the
force-kill behaviour ADR 0126 §4 replaces), and there is no variant that
takes a custom one. Instead this starts the gen_server **anonymously but
still linked** (`gen_server:start_link/3` — same link as the `local`
clause, since this also doubles as a `SupervisionSpec
withName:scope:#global` child's start MFA, ADR 0126 §4), then explicitly
`global:register_name/3`s the resulting pid with `resolve_global_conflict/3`
as the resolver. A losing race (`no` — another process already holds
`Name`) stops the just-started actor (`terminate/2` runs — this is an
ordinary, not a conflict, teardown) and reports `{already_started,
ExistingPid}`, the exact shape `spawn_named_scoped/4` already translates to
`name_registered` for the `local` path, so no separate error-mapping arm is
needed for `global`.

`Link` is `linked` for every caller above. `unlinked` (BT-3992,
spawn_named_for_caller/4) drops the link to the caller inside
start_link_and_await/3, before `trap_exit` is restored — see that
function's doc for why the caller cannot do it after.
""".
-spec safe_spawn_named(local | global, link_mode(), atom(), module(), map()) ->
    {ok, pid()} | {error, term()}.
safe_spawn_named(Scope, Link, Name, Module, InitArgs) ->
    case native_backing_module(Module) of
        none -> safe_spawn_gen_named(Scope, Link, Name, Module, InitArgs);
        Backing -> safe_start_native(Backing, InitArgs, {Scope, Name}, Link)
    end.

%% `safe_spawn_named/5` for a compiled (non-`native:`) actor module.
-spec safe_spawn_gen_named(local | global, link_mode(), atom(), module(), map()) ->
    {ok, pid()} | {error, term()}.
safe_spawn_gen_named(local, Link, Name, Module, InitArgs) ->
    start_link_and_await(
        fun(Args) -> gen_server:start_link({local, Name}, Module, Args, []) end, InitArgs, Link
    );
safe_spawn_gen_named(global, Link, Name, Module, InitArgs) ->
    case
        start_link_and_await(
            fun(Args) -> gen_server:start_link(Module, Args, []) end, InitArgs, Link
        )
    of
        {ok, Pid} ->
            case global:register_name(Name, Pid, fun ?MODULE:resolve_global_conflict/3) of
                yes ->
                    {ok, Pid};
                no ->
                    gen_server:stop(Pid, normal, 5000),
                    {error, {already_started, global:whereis_name(Name)}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
BT-4577: the backing module of a `native:` actor class's facade module
(ADR 0056), or `none` for any other module (a compiled actor, a hand-written
gen_server).

A `native:` actor compiles to a facade with no `init/1`; its process is the
backing module's, started by `BackingModule:start_link(Config)` — what the
facade's own generated `spawn/1` calls. The facade publishes that module as
`backing_module` in `__beamtalk_meta/0`, read here through
`beamtalk_class_registry:meta_backing_module/1`, so every start path that
is handed the class's module (`safe_spawn_linked/2`, `safe_spawn_named/5`,
and through them a supervisor's actor child, `beamtalk_supervisor:
start_actor_child/2,3`) starts the backing module instead of calling
`gen_server:start_link` on the facade (`undef` `init/1`).
""".
-spec native_backing_module(module()) -> module() | none.
native_backing_module(Module) ->
    _ = code:ensure_loaded(Module),
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true -> beamtalk_class_registry:meta_backing_module(Module:'__beamtalk_meta'());
        false -> none
    end.

-doc """
BT-4577: start a `native:` actor's backing module, linked, as
`safe_spawn_linked/2` (`Registration = none`) and `safe_spawn_named/5`
(`{local | global, Name}`) do for a compiled actor.

The start is `BackingModule:start_link(Config)`, the same call the facade's
`spawn/1` makes, on the same map `Config` (anything else is a `#type_error`,
as the facade raises). There is no Beamtalk `initialize` to await and no
init-ack target to add: the backing module's own `init/1` has run when
`start_link` returns. Exits are trapped around the start so a backing
`init/1` that fails is answered as `{error, Reason}` instead of killing the
caller.

The backing `start_link/1` takes no name, so a named start registers the
pid after it started — `register/2` for `local`, `global:register_name/3`
with `resolve_global_conflict/3` for `global` (as the compiled-actor `global`
path already does). A name already held is refused before the start, so the
backing `init/1` and its side effects (a port, an OS process) do not run for
nothing; a start that loses the race to another holder is stopped
(`stop_unregistered_native/1`). Either way the answer is `{error,
{already_started, Holder}}`, which `spawn_named_scoped/5` turns into
`#name_registered`. A registration that fails with no holder (the backing
process registered itself under another name, or already died) is
`{error, {register_failed, Name}}`. `Link = unlinked` drops the start link
before exits stop being trapped (`maybe_unlink_started/2`).

The backing `start_link/1` must link the new process to its caller, as
`gen_server:start_link/3` does (ADR 0056): that link is what a supervisor
restarts on.
""".
-spec safe_start_native(module(), term(), none | {local | global, atom()}, link_mode()) ->
    {ok, pid()} | {error, term()}.
safe_start_native(_Backing, Config, _Registration, _Link) when not is_map(Config) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'spawnWith:'),
            <<"spawnWith: expects a Dictionary argument">>
        )};
safe_start_native(Backing, Config, Registration, Link) ->
    case name_holder(Registration) of
        undefined -> start_native_trapped(Backing, Config, Registration, Link);
        Holder -> {error, {already_started, Holder}}
    end.

%% The pid already holding a named start's name, or `undefined`.
%%
%% A local name is released atomically when its process exits. A global name
%% is not: `global:whereis_name/1` reads global's table, whose row for a dead
%% process goes only once the global name server has handled its `'DOWN'`,
%% so a supervisor restarting a crashed `#global` child can still see the dead
%% pid. A dead holder on this node counts as none, so the restart is not
%% refused; a name that really is taken is still refused after the start by
%% `register_native_started/2`.
-spec name_holder(none | {local | global, atom()}) -> pid() | port() | undefined.
name_holder(none) ->
    undefined;
name_holder({local, Name}) ->
    whereis(Name);
name_holder({global, Name}) ->
    case global:whereis_name(Name) of
        Pid when is_pid(Pid), node(Pid) =:= node() ->
            case is_process_alive(Pid) of
                true -> Pid;
                false -> undefined
            end;
        Other ->
            Other
    end.

-spec start_native_trapped(module(), map(), none | {local | global, atom()}, link_mode()) ->
    {ok, pid()} | {error, term()}.
start_native_trapped(Backing, Config, Registration, Link) ->
    OldTrap = erlang:process_flag(trap_exit, true),
    Result =
        %% bt-catcher-audit: not-applicable-no-block - Backing:start_link/1
        %% is a hand-written Erlang start function, never a Beamtalk block
        try Backing:start_link(Config) of
            {ok, Pid} when is_pid(Pid) -> register_native_started(Registration, Pid);
            {error, Reason} -> {error, Reason};
            ignore -> {error, ignore};
            Other -> {error, {bad_start_link_return, Other}}
        catch
            _:CrashReason -> {error, CrashReason}
        end,
    case Result of
        {ok, Started} -> ok = maybe_unlink_started(Link, Started);
        {error, _} -> ok
    end,
    erlang:process_flag(trap_exit, OldTrap),
    Result.

%% BT-4577: register a just-started native actor under its name, or stop it
%% when the name cannot be registered.
-spec register_native_started(none | {local | global, atom()}, pid()) ->
    {ok, pid()} | {error, {already_started, pid() | port()} | {register_failed, atom()}}.
register_native_started(none, Pid) ->
    {ok, Pid};
register_native_started({local, Name} = Registration, Pid) ->
    try register(Name, Pid) of
        true -> {ok, Pid}
    catch
        error:badarg -> refuse_unregistered_native(Registration, Pid)
    end;
register_native_started({global, Name} = Registration, Pid) ->
    case global:register_name(Name, Pid, fun ?MODULE:resolve_global_conflict/3) of
        yes -> {ok, Pid};
        no -> refuse_unregistered_native(Registration, Pid)
    end.

%% A registration failed: stop the new process, and name the holder when the
%% name really is taken.
-spec refuse_unregistered_native({local | global, atom()}, pid()) ->
    {error, {already_started, pid() | port()} | {register_failed, atom()}}.
refuse_unregistered_native({_Scope, Name} = Registration, Pid) ->
    ok = stop_unregistered_native(Pid),
    case name_holder(Registration) of
        undefined -> {error, {register_failed, Name}};
        Holder -> {error, {already_started, Holder}}
    end.

%% Stop a native actor whose name could not be registered. Called with exits
%% trapped: the start link is removed and any `'EXIT'` it already delivered is
%% dropped, then the process is asked to shut down (running `terminate/2` if
%% it traps exits, so it can release what its `init/1` opened) and killed if
%% it is still alive after 5 s. It is awaited through a monitor, since the
%% backing `start_link` is not guaranteed to link.
-spec stop_unregistered_native(pid()) -> ok.
stop_unregistered_native(Pid) ->
    ok = maybe_unlink_started(unlinked, Pid),
    MonRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    after 5000 ->
        exit(Pid, kill),
        receive
            {'DOWN', MonRef, process, Pid, _} -> ok
        end
    end.

-doc """
Shared linked-start + trap_exit + await_initialize dance for
`safe_spawn_named/5` and `safe_spawn_linked/2`. `StartFun` performs the
actual `gen_server:start_link/3,4` call (parametrized so the two callers can
each pass their own registration shape) on `InitArgs` plus the
initialize-ack target (with_init_ack/1, BT-3992), and is invoked exactly
once, with `trap_exit` already set, so a crash during `initialize` delivers
an `'EXIT'` message here instead of taking this process down.

`Link = unlinked` (BT-3992) removes the start link before `trap_exit` is
restored, and drops any `'EXIT'` the actor already sent over it. A caller
that wants the actor unlinked (a class method's `self spawnAs:`, a remote
spawn's erpc worker) must not unlink afterwards: `spawn` now returns as soon
as `initialize` has returned, so continuations it requested run
concurrently with the caller, and one that stops the actor
(`self stopWith:`) in the window between the restore and the caller's
`unlink/1` would kill a caller that does not trap exits.
""".
-spec start_link_and_await(
    fun((term()) -> {ok, pid()} | {error, term()} | ignore), term(), link_mode()
) ->
    {ok, pid()} | {error, term()}.
start_link_and_await(StartFun, InitArgs, Link) ->
    OldTrap = erlang:process_flag(trap_exit, true),
    {AckArgs, AckRef} = with_init_ack(InitArgs),
    Result = StartFun(AckArgs),
    case Result of
        {ok, Pid} ->
            case await_initialize(Pid, erlang:monitor(process, Pid), AckRef) of
                ok ->
                    ok = maybe_unlink_started(Link, Pid),
                    erlang:process_flag(trap_exit, OldTrap),
                    {ok, Pid};
                {error, Reason} ->
                    %% Kill process if still alive (timeout case: initialize
                    %% still running but we gave up waiting). Then flush EXIT
                    %% BEFORE restoring trap_exit to avoid a fatal signal.
                    exit(Pid, kill),
                    %% Wait unconditionally — EXIT is guaranteed after kill
                    receive
                        {'EXIT', Pid, ExitReason} ->
                            ok = flush_init_ack(AckRef),
                            %% BT-3677: `noproc` means the actor died before
                            %% await_initialize could monitor it, so the monitor
                            %% lost the real reason. The link (established
                            %% atomically by start_link) delivered it in the EXIT
                            %% signal; prefer that. Any other Reason is already
                            %% the genuine stop reason / timeout, keep it.
                            erlang:process_flag(trap_exit, OldTrap),
                            {error, prefer_exit_reason(Reason, ExitReason)}
                    end
            end;
        {error, Reason} ->
            erlang:process_flag(trap_exit, OldTrap),
            {error, Reason};
        ignore ->
            erlang:process_flag(trap_exit, OldTrap),
            {error, ignore}
    end.

%% Unlink a just-started actor (BT-3992) while the caller still traps exits,
%% then drop an `'EXIT'` that arrived over the link before it was removed —
%% `unlink/1` guarantees no further link signal once it returns.
-spec maybe_unlink_started(link_mode(), pid()) -> ok.
maybe_unlink_started(linked, _Pid) ->
    ok;
maybe_unlink_started(unlinked, Pid) ->
    true = unlink(Pid),
    receive
        {'EXIT', Pid, _} -> ok
    after 0 -> ok
    end.

-spec prefer_exit_reason(term(), term()) -> term().
prefer_exit_reason(noproc, ExitReason) -> ExitReason;
prefer_exit_reason(Reason, _ExitReason) -> Reason.

-doc """
Shared await_initialize + force-kill-on-timeout tail for the unlinked
`safe_spawn/2` and `safe_spawn_monitored/2` paths. No link exists to fall
back on for an 'EXIT' message on the force-kill path, so this monitors
explicitly to deterministically observe termination before returning.

On success `MonRef` is demonitored (`release`) or left live for the caller
(`keep`); either way it is answered with the pid. Every failure has consumed
it (await_initialize/4).
""".
-spec await_initialize_or_kill_unlinked(
    pid(), reference(), reference() | none, monitor_mode()
) ->
    {ok, {pid(), reference()}} | {error, term()}.
await_initialize_or_kill_unlinked(Pid, MonRef, AckRef, Mode) ->
    case await_initialize(Pid, MonRef, AckRef, Mode) of
        ok ->
            {ok, {Pid, MonRef}};
        {error, Reason} ->
            KillRef = erlang:monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', KillRef, process, Pid, _} -> ok
            end,
            ok = flush_init_ack(AckRef),
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
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    list(),
    pid()
) ->
    ok.
%% ADR 0079 / ADR 0126: name-resolving proxy fan-out. Name-only selectors
%% answer from the proxy itself; other selectors resolve the name to the
%% currently-registered pid and re-enter the pid-based clauses below.
async_send(Ref, Selector, Args, FuturePid) when ?IS_PROXY_REF(Ref) ->
    case proxy_route(Ref, Selector, Args) of
        {answer, Value} ->
            beamtalk_future:resolve(FuturePid, Value),
            ok;
        {pid, Pid} ->
            async_send(Pid, Selector, Args, FuturePid);
        {error, Error} ->
            beamtalk_future:reject(FuturePid, Error),
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
    async_stop(ActorPid, normal, ?STOP_TIMEOUT, stop, FuturePid);
async_send(ActorPid, 'stopWith:', [Reason], FuturePid) ->
    %% BT-3967: stop with a caller-chosen exit reason.
    async_stop(ActorPid, Reason, ?STOP_TIMEOUT, 'stopWith:', FuturePid);
async_send(ActorPid, 'stopWith:timeout:', [Reason, Timeout], FuturePid) ->
    case is_stop_timeout(Timeout) of
        true ->
            async_stop(ActorPid, Reason, Timeout, 'stopWith:timeout:', FuturePid);
        false ->
            beamtalk_future:reject(FuturePid, stop_timeout_type_error('stopWith:timeout:')),
            ok
    end;
async_send(ActorPid, 'continueWith:' = Selector, [ContSelector], FuturePid) ->
    %% BT-3968: only meaningful on `self`; raises in the caller otherwise.
    async_request_continuation(ActorPid, Selector, ContSelector, none, FuturePid);
async_send(ActorPid, 'continueWith:withArguments:' = Selector, [ContSelector, ContArgs], FuturePid) ->
    async_request_continuation(ActorPid, Selector, ContSelector, {args, ContArgs}, FuturePid);
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
                %% bt-catcher-audit: not-applicable-other-process - onExit block runs in the
                %% spawned watcher, no home entry
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
    %% Instrumented dispatch span (ADR 0069 Phase 2a; BT-4113: events opt-in).
    %% Measures dispatch-to-mailbox time for async sends.
    Class = lookup_class(ActorPid),
    %% Check liveness before sending, and spawn a watcher to detect
    %% actor death during message processing. The watcher monitors both the
    %% actor and future processes: if the actor dies before the future is
    %% resolved, the watcher rejects the future with a structured error.
    beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, async, fun() ->
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
                                {ok, ok};
                            {error, EncErr} ->
                                %% Sender-side encode failure (request direction,
                                %% ADR 0126 §5.2) — never reaches the network.
                                beamtalk_future:reject(
                                    FuturePid, EncErr#beamtalk_error{selector = Selector}
                                ),
                                {ok, error}
                        end;
                    false ->
                        gen_server:cast(ActorPid, {Selector, Args, FuturePid, PropCtx}),
                        spawn_future_watcher(ActorPid, FuturePid, Selector),
                        {ok, ok}
                end;
            false ->
                beamtalk_future:reject(FuturePid, actor_dead_error_record(Selector)),
                {ok, error}
        end
    end),
    ok.

-doc """
Send a fire-and-forget message to an actor (no future, no return value).

A dead actor silently answers ok (fire-and-forget semantics — the caller
does not expect a reply): `gen_server:cast/2` to a dead pid is a no-op, so
there is no liveness pre-check (BT-4113).
""".
-spec cast_send(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()}, atom(), list()
) -> ok.
%% ADR 0079: name-resolving proxy fan-out for fire-and-forget sends.
%% A vanished name or an unreachable node silently drops the cast (the
%% "actor dead -> ok" contract). Intended difference from the other three
%% send paths (BT-4676): the name-only selectors (`isAlive`, `isRegistered`,
%% `registeredName`) are NOT answered locally — a cast has no reply to carry
%% an answer, so they are forwarded like any other selector.
cast_send(Ref, Selector, Args) when ?IS_PROXY_REF(Ref) ->
    case resolve_proxy(Ref) of
        {ok, Pid} -> cast_send(Pid, Selector, Args);
        undefined -> ok;
        {node_down, _Node} -> ok
    end;
cast_send(ActorPid, Selector, Args) ->
    cast_send_pid(ActorPid, Selector, Args, undefined).

-doc """
`cast_send/3` for a caller that already holds the actor's class (BT-4113):
`beamtalk_message_dispatch` passes the `#beamtalk_object{}` record's class,
so the send never looks it up. A name-resolving proxy reference resolves as
in `cast_send/3`.
""".
-spec cast_send_known(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    atom(),
    list()
) -> ok.
cast_send_known(ActorPid, Class, Selector, Args) when is_pid(ActorPid) ->
    cast_send_pid(ActorPid, Selector, Args, Class);
cast_send_known(Ref, _Class, Selector, Args) ->
    cast_send(Ref, Selector, Args).

-spec cast_send_pid(pid(), atom(), list(), atom() | undefined) -> ok.
cast_send_pid(ActorPid, 'continueWith:' = Selector, [ContSelector], _Class) ->
    %% BT-3968: `actor continueWith: #x!` from another process is caller
    %% misuse, not a remote request — raise in the caller rather than cast.
    request_continuation(ActorPid, Selector, ContSelector, none);
cast_send_pid(ActorPid, 'continueWith:withArguments:' = Selector, [ContSelector, ContArgs], _Class) ->
    request_continuation(ActorPid, Selector, ContSelector, {args, ContArgs});
cast_send_pid(ActorPid, Selector, Args, ClassHint) ->
    %% Instrumented dispatch span (ADR 0069 Phase 2a; BT-4113: events opt-in).
    %% Measures dispatch-to-mailbox time for cast sends. No liveness
    %% pre-check: `gen_server:cast/2` to a dead pid is already a silent no-op,
    %% which is exactly the fire-and-forget contract.
    Class = resolve_class(ActorPid, ClassHint),
    beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, cast, fun() ->
        PropCtx = get_propagated_ctx(),
        case node(ActorPid) =/= node() of
            true ->
                case beamtalk_wire:encode(Args) of
                    {ok, WireArgs} ->
                        gen_server:cast(
                            ActorPid,
                            {'$beamtalk_wire', ?BT_WIRE_VERSION, cast, Selector, WireArgs, PropCtx}
                        ),
                        {ok, cast};
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
                        {ok, cast}
                end;
            false ->
                gen_server:cast(ActorPid, {cast, Selector, Args, PropCtx}),
                {ok, cast}
        end
    end),
    ok.

-doc """
The class to report for a send to `Pid`: the caller's `ClassHint` when it
has one (BT-4113, from the `#beamtalk_object{}` record — also the only way a
remote pid's class is known, ADR 0126 §7.3), else `lookup_class/1`.
""".
-spec resolve_class(pid(), atom() | undefined) -> atom().
resolve_class(Pid, undefined) -> lookup_class(Pid);
resolve_class(_Pid, Class) -> Class.

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

All other messages go via gen_server:call and the result is unwrapped:
- A dead actor raises `#beamtalk_error{kind = actor_dead}` (the call's
  `noproc` exit — there is no liveness pre-check, BT-4113)
- A timeout raises `#beamtalk_error{kind = timeout}`
""".
-spec sync_send(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()}, atom(), list()
) ->
    term().
%% ADR 0079: name-resolving proxy fan-out. Name-only methods answer from the
%% proxy itself; other methods resolve to the currently-registered pid,
%% raising `no_such_process` if the name has vanished.
sync_send(Ref, Selector, Args) when ?IS_PROXY_REF(Ref) ->
    sync_send_proxy(Ref, Selector, Args, fun(Pid) -> sync_send(Pid, Selector, Args) end);
sync_send(ActorPid, Selector, Args) ->
    sync_send_pid(ActorPid, Selector, Args, undefined).

-doc """
`sync_send/3` for a caller that already holds the actor's class (BT-4113):
`beamtalk_message_dispatch` passes the `#beamtalk_object{}` record's class,
so the send never looks it up. A name-resolving proxy reference resolves as
in `sync_send/3`.
""".
-spec sync_send_known(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    atom(),
    list()
) -> term().
sync_send_known(ActorPid, Class, Selector, Args) when is_pid(ActorPid) ->
    sync_send_pid(ActorPid, Selector, Args, Class);
sync_send_known(Ref, _Class, Selector, Args) ->
    sync_send(Ref, Selector, Args).

-doc """
`sync_send/4` (explicit timeout) for a caller that already holds the
actor's class — see `sync_send_known/4`.
""".
-spec sync_send_known(
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    atom(),
    list(),
    timeout()
) -> term().
sync_send_known(ActorPid, Class, Selector, Args, Timeout) when is_pid(ActorPid) ->
    sync_send_pid(ActorPid, Selector, Args, Timeout, Class);
sync_send_known(Ref, _Class, Selector, Args, Timeout) ->
    sync_send(Ref, Selector, Args, Timeout).

-doc """
The pid clauses of `sync_send/3`: lifecycle selectors answered locally, and
every other message through the Layer 1 self-send check. `ClassHint` is the
caller's class, or `undefined` to look it up (see `resolve_class/2`).
""".
-spec sync_send_pid(pid(), atom(), list(), atom() | undefined) -> term().
sync_send_pid(ActorPid, isAlive, [], _Class) ->
    beamtalk_pid:is_alive(ActorPid);
sync_send_pid(ActorPid, isRemote, [], _Class) ->
    is_remote(ActorPid, fun() -> sync_send(ActorPid, node, []) end);
sync_send_pid(ActorPid, stop, [], _Class) ->
    %% stop is handled locally - gracefully stops the actor process
    %% No send-site telemetry for stop — terminate/2 handles it
    %% to avoid double-counting (request + termination).
    sync_stop(ActorPid, normal, ?STOP_TIMEOUT, stop);
sync_send_pid(ActorPid, 'stopWith:', [Reason], _Class) ->
    %% BT-3967: stop with a caller-chosen exit reason.
    sync_stop(ActorPid, Reason, ?STOP_TIMEOUT, 'stopWith:');
sync_send_pid(ActorPid, 'stopWith:timeout:', [Reason, Timeout], _Class) ->
    case is_stop_timeout(Timeout) of
        true ->
            sync_stop(ActorPid, Reason, Timeout, 'stopWith:timeout:');
        false ->
            beamtalk_exception_handler:reraise(stop_timeout_type_error('stopWith:timeout:'))
    end;
sync_send_pid(ActorPid, 'continueWith:' = Selector, [ContSelector], _Class) ->
    %% BT-3968: record a follow-up step for the current callback (self only).
    request_continuation(ActorPid, Selector, ContSelector, none);
sync_send_pid(ActorPid, 'continueWith:withArguments:' = Selector, [ContSelector, ContArgs], _Class) ->
    request_continuation(ActorPid, Selector, ContSelector, {args, ContArgs});
sync_send_pid(ActorPid, kill, [], _Class) ->
    %% kill is handled locally - forcefully kills the actor process.
    %% Telemetry, monitor-before-kill, and DOWN-wait are in kill_and_wait/1.
    case kill_and_wait(ActorPid) of
        ok -> ok;
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end;
sync_send_pid(_ActorPid, delegate, [], _Class) ->
    %% Non-native Actors do not have a backing Erlang module.
    %% Native Actors will override this at the codegen level.
    beamtalk_exception_handler:reraise(delegate_error(unknown));
sync_send_pid(ActorPid, pid, [], _Class) ->
    %% pid returns the raw Erlang PID backing the actor
    ActorPid;
sync_send_pid(ActorPid, monitor, [], _Class) ->
    erlang:monitor(process, ActorPid);
sync_send_pid(ActorPid, 'onExit:', [Block], _Class) ->
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
                %% bt-catcher-audit: not-applicable-other-process - onExit block runs in the
                %% spawned watcher, no home entry
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
sync_send_pid(ActorPid, Selector, Args, ClassHint) ->
    %% Layer 1: Fast-path for self-sends.
    %% If ActorPid is our own process AND the installed cell holds our own
    %% state (meaning we're inside a handle_call/handle_cast dispatch),
    %% dispatch directly to avoid gen_server:call deadlock. This catches
    %% aliased self-sends like:
    %%   other := self. other fieldNames
    case self_send_kind(ActorPid) of
        self ->
            self_dispatch(Selector, Args);
        remote ->
            sync_send_remote(ActorPid, Selector, Args, ClassHint);
        foreign ->
            raise_self_send_during_away_super(ActorPid, Selector)
    end.

-doc """
Remote sync-send via gen_server:call (the normal path).
Factored out of sync_send/3 for Layer 1 self-send fast-path.

TimeoutProxy manages its own timeout on the inner (proxy→target) hop, so
the outer (caller→proxy) hop uses `infinity` so it doesn't time out before
the proxy's configured timeout expires. Every other actor uses
`gen_server:call/2`'s 5000ms default.
""".
-spec sync_send_remote(pid(), atom(), list(), atom() | undefined) -> term().
sync_send_remote(ActorPid, Selector, Args, ClassHint) ->
    Class = resolve_class(ActorPid, ClassHint),
    CallTimeout =
        case Class of
            'TimeoutProxy' -> infinity;
            _ -> default
        end,
    sync_call(ActorPid, Selector, Args, CallTimeout, Class).

-doc """
The instrumented `gen_server:call` shared by `sync_send/3` (via
`sync_send_remote/4`) and `sync_send/4`.

The dispatch span (ADR 0069 Phase 2a) measures caller-perspective round-trip
time and records the always-on aggregate; it emits telemetry events only
when dispatch events are enabled (BT-4113). A raise inside the span is
re-raised with its original class and reason, so the outer try/catch can
convert exits into structured errors.

No liveness pre-check (BT-4113): `gen_server:call` to a dead actor exits
`noproc` (or `normal`/`shutdown` if it dies mid-call), which the catch
clauses below already map to `actor_dead` — without the snapshot race a
pre-check had.

Generated handle_call/3 wraps replies as {ok, Result} or {error, Error}, and
the reply is unwrapped here so callers receive the value directly. The
{error, Error} case has three sub-forms due to the safe_dispatch layer:
  1. Error = {ErlType, Value, Stacktrace} — with captured stacktrace
  2. Error = {ErlType, Value} — backward compat: without stacktrace
  3. Error = other term — e.g. #beamtalk_error{} from dispatch_user_method
All forms are re-raised as Erlang exceptions so the caller sees them.
""".
-spec sync_call(pid(), atom(), list(), default | timeout(), atom()) -> term().
sync_call(ActorPid, Selector, Args, CallTimeout, Class) ->
    try
        beamtalk_telemetry:dispatch_span(ActorPid, Class, Selector, sync, fun() ->
            PropCtx = get_sync_propagated_ctx(),
            %% Layer 2: Check for transitive cycles before gen_server:call.
            %% If ActorPid is already in the call stack, this send would deadlock.
            check_call_stack(ActorPid, Selector),
            case wire_sync_call(ActorPid, Selector, Args, PropCtx, CallTimeout) of
                {ok, Result} ->
                    {Result, ok};
                {error, {ErlType, ErrorValue, Stacktrace}} ->
                    %% safe_dispatch caught an Erlang exception with stacktrace;
                    %% re-raise with full type/stacktrace context, attaching the
                    %% active selector/class breadcrumb so a raw error escaping
                    %% the method is classified *and* located.
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
                    %% The block was away from home in ActorPid, which
                    %% `rethrow_from_away/1` tells the test-build commit
                    %% check (BT-4536).
                    beamtalk_actor_state:rethrow_from_away(Nlr);
                {error, Error} ->
                    beamtalk_exception_handler:reraise(Error);
                DirectValue ->
                    %% Backward compat: actors using beamtalk_actor:handle_call/3 directly
                    %% (rather than the generated handle_call) return values unwrapped.
                    {DirectValue, ok}
            end
        end)
    catch
        exit:Reason:Stack ->
            %% BT-4676: classification lives in beamtalk_call_failure. ADR 0126
            %% §7.1: a partitioned or never-connected node is `node_down` (the
            %% actor may be alive on the far side), not `actor_dead`.
            case beamtalk_call_failure:classify(exit, Reason) of
                dead ->
                    raise_actor_dead(Selector);
                timeout ->
                    raise_timeout(Selector);
                {unreachable, _Why} ->
                    raise_node_down(
                        beamtalk_call_failure:unreachable_node(Reason, node(ActorPid)), Selector
                    );
                other when is_tuple(Reason), tuple_size(Reason) =:= 2 ->
                    %% `{normal, _}`, `{shutdown, _}`, `{killed, _}`, custom stop
                    %% reasons: the actor died mid-call, so it is unavailable.
                    raise_actor_dead(Selector);
                other ->
                    erlang:raise(exit, Reason, Stack)
            end
    end.

-doc """
Sync `gen_server:call` for both the local and remote (ADR 0126 §5.1) paths,
called from `sync_call/5` (shared by `sync_send/3` and `sync_send/4`). `CallTimeout` is `default`
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
    pid() | {registered, atom()} | {registered, atom(), node()} | {global, atom()},
    atom(),
    list(),
    timeout()
) ->
    term().
%% ADR 0079: name-resolving proxy fan-out for the explicit-timeout path;
%% shares one body with sync_send/3 (BT-4676).
sync_send(Ref, Selector, Args, Timeout) when ?IS_PROXY_REF(Ref) ->
    sync_send_proxy(Ref, Selector, Args, fun(Pid) -> sync_send(Pid, Selector, Args, Timeout) end);
sync_send(ActorPid, Selector, Args, Timeout) ->
    sync_send_pid(ActorPid, Selector, Args, Timeout, undefined).

-doc """
The pid clauses of `sync_send/4`; `ClassHint` as in `sync_send_pid/4`.
""".
-spec sync_send_pid(pid(), atom(), list(), timeout(), atom() | undefined) -> term().
sync_send_pid(ActorPid, isRemote, [], Timeout, _ClassHint) ->
    is_remote(ActorPid, fun() -> sync_send(ActorPid, node, [], Timeout) end);
sync_send_pid(ActorPid, Selector, Args, _Timeout, _ClassHint) when
    Selector =:= 'continueWith:'; Selector =:= 'continueWith:withArguments:'
->
    %% BT-3968: never forwarded to the actor — a continuation can only be
    %% requested by the actor's own callback (see sync_send/3).
    sync_send(ActorPid, Selector, Args);
sync_send_pid(ActorPid, Selector, Args, Timeout, ClassHint) when
    is_integer(Timeout), Timeout >= 0;
    Timeout =:= infinity
->
    %% Layer 1: Fast-path for self-sends (same as sync_send/3).
    case self_send_kind(ActorPid) of
        self ->
            self_dispatch(Selector, Args);
        remote ->
            sync_call(ActorPid, Selector, Args, Timeout, resolve_class(ActorPid, ClassHint));
        foreign ->
            raise_self_send_during_away_super(ActorPid, Selector)
    end;
sync_send_pid(_ActorPid, Selector, _Args, _InvalidTimeout, _ClassHint) ->
    Error = beamtalk_error:new(
        type_error,
        unknown,
        Selector,
        <<"Timeout must be a non-negative integer (milliseconds) or #infinity">>
    ),
    beamtalk_exception_handler:reraise(Error).

-doc """
How a sync send to `ActorPid` from this process should go (Layer 1).

`self` (our own pid with our own state installed) dispatches directly.
`remote` is the normal `gen_server:call` path — including our own pid with
no cell installed, exactly as before. `foreign` is our own pid while the
cell holds another actor's state (stored by a nested away `super_send/5`):
the remote path could never serve our own pid (`gen_server:call` to self
exits `calling_self`, which the catch-all would misreport as `actor_dead`),
so the send sites raise `deadlock_detected` instead.
""".
-spec self_send_kind(pid() | tuple()) -> self | remote | foreign.
self_send_kind(ActorPid) ->
    case ActorPid =:= self() of
        true ->
            case beamtalk_actor_state:cell_owner() of
                ActorPid -> self;
                undefined -> remote;
                _Foreign -> foreign
            end;
        false ->
            remote
    end.

-doc """
Raise a structured `deadlock_detected` error for a self-send issued while
another actor's state is installed in this process's cell (a nested away
`super_send/5`): our own callback is blocked in that call, so the send could
never be served.
""".
-spec raise_self_send_during_away_super(pid(), atom()) -> no_return().
raise_self_send_during_away_super(ActorPid, Selector) ->
    Class = lookup_class(ActorPid),
    Error = beamtalk_error:new(
        deadlock_detected,
        Class,
        Selector,
        <<
            "Sync send to own actor while another actor's state is installed: ",
            "the actor is blocked in the call that carried the block away"
        >>
    ),
    Error1 = beamtalk_error:with_details(Error, #{
        target_pid => ActorPid,
        cell_owner => beamtalk_actor_state:cell_owner()
    }),
    beamtalk_exception_handler:reraise(Error1).

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
    State = beamtalk_actor_state:take(),
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

A `^` that unwound the callee (`safe_dispatch` answers `{error, Nlr,
EntryState}`) is re-thrown unchanged, with no store to the cell (ADR 0146 R3,
BT-4525): the cell already holds the callee's writes (every compiled method
stores before a call that may run the block), and storing `EntryState` would
rewind it; turning the throw into an error would stop the owner's method
boundary from catching it.
""".
-spec unwrap_dispatch_result(
    {reply, term(), map()} | {noreply, map()} | {error, term(), map()}
) -> term().
unwrap_dispatch_result({error, Nlr, _EntryState}) when ?IS_NLR(Nlr) ->
    throw(Nlr);
unwrap_dispatch_result(DispatchResult) ->
    case beamtalk_actor_state:adapt(DispatchResult) of
        {reply, Result} ->
            Result;
        noreply ->
            nil;
        {error, {ErlType, ErrorValue, Stacktrace}} ->
            %% safe_dispatch with stacktrace
            beamtalk_exception_handler:reraise(ErlType, ErrorValue, Stacktrace);
        {error, {ErlType, ErrorValue}} ->
            %% Backward compat: preserve exception class without stacktrace
            beamtalk_exception_handler:reraise(ErlType, ErrorValue, []);
        {error, Reason} ->
            beamtalk_exception_handler:reraise(Reason)
    end.

-doc """
Restore pdict entries after a dispatch completes.
Called in the `after` block of handle_call/handle_cast to clean up
both the stashed actor state and the call stack.
""".
-spec restore_dispatch_pdict(term()) -> ok.
restore_dispatch_pdict(OldState) ->
    ok = beamtalk_actor_state:uninstall(OldState),
    _ = erase(?CALL_FROM_KEY),
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
                    raise_call_cycle(ActorPid, Selector, CallStack)
            end
    end.

-spec raise_call_cycle(pid(), atom(), [pid()]) -> no_return().
raise_call_cycle(ActorPid, Selector, CallStack) ->
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
    beamtalk_exception_handler:reraise(Error1).

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
    Error = beamtalk_error:actor_timeout(Selector),
    beamtalk_exception_handler:reraise(Error).

-doc "Construct a structured actor_dead error record for the given selector.".
-spec actor_dead_error_record(atom()) -> #beamtalk_error{}.
actor_dead_error_record(Selector) ->
    beamtalk_error:actor_dead(unknown, Selector).

-doc """
Raise a structured node_down error as an Erlang exception (ADR 0126 §7.1).

Used by sync_call/5 (sync_send/3,4) when gen_server:call exits with
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
        %% bt-catcher-audit: not-applicable-other-process - AskNode is a gen_server:call to the
        %% actor
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
    beamtalk_error:node_down(unknown, Selector, Node).

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
BT-4676: the one resolver for name-resolving proxy refs (ADR 0079, 0126).
Answers the live pid, `undefined` when the name is not registered, or
`{node_down, Node}` when a node-qualified ref cannot be resolved because
`Node` is unreachable. Used by every send path (`async_send/4`, `cast_send/3`,
`sync_send/3,4`) so the three ref shapes are handled in exactly one place.
""".
-spec resolve_proxy(proxy_ref()) -> {ok, pid()} | undefined | {node_down, node()}.
resolve_proxy({registered, Name}) ->
    wrap_resolved(erlang:whereis(Name), undefined);
resolve_proxy({registered, Name, Node}) ->
    wrap_resolved(resolve_remote_registered(Name, Node), Node);
resolve_proxy({global, Name}) ->
    wrap_resolved(global:whereis_name(Name), undefined).

-spec wrap_resolved(pid() | undefined | node_down, node() | undefined) ->
    {ok, pid()} | undefined | {node_down, node()}.
wrap_resolved(Pid, _Node) when is_pid(Pid) -> {ok, Pid};
wrap_resolved(undefined, _Node) -> undefined;
wrap_resolved(node_down, Node) -> {node_down, Node}.

-doc """
BT-4676: route a send through a proxy ref. Name-only selectors (`isAlive`,
`isRegistered`, `registeredName`) are answered by the proxy itself; every
other selector resolves to a pid or a structured error. The caller decides
how to deliver the outcome (resolve/reject a future, or raise).
""".
-spec proxy_route(proxy_ref(), atom(), list()) ->
    {answer, term()} | {pid, pid()} | {error, #beamtalk_error{}}.
proxy_route(Ref, isAlive, []) ->
    {answer, is_pid_result(resolve_proxy(Ref))};
proxy_route(_Ref, isRegistered, []) ->
    {answer, true};
proxy_route({_Kind, Name}, registeredName, []) ->
    {answer, Name};
proxy_route({registered, Name, _Node}, registeredName, []) ->
    {answer, Name};
proxy_route(Ref, Selector, _Args) ->
    case resolve_proxy(Ref) of
        {ok, Pid} -> {pid, Pid};
        undefined -> {error, no_such_process_error(Ref, Selector)};
        {node_down, Node} -> {error, node_down_error_record(Node, Selector)}
    end.

-spec is_pid_result({ok, pid()} | undefined | {node_down, node()}) -> boolean().
is_pid_result({ok, _Pid}) -> true;
is_pid_result(_) -> false.

-doc "Sync half of `proxy_route/3`: answer, raise, or continue with the resolved pid.".
-spec sync_send_proxy(proxy_ref(), atom(), list(), fun((pid()) -> term())) -> term().
sync_send_proxy(Ref, Selector, Args, WithPid) ->
    case proxy_route(Ref, Selector, Args) of
        {answer, Value} -> Value;
        {pid, Pid} -> WithPid(Pid);
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end.

-doc "Construct a structured `no_such_process` error for the given proxy ref.".
-spec no_such_process_error(
    {registered, atom()} | {registered, atom(), node()} | {global, atom()}, atom()
) -> #beamtalk_error{}.
no_such_process_error({registered, Name}, Selector) ->
    no_such_process_error_record(Name, Selector);
no_such_process_error({registered, Name, _Node}, Selector) ->
    no_such_process_error_record(Name, Selector);
no_such_process_error({global, Name}, Selector) ->
    no_such_process_error_record(Name, Selector).

-spec no_such_process_error_record(atom(), atom()) -> #beamtalk_error{}.
no_such_process_error_record(Name, Selector) ->
    Error = beamtalk_error:new(
        no_such_process,
        unknown,
        Selector,
        beamtalk_error:chars_to_binary(
            io_lib:format(
                "No process is currently registered under name '~tp'", [Name]
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
BT-3967: stop `ActorPid` with `Reason`, shared by `stop`, `stopWith:` and
`stopWith:timeout:` on both the sync and async send paths.

From another process this is `beamtalk_stop:stop(ActorPid, Reason, Timeout)`
(the `gen_server:stop/3` contract, BT-4494);
an already-dead actor counts as stopped (idempotent `stop`). From inside the
actor's own process — `self stopWith:` from a method, or an aliased self —
`gen_server:stop/3` would fail with `calling_self`, so the stop is deferred
instead: request_deferred_stop/1 records it and the enclosing gen_server
callback returns `{stop, Reason, ...}` once the current dispatch finishes
(see apply_pending_actions/1).

Returns `ok`, or `{exit, ExitReason}` when the stop itself
exited (timeout, or the actor died with some other reason first).
""".
-spec stop_actor(pid(), term(), timeout()) -> ok | {exit, term()}.
stop_actor(ActorPid, Reason, Timeout) when ActorPid =:= self() ->
    %% Defer only inside a dispatching callback — the same guard as the
    %% Layer 1 self-send fast path in sync_send/3. The actor-state cell is
    %% installed by every callback wrapped with apply_pending_actions/1, so a
    %% request recorded here is guaranteed to be consumed. Anywhere else
    %% (e.g. a hand-written native gen_server) there is nothing to consume
    %% it, so fall through and let gen_server:stop/3 fail visibly
    %% (calling_self) instead of answering ok and never stopping.
    case beamtalk_actor_state:installed() of
        false -> stop_external(ActorPid, Reason, Timeout);
        true -> request_deferred_stop(Reason)
    end;
stop_actor(ActorPid, Reason, Timeout) ->
    stop_external(ActorPid, Reason, Timeout).

-spec stop_external(pid(), term(), timeout()) -> ok | {exit, term()}.
stop_external(ActorPid, Reason, Timeout) ->
    %% Not gen_server:stop/3: it raises error:timeout_value when the stop
    %% reply lands at a very short deadline (BT-4494).
    try beamtalk_stop:stop(ActorPid, Reason, Timeout) of
        ok -> ok
    catch
        exit:noproc ->
            %% Actor already stopped (bare atom exit) - treat as successful stop
            ok;
        exit:{noproc, _} ->
            %% Actor already stopped (tuple exit) - treat as successful stop
            ok;
        exit:Other ->
            {exit, Other}
    end.

-doc """
Sync-send half of stop_actor/3: answers `ok` or raises the structured
`actor_dead` error `stop` has always raised for a failed stop.
""".
-spec sync_stop(pid(), term(), timeout(), atom()) -> ok.
sync_stop(ActorPid, Reason, Timeout, Selector) ->
    case stop_actor(ActorPid, Reason, Timeout) of
        ok ->
            ok;
        {exit, Other} ->
            %% Preserve sync_send/3 contract: translate exits to structured errors
            beamtalk_exception_handler:reraise(
                beamtalk_error:with_details(
                    actor_dead_error_record(Selector), #{exit_reason => Other}
                )
            )
    end.

-doc """
Async-send half of stop_actor/3: resolves `FuturePid` with `ok`, or rejects
it with the structured error `stop` has always used for a failed stop.
""".
-spec async_stop(pid(), term(), timeout(), atom(), pid()) -> ok.
async_stop(ActorPid, Reason, Timeout, Selector, FuturePid) ->
    case stop_actor(ActorPid, Reason, Timeout) of
        ok ->
            beamtalk_future:resolve(FuturePid, ok);
        {exit, Other} ->
            %% Other stop failures (e.g., timeout) - reject Future deterministically
            Error = beamtalk_error:new(
                actor_dead,
                unknown,
                Selector,
                beamtalk_error:chars_to_binary(io_lib:format("Actor stop failed: ~tp", [Other]))
            ),
            beamtalk_future:reject(FuturePid, Error)
    end,
    ok.

-doc "Whether `Timeout` is a valid `gen_server:stop/3` timeout.".
-spec is_stop_timeout(term()) -> boolean().
is_stop_timeout(infinity) -> true;
is_stop_timeout(Timeout) -> is_integer(Timeout) andalso Timeout >= 0.

-spec stop_timeout_type_error(atom()) -> #beamtalk_error{}.
stop_timeout_type_error(Selector) ->
    beamtalk_error:new(
        type_error,
        unknown,
        Selector,
        <<"Timeout must be a non-negative integer (milliseconds) or #infinity">>
    ).

-doc """
BT-3967: record a deferred self-stop. The first request in a dispatch wins
(a later `stopWith:` in the same method does not overwrite the reason), so
the reason `terminate:` sees is the one the method asked for first.
""".
-spec request_deferred_stop(term()) -> ok.
request_deferred_stop(Reason) ->
    case get(?PENDING_STOP_KEY) of
        undefined ->
            put(?PENDING_MARKER_KEY, true),
            put(?PENDING_STOP_KEY, {stop, Reason});
        {stop, _} ->
            ok
    end,
    ok.

-doc """
BT-3967 / BT-3968: discard every deferred pending action (a self-stop
request and the continuation queue). Called on entry to every callback
wrapped with apply_pending_actions/1, so only a request made by that
callback's own dispatch can take effect — never a stale one recorded by
some unwrapped path earlier.
""".
-spec clear_pending_actions() -> ok.
clear_pending_actions() ->
    case erase(?PENDING_MARKER_KEY) of
        undefined ->
            ok;
        true ->
            _ = erase(?PENDING_STOP_KEY),
            _ = erase(?PENDING_CONTINUE_KEY),
            ok
    end,
    _ = erase(?CALL_FROM_KEY),
    ok.

-doc """
BT-4090: record the `From` of the sync call about to be dispatched, so
`self deferReply` (takeDeferredCall/1) can take it. Paired with
uninstall_call_from/0 around dispatch in every `handle_call` path.
""".
-spec install_call_from(term()) -> ok.
install_call_from(From) ->
    put(?CALL_FROM_KEY, From),
    ok.

-doc """
BT-4090: remove the call-`From` cell after dispatch. Returns `true` when the
dispatched method deferred its reply (`self deferReply`), in which case the
`handle_call` success arm must answer `{noreply, State}` without encoding
the (ignored) return value. Error arms ignore the result, so a method that
raises after deferring replies the error to its caller instead of hanging it.
""".
-spec uninstall_call_from() -> boolean().
uninstall_call_from() ->
    case erase(?CALL_FROM_KEY) of
        {deferred, _From} -> true;
        _ -> false
    end.

-doc """
BT-3967 / BT-3968: apply the deferred pending actions to a gen_server
callback's return value. Both requests are consumed.

- **Stop pending** (`self stopWith: Reason`, see request_deferred_stop/1):
  the callback's `{reply, Reply, State}` / `{noreply, State}` becomes
  `{stop, Reason, Reply, State}` / `{stop, Reason, State}`, so OTP replies to
  a pending caller first, then runs `terminate/2` with exactly `Reason` and
  exits with it. A pending stop wins over any queued continuation, which is
  dropped. A return value that is already a `{stop, ...}` (e.g. a failed
  `initialize`) wins over the request.
- **Continuations pending** (`self continueWith:`, see
  request_continuation/4): `{noreply, State}` / `{reply, Reply, State}` gain
  the action `{continue, {'$bt_continue', Queue}}` (request order), so OTP
  sends any reply first and runs `handle_continue/2` before the next
  mailbox message. A `{stop, ...}` return drops the queue.
- **Nothing pending:** the value is returned unchanged.
""".
-spec apply_pending_actions(Result) ->
    Result
    | {stop, term(), map()}
    | {stop, term(), term(), map()}
    | {noreply, map(), {continue, term()}}
    | {reply, term(), map(), {continue, term()}}
when
    Result :: term().
apply_pending_actions(Result) ->
    case erase(?PENDING_MARKER_KEY) of
        undefined ->
            %% Nothing requested (BT-4113: one read in the common case).
            Result;
        true ->
            Stop = erase(?PENDING_STOP_KEY),
            Continuations = erase(?PENDING_CONTINUE_KEY),
            case {Stop, Continuations} of
                {{stop, Reason}, _} -> pending_stop_result(Result, Reason);
                {undefined, [_ | _] = RevQueue} -> add_continue(Result, lists:reverse(RevQueue));
                {undefined, _} -> Result
            end
    end.

-spec add_continue(term(), [{atom(), list()}, ...]) -> term().
add_continue({noreply, State}, Queue) ->
    {noreply, State, {continue, {?BT_CONTINUE_TAG, Queue}}};
add_continue({reply, Reply, State}, Queue) ->
    {reply, Reply, State, {continue, {?BT_CONTINUE_TAG, Queue}}};
%% Stopping anyway (a failed `initialize`, say): no further step runs.
add_continue({stop, _, _} = Stop, _Queue) ->
    Stop;
add_continue({stop, _, _, _} = Stop, _Queue) ->
    Stop;
%% No dispatching callback returns an action of its own today; merging two
%% would silently drop one, so fail loudly instead.
add_continue(Result, _Queue) ->
    error({bt_continue_conflicts_with_action, Result}).

-doc """
BT-3968: seed the continuation queue at the start of a `handle_continue/2`
step with the continuations still waiting behind the one about to run, so
any request the running continuation makes is appended *after* them (FIFO).
Runs after clear_pending_actions/0 inside the wrapped callback. A step that
raises crashes the actor (ADR 0134), so nothing seeded here outlives a
failing step.
""".
-spec seed_pending_continuations([{atom(), list()}]) -> ok.
seed_pending_continuations([]) ->
    ok;
seed_pending_continuations(Rest) when is_list(Rest) ->
    RevRest = lists:reverse(Rest),
    put(?PENDING_MARKER_KEY, true),
    put(?PENDING_CONTINUE_KEY, RevRest),
    ok.

-doc """
BT-3968: drop the continuations requested by a dispatch that failed. Called
from the dispatch-error arms that reply (a sync call, an async send with a
future): the failed method's follow-up steps are void. Such a callback never
runs inside a `handle_continue/2` step, so no earlier continuation is queued
alongside them. A pending stop is left alone and still honoured. A failed
dispatch with no reply channel (a cast, `handleInfo:`, a continuation step)
crashes the actor instead (ADR 0134, see dispatch_cast/3), which drops the
whole queue.
""".
-spec discard_pending_continuations() -> ok.
discard_pending_continuations() ->
    _ = erase(?PENDING_CONTINUE_KEY),
    ok.

-doc """
BT-3968: record a follow-up send to `self` (`continueWith:` /
`continueWith:withArguments:`), run after the current gen_server callback
returns and before the next mailbox message (see apply_pending_actions/1).

`RequestSelector` is the public selector, for error reporting; `ArgSpec` is
`none` for `continueWith:` (unary selector, no arguments) or
`{args, Args}` for `continueWith:withArguments:`.

Raises in the caller — never silently drops the request:
- `invalid_continuation` when `Pid` is not the calling process, or it is
  but no wrapped dispatching callback is running (`beamtalk_actor_state:installed/0`
  is `false`), since nothing would consume the request — the same guard as
  stop_actor/3's deferred self-stop;
- `type_error` when the selector is not a Symbol or the arguments are not a
  List;
- `arity_mismatch` when the argument count does not match the selector.

Whether the selector is understood is not checked here: a
`does_not_understand` surfaces when the continuation runs, as an ordinary
no-reply error.
""".
-spec request_continuation(term(), atom(), term(), none | {args, term()}) -> ok.
request_continuation(Pid, RequestSelector, ContSelector, ArgSpec) ->
    case record_continuation(Pid, RequestSelector, ContSelector, ArgSpec) of
        ok -> ok;
        {error, Error} -> beamtalk_exception_handler:reraise(Error)
    end.

-doc "Async-send half of request_continuation/4: resolves or rejects `FuturePid`.".
-spec async_request_continuation(term(), atom(), term(), none | {args, term()}, pid()) -> ok.
async_request_continuation(Pid, RequestSelector, ContSelector, ArgSpec, FuturePid) ->
    case record_continuation(Pid, RequestSelector, ContSelector, ArgSpec) of
        ok -> beamtalk_future:resolve(FuturePid, ok);
        {error, Error} -> beamtalk_future:reject(FuturePid, Error)
    end,
    ok.

-spec record_continuation(term(), atom(), term(), none | {args, term()}) ->
    ok | {error, #beamtalk_error{}}.
record_continuation(Pid, RequestSelector, ContSelector, ArgSpec) ->
    case Pid =:= self() andalso beamtalk_actor_state:installed() of
        false ->
            {error, invalid_continuation_error(Pid, RequestSelector)};
        true ->
            case validate_continuation(RequestSelector, ContSelector, ArgSpec) of
                {ok, Args} ->
                    Queue =
                        case get(?PENDING_CONTINUE_KEY) of
                            undefined -> [];
                            Q -> Q
                        end,
                    put(?PENDING_MARKER_KEY, true),
                    put(?PENDING_CONTINUE_KEY, [{ContSelector, Args} | Queue]),
                    ok;
                {error, _} = Error ->
                    Error
            end
    end.

-spec validate_continuation(atom(), term(), none | {args, term()}) ->
    {ok, list()} | {error, #beamtalk_error{}}.
validate_continuation(RequestSelector, ContSelector, _ArgSpec) when not is_atom(ContSelector) ->
    {error,
        continuation_error(
            type_error, RequestSelector, <<"Continuation selector must be a Symbol">>
        )};
validate_continuation(RequestSelector, _ContSelector, {args, Args}) when not is_list(Args) ->
    {error,
        continuation_error(
            type_error, RequestSelector, <<"Continuation arguments must be a List">>
        )};
validate_continuation(RequestSelector, ContSelector, ArgSpec) ->
    Args =
        case ArgSpec of
            none -> [];
            {args, A} -> A
        end,
    Expected = beamtalk_class_builder:selector_arity(ContSelector),
    case length(Args) of
        Expected ->
            {ok, Args};
        Actual ->
            {error,
                beamtalk_error:with_details(
                    continuation_error(
                        arity_mismatch,
                        RequestSelector,
                        beamtalk_error:chars_to_binary(
                            io_lib:format(
                                "Continuation #~ts takes ~B argument(s), got ~B",
                                [atom_to_binary(ContSelector), Expected, Actual]
                            )
                        )
                    ),
                    #{continuation => ContSelector, expected => Expected, actual => Actual}
                )}
    end.

-doc """
BT-3968: the mailbox-boundary guard. `self continueWith:` is a direct,
in-process send and never arrives through the mailbox, so a call or cast
*message* that requests a continuation — `continueWith:` /
`continueWith:withArguments:` itself, or a `perform:` /
`perform:withArguments:` / `perform:withArguments:timeout:` naming one,
however deeply nested — was sent by another process. Once dispatched it would run inside the actor,
where request_continuation/4's `Pid =:= self()` check cannot tell, so the
handle_call/handle_cast entry rejects it here instead. Returns `ok`, or
`{error, #beamtalk_error{kind = invalid_continuation}}` (also logged, since
a rejected cast has no caller to tell).
""".
-spec mailbox_continuation_check(atom(), term()) -> ok | {error, #beamtalk_error{}}.
mailbox_continuation_check(Selector, _Args) when
    not ?IS_RESTRICTED_OR_PERFORM_SELECTOR(Selector)
->
    %% BT-4265: an ordinary selector names no restricted target — answered
    %% from the guard, without the restricted_target/2 walk.
    ok;
mailbox_continuation_check(Selector, Args) ->
    case restricted_target(Selector, Args) of
        none -> ok;
        deferReply -> rejected_mailbox_defer();
        Target -> rejected_mailbox_continuation(Target)
    end.

%% The restricted selector a message requests — a continuation selector or
%% BT-4090's `deferReply` — unwrapping nested `perform:` forms
%% (`perform: #perform:withArguments: withArguments: #(#continueWith:, ...)`),
%% or `none`. Both are in-process-only sends (`self continueWith:`,
%% `self deferReply`), so one arriving through the mailbox came from another
%% process.
-spec restricted_target(term(), term()) -> atom() | none.
restricted_target(Selector, Args) ->
    case performed_selector(Selector, Args) of
        Target when ?IS_CONTINUATION_SELECTOR(Target) -> Target;
        deferReply -> deferReply;
        _ -> none
    end.

-doc """
The selector a send of `Selector` with `Args` actually runs: `Selector`
itself, or, for a `perform:` / `perform:withArguments:` /
`perform:withArguments:timeout:` send, the selector it names, unwrapping
nested `perform:` forms to any depth
(`perform: #perform:withArguments: withArguments: #(#unlock:, ...)` runs
`unlock:`). Shared by restricted_target/2 and `beamtalk_actor_redaction`
(BT-4567), which treats a performed secret selector as a secret send.
""".
-spec performed_selector(term(), term()) -> term().
performed_selector(Selector, [Target | Rest]) when
    ?IS_PERFORM_SELECTOR(Selector), is_atom(Target)
->
    InnerArgs =
        case Rest of
            [ArgList | _] when is_list(ArgList) -> ArgList;
            _ -> []
        end,
    performed_selector(Target, InnerArgs);
performed_selector(Selector, _Args) ->
    Selector.

-spec rejected_mailbox_continuation(atom()) -> {error, #beamtalk_error{}}.
rejected_mailbox_continuation(Selector) ->
    Error = invalid_continuation_error(external, Selector),
    ?LOG_WARNING("Rejected continuation request from another process", #{
        selector => Selector, domain => [beamtalk, runtime]
    }),
    {error, Error}.

-spec rejected_mailbox_defer() -> {error, #beamtalk_error{}}.
rejected_mailbox_defer() ->
    ?LOG_WARNING("Rejected deferReply request from another process", #{
        selector => deferReply, domain => [beamtalk, runtime]
    }),
    {error, invalid_defer_error(external)}.

-spec invalid_defer_error(external | no_call) -> #beamtalk_error{}.
invalid_defer_error(Why) ->
    Message =
        case Why of
            external ->
                <<"deferReply can only be sent to self, from inside the actor's own methods">>;
            no_call ->
                <<"deferReply is only valid inside a sync method (a call in flight)">>
        end,
    beamtalk_error:with_hint(
        beamtalk_error:with_message(
            beamtalk_error:new(invalid_defer, 'Actor', deferReply), Message
        ),
        <<"Use `self deferReply` inside a method that was sent synchronously">>
    ).

-spec invalid_continuation_error(term(), atom()) -> #beamtalk_error{}.
invalid_continuation_error(Pid, RequestSelector) ->
    Message =
        case Pid =:= self() of
            true ->
                <<"continueWith: is only valid inside one of the actor's own message handlers">>;
            false ->
                <<"continueWith: can only be sent to self, from inside the actor's own methods">>
        end,
    beamtalk_error:with_hint(
        continuation_error(invalid_continuation, RequestSelector, Message),
        <<"Use `self continueWith: #selector` inside a method of the actor itself">>
    ).

-spec continuation_error(atom(), atom(), binary()) -> #beamtalk_error{}.
continuation_error(Kind, RequestSelector, Message) ->
    beamtalk_error:with_message(beamtalk_error:new(Kind, 'Actor', RequestSelector), Message).

-spec pending_stop_result(term(), term()) -> term().
pending_stop_result({reply, Reply, State}, Reason) -> {stop, Reason, Reply, State};
%% A `{continue, _}` / timeout action is dropped: once the actor has asked to
%% stop, no further step runs.
pending_stop_result({reply, Reply, State, _Action}, Reason) -> {stop, Reason, Reply, State};
pending_stop_result({noreply, State}, Reason) -> {stop, Reason, State};
pending_stop_result({noreply, State, _Action}, Reason) -> {stop, Reason, State};
%% Any other value — already a `{stop, ...}` — wins over the request.
pending_stop_result(Other, _Reason) -> Other.

-doc """
BT-3967: entry point for `Actor`'s quoted `@intrinsic` bodies, reached via
`bt@stdlib@actor:dispatch/3` when the message was not intercepted at the
send site — in practice a self-send (`self stop`, `self stopWith: r`,
`self pid`, ...) from inside one of the actor's own methods, which
dispatches through the class hierarchy rather than sync_send/3. Each
intrinsic maps back onto the matching send-site selector so both paths
share one implementation; the stop family defers via stop_actor/3 because
`Self`'s pid is the calling process.
""".
-spec dispatch(atom(), list(), #beamtalk_object{}) -> term().
dispatch(Intrinsic, Args, #beamtalk_object{class = Class, pid = Pid}) ->
    case intrinsic_selector(Intrinsic) of
        undefined ->
            beamtalk_exception_handler:reraise(
                beamtalk_error:new(does_not_understand, Class, Intrinsic)
            );
        Selector ->
            sync_send(Pid, Selector, Args)
    end.

%% Every selector here must have its own sync_send/3 clause ahead of the
%% generic Layer 1 self-send clause; otherwise a self-send would re-enter the
%% same Actor method through self_dispatch/2 and loop. The eunit test
%% bt3967_every_actor_intrinsic_is_mapped_test checks actor.bt against this.
-spec intrinsic_selector(atom()) -> atom() | undefined.
intrinsic_selector(actorStop) -> stop;
intrinsic_selector(actorStopWith) -> 'stopWith:';
intrinsic_selector(actorStopWithTimeout) -> 'stopWith:timeout:';
intrinsic_selector(actorKill) -> kill;
intrinsic_selector(actorPid) -> pid;
intrinsic_selector(actorMonitor) -> monitor;
intrinsic_selector(actorOnExit) -> 'onExit:';
intrinsic_selector(actorIsAlive) -> isAlive;
intrinsic_selector(actorDelegate) -> delegate;
intrinsic_selector(actorContinueWith) -> 'continueWith:';
intrinsic_selector(actorContinueWithArgs) -> 'continueWith:withArguments:';
intrinsic_selector(_) -> undefined.

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
    Error#beamtalk_error{
        message = beamtalk_primitive:string_from_binary(
            <<"delegate called on a non-native Actor">>
        )
    }.

-doc """
Emit an actor lifecycle telemetry event (`[beamtalk, actor, lifecycle, _]`)
and mirror `start`/`stop` into ActorSpawned/ActorStopped announcements.
Called from compiled actor `init/1`/`terminate/2` and the runtime's own
spawn/stop/kill paths. The telemetry emit itself is
`beamtalk_telemetry:emit/3` (BT-4025); this function only adds the
actor-specific announcement.
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
    beamtalk_telemetry:emit(EventName, Measurements, Metadata).

-doc """
Resolve an actor's class name from the instance registry's pid-keyed index
(`beamtalk_object_instances:class_of/1`): one `ets:lookup`, O(1) however
many actors are live (BT-4113). Returns 'unknown' for a non-Beamtalk pid
and for a remote pid — the registry is populated only on the node that
spawned the actor. A caller that already holds the class (from a
`#beamtalk_object{}` record) passes it to the `*_known` send functions
instead (ADR 0126 §7.3), so the send path never calls this.
""".
-spec lookup_class(pid()) -> atom().
lookup_class(Pid) ->
    case beamtalk_object_instances:class_of(Pid) of
        undefined -> unknown;
        Class -> Class
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
Called by beamtalk_trace_store:record_dispatch_stop/2 to merge causal IDs into trace event metadata.
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

Returns an extensible map holding only the context that is actually present
(BT-4113), so a send from a process with no context carries `#{}`:
- `otel`: the OTel trace context, when `otel_ctx` is loaded and the current
  context is non-empty
- `trace_ctx`: the application-level trace context (set via
  set_trace_context/1), when non-empty
- `causal`: causal trace IDs, when tracing has set them
The receiver (restore_propagated_ctx/1) treats a missing key as "none" and
clears what it has, so omitting an empty key loses nothing.
""".
-spec get_propagated_ctx() -> map().
get_propagated_ctx() ->
    WithOtel =
        case get_otel_ctx() of
            undefined -> #{};
            Otel when Otel =:= #{} -> #{};
            Otel -> #{otel => Otel}
        end,
    WithTrace =
        case get('$beamtalk_trace_ctx') of
            TraceCtx when is_map(TraceCtx), map_size(TraceCtx) > 0 ->
                WithOtel#{trace_ctx => TraceCtx};
            _ ->
                WithOtel
        end,
    %% Include causal trace IDs for cross-actor linking.
    %% Only present when tracing is enabled and dispatch_span has run.
    case get('$beamtalk_trace_id') of
        undefined ->
            WithTrace;
        TraceId ->
            SpanId = get('$beamtalk_span_id'),
            WithTrace#{causal => #{trace_id => TraceId, span_id => SpanId}}
    end.

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
propagated context map. Restores:
- OTel trace context: attaches to otel_ctx so spans are linked to caller's trace
- Application-level trace context: restores key-value pairs set via
  set_trace_context/1 and updates OTP logger process metadata
- Causal trace IDs and the sync call stack

A key that is absent (or, from an older sender, an empty `trace_ctx`) means
the sender had no such context, so whatever this process kept from a
previous message is cleared — no stale context leaks across messages. The
clearing is skipped when this process has nothing to clear (BT-4113), so a
message with no context costs no logger calls.
""".
-spec restore_propagated_ctx(map()) -> ok.
restore_propagated_ctx(PropCtx) when is_map(PropCtx) ->
    %% Restore OTel context if present; otherwise drop a context left by a
    %% previous message (the sender omits an empty one, BT-4113).
    case maps:get(otel, PropCtx, undefined) of
        undefined ->
            %% The common case (OpenTelemetry not loaded) costs one BIF.
            case erlang:function_exported(otel_ctx, get_current, 0) of
                false -> ok;
                true -> clear_stale_otel_ctx()
            end;
        OtelCtx ->
            case erlang:function_exported(otel_ctx, attach, 1) of
                true ->
                    erlang:apply(otel_ctx, attach, [OtelCtx]),
                    ok;
                false ->
                    ok
            end
    end,
    %% Restore application-level trace context if present. When it is
    %% absent or empty, clear any previously restored context so stale
    %% metadata never leaks across messages — but only when there is one.
    case maps:get(trace_ctx, PropCtx, undefined) of
        TraceCtx when is_map(TraceCtx), map_size(TraceCtx) > 0 ->
            clear_trace_context_if_set(),
            set_trace_context(TraceCtx);
        _ ->
            clear_trace_context_if_set()
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
            %% dispatches or get_propagated_ctx/0. dispatch_span sets the
            %% trace id whenever it sets a span id, so no trace id means
            %% nothing to clear.
            case get('$beamtalk_trace_id') of
                undefined ->
                    ok;
                _ ->
                    erase('$beamtalk_trace_id'),
                    erase('$beamtalk_span_id'),
                    erase('$beamtalk_parent_span_id')
            end
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

-doc "Clear the application-level trace context only when this process has one.".
-spec clear_trace_context_if_set() -> ok.
clear_trace_context_if_set() ->
    case get('$beamtalk_trace_ctx') of
        undefined -> ok;
        _ -> clear_trace_context()
    end.

-doc """
Detach a non-empty OTel context left by a previous message when the
incoming one carries none. Older senders always sent `otel => #{}` (which
attached the empty context); BT-4113 senders omit an empty context, so the
receiver clears instead. A no-op when OpenTelemetry is not loaded.
""".
-spec clear_stale_otel_ctx() -> ok.
clear_stale_otel_ctx() ->
    case get_otel_ctx() of
        undefined ->
            ok;
        Ctx when Ctx =:= #{} ->
            ok;
        _Stale ->
            erlang:apply(otel_ctx, attach, [#{}]),
            ok
    end.

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
                    %% ADR 0126 §4: start-time marker for
                    %% `resolve_global_conflict/3`'s "keep the older
                    %% registrant" tie-break — read back (possibly from
                    %% another node) via `actor_started_at/1`. Every actor
                    %% carries this, not just `global`-scoped ones: it's
                    %% cheap, and scope is a spawn-time choice this
                    %% process-dictionary write happens before either path
                    %% diverges.
                    erlang:put('$beamtalk_actor_started_at', erlang:system_time(microsecond)),
                    StateKeys = [
                        K
                     || K <- maps:keys(State),
                        K =/= '__methods__',
                        K =/= ClassKey,
                        K =/= '__class_mod__',
                        K =/= ?BT_INIT_ACK_KEY
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
                    %% BT-3992: no `initialize` step on the runtime path —
                    %% startup is complete once init/1 returns.
                    {ok, take_init_ack(State, initialized)};
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
An error in fire-and-forget has no caller to go to, so it crashes the
actor (ADR 0134, BT-3958; see dispatch_cast/3).
Errors in async-with-future are communicated via future rejection.
""".
-spec handle_cast(term(), map()) ->
    {noreply, map()} | {noreply, map(), {continue, term()}} | {stop, term(), map()}.
handle_cast(Msg, State) ->
    %% BT-3967 / BT-3968: honour a `self stopWith:` / `self continueWith:`
    %% requested during the dispatch.
    ok = clear_pending_actions(),
    apply_pending_actions(do_handle_cast(Msg, State)).

-doc """
BT-3968: run one queued continuation (`self continueWith:`) for a
runtime-path actor. The rest of the queue is seeded back first so any
continuation this step requests runs after them (FIFO); the step itself is
dispatched exactly like a fire-and-forget cast, sharing its error policy.
Compiled actors generate their own `handle_continue/2` with the same shape.
""".
-spec handle_continue(term(), map()) ->
    {noreply, map()} | {noreply, map(), {continue, term()}} | {stop, term(), map()}.
handle_continue({?BT_CONTINUE_TAG, [{Selector, Args} | Rest]}, State) ->
    ok = clear_pending_actions(),
    ok = seed_pending_continuations(Rest),
    apply_pending_actions(dispatch_cast(Selector, Args, State));
handle_continue(_Continue, State) ->
    {noreply, State}.

-spec do_handle_cast(term(), map()) -> {noreply, map()}.
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
do_handle_cast({'$beamtalk_wire', _, cast, _, _, _} = Msg, State) ->
    case decode_wire_cast(Msg) of
        {redispatch, Msg2} -> do_handle_cast(Msg2, State);
        noreply -> {noreply, State}
    end;
do_handle_cast({'$beamtalk_wire', _, async, _, _, _, _} = Msg, State) ->
    case decode_wire_cast(Msg) of
        {redispatch, Msg2} -> do_handle_cast(Msg2, State);
        noreply -> {noreply, State}
    end;
%% Fire-and-forget cast with propagated context (ADR 0069 Phase 2b)
do_handle_cast({cast, Selector, Args, PropCtx}, State) when
    is_atom(Selector), is_list(Args), is_map(PropCtx)
->
    restore_propagated_ctx(PropCtx),
    do_handle_cast({cast, Selector, Args}, State);
do_handle_cast({cast, Selector, Args}, State) when is_atom(Selector), is_list(Args) ->
    case mailbox_continuation_check(Selector, Args) of
        ok -> dispatch_cast(Selector, Args, State);
        {error, _} -> {noreply, State}
    end;
%% Async send with propagated context (ADR 0069 Phase 2b)
do_handle_cast({Selector, Args, FuturePid, PropCtx}, State) when is_map(PropCtx) ->
    restore_propagated_ctx(PropCtx),
    do_handle_cast({Selector, Args, FuturePid}, State);
do_handle_cast({Selector, Args, FuturePid}, State) ->
    case mailbox_continuation_check(Selector, Args) of
        ok ->
            dispatch_async(Selector, Args, FuturePid, State);
        {error, Error} ->
            maybe_reject_future(FuturePid, Error),
            {noreply, State}
    end;
do_handle_cast(Msg, State) ->
    %% Unknown cast message format - log and ignore
    ?LOG_WARNING("Unknown cast message", #{message => Msg, domain => [beamtalk, runtime]}),
    {noreply, State}.

%% Fire-and-forget dispatch for do_handle_cast/2's `{cast, ...}` clause.
-spec dispatch_cast(atom(), list(), map()) -> {noreply, map()}.
dispatch_cast(Selector, Args, State) ->
    Self = make_self(State),
    ?LOG_DEBUG("Actor dispatch (cast fire-and-forget)", #{
        class => beamtalk_tagged_map:class_of(State, unknown),
        selector => Selector,
        mode => cast,
        domain => [beamtalk, runtime]
    }),
    T0 = erlang:monotonic_time(microsecond),
    %% Stash State for re-entrant self-sends (Layer 1)
    OldState = beamtalk_actor_state:install(State),
    try
        case beamtalk_actor_state:adapt(dispatch(Selector, Args, Self, State)) of
            {reply, _Result} ->
                %% Fire-and-forget: result is discarded
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, cast, T0),
                {noreply, NewState};
            noreply ->
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, cast, T0),
                {noreply, NewState};
            {error, Reason} ->
                %% BT-3958 (ADR 0134): no reply channel, so the error
                %% crashes the actor (gen_server's crash report names the
                %% message; terminate/2 sees the error as its reason). It is
                %% raised as a wrapped Exception object, the same reason a
                %% compiled actor's generated no-reply error arms raise
                %% (`noreply_error_arms` in the codegen's
                %% gen_server/callbacks.rs).
                beamtalk_exception_handler:reraise(Reason)
        end
    after
        restore_dispatch_pdict(OldState)
    end.

%% Async-with-future dispatch for do_handle_cast/2's `{Selector, Args, FuturePid}` clause.
-spec dispatch_async(atom(), list(), pid(), map()) -> {noreply, map()}.
dispatch_async(Selector, Args, FuturePid, State) ->
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
    OldState = beamtalk_actor_state:install(State),
    try
        case beamtalk_actor_state:adapt(dispatch(Selector, Args, Self, State)) of
            {reply, Result} ->
                %% Resolve the future with the result
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_resolve_future(FuturePid, Result),
                {noreply, NewState};
            noreply ->
                %% Method didn't return a value, resolve with nil
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_resolve_future(FuturePid, nil),
                {noreply, NewState};
            {error, Reason} ->
                NewState = beamtalk_actor_state:take(),
                %% Method failed, reject the future.
                %% BT-3968: the failed dispatch's continuations are void.
                ok = discard_pending_continuations(),
                log_dispatch_complete(State, NewState, Selector, async, T0),
                maybe_reject_future(FuturePid, Reason),
                {noreply, NewState}
        end
    after
        restore_dispatch_pdict(OldState)
    end.

-doc """
Handle synchronous messages (call).
Message format: {Selector, Args} or {Selector, Args, PropCtx} (ADR 0069 Phase 2b)
Dispatches to method and returns result immediately.
""".
-spec handle_call(term(), term(), map()) ->
    {reply, term(), map()}
    | {reply, term(), map(), {continue, term()}}
    | {noreply, map()}
    | {noreply, map(), {continue, term()}}
    | {stop, term(), term(), map()}
    | {stop, term(), map()}.
handle_call(Msg, From, State) ->
    %% BT-3967 / BT-3968: honour a `self stopWith:` / `self continueWith:`
    %% requested during the dispatch.
    ok = clear_pending_actions(),
    apply_pending_actions(do_handle_call(Msg, From, State)).

-spec do_handle_call(term(), term(), map()) -> {reply, term(), map()} | {noreply, map()}.
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
do_handle_call({'$beamtalk_wire', _, call, _, _, _} = Msg, From, State) ->
    case decode_wire_call(Msg) of
        {redispatch, Msg2} -> do_handle_call(Msg2, From, State);
        {reply, ReplyVal} -> {reply, ReplyVal, State}
    end;
%% Sync call with propagated context (ADR 0069 Phase 2b)
do_handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    restore_propagated_ctx(PropCtx),
    do_handle_call({Selector, Args}, From, State);
do_handle_call({Selector, Args}, From, State) ->
    case mailbox_continuation_check(Selector, Args) of
        ok -> dispatch_call(Selector, Args, From, State);
        {error, Error} -> {reply, encode_reply_for(From, Selector, {error, Error}), State}
    end;
do_handle_call(Msg, _From, State) ->
    %% Unknown call message format
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_details(Error0, #{raw_message => Msg}),
    Error = beamtalk_error:with_hint(Error1, <<"Expected {Selector, Args} tuple">>),
    {reply, {error, Error}, State}.

%% Sync dispatch for do_handle_call/3's `{Selector, Args}` clause.
-spec dispatch_call(atom(), list(), term(), map()) -> {reply, term(), map()} | {noreply, map()}.
dispatch_call(Selector, Args, From, State) ->
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
    OldState = beamtalk_actor_state:install(State),
    %% BT-4090: expose `From` to `self deferReply`.
    ok = install_call_from(From),
    try
        case beamtalk_actor_state:adapt(dispatch(Selector, Args, Self, State)) of
            {reply, Result} ->
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                call_reply(From, Selector, Result, NewState);
            noreply ->
                %% Method didn't return a value, return nil
                NewState = beamtalk_actor_state:take(),
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                call_reply(From, Selector, nil, NewState);
            {error, Reason} ->
                NewState = beamtalk_actor_state:take(),
                %% Method failed, return error tuple.
                %% BT-3968: the failed dispatch's continuations are void.
                ok = discard_pending_continuations(),
                log_dispatch_complete(State, NewState, Selector, sync, T0),
                {reply, encode_reply_for(From, Selector, {error, Reason}), NewState}
        end
    after
        restore_dispatch_pdict(OldState)
    end.

%% BT-4090: a deferred call (`self deferReply`) is answered `{noreply, _}`
%% *before* encoding — the ignored return value must not be encoded (an
%% unencodable filler would otherwise replace the parked caller's reply).
-spec call_reply(term(), atom(), term(), map()) -> {reply, term(), map()} | {noreply, map()}.
call_reply(From, Selector, Result, NewState) ->
    case uninstall_call_from() of
        true -> {noreply, NewState};
        false -> {reply, encode_reply_for(From, Selector, Result), NewState}
    end.

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
recognise (ADR 0126 §5.1) — request direction, via the shared
`beamtalk_wire:wire_version_unsupported_error/4` (AGENTS.md "No duplicate
implementations"; `beamtalk_future:decode_wire_reply/1` builds the
reply-direction counterpart through the same constructor).
""".
-spec wire_version_unsupported_error(atom(), term()) -> #beamtalk_error{}.
wire_version_unsupported_error(Selector, SentVersion) ->
    beamtalk_wire:wire_version_unsupported_error(unknown, Selector, SentVersion, #{}).

-doc """
Recognise and decode a `'\$beamtalk_wire'` call envelope (ADR 0126 §5.1) —
the single place this decode/version-check logic lives (AGENTS.md "No
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
    beamtalk_telemetry:emit([beamtalk, dist, wire_rejected], #{count => 1}, #{
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
handle_linked_exit(Msg, State) ->
    handle_linked_exit(Msg, State, false).

-doc """
BT-4052: `handle_linked_exit/2` with a baked hook flag.

`HasHook` is `true` only in a `Server` subclass whose class (or an ancestor
other than `Server`) defines `onLinkedExit:reason:` — decided at codegen time
and baked into the generated `handle_info/2`. With the flag set, *every*
non-parent `'EXIT'` (normal and abnormal) returns `{hook, Pid, Reason}`; the
generated code dispatches it to `onLinkedExit:reason:` (same error arms and
state commit as `handleInfo:`) and the EXIT never reaches `handleInfo:`.
With the flag clear this is exactly `handle_linked_exit/2`.
""".
-spec handle_linked_exit(term(), map(), boolean()) ->
    {stop, term(), map()} | {noreply, map()} | {hook, term(), term()} | pass.
handle_linked_exit({'EXIT', From, Reason}, _State, true) ->
    {hook, From, Reason};
handle_linked_exit({'EXIT', _From, normal}, State, false) ->
    {noreply, State};
handle_linked_exit({'EXIT', _From, Reason}, State, false) ->
    {stop, Reason, State};
handle_linked_exit(_Msg, _State, _HasHook) ->
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

`{shutdown, global_name_conflict}` is its own Symbol (ADR 0126 §4,
Phase 5, BT-3603): `resolve_global_conflict/3` stops the losing side of a
`global` name conflict with exactly this reason (never `exit(Pid, kill)`,
which would skip `terminate/2` entirely and report `killed` — OTP's own
default resolver's behaviour, which this replaces), specifically so it
reads distinctly from an ordinary supervisor-initiated `shutdown`.
""".
-spec normalize_stop_reason(term()) -> normal | shutdown | crashed | 'globalNameConflict'.
normalize_stop_reason(normal) -> normal;
normalize_stop_reason(shutdown) -> shutdown;
normalize_stop_reason({shutdown, global_name_conflict}) -> 'globalNameConflict';
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
    %% BT-3633: every actor is tracked by the runtime-owned registry so
    %% `Node current actors` holds in every boot context (fire-and-forget).
    ok = beamtalk_repl_actors:track_spawned(Pid, Class),
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
                beamtalk_announcements:system_announce_running(EventClass, Fields)
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
notify_state_change(State, State) ->
    %% BT-4265: an unchanged state (a read) has no changed keys to publish,
    %% so the watch lookup is skipped: `maybe_publish_state_change/2`
    %% publishes nothing for an empty diff either way.
    ok;
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
    UserKeys = [K || K <- AllKeys -- InternalKeys, not beamtalk_tagged_map:is_local_temp_key(K)],
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
    case [K || K <- maps:keys(State), beamtalk_tagged_map:is_local_temp_key(K)] of
        [] -> State;
        LocalKeys -> maps:without(LocalKeys, State)
    end.

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
            %% bt-catcher-audit: not-applicable-other-process - method runs in the actor process,
            %% no class home entry
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
            %% bt-catcher-audit: not-applicable-other-process - method runs in the actor process,
            %% no class home entry
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
    %% bt-catcher-audit: not-applicable-other-process - DNU handler runs in the actor process, no
    %% class home entry
    try
        DnuFun(DnuArgs, Self, State)
    catch
        error:#beamtalk_error{} = BtError:_ ->
            {error, BtError, State};
        Class:Reason:Stacktrace ->
            wrap_dnu_handler_error(hd(DnuArgs), State, Class, Reason, Stacktrace)
    end;
call_dnu_handler(DnuFun, DnuArgs, _Self, State, 2) ->
    %% bt-catcher-audit: not-applicable-other-process - DNU handler runs in the actor process, no
    %% class home entry
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
        throw:Nlr when ?IS_NLR(Nlr) ->
            %% BT-4593: the walk re-raises a `^` from an invoked method
            %% instead of wrapping it. Relay it as the reply, exactly as
            %% dispatch_user_method/4 does for an own method, so a `^` aimed
            %% at a caller in another process never escapes handle_call.
            {error, Nlr, State};
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
    Error = beamtalk_error:does_not_understand(ClassName, Selector),
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
identification a second time for generated dispatch (AGENTS.md "No
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
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "No matching clause in ~ts>>~ts (called ~ts:~ts/~ts~ts)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarg, [{M, F, A, Loc} | _]) ->
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "Bad argument in ~ts>>~ts (called ~ts:~ts/~ts~ts)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, undef, [{M, F, A, Loc} | _]) ->
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "Undefined function in ~ts>>~ts (called ~ts:~ts/~ts~ts)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, error, badarith, [{M, F, A, Loc} | _]) ->
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "Arithmetic error in ~ts>>~ts (called ~ts:~ts/~ts~ts)",
            [ClassName, Selector, M, F, format_arity(A), format_location(Loc)]
        )
    );
format_method_error_message(ClassName, Selector, Class, Reason, _Stacktrace) ->
    beamtalk_error:chars_to_binary(
        io_lib:format(
            "~tp:~tp in ~ts>>~ts",
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
                File -> io_lib:format(" at ~ts:~B", [File, Line])
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
%%% Delegates kept for wire / persisted-MFA compatibility (BT-4701)
%%% =====================================================================
%%%
%%% One-line forwards to the modules the named-registration, remote-spawn and
%%% stdlib-shim code moved to. Kept under this module's name for at least one
%%% release: they are `erpc` targets, OTP supervisor child MFAs, or the resolve
%%% function registered with `global`, or an MFA recorded by an earlier release.

'spawnAs'(A1, A2) -> beamtalk_actor_registry:'spawnAs'(A1, A2).
'spawnAs'(A1, A2, A3) -> beamtalk_actor_registry:'spawnAs'(A1, A2, A3).
'spawnAsGlobal'(A1, A2) -> beamtalk_actor_registry:'spawnAsGlobal'(A1, A2).
'spawnAsGlobal'(A1, A2, A3) -> beamtalk_actor_registry:'spawnAsGlobal'(A1, A2, A3).
resolve_global_conflict(A1, A2, A3) -> beamtalk_actor_registry:resolve_global_conflict(A1, A2, A3).
started_at_from_dictionary(A1) -> beamtalk_actor_registry:started_at_from_dictionary(A1).
pid_class_candidates(A1) -> beamtalk_actor_registry:pid_class_candidates(A1).
pid_class_name(A1) -> beamtalk_actor_registry:pid_class_name(A1).
remote_spawn_target(A1, A2) -> beamtalk_actor_remote:remote_spawn_target(A1, A2).
remote_spawn_with_target(A1, A2, A3) -> beamtalk_actor_remote:remote_spawn_with_target(A1, A2, A3).
remote_named_target(A1, A2) -> beamtalk_actor_remote:remote_named_target(A1, A2).
'allRegistered'(A1) -> beamtalk_actor_ffi:'allRegistered'(A1).
unregister(A1) -> beamtalk_actor_ffi:unregister(A1).

%%% =====================================================================
%%% Deferred reply FFI shims (`self deferReply`, ReplyTo)
%%% =====================================================================

-doc """
FFI shim for `Actor>>deferReply -> ReplyTo` (BT-4090).

Takes the reply token of the sync call being dispatched: marks it deferred, so
the `handle_call` success arm answers `{noreply, State}` without replying, and
returns a `ReplyTo` handle (a runtime-minted tagged map, like `Inspector`)
carrying the opaque `From`. The caller stays blocked until `replyToDeferred/2`
or `replyErrorToDeferred/2` answers it via `gen_server:reply/2`.

Raises `invalid_defer` when no sync call is in flight (a cast, `handleInfo:`
or continuation step, or outside any dispatch) — nothing would consume it.
""".
-spec takeDeferredCall(term()) -> reply_to().
takeDeferredCall(_Self) ->
    case get(?CALL_FROM_KEY) of
        undefined ->
            beamtalk_exception_handler:reraise(invalid_defer_error(no_call));
        {deferred, From} ->
            reply_to_handle(From);
        From ->
            put(?CALL_FROM_KEY, {deferred, From}),
            reply_to_handle(From)
    end.

-spec reply_to_handle(term()) -> reply_to().
reply_to_handle(From) ->
    #{'$beamtalk_class' => 'ReplyTo', from => From}.

-doc """
FFI shim for `ReplyTo>>reply:` (BT-4090): answer a parked caller with
`Value`. Mirrors the `handle_call` success arm exactly — wire-encoded for a
remote caller, an encode failure replies `{error, EncErr}` — and answers
`gen_server:reply(From, {ok, Encoded})`. Replying to a caller that already
timed out or died, or a second reply to the same caller, is a silent no-op
(OTP drops it). Answers `ok`.
""".
-spec replyToDeferred(reply_to(), term()) -> ok.
replyToDeferred(#{'$beamtalk_class' := 'ReplyTo', from := From}, Value) ->
    Reply = encode_reply_for_tagged(From, 'reply:', Value),
    gen_server:reply(From, Reply),
    ok.

-doc """
FFI shim for `ReplyTo>>replyError:` (BT-4090): answer a parked caller with an
error, which the caller sees raised exactly like a failed sync call. Mirrors
the `handle_call` error arm (`{error, Encoded}`, wire-encoded when remote).
Same silent no-op rules as `replyToDeferred/2`. Answers `ok`.
""".
-spec replyErrorToDeferred(reply_to(), term()) -> ok.
replyErrorToDeferred(#{'$beamtalk_class' := 'ReplyTo', from := From}, Error) ->
    Reply =
        case encode_reply_for_tagged(From, 'replyError:', Error) of
            {ok, EncodedError} -> {error, EncodedError};
            %% Keep the flat `{error, EncErr}` shape, as the handle_call error arm does.
            {error, EncErr} -> {error, EncErr}
        end,
    gen_server:reply(From, Reply),
    ok.
