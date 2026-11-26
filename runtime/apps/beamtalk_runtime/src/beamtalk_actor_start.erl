%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_start).

%%% **DDD Context:** Actor System Context

-moduledoc """
Actor start, initialize-ack and `native:` start/stop observation, split out of `beamtalk_actor` (BT-5047).

Owns the awaited start machinery behind `beamtalk_actor:start_link/2,3`,
`safe_spawn/2` and the `safe_spawn_*` family (the `await_initialize/1,2,3,4`
wait, `start_link_and_await/3`, named starts) and the `native:` actor start
announcement and stop observer (`native_started/1`).

The entry points generated code calls (`beamtalk_actor:safe_spawn/2`,
`take_init_ack/2`, `ack_initialized/2`, ...) stay in `beamtalk_actor`; they
call into this module, so the start path costs the same number of calls as
before the split.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Default upper bound on a `global:register_name/3` (BT-5192);
%% `global_register_timeout_ms` (application env) overrides it.
-define(GLOBAL_REGISTER_TIMEOUT_MS, 5000).
-export([
    async_init_and_loop/2,
    await_initialize/1,
    await_initialize/2,
    await_initialize/3,
    native_backing_module/1,
    native_started/1,
    native_stop_observer_wait/3,
    safe_spawn_named/5,
    safe_start_native/4,
    start_link_and_await/3,
    start_monitor_and_await/3
]).

%% What a successful initialize wait does with the start monitor: `release`
%% removes it (every plain spawn), `keep` hands it to the caller
%% (safe_spawn_monitored/2, BT-4104).
-type monitor_mode() :: release | keep.

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
    case gen_server:start_monitor(Module, AckArgs, beamtalk_actor:gen_start_options()) of
        {ok, {Pid, MonRef}} -> await_initialize_or_kill_unlinked(Pid, MonRef, AckRef, Mode);
        {error, Reason} -> {error, Reason};
        ignore -> {error, ignore}
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
the start `beamtalk_supervisor:start_actor_child/3` makes for
`SupervisionSpec withName:`/`withName:scope:` children (ADR 0079; a child spec
recorded by a release before BT-4701 names `{beamtalk_actor, spawnAs, ...}` /
`{beamtalk_actor, spawnAsGlobal, ...}` instead, which the compatibility
delegates at the end of this module still forward to
`beamtalk_actor_registry` — either way this function is the start callback).
That link is not a
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
-spec safe_spawn_named(local | global, beamtalk_actor:link_mode(), atom(), module(), map()) ->
    {ok, pid()} | {error, term()}.
safe_spawn_named(Scope, Link, Name, Module, InitArgs) ->
    case native_backing_module(Module) of
        none -> safe_spawn_gen_named(Scope, Link, Name, Module, InitArgs);
        Backing -> safe_start_native(Backing, InitArgs, {Scope, Name}, Link)
    end.

%% `safe_spawn_named/5` for a compiled (non-`native:`) actor module.
-spec safe_spawn_gen_named(local | global, beamtalk_actor:link_mode(), atom(), module(), map()) ->
    {ok, pid()} | {error, term()}.

safe_spawn_gen_named(local, Link, Name, Module, InitArgs) ->
    start_link_and_await(
        fun(Args) ->
            gen_server:start_link({local, Name}, Module, Args, beamtalk_actor:gen_start_options())
        end,
        InitArgs,
        Link
    );
safe_spawn_gen_named(global, Link, Name, Module, InitArgs) ->
    case
        start_link_and_await(
            fun(Args) ->
                gen_server:start_link(Module, Args, beamtalk_actor:gen_start_options())
            end,
            InitArgs,
            Link
        )
    of
        {ok, Pid} ->
            case bounded_global_register(Name, Pid) of
                yes ->
                    {ok, Pid};
                no ->
                    gen_server:stop(Pid, normal, 5000),
                    {error, {already_started, global:whereis_name(Name)}};
                timeout ->
                    gen_server:stop(Pid, normal, 5000),
                    {error, {register_timeout, Name}}
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
-spec safe_start_native(
    module(), term(), none | {local | global, atom()}, beamtalk_actor:link_mode()
) ->
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

-spec start_native_trapped(
    module(), map(), none | {local | global, atom()}, beamtalk_actor:link_mode()
) ->
    {ok, pid()} | {error, term()}.

start_native_trapped(Backing, Config, Registration, Link) ->
    OldTrap = erlang:process_flag(trap_exit, true),
    Result =
        try Backing:start_link(Config) of
            {ok, Pid} when is_pid(Pid) -> register_native_started(Registration, Pid);
            {error, Reason} -> {error, Reason};
            ignore -> {error, ignore};
            Other -> {error, {bad_start_link_return, Other}}
        catch
            _:CrashReason -> {error, CrashReason}
        end,
    case Result of
        {ok, Started} ->
            ok = maybe_unlink_started(Link, Started),
            ok = native_started(Started);
        {error, _} ->
            ok
    end,
    erlang:process_flag(trap_exit, OldTrap),
    Result.

-doc """
BT-4970: announce a started `native:` actor as `ActorSpawned`, and arrange for
its `ActorStopped`.

A compiled actor announces both from the lifecycle telemetry in its generated
`init/1` and `terminate/2`. A `native:` backing gen_server (`beamtalk_subprocess`,
a user module) runs neither and is not ours to change, so the start path
announces for it, and an observer process monitors the pid and announces
`ActorStopped` when it goes down. The observer lives as long as the actor and
ends after that one message; it is unregistered, unlinked, unsupervised and owns
no table (ADR 0150: no registry). After setting its monitor it hibernates into
`native_stop_observer_wait/3`, so it holds no reference into this module's code
(a code purge cannot kill it) and keeps a minimal heap for the actor's whole
life. Nothing is done when the announcement bus is not running, as for a
compiled actor. A non-pid argument (a backing `start_link/1` that answered
`{ok, NotAPid}`) is ignored: announcing never fails a start that succeeded.

The class is the one the process scan reports for the pid
(`beamtalk_actor_scan:one_per_pid/1`, the rule every actor list uses), or
`unknown`, the same default a compiled actor's announcement falls back to.
""".
-spec native_started(term()) -> ok.
native_started(Pid) when is_pid(Pid) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _Bus ->
            %% Best-effort, like the announcement: the backing process has
            %% already started, so a failing lookup must not fail the start.
            Class =
                try beamtalk_actor_scan:one_per_pid(beamtalk_actor_scan:info(Pid)) of
                    [{Pid, Found, _StartedAt} | _] -> Found;
                    [] -> unknown
                catch
                    _:_ -> unknown
                end,
            ok = beamtalk_actor:do_announce_actor_lifecycle('ActorSpawned', #{
                actorClass => Class, pid => Pid
            }),
            _Observer = erlang:spawn(fun() -> native_stop_observer(Pid, Class) end),
            ok
    end;
native_started(_NotAPid) ->
    ok.

%% The observer sets its monitor when it is first scheduled, which can be after
%% `native_started/1` has returned and after the starter has handed the actor
%% back (a named start is also reachable by its name before that). An actor that
%% stops in that window gives `'DOWN'` with reason `noproc`, and its real exit
%% reason is not observable without making the start wait for the observer. It
%% is announced as `crashed` (what `normalize_stop_reason/1` makes of a reason it
%% does not know). That can report a prompt clean `stop` as a crash; it was
%% chosen over `normal` because a false alarm is cheaper than a crash reported
%% as clean.
%%
%% The announced reason is always in `normalize_stop_reason/1`'s range (`normal`,
%% `shutdown`, `crashed`, `'globalNameConflict'`).
-spec native_stop_observer(pid(), atom()) -> no_return().

native_stop_observer(Pid, Class) ->
    MonRef = erlang:monitor(process, Pid),
    erlang:hibernate(?MODULE, native_stop_observer_wait, [MonRef, Pid, Class]).

-doc false.
-spec native_stop_observer_wait(reference(), pid(), atom()) -> ok.

native_stop_observer_wait(MonRef, Pid, Class) ->
    receive
        {'DOWN', MonRef, process, Pid, Reason} ->
            beamtalk_actor:do_announce_actor_lifecycle('ActorStopped', #{
                actorClass => Class,
                pid => Pid,
                reason => beamtalk_actor:normalize_stop_reason(Reason)
            });
        _Stray ->
            %% Nothing sends this process anything; if something does, drop it and
            %% hibernate again so no stack stays in this module's code.
            erlang:hibernate(?MODULE, native_stop_observer_wait, [MonRef, Pid, Class])
    end.

%% BT-4577: register a just-started native actor under its name, or stop it
%% when the name cannot be registered.
-spec register_native_started(none | {local | global, atom()}, pid()) ->
    {ok, pid()}
    | {error,
        {already_started, pid() | port()} | {register_failed, atom()} | {register_timeout, atom()}}.

register_native_started(none, Pid) ->
    {ok, Pid};
register_native_started({local, Name} = Registration, Pid) ->
    try register(Name, Pid) of
        true -> {ok, Pid}
    catch
        error:badarg -> refuse_unregistered_native(Registration, Pid)
    end;
register_native_started({global, Name} = Registration, Pid) ->
    ok = record_native_start_time(Pid),
    case bounded_global_register(Name, Pid) of
        yes ->
            {ok, Pid};
        no ->
            refuse_unregistered_native(Registration, Pid);
        timeout ->
            ok = stop_unregistered_native(Pid),
            {error, {register_timeout, Name}}
    end.

-doc """
`global:register_name/3` with `resolve_global_conflict/3` as the conflict
resolver, waiting at most `global_register_timeout_ms` (application env,
default `?GLOBAL_REGISTER_TIMEOUT_MS`) for an answer (BT-5192).

`global` registers a name under its cluster-wide lock, and waits for as long
as another node holds that lock (a registration, a `global:trans/2`, a
partition-heal name sync). The call runs in a monitored helper so the caller
gets `timeout` instead of blocking indefinitely. The registrar may still
register the name after the deadline: every caller stops `Pid` on `timeout`,
and `global` drops a name whose process has exited, so a late registration
never outlives the refused start. A helper that crashes re-raises its exit
in the caller, as the inline call did.
""".
-spec bounded_global_register(atom(), pid()) -> yes | no | timeout.
bounded_global_register(Name, Pid) ->
    Timeout = application:get_env(
        beamtalk_runtime, global_register_timeout_ms, ?GLOBAL_REGISTER_TIMEOUT_MS
    ),
    Caller = self(),
    Tag = make_ref(),
    {Helper, Ref} = spawn_monitor(fun() ->
        Caller !
            {Tag,
                global:register_name(
                    Name, Pid, fun ?BT_ACTOR_WIRE_MODULE:resolve_global_conflict/3
                )}
    end),
    receive
        {Tag, Answer} ->
            erlang:demonitor(Ref, [flush]),
            Answer;
        {'DOWN', Ref, process, Helper, Reason} ->
            exit(Reason)
    after Timeout ->
        exit(Helper, kill),
        erlang:demonitor(Ref, [flush]),
        receive
            {Tag, Late} -> Late
        after 0 ->
            ?LOG_WARNING("Global name registration did not finish in time", #{
                name => Name, timeout_ms => Timeout, domain => [beamtalk, runtime]
            }),
            timeout
        end
    end.

%% BT-4956: give a `native:` actor the `'$beamtalk_actor_started_at'` key that a
%% compiled actor's generated `init/1` writes (BT-4937), so
%% `resolve_global_conflict/3` can keep the older of two registrants. The backing
%% module's process has no generated `init/1`, and a process-dictionary key can
%% only be written from inside the process, so the write runs there through
%% `sys:replace_state/3` (the backing module is a `gen_server`, ADR 0056) with
%% the state returned unchanged. It happens here, before `global:register_name/3`,
%% so the key exists before the name can conflict; and only for `global`, the
%% one scope the resolver reads, so anonymous and local starts pay nothing. A key
%% the backing module already wrote is left alone. A backing process that does
%% not answer the system message within a second stays untimed until it drains
%% its queue, and meanwhile behaves as before BT-4956: the other side wins, or
%% the lower-node tie-break if neither has one. The stamp it then writes still
%% carries the registration time, taken before the request.
-spec record_native_start_time(pid()) -> ok.

record_native_start_time(Pid) ->
    StartedAt = erlang:system_time(microsecond),
    Stamp = fun(State) ->
        case erlang:get('$beamtalk_actor_started_at') of
            undefined -> erlang:put('$beamtalk_actor_started_at', StartedAt);
            _Existing -> ok
        end,
        State
    end,
    try
        _ = sys:replace_state(Pid, Stamp, 1000),
        ok
    catch
        _:_ -> ok
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
    fun((term()) -> {ok, pid()} | {error, term()} | ignore), term(), beamtalk_actor:link_mode()
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
-spec maybe_unlink_started(beamtalk_actor:link_mode(), pid()) -> ok.

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
