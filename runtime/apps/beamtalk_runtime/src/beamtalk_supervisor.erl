%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor).

%%% **DDD Context:** Actor System Context

-moduledoc """
Erlang runtime glue for Beamtalk Supervisor and DynamicSupervisor.

This module provides the BEAM interop entry points called from the
Supervisor and DynamicSupervisor stdlib methods via Erlang FFI.
Generated `init/1` callbacks delegate to `static_init/3` and `dynamic_init/3`
(Phase 3 codegen) to avoid gen_server deadlocks. `is_supervisor/1` is used
for compile-time routing and child spec construction.

## Design (ADR 0059 Phase 2)

Supervisor instances are represented as:
  `{beamtalk_supervisor, ClassName, Module, Pid}`

This distinct tuple tag allows `beamtalk_message_dispatch` to route
messages directly to `Module:'method'(Self)` (Phase 3) or via the
stdlib hierarchy walk (Phase 2) without going through gen_server.

OTP supervisor behaviour handles `handle_call/3` internally, so inspection
methods (`children`, `which:`, etc.) are implemented as exported module
functions that call OTP APIs from the caller's process context.

## References

- ADR 0059: Supervision Tree Syntax, Phase 2
- stdlib/src/supervisor.bt, dynamic_supervisor.bt
- runtime/apps/beamtalk_runtime/src/beamtalk_message_dispatch.erl (routing)
""".

-export([
    startLink/1,
    class_start/4,
    supervise/2,
    doSpawn/1,
    doSpawnWith/2,
    doSpawnWith/3,
    doSpawnAs/2,
    named/2,
    classObject/1,
    start_supervisor_child/2,
    start_supervisor_child/3,
    start_dynamic_supervisor_child/2,
    start_dynamic_supervisor_child/3,
    start_dynamic_supervisor_child/4,
    instance_pids/0,
    instances/0,
    supervised_children_of/1,
    supervised_children_of/2,
    current/1,
    static_init/2,
    static_init/3,
    dynamic_init/2,
    dynamic_init/3,
    args/1,
    configured_flags/1,
    warn_removed_class_side_config/2,
    whichChildren/1,
    childInfos/1,
    childWithId/2,
    terminateChildId/2,
    restartChild/2,
    deleteChild/2,
    dynamicChildren/1,
    whichChild/2,
    terminateChild/2,
    'terminateChild:class:'/2,
    'terminateChild:child:'/2,
    startChild/1,
    startChild/2,
    countChildren/1,
    isAlive/1,
    stop/1,
    stopWithin/2,
    build_child_specs/1,
    spec_to_otp/1,
    spec_to_otp/2,
    is_supervisor/1,
    spawn_kind/1,
    register_root/1,
    get_root/0,
    ensure_root_table/0,
    clear_root/0,
    start_child_via_class_method/4,
    start_actor_child/2,
    start_actor_child/3,
    start_dynamic_child/2,
    start_dynamic_child/3,
    start_dynamic_child/4,
    checked_dynamic_start/1,
    checked_dynamic_start/2,
    checked_dynamic_start/3,
    checked_dynamic_start/4,
    startChild/3
]).

%% BT-5047: what the modules split out of this one (`beamtalk_supervisor_pool`,
%% `_children`, ...) call back into. Exported for the split only.
-export([
    sup_dictionary_value/2,
    with_live_supervisor/3
]).

-ifdef(TEST).
-export([
    check_max_children/2,
    classify_call_failure/2,
    to_otp_shutdown/3,
    to_otp_strategy/1,
    check_stop_timeout/2,
    init_child_capacity/1,
    wrap_capped_child_spec/3
]).
-endif.

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include("beamtalk_supervisor_internal.hrl").
-include_lib("kernel/include/logger.hrl").

%% Why a supervisor handle call could not reach its supervisor (BT-3984):
%% `dead` (no such process: `stale_handle`) or `{unreachable, Why}` (its node
%% is down, disconnected or did not answer in time: `node_down`).
-export_type([supervisor_unavailable/0, which_child/0, supervisor_child/0]).

-type supervisor_unavailable() :: dead | {unreachable, nodedown | noconnection | timeout}.
%% One `supervisor:which_children/1` entry.
-type which_child() :: {
    term(), pid() | restarting | undefined, worker | supervisor, [module()] | dynamic
}.
%% A `SupervisorChild` value (BT-3966, `stdlib/src/supervisor_child.bt`). The
%% exact `'$beamtalk_class'` field lets the type checker infer FFI results as
%% `SupervisorChild` (see `beamtalk_process_navigation`'s `node_map()`).
-type supervisor_child() :: #{
    ?BT_CLASS_TAG_KEY := 'SupervisorChild',
    id := term(),
    child := beamtalk_object() | pid() | nil,
    status := running | restarting | stopped,
    type := worker | supervisor,
    childClass := beamtalk_object() | nil
}.

%% ETS table name for the OTP application root supervisor registry.
%% Stores `{root, SupervisorTuple}` where SupervisorTuple is a
%% `{beamtalk_supervisor, ClassName, Module, Pid}` value.
-define(ROOT_SUPERVISOR_TABLE, beamtalk_root_supervisor).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
FFI shim for `class supervise` on Supervisor and DynamicSupervisor (ADR 0135 §1).

`Self` is the class object `{beamtalk_object, 'ClassName class', Module,
ClassPid}`. Delegates to `supervise/2`, which starts the class-named
singleton in the calling process (or answers the running one) and runs its
instance-side `initialize`.

An ordinary `X supervise` send never reaches this body: the runtime routes
it straight to `class_start/4` in the caller (`beamtalk_class_dispatch`), so
the start does not run in the class gen_server. This shim serves the
remaining paths: a `self supervise` (or `super supervise`) inside one of the
class's own class methods, which runs in that class's process like the rest
of the method body, so configuration started from there must not send to the
same class.
""".
-spec startLink(beamtalk_object()) ->
    {ok, {beamtalk_supervisor, atom(), module(), pid()}} | {error, #beamtalk_error{}}.
startLink(Self) ->
    {ClassName, Module} = class_identity(Self),
    supervise(ClassName, Module).

-doc """
Start a supervisor class from a class-side spawn-family message or
`supervise`, in the calling process (ADR 0135 §1).

The single runtime entry for `supervise`, `spawn`, `spawnWith:`, `spawnAs:`
and `spawnWith:as:` on a Supervisor or DynamicSupervisor class. Codegen calls
it directly for `ClassRef spawn…` and `self spawn…` sends on supervisor
classes, and `beamtalk_class_dispatch:class_send/3` calls it for every other
send of those selectors to a supervisor class, so the tree start never runs
in the class gen_server (where it would serialise every other class send
behind the start and deadlock any child that messages the class).

Returns what the corresponding stdlib method answers: a supervisor handle
for `spawn`/`spawnWith:` (raising `#supervisor_start_failed` on failure), and
a `Result` for `supervise`, `spawnAs:` and `spawnWith:as:`. An abstract
receiver (`Supervisor spawn`) raises `#instantiation_error`.
""".
-spec class_start(atom(), module(), atom(), list()) -> term().
class_start(ClassName, Module, Selector, Args) ->
    case beamtalk_class_metadata:lookup_is_abstract(ClassName) of
        {ok, true} ->
            beamtalk_error:raise(
                beamtalk_class_instantiation:abstract_class_error(ClassName, Selector)
            );
        _ ->
            class_start_concrete(ClassName, Module, Selector, Args)
    end.

-spec class_start_concrete(atom(), module(), atom(), list()) -> term().
class_start_concrete(ClassName, Module, supervise, []) ->
    beamtalk_result:from_tagged_tuple(supervise(ClassName, Module));
class_start_concrete(ClassName, Module, spawn, []) ->
    spawn_instance(ClassName, Module, nil, spawn);
class_start_concrete(ClassName, Module, 'spawnWith:', [Args]) ->
    spawn_instance(ClassName, Module, Args, 'spawnWith:');
class_start_concrete(ClassName, Module, 'spawnAs:', [Name]) ->
    beamtalk_result:from_tagged_tuple(
        spawn_named_instance(ClassName, Module, nil, Name, 'spawnAs:')
    );
class_start_concrete(ClassName, Module, 'spawnWith:as:', [Args, Name]) ->
    beamtalk_result:from_tagged_tuple(
        spawn_named_instance(ClassName, Module, Args, Name, 'spawnWith:as:')
    );
class_start_concrete(ClassName, _Module, Selector, _Args) when
    Selector =:= spawnMonitored;
    Selector =:= 'spawnMonitoredWith:';
    Selector =:= spawnMonitoredAsync;
    Selector =:= 'spawnMonitoredAsyncWith:'
->
    %% BT-4104: `Actor>>spawnMonitored`'s atomic start+monitor has no OTP
    %% supervisor counterpart; codegen routes a static send of it to a
    %% supervisor class here instead of to a `spawn_monitored` export the
    %% supervisor module does not have.
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            beamtalk_error:new(instantiation_error, ClassName, Selector),
            iolist_to_binary([
                atom_to_binary(Selector),
                <<
                    " is not supported for supervisors: start the supervisor, "
                    "then monitor its pid"
                >>
            ])
        )
    );
class_start_concrete(ClassName, _Module, Selector, Args) ->
    %% A dynamic send with the wrong argument count (`perform:withArguments:`).
    beamtalk_error:raise(
        beamtalk_error:new_fmt(
            arity_mismatch,
            ClassName,
            Selector,
            "~ts takes a different number of arguments, got ~b",
            [
                Selector, length(Args)
            ]
        )
    ).

-doc """
`supervise`: ensure the class-named singleton is running (ADR 0135 §1).

Starts `Module:start_link/0` (registered as `{local, Module}`, start args
`nil`) in the calling process, then unlinks it, so the tree is tied to
neither the caller nor the class process (the exdura failure: a restart
intensity exit used to take the class process down). A fresh start runs the
instance-side `initialize` here, in the caller; an already-running instance
is answered as is, after any concurrent caller's `initialize` has finished
(BT-3759).

Concurrent `supervise` calls on one class are serialised around the start
and the arming of the `initializing` gate by a node-local FIFO lock keyed by
module (`beamtalk_fifo_lock`, BT-3994): that is what the class gen_server
used to provide, so a second caller that gets `already_started` always finds
the gate armed. Waiters are served in arrival order as soon as the lock is
released; one that waits longer than `init_wait_timeout_ms` (60s by default)
for the lock, behind a start that does not finish, gets `Result error:`
`#supervisor_start_failed` instead of blocking for good. The same bound then
applies separately to waiting for a concurrent caller's `initialize`, so a
caller can wait up to twice the bound in all.

Every failure is a `Result error:` with kind `#supervisor_start_failed`,
including an instance-side `initialize` that raises (the supervisor is then
stopped) and a concurrent caller whose wait for another caller's
`initialize` fails (BT-3994; before, both raised). This matches
`Actor spawnAs:`, whose `Result` also carries a failed `initialize`.
""".
-spec supervise(atom(), module()) ->
    {ok, {beamtalk_supervisor, atom(), module(), pid()}} | {error, #beamtalk_error{}}.
supervise(ClassName, Module) ->
    %% No `whereis` fast path before the lock: the name is registered before
    %% `init/1` runs, so a running-looking singleton may still be inside its
    %% start with no gate armed yet. The lock makes such a caller wait until the
    %% gate is armed.
    Started = beamtalk_fifo_lock:with_lock(
        {beamtalk_supervise, Module},
        beamtalk_supervisor_init_gate:init_wait_timeout(),
        fun() ->
            %% Arm the gate before unlinking: if this caller dies after the
            %% unlink but before `run_initialize/1` claims the gate, the next
            %% `supervise` waits on it and reclaims the orphan at the deadline
            %% instead of answering an uninitialised singleton.
            start_unlinked(fun() ->
                case Module:start_link() of
                    {ok, Pid} = Ok ->
                        beamtalk_supervisor_init_gate:arm_init_gate(Pid),
                        Ok;
                    Other ->
                        Other
                end
            end)
        end
    ),
    case Started of
        {ok, {ok, Pid}} ->
            ?LOG_INFO("Supervisor started", #{
                supervisor => ClassName, module => Module, pid => Pid, domain => [beamtalk, runtime]
            }),
            SupTuple = {beamtalk_supervisor, ClassName, Module, Pid},
            initialize_result(
                ClassName, supervise, SupTuple, fun beamtalk_supervisor_init_gate:run_initialize/1
            );
        {ok, {error, {already_started, Pid}}} ->
            SupTuple = {beamtalk_supervisor, ClassName, Module, Pid},
            initialize_result(
                ClassName,
                supervise,
                SupTuple,
                fun beamtalk_supervisor_init_gate:await_initialized/1
            );
        {error, LockError} ->
            ?LOG_ERROR("Supervisor start lock wait failed", #{
                supervisor => ClassName,
                module => Module,
                reason => LockError,
                domain => [beamtalk, runtime]
            }),
            {error, start_lock_error(ClassName, LockError)};
        {ok, Failure} ->
            Reason =
                case Failure of
                    {error, R} -> R;
                    Other -> Other
                end,
            ?LOG_ERROR("Supervisor start failed", #{
                supervisor => ClassName,
                module => Module,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            {error,
                beamtalk_supervisor_errors:start_failed_error(
                    supervisor_start_failed, ClassName, supervise, Reason
                )}
    end.

%% A `supervise` caller that could not take the start lock (BT-3994): it gave
%% up waiting for a concurrent caller's start of the same class, or the lock
%% table was lost while it waited.
-spec start_lock_error(atom(), timeout | lost) -> #beamtalk_error{}.
start_lock_error(ClassName, timeout) ->
    beamtalk_error:new_fmt(
        supervisor_start_failed,
        ClassName,
        supervise,
        "another supervise call is still starting ~ts; gave up after ~b ms",
        [ClassName, beamtalk_supervisor_init_gate:init_wait_timeout()]
    );
start_lock_error(ClassName, lost) ->
    beamtalk_error:new(
        supervisor_start_failed,
        ClassName,
        supervise,
        <<"the supervise start lock table was lost while waiting; retry">>
    ).

%% Run `initialize` (or wait for a concurrent caller's, `Step`) and answer the
%% handle, or `{error, #supervisor_start_failed}` when it raised: the
%% `Result`-returning starts (`supervise`, `spawnAs:`, `spawnWith:as:`) report
%% a failed `initialize` in their `Result`, as `Actor spawnAs:` does (BT-3994).
%% `Step` has already stopped the supervisor. A `#supervisor_start_failed`
%% raised by `await_initialized/1` (a concurrent caller's wait failed) is
%% answered as is rather than wrapped in a second one.
-spec initialize_result(atom(), atom(), tuple(), fun((tuple()) -> ok)) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
initialize_result(ClassName, Selector, SupTuple, Step) ->
    try Step(SupTuple) of
        ok -> {ok, SupTuple}
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            case raised_error(Class, Reason, Stack) of
                #beamtalk_error{kind = supervisor_start_failed, class = ClassName} = Failed ->
                    {error, Failed#beamtalk_error{selector = Selector}};
                Cause ->
                    {error,
                        beamtalk_supervisor_errors:start_failed_error(
                            supervisor_start_failed, ClassName, Selector, Cause
                        )}
            end
    end.

%% The `#beamtalk_error{}` behind a raise: a Beamtalk exception carries it
%% under `error`; any other raise is classified as the exception handler does.
-spec raised_error(error | exit | throw, term(), list()) -> #beamtalk_error{}.
raised_error(_Class, #{error := #beamtalk_error{} = Error}, _Stack) ->
    Error;
raised_error(_Class, #beamtalk_error{} = Error, _Stack) ->
    Error;
raised_error(Class, Reason, Stack) ->
    case beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack) of
        #{error := #beamtalk_error{} = Error} -> Error;
        _ -> beamtalk_error:new(runtime_error, undefined)
    end.

-doc "FFI shim for `class spawn` (ADR 0135 §1); see `class_start/4`.".
-spec doSpawn(beamtalk_object()) -> term().
doSpawn(Self) ->
    {ClassName, Module} = class_identity(Self),
    class_start(ClassName, Module, spawn, []).

-doc "FFI shim for `class spawnWith:` (ADR 0135 §1); see `class_start/4`.".
-spec doSpawnWith(beamtalk_object(), term()) -> term().
doSpawnWith(Self, Args) ->
    {ClassName, Module} = class_identity(Self),
    class_start(ClassName, Module, 'spawnWith:', [Args]).

-doc "FFI shim for `class spawnWith:as:` (ADR 0135 §1).".
-spec doSpawnWith(beamtalk_object(), term(), term()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
doSpawnWith(Self, Args, Name) ->
    {ClassName, Module} = class_identity(Self),
    spawn_named_instance(ClassName, Module, Args, Name, 'spawnWith:as:').

-doc "FFI shim for `class spawnAs:` (ADR 0135 §1).".
-spec doSpawnAs(beamtalk_object(), term()) -> {ok, tuple()} | {error, #beamtalk_error{}}.
doSpawnAs(Self, Name) ->
    {ClassName, Module} = class_identity(Self),
    spawn_named_instance(ClassName, Module, nil, Name, 'spawnAs:').

-doc """
FFI shim for `class named: name` on supervisor classes (ADR 0135 §1).

`whereis(Name)` plus a class check, with `Actor named:`'s error kinds:
`#name_not_registered` for an unknown name, `#wrong_class` for a process that
is not a Beamtalk supervisor of the receiver class or a subclass (an actor,
say), and `#type_error` for a non-Symbol name.
""".
-spec named(beamtalk_object(), term()) -> {ok, tuple()} | {error, #beamtalk_error{}}.
named(Self, Name) when is_atom(Name) ->
    {ClassName, _Module} = class_identity(Self),
    case erlang:whereis(Name) of
        undefined ->
            {error,
                beamtalk_error:new_fmt(
                    name_not_registered,
                    ClassName,
                    'named:',
                    "No supervisor is registered under name '~ts'",
                    [Name]
                )};
        Pid ->
            case sup_dictionary_value(Pid, ?SUP_CLASS_KEY) of
                {ok, {Actual, ActualModule}} ->
                    case beamtalk_class_registry:inherits_from(Actual, ClassName) of
                        true -> {ok, {beamtalk_supervisor, Actual, ActualModule, Pid}};
                        false -> wrong_class_error(ClassName, Name, Actual)
                    end;
                _ ->
                    wrong_class_error(ClassName, Name, not_a_supervisor)
            end
    end;
named(Self, Name) ->
    {ClassName, _Module} = class_identity(Self),
    {error,
        beamtalk_error:new_fmt(
            type_error, ClassName, 'named:', "named: expects a Symbol, got ~tp", [Name]
        )}.

-spec wrong_class_error(atom(), atom(), atom()) -> {error, #beamtalk_error{}}.
wrong_class_error(ClassName, Name, not_a_supervisor) ->
    {error,
        beamtalk_error:new_fmt(
            wrong_class,
            ClassName,
            'named:',
            "Name '~ts' is not registered to a Beamtalk supervisor",
            [Name]
        )};
wrong_class_error(ClassName, Name, Actual) ->
    {error,
        beamtalk_error:new_fmt(
            wrong_class, ClassName, 'named:', "Name '~ts' is registered to a ~ts, not a ~ts", [
                Name, Actual, ClassName
            ]
        )}.

-doc """
FFI shim behind `class supervisionSpec` on Supervisor and DynamicSupervisor
(ADR 0135 §3): the receiver as its canonical class object.

An inherited class method's `self` carries the *defining* class's module
(`beamtalk_class_dispatch:apply_class_method_in_context/6`), so the stdlib
`supervisionSpec` would otherwise record `bt@stdlib@supervisor` as the
child's module and the spec's `actorClass` would not equal the class.
""".
-spec classObject(beamtalk_object()) -> beamtalk_object().
classObject(Self) ->
    beamtalk_class_registry:class_object_from_pid(element(4, Self)).

%% `{ClassName, Module}` of a class object passed to an FFI shim.
-spec class_identity(beamtalk_object()) -> {atom(), module()}.
class_identity(Self) ->
    ClassPid = element(4, Self),
    {
        beamtalk_object_class_read:class_name_safe(ClassPid),
        beamtalk_object_class:module_name_safe(ClassPid)
    }.

%% `spawn` / `spawnWith:`: start an anonymous, unlinked top-level instance and
%% run its `initialize` in the caller. Raises `#supervisor_start_failed` when
%% the start fails; an `initialize` that raises stops the instance and its
%% error propagates unchanged (these forms answer `Self`, not a `Result`).
-spec spawn_instance(atom(), module(), term(), atom()) -> tuple().
spawn_instance(ClassName, Module, Args, Selector) ->
    StartFun = fun() -> supervisor:start_link(Module, {?TOP_LEVEL_TAG, Args}) end,
    case start_top_level(ClassName, Module, StartFun) of
        {ok, SupTuple} ->
            beamtalk_supervisor_init_gate:run_initialize(SupTuple),
            SupTuple;
        {error, Reason} ->
            beamtalk_error:raise(
                beamtalk_supervisor_errors:start_failed_error(
                    supervisor_start_failed, ClassName, Selector, Reason
                )
            )
    end.

%% `spawnAs:` / `spawnWith:as:`: start a top-level instance registered as
%% `{local, Name}` (unlinked). A taken name is `#name_registered`, a reserved
%% one `#reserved_name`, a non-Symbol `#type_error`, as for `Actor spawnAs:`.
%% An `initialize` that raises stops the instance and answers
%% `{error, #supervisor_start_failed}` carrying its error, as `Actor spawnAs:`
%% answers `Result error:` for a failed `initialize` (BT-3994).
-spec spawn_named_instance(atom(), module(), term(), term(), atom()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
spawn_named_instance(ClassName, _Module, _Args, Name, Selector) when not is_atom(Name) ->
    {error,
        beamtalk_error:new_fmt(
            type_error, ClassName, Selector, "~ts expects a Symbol name, got ~tp", [Selector, Name]
        )};
spawn_named_instance(ClassName, Module, Args, Name, Selector) ->
    case beamtalk_actor_registry:reserved_name(Name) of
        true ->
            {error, beamtalk_error:supervisor_reserved_name(ClassName, Selector, Name)};
        false ->
            StartFun = fun() ->
                supervisor:start_link({local, Name}, Module, {?TOP_LEVEL_TAG, Args})
            end,
            case start_top_level(ClassName, Module, StartFun) of
                {ok, SupTuple} ->
                    initialize_result(
                        ClassName,
                        Selector,
                        SupTuple,
                        fun beamtalk_supervisor_init_gate:run_initialize/1
                    );
                {error, {already_started, _}} ->
                    {error, beamtalk_error:name_registered(ClassName, Selector, Name)};
                {error, Reason} ->
                    {error,
                        beamtalk_supervisor_errors:start_failed_error(
                            supervisor_start_failed, ClassName, Selector, Reason
                        )}
            end
    end.

%% Shared start of the spawn family: start (unlinked) and answer the handle.
%% The caller then runs `initialize` (`run_initialize/1`), raising or
%% answering a `Result` as its selector requires.
-spec start_top_level(atom(), module(), fun(() -> term())) ->
    {ok, tuple()} | {error, term()}.
start_top_level(ClassName, Module, StartFun) ->
    case start_unlinked(StartFun) of
        {ok, Pid} ->
            ?LOG_INFO("Supervisor instance started", #{
                supervisor => ClassName, module => Module, pid => Pid, domain => [beamtalk, runtime]
            }),
            {ok, {beamtalk_supervisor, ClassName, Module, Pid}};
        ignore ->
            {error, ignore};
        {error, _} = Error ->
            Error
    end.

-doc """
Run a supervisor `start_link` with exits trapped, then unlink the new
supervisor from the caller (ADR 0135 §1: top-level supervisors are unlinked,
like `Actor spawn`).

A failing `init/1` acks the caller with `{error, Reason}` and then exits
with `Reason` through the link; while trapping, that signal becomes an
`'EXIT'` message (`proc_lib` flushes it) instead of killing the caller
(BT-3965). After a successful start the link is removed, and an `'EXIT'`
from a supervisor that died in between is flushed. The previous
`trap_exit` flag is restored.
""".
-spec start_unlinked(fun(() -> term())) -> {ok, pid()} | ignore | {error, term()}.
start_unlinked(StartFun) ->
    {links, LinkedBefore} = erlang:process_info(self(), links),
    Trapping = process_flag(trap_exit, true),
    try StartFun() of
        {ok, Pid} = Ok when is_pid(Pid) ->
            unlink(Pid),
            receive
                {'EXIT', Pid, _} -> ok
            after 0 -> ok
            end,
            Ok;
        Other ->
            Other
    after
        process_flag(trap_exit, Trapping),
        _ = Trapping orelse propagate_trapped_exits(LinkedBefore)
    end.

%% The caller did not trap exits, so an exit signal that reached it through a
%% link it already had while `start_unlinked/1` was trapping would have killed
%% it. Honour that now instead of leaving the signal as an ignored message.
%% Only links that existed before the start are considered: the supervisor
%% being started (and anything `proc_lib` links during the start) are not.
-spec propagate_trapped_exits([pid() | port()]) -> true.
propagate_trapped_exits(LinkedBefore) ->
    receive
        {'EXIT', From, Reason} when Reason =/= normal ->
            case lists:member(From, LinkedBefore) of
                true -> exit(Reason);
                false -> propagate_trapped_exits(LinkedBefore)
            end
    after 0 ->
        true
    end.

-doc """
Start a nested supervisor child (ADR 0135 §3): the start MFA `spec_to_otp/2`
puts in a static parent's child spec for a supervisor child,
`{beamtalk_supervisor, start_supervisor_child, [Module, Args]}`.

Calls the child module's anonymous `start_link/1` with the spec's args
(`nil` without `withArgs:`), so a parent can hold any number of children of
one supervisor class. A module compiled before ADR 0135 exports only
`start_link/0`; it is started that way (registered under its module and
ignoring the args, as before) rather than failing with `undef` (ADR 0125).
The new child stays linked to the calling supervisor.
""".
-spec start_supervisor_child(module(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_supervisor_child(Module, Args) ->
    case exports(Module, start_link, 1) of
        true -> Module:start_link(Args);
        false -> Module:start_link()
    end.

-doc """
`start_supervisor_child/2` for a `withName:` child: `start_link/2` registers
the child as `{local, Name}`; a module without it (compiled before ADR 0135)
falls back to `start_link/0`.
""".
-spec start_supervisor_child(module(), term(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start_supervisor_child(Module, Args, Name) ->
    case beamtalk_actor_registry:reserved_name(Name) of
        true ->
            %% Same reserved-name policy as `spawnAs:` (ADR 0079).
            {error, beamtalk_error:supervisor_reserved_name('Supervisor', 'withName:', Name)};
        false ->
            case exports(Module, start_link, 2) of
                true ->
                    Module:start_link(Name, Args);
                false ->
                    ?LOG_WARNING(
                        "Supervisor child compiled before ADR 0135: withName: and args ignored",
                        #{module => Module, name => Name, domain => [beamtalk, runtime]}
                    ),
                    Module:start_link()
            end
    end.

-doc """
`simple_one_for_one` template entry for a DynamicSupervisor whose child class
is a supervisor (ADR 0135 §3): `{beamtalk_supervisor,
start_dynamic_supervisor_child, [Module, DefaultArgs]}`. OTP appends the
`startChild` arguments, so arity 2 is `startChild`, 3 `startChild: args` and
4 `startChild: args name: aName`, mirroring `start_dynamic_child/2,3,4` for
actor children. Each call starts a new anonymous child (`startChild:name:`
registers it) through `start_supervisor_child/2,3`.
""".
-spec start_dynamic_supervisor_child(module(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_dynamic_supervisor_child(Module, DefaultArgs) ->
    start_supervisor_child(Module, DefaultArgs).

-doc "`startChild: args` on a pool of supervisors; see `start_dynamic_supervisor_child/2`.".
-spec start_dynamic_supervisor_child(module(), term(), term()) ->
    {ok, pid()} | ignore | {error, term()}.
start_dynamic_supervisor_child(Module, _DefaultArgs, Args) ->
    start_supervisor_child(Module, Args).

-doc """
`startChild: args name: aName` on a pool of supervisors; see
`start_dynamic_supervisor_child/2`. A taken name surfaces as OTP's
`{already_started, Pid}`, which `startChild/3` reports as `#name_registered`.
""".
-spec start_dynamic_supervisor_child(module(), term(), term(), term()) ->
    {ok, pid()} | ignore | {error, term()}.
start_dynamic_supervisor_child(Module, _DefaultArgs, Args, Name) when is_atom(Name) ->
    start_supervisor_child(Module, Args, Name);
start_dynamic_supervisor_child(_Module, _DefaultArgs, _Args, Name) ->
    {error,
        beamtalk_error:new_fmt(
            type_error, 'DynamicSupervisor', 'startChild:name:', "name must be a Symbol, got ~tp", [
                Name
            ]
        )}.

-spec exports(module(), atom(), arity()) -> boolean().
exports(Module, Fun, Arity) ->
    _ = code:ensure_loaded(Module),
    erlang:function_exported(Module, Fun, Arity).

-doc """
Pids of the running top-level spawn-family supervisor instances on this node:
the local members of the `pg` group (default scope) each joins from its own
`init/1` (ADR 0135 §1). `pg` drops a member when it exits, so a stopped
instance is not listed. Used by process navigation and `Node current
supervisors` to find anonymous instances.

Scope (BT-3994): the default `pg` scope, as ADR 0135 §1 specifies and as the
runtime's other groups (`beamtalk_classes`, `beamtalk_repl_shells`) use, and,
like theirs (BT-5192), its reads are node-local. A
dedicated scope would not make membership node-local: `pg` synchronises a
scope with the same-named scope on every connected node, so it would only
add a scope process to start. The cost of the cluster-wide scope is one
membership broadcast per top-level start and stop, which is rare; reads use
`pg:get_local_members/1`, so another node's instances are never listed here.
""".
-spec instance_pids() -> [pid()].
instance_pids() ->
    try
        pg:get_local_members(?INSTANCES_GROUP)
    catch
        error:badarg -> [];
        exit:{noproc, _} -> []
    end.

-doc "`instance_pids/0` as supervisor handles (`{beamtalk_supervisor, Class, Module, Pid}`).".
-spec instances() -> [tuple()].
instances() ->
    lists:filtermap(
        fun(Pid) ->
            case sup_dictionary_value(Pid, ?SUP_CLASS_KEY) of
                {ok, {ClassName, Module}} -> {true, {beamtalk_supervisor, ClassName, Module, Pid}};
                _ -> false
            end
        end,
        instance_pids()
    ).

-doc """
Every supervisor child whose module is `Module` (BT-4936).

Walks all live supervisor instances (`instance_pids/0`, which includes nested
supervisors, static and dynamic) and returns one entry per child whose
`modules` list contains `Module`, in any state (running, restarting,
stopped). Used by `removeFromSystem` to refuse removing a class that a
supervisor still holds. A dead supervisor is skipped; one that cannot be queried (timeout, or
the caller is that supervisor) yields an entry with `id => unreachable`, so
the caller fails closed.
""".
-spec supervised_children_of(module()) ->
    [#{supervisor := atom() | nil, supervisor_pid := pid(), id := term(), pid := term()}].
supervised_children_of(Module) ->
    supervised_children_of(Module, []).

-doc """
`supervised_children_of/1`, also matching children whose pid is in `Pids`
(a `withClassMethod:` factory child can carry a module other than the class's
own, but its pid is a live actor of the class).
""".
-spec supervised_children_of(module(), [pid()]) ->
    [#{supervisor := atom() | nil, supervisor_pid := pid(), id := term(), pid := term()}].
supervised_children_of(Module, Pids) ->
    lists:flatmap(
        fun(SupPid) ->
            SupClass =
                case sup_dictionary_value(SupPid, ?SUP_CLASS_KEY) of
                    {ok, {Cls, _}} -> Cls;
                    _ -> nil
                end,
            case children_or_unreachable(SupPid) of
                dead ->
                    [];
                unreachable ->
                    %% Fail closed: a supervisor that cannot be asked (timeout,
                    %% or removal running inside it) may hold a child.
                    [
                        #{
                            supervisor => SupClass,
                            supervisor_pid => SupPid,
                            id => unreachable,
                            pid => unreachable
                        }
                    ];
                Children ->
                    matching_children(SupClass, SupPid, Children, Module, Pids)
            end
        end,
        instance_pids()
    ).

%% `which_children` with a bounded timeout. `dead` only for a gone supervisor.
-spec children_or_unreachable(pid()) -> [tuple()] | dead | unreachable.
children_or_unreachable(SupPid) when SupPid =:= self() ->
    unreachable;
children_or_unreachable(SupPid) ->
    try
        gen_server:call(SupPid, which_children, 5000)
    catch
        exit:{noproc, _} ->
            dead;
        _:_ ->
            %% Decide by liveness, not exit reason: a supervisor that was
            %% killed or crashed can still be listed in pg for a moment.
            case is_process_alive(SupPid) of
                false -> dead;
                true -> unreachable
            end
    end.

matching_children(SupClass, SupPid, Children, Module, Pids) ->
    [
        #{supervisor => SupClass, supervisor_pid => SupPid, id => Id, pid => P}
     || {Id, P, _T, Mods} <- Children,
        (is_list(Mods) andalso lists:member(Module, Mods)) orelse
            (is_pid(P) andalso lists:member(P, Pids))
    ].

-doc """
`static_init/3` with `nil` start args.

Kept for supervisor modules compiled before ADR 0135, whose generated
`init/1` calls the two-argument form (ADR 0125). Such a module carries its
configuration class-side and has no instance side, which the runtime no
longer reads, so its start fails with a "recompile" hint rather than an
inherited `subclassResponsibility`.
""".
-spec static_init(module(), atom()) -> {ok, {map(), [map()]}}.
static_init(Module, ClassName) ->
    ok = require_instance_side(Module, ClassName),
    static_init(Module, ClassName, nil).

-doc """
Initialize a static supervisor (ADR 0135 §2).

Called from the generated `init/1` of Supervisor subclasses with the start
args (`nil` for `supervise`). Runs in the new supervisor process: it stores
the args (`self args`), builds this supervisor's handle and reads each
configuration setting (`childSpecs`, `strategy`, `maxRestarts`,
`restartWindow`) by **instance dispatch** on that handle, so user overrides,
inherited defaults and runtime overrides resolve as for any instance method.

The configuration is read by instance dispatch, never through the class
gen_server, which may be blocked (a start made from one of its own class methods),
so a `class_send` from here could deadlock. A configuration method that messages
`self` makes OTP exit with `calling_self`; that, and any other failure, ends
`init/1` with a `#beamtalk_error{}` that the caller reports as
`supervisor_start_failed` (see `config_step/3`).
""".
-spec static_init(module(), atom(), term()) -> {ok, {map(), [map()]}}.
static_init(Module, ClassName, Args) ->
    Self = beamtalk_supervisor_init_config:begin_init(Module, ClassName, Args),
    Children = beamtalk_supervisor_init_config:read_config(Self, ClassName, childSpecs),
    BtStrategy = beamtalk_supervisor_init_config:read_config(Self, ClassName, strategy),
    Strategy = beamtalk_supervisor_init_config:config_result(
        beamtalk_supervisor_init_config:check_strategy(ClassName, BtStrategy)
    ),
    {MaxR, MaxT} = beamtalk_supervisor_init_config:read_restart_intensity(Self, ClassName),
    put(?SUP_FLAGS_KEY, #{strategy => BtStrategy, maxRestarts => MaxR, restartWindow => MaxT}),
    SupFlags = #{strategy => Strategy, intensity => MaxR, period => MaxT},
    Specs = beamtalk_supervisor_init_config:config_step(ClassName, childSpecs, fun() ->
        build_child_specs(Children)
    end),
    ChildIds = [maps:get(id, S) || S <- Specs],
    ?LOG_DEBUG("Supervisor static init", #{
        supervisor => ClassName,
        strategy => Strategy,
        max_restarts => MaxR,
        restart_window => MaxT,
        children => ChildIds,
        domain => [beamtalk, runtime]
    }),
    {ok, {SupFlags, Specs}}.

-doc """
`dynamic_init/3` with `nil` start args (pre-ADR 0135 compiled modules).
""".
-spec dynamic_init(module(), atom()) -> {ok, {map(), [map()]}}.
dynamic_init(Module, ClassName) ->
    ok = require_instance_side(Module, ClassName),
    dynamic_init(Module, ClassName, nil).

%% A supervisor module compiled before ADR 0135 exports no `dispatch/4`, so
%% its configuration cannot be read from the instance: fail `init/1` with a
%% structured error telling the user to recompile.
-spec require_instance_side(module(), atom()) -> ok.
require_instance_side(Module, ClassName) ->
    _ = code:ensure_loaded(Module),
    case erlang:function_exported(Module, dispatch, 4) of
        true ->
            ok;
        false ->
            exit(
                beamtalk_error:new(
                    supervisor_start_failed,
                    ClassName,
                    supervise,
                    <<
                        "this supervisor was compiled before supervisor configuration moved to "
                        "the instance side (ADR 0135); recompile it"
                    >>
                )
            )
    end.

-doc """
Initialize a dynamic supervisor (ADR 0135 §2).

Same mechanism as `static_init/3`: `childClass`, `maxRestarts`,
`restartWindow`, `maxChildren` and `childShutdown` are read from the pool
being started, by instance dispatch, so each pool instance can compute its
own values from `self args`. `maxChildren` and `childShutdown` are validated
here, once; a bad value fails `init/1` with the validator's error, which the
caller reports as `supervisor_start_failed` (a nested pool fails its
parent's start instead). The child class is stored in the process dictionary
(`?CHILD_CLASS_KEY`) for `startChild/1,2,3` (`pool_child_class/1`).
""".
-spec dynamic_init(module(), atom(), term()) -> {ok, {map(), [map()]}}.
dynamic_init(Module, ClassName, Args) ->
    Self = beamtalk_supervisor_init_config:begin_init(Module, ClassName, Args),
    ChildClass = beamtalk_supervisor_init_config:read_config(Self, ClassName, childClass),
    {MaxR, MaxT} = beamtalk_supervisor_init_config:read_restart_intensity(Self, ClassName),
    MaxC = beamtalk_supervisor_init_config:config_result(
        beamtalk_supervisor_capacity:check_max_children(
            ClassName, beamtalk_supervisor_init_config:read_config(Self, ClassName, maxChildren)
        )
    ),
    ChildShutdown = beamtalk_supervisor_init_config:config_result(
        beamtalk_supervisor_spec:check_child_shutdown(
            ClassName, beamtalk_supervisor_init_config:read_config(Self, ClassName, childShutdown)
        )
    ),
    ChildClassEntry = beamtalk_supervisor_init_config:child_class_entry_with_kind(
        beamtalk_supervisor_init_config:child_class_entry(ClassName, ChildClass)
    ),
    %% BT-3964/BT-4068: OTP's `supervisor` has no `max_children` flag (that is
    %% Elixir's DynamicSupervisor). `init/1` runs in the supervisor process,
    %% so the limit is stashed in its process dictionary: it lives exactly as
    %% long as this supervisor incarnation and is re-read from the instance on
    %% restart. When capped, `init/1` also installs the enforcement machinery
    %% (an ETS counter plus a monitor process) and wraps the child template so
    %% every start is counted atomically, inside the supervisor process, by
    %% `checked_dynamic_start/1..4` — one `supervisor:start_child/2` call per
    %% `startChild`, with no check-then-act race between concurrent starters.
    put(?MAX_CHILDREN_KEY, MaxC),
    Capacity = beamtalk_supervisor_capacity:init_child_capacity(MaxC),
    case Capacity of
        none ->
            ok;
        {CapTid, CapMon} ->
            put(?MAX_CHILDREN_STATE_KEY, {CapTid, CapMon})
    end,
    put(?CHILD_CLASS_KEY, ChildClassEntry),
    put(?SUP_FLAGS_KEY, #{
        strategy => simpleOneForOne, maxRestarts => MaxR, restartWindow => MaxT
    }),
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    %% BT-3965: `childShutdown` (when set) replaces the shutdown the child
    %% class's own `supervisionSpec` produced. `simple_one_for_one` has
    %% exactly one child spec, so the value covers every dynamic child.
    Specs = beamtalk_supervisor_init_config:config_step(ClassName, childClass, fun() ->
        [
            beamtalk_supervisor_capacity:wrap_capped_child_spec(
                beamtalk_supervisor_spec:apply_child_shutdown(S, ChildShutdown), MaxC, Capacity
            )
         || S <- build_child_specs([ChildClass], dynamic)
        ]
    end),
    ?LOG_DEBUG("DynamicSupervisor init", #{
        supervisor => ClassName,
        strategy => simple_one_for_one,
        max_restarts => MaxR,
        restart_window => MaxT,
        max_children => MaxC,
        child_shutdown => ChildShutdown,
        domain => [beamtalk, runtime]
    }),
    {ok, {SupFlags, Specs}}.

-doc """
Return a supervisor's start args (`self args`, ADR 0135 §2).

Called from `args` on Supervisor and DynamicSupervisor instances. Inside the
supervisor's own `init/1` (the configuration methods) it reads this
process's dictionary; from outside it reads the supervisor's
(`erlang:process_info/2`, via `erpc` for a remote supervisor). Returns
`{ok, nil}` for a supervisor started without args (or compiled before
ADR 0135), `{error, #beamtalk_error{kind = stale_handle}}` when the
supervisor is not running, and `{error, #beamtalk_error{kind =
node_down}}` when a remote supervisor's node cannot be reached (down,
partitioned or too slow to answer, BT-3984): that says nothing about whether
the supervisor is running, so it is not reported as a stale handle.
""".
-spec args(term()) -> {ok, term()} | {error, #beamtalk_error{}}.
args({beamtalk_supervisor, _ClassName, _Module, Pid} = Self) ->
    case sup_dictionary_value(Pid, ?SUP_ARGS_KEY) of
        {ok, Value} ->
            {ok, Value};
        undefined ->
            {ok, nil};
        Unavailable ->
            beamtalk_supervisor_errors:supervisor_unavailable_error(Self, args, Unavailable)
    end.

-doc """
Warn when an ADR 0084 runtime class-method override installs a class-side
supervisor configuration selector that ADR 0135 §2 removed (`children`,
`strategy`, ..., `initialize:`) on a Supervisor or DynamicSupervisor class.
The compiler rejects these in source; an override installed at runtime is
never read, so it is reported instead of silently doing nothing. The list is
generated from the compiler's (`beamtalk_supervisor_config.hrl`).
""".
-spec warn_removed_class_side_config(pid(), atom()) -> ok.
warn_removed_class_side_config(ClassPid, Selector) ->
    case lists:member(Selector, ?BT_REMOVED_CLASS_SIDE_SUPERVISOR_CONFIG) of
        true -> warn_if_supervisor(beamtalk_object_class_read:class_name_safe(ClassPid), Selector);
        false -> ok
    end.

-spec warn_if_supervisor(term(), atom()) -> ok.
warn_if_supervisor(ClassName, Selector) ->
    case is_atom(ClassName) andalso ClassName =/= undefined andalso is_supervisor(ClassName) of
        true ->
            ?LOG_WARNING(
                "Class-side supervisor configuration override is never read (ADR 0135)",
                #{
                    class => ClassName,
                    selector => Selector,
                    hint => <<
                        "configuration and initialize are instance-side; install an "
                        "instance method instead"
                    >>,
                    domain => [beamtalk, runtime]
                }
            );
        false ->
            ok
    end,
    ok.

-doc """
Return the configuration a supervisor's `init/1` read from its instance
(`#{strategy, maxRestarts, restartWindow}`, ADR 0135 §2), or `undefined` for
a supervisor not started through `static_init/3` / `dynamic_init/3` (a
foreign supervisor, or one compiled before ADR 0135) or no longer running.
Used by process navigation (ADR 0092), which must not run user code.
""".
-spec configured_flags(pid()) -> {ok, map()} | undefined.
configured_flags(Pid) when is_pid(Pid) ->
    case sup_dictionary_value(Pid, ?SUP_FLAGS_KEY) of
        {ok, Flags} when is_map(Flags) -> {ok, Flags};
        _ -> undefined
    end.

%% Read one key from a supervisor's process dictionary, wherever it runs:
%% `dead` when the process is not running, `{unreachable, Why}` when its node
%% cannot be reached (`classify_call_failure/2`), so a slow or partitioned
%% remote supervisor is not mistaken for a dead one (BT-3984).
-spec sup_dictionary_value(pid(), atom()) ->
    {ok, term()} | undefined | supervisor_unavailable().
sup_dictionary_value(Pid, Key) when Pid =:= self() ->
    case get(Key) of
        undefined -> undefined;
        Value -> {ok, Value}
    end;
sup_dictionary_value(Pid, Key) when node(Pid) =:= node() ->
    dictionary_info(erlang:process_info(Pid, {dictionary, Key}));
sup_dictionary_value(Pid, Key) ->
    try
        erpc:call(
            node(Pid), erlang, process_info, [Pid, {dictionary, Key}], ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        Info -> dictionary_info(Info)
    catch
        Class:Reason ->
            case classify_call_failure(Class, Reason) of
                {unreachable, _} = Unreachable -> Unreachable;
                _ -> dead
            end
    end.

-spec dictionary_info(term()) -> {ok, term()} | undefined | dead.
dictionary_info(undefined) -> dead;
dictionary_info({{dictionary, _Key}, undefined}) -> undefined;
dictionary_info({{dictionary, _Key}, Value}) -> {ok, Value}.

-doc """
Return the running supervisor instance, or nil if not started.

Called from `class current` on Supervisor and DynamicSupervisor subclasses.
Self is the class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
""".
-spec current(beamtalk_object()) -> term() | nil.
current(Self) ->
    ClassPid = element(4, Self),
    ClassName = beamtalk_object_class_read:class_name_safe(ClassPid),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case whereis(Module) of
        undefined ->
            nil;
        Pid ->
            {beamtalk_supervisor, ClassName, Module, Pid}
    end.

-doc """
Return the live child objects of the running children, in declaration order.

Called from `children` on Supervisor instances (BT-3966: it used to return
the child ids). Each running child is wrapped like `which:` wraps it: a
`{beamtalk_supervisor, ...}` handle for a supervisor child, a
`{beamtalk_object, ...}` for an actor, the bare pid for a child whose module
is not a Beamtalk class. Restarting and stopped children are left out (see
`childInfos/1` for every spec). Returns `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead.
""".
-spec whichChildren(term()) -> {ok, [beamtalk_object() | pid()]} | {error, #beamtalk_error{}}.
whichChildren(Self) ->
    Pid = element(4, Self),
    with_live_supervisor(Self, children, fun() ->
        %% Running entries only, wrapped directly: no `SupervisorChild` maps
        %% (and no `childClass` lookups) are built for `children`.
        Running = [E || {_Id, P, _T, _M} = E <- supervisor:which_children(Pid), is_pid(P)],
        ModClasses = beamtalk_supervisor_children:module_classes(Running),
        {ok,
            lists:reverse([
                beamtalk_supervisor_children:child_object(ChildClass, Module, P)
             || {_Id, P, _T, Mods} <- Running,
                {ChildClass, Module} <- [beamtalk_supervisor_children:entry_class(Mods, ModClasses)]
            ])}
    end).

-doc """
Return one `SupervisorChild` value per child spec, in declaration order
(BT-3966). Backs `Supervisor>>whichChildren`.

Includes children that OTP is restarting (`status => restarting`) and ones
that were terminated but whose spec is still held (`status => stopped`), both
with `child => nil`; it never raises on them. Returns `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead.
""".
-spec childInfos(term()) -> {ok, [supervisor_child()]} | {error, #beamtalk_error{}}.
childInfos(Self) ->
    Pid = element(4, Self),
    with_live_supervisor(Self, whichChildren, fun() ->
        {ok, beamtalk_supervisor_children:supervisor_child_entries(supervisor:which_children(Pid))}
    end).

-doc """
Return the running child with the given OTP child id (BT-3966). Backs
`Supervisor>>childWithId:`.

`{ok, Child}` for a running child; `{ok, nil}` when the id names a spec whose
child is restarting or stopped (the `which:` convention);
`{error, #beamtalk_error{kind = child_not_found}}` (details `#{id => Id}`)
when no spec has that id; `type_error` for an id that is neither a Symbol nor
an Integer; `stale_handle` when the supervisor process is dead.
""".
-spec childWithId(term(), atom() | integer()) ->
    {ok, beamtalk_object() | pid() | nil} | {error, #beamtalk_error{}}.
childWithId(Self, Id) ->
    Pid = element(4, Self),
    SupClass = element(2, Self),
    Selector = 'childWithId:',
    beamtalk_supervisor_children:with_child_id(Self, Selector, Id, fun() ->
        case lists:keyfind(Id, 1, supervisor:which_children(Pid)) of
            false ->
                {error, beamtalk_supervisor_errors:child_not_found_error(SupClass, Selector, Id)};
            {_Id, ChildPid, _Type, Mods} when is_pid(ChildPid) ->
                {ClassName, Module} = beamtalk_supervisor_children:class_for_modules(Mods),
                {ok, beamtalk_supervisor_children:child_object(ClassName, Module, ChildPid)};
            _NotRunning ->
                {ok, nil}
        end
    end).

-doc """
Terminate the child with the given OTP child id (BT-3966). Backs
`Supervisor>>terminateChild:`.

`{ok, nil}` on success, and also when no spec has that id (idempotent,
ADR 0080: a terminated `#temporary` child's spec is gone, which cannot be
told apart from an unknown id). `terminate_failed` for any other OTP error,
`type_error` for an id that is neither a Symbol nor an Integer,
`stale_handle` when the supervisor process is dead.
""".
-spec terminateChildId(term(), atom() | integer()) -> {ok, nil} | {error, #beamtalk_error{}}.
terminateChildId(Self, Id) ->
    Pid = element(4, Self),
    SupClass = element(2, Self),
    beamtalk_supervisor_children:with_child_id(Self, 'terminateChild:', Id, fun() ->
        beamtalk_supervisor_children:handle_terminate_child_result(
            SupClass, #{child => Id}, supervisor:terminate_child(Pid, Id)
        )
    end).

-doc """
Restart a terminated child by its OTP child id (BT-3966). Backs
`Supervisor>>restartChild:`.

Returns `{ok, Child}` with the newly started child (`{ok, nil}` when its start
function returned `ignore`). Errors: `child_not_found` (details `#{id => Id}`)
when no spec has that id, which includes any terminated `#temporary` child
(OTP deletes its spec when it exits); `child_running` (details
`#{id => Id, status => running | restarting}`) when the child is running or
being restarted; `child_start_failed` for a failing start; `type_error`,
`stale_handle` as for `childWithId/2`.
""".
-spec restartChild(term(), atom() | integer()) ->
    {ok, beamtalk_object() | pid() | nil} | {error, #beamtalk_error{}}.
restartChild(Self, Id) ->
    Pid = element(4, Self),
    SupClass = element(2, Self),
    Selector = 'restartChild:',
    beamtalk_supervisor_children:with_child_id(Self, Selector, Id, fun() ->
        case supervisor:restart_child(Pid, Id) of
            {ok, ChildPid} ->
                {ok, beamtalk_supervisor_children:restarted_child(Pid, Id, ChildPid)};
            {ok, ChildPid, _Info} ->
                {ok, beamtalk_supervisor_children:restarted_child(Pid, Id, ChildPid)};
            {error, not_found} ->
                {error, beamtalk_supervisor_errors:child_not_found_error(SupClass, Selector, Id)};
            {error, running} ->
                {error,
                    beamtalk_supervisor_errors:child_running_error(SupClass, Selector, Id, running)};
            {error, restarting} ->
                {error,
                    beamtalk_supervisor_errors:child_running_error(
                        SupClass, Selector, Id, restarting
                    )};
            {error, Reason} ->
                {error,
                    beamtalk_supervisor_errors:start_failed_error(
                        child_start_failed, SupClass, Selector, Reason
                    )}
        end
    end).

-doc """
Delete the spec of a terminated child by its OTP child id (BT-3966). Backs
`Supervisor>>deleteChild:`.

`{ok, nil}` on success. Errors: `child_not_found` (details `#{id => Id}`),
`child_running` when the child is still running or restarting (terminate it
first), `type_error`, `stale_handle` as for `childWithId/2`.
""".
-spec deleteChild(term(), atom() | integer()) -> {ok, nil} | {error, #beamtalk_error{}}.
deleteChild(Self, Id) ->
    Pid = element(4, Self),
    SupClass = element(2, Self),
    Selector = 'deleteChild:',
    beamtalk_supervisor_children:with_child_id(Self, Selector, Id, fun() ->
        case supervisor:delete_child(Pid, Id) of
            ok ->
                {ok, nil};
            {error, not_found} ->
                {error, beamtalk_supervisor_errors:child_not_found_error(SupClass, Selector, Id)};
            {error, running} ->
                {error,
                    beamtalk_supervisor_errors:child_running_error(SupClass, Selector, Id, running)};
            {error, restarting} ->
                {error,
                    beamtalk_supervisor_errors:child_running_error(
                        SupClass, Selector, Id, restarting
                    )};
            {error, Reason} ->
                {error, beamtalk_supervisor_errors:delete_failed_error(SupClass, Selector, Reason)}
        end
    end).

-doc """
Return the live children of a DynamicSupervisor as objects of its child class
(BT-3966). Backs `DynamicSupervisor>>children`.

Copies the whole child list out of the supervisor in one message (O(n)), so
it is meant for bulk operations; restarting children are left out and the
order is unspecified. The child class comes from `pool_child_class/1`, the
lookup `startChild/1` uses. Returns `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead.
""".
-spec dynamicChildren(term()) -> {ok, [beamtalk_object()]} | {error, #beamtalk_error{}}.
dynamicChildren(Self) ->
    SupPid = element(4, Self),
    with_live_supervisor(Self, children, fun() ->
        case [P || {_Id, P, _Type, _Mods} <- supervisor:which_children(SupPid), is_pid(P)] of
            [] ->
                %% No live child: nothing to wrap, so no child class lookup
                %% (a pool started without a recorded child class would
                %% otherwise report `child_class_unknown`).
                {ok, []};
            Pids ->
                beamtalk_supervisor_pool:with_pool_child_class(Self, children, fun(
                    ChildClass, ChildModule
                ) ->
                    {ok, [
                        beamtalk_supervisor_announce:wrap_child(ChildClass, ChildModule, P)
                     || P <- Pids
                    ]}
                end)
        end
    end).

-doc """
Return the running child object for the given class, or nil.

Called from `which: aClass` on Supervisor instances.
ClassArg is a class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
Matches by child module (position 4 in which_children tuples) rather than child id
so that custom ids set via `withId:` in SupervisionSpec still resolve correctly.

Returns `{ok, {beamtalk_supervisor, ...}}` for supervisor subclasses or
`{ok, {beamtalk_object, ...}}` for worker children; `{ok, nil}` when no
spec matches or the one matching child is not running. When several specs
match the class (BT-3966), returns
`{error, #beamtalk_error{kind = ambiguous_child}}` with the matching ids in
`details` (`#{ids => [Id]}`): use `childWithId:` then. Returns
`{error, BtError}` with `kind = stale_handle` when the supervisor process is
dead.
""".
-spec whichChild(term(), Class :: beamtalk_object()) ->
    {ok, tuple() | nil} | {error, #beamtalk_error{}}.
whichChild(Self, ClassArg) ->
    SupPid = element(4, Self),
    ClassName = element(2, Self),
    ChildModule = element(3, ClassArg),
    ChildClassPid = element(4, ClassArg),
    with_live_supervisor(Self, 'which:', fun() ->
        case
            beamtalk_supervisor_children:children_for_module(
                supervisor:which_children(SupPid), ChildModule
            )
        of
            [] ->
                {ok, nil};
            [{_Id, ChildPid, _, _}] when is_pid(ChildPid) ->
                ChildClass = beamtalk_object_class_read:class_name_safe(ChildClassPid),
                {ok, beamtalk_supervisor_announce:wrap_child(ChildClass, ChildModule, ChildPid)};
            [_NotRunning] ->
                {ok, nil};
            Matches ->
                {error,
                    beamtalk_supervisor_errors:ambiguous_child_error(ClassName, 'which:', Matches)}
        end
    end).

-doc """
Terminate a supervised child.

For Supervisor (static): Arg is a class object. The child id is resolved by
matching the class's module against the children's modules, the way
`whichChild/2` does, so a child declared `withId:` is found too (BT-3966: it
used to assume the id was the class name, so such a child was never
terminated). No match → `{ok, nil}` (idempotent); several matches →
`{error, #beamtalk_error{kind = ambiguous_child}}` with the ids in `details`,
and nothing is terminated.

For DynamicSupervisor (dynamic): Arg is an actor or supervisor instance —
terminates child by its process pid (simple_one_for_one semantics).

## Return shape (ADR 0080 Phase 1)

Returns `{ok, nil}` on success. Returns `{ok, nil}` on
`{error, not_found}` (idempotent: "child is already gone" is the
goal state, so treating it as success lets callers write safe cleanup
code without swallowing unrelated failures). Returns
`{error, #beamtalk_error{kind = terminate_failed, ...}}` on any other
`{error, Reason}` from `supervisor:terminate_child/2`, and
`{error, #beamtalk_error{kind = stale_handle, ...}}` when the supervisor
process is not alive.

**Behavior change on the static path:** previously, the static path
raised on every `{error, Reason}` — including `{error, not_found}`.
It now returns `{ok, nil}` on `not_found`, aligning with the dynamic
path's existing `not_found` → success branch (which pre-dates this PR
and returned bare `nil`; after this migration both paths now return
`{ok, nil}`). Any caller that relied on the static path raising for a
missing child loses that signal — by design (see ADR 0080 §Decision,
"idempotent-startup convention").

FFI coercion in `beamtalk_erlang_proxy:coerce_ffi_result/2` wraps this
into a Beamtalk `Result` tagged map for the class method body's return.
""".
%% Canonical specs for the two BT selectors that map to this function:
%%   Supervisor:        terminate: aClass     → terminateChild: self class: aClass
%%   DynamicSupervisor: terminateChild: child → terminateChild: self child: child
-spec 'terminateChild:class:'(term(), Class :: beamtalk_object()) ->
    {ok, nil} | {error, #beamtalk_error{}}.
'terminateChild:class:'(Self, Class) -> terminateChild(Self, Class).
-spec 'terminateChild:child:'(term(), Child :: term()) ->
    {ok, nil} | {error, #beamtalk_error{}}.
'terminateChild:child:'(Self, Child) -> terminateChild(Self, Child).
-spec terminateChild(term(), term()) -> {ok, nil} | {error, #beamtalk_error{}}.
terminateChild(Self, Arg) ->
    SupPid = element(4, Self),
    SupClass = element(2, Self),
    with_live_supervisor(Self, 'terminateChild:', fun() ->
        case beamtalk_class_registry:is_class_object(Arg) of
            true ->
                %% Supervisor case: resolve the child id by module (BT-3966).
                ChildModule = element(3, Arg),
                case
                    beamtalk_supervisor_children:children_for_module(
                        supervisor:which_children(SupPid), ChildModule
                    )
                of
                    [] ->
                        {ok, nil};
                    [{ChildId, _, _, _}] ->
                        beamtalk_supervisor_children:handle_terminate_child_result(
                            SupClass,
                            #{child => ChildId},
                            supervisor:terminate_child(SupPid, ChildId)
                        );
                    Matches ->
                        {error,
                            beamtalk_supervisor_errors:ambiguous_child_error(
                                SupClass, 'terminate:', Matches
                            )}
                end;
            false when is_tuple(Arg), tuple_size(Arg) >= 4, is_pid(element(4, Arg)) ->
                %% DynamicSupervisor case: terminate by child process pid
                ChildPid = element(4, Arg),
                TermResult = supervisor:terminate_child(SupPid, ChildPid),
                case TermResult of
                    ok -> beamtalk_supervisor_capacity:release_child_capacity(SupPid, ChildPid);
                    _ -> ok
                end,
                beamtalk_supervisor_children:handle_terminate_child_result(
                    SupClass,
                    #{child_pid => ChildPid},
                    TermResult
                );
            false ->
                %% Not a child object (e.g. a Symbol id sent to a
                %% DynamicSupervisor, whose children have no ids, BT-3966).
                {error,
                    beamtalk_error:with_details(
                        beamtalk_error:new(
                            type_error,
                            SupClass,
                            'terminateChild:',
                            <<"Pass the child object to terminate">>
                        ),
                        #{child => Arg}
                    )}
        end
    end).

-doc """
Start a new child with default args under a DynamicSupervisor.

Called from `startChild` on DynamicSupervisor instances.
Resolves the child class with `pool_child_class/1` (ADR 0135),
then starts the child via OTP simple_one_for_one.

## Return shape (ADR 0080 Phase 1)

Returns `{ok, {beamtalk_supervisor, ChildClass, ChildModule, ChildPid}}`
for supervisor subclasses, or `{ok, {beamtalk_object, ...}}` for workers.
On `supervisor:start_child/2` failure (typically a child `init/1` crash)
returns `{error, #beamtalk_error{kind = child_start_failed}}`. On a stale
supervisor handle returns `{error, #beamtalk_error{kind = stale_handle}}`.
FFI coercion in `beamtalk_erlang_proxy:coerce_ffi_result/2` wraps the
tagged tuple into a Beamtalk `Result` for the stdlib method body.
""".
-spec startChild(term()) -> {ok, tuple()} | {error, #beamtalk_error{}}.
startChild(Self) ->
    SupPid = element(4, Self),
    SupClass = element(2, Self),
    beamtalk_supervisor_pool:with_pool_child(Self, startChild, fun(
        ChildClass, ChildModule, ChildKind, MaxChildren
    ) ->
        beamtalk_supervisor_capacity:with_child_capacity(
            SupClass, startChild, SupPid, MaxChildren, [], fun(StartArgs) ->
                case supervisor:start_child(SupPid, StartArgs) of
                    {error, {?MAX_CHILDREN_REFUSED, _}} = Refused ->
                        %% BT-4068: at the cap — pass through untouched for
                        %% `with_child_capacity/6` to map to `max_children`
                        %% (a debug log, no crash announcement).
                        Refused;
                    {ok, ChildPid} ->
                        ?LOG_INFO("DynamicSupervisor child started", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            module => ChildModule,
                            child_pid => ChildPid,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_added(
                            SupClass, ChildClass, ChildPid
                        ),
                        {ok,
                            beamtalk_supervisor_announce:wrap_child(
                                ChildClass, ChildModule, ChildKind, ChildPid
                            )};
                    {error, Reason} ->
                        ?LOG_ERROR("DynamicSupervisor child start failed", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            reason => Reason,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_crashed(
                            SupClass, ChildClass, Reason
                        ),
                        {error,
                            beamtalk_supervisor_errors:start_failed_error(
                                child_start_failed, SupClass, startChild, Reason
                            )}
                end
            end
        )
    end).

-doc """
Start a new child with args under a DynamicSupervisor.

Called from `startChild: args` on DynamicSupervisor instances.
Args is passed as the extra argument to OTP simple_one_for_one,
which appends it to the child start function's argument list.

## Return shape (ADR 0080 Phase 1)

Returns `{ok, {beamtalk_supervisor, ChildClass, ChildModule, ChildPid}}`
for supervisor subclasses, or `{ok, {beamtalk_object, ...}}` for workers.
On `supervisor:start_child/2` failure returns
`{error, #beamtalk_error{kind = child_start_failed}}`. On a stale handle
returns `{error, #beamtalk_error{kind = stale_handle}}`. FFI coercion
wraps this into a Beamtalk `Result` for the stdlib method body.
""".
-spec startChild(term(), term()) -> {ok, tuple()} | {error, #beamtalk_error{}}.
startChild(Self, Args) ->
    SupPid = element(4, Self),
    SupClass = element(2, Self),
    beamtalk_supervisor_pool:with_pool_child(Self, 'startChild:', fun(
        ChildClass, ChildModule, ChildKind, MaxChildren
    ) ->
        beamtalk_supervisor_capacity:with_child_capacity(
            SupClass, 'startChild:', SupPid, MaxChildren, [Args], fun(StartArgs) ->
                case supervisor:start_child(SupPid, StartArgs) of
                    {error, {?MAX_CHILDREN_REFUSED, _}} = Refused ->
                        %% BT-4068: at the cap — pass through untouched for
                        %% `with_child_capacity/6` to map to `max_children`
                        %% (a debug log, no crash announcement).
                        Refused;
                    {ok, ChildPid} ->
                        ?LOG_INFO("DynamicSupervisor child started", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            module => ChildModule,
                            child_pid => ChildPid,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_added(
                            SupClass, ChildClass, ChildPid
                        ),
                        {ok,
                            beamtalk_supervisor_announce:wrap_child(
                                ChildClass, ChildModule, ChildKind, ChildPid
                            )};
                    {error, Reason} ->
                        ?LOG_ERROR("DynamicSupervisor child start failed", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            reason => Reason,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_crashed(
                            SupClass, ChildClass, Reason
                        ),
                        {error,
                            beamtalk_supervisor_errors:start_failed_error(
                                child_start_failed, SupClass, 'startChild:', Reason
                            )}
                end
            end
        )
    end).

-doc """
Start a new child with args, registered under a name, under a DynamicSupervisor.

Called from `startChild: args name: aName` on DynamicSupervisor instances
(ADR 0079 amendment). Combines args-replay with named
registration: `Name` is appended as a third extra argument that
`supervisor:start_child/2` threads onto the shared `simple_one_for_one`
template, landing on `start_dynamic_child/4`, which spawns the child via
`start_actor_child/3` with `{local, Name}` — the same awaited, named start
`SupervisionSpec withName:` uses for static children (ADR 0079, BT-4033).
Because `simple_one_for_one` replays each dynamic child's own start args (now including `Name`) on automatic OTP restart, a
crashed named child re-registers under the same name every time, with no
supervisor-side bookkeeping.

## Return shape (ADR 0080 Phase 1)

Returns `{ok, {beamtalk_supervisor, ChildClass, ChildModule, ChildPid}}`
for supervisor subclasses, or `{ok, {beamtalk_object, ...}}` for workers.
On a duplicate name returns `{error, #beamtalk_error{kind = name_registered}}`;
on a reserved name `{error, #beamtalk_error{kind = reserved_name}}`; on any
other `supervisor:start_child/2` failure (e.g. a child `init/1` crash)
`{error, #beamtalk_error{kind = child_start_failed}}`. On a stale
supervisor handle returns `{error, #beamtalk_error{kind = stale_handle}}`.
""".
-spec startChild(term(), term(), term()) -> {ok, tuple()} | {error, #beamtalk_error{}}.
startChild(Self, Args, Name) ->
    SupPid = element(4, Self),
    SupClass = element(2, Self),
    beamtalk_supervisor_pool:with_pool_child(Self, 'startChild:name:', fun(
        ChildClass, ChildModule, ChildKind, MaxChildren
    ) ->
        beamtalk_supervisor_capacity:with_child_capacity(
            SupClass, 'startChild:name:', SupPid, MaxChildren, [Args, Name], fun(
                StartArgs
            ) ->
                case supervisor:start_child(SupPid, StartArgs) of
                    {error, {?MAX_CHILDREN_REFUSED, _}} = Refused ->
                        %% BT-4068: at the cap — pass through untouched for
                        %% `with_child_capacity/6` to map to `max_children`
                        %% (a debug log, no crash announcement).
                        Refused;
                    {ok, ChildPid} ->
                        ?LOG_INFO("DynamicSupervisor named child started", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            module => ChildModule,
                            child_pid => ChildPid,
                            name => Name,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_added(
                            SupClass, ChildClass, ChildPid
                        ),
                        {ok,
                            beamtalk_supervisor_announce:wrap_child(
                                ChildClass, ChildModule, ChildKind, ChildPid
                            )};
                    {error, #beamtalk_error{} = Err} ->
                        %% `'spawnAs'/3` already returns a structured error for
                        %% name_registered/type_error/reserved_name — re-attribute
                        %% it to this call site rather than wrapping it again. A
                        %% failed `initialize` arrives as `{initialize_failed, _}`
                        %% (BT-4033, start_actor_child/3) and falls through to
                        %% `#child_start_failed` below, with its error as the cause.
                        ?LOG_ERROR("DynamicSupervisor named child start failed", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            name => Name,
                            reason => Err#beamtalk_error.kind,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_crashed(
                            SupClass, ChildClass, Err
                        ),
                        {error, Err#beamtalk_error{class = SupClass, selector = 'startChild:name:'}};
                    {error, {already_started, _}} ->
                        %% A supervisor child class (ADR 0135 §3) registers
                        %% through `supervisor:start_link({local, Name}, ...)`,
                        %% which reports a taken name this way.
                        beamtalk_supervisor_announce:announce_child_crashed(
                            SupClass, ChildClass, name_registered
                        ),
                        {error, beamtalk_error:name_registered(SupClass, 'startChild:name:', Name)};
                    {error, Reason} ->
                        ?LOG_ERROR("DynamicSupervisor named child start failed", #{
                            supervisor => SupClass,
                            child => ChildClass,
                            name => Name,
                            reason => Reason,
                            domain => [beamtalk, runtime]
                        }),
                        beamtalk_supervisor_announce:announce_child_crashed(
                            SupClass, ChildClass, Reason
                        ),
                        {error,
                            beamtalk_supervisor_errors:start_failed_error(
                                child_start_failed, SupClass, 'startChild:name:', Reason
                            )}
                end
            end
        )
    end).

-doc """
Return the count of active children.

Called from `count` on Supervisor and DynamicSupervisor instances.
Uses `supervisor:count_children/1` which returns a proplist with
`active` (running), `workers`, `supervisors`, `specs` counts.

Returns `{ok, Count}` on success; `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead.
""".
-spec countChildren(term()) -> {ok, non_neg_integer()} | {error, #beamtalk_error{}}.
countChildren(Self) ->
    Pid = element(4, Self),
    with_live_supervisor(Self, count, fun() ->
        Counts = supervisor:count_children(Pid),
        {ok, proplists:get_value(active, Counts, 0)}
    end).

-doc """
Is the supervisor running? Backs `isAlive` on Supervisor and DynamicSupervisor
instances (sealed, BT-3984), which `beamtalk_message_dispatch:send/3` also
answers directly.

A local supervisor is checked with `erlang:is_process_alive/1`; a remote
one's node is asked, as for actors (`beamtalk_pid:probe_alive/1`, BT-5192).
When that node cannot tell, it raises the `node_down` every other handle call
raises for an unreachable supervisor
(`beamtalk_supervisor_errors:supervisor_unavailable_error/3`, `reason`
`#noconnection` or `#timeout`).
""".
-spec isAlive(term()) -> boolean().
isAlive({beamtalk_supervisor, undefined, _Module, _Pid}) ->
    false;
isAlive({beamtalk_supervisor, ClassName, _Module, Pid} = Self) when is_pid(Pid) ->
    case beamtalk_pid:probe_alive(Pid) of
        {ok, Alive} ->
            Alive;
        {error, {node_down, _}} ->
            raise_unavailable(Self, {unreachable, noconnection});
        {error, timeout} ->
            raise_unavailable(Self, {unreachable, timeout});
        {error, {other, Reason}} ->
            beamtalk_error:raise(
                beamtalk_pid:unexpected_answer_error(ClassName, isAlive, node(Pid), Reason)
            )
    end;
isAlive(_) ->
    false.

-spec raise_unavailable(tuple(), supervisor_unavailable()) -> no_return().
raise_unavailable(Self, Unavailable) ->
    {error, Error} = beamtalk_supervisor_errors:supervisor_unavailable_error(
        Self, isAlive, Unavailable
    ),
    beamtalk_error:raise(Error).

-doc """
Stop the supervisor and all its children.

Called from `stop` on Supervisor and DynamicSupervisor instances (sealed,
BT-3984), which `beamtalk_message_dispatch:send/3` also routes here.
Uses gen_server:stop/1 since supervisors are OTP gen_servers.

Returns `{ok, nil}` on success; `{error, BtError}` with `kind = stale_handle`
when the supervisor process is already dead, `kind = node_down` when
its node cannot be reached.
""".
-spec stop(term()) -> {ok, nil} | {error, #beamtalk_error{}}.
stop(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(Self, stop, fun() ->
        ?LOG_INFO("Supervisor stopping", #{
            supervisor => ClassName, pid => Pid, domain => [beamtalk, runtime]
        }),
        gen_server:stop(Pid),
        {ok, nil}
    end).

-doc """
Stop the supervisor and all its children, waiting up to `Ms` milliseconds.

Called from `stopWithin:` on Supervisor and DynamicSupervisor instances
(sealed, BT-4047) via the `stopWithinResult:` FFI seam. Uses
`beamtalk_stop:stop/3`, the `gen_server:stop/3` contract without its
negative-wait `timeout_value` error on very short waits (BT-4494).

Returns `{ok, nil}` once the supervisor has stopped; `{error, BtError}` with:
* `kind = timeout` when the shutdown overruns the wait — the caller gets
  the error but the supervisor keeps shutting down in the background
  (`proc_lib:stop/3` delivers the terminate request before its wait
  expires, so the shutdown is already underway);
* `kind = stale_handle` when the supervisor process is already dead,
  `kind = node_down` when its node cannot be reached (via
  `with_live_supervisor/3`, as `stop/1`);
* `kind = type_error` when `Ms` is neither a non-negative integer nor
  `infinity` (Beamtalk `#infinity`), as `Actor stopWith:timeout:`.
""".
-spec stopWithin(term(), term()) -> {ok, nil} | {error, #beamtalk_error{}}.
stopWithin(Self, Ms) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    case check_stop_timeout(ClassName, Ms) of
        {ok, Timeout} ->
            with_live_supervisor(Self, 'stopWithin:', fun() ->
                ?LOG_INFO("Supervisor stopping", #{
                    supervisor => ClassName, pid => Pid, domain => [beamtalk, runtime]
                }),
                %% Only the bare `timeout` exit is converted; `noproc` still
                %% reaches `with_live_supervisor/3` (`stale_handle`) and
                %% anything else propagates, as in `stop/1`.
                try
                    beamtalk_stop:stop(Pid, normal, Timeout),
                    {ok, nil}
                catch
                    exit:timeout ->
                        ?LOG_WARNING(
                            "Supervisor stop timed out; shutdown continues in background", #{
                                supervisor => ClassName,
                                pid => Pid,
                                timeout_ms => Timeout,
                                domain => [beamtalk, runtime]
                            }
                        ),
                        {error, beamtalk_supervisor_errors:stop_timeout_error(ClassName, Timeout)}
                end
            end);
        {error, _} = Error ->
            Error
    end.

-doc """
Validate a `stopWithin:` wait (BT-4047).

Accepts a non-negative integer (milliseconds) or `infinity` (Beamtalk
`#infinity`); anything else is a `type_error` carrying the offending value in
`details`, as `Actor stopWith:timeout:` reports it.
""".
-spec check_stop_timeout(atom(), term()) ->
    {ok, non_neg_integer() | infinity} | {error, #beamtalk_error{}}.
check_stop_timeout(_ClassName, infinity) ->
    {ok, infinity};
check_stop_timeout(_ClassName, Ms) when is_integer(Ms), Ms >= 0 ->
    {ok, Ms};
check_stop_timeout(ClassName, Other) ->
    {error,
        beamtalk_error:bad_value(
            ClassName,
            'stopWithin:',
            "stopWithin: ms must be a non-negative Integer or #infinity, got ~tp",
            [Other],
            Other
        )}.

-doc """
Build OTP child specs from a list of class objects or SupervisionSpec values.

Called from the generated `init/1` of Supervisor subclasses (Phase 3 codegen).
For each element:
- Class object: calls `supervisionSpec` on the class to get a SupervisionSpec,
  then calls `childSpec` on the spec to get the Beamtalk dict.
- SupervisionSpec map: calls `childSpec` directly.
Converts Beamtalk child spec dicts to OTP-compatible maps.
""".
-spec build_child_specs([term()]) -> [map()].
build_child_specs(Children) ->
    build_child_specs(Children, static).

-doc """
Build OTP child specs, threading `Mode` through to `spec_to_otp/2`.

`Mode = dynamic` is used only by `dynamic_init/3` (a DynamicSupervisor's
`simple_one_for_one` template): its default (`#spawn`) worker child spec
gets a **zero**-static-arg MFA that routes through `start_dynamic_child/2,3`
instead of baking `[#{}]` directly into the static start MFA —
see `spec_to_otp/2`. `Mode = static` (the default, used by
`static_init/3` via `build_child_specs/1`) is unchanged: a static
Supervisor's children start immediately at supervisor-init time and never
receive appended `supervisor:start_child/2` args, so the original
`[#{}]` shape is still correct there.
""".
-spec build_child_specs([term()], static | dynamic) -> [map()].
build_child_specs(Children, Mode) ->
    [beamtalk_supervisor_spec:build_child_spec(C, Mode) || C <- Children].

-doc "Delegates to `beamtalk_supervisor_spec:spec_to_otp/1` (BT-4705).".
-spec spec_to_otp(map()) -> map().
spec_to_otp(BtSpec) ->
    beamtalk_supervisor_spec:spec_to_otp(BtSpec).

-doc "Delegates to `beamtalk_supervisor_spec:spec_to_otp/2` (BT-4705).".
-spec spec_to_otp(map(), static | dynamic) -> map().
spec_to_otp(BtSpec, Mode) ->
    beamtalk_supervisor_spec:spec_to_otp(BtSpec, Mode).

%% The `checked_dynamic_start/1..4` MFAs are baked into live capped pools' OTP
%% child templates by `beamtalk_supervisor_capacity:wrap_capped_child_spec/3`
%% and replayed on restart, so they keep their module and name here and
%% delegate to the capacity module (BT-4705).
-doc """
Count one child start against a capped pool's `maxChildren` limit (BT-4068).

This is the `simple_one_for_one` template MFA `wrap_capped_child_spec/3`
installs on a capped pool: `{beamtalk_supervisor, checked_dynamic_start,
[Params]}`, with OTP appending each start's extra args. It runs in the
supervisor process, so the ETS check-and-increment is atomic with the start —
concurrent `startChild` callers can never overshoot the limit, and each costs
a single `supervisor:start_child/2` call.

`Params` is `#{tid, max, monitor, m, f, args}`: the counter table, the limit,
the monitor process that releases dead children's slots, and the wrapped
`{M, F, StaticArgs}` template. A manual start carries a caller-minted tagged
token as its first extra arg (`beamtalk_supervisor_capacity:with_child_capacity/6`); the token is
consumed here with `ets:take/2`, so an OTP restart replay — the same args
verbatim — finds its token already consumed and is recognised as a restart.
The consumed token is stripped, never passed to the child's start; a first
arg without the tag is a genuinely untokened start (a remote caller or a
direct `supervisor:start_child/2`), admitted like a restart. Manual starts
past the cap are refused with `{error, {?MAX_CHILDREN_REFUSED, Max}}` (OTP
passes it back through `supervisor:start_child/2` verbatim); restarts are
always admitted, because refusing one would send OTP into its try-again
loop, burning restart intensity until the supervisor shut down.

A start that fails (or raises) releases its slot before returning, so a
failing child never consumes capacity. A refused manual start likewise
releases the slot it just took.
""".
-spec checked_dynamic_start(map()) -> {ok, pid()} | {error, term()}.
checked_dynamic_start(Params) ->
    beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, []).
-doc "See `checked_dynamic_start/1` (`startChild:`, tokened).".
-spec checked_dynamic_start(map(), term()) -> {ok, pid()} | {error, term()}.
checked_dynamic_start(Params, E1) ->
    beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, [E1]).
-doc "See `checked_dynamic_start/1` (`startChild:name:`, tokened).".
-spec checked_dynamic_start(map(), term(), term()) -> {ok, pid()} | {error, term()}.
checked_dynamic_start(Params, E1, E2) ->
    beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, [E1, E2]).
-doc "See `checked_dynamic_start/1`.".
-spec checked_dynamic_start(map(), term(), term(), term()) -> {ok, pid()} | {error, term()}.
checked_dynamic_start(Params, E1, E2, E3) ->
    beamtalk_supervisor_capacity:checked_dynamic_start_n(Params, [E1, E2, E3]).

-doc """
Check if a class name is a Supervisor or DynamicSupervisor subclass.

Used by codegen (Phase 3) to determine routing at compile time and by
`SupervisionSpec childSpec` to determine child `type` and `shutdown`.
ClassName must be the base class name atom (e.g., 'WebApp', not 'WebApp class').

Also on the hot path of every dynamic `spawn`-family send to any class,
actors included (`beamtalk_class_dispatch:class_send/3`), so it walks the
superclass chain once and stops at the first ancestor that decides the
answer: `Supervisor`/`DynamicSupervisor` (true), or `Actor`/`Object`
(false: neither descends from a supervisor base, so a walk that reaches one
has already passed any `Supervisor`/`DynamicSupervisor` in the chain). An `Actor`
subclass is answered after one or two ETS reads instead of two walks to the
root. A per-class cached flag was considered and rejected (BT-3994): it would
go stale when an ancestor is redefined under a different superclass, and the
early exit already makes the common case constant-time.
""".
-spec is_supervisor(atom()) -> boolean().
is_supervisor(ClassName) ->
    spawn_kind(ClassName) =:= supervisor.

-doc """
The spawnable kind of a class, from one walk of its superclass chain:

- `supervisor` — a `Supervisor`/`DynamicSupervisor` (subclass);
- `actor` — an `Actor` (subclass); the two hierarchies are disjoint
  (`Supervisor` and `DynamicSupervisor` are `Object` subclasses);
- `none` — the chain reaches `Object` (or a root class) without either;
- `unknown` — the chain could not be followed to a decision: an ancestor has
  no metadata row (removed, renamed, or not yet loaded) or the walk hit the
  depth guard. Callers must not read `unknown` as "not spawnable".

Shared by `is_supervisor/1` and the shadowed spawn-module lookup
(`beamtalk_class_instantiation:resolve_spawn_module_or_raise/3`, BT-4404).
""".
-spec spawn_kind(atom()) -> supervisor | actor | none | unknown.
spawn_kind(ClassName) ->
    Step = fun
        (Base, _Depth) when Base =:= 'Supervisor'; Base =:= 'DynamicSupervisor' ->
            {found, supervisor};
        ('Actor', _Depth) ->
            {found, actor};
        ('Object', _Depth) ->
            {found, none};
        (Class, _Depth) ->
            case beamtalk_class_metadata:lookup_superclass(Class) of
                {ok, none} -> {found, none};
                {ok, Super} -> {next, Super};
                not_found -> not_found
            end
    end,
    %% Deliberately `walk_ancestors/3`, not `walk_or_warn/4`: `unknown` is a
    %% documented answer (callers must not read it as "not spawnable"), and a
    %% cycle is one of its ordinary causes next to a missing metadata row.
    case beamtalk_hierarchy:walk_ancestors(ClassName, Step, ?MAX_HIERARCHY_DEPTH) of
        {found, Kind} -> Kind;
        _ -> unknown
    end.

-doc """
Register the OTP application root supervisor.

Called from the generated `beamtalk_{appname}_app:start/2` callback after
the root supervisor has started. SupervisorTuple must be a
`{beamtalk_supervisor, ClassName, Module, Pid}` value as returned by
`startLink/1`. Creates the ETS table if it does not already exist.
""".
-spec register_root(term()) -> ok.
register_root(SupervisorTuple) ->
    ensure_root_table(),
    ets:insert(?ROOT_SUPERVISOR_TABLE, {root, SupervisorTuple}),
    ok.

-doc """
Return the registered OTP application root supervisor, or `nil`.

Called by `Program rootSupervisor` (`beamtalk_program:rootSupervisor/0`).
Returns the `{beamtalk_supervisor, ClassName, Module, Pid}` tuple registered
by `register_root/1`, or the Beamtalk `nil` atom if no root supervisor has
been registered (e.g. no `[application]` section in `beamtalk.toml`).
""".
-spec get_root() -> term() | nil.
get_root() ->
    ensure_root_table(),
    ets:lookup_element(?ROOT_SUPERVISOR_TABLE, root, 2, nil).

-doc """
Clear the registered root supervisor entry.

Called when the root supervisor is stopped via `Workspace stopSupervisor:`.
Removes the ETS entry so that `get_root/0` returns `nil` afterwards.
""".
-spec clear_root() -> ok.
clear_root() ->
    ensure_root_table(),
    ets:delete(?ROOT_SUPERVISOR_TABLE, root),
    ok.

-doc """
Start a supervised child by dispatching through its keyword class method.

When a SupervisionSpec uses `withClassMethod:`, the supervisor must
route starts/restarts through the actor's keyword class method (e.g.,
`start:linearClient:`) instead of calling `start_link/init` directly. The class
method transforms raw constructor args into properly shaped state before calling
`spawnWith:`.

Runs the class method directly in the supervisor process via
`call_class_method_direct` (not `class_send`). This is critical because
`gen_server:start_link` inside the class method must link the new child to
the supervisor process — not the class gen_server. Running via `class_send`
would execute inside the class gen_server, linking the child there instead,
breaking OTP supervision semantics (supervisor would not detect child exits).

Process dictionary entries `beamtalk_class_name` / `beamtalk_class_module` are
set temporarily as a defensive fallback for call sites that still read class
identity from the process dictionary (e.g. `handle_class_self_call/1`'s
deadlock-diagnostic hint) when the class method runs correctly in the
supervisor process instead of the class's own gen_server.

`?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` is
also set here (and only here). Unlike `beamtalk_class_name`/
`beamtalk_class_module` — which `beamtalk_object_class`'s `init/1` *also*
sets, in the class's own gen_server process — this key is exclusive to this
function, so `beamtalk_actor:safe_spawn/2` and
`beamtalk_class_instantiation:do_class_self_named_spawn/6` can check it to
tell "running a supervised child's factory in the real supervisor process"
(stay linked — that link is the restart mechanism) apart from "running a
class method inside the class's own gen_server, or a plain unsupervised
spawn" (unlink — the original fix for this distinction). This is a plain process
dictionary read with no process boundary crossed between here and
`safe_spawn/2`: `call_class_method_direct` below reaches the class method via
`erlang:apply/3`, and the compiled `self spawn`/`self spawnWith:` body
reaches `safe_spawn/2` via further direct calls
(`beamtalk_class_instantiation:handle_spawn/4` → `erlang:apply(Module,
spawn, Args)` → the generated `spawn/1` wrapper) — all synchronous, same
process, so the key set here is still visible when `safe_spawn/2` checks it.

`beamtalk_class_is_abstract` is deliberately **not** seeded here.
`self spawnWith:`/`self spawnAs:`/`self spawnWith:as:` in a compiled class
method resolve `is_abstract` by class name via
`beamtalk_class_instantiation:resolve_is_abstract_or_raise/2` (ADR
0109 amendment), and `self new`/`self new:` resolve it via a name-keyed
`beamtalk_class_metadata:lookup_is_abstract/1` lookup inside
`beamtalk_class_instantiation:handle_new_generic/2` — neither reads this PD
entry. Seeding a hard-coded `false` here previously fabricated a cache value
that could let an abstract class bypass the abstract-instantiation guard on
any PD-reading path added in the future; name-keyed metadata is the single
source of truth instead.

The MFA stored in the OTP child spec is:
  {beamtalk_supervisor, start_child_via_class_method, [ClassName, Module, Selector, Args]}
All terms are atoms/lists (no pids), so the MFA survives class object restarts.
Selector is in compiled form (e.g., `class_create:value:`).
""".
-spec start_child_via_class_method(atom(), module(), atom(), [term()]) ->
    {ok, pid()} | {error, term()}.
start_child_via_class_method(ClassName, Module, Selector, Args) ->
    %% Set process dictionary entries needed by legacy PD-reading fallbacks.
    %% These are normally set by the class gen_server during init;
    %% we replicate them here so the class method runs correctly
    %% in the supervisor process. `beamtalk_class_is_abstract` is intentionally
    %% excluded — see the doc comment above.
    put(beamtalk_class_name, ClassName),
    put(beamtalk_class_module, Module),
    %% Mark this process as running a
    %% withClassMethod: child's factory directly in the real OTP supervisor
    %% process, so that any `self spawn`/`self spawnWith:`/`self spawnAs:`/
    %% `self spawnWith:as:` the factory calls (beamtalk_actor:safe_spawn/2,
    %% beamtalk_class_instantiation:do_class_self_named_spawn/6) stays linked
    %% to us instead of unlinking — the link is what lets us, the supervisor,
    %% detect the child's exit and restart it. See ?BT_SUPERVISOR_SPAWN_CONTEXT_KEY.
    put(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY, true),
    try
        ClassSelf = beamtalk_supervisor_class_call:make_init_class_self(ClassName, Module),
        %% See static_init/3: the factory runs in the caller with no class-variable
        %% key installed, so a class-variable write raises.
        Result = beamtalk_supervisor_class_call:call_class_method_direct(
            ClassName, Module, Selector, ClassSelf, Args
        ),
        case Result of
            #beamtalk_object{pid = Pid} when is_pid(Pid) ->
                {ok, Pid};
            {beamtalk_supervisor, _Class, _Mod, Pid} when is_pid(Pid) ->
                {ok, Pid};
            Other ->
                Error = beamtalk_error:new_fmt(
                    runtime_error,
                    ClassName,
                    Selector,
                    "class method must return an Actor object, got: ~tp",
                    [Other]
                ),
                {error, beamtalk_exception_handler:ensure_wrapped(Error)}
        end
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module),
        erase(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY)
    end.

-doc """
BT-4033: the start function of every supervised actor child that is not a
`withClassMethod:` factory — a static `Supervisor` child (`spawn`,
`withArgs:`, `withName:`, `withName:scope:`) and a `DynamicSupervisor` pool
child (`startChild`, `startChild:`, `startChild:name:`), on its first start
and on every OTP restart.

The start is **awaited**, exactly like `Actor spawn`/`spawnWith:` (BT-3992)
and like OTP `init/1`: it returns `{ok, Pid}` only once the child's
`initialize` has returned (not the `continueWith:` steps it requested, which
run after). An `initialize` that raises, fails the uninitialized-state check
or stops the actor fails the start with `{error, Reason}`, so the parent's
start fails (`#supervisor_start_failed` / `#child_start_failed`) instead of
reporting success and then crash-looping the child; an OTP restart that
fails this way counts against the restart intensity. An `initialize` still
running after 5 s (typically a synchronous call back to this supervisor or
an ancestor, which is blocked in this very start — the ADR 0135 hang rule)
fails the start with a hint naming the rule.

`Name` is `none` (an anonymous child, `beamtalk_actor:safe_spawn_linked/2`)
or `{local | global, Atom}` (`withName:` / `startChild:name:`, through
`beamtalk_actor_registry:'spawnAs'/3` / `'spawnAsGlobal'/3`, ADR 0079 / ADR 0126 §4).
Both are the same linked, trap-exit, init-ack-awaited start
(`beamtalk_actor:start_link_and_await/3`); the link is what lets the calling
supervisor see the child's exit. The answer is `{ok, Pid}` or `{error,
Reason}`, with one exception: a `{global, Name}` start (BT-5231, ADR 0126
BT-5193 amendment D1).

A `{global, Name}` start that finds `Name` held by a pid **on another node**
answers `ignore` and logs a warning naming the child and the holder's node.
That is the netsplit-heal case: this node's child lost the name conflict and
was stopped with `{shutdown, global_name_conflict}` (announced as the loser's
`ActorStopped`, reason `#globalNameConflict`). Answering an error would
restart-loop a `#permanent` child until the supervisor reached its restart
intensity and died with its siblings. With `ignore`, OTP keeps the child
spec with no pid, on the initial start and on restarts alike. Nothing
watches the name: the child stays down until a later `restartChild:` finds
it free; while the name is still held, `restartChild:` answers `Ok(nil)`.
Under `one_for_all` / `rest_for_one` the loser's exit still restarts its
siblings as the strategy says.
A `{global, Name}` start whose registration timed out (`global`'s lock held
on another node, BT-5192) also answers `ignore` with a logged warning, for the
same reason.
If the holder is gone by the time the start looks (`global:whereis_name/1`
answers `undefined`), the start is tried once more and otherwise keeps its
error. A holder on the **same** node stays an error (a programming mistake).
An unsupervised `spawnAs:scope: #global` keeps its `name_registered` error;
only this supervised start answers `ignore`. An `{error, ignore}` from the
actor's own start is still an error.

A `native:` actor child (ADR 0056, BT-4577) goes through the same calls:
`ChildModule` is then its facade, which has no `init/1`, and those
`beamtalk_actor` starts resolve the facade's backing module and start it
with `BackingModule:start_link(Args)` instead, as the facade's own `spawn/1`
does (`beamtalk_actor:safe_start_native/4`). A named native child is
registered just after it started.
""".
-spec start_actor_child(module(), term()) -> {ok, pid()} | {error, term()}.
start_actor_child(ChildModule, Args) ->
    start_actor_child(ChildModule, Args, none).

-spec start_actor_child(module(), term(), none | {local | global, atom()}) ->
    {ok, pid()} | ignore | {error, term()}.
start_actor_child(ChildModule, Args, Name) ->
    Result =
        case Name of
            none ->
                beamtalk_actor:safe_spawn_linked(ChildModule, Args);
            {local, LocalName} ->
                beamtalk_actor_registry:'spawnAs'(LocalName, ChildModule, Args);
            {global, GlobalName} ->
                start_global_actor_child(GlobalName, ChildModule, Args, 1)
        end,
    case Result of
        ignore ->
            ignore;
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, beamtalk_supervisor_class_call:actor_child_start_error(ChildModule, Reason)}
    end.

%% A supervised `{global, Name}` start (BT-5231): `ignore` when `Name` is
%% held on another node, one retry when the holder is already gone. A
%% registration that timed out (`global`'s lock held elsewhere, typically a
%% partition-heal name sync, BT-5192) is `ignore` too, for the same reason:
%% failing the start would spend restart intensity on a condition that only
%% clears when `global` does, and could take the siblings down.
-spec start_global_actor_child(atom(), module(), term(), non_neg_integer()) ->
    {ok, pid()} | ignore | {error, term()}.
start_global_actor_child(Name, ChildModule, Args, Retries) ->
    case beamtalk_actor_registry:'spawnAsGlobal'(Name, ChildModule, Args) of
        {error, #beamtalk_error{kind = name_registered}} = Error ->
            case global:whereis_name(Name) of
                Holder when is_pid(Holder), node(Holder) =/= node() ->
                    ?LOG_WARNING(
                        "Supervised #global child ~p (~p) not started: the name is held on ~p",
                        [Name, ChildModule, node(Holder)],
                        #{
                            domain => [beamtalk, runtime],
                            child => Name,
                            holder_node => node(Holder)
                        }
                    ),
                    ignore;
                undefined when Retries > 0 ->
                    start_global_actor_child(Name, ChildModule, Args, Retries - 1);
                _ ->
                    Error
            end;
        {error, #beamtalk_error{kind = timeout}} ->
            ?LOG_WARNING(
                "Supervised #global child ~p (~p) not started: global name registration timed out",
                [Name, ChildModule],
                #{domain => [beamtalk, runtime], child => Name}
            ),
            ignore;
        Other ->
            Other
    end.

-doc """
Start a DynamicSupervisor's default (no per-call args) child.

This is the arity-2 half of the zero-static-arg MFA `spec_to_otp/2` builds
for a DynamicSupervisor's `simple_one_for_one` template (`{beamtalk_supervisor,
start_dynamic_child, [ChildModule, DefaultArgs]}`). `DefaultArgs` is `#{}` for
the plain `#spawn` startFn, or the baked args map for a childClass whose
`class supervisionSpec` override sets `withArgs:` (startFn `#spawnWith:`) —
either way, OTP applies this function at arity 2 whenever no extra args were
appended to the template for a given start — i.e. the no-arg `startChild`
call (`supervisor:start_child(SupPid, [])`).

Automatic OTP restart (`#permanent`/`#transient`) does **not** always land
here: OTP's `simple_one_for_one` supervisor records, per dynamically-started
child, the exact args used to start it (`StaticArgs ++ EArgs` from that
child's own `start_child/2` call — see `supervisor:dyn_store/3` /
`find_child_and_args/2` in OTP's `supervisor.erl`) and replays those same
args verbatim on restart, not just the bare static template. So restart
lands on arity 2 (here) only for a child that was itself started with no
extra args; a child started via `startChild: args` restarts through
`start_dynamic_child/3` with that same `Args` — see its doc.
""".
-spec start_dynamic_child(module(), map()) -> {ok, pid()} | {error, term()}.
start_dynamic_child(ChildModule, DefaultArgs) ->
    start_actor_child(ChildModule, DefaultArgs).

-doc """
Start a DynamicSupervisor child with caller-supplied init args.

This is the arity-3 half of the zero-static-arg MFA `spec_to_otp/2` builds
for a DynamicSupervisor's `simple_one_for_one` template. `startChild: args`
(`beamtalk_supervisor:startChild/2`) appends `[Args]` to the template via
`supervisor:start_child(SupPid, [Args])`, landing OTP's `apply/3` here at
arity 3. `DefaultArgs` (the baked template value) is ignored in favour of
the caller's `Args` — this is what makes `startChild: args` deliver
exactly the args the caller passed to the child's start, instead
of the pre-fix behaviour where OTP concatenated the baked `#{}` and the
caller's `Args` into a single 2-element arg list that `ChildModule` has no
matching arity for and OTP misread as its 4-arity named-registration
`start_link` form, crashing with `badarg`.

Also the entry point automatic OTP restart lands on for a child started
this way: OTP's `simple_one_for_one` bookkeeping (`supervisor:dyn_store/3`)
records this exact call's `[ChildModule, DefaultArgs, Args]` and replays it
verbatim on restart, so a crashed child restarts with the SAME `Args` it
was originally given — not a blank default. See `start_dynamic_child/2`'s
doc for the arity-2 (no-args) counterpart.
""".
-spec start_dynamic_child(module(), map(), term()) -> {ok, pid()} | {error, term()}.
start_dynamic_child(ChildModule, _DefaultArgs, Args) ->
    start_actor_child(ChildModule, Args).

-doc """
Start a DynamicSupervisor child with caller-supplied init args, registered
under a name (ADR 0079 amendment).

The arity-4 entry point `startChild: args name: aName` lands on:
`supervisor:start_child/2` appends `[Args, Name]` to the template's
`[ChildModule, DefaultArgs]` static args. Starts the child through
`start_actor_child/3` with `{local, Name}`, so it registers under `Name`
(through `beamtalk_actor_registry:'spawnAs'/3`) — the same start
`SupervisionSpec withName:` uses for static children (ADR 0079) — and the
start waits for its `initialize` (BT-4033).

Also the entry point automatic OTP restart lands on for a child started
this way: `simple_one_for_one` bookkeeping (`supervisor:dyn_store/3`)
records this exact call's `[ChildModule, DefaultArgs, Args, Name]` and
replays it verbatim on restart, so a crashed named child restarts with the
SAME `Args` and re-registers under the SAME `Name` — not a blank, unnamed
default. See `start_dynamic_child/2,3`'s docs for the unnamed counterparts.
""".
-spec start_dynamic_child(module(), map(), term(), atom()) -> {ok, pid()} | {error, term()}.
start_dynamic_child(ChildModule, _DefaultArgs, Args, Name) ->
    start_actor_child(ChildModule, Args, {local, Name}).

-doc """
Execute Fun(), translating a raw failure to reach the supervisor into a
structured error instead of letting it leak across the public API boundary.
Every Supervisor/DynamicSupervisor handle call runs its OTP calls through
here, so they all report the same two kinds (BT-3984):

* `stale_handle`: the supervisor process is not running. `supervisor:*`
  calls exit with `{noproc, MFA}`; `gen_server:stop/1` with bare `noproc`.
* `node_down`: a remote supervisor's node is down, disconnected or
  did not answer in time (`{{nodedown, Node}, MFA}`, `noconnection`,
  `{timeout, MFA}`, an `erpc` `noconnection`/`timeout`). The supervisor may
  still be running, so this is not reported as a stale handle.

The classification is `classify_call_failure/2`; any other exception
propagates unchanged, as does a failure whose OTP call names another process
(`call_target/1`), or an unreachable-node shape under a local supervisor:
those came from some other call `Fun` made.

## Inner fun contract

Each inner fun returns `{ok, Value}` on success and
`{error, #beamtalk_error{}}` on application-level failure.
`with_live_supervisor/3` returns whichever tagged tuple Fun produced, or the
`{error, #beamtalk_error{}}` above. FFI coercion in
`beamtalk_erlang_proxy:coerce_ffi_result/2` wraps the tagged tuple into
a Beamtalk `Result` for the stdlib method body.
""".
-spec with_live_supervisor(tuple(), atom(), fun(() -> term())) ->
    term() | {error, #beamtalk_error{}}.
with_live_supervisor(Self, Selector, Fun) ->
    try
        Fun()
    catch
        Class:Reason:Stack ->
            case classify_call_failure(Class, Reason) of
                other ->
                    erlang:raise(Class, Reason, Stack);
                {unreachable, _} when node(element(4, Self)) =:= node() ->
                    %% A local supervisor's node cannot be unreachable: the
                    %% timeout/nodedown came from some other call Fun made
                    %% (a child class lookup, say), so it is not ours to map.
                    erlang:raise(Class, Reason, Stack);
                Unavailable ->
                    case call_target(Reason) of
                        {ok, Target} when Target =/= element(4, Self) ->
                            %% The failed call was to another process.
                            erlang:raise(Class, Reason, Stack);
                        _ ->
                            beamtalk_supervisor_errors:supervisor_unavailable_error(
                                Self, Selector, Unavailable
                            )
                    end
            end
    end.

%% The process an OTP call failure names (`{Reason, {M, F, [Target | _]}}`,
%% as `gen_server:call` and `proc_lib:stop` exit), or `none` for a bare reason.
%% A target-less shape (bare `noproc`/`timeout`/`noconnection`, an `erpc`
%% error) is assumed to be the supervisor's own: today only `gen_server:stop/1`
%% (`stop/1`) and `beamtalk_stop:stop/3` (`stopWithin/2`, whose `timeout` exit is
%% caught inside its own fun) raise one inside `with_live_supervisor/3`. A
%% `Fun` that makes its own target-less call (an `erpc` to another node, say)
%% must catch it itself.
-spec call_target(term()) -> {ok, term()} | none.
call_target({_Reason, {M, F, [Target | _]}}) when is_atom(M), is_atom(F) -> {ok, Target};
call_target(_Reason) -> none.

-doc """
Classify an exception raised by a call to a supervisor process (an OTP
`supervisor:*`/`gen_server:*` call, or an `erpc` call to its node): `dead`
when the process does not exist, `{unreachable, Why}` when its node could not
be reached, `other` for anything else. The single place these exit shapes
are recognised (BT-3984).
""".
-spec classify_call_failure(error | exit | throw, term()) -> supervisor_unavailable() | other.
classify_call_failure(Class, Reason) ->
    %% A timeout is "unreachable" here (a slow remote supervisor); `shutdown` /
    %% `killed` stay `other` — see beamtalk_call_failure's moduledoc.
    case beamtalk_call_failure:classify(Class, Reason) of
        timeout -> {unreachable, timeout};
        Classified -> Classified
    end.

-doc """
Ensure the root supervisor ETS table exists.
Uses `public` access so the generated app callback and `Program rootSupervisor`
can both read/write without process ownership constraints. `beamtalk_runtime_app`
creates it at application start so it is owned by the runtime (not by whichever
short-lived process registers a root first) and exists in every boot context.
""".
-spec ensure_root_table() -> ok.
ensure_root_table() ->
    case ets:info(?ROOT_SUPERVISOR_TABLE, id) of
        undefined ->
            try
                ets:new(?ROOT_SUPERVISOR_TABLE, [named_table, public, set])
            catch
                error:badarg ->
                    %% Another process created the table concurrently — that's fine
                    ok
            end,
            ok;
        _ ->
            ok
    end.

-ifdef(TEST).
%% TEST-only delegates: the unit tests reach these through this module.
-spec check_max_children(atom(), term()) -> term().
check_max_children(C, V) -> beamtalk_supervisor_capacity:check_max_children(C, V).
-spec to_otp_shutdown(atom(), atom(), term()) -> term().
to_otp_shutdown(C, S, V) -> beamtalk_supervisor_spec:to_otp_shutdown(C, S, V).
-spec to_otp_strategy(atom()) -> atom().
to_otp_strategy(S) -> beamtalk_supervisor_spec:to_otp_strategy(S).
-spec init_child_capacity(infinity | non_neg_integer()) -> term().
init_child_capacity(M) -> beamtalk_supervisor_capacity:init_child_capacity(M).
-spec wrap_capped_child_spec(map(), non_neg_integer() | infinity, term()) -> map().
wrap_capped_child_spec(S, M, C) -> beamtalk_supervisor_capacity:wrap_capped_child_spec(S, M, C).
-endif.
