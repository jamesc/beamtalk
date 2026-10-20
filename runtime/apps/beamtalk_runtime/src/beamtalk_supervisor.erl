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
    build_child_specs/1,
    spec_to_otp/1,
    spec_to_otp/2,
    is_supervisor/1,
    register_root/1,
    get_root/0,
    ensure_root_table/0,
    clear_root/0,
    run_initialize/1,
    await_initialized/1,
    ensure_init_gate_table/0,
    start_child_via_class_method/4,
    start_dynamic_child/2,
    start_dynamic_child/3,
    start_dynamic_child/4,
    startChild/3
]).

-ifdef(TEST).
-export([
    check_max_children/2,
    classify_call_failure/2,
    with_live_supervisor/3,
    to_otp_shutdown/3,
    to_otp_strategy/1,
    start_failed_error/4,
    supervisor_child_entry/2,
    supervisor_child_entries/1
]).
-endif.

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include_lib("kernel/include/logger.hrl").

%% Why a supervisor handle call could not reach its supervisor (BT-3984):
%% `dead` (no such process: `stale_handle`) or `{unreachable, Why}` (its node
%% is down, disconnected or did not answer in time: `node_down`).
-type supervisor_unavailable() :: dead | {unreachable, nodedown | noconnection | timeout}.
%% One `supervisor:which_children/1` entry.
-type which_child() :: {
    term(), pid() | restarting | undefined, worker | supervisor, [module()] | dynamic
}.
%% A `SupervisorChild` value (BT-3966, `stdlib/src/supervisor_child.bt`). The
%% exact `'$beamtalk_class'` field lets the type checker infer FFI results as
%% `SupervisorChild` (see `beamtalk_process_navigation`'s `node_map()`).
-type supervisor_child() :: #{
    '$beamtalk_class' := 'SupervisorChild',
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
%% BT-3759: bag of `initializing` gates (see run_initialize/1).
-define(INIT_GATE_TABLE, beamtalk_supervisor_init_gates).
%% Upper bound on how long an already_started caller waits for a concurrent
%% initialize hook (covers an initializer that dies before it claims the gate).
-define(INIT_WAIT_TIMEOUT, 60000).
%% Poll interval for a waiting caller (see wait_loop/8).
-define(INIT_POLL_MS, 200).

%% BT-3964: process-dictionary key (in the DynamicSupervisor process) holding
%% the `maxChildren` limit read by `dynamic_init/3`.
-define(MAX_CHILDREN_KEY, '$beamtalk_max_children').

%% ADR 0135: process-dictionary keys (in the supervisor process) set by
%% `static_init/3` / `dynamic_init/3`. `?SUP_ARGS_KEY` holds the start args
%% (`self args`), `?CHILD_CLASS_KEY` a DynamicSupervisor's
%% `{ChildClassName, ChildModule}`, `?SUP_FLAGS_KEY` the configured
%% `#{strategy, maxRestarts, restartWindow}` (read by process navigation).
-define(SUP_ARGS_KEY, '$beamtalk_sup_args').
-define(CHILD_CLASS_KEY, '$beamtalk_child_class').
-define(SUP_FLAGS_KEY, '$beamtalk_sup_flags').
%% ADR 0135 §1: `{ClassName, Module}` of the supervisor, set by `begin_init/3`
%% (read by `named/2` to check the class of a registered supervisor).
-define(SUP_CLASS_KEY, '$beamtalk_sup_class').

%% ADR 0135 §1: the init arg of a top-level spawn-family start is wrapped in
%% `{?TOP_LEVEL_TAG, Args}`; `begin_init/3` unwraps it and joins the instance
%% to `?INSTANCES_GROUP` so `Node current supervisors` finds it.
-define(TOP_LEVEL_TAG, '$beamtalk_top_level').
-define(INSTANCES_GROUP, beamtalk_supervisor_instances).

%% ADR 0135 §2: hint for a configuration method that messaged its own
%% supervisor (OTP `calling_self`) while it was still in `init/1`.
-define(CALLING_SELF_HINT, <<
    "configuration methods run inside the supervisor's init; use `self args` "
    "and return specs, do not send messages to `self`"
>>).

%% BT-3965: largest Integer shutdown timeout. OTP's own child-spec check has
%% no upper bound, but a static supervisor waits with `receive ... after`,
%% whose timeout is limited to 16#FFFFFFFF ms; a larger value would crash the
%% supervisor mid-shutdown. `#infinity` covers anything longer.
-define(MAX_SHUTDOWN_MS, 16#FFFFFFFF).

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
class_start_concrete(ClassName, _Module, Selector, Args) ->
    %% A dynamic send with the wrong argument count (`perform:withArguments:`).
    beamtalk_error:raise(
        beamtalk_error:new(
            arity_mismatch,
            ClassName,
            Selector,
            iolist_to_binary(
                io_lib:format("~ts takes a different number of arguments, got ~b", [
                    Selector, length(Args)
                ])
            )
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
and the arming of the `initializing` gate (a node-local `global` lock keyed
by module): that is what the class gen_server used to provide, so a second
caller that gets `already_started` always finds the gate armed.
""".
-spec supervise(atom(), module()) ->
    {ok, {beamtalk_supervisor, atom(), module(), pid()}} | {error, #beamtalk_error{}}.
supervise(ClassName, Module) ->
    %% No `whereis` fast path before the lock: the name is registered before
    %% `init/1` runs, so a running-looking singleton may still be inside its
    %% start with no gate armed yet. The lock makes such a caller wait until the
    %% gate is armed.
    Started = global:trans(
        {{beamtalk_supervise, Module}, self()},
        fun() ->
            %% Arm the gate before unlinking: if this caller dies after the
            %% unlink but before `run_initialize/1` claims the gate, the next
            %% `supervise` waits on it and reclaims the orphan at the deadline
            %% instead of answering an uninitialised singleton.
            start_unlinked(fun() ->
                case Module:start_link() of
                    {ok, Pid} = Ok ->
                        arm_init_gate(Pid),
                        Ok;
                    Other ->
                        Other
                end
            end)
        end,
        [node()],
        infinity
    ),
    case Started of
        {ok, Pid} ->
            ?LOG_INFO("Supervisor started", #{
                supervisor => ClassName, module => Module, pid => Pid, domain => [beamtalk, runtime]
            }),
            SupTuple = {beamtalk_supervisor, ClassName, Module, Pid},
            run_initialize(SupTuple),
            {ok, SupTuple};
        {error, {already_started, Pid}} ->
            SupTuple = {beamtalk_supervisor, ClassName, Module, Pid},
            await_initialized(SupTuple),
            {ok, SupTuple};
        Failure ->
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
            {error, start_failed_error(supervisor_start_failed, ClassName, supervise, Reason)}
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
                beamtalk_error:new(
                    name_not_registered,
                    ClassName,
                    'named:',
                    iolist_to_binary(
                        io_lib:format("No supervisor is registered under name '~ts'", [Name])
                    )
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
        beamtalk_error:new(
            type_error,
            ClassName,
            'named:',
            iolist_to_binary(io_lib:format("named: expects a Symbol, got ~tp", [Name]))
        )}.

-spec wrong_class_error(atom(), atom(), atom()) -> {error, #beamtalk_error{}}.
wrong_class_error(ClassName, Name, not_a_supervisor) ->
    {error,
        beamtalk_error:new(
            wrong_class,
            ClassName,
            'named:',
            iolist_to_binary(
                io_lib:format("Name '~ts' is not registered to a Beamtalk supervisor", [Name])
            )
        )};
wrong_class_error(ClassName, Name, Actual) ->
    {error,
        beamtalk_error:new(
            wrong_class,
            ClassName,
            'named:',
            iolist_to_binary(
                io_lib:format(
                    "Name '~ts' is registered to a ~ts, not a ~ts", [Name, Actual, ClassName]
                )
            )
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
        beamtalk_object_class:class_name_safe(ClassPid),
        beamtalk_object_class:module_name_safe(ClassPid)
    }.

%% `spawn` / `spawnWith:`: start an anonymous, unlinked top-level instance and
%% run its `initialize` in the caller. Raises `#supervisor_start_failed`.
-spec spawn_instance(atom(), module(), term(), atom()) -> tuple().
spawn_instance(ClassName, Module, Args, Selector) ->
    StartFun = fun() -> supervisor:start_link(Module, {?TOP_LEVEL_TAG, Args}) end,
    case start_top_level(ClassName, Module, StartFun) of
        {ok, SupTuple} ->
            SupTuple;
        {error, Reason} ->
            beamtalk_error:raise(
                start_failed_error(supervisor_start_failed, ClassName, Selector, Reason)
            )
    end.

%% `spawnAs:` / `spawnWith:as:`: start a top-level instance registered as
%% `{local, Name}` (unlinked). A taken name is `#name_registered`, a reserved
%% one `#reserved_name`, a non-Symbol `#type_error`, as for `Actor spawnAs:`.
-spec spawn_named_instance(atom(), module(), term(), term(), atom()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
spawn_named_instance(ClassName, _Module, _Args, Name, Selector) when not is_atom(Name) ->
    {error,
        beamtalk_error:new(
            type_error,
            ClassName,
            Selector,
            iolist_to_binary(io_lib:format("~ts expects a Symbol name, got ~tp", [Selector, Name]))
        )};
spawn_named_instance(ClassName, Module, Args, Name, Selector) ->
    case beamtalk_actor:reserved_name(Name) of
        true ->
            {error,
                beamtalk_error:new(
                    reserved_name,
                    ClassName,
                    Selector,
                    iolist_to_binary(
                        io_lib:format("Cannot start a supervisor under reserved name '~ts'", [Name])
                    )
                )};
        false ->
            StartFun = fun() ->
                supervisor:start_link({local, Name}, Module, {?TOP_LEVEL_TAG, Args})
            end,
            case start_top_level(ClassName, Module, StartFun) of
                {ok, _} = Ok ->
                    Ok;
                {error, {already_started, _}} ->
                    {error,
                        beamtalk_error:new(
                            name_registered,
                            ClassName,
                            Selector,
                            iolist_to_binary(
                                io_lib:format("Name '~ts' is already registered", [Name])
                            )
                        )};
                {error, Reason} ->
                    {error,
                        start_failed_error(supervisor_start_failed, ClassName, Selector, Reason)}
            end
    end.

%% Shared tail of the spawn family: start (unlinked), then run `initialize`
%% in the caller. `initialize` raising stops the instance and re-raises.
-spec start_top_level(atom(), module(), fun(() -> term())) ->
    {ok, tuple()} | {error, term()}.
start_top_level(ClassName, Module, StartFun) ->
    case start_unlinked(StartFun) of
        {ok, Pid} ->
            ?LOG_INFO("Supervisor instance started", #{
                supervisor => ClassName, module => Module, pid => Pid, domain => [beamtalk, runtime]
            }),
            SupTuple = {beamtalk_supervisor, ClassName, Module, Pid},
            run_initialize(SupTuple),
            {ok, SupTuple};
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
    case beamtalk_actor:reserved_name(Name) of
        true ->
            %% Same reserved-name policy as `spawnAs:` (ADR 0079).
            {error,
                beamtalk_error:new(
                    reserved_name,
                    'Supervisor',
                    'withName:',
                    iolist_to_binary(
                        io_lib:format("Cannot start a supervisor under reserved name '~ts'", [Name])
                    )
                )};
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
        beamtalk_error:new(
            type_error,
            'DynamicSupervisor',
            'startChild:name:',
            iolist_to_binary(io_lib:format("name must be a Symbol, got ~tp", [Name]))
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
Build the `Kind` error (`supervisor_start_failed` for `supervise`,
`child_start_failed` for `startChild`) for a failed start.

A configuration failure raised inside a supervisor's `init/1`
(`static_init/3`, `dynamic_init/3`) exits with a `#beamtalk_error{}`
(ADR 0135 §2): for `supervise` that is the start reason, and for a
DynamicSupervisor whose child class is a supervisor it is the child's start
reason; for a static parent whose nested supervisor child failed it is
wrapped in OTP's `{shutdown, {failed_to_start_child, Id, _}}`. Its hint (the
validator's message, or the `calling_self` hint) — or, when it has none, its
message — becomes the hint of the start error, with the original error in
`details`. Any other reason is formatted into the hint as before.
""".
-spec start_failed_error(atom(), atom(), atom(), term()) -> #beamtalk_error{}.
start_failed_error(Kind, ClassName, Selector, #beamtalk_error{} = Cause) ->
    Error0 = beamtalk_error:new(Kind, ClassName, Selector, cause_hint(Cause)),
    beamtalk_error:with_details(Error0, #{cause => Cause});
start_failed_error(
    Kind, ClassName, Selector, {shutdown, {failed_to_start_child, _Id, _Inner}} = Reason
) ->
    case nested_start_cause(Reason) of
        {ok, Cause} -> start_failed_error(Kind, ClassName, Selector, Cause);
        none -> format_start_failed_error(Kind, ClassName, Selector, Reason)
    end;
start_failed_error(Kind, ClassName, Selector, Reason) ->
    format_start_failed_error(Kind, ClassName, Selector, Reason).

%% The `#beamtalk_error{}` at the bottom of any depth of nested
%% `failed_to_start_child` wrappers (Root -> MidSup -> Pool), if there is one.
-spec nested_start_cause(term()) -> {ok, #beamtalk_error{}} | none.
nested_start_cause(#beamtalk_error{} = Cause) ->
    {ok, Cause};
nested_start_cause({shutdown, {failed_to_start_child, _Id, Inner}}) ->
    nested_start_cause(Inner);
nested_start_cause(_) ->
    none.

-spec format_start_failed_error(atom(), atom(), atom(), term()) -> #beamtalk_error{}.
format_start_failed_error(Kind, ClassName, Selector, Reason) ->
    Verb =
        case Kind of
            child_start_failed -> "start_child";
            _ -> "start_link"
        end,
    beamtalk_error:new(
        Kind,
        ClassName,
        Selector,
        iolist_to_binary(io_lib:format("supervisor ~s failed: ~p", [Verb, Reason]))
    ).

%% The text to show for a start failure's cause: its hint, else its message,
%% else its kind.
-spec cause_hint(#beamtalk_error{}) -> binary().
cause_hint(#beamtalk_error{hint = Hint}) when is_binary(Hint), Hint =/= <<>> ->
    Hint;
cause_hint(#beamtalk_error{message = Message}) when is_binary(Message), Message =/= <<>> ->
    Message;
cause_hint(#beamtalk_error{kind = Kind}) ->
    atom_to_binary(Kind, utf8).

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
    Self = begin_init(Module, ClassName, Args),
    Children = read_config(Self, ClassName, childSpecs),
    BtStrategy = read_config(Self, ClassName, strategy),
    Strategy = config_result(check_strategy(ClassName, BtStrategy)),
    {MaxR, MaxT} = read_restart_intensity(Self, ClassName),
    put(?SUP_FLAGS_KEY, #{strategy => BtStrategy, maxRestarts => MaxR, restartWindow => MaxT}),
    SupFlags = #{strategy => Strategy, intensity => MaxR, period => MaxT},
    Specs = config_step(ClassName, childSpecs, fun() -> build_child_specs(Children) end),
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
    Self = begin_init(Module, ClassName, Args),
    ChildClass = read_config(Self, ClassName, childClass),
    {MaxR, MaxT} = read_restart_intensity(Self, ClassName),
    MaxC = config_result(check_max_children(ClassName, read_config(Self, ClassName, maxChildren))),
    ChildShutdown = config_result(
        check_child_shutdown(ClassName, read_config(Self, ClassName, childShutdown))
    ),
    ChildClassEntry = child_class_entry(ClassName, ChildClass),
    %% BT-3964: OTP's `supervisor` has no `max_children` flag (that is Elixir's
    %% DynamicSupervisor), so the limit is enforced by `startChild/1,2,3` via
    %% `with_child_capacity/4`. `init/1` runs in the supervisor process, so the
    %% limit is stashed in its process dictionary: it lives exactly as long as
    %% this supervisor incarnation and is re-read from the instance on restart.
    put(?MAX_CHILDREN_KEY, MaxC),
    put(?CHILD_CLASS_KEY, ChildClassEntry),
    put(?SUP_FLAGS_KEY, #{
        strategy => simpleOneForOne, maxRestarts => MaxR, restartWindow => MaxT
    }),
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    %% BT-3965: `childShutdown` (when set) replaces the shutdown the child
    %% class's own `supervisionSpec` produced. `simple_one_for_one` has
    %% exactly one child spec, so the value covers every dynamic child.
    Specs = config_step(ClassName, childClass, fun() ->
        [
            apply_child_shutdown(S, ChildShutdown)
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

%% Read and check `maxRestarts` / `restartWindow` (non-negative / positive
%% Integers, as OTP requires) so a bad value fails with a clear hint.
-spec read_restart_intensity(tuple(), atom()) -> {non_neg_integer(), pos_integer()}.
read_restart_intensity(Self, ClassName) ->
    MaxR = read_config(Self, ClassName, maxRestarts),
    MaxT = read_config(Self, ClassName, restartWindow),
    _ = config_result(check_integer(ClassName, maxRestarts, MaxR, 0)),
    _ = config_result(check_integer(ClassName, restartWindow, MaxT, 1)),
    {MaxR, MaxT}.

-spec check_integer(atom(), atom(), term(), integer()) ->
    {ok, integer()} | {error, #beamtalk_error{}}.
check_integer(_ClassName, _Selector, Value, Min) when is_integer(Value), Value >= Min ->
    {ok, Value};
check_integer(ClassName, Selector, Value, Min) ->
    Error0 = beamtalk_error:new(
        type_error,
        ClassName,
        Selector,
        iolist_to_binary(
            io_lib:format("~s must be an Integer >= ~b, got ~p", [Selector, Min, Value])
        )
    ),
    {error, beamtalk_error:with_details(Error0, #{value => Value})}.

%% Check a static supervisor's `strategy` and translate it to OTP's atom.
-spec check_strategy(atom(), term()) ->
    {ok, one_for_one | one_for_all | rest_for_one} | {error, #beamtalk_error{}}.
check_strategy(ClassName, Value) ->
    case to_otp_strategy(Value) of
        Otp when Otp =:= one_for_one; Otp =:= one_for_all; Otp =:= rest_for_one ->
            {ok, Otp};
        _ ->
            Error0 = beamtalk_error:new(
                type_error,
                ClassName,
                strategy,
                iolist_to_binary(
                    io_lib:format(
                        "strategy must be #oneForOne, #oneForAll or #restForOne, got ~p", [Value]
                    )
                )
            ),
            {error, beamtalk_error:with_details(Error0, #{value => Value})}
    end.

%% Shared prologue of `static_init/3` / `dynamic_init/3`: record the start
%% args for `self args` and the class for `named:`, and return this
%% supervisor's own handle. A top-level spawn-family start wraps its args in
%% `{?TOP_LEVEL_TAG, Args}` (ADR 0135 §1): unwrap them and join
%% `?INSTANCES_GROUP` before `start_link` returns, so the instance is listed by
%% process navigation for as long as it runs (`pg` drops it on exit).
-spec begin_init(module(), atom(), term()) -> tuple().
begin_init(Module, ClassName, {?TOP_LEVEL_TAG, Args}) ->
    ok = join_instances_group(),
    begin_init(Module, ClassName, Args);
begin_init(Module, ClassName, Args) ->
    put(?SUP_ARGS_KEY, Args),
    put(?SUP_CLASS_KEY, {ClassName, Module}),
    {beamtalk_supervisor, ClassName, Module, self()}.

%% Join the default-scope `pg` group of top-level instances. `pg` is started
%% by `beamtalk_bootstrap`; without it (a bare test node) the instance is
%% simply not listed.
-spec join_instances_group() -> ok.
join_instances_group() ->
    %% `init/1` can run again in the same process (a supervisor's
    %% `code_change` re-runs it with the stored args): join only once.
    try
        case lists:member(self(), pg:get_local_members(?INSTANCES_GROUP)) of
            true -> ok;
            false -> pg:join(?INSTANCES_GROUP, self())
        end
    catch
        error:badarg -> ok;
        exit:{noproc, _} -> ok
    end.

-doc """
Read one configuration setting by sending `Selector` to the supervisor being
started (`Self`, its own handle): ordinary instance dispatch, in this
process (ADR 0135 §2).

The send runs under `beamtalk_class_vars:with_snapshot/2` (ADR 0130 §5), as
the class-side configuration did. Any failure ends `init/1` via
`config_step/3`.
""".
-spec read_config(tuple(), atom(), atom()) -> term().
read_config(Self, ClassName, Selector) ->
    Module = element(3, Self),
    ClassSelf = make_init_class_self(ClassName, Module),
    config_step(ClassName, Selector, fun() ->
        beamtalk_class_vars:with_snapshot(ClassSelf, fun() ->
            beamtalk_message_dispatch:send(Self, Selector, [])
        end)
    end).

-doc """
Run one step of supervisor configuration inside `init/1`; on failure, exit
`init/1` with a `#beamtalk_error{}` (ADR 0135 §2).

`supervise/2` and the spawn family report such an exit as `supervisor_start_failed`
carrying the error's hint (a parent reports a nested failure as its own
start failure). A configuration method that sent a message to its own
supervisor (OTP `calling_self`, raw or wrapped by dispatch) gets the
`?CALLING_SELF_HINT` instead.
""".
-spec config_step(atom(), atom(), fun(() -> T)) -> T.
config_step(ClassName, Selector, Fun) ->
    %% bt-catcher-audit: not-applicable-reraises - every clause exits init/1, so the
    %% supervisor process (and any class-variable region in it) ends here
    try
        Fun()
    catch
        Class:Reason:Stack ->
            exit(config_error(ClassName, Selector, Class, Reason, Stack))
    end.

-spec config_error(atom(), atom(), atom(), term(), list()) -> #beamtalk_error{}.
config_error(ClassName, Selector, Class, Reason, Stack) ->
    case contains_calling_self(Reason) of
        true ->
            Error0 = beamtalk_error:new(
                supervisor_start_failed, ClassName, Selector, ?CALLING_SELF_HINT
            ),
            beamtalk_error:with_details(Error0, #{reason => calling_self});
        false ->
            #{error := BtError} = beamtalk_exception_handler:ensure_wrapped(
                Class, Reason, Stack, #{selector => Selector, class => ClassName}
            ),
            BtError
    end.

%% Does an exception reason (raw, or a wrapped Beamtalk error/exception)
%% carry OTP's `calling_self` exit (`{calling_self, {gen_server, call, _}}`)
%% anywhere inside it?
-spec contains_calling_self(term()) -> boolean().
contains_calling_self({calling_self, {gen_server, _, _}}) ->
    true;
contains_calling_self(T) when is_tuple(T) ->
    lists:any(fun contains_calling_self/1, tuple_to_list(T));
contains_calling_self(M) when is_map(M) ->
    lists:any(fun contains_calling_self/1, maps:values(M));
contains_calling_self(L) when is_list(L) ->
    contains_calling_self_list(L);
contains_calling_self(_) ->
    false.

%% Walk a possibly improper list.
-spec contains_calling_self_list(maybe_improper_list()) -> boolean().
contains_calling_self_list([]) ->
    false;
contains_calling_self_list([H | T]) ->
    contains_calling_self(H) orelse contains_calling_self_list(T);
contains_calling_self_list(Tail) ->
    contains_calling_self(Tail).

%% Unwrap a `check_*` result inside `init/1`: the validator's error ends
%% `init/1` (reported as `supervisor_start_failed` with its hint).
-spec config_result({ok, T} | {error, #beamtalk_error{}}) -> T.
config_result({ok, Value}) -> Value;
config_result({error, #beamtalk_error{} = Error}) -> exit(Error).

%% Resolve a DynamicSupervisor's `childClass` value to the
%% `{ChildClassName, ChildModule}` entry stored for `pool_child_class/1`.
-spec child_class_entry(atom(), term()) -> {atom(), module()}.
child_class_entry(ClassName, ChildClass) ->
    case is_tuple(ChildClass) andalso beamtalk_class_registry:is_class_object(ChildClass) of
        true ->
            {beamtalk_object_class:class_name_safe(element(4, ChildClass)), element(3, ChildClass)};
        false ->
            Error0 = beamtalk_error:new(
                type_error,
                ClassName,
                childClass,
                iolist_to_binary(
                    io_lib:format("childClass must return a class, got ~p", [ChildClass])
                )
            ),
            exit(beamtalk_error:with_details(Error0, #{value => ChildClass}))
    end.

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
        {ok, Value} -> {ok, Value};
        undefined -> {ok, nil};
        Unavailable -> supervisor_unavailable_error(Self, args, Unavailable)
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
        true -> warn_if_supervisor(beamtalk_object_class:class_name_safe(ClassPid), Selector);
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
    try erpc:call(node(Pid), erlang, process_info, [Pid, {dictionary, Key}], 5000) of
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
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
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
        ModClasses = module_classes(Running),
        {ok,
            lists:reverse([
                child_object(ChildClass, Module, P)
             || {_Id, P, _T, Mods} <- Running,
                {ChildClass, Module} <- [entry_class(Mods, ModClasses)]
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
        {ok, supervisor_child_entries(supervisor:which_children(Pid))}
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
    with_child_id(Self, Selector, Id, fun() ->
        case lists:keyfind(Id, 1, supervisor:which_children(Pid)) of
            false ->
                {error, child_not_found_error(SupClass, Selector, Id)};
            {_Id, ChildPid, _Type, Mods} when is_pid(ChildPid) ->
                {ClassName, Module} = class_for_modules(Mods),
                {ok, child_object(ClassName, Module, ChildPid)};
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
    with_child_id(Self, 'terminateChild:', Id, fun() ->
        handle_terminate_child_result(
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
    with_child_id(Self, Selector, Id, fun() ->
        case supervisor:restart_child(Pid, Id) of
            {ok, ChildPid} ->
                {ok, restarted_child(Pid, Id, ChildPid)};
            {ok, ChildPid, _Info} ->
                {ok, restarted_child(Pid, Id, ChildPid)};
            {error, not_found} ->
                {error, child_not_found_error(SupClass, Selector, Id)};
            {error, running} ->
                {error, child_running_error(SupClass, Selector, Id, running)};
            {error, restarting} ->
                {error, child_running_error(SupClass, Selector, Id, restarting)};
            {error, Reason} ->
                {error, start_failed_error(child_start_failed, SupClass, Selector, Reason)}
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
    with_child_id(Self, Selector, Id, fun() ->
        case supervisor:delete_child(Pid, Id) of
            ok -> {ok, nil};
            {error, not_found} -> {error, child_not_found_error(SupClass, Selector, Id)};
            {error, running} -> {error, child_running_error(SupClass, Selector, Id, running)};
            {error, restarting} -> {error, child_running_error(SupClass, Selector, Id, restarting)};
            {error, Reason} -> {error, delete_failed_error(SupClass, Selector, Reason)}
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
                with_pool_child_class(Self, children, fun(ChildClass, ChildModule) ->
                    {ok, [wrap_child(ChildClass, ChildModule, P) || P <- Pids]}
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
        case children_for_module(supervisor:which_children(SupPid), ChildModule) of
            [] ->
                {ok, nil};
            [{_Id, ChildPid, _, _}] when is_pid(ChildPid) ->
                ChildClass = beamtalk_object_class:class_name_safe(ChildClassPid),
                {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
            [_NotRunning] ->
                {ok, nil};
            Matches ->
                {error, ambiguous_child_error(ClassName, 'which:', Matches)}
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
                case children_for_module(supervisor:which_children(SupPid), ChildModule) of
                    [] ->
                        {ok, nil};
                    [{ChildId, _, _, _}] ->
                        handle_terminate_child_result(
                            SupClass,
                            #{child => ChildId},
                            supervisor:terminate_child(SupPid, ChildId)
                        );
                    Matches ->
                        {error, ambiguous_child_error(SupClass, 'terminate:', Matches)}
                end;
            false when is_tuple(Arg), tuple_size(Arg) >= 4, is_pid(element(4, Arg)) ->
                %% DynamicSupervisor case: terminate by child process pid
                ChildPid = element(4, Arg),
                handle_terminate_child_result(
                    SupClass,
                    #{child_pid => ChildPid},
                    supervisor:terminate_child(SupPid, ChildPid)
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

%% Shared outcome dispatcher for both static and dynamic terminateChild paths.
%% `ok` → {ok, nil}; `{error, not_found}` → {ok, nil} (idempotent);
%% other `{error, Reason}` → {error, #beamtalk_error{kind = terminate_failed}}.
-spec handle_terminate_child_result(
    atom(), map(), ok | {error, not_found} | {error, term()}
) ->
    {ok, nil} | {error, #beamtalk_error{}}.
handle_terminate_child_result(SupClass, LogCtx, ok) ->
    ?LOG_INFO(
        "Supervisor child terminated",
        (LogCtx)#{supervisor => SupClass, domain => [beamtalk, runtime]}
    ),
    {ok, nil};
handle_terminate_child_result(SupClass, LogCtx, {error, not_found}) ->
    %% ADR 0080 Phase 1: idempotent — "child already gone" is success.
    ?LOG_DEBUG(
        "Supervisor child already terminated",
        (LogCtx)#{supervisor => SupClass, domain => [beamtalk, runtime]}
    ),
    {ok, nil};
handle_terminate_child_result(SupClass, _LogCtx, {error, Reason}) ->
    {error,
        beamtalk_error:new(
            terminate_failed,
            SupClass,
            'terminateChild:',
            iolist_to_binary(io_lib:format("~p", [Reason]))
        )}.

%%% ============================================================================
%%% Child introspection by id (BT-3966)
%%% ============================================================================

-doc """
Map `supervisor:which_children/1` output to `SupervisorChild` values in
declaration order. OTP lists a static supervisor's children newest first, so
the list is reversed. Module-to-class resolution reads the class metadata
table once for the whole list.
""".
-spec supervisor_child_entries([which_child()]) -> [supervisor_child()].
supervisor_child_entries(Children) ->
    ModClasses = module_classes(Children),
    lists:reverse([supervisor_child_entry(Entry, ModClasses) || Entry <- Children]).

-doc """
Map one `supervisor:which_children/1` entry to a `SupervisorChild` value
(the tagged map behind `stdlib/src/supervisor_child.bt`). A pid gives
`status => running` and the wrapped child object; `restarting` gives
`status => restarting`, anything else (`undefined`) `status => stopped`, both
with `child => nil`. `ModClasses` maps child modules to Beamtalk class names
(`module_classes/1`); a module with no class gives `childClass => nil` and,
when running, the bare pid as `child`. Never raises.
""".
-spec supervisor_child_entry(which_child(), #{module() => atom()}) -> supervisor_child().
supervisor_child_entry({Id, PidOrStatus, Type, Mods}, ModClasses) ->
    {ClassName, ChildModule} = entry_class(Mods, ModClasses),
    {Status, Child} =
        case PidOrStatus of
            ChildPid when is_pid(ChildPid) ->
                {running, child_object(ClassName, ChildModule, ChildPid)};
            restarting ->
                {restarting, nil};
            _ ->
                {stopped, nil}
        end,
    #{
        '$beamtalk_class' => 'SupervisorChild',
        id => Id,
        child => Child,
        status => Status,
        type => Type,
        childClass => child_class_object(ClassName)
    }.

%% `{ClassName | nil, Module | nil}` for one child's modules: the single-child
%% counterpart of `module_classes/1` + `entry_class/2`.
-spec class_for_modules([module()] | dynamic) -> {atom() | nil, module() | nil}.
class_for_modules(Mods) ->
    entry_class(Mods, beamtalk_class_registry:module_to_class_map()).

%% Module -> Beamtalk class name map for resolving many entries' modules: one
%% read of the class metadata table (`module_to_class_map/0`), not one per
%% child, and none when there are no entries.
-spec module_classes([which_child()]) -> #{module() => atom()}.
module_classes([]) ->
    #{};
module_classes(_Children) ->
    beamtalk_class_registry:module_to_class_map().

-spec entry_class([module()] | dynamic, #{module() => atom()}) ->
    {atom() | nil, module() | nil}.
entry_class([Module | _], ModClasses) when is_atom(Module) ->
    {maps:get(Module, ModClasses, nil), Module};
entry_class(_Mods, _ModClasses) ->
    {nil, nil}.

-spec child_object(atom() | nil, module() | nil, pid()) -> beamtalk_object() | pid().
child_object(nil, _Module, ChildPid) -> ChildPid;
child_object(ClassName, Module, ChildPid) -> wrap_child(ClassName, Module, ChildPid).

-spec child_class_object(atom() | nil) -> beamtalk_object() | nil.
child_class_object(nil) ->
    nil;
child_class_object(ClassName) ->
    case beamtalk_class_registry:resolve_class_object(ClassName) of
        undefined -> nil;
        ClassObj -> ClassObj
    end.

%% The `which_children` entries whose modules include `Module`, in any state.
-spec children_for_module([which_child()], module()) -> [which_child()].
%% A `simple_one_for_one` child has no id (`undefined`) and cannot be addressed
%% by id, so it never matches: `terminate:`/`which:` by class on a
%% DynamicSupervisor find nothing rather than an `undefined` id per child.
children_for_module(Children, Module) ->
    [
        Entry
     || {Id, _P, _T, Mods} = Entry <- Children,
        Id =/= undefined,
        is_list(Mods),
        lists:member(Module, Mods)
    ].

%% The child object for a pid `supervisor:restart_child/2` just returned.
-spec restarted_child(pid(), atom() | integer(), pid() | undefined) ->
    beamtalk_object() | pid() | nil.
restarted_child(SupPid, Id, ChildPid) when is_pid(ChildPid) ->
    %% `get_childspec/2` reads one spec, not the whole child list.
    case supervisor:get_childspec(SupPid, Id) of
        {ok, #{modules := Mods}} ->
            {ClassName, Module} = class_for_modules(Mods),
            child_object(ClassName, Module, ChildPid);
        _ ->
            ChildPid
    end;
restarted_child(_SupPid, _Id, _NoPid) ->
    nil.

%% Validate a child id (a Symbol or an Integer, ADR 0135 §3) and run `Fun`
%% against the live supervisor.
-spec with_child_id(tuple(), atom(), term(), fun(() -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_child_id(Self, Selector, Id, Fun) when is_atom(Id); is_integer(Id) ->
    with_live_supervisor(Self, Selector, Fun);
with_child_id(Self, Selector, Id, _Fun) ->
    SupClass = element(2, Self),
    Error = beamtalk_error:new(
        type_error,
        SupClass,
        Selector,
        <<"A child id is a Symbol or an Integer">>
    ),
    {error, beamtalk_error:with_details(Error, #{id => Id})}.

-spec child_not_found_error(atom(), atom(), atom() | integer()) -> #beamtalk_error{}.
child_not_found_error(SupClass, Selector, Id) ->
    Error0 = beamtalk_error:new(child_not_found, SupClass, Selector),
    Error1 = beamtalk_error:with_message(
        Error0, iolist_to_binary(io_lib:format("No child with id ~p", [Id]))
    ),
    beamtalk_error:with_details(Error1, #{id => Id}).

-spec child_running_error(atom(), atom(), atom() | integer(), running | restarting) ->
    #beamtalk_error{}.
child_running_error(SupClass, Selector, Id, Status) ->
    Error0 = beamtalk_error:new(
        child_running,
        SupClass,
        Selector,
        <<"Terminate the child with terminateChild: first">>
    ),
    Error1 = beamtalk_error:with_message(
        Error0, iolist_to_binary(io_lib:format("Child ~p is ~s", [Id, Status]))
    ),
    beamtalk_error:with_details(Error1, #{id => Id, status => Status}).

-spec ambiguous_child_error(atom(), atom(), [which_child()]) -> #beamtalk_error{}.
ambiguous_child_error(SupClass, Selector, Matches) ->
    %% Declaration order, as `whichChildren` lists them.
    Ids = lists:reverse([Id || {Id, _, _, _} <- Matches]),
    Error0 = beamtalk_error:new(
        ambiguous_child,
        SupClass,
        Selector,
        <<"Several children share this class; use childWithId: / terminateChild: with an id">>
    ),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(io_lib:format("Several children share this class: ~p", [Ids]))
    ),
    beamtalk_error:with_details(Error1, #{ids => Ids}).

-spec delete_failed_error(atom(), atom(), term()) -> #beamtalk_error{}.
delete_failed_error(SupClass, Selector, Reason) ->
    beamtalk_error:new(
        delete_failed,
        SupClass,
        Selector,
        iolist_to_binary(io_lib:format("~p", [Reason]))
    ).

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
    with_pool_child_class(Self, startChild, fun(ChildClass, ChildModule) ->
        with_child_capacity(SupClass, startChild, SupPid, fun() ->
            case supervisor:start_child(SupPid, []) of
                {ok, ChildPid} ->
                    ?LOG_INFO("DynamicSupervisor child started", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        module => ChildModule,
                        child_pid => ChildPid,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_added(SupClass, ChildClass, ChildPid),
                    {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
                {error, Reason} ->
                    ?LOG_ERROR("DynamicSupervisor child start failed", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        reason => Reason,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_crashed(SupClass, ChildClass, Reason),
                    {error, start_failed_error(child_start_failed, SupClass, startChild, Reason)}
            end
        end)
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
    with_pool_child_class(Self, 'startChild:', fun(ChildClass, ChildModule) ->
        with_child_capacity(SupClass, 'startChild:', SupPid, fun() ->
            case supervisor:start_child(SupPid, [Args]) of
                {ok, ChildPid} ->
                    ?LOG_INFO("DynamicSupervisor child started", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        module => ChildModule,
                        child_pid => ChildPid,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_added(SupClass, ChildClass, ChildPid),
                    {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
                {error, Reason} ->
                    ?LOG_ERROR("DynamicSupervisor child start failed", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        reason => Reason,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_crashed(SupClass, ChildClass, Reason),
                    {error, start_failed_error(child_start_failed, SupClass, 'startChild:', Reason)}
            end
        end)
    end).

-doc """
Start a new child with args, registered under a name, under a DynamicSupervisor.

Called from `startChild: args name: aName` on DynamicSupervisor instances
(ADR 0079 amendment). Combines args-replay with named
registration: `Name` is appended as a third extra argument that
`supervisor:start_child/2` threads onto the shared `simple_one_for_one`
template, landing on `start_dynamic_child/4`, which spawns the child via
`beamtalk_actor:'spawnAs'/3` instead of a bare `ChildModule:start_link/1` —
the same primitive `SupervisionSpec withName:` already uses for static
children (ADR 0079). Because `simple_one_for_one` replays each dynamic
child's own start args (now including `Name`) on automatic OTP restart, a
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
    with_pool_child_class(Self, 'startChild:name:', fun(ChildClass, ChildModule) ->
        with_child_capacity(SupClass, 'startChild:name:', SupPid, fun() ->
            case supervisor:start_child(SupPid, [Args, Name]) of
                {ok, ChildPid} ->
                    ?LOG_INFO("DynamicSupervisor named child started", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        module => ChildModule,
                        child_pid => ChildPid,
                        name => Name,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_added(SupClass, ChildClass, ChildPid),
                    {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
                {error, #beamtalk_error{} = Err} ->
                    %% `'spawnAs'/3` already returns a structured error for
                    %% name_registered/type_error/reserved_name — re-attribute
                    %% it to this call site rather than wrapping it again.
                    ?LOG_ERROR("DynamicSupervisor named child start failed", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        name => Name,
                        reason => Err#beamtalk_error.kind,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_crashed(SupClass, ChildClass, Err),
                    {error, Err#beamtalk_error{class = SupClass, selector = 'startChild:name:'}};
                {error, {already_started, _}} ->
                    %% A supervisor child class (ADR 0135 §3) registers
                    %% through `supervisor:start_link({local, Name}, ...)`,
                    %% which reports a taken name this way.
                    announce_child_crashed(SupClass, ChildClass, name_registered),
                    {error,
                        beamtalk_error:new(
                            name_registered,
                            SupClass,
                            'startChild:name:',
                            iolist_to_binary(
                                io_lib:format("Name '~ts' is already registered", [Name])
                            )
                        )};
                {error, Reason} ->
                    ?LOG_ERROR("DynamicSupervisor named child start failed", #{
                        supervisor => SupClass,
                        child => ChildClass,
                        name => Name,
                        reason => Reason,
                        domain => [beamtalk, runtime]
                    }),
                    announce_child_crashed(SupClass, ChildClass, Reason),
                    {error,
                        start_failed_error(
                            child_start_failed, SupClass, 'startChild:name:', Reason
                        )}
            end
        end)
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

A local supervisor is checked with `erlang:is_process_alive/1`; a remote one
answers `true` optimistically, as for actors (`beamtalk_pid:is_alive/1`): the
next call to it reports `stale_handle` or `node_down`.
""".
-spec isAlive(term()) -> boolean().
isAlive({beamtalk_supervisor, ClassName, _Module, Pid}) when is_pid(Pid) ->
    ClassName =/= undefined andalso beamtalk_pid:is_alive(Pid);
isAlive(_) ->
    false.

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
instead of baking `[#{}]` directly into `{ChildModule, start_link, _}` —
see `spec_to_otp/2`. `Mode = static` (the default, used by
`static_init/3` via `build_child_specs/1`) is unchanged: a static
Supervisor's children start immediately at supervisor-init time and never
receive appended `supervisor:start_child/2` args, so the original
`[#{}]` shape is still correct there.
""".
-spec build_child_specs([term()], static | dynamic) -> [map()].
build_child_specs(Children, Mode) ->
    [build_child_spec(C, Mode) || C <- Children].

-doc """
Check if a class name is a Supervisor or DynamicSupervisor subclass.

Used by codegen (Phase 3) to determine routing at compile time and by
`SupervisionSpec childSpec` to determine child `type` and `shutdown`.
ClassName must be the base class name atom (e.g., 'WebApp', not 'WebApp class').
""".
-spec is_supervisor(atom()) -> boolean().
is_supervisor(ClassName) ->
    beamtalk_class_registry:inherits_from(ClassName, 'Supervisor') orelse
        beamtalk_class_registry:inherits_from(ClassName, 'DynamicSupervisor').

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

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc """
Run the instance-side `initialize` lifecycle hook on a freshly started
supervisor (ADR 0135 §1).

Called by `supervise/2` and the spawn family (`start_top_level/3`) right after
the start, in the caller's process — never the class gen_server — so messages
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

-doc """
Build a ClassSelf tuple for use in direct class method calls during supervisor init.
The pid field is set to the class gen_server pid (may be blocked, but ClassSelf is
used only as a value object — pure class methods do not send messages to self).
""".
-spec make_init_class_self(atom(), module()) -> beamtalk_object().
make_init_class_self(ClassName, Module) ->
    ClassPid = beamtalk_class_registry:whereis_class(ClassName),
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    {beamtalk_object, ClassTag, Module, ClassPid}.

-doc """
Call a class method directly by invoking the module function, bypassing the
class gen_server, with extra user-facing arguments. Tries the subclass module
first, then walks the class hierarchy via ETS until the method is found in an
ancestor's module.
The compiled convention is `class_<sel>(ClassSelf, Args...)` returning the bare
result (ADR 0130 §3), so the arity is `1 + length(ExtraArgs)`. The call runs in
`beamtalk_class_vars:with_snapshot/2`: class-variable reads see the ETS mirror
(or the live map if this process is already inside the class's invocation) and
writes raise `class_state_read_only`.
""".
-spec call_class_method_direct(atom(), module(), atom(), tuple(), [term()]) -> term().
call_class_method_direct(ClassName, Module, FunName, ClassSelf, ExtraArgs) ->
    beamtalk_class_vars:with_snapshot(ClassSelf, fun() ->
        Arity = 1 + length(ExtraArgs),
        case erlang:function_exported(Module, FunName, Arity) of
            true ->
                erlang:apply(Module, FunName, [ClassSelf | ExtraArgs]);
            false ->
                call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ExtraArgs, 0)
        end
    end).

-spec call_inherited_class_method_direct(
    atom(), atom(), tuple(), [term()], non_neg_integer()
) ->
    term().
call_inherited_class_method_direct(_ClassName, FunName, _ClassSelf, _ExtraArgs, Depth) when
    Depth > 30
->
    error({supervisor_init_method_not_found, FunName});
call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ExtraArgs, Depth) ->
    Arity = 1 + length(ExtraArgs),
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        not_found ->
            error({supervisor_init_method_not_found, FunName});
        {ok, SuperclassName} ->
            %% Look up the ancestor module via ETS instead of gen_server:call.
            %% If the ancestor is itself a Supervisor subclass currently being initialised,
            %% its class gen_server is blocked inside startLink/1 waiting for OTP
            %% supervisor:start_link to return.  A gen_server:call to it would deadlock.
            %% beamtalk_class_metadata stores module names written during class init
            %% and is safe to read from any process without coordination.
            case beamtalk_class_metadata:lookup_module(SuperclassName) of
                not_found ->
                    %% Class not yet registered or module not yet recorded — skip upward.
                    call_inherited_class_method_direct(
                        SuperclassName, FunName, ClassSelf, ExtraArgs, Depth + 1
                    );
                {ok, SuperModule} ->
                    case erlang:function_exported(SuperModule, FunName, Arity) of
                        true ->
                            erlang:apply(SuperModule, FunName, [ClassSelf | ExtraArgs]);
                        false ->
                            call_inherited_class_method_direct(
                                SuperclassName, FunName, ClassSelf, ExtraArgs, Depth + 1
                            )
                    end
            end
    end.

-doc "Build a single OTP child spec from a class object or SupervisionSpec map.".
-spec build_child_spec(term(), static | dynamic) -> map().
build_child_spec(ClassObj, Mode) when
    is_tuple(ClassObj), element(1, ClassObj) =:= beamtalk_object
->
    case beamtalk_class_registry:is_class_object(ClassObj) of
        true ->
            %% A bare class, actor or supervisor alike (ADR 0135 §3): its
            %% `supervisionSpec`, then `childSpec`. A bare nested supervisor
            %% is therefore an anonymous child, like any other.
            ChildClassPid = element(4, ClassObj),
            BtSpec = beamtalk_object_class:class_send(ChildClassPid, 'supervisionSpec', []),
            spec_to_otp(beamtalk_message_dispatch:send(BtSpec, 'childSpec', []), Mode);
        false ->
            %% Actor instance passed as spec — treat as SupervisionSpec-like value
            spec_to_otp(beamtalk_message_dispatch:send(ClassObj, 'childSpec', []), Mode)
    end;
build_child_spec(Spec, Mode) when is_map(Spec) ->
    %% SupervisionSpec value (tagged map): call childSpec directly
    spec_to_otp(beamtalk_message_dispatch:send(Spec, 'childSpec', []), Mode).

-doc """
Translate Beamtalk strategy symbol to OTP supervisor strategy atom.

OTP expects snake_case atoms (one_for_one), while Beamtalk uses camelCase
symbols (#oneForOne). Unknown strategies pass through unchanged so OTP
can report the error with context.
""".
-spec to_otp_strategy(atom()) -> atom().
to_otp_strategy(oneForOne) -> one_for_one;
to_otp_strategy(oneForAll) -> one_for_all;
to_otp_strategy(restForOne) -> rest_for_one;
to_otp_strategy(S) -> S.

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
        ClassSelf = make_init_class_self(ClassName, Module),
        %% See static_init/3: the factory runs in a read-only snapshot region,
        %% so a class-variable write raises instead of being discarded.
        Result = call_class_method_direct(ClassName, Module, Selector, ClassSelf, Args),
        case Result of
            #beamtalk_object{pid = Pid} when is_pid(Pid) ->
                {ok, Pid};
            {beamtalk_supervisor, _Class, _Mod, Pid} when is_pid(Pid) ->
                {ok, Pid};
            Other ->
                Error = beamtalk_error:new(
                    runtime_error,
                    ClassName,
                    Selector,
                    iolist_to_binary(
                        io_lib:format(
                            "class method must return an Actor object, got: ~p",
                            [Other]
                        )
                    )
                ),
                {error, beamtalk_exception_handler:ensure_wrapped(Error)}
        end
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module),
        erase(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY)
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
-spec start_dynamic_child(module(), map()) -> {ok, pid()} | ignore | {error, term()}.
start_dynamic_child(ChildModule, DefaultArgs) ->
    ChildModule:start_link(DefaultArgs).

-doc """
Start a DynamicSupervisor child with caller-supplied init args.

This is the arity-3 half of the zero-static-arg MFA `spec_to_otp/2` builds
for a DynamicSupervisor's `simple_one_for_one` template. `startChild: args`
(`beamtalk_supervisor:startChild/2`) appends `[Args]` to the template via
`supervisor:start_child(SupPid, [Args])`, landing OTP's `apply/3` here at
arity 3. `DefaultArgs` (the baked template value) is ignored in favour of
the caller's `Args` — this is what makes `startChild: args` deliver
exactly the args the caller passed to `ChildModule:start_link/1`, instead
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
-spec start_dynamic_child(module(), map(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_dynamic_child(ChildModule, _DefaultArgs, Args) ->
    ChildModule:start_link(Args).

-doc """
Start a DynamicSupervisor child with caller-supplied init args, registered
under a name (ADR 0079 amendment).

The arity-4 entry point `startChild: args name: aName` lands on:
`supervisor:start_child/2` appends `[Args, Name]` to the template's
`[ChildModule, DefaultArgs]` static args. Delegates to
`beamtalk_actor:'spawnAs'/3` instead of `ChildModule:start_link/1` so the
child starts under `{local, Name}` registration — the same primitive
`SupervisionSpec withName:` uses for static children (ADR 0079).

Also the entry point automatic OTP restart lands on for a child started
this way: `simple_one_for_one` bookkeeping (`supervisor:dyn_store/3`)
records this exact call's `[ChildModule, DefaultArgs, Args, Name]` and
replays it verbatim on restart, so a crashed named child restarts with the
SAME `Args` and re-registers under the SAME `Name` — not a blank, unnamed
default. See `start_dynamic_child/2,3`'s docs for the unnamed counterparts.
""".
-spec start_dynamic_child(module(), map(), term(), atom()) -> {ok, pid()} | {error, term()}.
start_dynamic_child(ChildModule, _DefaultArgs, Args, Name) ->
    beamtalk_actor:'spawnAs'(Name, ChildModule, Args).

-doc """
Convert a Beamtalk child spec dict to an OTP-compatible child spec map.

The Beamtalk dict from `SupervisionSpec childSpec` has keys:
  id, start ([ClassObj, FnAtom, ArgsList]), restart, shutdown, type
The `start` value is a Beamtalk Array [ClassObj, #spawn, #()] that must be
converted to the OTP MFA tuple {Module, Function, Args}.

For nested supervisor children (Supervisor/DynamicSupervisor subclasses), the
start goes through the `start_supervisor_child` trampoline (ADR 0135 §3; see
`supervisor_child_start/4`), so the child is anonymous, gets the spec's args
and stays linked into the tree.

For worker children, Beamtalk's spawn/0 returns {beamtalk_object,...} which
OTP supervisor does not accept (it expects {ok, Pid}). The generated
start_link/1 returns {ok, Pid} directly from gen_server:start_link, so
worker children use start_link/1 with an init-args map instead of spawn/0.

`Mode` selects between a static Supervisor's immediate-start semantics and
a DynamicSupervisor's extensible `simple_one_for_one` template — see the
`spawn` case below.
""".
-spec spec_to_otp(map()) -> map().
spec_to_otp(BtSpec) ->
    spec_to_otp(BtSpec, static).

-spec spec_to_otp(map(), static | dynamic) -> map().
spec_to_otp(BtSpec, Mode) ->
    %% `start` is a Beamtalk Array #[ClassObj, StartFn, StartArgs]. Beamtalk
    %% Arrays are tagged maps with a canonical index→value `'data'` map (ADR
    %% 0090); read the elements as an ordered list via beamtalk_tagged_map.
    StartBtArray = maps:get(start, BtSpec),
    StartElems = beamtalk_tagged_map:array_to_list(StartBtArray),
    ClassObj = lists:nth(1, StartElems),
    ChildModule = element(3, ClassObj),
    ChildClassPid = element(4, ClassObj),
    ChildClass = beamtalk_object_class:class_name_safe(ChildClassPid),
    OtpStart =
        case is_supervisor(ChildClass) of
            true ->
                supervisor_child_start(ChildClass, ChildModule, StartElems, Mode);
            false ->
                %% Worker: use start_link/1 (returns {ok, Pid}) for OTP compatibility.
                %% spawn/0 wraps the pid in {beamtalk_object,...} which OTP rejects.
                StartFn = lists:nth(2, StartElems),
                case StartFn of
                    spawn when Mode =:= dynamic ->
                        %% A DynamicSupervisor's simple_one_for_one template
                        %% must NOT bake a static arg here. OTP appends whatever extra
                        %% args supervisor:start_child/2 was given to this MFA's args —
                        %% baking `[#{}]` meant `startChild: args` landed as
                        %% `ChildModule:start_link(#{}, Args)`, a 2-arity call that
                        %% doesn't exist as start_link/1 and gets misread by OTP as the
                        %% 4-arity named-registration form, crashing with badarg.
                        %% Using a zero-static-arg indirection through
                        %% start_dynamic_child/2,3 instead lets both the no-arg
                        %% `startChild` (arity 2: DefaultArgs) and `startChild: args`
                        %% (arity 3: caller's Args wins) resolve to the single correct
                        %% argument for `ChildModule:start_link/1`. See
                        %% start_dynamic_child/2,3's docs for how each restarts —
                        %% OTP's own simple_one_for_one bookkeeping replays each
                        %% child's own original start args, so this is unaffected
                        %% either way.
                        {beamtalk_supervisor, start_dynamic_child, [ChildModule, #{}]};
                    spawn ->
                        %% Static Supervisor: no init args — start with empty state map.
                        %% Static children start once at supervisor-init time and never
                        %% have extra args appended, so a single baked arg is correct.
                        {ChildModule, start_link, [#{}]};
                    'spawnWith:' when Mode =:= dynamic ->
                        %% A DynamicSupervisor childClass
                        %% can override `class supervisionSpec` to bake default args via
                        %% `withArgs:` (supervision_spec.bt childSpec), which also compiles
                        %% to startFn #spawnWith:. That hits the exact same arity-mismatch
                        %% badarg as the plain #spawn case once `startChild: args` appends
                        %% its own args on top of the baked ones — so it needs the same
                        %% zero-static-arg indirection, using the baked map as DefaultArgs
                        %% instead of #{}.
                        [InitArgsMap] = lists:nth(3, StartElems),
                        {beamtalk_supervisor, start_dynamic_child, [ChildModule, InitArgsMap]};
                    'spawnWith:' ->
                        %% #(self args) uses list syntax (#(...)) so it compiles to an
                        %% Erlang list [ArgsMap] — use it directly as the start_link arg.
                        InitArgs = lists:nth(3, StartElems),
                        {ChildModule, start_link, InitArgs};
                    'spawnAs:' ->
                        %% ADR 0079: SupervisionSpec withName: routes
                        %% named children through `beamtalk_actor:spawnAs/2,3`
                        %% so the child registers under `Name` atomically inside
                        %% gen_server:start_link({local, Name}, ...). The startArgs
                        %% in `SupervisionSpec childSpec` carry just the Name as
                        %% `#(name)` — i.e. an Erlang list `[Name]`.
                        SpawnAsArgs = lists:nth(3, StartElems),
                        [Name] = SpawnAsArgs,
                        {beamtalk_actor, 'spawnAs', [Name, ChildModule]};
                    'spawnWith:as:' ->
                        %% ADR 0079: spawn-with-args under a registered
                        %% name. startArgs are `#(args, name)` → Erlang `[Args, Name]`.
                        %% Translates to `beamtalk_actor:spawnAs/3` so the child is
                        %% registered atomically with init args at start time.
                        SpawnAsArgs2 = lists:nth(3, StartElems),
                        [InitArgsMap, NameAtom] = SpawnAsArgs2,
                        {beamtalk_actor, 'spawnAs', [NameAtom, ChildModule, InitArgsMap]};
                    'spawnAs:scope:' ->
                        %% ADR 0126 §4, Phase 5: `SupervisionSpec withName:
                        %% scope: #global` — same shape as `spawnAs:` above
                        %% (startArgs carries just the Name, `#(name)` →
                        %% Erlang `[Name]`), but routes through
                        %% `beamtalk_actor:'spawnAsGlobal'/2` so the child
                        %% registers with OTP `global` instead of the local
                        %% registry. `childSpec` only ever emits this
                        %% startFn when `scope` is `#global` — `#local`
                        %% still emits plain `#spawnAs:` above.
                        SpawnAsGlobalArgs = lists:nth(3, StartElems),
                        [GlobalName] = SpawnAsGlobalArgs,
                        {beamtalk_actor, 'spawnAsGlobal', [GlobalName, ChildModule]};
                    'spawnWith:as:scope:' ->
                        %% ADR 0126 §4, Phase 5: cluster-global spawn-with-args
                        %% sibling of `spawnWith:as:` above — same startArgs
                        %% shape (`#(args, name)` → Erlang `[Args, Name]`),
                        %% routed through `beamtalk_actor:'spawnAsGlobal'/3`.
                        SpawnAsGlobalArgs2 = lists:nth(3, StartElems),
                        [GlobalInitArgsMap, GlobalNameAtom] = SpawnAsGlobalArgs2,
                        {beamtalk_actor, 'spawnAsGlobal', [
                            GlobalNameAtom, ChildModule, GlobalInitArgsMap
                        ]};
                    classMethod ->
                        %% Route through the actor's keyword class method.
                        %% StartArgs is #(selector, argsList) — compiled as an
                        %% Erlang list [Selector, ArgsList]. The selector is stored
                        %% in source form (e.g., 'create:value:'); we prepend 'class_'
                        %% to get the compiled function name for call_class_method_direct.
                        [Selector, ArgsList] = lists:nth(3, StartElems),
                        % elp:fixme W0023 intentional atom creation
                        CompiledSelector = list_to_atom(
                            "class_" ++ atom_to_list(Selector)
                        ),
                        {beamtalk_supervisor, start_child_via_class_method, [
                            ChildClass, ChildModule, CompiledSelector, ArgsList
                        ]};
                    Other ->
                        %% Unknown start function — raise structured error rather than crashing.
                        %% Tag as 'SupervisionSpec'/'childSpec' to reflect where the spec
                        %% originated (SupervisionSpec>>childSpec), not the supervisor itself.
                        Error = beamtalk_error:new(
                            runtime_error,
                            'SupervisionSpec',
                            childSpec,
                            iolist_to_binary(
                                io_lib:format(
                                    "unsupported child start function: ~p "
                                    "(expected spawn, spawnWith:, spawnAs:, "
                                    "spawnWith:as:, spawnAs:scope:, "
                                    "spawnWith:as:scope:, or classMethod)",
                                    [Other]
                                )
                            )
                        ),
                        beamtalk_exception_handler:reraise(Error)
                end
        end,
    #{
        id => maps:get(id, BtSpec),
        start => OtpStart,
        restart => maps:get(restart, BtSpec),
        shutdown => to_otp_shutdown('SupervisionSpec', shutdown, maps:get(shutdown, BtSpec)),
        type => maps:get(type, BtSpec),
        modules => [ChildModule]
    }.

-doc """
The start MFA for a nested supervisor child (ADR 0135 §3), from the
`#[Class, StartFn, StartArgs]` that `SupervisionSpec childSpec` built.

A static parent's child starts through `start_supervisor_child/2,3` with the
spec's args (`nil` without `withArgs:`) and, for `withName:`, its name. A
DynamicSupervisor's `simple_one_for_one` template is
`start_dynamic_supervisor_child` with the spec's args as the default, so
`startChild` / `startChild:` / `startChild:name:` each start a new child.
`withClassMethod:` (a class-method factory would run the start in the class
process and link the child there) and `scope: #global` are not supported for
supervisor children: they raise a structured `type_error`, which fails the
parent's start.
""".
-spec supervisor_child_start(atom(), module(), [term()], static | dynamic) ->
    {module(), atom(), [term()]}.
supervisor_child_start(ChildClass, ChildModule, StartElems, Mode) ->
    StartFn = lists:nth(2, StartElems),
    StartArgs = lists:nth(3, StartElems),
    {Args, Name} =
        case {StartFn, StartArgs} of
            {spawn, []} -> {nil, none};
            {'spawnWith:', [A]} -> {A, none};
            {'spawnAs:', [N]} -> {nil, N};
            {'spawnWith:as:', [A, N]} -> {A, N};
            _ -> unsupported_supervisor_child(ChildClass, StartFn)
        end,
    case {Mode, Name} of
        {static, none} ->
            {beamtalk_supervisor, start_supervisor_child, [ChildModule, Args]};
        {static, _} ->
            {beamtalk_supervisor, start_supervisor_child, [ChildModule, Args, Name]};
        {dynamic, none} ->
            {beamtalk_supervisor, start_dynamic_supervisor_child, [ChildModule, Args]};
        {dynamic, _} ->
            %% A pool's single template cannot carry one name for every
            %% child; `startChild:name:` names each child instead.
            unsupported_supervisor_child(ChildClass, StartFn)
    end.

-spec unsupported_supervisor_child(atom(), term()) -> no_return().
unsupported_supervisor_child(ChildClass, StartFn) ->
    Hint = iolist_to_binary(
        io_lib:format(
            "~ts is a supervisor: its supervisionSpec supports withId:, withArgs:, "
            "withRestart:, withShutdown: and withName: (a static child), not ~ts",
            [ChildClass, unsupported_field(StartFn)]
        )
    ),
    Error0 = beamtalk_error:new(type_error, ChildClass, supervisionSpec, Hint),
    beamtalk_exception_handler:reraise(
        beamtalk_error:with_details(Error0, #{start => StartFn})
    ).

-spec unsupported_field(term()) -> string().
unsupported_field(classMethod) -> "withClassMethod:";
unsupported_field('spawnAs:scope:') -> "scope: #global";
unsupported_field('spawnWith:as:scope:') -> "scope: #global";
unsupported_field('spawnAs:') -> "withName: on a DynamicSupervisor's child class";
unsupported_field('spawnWith:as:') -> "withName: on a DynamicSupervisor's child class";
unsupported_field(Other) -> io_lib:format("~p", [Other]).

-doc """
Wrap a child pid with the correct Beamtalk tuple tag.
Supervisor subclasses use {beamtalk_supervisor, ...} so follow-up sends
use the supervisor dispatch path rather than the actor/gen_server path.
""".
-spec wrap_child(atom(), module(), pid()) -> tuple().
wrap_child(ChildClass, ChildModule, ChildPid) ->
    case is_supervisor(ChildClass) of
        true -> {beamtalk_supervisor, ChildClass, ChildModule, ChildPid};
        false -> {beamtalk_object, ChildClass, ChildModule, ChildPid}
    end.

-doc """
Announce `SupervisionChildAdded` on the `SystemAnnouncer` bus after a
DynamicSupervisor successfully starts a child (ADR 0093 §2 / ADR 0092).
Best-effort and fault-isolated — see `announce_supervision/2`.
""".
-spec announce_child_added(atom(), atom(), pid()) -> ok.
announce_child_added(SupClass, ChildClass, ChildPid) ->
    announce_supervision('SupervisionChildAdded', #{
        supervisor => SupClass, childClass => ChildClass, childPid => ChildPid
    }).

-doc """
Announce `SupervisionChildCrashed` on the `SystemAnnouncer` bus when a
DynamicSupervisor child fails to start (ADR 0093 §2 / ADR 0092). The
arbitrary OTP failure `Reason` is normalised to a Symbol for the typed event
payload. Best-effort and fault-isolated — see `announce_supervision/2`.
""".
-spec announce_child_crashed(atom(), atom(), term()) -> ok.
announce_child_crashed(SupClass, ChildClass, Reason) ->
    announce_supervision('SupervisionChildCrashed', #{
        supervisor => SupClass,
        childClass => ChildClass,
        childPid => nil,
        reason => normalize_crash_reason(Reason)
    }).

-doc """
Normalise an OTP `supervisor:start_child/2` failure reason to a stable Symbol
for the `SupervisionChildCrashed` event payload: a `#beamtalk_error{}`
(e.g. from `startChild:name:`'s `'spawnAs'/3` path) yields its `kind`
(e.g. `name_registered`), a leading atom is kept (e.g. `already_present`),
anything else collapses to `child_start_failed`. Keeps the typed
`reason :: Symbol` field flat (the full reason is in the returned
`#beamtalk_error{}` for diagnostics).
""".
-spec normalize_crash_reason(term()) -> atom().
normalize_crash_reason(#beamtalk_error{kind = Kind}) -> Kind;
normalize_crash_reason(Reason) when is_atom(Reason) -> Reason;
normalize_crash_reason({Reason, _}) when is_atom(Reason) -> Reason;
normalize_crash_reason(_Reason) -> child_start_failed.

-doc """
Emit a supervision-lifecycle system event on the `SystemAnnouncer` bus.

Guarded by a `whereis` check (the announcements worker starts after bootstrap)
and wrapped in try/catch: announcing is a best-effort observability side effect
and must never fail or delay the supervision operation.
""".
-spec announce_supervision(atom(), map()) -> ok.
announce_supervision(EventClass, Fields) ->
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
    %% bt-catcher-audit: not-applicable-other-process - Fun is a supervisor:* call; child code
    %% runs in the supervisor
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
                            supervisor_unavailable_error(Self, Selector, Unavailable)
                    end
            end
    end.

%% The process an OTP call failure names (`{Reason, {M, F, [Target | _]}}`,
%% as `gen_server:call` and `proc_lib:stop` exit), or `none` for a bare reason.
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
classify_call_failure(exit, noproc) -> dead;
classify_call_failure(exit, {noproc, _}) -> dead;
classify_call_failure(exit, {{nodedown, _Node}, _}) -> {unreachable, nodedown};
classify_call_failure(exit, {nodedown, _Node}) -> {unreachable, nodedown};
classify_call_failure(exit, noconnection) -> {unreachable, noconnection};
classify_call_failure(exit, {noconnection, _}) -> {unreachable, noconnection};
classify_call_failure(exit, timeout) -> {unreachable, timeout};
classify_call_failure(exit, {timeout, _}) -> {unreachable, timeout};
classify_call_failure(error, {erpc, noconnection}) -> {unreachable, noconnection};
classify_call_failure(error, {erpc, timeout}) -> {unreachable, timeout};
classify_call_failure(_Class, _Reason) -> other.

-doc """
The `{error, #beamtalk_error{}}` for a supervisor the handle `Self` could not
reach: `stale_handle` (`stale_handle_error/2`) for `dead`; for
`{unreachable, Why}`, `node_down` (the kind a remote actor send reports for
the same condition, ADR 0126 §7.1) with `details => #{node => Node, reason =>
Why}`.
""".
-spec supervisor_unavailable_error(tuple(), atom(), supervisor_unavailable()) ->
    {error, #beamtalk_error{}}.
supervisor_unavailable_error(Self, Selector, dead) ->
    stale_handle_error(element(2, Self), Selector);
supervisor_unavailable_error(Self, Selector, {unreachable, Why}) ->
    ClassName = element(2, Self),
    Node = node(element(4, Self)),
    ?LOG_WARNING("Supervisor node unreachable", #{
        supervisor => ClassName,
        selector => Selector,
        node => Node,
        reason => Why,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(
        node_down,
        ClassName,
        Selector,
        <<"The supervisor may still be running; retry when its node is reachable">>
    ),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format("Supervisor's node ~s is unreachable (~s)", [Node, Why])
        )
    ),
    {error, beamtalk_error:with_details(Error1, #{node => Node, reason => Why})}.

-doc """
Validate a DynamicSupervisor's `maxChildren` value (BT-3964).

Accepts a non-negative integer or `infinity` (Beamtalk `#infinity`); anything
else is a `type_error` carrying the offending value in `details`.
""".
-spec check_max_children(atom(), term()) ->
    {ok, non_neg_integer() | infinity} | {error, #beamtalk_error{}}.
check_max_children(_ClassName, infinity) ->
    {ok, infinity};
check_max_children(_ClassName, Max) when is_integer(Max), Max >= 0 ->
    {ok, Max};
check_max_children(ClassName, Other) ->
    Error0 = beamtalk_error:new(
        type_error,
        ClassName,
        maxChildren,
        iolist_to_binary(
            io_lib:format(
                "maxChildren must be a non-negative Integer or #infinity, got ~p", [Other]
            )
        )
    ),
    {error, beamtalk_error:with_details(Error0, #{value => Other})}.

-doc """
Translate a Beamtalk shutdown value into the OTP child-spec `shutdown` term
(BT-3965).

Accepts an Integer from 0 to `?MAX_SHUTDOWN_MS` (milliseconds), `infinity`
(Beamtalk `#infinity`) or `brutalKill` (Beamtalk `#brutalKill`, emitted as
OTP `brutal_kill`); anything else raises a `type_error` attributed to
`Class`/`Selector`, with the offending value in `details`. The single place
the `ShutdownTimeout` rule is checked: both `SupervisionSpec childSpec`
(via `spec_to_otp/2`) and `DynamicSupervisor childShutdown` go
through it.
""".
-spec to_otp_shutdown(atom(), atom(), term()) -> brutal_kill | infinity | non_neg_integer().
to_otp_shutdown(Class, Selector, Value) ->
    case check_shutdown(Class, Selector, Value) of
        {ok, Shutdown} -> Shutdown;
        {error, Error} -> beamtalk_error:raise(Error)
    end.

-spec check_shutdown(atom(), atom(), term()) ->
    {ok, brutal_kill | infinity | non_neg_integer()} | {error, #beamtalk_error{}}.
check_shutdown(_Class, _Selector, infinity) ->
    {ok, infinity};
check_shutdown(_Class, _Selector, brutalKill) ->
    {ok, brutal_kill};
check_shutdown(_Class, _Selector, Ms) when is_integer(Ms), Ms >= 0, Ms =< ?MAX_SHUTDOWN_MS ->
    {ok, Ms};
check_shutdown(Class, Selector, Other) ->
    Error0 = beamtalk_error:new(
        type_error,
        Class,
        Selector,
        iolist_to_binary(
            io_lib:format(
                "~s must be an Integer from 0 to ~b (ms), #infinity or #brutalKill, got ~p",
                [Selector, ?MAX_SHUTDOWN_MS, Other]
            )
        )
    ),
    {error, beamtalk_error:with_details(Error0, #{value => Other})}.

-doc """
Check a DynamicSupervisor's `childShutdown` (BT-3965): `nil` (use the
child class's own spec value) or any value `check_shutdown/3` accepts.
""".
-spec check_child_shutdown(atom(), term()) ->
    {ok, nil | brutal_kill | infinity | non_neg_integer()} | {error, #beamtalk_error{}}.
check_child_shutdown(_ClassName, nil) ->
    {ok, nil};
check_child_shutdown(ClassName, Value) ->
    check_shutdown(ClassName, childShutdown, Value).

-spec apply_child_shutdown(map(), nil | brutal_kill | infinity | non_neg_integer()) -> map().
apply_child_shutdown(Spec, nil) ->
    Spec;
apply_child_shutdown(#{type := supervisor, id := Id} = Spec, Shutdown) when
    Shutdown =/= infinity
->
    %% OTP allows it, but a supervisor child with a finite shutdown can be
    %% killed while it is still stopping its own children, which then exit
    %% with `killed` instead of their own shutdown. Applied as configured.
    ?LOG_WARNING("DynamicSupervisor childShutdown is not #infinity for a supervisor child", #{
        child => Id,
        child_shutdown => Shutdown,
        domain => [beamtalk, runtime]
    }),
    Spec#{shutdown => Shutdown};
apply_child_shutdown(Spec, Shutdown) ->
    Spec#{shutdown => Shutdown}.

-doc """
Run `Fun(ChildClass, ChildModule)` for a live DynamicSupervisor, with its
child class resolved by `pool_child_class/1`. A dead supervisor is
`stale_handle`; an unresolvable child class is returned as its error.
""".
-spec with_pool_child_class(tuple(), atom(), fun((atom(), module()) -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_pool_child_class(Self, Selector, Fun) ->
    with_live_supervisor(Self, Selector, fun() ->
        case pool_child_class(Self) of
            {ok, {ChildClass, ChildModule}} -> Fun(ChildClass, ChildModule);
            {error, #beamtalk_error{} = Error} -> {error, Error#beamtalk_error{selector = Selector}}
        end
    end).

-doc """
Resolve a DynamicSupervisor's child class as `{ChildClassName, ChildModule}`
(ADR 0135 §2).

`dynamic_init/3` stores the value the pool instance's `childClass` returned
in the pool's process dictionary (`?CHILD_CLASS_KEY`); it is read from there,
locally or via `erpc` for a remote pool. A pool without the entry (started
before ADR 0135) falls back to the module of a running child, from
`supervisor:which_children/1`; with no running child the result is a
structured `child_class_unknown` error. The class-side `childClass` is never
consulted: configuration is per instance.
""".
-spec pool_child_class(tuple()) -> {ok, {atom(), module()}} | {error, #beamtalk_error{}}.
pool_child_class({beamtalk_supervisor, SupClass, _Module, SupPid} = Self) ->
    case sup_dictionary_value(SupPid, ?CHILD_CLASS_KEY) of
        {ok, {ChildClass, ChildModule}} ->
            {ok, {ChildClass, ChildModule}};
        undefined ->
            child_class_from_children(SupClass, SupPid);
        {ok, _Malformed} ->
            child_class_from_children(SupClass, SupPid);
        Unavailable ->
            supervisor_unavailable_error(Self, childClass, Unavailable)
    end.

-spec child_class_from_children(atom(), pid()) ->
    {ok, {atom(), module()}} | {error, #beamtalk_error{}}.
child_class_from_children(SupClass, SupPid) ->
    Modules = [
        M
     || {_Id, Pid, _Type, [M | _]} <- supervisor:which_children(SupPid),
        is_pid(Pid),
        is_atom(M)
    ],
    Resolved = [
        {ClassName, M}
     || M <- Modules,
        {ok, ClassName} <- [beamtalk_class_registry:class_name_for_module(M)]
    ],
    case Resolved of
        [Entry | _] ->
            {ok, Entry};
        [] ->
            {error,
                beamtalk_error:new(
                    child_class_unknown,
                    SupClass,
                    childClass,
                    <<
                        "cannot determine this pool's child class: it was started without one "
                        "recorded (before ADR 0135) and has no running child; restart the pool"
                    >>
                )}
    end.

-doc """
Read the `maxChildren` limit `dynamic_init/3` stashed in the supervisor's
process dictionary. A supervisor not started through `dynamic_init/3` (or one
that has already exited) has no entry and is treated as unlimited; a dead
supervisor is then reported as `stale_handle` by the `supervisor:start_child/2`
call that follows.
""".
-spec max_children(pid()) -> non_neg_integer() | infinity.
max_children(SupPid) when is_pid(SupPid), node(SupPid) =:= node() ->
    case erlang:process_info(SupPid, {dictionary, ?MAX_CHILDREN_KEY}) of
        {{dictionary, ?MAX_CHILDREN_KEY}, Max} when is_integer(Max) -> Max;
        _ -> infinity
    end;
max_children(_SupPid) ->
    infinity.

-doc """
Run `StartFun` (a `supervisor:start_child/2` call) only if the DynamicSupervisor
is below its `maxChildren` limit (BT-3964); otherwise return
`{error, #beamtalk_error{kind = max_children}}` with the limit in
`details` (`#{max_children => Max}`) so callers can back off and retry.

The count is every dynamic child the supervisor tracks — what
`countChildren/1` (Beamtalk `count`) reports plus any child whose restart is
pending retry. `supervisor:count_children/1` is O(1) for `simple_one_for_one`.
The check runs in the caller, not inside the supervisor, so it is exact when
one process starts children (e.g. a single acceptor), but concurrent starters
racing past the check can briefly exceed the limit by up to the number of
racers. Supervisor-driven restarts are never refused.

Rejections log at debug: hitting the cap is expected back-pressure under load,
and logging every refused start would flood the log exactly when the system
is busiest.
""".
-spec with_child_capacity(atom(), atom(), pid(), fun(() -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().
with_child_capacity(SupClass, Selector, SupPid, StartFun) ->
    case max_children(SupPid) of
        infinity ->
            StartFun();
        Max ->
            Counts = supervisor:count_children(SupPid),
            %% `workers` + `supervisors` is every dynamic child, including one
            %% whose supervisor-driven restart is pending retry (not yet
            %% `active`): counting it keeps a retry that later succeeds from
            %% pushing the pool past Max.
            Total =
                proplists:get_value(workers, Counts, 0) +
                    proplists:get_value(supervisors, Counts, 0),
            case Total >= Max of
                false ->
                    StartFun();
                true ->
                    ?LOG_DEBUG("DynamicSupervisor at maxChildren", #{
                        supervisor => SupClass,
                        selector => Selector,
                        max_children => Max,
                        domain => [beamtalk, runtime]
                    }),
                    Error0 = beamtalk_error:new(
                        max_children,
                        SupClass,
                        Selector,
                        <<"Back off and retry, or free a slot with terminateChild:">>
                    ),
                    Error1 = beamtalk_error:with_message(
                        Error0,
                        iolist_to_binary(
                            io_lib:format(
                                "DynamicSupervisor is at its maxChildren limit (~b)", [Max]
                            )
                        )
                    ),
                    {error, beamtalk_error:with_details(Error1, #{max_children => Max})}
            end
    end.

-doc """
Build the `{error, #beamtalk_error{kind = stale_handle}}` return for
`with_live_supervisor/3`. Logs a warning and constructs the Beamtalk
error with class and selector context.
""".
-spec stale_handle_error(atom(), atom()) -> {error, #beamtalk_error{}}.
stale_handle_error(ClassName, Selector) ->
    ?LOG_WARNING("Supervisor stale handle", #{
        supervisor => ClassName,
        selector => Selector,
        domain => [beamtalk, runtime]
    }),
    Error = beamtalk_error:new(
        stale_handle,
        ClassName,
        Selector,
        <<"supervisor is not running; the handle is stale">>
    ),
    {error, Error}.

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

%% Overridable (application env) so tests can exercise the timeout paths.
-spec init_wait_timeout() -> non_neg_integer().
init_wait_timeout() ->
    application:get_env(beamtalk_runtime, init_wait_timeout_ms, ?INIT_WAIT_TIMEOUT).
