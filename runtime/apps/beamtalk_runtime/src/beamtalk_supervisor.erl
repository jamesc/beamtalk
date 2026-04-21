%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor).

%%% **DDD Context:** Actor System Context

-moduledoc """
Erlang runtime glue for Beamtalk Supervisor and DynamicSupervisor.

This module provides the BEAM interop entry points called from the
Supervisor and DynamicSupervisor stdlib methods via Erlang FFI.
Generated `init/1` callbacks delegate to `static_init/2` and `dynamic_init/2`
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
- stdlib/src/Supervisor.bt, DynamicSupervisor.bt
- runtime/apps/beamtalk_runtime/src/beamtalk_message_dispatch.erl (routing)
""".

-export([
    startLink/1,
    current/1,
    static_init/2,
    dynamic_init/2,
    whichChildren/1,
    whichChild/2,
    terminateChild/2,
    'terminateChild:class:'/2,
    'terminateChild:child:'/2,
    startChild/1,
    startChild/2,
    countChildren/1,
    stop/1,
    build_child_specs/1,
    spec_to_otp/1,
    is_supervisor/1,
    register_root/1,
    get_root/0,
    clear_root/0,
    run_initialize/1,
    start_child_via_class_method/4
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% ETS table name for the OTP application root supervisor registry (BT-1191).
%% Stores `{root, SupervisorTuple}` where SupervisorTuple is a
%% `{beamtalk_supervisor, ClassName, Module, Pid}` value.
-define(ROOT_SUPERVISOR_TABLE, beamtalk_root_supervisor).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Start (or return) the running supervisor for the given class.

Called from `class supervise` on Supervisor and DynamicSupervisor subclasses.
Self is the class object {beamtalk_object, 'ClassName class', Module, ClassPid}.

## Return shape (ADR 0080 Phase 0a — Option 2 probe, BT-1994)

Returns `{ok, {beamtalk_supervisor_new, ClassName, Module, Pid}}` on a
fresh start, `{ok, {beamtalk_supervisor, ClassName, Module, Pid}}` on
the idempotent `{already_started, Pid}` branch, and
`{error, #beamtalk_error{}}` on start failure. FFI coercion in
`beamtalk_erlang_proxy:coerce_ffi_result/2` wraps this into a Beamtalk
`Result` tagged map for the class method body's return.

The `beamtalk_supervisor_new` inner tag signals a fresh start to the
post-dispatch hook in `beamtalk_class_dispatch:class_send_dispatch/3`.
The hook matches two shapes:

  * the bare `{beamtalk_supervisor_new, ...}` tuple, seen when the
    stdlib `supervise` method calls `.unwrap` on the FFI-coerced
    Result (the current Phase 0a shim), and
  * a Result tagged map wrapping `{beamtalk_supervisor_new, ...}`,
    seen after the Phase 1 stdlib migration when callers handle the
    Result themselves.

In both cases the hook rewrites the inner tag to
`{beamtalk_supervisor, ...}`, runs `class initialize:` in the caller's
process (preserving the BT-1285 / ADR 0059 guarantee), and returns the
rewritten shape (bare tuple or re-wrapped Result) to the caller.
""".
-spec startLink(beamtalk_object()) ->
    {ok, {beamtalk_supervisor_new | beamtalk_supervisor, atom(), module(), pid()}}
    | {error, #beamtalk_error{}}.
startLink(Self) ->
    ClassPid = element(4, Self),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name(ClassPid),
    case Module:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("Supervisor started", #{
                supervisor => ClassName, module => Module, pid => Pid, domain => [beamtalk, runtime]
            }),
            %% BT-1542 + BT-1994 (ADR 0080 Phase 0a, option 2): use the
            %% beamtalk_supervisor_new tag to signal to the post-dispatch
            %% hook in beamtalk_class_dispatch that this is a fresh start.
            %% The hook unpacks the Result tagged map produced by FFI
            %% coercion, rewrites the inner tag to beamtalk_supervisor,
            %% runs initialize: in the caller's process, and rewraps.
            {ok, {beamtalk_supervisor_new, ClassName, Module, Pid}};
        {error, {already_started, Pid}} ->
            %% Idempotent branch: no initialize: re-run.
            {ok, {beamtalk_supervisor, ClassName, Module, Pid}};
        {error, Reason} ->
            ?LOG_ERROR("Supervisor start failed", #{
                supervisor => ClassName,
                module => Module,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            Error = beamtalk_error:new(
                supervisor_start_failed,
                ClassName,
                supervise,
                iolist_to_binary(io_lib:format("supervisor start_link failed: ~p", [Reason]))
            ),
            {error, Error}
    end.

-doc """
Initialize a static supervisor without calling through the class gen_server.

Called from the generated `init/1` callback of Supervisor subclasses.

The problem with calling `beamtalk_object_class:class_send/3` from `init/1`:
OTP spawns a new process and calls `Module:init([])`. Inside `init/1`, calling
`class_send(ClassPid, ...)` sends a `gen_server:call` to ClassPid — which is
blocked inside `startLink/1` waiting for `supervisor:start_link` to return.
This is a deadlock.

Solution: call class module functions directly (bypassing the gen_server).
We use ETS for the class hierarchy walk (no gen_server needed for lookup).
""".
-spec static_init(module(), atom()) -> {ok, {map(), [map()]}}.
static_init(Module, ClassName) ->
    ClassSelf = make_init_class_self(ClassName, Module),
    ClassVars = #{},
    Children = call_class_method_direct(ClassName, Module, class_children, ClassSelf, ClassVars),
    BtStrategy = call_class_method_direct(ClassName, Module, class_strategy, ClassSelf, ClassVars),
    MaxR = call_class_method_direct(ClassName, Module, class_maxRestarts, ClassSelf, ClassVars),
    MaxT = call_class_method_direct(ClassName, Module, class_restartWindow, ClassSelf, ClassVars),
    Strategy = to_otp_strategy(BtStrategy),
    SupFlags = #{strategy => Strategy, intensity => MaxR, period => MaxT},
    Specs = build_child_specs(Children),
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
Initialize a dynamic supervisor without calling through the class gen_server.

Called from the generated `init/1` callback of DynamicSupervisor subclasses.
Same deadlock avoidance rationale as `static_init/2`.
""".
-spec dynamic_init(module(), atom()) -> {ok, {map(), [map()]}}.
dynamic_init(Module, ClassName) ->
    ClassSelf = make_init_class_self(ClassName, Module),
    ClassVars = #{},
    ChildClass = call_class_method_direct(
        ClassName, Module, class_childClass, ClassSelf, ClassVars
    ),
    MaxR = call_class_method_direct(ClassName, Module, class_maxRestarts, ClassSelf, ClassVars),
    MaxT = call_class_method_direct(ClassName, Module, class_restartWindow, ClassSelf, ClassVars),
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    Specs = build_child_specs([ChildClass]),
    ?LOG_DEBUG("DynamicSupervisor init", #{
        supervisor => ClassName,
        strategy => simple_one_for_one,
        max_restarts => MaxR,
        restart_window => MaxT,
        domain => [beamtalk, runtime]
    }),
    {ok, {SupFlags, Specs}}.

-doc """
Return the running supervisor instance, or nil if not started.

Called from `class current` on Supervisor and DynamicSupervisor subclasses.
Self is the class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
""".
-spec current(beamtalk_object()) -> term() | nil.
current(Self) ->
    ClassPid = element(4, Self),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name(ClassPid),
    case whereis(Module) of
        undefined ->
            nil;
        Pid ->
            {beamtalk_supervisor, ClassName, Module, Pid}
    end.

-doc """
Return the child ids of currently-running children.

Called from `children` on Supervisor instances.
Returns `{ok, [Id]}` with a list of child id atoms (class name atoms by
default, or custom ids when `withId:` was used in `SupervisionSpec`).
Dead or restarting children are excluded. Returns `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead (BT-1997).
""".
-spec whichChildren(term()) -> {ok, [atom()]} | {error, #beamtalk_error{}}.
whichChildren(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, children, fun() ->
        Ids = [
            Id
         || {Id, ChildPid, _, _} <- supervisor:which_children(Pid),
            Id =/= undefined,
            is_pid(ChildPid)
        ],
        {ok, Ids}
    end).

-doc """
Return the running child object for the given class, or nil.

Called from `which: aClass` on Supervisor instances.
ClassArg is a class object {beamtalk_object, 'ClassName class', Module, ClassPid}.
Matches by child module (position 4 in which_children tuples) rather than child id
so that custom ids set via `withId:` in SupervisionSpec still resolve correctly.

Returns `{ok, {beamtalk_supervisor, ...}}` for supervisor subclasses or
`{ok, {beamtalk_object, ...}}` for worker children; `{ok, nil}` when no
running child matches. Returns `{error, BtError}` with `kind = stale_handle`
when the supervisor process is dead (BT-1997).
""".
-spec whichChild(term(), Class :: beamtalk_object()) ->
    {ok, tuple() | nil} | {error, #beamtalk_error{}}.
whichChild(Self, ClassArg) ->
    SupPid = element(4, Self),
    ClassName = element(2, Self),
    ChildModule = element(3, ClassArg),
    ChildClassPid = element(4, ClassArg),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    with_live_supervisor(ClassName, 'which:', fun() ->
        Children = supervisor:which_children(SupPid),
        case
            lists:search(
                fun({_Id, CPid, _, Mods}) ->
                    is_pid(CPid) andalso lists:member(ChildModule, Mods)
                end,
                Children
            )
        of
            {value, {_Id, ChildPid, _, _}} ->
                {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
            false ->
                {ok, nil}
        end
    end).

-doc """
Terminate a supervised child.

For Supervisor (static): Arg is a class object — terminates child by its
class name, which is the OTP child id.

For DynamicSupervisor (dynamic): Arg is an actor or supervisor instance —
terminates child by its process pid (simple_one_for_one semantics).

## Return shape (ADR 0080 Phase 1 — BT-1998)

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
    try
        case beamtalk_class_registry:is_class_object(Arg) of
            true ->
                %% Supervisor case: terminate by class name (the default child id)
                ChildClassPid = element(4, Arg),
                ChildId = beamtalk_object_class:class_name(ChildClassPid),
                handle_terminate_child_result(
                    SupClass,
                    #{child => ChildId},
                    supervisor:terminate_child(SupPid, ChildId)
                );
            false ->
                %% DynamicSupervisor case: terminate by child process pid
                ChildPid = element(4, Arg),
                handle_terminate_child_result(
                    SupClass,
                    #{child_pid => ChildPid},
                    supervisor:terminate_child(SupPid, ChildPid)
                )
        end
    catch
        exit:{noproc, _} ->
            %% supervisor:terminate_child uses gen_server:call internally,
            %% which exits with {noproc, MFA} when the supervisor pid is dead.
            terminate_child_stale_handle(SupClass);
        exit:noproc ->
            %% Defensive: gen_server:stop/1 exits with the bare atom noproc.
            %% supervisor:terminate_child should not reach this path, but
            %% matching it mirrors with_live_supervisor/3 for safety.
            terminate_child_stale_handle(SupClass)
    end.

-spec terminate_child_stale_handle(atom()) -> {error, #beamtalk_error{}}.
terminate_child_stale_handle(SupClass) ->
    ?LOG_WARNING("Supervisor stale handle", #{
        supervisor => SupClass,
        selector => 'terminateChild:',
        domain => [beamtalk, runtime]
    }),
    {error,
        beamtalk_error:new(
            stale_handle,
            SupClass,
            'terminateChild:',
            <<"supervisor is not running — the handle is stale">>
        )}.

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

-doc """
Start a new child with default args under a DynamicSupervisor.

Called from `startChild` on DynamicSupervisor instances.
Calls `Module:'childClass'()` to determine the child class and module,
then starts the child via OTP simple_one_for_one.

## Return shape (ADR 0080 Phase 1 — BT-1997)

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
    SupMod = element(3, Self),
    SupClass = element(2, Self),
    ChildClassObj = SupMod:'childClass'(),
    ChildClassPid = element(4, ChildClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    ChildModule = element(3, ChildClassObj),
    with_live_supervisor(SupClass, startChild, fun() ->
        case supervisor:start_child(SupPid, []) of
            {ok, ChildPid} ->
                ?LOG_INFO("DynamicSupervisor child started", #{
                    supervisor => SupClass,
                    child => ChildClass,
                    module => ChildModule,
                    child_pid => ChildPid,
                    domain => [beamtalk, runtime]
                }),
                {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
            {error, Reason} ->
                ?LOG_ERROR("DynamicSupervisor child start failed", #{
                    supervisor => SupClass,
                    child => ChildClass,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }),
                Error = beamtalk_error:new(
                    child_start_failed,
                    SupClass,
                    startChild,
                    iolist_to_binary(
                        io_lib:format("supervisor start_child failed: ~p", [Reason])
                    )
                ),
                {error, Error}
        end
    end).

-doc """
Start a new child with args under a DynamicSupervisor.

Called from `startChild: args` on DynamicSupervisor instances.
Args is passed as the extra argument to OTP simple_one_for_one,
which appends it to the child start function's argument list.

## Return shape (ADR 0080 Phase 1 — BT-1997)

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
    SupMod = element(3, Self),
    SupClass = element(2, Self),
    ChildClassObj = SupMod:'childClass'(),
    ChildClassPid = element(4, ChildClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    ChildModule = element(3, ChildClassObj),
    with_live_supervisor(SupClass, 'startChild:', fun() ->
        case supervisor:start_child(SupPid, [Args]) of
            {ok, ChildPid} ->
                ?LOG_INFO("DynamicSupervisor child started", #{
                    supervisor => SupClass,
                    child => ChildClass,
                    module => ChildModule,
                    child_pid => ChildPid,
                    domain => [beamtalk, runtime]
                }),
                {ok, wrap_child(ChildClass, ChildModule, ChildPid)};
            {error, Reason} ->
                ?LOG_ERROR("DynamicSupervisor child start failed", #{
                    supervisor => SupClass,
                    child => ChildClass,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }),
                Error = beamtalk_error:new(
                    child_start_failed,
                    SupClass,
                    'startChild:',
                    iolist_to_binary(
                        io_lib:format("supervisor start_child failed: ~p", [Reason])
                    )
                ),
                {error, Error}
        end
    end).

-doc """
Return the count of active children.

Called from `count` on Supervisor and DynamicSupervisor instances.
Uses `supervisor:count_children/1` which returns a proplist with
`active` (running), `workers`, `supervisors`, `specs` counts.

Returns `{ok, Count}` on success; `{error, BtError}` with
`kind = stale_handle` when the supervisor process is dead (BT-1997).
""".
-spec countChildren(term()) -> {ok, non_neg_integer()} | {error, #beamtalk_error{}}.
countChildren(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, count, fun() ->
        Counts = supervisor:count_children(Pid),
        {ok, proplists:get_value(active, Counts, 0)}
    end).

-doc """
Stop the supervisor and all its children.

Called from `stop` on Supervisor and DynamicSupervisor instances.
Uses gen_server:stop/1 since supervisors are OTP gen_servers.

Returns `{ok, nil}` on success; `{error, BtError}` with `kind = stale_handle`
when the supervisor process is already dead (BT-1997).
""".
-spec stop(term()) -> {ok, nil} | {error, #beamtalk_error{}}.
stop(Self) ->
    Pid = element(4, Self),
    ClassName = element(2, Self),
    with_live_supervisor(ClassName, stop, fun() ->
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
    [build_child_spec(C) || C <- Children].

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
Register the OTP application root supervisor (BT-1191).

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

Called by `Workspace supervisor` via the workspace interface primitives.
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
Run the class-side `initialize:` lifecycle hook on a supervisor tuple.

Called from `beamtalk_message_dispatch:send/3` AFTER `class_send` returns
the supervisor tuple from a `supervise` call. This ensures `initialize:`
runs in the caller's process — where the class gen_server is free to answer
`has_method`, `superclass`, and other hierarchy lookups that Beamtalk
dispatch requires.

Uses `call_class_method_direct` to bypass the class gen_server for the
initial `class_initialize:` method lookup (same pattern as `static_init/2`).
""".
-spec run_initialize(term()) -> ok.
run_initialize({beamtalk_supervisor, ClassName, Module, _Pid} = SupTuple) ->
    ClassSelf = make_init_class_self(ClassName, Module),
    ClassVars = #{},
    call_class_method_direct(ClassName, Module, 'class_initialize:', ClassSelf, ClassVars, [
        SupTuple
    ]),
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
Call a class method directly by invoking the module function, bypassing the class
gen_server. Tries the subclass module first, then walks the class hierarchy via
ETS until the method is found in an ancestor's module.
""".
-spec call_class_method_direct(atom(), module(), atom(), tuple(), map()) -> term().
call_class_method_direct(ClassName, Module, FunName, ClassSelf, ClassVars) ->
    call_class_method_direct(ClassName, Module, FunName, ClassSelf, ClassVars, []).

-doc """
Call a class method directly with extra user-facing arguments.
ExtraArgs are appended after [ClassSelf, ClassVars].
""".
-spec call_class_method_direct(atom(), module(), atom(), tuple(), map(), [term()]) -> term().
call_class_method_direct(ClassName, Module, FunName, ClassSelf, ClassVars, ExtraArgs) ->
    Arity = 2 + length(ExtraArgs),
    case erlang:function_exported(Module, FunName, Arity) of
        true ->
            erlang:apply(Module, FunName, [ClassSelf, ClassVars | ExtraArgs]);
        false ->
            call_inherited_class_method_direct(
                ClassName, FunName, ClassSelf, ClassVars, ExtraArgs, 0
            )
    end.

-spec call_inherited_class_method_direct(
    atom(), atom(), tuple(), map(), [term()], non_neg_integer()
) ->
    term().
call_inherited_class_method_direct(
    _ClassName, FunName, _ClassSelf, _ClassVars, _ExtraArgs, Depth
) when
    Depth > 30
->
    error({supervisor_init_method_not_found, FunName});
call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ClassVars, ExtraArgs, Depth) ->
    Arity = 2 + length(ExtraArgs),
    case beamtalk_class_hierarchy_table:lookup(ClassName) of
        not_found ->
            error({supervisor_init_method_not_found, FunName});
        {ok, SuperclassName} ->
            %% BT-1285: Look up the ancestor module via ETS instead of gen_server:call.
            %% If the ancestor is itself a Supervisor subclass currently being initialised,
            %% its class gen_server is blocked inside startLink/1 waiting for OTP
            %% supervisor:start_link to return.  A gen_server:call to it would deadlock.
            %% beamtalk_class_module_table stores module names written during class init
            %% and is safe to read from any process without coordination.
            case beamtalk_class_module_table:lookup(SuperclassName) of
                not_found ->
                    %% Class not yet registered or module not yet recorded — skip upward.
                    call_inherited_class_method_direct(
                        SuperclassName, FunName, ClassSelf, ClassVars, ExtraArgs, Depth + 1
                    );
                {ok, SuperModule} ->
                    case erlang:function_exported(SuperModule, FunName, Arity) of
                        true ->
                            erlang:apply(SuperModule, FunName, [ClassSelf, ClassVars | ExtraArgs]);
                        false ->
                            call_inherited_class_method_direct(
                                SuperclassName, FunName, ClassSelf, ClassVars, ExtraArgs, Depth + 1
                            )
                    end
            end
    end.

-doc "Build a single OTP child spec from a class object or SupervisionSpec map.".
-spec build_child_spec(term()) -> map().
build_child_spec(ClassObj) when is_tuple(ClassObj), element(1, ClassObj) =:= beamtalk_object ->
    case beamtalk_class_registry:is_class_object(ClassObj) of
        true ->
            ChildClassPid = element(4, ClassObj),
            ChildClass = beamtalk_object_class:class_name(ChildClassPid),
            ChildModule = element(3, ClassObj),
            case is_supervisor(ChildClass) of
                true ->
                    %% Nested supervisor child — build OTP spec directly.
                    %% Supervisor subclasses don't have supervisionSpec (that is on Actor).
                    %% OTP requires supervisor children to use start_link/0.
                    #{
                        id => ChildClass,
                        start => {ChildModule, start_link, []},
                        restart => permanent,
                        shutdown => infinity,
                        type => supervisor,
                        modules => [ChildModule]
                    };
                false ->
                    %% Worker child: call supervisionSpec then childSpec.
                    BtSpec = beamtalk_object_class:class_send(ChildClassPid, 'supervisionSpec', []),
                    spec_to_otp(beamtalk_message_dispatch:send(BtSpec, 'childSpec', []))
            end;
        false ->
            %% Actor instance passed as spec — treat as SupervisionSpec-like value
            spec_to_otp(beamtalk_message_dispatch:send(ClassObj, 'childSpec', []))
    end;
build_child_spec(Spec) when is_map(Spec) ->
    %% SupervisionSpec value (tagged map): call childSpec directly
    spec_to_otp(beamtalk_message_dispatch:send(Spec, 'childSpec', [])).

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

BT-1862: When a SupervisionSpec uses `withClassMethod:`, the supervisor must
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

Process dictionary entries (`beamtalk_class_name`, `beamtalk_class_module`,
`beamtalk_class_is_abstract`) are set temporarily so that `self spawnWith:`
in the class method body can resolve class metadata via `class_self_spawn/4`.

The MFA stored in the OTP child spec is:
  {beamtalk_supervisor, start_child_via_class_method, [ClassName, Module, Selector, Args]}
All terms are atoms/lists (no pids), so the MFA survives class object restarts.
Selector is in compiled form (e.g., `class_create:value:`).
""".
-spec start_child_via_class_method(atom(), module(), atom(), [term()]) ->
    {ok, pid()} | {error, term()}.
start_child_via_class_method(ClassName, Module, Selector, Args) ->
    %% Set process dictionary entries needed by class_self_spawn/4.
    %% These are normally set by the class gen_server during init;
    %% we replicate them here so the class method runs correctly
    %% in the supervisor process.
    put(beamtalk_class_name, ClassName),
    put(beamtalk_class_module, Module),
    put(beamtalk_class_is_abstract, false),
    try
        ClassSelf = make_init_class_self(ClassName, Module),
        ClassVars = #{},
        RawResult = call_class_method_direct(
            ClassName, Module, Selector, ClassSelf, ClassVars, Args
        ),
        %% Handle class_var_result wrapper if class method mutates class vars
        Result =
            case RawResult of
                {class_var_result, R, _NewClassVars} -> R;
                R -> R
            end,
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
        erase(beamtalk_class_is_abstract)
    end.

-doc """
Convert a Beamtalk child spec dict to an OTP-compatible child spec map.

The Beamtalk dict from `SupervisionSpec childSpec` has keys:
  id, start ([ClassObj, FnAtom, ArgsList]), restart, shutdown, type
The `start` value is a Beamtalk Array [ClassObj, #spawn, #()] that must be
converted to the OTP MFA tuple {Module, Function, Args}.

For nested supervisor children (Supervisor/DynamicSupervisor subclasses), the
Beamtalk IR uses #spawn as the start function, but OTP requires start_link/0
so the supervisor process is linked into the tree. This is translated here.

For worker children, Beamtalk's spawn/0 returns {beamtalk_object,...} which
OTP supervisor does not accept (it expects {ok, Pid}). The generated
start_link/1 returns {ok, Pid} directly from gen_server:start_link, so
worker children use start_link/1 with an init-args map instead of spawn/0.
""".
-spec spec_to_otp(map()) -> map().
spec_to_otp(BtSpec) ->
    %% `start` is a Beamtalk Array #[ClassObj, StartFn, StartArgs].
    %% Beamtalk Arrays are tagged maps: #{'$beamtalk_class' => 'Array', 'data' => ErlangArray}.
    %% Use array:get/2 (0-based) to access elements.
    StartBtArray = maps:get(start, BtSpec),
    StartErlArray = maps:get(data, StartBtArray),
    ClassObj = array:get(0, StartErlArray),
    ChildModule = element(3, ClassObj),
    ChildClassPid = element(4, ClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    OtpStart =
        case is_supervisor(ChildClass) of
            true ->
                %% Nested supervisor: OTP expects start_link/0 to link the child supervisor.
                {ChildModule, start_link, []};
            false ->
                %% Worker: use start_link/1 (returns {ok, Pid}) for OTP compatibility.
                %% spawn/0 wraps the pid in {beamtalk_object,...} which OTP rejects.
                StartFn = array:get(1, StartErlArray),
                case StartFn of
                    spawn ->
                        %% No init args — start with empty state map.
                        {ChildModule, start_link, [#{}]};
                    'spawnWith:' ->
                        %% #(self args) uses list syntax (#(...)) so it compiles to an
                        %% Erlang list [ArgsMap] — use it directly as the start_link arg.
                        InitArgs = array:get(2, StartErlArray),
                        {ChildModule, start_link, InitArgs};
                    'spawnAs:' ->
                        %% ADR 0079 / BT-1990: SupervisionSpec withName: routes
                        %% named children through `beamtalk_actor:spawnAs/2,3`
                        %% so the child registers under `Name` atomically inside
                        %% gen_server:start_link({local, Name}, ...). The startArgs
                        %% in `SupervisionSpec childSpec` carry just the Name as
                        %% `#(name)` — i.e. an Erlang list `[Name]`.
                        SpawnAsArgs = array:get(2, StartErlArray),
                        [Name] = SpawnAsArgs,
                        {beamtalk_actor, 'spawnAs', [Name, ChildModule]};
                    'spawnWith:as:' ->
                        %% ADR 0079 / BT-1990: spawn-with-args under a registered
                        %% name. startArgs are `#(args, name)` → Erlang `[Args, Name]`.
                        %% Translates to `beamtalk_actor:spawnAs/3` so the child is
                        %% registered atomically with init args at start time.
                        SpawnAsArgs2 = array:get(2, StartErlArray),
                        [InitArgsMap, NameAtom] = SpawnAsArgs2,
                        {beamtalk_actor, 'spawnAs', [NameAtom, ChildModule, InitArgsMap]};
                    classMethod ->
                        %% BT-1862: Route through the actor's keyword class method.
                        %% StartArgs is #(selector, argsList) — compiled as an
                        %% Erlang list [Selector, ArgsList]. The selector is stored
                        %% in source form (e.g., 'create:value:'); we prepend 'class_'
                        %% to get the compiled function name for call_class_method_direct.
                        [Selector, ArgsList] = array:get(2, StartErlArray),
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
                                    "spawnWith:as:, or classMethod)",
                                    [Other]
                                )
                            )
                        ),
                        error(beamtalk_exception_handler:ensure_wrapped(Error))
                end
        end,
    #{
        id => maps:get(id, BtSpec),
        start => OtpStart,
        restart => maps:get(restart, BtSpec),
        shutdown => maps:get(shutdown, BtSpec),
        type => maps:get(type, BtSpec),
        modules => [ChildModule]
    }.

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
Execute Fun(), catching raw OTP process exits that indicate a stale
supervisor handle (the target process is dead) and translating them to
`{error, #beamtalk_error{kind = stale_handle}}` instead of letting the raw
exit leak across the public API boundary.

Two distinct exit shapes are handled:

* `supervisor:*` calls route through `gen_server:call`, which exits with
  `{noproc, MFA}` (tuple) when the target process is not alive.
* `gen_server:stop/1` exits with the bare atom `noproc` (no MFA wrapper).

## Inner fun contract (BT-1997)

After BT-1997, each inner fun returns `{ok, Value}` on success and
`{error, #beamtalk_error{}}` on application-level failure.
`with_live_supervisor/3` returns whichever tagged tuple Fun produced, or
intercepts the raw OTP `noproc` exit and returns
`{error, #beamtalk_error{kind = stale_handle, ...}}`. FFI coercion in
`beamtalk_erlang_proxy:coerce_ffi_result/2` wraps the tagged tuple into
a Beamtalk `Result` for the stdlib method body. (`terminateChild/2`
migrates in the sibling BT-1998 issue; until then its Fun still calls
`error/1` on failure, which passes through the try/catch here unchanged.)
""".
-spec with_live_supervisor(atom(), atom(), fun(() -> term())) ->
    term() | {error, #beamtalk_error{}}.
with_live_supervisor(ClassName, Selector, Fun) ->
    try
        Fun()
    catch
        exit:{noproc, _} ->
            %% supervisor:* calls use gen_server:call internally, which exits
            %% with {noproc, MFA} when the target process is dead.
            stale_handle_error(ClassName, Selector);
        exit:noproc ->
            %% gen_server:stop/1 exits with the bare atom noproc (no MFA wrapper).
            stale_handle_error(ClassName, Selector)
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
        <<"supervisor is not running — the handle is stale">>
    ),
    {error, Error}.

-doc """
Ensure the root supervisor ETS table exists.
Uses `public` access so the generated app callback and workspace primitives
can both read/write without process ownership constraints.
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
