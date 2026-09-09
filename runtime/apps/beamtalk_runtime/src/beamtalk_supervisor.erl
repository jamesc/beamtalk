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
- stdlib/src/supervisor.bt, dynamic_supervisor.bt
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
    spec_to_otp/2,
    is_supervisor/1,
    register_root/1,
    get_root/0,
    clear_root/0,
    run_initialize/1,
    start_child_via_class_method/4,
    start_dynamic_child/2,
    start_dynamic_child/3,
    start_dynamic_child/4,
    startChild/3
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
    %% BT-3407: read the class's live classState snapshot instead of a
    %% hardcoded empty map, so a value set via an ordinary class-method call
    %% before `supervise` (e.g. `configure:`) is visible to `class children`
    %% here.
    ClassVars = class_vars_snapshot(ClassSelf),
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
    %% BT-3407: see static_init/2's identical comment.
    ClassVars = class_vars_snapshot(ClassSelf),
    ChildClass = call_class_method_direct(
        ClassName, Module, class_childClass, ClassSelf, ClassVars
    ),
    MaxR = call_class_method_direct(ClassName, Module, class_maxRestarts, ClassSelf, ClassVars),
    MaxT = call_class_method_direct(ClassName, Module, class_restartWindow, ClassSelf, ClassVars),
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxR, period => MaxT},
    Specs = build_child_specs([ChildClass], dynamic),
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
            <<"supervisor is not running; the handle is stale">>
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
Start a new child with args, registered under a name, under a DynamicSupervisor.

Called from `startChild: args name: aName` on DynamicSupervisor instances
(ADR 0079 amendment, BT-3376). Combines args-replay (BT-3365) with named
registration: `Name` is appended as a third extra argument that
`supervisor:start_child/2` threads onto the shared `simple_one_for_one`
template, landing on `start_dynamic_child/4`, which spawns the child via
`beamtalk_actor:'spawnAs'/3` instead of a bare `ChildModule:start_link/1` —
the same primitive `SupervisionSpec withName:` already uses for static
children (ADR 0079). Because `simple_one_for_one` replays each dynamic
child's own start args (now including `Name`) on automatic OTP restart, a
crashed named child re-registers under the same name every time, with no
supervisor-side bookkeeping.

## Return shape (ADR 0080 Phase 1 — BT-1997)

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
    SupMod = element(3, Self),
    SupClass = element(2, Self),
    ChildClassObj = SupMod:'childClass'(),
    ChildClassPid = element(4, ChildClassObj),
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    ChildModule = element(3, ChildClassObj),
    with_live_supervisor(SupClass, 'startChild:name:', fun() ->
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
            {error, Reason} ->
                ?LOG_ERROR("DynamicSupervisor named child start failed", #{
                    supervisor => SupClass,
                    child => ChildClass,
                    name => Name,
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }),
                announce_child_crashed(SupClass, ChildClass, Reason),
                Error = beamtalk_error:new(
                    child_start_failed,
                    SupClass,
                    'startChild:name:',
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
    build_child_specs(Children, static).

-doc """
Build OTP child specs, threading `Mode` through to `spec_to_otp/2`.

`Mode = dynamic` is used only by `dynamic_init/2` (a DynamicSupervisor's
`simple_one_for_one` template): its default (`#spawn`) worker child spec
gets a **zero**-static-arg MFA that routes through `start_dynamic_child/2,3`
instead of baking `[#{}]` directly into `{ChildModule, start_link, _}` —
see `spec_to_otp/2` and BT-3365. `Mode = static` (the default, used by
`static_init/2` via `build_child_specs/1`) is unchanged: a static
Supervisor's children start immediately at supervisor-init time and never
receive appended `supervisor:start_child/2` args, so the pre-BT-3365
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
    %% BT-3407: see static_init/2's identical comment.
    ClassVars = class_vars_snapshot(ClassSelf),
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
Read a class's live classState snapshot (BT-3407), given the `ClassSelf`
tuple `make_init_class_self/2` already built for the same call site.

Reuses `ClassSelf`'s own `ClassPid` (position 4) rather than re-resolving
it via a second `whereis_class/1` call — every one of this function's
callers has just built `ClassSelf` from that exact pid. Deadlock-safe like
`class_state_snapshot/1` itself: reads an ETS mirror, never messages the
class gen_server (which may be blocked waiting for this `init/1` to
return).
""".
-spec class_vars_snapshot(beamtalk_object()) -> map().
class_vars_snapshot(ClassSelf) ->
    beamtalk_class_registry:class_state_snapshot(element(4, ClassSelf)).

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
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        not_found ->
            error({supervisor_init_method_not_found, FunName});
        {ok, SuperclassName} ->
            %% BT-1285: Look up the ancestor module via ETS instead of gen_server:call.
            %% If the ancestor is itself a Supervisor subclass currently being initialised,
            %% its class gen_server is blocked inside startLink/1 waiting for OTP
            %% supervisor:start_link to return.  A gen_server:call to it would deadlock.
            %% beamtalk_class_metadata stores module names written during class init
            %% and is safe to read from any process without coordination.
            case beamtalk_class_metadata:lookup_module(SuperclassName) of
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
-spec build_child_spec(term(), static | dynamic) -> map().
build_child_spec(ClassObj, Mode) when
    is_tuple(ClassObj), element(1, ClassObj) =:= beamtalk_object
->
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
                    spec_to_otp(beamtalk_message_dispatch:send(BtSpec, 'childSpec', []), Mode)
            end;
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

Process dictionary entries `beamtalk_class_name` / `beamtalk_class_module` are
set temporarily as a defensive fallback for call sites that still read class
identity from the process dictionary (e.g. `handle_class_self_call/1`'s
deadlock-diagnostic hint) when the class method runs correctly in the
supervisor process instead of the class's own gen_server.

BT-3243 (supervisor-restart follow-up): `?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` is
also set here (and only here). Unlike `beamtalk_class_name`/
`beamtalk_class_module` — which `beamtalk_object_class`'s `init/1` *also*
sets, in the class's own gen_server process — this key is exclusive to this
function, so `beamtalk_actor:safe_spawn/2` and
`beamtalk_class_instantiation:do_class_self_named_spawn/6` can check it to
tell "running a supervised child's factory in the real supervisor process"
(stay linked — that link is the restart mechanism) apart from "running a
class method inside the class's own gen_server, or a plain unsupervised
spawn" (unlink — the original BT-3243 fix). This is a plain process
dictionary read with no process boundary crossed between here and
`safe_spawn/2`: `call_class_method_direct` below reaches the class method via
`erlang:apply/3`, and the compiled `self spawn`/`self spawnWith:` body
reaches `safe_spawn/2` via further direct calls
(`beamtalk_class_instantiation:handle_spawn/4` → `erlang:apply(Module,
spawn, Args)` → the generated `spawn/1` wrapper) — all synchronous, same
process, so the key set here is still visible when `safe_spawn/2` checks it.

BT-3106: `beamtalk_class_is_abstract` is deliberately **not** seeded here.
`self spawnWith:`/`self spawnAs:`/`self spawnWith:as:` in a compiled class
method resolve `is_abstract` by class name via
`beamtalk_class_instantiation:resolve_is_abstract_or_raise/2` (BT-3047 / ADR
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
    %% excluded — see the doc comment above (BT-3106).
    put(beamtalk_class_name, ClassName),
    put(beamtalk_class_module, Module),
    %% BT-3243 (supervisor-restart follow-up): mark this process as running a
    %% withClassMethod: child's factory directly in the real OTP supervisor
    %% process, so that any `self spawn`/`self spawnWith:`/`self spawnAs:`/
    %% `self spawnWith:as:` the factory calls (beamtalk_actor:safe_spawn/2,
    %% beamtalk_class_instantiation:do_class_self_named_spawn/6) stays linked
    %% to us instead of unlinking — the link is what lets us, the supervisor,
    %% detect the child's exit and restart it. See ?BT_SUPERVISOR_SPAWN_CONTEXT_KEY.
    put(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY, true),
    try
        ClassSelf = make_init_class_self(ClassName, Module),
        %% BT-3407: see static_init/2's identical comment.
        ClassVars = class_vars_snapshot(ClassSelf),
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
        erase(?BT_SUPERVISOR_SPAWN_CONTEXT_KEY)
    end.

-doc """
Start a DynamicSupervisor's default (no per-call args) child (BT-3365).

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
Start a DynamicSupervisor child with caller-supplied init args (BT-3365).

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
under a name (BT-3376, ADR 0079 amendment).

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
Beamtalk IR uses #spawn as the start function, but OTP requires start_link/0
so the supervisor process is linked into the tree. This is translated here.

For worker children, Beamtalk's spawn/0 returns {beamtalk_object,...} which
OTP supervisor does not accept (it expects {ok, Pid}). The generated
start_link/1 returns {ok, Pid} directly from gen_server:start_link, so
worker children use start_link/1 with an init-args map instead of spawn/0.

`Mode` selects between a static Supervisor's immediate-start semantics and
a DynamicSupervisor's extensible `simple_one_for_one` template — see the
`spawn` case below and BT-3365.
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
    ChildClass = beamtalk_object_class:class_name(ChildClassPid),
    OtpStart =
        case is_supervisor(ChildClass) of
            true ->
                %% Nested supervisor: OTP expects start_link/0 to link the child supervisor.
                {ChildModule, start_link, []};
            false ->
                %% Worker: use start_link/1 (returns {ok, Pid}) for OTP compatibility.
                %% spawn/0 wraps the pid in {beamtalk_object,...} which OTP rejects.
                StartFn = lists:nth(2, StartElems),
                case StartFn of
                    spawn when Mode =:= dynamic ->
                        %% BT-3365: a DynamicSupervisor's simple_one_for_one template
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
                        %% BT-3365 (review follow-up): a DynamicSupervisor childClass
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
                        %% ADR 0079 / BT-1990: SupervisionSpec withName: routes
                        %% named children through `beamtalk_actor:spawnAs/2,3`
                        %% so the child registers under `Name` atomically inside
                        %% gen_server:start_link({local, Name}, ...). The startArgs
                        %% in `SupervisionSpec childSpec` carry just the Name as
                        %% `#(name)` — i.e. an Erlang list `[Name]`.
                        SpawnAsArgs = lists:nth(3, StartElems),
                        [Name] = SpawnAsArgs,
                        {beamtalk_actor, 'spawnAs', [Name, ChildModule]};
                    'spawnWith:as:' ->
                        %% ADR 0079 / BT-1990: spawn-with-args under a registered
                        %% name. startArgs are `#(args, name)` → Erlang `[Args, Name]`.
                        %% Translates to `beamtalk_actor:spawnAs/3` so the child is
                        %% registered atomically with init args at start time.
                        SpawnAsArgs2 = lists:nth(3, StartElems),
                        [InitArgsMap, NameAtom] = SpawnAsArgs2,
                        {beamtalk_actor, 'spawnAs', [NameAtom, ChildModule, InitArgsMap]};
                    classMethod ->
                        %% BT-1862: Route through the actor's keyword class method.
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
                                    "spawnWith:as:, or classMethod)",
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
Announce `SupervisionChildAdded` on the `SystemAnnouncer` bus after a
DynamicSupervisor successfully starts a child (ADR 0093 §2 / ADR 0092, BT-2445).
Best-effort and fault-isolated — see `announce_supervision/2`.
""".
-spec announce_child_added(atom(), atom(), pid()) -> ok.
announce_child_added(SupClass, ChildClass, ChildPid) ->
    announce_supervision('SupervisionChildAdded', #{
        supervisor => SupClass, childClass => ChildClass, childPid => ChildPid
    }).

-doc """
Announce `SupervisionChildCrashed` on the `SystemAnnouncer` bus when a
DynamicSupervisor child fails to start (ADR 0093 §2 / ADR 0092, BT-2445). The
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
for the `SupervisionChildCrashed` event payload (BT-2445): a `#beamtalk_error{}`
(e.g. from `startChild:name:`'s `'spawnAs'/3` path, BT-3376) yields its `kind`
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
        <<"supervisor is not running; the handle is stale">>
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
