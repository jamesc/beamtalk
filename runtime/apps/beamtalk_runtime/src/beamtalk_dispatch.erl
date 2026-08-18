%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dispatch).

%%% **DDD Context:** Object System Context

-moduledoc """
Method dispatch domain service (DDD).

This module provides the unified dispatch service for Beamtalk method calls,
implementing runtime hierarchy walking and method resolution. It replaces
scattered dispatch logic with a single domain service.

## Responsibilities

- Method lookup via hierarchy walking (starts at given class, walks upward)
- Super send dispatch (starts at immediate superclass)
- Method invocation (handles compiled and dynamic classes)
- Method combinations (before/after/primary ordering)
- Extension method checking at each hierarchy level
- Structured error generation (`#beamtalk_error{}`)

## Design Principles (ADR 0006)

1. **Runtime lookup is source of truth**: All dispatch paths must use this service
2. **Hierarchy walking**: Check current class → extensions → superclass → recurse
3. **Super semantics**: Skip current class, start at immediate superclass
4. **Method combinations**: Collect before/after from full chain, primary from defining class
5. **Structured errors**: Always return `#beamtalk_error{}`, never bare tuples

## Integration with Other Modules (DDD Separation)

- **beamtalk_object_class**: Class registry, method storage, metadata (data layer)
- **beamtalk_extensions**: Extension method registry (data layer)
- **beamtalk_dispatch**: Method lookup, hierarchy walking, invocation (domain service)
- **beamtalk_actor**: Actor lifecycle, spawn, supervision (actor domain)

## API

### lookup/5
Core dispatch entry point. Walks hierarchy from CurrentClass upward.
```erlang
lookup(Selector, Args, Self, State, CurrentClass)
    -> {reply, Result, NewState} | {error, #beamtalk_error{}}
```

### super/5
Super send dispatch. Starts at immediate superclass (no double-walk).
```erlang
super(Selector, Args, Self, State, CurrentClass)
    -> {reply, Result, NewState} | {error, #beamtalk_error{}}
```

## References

- ADR 0006: Unified Method Dispatch with Hierarchy Walking
- docs/internal/design-self-as-object.md: Method resolution design
- runtime/src/beamtalk_object_class.erl: Class registry implementation
""".

-export([
    lookup/5,
    super/5,
    super_value/4,
    responds_to/2,
    invoke_extension/6,
    check_extension/2,
    apply_extension_by_arity/4
]).

-include("beamtalk.hrl").

-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Types
%%% ============================================================================

-type selector() :: atom().
-type args() :: [term()].
% #beamtalk_object{} or value
-type bt_self() :: term().
-type state() :: map().
-type class_name() :: atom().
-type dispatch_result() :: {reply, term(), state()} | {error, #beamtalk_error{}}.

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Core dispatch entry point - walks hierarchy from CurrentClass upward.

This is the primary method lookup function. It:
1. Checks extension registry for CurrentClass + Selector
2. Checks CurrentClass's method table for Selector
3. If not found, recursively checks superclass chain
4. Returns structured #beamtalk_error{} if not found in entire chain

## Example

```erlang
%% Call Counter increment (defined locally)
lookup(increment, [], Self, State, 'Counter')
    -> {reply, 1, NewState}

%% Call Counter class (inherited from Object)
lookup(class, [], Self, State, 'Counter')
    -> {reply, 'Counter', State}  % walks to Object, finds method

%% Call Counter unknownMethod (not found anywhere)
lookup(unknownMethod, [], Self, State, 'Counter')
    -> {error, #beamtalk_error{kind=does_not_understand, class='Counter', selector=unknownMethod}}
```
""".
-spec lookup(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
lookup(Selector, Args, Self, State, CurrentClass) ->
    %% Step 1: Check extension registry first (guard against missing ETS table)
    case check_extension(CurrentClass, Selector) of
        {ok, Fun} ->
            %% Found extension method - invoke it
            ?LOG_DEBUG("Found extension method", #{
                selector => Selector, class => CurrentClass, domain => [beamtalk, runtime]
            }),
            invoke_extension(Fun, Selector, CurrentClass, Args, Self, State);
        not_found ->
            %% Step 2: Check class's own method table
            lookup_in_class_chain(Selector, Args, Self, State, CurrentClass)
    end.

-doc """
Super send dispatch - starts at immediate superclass.

This function implements `super` semantics. It skips the current class
and starts the hierarchy walk at the immediate superclass. This is critical
for preventing infinite recursion when a subclass method calls super.

## Implementation Note

Unlike `lookup/5`, this function does NOT check the CurrentClass's method table.
It immediately looks up the superclass and delegates to `lookup_in_class_chain/5`.

## Example

```erlang
%% In LoggingCounter increment implementation:
%% super increment        <- calls this function
super(increment, [], Self, State, 'LoggingCounter')
    -> looks up Counter (superclass)
    -> delegates to Counter's increment implementation
    -> returns {reply, NewValue, NewState}
```
""".
-spec super(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
super(Selector, Args, Self, State, CurrentClass) ->
    %% Look up the current class to get its superclass
    case beamtalk_class_registry:whereis_class(CurrentClass) of
        undefined ->
            %% Class not found - return error
            {error, beamtalk_error:new(class_not_found, CurrentClass, Selector)};
        ClassPid ->
            %% Get the superclass
            case beamtalk_object_class:superclass(ClassPid) of
                none ->
                    %% No superclass - method not found
                    Error = beamtalk_error:new(
                        does_not_understand,
                        CurrentClass,
                        Selector,
                        <<"Method not found in superclass chain">>
                    ),
                    {error, Error};
                SuperclassName ->
                    %% Check extension registry on superclass before hierarchy walk
                    case check_extension(SuperclassName, Selector) of
                        {ok, Fun} ->
                            ?LOG_DEBUG("Found extension method via super", #{
                                selector => Selector,
                                class => SuperclassName,
                                domain => [beamtalk, runtime]
                            }),
                            invoke_extension(Fun, Selector, SuperclassName, Args, Self, State);
                        not_found ->
                            %% Start lookup at superclass (skip current class)
                            lookup_in_class_chain(Selector, Args, Self, State, SuperclassName)
                    end
            end
    end.

-doc """
Value-context `super` send (BT-2252).

Walks the superclass chain exactly like `super/5`, but for value/primitive
types whose methods and foreign extensions lower to state-less funs
(`fun(Args, Self) -> Result`). Such a fun has no `State` binding, so the
generated code cannot thread one. This wrapper supplies an empty state to the
shared hierarchy walk (state is meaningless for immutable value types) and
unwraps the `{reply, Value, _State}` result to a plain value, re-raising any
structured error.
""".
-spec super_value(selector(), args(), bt_self(), class_name()) -> term().
super_value(Selector, Args, Self, CurrentClass) ->
    case super(Selector, Args, Self, #{}, CurrentClass) of
        {reply, Result, _State} -> Result;
        {error, Error} -> beamtalk_error:raise(Error)
    end.

-doc """
Check if a class or any of its ancestors responds to a selector.

Walks the class hierarchy from ClassName upward, checking each class's
local method table via has_method/2. Returns true if any class in the
chain has the method.

ADR 0032 Phase 3: Delegates to beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2,
the single canonical hierarchy-walk implementation, removing the duplicate
responds_to_chain walk that previously lived here.

Used by Object's `respondsTo:` implementation to check the full hierarchy.
""".
-spec responds_to(selector(), class_name()) -> boolean().
responds_to(Selector, ClassName) ->
    %% Check extension registry first (covers all classes)
    case check_extension(ClassName, Selector) of
        {ok, _Fun} ->
            true;
        not_found ->
            beamtalk_behaviour_intrinsics:classCanUnderstandFromName(ClassName, Selector)
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc """
Look up method in class chain via direct hierarchy walk.

ADR 0032 Phase 1: Replaces the flattened table fast path with a direct
chain walk via has_method/2 + superclass/1. At max hierarchy depth of 6
(typical 4), this means at most ~12 gen_server calls — microseconds on a
local node. The flattened table cache was removed to eliminate BT-510's
race window and O(N) rebuild broadcast cascade.

BT-2786: The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; `class_chain_step/6` supplies only
the per-class `has_method/2` + `superclass/1` probe and method invocation.
""".
-spec lookup_in_class_chain(selector(), args(), bt_self(), state(), class_name()) ->
    dispatch_result().
lookup_in_class_chain(Selector, Args, Self, State, ClassName) ->
    %% No separate whereis_class/1 pre-check here: class_chain_step/6 performs
    %% that lookup for every node it visits (including ClassName itself, at
    %% depth 0) and already produces the identical class_not_found error, so
    %% a pre-check would just be a redundant registry lookup on the hot path.
    StepFun = fun(CurrentClass, Depth) ->
        class_chain_step(Selector, Args, Self, State, CurrentClass, Depth)
    end,
    case beamtalk_hierarchy:walk_ancestors(ClassName, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, Result} ->
            Result;
        {max_depth_exceeded, CycleClass} ->
            ?LOG_WARNING("Max hierarchy depth exceeded — possible cycle", #{
                max_depth => ?MAX_HIERARCHY_DEPTH,
                class => ClassName,
                cycle_detected_at => CycleClass,
                selector => Selector,
                domain => [beamtalk, runtime]
            }),
            {error,
                beamtalk_error:new(
                    does_not_understand,
                    ClassName,
                    Selector,
                    <<"Hierarchy depth limit exceeded; possible cycle in class hierarchy">>
                )};
        not_found ->
            %% Unreachable: class_chain_step/6 always resolves to
            %% {found, _} or {next, _}, never a bare not_found. Assert the
            %% invariant instead of silently returning a plausible-looking
            %% does_not_understand — if class_chain_step/6 is ever changed to
            %% violate its contract, this must fail loudly, not hide the bug
            %% behind a normal-looking DNU.
            erlang:error({unreachable, not_found, ClassName, Selector})
    end.

-doc """
Per-node probe for the `beamtalk_hierarchy:walk_ancestors/3` walk: check
whether `ClassName` has `Selector` locally and, if so, invoke it; otherwise
advance to its superclass. Always resolves to `{found, dispatch_result()}` —
this module never lets the generic walker's bare `not_found` escape, since
every "not found here" branch already knows how to build a structured
`#beamtalk_error{}`.
""".
-spec class_chain_step(selector(), args(), bt_self(), state(), class_name(), non_neg_integer()) ->
    beamtalk_hierarchy:step_result(dispatch_result()).
class_chain_step(Selector, Args, Self, State, ClassName, _Depth) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {found, {error, beamtalk_error:new(class_not_found, ClassName, Selector)}};
        ClassPid ->
            case beamtalk_object_class:has_method(ClassPid, Selector) of
                true ->
                    %% Found the method - invoke it
                    invoke_step(ClassName, ClassPid, Selector, Args, Self, State);
                false ->
                    %% Not found in this class - try superclass
                    case beamtalk_object_class:superclass(ClassPid) of
                        none ->
                            %% Reached root without finding method
                            ?LOG_DEBUG("Method not found in hierarchy", #{
                                selector => Selector,
                                root => ClassName,
                                domain => [beamtalk, runtime]
                            }),
                            %% BT-753: Derive class from Self when State is empty (class objects).
                            ErrorClass = class_name_from(Self, State, ClassName),
                            Error = beamtalk_error:new(
                                does_not_understand,
                                ErrorClass,
                                Selector,
                                <<"Check spelling or use 'respondsTo:' to verify method exists">>
                            ),
                            {found, {error, Error}};
                        SuperclassName ->
                            {next, SuperclassName}
                    end
            end
    end.

-doc """
Invoke a method found in the hierarchy, adapting `invoke_method/6`'s
`{continue, SuperclassName}` escape hatch (module-less / dispatch-less
classes, BT-427) to the walker's step protocol.
""".
-spec invoke_step(class_name(), pid(), selector(), args(), bt_self(), state()) ->
    beamtalk_hierarchy:step_result(dispatch_result()).
invoke_step(ClassName, ClassPid, Selector, Args, Self, State) ->
    case invoke_method(ClassName, ClassPid, Selector, Args, Self, State) of
        {continue, none} ->
            %% Reached root without finding a dispatchable method (BT-427).
            %% BT-753: Derive class from Self when State is empty (class objects).
            ErrorClass = class_name_from(Self, State, unknown),
            Error = beamtalk_error:new(
                does_not_understand,
                ErrorClass,
                Selector,
                <<"Check spelling or use 'respondsTo:' to verify method exists">>
            ),
            {found, {error, Error}};
        {continue, SuperclassName} ->
            {next, SuperclassName};
        Result ->
            {found, Result}
    end.

-doc """
Invoke a method found in the hierarchy.

This function handles invocation for both compiled and dynamic classes:
- Compiled classes: call Module:dispatch(Selector, Args, Self, State)
- Dynamic classes: call apply(Fun, [Self, Args, State])

The class process knows the module name, so we can determine which strategy to use.
ClassPid is passed from the caller to avoid a redundant whereis_class lookup.
Returns `{continue, SuperclassName | none}` when this class has no
dispatchable module (BT-427) — the caller (`invoke_step/6`) advances the
walk or raises `does_not_understand` accordingly.
""".
-spec invoke_method(class_name(), pid(), selector(), args(), bt_self(), state()) ->
    dispatch_result() | {continue, class_name() | none}.
invoke_method(MethodOwner, ClassPid, Selector, Args, Self, State) ->
    %% Get the module name for this class
    case beamtalk_object_class:module_name_safe(ClassPid) of
        undefined ->
            %% Dynamic class or no module — continue to superclass (BT-427)
            {continue, beamtalk_object_class:superclass(ClassPid)};
        ModuleName ->
            %% Ensure the module is loaded before checking exports.
            %% BEAM lazy-loads modules, and function_exported/3 only checks
            %% loaded modules.
            _ = code:ensure_loaded(ModuleName),
            %% Verify the module exports dispatch/4 before calling it.
            %% This avoids catching error:undef broadly, which could mask
            %% bugs inside the dispatch function itself.
            case erlang:function_exported(ModuleName, dispatch, 4) of
                false ->
                    %% Module exists but lacks dispatch/4 — continue to superclass (BT-427)
                    {continue, beamtalk_object_class:superclass(ClassPid)};
                true ->
                    %% Intercept printString/displayString/inspect for actor and
                    %% supervisor instances and route them to beamtalk_object_ops —
                    %% but only when the method resolved to a class the instance
                    %% did NOT override it on. A user subclass override is found
                    %% earlier in the walk (MethodOwner is that subclass) and must
                    %% be honoured.
                    %%
                    %% ADR 0094: a default actor's or supervisor's
                    %% printString/displayString must render the kind-headed
                    %% `Actor(ClassName, pid)` / `Supervisor(ClassName, pid)` /
                    %% `DynamicSupervisor(ClassName, pid)` label, which only
                    %% beamtalk_object_ops produces — the compiled Object methods
                    %% return a bare class name. Routing here unifies all three
                    %% display selectors onto the runtime renderer.
                    %%
                    %% For actors, an unoverridden printString/displayString/inspect
                    %% resolves with MethodOwner = 'Object': actor-class codegen only
                    %% reports `has_method` true for locally-declared selectors, so the
                    %% hierarchy walk (`class_chain_step`) keeps advancing until it
                    %% reaches Object itself.
                    %%
                    %% For supervisors it's different (BT-3082): `Supervisor`/
                    %% `DynamicSupervisor` are plain "Value" classes (`Object
                    %% subclass: Supervisor`), and value-type codegen's `has_method/1`
                    %% *delegates* to its superclass for any selector it doesn't
                    %% locally list (see `value_type_codegen.rs`
                    %% `generate_primitive_has_method`/`generate_minimal_has_method`).
                    %% That delegation makes `beamtalk_object_class:has_method/2`
                    %% report `true` as soon as it reaches `Supervisor`/
                    %% `DynamicSupervisor` — the walk never actually visits a node
                    %% named `'Object'` for an unoverridden supervisor, it stops one
                    %% level short. So an unoverridden supervisor's MethodOwner is
                    %% `'Supervisor'` or `'DynamicSupervisor'`, never `'Object'` —
                    %% before this fix, that meant `aSupervisor printString` fell
                    %% through to the compiled Object method's bare class name
                    %% instead of matching the REPL's kind-headed label.
                    %%
                    %% This also avoids the displayString deadlock: the compiled
                    %% Object displayString sends a message back to Self
                    %% (displayString calls self printString), producing a second
                    %% gen_server:call on the same actor process. beamtalk_object_ops
                    %% derives the label directly from the tuple with no self-sends.
                    %% (Supervisors dispatch in-process — not via gen_server:call —
                    %% so they're not deadlock-prone the same way, but routing them
                    %% through the same renderer keeps one label authority.)
                    %%
                    %% ADR 0095 Phase 3 (BT-2504): `inspect` on a default actor is
                    %% also routed here. Although it returns an `Inspector` cursor
                    %% (not a self-rendered string), the compiled `Object>>inspect`
                    %% is `Inspector on: self`, i.e. `on/1` — which, when it runs
                    %% *inside* the actor's own `handle_call`, issues
                    %% `sys:get_state(self())` and dead-locks (time-out →
                    %% `#unavailable`). beamtalk_object_ops:dispatch(inspect, ...)
                    %% instead seeds the cursor from the in-hand `State` via
                    %% `on/2` — the self-inspection-safe path. A user `inspect`
                    %% override is honoured as usual.
                    %% `inspect` is deliberately *not* extended to supervisors here
                    %% (unlike printString/displayString above): supervisors dispatch
                    %% in-process, so `Object>>inspect`'s self-send isn't deadlock-prone
                    %% for them, and BT-3082 only reported the printString/displayString
                    %% divergence — widening `inspect`'s behaviour is out of scope.
                    IsDisplaySelector =
                        Selector =:= 'printString' orelse Selector =:= 'displayString',
                    IsUnoverriddenSupervisorMethod =
                        is_supervisor_instance(Self) andalso IsDisplaySelector andalso
                            (MethodOwner =:= 'Object' orelse MethodOwner =:= 'Supervisor' orelse
                                MethodOwner =:= 'DynamicSupervisor'),
                    IsUnoverriddenActorMethod =
                        MethodOwner =:= 'Object' andalso is_actor_instance(Self) andalso
                            (IsDisplaySelector orelse Selector =:= inspect),
                    case IsUnoverriddenActorMethod orelse IsUnoverriddenSupervisorMethod of
                        true ->
                            %% beamtalk_object_ops:dispatch is known-safe, but wrap it
                            %% with the same normalization as the slow path so callers
                            %% always receive a canonical {reply,_,_} | {error,_} tuple.
                            try beamtalk_object_ops:dispatch(Selector, Args, Self, State) of
                                {reply, _, _} = Reply -> Reply;
                                {error, Error, _State} -> {error, Error}
                            catch
                                Type:Reason:Stack ->
                                    ?LOG_DEBUG("Erlang error in beamtalk_object_ops:dispatch", #{
                                        selector => Selector,
                                        reason => beamtalk_error:format_reason(Type, Reason),
                                        domain => [beamtalk, runtime]
                                    }),
                                    Wrapped = beamtalk_exception_handler:ensure_wrapped(
                                        Type,
                                        Reason,
                                        Stack,
                                        dispatch_context(Selector, Self, State, MethodOwner)
                                    ),
                                    #{error := BtError} = Wrapped,
                                    {error, BtError}
                            end;
                        false ->
                            %% Normalize the return value: dispatch/4 returns either
                            %% {reply, Result, NewState} or {error, Error, State} (3-tuple).
                            %% We call it inside a try/catch to translate raw Erlang exceptions
                            %% into structured beamtalk errors instead of letting them escape.
                            try
                                case ModuleName:dispatch(Selector, Args, Self, State) of
                                    {reply, _, _} = Reply -> Reply;
                                    {error, Error, _State} -> {error, Error};
                                    Other -> Other
                                end
                            catch
                                Type:Reason:Stack ->
                                    ?LOG_DEBUG("Erlang error in compiled dispatch", #{
                                        module => ModuleName,
                                        selector => Selector,
                                        reason => beamtalk_error:format_reason(Type, Reason),
                                        domain => [beamtalk, runtime]
                                    }),
                                    Wrapped = beamtalk_exception_handler:ensure_wrapped(
                                        Type,
                                        Reason,
                                        Stack,
                                        dispatch_context(Selector, Self, State, MethodOwner)
                                    ),
                                    #{error := BtError} = Wrapped,
                                    {error, BtError}
                            end
                    end
            end
    end.

%% Build the dispatch breadcrumb (BT-2705) handed to the error-wrap boundary so
%% raw Erlang errors escaping a compiled method are classified *and* located.
%% Computed only on the error path; the receiver's actual class is preferred
%% over the method owner so the breadcrumb names what the user sent to.
-spec dispatch_context(selector(), bt_self(), state(), class_name()) -> map().
dispatch_context(Selector, Self, State, MethodOwner) ->
    #{selector => Selector, class => class_name_from(Self, State, MethodOwner)}.

-doc """
Invoke an extension method.

Extension methods are stored as closures in the extension registry.
Supports two signatures based on the target class type:

- Actor extensions: fun(Args, Self, State) -> {Result, NewState}
  State mutations are threaded back to the gen_server.
- Value-type extensions: fun(Args, Self) -> Result
  No state threading (value types have no mutable state).

BT-1512: The arity is checked at call time to support both signatures
from a single dispatch path.

BT-3199: A crashing extension body is caught and converted to a structured
`#beamtalk_error{}` via `beamtalk_exception_handler:ensure_wrapped/4` +
`dispatch_context/4` — the exact same pattern `invoke_method/6` already uses
for a crash inside a compiled/runtime-installed method reached via this same
hierarchy walk. Before this, a crashing extension was the one dispatch path
in this module that instead re-raised the bare Erlang exception. For a
value-type receiver (evaluated inline in the caller's process) that was
mostly harmless, but for an actor instance — whose `lookup/5` call runs
inside its own gen_server's `handle_call` (`beamtalk_actor:dispatch_via_hierarchy/4`,
which only catches `exit:`, not `error:`) — the re-raise escaped uncaught and
crashed the actor process, unlike an equivalent crash in a regular method
body (already crash-safe via `dispatch_user_method/4`'s own catch). This
closes that asymmetry and mirrors the class-side crash-safety guarantee
BT-3192 already established for class-side extensions (`ClassName` is the
class the extension was registered under — `CurrentClass` from `lookup/5`
or `SuperclassName` from `super/5` — used for the error's breadcrumb
context, same role `MethodOwner` plays for `invoke_method/6`).

A connected `Program exit: N` (ADR 0099 §3) and a `^` non-local return in
flight (ADR 0041/BT-3022, thrown as `{'$bt_nlr', ...}` — see
`beamtalk_result:'tryDo:'/1` for the same two tuple shapes) must pass through
this frame untouched rather than be caught by the generic clause below: for a
value-type extension (arity-2), `apply_extension_by_arity/4` runs the fun
inline in the caller's own process, so the throw is a control-flow signal
aimed at a catch further up that *same* call stack, not a crash to report —
catching and wrapping it here would turn a non-local return into a spurious
`#beamtalk_error{}` instead of letting it unwind. Mirrors the passthrough
`beamtalk_class_dispatch:apply_class_extension_fun/6` already has for the
class-side extension path (that sibling also relays the NLR outward via a
tagged `{nlr_relay, ...}` return, since a class method crosses its
gen_server's `handle_call` boundary and BT-3198's shadow-relay machinery
needs the tag; plain instance dispatch has no such boundary to relay across
here, so re-raising is enough).
""".
-spec invoke_extension(fun(), selector(), class_name(), args(), bt_self(), state()) ->
    dispatch_result().
invoke_extension(Fun, Selector, ClassName, Args, Self, State) ->
    try
        {Result, NewState} = apply_extension_by_arity(Fun, Args, Self, State),
        {reply, Result, NewState}
    catch
        throw:{beamtalk_script_exit, _} = ScriptExit:ScriptStack ->
            erlang:raise(throw, ScriptExit, ScriptStack);
        throw:Nlr:NlrStack when ?IS_NLR(Nlr) ->
            erlang:raise(throw, Nlr, NlrStack);
        Type:Reason:Stack ->
            ?LOG_DEBUG("Erlang error in extension method", #{
                selector => Selector,
                reason => beamtalk_error:format_reason(Type, Reason),
                domain => [beamtalk, runtime]
            }),
            Wrapped = beamtalk_exception_handler:ensure_wrapped(
                Type,
                Reason,
                Stack,
                dispatch_context(Selector, Self, State, ClassName)
            ),
            #{error := BtError} = Wrapped,
            {error, BtError}
    end.

-doc """
Apply an extension fun given its registered arity, unifying both signatures
to a plain `{Result, NewState}` pair — the shared "how do I call this fun"
core behind `invoke_extension/6`.

BT-3192: exported so `beamtalk_class_dispatch:invoke_class_extension/7` can
reuse this exact arity convention for class-side extensions instead of
duplicating it. Deliberately does NOT decide how to handle an error — that is
context-dependent: `invoke_extension/6` (instance-side dispatch, below) uses
the module's shared `ensure_wrapped/4` classification (BT-3199), matching
every other crash-safe dispatch path in this file; class-side dispatch
instead needs its own finer-grained classification (`undef_in_body` vs.
generic, plus NLR-relay / script-exit passthrough for self-sends inside class
methods — see `beamtalk_class_dispatch:apply_class_extension_fun/6`), since
the class's own long-lived gen_server must survive a bad extension body the
same way it survives a bad compiled/runtime-installed class method.
""".
-spec apply_extension_by_arity(fun(), args(), bt_self(), state()) -> {term(), state()}.
apply_extension_by_arity(Fun, Args, Self, State) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    case Arity of
        3 ->
            %% Actor extension: fun(Args, Self, State) -> {Result, NewState}
            apply(Fun, [Args, Self, State]);
        2 ->
            %% Value-type extension: fun(Args, Self) -> Result
            {apply(Fun, [Args, Self]), State};
        _ ->
            error({bad_extension_arity, Arity})
    end.

-doc """
Extract class name from Self or State, with a default fallback.

Delegates to beamtalk_object_ops:class_name/3 to avoid duplication.
""".
-spec class_name_from(bt_self(), state(), atom()) -> atom().
class_name_from(Self, State, Default) ->
    beamtalk_object_ops:class_name(Self, State, Default).

-doc """
Safe extension registry lookup.

Guards against the ETS table not existing (e.g., during early bootstrap).
Returns {ok, Fun} if found, not_found otherwise.

BT-3192: exported so `beamtalk_class_dispatch:handle_class_method_call/6` can
share this exact bootstrap guard when checking the extension registry for a
class-side extension (keyed under the metaclass tag), instead of duplicating
the `error:badarg` catch.
""".
-spec check_extension(class_name(), selector()) -> {ok, fun()} | not_found.
check_extension(ClassName, Selector) ->
    try beamtalk_extensions:lookup(ClassName, Selector) of
        {ok, Fun, _Owner} -> {ok, Fun};
        not_found -> not_found
    catch
        error:badarg ->
            %% ETS table doesn't exist yet (early bootstrap)
            not_found
    end.

-doc """
Return true if Self is an actor instance (a #beamtalk_object{} with a pid).

Used to detect the deadlock-prone case where displayString/printString are sent
to an actor while inside the actor's gen_server callback. In that case the
compiled bt@stdlib@object:displayString/1 would call
beamtalk_message_dispatch:send(Self, 'printString', []), causing a second
gen_server:call on the same process → deadlock. beamtalk_object_ops handles
these methods safely without any self-sends.
""".
-spec is_actor_instance(term()) -> boolean().
is_actor_instance(#beamtalk_object{pid = Pid}) when is_pid(Pid) -> true;
is_actor_instance(_) -> false.

-doc """
Return true if Self is a live supervisor reference (BT-3082).

Used alongside `is_actor_instance/1` to route a default (unoverridden)
`printString`/`displayString` to `beamtalk_object_ops`, which renders the
ADR 0094 kind-headed `Supervisor(ClassName, pid)` / `DynamicSupervisor(...)`
label — matching the REPL's canonical rendering — instead of the compiled
`Object>>printString`'s bare class name.
""".
-spec is_supervisor_instance(term()) -> boolean().
is_supervisor_instance({beamtalk_supervisor, _ClassName, _Module, _Pid}) -> true;
is_supervisor_instance(_) -> false.
