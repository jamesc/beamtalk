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
    send_threaded/4,
    call_threaded/3,
    call_threaded_with_arguments/3,
    is_tier2_block/1
]).

-include("beamtalk.hrl").

%% The ADR 0132 Tier 2 mark and plain-entry sentinel `call_threaded/3` reads.
%% Codegen no longer mints either: ADR 0133 Phase 2 (BT-3904) removed the
%% class-method threaded entries (whose plain wrapper passed the sentinel), the
%% `letrec` mark on Tier 2 block funs and the generated
%% `beamtalk_threading_abi.hrl`. Only Erlang test code
%% (`beamtalk_tier2_test_block`, `beamtalk_threaded_dispatch_tests`) produces
%% them now; BT-3907 removes this gate with the rest of the runtime floor.
-define(BT_TIER2_BLOCK_NAME, 'BtTier2Block').
-define(BT_PLAIN_ENTRY_ACC(ClassTag, Selector), {plain, ClassTag, Selector}).

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
%% The plain-entry sentinel `Acc` (`?BT_PLAIN_ENTRY_ACC(ClassTag, Selector)`,
%% ADR 0132 §2). The plain wrapper of a class-method threaded entry passed it;
%% no compiled code does since BT-3904, only the EUnit suites.
-type plain_entry_acc() :: {plain, atom(), selector()}.
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
            %% Get the superclass. `superclass_safe/1' (not the raw
            %% `superclass/1') so a hierarchy walk reached from a class's own
            %% process (or a process it is synchronously blocked on) resolves
            %% via metadata instead of a `gen_server:call' that would
            %% deadlock and surface only as an opaque `{calling_self, ...}'
            %% exit — see `beamtalk_object_class:superclass_safe/1's doc.
            case beamtalk_object_class:superclass_safe(ClassPid) of
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
Value-context `super` send.

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
local node. The flattened table cache was removed to eliminate its race
window and O(N) rebuild broadcast cascade.

The walk itself (depth guard, cycle warning, advance-to-superclass)
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

Probes via `beamtalk_object_class:has_method_local/2`, not
`has_method/2` — this walk supplies its own node-by-node hierarchy traversal,
so the per-node answer must mean "does *this exact class* define `Selector`
locally", never "does this class or any ancestor". `has_method/2` (through a
compiled actor class's `has_method/1`, which delegates dynamically)
answers the latter, which would make this walk stop at the wrong node and
`invoke_method` dispatch to the wrong owner — see `invoke_method/6`'s
`IsUnoverriddenActorMethod` doc for the mechanism this guards against.
""".
-spec class_chain_step(selector(), args(), bt_self(), state(), class_name(), non_neg_integer()) ->
    beamtalk_hierarchy:step_result(dispatch_result()).
class_chain_step(Selector, Args, Self, State, ClassName, _Depth) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {found, {error, beamtalk_error:new(class_not_found, ClassName, Selector)}};
        ClassPid ->
            case beamtalk_object_class:has_method_local(ClassPid, Selector) of
                true ->
                    %% Found the method - invoke it
                    invoke_step(ClassName, ClassPid, Selector, Args, Self, State);
                false ->
                    %% Not found in this class - try superclass
                    case beamtalk_object_class:superclass_safe(ClassPid) of
                        none ->
                            %% Reached root without finding method
                            ?LOG_DEBUG("Method not found in hierarchy", #{
                                selector => Selector,
                                root => ClassName,
                                domain => [beamtalk, runtime]
                            }),
                            %% Derive class from Self when State is empty (class objects).
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
classes) to the walker's step protocol.
""".
-spec invoke_step(class_name(), pid(), selector(), args(), bt_self(), state()) ->
    beamtalk_hierarchy:step_result(dispatch_result()).
invoke_step(ClassName, ClassPid, Selector, Args, Self, State) ->
    case invoke_method(ClassName, ClassPid, Selector, Args, Self, State) of
        {continue, none} ->
            %% Reached root without finding a dispatchable method.
            %% Derive class from Self when State is empty (class objects).
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
dispatchable module — the caller (`invoke_step/6`) advances the
walk or raises `does_not_understand` accordingly.
""".
-spec invoke_method(class_name(), pid(), selector(), args(), bt_self(), state()) ->
    dispatch_result() | {continue, class_name() | none}.
invoke_method(MethodOwner, ClassPid, Selector, Args, Self, State) ->
    %% Get the module name for this class
    case beamtalk_object_class:module_name_safe(ClassPid) of
        undefined ->
            %% Dynamic class or no module — continue to superclass
            {continue, beamtalk_object_class:superclass_safe(ClassPid)};
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
                    %% Module exists but lacks dispatch/4 — continue to superclass
                    {continue, beamtalk_object_class:superclass_safe(ClassPid)};
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
                    %% resolves with MethodOwner = 'Object': class_chain_step/6 probes
                    %% each level with beamtalk_object_class:has_method_local/2,
                    %% which for a compiled actor class answers via
                    %% `has_method_local/1` — a strictly local own-methods-or-extension
                    %% check that never delegates to a superclass, unlike `has_method/1`
                    %% itself. So the hierarchy walk keeps advancing one class at a time
                    %% until it reaches Object itself (see class_chain_step/6's own doc
                    %% for the O(depth²) this costs).
                    %%
                    %% For supervisors it's different: `Supervisor`/
                    %% `DynamicSupervisor` are plain "Value" classes (`Object
                    %% subclass: Supervisor`), whose codegen never sets
                    %% `DispatchSpec.emit_local_probe` — they export no
                    %% `has_method_local/1`, so `has_method_local/2` falls back to their
                    %% `has_method/1`, which *delegates* to its superclass for any
                    %% selector it doesn't locally list (see `value_type_codegen.rs`
                    %% `generate_primitive_has_method`/`generate_minimal_has_method`).
                    %% That delegation makes the per-node probe report `true` as soon as
                    %% it reaches `Supervisor`/`DynamicSupervisor` — the walk never
                    %% actually visits a node named `'Object'` for an unoverridden
                    %% supervisor, it stops one level short. So an unoverridden
                    %% supervisor's MethodOwner is `'Supervisor'` or
                    %% `'DynamicSupervisor'`, never `'Object'` — without this branch,
                    %% `aSupervisor printString` would fall through to the compiled
                    %% Object method's bare class name instead of matching the REPL's
                    %% kind-headed label.
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
                    %% ADR 0095 Phase 3: `inspect` on a default actor is
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
                    %% for them, and only the printString/displayString divergence was
                    %% reported — widening `inspect`'s behaviour is out of scope.
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
                            %% bt-catcher-audit: not-applicable-reraises - {error,_} is re-raised
                            %% by every in-tree caller (or replied by an actor)
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

%% Build the dispatch breadcrumb handed to the error-wrap boundary so
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

The arity is checked at call time to support both signatures
from a single dispatch path.

A crashing extension body is caught and converted to a structured
`#beamtalk_error{}` via `beamtalk_exception_handler:ensure_wrapped/4` +
`dispatch_context/4` — the exact same pattern `invoke_method/6` already uses
for a crash inside a compiled/runtime-installed method reached via this same
hierarchy walk. Without this catch, a crashing extension would be the one
dispatch path in this module that instead re-raises the bare Erlang
exception. For a value-type receiver (evaluated inline in the caller's
process) that would be mostly harmless, but for an actor instance — whose
`lookup/5` call runs inside its own gen_server's `handle_call`
(`beamtalk_actor:dispatch_via_hierarchy/4`, which only catches `exit:`, not
`error:`) — the re-raise would escape uncaught and crash the actor process,
unlike an equivalent crash in a regular method body (already crash-safe via
`dispatch_user_method/4`'s own catch). This mirrors the class-side
crash-safety guarantee established for class-side extensions (`ClassName` is
the class the extension was registered under — `CurrentClass` from
`lookup/5` or `SuperclassName` from `super/5` — used for the error's
breadcrumb context, same role `MethodOwner` plays for `invoke_method/6`).

A connected `Program exit: N` (ADR 0099 §3) and a `^` non-local return in
flight (ADR 0041, thrown as `{'$bt_nlr', ...}` — see
`beamtalk_result:'tryDo:'/1` for the same two tuple shapes) must pass through
this frame untouched rather than be caught by the generic clause below: for a
value-type extension (arity-2), `apply_extension_by_arity/4` runs the fun
inline in the caller's own process, so the throw is a control-flow signal
aimed at a catch further up that *same* call stack, not a crash to report —
catching and wrapping it here would turn a non-local return into a spurious
`#beamtalk_error{}` instead of letting it unwind. Mirrors the passthrough
`beamtalk_class_dispatch:apply_class_extension_fun/5` already has for the
class-side extension path (that sibling also relays the NLR outward via a
tagged `{nlr_relay, ...}` return, since a class method crosses its
gen_server's `handle_call` boundary and the entry point
needs the tag to keep the class-variable writes made before the unwind; plain instance dispatch has no such boundary to relay across
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

Internal to this module: only `invoke_extension/6` (instance-side dispatch)
calls it. Class-side extensions no longer share this arity convention (ADR 0130
§3: they are `fun(Args, ClassSelf)` and `beamtalk_class_dispatch:
apply_class_extension_fun/5` calls them directly). Deliberately does NOT decide
how to handle an error; `invoke_extension/6` classifies via the shared
`ensure_wrapped/4`.
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

Exported so `beamtalk_class_dispatch:handle_class_method_call/6` can
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

%%% ============================================================================
%%% Threaded sends (ADR 0132)
%%% ============================================================================

-doc """
Send `Selector` to `Receiver` through its threaded entry (ADR 0132 §4).

The threaded form of a plain send whose arguments include a Tier 2 (stateful)
block: `Acc` is the caller's map of outer locals the block writes, and the
answer is `{Result, Acc1}` with the block's writes applied. The entry is found
by **metadata**, never by fun arity: the receiver's class must advertise it in
`tier2_methods` / `tier2_class_methods` and export it
(`beamtalk_class_dispatch:lookup_threaded_entry/3` and
`lookup_threaded_class_entry/3`, both guarded by `erlang:function_exported/3`).

Anything without an entry raises `#beamtalk_error{kind =
stateful_block_dispatch}` naming the receiver's class and the selector, never a
silent drop of the block's writes and never an `undef`.

**ADR 0132 Phase 0 floor.** This resolves at the receiver's exact class only,
and only where the plain send runs in the caller: a class method that
`class_send/3` would direct-call (ADR 0129 Phase 0b), or a value's own class
with no extension (ADR 0066) shadowing the selector. Actor instances,
metaclasses, gen_server-dispatched class methods and inherited methods all
raise until the Phase 2 (resolution walk) and Phase 4 (processes) paths exist.
No compiled module advertises an entry before ADR 0132 Phase 1, so for every
compiled receiver this raises.
""".
-spec send_threaded(term(), selector(), args(), map()) -> {term(), map()}.
send_threaded({beamtalk_future, _} = Future, Selector, Args, Acc) ->
    %% Auto-await, as the plain send does (beamtalk_message_dispatch:send/3).
    send_threaded(beamtalk_future:await(Future), Selector, Args, Acc);
send_threaded(Receiver, Selector, Args, Acc) when
    is_atom(Selector), is_list(Args), is_map(Acc)
->
    case threaded_target(Receiver, Selector, Args) of
        {ok, Module, EntryFn, Self} ->
            erlang:apply(Module, EntryFn, [Self | Args] ++ [Acc]);
        error ->
            raise_no_threaded_entry(Receiver, Selector)
    end.

-spec threaded_target(term(), selector(), args()) ->
    {ok, module(), atom(), term()} | error.
threaded_target(#beamtalk_object{pid = ClassPid} = Receiver, Selector, Args) ->
    case beamtalk_class_registry:is_class_object(Receiver) of
        true ->
            %% Only where the plain send is a direct call in the caller (and
            %% so runs with `ClassSelf = nil`, as `class_send/3` does). A class
            %% method that runs in its class process needs the threaded
            %% gen_server variant (ADR 0132 Phase 4).
            case beamtalk_class_dispatch:lookup_direct_call(ClassPid, Selector, Args) of
                {ok, _, _} ->
                    with_self(
                        beamtalk_class_dispatch:lookup_threaded_class_entry(
                            ClassPid, Selector, Args
                        ),
                        nil
                    );
                error ->
                    error
            end;
        false ->
            %% An actor instance or a metaclass: no threaded path before
            %% ADR 0132 Phase 4.
            error
    end;
threaded_target(Receiver, Selector, Args) ->
    ClassName = beamtalk_primitive:class_of(Receiver),
    case check_extension(ClassName, Selector) of
        {ok, _Fun} ->
            %% An extension shadows the compiled method and has no entry.
            error;
        not_found ->
            with_self(
                beamtalk_class_dispatch:lookup_threaded_entry(ClassName, Selector, Args),
                Receiver
            )
    end.

with_self({ok, Module, EntryFn}, Self) -> {ok, Module, EntryFn, Self};
with_self(error, _Self) -> error.

-spec raise_no_threaded_entry(term(), selector()) -> no_return().
raise_no_threaded_entry(Receiver, Selector) ->
    Class = beamtalk_primitive:class_of(Receiver),
    Error0 = beamtalk_error:new(stateful_block_dispatch, Class, Selector),
    Error1 = beamtalk_error:with_message(
        Error0,
        beamtalk_error:generate_message(stateful_block_dispatch, Class, {threaded, Selector})
    ),
    %% atom_to_binary rather than io_lib:format: see handle_class_self_call/1
    %% in beamtalk_class_dispatch for why error construction must not badarg.
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"`">>,
            atom_to_binary(Selector, utf8),
            <<"` on `">>,
            atom_to_binary(Class, utf8),
            <<
                "` does not thread its block: return the new value from the "
                "block and assign it where it is called"
            >>
        ])
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(Error2, #{reason => no_threaded_entry})
    ).

-doc """
Invoke a callable that may be a Tier 2 block, threading `Acc` (ADR 0132 §2).

The per-invocation tier gate every threaded entry uses for a threading
parameter, answering `{Result, Acc1}`. **The tier is decided before anything
is applied** (BT-3885): a fun of arity `length(Args) + 1` is a Tier 2 block only
when it carries the Tier 2 mark (`is_tier2_block/1`, the fun name
`?BT_TIER2_BLOCK_NAME`); otherwise it is a plain block of the wrong arity, and
it is never run. Since ADR 0133 Phase 2 (BT-3904) codegen mints no marked fun
and passes no sentinel; only Erlang code reaches this gate.

- a fun of arity `length(Args)` (Tier 1) is applied to `Args` and answers
  `{Result, Acc}` with `Acc` unchanged;
- with a map `Acc`, a marked fun of arity `length(Args) + 1` (Tier 2) is applied
  to `Args ++ [Acc]` and answers its own `{Result, Acc1}`;
- with the plain-entry sentinel `?BT_PLAIN_ENTRY_ACC(ClassTag, Selector)` as
  `Acc` (what the plain wrapper of a class-method threaded entry passed before
  BT-3904 removed the entries: it had no caller map), a marked fun of arity `length(Args) + 1` raises the structured
  `stateful_block_dispatch` naming `ClassTag>>Selector`, *without running*,
  because its writes would be dropped. The sentinel is never used as a map;
- any other fun (an unmarked fun of arity `length(Args) + 1` included) raises a
  structured `arity_mismatch`, unrun, never a raw `badarity`;
- a non-fun callable (a `Symbol`, an object answering `value:`, ADR 0128's
  opaque callables) gets the ordinary `value`-family send and answers
  `{Result, Acc}`.
""".
-spec call_threaded(term(), args(), map() | plain_entry_acc()) ->
    {term(), map() | plain_entry_acc()}.
call_threaded(Block, Args, Acc) when is_function(Block), is_list(Args) ->
    N = length(Args),
    case erlang:is_function(Block, N) of
        true ->
            {erlang:apply(Block, Args), Acc};
        false ->
            case erlang:is_function(Block, N + 1) andalso is_tier2_block(Block, N + 1) of
                true -> call_tier2(Block, Args, Acc);
                false -> raise_block_arity_mismatch(block_arity(Block), N)
            end
    end;
call_threaded(Callable, Args, Acc) when is_list(Args) ->
    {beamtalk_message_dispatch:send(Callable, value_selector(length(Args)), value_args(Args)), Acc}.

-spec call_tier2(function(), args(), map() | plain_entry_acc()) -> {term(), map()}.
call_tier2(Block, Args, Acc) when is_map(Acc) ->
    case erlang:apply(Block, Args ++ [Acc]) of
        {_Result, Acc1} = Reply when is_map(Acc1) -> Reply;
        %% Only codegen mints the mark, and it always answers this shape.
        Other -> erlang:error({malformed_tier2_reply, Other})
    end;
call_tier2(_Block, Args, ?BT_PLAIN_ENTRY_ACC(ClassTag, Selector)) ->
    %% The plain-entry sentinel (BT-3871 review; no compiled code passes it
    %% since BT-3904): there is no caller map to thread, so a Tier 2 block
    %% cannot be run - its writes would be dropped silently.
    raise_plain_entry_refusal(ClassTag, Selector, length(Args)).

-doc """
True when `Fun` is a Tier 2 block: a fun minted under the Tier 2 mark
(`?BT_TIER2_BLOCK_NAME`, ADR 0132 / BT-3885).

Codegen bound every Tier 2 block fun with `letrec 'BtTier2Block'/A = fun ...`
until ADR 0133 Phase 2 (BT-3904); now only Erlang code mints the mark, as a
named fun called `BtTier2Block` (`beamtalk_tier2_test_block:mark/1`). The Erlang
compiler names the lifted function of such a fun
`-<Function>/<Arity>-BtTier2Block/<A>-<N>-` (an anonymous fun is
`-<Function>/<Arity>-anonymous-<N>-`, an Erlang `fun` `-<Function>/<Arity>-fun-<N>-`),
so `erlang:fun_info(Fun, name)` carries the mark on the fun value: the gate
reads it instead of applying the fun to find out. Only the *last* segment
counts, so a plain fun created inside a Tier 2 block is not marked.
""".
-spec is_tier2_block(term()) -> boolean().
is_tier2_block(Fun) when is_function(Fun) ->
    is_tier2_block(Fun, block_arity(Fun));
is_tier2_block(_) ->
    false.

%% `is_tier2_block/1` for a fun the caller already knows has arity `Arity`
%% (the gate's hot path: one `fun_info/2`, no pattern compilation).
-spec is_tier2_block(function(), arity()) -> boolean().
is_tier2_block(Fun, Arity) ->
    {name, Name} = erlang:fun_info(Fun, name),
    is_tier2_fun_name(atom_to_binary(Name, utf8), Arity).

%% Reads `Name` from the end, without building a search pattern: it must end in
%% `-<Mark>/<Arity>-<N>-`, `<N>` one or more digits.
-spec is_tier2_fun_name(binary(), arity()) -> boolean().
is_tier2_fun_name(Name, Arity) ->
    Last = byte_size(Name) - 1,
    Last >= 1 andalso binary:at(Name, Last) =:= $- andalso
        lifted_index_start(Name, Last - 1, Last - 1, Arity).

%% Scans the digits of `<N>` backwards from `Pos` (`End` is its last digit).
-spec lifted_index_start(binary(), integer(), integer(), arity()) -> boolean().
lifted_index_start(Name, Pos, End, Arity) when Pos >= 0 ->
    case binary:at(Name, Pos) of
        D when D >= $0, D =< $9 -> lifted_index_start(Name, Pos - 1, End, Arity);
        $- when Pos < End -> lifted_arity(Name, Pos - 1, Arity, 0, 1);
        _ -> false
    end;
lifted_index_start(_Name, _Pos, _End, _Arity) ->
    false.

%% Reads the `<Arity>` digits backwards from `Pos` into `Value` (`Scale` is
%% the next digit's place value), then expects `-<Mark>/` before them.
-spec lifted_arity(binary(), integer(), arity(), non_neg_integer(), pos_integer()) ->
    boolean().
lifted_arity(Name, Pos, Arity, Value, Scale) when Pos >= 0 ->
    case binary:at(Name, Pos) of
        D when D >= $0, D =< $9 ->
            lifted_arity(Name, Pos - 1, Arity, Value + (D - $0) * Scale, Scale * 10);
        $/ when Scale > 1, Value =:= Arity ->
            %% A literal: the compiler folds `atom_to_binary/2` of an atom.
            Mark = <<"-", (atom_to_binary(?BT_TIER2_BLOCK_NAME, utf8))/binary>>,
            MarkLen = byte_size(Mark),
            Pos >= MarkLen andalso binary:part(Name, Pos - MarkLen, MarkLen) =:= Mark;
        _ ->
            false
    end;
lifted_arity(_Name, _Pos, _Arity, _Value, _Scale) ->
    false.

-doc """
`call_threaded/3` for `Callable valueWithArguments: Xs` (ADR 0132 §2).

A fun with a list `Xs` is gated exactly as `call_threaded(Callable, Xs, Acc)`,
like every other invocation: a fun of the wrong arity raises the structured
`arity_mismatch` (or, for a Tier 2 block with the plain-entry sentinel,
`stateful_block_dispatch`), unrun, where the plain `valueWithArguments:` lowering
raised a raw `badarity`, the same change `value:` gets. Everything else keeps
the plain lowering's behaviour: a fun with a non-list `Xs` is applied as it
always was (raising the same error), and a non-fun callable is sent
`valueWithArguments:` itself, never `value:`/`value:value:`, with `Acc`
unchanged.
""".
-spec call_threaded_with_arguments(term(), term(), map() | plain_entry_acc()) ->
    {term(), map() | plain_entry_acc()}.
call_threaded_with_arguments(Block, Xs, Acc) when is_function(Block), is_list(Xs) ->
    call_threaded(Block, Xs, Acc);
call_threaded_with_arguments(Block, Xs, Acc) when is_function(Block) ->
    {erlang:apply(Block, Xs), Acc};
call_threaded_with_arguments(Callable, Xs, Acc) ->
    {beamtalk_message_dispatch:send(Callable, 'valueWithArguments:', [Xs]), Acc}.

-spec block_arity(function()) -> non_neg_integer().
block_arity(Block) ->
    {arity, Arity} = erlang:fun_info(Block, arity),
    Arity.

-doc """
Raise the refusal of a Tier 2 block that reached `call_threaded/3` with the
plain-entry sentinel of class method `ClassTag>>Selector` (BT-3871 review,
BT-3885; no compiled code passes the sentinel since BT-3904): a late-bound send (`perform:`,
a variable receiver) has no caller map to thread its writes into, so the block
is refused before it runs. The error names the class method the caller sent,
never the `value` the body would have sent it.
""".
-spec raise_plain_entry_refusal(atom(), selector(), non_neg_integer()) -> no_return().
raise_plain_entry_refusal(ClassTag, Selector, N) ->
    Error0 = beamtalk_error:new(stateful_block_dispatch, ClassTag, Selector),
    Error1 = beamtalk_error:with_message(
        Error0,
        beamtalk_error:generate_message(stateful_block_dispatch, ClassTag, {plain_entry, Selector})
    ),
    %% atom_to_binary rather than io_lib:format: see raise_no_threaded_entry/2.
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"`">>,
            atom_to_binary(Selector, utf8),
            <<"` on `">>,
            atom_to_binary(ClassTag, utf8),
            <<
                "` was reached by a send that cannot thread the block's writes: "
                "return the new value from the block and assign it where it is called"
            >>
        ])
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(Error2, #{reason => plain_entry, block_arity => N + 1})
    ).

-spec value_selector(non_neg_integer()) -> selector().
value_selector(0) -> value;
value_selector(1) -> 'value:';
value_selector(2) -> 'value:value:';
value_selector(3) -> 'value:value:value:';
value_selector(_) -> 'valueWithArguments:'.

-spec value_args(args()) -> args().
value_args(Args) when length(Args) =< 3 -> Args;
value_args(Args) -> [Args].

-spec raise_block_arity_mismatch(non_neg_integer(), non_neg_integer()) -> no_return().
raise_block_arity_mismatch(Arity, N) ->
    Error0 = beamtalk_error:new(arity_mismatch, 'Block', value_selector(N)),
    Error1 = beamtalk_error:with_hint(
        Error0,
        iolist_to_binary([
            <<"The block takes ">>,
            integer_to_binary(Arity),
            <<" argument(s) but was called with ">>,
            integer_to_binary(N)
        ])
    ),
    beamtalk_error:raise(
        beamtalk_error:with_details(Error1, #{arity => Arity, arg_count => N})
    ).

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
Return true if Self is a live supervisor reference.

Used alongside `is_actor_instance/1` to route a default (unoverridden)
`printString`/`displayString` to `beamtalk_object_ops`, which renders the
ADR 0094 kind-headed `Supervisor(ClassName, pid)` / `DynamicSupervisor(...)`
label — matching the REPL's canonical rendering — instead of the compiled
`Object>>printString`'s bare class name.
""".
-spec is_supervisor_instance(term()) -> boolean().
is_supervisor_instance({beamtalk_supervisor, _ClassName, _Module, _Pid}) -> true;
is_supervisor_instance(_) -> false.
