%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Method dispatch domain service (DDD).
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% This module provides the unified dispatch service for Beamtalk method calls,
%%% implementing runtime hierarchy walking and method resolution. It replaces
%%% scattered dispatch logic with a single domain service.
%%%
%%% ## Responsibilities
%%%
%%% - Method lookup via hierarchy walking (starts at given class, walks upward)
%%% - Super send dispatch (starts at immediate superclass)
%%% - Method invocation (handles compiled and dynamic classes)
%%% - Method combinations (before/after/primary ordering)
%%% - Extension method checking at each hierarchy level
%%% - Structured error generation (`#beamtalk_error{}`)
%%%
%%% ## Design Principles (ADR 0006)
%%%
%%% 1. **Runtime lookup is source of truth**: All dispatch paths must use this service
%%% 2. **Hierarchy walking**: Check current class → extensions → superclass → recurse
%%% 3. **Super semantics**: Skip current class, start at immediate superclass
%%% 4. **Method combinations**: Collect before/after from full chain, primary from defining class
%%% 5. **Structured errors**: Always return `#beamtalk_error{}`, never bare tuples
%%%
%%% ## Integration with Other Modules (DDD Separation)
%%%
%%% - **beamtalk_object_class**: Class registry, method storage, metadata (data layer)
%%% - **beamtalk_extensions**: Extension method registry (data layer)
%%% - **beamtalk_dispatch**: Method lookup, hierarchy walking, invocation (domain service)
%%% - **beamtalk_actor**: Actor lifecycle, spawn, supervision (actor domain)
%%%
%%% ## API
%%%
%%% ### lookup/5
%%% Core dispatch entry point. Walks hierarchy from CurrentClass upward.
%%% ```erlang
%%% lookup(Selector, Args, Self, State, CurrentClass)
%%%     -> {reply, Result, NewState} | {error, #beamtalk_error{}}
%%% ```
%%%
%%% ### super/5
%%% Super send dispatch. Starts at immediate superclass (no double-walk).
%%% ```erlang
%%% super(Selector, Args, Self, State, CurrentClass)
%%%     -> {reply, Result, NewState} | {error, #beamtalk_error{}}
%%% ```
%%%
%%% ## References
%%%
%%% - ADR 0006: Unified Method Dispatch with Hierarchy Walking
%%% - docs/internal/design-self-as-object.md: Method resolution design
%%% - runtime/src/beamtalk_object_class.erl: Class registry implementation

-module(beamtalk_dispatch).

-export([
    lookup/5,
    super/5,
    responds_to/2
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

%% @doc Core dispatch entry point - walks hierarchy from CurrentClass upward.
%%
%% This is the primary method lookup function. It:
%% 1. Checks extension registry for CurrentClass + Selector
%% 2. Checks CurrentClass's method table for Selector
%% 3. If not found, recursively checks superclass chain
%% 4. Returns structured #beamtalk_error{} if not found in entire chain
%%
%% ## Example
%%
%% ```erlang
%% %% Call Counter increment (defined locally)
%% lookup(increment, [], Self, State, 'Counter')
%%     -> {reply, 1, NewState}
%%
%% %% Call Counter class (inherited from Object)
%% lookup(class, [], Self, State, 'Counter')
%%     -> {reply, 'Counter', State}  % walks to Object, finds method
%%
%% %% Call Counter unknownMethod (not found anywhere)
%% lookup(unknownMethod, [], Self, State, 'Counter')
%%     -> {error, #beamtalk_error{kind=does_not_understand, class='Counter', selector=unknownMethod}}
%% ```
-spec lookup(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
lookup(Selector, Args, Self, State, CurrentClass) ->
    ?LOG_DEBUG("Dispatching ~p on class ~p", [Selector, CurrentClass]),

    %% Step 1: Check extension registry first (guard against missing ETS table)
    case check_extension(CurrentClass, Selector) of
        {ok, Fun} ->
            %% Found extension method - invoke it
            ?LOG_DEBUG("Found extension method ~p on ~p", [Selector, CurrentClass]),
            invoke_extension(Fun, Args, Self, State);
        not_found ->
            %% Step 2: Check class's own method table
            lookup_in_class_chain(Selector, Args, Self, State, CurrentClass)
    end.

%% @doc Super send dispatch - starts at immediate superclass.
%%
%% This function implements `super` semantics. It skips the current class
%% and starts the hierarchy walk at the immediate superclass. This is critical
%% for preventing infinite recursion when a subclass method calls super.
%%
%% ## Implementation Note
%%
%% Unlike `lookup/5`, this function does NOT check the CurrentClass's method table.
%% It immediately looks up the superclass and delegates to `lookup_in_class_chain/5`.
%%
%% ## Example
%%
%% ```erlang
%% %% In LoggingCounter increment implementation:
%% %% super increment        <- calls this function
%% super(increment, [], Self, State, 'LoggingCounter')
%%     -> looks up Counter (superclass)
%%     -> delegates to Counter's increment implementation
%%     -> returns {reply, NewValue, NewState}
%% ```
-spec super(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
super(Selector, Args, Self, State, CurrentClass) ->
    ?LOG_DEBUG("Super dispatch ~p from class ~p", [Selector, CurrentClass]),

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
                            ?LOG_DEBUG("Found extension method ~p on superclass ~p via super", [
                                Selector, SuperclassName
                            ]),
                            invoke_extension(Fun, Args, Self, State);
                        not_found ->
                            %% Start lookup at superclass (skip current class)
                            lookup_in_class_chain(Selector, Args, Self, State, SuperclassName)
                    end
            end
    end.

%% @doc Check if a class or any of its ancestors responds to a selector.
%%
%% Walks the class hierarchy from ClassName upward, checking each class's
%% local method table via has_method/2. Returns true if any class in the
%% chain has the method.
%%
%% ADR 0032 Phase 1: Direct chain walk replaces the flattened table fast path.
%% Fixes BT-721 where responds_to_slow called try_flattened_lookup instead
%% of has_method, causing inherited method detection to fail.
%%
%% Used by Object's `respondsTo:` implementation to check the full hierarchy.
-spec responds_to(selector(), class_name()) -> boolean().
responds_to(Selector, ClassName) ->
    %% Check extension registry first (covers all classes)
    case check_extension(ClassName, Selector) of
        {ok, _Fun} ->
            true;
        not_found ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    false;
                ClassPid ->
                    responds_to_chain(Selector, ClassPid)
            end
    end.

%% @private
%% @doc Walk hierarchy checking each class's local method table via has_method/2.
-spec responds_to_chain(selector(), pid()) -> boolean().
responds_to_chain(Selector, ClassPid) ->
    responds_to_chain(Selector, ClassPid, 0).

-spec responds_to_chain(selector(), pid(), non_neg_integer()) -> boolean().
responds_to_chain(_Selector, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ?LOG_WARNING("responds_to_chain: max hierarchy depth ~p exceeded — possible cycle", [
        ?MAX_HIERARCHY_DEPTH
    ]),
    false;
responds_to_chain(Selector, ClassPid, Depth) ->
    case beamtalk_object_class:has_method(ClassPid, Selector) of
        true ->
            true;
        false ->
            case beamtalk_object_class:superclass(ClassPid) of
                none ->
                    false;
                SuperclassName ->
                    case beamtalk_class_registry:whereis_class(SuperclassName) of
                        undefined -> false;
                        SuperclassPid -> responds_to_chain(Selector, SuperclassPid, Depth + 1)
                    end
            end
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Look up method in class chain via direct hierarchy walk.
%%
%% ADR 0032 Phase 1: Replaces the flattened table fast path with a direct
%% chain walk via has_method/2 + superclass/1. At max hierarchy depth of 6
%% (typical 4), this means at most ~12 gen_server calls — microseconds on a
%% local node. The flattened table cache was removed to eliminate BT-510's
%% race window and O(N) rebuild broadcast cascade.
-spec lookup_in_class_chain(selector(), args(), bt_self(), state(), class_name()) ->
    dispatch_result().
lookup_in_class_chain(Selector, Args, Self, State, ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {error, beamtalk_error:new(class_not_found, ClassName, Selector)};
        ClassPid ->
            lookup_in_class_chain_slow(Selector, Args, Self, State, ClassName, ClassPid, 0)
    end.

%% @private
%% @doc Slow path: recursive hierarchy walk (O(depth)).
%%
%% Used when flattened table is missing, stale, or incomplete.
%% This is the original ADR 0006 Phase 1 implementation.
-spec lookup_in_class_chain_slow(
    selector(), args(), bt_self(), state(), class_name(), pid(), non_neg_integer()
) -> dispatch_result().
lookup_in_class_chain_slow(Selector, _Args, _Self, _State, ClassName, _ClassPid, Depth) when
    Depth > ?MAX_HIERARCHY_DEPTH
->
    ?LOG_WARNING(
        "lookup_in_class_chain_slow: max hierarchy depth ~p exceeded at ~p — possible cycle",
        [?MAX_HIERARCHY_DEPTH, ClassName]
    ),
    {error,
        beamtalk_error:new(
            does_not_understand,
            ClassName,
            Selector,
            <<"Hierarchy depth limit exceeded — possible cycle in class hierarchy">>
        )};
lookup_in_class_chain_slow(Selector, Args, Self, State, ClassName, ClassPid, Depth) ->
    %% Check if this class has the method
    case beamtalk_object_class:has_method(ClassPid, Selector) of
        true ->
            %% Found the method - invoke it
            ?LOG_DEBUG("Found method ~p in class ~p (hierarchy walk)", [Selector, ClassName]),
            invoke_method(ClassName, ClassPid, Selector, Args, Self, State, Depth);
        false ->
            %% Not found in this class - try superclass
            case beamtalk_object_class:superclass(ClassPid) of
                none ->
                    %% Reached root without finding method
                    ?LOG_DEBUG("Method ~p not found in hierarchy (root: ~p)", [Selector, ClassName]),
                    Error = beamtalk_error:new(
                        does_not_understand,
                        beamtalk_tagged_map:class_of(State, ClassName),
                        Selector,
                        <<"Check spelling or use 'respondsTo:' to verify method exists">>
                    ),
                    {error, Error};
                SuperclassName ->
                    %% Recurse to superclass
                    ?LOG_DEBUG("Method ~p not in ~p, trying superclass ~p", [
                        Selector, ClassName, SuperclassName
                    ]),
                    case beamtalk_class_registry:whereis_class(SuperclassName) of
                        undefined ->
                            {error, beamtalk_error:new(class_not_found, SuperclassName, Selector)};
                        SuperclassPid ->
                            lookup_in_class_chain_slow(
                                Selector,
                                Args,
                                Self,
                                State,
                                SuperclassName,
                                SuperclassPid,
                                Depth + 1
                            )
                    end
            end
    end.

%% @private
%% @doc Invoke a method found in the hierarchy.
%%
%% This function handles invocation for both compiled and dynamic classes:
%% - Compiled classes: call Module:dispatch(Selector, Args, Self, State)
%% - Dynamic classes: call apply(Fun, [Self, Args, State])
%%
%% The class process knows the module name, so we can determine which strategy to use.
%% ClassPid is passed from the caller to avoid a redundant whereis_class lookup.
%% Depth is threaded through for cycle detection in continue_to_superclass.
-spec invoke_method(class_name(), pid(), selector(), args(), bt_self(), state(), non_neg_integer()) ->
    dispatch_result().
invoke_method(_ClassName, ClassPid, Selector, Args, Self, State, Depth) ->
    %% Get the module name for this class
    case beamtalk_object_class:module_name(ClassPid) of
        undefined ->
            %% Dynamic class or no module — continue to superclass (BT-427)
            continue_to_superclass(Selector, Args, Self, State, ClassPid, Depth);
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
                    continue_to_superclass(Selector, Args, Self, State, ClassPid, Depth);
                true ->
                    ?LOG_DEBUG("Invoking ~p:dispatch(~p, ...)", [ModuleName, Selector]),
                    %% Normalize the return value: dispatch/4 returns either
                    %% {reply, Result, NewState} or {error, Error, State} (3-tuple).
                    %% We normalize {error, Error, State} to {error, Error} to match
                    %% the dispatch_result() type and the codegen pattern matching.
                    case ModuleName:dispatch(Selector, Args, Self, State) of
                        {reply, _, _} = Reply -> Reply;
                        {error, Error, _State} -> {error, Error};
                        Other -> Other
                    end
            end
    end.

%% @private
%% @doc Continue hierarchy walk to superclass when current class can't dispatch.
%% Used when a class has no module or its module lacks dispatch/4 (abstract classes).
-spec continue_to_superclass(selector(), args(), bt_self(), state(), pid(), non_neg_integer()) ->
    dispatch_result().
continue_to_superclass(Selector, Args, Self, State, ClassPid, Depth) ->
    case beamtalk_object_class:superclass(ClassPid) of
        none ->
            %% Reached root without finding dispatchable method
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            Error = beamtalk_error:new(
                does_not_understand,
                ClassName,
                Selector,
                <<"Check spelling or use 'respondsTo:' to verify method exists">>
            ),
            {error, Error};
        SuperclassName ->
            case beamtalk_class_registry:whereis_class(SuperclassName) of
                undefined ->
                    {error, beamtalk_error:new(class_not_found, SuperclassName, Selector)};
                SuperclassPid ->
                    lookup_in_class_chain_slow(
                        Selector, Args, Self, State, SuperclassName, SuperclassPid, Depth + 1
                    )
            end
    end.

%% @private
%% @doc Invoke an extension method.
%%
%% Extension methods are stored as closures in the extension registry.
%% They are invoked with apply(Fun, [Args, Value]) where Value is the receiver.
%%
%% ## Note
%%
%% Extensions return results directly, not {reply, Result, NewState} tuples.
%% This is because extensions operate on value types (Integer, String) which
%% have no mutable state. For actors, extensions would need to be wrapped.
-spec invoke_extension(fun(), args(), bt_self(), state()) -> dispatch_result().
invoke_extension(Fun, Args, _Self, State) ->
    try
        Result = apply(Fun, [Args, State]),
        {reply, Result, State}
    catch
        error:Reason:Stacktrace ->
            ?LOG_ERROR(
                "Extension method threw error: ~p~nStacktrace: ~p",
                [Reason, Stacktrace]
            ),
            erlang:raise(error, Reason, Stacktrace)
    end.

%% @private
%% @doc Safe extension registry lookup.
%%
%% Guards against the ETS table not existing (e.g., during early bootstrap).
%% Returns {ok, Fun} if found, not_found otherwise.
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
