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
%%% ### invoke_with_combinations/5
%%% Method combinations. Collects before/after from full chain.
%%% ```erlang
%%% invoke_with_combinations(Selector, Args, Self, State, CurrentClass)
%%%     -> {reply, Result, NewState}
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
    invoke_with_combinations/5,
    responds_to/2
]).

-include("beamtalk.hrl").

-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Types
%%% ============================================================================

-type selector() :: atom().
-type args() :: [term()].
-type bt_self() :: term().  % #beamtalk_object{} or value
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
    logger:debug("Dispatching ~p on class ~p", [Selector, CurrentClass]),
    
    %% Step 1: Check extension registry first (guard against missing ETS table)
    case check_extension(CurrentClass, Selector) of
        {ok, Fun} ->
            %% Found extension method - invoke it
            logger:debug("Found extension method ~p on ~p", [Selector, CurrentClass]),
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
    logger:debug("Super dispatch ~p from class ~p", [Selector, CurrentClass]),
    
    %% Look up the current class to get its superclass
    case beamtalk_object_class:whereis_class(CurrentClass) of
        undefined ->
            %% Class not found - return error
            Error0 = beamtalk_error:new(class_not_found, CurrentClass),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            {error, Error1};
        ClassPid ->
            %% Get the superclass
            case beamtalk_object_class:superclass(ClassPid) of
                none ->
                    %% No superclass - method not found
                    Error0 = beamtalk_error:new(does_not_understand, CurrentClass),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Method not found in superclass chain">>),
                    {error, Error2};
                SuperclassName ->
                    %% Start lookup at superclass (skip current class)
                    lookup_in_class_chain(Selector, Args, Self, State, SuperclassName)
            end
    end.

%% @doc Method combinations - collects before/after from full chain.
%%
%% This function implements method combinations (before/after/primary ordering).
%% It walks the full superclass chain and collects:
%% - Before methods: run from superclass → subclass (setup flows down)
%% - Primary method: run only the most specific method (no chain)
%% - After methods: run from subclass → superclass (cleanup flows up)
%%
%% ## Method Combination Ordering
%%
%% ```
%% ProtoObject>>method:before   ← run 1st
%% Object>>method:before        ← run 2nd
%% Counter>>method:before       ← run 3rd
%% Counter>>method              ← run 4th (primary)
%% Counter>>method:after        ← run 5th
%% Object>>method:after         ← run 6th
%% ProtoObject>>method:after    ← run 7th
%% ```
%%
%% ## TODO
%%
%% Method combinations are not yet implemented. This is a placeholder
%% for Phase 1b. For now, this function delegates to `lookup/5`.
-spec invoke_with_combinations(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
invoke_with_combinations(Selector, Args, Self, State, CurrentClass) ->
    %% TODO: Implement method combinations
    %% For now, just delegate to normal lookup
    logger:debug("Method combinations not yet implemented, using normal lookup", []),
    lookup(Selector, Args, Self, State, CurrentClass).

%% @doc Check if a class or any of its ancestors responds to a selector.
%%
%% Walks the class hierarchy from ClassName upward, checking each class's
%% method table (metadata + module has_method/1). Returns true if any class
%% in the chain has the method.
%%
%% Used by Object's `respondsTo:` implementation to check the full hierarchy.
-spec responds_to(selector(), class_name()) -> boolean().
responds_to(Selector, ClassName) ->
    %% Check extension registry first (covers all classes)
    case check_extension(ClassName, Selector) of
        {ok, _Fun} ->
            true;
        not_found ->
            case beamtalk_object_class:whereis_class(ClassName) of
                undefined ->
                    false;
                ClassPid ->
                    case beamtalk_object_class:has_method(ClassPid, Selector) of
                        true ->
                            true;
                        false ->
                            case beamtalk_object_class:superclass(ClassPid) of
                                none -> false;
                                SuperclassName -> responds_to(Selector, SuperclassName)
                            end
                    end
            end
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Look up method in class chain (with flattened table optimization).
%%
%% ADR 0006 Phase 2: This function first checks the flattened method table
%% for O(1) lookup. If the flattened table is missing or stale, it falls back
%% to the recursive hierarchy walk.
%%
%% Algorithm:
%% 1. Check if class exists
%% 2. Try flattened table lookup (O(1) - fast path)
%% 3. If not in flattened table or table missing, fall back to hierarchy walk
%% 4. If found, invoke the method from the defining class
%% 5. Returns error if not found anywhere
-spec lookup_in_class_chain(selector(), args(), bt_self(), state(), class_name()) -> dispatch_result().
lookup_in_class_chain(Selector, Args, Self, State, ClassName) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            %% Class not found
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            {error, Error1};
        ClassPid ->
            %% ADR 0006 Phase 2: Try flattened table first (O(1) lookup)
            case try_flattened_lookup(ClassPid, Selector) of
                {ok, DefiningClass, _MethodInfo} ->
                    %% Found in flattened table - invoke from defining class
                    logger:debug("Found method ~p in flattened table (defined in ~p)", [Selector, DefiningClass]),
                    case beamtalk_object_class:whereis_class(DefiningClass) of
                        undefined ->
                            %% Defining class no longer exists (hot reload edge case)
                            %% Fall back to hierarchy walk
                            logger:debug("Defining class ~p not found, falling back to hierarchy walk", [DefiningClass]),
                            lookup_in_class_chain_slow(Selector, Args, Self, State, ClassName, ClassPid);
                        DefiningClassPid ->
                            invoke_method(DefiningClass, DefiningClassPid, Selector, Args, Self, State)
                    end;
                not_found ->
                    %% Not in flattened table - fall back to hierarchy walk
                    %% This handles:
                    %% - Methods added at runtime (not yet in flattened table)
                    %% - Bootstrap ordering (flattened table incomplete)
                    %% - Extension methods (checked by caller before this)
                    logger:debug("Method ~p not in flattened table, falling back to hierarchy walk", [Selector]),
                    lookup_in_class_chain_slow(Selector, Args, Self, State, ClassName, ClassPid)
            end
    end.

%% @private
%% @doc Slow path: recursive hierarchy walk (O(depth)).
%%
%% Used when flattened table is missing, stale, or incomplete.
%% This is the original ADR 0006 Phase 1 implementation.
-spec lookup_in_class_chain_slow(selector(), args(), bt_self(), state(), class_name(), pid()) -> dispatch_result().
lookup_in_class_chain_slow(Selector, Args, Self, State, ClassName, ClassPid) ->
    %% Check if this class has the method
    case beamtalk_object_class:has_method(ClassPid, Selector) of
        true ->
            %% Found the method - invoke it
            logger:debug("Found method ~p in class ~p (hierarchy walk)", [Selector, ClassName]),
            invoke_method(ClassName, ClassPid, Selector, Args, Self, State);
        false ->
            %% Not found in this class - try superclass
            case beamtalk_object_class:superclass(ClassPid) of
                none ->
                    %% Reached root without finding method
                    logger:debug("Method ~p not found in hierarchy (root: ~p)", [Selector, ClassName]),
                    Error0 = beamtalk_error:new(does_not_understand, maps:get('__class__', State, ClassName)),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'respondsTo:' to verify method exists">>),
                    {error, Error2};
                SuperclassName ->
                    %% Recurse to superclass
                    logger:debug("Method ~p not in ~p, trying superclass ~p", [Selector, ClassName, SuperclassName]),
                    case beamtalk_object_class:whereis_class(SuperclassName) of
                        undefined ->
                            Error0 = beamtalk_error:new(class_not_found, SuperclassName),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            {error, Error1};
                        SuperclassPid ->
                            lookup_in_class_chain_slow(Selector, Args, Self, State, SuperclassName, SuperclassPid)
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
-spec invoke_method(class_name(), pid(), selector(), args(), bt_self(), state()) -> dispatch_result().
invoke_method(ClassName, ClassPid, Selector, Args, Self, State) ->
    %% Get the module name for this class
    case beamtalk_object_class:module_name(ClassPid) of
        undefined ->
            %% Dynamic class or error
            Error0 = beamtalk_error:new(internal_error, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(Error1, <<"Class has no module name">>),
            {error, Error2};
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
                    Error0 = beamtalk_error:new(internal_error, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Module missing dispatch/4 function">>),
                    {error, Error2};
                true ->
                    logger:debug("Invoking ~p:dispatch(~p, ...)", [ModuleName, Selector]),
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
            logger:error("Extension method threw error: ~p~nStacktrace: ~p",
                        [Reason, Stacktrace]),
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

%% @private
%% @doc Try to look up a method in the flattened method table.
%%
%% ADR 0006 Phase 2: Fast O(1) lookup using pre-computed method table.
%% Returns {ok, DefiningClass, MethodInfo} if found, not_found otherwise.
%%
%% The flattened table maps Selector => {DefiningClass, MethodInfo} where
%% DefiningClass is the class that actually implements the method.
-spec try_flattened_lookup(pid(), selector()) ->
    {ok, class_name(), term()} | not_found.
try_flattened_lookup(ClassPid, Selector) ->
    try
        gen_server:call(ClassPid, {lookup_flattened, Selector}, 5000)
    catch
        _:_ ->
            %% gen_server call failed (timeout, noproc, etc.)
            not_found
    end.
