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
    invoke_with_combinations/5
]).

-include("beamtalk.hrl").

-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Types
%%% ============================================================================

-type selector() :: atom().
-type args() :: [term()].
-type self() :: term().  % #beamtalk_object{} or value
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
-spec lookup(selector(), args(), self(), state(), class_name()) -> dispatch_result().
lookup(Selector, Args, Self, State, CurrentClass) ->
    logger:debug("Dispatching ~p on class ~p", [Selector, CurrentClass]),
    
    %% Step 1: Check extension registry first
    case beamtalk_extensions:lookup(CurrentClass, Selector) of
        {ok, Fun, _Owner} ->
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
-spec super(selector(), args(), self(), state(), class_name()) -> dispatch_result().
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
-spec invoke_with_combinations(selector(), args(), self(), state(), class_name()) -> dispatch_result().
invoke_with_combinations(Selector, Args, Self, State, CurrentClass) ->
    %% TODO: Implement method combinations
    %% For now, just delegate to normal lookup
    logger:debug("Method combinations not yet implemented, using normal lookup", []),
    lookup(Selector, Args, Self, State, CurrentClass).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Look up method in class chain (recursive hierarchy walk).
%%
%% This is the core hierarchy walking function. It:
%% 1. Checks if the class exists
%% 2. Checks the class's method table for the selector
%% 3. If found, invokes the method
%% 4. If not found, recurses to the superclass
%% 5. Returns error if reached root without finding method
-spec lookup_in_class_chain(selector(), args(), self(), state(), class_name()) -> dispatch_result().
lookup_in_class_chain(Selector, Args, Self, State, ClassName) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            %% Class not found
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            {error, Error1};
        ClassPid ->
            %% Check if this class has the method
            case beamtalk_object_class:has_method(ClassPid, Selector) of
                true ->
                    %% Found the method - invoke it
                    logger:debug("Found method ~p in class ~p", [Selector, ClassName]),
                    invoke_method(ClassName, Selector, Args, Self, State);
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
                            lookup_in_class_chain(Selector, Args, Self, State, SuperclassName)
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
-spec invoke_method(class_name(), selector(), args(), self(), state()) -> dispatch_result().
invoke_method(ClassName, Selector, Args, Self, State) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            {error, Error1};
        ClassPid ->
            %% Get the module name for this class
            case beamtalk_object_class:module_name(ClassPid) of
                undefined ->
                    %% Dynamic class or error
                    Error0 = beamtalk_error:new(internal_error, ClassName),
                    Error1 = beamtalk_error:with_selector(Error0, Selector),
                    Error2 = beamtalk_error:with_hint(Error1, <<"Class has no module name">>),
                    {error, Error2};
                ModuleName ->
                    %% Compiled class - call its dispatch function
                    logger:debug("Invoking ~p:dispatch(~p, ~p, ~p, ~p)", [ModuleName, Selector, Args, Self, State]),
                    try
                        ModuleName:dispatch(Selector, Args, Self, State)
                    catch
                        error:undef ->
                            %% Module doesn't have dispatch/4 - might be dynamic
                            %% TODO: Handle dynamic classes
                            Error0 = beamtalk_error:new(internal_error, ClassName),
                            Error1 = beamtalk_error:with_selector(Error0, Selector),
                            Error2 = beamtalk_error:with_hint(Error1, <<"Module missing dispatch/4 function">>),
                            {error, Error2};
                        error:Reason:Stacktrace ->
                            %% Method threw an error
                            logger:error("Method ~p:~p threw error: ~p~nStacktrace: ~p",
                                        [ClassName, Selector, Reason, Stacktrace]),
                            %% Re-throw the error
                            erlang:raise(error, Reason, Stacktrace)
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
%%
%% TODO: Verify this is correct behavior. May need to handle actor extensions differently.
-spec invoke_extension(fun(), args(), self(), state()) -> dispatch_result().
invoke_extension(Fun, Args, _Self, State) ->
    %% Extract the receiver value from state
    %% For primitives, state might contain the value directly
    %% For actors, we need to be careful about state management
    %%
    %% TODO: This needs to handle both value types and actor types correctly
    %% For now, assume extensions operate on primitives and return values directly
    try
        Result = apply(Fun, [Args, State]),
        {reply, Result, State}
    catch
        error:Reason:Stacktrace ->
            logger:error("Extension method threw error: ~p~nStacktrace: ~p",
                        [Reason, Stacktrace]),
            erlang:raise(error, Reason, Stacktrace)
    end.
