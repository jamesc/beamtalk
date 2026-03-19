%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Method dispatch domain service (DDD).
%%%
%%% **DDD Context:** Object System Context
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
    %% Step 1: Check extension registry first (guard against missing ETS table)
    case check_extension(CurrentClass, Selector) of
        {ok, Fun} ->
            %% Found extension method - invoke it
            ?LOG_DEBUG("Found extension method", #{
                selector => Selector, class => CurrentClass, domain => [beamtalk, runtime]
            }),
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
%% ADR 0032 Phase 3: Delegates to beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2,
%% the single canonical hierarchy-walk implementation, removing the duplicate
%% responds_to_chain walk that previously lived here.
%%
%% Used by Object's `respondsTo:` implementation to check the full hierarchy.
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
    ?LOG_WARNING("Max hierarchy depth exceeded — possible cycle", #{
        max_depth => ?MAX_HIERARCHY_DEPTH,
        class => ClassName,
        selector => Selector,
        domain => [beamtalk, runtime]
    }),
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
            invoke_method(ClassName, ClassPid, Selector, Args, Self, State, Depth);
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
                    {error, Error};
                SuperclassName ->
                    %% Recurse to superclass
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
                    %% Intercept displayString/inspect for actor instances to avoid
                    %% deadlock. Both compiled Object methods send a message back to
                    %% Self (displayString calls self printString, inspect delegates
                    %% to self printString), which produces a second gen_server:call
                    %% on the same actor process → deadlock.
                    %% beamtalk_object_ops handles these without any self-sends.
                    %% Note: printString does NOT deadlock (uses class_of_object → metaclass).
                    case
                        is_actor_instance(Self) andalso
                            (Selector =:= 'displayString' orelse Selector =:= inspect)
                    of
                        true ->
                            %% beamtalk_object_ops:dispatch is known-safe, but wrap it
                            %% with the same normalization as the slow path so callers
                            %% always receive a canonical {reply,_,_} | {error,_} tuple.
                            try beamtalk_object_ops:dispatch(Selector, Args, Self, State) of
                                {reply, _, _} = Reply -> Reply;
                                {error, Error, _State} -> {error, Error}
                            catch
                                Type:Reason:Stack ->
                                    ?LOG_ERROR("Erlang error in beamtalk_object_ops:dispatch", #{
                                        error_class => Type,
                                        reason => Reason,
                                        selector => Selector,
                                        stacktrace => Stack,
                                        domain => [beamtalk, runtime]
                                    }),
                                    Wrapped = beamtalk_exception_handler:ensure_wrapped(
                                        Type, Reason, Stack
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
                                    ?LOG_ERROR("Erlang error in compiled dispatch", #{
                                        error_class => Type,
                                        module => ModuleName,
                                        selector => Selector,
                                        reason => Reason,
                                        stacktrace => Stack,
                                        domain => [beamtalk, runtime]
                                    }),
                                    Wrapped = beamtalk_exception_handler:ensure_wrapped(
                                        Type, Reason, Stack
                                    ),
                                    #{error := BtError} = Wrapped,
                                    {error, BtError}
                            end
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
            %% BT-753: Derive class from Self when State is empty (class objects).
            ClassName = class_name_from(Self, State, unknown),
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
%% Supports two signatures based on the target class type:
%%
%% - Actor extensions: fun(Args, Self, State) -> {Result, NewState}
%%   State mutations are threaded back to the gen_server.
%% - Value-type extensions: fun(Args, Self) -> Result
%%   No state threading (value types have no mutable state).
%%
%% BT-1512: The arity is checked at call time to support both signatures
%% from a single dispatch path.
-spec invoke_extension(fun(), args(), bt_self(), state()) -> dispatch_result().
invoke_extension(Fun, Args, Self, State) ->
    try
        {arity, Arity} = erlang:fun_info(Fun, arity),
        case Arity of
            3 ->
                %% Actor extension: fun(Args, Self, State) -> {Result, NewState}
                {Result, NewState} = apply(Fun, [Args, Self, State]),
                {reply, Result, NewState};
            2 ->
                %% Value-type extension: fun(Args, Self) -> Result
                Result = apply(Fun, [Args, Self]),
                {reply, Result, State};
            _ ->
                error({bad_extension_arity, Arity})
        end
    catch
        error:Reason:Stacktrace ->
            ?LOG_ERROR("Extension method threw error", #{
                reason => Reason,
                stacktrace => Stacktrace,
                domain => [beamtalk, runtime]
            }),
            erlang:raise(error, Reason, Stacktrace)
    end.

%% @private
%% @doc Extract class name from Self or State, with a default fallback.
%%
%% Delegates to beamtalk_object_ops:class_name/3 to avoid duplication.
-spec class_name_from(bt_self(), state(), atom()) -> atom().
class_name_from(Self, State, Default) ->
    beamtalk_object_ops:class_name(Self, State, Default).

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
%% @doc Return true if Self is an actor instance (a #beamtalk_object{} with a pid).
%%
%% Used to detect the deadlock-prone case where displayString/printString are sent
%% to an actor while inside the actor's gen_server callback. In that case the
%% compiled bt@stdlib@object:displayString/1 would call
%% beamtalk_message_dispatch:send(Self, 'printString', []), causing a second
%% gen_server:call on the same process → deadlock. beamtalk_object_ops handles
%% these methods safely without any self-sends.
-spec is_actor_instance(term()) -> boolean().
is_actor_instance(#beamtalk_object{pid = Pid}) when is_pid(Pid) -> true;
is_actor_instance(_) -> false.
