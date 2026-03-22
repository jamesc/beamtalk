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
    responds_to/2,
    collect_before_methods/2,
    collect_after_methods/2
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
            %% Found the method - invoke it, wrapped with method combinations (BT-102).
            %% Collect before/after daemons from the class chain, then run:
            %% before (superclass->subclass), primary, after (subclass->superclass).
            invoke_with_combinations(ClassName, ClassPid, Selector, Args, Self, State, Depth);
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
                                    ?LOG_DEBUG("Erlang error in beamtalk_object_ops:dispatch", #{
                                        selector => Selector,
                                        reason => beamtalk_error:format_reason(Type, Reason),
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
                                    ?LOG_DEBUG("Erlang error in compiled dispatch", #{
                                        module => ModuleName,
                                        selector => Selector,
                                        reason => beamtalk_error:format_reason(Type, Reason),
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

%%% ============================================================================
%%% BT-102: Method Combinations (Before/After Daemons)
%%% ============================================================================

%% @private
%% @doc Invoke a method with before/after method combinations.
%%
%% Collects before/after daemons from the class chain (via the class gen_server),
%% then runs them around the primary method in ADR 0006 ordering:
%%   1. Before methods: superclass -> subclass (setup flows down)
%%   2. Primary method: most specific only
%%   3. After methods: subclass -> superclass (cleanup flows up)
%%
%% If no before/after daemons are registered, falls through directly to
%% invoke_method (zero overhead for the common case).
-spec invoke_with_combinations(
    class_name(), pid(), selector(), args(), bt_self(), state(), non_neg_integer()
) -> dispatch_result().
invoke_with_combinations(ClassName, ClassPid, Selector, Args, Self, State, Depth) ->
    %% Collect before/after from the defining class up through the chain.
    %% Start from the class where Self was instantiated (the bottom of the chain).
    OriginClass = class_name_from(Self, State, ClassName),
    BeforeFuns = collect_before_methods(Selector, OriginClass),
    AfterFuns = collect_after_methods(Selector, OriginClass),
    case {BeforeFuns, AfterFuns} of
        {[], []} ->
            %% No method combinations — fast path, no overhead.
            invoke_method(ClassName, ClassPid, Selector, Args, Self, State, Depth);
        _ ->
            %% Run before methods (superclass -> subclass order).
            %% Before funs can short-circuit by returning {short_circuit, Result, State}.
            case run_before_methods(BeforeFuns, Self, State) of
                {short_circuit, Result, NewState} ->
                    %% A before daemon short-circuited — skip primary and after methods.
                    {reply, Result, NewState};
                {ok, BeforeState} ->
                    %% Run the primary method.
                    case
                        invoke_method(ClassName, ClassPid, Selector, Args, Self, BeforeState, Depth)
                    of
                        {reply, PrimaryResult, PrimaryState} ->
                            %% Run after methods (subclass -> superclass order).
                            {FinalResult, FinalState} = run_after_methods(
                                AfterFuns, Self, PrimaryResult, PrimaryState
                            ),
                            {reply, FinalResult, FinalState};
                        ErrorOrOther ->
                            %% Primary method failed — don't run after methods.
                            ErrorOrOther
                    end
            end
    end.

%% @doc Collect before-method daemons for a selector from the entire class chain.
%%
%% Walks the superclass chain starting from ClassName, collecting before-method
%% daemons at each level. Returns them in superclass -> subclass order (ADR 0006:
%% before methods run from superclass toward subclass, so setup flows down).
%%
%% Each daemon is a fun(Self, State) -> {ok, State} | {short_circuit, Result, State}.
-spec collect_before_methods(selector(), class_name()) -> [function()].
collect_before_methods(Selector, ClassName) ->
    collect_methods_from_chain(Selector, ClassName, before, []).

%% @doc Collect after-method daemons for a selector from the entire class chain.
%%
%% Walks the superclass chain starting from ClassName, collecting after-method
%% daemons at each level. Returns them in subclass -> superclass order (ADR 0006:
%% after methods run from subclass toward superclass, so cleanup flows up).
%%
%% Each daemon is a fun(Self, Result, State) -> {ok, NewResult, State}.
-spec collect_after_methods(selector(), class_name()) -> [function()].
collect_after_methods(Selector, ClassName) ->
    lists:reverse(collect_methods_from_chain(Selector, ClassName, after_, [])).

%% @private
%% @doc Walk the class chain collecting before or after methods.
%%
%% Accumulates daemons from root (top of hierarchy) to ClassName (bottom).
%% The accumulator is built by prepending at each level, so the result
%% is in superclass -> subclass order.
%%
%% Includes a depth guard matching ?MAX_HIERARCHY_DEPTH to prevent infinite
%% recursion in case of cyclic class hierarchies.
-spec collect_methods_from_chain(selector(), class_name() | none, before | after_, [function()]) ->
    [function()].
collect_methods_from_chain(Selector, ClassName, Type, Acc) ->
    collect_methods_from_chain(Selector, ClassName, Type, Acc, 0).

-spec collect_methods_from_chain(
    selector(), class_name() | none, before | after_, [function()], non_neg_integer()
) -> [function()].
collect_methods_from_chain(_Selector, none, _Type, Acc, _Depth) ->
    Acc;
collect_methods_from_chain(_Selector, _ClassName, _Type, Acc, Depth) when
    Depth > ?MAX_HIERARCHY_DEPTH
->
    Acc;
collect_methods_from_chain(Selector, ClassName, Type, Acc, Depth) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Acc;
        ClassPid ->
            %% Get daemons for this class level.
            Daemons =
                try
                    case Type of
                        before ->
                            beamtalk_object_class:get_before_methods(ClassPid, Selector);
                        after_ ->
                            beamtalk_object_class:get_after_methods(ClassPid, Selector)
                    end
                catch
                    exit:{noproc, _} -> [];
                    exit:{timeout, _} -> []
                end,
            %% Extract just the funs (drop the Id references).
            Funs = [Fun || {_Id, Fun} <- Daemons],
            %% Walk to superclass, collecting as we go.
            %% We prepend superclass funs first, then this class's funs,
            %% so the final list is superclass -> subclass order.
            SuperclassName =
                try beamtalk_object_class:superclass(ClassPid) of
                    none -> none;
                    S -> S
                catch
                    exit:{noproc, _} -> none;
                    exit:{timeout, _} -> none
                end,
            SuperAcc = collect_methods_from_chain(
                Selector, SuperclassName, Type, Acc, Depth + 1
            ),
            SuperAcc ++ Funs
    end.

%% @private
%% @doc Run before-method daemons in order (superclass -> subclass).
%%
%% Each daemon receives (Self, State) and returns:
%%   {ok, NewState}                — continue with updated state
%%   {short_circuit, Result, State} — abort the chain and return Result
%%
%% If any daemon short-circuits, remaining daemons and the primary method are skipped.
-spec run_before_methods([function()], bt_self(), state()) ->
    {ok, state()} | {short_circuit, term(), state()}.
run_before_methods([], _Self, State) ->
    {ok, State};
run_before_methods([Fun | Rest], Self, State) ->
    try Fun(Self, State) of
        {ok, NewState} ->
            run_before_methods(Rest, Self, NewState);
        {short_circuit, Result, NewState} ->
            {short_circuit, Result, NewState}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING("Before-method daemon raised error", #{
                reason => beamtalk_error:format_reason(Class, Reason),
                stacktrace => Stack,
                domain => [beamtalk, runtime]
            }),
            %% Continue with unchanged state if a daemon fails.
            run_before_methods(Rest, Self, State)
    end.

%% @private
%% @doc Run after-method daemons in order (subclass -> superclass).
%%
%% Each daemon receives (Self, Result, State) and returns:
%%   {ok, NewResult, NewState}   — possibly transform the result
%%
%% After daemons receive the primary method's result and can transform it.
-spec run_after_methods([function()], bt_self(), term(), state()) ->
    {term(), state()}.
run_after_methods([], _Self, Result, State) ->
    {Result, State};
run_after_methods([Fun | Rest], Self, Result, State) ->
    try Fun(Self, Result, State) of
        {ok, NewResult, NewState} ->
            run_after_methods(Rest, Self, NewResult, NewState)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING("After-method daemon raised error", #{
                reason => beamtalk_error:format_reason(Class, Reason),
                stacktrace => Stack,
                domain => [beamtalk, runtime]
            }),
            %% Continue with unchanged result/state if a daemon fails.
            run_after_methods(Rest, Self, Result, State)
    end.
