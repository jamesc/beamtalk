%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Flattened method table management for Beamtalk class hierarchy.
%%%
%%% **DDD Context:** Object System
%%%
%% Manages the pre-computed flattened method tables that cache both local and
%% inherited methods for O(1) lookup. Handles building tables by walking the
%% class hierarchy and rebuilding when parent classes change.
%%%
%%% Extracted from beamtalk_object_class.erl (BT-704).
-module(beamtalk_class_hierarchy).

-include_lib("kernel/include/logger.hrl").

-export([
    build_flattened_methods/3,
    build_flattened_methods/4,
    rebuild_all_flattened_tables/5
]).

-type class_name() :: atom().
-type selector() :: atom().
-type method_info() :: #{
    arity => non_neg_integer(),
    block => fun(),
    is_sealed => boolean()
}.

%% @doc Rebuild both instance and class flattened method tables.
%%
%% Returns {NewFlattened, NewFlattenedClass} tuple.
-spec rebuild_all_flattened_tables(
    class_name(), class_name() | none,
    #{selector() => method_info()},
    #{selector() => method_info()},
    #{selector() => method_info()}
) -> {#{selector() => {class_name(), method_info()}}, #{selector() => {class_name(), method_info()}}}.
rebuild_all_flattened_tables(Name, Superclass, InstanceMethods, ClassMethods, _OldFlattened) ->
    NewFlattened = build_flattened_methods(Name, Superclass, InstanceMethods),
    NewFlattenedClass = build_flattened_methods(Name, Superclass, ClassMethods, get_flattened_class_methods),
    {NewFlattened, NewFlattenedClass}.

%% @doc Build flattened method table by walking hierarchy and merging methods.
%%
%% ADR 0006 Phase 2: Pre-compute all methods (local + inherited) at class
%% registration time for O(1) lookup. Child methods override parent methods.
%%
%% Returns a map of {Selector => {DefiningClass, MethodInfo}} where DefiningClass
%% is the class that actually defines the method.
%%
%% ## Algorithm
%%
%% 1. Start with empty accumulator
%% 2. Walk from current class up to root (none)
%% 3. At each level, add methods from that class (if not already present)
%% 4. Child methods naturally override parent (first-seen wins)
-spec build_flattened_methods(class_name(), class_name() | none, #{selector() => method_info()}) ->
    #{selector() => {class_name(), method_info()}}.
build_flattened_methods(CurrentClass, Superclass, LocalMethods) ->
    build_flattened_methods(CurrentClass, Superclass, LocalMethods, get_flattened_methods).

%% @doc Build flattened method table with configurable superclass query message.
%% Used for both instance methods (get_flattened_methods) and class methods
%% (get_flattened_class_methods).
-spec build_flattened_methods(class_name(), class_name() | none, #{selector() => method_info()}, atom()) ->
    #{selector() => {class_name(), method_info()}}.
build_flattened_methods(CurrentClass, Superclass, LocalMethods, QueryMsg) ->
    %% Start with local methods (maps each selector to {CurrentClass, MethodInfo})
    LocalFlattened = maps:fold(fun(Selector, MethodInfo, Acc) ->
        maps:put(Selector, {CurrentClass, MethodInfo}, Acc)
    end, #{}, LocalMethods),
    
    %% Walk up the hierarchy and merge inherited methods
    case Superclass of
        none ->
            %% At root - return local methods
            LocalFlattened;
        SuperclassName ->
            %% Get inherited methods from superclass
            case beamtalk_class_registry:whereis_class(SuperclassName) of
                undefined ->
                    %% BT-510: Superclass not registered yet (out-of-order loading).
                    %% Return local methods only; invalidate_subclass_flattened_tables
                    %% will trigger a rebuild once the superclass registers.
                    ?LOG_DEBUG("Superclass unavailable during init, flattened methods incomplete",
                                 #{class => CurrentClass, superclass => SuperclassName}),
                    LocalFlattened;
                SuperclassPid ->
                    SuperclassFlattenedMethods = try gen_server:call(SuperclassPid, QueryMsg, 5000) of
                        {ok, Methods} -> Methods;
                        _ -> #{}
                    catch
                        _:_ -> #{}
                    end,
                    
                    %% Merge: local methods override inherited
                    maps:merge(SuperclassFlattenedMethods, LocalFlattened)
            end
    end.
