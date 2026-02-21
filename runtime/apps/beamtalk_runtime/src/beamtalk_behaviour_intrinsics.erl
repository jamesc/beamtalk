%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Thin Behaviour/Class intrinsics (ADR 0032 Phase 2).
%%%
%% **DDD Context:** Object System
%%%
%%% These functions back the `@primitive "classXxx"` declarations in `lib/Behaviour.bt`
%%% and `lib/Class.bt`. Each function receives a class object `Self`
%%% (a `#beamtalk_object{}` tuple with a ClassPid at position 4) and exposes
%%% either raw data from the class gen_server state / registry or small,
%%% hierarchy-aware queries over that data.
%%%
%%% Richer hierarchy-walking and protocol logic still lives in Beamtalk-level
%%% code; this module provides a minimal, side-effect-free intrinsic surface
%%% that the Behaviour/Class libraries can rely on.
%%%
%%% ## Intrinsic Table
%%%
%%% | Erlang function             | Backing data source / derivation                          |
%%% |-----------------------------|------------------------------------------------------------|
%%% | classSuperclass/1           | Direct superclass from class gen_server state             |
%%% | classAllSuperclasses/1      | Recursively walks the superclass chain from classSuperclass/1 |
%%% | classSubclasses/1           | Direct subclasses from class registry                     |
%%% | classAllSubclasses/1        | All subclasses from class registry                        |
%%% | classLocalMethods/1         | Local method dictionary from class gen_server state       |
%%% | classMethods/1              | Combined local + inherited methods via superclass chain   |
%%% | classIncludesSelector/2     | Membership check in local method dictionary               |
%%% | classCanUnderstand/2        | Selector lookup against full method dictionary (classMethods/1) |
%%% | classInheritsFrom/2         | Predicate over the superclass chain (classAllSuperclasses/1) |
%%% | classIncludesBehaviour/2    | Behaviour / interface membership via superclass chain     |
%%% | classWhichIncludesSelector/2| First class in hierarchy whose local methods include selector |
%%% | classInstVarNames/1         | Instance variable names from class gen_server state       |
%%% | classAllInstVarNames/1      | Combined instance variable names via superclass chain     |
%%% | className/1                 | Class name from class gen_server state                    |
%%% | classClass/1                | Virtual metaclass sentinel                                |
%%% | classDoc/1                  | Class doc string from class gen_server state (ADR 0033)   |
%%% | classSetDoc/2               | Set class doc string (ADR 0033)                           |
%%% | classSetMethodDoc/3         | Set method doc string for a selector (ADR 0033)           |

-module(beamtalk_behaviour_intrinsics).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    classSuperclass/1,
    classAllSuperclasses/1,
    classSubclasses/1,
    classAllSubclasses/1,
    classLocalMethods/1,
    classMethods/1,
    classIncludesSelector/2,
    classCanUnderstand/2,
    classInheritsFrom/2,
    classIncludesBehaviour/2,
    classWhichIncludesSelector/2,
    classInstVarNames/1,
    classAllInstVarNames/1,
    className/1,
    classClass/1,
    %% ADR 0033: Runtime-embedded documentation
    classDoc/1,
    classSetDoc/2,
    classSetMethodDoc/3
]).

%%% ============================================================================
%%% Public Intrinsics
%%% ============================================================================

%% @doc Return the superclass of the receiver as a class object, or nil for roots.
%%
%% ADR 0032: Returns a proper #beamtalk_object{} instead of a bare atom,
%% fixing the inconsistency where `Counter class` returned an object but
%% `Counter superclass` returned an atom.
-spec classSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | nil.
classSuperclass(Self) ->
    ClassPid = erlang:element(4, Self),
    case gen_server:call(ClassPid, superclass) of
        none -> nil;
        SuperclassName -> atom_to_class_object(SuperclassName)
    end.

%% @doc Return direct subclasses of the receiver as a list of class objects.
%%
%% Queries the ETS hierarchy table for O(1) lookup per level.
-spec classSubclasses(#beamtalk_object{}) -> [#beamtalk_object{}].
classSubclasses(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    Subclasses = beamtalk_class_registry:direct_subclasses(ClassName),
    lists:filtermap(
        fun(SC) ->
            case atom_to_class_object(SC) of
                nil -> false;
                Obj -> {true, Obj}
            end
        end,
        Subclasses
    ).

%% @doc Return all subclasses transitively (breadth-first) as class objects.
%%
%% Queries the ETS hierarchy table recursively.
-spec classAllSubclasses(#beamtalk_object{}) -> [#beamtalk_object{}].
classAllSubclasses(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    AllSubclasses = beamtalk_class_registry:all_subclasses(ClassName),
    lists:filtermap(
        fun(SC) ->
            case atom_to_class_object(SC) of
                nil -> false;
                Obj -> {true, Obj}
            end
        end,
        AllSubclasses
    ).

%% @doc Return the local method selectors of the receiver (non-inherited).
%%
%% Returns only methods defined directly in this class, not inherited ones.
%% Full chain walk for all methods is implemented in Behaviour.methods (pure Beamtalk).
-spec classLocalMethods(#beamtalk_object{}) -> [atom()].
classLocalMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, methods).

%% @doc Return all superclasses of the receiver in order (immediate parent to root).
-spec classAllSuperclasses(#beamtalk_object{}) -> [#beamtalk_object{}].
classAllSuperclasses(Self) ->
    ClassPid = erlang:element(4, Self),
    SuperName = gen_server:call(ClassPid, superclass),
    Supers = walk_hierarchy(
        SuperName,
        fun(CN, CPid, Acc) ->
            Module = gen_server:call(CPid, module_name),
            Tag = beamtalk_class_registry:class_object_tag(CN),
            ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = CPid},
            {cont, [ClassObj | Acc]}
        end,
        []
    ),
    lists:reverse(Supers).

%% @doc Return all method selectors understood by instances (full inheritance chain).
-spec classMethods(#beamtalk_object{}) -> [atom()].
classMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            Methods = gen_server:call(CPid, methods),
            {cont, ordsets:union(A, ordsets:from_list(Methods))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc).

%% @doc Test whether instances understand the selector (full inheritance chain).
-spec classCanUnderstand(#beamtalk_object{}, atom()) -> boolean().
classCanUnderstand(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, _Acc) ->
            case beamtalk_object_class:has_method(CPid, Selector) of
                true -> {halt, true};
                false -> {cont, false}
            end
        end,
        false
    ).

%% @doc Test whether the receiver strictly inherits from aClass (self not included).
-spec classInheritsFrom(#beamtalk_object{}, #beamtalk_object{}) -> boolean().
classInheritsFrom(Self, TargetClassObj) ->
    TargetPid = erlang:element(4, TargetClassObj),
    TargetName = gen_server:call(TargetPid, class_name),
    ClassPid = erlang:element(4, Self),
    SuperName = gen_server:call(ClassPid, superclass),
    case SuperName of
        TargetName ->
            true;
        _ ->
            walk_hierarchy(
                SuperName,
                fun(CN, _CPid, _Acc) ->
                    case CN of
                        TargetName -> {halt, true};
                        _ -> {cont, false}
                    end
                end,
                false
            )
    end.

%% @doc Test whether aBehaviour is the receiver or one of its ancestors.
-spec classIncludesBehaviour(#beamtalk_object{}, #beamtalk_object{}) -> boolean().
classIncludesBehaviour(Self, TargetClassObj) ->
    SelfPid = erlang:element(4, Self),
    SelfName = gen_server:call(SelfPid, class_name),
    TargetPid = erlang:element(4, TargetClassObj),
    TargetName = gen_server:call(TargetPid, class_name),
    case SelfName of
        TargetName ->
            true;
        _ ->
            walk_hierarchy(
                SelfName,
                fun(CN, _CPid, _Acc) ->
                    case CN of
                        TargetName -> {halt, true};
                        _ -> {cont, false}
                    end
                end,
                false
            )
    end.

%% @doc Walk the hierarchy and return the class object that defines the selector, or nil.
-spec classWhichIncludesSelector(#beamtalk_object{}, atom()) -> #beamtalk_object{} | nil.
classWhichIncludesSelector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    walk_hierarchy(
        ClassName,
        fun(CN, CPid, _Acc) ->
            case beamtalk_object_class:has_method(CPid, Selector) of
                true ->
                    Module = gen_server:call(CPid, module_name),
                    Tag = beamtalk_class_registry:class_object_tag(CN),
                    {halt, #beamtalk_object{class = Tag, class_mod = Module, pid = CPid}};
                false ->
                    {cont, nil}
            end
        end,
        nil
    ).

%% @doc Return all instance variable names including inherited, in slot order.
-spec classAllInstVarNames(#beamtalk_object{}) -> [atom()].
classAllInstVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            IVars = gen_server:call(CPid, instance_variables),
            {cont, IVars ++ Acc}
        end,
        []
    ).

%% @doc Test whether the selector is defined locally in this class.
%%
%% Does NOT check superclasses — local containment only.
%% Full chain walk for canUnderstand: is implemented via classCanUnderstand.
-spec classIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
classIncludesSelector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    LocalMethods = gen_server:call(ClassPid, methods),
    lists:member(Selector, LocalMethods).

%% @doc Return the names of instance variables declared in this class (not inherited).
-spec classInstVarNames(#beamtalk_object{}) -> [atom()].
classInstVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, instance_variables).

%% @doc Return the name of the class as a Symbol (atom).
-spec className(#beamtalk_object{}) -> atom().
className(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, class_name).

%% @doc Return the virtual metaclass sentinel.
%%
%% ADR 0013: Virtual metaclasses — full metaclass tower is future work.
%% Returns 'Metaclass' as the sentinel atom for now.
-spec classClass(#beamtalk_object{}) -> atom().
classClass(_Self) ->
    'Metaclass'.

%% @doc Return the class documentation string, or nil if none set.
%%
%% ADR 0033: Runtime-embedded documentation.
%% The class gen_server stores `none` internally; we return `nil` for Beamtalk.
-spec classDoc(#beamtalk_object{}) -> binary() | nil.
classDoc(Self) ->
    ClassPid = erlang:element(4, Self),
    case gen_server:call(ClassPid, get_doc) of
        none -> nil;
        Doc -> Doc
    end.

%% @doc Set the class documentation string.
%%
%% ADR 0033: Post-hoc setter for class-level doc.
-spec classSetDoc(#beamtalk_object{}, binary()) -> #beamtalk_object{}.
classSetDoc(Self, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = gen_server:call(ClassPid, {set_doc, DocBinary}),
    Self.

%% @doc Set the documentation string for a specific method selector.
%%
%% ADR 0033: Post-hoc setter for method-level doc.
-spec classSetMethodDoc(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classSetMethodDoc(Self, Selector, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = gen_server:call(ClassPid, {set_method_doc, Selector, DocBinary}),
    Self.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

%% @private
%% @doc Generic fold over the superclass chain starting from ClassName.
%%
%% Fun receives (ClassName, ClassPid, Acc) and returns:
%%   {cont, NewAcc}   — continue walking to superclass
%%   {halt, Result}   — stop and return Result immediately
%%
%% Returns Acc when the chain is exhausted (none or unregistered class).
-spec walk_hierarchy(atom() | none, fun((atom(), pid(), Acc) -> {cont, Acc} | {halt, Result}), Acc) ->
    Acc | Result.
walk_hierarchy(none, _Fun, Acc) ->
    Acc;
walk_hierarchy(ClassName, Fun, Acc) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Acc;
        ClassPid ->
            case Fun(ClassName, ClassPid, Acc) of
                {halt, Result} ->
                    Result;
                {cont, NewAcc} ->
                    Super = gen_server:call(ClassPid, superclass),
                    walk_hierarchy(Super, Fun, NewAcc)
            end
    end.

%% @private
%% @doc Convert a class name atom to a class object (#beamtalk_object{}).
%%
%% Looks up the class process, gets its module name, and constructs
%% the class object tuple. Returns nil if the class is not registered
%% (safe during bootstrap window).
-spec atom_to_class_object(atom()) -> #beamtalk_object{} | nil.
atom_to_class_object(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            ?LOG_DEBUG("atom_to_class_object: class ~p not registered", [ClassName]),
            nil;
        ClassPid ->
            Module = gen_server:call(ClassPid, module_name),
            Tag = beamtalk_class_registry:class_object_tag(ClassName),
            #beamtalk_object{class = Tag, class_mod = Module, pid = ClassPid}
    end.
