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
    classClass/1
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
    lists:filtermap(fun(SC) ->
        case atom_to_class_object(SC) of
            nil -> false;
            Obj -> {true, Obj}
        end
    end, Subclasses).

%% @doc Return all subclasses transitively (breadth-first) as class objects.
%%
%% Queries the ETS hierarchy table recursively.
-spec classAllSubclasses(#beamtalk_object{}) -> [#beamtalk_object{}].
classAllSubclasses(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    AllSubclasses = beamtalk_class_registry:all_subclasses(ClassName),
    lists:filtermap(fun(SC) ->
        case atom_to_class_object(SC) of
            nil -> false;
            Obj -> {true, Obj}
        end
    end, AllSubclasses).

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
    collect_superclasses(SuperName, []).

%% @doc Return all method selectors understood by instances (full inheritance chain).
-spec classMethods(#beamtalk_object{}) -> [atom()].
classMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    collect_all_methods(ClassName, ordsets:new()).

%% @doc Test whether instances understand the selector (full inheritance chain).
-spec classCanUnderstand(#beamtalk_object{}, atom()) -> boolean().
classCanUnderstand(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    can_understand_check(ClassName, Selector).

%% @doc Test whether the receiver strictly inherits from aClass (self not included).
-spec classInheritsFrom(#beamtalk_object{}, #beamtalk_object{}) -> boolean().
classInheritsFrom(Self, TargetClassObj) ->
    TargetPid = erlang:element(4, TargetClassObj),
    TargetName = gen_server:call(TargetPid, class_name),
    ClassPid = erlang:element(4, Self),
    SuperName = gen_server:call(ClassPid, superclass),
    inherits_from_check(SuperName, TargetName).

%% @doc Test whether aBehaviour is the receiver or one of its ancestors.
-spec classIncludesBehaviour(#beamtalk_object{}, #beamtalk_object{}) -> boolean().
classIncludesBehaviour(Self, TargetClassObj) ->
    SelfPid = erlang:element(4, Self),
    SelfName = gen_server:call(SelfPid, class_name),
    TargetPid = erlang:element(4, TargetClassObj),
    TargetName = gen_server:call(TargetPid, class_name),
    includes_behaviour_check(SelfName, TargetName).

%% @doc Walk the hierarchy and return the class object that defines the selector, or nil.
-spec classWhichIncludesSelector(#beamtalk_object{}, atom()) -> #beamtalk_object{} | nil.
classWhichIncludesSelector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    which_includes_check(ClassName, Selector).

%% @doc Return all instance variable names including inherited, in slot order.
-spec classAllInstVarNames(#beamtalk_object{}) -> [atom()].
classAllInstVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    collect_all_inst_vars(ClassName, []).

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

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

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

%% @private
collect_superclasses(none, Acc) ->
    lists:reverse(Acc);
collect_superclasses(ClassName, Acc) ->
    case atom_to_class_object(ClassName) of
        nil -> lists:reverse(Acc);
        ClassObj ->
            ClassPid = erlang:element(4, ClassObj),
            Super = gen_server:call(ClassPid, superclass),
            collect_superclasses(Super, [ClassObj | Acc])
    end.

%% @private
collect_all_methods(none, Acc) ->
    ordsets:to_list(Acc);
collect_all_methods(ClassName, Acc) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> ordsets:to_list(Acc);
        ClassPid ->
            Methods = gen_server:call(ClassPid, methods),
            NewAcc = ordsets:union(Acc, ordsets:from_list(Methods)),
            Super = gen_server:call(ClassPid, superclass),
            collect_all_methods(Super, NewAcc)
    end.

%% @private
can_understand_check(none, _Selector) -> false;
can_understand_check(ClassName, Selector) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> false;
        ClassPid ->
            case beamtalk_object_class:has_method(ClassPid, Selector) of
                true -> true;
                false ->
                    Super = gen_server:call(ClassPid, superclass),
                    can_understand_check(Super, Selector)
            end
    end.

%% @private
inherits_from_check(none, _Target) -> false;
inherits_from_check(Target, Target) -> true;
inherits_from_check(ClassName, Target) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> false;
        ClassPid ->
            Super = gen_server:call(ClassPid, superclass),
            inherits_from_check(Super, Target)
    end.

%% @private
includes_behaviour_check(Name, Name) -> true;
includes_behaviour_check(Name, Target) ->
    case beamtalk_class_registry:whereis_class(Name) of
        undefined -> false;
        ClassPid ->
            Super = gen_server:call(ClassPid, superclass),
            includes_behaviour_check_super(Super, Target)
    end.

includes_behaviour_check_super(none, _Target) -> false;
includes_behaviour_check_super(Name, Target) ->
    includes_behaviour_check(Name, Target).

%% @private
which_includes_check(none, _Selector) -> nil;
which_includes_check(ClassName, Selector) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> nil;
        ClassPid ->
            case beamtalk_object_class:has_method(ClassPid, Selector) of
                true -> atom_to_class_object(ClassName);
                false ->
                    Super = gen_server:call(ClassPid, superclass),
                    which_includes_check(Super, Selector)
            end
    end.

%% @private
collect_all_inst_vars(none, Acc) ->
    Acc;
collect_all_inst_vars(ClassName, Acc) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> Acc;
        ClassPid ->
            IVars = gen_server:call(ClassPid, instance_variables),
            Super = gen_server:call(ClassPid, superclass),
            collect_all_inst_vars(Super, IVars ++ Acc)
    end.
