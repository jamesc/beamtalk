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
%%% | classFieldNames/1           | Field names from class gen_server state                   |
%%% | classAllFieldNames/1        | Combined field names via superclass chain                 |
%%% | className/1                 | Class name from class gen_server state                    |
%%% | classClass/1                | Real metaclass object (ADR 0036)                          |
%%% | classDoc/1                  | Class doc string from class gen_server state (ADR 0033)   |
%%% | classSetDoc/2               | Set class doc string (ADR 0033)                           |
%%% | classSetMethodDoc/3         | Set method doc string for a selector (ADR 0033)           |
%%% | classRemoveFromSystem/1     | Remove class and cleanup runtime state                    |

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
    classCanUnderstandFromName/2,
    classInheritsFrom/2,
    classIncludesBehaviour/2,
    classWhichIncludesSelector/2,
    classFieldNames/1,
    classAllFieldNames/1,
    className/1,
    classClass/1,
    %% ADR 0036: Metaclass primitives
    metaclassThisClass/1,
    metaclassSuperclass/1,
    metaclassClassMethods/1,
    metaclassLocalClassMethods/1,
    metaclassIncludesSelector/2,
    metaclassNew/0,
    %% ADR 0033: Runtime-embedded documentation
    classDoc/1,
    classSetDoc/2,
    classSetMethodDoc/3,
    %% BT-785: Class removal
    classRemoveFromSystem/1
]).

%%% ============================================================================
%%% Public Intrinsics
%%% ============================================================================

%% @doc Return the superclass of the receiver as a class object, or nil for roots.
%%
%% ADR 0032: Returns a proper #beamtalk_object{} instead of a bare atom,
%% fixing the inconsistency where `Counter class` returned an object but
%% `Counter superclass` returned an atom.
-spec classSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
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

%% @doc Test whether the class named ClassName has instances that understand Selector.
%%
%% ADR 0032 Phase 3: Canonical single-source hierarchy walk used by
%% beamtalk_dispatch:responds_to/2. Takes ClassName directly (not a class object)
%% to avoid the extra gen_server:call that classCanUnderstand/2 needs to re-fetch
%% the class name from the pid.
-spec classCanUnderstandFromName(atom(), atom()) -> boolean().
classCanUnderstandFromName(ClassName, Selector) ->
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
-spec classWhichIncludesSelector(#beamtalk_object{}, atom()) -> #beamtalk_object{} | 'nil'.
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

%% @doc Return all field names including inherited, in slot order.
-spec classAllFieldNames(#beamtalk_object{}) -> [atom()].
classAllFieldNames(Self) ->
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

%% @doc Return the names of fields declared in this class (not inherited).
-spec classFieldNames(#beamtalk_object{}) -> [atom()].
classFieldNames(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, instance_variables).

%% @doc Return the name of the class as a Symbol (atom).
-spec className(#beamtalk_object{}) -> atom().
className(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, class_name).

%% @doc Return the metaclass object for the receiver.
%%
%% ADR 0036: Replaces the sentinel atom with a real `#beamtalk_object{}`.
%% Wraps the same class pid but dispatches through the 'Metaclass' chain.
%% No new gen_server process — virtual tag approach from ADR 0013 continues.
%%
%% Idempotent: when called on a `class='Metaclass'`-tagged object (i.e.,
%% `Metaclass class class`), extracts pid and returns a new structurally
%% identical record. This enables `Metaclass class class == Metaclass class`
%% (Erlang structural `==` compares all three fields: class, class_mod, pid).
-spec classClass(#beamtalk_object{}) -> #beamtalk_object{}.
classClass(Self) ->
    Pid = erlang:element(4, Self),
    #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid}.

%% @doc Return the class documentation string, or nil if none set.
%%
%% ADR 0033: Runtime-embedded documentation.
%% The class gen_server stores `none` internally; we return `nil` for Beamtalk.
-spec classDoc(#beamtalk_object{}) -> binary() | 'nil'.
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

%% @doc Remove this class from the system, performing full cleanup.
%%
%% BT-785: Implements `removeFromSystem` for class objects (Smalltalk convention).
%%
%% Safety checks (raises errors for):
%%   - Stdlib classes (module name starts with `bt@stdlib@`)
%%   - Classes with direct subclasses (must remove children first)
%%
%% Cleanup sequence:
%%   1. Stop all live actors of this class (via beamtalk_actor_registry)
%%   2. Stop the class gen_server (terminate/2 removes ETS entry and pg group)
%%   3. Purge the BEAM module (code:soft_purge + code:delete)
-spec classRemoveFromSystem(#beamtalk_object{}) -> 'nil'.
classRemoveFromSystem(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    Module = gen_server:call(ClassPid, module_name),
    %% Safety: refuse to remove stdlib classes
    case is_stdlib_module_name(Module) of
        true ->
            Error0 = beamtalk_error:new(runtime_error, ClassName),
            Error1 = beamtalk_error:with_message(
                Error0,
                iolist_to_binary([
                    <<"Cannot remove stdlib class '">>,
                    atom_to_binary(ClassName, utf8),
                    <<"'">>
                ])
            ),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Stdlib classes are protected and cannot be removed.">>
            ),
            beamtalk_error:raise(Error2);
        false ->
            %% Safety: refuse if class has direct subclasses
            case beamtalk_class_registry:direct_subclasses(ClassName) of
                [] ->
                    %% Stop live actors of this class
                    stop_class_actors(ClassName),
                    %% Stop the class gen_server
                    %% (terminate/2 in beamtalk_object_class removes ETS entry and pg group)
                    gen_server:stop(ClassPid),
                    %% Purge the BEAM module
                    _ = code:soft_purge(Module),
                    _ = code:delete(Module),
                    nil;
                Subclasses ->
                    NameBins = [atom_to_binary(S, utf8) || S <- Subclasses],
                    NamesStr = iolist_to_binary(lists:join(<<", ">>, NameBins)),
                    Error0 = beamtalk_error:new(runtime_error, ClassName),
                    Error1 = beamtalk_error:with_message(
                        Error0,
                        iolist_to_binary([
                            <<"Cannot remove class '">>,
                            atom_to_binary(ClassName, utf8),
                            <<"' — it has subclasses">>
                        ])
                    ),
                    Error2 = beamtalk_error:with_hint(
                        Error1,
                        iolist_to_binary([<<"Remove subclasses first: ">>, NamesStr])
                    ),
                    beamtalk_error:raise(Error2)
            end
    end.

%%% ============================================================================
%%% Metaclass Primitives (ADR 0036 Phase 1)
%%% ============================================================================

%% @doc Return the class this metaclass describes.
%%
%% ADR 0036: Backs `@primitive "metaclassThisClass"` in Metaclass.bt.
%% A metaclass object carries the class pid; we retrieve its name and return
%% the class object. Example: `Counter class class thisClass == Counter`.
-spec metaclassThisClass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassThisClass(Self) ->
    Pid = erlang:element(4, Self),
    ClassName = gen_server:call(Pid, class_name),
    atom_to_class_object(ClassName).

%% @doc Return the superclass of the metaclass parallel hierarchy.
%%
%% ADR 0036: Backs `@primitive "metaclassSuperclass"` in Metaclass.bt.
%% The superclass of Counter's metaclass is the metaclass of Counter's superclass.
%% Example: `Counter class superclass == Actor class`.
-spec metaclassSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassSuperclass(Self) ->
    Pid = erlang:element(4, Self),
    case gen_server:call(Pid, superclass) of
        none ->
            nil;
        SuperName ->
            case atom_to_class_object(SuperName) of
                nil -> nil;
                SuperClassObj -> classClass(SuperClassObj)
            end
    end.

%% @doc Return all class-side method selectors (full inheritance chain).
%%
%% ADR 0036: Backs `@primitive "metaclassClassMethods"` in Metaclass.bt.
%% Walks the superclass chain collecting all class-side selectors.
-spec metaclassClassMethods(#beamtalk_object{}) -> [atom()].
metaclassClassMethods(Self) ->
    Pid = erlang:element(4, Self),
    ClassName = gen_server:call(Pid, class_name),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            ClassMethods = gen_server:call(CPid, get_local_class_methods),
            Selectors = maps:keys(ClassMethods),
            {cont, ordsets:union(A, ordsets:from_list(Selectors))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc).

%% @doc Return local class-side method selectors (non-inherited).
%%
%% ADR 0036: Backs `@primitive "metaclassLocalClassMethods"` in Metaclass.bt.
%% Returns only class methods defined directly on this class.
-spec metaclassLocalClassMethods(#beamtalk_object{}) -> [atom()].
metaclassLocalClassMethods(Self) ->
    Pid = erlang:element(4, Self),
    ClassMethods = gen_server:call(Pid, get_local_class_methods),
    maps:keys(ClassMethods).

%% @doc Test whether the selector is defined as a class-side method.
%%
%% ADR 0036: Backs `@primitive "metaclassIncludesSelector"` in Metaclass.bt.
%% Does NOT check superclasses — local containment only.
-spec metaclassIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
metaclassIncludesSelector(Self, Selector) ->
    Pid = erlang:element(4, Self),
    ClassMethods = gen_server:call(Pid, get_local_class_methods),
    maps:is_key(Selector, ClassMethods).

%% @doc Guard for direct Metaclass instantiation — backs `class sealed new`.
%%
%% ADR 0036: Backs `@primitive "metaclassNew"` in Metaclass.bt.
%% Called from the generated `new/0` constructor of Metaclass.
%% Always raises a user_error; metaclasses must be obtained via `x class class`.
-spec metaclassNew() -> no_return().
metaclassNew() ->
    Error = beamtalk_error:new(
        user_error, 'Metaclass', 'new', <<"Use x class class to obtain a metaclass">>
    ),
    beamtalk_error:raise(Error).

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
%% Guards against cycles via ?MAX_HIERARCHY_DEPTH.
-spec walk_hierarchy(atom() | none, fun((atom(), pid(), Acc) -> {cont, Acc} | {halt, Result}), Acc) ->
    Acc | Result.
walk_hierarchy(ClassName, Fun, Acc) ->
    walk_hierarchy(ClassName, Fun, Acc, 0).

-spec walk_hierarchy(
    atom() | none,
    fun((atom(), pid(), Acc) -> {cont, Acc} | {halt, Result}),
    Acc,
    non_neg_integer()
) -> Acc | Result.
walk_hierarchy(none, _Fun, Acc, _Depth) ->
    Acc;
walk_hierarchy(_ClassName, _Fun, Acc, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    ?LOG_WARNING(
        "walk_hierarchy: max hierarchy depth ~p exceeded — possible cycle",
        [?MAX_HIERARCHY_DEPTH]
    ),
    Acc;
walk_hierarchy(ClassName, Fun, Acc, Depth) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Acc;
        ClassPid ->
            case Fun(ClassName, ClassPid, Acc) of
                {halt, Result} ->
                    Result;
                {cont, NewAcc} ->
                    Super = gen_server:call(ClassPid, superclass),
                    walk_hierarchy(Super, Fun, NewAcc, Depth + 1)
            end
    end.

%% @private
%% @doc Convert a class name atom to a class object (#beamtalk_object{}).
%%
%% Looks up the class process, gets its module name, and constructs
%% the class object tuple. Returns nil if the class is not registered
%% (safe during bootstrap window).
-spec atom_to_class_object(atom()) -> #beamtalk_object{} | 'nil'.
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
%% @doc Check if a module name belongs to the Beamtalk stdlib.
%% BT-785: Stdlib modules have the prefix `bt@stdlib@`.
-spec is_stdlib_module_name(atom()) -> boolean().
is_stdlib_module_name(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@stdlib@", _/binary>> -> true;
        _ -> false
    end;
is_stdlib_module_name(_) ->
    false.

%% @private
%% @doc Stop all live actors of a given class.
%%
%% BT-785: Queries the actor registry (if available) for all actors belonging
%% to the class, then kills each one. The registry is accessed by its
%% registered name to avoid a module-level dependency on beamtalk_workspace.
-spec stop_class_actors(atom()) -> ok.
stop_class_actors(ClassName) ->
    case erlang:whereis(beamtalk_actor_registry) of
        undefined ->
            ok;
        RegistryPid ->
            Actors = gen_server:call(RegistryPid, list_actors),
            ClassActors = [
                maps:get(pid, Meta)
             || Meta <- Actors, maps:get(class, Meta, undefined) =:= ClassName
            ],
            lists:foreach(
                fun(Pid) ->
                    catch gen_server:call(RegistryPid, {kill, Pid})
                end,
                ClassActors
            )
    end.
