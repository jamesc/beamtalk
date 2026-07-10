%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics).

%%% **DDD Context:** Object System Context

-moduledoc """
Thin Behaviour/Class intrinsics (ADR 0032 Phase 2).

These functions back the `@primitive "classXxx"` declarations in `lib/Behaviour.bt`
and `lib/Class.bt`. Each function receives a class object `Self`
(a `#beamtalk_object{}` tuple with a ClassPid at position 4) and exposes
either raw data from the class gen_server state / registry or small,
hierarchy-aware queries over that data.

Richer hierarchy-walking and protocol logic still lives in Beamtalk-level
code; this module provides a minimal, side-effect-free intrinsic surface
that the Behaviour/Class libraries can rely on.

## Intrinsic Table

| Erlang function             | Backing data source / derivation                          |
|-----------------------------|------------------------------------------------------------|
| classSuperclass/1           | Direct superclass from class gen_server state             |
| classAllSuperclasses/1      | Recursively walks the superclass chain from classSuperclass/1 |
| classSubclasses/1           | Direct subclasses from class registry                     |
| classAllSubclasses/1        | All subclasses from class registry                        |
| classLocalMethods/1         | Local method dictionary from class gen_server state       |
| classMethods/1              | Combined local + inherited methods via superclass chain   |
| classIncludesSelector/2     | Membership check in local method dictionary               |
| classFieldNames/1           | Field names from class gen_server state                   |
| classAllFieldNames/1        | Combined field names via superclass chain                 |
| classClassVarNames/1        | Class-side field names (class variables) from class meta   |
| classAllClassVarNames/1     | Combined class-side field names via superclass chain       |
| className/1                 | Class name from class gen_server state                    |
| classClass/1                | Real metaclass object (ADR 0036)                          |
| classDoc/1                  | Class doc string from class gen_server state (ADR 0033)   |
| classSetDoc/2               | Set class doc string (ADR 0033)                           |
| classSetMethodDoc/3         | Set method doc string for a selector (ADR 0033)           |
| classDocForMethod/2         | Get method doc string for a selector, or nil (BT-991)     |
| classRemoveFromSystem/1     | Remove class and cleanup runtime state                    |
| classSourceFile/1           | Source file path from beamtalk_source module attr (BT-845)|
| classReload/1               | Recompile from sourceFile + hot-swap (BT-845)             |
| classConformsTo/2           | Check if class conforms to a protocol (ADR 0068 Phase 2c) |
| classProtocols/1            | List protocols the class conforms to (ADR 0068 Phase 2c)  |
""".

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
    classCanUnderstandFromName/2,
    classFieldNames/1,
    classAllFieldNames/1,
    %% BT-2238: class-side field (class variable) reflection
    classClassVarNames/1,
    classAllClassVarNames/1,
    className/1,
    classClass/1,
    %% ADR 0036: Metaclass primitives
    metaclassThisClass/1,
    metaclassSuperclass/1,
    metaclassAllMethods/1,
    metaclassClassMethods/1,
    metaclassLocalClassMethods/1,
    metaclassIncludesSelector/2,
    metaclassNew/0,
    %% ADR 0033: Runtime-embedded documentation
    classDoc/1,
    classSetDoc/2,
    classSetMethodDoc/3,
    classDocForMethod/2,
    %% BT-785: Class removal
    classRemoveFromSystem/1,
    %% BT-1239: Programmatic class removal by name (for workspace/MCP unload)
    classRemoveFromSystemByName/1,
    %% BT-845: ADR 0040 Phase 2 — class-based reload
    classSourceFile/1,
    classReload/1,
    %% ADR 0082 Phase 1 (BT-2283): live method patch primitives
    classCompileSource/3,
    classTryCompileSource/3,
    %% ADR 0105 Phase 3 (BT-2782): pre-save advisory precheck
    classPrecheckCompileSource/3,
    %% ADR 0068 Phase 2c: Runtime protocol queries
    classConformsTo/2,
    classProtocols/1,
    %% ADR 0079 / BT-1988: exposed for cross-module hierarchy checks
    walk_hierarchy/3
]).

%%% ============================================================================
%%% Public Intrinsics
%%% ============================================================================

-doc """
Return the superclass of the receiver as a class object, or nil for roots.

ADR 0032: Returns a proper #beamtalk_object{} instead of a bare atom,
fixing the inconsistency where `Counter class` returned an object but
`Counter superclass` returned an atom.

BT-942: Uses __beamtalk_meta/0 when available; falls back to gen_server
for dynamic classes created via beamtalk_class_builder.
""".
-spec classSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
classSuperclass(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name(ClassPid),
    SuperclassName =
        case meta_for_module(Module) of
            {ok, Meta} ->
                maps:get(superclass, Meta);
            not_available ->
                case gen_server:call(ClassPid, superclass) of
                    none -> nil;
                    Name -> Name
                end
        end,
    case SuperclassName of
        nil -> nil;
        SuperName -> atom_to_class_object(SuperName)
    end.

-doc """
Return direct subclasses of the receiver as a list of class objects.

Queries the ETS hierarchy table for O(1) lookup per level.
""".
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

-doc """
Return all subclasses transitively (breadth-first) as class objects.

Queries the ETS hierarchy table recursively.
""".
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

-doc """
Return the local method selectors of the receiver (non-inherited).

Returns only methods defined directly in this class, not inherited ones.
Full chain walk for all methods is implemented in Behaviour.methods (pure Beamtalk).

BT-942: Uses __beamtalk_meta/0 when available; falls back to gen_server
for dynamic classes created via beamtalk_class_builder.

BT-1635: When called on a metaclass object (Foo class methods), returns
user-defined class methods instead of instance methods.
""".
-spec classLocalMethods(#beamtalk_object{}) -> [atom()].
classLocalMethods(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    %% BT-1635: Metaclass receiver — return user-defined class methods.
    maps:keys(beamtalk_object_class:local_class_methods_map(ClassPid));
classLocalMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:keys(maps:get(method_info, Meta));
        not_available ->
            gen_server:call(ClassPid, methods)
    end.

-doc """
Return all superclasses of the receiver in order (immediate parent to root).

BT-2194/BT-2217: For metaclass receivers (objects tagged `'Metaclass'`)
walks the *parallel* metaclass hierarchy and then grounds into the
instance-side `Class → Behaviour → Object → ProtoObject` tower (ADR 0036).
For `Counter class` this yields
`[Actor class, Object class, ProtoObject class, Class, Behaviour, Object, ProtoObject]`.
Consequence: `Counter class isKindOf: Object` returns `true`, agreeing with
the dispatch chain (`Counter class respondsTo: #printString` is `true` via
the same Class/Behaviour/Object protocol).
""".
-spec classAllSuperclasses(#beamtalk_object{}) -> [#beamtalk_object{}].
classAllSuperclasses(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    %% BT-2194: walk the parallel metaclass hierarchy (returns metaclass objects).
    SuperName = gen_server:call(ClassPid, superclass),
    MetaSupers = walk_hierarchy(
        SuperName,
        fun(_CN, CPid, Acc) ->
            MetaObj = #beamtalk_object{
                class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = CPid
            },
            {cont, [MetaObj | Acc]}
        end,
        []
    ),
    %% BT-2217: Ground the parallel chain into the instance-side `Class` tower
    %% so the metaclass hierarchy merges with `Class → Behaviour → Object →
    %% ProtoObject` (ADR 0036). `Class` may be absent during early bootstrap;
    %% walk_hierarchy/3 returns the initial accumulator in that case.
    InstanceSupers = walk_hierarchy(
        'Class',
        fun(CN, CPid, Acc) ->
            Module = gen_server:call(CPid, module_name),
            Tag = beamtalk_class_registry:class_object_tag(CN),
            ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = CPid},
            {cont, [ClassObj | Acc]}
        end,
        []
    ),
    lists:reverse(MetaSupers) ++ lists:reverse(InstanceSupers);
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

-doc """
Return all method selectors understood by instances (full inheritance chain).

BT-942: Uses __beamtalk_meta/0 at each hierarchy level when available;
falls back to gen_server for dynamic classes.

BT-1635: When called on a metaclass object (Foo class allMethods), walks
the hierarchy collecting user-defined class methods at each level instead
of instance methods.
""".
-spec classMethods(#beamtalk_object{}) -> [atom()].
classMethods(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    %% BT-1635: Metaclass receiver — collect class methods up the hierarchy.
    ClassName = gen_server:call(ClassPid, class_name),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            ClassMethods = maps:keys(
                beamtalk_object_class:local_class_methods_map(CPid)
            ),
            {cont, ordsets:union(A, ordsets:from_list(ClassMethods))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc);
classMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            Module = beamtalk_object_class:module_name(CPid),
            Methods =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:keys(maps:get(method_info, Meta));
                    not_available ->
                        gen_server:call(CPid, methods)
                end,
            {cont, ordsets:union(A, ordsets:from_list(Methods))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc).

-doc """
Test whether the class named ClassName has instances that understand Selector.

ADR 0032 Phase 3: Canonical single-source hierarchy walk used by
beamtalk_dispatch:responds_to/2. Takes ClassName directly (not a class object)
to avoid an extra gen_server:call needed to re-fetch the class name from a pid.
""".
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

-doc """
Return all field names including inherited, in slot order.

BT-942: Uses __beamtalk_meta/0 at each hierarchy level when available;
falls back to gen_server for dynamic classes.
""".
-spec classAllFieldNames(#beamtalk_object{}) -> [atom()].
classAllFieldNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name(CPid),
            IVars =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:get(fields, Meta);
                    not_available ->
                        gen_server:call(CPid, instance_variables)
                end,
            {cont, IVars ++ Acc}
        end,
        []
    ).

-doc """
Test whether the selector is defined locally in this class.

Does NOT check superclasses — local containment only.
Full chain walk for `canUnderstand:` is implemented in pure Beamtalk
(`Behaviour>>canUnderstand:`) on top of `classMethods/1`.

BT-1635: When called on a metaclass object, checks class methods instead
of instance methods.

BT-2189: Uses __beamtalk_meta/0 fast path when available — mirrors the
fast path already in `classLocalMethods/1` and avoids a gen_server
round-trip per call, which matters for the bulk iteration done by
`SystemNavigation implementorsOf:`. Falls back to gen_server for dynamic
classes built via ClassBuilder.
""".
-spec classIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
classIncludesSelector(#beamtalk_object{class = 'Metaclass', pid = ClassPid}, Selector) ->
    %% BT-1635: Metaclass receiver — check class methods.
    %% BT-2189: Use __beamtalk_meta/0 fast path when available, matching the
    %% instance-side clause below. Avoids per-call gen_server hops during the
    %% bulk iteration done by `SystemNavigation implementorsOf:`.
    Module = beamtalk_object_class:module_name(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:is_key(Selector, maps:get(class_method_info, Meta, #{}));
        not_available ->
            ClassMethods = beamtalk_object_class:local_class_methods_map(ClassPid),
            maps:is_key(Selector, ClassMethods)
    end;
classIncludesSelector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:is_key(Selector, maps:get(method_info, Meta));
        not_available ->
            LocalMethods = gen_server:call(ClassPid, methods),
            lists:member(Selector, LocalMethods)
    end.

-doc """
Return the names of fields declared in this class (not inherited).

BT-942: Uses __beamtalk_meta/0 when available; falls back to gen_server
for dynamic classes created via beamtalk_class_builder.
""".
-spec classFieldNames(#beamtalk_object{}) -> [atom()].
classFieldNames(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:get(fields, Meta);
        not_available ->
            gen_server:call(ClassPid, instance_variables)
    end.

-doc """
Return the class-side field (class variable) names declared in this class.

BT-2238: Backs `@primitive "classClassVarNames"` (`Behaviour>>classVarNames`)
— the class-side counterpart to `classFieldNames/1`. Reads the `class_fields`
key emitted into `__beamtalk_meta/0` from `classState:` declarations. Returns
`[]` for dynamic classes built via ClassBuilder (no static meta) and for
classes with no class-side state.

A distinct selector (not `fieldNames`) is required: `fieldNames` is intercepted
at the call site as an instance-reflection primitive and never reaches the
Behaviour method table.
""".
-spec classClassVarNames(#beamtalk_object{}) -> [atom()].
classClassVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:get(class_fields, Meta, []);
        not_available ->
            []
    end.

-doc """
Return all class-side field names including inherited, in slot order.

BT-2238: Backs `@primitive "classAllClassVarNames"`
(`Behaviour>>allClassVarNames`) — the class-side counterpart to
`classAllFieldNames/1`. Walks the superclass chain collecting each level's
`class_fields`, mirroring the slot order of `classAllFieldNames/1` (ancestor
class-side slots precede subclass ones).
""".
-spec classAllClassVarNames(#beamtalk_object{}) -> [atom()].
classAllClassVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name(CPid),
            CVars =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:get(class_fields, Meta, []);
                    not_available ->
                        []
                end,
            {cont, CVars ++ Acc}
        end,
        []
    ).

-doc "Return the name of the class as a Symbol (atom).".
-spec className(#beamtalk_object{}) -> atom().
className(Self) ->
    ClassPid = erlang:element(4, Self),
    gen_server:call(ClassPid, class_name).

-doc """
Return the metaclass object for the receiver.

ADR 0036: Replaces the sentinel atom with a real `#beamtalk_object{}`.
Wraps the same class pid but dispatches through the 'Metaclass' chain.
No new gen_server process — virtual tag approach from ADR 0013 continues.

Idempotent: when called on a `class='Metaclass'`-tagged object (i.e.,
`Metaclass class class`), extracts pid and returns a new structurally
identical record. This enables `Metaclass class class == Metaclass class`
(Erlang structural `==` compares all three fields: class, class_mod, pid).
""".
-spec classClass(#beamtalk_object{}) -> #beamtalk_object{}.
classClass(Self) ->
    Pid = erlang:element(4, Self),
    #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid}.

-doc """
Return the class documentation string, or nil if none set.

ADR 0033: Runtime-embedded documentation.
The class gen_server stores `none` internally; we return `nil` for Beamtalk.
""".
-spec classDoc(#beamtalk_object{}) -> binary() | 'nil'.
classDoc(Self) ->
    ClassPid = erlang:element(4, Self),
    case gen_server:call(ClassPid, get_doc) of
        none -> nil;
        Doc -> Doc
    end.

-doc """
Set the class documentation string.

ADR 0033: Post-hoc setter for class-level doc.
""".
-spec classSetDoc(#beamtalk_object{}, binary()) -> #beamtalk_object{}.
classSetDoc(Self, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = gen_server:call(ClassPid, {set_doc, DocBinary}),
    Self.

-doc """
Set the documentation string for a specific method selector.

ADR 0033: Post-hoc setter for method-level doc.
""".
-spec classSetMethodDoc(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classSetMethodDoc(Self, Selector, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = gen_server:call(ClassPid, {set_method_doc, Selector, DocBinary}),
    Self.

-doc """
Get the documentation string for a specific method selector, or nil.

BT-991: Completes the documentation API symmetry on Behaviour.
Returns the doc binary if set, nil if the method does not exist or has no
documentation. Walks the superclass chain via beamtalk_method_resolver.
""".
-spec classDocForMethod(#beamtalk_object{}, atom()) -> binary() | 'nil'.
classDocForMethod(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    case beamtalk_method_resolver:resolve(ClassPid, Selector) of
        nil ->
            nil;
        MethodObj when is_map(MethodObj) ->
            maps:get('__doc__', MethodObj, nil)
    end.

-doc """
Remove this class from the system, performing full cleanup.

BT-785: Implements `removeFromSystem` for class objects (Smalltalk convention).

Delegates to classRemoveFromSystemByName/1 after extracting the class name.
""".
-spec classRemoveFromSystem(#beamtalk_object{}) -> 'nil'.
classRemoveFromSystem(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    classRemoveFromSystemByName(ClassName).

-doc """
Remove a class from the system by name, performing full cleanup.

BT-1239: Programmatic variant of removeFromSystem — used by the MCP/REPL
unload op so workspace code can trigger removal without a Beamtalk object.

Safety checks (raises errors for):
  - Class not found in registry
  - Stdlib classes (module name starts with `bt@stdlib@`)
  - Classes with direct subclasses (must remove children first)

Cleanup sequence:
  1. Stop all live actors of this class (via beamtalk_actor_registry)
  2. Stop the class gen_server (terminate/2 removes ETS entry and pg group)
  3. Purge the BEAM module (code:soft_purge + code:delete)
""".
-spec classRemoveFromSystemByName(atom()) -> 'nil'.
classRemoveFromSystemByName(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            Error1 = beamtalk_error:with_message(
                Error0,
                iolist_to_binary([
                    <<"Class not found: '">>,
                    atom_to_binary(ClassName, utf8),
                    <<"'">>
                ])
            ),
            beamtalk_error:raise(Error1);
        ClassPid ->
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
                            %% Fully unload the BEAM module.
                            %% soft_purge removes any old-slot code from a prior reload.
                            %% delete moves current code to the old slot.
                            %% A second purge removes that old slot, freeing memory.
                            %% Actors have already been stopped above, so no process
                            %% should be running old code — soft_purge is safe both times.
                            ok = ensure_code_step(
                                ClassName,
                                Module,
                                soft_purge_before_delete,
                                code:soft_purge(Module)
                            ),
                            ok = ensure_code_step(
                                ClassName,
                                Module,
                                delete,
                                code:delete(Module)
                            ),
                            ok = ensure_code_step(
                                ClassName,
                                Module,
                                soft_purge_after_delete,
                                code:soft_purge(Module)
                            ),
                            publish_class_removed(ClassName, Module),
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
            end
    end.

-doc """
Return the source file path for this class, or nil if not set.

BT-845/BT-860: Reads `beamtalk_source` module attribute embedded at compile time.
This is the definitive source-of-truth (survives workspace restarts).
Returns nil for stdlib/bootstrap/ClassBuilder-created classes.
""".
-spec classSourceFile(#beamtalk_object{}) -> binary() | 'nil'.
classSourceFile(Self) ->
    ClassPid = erlang:element(4, Self),
    ModuleName = beamtalk_object_class:module_name(ClassPid),
    beamtalk_reflection:source_file_from_module(ModuleName).

-doc """
Recompile from sourceFile and hot-swap the BEAM module.

BT-845: ADR 0040 Phase 2.
Raises an error if sourceFile is nil (stdlib / dynamic class).
Delegates compilation to beamtalk_repl_eval:reload_class_file/1 via
erlang:apply/3 to avoid a compile-time dep from beamtalk_runtime to
beamtalk_workspace (follows the beamtalk_actor_registry registered-name pattern).
""".
-spec classReload(#beamtalk_object{}) -> #beamtalk_object{}.
classReload(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    ModuleName = beamtalk_object_class:module_name(ClassPid),
    SourceFile = beamtalk_reflection:source_file_from_module(ModuleName),
    case SourceFile of
        nil ->
            Error0 = beamtalk_error:new(no_source_file, ClassName),
            Msg = iolist_to_binary([
                atom_to_binary(ClassName, utf8),
                <<" has no source file — stdlib classes cannot be reloaded">>
            ]),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg));
        SourcePath ->
            SourcePathStr = binary_to_list(SourcePath),
            %% BT-1719: Demand-driven native .erl compilation before reload.
            %% Uses dynamic dispatch to avoid compile-time dep on beamtalk_workspace.
            try
                ProjectRoot = erlang:apply(
                    beamtalk_repl_ops_load, find_project_root, [SourcePathStr]
                ),
                _ = erlang:apply(
                    beamtalk_repl_ops_load,
                    maybe_recompile_native_deps,
                    [SourcePathStr, ProjectRoot]
                )
            catch
                error:undef -> ok
            end,
            try erlang:apply(beamtalk_repl_eval, reload_class_file, [SourcePathStr, ClassName]) of
                {ok, _Classes} ->
                    Self;
                {error, {class_not_found, _, Path, Defined}} ->
                    Error0 = beamtalk_error:new(reload_failed, ClassName),
                    DefinedStr = lists:join(<<", ">>, [list_to_binary(D) || D <- Defined]),
                    Msg = iolist_to_binary([
                        atom_to_binary(ClassName, utf8),
                        <<" is no longer defined in ">>,
                        list_to_binary(Path),
                        <<" (found: ">>,
                        DefinedStr,
                        <<")">>
                    ]),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(Error0, Msg)
                    );
                {error, Reason} ->
                    Error0 = beamtalk_error:new(reload_failed, ClassName),
                    Msg = iolist_to_binary(
                        io_lib:format("Reload failed: ~p", [Reason])
                    ),
                    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
            catch
                error:undef ->
                    Error0 = beamtalk_error:new(runtime_error, ClassName),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Error0,
                            <<"Workspace not available — reload requires a running workspace">>
                        )
                    )
            end
    end.

%%% ============================================================================
%%% Live Method Patch Primitives (ADR 0082 Phase 1, BT-2283)
%%% ============================================================================

-doc """
Compile a method body String and install it in this class as a **durable** live
patch (ADR 0082 Phase 1).

Backs `@primitive "classCompileSource"' (`Behaviour>>compile:source:') and the
target of MCP `save_method' / the browser "Save" action. The `>>' patcher form
and `compile:source:' are distinct front doors that converge at the runtime
install chokepoint (`beamtalk_repl_loader:load_recompiled_method/7'): `>>' is not
parser sugar for this primitive, but both produce the same in-memory patch and
ChangeLog entry there. `Selector' is a Symbol (atom), `Source' the method body
String (binary) passed as a value. Installs in memory and attempts (best-effort)
to record a durable ChangeLog entry; returns the receiver class.
""".
-spec classCompileSource(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classCompileSource(Self, Selector, Source) ->
    do_compile_source(Self, Selector, Source, durable).

-doc """
Compile a method body String and install it as an **ephemeral** live patch
(ADR 0082 Phase 1).

Backs `@primitive "classTryCompileSource"' (`Behaviour>>tryCompile:source:') and
the MCP `try_method' tool. Identical install to `classCompileSource/3' but the
ChangeLog entry is tagged `intent: ephemeral' so it auto-prunes on flush and on
workspace restart unless promoted via `compile:source:'. Returns the receiver.
""".
-spec classTryCompileSource(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classTryCompileSource(Self, Selector, Source) ->
    do_compile_source(Self, Selector, Source, ephemeral).

%% Shared compile-and-install path for compile:source: / tryCompile:source:.
%% Routes to beamtalk_repl_eval:compile_method/6 via erlang:apply to keep
%% beamtalk_runtime free of a compile-time dependency on beamtalk_workspace
%% (the same indirection classReload/1 uses).
-spec do_compile_source(#beamtalk_object{}, atom(), binary(), durable | ephemeral) ->
    #beamtalk_object{}.
do_compile_source(Self, Selector, Source, Intent) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    SourceBin = ensure_source_binary(Selector, Source, Intent, ClassName),
    {Author, AuthorKind} = current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, compile_method, [
            ClassNameBin, Selector, SourceBin, Intent, Author, AuthorKind
        ])
    of
        {ok, _ClassNameBin} ->
            Self;
        {error, Reason} ->
            Error0 = beamtalk_error:new(compile_failed, ClassName),
            Msg = iolist_to_binary(
                io_lib:format("Could not compile method: ~p", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    catch
        error:undef ->
            Error0 = beamtalk_error:new(runtime_error, ClassName),
            beamtalk_error:raise(
                beamtalk_error:with_message(
                    Error0,
                    <<
                        "Workspace not available — live method editing requires a "
                        "running workspace"
                    >>
                )
            )
    end.

-doc """
Compile a pending method edit and report would-be-stale dependents,
**without installing** (ADR 0105 Phase 3, BT-2782).

Backs `@primitive "classPrecheckCompileSource"' (`Behaviour>>precheckCompile:
source:') — the editor/LSP's "check before save" hook (ADR 0105's Phase 3
steelman accommodation: non-blocking, the post-reload image check remains
the authority; this is an early warning against the *pending* edit).
`Selector' is a Symbol (atom), `Source' the pending method body String
(binary), same argument shape as `compile:source:'. Nothing installs and
nothing is recorded to the ChangeLog — this is read-only. Returns a
Dictionary with keys `#findings` (List of finding Dictionaries: `#owner`,
`#changedClass`, `#selector`, `#classification`, `#severity`, `#category`,
`#message`, `#note`, `#sites`, `#start`, `#end`), `#checked`,
`#totalCandidates`, `#notChecked`, `#capNote`, `#checkedOwners` — mirroring
`beamtalk_recheck:result()`/`finding()`, camelCased for the Beamtalk surface
(matching `Workspace flush`'s `#newClasses` convention;
`precheck_result_to_dictionary/1` is this primitive's own encoder, parallel
to `beamtalk_ws_handler:encode_reload_check_event/1`'s wire encoder for the
same underlying shape).
""".
-spec classPrecheckCompileSource(#beamtalk_object{}, atom(), binary()) -> map().
classPrecheckCompileSource(Self, Selector, Source) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    SourceBin = ensure_precheck_source_binary(Source, ClassName),
    try
        %% instance-side only: like classCompileSource/3, the class-side
        %% patch path (`Class class >> sel') does not route through this
        %% primitive — see beamtalk_repl_loader's patch_side/1 callers.
        erlang:apply(beamtalk_repl_eval, precheck_method, [
            ClassNameBin, Selector, SourceBin, instance
        ])
    of
        {ok, Result} ->
            precheck_result_to_dictionary(Result);
        {error, Reason} ->
            Error0 = beamtalk_error:new(compile_failed, ClassName),
            Msg = iolist_to_binary(
                io_lib:format("Could not precheck method: ~p", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    catch
        error:undef ->
            Error0 = beamtalk_error:new(runtime_error, ClassName),
            beamtalk_error:raise(
                beamtalk_error:with_message(
                    Error0,
                    <<
                        "Workspace not available — pre-save precheck requires a "
                        "running workspace"
                    >>
                )
            )
    end.

%% Validate the `Source' argument for `precheckCompile:source:' is a String
%% (binary). Distinct from ensure_source_binary/4 because there is no
%% `Intent' to derive the error's selector name from — precheck only ever
%% backs one public selector.
-spec ensure_precheck_source_binary(term(), atom()) -> binary().
ensure_precheck_source_binary(Source, _ClassName) when is_binary(Source) ->
    Source;
ensure_precheck_source_binary(Source, ClassName) ->
    Error0 = beamtalk_error:new(type_error, ClassName),
    Msg = iolist_to_binary(
        io_lib:format("precheckCompile:source: expects a String body, got: ~p", [Source])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

%% Encode a `beamtalk_recheck:result()' as a Beamtalk-facing Dictionary,
%% camelCasing the compound-word keys (`total_candidates' -> `totalCandidates',
%% etc.) — Beamtalk-facing maps use camelCase (`Workspace flush''s
%% `newClasses'), while the internal ADR 0105 result/finding shape is
%% snake_case throughout the Erlang side. Parallels
%% `beamtalk_ws_handler:encode_reload_check_event/1', which does the same
%% translation for the `reload_check' WS push frame's JSON wire shape.
%% `map()`, not `beamtalk_recheck:result()`: beamtalk_runtime has no
%% compile-time dependency on beamtalk_workspace (same reason do_compile_source/4
%% dispatches via erlang:apply/3 rather than a direct call) — the shape is
%% documented above instead of type-referenced.
-spec precheck_result_to_dictionary(map()) -> map().
precheck_result_to_dictionary(#{
    findings := Findings,
    checked := Checked,
    total_candidates := TotalCandidates,
    not_checked := NotChecked,
    cap_note := CapNote,
    checked_owners := CheckedOwners
}) ->
    #{
        findings => [precheck_finding_to_dictionary(F) || F <- Findings],
        checked => Checked,
        totalCandidates => TotalCandidates,
        notChecked => NotChecked,
        capNote => CapNote,
        checkedOwners => CheckedOwners
    }.

-spec precheck_finding_to_dictionary(map()) -> map().
precheck_finding_to_dictionary(#{
    owner := Owner,
    changed_class := ChangedClass,
    selector := Selector,
    classification := Classification,
    severity := Severity,
    category := Category,
    message := Message,
    note := Note,
    sites := Sites,
    start := Start,
    'end' := End
}) ->
    #{
        owner => Owner,
        changedClass => ChangedClass,
        selector => Selector,
        classification => Classification,
        severity => Severity,
        category => Category,
        message => Message,
        note => Note,
        sites => Sites,
        start => Start,
        'end' => End
    }.

%% Validate the `Source' argument is a String (binary). Raises a typed error for
%% a non-binary so callers get a clear message instead of a deep crash. The
%% message names the actual selector invoked (derived from `Intent') so callers
%% of `tryCompile:source:' do not see a misleading `compile:source:' message.
-spec ensure_source_binary(atom(), term(), durable | ephemeral, atom()) -> binary().
ensure_source_binary(_Selector, Source, _Intent, _ClassName) when is_binary(Source) ->
    Source;
ensure_source_binary(_Selector, Source, Intent, ClassName) ->
    Error0 = beamtalk_error:new(type_error, ClassName),
    SelectorName = intent_selector(Intent),
    Msg = iolist_to_binary(
        io_lib:format("~s expects a String body, got: ~p", [SelectorName, Source])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

%% Surface selector for the public message: `compile:source:' is durable,
%% `tryCompile:source:' is ephemeral.
-spec intent_selector(durable | ephemeral) -> binary().
intent_selector(ephemeral) -> <<"tryCompile:source:">>;
intent_selector(durable) -> <<"compile:source:">>.

%% Resolve the audit author for the current patch. ADR 0082 distinguishes
%% `human' (REPL / interactive) from `agent' (MCP `save_method' / `try_method').
%% The submission boundary stamps the kind into the process dictionary before
%% dispatching; absent that stamp (e.g. a direct REPL `compile:source:' call) we
%% default to `human'/`repl'. Returns `{Author, AuthorKind}'.
-spec current_author_context() -> {binary(), human | agent}.
current_author_context() ->
    case erlang:get('$beamtalk_author_kind') of
        agent ->
            Author =
                case erlang:get('$beamtalk_author') of
                    A when is_binary(A) -> A;
                    _ -> <<"agent">>
                end,
            {Author, agent};
        _ ->
            Author =
                case erlang:get('$beamtalk_author') of
                    A when is_binary(A) -> A;
                    _ -> <<"repl">>
                end,
            {Author, human}
    end.

%%% ============================================================================
%%% Protocol Query Primitives (ADR 0068 Phase 2c)
%%% ============================================================================

-doc """
Check if the receiver class conforms to a protocol.

ADR 0068 Phase 2c: Backs `@primitive "classConformsTo"` in Behaviour.bt.
Structural conformance — the class conforms if it responds to all required
selectors of the protocol.

The protocol argument is expected to be a Symbol (atom) naming the protocol.
""".
-spec classConformsTo(#beamtalk_object{}, atom()) -> boolean().
classConformsTo(Self, ProtocolName) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    beamtalk_protocol_registry:conforms_to(ClassName, ProtocolName).

-doc """
Return the list of protocols the receiver class conforms to.

ADR 0068 Phase 2c: Backs `@primitive "classProtocols"` in Behaviour.bt.
Returns a list of protocol name atoms, sorted alphabetically.
""".
-spec classProtocols(#beamtalk_object{}) -> [atom()].
classProtocols(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    beamtalk_protocol_registry:protocols_for_class(ClassName).

%%% ============================================================================
%%% Metaclass Primitives (ADR 0036 Phase 1)
%%% ============================================================================

-doc """
Return the class this metaclass describes.

ADR 0036: Backs `@primitive "metaclassThisClass"` in Metaclass.bt.
A metaclass object carries the class pid; we retrieve its name and return
the class object. Example: `Counter class class thisClass == Counter`.
""".
-spec metaclassThisClass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassThisClass(Self) ->
    Pid = erlang:element(4, Self),
    ClassName = gen_server:call(Pid, class_name),
    atom_to_class_object(ClassName).

-doc """
Return the superclass of the metaclass parallel hierarchy.

ADR 0036: Backs `@primitive "metaclassSuperclass"` in Metaclass.bt.
The superclass of Counter's metaclass is the metaclass of Counter's superclass.
Example: `Counter class superclass == Actor class`.

BT-1186: Now uses gen_server:call(Pid, superclass) directly. BT-1185 fixed
apply_class_info/2 to update the gen_server superclass from __beamtalk_meta/0,
so the gen_server always holds the correct superclass.

BT-2217: Grounds the parallel chain at `ProtoObject class superclass == Class`
(ADR 0036). When the gen_server reports no superclass, return the instance-side
`Class` class object. From there, subsequent `superclass` sends route through
the regular instance-side dispatch (`classSuperclass`), unfolding
`Class → Behaviour → Object → ProtoObject` via the standard walker. `Class`
may be absent during early bootstrap; `atom_to_class_object/1` returns `nil`
in that case, preserving the pre-grounding behaviour.
""".
-spec metaclassSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassSuperclass(Self) ->
    Pid = erlang:element(4, Self),
    case gen_server:call(Pid, superclass) of
        none ->
            atom_to_class_object('Class');
        SuperName ->
            case atom_to_class_object(SuperName) of
                nil -> nil;
                SuperClassObj -> classClass(SuperClassObj)
            end
    end.

-doc """
Return all selectors callable on the described class object (class-side + Behaviour protocol).

BT-1169: Backs `@primitive "metaclassAllMethods"` in Metaclass.bt.
Combines class-side selectors of the described class (via metaclassClassMethods/1)
with all instance methods of the 'Class' hierarchy (Behaviour protocol: reload,
superclass, etc.). Result is deduplicated and sorted.

We walk the instance method chain of 'Class' directly in Erlang to avoid
dispatching through the Metaclass chain (which would recurse into this method).

BT-1186: Now uses walk_hierarchy/3 directly. BT-1185 fixed apply_class_info/2
to update the gen_server superclass from __beamtalk_meta/0, so walk_hierarchy/3
correctly traverses Class → Behaviour → Object.
""".
-spec metaclassAllMethods(#beamtalk_object{}) -> [atom()].
metaclassAllMethods(Self) ->
    ClassMethods = metaclassClassMethods(Self),
    BehaviourMethodsOrdset = walk_hierarchy(
        'Class',
        fun(_CN, CPid, A) ->
            Module = beamtalk_object_class:module_name(CPid),
            Methods =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:keys(maps:get(method_info, Meta));
                    not_available ->
                        gen_server:call(CPid, methods)
                end,
            {cont, ordsets:union(A, ordsets:from_list(Methods))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(
        ordsets:union(BehaviourMethodsOrdset, ordsets:from_list(ClassMethods))
    ).

-doc """
Return all class-side method selectors (full inheritance chain).

ADR 0036: Backs `@primitive "metaclassClassMethods"` in Metaclass.bt.
Walks the superclass chain collecting all class-side selectors.
""".
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

-doc """
Return local class-side method selectors (non-inherited).

ADR 0036: Backs `@primitive "metaclassLocalClassMethods"` in Metaclass.bt.
Returns only class methods defined directly on this class.
""".
-spec metaclassLocalClassMethods(#beamtalk_object{}) -> [atom()].
metaclassLocalClassMethods(Self) ->
    Pid = erlang:element(4, Self),
    ClassMethods = gen_server:call(Pid, get_local_class_methods),
    maps:keys(ClassMethods).

-doc """
Test whether the selector is defined as a class-side method.

ADR 0036: Backs `@primitive "metaclassIncludesSelector"` in Metaclass.bt.
Does NOT check superclasses — local containment only.
""".
-spec metaclassIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
metaclassIncludesSelector(Self, Selector) ->
    Pid = erlang:element(4, Self),
    ClassMethods = gen_server:call(Pid, get_local_class_methods),
    maps:is_key(Selector, ClassMethods).

-doc """
Guard for direct Metaclass instantiation — backs `class sealed new`.

ADR 0036: Backs `@primitive "metaclassNew"` in Metaclass.bt.
Called from the generated `new/0` constructor of Metaclass.
Always raises a user_error; metaclasses must be obtained via `x class class`.
""".
-spec metaclassNew() -> no_return().
metaclassNew() ->
    Error = beamtalk_error:new(
        user_error, 'Metaclass', 'new', <<"Use x class class to obtain a metaclass">>
    ),
    beamtalk_error:raise(Error).

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc """
Try to retrieve reflection metadata from a compiled module's __beamtalk_meta/0.

BT-942: Returns `{ok, Meta}` if the module exports `__beamtalk_meta/0` and
the call succeeds; returns `not_available` otherwise (dynamic classes from
beamtalk_class_builder, or any module that doesn't export the function).

Note on superclass representation: the meta map uses atom `nil` for root
classes (no superclass), while the gen_server `superclass` message returns
atom `none`. Callers must normalize both to Beamtalk `nil`.
""".
-spec meta_for_module(atom()) -> {ok, map()} | not_available.
meta_for_module(Module) ->
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true ->
            try Module:'__beamtalk_meta'() of
                Meta when is_map(Meta) -> {ok, Meta}
            catch
                _:_ -> not_available
            end;
        false ->
            not_available
    end.

-doc """
Generic fold over the superclass chain starting from ClassName.

Fun receives (ClassName, ClassPid, Acc) and returns:
  {cont, NewAcc}   — continue walking to superclass
  {halt, Result}   — stop and return Result immediately

Returns Acc when the chain is exhausted (none or unregistered class).
Guards against cycles via ?MAX_HIERARCHY_DEPTH.
""".
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
        [?MAX_HIERARCHY_DEPTH],
        #{domain => [beamtalk, runtime]}
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

-doc """
Convert a class name atom to a class object (#beamtalk_object{}).

Looks up the class process, gets its module name, and constructs
the class object tuple. Returns nil if the class is not registered
(safe during bootstrap window).
""".
-spec atom_to_class_object(atom()) -> #beamtalk_object{} | 'nil'.
atom_to_class_object(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            ?LOG_DEBUG("atom_to_class_object: class ~p not registered", [ClassName], #{
                domain => [beamtalk, runtime]
            }),
            nil;
        ClassPid ->
            Module = gen_server:call(ClassPid, module_name),
            Tag = beamtalk_class_registry:class_object_tag(ClassName),
            #beamtalk_object{class = Tag, class_mod = Module, pid = ClassPid}
    end.

-doc """
Notify workspace layer that a class was successfully removed.

BT-1242: Cleans up stale entries in workspace_meta and REPL session trackers.
Uses registered-name tricks to avoid a hard DDD dep from beamtalk_runtime →
beamtalk_workspace (same pattern as stop_class_actors/1 and classReload/1).

  - beamtalk_workspace_meta has a known registered name — cast directly.
  - beamtalk_repl_shells pg group — broadcast to all active REPL sessions.
""".
-spec publish_class_removed(atom(), atom()) -> ok.
publish_class_removed(ClassName, Module) ->
    %% Workspace metadata cleanup.
    gen_server:cast(beamtalk_workspace_meta, {unregister_module, Module}),
    %% REPL session tracker cleanup — broadcast to all joined session shells.
    Shells =
        try
            pg:get_members(beamtalk_repl_shells)
        catch
            _:_ -> []
        end,
    lists:foreach(fun(Pid) -> Pid ! {class_removed, ClassName, Module} end, Shells).

-doc """
Assert that a code-server step succeeded; raise a structured error if not.

Both code:soft_purge/1 and code:delete/1 return false on failure (e.g.,
processes still linger in old code, or there is already old code that must
be purged first). Silently ignoring false would leave the BEAM module
resident while the class registry entry is already removed. This helper
converts a false result into a beamtalk_error so the caller is notified.
""".
-spec ensure_code_step(atom(), atom(), atom(), boolean()) -> ok.
ensure_code_step(_ClassName, _Module, _Step, true) ->
    ok;
ensure_code_step(ClassName, Module, Step, false) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Msg = iolist_to_binary(
        io_lib:format("Failed to ~p module ~p during unload", [Step, Module])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

-doc """
Check if a module name belongs to the Beamtalk stdlib.
BT-785: Stdlib modules have the prefix `bt@stdlib@`.
""".
-spec is_stdlib_module_name(atom()) -> boolean().
is_stdlib_module_name(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@stdlib@", _/binary>> -> true;
        _ -> false
    end;
is_stdlib_module_name(_) ->
    false.

-doc """
Stop all live actors of a given class.

BT-785: Queries the actor registry (if available) for all actors belonging
to the class, then kills each one. The registry is accessed by its
registered name to avoid a module-level dependency on beamtalk_workspace.
""".
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
                    try
                        gen_server:call(RegistryPid, {kill, Pid})
                    catch
                        _:_ -> ok
                    end
                end,
                ClassActors
            )
    end.
