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
| classSuperclass/1           | Direct superclass from unified class metadata (ETS, BT-3107) |
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
| classRemoveSelector/2       | Remove a method selector, raising if absent (ADR 0112)    |
| classRemoveSelectorIfAbsent/3 | Remove a method selector, running a fallback block if absent (ADR 0112) |
| classSourceFile/1           | Source file path from beamtalk_source module attr (BT-845)|
| classReload/1               | Recompile from sourceFile + hot-swap (BT-845)             |
| classConformsTo/2           | Check if class conforms to a protocol (ADR 0068 Phase 2c) |
| classProtocols/1            | List protocols the class conforms to (ADR 0068 Phase 2c)  |
| classRenameTo/2             | Rename the class + rewrite reference sites (ADR 0114 Phase 2, BT-3278) |
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
    %% ADR 0112 Phase 2 (BT-3186): method-level removal primitives
    classRemoveSelector/2,
    classRemoveSelectorIfAbsent/3,
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
    %% ADR 0114 Phase 2 (BT-3278): class rename primitive
    classRenameTo/2,
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

BT-942: Uses __beamtalk_meta/0 when available; falls back to the
deadlock-safe metadata lookup (`beamtalk_object_class:superclass_safe/1`,
BT-3107) for dynamic classes created via beamtalk_class_builder — the same
ETS source `beamtalk_class_dispatch` reads, instead of a separate
`gen_server:call` that could disagree with it mid-reload.
""".
-spec classSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
classSuperclass(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    SuperclassName =
        case meta_for_module(Module) of
            {ok, Meta} ->
                maps:get(superclass, Meta);
            not_available ->
                case beamtalk_object_class:superclass_safe(ClassPid) of
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
    %% BT-3107: deadlock-safe metadata lookup, same source dispatch reads.
    SuperName = beamtalk_object_class:superclass_safe(ClassPid),
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
    %% BT-3107: deadlock-safe metadata lookup, same source dispatch reads.
    SuperName = beamtalk_object_class:superclass_safe(ClassPid),
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
            Module = beamtalk_object_class:module_name_safe(CPid),
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
            Module = beamtalk_object_class:module_name_safe(CPid),
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:is_key(Selector, maps:get(class_method_info, Meta, #{}));
        not_available ->
            ClassMethods = beamtalk_object_class:local_class_methods_map(ClassPid),
            maps:is_key(Selector, ClassMethods)
    end;
classIncludesSelector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
    Module = beamtalk_object_class:module_name_safe(ClassPid),
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
            Module = beamtalk_object_class:module_name_safe(CPid),
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
  4. Purge derived registries — xref, extensions, protocol registry,
     compiler cache, workspace class_sources (BT-3105, via
     beamtalk_class_lifecycle:class_removed/2)
  5. Notify the workspace layer / REPL sessions (publish_class_removed/2)
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
                            %% BT-3206: snapshot the class's current full
                            %% source and its on-disk flushability
                            %% classification BEFORE any teardown step below
                            %% runs — beamtalk_class_lifecycle:class_removed/2
                            %% purges the very class_sources entry and
                            %% class-registry pid this reads, so capturing
                            %% after removal would have nothing left to read
                            %% (mirrors the "read+parse before mutate"
                            %% ordering compile:source:'s patch hook already
                            %% uses).
                            ClassNameBin = atom_to_binary(ClassName, utf8),
                            RemovalSnapshot = capture_class_removal_snapshot(ClassNameBin),
                            %% BT-3236: stop eager crash recovery for this
                            %% class BEFORE killing its actors. BT-3243
                            %% removed the link that used to exist between an
                            %% actor and its class gen_server, so the kills
                            %% below can no longer take the class process
                            %% down with them — this unwatch is now defense
                            %% in depth against any other crash/kill landing
                            %% on the class process during the removal
                            %% window, which the monitor would otherwise
                            %% classify as a crash and resurrect mid-removal.
                            beamtalk_class_monitor:unwatch(ClassName),
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
                            %% BT-3105: Purge every derived registry (xref,
                            %% extensions, protocol registry, compiler cache,
                            %% workspace class_sources) — the single teardown
                            %% path, run before the workspace/REPL notification
                            %% below.
                            ok = beamtalk_class_lifecycle:class_removed(ClassName, Module),
                            publish_class_removed(ClassName, Module),
                            log_class_removal(ClassNameBin, RemovalSnapshot),
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
                                    <<"'; it has subclasses">>
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

%%% ============================================================================
%%% Method Removal Primitives (ADR 0112 Phase 2, BT-3186)
%%% ============================================================================

-doc """
Remove `Selector` from the receiver, raising if it is not defined locally or
as an extension.

Backs `@primitive "classRemoveSelector"` (`Behaviour>>removeSelector:`). Side
follows `classIncludesSelector/2`'s own convention: a `Self` tagged
`class = 'Metaclass'` (i.e. `Counter class removeSelector: #foo`) touches the
class-side method table; any other `Self` (`Counter removeSelector: #foo`)
touches the instance-side table.

Resolution mirrors `beamtalk_dispatch:lookup/5`'s own order (ADR 0112 §
Extension methods): the extension registry (ADR 0066 open classes) is
checked first — an extension shadows a same-named local method, so removing
it re-exposes the local one (a second `removeSelector:` call removes that
too) — then the local method table. Removing a locally-defined override
re-exposes whatever the superclass chain supplies with no restart needed
(ADR 0032's chain-walk dispatch does the work; nothing here invalidates a
cache because there isn't one).

Installs unconditionally — no receiver-side stdlib/sealed refusal, unlike
`classRemoveFromSystemByName/1`. What varies is whether the change is
flushable to disk (a later concern, ADR 0112 Phase 3), not whether it takes
effect in memory — the same "flushability, not refusal" rule
`classCompileSource/3` already follows for stdlib/dynamic/dependency classes.

Raises a `selector_not_found` `#beamtalk_error{}` — deliberately not
`does_not_understand` (see the ADR's *Error behaviour on absent selector*) —
when the selector resolves nowhere.
""".
-spec classRemoveSelector(#beamtalk_object{}, atom()) -> #beamtalk_object{}.
classRemoveSelector(Self, Selector) ->
    case remove_selector(Self, Selector) of
        removed -> Self;
        absent -> beamtalk_error:raise(selector_not_found_error(Self, Selector))
    end.

-doc """
Remove `Selector` from the receiver like `classRemoveSelector/2`, but
evaluate `AbsentBlock` instead of raising when the selector resolves nowhere.

Backs `@primitive "classRemoveSelectorIfAbsent"`
(`Behaviour>>removeSelector:ifAbsent:`). `AbsentBlock` arrives as an ordinary
compiled Block value — a 0-arity Erlang fun (Beamtalk blocks compile to
funs; see `Block.bt`) — called directly here, exactly as
`beamtalk_actor.erl`'s `onExit:` callback calls its own block argument.

Unlike a block argument received by a *locally-defined* class method (which
runs inside that class's own gen_server call, per CLAUDE.md's "Blocks into
class methods" rule), this primitive — like every `Behaviour` tower
primitive — is itself reached via the Class -> Behaviour chain-walk
fallthrough (`beamtalk_dispatch:lookup/5`) when sent to a class object,
which runs in the *sender's* process, not the receiver's. `AbsentBlock`
therefore runs there too: messaging the receiver class back from inside it
is an ordinary cross-process send, not a re-entrant one — verified
empirically (`tests/repl-protocol/cases/remove_selector.btscript`) rather
than assumed from the general class-method-block rule, which does not apply
to this call shape.

Returns the receiver on success, or `AbsentBlock`'s value on absence.
""".
-spec classRemoveSelectorIfAbsent(#beamtalk_object{}, atom(), fun(() -> term())) ->
    #beamtalk_object{} | term().
classRemoveSelectorIfAbsent(Self, Selector, AbsentBlock) ->
    case remove_selector(Self, Selector) of
        removed -> Self;
        absent -> AbsentBlock()
    end.

%% Shared resolution + removal for classRemoveSelector/2 and
%% classRemoveSelectorIfAbsent/3. Resolves `Selector` against whichever table
%% currently supplies dispatch for it on `Self`'s side — extension registry
%% first, local method table second (ADR 0112 § Extension methods) — removes
%% it there, and reports whether anything was found. A removal that resolves
%% but then fails unexpectedly (e.g. a local-method recompile error) raises
%% directly rather than returning through this function, so its return type
%% only distinguishes "removed" from "nothing to remove here".
-spec remove_selector(#beamtalk_object{}, atom()) -> removed | absent.
remove_selector(Self, Selector) ->
    {Side, ClassPid} = removal_target(Self),
    ClassName = gen_server:call(ClassPid, class_name),
    EtsClass = extension_ets_class(ClassName, Side),
    case beamtalk_extensions:has(EtsClass, Selector) of
        true ->
            remove_extension_selector(ClassName, EtsClass, Selector, Side),
            removed;
        false ->
            case classIncludesSelector(Self, Selector) of
                true ->
                    remove_local_method(ClassName, Selector, Side),
                    removed;
                false ->
                    absent
            end
    end.

%% Remove an extension method and best-effort log its removal (ADR 0112 § Extension
%% methods, § ChangeLog interaction; BT-3187). Captures the extension's owner +
%% stored source BEFORE unregistering — both are gone from `beamtalk_extensions`'s
%% ETS tables the instant `unregister/3` returns — so the ChangeLog entry can
%% still attribute the removal and record what was removed for the audit trail.
-spec remove_extension_selector(atom(), atom(), atom(), instance | class) -> ok.
remove_extension_selector(ClassName, EtsClass, Selector, Side) ->
    Owner = extension_owner(EtsClass, Selector),
    PrevSource = extension_prev_source(EtsClass, Selector),
    ok = beamtalk_extensions:unregister(ClassName, Selector, Side =:= class),
    log_extension_removal(ClassName, Selector, Side, Owner, PrevSource).

-spec extension_owner(atom(), atom()) -> atom() | undefined.
extension_owner(EtsClass, Selector) ->
    case beamtalk_extensions:lookup(EtsClass, Selector) of
        {ok, _Fun, Owner} -> Owner;
        not_found -> undefined
    end.

-spec extension_prev_source(atom(), atom()) -> binary() | undefined.
extension_prev_source(EtsClass, Selector) ->
    case beamtalk_extensions:getSource(EtsClass, Selector) of
        {ok, Source} -> Source;
        not_found -> undefined
    end.

%% Best-effort ChangeLog append for an extension removal — mirrors
%% log_local_removal/3's `error:undef` guard below: the extension install
%% already succeeded above, so a missing/unavailable workspace here must
%% never surface as a caller-visible error.
-spec log_extension_removal(
    atom(), atom(), instance | class, atom() | undefined, binary() | undefined
) ->
    ok.
log_extension_removal(ClassName, Selector, Side, Owner, PrevSource) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    {Author, AuthorKind} = current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, emit_extension_remove_change_entry, [
            ClassNameBin, Selector, Side, Owner, PrevSource, Author, AuthorKind
        ])
    catch
        error:undef -> ok
    end,
    ok.

%% `Self`'s removal side and class pid. A metaclass-tagged receiver
%% (`Counter class removeSelector: #foo`) is class-side; any other class
%% object (`Counter removeSelector: #foo`) is instance-side — the same
%% branch `classIncludesSelector/2` already makes.
-spec removal_target(#beamtalk_object{}) -> {instance | class, pid()}.
removal_target(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    {class, ClassPid};
removal_target(Self) ->
    {instance, erlang:element(4, Self)}.

%% The `beamtalk_extensions` ETS key for `Side` — BT-3185's established
%% convention: an instance-side extension is keyed under the bare class name,
%% a class-side one under the metaclass tag (`unregister/3`'s own `ClassSide`
%% resolution, mirrored here so a lookup and its matching removal always
%% agree on the key).
-spec extension_ets_class(atom(), instance | class) -> atom().
extension_ets_class(ClassName, instance) -> ClassName;
extension_ets_class(ClassName, class) -> beamtalk_class_registry:class_object_tag(ClassName).

%% Remove `Selector` from `ClassName`'s own (non-extension) method table via
%% the existing revert-of-add removal mechanism (ADR 0082/BT-2663, generalized
%% by ADR 0112 rather than duplicated — see the ADR's *Implementation*
%% section), with the stdlib gate relaxed (BT-3184's `allow_stdlib` policy)
%% since `removeSelector:` installs unconditionally. Routed via
%% `erlang:apply/3` to avoid a compile-time dependency from `beamtalk_runtime`
%% to `beamtalk_workspace` — the same indirection `do_compile_source/4` uses.
%% Raises a structured `runtime_error` if the removal itself fails (recompile
%% error, or no running workspace to route it through). Best-effort logs a
%% `"remove-method"` ChangeLog entry after a successful removal (ADR 0112
%% Phase 3, BT-3187) — see log_local_removal/3.
-spec remove_local_method(atom(), atom(), instance | class) -> ok.
remove_local_method(ClassName, Selector, Side) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    try
        erlang:apply(beamtalk_repl_eval, remove_method, [
            ClassNameBin, Selector, Side, allow_stdlib
        ])
    of
        {ok, _} ->
            log_local_removal(ClassNameBin, Selector, Side),
            ok;
        {error, Reason} ->
            Error0 = beamtalk_error:new(runtime_error, ClassName, Selector),
            Msg = iolist_to_binary(
                io_lib:format("Could not remove method: ~p", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    catch
        error:undef ->
            Error0 = beamtalk_error:new(runtime_error, ClassName, Selector),
            beamtalk_error:raise(
                beamtalk_error:with_message(
                    Error0,
                    <<
                        "Workspace not available; removeSelector: requires a "
                        "running workspace"
                    >>
                )
            )
    end.

%% Best-effort ChangeLog append after a successful local-method removal (ADR
%% 0112 Phase 3, BT-3187) — the install already succeeded above, so a logging
%% failure must never surface to the caller. `emit_remove_change_entry/5`
%% already self-swallows every internal failure (mirrors `emit_change_entry/1`
%% for a patch); the `error:undef` catch here only guards the degenerate case
%% where the workspace app itself is unreachable, which cannot realistically
%% happen immediately after the removal above succeeded through the identical
%% `erlang:apply/3` seam, but is cheap to guard defensively all the same.
-spec log_local_removal(binary(), atom(), instance | class) -> ok.
log_local_removal(ClassNameBin, Selector, Side) ->
    {Author, AuthorKind} = current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, emit_remove_change_entry, [
            ClassNameBin, Selector, Side, Author, AuthorKind
        ])
    catch
        error:undef -> ok
    end,
    ok.

%% BT-3206: best-effort snapshot of a class's current full source and its
%% on-disk flushability classification, taken by
%% classRemoveFromSystemByName/1 before its teardown begins (see that call
%% site's comment for why the ordering matters). Routed via `erlang:apply/3`
%% to avoid a compile-time dependency from `beamtalk_runtime` to
%% `beamtalk_workspace` — the same indirection `remove_local_method/3` uses.
%% Degrades to a not-flushable/`"dynamic"` snapshot on ANY failure — not just
%% `error:undef` (the missing-workspace-app case `remove_local_method/3`'s own
%% catch guards) — because unlike that helper's post-mutation logging, this
%% call runs BEFORE teardown starts: an uncaught exception here (e.g. a
%% transient `gen_server:call` failure against `beamtalk_workspace_meta`)
%% would abort the removal itself, not just degrade its audit trail. A
%% snapshot capture failure must never block the removal it is only trying to
%% describe.
-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    try
        erlang:apply(beamtalk_repl_eval, capture_class_removal_snapshot, [ClassNameBin])
    catch
        error:undef ->
            %% Expected when the workspace app isn't running (e.g. plain
            %% runtime, no live ChangeLog) — not a bug, so no log line.
            #{flushable => false, not_flushable_reason => <<"dynamic">>};
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "capture_class_removal_snapshot failed unexpectedly for ~p — logging removal as dynamic",
                [ClassNameBin],
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            #{flushable => false, not_flushable_reason => <<"dynamic">>}
    end.

%% Best-effort ChangeLog append after a successful class removal (BT-3206),
%% called at classRemoveFromSystemByName/1's existing success point
%% (immediately after publish_class_removed/2) with the snapshot captured
%% before teardown. Mirrors log_local_removal/3's placement and
%% self-swallowing failure handling: the removal is already complete and
%% irreversible in memory by this point, so a logging failure must never
%% surface to the caller.
-spec log_class_removal(binary(), map()) -> ok.
log_class_removal(ClassNameBin, Snapshot) ->
    {Author, AuthorKind} = current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, emit_remove_class_change_entry, [
            ClassNameBin, Snapshot, Author, AuthorKind
        ])
    catch
        error:undef -> ok
    end,
    ok.

%%% ============================================================================
%%% Class Rename Primitive (ADR 0114 Phase 2, BT-3278)
%%% ============================================================================

-doc """
Rename the receiver class to `NewName`, rewriting every in-project reference
site found via `SystemNavigation referencesTo:` / `beamtalk_class_registry:
direct_subclasses/1`, and re-registering it under `NewName` in memory.

Backs `@primitive "classRenameTo"` (`Behaviour>>renameTo:`), modelled on
`classRemoveSelector/2`'s shape immediately above: resolve the target,
validate, mutate, best-effort log, return the receiver. Unlike
`removeSelector:`'s "flushable, not refusal" rule, this primitive follows
ADR 0114 § "Refusal vs flushability"'s per-operation table — a stdlib or
dependency class is refused BEFORE any memory mutation (the xref index only
covers in-project source, so site discovery for either could never be
complete), while a dynamic (`ClassBuilder`) class is allowed with
`flushable: false` (`"dynamic"`), same as a project class's disk half
(BT-3271, out of scope here — this primitive is in-memory only).

## Ordering

1. Resolve `OldName` from the receiver's live class-object state (mirrors
   `className/1` — never trust `Self`'s own possibly-stale `class` field).
2. Collision refusal: `NewName` already a loaded class — raised before any
   other check, cheapest and most fundamental ("nothing about this call can
   proceed").
3. Stdlib/dependency refusal, reusing `capture_class_removal_snapshot/1`'s
   existing flushability classification (no new stdlib/dependency/dynamic
   detection logic — see that function's own doc). Still read-only.
4. Site discovery (`discover_rename_sites/3`) — read-only: computes the
   union of `referencesTo:`/`direct_subclasses/1` translated into
   `beamtalk_repl_loader:rewrite_site()` maps, plus the class's own
   declaration-header span as the definition site.
5. Mutate: `rewrite_sites/2` (shared mechanism, BT-3270) installs every site
   transactionally; then `install_class_rename/3` moves the registry
   identity from `OldName` to `NewName` (see its own doc for the
   dynamic-vs-compiled split).
6. Best-effort ChangeLog append (`log_class_rename/4`), mirroring
   `log_local_removal/3`'s placement — the rename is already live in memory
   by this point, so a logging failure must never surface to the caller.

Raises a structured `#beamtalk_error{}` for the collision/stdlib/dependency
refusals and for a `rewrite_sites/2` failure (validation or partial
install); returns the receiver, re-pointed at the newly-installed class
object, on success.
""".
-spec classRenameTo(#beamtalk_object{}, atom()) -> #beamtalk_object{}.
classRenameTo(Self, NewName) when is_atom(NewName) ->
    ClassPid = erlang:element(4, Self),
    OldName = gen_server:call(ClassPid, class_name),
    ok = ensure_rename_collision_free(OldName, NewName),
    OldNameBin = atom_to_binary(OldName, utf8),
    Classification = capture_class_removal_snapshot(OldNameBin),
    ok = ensure_class_renamable(OldName, Classification),
    NewNameBin = atom_to_binary(NewName, utf8),
    {DefinitionSite, ReferenceSites} =
        discover_rename_sites(OldName, OldNameBin, NewNameBin, Classification),
    case rewrite_class_sites(OldName, DefinitionSite, ReferenceSites) of
        {ok, RewriteResult} ->
            NewPid = install_class_rename(OldName, NewName, Classification),
            log_class_rename(OldNameBin, NewNameBin, Classification, RewriteResult),
            beamtalk_class_registry:class_object_from_pid(NewPid);
        {error, Reason} ->
            beamtalk_error:raise(rename_rewrite_failed_error(OldName, Reason))
    end.

%% Collision refusal (ADR 0114 § Decision): `renameTo: #Existing` when
%% `Existing` already names a loaded class raises rather than silently
%% overwriting — exact hint text from the ADR's own worked example.
-spec ensure_rename_collision_free(atom(), atom()) -> ok.
ensure_rename_collision_free(OldName, NewName) ->
    case beamtalk_class_registry:whereis_class(NewName) of
        undefined -> ok;
        _Pid -> beamtalk_error:raise(rename_collision_error(OldName, NewName))
    end.

-spec rename_collision_error(atom(), atom()) -> #beamtalk_error{}.
rename_collision_error(OldName, NewName) ->
    OldBin = atom_to_binary(OldName, utf8),
    NewBin = atom_to_binary(NewName, utf8),
    Error0 = beamtalk_error:new(class_already_exists, OldName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"cannot rename ">>,
            OldBin,
            <<" to ">>,
            NewBin,
            <<" — "/utf8>>,
            NewBin,
            <<" already exists">>
        ])
    ),
    beamtalk_error:with_hint(Error1, <<"remove or rename the existing class first">>).

%% Stdlib/dependency refusal (ADR 0114 § "Refusal vs flushability"): a
%% dynamic class (`not_flushable_reason: "dynamic"`) and an ordinary project
%% class (`flushable: true`) both proceed; only "stdlib" and "dependency:*"
%% refuse, BEFORE any site discovery or mutation runs.
-spec ensure_class_renamable(atom(), map()) -> ok.
ensure_class_renamable(ClassName, #{not_flushable_reason := <<"stdlib">>}) ->
    beamtalk_error:raise(stdlib_rename_refusal_error(ClassName));
ensure_class_renamable(ClassName, #{not_flushable_reason := <<"dependency:", _/binary>> = Reason}) ->
    beamtalk_error:raise(dependency_rename_refusal_error(ClassName, Reason));
ensure_class_renamable(_ClassName, _Classification) ->
    ok.

-spec stdlib_rename_refusal_error(atom()) -> #beamtalk_error{}.
stdlib_rename_refusal_error(ClassName) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"Cannot rename stdlib class '">>, atom_to_binary(ClassName, utf8), <<"'">>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        <<
            "Stdlib classes are protected and cannot be renamed; the xref "
            "index only covers in-project source, so references outside the "
            "project could never be found and rewritten."
        >>
    ).

-spec dependency_rename_refusal_error(atom(), binary()) -> #beamtalk_error{}.
dependency_rename_refusal_error(ClassName, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"Cannot rename dependency class '">>, atom_to_binary(ClassName, utf8), <<"'">>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"This class is defined outside the project (">>,
            Reason,
            <<
                "); the xref index only covers in-project source, so its "
                "references cannot be found and rewritten safely."
            >>
        ])
    ).

-spec rename_rewrite_failed_error(atom(), term()) -> #beamtalk_error{}.
rename_rewrite_failed_error(OldName, workspace_unavailable) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    beamtalk_error:with_message(
        Error0,
        <<"Workspace not available; renameTo: requires a running workspace">>
    );
rename_rewrite_failed_error(OldName, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    Msg = iolist_to_binary(io_lib:format("Could not rename class: ~p", [Reason])),
    beamtalk_error:with_message(Error0, Msg).

%%% ----------------------------------------------------------------------------
%%% Site discovery (ADR 0114 § "renameTo: rewrites cross-file references")
%%% ----------------------------------------------------------------------------

%% Full site list: the union of `referencesTo:`/`direct_subclasses/1`
%% translated into `rewrite_site()` maps, plus the class's own declaration
%% header as the definition site. `referencesTo:`/`direct_subclasses/1` only
%% say WHICH method/header mentions `OldName` (owner, side, selector) — they
%% carry no byte span — so each hit here is independently re-resolved to an
%% exact byte span via `beamtalk_compiler:resolve_method_span/4` /
%% `resolve_class_span/2` (both already-shipped, ADR 0082) rather than
%% reimplementing span resolution. A dynamic class (no `source_file` in
%% `Classification`) has no declaration to rewrite, matching the ChangeLog
%% schema's `sites[0] = null` case.
-spec discover_rename_sites(atom(), binary(), binary(), map()) ->
    {map() | undefined, [map()]}.
discover_rename_sites(OldName, OldNameBin, NewNameBin, Classification) ->
    DefinitionSite = definition_rewrite_site(OldNameBin, NewNameBin, Classification),
    ReferenceSites =
        reference_rewrite_sites(OldName, NewNameBin) ++
            subclass_header_rewrite_sites(OldName, OldNameBin, NewNameBin),
    {DefinitionSite, ReferenceSites}.

-spec definition_rewrite_site(binary(), binary(), map()) ->
    map() | undefined.
definition_rewrite_site(OldNameBin, NewNameBin, #{source_file := SourceFile}) ->
    case current_class_source(OldNameBin) of
        undefined ->
            undefined;
        Source ->
            Pattern = <<"subclass:\\s*(", OldNameBin/binary, ")\\b">>,
            case header_token_span(Source, OldNameBin, Pattern) of
                {ok, Span} ->
                    #{
                        class => OldNameBin,
                        source_file => SourceFile,
                        span => Span,
                        new_text => NewNameBin
                    };
                not_found ->
                    undefined
            end
    end;
definition_rewrite_site(_OldNameBin, _NewNameBin, _Classification) ->
    %% Dynamic class: no backing source (ChangeLog schema's sites[0] = null).
    undefined.

%% Every direct subclass's own declaration header names `OldName` as its
%% superclass (`OldName subclass: Sub`) — the one reference kind
%% `referencesTo:` doesn't cover (ADR 0114 § Decision).
-spec subclass_header_rewrite_sites(atom(), binary(), binary()) ->
    [map()].
subclass_header_rewrite_sites(OldName, OldNameBin, NewNameBin) ->
    Subs = beamtalk_class_registry:direct_subclasses(OldName),
    lists:filtermap(
        fun(Sub) -> subclass_header_rewrite_site(Sub, OldNameBin, NewNameBin) end,
        Subs
    ).

-spec subclass_header_rewrite_site(atom(), binary(), binary()) ->
    {true, map()} | false.
subclass_header_rewrite_site(Sub, OldNameBin, NewNameBin) ->
    SubBin = atom_to_binary(Sub, utf8),
    case current_class_source(SubBin) of
        undefined ->
            false;
        Source ->
            Pattern = <<"(", OldNameBin/binary, ")\\s+subclass:">>,
            case header_token_span(Source, SubBin, Pattern) of
                {ok, Span} ->
                    {true, #{
                        class => SubBin,
                        source_file => class_source_file_for(SubBin),
                        span => Span,
                        new_text => NewNameBin
                    }};
                not_found ->
                    false
            end
    end.

%% Resolve `ClassNameBin`'s declaration-header + state-declaration span
%% (`beamtalk_compiler:resolve_class_span/2`, ADR 0082/BT-3248 — deliberately
%% never a method body) and search `Pattern` WITHIN that slice only, not the
%% whole file — a doc comment example mentioning the identical header text
%% (common in stdlib doc comments) must never be mistaken for the real
%% declaration. Returns the matched capture group's span translated back to
%% absolute offsets into `Source`.
-spec header_token_span(binary(), binary(), binary()) ->
    {ok, map()} | not_found.
header_token_span(Source, ClassNameBin, Pattern) ->
    case class_header_span(Source, ClassNameBin) of
        {ok, {HStart, HEnd}} ->
            HeaderText = binary:part(Source, HStart, HEnd - HStart),
            case re:run(HeaderText, Pattern, [{capture, [1], index}]) of
                {match, [{Start, Len}]} ->
                    {ok, #{start => HStart + Start, 'end' => HStart + Start + Len}};
                nomatch ->
                    not_found
            end;
        not_found ->
            not_found
    end.

-spec class_header_span(binary(), binary()) ->
    {ok, {non_neg_integer(), non_neg_integer()}} | not_found.
class_header_span(Source, ClassNameBin) ->
    try erlang:apply(beamtalk_compiler, resolve_class_span, [Source, ClassNameBin]) of
        {ok, #{start := S, 'end' := E}, _PrevSource} -> {ok, {S, E}};
        {error, _Reason, _Message} -> not_found
    catch
        error:undef -> not_found
    end.

%% Every `{owner, class_side, method}` triple `referencesTo:` reports for
%% `OldName`, deduped across its possibly-multiple line-rows (ADR 0114
%% site-discovery spike finding #2: two mentions on two different lines of
%% the same method are two rows; this only needs the method once).
-spec reference_rewrite_sites(atom(), binary()) -> [map()].
reference_rewrite_sites(OldName, NewNameBin) ->
    Sites = beamtalk_xref:references_to(OldName),
    OldNameBin = atom_to_binary(OldName, utf8),
    Triples = lists:usort([
        {Owner, ClassSide, Method}
     || #{owner := Owner, class_side := ClassSide, method := Method} <- Sites
    ]),
    lists:flatmap(
        fun({Owner, ClassSide, Method}) ->
            method_body_rewrite_sites(Owner, ClassSide, Method, OldNameBin, NewNameBin)
        end,
        Triples
    ).

%% Every whole-word occurrence of `OldNameBin` within `{Owner, ClassSide,
%% Method}`'s own resolved byte span (`resolve_method_span/4`) becomes its
%% own rewrite site — a method can mention the renamed class more than once
%% (e.g. a param type AND a return type, spike finding #2's `Duration>>+`
%% case), each needing its own splice.
-spec method_body_rewrite_sites(atom(), boolean(), atom(), binary(), binary()) ->
    [map()].
method_body_rewrite_sites(Owner, IsClassSide, Method, OldNameBin, NewNameBin) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case current_class_source(OwnerBin) of
        undefined ->
            [];
        Source ->
            SelectorBin = atom_to_binary(Method, utf8),
            %% `beamtalk_xref:site()`'s `class_side` field is a boolean (`true`
            %% = class-side, `false` = instance-side) — translate to the
            %% `instance | class` atom `resolve_method_span/4` expects.
            Side =
                case IsClassSide of
                    true -> class;
                    false -> instance
                end,
            case method_token_span(Source, OwnerBin, SelectorBin, Side) of
                {ok, {MStart, MEnd}} ->
                    MethodText = binary:part(Source, MStart, MEnd - MStart),
                    SourceFile = class_source_file_for(OwnerBin),
                    [
                        #{
                            class => OwnerBin,
                            source_file => SourceFile,
                            span => #{start => MStart + LStart, 'end' => MStart + LEnd},
                            new_text => NewNameBin
                        }
                     || {LStart, LEnd} <- word_occurrence_spans(MethodText, OldNameBin)
                    ];
                not_found ->
                    []
            end
    end.

-spec method_token_span(binary(), binary(), binary(), instance | class) ->
    {ok, {non_neg_integer(), non_neg_integer()}} | not_found.
method_token_span(Source, OwnerBin, SelectorBin, ClassSide) ->
    try
        erlang:apply(beamtalk_compiler, resolve_method_span, [
            Source, OwnerBin, SelectorBin, ClassSide
        ])
    of
        {ok, #{start := S, 'end' := E}, _PrevSource} -> {ok, {S, E}};
        {error, _Reason, _Message} -> not_found
    catch
        error:undef -> not_found
    end.

%% Every non-overlapping whole-word occurrence of `WordBin` in `Text`, as
%% `{Start, End}` byte offsets relative to `Text`'s own start.
-spec word_occurrence_spans(binary(), binary()) -> [{non_neg_integer(), non_neg_integer()}].
word_occurrence_spans(Text, WordBin) ->
    Pattern = <<"\\b", WordBin/binary, "\\b">>,
    case re:run(Text, Pattern, [global, {capture, first, index}]) of
        {match, Matches} -> [{S, S + L} || [{S, L}] <- Matches];
        nomatch -> []
    end.

%% `ClassNameBin`'s CURRENT tracked source (`beamtalk_workspace_meta:
%% get_class_source/1` — the in-memory-merged text `rewrite_sites/2` itself
%% splices against, per that function's own doc; NOT necessarily identical
%% to what's on disk). Routed via `erlang:apply/3` to avoid a compile-time
%% dependency from `beamtalk_runtime` to `beamtalk_workspace`, the same
%% indirection every other cross-app call in this module already uses.
-spec current_class_source(binary()) -> binary() | undefined.
current_class_source(ClassNameBin) ->
    try erlang:apply(beamtalk_workspace_meta, get_class_source, [ClassNameBin]) of
        Source when is_list(Source) -> unicode:characters_to_binary(Source);
        undefined -> undefined
    catch
        error:undef -> undefined
    end.

%% `ClassNameBin`'s on-disk source file, for a `rewrite_site()`'s
%% `source_file` ChangeLog-attribution field only (see that type's doc) —
%% reuses `beamtalk_repl_loader:class_source_file/1` (already exported for
%% exactly this kind of cross-module reuse, BT-3238) rather than re-deriving
%% it. `undefined` for a class with no backing file.
-spec class_source_file_for(binary()) -> binary() | undefined.
class_source_file_for(ClassNameBin) ->
    try erlang:apply(beamtalk_repl_loader, class_source_file, [ClassNameBin]) of
        SourceFile when is_binary(SourceFile) -> SourceFile;
        nil -> undefined
    catch
        error:undef -> undefined
    end.

%%% ----------------------------------------------------------------------------
%%% Mutation + registry re-registration
%%% ----------------------------------------------------------------------------

%% Thin `erlang:apply/3` forwarding to the shared multi-site rewrite
%% mechanism (BT-3270) — same indirection every other cross-app call in this
%% module already uses. A dynamic class with neither a definition site nor
%% any reference site (never mentioned anywhere in-project) has nothing to
%% rewrite at all; `rewrite_sites/2` itself refuses that shape
%% (`{error, no_sites}`) since it is normally a caller bug, but here it is
%% the ordinary, legitimate "freestanding dynamic class" case, so it is
%% special-cased to a trivial success rather than surfaced as an error.
-spec rewrite_class_sites(atom(), map() | undefined, [map()]) ->
    {ok, map()} | {error, term()}.
rewrite_class_sites(_OldName, undefined, []) ->
    {ok, #{definition => undefined, sites => []}};
rewrite_class_sites(_OldName, DefinitionSite, ReferenceSites) ->
    try
        erlang:apply(beamtalk_repl_eval, rewrite_sites, [DefinitionSite, ReferenceSites])
    catch
        error:undef -> {error, workspace_unavailable}
    end.

-doc """
Move the class registry identity from `OldName` to `NewName` after a
successful `rewrite_sites/2` install, returning the pid now serving
`NewName`.

Ordinary (project) class: the definition site's recompile already installed
a fresh class-object process under `NewName` as an ordinary side effect of
the standard `activate_module/4` -> `register_class/0` ->
`beamtalk_class_builder:register/1` pipeline (a NEW pid — hot-reload only
reuses the SAME pid when the registered name is unchanged, which a rename by
definition is not). This branch therefore only needs to retire `OldName`:
stop its now-orphaned class-object gen_server (`terminate/2` cleans up the
ETS hierarchy entry, pg group, and loaded-class/backing-module indexes —
NOT actors, per ADR 0114 Constraint 3: existing instances dispatch via
`class_mod` bound at spawn, never by looking the class up by its registered
name) and purge the four name-keyed derived registries
(`beamtalk_class_lifecycle:purge_class_registries/1`, reused rather than
`class_removed/2`'s full teardown — see that function's doc for why
`purge_protocol/1`'s module-keyed purge must NOT run here: the SAME BEAM
module atom still backs the renamed class, in-memory-only, no disk flush
(BT-3271)).

Dynamic (`ClassBuilder`) class: nothing above ever ran (no source, no
`rewrite_sites/2` recompile) — `beamtalk_object_class:rename/2` moves the
SAME live process to the new registered name in place.
""".
-spec install_class_rename(atom(), atom(), map()) -> pid().
install_class_rename(OldName, NewName, #{not_flushable_reason := <<"dynamic">>}) ->
    case beamtalk_object_class:rename(OldName, NewName) of
        {ok, Pid} ->
            Pid;
        {error, Reason} ->
            Error0 = beamtalk_error:new(runtime_error, OldName),
            Msg = iolist_to_binary(
                io_lib:format(
                    "Renamed in the xref/ChangeLog sense, but the dynamic class's own registration could not move: ~p",
                    [
                        Reason
                    ]
                )
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    end;
install_class_rename(OldName, NewName, _Classification) ->
    case beamtalk_class_registry:whereis_class(OldName) of
        undefined ->
            ok;
        OldPid ->
            beamtalk_class_monitor:unwatch(OldName),
            gen_server:stop(OldPid)
    end,
    ok = beamtalk_class_lifecycle:purge_class_registries(OldName),
    beamtalk_class_registry:whereis_class(NewName).

%% Best-effort ChangeLog append after a successful `renameTo:` (ADR 0114 §
%% ChangeLog schema, `kind: "rename-class"`) — mirrors `log_local_removal/3`'s
%% placement and self-swallowing failure handling: the rename is already
%% live in memory by this point, so a logging failure must never surface to
%% the caller. `side`/`selector`/`old_selector` are absent (`null`) — a
%% class rename has no method-level target, matching the schema.
-spec log_class_rename(binary(), binary(), map(), map()) -> ok.
log_class_rename(OldNameBin, NewNameBin, Classification, RewriteResult) ->
    {Author, AuthorKind} = current_author_context(),
    Spec = #{
        kind => 'rename-class',
        class => NewNameBin,
        old_class => OldNameBin,
        old_path => maps:get(source_file, Classification, undefined),
        new_path => undefined,
        intent => durable,
        author => Author,
        author_kind => AuthorKind
    },
    try
        erlang:apply(beamtalk_repl_eval, emit_rewrite_change_entry, [Spec, RewriteResult])
    catch
        error:undef -> ok
    end,
    ok.

%% Structured `selector_not_found` error for the bare `removeSelector:` form
%% (ADR 0112 § Error behaviour on absent selector) — deliberately distinct
%% from `does_not_understand`: the message itself (`removeSelector:`) was
%% understood and executed; only its *argument* had nothing to act on. Carries
%% a hint pointing at the discovery/escape-hatch methods a caller most likely
%% wants next.
-spec selector_not_found_error(#beamtalk_object{}, atom()) -> #beamtalk_error{}.
selector_not_found_error(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = gen_server:call(ClassPid, class_name),
    Error0 = beamtalk_error:new(selector_not_found, ClassName, Selector),
    Msg = iolist_to_binary([
        atom_to_binary(ClassName, utf8),
        <<" does not define #">>,
        atom_to_binary(Selector, utf8),
        <<" locally (or as an extension)">>
    ]),
    Error1 = beamtalk_error:with_message(Error0, Msg),
    beamtalk_error:with_hint(
        Error1,
        <<
            "Use includesSelector: or whichClassIncludesSelector: to check "
            "first, or removeSelector:ifAbsent: to supply a fallback."
        >>
    ).

-doc """
Return the source file path for this class, or nil if not set.

BT-845/BT-860: Reads `beamtalk_source` module attribute embedded at compile time.
This is the definitive source-of-truth (survives workspace restarts).
Returns nil for stdlib/bootstrap/ClassBuilder-created classes.
""".
-spec classSourceFile(#beamtalk_object{}) -> binary() | 'nil'.
classSourceFile(Self) ->
    ClassPid = erlang:element(4, Self),
    ModuleName = beamtalk_object_class:module_name_safe(ClassPid),
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
    ModuleName = beamtalk_object_class:module_name_safe(ClassPid),
    SourceFile = beamtalk_reflection:source_file_from_module(ModuleName),
    case SourceFile of
        nil ->
            Error0 = beamtalk_error:new(no_source_file, ClassName),
            Msg = iolist_to_binary([
                atom_to_binary(ClassName, utf8),
                <<" has no source file; stdlib classes cannot be reloaded">>
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
                            <<"Workspace not available; reload requires a running workspace">>
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
                        "Workspace not available; live method editing requires a "
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
                        "Workspace not available; pre-save precheck requires a "
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

BT-1186: Previously used gen_server:call(Pid, superclass) directly (BT-1185
fixed apply_class_info/2 to update the gen_server superclass from
__beamtalk_meta/0, so the gen_server always held the correct superclass).

BT-3107: Now uses `beamtalk_object_class:superclass_safe/1` — the same
unified-metadata ETS source `beamtalk_class_dispatch` reads — instead of a
separate `gen_server:call`, so this and dispatch can never disagree mid-reload.

BT-2217: Grounds the parallel chain at `ProtoObject class superclass == Class`
(ADR 0036). When the lookup reports no superclass, return the instance-side
`Class` class object. From there, subsequent `superclass` sends route through
the regular instance-side dispatch (`classSuperclass`), unfolding
`Class → Behaviour → Object → ProtoObject` via the standard walker. `Class`
may be absent during early bootstrap; `atom_to_class_object/1` returns `nil`
in that case, preserving the pre-grounding behaviour.
""".
-spec metaclassSuperclass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassSuperclass(Self) ->
    Pid = erlang:element(4, Self),
    case beamtalk_object_class:superclass_safe(Pid) of
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
            Module = beamtalk_object_class:module_name_safe(CPid),
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

When the chain is exhausted (none or unregistered class), returns the fully
folded accumulator built up through every ancestor visited.

When the walk instead exhausts `?MAX_HIERARCHY_DEPTH` (a hierarchy cycle),
returns the partial accumulator folded up through every ancestor actually
visited before the guard tripped (BT-3096) — not the original `Acc` passed
into this call — and logs a `?LOG_WARNING` naming the ancestor where the
cycle was detected. This matches the hand-rolled recursion this function
replaced, which also returned the partial fold on depth exhaustion. Only
reachable via an actual hierarchy cycle or a legitimately
`?MAX_HIERARCHY_DEPTH`-level-deep hierarchy.

BT-3087: The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; this function supplies only the
per-ancestor registry lookup and threads Acc through the walk. Since
`walk_ancestors/3` only threads a bare node id between steps (not a
separate accumulator), Acc rides along inside the node as `{ClassName, Acc}`.
BT-3096: this is also why `max_depth_exceeded` carries `LastNode` back to
the caller — it's the only way to recover the partial `Acc` folded up to
that point, since it rode along inside the node the whole time.
""".
-spec walk_hierarchy(atom() | none, fun((atom(), pid(), Acc) -> {cont, Acc} | {halt, Result}), Acc) ->
    Acc | Result.
walk_hierarchy(none, _Fun, Acc) ->
    Acc;
walk_hierarchy(ClassName, Fun, Acc) ->
    StepFun = fun({CurrentClassName, CurrentAcc}, _Depth) ->
        case beamtalk_class_registry:whereis_class(CurrentClassName) of
            undefined ->
                {found, {result, CurrentAcc}};
            ClassPid ->
                case Fun(CurrentClassName, ClassPid, CurrentAcc) of
                    {halt, Result} ->
                        {found, {result, Result}};
                    {cont, NewAcc} ->
                        %% BT-3107: deadlock-safe metadata lookup, same source
                        %% dispatch reads, instead of a separate gen_server:call.
                        case beamtalk_object_class:superclass_safe(ClassPid) of
                            none -> {found, {result, NewAcc}};
                            Super -> {next, {Super, NewAcc}}
                        end
                end
        end
    end,
    case beamtalk_hierarchy:walk_ancestors({ClassName, Acc}, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, {result, Result}} ->
            Result;
        {max_depth_exceeded, {CycleClassName, PartialAcc}} ->
            ?LOG_WARNING(
                "walk_hierarchy: max hierarchy depth ~p exceeded at ~p — possible cycle",
                [?MAX_HIERARCHY_DEPTH, CycleClassName],
                #{domain => [beamtalk, runtime]}
            ),
            PartialAcc;
        not_found ->
            %% Unreachable: StepFun above always resolves to {found, _} — a
            %% `none` superclass or an unregistered ancestor is translated to
            %% a terminal {found, {result, _}}, never a bare `none` node.
            erlang:error({unreachable, not_found, ClassName})
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

BT-3081: delegates to `beamtalk_module_name:is_stdlib_module/1`, the single
authority for this check (was byte-identical to
`beamtalk_class_registry:is_stdlib_module/1`).
""".
-spec is_stdlib_module_name(atom()) -> boolean().
is_stdlib_module_name(Module) ->
    beamtalk_module_name:is_stdlib_module(Module).

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
