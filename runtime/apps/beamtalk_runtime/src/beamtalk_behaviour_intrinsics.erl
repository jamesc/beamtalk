%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_behaviour_intrinsics).

%%% **DDD Context:** Object System Context

-moduledoc """
Thin Behaviour/Class intrinsics (ADR 0032 Phase 2).

These functions back the `@primitive "classXxx"` declarations in `lib/behaviour.bt`
and `lib/class.bt`. Each function receives a class object `Self`
(a `#beamtalk_object{}` tuple with a ClassPid at position 4) and exposes
either raw data from the class gen_server state / registry or small,
hierarchy-aware queries over that data.

Richer hierarchy-walking and protocol logic still lives in Beamtalk-level
code; this module provides a minimal, side-effect-free intrinsic surface
that the Behaviour/Class libraries can rely on.

## Intrinsic Table

| Erlang function             | Backing data source / derivation                          |
|-----------------------------|------------------------------------------------------------|
| classSuperclass/1           | Direct superclass from unified class metadata (ETS) |
| classAllSuperclasses/1      | Recursively walks the superclass chain from classSuperclass/1 |
| classSubclasses/1           | Direct subclasses from class registry                     |
| classAllSubclasses/1        | All subclasses from class registry                        |
| classLocalMethods/1         | Local method dictionary from class gen_server state       |
| classMethods/1              | Combined local + inherited methods via superclass chain   |
| classIncludesSelector/2     | Membership check in local method dictionary               |
| classFieldNames/1           | Field names from class gen_server state                   |
| classAllFieldNames/1        | Combined field names via superclass chain                 |
| classAllFieldNamesByName/1  | Same as classAllFieldNames/1, by ClassName (no instance needed) |
| classAllFieldTypesByName/1  | Flattened field name -> declared type atom (ADR 0123 Phase 2) |
| classAllFieldHasDefaultByName/1 | Flattened field name -> has-default boolean (ADR 0123 Phase 2) |
| classFieldKinds/1            | Field name -> eager/late kind, local only (ADR 0124 §9/B5b) |
| classAllFieldKinds/1         | Combined field name -> eager/late kind via superclass chain (ADR 0124 §9/B5b) |
| classAllFieldKindsByName/1   | Same as classAllFieldKinds/1, by ClassName (ADR 0124 §9/B5b)  |
| classClassVarNames/1        | Class-side field names (class variables) from class meta   |
| classAllClassVarNames/1     | Combined class-side field names via superclass chain       |
| className/1                 | Class name from class gen_server state                    |
| classClass/1                | Real metaclass object (ADR 0036)                          |
| classDoc/1                  | Class doc string from published metadata (ADR 0033)       |
| classSetDoc/2               | Set class doc string (ADR 0033)                           |
| classSetMethodDoc/3         | Set method doc string for a selector (ADR 0033)           |
| classDocForMethod/2         | Get method doc string for a selector, or nil               |
| classRemoveFromSystem/1     | Remove class and cleanup runtime state                    |
| classRemoveSelector/2       | Moved to `beamtalk_class_refactor` (BT-4703)              |
| classRemoveSelectorIfAbsent/3 | Moved to `beamtalk_class_refactor` (BT-4703)            |
| classSourceFile/1           | Source file path from beamtalk_source module attr          |
| classReload/1               | Recompile from sourceFile + hot-swap                        |
| classConformsTo/2           | Check if class conforms to a protocol (ADR 0068 Phase 2c) |
| classProtocols/1            | List protocols the class conforms to (ADR 0068 Phase 2c)  |
| classUsedProtocols/1        | This class's own `uses:` list, in declaration order (ADR 0127 §12) |
| classRenameTo/2             | Moved to `beamtalk_class_refactor` (BT-4703)              |
| classRenameSelector/3       | Moved to `beamtalk_selector_refactor` (BT-4703)           |
| classRenameSelectorIfAbsent/4 | Moved to `beamtalk_selector_refactor` (BT-4703)         |
| classShapeVersion/1         | Declared shapeVersion: N from class meta, default 1 (ADR 0123 §1) |
| classMigrateShapeFrom/3     | Run the migrateFromVN: chain + reconcile (ADR 0123 §3, wraps beamtalk_shape_migration:migrate/3) |
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
    %% BT-4703: the compiled-methods-only membership check, shared with
    %% beamtalk_class_refactor and beamtalk_selector_refactor (it needs
    %% `meta_for_module/1`, which stays here with the introspection code).
    compiled_includes_selector/2,
    classCanUnderstandFromName/2,
    selectorProviderFromName/2,
    overridesCollectionSelectorFromName/1,
    speciesConstructionOverriddenFromName/2,
    classFieldNames/1,
    classAllFieldNames/1,
    classAllFieldNamesByName/1,
    %% ADR 0123 Phase 2 (BT-3536): flattened per-field declared metadata,
    %% for beamtalk_shape_migration's reconcile and pack/1 tier walk.
    classAllFieldTypesByName/1,
    classAllFieldHasDefaultByName/1,
    %% ADR 0124 §9/B5b (BT-3550): field name -> eager|late kind reflection
    classFieldKinds/1,
    classAllFieldKinds/1,
    classAllFieldKindsByName/1,
    %% class-side field (class variable) reflection
    classClassVarNames/1,
    classAllClassVarNames/1,
    className/1,
    classClass/1,
    %% BT-4065: `Object>>isKindOf:` in the caller, no class-process sends
    objectIsKindOf/2,
    primitiveIsKindOf/2,
    isKindOfBuiltin/3,
    isKindOfNamed/2,
    %% BT-4319: `binding :: Class` match arm on a class with subclasses
    valueIsKindOfClass/2,
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
    %% Class removal
    classRemoveFromSystem/1,
    %% Programmatic class removal by name (for workspace/MCP unload)
    classRemoveFromSystemByName/1,
    %% ADR 0040 Phase 2 — class-based reload
    classSourceFile/1,
    classReload/1,
    %% ADR 0068 Phase 2c: Runtime protocol queries
    classConformsTo/2,
    classProtocols/1,
    %% ADR 0127 §12: trait `uses:` reflection
    classUsedProtocols/1,
    %% ADR 0123 §1/§3: shape versioning
    classShapeVersion/1,
    classMigrateShapeFrom/3,
    %% ADR 0079: exposed for cross-module hierarchy checks
    walk_hierarchy/3,
    publish_class_removed/2
]).

-ifdef(TEST).
-export([plain_value/1, kind_of_builtin/4, kind_of_named/4]).
-endif.

%%% ============================================================================
%%% Public Intrinsics
%%% ============================================================================

-doc """
Return the superclass of the receiver as a class object, or nil for roots.

ADR 0032: Returns a proper #beamtalk_object{} instead of a bare atom,
fixing the inconsistency where `Counter class` returned an object but
`Counter superclass` returned an atom.

Uses __beamtalk_meta/0 when available; falls back to the
deadlock-safe metadata lookup (`beamtalk_object_class:superclass_safe/1`)
for dynamic classes created via beamtalk_class_builder — the same
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
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
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
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
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

Uses __beamtalk_meta/0 when available; falls back to the published class metadata (no class-process call, BT-3826)
for dynamic classes created via beamtalk_class_builder.

When called on a metaclass object (Foo class methods), returns
user-defined class methods instead of instance methods.
""".
-spec classLocalMethods(#beamtalk_object{}) -> [atom()].
classLocalMethods(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    %% Metaclass receiver — return user-defined class methods.
    beamtalk_object_class:local_class_method_selectors_safe(ClassPid);
classLocalMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:keys(maps:get(method_info, Meta));
        not_available ->
            beamtalk_object_class:instance_methods_safe(ClassPid)
    end.

-doc """
Return all superclasses of the receiver in order (immediate parent to root).

For metaclass receivers (objects tagged `'Metaclass'`)
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
    %% walk the parallel metaclass hierarchy (returns metaclass objects).
    %% deadlock-safe metadata lookup, same source dispatch reads.
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
    %% Ground the parallel chain into the instance-side `Class` tower
    %% so the metaclass hierarchy merges with `Class → Behaviour → Object →
    %% ProtoObject` (ADR 0036). `Class` may be absent during early bootstrap;
    %% walk_hierarchy/3 returns the initial accumulator in that case.
    InstanceSupers = walk_hierarchy(
        'Class',
        fun(CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            Tag = beamtalk_class_registry:class_object_tag(CN),
            ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = CPid},
            {cont, [ClassObj | Acc]}
        end,
        []
    ),
    lists:reverse(MetaSupers) ++ lists:reverse(InstanceSupers);
classAllSuperclasses(Self) ->
    ClassPid = erlang:element(4, Self),
    %% deadlock-safe metadata lookup, same source dispatch reads.
    SuperName = beamtalk_object_class:superclass_safe(ClassPid),
    Supers = walk_hierarchy(
        SuperName,
        fun(CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            Tag = beamtalk_class_registry:class_object_tag(CN),
            ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = CPid},
            {cont, [ClassObj | Acc]}
        end,
        []
    ),
    lists:reverse(Supers).

-doc """
Return all method selectors understood by instances (full inheritance chain).

Uses __beamtalk_meta/0 at each hierarchy level when available;
falls back to the published class metadata for dynamic classes (BT-3826).

When called on a metaclass object (Foo class allMethods), walks
the hierarchy collecting user-defined class methods at each level instead
of instance methods.
""".
-spec classMethods(#beamtalk_object{}) -> [atom()].
classMethods(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    %% Metaclass receiver — collect class methods up the hierarchy.
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            ClassMethods = beamtalk_object_class:local_class_method_selectors_safe(CPid),
            {cont, ordsets:union(A, ordsets:from_list(ClassMethods))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc);
classMethods(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            Methods =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:keys(maps:get(method_info, Meta));
                    not_available ->
                        beamtalk_object_class:instance_methods_safe(CPid)
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
Who answers `Selector` for instances of the class named ClassName (BT-3916).

Walks the hierarchy exactly as `classCanUnderstandFromName/2` does and answers
`stdlib` when the first class on the chain that defines the selector is a
stdlib module, `user` when it is any other module (unless the method is a provision of a
stdlib protocol the class `uses:`, recorded in its meta) or a registered
extension method, and `none` when nothing on the chain defines it. The receiver guard of
an inlined list-op fold (`beamtalk_collection:to_list_for_threading/2`) reads
this to tell "the stdlib's own `inject:into:`, which is the fold over `do:`"
from a class's own method the fold would otherwise bypass.
""".
-spec selectorProviderFromName(atom(), atom()) -> stdlib | user | none.
selectorProviderFromName(ClassName, Selector) ->
    walk_hierarchy(
        ClassName,
        fun(CN, CPid, _Acc) ->
            case beamtalk_dispatch:check_extension(CN, Selector) of
                {ok, _Fun} ->
                    {halt, user};
                not_found ->
                    case defines_selector_locally(CN, CPid, Selector) of
                        true ->
                            Module = beamtalk_object_class:module_name_safe(CPid),
                            case
                                is_stdlib_module_name(Module) orelse
                                    is_stdlib_trait_provision(Module, Selector)
                            of
                                true -> {halt, stdlib};
                                false -> {halt, user}
                            end;
                        false ->
                            {cont, none}
                    end
            end
        end,
        none
    ).

%% Does this exact class (not an ancestor) define `Selector`? The published
%% local selector list answers it. A compiled value class's `has_method/1`
%% delegates to its superclass's module, and `has_method_local/2` falls back to
%% that probe for a module without `has_method_local/1` (a value class compiled
%% before BT-4260), so it would credit an inherited stdlib method to the
%% user's subclass.
-spec defines_selector_locally(atom(), pid(), atom()) -> boolean().
defines_selector_locally(ClassName, ClassPid, Selector) ->
    case beamtalk_class_metadata:lookup_instance_methods(ClassName) of
        {ok, Selectors} -> lists:member(Selector, Selectors);
        not_found -> beamtalk_object_class:has_method_local(ClassPid, Selector)
    end.

%% A provision of a stdlib protocol (`uses: Enumerable`) is flattened into the
%% using class's own module (ADR 0127), so the module is a package's, but the
%% method is the stdlib's. Codegen records those selectors in the class meta
%% (`stdlib_trait_methods`, BT-3916); a class without the key (older build,
%% dynamic class) has none.
-spec is_stdlib_trait_provision(atom(), atom()) -> boolean().
is_stdlib_trait_provision(Module, Selector) ->
    case meta_for_module(Module) of
        {ok, Meta} ->
            lists:member(Selector, maps:get(stdlib_trait_methods, Meta, []));
        not_available ->
            false
    end.

-doc """
Whether any non-stdlib class on the chain from ClassName defines, outside the
stdlib, a collection message the stdlib's own collection methods run through
(BT-3916).

Reads the `overrides_collection_selector` key codegen bakes into each class's
meta from the compiler's one list of those messages
(`state_threading_selectors::overridable_collection_selectors`). A runtime
extension method (BT-3951) is invisible to that flag, so each class on the chain
is also asked `beamtalk_collection_selectors:extended_on/1`, whose list a Rust
test holds equal to the compiler's. A class without meta (dynamic, older build)
counts as `false`.
""".
-spec overridesCollectionSelectorFromName(atom()) -> boolean().
overridesCollectionSelectorFromName(ClassName) ->
    walk_hierarchy(
        ClassName,
        fun(CN, CPid, _Acc) ->
            %% A runtime extension (BT-3951) is invisible to the compile-time
            %% flag, and may sit on a stdlib class.
            case beamtalk_collection_selectors:extended_on(CN) of
                true ->
                    {halt, true};
                false ->
                    Module = beamtalk_object_class:module_name_safe(CPid),
                    case is_stdlib_module_name(Module) of
                        true ->
                            {cont, false};
                        false ->
                            case meta_for_module(Module) of
                                {ok, #{overrides_collection_selector := true}} -> {halt, true};
                                _ -> {cont, false}
                            end
                    end
            end
        end,
        false
    ).

-doc """
Whether a `species` or class-side `withAll:` of the receiver's class changes
what the stdlib's `Selector` answers (BT-3951).

Only `collect:`, `select:` and `reject:` end in `self species withAll:`
(`beamtalk_collection_selectors:species_dependent/0`), and only the `Collection`
provision does: a selector the class gets from a stdlib trait such as
`Enumerable` forwards to `self elements` and never reaches `species`. So this is
`false` unless the selector is species-dependent, its first definer on the chain
is not a trait provision, and a class on the chain defines a species-construction
message outside the stdlib (the `overrides_species_construction` meta key) or a
runtime extension does. A class without meta counts as `false`.
""".
-spec speciesConstructionOverriddenFromName(atom(), atom()) -> boolean().
speciesConstructionOverriddenFromName(ClassName, Selector) ->
    lists:member(Selector, beamtalk_collection_selectors:species_dependent()) andalso
        reached_through_collection(ClassName, Selector) andalso
        walk_hierarchy(
            ClassName,
            fun(CN, CPid, _Acc) ->
                case beamtalk_collection_selectors:species_construction_extended_on(CN) of
                    true ->
                        {halt, true};
                    false ->
                        Module = beamtalk_object_class:module_name_safe(CPid),
                        case is_stdlib_module_name(Module) of
                            true ->
                                {cont, false};
                            false ->
                                case meta_for_module(Module) of
                                    {ok, #{overrides_species_construction := true}} ->
                                        {halt, true};
                                    _ ->
                                        {cont, false}
                                end
                        end
                end
            end,
            false
        ).

%% Is the first definer of `Selector` on the chain a stdlib class's own method
%% (`Collection`'s), rather than a stdlib trait's provision flattened into a
%% using class? Callers have already ruled out a non-stdlib definer.
-spec reached_through_collection(atom(), atom()) -> boolean().
reached_through_collection(ClassName, Selector) ->
    walk_hierarchy(
        ClassName,
        fun(CN, CPid, _Acc) ->
            case defines_selector_locally(CN, CPid, Selector) of
                true ->
                    Module = beamtalk_object_class:module_name_safe(CPid),
                    {halt, not is_stdlib_trait_provision(Module, Selector)};
                false ->
                    {cont, false}
            end
        end,
        false
    ).

-doc """
Return all field names including inherited, in slot order.

Uses __beamtalk_meta/0 at each hierarchy level when available;
falls back to the published class metadata for dynamic classes (BT-3826).
""".
-spec classAllFieldNames(#beamtalk_object{}) -> [atom()].
classAllFieldNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    classAllFieldNamesByName(ClassName).

-doc """
Same as `classAllFieldNames/1`, taking a class name directly instead of an
instance's class object — for callers that only have a `ClassName` atom
(e.g. `beamtalk_hot_reload`'s BT-3531 field-migration reconcile step, which
needs the flattened field set for a class it never has a
`#beamtalk_object{}` for). Returns `[]` for an unregistered class, same as
`walk_hierarchy/3`'s own empty-chain behavior.
""".
-spec classAllFieldNamesByName(atom()) -> [atom()].
classAllFieldNamesByName(ClassName) ->
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            IVars =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:get(fields, Meta);
                    not_available ->
                        beamtalk_object_class:instance_variables_safe(CPid)
                end,
            {cont, IVars ++ Acc}
        end,
        []
    ).

-doc """
Return the flattened field name → declared type atom map, including
inherited fields (ADR 0123 Phase 2, BT-3536).

Reuses `walk_hierarchy/3` — the same superclass-chain walk
`classAllFieldNamesByName/1` uses — reading each level's own `'field_types'`
key from `__beamtalk_meta/0` (`class_meta.rs`'s `meta_field_types_map`,
`'none'` for an untyped field). `beamtalk_shape_migration:pack/1` walks this
to grade each field's sendability tier before packing.

On a conflict (a field name declared at more than one hierarchy level, which
should not happen in practice), the more-derived class's entry wins — merge
order mirrors `classAllFieldNamesByName/1`'s "ancestor precedes subclass"
walk, with the accumulator (already-visited, more-derived levels) taking
precedence in `maps:merge/2`. Dynamic classes with no `__beamtalk_meta/0`
contribute no entries at their level (same `not_available` degrade as
`classFieldNames/1`); returns `#{}` for an unregistered class.
""".
-spec classAllFieldTypesByName(atom()) -> #{atom() => atom()}.
classAllFieldTypesByName(ClassName) ->
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            FieldTypes =
                case meta_for_module(Module) of
                    {ok, Meta} -> maps:get(field_types, Meta, #{});
                    not_available -> #{}
                end,
            {cont, maps:merge(FieldTypes, Acc)}
        end,
        #{}
    ).

-doc """
Return the flattened field name → has-default boolean map, including
inherited fields (ADR 0123 Phase 2, BT-3536).

Same walk and merge-precedence rule as `classAllFieldTypesByName/1`, reading
each level's `'field_has_default'` key (`class_meta.rs`'s
`meta_field_has_default_map`). `beamtalk_shape_migration`'s reconcile step
uses this to tell "declared with no default" (fails on a `typed` class if
still absent after the chain) from "declared with a default" (falls back to
`Module:init/1`'s default) without needing the AST.
""".
-spec classAllFieldHasDefaultByName(atom()) -> #{atom() => boolean()}.
classAllFieldHasDefaultByName(ClassName) ->
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            HasDefault =
                case meta_for_module(Module) of
                    {ok, Meta} -> maps:get(field_has_default, Meta, #{});
                    not_available -> #{}
                end,
            {cont, maps:merge(HasDefault, Acc)}
        end,
        #{}
    ).

-doc """
Return the field name -> kind (`'eager'` | `'late'`) map declared in this
class (not inherited).

Backs `@primitive "classFieldKinds"` (`Behaviour>>fieldKinds`, ADR 0124 §9/B5b)
— the instance-side counterpart to `classFieldNames/1`, reading B5a's
`'field_kinds'` key from `__beamtalk_meta/0` instead of walking the AST.
Instance side only: a class-side (`classState:`) kind map is deliberately
*not* exposed under this name — an instance slot and a class var can share
a name, so folding both into one dictionary would be ambiguous (ADR 0124
§9's B10 documentation pass records the split; `classVarNames/1` is the
existing precedent for keeping class-side reflection under its own
selector rather than merging it into the instance-side one).

Dynamic classes built via `beamtalk_class_builder` carry no static meta,
so they answer `'eager'` for every field they report — ClassBuilder has no
`late`-slot concept — via the same
`beamtalk_object_class:instance_variables_safe/1` fallback `classFieldNames/1` uses.
""".
-spec classFieldKinds(#beamtalk_object{}) -> #{atom() => eager | late}.
classFieldKinds(Self) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:get(field_kinds, Meta, #{});
        not_available ->
            Fields = beamtalk_object_class:instance_variables_safe(ClassPid),
            maps:from_list([{F, eager} || F <- Fields])
    end.

-doc """
Return the flattened field name -> kind (`'eager'` | `'late'`) map,
including inherited fields (ADR 0124 §9/B5b).

Backs `@primitive "classAllFieldKinds"` (`Behaviour>>allFieldKinds`) — the
instance-side counterpart to `classAllFieldNames/1`. Delegates to
`classAllFieldKindsByName/1` for the actual hierarchy walk.
""".
-spec classAllFieldKinds(#beamtalk_object{}) -> #{atom() => eager | late}.
classAllFieldKinds(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    classAllFieldKindsByName(ClassName).

-doc """
Same as `classAllFieldKinds/1`, taking a class name directly instead of an
instance's class object — mirrors `classAllFieldNamesByName/1` /
`classAllFieldTypesByName/1`'s "by name" siblings, for callers (B9's
hot-reload keep set) that only have a `ClassName` atom.

Reuses `walk_hierarchy/3` — the same superclass-chain walk
`classAllFieldTypesByName/1` uses — reading each level's own `'field_kinds'`
key from `__beamtalk_meta/0`. Same merge-precedence rule: on a conflict (a
field redeclared at more than one hierarchy level), the more-derived
class's entry wins.

A dynamic level with no `__beamtalk_meta/0` (ClassBuilder-built) contributes
`'eager'` for every field *it* reports, via that level's own
`beamtalk_object_class:instance_variables_safe(ClassPid)` fallback — same degrade as
`classFieldKinds/1`. Returns `#{}` for an unregistered class.
""".
-spec classAllFieldKindsByName(atom()) -> #{atom() => eager | late}.
classAllFieldKindsByName(ClassName) ->
    walk_hierarchy(
        ClassName,
        fun(_CN, CPid, Acc) ->
            Module = beamtalk_object_class:module_name_safe(CPid),
            FieldKinds =
                case meta_for_module(Module) of
                    {ok, Meta} ->
                        maps:get(field_kinds, Meta, #{});
                    not_available ->
                        Fields = beamtalk_object_class:instance_variables_safe(CPid),
                        maps:from_list([{F, eager} || F <- Fields])
                end,
            {cont, maps:merge(FieldKinds, Acc)}
        end,
        #{}
    ).

-doc """
Test whether the selector is defined locally in this class.

Does NOT check superclasses — local containment only.
Full chain walk for `canUnderstand:` is implemented in pure Beamtalk
(`Behaviour>>canUnderstand:`) on top of `classMethods/1`.

When called on a metaclass object, checks class methods instead
of instance methods.

Uses __beamtalk_meta/0 fast path when available — mirrors the
fast path already in `classLocalMethods/1` and avoids a gen_server
round-trip per call, which matters for the bulk iteration done by
`SystemNavigation implementorsOf:`. Falls back to the published class metadata for dynamic
classes built via ClassBuilder.
""".
-spec classIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
classIncludesSelector(#beamtalk_object{class = 'Metaclass', pid = ClassPid} = Self, Selector) ->
    %% BT-4438: a class-side ADR 0084 runtime fun is a local class method too
    %% (and `removeSelector:` removes it), even when the compiled module has
    %% no method of that name.
    compiled_includes_selector(Self, Selector) orelse
        beamtalk_refactor_support:has_runtime_class_fun(
            beamtalk_object_class:class_name_safe(ClassPid), Selector
        );
classIncludesSelector(Self, Selector) ->
    compiled_includes_selector(Self, Selector).

%% The class's local method table as compiled (or as a builder class
%% published it): `__beamtalk_meta/0` when available, else the published
%% selectors. Excludes a class-side runtime fun installed over a compiled
%% class -- `classIncludesSelector/2` adds that.
-spec compiled_includes_selector(#beamtalk_object{}, atom()) -> boolean().
compiled_includes_selector(#beamtalk_object{class = 'Metaclass', pid = ClassPid}, Selector) ->
    %% Metaclass receiver — check class methods.
    %% Use __beamtalk_meta/0 fast path when available, matching the
    %% instance-side clause below. Avoids per-call gen_server hops during the
    %% bulk iteration done by `SystemNavigation implementorsOf:`.
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:is_key(Selector, maps:get(class_method_info, Meta, #{}));
        not_available ->
            ClassMethods = beamtalk_object_class:local_class_method_selectors_safe(ClassPid),
            lists:member(Selector, ClassMethods)
    end;
compiled_includes_selector(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case meta_for_module(Module) of
        {ok, Meta} ->
            maps:is_key(Selector, maps:get(method_info, Meta));
        not_available ->
            LocalMethods = beamtalk_object_class:instance_methods_safe(ClassPid),
            lists:member(Selector, LocalMethods)
    end.

-doc """
Return the names of fields declared in this class (not inherited).

Uses __beamtalk_meta/0 when available; falls back to the published class metadata (BT-3826)
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
            beamtalk_object_class:instance_variables_safe(ClassPid)
    end.

-doc """
Return the class-side field (class variable) names declared in this class.

Backs `@primitive "classClassVarNames"` (`Behaviour>>classVarNames`)
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

Backs `@primitive "classAllClassVarNames"`
(`Behaviour>>allClassVarNames`) — the class-side counterpart to
`classAllFieldNames/1`. Walks the superclass chain collecting each level's
`class_fields`, mirroring the slot order of `classAllFieldNames/1` (ancestor
class-side slots precede subclass ones).
""".
-spec classAllClassVarNames(#beamtalk_object{}) -> [atom()].
classAllClassVarNames(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
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
    beamtalk_object_class:class_name_safe(ClassPid).

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
`Object>>isKindOf:` — answer `Receiver class includesBehaviour: Behaviour`
without messaging any class process (BT-4065).

The Beamtalk definition, `self class includesBehaviour: aClass`, sends
`includesBehaviour:`, then `inheritsFrom:` and `allSuperclasses`, to the
receiver's class object; each of those is a `gen_server:call` into that class's
process (to rule out a user class method of the same name, which `Behaviour`'s
sealing forbids) before falling through to the `Behaviour` method. Every
`isKindOf:` in the node queued on the receiver's class process.

This computes the same answer from the class table. `includesBehaviour:` is
`self == aBehaviour or: [self allSuperclasses includes: aBehaviour]`, where
`self` is `class_of_object(Receiver)` and `allSuperclasses` is
`classAllSuperclasses/1`:

* an ordinary class `C` (an instance's class): `[C's superclasses]`, as class
  objects;
* a metaclass `C class` (a class object's class): `[the metaclasses of C's
  superclasses] ++ [Class and its superclasses]`, the parallel tower grounded
  at `Class` (ADR 0036).

So for a *canonical* argument — `==` to the class or metaclass object the
registry builds for its name — the answer is `inherits_from/2` on names
(cached in `beamtalk_class_derived_cache`, invalidated on every class write):

| receiver's class | argument     | answer                         |
|------------------|--------------|--------------------------------|
| class `C`        | class `A`    | `inherits_from(C, A)`          |
| class `C`        | metaclass    | `false`                        |
| metaclass `C class` | metaclass `A class` | `inherits_from(C, A)` |
| metaclass `C class` | class `A`  | `inherits_from('Class', A)`    |

Anything else — an argument that is not a canonical class or metaclass object
(a protocol, a stale class object from a dead class process, a
package-qualified tag, a non-class value), a receiver whose class is not
registered, a future — takes the original route,
`includesBehaviour:` sent to `class_of_object(Receiver)`, so its answer
(or error) is unchanged.

One transient difference: `classAllSuperclasses/1` stops at the first
ancestor with no registered process, while `inherits_from/2` walks metadata
rows. The two disagree only while an intermediate ancestor's process has been
killed without `terminate/2` (its row survives) and before its supervisor
restarts it; this answers as if the class were still there.

A class-object receiver (`Counter isKindOf: X`) still reaches this method by a
class-side send; only the `includesBehaviour:`/`inheritsFrom:`/
`allSuperclasses` sends this used to make are gone.

**Instance fast path (BT-4221).** For a receiver that is an ordinary instance
(a primitive, a Value, an actor — anything but a class object, a metaclass or
a future), deciding the table above costs four cache lookups plus two
`class_name_for_pid` reads per call. The decision for an instance of class
`C` against a given behaviour object is a function of `(C, Behaviour)` and the
class table alone, so it is memoised in `beamtalk_class_derived_cache` under
`{is_kind_of, C, Behaviour}` — one lookup per call. Only decided answers
(both sides canonical) are cached, together with the two class pids, and a
hit is used only while both pids are alive: the same liveness rule
`canonical_class_object/1` applies, so a class process killed without a
restart still falls back to the send. Every class write (definition, reload,
restart) bumps the cache generation, which drops the entry, exactly as it
drops the `inherits_from/2` and class-object entries it was derived from.
""".
-spec objectIsKindOf(term(), term()) -> boolean().
objectIsKindOf(Receiver, Behaviour) ->
    case receiver_class_side(Receiver) of
        {instance, C} ->
            instance_is_kind_of(C, Receiver, Behaviour);
        {meta, C} ->
            case behaviour_side(Behaviour) of
                {meta, A} -> beamtalk_class_derived_cache:inherits_from(C, A);
                {class, A} -> beamtalk_class_derived_cache:inherits_from('Class', A);
                other -> is_kind_of_by_send(Receiver, Behaviour)
            end;
        other ->
            is_kind_of_by_send(Receiver, Behaviour)
    end.

-doc """
An `isKindOf:` *send* to a primitive receiver (BT-4473).

`beamtalk_message_dispatch:send/3` routes `Receiver isKindOf: Arg` here when
`Receiver` is a raw-term primitive or a `String`. The full dynamic send walks
the receiver's dispatch chain — its class's `dispatch/3`, an
`inherited_target` lookup, then `Object`'s `dispatch/3` — to arrive at
`Object>>isKindOf:`, i.e. `objectIsKindOf/2`, whose answer is one more
derived-cache lookup.

Here one lookup answers both questions: under `{primitive_is_kind_of, C,
Behaviour}` the derived cache keeps the decided `objectIsKindOf/2` answer for
an instance of `C`, recorded only when nothing on `C`'s chain overrides
`isKindOf:` (no compiled method below `Object` and no extension at any level,
`beamtalk_default_method:owner/2`), and validated on every hit by the same
class-pid liveness check the `is_kind_of` entry uses. Every class write and
every extension (un)registration flushes it. Anything else — an override, an
unpublished chain, an argument that is not a canonical class or metaclass
object — takes the ordinary primitive send.
""".
-spec primitiveIsKindOf(term(), term()) -> term().
primitiveIsKindOf(Receiver, #beamtalk_object{pid = BPid} = Behaviour) when is_pid(BPid) ->
    C = beamtalk_primitive:class_of(Receiver),
    case
        beamtalk_class_derived_cache:fetch(
            {primitive_is_kind_of, C, Behaviour},
            fun() -> decide_primitive_is_kind_of(C, Behaviour) end,
            fun primitive_kind_of_valid/1
        )
    of
        {kind_of, Answer, _, _} -> Answer;
        _SendIt -> beamtalk_primitive:send(Receiver, 'isKindOf:', [Behaviour])
    end;
primitiveIsKindOf(Receiver, Behaviour) ->
    beamtalk_primitive:send(Receiver, 'isKindOf:', [Behaviour]).

-doc """
Runtime test behind a `binding :: Class` match arm where `Class` has subclasses
(BT-4319, ADR 0107 Phase B): is `Value`'s class `Class` or a (transitive)
subclass of it?

Answers `Value isKindOf: Class` for an instance of any representation, from the
class table (`beamtalk_class_derived_cache:inherits_from/2`), without sending a
message: a match arm must not run user code or call into a class process. An
unregistered class name answers `false`.

A future answers `false` without being awaited, so a pending or rejected future
never blocks or raises inside an arm test (the same as an exact-class arm).
""".
-spec valueIsKindOfClass(term(), atom()) -> boolean().
valueIsKindOfClass({beamtalk_future, _}, Class) when is_atom(Class) ->
    false;
valueIsKindOfClass(Value, Class) when is_atom(Class) ->
    beamtalk_class_derived_cache:inherits_from(beamtalk_primitive:class_of(Value), Class).

-doc """
`Receiver isKindOf: Behaviour` where `Behaviour` is a class literal the
compiler resolved to a sealed built-in with an exact representation test
(BT-4473), and `TestAnswer` is that test applied to `Receiver` — the same test
a `binding :: Class` match arm uses, from the shared routing fixture.

For a plain value — anything but an actor, class object or supervisor handle,
a future (any tuple headed by one of those reserved tags), or a map the
runtime treats as tagged (`beamtalk_tagged_map:is_tagged/1`) — the class is one
of the stdlib's built-ins. So while no class other than `Object` has ever
defined `isKindOf:`, compiled or as an extension
(`beamtalk_default_method:overridden_anywhere/1`, which also covers code
compiled against an older stdlib), the send would run `Object>>isKindOf:`, and
its answer is `TestAnswer`: a value passes the test exactly when its class is
`Behaviour` or one of its (stdlib, sealed) subclasses. Everything else takes
the ordinary send, unchanged.

The compiled caller (BT-4764) answers inline when the receiver's test passes
and, for a receiver that is neither a tuple nor a map, when it fails; it checks
that the class process is alive first, and takes the plain send (which raises
`class_not_found` for a dead class) otherwise. This function is the cold path
for tuple and map receivers and for strategies that cannot prove a plain value.
It does not look at `Behaviour` on its plain-value branch.
""".
-spec isKindOfBuiltin(term(), term(), boolean()) -> term().
isKindOfBuiltin(Receiver, Behaviour, TestAnswer) ->
    kind_of_builtin(
        Receiver,
        Behaviour,
        TestAnswer,
        beamtalk_default_method:overridden_anywhere('isKindOf:')
    ).

-doc """
`Receiver isKindOf: <user class literal named Name>` (BT-4757, BT-4769): the
answer when it can be given by name, else `undecided`.

The compiled caller has already checked that no class other than `Object` has
defined `isKindOf:` (the override flag, read on every execution, ADR 0142 §2)
and that `Name`'s class process is alive (BT-4768); on `undecided` it builds
the class reference and takes the ordinary send, so this function never needs
the reference. While nothing overrides `isKindOf:` the send would run
`objectIsKindOf/2`; for an ordinary instance that is `inherits_from(Class,
Name)` on the class table, answered here directly by name: no class-object
cache key. A class-object or metaclass receiver, a future, a class that is not
in the class table, or one that does not inherit `Object` (a `ProtoObject`
proxy forwarding `doesNotUnderstand:` never runs `Object>>isKindOf:`) answers
`undecided`.

One `beamtalk_class_derived_cache:fetch/2` per call, keyed `{is_kind_of_named,
Class, Name}`, holds that verdict (`true`, `false` or `undecided`); every class
write drops it, so a reloaded superclass is seen at once. An unregistered
class is never cached.

Strict (BT-4768): a dead process raises exactly as the send always did. The
caller's probe covers `Name`'s class process; here the receiver's side is
checked on every call: an actor whose process is dead, or an instance whose
class process is dead or unregistered, answers `undecided`, and the send
raises.
""".
-spec isKindOfNamed(term(), atom()) -> boolean() | undecided.
isKindOfNamed(Receiver, Name) ->
    case receiver_class_side(Receiver) of
        {instance, C} ->
            case instance_alive(Receiver, C) of
                true ->
                    beamtalk_class_derived_cache:fetch(
                        {is_kind_of_named, C, Name}, fun() -> decide_kind_of_named(C, Name) end
                    );
                false ->
                    undecided
            end;
        _ ->
            undecided
    end.

%% The cached verdict of `isKindOfNamed/2` for an instance of `C`, from the
%% class table alone (the registry's own walk, so one cache entry answers the
%% whole call).
-spec decide_kind_of_named(atom(), atom()) -> {cache | nocache, boolean() | undecided}.
decide_kind_of_named(C, Name) ->
    case beamtalk_class_metadata:lookup_superclass(C) of
        not_found ->
            {nocache, undecided};
        {ok, _} ->
            case beamtalk_class_registry:inherits_from(C, 'Object') of
                true -> {cache, beamtalk_class_registry:inherits_from(C, Name)};
                false -> {cache, undecided}
            end
    end.

-ifdef(TEST).
%% `isKindOfNamed/2` plus the send it falls back to, with the node-wide
%% override flag passed in, for tests.
-spec kind_of_named(term(), term(), atom(), boolean()) -> term().
kind_of_named(Receiver, Behaviour, Name, false) ->
    case isKindOfNamed(Receiver, Name) of
        undecided -> beamtalk_message_dispatch:send(Receiver, 'isKindOf:', [Behaviour]);
        Answer -> Answer
    end;
kind_of_named(Receiver, Behaviour, _Name, true) ->
    beamtalk_message_dispatch:send(Receiver, 'isKindOf:', [Behaviour]).
-endif.

%% Whether the processes a send to an instance of `C` would reach are alive:
%% the actor itself (a local one; a remote pid is the send's to judge) and
%% `C`'s registered class process.
-spec instance_alive(term(), atom()) -> boolean().
instance_alive(#beamtalk_object{pid = Pid}, C) when is_pid(Pid), node(Pid) =:= node() ->
    is_process_alive(Pid) andalso class_process_alive(C);
instance_alive(_Receiver, C) ->
    class_process_alive(C).

-spec class_process_alive(atom()) -> boolean().
class_process_alive(C) ->
    is_pid(beamtalk_class_registry:whereis_class(C)).

%% `isKindOfBuiltin/3` with the node-wide override flag passed in, so tests
%% can check both branches whatever the flag holds in their node.
-spec kind_of_builtin(term(), term(), boolean(), boolean()) -> term().
kind_of_builtin(Receiver, Behaviour, TestAnswer, Overridden) ->
    case not Overridden andalso plain_value(Receiver) of
        true -> TestAnswer;
        false -> beamtalk_message_dispatch:send(Receiver, 'isKindOf:', [Behaviour])
    end.

%% A value whose class is a stdlib built-in reached without dispatch to user
%% code: not a tuple headed by one of the routing fixture's reserved tags other
%% than `String`'s (an actor or class object, a supervisor handle, a future,
%% whatever its size) and not a map the runtime treats as tagged (a Value, an
%% Object-kind instance, a `'__class_mod__'` map, the native proxy). Pinned to
%% the fixture's `reserved_tuple_tags` by
%% `beamtalk_tagged_map_routing_conformance_tests`.
-spec plain_value(term()) -> boolean().
plain_value(Tuple) when is_tuple(Tuple), tuple_size(Tuple) >= 1 ->
    not lists:member(element(1, Tuple), [beamtalk_object, beamtalk_supervisor, beamtalk_future]);
plain_value(Map) when is_map(Map) ->
    not beamtalk_tagged_map:is_tagged(Map);
plain_value(_) ->
    true.

%% `objectIsKindOf/2`'s decided answer for an instance of `C`, when `C` runs
%% `Object>>isKindOf:`; `overridden` (cached: the next class write or
%% extension change flushes it) when something on the chain overrides it.
-spec decide_primitive_is_kind_of(atom(), #beamtalk_object{}) ->
    {cache, {kind_of, boolean(), pid(), pid()} | overridden} | {nocache, undecided}.
decide_primitive_is_kind_of(C, Behaviour) ->
    case beamtalk_default_method:owner(C, 'isKindOf:') of
        'Object' -> decide_instance_is_kind_of(C, Behaviour);
        unpublished -> {nocache, undecided};
        _Override -> {cache, overridden}
    end.

-spec primitive_kind_of_valid({kind_of, boolean(), pid(), pid()} | overridden) -> boolean().
primitive_kind_of_valid(overridden) -> true;
primitive_kind_of_valid(Decided) -> kind_of_pids_alive(Decided).

%% The memoised decision for an instance of class `C` (see the doc above):
%% the `class C` rows of the table.
-spec instance_is_kind_of(atom(), term(), term()) -> term().
instance_is_kind_of(C, Receiver, #beamtalk_object{pid = BPid} = Behaviour) when is_pid(BPid) ->
    case
        beamtalk_class_derived_cache:fetch(
            {is_kind_of, C, Behaviour},
            fun() -> decide_instance_is_kind_of(C, Behaviour) end,
            fun kind_of_pids_alive/1
        )
    of
        {kind_of, Answer, _, _} -> Answer;
        undecided -> is_kind_of_by_send(Receiver, Behaviour)
    end;
instance_is_kind_of(_C, Receiver, Behaviour) ->
    is_kind_of_by_send(Receiver, Behaviour).

%% Decided (and cacheable) only when `C` is a live registered class and
%% `Behaviour` is a canonical class or metaclass object.
-spec decide_instance_is_kind_of(atom(), #beamtalk_object{}) ->
    {cache, {kind_of, boolean(), pid(), pid()}} | {nocache, undecided}.
decide_instance_is_kind_of(C, #beamtalk_object{pid = BPid} = Behaviour) ->
    case {canonical_class_object(C), behaviour_side(Behaviour)} of
        {#beamtalk_object{pid = CPid}, {class, A}} ->
            {cache, {kind_of, beamtalk_class_derived_cache:inherits_from(C, A), CPid, BPid}};
        {#beamtalk_object{pid = CPid}, {meta, _}} ->
            {cache, {kind_of, false, CPid, BPid}};
        _ ->
            {nocache, undecided}
    end.

-spec kind_of_pids_alive({kind_of, boolean(), pid(), pid()}) -> boolean().
kind_of_pids_alive({kind_of, _, CPid, BPid}) ->
    is_process_alive(CPid) andalso is_process_alive(BPid).

%% The original `self class includesBehaviour: aClass`.
-spec is_kind_of_by_send(term(), term()) -> term().
is_kind_of_by_send(Receiver, Behaviour) ->
    beamtalk_message_dispatch:send(
        beamtalk_primitive:class_of_object(Receiver), 'includesBehaviour:', [Behaviour]
    ).

%% Which behaviour `class_of_object(Receiver)` is: `{instance, C}` for an
%% ordinary instance, whose class `C` is named by the receiver alone;
%% `{meta, Name}` for a class object or metaclass whose class is the
%% canonical metaclass of a live registered class; `other` otherwise (a
%% future, a stale or non-canonical class object).
-spec receiver_class_side(term()) -> {instance | meta, atom()} | other.
receiver_class_side({beamtalk_future, _}) ->
    other;
receiver_class_side(#beamtalk_object{
    class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid
}) ->
    %% class_of_object/1 of a metaclass is the metaclass itself.
    canonical_meta(Pid);
receiver_class_side(#beamtalk_object{class = 'Metaclass'}) ->
    other;
receiver_class_side(#beamtalk_object{class = Tag, pid = Pid}) ->
    case beamtalk_class_registry:is_class_name(Tag) of
        %% A class object: its class is the metaclass on the same pid.
        true -> canonical_meta(Pid);
        false -> {instance, Tag}
    end;
receiver_class_side(Receiver) ->
    {instance, beamtalk_primitive:class_of(Receiver)}.

%% `{class, Name}` when `Behaviour` is `==` to the registry's class object
%% for its name, `{meta, Name}` when it is that class's metaclass object.
-spec behaviour_side(term()) -> {class | meta, atom()} | other.
behaviour_side(#beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid}) ->
    canonical_meta(Pid);
behaviour_side(#beamtalk_object{pid = Pid} = Behaviour) when is_pid(Pid) ->
    case beamtalk_class_registry:class_name_for_pid(Pid) of
        {ok, Name} ->
            case canonical_class_object(Name) of
                Behaviour -> {class, Name};
                _ -> other
            end;
        not_found ->
            other
    end;
behaviour_side(_) ->
    other.

-spec canonical_meta(term()) -> {meta, atom()} | other.
canonical_meta(Pid) when is_pid(Pid) ->
    case beamtalk_class_registry:class_name_for_pid(Pid) of
        {ok, Name} ->
            case canonical_class_object(Name) of
                #beamtalk_object{pid = Pid} -> {meta, Name};
                _ -> other
            end;
        not_found ->
            other
    end;
canonical_meta(_) ->
    other.

%% `beamtalk_class_registry:resolve_class_object/1` — the tuple a class
%% reference, `class_of_object/1` and `classAllSuperclasses/1` all build —
%% memoised. A class (re)start rewrites its metadata row, which invalidates
%% the entry; the liveness check covers a class process killed without a
%% restart (its registered name is gone, so the registry answers `undefined`).
-spec canonical_class_object(atom()) -> #beamtalk_object{} | undefined.
canonical_class_object(Name) ->
    beamtalk_class_derived_cache:fetch(
        {class_object, Name},
        fun() ->
            case beamtalk_class_registry:resolve_class_object(Name) of
                undefined -> {nocache, undefined};
                Obj -> {cache, Obj}
            end
        end,
        fun(#beamtalk_object{pid = Pid}) -> is_process_alive(Pid) end
    ).

-doc """
Return the class documentation string, or nil if none set.

ADR 0033: Runtime-embedded documentation.
The class gen_server stores `none` internally; we return `nil` for Beamtalk.
""".
-spec classDoc(#beamtalk_object{}) -> beamtalk_string:t() | 'nil'.
classDoc(Self) ->
    ClassPid = erlang:element(4, Self),
    case class_doc(ClassPid) of
        %% BT-4173: `doc: nil` stores the atom `nil`; unset is `none`.
        none -> nil;
        nil -> nil;
        %% Docs store raw (compile-time) or tagged (set_doc) — normalize
        %% to a String at the read boundary.
        {beamtalk_string, _} = Tagged -> Tagged;
        Doc when is_binary(Doc) -> beamtalk_primitive:string_from_binary(Doc)
    end.

%% BT-4262: the class doc from its published metadata row (no message to the
%% class, so it answers from inside the class's own process and while the
%% class waits on another one), falling back to asking the class process for
%% a class that has not published it, or on another node (whose doc this
%% node's metadata does not hold).
-spec class_doc(pid()) -> term().
class_doc(ClassPid) when node(ClassPid) =/= node() ->
    gen_server:call(ClassPid, get_doc);
class_doc(ClassPid) ->
    case beamtalk_object_class:class_name_safe(ClassPid) of
        undefined ->
            gen_server:call(ClassPid, get_doc);
        ClassName ->
            case beamtalk_class_metadata:lookup_class_doc(ClassName) of
                {ok, Doc} -> Doc;
                not_found -> gen_server:call(ClassPid, get_doc)
            end
    end.

-doc """
Set the class documentation string.

ADR 0033: Post-hoc setter for class-level doc.
""".
-spec classSetDoc(#beamtalk_object{}, binary()) -> #beamtalk_object{}.
classSetDoc(Self, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, 'doc:'),
    ok = gen_server:call(ClassPid, {set_doc, DocBinary}),
    Self.

-doc """
Set the documentation string for a specific method selector.

ADR 0033: Post-hoc setter for method-level doc.
""".
-spec classSetMethodDoc(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classSetMethodDoc(Self, Selector, DocBinary) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, 'setDocForMethod:to:'),
    ok = gen_server:call(ClassPid, {set_method_doc, Selector, DocBinary}),
    Self.

-doc """
Get the documentation string for a specific method selector, or nil.

Completes the documentation API symmetry on Behaviour.
Returns the doc binary if set, nil if the method does not exist or has no
documentation. Walks the superclass chain via beamtalk_method_resolver.
""".
-spec classDocForMethod(#beamtalk_object{}, atom()) -> beamtalk_string:t() | 'nil'.
classDocForMethod(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    case beamtalk_method_resolver:resolve(ClassPid, Selector) of
        nil ->
            nil;
        MethodObj when is_map(MethodObj) ->
            %% BT-4173: normalize like classDoc/1 (see above).
            case maps:get('__doc__', MethodObj, nil) of
                nil -> nil;
                {beamtalk_string, _} = Tagged -> Tagged;
                Doc when is_binary(Doc) -> beamtalk_primitive:string_from_binary(Doc)
            end
    end.

-doc """
Remove this class from the system, performing full cleanup.

Implements `removeFromSystem` for class objects (Smalltalk convention).

Delegates to classRemoveFromSystemByName/1 after extracting the class name.
""".
-spec classRemoveFromSystem(#beamtalk_object{}) -> 'nil'.
classRemoveFromSystem(Self) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, removeFromSystem),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    classRemoveFromSystemByName(ClassName).

-doc """
Remove a class from the system by name, performing full cleanup.

Programmatic variant of removeFromSystem — used by the MCP/REPL
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
     compiler cache, workspace class_sources (via
     beamtalk_class_lifecycle:class_removed/2)
  5. Notify the workspace layer / REPL sessions (publish_class_removed/2)
""".
-spec classRemoveFromSystemByName(atom()) -> 'nil'.
classRemoveFromSystemByName(ClassName) ->
    ok = beamtalk_refactor_support:require_class_capability(
        removeFromSystem, ClassName, <<"removeFromSystem">>
    ),
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
            ok = beamtalk_refactor_support:require_class_responsive(
                ClassPid, ClassName, removeFromSystem
            ),
            Module = beamtalk_object_class:module_name_safe(ClassPid),
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
                            %% snapshot the class's current full
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
                            RemovalSnapshot = beamtalk_refactor_support:capture_class_removal_snapshot(
                                ClassNameBin
                            ),
                            %% stop eager crash recovery for this
                            %% class BEFORE killing its actors. There is no
                            %% longer a link between an
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
                            %% BT-3895: a programmatic ClassBuilder class has no
                            %% BEAM module behind it (`moduleName` defaults to the
                            %% class name, but nothing is ever loaded under it), so
                            %% `code:delete/1` answers `false` ("no current code").
                            %% That is not a failed unload — there is nothing to
                            %% delete. Only a module that is actually loaded must
                            %% be deleted successfully.
                            ok = delete_module_code(ClassName, Module),
                            ok = ensure_code_step(
                                ClassName,
                                Module,
                                soft_purge_after_delete,
                                code:soft_purge(Module)
                            ),
                            %% Purge every derived registry (xref,
                            %% extensions, protocol registry, compiler cache,
                            %% workspace class_sources) — the single teardown
                            %% path, run before the workspace/REPL notification
                            %% below.
                            ok = beamtalk_class_lifecycle:class_removed(ClassName, Module),
                            publish_class_removed(ClassName, Module),
                            beamtalk_refactor_support:log_class_removal(
                                ClassNameBin, RemovalSnapshot
                            ),
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

-doc """
Return the source file path for this class, or nil if not set.

Reads `beamtalk_source` module attribute embedded at compile time.
This is the definitive source-of-truth (survives workspace restarts).
Returns nil for stdlib/bootstrap/ClassBuilder-created classes.
""".
-spec classSourceFile(#beamtalk_object{}) -> beamtalk_string:t() | 'nil'.
classSourceFile(Self) ->
    ClassPid = erlang:element(4, Self),
    ModuleName = beamtalk_object_class:module_name_safe(ClassPid),
    %% BT-4173: the source path is text — answer a String (other
    %% `source_file_from_module/1` callers keep the raw bytes they need).
    case beamtalk_reflection:source_file_from_module(ModuleName) of
        nil -> nil;
        Path -> beamtalk_primitive:string_from_binary(Path)
    end.

-doc """
Recompile from sourceFile and hot-swap the BEAM module.

ADR 0040 Phase 2.
Raises an error if sourceFile is nil (stdlib / dynamic class).
Delegates compilation to beamtalk_repl_eval:reload_class_file/1 via
erlang:apply/3 to avoid a compile-time dep from beamtalk_runtime to
beamtalk_workspace (follows the beamtalk_actor_registry registered-name pattern).
""".
%% ADR 0127 §11 / BT-3593: a protocol's class object is dispatched by the
%% single shared `beamtalk_protocol_object` module for EVERY protocol
%% (`beamtalk_protocol_registry:create_protocol_class/2`) — reflecting on
%% that shared dispatch module's own (internal runtime) source would never
%% find the protocol's `.bt` file, so `Greeter reload` always raised
%% `no_source_file` before this, for every protocol regardless of whether it
%% genuinely had a workspace source file. Resolve a protocol's OWN defining
%% module instead — already recorded on its registry entry at registration
%% time for exactly this purpose (see
%% `beamtalk-codegen`'s `generate_protocol_registrations`'s `module` field
%% doc, "so the runtime — and the System Browser — can resolve a protocol
%% class object's origin/source"). Falls back to the ordinary
%% `module_name_safe/1` path for a non-protocol class, or a protocol
%% registered before that field existed.
-spec protocol_or_class_module(atom(), pid()) -> atom() | undefined.
protocol_or_class_module(ClassName, ClassPid) ->
    case beamtalk_protocol_registry:is_protocol(ClassName) of
        true ->
            case beamtalk_protocol_registry:protocol_info(ClassName) of
                #{module := ModuleName} when is_atom(ModuleName) ->
                    ModuleName;
                _ ->
                    beamtalk_object_class:module_name_safe(ClassPid)
            end;
        false ->
            beamtalk_object_class:module_name_safe(ClassPid)
    end.

-spec classReload(#beamtalk_object{}) -> #beamtalk_object{}.
classReload(Self) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, reload),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    ok = beamtalk_refactor_support:require_class_capability(reload, ClassName, <<"reload">>),
    ModuleName = protocol_or_class_module(ClassName, ClassPid),
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
            %% Checked after the source-file lookup so a class with no source
            %% file keeps reporting `no_source_file` on a bare runtime.
            ok = beamtalk_capability:require_workspace(reload, ClassName),
            SourcePathStr = binary_to_list(SourcePath),
            %% Demand-driven native .erl compilation before reload.
            %% Uses dynamic dispatch to avoid compile-time dep on beamtalk_workspace.
            try
                ProjectRoot = erlang:apply(
                    beamtalk_workspace_project_sync, find_project_root, [SourcePathStr]
                ),
                _ = erlang:apply(
                    beamtalk_workspace_project_sync,
                    maybe_recompile_native_deps,
                    [SourcePathStr, ProjectRoot]
                )
            catch
                error:undef -> ok
            end,
            case erlang:apply(beamtalk_repl_eval, reload_class_file, [SourcePathStr, ClassName]) of
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
                %% ADR 0127 §11 / BT-3593: the protocol-reload fan-out
                %% already raises an already-structured `#beamtalk_error{}`
                %% (stdlib refusal, or "rejected — names every failing
                %% user") — re-raise it as-is rather than re-wrapping it
                %% inside a generic "Reload failed: ~p" message, which would
                %% bury that carefully-composed text behind a raw `~p` dump
                %% of the record.
                {error, #beamtalk_error{} = StructuredError} ->
                    beamtalk_error:raise(StructuredError);
                {error, Reason} ->
                    Error0 = beamtalk_error:new(reload_failed, ClassName),
                    Msg = beamtalk_error:chars_to_binary(
                        io_lib:format("Reload failed: ~tp", [Reason])
                    ),
                    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
            end
    end.

%%====================================================================
%% ADR 0123 §1/§3: Shape versioning
%%====================================================================

-doc """
Return the class's declared shape version — the compiled `shapeVersion: N`
header clause or the `ClassBuilder shapeVersion:` builder keyword, default
`1` (ADR 0123 §1). Reads the class gen_server's own `shape_version` field
(`beamtalk_object_class:shape_version/1`), not `__beamtalk_meta/0`
directly — same as `classDoc`/`classIsSealed`-shaped reflection above: the
class process seeds it at registration from meta-then-ClassInfo (see
`beamtalk_object_class:init/1`/`apply_class_info/2`), so this answers
correctly for a compiled class *and* a `ClassBuilder`-registered dynamic one
with no `__beamtalk_meta/0` at all.
""".
-spec classShapeVersion(#beamtalk_object{}) -> pos_integer().
classShapeVersion(Self) ->
    ClassPid = erlang:element(4, Self),
    beamtalk_object_class:shape_version(ClassPid).

-doc """
Run the whole `migrateFromVN:` chain from `FromVersion` to this class's
current `shapeVersion`, then reconcile against its declared fields — the
Beamtalk surface (`Behaviour >> migrateShape:from:`, ADR 0123 §3) of
`beamtalk_shape_migration:migrate/3`, the exact call `code_change/3` makes
on hot reload, so a chain is testable at the REPL before any live instance
depends on it. Raises the underlying `#beamtalk_error{}` (`class_not_found`
or `shape_migration_failed`) on failure.
""".
-spec classMigrateShapeFrom(#beamtalk_object{}, map(), pos_integer()) -> map().
classMigrateShapeFrom(Self, Fields, FromVersion) when
    is_map(Fields), is_integer(FromVersion), FromVersion > 0
->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    case beamtalk_shape_migration:migrate(ClassName, FromVersion, Fields) of
        {ok, NewFields, _ToVersion} -> NewFields;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

%%% ============================================================================
%%% Protocol Query Primitives (ADR 0068 Phase 2c)
%%% ============================================================================

-doc """
Check if the receiver class conforms to a protocol.

ADR 0068 Phase 2c: Backs `@primitive "classConformsTo"` in behaviour.bt.
Structural conformance — the class conforms if it responds to all required
selectors of the protocol.

The protocol argument is expected to be a Symbol (atom) naming the protocol.
""".
-spec classConformsTo(#beamtalk_object{}, atom()) -> boolean().
classConformsTo(Self, ProtocolName) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    beamtalk_protocol_registry:conforms_to(ClassName, ProtocolName).

-doc """
Return the list of protocols the receiver class conforms to.

ADR 0068 Phase 2c: Backs `@primitive "classProtocols"` in behaviour.bt.
Returns a list of protocol name atoms, sorted alphabetically.
""".
-spec classProtocols(#beamtalk_object{}) -> [atom()].
classProtocols(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    beamtalk_protocol_registry:protocols_for_class(ClassName).

-doc """
Return the protocols the receiver class directly `uses:`, in declaration
order.

ADR 0127 §12: Backs `@primitive "classUsedProtocols"` in behaviour.bt.
Distinct from `classProtocols/1` above, which answers *structural*
conformance (ADR 0068) — this answers only the trait composition this class
itself declared. `allUsedProtocols` (transitive, including superclasses') is
pure Beamtalk composed on top of this primitive plus `allSuperclasses`.
""".
-spec classUsedProtocols(#beamtalk_object{}) -> [atom()].
classUsedProtocols(Self) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    beamtalk_protocol_registry:used_protocols(ClassName).

%%% ============================================================================
%%% Metaclass Primitives (ADR 0036 Phase 1)
%%% ============================================================================

-doc """
Return the class this metaclass describes.

ADR 0036: Backs `@primitive "metaclassThisClass"` in metaclass.bt.
A metaclass object carries the class pid; we retrieve its name and return
the class object. Example: `Counter class class thisClass == Counter`.
""".
-spec metaclassThisClass(#beamtalk_object{}) -> #beamtalk_object{} | 'nil'.
metaclassThisClass(Self) ->
    Pid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(Pid),
    atom_to_class_object(ClassName).

-doc """
Return the superclass of the metaclass parallel hierarchy.

ADR 0036: Backs `@primitive "metaclassSuperclass"` in metaclass.bt.
The superclass of Counter's metaclass is the metaclass of Counter's superclass.
Example: `Counter class superclass == Actor class`.

Previously used gen_server:call(Pid, superclass) directly (a fix to
apply_class_info/2 made it update the gen_server superclass from
__beamtalk_meta/0, so the gen_server always held the correct superclass).

Now uses `beamtalk_object_class:superclass_safe/1` — the same
unified-metadata ETS source `beamtalk_class_dispatch` reads — instead of a
separate `gen_server:call`, so this and dispatch can never disagree mid-reload.

Grounds the parallel chain at `ProtoObject class superclass == Class`
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

Backs `@primitive "metaclassAllMethods"` in metaclass.bt.
Combines class-side selectors of the described class (via metaclassClassMethods/1)
with all instance methods of the 'Class' hierarchy (Behaviour protocol: reload,
superclass, etc.). Result is deduplicated and sorted.

We walk the instance method chain of 'Class' directly in Erlang to avoid
dispatching through the Metaclass chain (which would recurse into this method).

Now uses walk_hierarchy/3 directly, since apply_class_info/2 updates the
gen_server superclass from __beamtalk_meta/0, so walk_hierarchy/3
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
                        beamtalk_object_class:instance_methods_safe(CPid)
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

ADR 0036: Backs `@primitive "metaclassClassMethods"` in metaclass.bt.
Walks the superclass chain collecting all class-side selectors.
""".
-spec metaclassClassMethods(#beamtalk_object{}) -> [atom()].
metaclassClassMethods(Self) ->
    Pid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(Pid),
    Acc = walk_hierarchy(
        ClassName,
        fun(_CN, CPid, A) ->
            Selectors = beamtalk_object_class:local_class_method_selectors_safe(CPid),
            {cont, ordsets:union(A, ordsets:from_list(Selectors))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Acc).

-doc """
Return local class-side method selectors (non-inherited).

ADR 0036: Backs `@primitive "metaclassLocalClassMethods"` in metaclass.bt.
Returns only class methods defined directly on this class.
""".
-spec metaclassLocalClassMethods(#beamtalk_object{}) -> [atom()].
metaclassLocalClassMethods(Self) ->
    Pid = erlang:element(4, Self),
    beamtalk_object_class:local_class_method_selectors_safe(Pid).

-doc """
Test whether the selector is defined as a class-side method.

ADR 0036: Backs `@primitive "metaclassIncludesSelector"` in metaclass.bt.
Does NOT check superclasses — local containment only.
""".
-spec metaclassIncludesSelector(#beamtalk_object{}, atom()) -> boolean().
metaclassIncludesSelector(Self, Selector) ->
    Pid = erlang:element(4, Self),
    lists:member(Selector, beamtalk_object_class:local_class_method_selectors_safe(Pid)).

-doc """
Guard for direct Metaclass instantiation — backs `class sealed new`.

ADR 0036: Backs `@primitive "metaclassNew"` in metaclass.bt.
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

Returns `{ok, Meta}` if the module exports `__beamtalk_meta/0` and
the call succeeds; returns `not_available` otherwise (dynamic classes from
beamtalk_class_builder, or any module that doesn't export the function).

Note on superclass representation: the meta map uses atom `nil` for root
classes (no superclass), while the gen_server `superclass` message returns
atom `none`. Callers must normalize both to Beamtalk `nil`.
""".
-spec meta_for_module(atom()) -> {ok, map()} | not_available.
meta_for_module(Module) ->
    beamtalk_class_metadata:read_meta(Module).

-doc """
Generic fold over the superclass chain starting from ClassName.

Fun receives (ClassName, ClassPid, Acc) and returns:
  {cont, NewAcc}   — continue walking to superclass
  {halt, Result}   — stop and return Result immediately

When the chain is exhausted (none or unregistered class), returns the fully
folded accumulator built up through every ancestor visited.

When the walk instead exhausts `?MAX_HIERARCHY_DEPTH` (a hierarchy cycle),
returns the partial accumulator folded up through every ancestor actually
visited before the guard tripped — not the original `Acc` passed
into this call — and logs a `?LOG_WARNING` naming the ancestor where the
cycle was detected. This matches the hand-rolled recursion this function
replaced, which also returned the partial fold on depth exhaustion. Only
reachable via an actual hierarchy cycle or a legitimately
`?MAX_HIERARCHY_DEPTH`-level-deep hierarchy.

The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; this function supplies only the
per-ancestor registry lookup and threads Acc through the walk. Since
`walk_ancestors/3` only threads a bare node id between steps (not a
separate accumulator), Acc rides along inside the node as `{ClassName, Acc}`.
this is also why `max_depth_exceeded` carries `LastNode` back to
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
                        %% deadlock-safe metadata lookup, same source
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

Thin wrapper over `beamtalk_class_registry:resolve_class_object/1` (promoted
there per ADR 0126 Phase 0.5 finding (e) — see that function's doc), mapping
its `undefined` to the Beamtalk `nil` sentinel this function has always
returned for an unregistered class (safe during bootstrap window).
""".
-spec atom_to_class_object(atom()) -> #beamtalk_object{} | 'nil'.
atom_to_class_object(ClassName) ->
    case beamtalk_class_registry:resolve_class_object(ClassName) of
        undefined ->
            ?LOG_DEBUG("atom_to_class_object: class ~p not registered", [ClassName], #{
                domain => [beamtalk, runtime]
            }),
            nil;
        ClassObj ->
            ClassObj
    end.

-doc """
Notify workspace layer that a class was successfully removed.

Cleans up stale entries in workspace_meta and REPL session trackers.
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

-doc "Delete a module's current code if it is loaded; a never-loaded module (ClassBuilder class, BT-3895) is a no-op.".
-spec delete_module_code(atom(), atom()) -> ok.
delete_module_code(ClassName, Module) ->
    case erlang:module_loaded(Module) of
        true -> ensure_code_step(ClassName, Module, delete, code:delete(Module));
        false -> ok
    end.

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
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format("Failed to ~tp module ~tp during unload", [Step, Module])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

-doc """
Check if a module name belongs to the Beamtalk stdlib.
Stdlib modules have the prefix `bt@stdlib@`.

delegates to `beamtalk_module_name:is_stdlib_module/1`, the single
authority for this check (was byte-identical to
`beamtalk_class_registry:is_stdlib_module/1`).
""".
-spec is_stdlib_module_name(atom()) -> boolean().
is_stdlib_module_name(Module) ->
    beamtalk_module_name:is_stdlib_module(Module).

-doc """
Stop all live actors of a given class.

Queries the actor registry (if available) for all actors belonging
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
