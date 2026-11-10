// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Core data structures for class hierarchy metadata.
//!
//! Contains `MethodInfo`, `SuperclassTypeArg`, and `ClassInfo` — the value objects
//! that describe classes and methods in the static hierarchy.

use crate::ast::{
    ClassDefinition, ClassKind, ConstantValue, Expression, MethodDefinition, MethodKind, SlotKind,
};
use ecow::EcoString;
use std::collections::{BTreeSet, HashMap};

use super::{ClassHierarchy, DeclaredType};

/// Information about a class variable (`classState:` declaration, ADR 0067).
///
/// The class-side counterpart to a `state:` field's `state_types`/
/// `state_has_default`/`state_kinds` entries on [`ClassInfo`] — but class
/// variables have no per-name-keyed maps to hang extra data off (there is no
/// existing `class_variable_types` map to extend), so this carries name,
/// type, default-presence and slot kind together in one structure (ADR 0124
/// B5a). Populated for local classes from the AST's `class_variables:
/// Vec<StateDeclaration>`. For cross-file classes only `name` and `kind` are
/// recoverable today, from `__beamtalk_meta/0`'s `class_fields` (name list)
/// and `class_field_kinds` (name → `eager`/`late`) keys — `ty`/`has_default`
/// degrade to `None`/`false` because there is no `class_field_types`/
/// `class_field_has_default` wire representation yet (only the instance-side
/// `field_types`/`field_has_default` exist); see
/// `parse_class_info_from_meta_term` (beamtalk-compiler-port).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ClassVarInfo {
    /// The class variable name.
    pub name: EcoString,
    /// Declared type annotation, if any.
    pub ty: Option<DeclaredType>,
    /// Whether this class variable carries an explicit default value.
    pub has_default: bool,
    /// Whether this class variable is `late` (ADR 0124 §1). Defaults to
    /// [`SlotKind::Eager`] for every declaration that doesn't use the
    /// modifier.
    pub kind: SlotKind,
}

/// Information about a method in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MethodInfo {
    /// Full selector name (e.g., "at:put:", "+", "size").
    pub selector: EcoString,
    /// Number of arguments.
    pub arity: usize,
    /// Method kind.
    pub kind: MethodKind,
    /// Class that defines this method.
    pub defined_in: EcoString,
    /// Whether this method is sealed (cannot be overridden).
    pub is_sealed: bool,
    /// Whether this method is internal (package-scoped visibility, ADR 0071).
    ///
    /// Internal methods are only callable from within the same package.
    pub is_internal: bool,
    /// Whether this method spawns its block argument in a separate BEAM process.
    ///
    /// When `true`, self-sends inside the block are safe for Actor classes
    /// because they execute in a different process and cannot re-enter the
    /// actor's own call-stack (`calling_self` dispatch does not apply).
    pub spawns_block: bool,
    /// Inferred return type (e.g., "Integer", "String", "Boolean").
    /// `None` means the return type is unknown (Dynamic).
    pub return_type: Option<DeclaredType>,
    /// Parameter type annotations (e.g., `vec![Some("Number")]` for `+ other: Number`).
    /// Empty for unary methods. `None` elements mean the parameter type is unknown.
    pub param_types: Vec<Option<DeclaredType>>,
    /// Documentation string for this method.
    /// Populated for compiler-synthesized methods and stdlib methods with `///` doc comments;
    /// `None` for user-written methods without doc comments.
    pub doc: Option<EcoString>,
    /// The protocol this method was flattened from (ADR 0127 §3), or `None`
    /// for a method the class wrote itself (including one that overrides a
    /// dropped provision, §3 step 4 — "class wins").
    ///
    /// `defined_in` deliberately keeps meaning "the class whose module holds
    /// the code" (the *using* class after flattening), since every existing
    /// consumer of `defined_in` reads it as a class name (inherited
    /// type-parameter substitution, method-local type-parameter inference,
    /// internal-visibility package lookup, DNU-override detection, the LSP's
    /// `find_defining_class`) — see `trait_expansion`'s module doc. `origin`
    /// carries the trait provenance those consumers must not see, for
    /// reflection, diagnostics and tooling instead (ADR 0127 §12:
    /// `CompiledMethod origin`, xref `provenance := protocol`).
    ///
    /// Set by `semantic_analysis::trait_expansion::apply_origins` after
    /// `ClassHierarchy::build` — the pass that flattens provisions into
    /// `ClassDefinition.methods` runs before the hierarchy exists, so it
    /// cannot stamp `MethodInfo` directly; it instead returns an
    /// `(class, selector) -> protocol` map that `analyse_full` applies once
    /// the hierarchy is built.
    pub origin: Option<EcoString>,
    /// The quoted primitive name when the method body is exactly
    /// `@primitive "<name>"` (e.g. `"classSignal:"` for `Exception class >>
    /// signal:`), else `None`. Read from the method's own source
    /// ([`MethodDefinition::quoted_primitive_name`](crate::ast::MethodDefinition::quoted_primitive_name)),
    /// so codegen can recognise a caller-safe class primitive at a
    /// class-literal send without restating which selector it implements
    /// (BT-4115).
    #[cfg_attr(feature = "serde", serde(default))]
    pub primitive: Option<EcoString>,
    /// The literal when the method body is exactly one scalar literal
    /// ([`MethodDefinition::literal_constant_body`](crate::ast::MethodDefinition::literal_constant_body)),
    /// else `None`. Codegen answers a class-literal send of such a class-side
    /// method with the literal, behind a static call key the runtime installs
    /// only while the defining module's metadata still certifies that body
    /// (BT-4363, BT-4504).
    #[cfg_attr(feature = "serde", serde(default))]
    pub constant_value: Option<ConstantValue>,
}

impl MethodInfo {
    /// The [`MethodInfo::constant_value`] to record for `method`: its literal
    /// body ([`MethodDefinition::literal_constant_body`]) on the class side,
    /// `None` on the instance side.
    ///
    /// Only a class-literal send of a *class-side* method is folded
    /// (BT-4363). An instance-side constant would put method bodies into the
    /// interface surface, so a body-only edit (`=> self name` to `=> "x"`)
    /// would rebuild unchanged callers (BT-4504). Every producer of
    /// `MethodInfo`/stdlib method metadata goes through this one rule.
    #[must_use]
    pub fn recorded_constant(method: &MethodDefinition, class_side: bool) -> Option<ConstantValue> {
        if class_side {
            method.literal_constant_body()
        } else {
            None
        }
    }

    /// Returns `true` if `self` (a subclass method) can override `ancestor`.
    ///
    /// A method cannot override a sealed method with the same selector.
    /// Different selectors are never overrides.
    #[must_use]
    pub fn can_override(&self, ancestor: &MethodInfo) -> bool {
        if self.selector != ancestor.selector {
            return false;
        }
        !ancestor.is_sealed
    }
}

/// How a subclass maps one of its type parameters (or a concrete type) to a
/// superclass type parameter position.
///
/// Used in `ClassInfo::superclass_type_args` to track the mapping established by
/// `Collection(E) subclass: Array(E)` or `Collection(Integer) subclass: IntArray`.
///
/// **References:** ADR 0068 Challenge 4
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SuperclassTypeArg {
    /// The subclass forwards one of its own type params to the superclass.
    ///
    /// Example: `Collection(E) subclass: Array(E)` — Array's `E` (at `param_index` 0)
    /// maps to Collection's position 0.
    ParamRef {
        /// Index into this class's `type_params` vec.
        param_index: usize,
    },
    /// A concrete (fixed) type is supplied to the superclass.
    ///
    /// Example: `Collection(Integer) subclass: IntArray` — `Integer` is fixed.
    Concrete {
        /// The concrete type (e.g., `Integer`).
        declared: DeclaredType,
    },
}

/// Information about a class in the hierarchy.
///
/// **DDD Context:** Semantic Analysis — Value Object
/// Constructor synthesis record: what the class's own compilation
/// emitted for value construction.
///
/// `Some` for every class compiled as a `Value` (its post-writeback kind):
/// direct `Value subclass:` declarations at extraction, and indirect Value
/// subclasses once `ClassHierarchy::propagate_class_kind` (or, where the
/// stored state determines the keyword decision,
/// `propagate_cross_file_class_kind`) has walked the chain (BT-4132). The
/// one rule is [`ClassInfo::record_constructor_anchor`].
///
/// `None` for non-Value classes, for AST-less classes whose keyword decision
/// is not derivable from stored state, for binary-only classes decoded from
/// BEAM metadata (the wire does not carry the anchor), and for
/// hand-written/cross-wire infos that predate the field
/// (`#[serde(default)]`). Readers treat `None` as "no direct constructor
/// calls" (safe fallback to `class_send`). `Some` only makes a class
/// eligible to try the direct path: every direct constructor call is
/// guarded at run time (BT-4409).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AutoConstructors {
    /// Synthesized keyword constructor selector (e.g. `x:y:`), if any.
    pub keyword_constructor: Option<EcoString>,
}

/// Constructor direct-call eligibility: which value-construction
/// sends for a class build in the caller instead of routing through the
/// class process's `gen_server`.
///
/// Computed once per class by
/// [`ClassHierarchy::direct_constructors`](super::ClassHierarchy::direct_constructors):
/// codegen consults it for static sends and bakes the `new`/`new:` answers
/// into `__beamtalk_meta` (as `direct_constructors`) for dynamic sends, so
/// the runtime never re-derives eligibility.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DirectConstructors {
    /// `Class new` (no args) builds via a direct `Module:'new'()` call.
    pub new0: bool,
    /// `Class new: map` is eligible for a direct `Module:'new'(Map)` call.
    /// Consulted by dynamic sends only: static `new:` keeps `class_send`
    /// so the chain verdict stays single-sourced in the baked metadata.
    pub new1: bool,
    /// Keyword constructor selector callable directly (e.g. `x:y:`), if any.
    pub keyword: Option<EcoString>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[allow(clippy::struct_excessive_bools)]
pub struct ClassInfo {
    /// Class name.
    pub name: EcoString,
    /// Superclass name (`None` for `ProtoObject`).
    pub superclass: Option<EcoString>,
    /// Whether this class is sealed (cannot be subclassed).
    pub is_sealed: bool,
    /// Whether this class is abstract.
    pub is_abstract: bool,
    /// Whether this class has the explicit `typed` modifier.
    /// Use `ClassHierarchy::is_typed()` to check inherited typed status.
    pub is_typed: bool,
    /// Whether this class is internal (package-scoped visibility, ADR 0071).
    ///
    /// Internal classes are only visible within their declaring package.
    pub is_internal: bool,
    /// Package that declares this class (ADR 0070/0071).
    ///
    /// Extracted from the BEAM module name pattern `bt@{package}@{class}`
    /// via [`add_from_beam_meta`], or set from the build pipeline's current
    /// package context. `None` for REPL classes and bootstrap builtins.
    pub package: Option<EcoString>,
    /// Whether this class is a Value subclass (ADR 0042).
    ///
    /// `true` for classes that directly or indirectly inherit from `Value`.
    /// Initially set from the direct superclass name, then corrected by
    /// [`ClassHierarchy::propagate_class_kind`] after the full hierarchy is built.
    pub is_value: bool,
    /// Whether this class is a native Actor (declared with `native:` keyword, ADR 0056).
    ///
    /// Native actors delegate to a backing Erlang module and cannot declare
    /// `state:` fields (state is owned by the Erlang `gen_server`).
    pub is_native: bool,
    /// Constructor synthesis record, if the class compiles as a `Value`
    /// (direct or indirect). See [`AutoConstructors`] for the invariant.
    #[cfg_attr(feature = "serde", serde(default))]
    pub auto_ctors: Option<AutoConstructors>,
    /// Declared sendability handle scope (ADR 0103, `handleScope: #symbol`).
    ///
    /// The bare symbol text (e.g. `"process"`, `"node"`) declared via a
    /// class-side `handleScope:` clause. `None` for classes with no
    /// declaration. Inherited via [`ClassHierarchy::handle_scope`], which walks
    /// the superclass chain. Only meaningful on `Object`-kind classes.
    pub handle_scope: Option<EcoString>,
    /// Whether this class was extracted from a file with parse errors
    /// (ADR 0100 Rule 2 parse-error guard).
    ///
    /// Parser error recovery can silently drop method definitions, so the
    /// method surface recorded here may be incomplete. The receiver-knowledge
    /// classifier treats any receiver whose superclass chain contains a
    /// marked class as `Open` (no unresolved-selector diagnostics), so a
    /// half-parsed file degrades only its own classes to silence instead of
    /// producing false hints. Set by the orchestrators that extract
    /// cross-file class metadata (CLI build Pass 1, lint's package walk, LSP
    /// `update_file`); always `false` for builtins and AST-built classes in
    /// the current module (their parse errors surface directly).
    #[cfg_attr(feature = "serde", serde(default))]
    pub surface_incomplete: bool,
    /// BT-4501: whether this class's own explicit class-side `new:` runs in
    /// the caller ([`ClassHierarchy::explicit_class_new1_runs_in_caller`]),
    /// i.e. codegen leaves `new:` out of the class's published
    /// `process_bound_class_selectors`. Decided where the AST is in hand
    /// (`ClassHierarchy::build_with_options`, and the stdlib snapshot in
    /// `generated_builtins.rs`) and published here so
    /// [`ClassHierarchy::runs_class_method_in_caller`] answers `new:` from
    /// the same fact. Own-class only: a subclass inheriting the `new:`
    /// keeps the class process, so this is never inherited.
    #[cfg_attr(feature = "serde", serde(default))]
    pub in_caller_new1: bool,
    /// Whether this class has no compiled BEAM module (BT-4069).
    ///
    /// True for live `ClassBuilder` classes (whose methods are funs in the
    /// class process — set by the live-image decode in
    /// `beamtalk-compiler-port`, comparing the meta map's `module` to the
    /// class name), for synthetic hierarchy entries, and for runtime-only
    /// `Future` (native-backed, no `.bt` module). A module-less class
    /// registers its own class name as its module, so its instances dispatch
    /// through `runtime_instance_method`, never through a
    /// `Module:dispatch/3` — any consumer resolving a static dispatch target
    /// must fail closed on this flag. Always `false` for source-built and
    /// generated-builtin classes.
    #[cfg_attr(feature = "serde", serde(default))]
    pub is_module_less: bool,
    /// State (instance variable) names.
    pub state: Vec<EcoString>,
    /// Declared type annotations for state fields (field name → type).
    /// Only populated for fields with explicit type annotations.
    pub state_types: HashMap<EcoString, DeclaredType>,
    /// Which state fields carry an explicit default value (field name → has default).
    /// Populated for every declared state field (both typed and untyped).
    ///
    /// Needed so cross-file consumers can identify typed-no-default
    /// fields (for post-initialize validation in `gen_server` codegen) even
    /// when the defining class's AST is not present in the current compilation.
    pub state_has_default: HashMap<EcoString, bool>,
    /// Slot kind (`#eager` | `#late`, ADR 0124 §1) for each declared state
    /// field (field name → kind). Populated for every declared state field,
    /// mirroring `state_has_default`. Fields absent from this map (older
    /// cross-file metadata) are treated as [`SlotKind::Eager`] by readers —
    /// see [`ClassHierarchy::state_field_kind`].
    pub state_kinds: HashMap<EcoString, SlotKind>,
    /// Slots this class's *own* `initialize` method definitely assigns —
    /// a must-analysis (intersection over branches/completions, ADR 0124
    /// A2a) over the method body, computed by
    /// [`crate::semantic_analysis::analyze_initialize_assigns`]. Empty when
    /// the class declares no `initialize`.
    ///
    /// This is a per-class summary, not the flattened chain: use
    /// [`ClassHierarchy::all_initialize_assigns`](super::ClassHierarchy::all_initialize_assigns)
    /// to compose it parent-first across the ADR 0078 inheritance chain the
    /// same way [`Self::state_has_default`] is composed by
    /// `inherited_typed_no_default_fields` (`beamtalk-codegen`).
    pub initialize_assigns: BTreeSet<EcoString>,
    /// `true` when some instance method on this class (any method, not only
    /// `initialize`) sends `fieldAt:put:` or `perform:` anywhere in its
    /// body — computed by
    /// [`crate::semantic_analysis::has_dynamic_field_writer`] over the
    /// class's own [`crate::ast::MethodDefinition`]s (ADR 0124 §6), both
    /// here and by `beamtalk-cli`'s `build_stdlib` for generated-builtin
    /// `ClassInfo`s (mirroring how [`Self::initialize_assigns`] is
    /// computed in both places). `false` only for cross-file metadata that
    /// predates this field (`#[serde(default)]`) or arrives through a wire
    /// format that doesn't carry it (`beamtalk-compiler-port`'s
    /// cross-package `__beamtalk_meta` decode) — a conservative
    /// under-approximation there, not a source of false positives:
    /// [`super::ClassHierarchy::all_initialize_assigns`]'s
    /// `has_dynamic_writer` only *demotes* BT-1948's construction-site
    /// finding from Warning to Hint, it never suppresses it, so missing
    /// this signal costs confidence, not correctness.
    #[cfg_attr(feature = "serde", serde(default))]
    pub has_dynamic_field_writer: bool,
    /// Methods defined directly on this class (instance-side).
    pub methods: Vec<MethodInfo>,
    /// Class-side methods defined on this class.
    pub class_methods: Vec<MethodInfo>,
    /// Class variables (declared with `classState:`) — name, declared type,
    /// default-value presence and slot kind (ADR 0124 B5a).
    pub class_variables: Vec<ClassVarInfo>,
    /// Type parameters for generic classes (e.g., `["T", "E"]` for `Result(T, E)`).
    ///
    /// Empty for non-generic classes.
    pub type_params: Vec<EcoString>,
    /// Protocol bounds for each type parameter (ADR 0068 Phase 2d).
    ///
    /// Parallel to `type_params`: `type_param_bounds[i]` is the bound for `type_params[i]`.
    /// `None` means the parameter is unbounded (accepts any type).
    /// `Some("Printable")` means the concrete type arg must conform to that protocol.
    pub type_param_bounds: Vec<Option<EcoString>>,
    /// How this class's type params (or concrete types) map to the superclass's type params.
    ///
    /// Empty when the superclass is not generic or no type args are applied.
    ///
    /// Example: `Collection(E) subclass: Array(E)` → `[ParamRef { param_index: 0 }]`
    /// Example: `Collection(Integer) subclass: IntArray` → `[Concrete { declared: DeclaredType::simple("Integer") }]`
    ///
    /// **References:** ADR 0068 Challenge 4
    pub superclass_type_args: Vec<SuperclassTypeArg>,
}

/// The composed "`initialize` definitely assigns" summary for a class,
/// flattened parent-first over the ADR 0078 inheritance chain (ADR 0124
/// A2a) — see [`super::ClassHierarchy::all_initialize_assigns`].
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct InitializeAssignsSummary {
    /// Slots definitely assigned by *some* class's own `initialize` in the
    /// chain (the union of every chain link's own [`ClassInfo::initialize_assigns`] —
    /// safe to union rather than intersect, because ADR 0078 auto-chains
    /// every ancestor's `initialize` to run, parent-first, before the leaf's
    /// own, so an ancestor's assignment is guaranteed present by the time the
    /// chain finishes regardless of what the leaf's own `initialize` does).
    pub assigned: BTreeSet<EcoString>,
    /// `true` when the chain could not be fully resolved: a `native:`
    /// ancestor (ADR 0056, whose slots are owned by its backing `gen_server`
    /// and carry no `initialize` AST to analyse) or a missing chain link (an
    /// ancestor absent from this [`ClassHierarchy`] entirely — see
    /// [`super::ClassHierarchy::has_cross_file_parent`]) makes `assigned`
    /// potentially incomplete rather than merely empty. Callers that need to
    /// distinguish "genuinely assigns nothing" from "can't fully tell" read
    /// this flag rather than treating an empty `assigned` as the answer.
    pub incomplete: bool,
    /// `true` when *any* class in the chain (the leaf or an ancestor) has
    /// its own [`ClassInfo::has_dynamic_field_writer`] set — a
    /// `fieldAt:put:`/`perform:` write anywhere in that class's methods
    /// (ADR 0124 §6, BT-1948). An ancestor's dynamic write is just as
    /// capable of assigning a slot outside this must-analysis's view as the
    /// leaf's own, so BT-1948's construction-site check reads this
    /// chain-wide flag — not [`ClassInfo::has_dynamic_field_writer`]
    /// directly — to decide whether to demote its finding to Hint.
    pub has_dynamic_writer: bool,
}

/// The class-side selectors that keep their class-process (or dedicated)
/// routing whatever the class declares (ADR 0142 §1 rule 3). The single copy
/// of the list: codegen emits it into every class's `__beamtalk_meta/0` as
/// `'process_bound_class_selectors'`, so the runtime reads it as generated
/// metadata and never keeps its own copy (the Erlang clauses that route part
/// of it are pinned to the emitted list by
/// `beamtalk_process_bound_selectors_tests`).
///
/// - `new`, `new:`, `startLink`, `startLink:`: process-state constructors
///   (ADR 0129); `basicNew`/`basicNewWith` read `beamtalk_class_name` /
///   `beamtalk_class_module` from the class process's dictionary.
/// - `spawn`, `spawnWith:`: actor instantiation, with dedicated
///   `beamtalk_class_dispatch:class_send_plain/3` clauses.
/// - `supervise`, `spawnAs:`, `spawnWith:as:` (with `spawn`, `spawnWith:`
///   above): the `Supervisor`/`DynamicSupervisor` routing selectors, started in
///   the caller before `class_send_plain/3` (ADR 0135 §1).
/// - `runAll`, `runAll:`, `run:`, `run:method:`: the `TestCase` run selectors,
///   which answer `test_spawn` and need the class `gen_server`'s `noreply`.
const PROCESS_BOUND_CLASS_SELECTORS: &[&str] = &[
    "new",
    "new:",
    "startLink",
    "startLink:",
    "spawn",
    "spawnWith:",
    "supervise",
    "spawnAs:",
    "spawnWith:as:",
    "runAll",
    "runAll:",
    "run:",
    "run:method:",
];

impl ClassInfo {
    /// Whether a class registered under `module` is module-less
    /// ([`Self::is_module_less`]): a live module-less class registers its own
    /// class name as its module (`beamtalk_object_class:init/1`'s default),
    /// while a compiled class's module is always `bt@…`-prefixed. The one
    /// rule for every reader of a registered module — the live-image meta
    /// decode and codegen's class-module index alike.
    #[must_use]
    pub fn registered_module_is_module_less(module: &str, class_name: &str) -> bool {
        module == class_name
    }

    /// Returns `true` if this class can be subclassed.
    #[must_use]
    pub fn can_be_subclassed(&self) -> bool {
        !self.is_sealed
    }

    /// Whether this class declares class variables (`classState:`) of its
    /// own — class variables are per class, never inherited (BT-3762). The
    /// one Rust-side answer to "does this class declare class state?" (ADR
    /// 0142 §1 rule 1). Whether a class *needs* its process is the chain-wide
    /// [`ClassHierarchy::chain_has_class_variables`], which reads this per
    /// class.
    #[must_use]
    pub fn has_class_variables(&self) -> bool {
        !self.class_variables.is_empty()
    }

    /// Whether `selector` is one of the process-bound class-side selectors
    /// (ADR 0142 §1 rule 3, [`PROCESS_BOUND_CLASS_SELECTORS`]) that keep their
    /// class-process or dedicated routing whatever the class declares.
    #[must_use]
    pub fn is_process_bound_class_selector(selector: &str) -> bool {
        PROCESS_BOUND_CLASS_SELECTORS.contains(&selector)
    }

    /// Whether `selector` is a class-variable reflection send (`hasField:`,
    /// `clearField:`, `fieldAt:`, `fieldAt:put:`; BT-4148): the class process
    /// answers it from its own state ahead of the class-method lookup
    /// (`beamtalk_object_class:is_class_var_reflection_selector/1`), so a
    /// class method of that name is never what a send reaches and is never
    /// direct-call eligible. Both sides are pinned to
    /// `runtime/apps/beamtalk_runtime/test/fixtures/class_var_reflection_selectors_conformance.json`.
    #[must_use]
    pub fn is_class_var_reflection_selector(selector: &str) -> bool {
        use crate::ast::WellKnownSelector;
        matches!(
            WellKnownSelector::from_name(selector),
            Some(
                WellKnownSelector::HasField
                    | WellKnownSelector::ClearField
                    | WellKnownSelector::FieldAt
                    | WellKnownSelector::FieldAtPut
            )
        )
    }

    /// The process-bound class-side selectors (ADR 0142 §1 rule 3), in a fixed
    /// order: see [`PROCESS_BOUND_CLASS_SELECTORS`]. Codegen emits them into
    /// every class's `__beamtalk_meta/0`.
    #[must_use]
    pub fn process_bound_class_selectors() -> &'static [&'static str] {
        PROCESS_BOUND_CLASS_SELECTORS
    }

    /// Build a `ClassInfo` from a parsed `ClassDefinition` AST node.
    ///
    /// Shared by `extract_class_infos` and `add_module_classes` to avoid
    /// duplicating the field-mapping logic.
    #[allow(clippy::too_many_lines)] // field-mapping function — length is proportional to struct fields
    pub(super) fn from_class_definition(class: &ClassDefinition) -> Self {
        let instance_methods: Vec<MethodInfo> = class
            .methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                is_internal: m.is_internal,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(DeclaredType::from),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(DeclaredType::from))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
                // Stamped after `ClassHierarchy::build` by
                // `trait_expansion::apply_origins` (ADR 0127) — see
                // `MethodInfo::origin`'s doc for why it can't be set here.
                origin: None,
                primitive: m.quoted_primitive_name().cloned(),
                constant_value: MethodInfo::recorded_constant(m, false),
            })
            .collect();

        let class_methods: Vec<MethodInfo> = class
            .class_methods
            .iter()
            .map(|m| MethodInfo {
                selector: m.selector.name(),
                arity: m.selector.arity(),
                kind: m.kind,
                defined_in: class.name.name.clone(),
                is_sealed: m.is_sealed,
                is_internal: m.is_internal,
                spawns_block: false,
                return_type: m.return_type.as_ref().map(DeclaredType::from),
                param_types: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.as_ref().map(DeclaredType::from))
                    .collect(),
                doc: m.doc_comment.clone().map(Into::into),
                // Class-side provisions are post-v1 (ADR 0127 §9); class
                // methods are never flattened in v1, so this is always `None`.
                origin: None,
                primitive: m.quoted_primitive_name().cloned(),
                constant_value: MethodInfo::recorded_constant(m, true),
            })
            .collect();

        let mut info = Self {
            name: class.name.name.clone(),
            superclass: class.superclass.as_ref().map(|s| s.name.clone()),
            is_sealed: class.is_sealed,
            is_abstract: class.is_abstract,
            is_typed: class.is_typed,
            is_internal: class.is_internal,
            package: None, // Populated later by the build pipeline or BEAM metadata
            is_value: class.class_kind == ClassKind::Value,
            is_native: class.backing_module.is_some(),
            // Set below for a `Value subclass:` (BT-4132's shared rule).
            auto_ctors: None,
            handle_scope: class.handle_scope.as_ref().map(|s| s.name.clone()),
            surface_incomplete: false,
            in_caller_new1: false,
            // AST-built classes always compile to a module.
            is_module_less: false,
            state: class.state.iter().map(|s| s.name.name.clone()).collect(),
            state_types: class
                .state
                .iter()
                .filter_map(|s| {
                    s.type_annotation
                        .as_ref()
                        .map(|ty| (s.name.name.clone(), DeclaredType::from(ty)))
                })
                .collect(),
            state_has_default: class
                .state
                .iter()
                .map(|s| (s.name.name.clone(), s.default_value.is_some()))
                .collect(),
            state_kinds: class
                .state
                .iter()
                .map(|s| (s.name.name.clone(), s.slot_kind))
                .collect(),
            initialize_assigns: class
                .methods
                .iter()
                .find(|m| m.kind == MethodKind::Primary && m.selector.name() == "initialize")
                .map(|m| crate::semantic_analysis::analyze_initialize_assigns(&m.body))
                .unwrap_or_default(),
            has_dynamic_field_writer: crate::semantic_analysis::has_dynamic_field_writer(
                &class.methods,
            ),
            methods: instance_methods,
            class_methods,
            class_variables: class
                .class_variables
                .iter()
                .map(|cv| ClassVarInfo {
                    name: cv.name.name.clone(),
                    ty: cv.type_annotation.as_ref().map(DeclaredType::from),
                    has_default: cv.default_value.is_some(),
                    kind: cv.slot_kind,
                })
                .collect(),
            type_params: class
                .type_params
                .iter()
                .map(|tp| tp.name.name.clone())
                .collect(),
            type_param_bounds: class
                .type_params
                .iter()
                .map(|tp| tp.bound.as_ref().map(|b| b.name.clone()))
                .collect(),
            superclass_type_args: {
                let own_params: Vec<EcoString> = class
                    .type_params
                    .iter()
                    .map(|tp| tp.name.name.clone())
                    .collect();
                class
                    .superclass_type_args
                    .iter()
                    .map(|ta| {
                        let declared = DeclaredType::from(ta);
                        if let DeclaredType::Simple(name) = &declared {
                            if let Some(idx) = own_params.iter().position(|p| p == name) {
                                return SuperclassTypeArg::ParamRef { param_index: idx };
                            }
                        }
                        SuperclassTypeArg::Concrete { declared }
                    })
                    .collect()
            },
        };
        // For `Value subclass:` classes, synthesize auto-generated slot
        // methods so the type checker and cross-file consumers can resolve
        // them, and record the constructor anchor. An indirect Value subclass
        // gets the same treatment from `propagate_class_kind`, once its chain
        // is known.
        if class.class_kind == ClassKind::Value {
            info.synthesize_value_surface(class);
        }
        info
    }

    /// Synthesize a class's Value slot methods from its AST and record the
    /// constructor anchor (BT-4132): the one place an AST-built class becomes
    /// eligible for direct construction, shared by `from_class_definition`
    /// (direct `Value subclass:`) and `ClassHierarchy::propagate_class_kind`
    /// (indirect Value subclasses, once the chain is known).
    pub(super) fn synthesize_value_surface(&mut self, class: &ClassDefinition) {
        let keyword_constructor = ClassHierarchy::add_value_auto_methods(
            class,
            &mut self.methods,
            &mut self.class_methods,
        );
        self.record_constructor_anchor(keyword_constructor);
    }

    /// Record the constructor anchor for a class compiled as a Value: its
    /// synthesized keyword constructor decision (BT-4132). Codegen emits the
    /// synthesized `new/0`, `new/1` and keyword constructor for every class
    /// whose post-writeback kind is `Value`, direct or indirect, so the anchor
    /// only makes the class eligible to *try* the direct path; the run-time
    /// guard (BT-4409) decides each send. Callers set it only where the
    /// keyword decision is known; everywhere else the anchor stays `None`
    /// (fail closed: no direct-construction verdicts).
    pub(super) fn record_constructor_anchor(&mut self, keyword_constructor: Option<EcoString>) {
        self.auto_ctors = Some(AutoConstructors {
            keyword_constructor,
        });
    }
}

/// Formats an AST expression as a compact string for use in generated doc comments.
///
/// Only handles simple literal values (integer, float, string, boolean, nil, list).
/// Complex expressions fall back to `"..."`.
pub fn format_default_value(expr: &Expression) -> String {
    match expr {
        Expression::Literal(lit, _) => crate::unparse::unparse_literal_display(lit),
        Expression::Identifier(ident) if ident.name == "nil" => "nil".to_string(),
        Expression::Identifier(ident) if ident.name == "true" => "true".to_string(),
        Expression::Identifier(ident) if ident.name == "false" => "false".to_string(),
        // `#(...)` list literal defaults (e.g. `field: xs :: List(String) = #()`)
        // parse as ListLiteral, not Literal::List — only the no-tail (non-cons)
        // form is renderable as flat Beamtalk source.
        Expression::ListLiteral {
            elements,
            tail: None,
            ..
        } => {
            let inner = elements
                .iter()
                .map(format_default_value)
                .collect::<Vec<_>>()
                .join(", ");
            format!("#({inner})")
        }
        _ => "...".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::format_default_value;
    use crate::ast::{Expression, Literal};
    use crate::source_analysis::Span;
    use crate::unparse::unparse_literal_display;

    fn span() -> Span {
        Span::new(0, 0)
    }

    // Parity tests: the stdlib-metadata path (`format_default_value`) must
    // render literals identically to the unparse path
    // (`unparse_literal_display`) — both must escape consistently rather
    // than `format_default_value` using its own hand-rolled, Rust-flavoured
    // escaping.

    #[test]
    fn string_with_embedded_quote_matches_unparse() {
        let lit = Literal::String("say \"hi\"".into());
        let expr = Expression::Literal(lit.clone(), span());
        assert_eq!(format_default_value(&expr), unparse_literal_display(&lit));
        assert_eq!(format_default_value(&expr), "\"say \"\"hi\"\"\"");
    }

    #[test]
    fn symbol_with_space_matches_unparse() {
        let lit = Literal::Symbol("with space".into());
        let expr = Expression::Literal(lit.clone(), span());
        assert_eq!(format_default_value(&expr), unparse_literal_display(&lit));
        assert_eq!(format_default_value(&expr), "#'with space'");
    }

    #[test]
    fn newline_character_matches_unparse() {
        let lit = Literal::Character('\n');
        let expr = Expression::Literal(lit.clone(), span());
        assert_eq!(format_default_value(&expr), unparse_literal_display(&lit));
        assert_eq!(format_default_value(&expr), "$\\n");
    }

    #[test]
    fn integral_float_matches_unparse() {
        let lit = Literal::Float(1.0);
        let expr = Expression::Literal(lit.clone(), span());
        assert_eq!(format_default_value(&expr), unparse_literal_display(&lit));
        assert_eq!(format_default_value(&expr), "1.0");
    }
}
