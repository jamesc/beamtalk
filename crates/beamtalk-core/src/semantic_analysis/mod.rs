// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis for Beamtalk.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module performs semantic analysis on the AST, including:
//! - Variable scope and lifetime analysis (via `scope` module)
//! - Pattern variable binding in match expressions
//! - Block context determination (control flow, stored, passed)
//! - Capture analysis for blocks
//! - Mutation tracking for captured variables in blocks
//!
//! The analysis produces diagnostics and metadata used by the code generator.

use crate::ast::Module;
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

pub mod alias_registry;
mod block_analyzer;
pub(crate) mod block_context;
pub mod block_facts;
pub mod class_hierarchy;
pub mod class_kind_writeback;
pub mod collision_checker;
pub mod error;
pub mod facts;
pub mod lowering;
pub(crate) mod method_validators;
pub mod module_validator;
pub mod name_resolver;
pub(crate) mod pattern_bindings;
pub mod primitive_validator;
pub mod protocol_registry;
pub mod receiver_knowledge;
pub mod return_type_writeback;
pub(crate) mod scope;
pub(crate) mod string_utils;
pub mod supervisor_kind_writeback;
pub mod type_checker;
pub(crate) mod validators;

// Property-based tests for semantic analysis (ADR 0011 Phase 2)
#[cfg(test)]
mod property_tests;
#[cfg(test)]
pub mod test_helpers;

pub use alias_registry::{AliasInfo, AliasRegistry};
pub use block_facts::BlockMutationAnalysis;
pub use block_facts::analyze_block;
pub use class_hierarchy::ClassHierarchy;
pub use class_kind_writeback::apply_class_kind_writeback;
pub use collision_checker::{
    DepInfo, DependencyRegistry, build_dependency_registry, build_dependency_registry_with_graph,
    check_collision_at_use_sites, check_stdlib_reservation, check_transitive_dep_usage,
};
pub use error::{SemanticError, SemanticErrorKind};
pub use facts::{DispatchKind, SemanticFacts, compute_semantic_facts};
pub use lowering::lower_module_for_codegen;
pub use name_resolver::NameResolver;
pub use pattern_bindings::{extract_match_arm_bindings, extract_pattern_bindings};
pub use protocol_registry::{ProtocolInfo, ProtocolRegistry};
pub use receiver_knowledge::{KnowledgeScope, ReceiverKnowledge, classify_receiver};
pub use return_type_writeback::{
    apply_return_type_writeback, apply_return_type_writeback_from_map,
    clear_return_type_writeback_for_key, clear_return_type_writeback_for_keys,
};
pub use scope::BindingKind;
pub use supervisor_kind_writeback::apply_supervisor_kind_writeback;
pub use type_checker::{
    ClassCoverage, CoverageReport, DynamicEntry, DynamicReason, InferredType, MethodReturnKey,
    TypeChecker, TypeMap, TypeProvenance, infer_method_return_types, infer_types,
    infer_types_and_returns,
};

/// BT-738: Warn when a user-defined class name shadows a stdlib built-in.
///
/// Must NOT be called for stdlib compilation (`stdlib_mode = true`). Call
/// this alongside `validate_primitives`, guarded by `!options.stdlib_mode`.
pub fn check_stdlib_name_shadowing(
    module: &Module,
    diagnostics: &mut Vec<crate::source_analysis::Diagnostic>,
) {
    validators::check_stdlib_name_shadowing(module, diagnostics);
}

/// Result of semantic analysis.
///
/// BT-3123: Also carries [`SemanticFacts`] and inferred method return types
/// (`method_return_types`) alongside the class hierarchy, protocol registry,
/// and alias registry already here — the full set of analysis outputs codegen
/// needs. This lets a driver that already ran [`analyse_full`] for
/// diagnostics hand the *same* `AnalysisResult` to codegen (via
/// `CodegenOptions::with_analysis`) instead of codegen re-deriving its own
/// view (a fresh `ClassHierarchy::build`, a fresh `compute_semantic_facts`,
/// and a full re-run of `infer_method_return_types` — see BT-3123).
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// Block metadata indexed by block span.
    pub block_info: HashMap<Span, BlockInfo>,

    /// Static class hierarchy (built-in + user-defined classes).
    pub class_hierarchy: ClassHierarchy,

    /// Protocol registry (ADR 0068 Phase 2b).
    pub protocol_registry: ProtocolRegistry,

    /// Type alias registry (ADR 0108 Phase 2, BT-2895).
    pub alias_registry: AliasRegistry,

    /// Pre-codegen semantic facts (BT-1288) computed in the same pass as the
    /// rest of analysis (BT-3123) — block mutation/capture profiles, dispatch
    /// classification, and non-local-return spans. Consumed by codegen via
    /// `CodegenOptions::with_analysis` instead of a second
    /// `compute_semantic_facts` call.
    pub semantic_facts: facts::SemanticFacts,

    /// Inferred return types for unannotated methods (BT-1005), keyed by
    /// `(ClassName, Selector, IsClassMethod)`. Populated from the same
    /// [`TypeChecker`] pass that builds `type_map` during Phase 2 below —
    /// previously discarded here and re-derived by codegen's return-type
    /// writeback pass via a second, full `infer_method_return_types` call
    /// (BT-3123). `Dynamic` results are omitted (absence = dynamic), matching
    /// [`type_checker::infer_method_return_types`]'s own contract.
    pub method_return_types: HashMap<type_checker::MethodReturnKey, InferredType>,

    /// Every alias name transitively referenced by an annotation resolved
    /// during this compile (ADR 0108 hot-reload re-check trigger, BT-2899).
    ///
    /// Sorted and deduplicated. Populated from
    /// [`type_checker::TypeChecker::take_referenced_aliases`] — see that
    /// method's doc for why the set already spans the full transitive
    /// expansion walk (resolving `p :: B` where `type B = A | #z` records
    /// both `B` and `A`). Consumed by the compiler port to answer "does this
    /// class's compile depend on alias X?" without re-parsing — the
    /// alias-name-keyed candidate lookup a live alias redefinition's
    /// re-check trigger needs (unlike ADR 0107's `trigger_leaf_change/1`,
    /// which has no such key and sweeps every live class).
    ///
    /// **Scope boundary:** only spans the annotation-resolution call sites
    /// that were switched to `resolve_type_annotation_with_alias_deps`
    /// (method parameters, return types, local `::` assignments, generic
    /// type-parameter bounds — every production caller that already threads
    /// `alias_registry`). A `state:`/`field:` declared type does **not**
    /// currently flow through alias resolution at all (`check_state_defaults`
    /// compares against `type_annotation.type_name()`, the raw written name,
    /// never `resolve_type_annotation`), so a state field typed with an
    /// alias is invisible to this set today — harmless *only* because state
    /// fields don't expand aliases yet either. If a future change adds alias
    /// expansion to state-field types, that call site must also be switched
    /// to the `_with_alias_deps` variant, or its class silently drops out of
    /// `beamtalk_alias_xref` and a live redefinition stops re-checking it.
    pub referenced_aliases: Vec<EcoString>,

    /// Per-expression inferred types (ADR 0025), keyed by file-absolute
    /// `Span`. Recovered from the same [`TypeChecker`] pass that already
    /// computes `method_return_types` above — `analyse_full` destructures
    /// the `Analyser` at the end of analysis to recover it (rather than a
    /// consuming accessor mid-pass, since `analyser.result` is still
    /// consumed afterward for `block_info`/`diagnostics`).
    ///
    /// Consumed by codegen (via `CodegenOptions::with_analysis` /
    /// `CoreErlangGenerator::type_map`) to project a message send's receiver
    /// type onto the xref `recv_type` field (BT-3217, ADR 0115 Phase 2) —
    /// see `docs/internal/adr-0115-phase1-spike-findings.md` §1a/§1d for why
    /// this field was previously computed and discarded rather than plumbed
    /// through.
    pub type_map: TypeMap,
}

impl AnalysisResult {
    /// Create a new empty analysis result.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            block_info: HashMap::new(),
            class_hierarchy: ClassHierarchy::with_builtins(),
            protocol_registry: ProtocolRegistry::new(),
            alias_registry: AliasRegistry::new(),
            semantic_facts: facts::SemanticFacts::default(),
            method_return_types: HashMap::new(),
            referenced_aliases: Vec::new(),
            type_map: TypeMap::new(),
        }
    }
}

impl Default for AnalysisResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Information about a block expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockInfo {
    /// Context in which the block is used.
    pub context: BlockContext,

    /// Variables captured from outer scopes.
    pub captures: Vec<CapturedVar>,

    /// Mutations that occur within the block.
    pub mutations: Vec<Mutation>,
}

/// Context in which a block is used, routing between codegen tiers (ADR 0041).
///
/// - `ControlFlow` → **Tier 1** inline codegen (optimized pack/unpack)
/// - `Stored` / `Passed` / `Other` / `Unknown` → **Tier 2** universal stateful protocol
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockContext {
    /// Block in control flow position → **Tier 1** inline codegen.
    /// Whitelisted selectors (whileTrue:, ifTrue:, do:, etc.) generate optimized
    /// pack/unpack scaffolding directly.
    ControlFlow,

    /// Block stored in a variable or field → **Tier 2** stateful protocol.
    /// Captured variable mutations are threaded through `StateAcc` maps.
    Stored,

    /// Block passed as argument to a message send → **Tier 2** stateful protocol.
    Passed,

    /// Other known context (e.g., immediate evaluation) → **Tier 2** by default.
    Other,

    /// Context could not be determined → **Tier 2** by default.
    Unknown,
}

/// A variable captured from an outer scope.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CapturedVar {
    /// Name of the captured variable.
    pub name: EcoString,

    /// Span where the variable was defined.
    pub defined_at: Span,
}

/// A mutation that occurs within a block or method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mutation {
    /// Kind of mutation.
    pub kind: MutationKind,

    /// Span of the mutation.
    pub span: Span,
}

/// Type of mutation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MutationKind {
    /// Assignment to a local variable.
    LocalVariable { name: EcoString },

    /// Assignment to a captured variable.
    CapturedVariable { name: EcoString },

    /// Assignment to an object field.
    Field { name: EcoString },
}

/// Bundles the knobs threaded through [`analyse_full`], the single semantic
/// analysis entry point (BT-3114, consolidating BT-2804's wrapper family).
///
/// Call sites build a context with the builder methods below —
/// `AnalysisContext::default().with_known_vars(...).with_natives(...)` — and
/// pass it to `analyse_full` instead of calling one of a dozen
/// progressively-richer `analyse_with_*` free functions. Each field's
/// `Default` reproduces today's most conservative behaviour (empty
/// cross-file knowledge, `ModuleOnly` scope, no package context) — so a
/// context that leaves a field unset degrades safely (quietly loses
/// precision) rather than silently misbehaving.
#[derive(Debug, Default)]
pub struct AnalysisContext<'a> {
    /// Pre-defined variables treated as already bound (REPL context).
    pub known_vars: &'a [&'a str],
    /// Permits built-in classes to subclass sealed classes (BT-791);
    /// only set when compiling stdlib sources.
    pub stdlib_mode: bool,
    /// Suppresses the effect-free module-level expression lint
    /// (bootstrap-test compilation).
    pub skip_module_expression_lint: bool,
    /// Cross-file class metadata injected into the class hierarchy before
    /// type checking (BT-1523, ADR 0050 Phase 4).
    pub pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
    /// Protocol definitions extracted from other source files, e.g. `BUnit`
    /// fixtures (BT-2006).
    pub pre_loaded_protocols: Vec<protocol_registry::ProtocolInfo>,
    /// Type alias definitions extracted from other source files or packages
    /// (BT-2898, ADR 0108 Phase 5), *or* type aliases declared in earlier
    /// turns of the same REPL session (ADR 0108 Phase 8, BT-2902) — both
    /// uses funnel through the same field and the same
    /// `AliasRegistry::add_pre_loaded` seeding call in `analyse_full`.
    /// `internal` entries are excluded at the seeding boundary before this
    /// ever reaches `AliasRegistry::add_pre_loaded` when compiling with a
    /// package context; a REPL/script session (no `current_package`) has no
    /// package boundary to enforce, so carried-over `internal` aliases stay
    /// visible. Aliases erase to nothing at runtime, so (unlike
    /// `pre_loaded_classes`) there is no live BEAM artifact to recover a
    /// REPL session's prior declarations from — the REPL layer re-parses
    /// each previously successfully-declared `type Name = ...` line
    /// standalone every turn and passes the result here so
    /// `resolve_type_annotation` sees names declared in prior turns.
    pub pre_loaded_aliases: Vec<alias_registry::AliasInfo>,
    /// Known package names for package-qualifier validation (ADR 0070
    /// Phase 2). `None` skips the check entirely.
    pub known_packages: Option<std::collections::HashSet<String>>,
    /// The package the module being analysed belongs to (ADR 0071).
    pub current_package: Option<&'a str>,
    /// Native type registry for FFI call inference (ADR 0075).
    pub native_type_registry: Option<std::sync::Arc<type_checker::NativeTypeRegistry>>,
    /// How complete the injected cross-file class knowledge is (BT-2796,
    /// ADR 0100 Rule 2). Defaults to the conservative `ModuleOnly`.
    pub knowledge_scope: KnowledgeScope,
    /// Project-wide standalone extension definitions (BT-2795, ADR 0066).
    /// `None`/empty means only the current module's own extensions are
    /// visible.
    pub cross_file_extensions: Option<&'a crate::compilation::extension_index::ExtensionIndex>,
    /// Whether the current package has dependencies whose extensions are
    /// not visible here (BT-2794, ADR 0100 Rule 2).
    pub has_package_dependencies: bool,
}

impl<'a> AnalysisContext<'a> {
    /// Pre-defined variables treated as already bound (REPL context).
    ///
    /// Prevents "Undefined variable" errors for REPL session variables.
    #[must_use]
    pub fn with_known_vars(mut self, known_vars: &'a [&'a str]) -> Self {
        self.known_vars = known_vars;
        self
    }

    /// Applies the subset of [`crate::CompilerOptions`] that semantic
    /// analysis cares about: `stdlib_mode`, `skip_module_expression_lint`,
    /// `current_package`, `knowledge_scope`, `has_package_dependencies`.
    #[must_use]
    pub fn with_options(mut self, options: &'a crate::CompilerOptions) -> Self {
        self.stdlib_mode = options.stdlib_mode;
        self.skip_module_expression_lint = options.skip_module_expression_lint;
        self.current_package = options.current_package.as_deref();
        self.knowledge_scope = options.knowledge_scope;
        self.has_package_dependencies = options.has_package_dependencies;
        self
    }

    /// Cross-file class metadata injected into the class hierarchy before
    /// type checking (BT-1523, ADR 0050 Phase 4).
    #[must_use]
    pub fn with_pre_loaded_classes(
        mut self,
        pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
    ) -> Self {
        self.pre_loaded_classes = pre_loaded_classes;
        self
    }

    /// Protocol definitions extracted from other source files, e.g. `BUnit`
    /// fixtures (BT-2006).
    #[must_use]
    pub fn with_pre_loaded_protocols(
        mut self,
        pre_loaded_protocols: Vec<protocol_registry::ProtocolInfo>,
    ) -> Self {
        self.pre_loaded_protocols = pre_loaded_protocols;
        self
    }

    /// Type alias definitions extracted from other source files/packages, or
    /// carried over from earlier turns of the same REPL session — see the
    /// `pre_loaded_aliases` field's own doc for the full semantics.
    #[must_use]
    pub fn with_pre_loaded_aliases(
        mut self,
        pre_loaded_aliases: Vec<alias_registry::AliasInfo>,
    ) -> Self {
        self.pre_loaded_aliases = pre_loaded_aliases;
        self
    }

    /// Known package names for package-qualifier validation (ADR 0070 Phase
    /// 2). Unknown package qualifiers produce compile errors.
    #[allow(clippy::implicit_hasher)] // concrete HashSet is simpler for callers
    #[must_use]
    pub fn with_known_packages(
        mut self,
        known_packages: std::collections::HashSet<String>,
    ) -> Self {
        self.known_packages = Some(known_packages);
        self
    }

    /// Native type registry for FFI call inference (ADR 0075). When `Some`,
    /// FFI calls (`Erlang <module> <function>:`) get return type inference
    /// and keyword mismatch warnings from the registry.
    #[must_use]
    pub fn with_native_type_registry(
        mut self,
        native_type_registry: Option<std::sync::Arc<type_checker::NativeTypeRegistry>>,
    ) -> Self {
        self.native_type_registry = native_type_registry;
        self
    }

    /// Project-wide standalone extension definitions (BT-2795, ADR 0066), so
    /// a cross-file `ClassName >> selector` extension resolves instead of
    /// producing a false `Dnu` hint. May safely include the current file's
    /// own entries — the current module's extensions are registered first
    /// and duplicates are skipped.
    #[must_use]
    pub fn with_cross_file_extensions(
        mut self,
        cross_file_extensions: &'a crate::compilation::extension_index::ExtensionIndex,
    ) -> Self {
        self.cross_file_extensions = Some(cross_file_extensions);
        self
    }
}

/// Perform semantic analysis on a module.
///
/// This is the main entry point for semantic analysis. It orchestrates the
/// `NameResolver` and `TypeChecker` domain services to analyze the module AST
/// and returns diagnostics and metadata for code generation.
///
/// **DDD Context:** Semantic Analysis
///
/// This function orchestrates the following domain services:
/// - `NameResolver`: Resolves identifiers to bindings and detects undefined variables
/// - `TypeChecker`: Validates type constraints (currently stub)
/// - `Analyser`: Performs block context analysis, capture tracking, and mutation detection
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::analyse;
/// # use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
/// let module = Module::new(vec![], Span::default());
/// let result = analyse(&module);
/// assert_eq!(result.diagnostics.len(), 0);
/// ```
pub fn analyse(module: &Module) -> AnalysisResult {
    analyse_full(module, AnalysisContext::default())
}

/// Perform semantic analysis on a module with a fully-populated
/// [`AnalysisContext`] (BT-3114, the single entry point that replaces the
/// former `analyse_with_*` wrapper family — REPL known-vars, compiler
/// options, cross-file classes/protocols/aliases, native FFI types,
/// cross-file extensions, and package-qualifier validation all thread
/// through `ctx`, built via its `with_*` builder methods).
///
/// ADR 0075: When `ctx.native_type_registry` is `Some`, FFI calls (`Erlang <module> <function>:`)
/// get return type inference and keyword mismatch warnings from the registry.
///
/// # Panics
///
/// Never panics in practice: `ClassHierarchy::build_with_options` is
/// documented infallible and its `Result` is unwrapped internally with an
/// `.expect()` that exists only to surface a contract violation loudly
/// rather than silently, should that documented invariant ever break.
///
/// # Examples
///
/// ```
/// # use beamtalk_core::semantic_analysis::{analyse_full, AnalysisContext};
/// # use beamtalk_core::ast::Module;
/// # use beamtalk_core::source_analysis::Span;
/// let module = Module::new(vec![], Span::default());
/// let known_vars = ["x"];
/// let ctx = AnalysisContext::default().with_known_vars(&known_vars);
/// let result = analyse_full(&module, ctx);
/// assert_eq!(result.diagnostics.len(), 0);
/// ```
#[allow(clippy::too_many_lines)] // orchestration function — one call per analysis phase
pub fn analyse_full(module: &Module, ctx: AnalysisContext<'_>) -> AnalysisResult {
    let AnalysisContext {
        known_vars,
        stdlib_mode,
        skip_module_expression_lint,
        pre_loaded_classes,
        pre_loaded_protocols,
        pre_loaded_aliases,
        known_packages,
        current_package,
        native_type_registry,
        knowledge_scope,
        cross_file_extensions,
        has_package_dependencies,
    } = ctx;

    let mut result = AnalysisResult::new();

    // BT-1288 / BT-3123: Compute pre-codegen semantic facts in the same pass
    // as the rest of analysis, on the same (pre-writeback) module AST that
    // codegen's own `compute_semantic_facts(module)` call used to re-derive.
    // A pure AST walk with no hierarchy/type dependency, so it can run
    // anywhere in this function; done first for symmetry with the writeback
    // trio's ordering in codegen.
    result.semantic_facts = facts::compute_semantic_facts(module);

    // Phase 0: Build Class Hierarchy (ADR 0006 Phase 1a)
    let (hierarchy_result, hierarchy_diags) =
        ClassHierarchy::build_with_options(module, stdlib_mode);
    // build_with_options is infallible; propagate any diagnostics it produced
    result.class_hierarchy = hierarchy_result.expect("ClassHierarchy::build is infallible");
    result.diagnostics.extend(hierarchy_diags);

    // BT-2796: Record how complete the injected cross-file knowledge is so
    // the receiver-knowledge classifier can consult it (ADR 0100 Rule 2).
    result.class_hierarchy.set_knowledge_scope(knowledge_scope);
    result
        .class_hierarchy
        .set_dependency_extensions_unknown(has_package_dependencies);

    // ADR 0071 BT-1700: Stamp current package on AST-derived classes
    if let Some(pkg) = current_package {
        result.class_hierarchy.stamp_package(pkg);
    }

    // BT-1726: Remember whether cross-file class metadata was provided so the
    // structural validator can decide whether unresolved-class warnings are useful.
    let has_cross_file_classes = !pre_loaded_classes.is_empty();

    // BT-2088: Filter out pre-loaded class entries whose names match a protocol
    // definition in the current module. The compiler server's class cache
    // includes synthetic protocol class entries from prior loads; injecting
    // them into the hierarchy would cause a spurious "namespace collision"
    // error when `register_module` sees the protocol name as an existing class.
    let pre_loaded_classes = if !module.protocols.is_empty() && !pre_loaded_classes.is_empty() {
        let current_protocol_names: std::collections::HashSet<&ecow::EcoString> =
            module.protocols.iter().map(|p| &p.name.name).collect();
        pre_loaded_classes
            .into_iter()
            .filter(|ci| !current_protocol_names.contains(&ci.name))
            .collect()
    } else {
        pre_loaded_classes
    };

    // ADR 0050 Phase 4: inject REPL-session class metadata before TypeChecking
    if !pre_loaded_classes.is_empty() {
        result
            .class_hierarchy
            .add_from_beam_meta(pre_loaded_classes);
        // BT-1559: Re-propagate class kind after cross-file classes are injected.
        // Both module-local classes (whose superclass is in another file) and
        // cross-file classes (whose is_value wasn't set at extraction time) may
        // need fixup. propagate_class_kind handles AST classes from this module;
        // propagate_cross_file_class_kind handles all remaining classes.
        result.class_hierarchy.propagate_class_kind(module);
        result.class_hierarchy.propagate_cross_file_class_kind();
    }

    // ADR 0066 Phase 4: Register extension methods into the class hierarchy
    // so the type checker sees them as part of each class's method surface.
    // Extension methods defined via `ClassName >> selector => ...` are collected
    // from the current module and added to the hierarchy before type checking.
    if !module.method_definitions.is_empty() {
        let mut ext_index = crate::compilation::extension_index::ExtensionIndex::new();
        ext_index.add_module(module, std::path::Path::new("<current>"));
        result.class_hierarchy.register_extensions(&ext_index);
    }

    // BT-2795 (ADR 0066 / ADR 0100 Rule 2 WS1): Register project-wide
    // cross-file extensions so a same-project `ClassName >> selector`
    // defined in another file resolves instead of producing a false `Dnu`
    // hint. Registered *after* the current module's own extensions —
    // `register_extensions` skips selectors the class already defines, so
    // the current file's definitions win and an index that includes the
    // current file's own entries is harmless.
    if let Some(cross_file_extensions) = cross_file_extensions {
        if !cross_file_extensions.is_empty() {
            result
                .class_hierarchy
                .register_extensions(cross_file_extensions);
        }
    }

    // Phase 0.5: Protocol Registration (ADR 0068 Phase 2b)
    // Register protocol definitions from the module into the protocol registry.
    // Must happen after the class hierarchy is fully built (for namespace collision
    // checks) and before type checking (so protocol names resolve in type annotations).
    //
    // BT-2006: Seed the registry with pre-loaded protocols (e.g. BUnit fixture
    // protocols) *before* registering the current module's protocols so that
    // fixture protocol names are visible to `extending:` resolution and the
    // unresolved-class validator. Skip pre-loaded entries whose names also
    // appear in the current module — this mirrors `cross_file_class_infos`
    // and prevents spurious "Duplicate protocol definition" errors when a
    // fixture file is itself being compiled (its own protocols are already
    // present in the pre_loaded slice).
    if !pre_loaded_protocols.is_empty() {
        let current_names: std::collections::HashSet<&EcoString> =
            module.protocols.iter().map(|p| &p.name.name).collect();
        let cross_file_protocols: Vec<_> = pre_loaded_protocols
            .into_iter()
            .filter(|p| !current_names.contains(&p.name))
            .collect();
        let collision_diags = result
            .protocol_registry
            .add_pre_loaded(cross_file_protocols, &result.class_hierarchy);
        result.diagnostics.extend(collision_diags);
    }
    if !module.protocols.is_empty() {
        let proto_diags = result
            .protocol_registry
            .register_module(module, &result.class_hierarchy);
        result.diagnostics.extend(proto_diags);
    }

    // Phase 0.6: Type Alias Registration (ADR 0108 Phase 2/5/8,
    // BT-2895/BT-2898/BT-2902)
    // Must happen after both the class hierarchy and protocol registry are
    // fully built for the current module — aliases share the class/protocol
    // namespace, and this ordering (classes → protocols → aliases) is what
    // gives `AliasRegistry::register_module` bidirectional collision
    // detection within a single batch compile (see its doc comment).
    //
    // BT-2898: Seed the registry with pre-loaded aliases (e.g. other files in
    // the same package, or a dependency's exported aliases) *before*
    // registering the current module's own aliases — mirrors the protocol
    // seeding immediately above. Skip pre-loaded entries whose names also
    // appear in the current module (current-module wins), and let
    // `add_pre_loaded` itself apply the seeding-boundary exclusion for
    // `internal` entries from a *different* package (ADR 0108 Semantics) —
    // a same-package `internal` entry (e.g. another file in the same
    // multi-file package) is still seeded, since ADR 0108 scopes `internal`
    // aliases to the whole declaring *package*, not just the declaring file.
    //
    // ADR 0108 Phase 8 (BT-2902): the same field/call also carries aliases
    // declared in earlier turns of the same REPL session — filtering out any
    // name the current module/turn itself redeclares (current turn wins,
    // same as above) keeps a live `type Foo = ...` redefinition from
    // tripping `register_module`'s duplicate-name check (ADR 0108 Semantics:
    // a live session can legally redefine an alias). `referenced_aliases`
    // (ADR 0108 hot-reload re-check trigger, BT-2899, below) is what lets
    // the Erlang side re-check dependent annotation sites once this
    // redefinition installs. A REPL session has no `current_package`, so
    // the seeding-boundary exclusion above never filters a carried-over
    // `internal` alias out.
    if !pre_loaded_aliases.is_empty() {
        let current_alias_names: std::collections::HashSet<&EcoString> =
            module.type_aliases.iter().map(|a| &a.name.name).collect();
        let cross_file_aliases: Vec<_> = pre_loaded_aliases
            .into_iter()
            .filter(|a| !current_alias_names.contains(&a.name))
            .collect();
        let collision_diags = result.alias_registry.add_pre_loaded(
            cross_file_aliases,
            &result.class_hierarchy,
            &result.protocol_registry,
            current_package,
        );
        result.diagnostics.extend(collision_diags);
    }
    if !module.type_aliases.is_empty() {
        let alias_diags = result.alias_registry.register_module(
            module,
            &result.class_hierarchy,
            &result.protocol_registry,
        );
        result.diagnostics.extend(alias_diags);
    }

    // Phase 1: Name Resolution
    let mut name_resolver = NameResolver::new();
    name_resolver.define_known_vars(known_vars, module.span);
    name_resolver.resolve_module(module);
    result.diagnostics.extend(name_resolver.take_diagnostics());

    // Extract scope from NameResolver to pass to Analyser
    // This eliminates duplicate scope building - the scope already contains:
    // - Built-in identifiers (true, false, nil)
    // - Known REPL variables
    // - All variable bindings from name resolution
    let scope = name_resolver.into_scope();

    // Phase 2: Type Checking (ADR 0025 Phase 1 — zero-syntax inference)
    // ADR 0071 Phase 3 (BT-1702): Create type checker with package context for
    // internal method visibility enforcement (E0403).
    let mut type_checker = if let Some(pkg) = current_package {
        TypeChecker::with_package(pkg)
    } else {
        TypeChecker::new()
    };
    // ADR 0075: Wire native type registry for FFI call inference.
    if let Some(registry) = native_type_registry {
        type_checker.set_native_type_registry(registry);
    }
    type_checker.check_module_with_protocols_and_aliases(
        module,
        &result.class_hierarchy,
        &result.protocol_registry,
        &result.alias_registry,
    );
    result.diagnostics.extend(type_checker.take_diagnostics());
    // ADR 0108 hot-reload re-check trigger (BT-2899): sorted, deduplicated
    // snapshot of every alias name this compile's annotations transitively
    // depended on — see the field's own doc.
    let mut referenced_aliases: Vec<EcoString> =
        type_checker.take_referenced_aliases().into_iter().collect();
    referenced_aliases.sort();
    result.referenced_aliases = referenced_aliases;
    // BT-3123: capture the method-return-type inferences this same
    // `TypeChecker` pass already computed, so codegen's return-type
    // writeback pass can consume them instead of re-running inference.
    result.method_return_types = type_checker.take_method_return_types();
    let type_map = type_checker.take_type_map();

    // BT-2140: Lint redundant local-variable type annotations using the
    // populated TypeMap. Must run before the Analyser consumes `type_map`.
    validators::check_redundant_local_type_annotation(module, &type_map, &mut result.diagnostics);

    // Phase 3: Block Context Analysis (captures, mutations, context determination)
    // Receive scope from NameResolver and TypeMap from TypeChecker
    let mut analyser = Analyser::with_scope(scope, type_map);

    analyser.analyse_module(module);
    // ADR 0103 Phase 2 (BT-2756): block-capture sendability. Runs here where
    // both the type_map (owned by the analyser) and the computed block captures
    // are live.
    validators::check_block_capture_sendability(
        module,
        &result.class_hierarchy,
        analyser.type_map(),
        &analyser.result.block_info,
        Some(&result.alias_registry),
        &mut result.diagnostics,
    );
    // BT-3217 (ADR 0115 Phase 2): recover the `TypeMap` by destructuring
    // `analyser` here — a consuming accessor mid-pass isn't viable since
    // `analyser.result` is still read/moved-from below (`diagnostics`, then
    // `block_info`), per the ADR 0115 Phase 1 spike's §1d plumbing note.
    let Analyser {
        result: analyser_result,
        type_map: analyser_type_map,
        ..
    } = analyser;
    result.diagnostics.extend(analyser_result.diagnostics);
    result.block_info = analyser_result.block_info;
    result.type_map = analyser_type_map;

    // Phase 4: Abstract instantiation check (BT-105)
    validators::check_abstract_instantiation(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // Phase 5: Class-aware diagnostics (BT-563, BT-1540)
    validators::check_actor_new_usage(module, &result.class_hierarchy, &mut result.diagnostics);
    validators::check_object_new_usage(module, &result.class_hierarchy, &mut result.diagnostics);
    validators::check_new_field_names(module, &result.class_hierarchy, &mut result.diagnostics);
    // BT-2718: Reject user names that collide with the reserved internal `__`
    // state-key namespace (`__local__…`, `__methods__`, `__class_mod__`).
    validators::check_reserved_internal_names(module, &mut result.diagnostics);
    // BT-2997: Reject method declarations for the equality operators codegen
    // lowers straight to Erlang BIFs (`=:=`, `=/=`, `==`, `/=`, ADR 0002) —
    // they never dispatch, so such a method is silently dead code.
    validators::check_non_overridable_operator_methods(module, &mut result.diagnostics);
    validators::check_class_variable_access(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    validators::check_empty_method_bodies(module, &mut result.diagnostics);
    validators::check_value_slot_assignment(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // ADR 0103: `handleScope:` is only meaningful on Object-kind classes.
    validators::check_handle_scope_on_object(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // ADR 0103: nudge undeclared FFI-wrapping handle classes. Suppressed for
    // stdlib compilation — the builtin tier table already covers the stdlib.
    if !stdlib_mode {
        validators::check_undeclared_handle_class(
            module,
            &result.class_hierarchy,
            &mut result.diagnostics,
        );
    }
    // BT-919: Reject cast (!) on value types
    validators::check_cast_on_value_type(module, &result.class_hierarchy, &mut result.diagnostics);
    // BT-1793: Reject actor state mutation inside non-state-threading block closures
    validators::check_actor_field_mutation_in_closure(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // BT-950: Warn on redundant assignment (x := x)
    validators::check_redundant_assignment(module, &mut result.diagnostics);
    // BT-955: Warn on literal boolean conditions (always true / always false)
    validators::check_literal_boolean_condition(module, &mut result.diagnostics);
    // BT-1955: Warn on redundant `super initialize` in Actor initialize methods (ADR 0078 Phase 2)
    validators::check_redundant_super_initialize(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // BT-1052: Error on -> Nil return type on Value instance methods
    validators::check_value_nil_return(module, &result.class_hierarchy, &mut result.diagnostics);
    // BT-1218: Validate supervisionPolicy overrides + warn for children without explicit policy
    validators::check_supervision_policy_override(module, &mut result.diagnostics);
    validators::check_children_supervision_policy(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // BT-1207: Native actor validation (ADR 0056)
    validators::check_native_state_fields(module, &result.class_hierarchy, &mut result.diagnostics);
    validators::check_native_delegate_return_type(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );
    // BT-2720: Reserved-word backing-function check for native Objects (ADR 0101)
    validators::check_native_delegate_reserved_word(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // BT-1535: Error on data keyword / class-kind mismatch (ADR 0067 Phase 4)
    validators::check_data_keyword_class_kind(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // BT-2830: Error on Value subclass slots that collide on the auto-generated
    // `with*:` setter selector (case-insensitive first-letter collision).
    validators::check_value_slot_case_collision(
        module,
        &result.class_hierarchy,
        &mut result.diagnostics,
    );

    // BT-1299: Error on non-exhaustive match: for sealed types (e.g. Result missing error: arm)
    validators::check_match_exhaustiveness(module, &mut result.diagnostics);

    // Warn on local variable assignments inside match arm bodies (footgun — the
    // assignment has no effect because Core Erlang case arms don't leak bindings).
    validators::warn_assignment_in_match_arms(module, &mut result.diagnostics);

    // BT-951/BT-979: Lint on effect-free statements (suppressed during normal compile).
    // Module-level expressions are checked by default; set skip_module_expression_lint
    // to opt out (bootstrap-test compilation uses this).
    validators::check_effect_free_statements(
        module,
        &mut result.diagnostics,
        skip_module_expression_lint,
    );

    // Phase 5b: Structural validation (BT-1726)
    // Only check unresolved classes when cross-file metadata has been loaded
    // (pre_loaded_classes non-empty). Without it, any class reference might
    // be a cross-file dependency that we simply don't know about yet.
    if has_cross_file_classes {
        validators::check_unresolved_classes(
            module,
            &result.class_hierarchy,
            &result.protocol_registry,
            known_vars,
            &mut result.diagnostics,
        );
    }
    // BT-2897 / ADR 0108: warn when a type annotation closely resembles a
    // registered alias name but doesn't resolve to one. Gated on
    // `has_cross_file_classes` for the same open-world reason as the
    // unresolved-class check above: without cross-file metadata, a name
    // that looks like a near-miss of a *local* alias might actually be a
    // legitimate cross-file class we simply haven't loaded yet — flagging it
    // would be a false positive, not a real typo.
    if has_cross_file_classes {
        validators::check_unresolved_type_aliases(
            module,
            &result.class_hierarchy,
            &result.protocol_registry,
            &result.alias_registry,
            &mut result.diagnostics,
        );
    }
    // BT-2854 / ADR 0107 Phase A: validate `Pattern::Type` class names in
    // `match:` arms (unknown class, non-leaf class, `Character` exclusion).
    // The unknown-class branch is internally gated on `has_cross_file_classes`,
    // same open-world policy as `check_unresolved_classes` above; the
    // leaf-class and `Character` checks run unconditionally.
    validators::check_type_pattern_classes(
        module,
        &result.class_hierarchy,
        has_cross_file_classes,
        &mut result.diagnostics,
    );
    // BT-1759: Warn when a workspace binding shadows a class name.
    // This check works against the full class hierarchy (including locally
    // defined classes), so it does not require cross-file metadata.
    validators::check_workspace_shadows(
        &result.class_hierarchy,
        known_vars,
        &mut result.diagnostics,
    );
    // Warn on Erlang FFI calls to unknown modules.
    validators::check_unresolved_ffi_modules(module, &mut result.diagnostics);
    // Warn on Erlang FFI calls with wrong arity for known functions.
    validators::check_ffi_arity(module, &mut result.diagnostics);

    // Phase 6: Module-level validation (BT-349, BT-1666)
    let module_diags = module_validator::validate_single_definition(module);
    result.diagnostics.extend(module_diags);

    // Phase 7: Package qualifier validation (ADR 0070 Phase 2)
    if let Some(packages) = known_packages {
        validators::check_package_qualifiers(module, &packages, &mut result.diagnostics);
    }

    // Phase 8: Visibility enforcement (ADR 0071)
    // E0401: cross-package internal class references (BT-1701)
    // E0402: leaked visibility — internal class/alias in public signature (BT-1701/BT-2898)
    validators::check_class_visibility(
        module,
        &result.class_hierarchy,
        &result.alias_registry,
        current_package,
        &mut result.diagnostics,
    );
    // E0402: internal method satisfying a public protocol requirement (BT-1702)
    validators::check_leaked_method_visibility(
        module,
        &result.class_hierarchy,
        &result.protocol_registry,
        current_package,
        &mut result.diagnostics,
    );
    // E0402 (BT-2898, ADR 0108 Semantics): a public alias whose expansion
    // transitively reaches an internal class/alias leaks it, even when the
    // internal name never appears directly in any signature.
    validators::check_alias_leaked_visibility(
        module,
        &result.class_hierarchy,
        &result.alias_registry,
        current_package,
        &mut result.diagnostics,
    );
    // W0401: subclass method shadowing an internal superclass method (BT-1702)
    // Only meaningful in a package context — skip for REPL/scripts
    if current_package.is_some() {
        validators::check_internal_method_shadow(
            module,
            &result.class_hierarchy,
            &mut result.diagnostics,
        );
    }

    result
}

mod analyser;
use analyser::{Analyser, ExprContext};

#[cfg(test)]
mod tests;
