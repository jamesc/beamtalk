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

mod block_analyzer;
pub(crate) mod block_context;
pub mod block_facts;
pub mod class_hierarchy;
pub mod class_kind_writeback;
pub mod collision_checker;
pub mod error;
pub mod facts;
pub(crate) mod method_validators;
pub mod module_validator;
pub mod name_resolver;
pub(crate) mod pattern_bindings;
pub mod primitive_validator;
pub mod protocol_registry;
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
pub use name_resolver::NameResolver;
pub use pattern_bindings::{extract_match_arm_bindings, extract_pattern_bindings};
pub use protocol_registry::{ProtocolInfo, ProtocolRegistry};
pub use return_type_writeback::apply_return_type_writeback;
pub use scope::BindingKind;
pub use supervisor_kind_writeback::apply_supervisor_kind_writeback;
pub use type_checker::{
    InferredType, MethodReturnKey, TypeChecker, TypeMap, TypeProvenance, infer_method_return_types,
    infer_types, infer_types_and_returns,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisResult {
    /// Diagnostics (errors and warnings) from analysis.
    pub diagnostics: Vec<Diagnostic>,

    /// Block metadata indexed by block span.
    pub block_info: HashMap<Span, BlockInfo>,

    /// Static class hierarchy (built-in + user-defined classes).
    pub class_hierarchy: ClassHierarchy,

    /// Protocol registry (ADR 0068 Phase 2b).
    pub protocol_registry: ProtocolRegistry,
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
    analyse_full(module, &[], false, false, vec![], None, None)
}

/// Analyse a module with pre-defined variables (for REPL context).
///
/// Variables passed in `known_vars` are treated as already defined,
/// preventing "Undefined variable" errors for REPL session variables.
///
/// **DDD Context:** Semantic Analysis
///
/// This function orchestrates the `NameResolver`, `TypeChecker`, and block analysis
/// services. Pre-defining known variables is essential for REPL contexts where
/// users build up state incrementally across multiple evaluations.
pub fn analyse_with_known_vars(module: &Module, known_vars: &[&str]) -> AnalysisResult {
    analyse_full(module, known_vars, false, false, vec![], None, None)
}

/// Analyse a module with compiler options controlling stdlib-specific behaviour.
///
/// When `stdlib_mode` is true, built-in classes are permitted to subclass sealed
/// classes (BT-791). This should only be set when compiling stdlib sources.
pub fn analyse_with_options(module: &Module, options: &crate::CompilerOptions) -> AnalysisResult {
    analyse_full(
        module,
        &[],
        options.stdlib_mode,
        options.skip_module_expression_lint,
        vec![],
        None,
        options.current_package.as_deref(),
    )
}

/// Analyse a module with compiler options and pre-loaded class entries.
///
/// BT-1523: Used by the build pipeline to inject cross-file class metadata
/// from Pass 1 into Pass 2's semantic analysis, enabling proper method
/// resolution across files.
pub fn analyse_with_options_and_classes(
    module: &Module,
    options: &crate::CompilerOptions,
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
) -> AnalysisResult {
    analyse_full(
        module,
        &[],
        options.stdlib_mode,
        options.skip_module_expression_lint,
        pre_loaded_classes,
        None,
        options.current_package.as_deref(),
    )
}

/// Analyse a module with pre-defined variables and pre-loaded class entries
/// from BEAM metadata (ADR 0050 Phase 4).
///
/// `pre_loaded_classes` are injected into the `ClassHierarchy` *before*
/// `TypeChecking`, so user-defined REPL classes are visible to the `TypeChecker`.
pub fn analyse_with_known_vars_and_classes(
    module: &Module,
    known_vars: &[&str],
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
) -> AnalysisResult {
    analyse_full(
        module,
        known_vars,
        false,
        false,
        pre_loaded_classes,
        None,
        None,
    )
}

/// Analyse a module with known package dependencies (ADR 0070 Phase 2).
///
/// Package qualifiers in class references and extension targets are validated
/// against the provided set of known package names. Unknown package qualifiers
/// produce compile errors.
#[allow(clippy::implicit_hasher)] // concrete HashSet is simpler for callers
pub fn analyse_with_packages(
    module: &Module,
    options: &crate::CompilerOptions,
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
    known_packages: std::collections::HashSet<String>,
) -> AnalysisResult {
    analyse_full(
        module,
        &[],
        options.stdlib_mode,
        options.skip_module_expression_lint,
        pre_loaded_classes,
        Some(known_packages),
        options.current_package.as_deref(),
    )
}

/// Internal: full analysis with all knobs.
#[allow(clippy::too_many_lines)] // orchestration function — one call per analysis phase
fn analyse_full(
    module: &Module,
    known_vars: &[&str],
    stdlib_mode: bool,
    skip_module_expression_lint: bool,
    pre_loaded_classes: Vec<class_hierarchy::ClassInfo>,
    known_packages: Option<std::collections::HashSet<String>>,
    current_package: Option<&str>,
) -> AnalysisResult {
    let mut result = AnalysisResult::new();

    // Phase 0: Build Class Hierarchy (ADR 0006 Phase 1a)
    let (hierarchy_result, hierarchy_diags) =
        ClassHierarchy::build_with_options(module, stdlib_mode);
    // build_with_options is infallible; propagate any diagnostics it produced
    result.class_hierarchy = hierarchy_result.expect("ClassHierarchy::build is infallible");
    result.diagnostics.extend(hierarchy_diags);

    // ADR 0071 BT-1700: Stamp current package on AST-derived classes
    if let Some(pkg) = current_package {
        result.class_hierarchy.stamp_package(pkg);
    }

    // BT-1726: Remember whether cross-file class metadata was provided so the
    // structural validator can decide whether unresolved-class warnings are useful.
    let has_cross_file_classes = !pre_loaded_classes.is_empty();

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

    // Phase 0.5: Protocol Registration (ADR 0068 Phase 2b)
    // Register protocol definitions from the module into the protocol registry.
    // Must happen after the class hierarchy is fully built (for namespace collision
    // checks) and before type checking (so protocol names resolve in type annotations).
    if !module.protocols.is_empty() {
        let proto_diags = result
            .protocol_registry
            .register_module(module, &result.class_hierarchy);
        result.diagnostics.extend(proto_diags);
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
    type_checker.check_module_with_protocols(
        module,
        &result.class_hierarchy,
        &result.protocol_registry,
    );
    result.diagnostics.extend(type_checker.take_diagnostics());
    let type_map = type_checker.take_type_map();

    // Phase 3: Block Context Analysis (captures, mutations, context determination)
    // Receive scope from NameResolver and TypeMap from TypeChecker
    let mut analyser = Analyser::with_scope(scope, type_map);

    analyser.analyse_module(module);
    result.diagnostics.extend(analyser.result.diagnostics);
    result.block_info = analyser.result.block_info;

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
    // BT-919: Reject cast (!) on value types
    validators::check_cast_on_value_type(module, &result.class_hierarchy, &mut result.diagnostics);
    // BT-950: Warn on redundant assignment (x := x)
    validators::check_redundant_assignment(module, &mut result.diagnostics);
    // BT-955: Warn on literal boolean conditions (always true / always false)
    validators::check_literal_boolean_condition(module, &mut result.diagnostics);
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
    validators::check_native_state_fields(module, &mut result.diagnostics);
    validators::check_native_delegate_return_type(
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
            known_vars,
            &mut result.diagnostics,
        );
    }
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
    // E0402: leaked visibility — internal class in public signature (BT-1701)
    validators::check_class_visibility(
        module,
        &result.class_hierarchy,
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
