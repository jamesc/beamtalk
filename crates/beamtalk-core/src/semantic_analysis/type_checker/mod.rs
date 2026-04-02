// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type checking and inference for semantic analysis (ADR 0025 Phase 1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements zero-syntax type inference. It walks the AST,
//! infers types from literals, assignments, and message sends, then
//! validates message sends against `ClassHierarchy` method tables.
//!
//! **Key design decisions:**
//! - Warnings only, never errors (avoid false positives)
//! - `Dynamic` type = no checking (fallback for unknowns)
//! - Classes with `doesNotUnderstand:args:` override suppress warnings
//! - Cascade receiver type unchanged
//!
//! **References:**
//! - `docs/ADR/0025-gradual-typing-and-protocols.md` — Phase 1

use crate::ast::Module;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

mod inference;
pub mod native_type_registry;
mod protocol;
#[cfg(test)]
mod tests;
mod types;
mod validation;

pub use native_type_registry::NativeTypeRegistry;
pub(in crate::semantic_analysis) use types::is_generic_type_param;
pub use types::{InferredType, TypeProvenance};

/// Map of expression spans to their inferred types.
///
/// Used by LSP providers (hover, completions) to query types at cursor positions.
/// Keyed by full span (start + end) to avoid collisions between nested expressions
/// that share the same start offset (e.g., a message send and its receiver).
#[derive(Debug, Clone, Default)]
pub struct TypeMap {
    types: HashMap<Span, InferredType>,
}

impl TypeMap {
    /// Creates an empty type map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    /// Looks up the inferred type for an expression span.
    ///
    /// Returns `None` if no type is recorded at that span.
    #[must_use]
    pub fn get(&self, span: Span) -> Option<&InferredType> {
        self.types.get(&span)
    }

    /// Records an inferred type for an expression span.
    fn insert(&mut self, span: Span, ty: InferredType) {
        self.types.insert(span, ty);
    }
}

/// Runs type inference on a module and returns the type map.
///
/// This is the main entry point for LSP providers that need type information
/// at specific positions (hover, completions).
#[must_use]
pub fn infer_types(module: &Module, hierarchy: &ClassHierarchy) -> TypeMap {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker.take_type_map()
}

/// Key for method return type map: (`ClassName`, Selector, `IsClassMethod`)
///
/// Used by the return-type writeback pass (BT-1005) to track inferred return
/// types for each method before writing them back into the AST.
pub type MethodReturnKey = (EcoString, EcoString, bool);

/// Infers return types for all unannotated methods in a module.
///
/// Only returns `InferredType::Known(class_name)` results. `Dynamic` types are
/// omitted from the map (absence = dynamic, no annotation written back).
///
/// This is used by the return-type writeback pass (BT-1005, ADR 0045 Phase 1b)
/// before codegen so that unannotated user-defined methods appear in the emitted
/// `method_return_types` map, enabling REPL expression completion.
#[must_use]
pub fn infer_method_return_types(
    module: &Module,
    hierarchy: &ClassHierarchy,
) -> HashMap<MethodReturnKey, EcoString> {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker.take_method_return_types()
}

/// Runs type inference on a module and returns both the type map and the inferred
/// method return types in a single [`TypeChecker`] pass.
///
/// This is the combined entry point used by LSP providers that need both
/// [`TypeMap`] (for hover/completion position types) and method return types
/// (for hierarchy enrichment). Using this avoids the double-pass previously
/// required by calling [`infer_method_return_types`] followed by [`infer_types`].
#[must_use]
pub fn infer_types_and_returns(
    module: &Module,
    hierarchy: &ClassHierarchy,
) -> (TypeMap, HashMap<MethodReturnKey, EcoString>) {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    (checker.take_type_map(), checker.take_method_return_types())
}

/// Type checking domain service.
///
/// **DDD Context:** Semantic Analysis - Domain Service
///
/// Performs zero-syntax type inference (ADR 0025 Phase 1):
/// - Infers types from literals and class references
/// - Tracks variable types through assignments
/// - Validates message sends against class method tables
/// - Emits warnings for unknown selectors with hints
/// - Protocol conformance checking for protocol-typed parameters (ADR 0068 Phase 2b)
#[derive(Debug)]
pub struct TypeChecker {
    pub(super) diagnostics: Vec<Diagnostic>,
    pub(super) type_map: TypeMap,
    pub(super) method_return_types: HashMap<MethodReturnKey, EcoString>,
    /// The package being compiled (ADR 0071 Phase 3).
    ///
    /// When set, enables cross-package `internal` method visibility enforcement
    /// (E0403). `None` means no package context (REPL, single-file scripts) —
    /// visibility checks are skipped.
    pub(super) current_package: Option<EcoString>,
    /// Protocol registry for `respondsTo:` narrowing (ADR 0068 Phase 2e).
    ///
    /// When set, `detect_narrowing` can refine `respondsTo:` narrowing from
    /// `Dynamic` to a specific protocol type by looking up which protocol
    /// requires the tested selector. Set by `check_module_with_protocols`
    /// before running the main type checking pass.
    pub(super) protocol_registry: Option<ProtocolRegistry>,
}

impl TypeChecker {
    /// Creates a new type checker.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            type_map: TypeMap::new(),
            method_return_types: HashMap::new(),
            current_package: None,
            protocol_registry: None,
        }
    }

    /// Creates a new type checker with a package context for visibility enforcement.
    ///
    /// When `current_package` is set, the type checker emits E0403 diagnostics
    /// for cross-package sends to `internal` methods (ADR 0071 Phase 3).
    #[must_use]
    pub fn with_package(package: &str) -> Self {
        Self {
            diagnostics: Vec::new(),
            type_map: TypeMap::new(),
            method_return_types: HashMap::new(),
            current_package: Some(EcoString::from(package)),
            protocol_registry: None,
        }
    }

    /// Returns all diagnostics collected during type checking.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Takes ownership of diagnostics, leaving an empty vec.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Returns a reference to the type map built during checking.
    #[must_use]
    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    /// Takes ownership of the type map, leaving an empty map.
    pub fn take_type_map(&mut self) -> TypeMap {
        std::mem::take(&mut self.type_map)
    }

    /// Returns a reference to the method return types collected during checking.
    #[must_use]
    pub fn method_return_types(&self) -> &HashMap<MethodReturnKey, EcoString> {
        &self.method_return_types
    }

    /// Takes ownership of the method return types map, leaving an empty map.
    pub fn take_method_return_types(&mut self) -> HashMap<MethodReturnKey, EcoString> {
        std::mem::take(&mut self.method_return_types)
    }

    /// Checks types in a module using both the class hierarchy and protocol registry.
    ///
    /// This is the protocol-aware entry point used by `analyse_full` when protocols
    /// are present. It delegates to `check_module` for the main type checking pass
    /// and then performs protocol conformance checking on type annotations.
    ///
    /// **References:** ADR 0068 Phase 2b
    pub fn check_module_with_protocols(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Store protocol registry for respondsTo: narrowing (BT-1833).
        // This allows detect_narrowing to refine Dynamic → protocol type.
        self.protocol_registry = Some(protocol_registry.clone());

        // Run the standard type checking pass first
        self.check_module(module, hierarchy);

        // Clear the registry after the main pass (it's borrowed by the caller
        // for the remaining protocol-specific passes below).
        self.protocol_registry = None;

        // Phase 2b: Protocol conformance checking on type annotations.
        // When a parameter type annotation resolves to a protocol name, check
        // that the argument type (if known) conforms structurally.
        self.check_protocol_conformance_in_module(module, hierarchy, protocol_registry);

        // Phase 2d: Type parameter bounds checking (ADR 0068).
        // When a type annotation uses a generic class with bounded type params
        // (e.g., `Logger(Integer)` where Logger has `T :: Printable`), check
        // that the concrete type args conform to their bounds.
        self.check_type_param_bounds_in_module(module, hierarchy, protocol_registry);

        // Phase 2f: Generic variance checking (ADR 0068).
        // When a method parameter expects a generic type (e.g., `Array(Printable)`)
        // and the argument is the same generic class with different type args
        // (e.g., `Array(Integer)`), check covariance for sealed Value classes.
        self.check_generic_variance_in_module(module, hierarchy, protocol_registry);
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Describes where a variable got its type (BT-1588).
///
/// Used to generate "variable has type X because ..." notes in diagnostics.
#[derive(Debug, Clone)]
struct TypeOrigin {
    /// Human-readable description of why the variable has this type.
    /// e.g., "returned from `Dictionary at:ifAbsent:` at this location"
    description: EcoString,
    /// The source span where the type was assigned.
    span: Span,
}

/// Type environment for tracking variable → type mappings.
///
/// Supports nested scopes via `child()` which clones the parent env.
#[derive(Debug, Clone)]
struct TypeEnv {
    bindings: HashMap<EcoString, InferredType>,
    /// Where each variable got its type (BT-1588).
    origins: HashMap<EcoString, TypeOrigin>,
    /// Whether we're inside a class method body (self refers to class-side).
    in_class_method: bool,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            origins: HashMap::new(),
            in_class_method: false,
        }
    }

    fn get(&self, name: &str) -> Option<InferredType> {
        self.bindings.get(name).cloned()
    }

    /// Get the origin description for a variable's type, if tracked.
    fn get_origin(&self, name: &str) -> Option<&TypeOrigin> {
        self.origins.get(name)
    }

    fn set(&mut self, name: &str, ty: InferredType) {
        self.bindings.insert(name.into(), ty);
    }

    /// Set a variable's type with origin tracking (BT-1588).
    fn set_with_origin(
        &mut self,
        name: &str,
        ty: InferredType,
        description: impl Into<EcoString>,
        span: Span,
    ) {
        self.bindings.insert(name.into(), ty);
        self.origins.insert(
            name.into(),
            TypeOrigin {
                description: description.into(),
                span,
            },
        );
    }

    /// Create a child scope that inherits parent bindings.
    fn child(&self) -> Self {
        self.clone()
    }
}
