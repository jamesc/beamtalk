// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Query modules for language service operations.
//!
//! This module provides domain services (Providers) from the Language Service
//! context in the DDD model. Each provider implements a specific language
//! service capability:
//!
//! - [`completion_provider`] - Suggest completions at cursor position
//! - [`definition_provider`] - Go-to-definition (single-file and cross-file)
//! - [`diagnostic_provider`] - Collect errors and warnings
//! - [`document_symbols_provider`] - Return document outline symbols
//! - [`folding_range_provider`] - Return foldable ranges for section dividers
//! - [`hover_provider`] - Show information on hover
//! - [`implementors_provider`] - Find every class that defines a given selector
//! - [`references_provider`] - Find all references to a symbol across files
//! - [`return_value_fix_provider`] - The ADR 0133 "return the value instead" quick-fix
//! - [`signature_help_provider`] - Show parameter info for keyword messages
//!
//! **DDD Context:** Language Service
//!
//! These providers follow the DDD ubiquitous language where "Provider" is
//! the standard term for domain services that compute language service
//! responses. This aligns with LSP terminology (`CompletionProvider`,
//! `DiagnosticProvider`, `HoverProvider`) and makes the code self-documenting.
//!
//! ## References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol

pub mod all_sends_query;
pub mod announce_sites_query;
pub mod completion_provider;
pub mod definition_provider;
pub mod diagnostic_provider;
pub mod document_symbols_provider;
mod erlang_modules;
pub mod ffi_sites_query;
pub mod field_accesses_query;
pub mod folding_range_provider;
pub mod hover_provider;
pub mod implementors_provider;
pub mod references_provider;
pub mod references_to_query;
pub mod return_value_fix_provider;
pub mod selector_rename_query;
pub mod senders_query;
pub mod signature_help_provider;

use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::type_checker::TypeMap;
use beamtalk_core::semantic_analysis::{
    AliasRegistry, ClassHierarchy, InferredType, ProtocolRegistry,
};
use std::sync::Arc;

/// Re-exported from [`beamtalk_core::method_source_walker`] so sibling query modules
/// (`senders_query`, `all_sends_query`, `announce_sites_query`, `ffi_sites_query`)
/// can reach it via `super::selector_span` without importing across layers.
pub(crate) use beamtalk_core::method_source_walker::selector_span;

/// The type `type_map` gives `expr`, a node of the **parsed** module.
///
/// The providers walk the parsed tree (positions must land on the
/// parentheses as written), but the type checker builds its [`TypeMap`] from
/// the operand-normalised tree (BT-4237, `TypeChecker::check_module`), so a
/// removed send-operand wrapper has no entry of its own. A wrapper's type is
/// its inner expression's, so the lookup falls through every
/// `Parenthesized` layer to that node's span, which both trees share.
pub(crate) fn type_of_parsed<'t>(
    type_map: &'t TypeMap,
    expr: &Expression,
) -> Option<&'t InferredType> {
    type_map
        .get(expr.span())
        .or_else(|| type_map.get(expr.unwrap_parens().span()))
}

/// The protocol registry a hover/completion query types `module` against.
///
/// `project` is the caller's registry for the file — cross-file protocols
/// plus `module`'s own, built with
/// [`ProtocolRegistry::seed_and_register_module`] — and is passed through as
/// is. Without one (a caller with no project index), only `module`'s own
/// protocols are registered.
///
/// The registry is an `Arc` so the one built per request is shared with the
/// type pass's `TypeChecker` rather than deep-cloned into it (BT-4450).
pub(crate) fn protocols_for_module(
    project: Option<Arc<ProtocolRegistry>>,
    module: &Module,
    hierarchy: &ClassHierarchy,
) -> Arc<ProtocolRegistry> {
    if let Some(registry) = project {
        return registry;
    }
    let mut registry = ProtocolRegistry::new();
    // Collision/duplicate diagnostics belong to the diagnostics provider.
    let _ = registry.seed_and_register_module(module, Vec::new(), hierarchy, None);
    Arc::new(registry)
}

/// Enriches a class hierarchy with method return types inferred from a module's source,
/// and returns the [`TypeMap`] from the same single [`TypeChecker`] pass.
///
/// Returns `(Some(enriched_copy), type_map)` when inference produces any results,
/// cloning the hierarchy and applying the inferred types. Returns `(None, type_map)`
/// when there is nothing to infer, avoiding the allocation of an unnecessary clone.
///
/// The [`TypeMap`] is returned from the same pass so callers do not need to run
/// a second `infer_types` call.
///
/// Used by [`completion_provider`] and [`hover_provider`] so both share identical
/// enrichment logic.
///
/// `native_type_registry`, when `Some`, lets expressions *downstream*
/// of a typed FFI call (e.g. `x := (Erlang m) f:. x bar`) see `x`'s real type
/// in the returned [`TypeMap`] too — not just the FFI call site itself, which
/// callers already special-case separately via their own `native_types`
/// parameter.
///
/// `protocol_registry` is the file's protocol registry (see
/// [`protocols_for_module`]), so a `P class` annotation resolves to the
/// protocol metatype (ADR 0144) as it does in the compiler.
/// `alias_registry` resolves type alias references (ADR 0108, see
/// [`TypeProvenance::Aliased`]); `None` leaves them unresolved.
///
/// [`TypeChecker`]: beamtalk_core::semantic_analysis::type_checker::TypeChecker
/// [`TypeProvenance::Aliased`]: beamtalk_core::semantic_analysis::type_checker::TypeProvenance
pub(crate) fn enrich_hierarchy_with_inferred_returns(
    module: &Module,
    hierarchy: &ClassHierarchy,
    native_type_registry: Option<
        &Arc<beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry>,
    >,
    protocol_registry: &Arc<ProtocolRegistry>,
    alias_registry: Option<&Arc<AliasRegistry>>,
) -> (Option<ClassHierarchy>, TypeMap) {
    let (type_map, inferred) =
        beamtalk_core::semantic_analysis::type_checker::infer_types_and_returns_with_registries(
            module,
            hierarchy,
            native_type_registry.map(Arc::clone),
            Some(Arc::clone(protocol_registry)),
            alias_registry.map(Arc::clone),
        );
    let enriched = if inferred.is_empty() {
        None
    } else {
        let mut h = hierarchy.clone();
        h.apply_inferred_return_types(&inferred);
        Some(h)
    };
    (enriched, type_map)
}

#[cfg(test)]
pub(crate) use beamtalk_core::test_helpers::test_support::parse_source;

#[cfg(test)]
mod tests {
    use super::*;

    /// BT-4450: the caller's registry is passed through, not copied — one
    /// registry build per request, shared with the type pass.
    #[test]
    fn protocols_for_module_passes_the_project_registry_through() {
        let module = parse_source("x := 1");
        let hierarchy = ClassHierarchy::with_builtins();
        let project = Arc::new(ProtocolRegistry::new());
        let registry = protocols_for_module(Some(Arc::clone(&project)), &module, &hierarchy);
        assert!(Arc::ptr_eq(&registry, &project));
    }
}
