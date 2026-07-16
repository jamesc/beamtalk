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
//! - [`hover_provider`] - Show information on hover
//! - [`implementors_provider`] - Find every class that defines a given selector (BT-2241)
//! - [`references_provider`] - Find all references to a symbol across files
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
pub mod hover_provider;
pub mod implementors_provider;
pub mod references_provider;
pub mod references_to_query;
pub mod senders_query;
pub mod signature_help_provider;

use crate::ast::{MessageSelector, Module};
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{AliasRegistry, ClassHierarchy, infer_types_and_returns};
use crate::source_analysis::Span;

/// Returns the source span covering a keyword selector's keyword tokens, if any.
///
/// For a keyword selector with at least one part, merges the span of the first and last
/// keyword tokens. Returns `None` for unary and binary selectors.
///
/// Shared by [`senders_query`], [`all_sends_query`], [`announce_sites_query`], and
/// [`ffi_sites_query`] to avoid duplicating the same nine-line function in each module.
pub(crate) fn selector_span(selector: &MessageSelector) -> Option<Span> {
    match selector {
        MessageSelector::Keyword(parts) if !parts.is_empty() => {
            let first = parts.first().unwrap().span;
            let last = parts.last().unwrap().span;
            Some(first.merge(last))
        }
        _ => None,
    }
}

/// Enriches a class hierarchy with method return types inferred from a module's source,
/// and returns the [`TypeMap`] from the same single [`TypeChecker`] pass (BT-1047).
///
/// Returns `(Some(enriched_copy), type_map)` when inference produces any results,
/// cloning the hierarchy and applying the inferred types. Returns `(None, type_map)`
/// when there is nothing to infer, avoiding the allocation of an unnecessary clone.
///
/// The [`TypeMap`] is returned from the same pass so callers do not need to run
/// a second `infer_types` call.
///
/// Used by [`completion_provider`] and [`hover_provider`] so both share identical
/// enrichment logic (BT-1014).
///
/// BT-2867: `native_type_registry`, when `Some`, lets expressions *downstream*
/// of a typed FFI call (e.g. `x := (Erlang m) f:. x bar`) see `x`'s real type
/// in the returned [`TypeMap`] too — not just the FFI call site itself, which
/// callers already special-case separately via their own `native_types`
/// parameter.
///
/// `alias_registry` is `None` here (ADR 0108, BT-2897) — [`ProjectIndex`]
/// does not yet track a project-wide alias table (that's LSP integration,
/// deferred to the later ADR 0108 phase BT-2901), so completions see aliases
/// as ordinary unresolved names for now. [`hover_provider::compute_hover`]
/// takes its own `alias_registry` parameter and calls
/// [`enrich_hierarchy_with_inferred_returns_and_aliases`] directly instead of
/// this function when a caller has one to offer.
///
/// [`ProjectIndex`]: crate::language_service::project_index::ProjectIndex
pub(crate) fn enrich_hierarchy_with_inferred_returns(
    module: &Module,
    hierarchy: &ClassHierarchy,
    native_type_registry: Option<&crate::semantic_analysis::type_checker::NativeTypeRegistry>,
) -> (Option<ClassHierarchy>, TypeMap) {
    let (type_map, inferred) = infer_types_and_returns(module, hierarchy, native_type_registry);
    let enriched = if inferred.is_empty() {
        None
    } else {
        let mut h = hierarchy.clone();
        h.apply_inferred_return_types(&inferred);
        Some(h)
    };
    (enriched, type_map)
}

/// [`enrich_hierarchy_with_inferred_returns`], additionally threading a type
/// alias registry (ADR 0108, BT-2897) through to
/// [`infer_types_and_returns_with_aliases`] so the underlying [`TypeMap`]
/// resolves and tags alias references (see [`TypeProvenance::Aliased`]).
/// `alias_registry = None` is identical to
/// [`enrich_hierarchy_with_inferred_returns`].
///
/// [`TypeProvenance::Aliased`]: crate::semantic_analysis::type_checker::TypeProvenance
pub(crate) fn enrich_hierarchy_with_inferred_returns_and_aliases(
    module: &Module,
    hierarchy: &ClassHierarchy,
    native_type_registry: Option<&crate::semantic_analysis::type_checker::NativeTypeRegistry>,
    alias_registry: Option<&AliasRegistry>,
) -> (Option<ClassHierarchy>, TypeMap) {
    let (type_map, inferred) =
        crate::semantic_analysis::type_checker::infer_types_and_returns_with_aliases(
            module,
            hierarchy,
            native_type_registry,
            alias_registry,
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
