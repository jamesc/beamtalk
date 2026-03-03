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

pub mod completion_provider;
pub mod definition_provider;
pub mod diagnostic_provider;
pub mod document_symbols_provider;
mod erlang_modules;
pub mod hover_provider;
pub mod references_provider;
pub mod signature_help_provider;

use crate::ast::Module;
use crate::semantic_analysis::type_checker::TypeMap;
use crate::semantic_analysis::{ClassHierarchy, infer_types_and_returns};

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
pub(crate) fn enrich_hierarchy_with_inferred_returns(
    module: &Module,
    hierarchy: &ClassHierarchy,
) -> (Option<ClassHierarchy>, TypeMap) {
    let (type_map, inferred) = infer_types_and_returns(module, hierarchy);
    let enriched = if inferred.is_empty() {
        None
    } else {
        let mut h = hierarchy.clone();
        h.apply_inferred_return_types(&inferred);
        Some(h)
    };
    (enriched, type_map)
}
