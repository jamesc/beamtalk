// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Expansion of a dependency's own `internal type` aliases in the signatures
//! it exports (BT-4615, BT-4707).
//!
//! **DDD Context:** Build System
//!
//! A dependency's class and protocol signatures reach a consumer's checker from
//! the dependency's *source*. Its `internal type` aliases do not exist in the
//! consumer's alias scope, so they are expanded before the signatures leave the
//! dependency. Both CLI dependency scanners and the MCP lint scanner call these
//! helpers, and the LSP applies the same expansion to what it indexes. This
//! module lives in `beamtalk-project`, below `beamtalk-cli` and the LSP, so
//! the rule is written once.

use beamtalk_core::ast::ProtocolDefinition;
use beamtalk_core::semantic_analysis::alias_expansion::InternalAliasExpander;
use beamtalk_core::semantic_analysis::alias_registry::AliasInfo;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
use beamtalk_core::semantic_analysis::protocol_registry::ProtocolInfo;

/// Expands a dependency's own `internal type` aliases in its class
/// signatures (BT-4615). Must run **after** trait flattening: flattening
/// re-extracts each trait-using class from its AST and replaces the whole
/// `ClassInfo`, which would discard an earlier expansion.
pub fn expand_dependency_class_aliases(
    alias_infos: &[AliasInfo],
    class_infos: &mut [ClassInfo],
    protocol_infos: &mut [ProtocolInfo],
) {
    let expander = InternalAliasExpander::from_alias_infos(alias_infos);
    for info in class_infos {
        expander.expand_class_info(info);
    }
    for info in protocol_infos {
        expander.expand_protocol_info(info);
    }
}

/// Expands a dependency's own `internal type` aliases in the protocol ASTs it
/// exports (BT-4707) — the ASTs a consumer's (or a later dependency's)
/// `uses: dep@Proto` flattens provisions from in *its* alias scope, where the
/// dependency's internal aliases do not exist. Must run after the dependency's
/// own trait flattening (which reads the unexpanded ASTs in the dependency's
/// own scope) and **before** any later dependency flattens them.
pub fn expand_dependency_protocol_defs(
    alias_infos: &[AliasInfo],
    protocol_defs: &mut [ProtocolDefinition],
) {
    let expander = InternalAliasExpander::from_alias_infos(alias_infos);
    for protocol in protocol_defs {
        expander.expand_protocol_definition(protocol);
    }
}
