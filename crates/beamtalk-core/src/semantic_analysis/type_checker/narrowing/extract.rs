// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utilities for extracting narrowable variable names from expressions.
//!
//! Narrowing rules need to lift a stable key out of the expression under the
//! type test so that the refinement can be stored in `TypeEnv` keyed by that
//! key. BT-2062 replaced the old `"self.field"` string convention with a
//! typed [`EnvKey`] — the shape of the refinement is now visible in the type
//! system rather than buried in a prefix.
//!
//! Extracted from `inference.rs` under BT-2050; re-typed under BT-2062.
//!
//! BT-2063 note: these helpers deliberately do **not** use
//! [`crate::ast::visitor`]. They are shape destructors — "peel parens, then
//! match one specific shape and return" — not structural walkers; feeding
//! them through a recursive visitor would obscure their intent and change
//! their soundness story (both callers want to reject anything that isn't
//! exactly `Identifier` or `self.<field>`, not "contains an identifier
//! somewhere").

use crate::ast::Expression;
use crate::semantic_analysis::type_checker::EnvKey;

/// Peel `Expression::Parenthesized` wrappers so callers can pattern-match on
/// the inner expression directly.
pub(crate) fn unwrap_parens(expr: &Expression) -> &Expression {
    match expr {
        Expression::Parenthesized { expression, .. } => unwrap_parens(expression),
        other => other,
    }
}

/// Extract a [`EnvKey`] naming the variable under a type test.
///
/// Supports identifiers, parenthesized identifiers, and `self.<field>`
/// access (BT-2048). Returns `None` for any other shape — narrowing only
/// applies to bindings the env can key.
pub(crate) fn extract_variable_name(expr: &Expression) -> Option<EnvKey> {
    match expr {
        Expression::Identifier(ident) => Some(EnvKey::local(ident.name.clone())),
        Expression::Parenthesized { expression, .. } => extract_variable_name(expression),
        // BT-2048: `self.<field>` — the binding lives in the narrowing
        // overlay, not on the class hierarchy.
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            if let Expression::Identifier(recv_id) = receiver.as_ref() {
                if recv_id.name == "self" {
                    return Some(EnvKey::self_field(field.name.clone()));
                }
            }
            None
        }
        _ => None,
    }
}
