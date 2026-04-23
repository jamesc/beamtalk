// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Utilities for extracting narrowable variable names from expressions.
//!
//! Narrowing rules need to lift a stable key (variable name, or synthetic
//! `self.field` key) out of the expression under the type test, so that the
//! refinement can be stored in `TypeEnv` keyed by that name.
//!
//! Extracted from `inference.rs` under BT-2050.

use ecow::{EcoString, eco_format};

use crate::ast::Expression;

/// Peel `Expression::Parenthesized` wrappers so callers can pattern-match on
/// the inner expression directly.
pub(crate) fn unwrap_parens(expr: &Expression) -> &Expression {
    match expr {
        Expression::Parenthesized { expression, .. } => unwrap_parens(expression),
        other => other,
    }
}

/// Extract a variable name from an expression, supporting identifiers,
/// parenthesized identifiers, and `self.field` access (BT-2048).
///
/// For `self.field` expressions, returns `"self.fieldname"` as a synthetic
/// key so that narrowing can be applied via the type environment.
pub(crate) fn extract_variable_name(expr: &Expression) -> Option<EcoString> {
    match expr {
        Expression::Identifier(ident) => Some(ident.name.clone()),
        Expression::Parenthesized { expression, .. } => extract_variable_name(expression),
        // BT-2048: `self.field` — return synthetic key "self.fieldname"
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            if let Expression::Identifier(recv_id) = receiver.as_ref() {
                if recv_id.name == "self" {
                    return Some(eco_format!("self.{}", field.name));
                }
            }
            None
        }
        _ => None,
    }
}
