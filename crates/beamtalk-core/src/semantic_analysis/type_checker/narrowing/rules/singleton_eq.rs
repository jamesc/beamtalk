// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x = #foo` / `#foo = x` singleton (in)equality narrowing (BT-2617).
//!
//! Detects an (in)equality test of a variable against a singleton symbol
//! literal (`#foo`). `detect` only sees the AST, so it records the tested
//! singleton and whether the test was negated; `refine_singleton_narrowing`
//! in `inference.rs` resolves the variable's current union type and sets the
//! branch types — the matching branch narrows to the singleton, the
//! complementary branch to the union with that singleton removed
//! (`Integer | #infinity` minus `#infinity` ⇒ `Integer`).

use ecow::{EcoString, eco_format};

use crate::ast::{Expression, Literal, MessageSelector};
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType};

use super::super::extract::{extract_variable_name, unwrap_parens};
use super::super::info::{NarrowingInfo, SingletonEqInfo};
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: lhs,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = receiver
    else {
        return None;
    };
    let negated = match op.as_str() {
        "=" | "=:=" => false,
        "/=" | "=/=" => true,
        _ => return None,
    };
    let lhs_expr = lhs.as_ref();
    let rhs_expr = arguments.first()?;
    // Accept either `x = #foo` or `#foo = x`; reject `#a = #b` (two literals)
    // and any test where neither side is a singleton literal.
    let (var_expr, symbol_name) = match (symbol_literal(lhs_expr), symbol_literal(rhs_expr)) {
        (None, Some(name)) => (lhs_expr, name),
        (Some(name), None) => (rhs_expr, name),
        _ => return None,
    };
    let variable = extract_variable_name(var_expr)?;
    Some(NarrowingInfo {
        variable,
        // Provisional — `refine_singleton_narrowing` overwrites both branch
        // types once the variable's current union type is known.
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: Some(SingletonEqInfo {
            singleton: eco_format!("#{symbol_name}"),
            negated,
        }),
    })
}

/// Returns the symbol name of a `#foo` literal (without the leading `#`),
/// unwrapping any nesting of parentheses; `None` for any other expression.
fn symbol_literal(expr: &Expression) -> Option<&EcoString> {
    match unwrap_parens(expr) {
        Expression::Literal(Literal::Symbol(name), _) => Some(name),
        _ => None,
    }
}
