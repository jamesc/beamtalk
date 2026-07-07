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

use ecow::EcoString;

use crate::ast::{Expression, Literal, MessageSelector};
use crate::semantic_analysis::type_checker::{DynamicReason, EnvKey, InferredType};

use super::super::extract::{extract_variable_name, unwrap_parens};
use super::super::info::{NarrowingInfo, SingletonEqInfo, SingletonName};
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
    let eq = detect_binary(lhs.as_ref(), op, arguments)?;
    Some(NarrowingInfo {
        variable: eq.variable,
        // Provisional — `refine_singleton_narrowing` overwrites both branch
        // types once the variable's current union type is known.
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: Some(eq.info),
        class_test: None,
    })
}

/// A singleton (in)equality test recovered from a binary send: the tested
/// variable plus the recorded singleton/negation (BT-2617, BT-2631).
pub(crate) struct SingletonEqDetection {
    pub(crate) variable: EnvKey,
    pub(crate) info: SingletonEqInfo,
}

/// Recognises a singleton (in)equality test from the deconstructed binary send
/// `lhs <op> rhs` (where `rhs` is `arguments.first()`). Reused by both the
/// narrowing-guard path (via [`detect`]) and the standalone-send path
/// (`infer_union_message_send`, BT-2631) so the operand-matching rule — accept
/// `x = #foo` or `#foo = x`, reject `#a = #b` and non-singleton tests — lives in
/// one place.
pub(crate) fn detect_binary(
    lhs: &Expression,
    op: &EcoString,
    arguments: &[Expression],
) -> Option<SingletonEqDetection> {
    let negated = match op.as_str() {
        "=" | "=:=" => false,
        "/=" | "=/=" => true,
        _ => return None,
    };
    let rhs_expr = arguments.first()?;
    // Accept either `x = #foo` or `#foo = x`; reject `#a = #b` (two literals)
    // and any test where neither side is a singleton literal.
    let (var_expr, symbol_name) = match (symbol_literal(lhs), symbol_literal(rhs_expr)) {
        (None, Some(name)) => (lhs, name),
        (Some(name), None) => (rhs_expr, name),
        _ => return None,
    };
    let variable = extract_variable_name(var_expr)?;
    Some(SingletonEqDetection {
        variable,
        info: SingletonEqInfo {
            singleton: SingletonName::from_symbol_identifier(symbol_name),
            negated,
        },
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
