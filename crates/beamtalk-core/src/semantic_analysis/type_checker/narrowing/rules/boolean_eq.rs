// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x =:= false` / `x =:= true` (and `==`, either operand order) narrowing
//! (BT-4638).
//!
//! `true` and `false` are the only instances of the sealed classes `True` and
//! `False`, so an equality test against one of them is exactly an
//! `isKindOf: True` / `isKindOf: False` test. The rule therefore records a
//! [`ClassTestKind::KindOf`] test and lets `compute_class_narrowing` resolve
//! both branches: the matching branch intersects with the boolean class, the
//! other branch removes it (`Integer | False` minus `False` ⇒ `Integer`).
//! Inequality (`/=`, `=/=`) is not recognised.

use ecow::EcoString;

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType};

use super::super::extract::extract_variable_name;
use super::super::info::{ClassTestInfo, ClassTestKind, NarrowingInfo};
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
    // Only the positive spellings: the matching branch is the boolean class.
    if !matches!(op.as_str(), "=:=" | "==") {
        return None;
    }
    let rhs = arguments.first()?;
    let (var_expr, class_name) = match (boolean_class(lhs), boolean_class(rhs)) {
        (None, Some(name)) => (lhs.as_ref(), name),
        (Some(name), None) => (rhs, name),
        _ => return None,
    };
    let variable = extract_variable_name(var_expr)?;
    Some(NarrowingInfo {
        variable,
        // Provisional — overwritten by `compute_class_narrowing`.
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: None,
        class_test: Some(ClassTestInfo {
            class_name: EcoString::from(class_name),
            kind: ClassTestKind::KindOf,
        }),
    })
}

/// `"True"` / `"False"` for a `true` / `false` literal (unwrapping parens).
fn boolean_class(expr: &Expression) -> Option<&'static str> {
    Some(if expr.as_literal_bool()? {
        "True"
    } else {
        "False"
    })
}
