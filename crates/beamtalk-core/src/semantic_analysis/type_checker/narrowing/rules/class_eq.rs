// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x class = ClassName` / `x class =:= ClassName` narrowing (BT-1573 Phase 1g).
//!
//! Also handles `(x class) = ClassName` via a parenthesized-unwrap.

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::type_checker::InferredType;

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = receiver
    else {
        return None;
    };
    if !(op.as_str() == "=" || op.as_str() == "=:=") {
        return None;
    }
    // The inner receiver should be `x class` or `(x class)`
    let class_send = match inner_recv.as_ref() {
        Expression::Parenthesized { expression, .. } => expression.as_ref(),
        other => other,
    };
    let Expression::MessageSend {
        receiver: var_expr,
        selector: MessageSelector::Unary(sel),
        ..
    } = class_send
    else {
        return None;
    };
    if sel.as_str() != "class" {
        return None;
    }
    let var_name = extract_variable_name(var_expr)?;
    let Some(Expression::ClassReference { name, .. }) = arguments.first() else {
        return None;
    };
    Some(NarrowingInfo {
        variable: var_name,
        true_type: InferredType::known(name.name.clone()),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
    })
}
