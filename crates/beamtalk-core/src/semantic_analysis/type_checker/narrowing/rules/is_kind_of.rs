// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x isKindOf: ClassName` narrowing (BT-1573 Phase 1g).

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::type_checker::InferredType;

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = receiver
    else {
        return None;
    };
    if !(parts.len() == 1 && parts[0].keyword == "isKindOf:") {
        return None;
    }
    let var_name = extract_variable_name(inner_recv)?;
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
