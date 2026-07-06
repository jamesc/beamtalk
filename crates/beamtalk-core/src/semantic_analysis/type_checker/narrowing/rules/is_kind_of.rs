// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x isKindOf: ClassName` narrowing (BT-1573 Phase 1g).

use crate::ast::{Expression, WellKnownSelector};
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType};

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector,
        arguments,
        ..
    } = receiver
    else {
        return None;
    };
    if selector.well_known() != Some(WellKnownSelector::IsKindOf) {
        return None;
    }
    let var_name = extract_variable_name(inner_recv)?;
    let Some(Expression::ClassReference { name, .. }) = arguments.first() else {
        return None;
    };
    Some(NarrowingInfo {
        variable: var_name,
        // Provisional — `refine_class_narrowing` overwrites `true_type` with
        // `intersect(current, name)` once the variable's current type is
        // known (ADR 0102 §2 group 2).
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: None,
        class_test: Some(name.name.clone()),
    })
}
