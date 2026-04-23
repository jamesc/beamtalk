// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x isNil` narrowing rule (BT-1573 Phase 1g).
//!
//! True branch narrows `x` to `UndefinedObject` (the nil type). False branch
//! narrows `x` to non-nil via the `is_nil_check` flag — the post-guard
//! narrowing in BT-2049 also keys off this flag.
//!
//! Supports `x isNil` and `self.field isNil` (BT-2048 synthetic key path via
//! [`extract_variable_name`]).

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::type_checker::InferredType;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector: MessageSelector::Unary(sel),
        ..
    } = receiver
    else {
        return None;
    };
    if sel.as_str() != "isNil" {
        return None;
    }
    let var_name = extract_variable_name(inner_recv)?;
    Some(NarrowingInfo {
        variable: var_name,
        true_type: InferredType::known(WellKnownClass::UndefinedObject.as_str()),
        false_type: None,
        is_nil_check: true,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
    })
}
