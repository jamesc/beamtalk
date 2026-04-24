// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x respondsTo: #selector` narrowing (ADR 0068 Phase 2e, BT-1833).
//!
//! Sets `true_type` to `Dynamic` initially; `refine_responds_to_narrowing` in
//! `inference.rs` consults the protocol registry and upgrades to a concrete
//! protocol type when exactly one protocol requires the tested selector.

use crate::ast::{Expression, Literal, WellKnownSelector};
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
    if selector.well_known() != Some(WellKnownSelector::RespondsTo) {
        return None;
    }
    let var_name = extract_variable_name(inner_recv)?;
    // Extract the selector name from a symbol literal argument (#selector)
    let Some(Expression::Literal(Literal::Symbol(sel_name), _)) = arguments.first() else {
        return None;
    };
    Some(NarrowingInfo {
        variable: var_name,
        // Narrow to Dynamic — we know the object responds to the selector,
        // but not its concrete class. Dynamic suppresses DNU warnings.
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: Some(sel_name.clone()),
    })
}
