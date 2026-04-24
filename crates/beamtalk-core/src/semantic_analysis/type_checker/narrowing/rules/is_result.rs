// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Result-shape narrowing rules: `isOk`, `ok`, `isError` (BT-1859).
//!
//! All three set a placeholder `Dynamic(Unknown)` `true_type`; the real types
//! are filled in by `refine_result_narrowing` in `inference.rs` once the
//! variable's current type is resolved from the environment.

use crate::ast::{Expression, MessageSelector, WellKnownSelector};
use crate::semantic_analysis::type_checker::{DynamicReason, InferredType};

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

/// Detects `x isOk` and `x ok` — Result ok-check narrowing.
pub(super) const IS_OK_OR_OK_RULE: NarrowingRule = NarrowingRule {
    detect: detect_is_ok_or_ok,
};

/// Detects `x isError` — Result error-check narrowing.
pub(super) const IS_ERROR_RULE: NarrowingRule = NarrowingRule {
    detect: detect_is_error,
};

fn detect_is_ok_or_ok(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector,
        ..
    } = receiver
    else {
        return None;
    };
    // `isOk` is a well-known selector; `ok` is a bare unary selector that is
    // not (and deliberately is not) part of `WellKnownSelector` because it is
    // also a legitimate user-defined selector on many receivers. We only
    // narrow when shape is unambiguous (unary send with receiver being a
    // variable-shaped expression) — `refine_result_narrowing` later checks
    // the receiver's declared type and drops the narrowing if it isn't a
    // Result.
    let is_ok = match selector.well_known() {
        Some(WellKnownSelector::IsOk) => true,
        None => matches!(selector, MessageSelector::Unary(sel) if sel.as_str() == "ok"),
        _ => false,
    };
    if !is_ok {
        return None;
    }
    build_info(inner_recv, /* is_error */ false)
}

fn detect_is_error(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector,
        ..
    } = receiver
    else {
        return None;
    };
    if selector.well_known() != Some(WellKnownSelector::IsError) {
        return None;
    }
    build_info(inner_recv, /* is_error */ true)
}

fn build_info(inner_recv: &Expression, is_error: bool) -> Option<NarrowingInfo> {
    let var_name = extract_variable_name(inner_recv)?;
    Some(NarrowingInfo {
        variable: var_name,
        // Placeholder — refined by `refine_result_narrowing` once we know
        // the variable's actual type from the env.
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: !is_error,
        is_result_error_check: is_error,
        responded_selector: None,
    })
}
