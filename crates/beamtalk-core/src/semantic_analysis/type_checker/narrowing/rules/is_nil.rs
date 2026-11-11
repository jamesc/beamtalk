// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x isNil` narrowing rule.
//!
//! True branch narrows `x` to `UndefinedObject` (the nil type). False branch
//! narrows `x` to non-nil via the `is_nil_check` flag — the post-guard
//! narrowing also keys off this flag.
//!
//! Supports `x isNil` and `self.field isNil` (synthetic key path via
//! [`extract_variable_name`]).

use crate::ast::{Expression, WellKnownSelector};
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{EnvKey, InferredType};

use super::super::extract::extract_variable_name;
use super::super::info::NarrowingInfo;
use super::NarrowingRule;

pub(super) const RULE: NarrowingRule = NarrowingRule { detect };

fn detect(receiver: &Expression) -> Option<NarrowingInfo> {
    let Expression::MessageSend {
        receiver: inner_recv,
        selector,
        ..
    } = receiver
    else {
        return None;
    };
    if selector.well_known() != Some(WellKnownSelector::IsNil) {
        return None;
    }
    let var_name = extract_variable_name(inner_recv)?;
    Some(nil_check_info(var_name))
}

/// The [`NarrowingInfo`] for "`variable` is nil" — shared with the `x =:= nil`
/// rule ([`super::nil_eq`], BT-4712) so both spellings stay identical.
pub(super) fn nil_check_info(variable: EnvKey) -> NarrowingInfo {
    NarrowingInfo {
        variable,
        true_type: InferredType::known(WellKnownClass::UndefinedObject.as_str()),
        false_type: None,
        is_nil_check: true,
        nil_test_negated: false,
        is_result_ok_check: false,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: None,
        class_test: None,
    }
}
