// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `x isNil` / `x notNil` narrowing rule.
//!
//! For `isNil` the true branch narrows `x` to `UndefinedObject` (the nil
//! type) and the false branch narrows `x` to non-nil via the `is_nil_check`
//! flag — the post-guard narrowing also keys off this flag. `notNil` (BT-4710)
//! is the negated reading, recorded in [`NarrowingInfo::nil_test_negated`]
//! exactly like `x =/= nil` (BT-4712) so `refine_nil_negation` swaps the
//! branches once the live type is known.
//!
//! Supports `x isNil`, `x notNil` and `self.field isNil` / `notNil`
//! (synthetic key path via [`extract_variable_name`]).

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
    let negated = match selector.well_known() {
        Some(WellKnownSelector::IsNil) => false,
        Some(WellKnownSelector::NotNil) => true,
        _ => return None,
    };
    let var_name = extract_variable_name(inner_recv)?;
    let mut info = nil_check_info(var_name);
    info.nil_test_negated = negated;
    Some(info)
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
