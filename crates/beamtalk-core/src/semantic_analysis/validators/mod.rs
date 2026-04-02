// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Validation checks for semantic analysis.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains validation checks that run after the main analysis
//! pipeline. Validators are organized by concern:
//! - `class_validators` — class-hierarchy-dependent checks
//! - `lint_validators` — code quality lints
//! - `supervision_validators` — OTP supervision policy checks
//! - `match_validators` — pattern match exhaustiveness
//! - `native_validators` — native actor validation

mod class_validators;
mod lint_validators;
mod match_validators;
mod native_validators;
pub(crate) mod package_validators;
mod structural_validators;
mod supervision_validators;
mod visibility_validators;

// Re-export all validators so callers don't need to know the submodule structure.
pub use class_validators::check_stdlib_name_shadowing;
pub(crate) use class_validators::{
    check_abstract_instantiation, check_actor_field_mutation_in_closure, check_actor_new_usage,
    check_cast_on_value_type, check_class_variable_access, check_data_keyword_class_kind,
    check_new_field_names, check_object_new_usage, check_value_nil_return,
    check_value_slot_assignment,
};
pub(crate) use lint_validators::{
    check_effect_free_statements, check_empty_method_bodies, check_literal_boolean_condition,
    check_redundant_assignment,
};
pub(crate) use match_validators::{check_match_exhaustiveness, warn_assignment_in_match_arms};
pub(crate) use native_validators::{check_native_delegate_return_type, check_native_state_fields};
pub(crate) use package_validators::check_package_qualifiers;
pub(crate) use structural_validators::{
    check_ffi_arity, check_unresolved_classes, check_unresolved_ffi_modules,
    check_workspace_shadows,
};
pub(crate) use supervision_validators::{
    check_children_supervision_policy, check_supervision_policy_override,
};
pub(crate) use visibility_validators::{
    check_class_visibility, check_internal_method_shadow, check_leaked_method_visibility,
};
