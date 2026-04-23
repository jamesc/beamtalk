// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type checker tests, split by feature per BT-2061.
//!
//! Shared fixtures live in `common`. Each feature module is kept under ~2000
//! lines so parallel PRs adding tests rarely conflict on the same region.

mod common;

mod arg_return_checking;
mod basic_inference;
mod destructure_and_extension;
mod dynamic_and_blocks;
mod dynamic_iterable;
mod ffi;
mod generic_inheritance;
mod generic_preservation;
mod generic_validation;
mod generics_substitution;
mod local_annotations;
mod method_local_params;
mod narrowing_control_flow_legacy;
mod narrowing_if_nil_if_not_nil;
mod narrowing_if_true_if_false;
mod narrowing_post_guard;
mod never_divergence;
mod never_error;
mod on_do_and_if_not_nil;
mod protocol;
mod result_and_type_args;
mod self_type;
mod state_fields;
mod super_and_binary;
mod typed_class;
mod union_types;
mod unions_checking;
