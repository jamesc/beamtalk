// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type checker tests, split by feature per BT-2061.
//!
//! Shared fixtures live in `common`. Each feature module is kept under ~2000
//! lines so parallel PRs adding tests rarely conflict on the same region.

mod common;

mod adr_0104_integration;
mod arg_return_checking;
mod argument_check_matrix;
mod asserted_match_exhaustiveness;
mod basic_inference;
mod bt2826_block_param_types;
mod bt2829_return_type_union_dynamic;
mod bt2835_stream_collect_return_type;
mod bt2843_binary_arg_union_check;
mod bt2847_nested_union_type_args;
mod bt2850_cascade_spawn_with_keys;
mod bt2862_self_delegate_return_type;
mod bt2864_union_block_arg_type_inference;
mod bt2865_dynamic_type_arg_iskindof;
mod bt2866_ifok_iferror_nonlocal_return;
mod bt2871_cascade_binary_arg_check;
mod bt2872_and_not_nil_narrowing;
mod bt2879_cascade_meta_branch;
mod cast_and_sync_send;
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
mod nil_type_match_exhaustiveness;
mod on_do_and_if_not_nil;
mod protocol;
mod result_and_type_args;
mod self_class;
mod self_type;
mod sendability_checking;
mod set_operators;
mod singleton_match_exhaustiveness;
mod spawn_with_keys;
mod state_fields;
mod super_and_binary;
mod type_alias_exhaustiveness;
mod typed_class;
mod union_types;
mod unions_checking;
mod with_timeout_transparency;
