// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Single integration-test binary for `beamtalk-repl` (BT-4909).
//!
//! Each former top-level `tests/*.rs` file is a submodule here, so the crate links
//! one test binary instead of eleven. Test bodies are unchanged; names gain the
//! module path prefix.

mod common;

mod codegen_property_tests;
mod ffi_block_agreement;
mod repl_codegen_smoke;
mod repl_local_producer;
mod repl_loop_body_conditional_receiver;
mod repl_loop_body_return;
mod repl_nested_assign_pin;
mod repl_nested_conditional_size;
mod repl_repeat_binding;
mod repl_while_condition_binding;
mod value_position_assignment_agreement;
