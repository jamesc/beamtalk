// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for control-flow Core Erlang code generation.
//!
//! Split by construct (see `docs/development/rust-guidelines.md` §
//! Testing); each submodule stays under ~1,500 lines:
//! - [`while_loops`] — `whileTrue:`/`whileFalse:`/`repeat` codegen
//! - [`stored_closures`] — stored-closure validation and codegen
//! - [`conditionals`] — `ifTrue:`/`ifFalse:` codegen
//! - [`match_patterns`] — `match:` pattern-kind codegen
//! - [`match_arm_state_threading`] — state threading through `match:`
//!   arms and loop/conditional assignment right-hand-sides
//! - [`tier2_block_values`] — Tier 2 stored/invoked block-value-call
//!   codegen
//! - [`list_and_loop_value_threading`] — value-type field-write threading
//!   through loop constructs and conditionals
//! - [`exception_handling`] — `on:do:`/`ensure:` state threading

use super::*;

/// Compiles `src` and returns the field name from the
/// `FieldAssignmentInUnsupportedBlock` it must produce.
///
/// Both halves of every parity pair below assert through this one helper, so a
/// test can only pass by producing the SAME error variant — the whole point of
/// the parity claim (a `ClassVar` and a `ValueType` write of the identical
/// shape get the identical diagnostic), rather than each half asserting its own
/// error in its own way.
///
/// Also asserts the RENDERED message opens with the exact wording the
/// acceptance criteria name ("Cannot assign to field '…' inside this block"),
/// so a future edit that keeps the variant but rewrites the text — or that
/// drops the `field_capitalized` derivation
/// `CodeGenError::field_assignment_in_unsupported_block` centralises — cannot
/// pass silently.
fn field_assignment_rejection_field(src: &str, module_name: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new(module_name).with_workspace_mode(true),
    );
    let err = match result {
        Err(err @ CodeGenError::FieldAssignmentInUnsupportedBlock { .. }) => err,
        other => {
            panic!("Expected FieldAssignmentInUnsupportedBlock for {module_name}. Got: {other:?}")
        }
    };
    // Render before destructuring — `to_string()` is `thiserror`'s `Display`
    // over the whole variant, which is what a user actually sees.
    let rendered = err.to_string();
    let CodeGenError::FieldAssignmentInUnsupportedBlock {
        field,
        field_capitalized,
        ..
    } = err
    else {
        unreachable!("the match above admits no other variant")
    };
    assert!(
        rendered.starts_with(&format!(
            "Cannot assign to field '{field}' inside this block at "
        )),
        "{module_name} must produce BT-3488's agreed diagnostic wording. Got:\n{rendered}"
    );
    assert!(
        rendered.contains(&format!("addTo{field_capitalized}:")),
        "the message's method suggestion must use the capitalized field name. Got:\n{rendered}"
    );
    field
}

mod conditionals;
mod exception_handling;
mod list_and_loop_value_threading;
mod match_arm_state_threading;
mod match_patterns;
mod stored_closures;
mod tier2_block_values;
mod while_loops;
