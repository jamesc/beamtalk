// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5031: `(s)` is exactly `s` in every statement of a value-type conditional arm.
//!
//! The arm builder (`build_vt_conditional_branch_pieces_inner`) and its field-write guard
//! (`reject_unthreadable_value_self_field_write_in_arm`) classify each arm statement with
//! predicates that do not look through parentheses, so a parenthesised arm statement fell
//! to the generic path: a local rebind lost its write and a field write was rejected.

use super::{assert_compiles_through_erlc, codegen_named};

/// The `run:` function of the generated module: the part under test.
fn run_fn<'a>(code: &'a str, name: &str) -> &'a str {
    let start = code.find(name).expect("method is generated");
    let rest = &code[start..];
    rest.find("\n\n'").map_or(rest, |end| &rest[..end])
}

fn vt_method(arm_stmt: &str) -> String {
    format!(
        "Value subclass: ParenArmProbe\n  field: n = 0\n  run: c =>\n    c ifTrue: [\n      x := 1\n      {arm_stmt}\n      x\n    ] ifFalse: [0]\n"
    )
}

fn vt_field_method(arm_stmt: &str) -> String {
    format!(
        "Value subclass: ParenArmProbe\n  field: n = 0\n  run: c =>\n    c ifTrue: [\n      {arm_stmt}\n      self.n\n    ] ifFalse: [0]\n"
    )
}

#[test]
fn parenthesised_arm_local_rebind_matches_bare() {
    let bare = codegen_named(&vt_method("x := x + 1"), "bt@paren_arm_local");
    let paren = codegen_named(&vt_method("(x := x + 1)"), "bt@paren_arm_local");
    assert_eq!(
        run_fn(&bare, "'run:'/2 ="),
        run_fn(&paren, "'run:'/2 ="),
        "`(x := x + 1)` must lower exactly like `x := x + 1`"
    );
    assert_compiles_through_erlc("bt@paren_arm_local", &paren);
}

#[test]
fn parenthesised_arm_field_write_matches_bare() {
    let bare = codegen_named(
        &vt_field_method("self.n := self.n + 1"),
        "bt@paren_arm_field",
    );
    let paren = codegen_named(
        &vt_field_method("(self.n := self.n + 1)"),
        "bt@paren_arm_field",
    );
    assert_eq!(
        run_fn(&bare, "'run:'/2 ="),
        run_fn(&paren, "'run:'/2 ="),
        "`(self.n := ...)` must lower exactly like `self.n := ...`"
    );
    assert_compiles_through_erlc("bt@paren_arm_field", &paren);
}

#[test]
fn parenthesised_plain_block_local_rebind_matches_bare() {
    // A plain (Tier 1) closure body goes through the same `generate_block_body_slice`.
    let src = |stmt: &str| {
        format!(
            "Value subclass: ParenBlockProbe\n  field: n = 0\n  run =>\n    b := [\n      x := 1\n      {stmt}\n      x\n    ]\n    b value\n"
        )
    };
    let bare = codegen_named(&src("x := x + 1"), "bt@paren_block_local");
    let paren = codegen_named(&src("(x := x + 1)"), "bt@paren_block_local");
    assert_eq!(
        run_fn(&bare, "'run'/1 ="),
        run_fn(&paren, "'run'/1 ="),
        "`(x := x + 1)` in a block body must lower exactly like `x := x + 1`"
    );
    assert_compiles_through_erlc("bt@paren_block_local", &paren);
}

#[test]
fn parenthesised_while_condition_local_rebind_matches_bare() {
    // A non-tail statement of a carried `whileTrue:` condition: the lowering and the
    // verifier's write-site list must see the same (unwrapped) node.
    let src = |stmt: &str| {
        format!(
            "Value subclass: ParenWhileProbe\n  field: n = 0\n  run =>\n    i := 0\n    [\n      {stmt}\n      i < 3\n    ] whileTrue: [self.n := self.n + 1]\n    i\n"
        )
    };
    let bare = codegen_named(&src("i := i + 1"), "bt@paren_while_cond");
    let paren = codegen_named(&src("(i := i + 1)"), "bt@paren_while_cond");
    assert_eq!(
        run_fn(&bare, "'run'/1 ="),
        run_fn(&paren, "'run'/1 ="),
        "`(i := i + 1)` in a while condition must lower exactly like `i := i + 1`"
    );
    assert_compiles_through_erlc("bt@paren_while_cond", &paren);
}
