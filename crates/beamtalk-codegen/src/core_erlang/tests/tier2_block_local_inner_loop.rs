// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3939: a Tier 2 block (a stored block that writes a captured local) that
//! assigns a local and rebinds it in an inner loop or fold used to answer the local's pre-loop value, silently (`#(0, 0)`
//! for the repro below). The Tier 2 body lowers the loop inside an opaque
//! `let _ = ( ... )`, so the rebind and the state it hands back are lost.
//!
//! PIN-BUG BT-3947: the correct answer is `#(3, 3)`. It is parked with
//! BT-3947's frame work (the body as a frame whose loops are producers); until
//! then the shape is a compile error, so nothing answers wrong silently.

use crate::core_erlang::{CodegenOptions, generate_module};

pub(super) fn compile(src: &str) -> Result<String, String> {
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).map_err(|e| e.to_string())
}

fn actor(sums_body: &str) -> String {
    format!(
        "Actor subclass: P
  state: hits = 0
  sums =>
    c := 0
    blk := [:xs | c := c + 1. {sums_body}]
    blk value: #(1, 2)
"
    )
}

fn assert_loud(src: &str) {
    let err = compile(src).expect_err("the shape must be a compile error, not a silent #(0, 0)");
    assert!(err.contains("BT-3939") && err.contains("`n`"), "{err}");
}

// PIN-BUG BT-3947: correct answer #(3, 3), parked with BT-3947's frame work.
#[test]
fn a_tier_2_block_rebinding_its_local_in_an_inner_do_is_an_error() {
    assert_loud(&actor("n := 0. xs do: [:y | n := n + y]. n"));
}

// PIN-BUG BT-3947: the same through a fold and an assignment's value.
#[test]
fn a_tier_2_block_rebinding_its_local_in_an_inner_fold_is_an_error() {
    assert_loud(&actor(
        "n := 0. r := xs inject: 0 into: [:a :y | n := n + y. a + y]. n",
    ));
}

// PIN-BUG BT-3947: and through a counted loop.
#[test]
fn a_tier_2_block_rebinding_its_local_in_a_counted_loop_is_an_error() {
    assert_loud(&actor("n := 0. 1 to: 2 do: [:i | n := n + i]. n"));
}

#[test]
fn a_loop_that_rebinds_no_block_local_still_compiles() {
    // Writes a field only.
    compile(&actor("xs do: [:y | self.hits := self.hits + y]. 1")).expect("compiles");
    // Binds its own local inside the loop.
    compile(&actor("xs do: [:y | m := y + 1. m]. 1")).expect("compiles");
    // No loop.
    compile(&actor("n := 0. n := n + 1. n")).expect("compiles");
}
