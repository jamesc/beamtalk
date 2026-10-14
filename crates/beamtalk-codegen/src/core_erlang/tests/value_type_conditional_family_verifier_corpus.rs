// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3821: the `ConstructFamilyDropped` verifier invariant over a generated corpus of real
//! compiles, not only hand-built IR (`threaded_ir/tests/local_rebind.rs`).
//!
//! **Invariant** (every scope kind a value-type conditional can sit in): a conditional whose
//! arms write a field, lowered in value position, declares the `Self` family its arms advanced
//! on its construct tuple, and the merge `Bind` of that family follows the tuple in the caller's
//! statement sequence. A conditional lowered without that `Bind` scopes the arms' `Self` rebind
//! inside its tuple: the field write is silently lost to every later statement, `erlc`-clean.
//!
//! The corpus is the cross product of the field-writing-arm value positions and the
//! statement-level shapes of `field_assignment_value_position.rs` (the very programs that
//! must compile). Each is compiled twice: unbroken (control: compiles and the verifier is
//! silent) and with the producer's merge `Bind` deliberately dropped
//! ([`fault::LocalRebindFault::DropFamilyBind`]), which the verifier must report.

use super::codegen;
use super::field_assignment_value_position::{VT_HEADER, VT_POSITIONS, VT_SHAPES};
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};

/// Whether the position's own construct is a conditional whose ARM writes a field (the value
/// the producer merges), as opposed to one that only holds a field write in its receiver.
fn has_field_writing_arm(template: &str) -> bool {
    [
        "ifTrue: [self.",
        "ifFalse: [self.",
        "ifTrue: [t := t + 1. self.",
    ]
    .iter()
    .any(|arm| template.contains(arm))
}

/// The corpus: `(case name, program)` for every statement-level shape (a shape that cannot be
/// rejected) x every field-writing-arm position.
fn programs() -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (shape_name, shape, may_reject) in VT_SHAPES {
        if *may_reject {
            continue;
        }
        for (i, (position_name, position)) in VT_POSITIONS.iter().enumerate() {
            if !has_field_writing_arm(position) {
                continue;
            }
            let stmt = position.replace("EXPR", "(self.n := 5)");
            out.push((
                format!("{shape_name} / {position_name}"),
                format!(
                    "{VT_HEADER}\n  probe{i} =>\n{}",
                    shape.replace("STMT", &stmt)
                ),
            ));
        }
    }
    out
}

/// The verifier's debug-build report for `src` compiled with `fault` active: the message of the
/// panic `report_threaded_ir_verify_errors` raises, or `None` when the compile succeeds.
fn verifier_report(src: &str, fault: Option<LocalRebindFault>) -> Option<String> {
    let compile = || std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)));
    let result = match fault {
        Some(fault) => fault::with(fault, compile),
        None => compile(),
    };
    result.err().map(|payload| {
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()
    })
}

#[test]
fn the_corpus_covers_every_field_writing_arm_position_in_every_statement_shape() {
    let positions = VT_POSITIONS
        .iter()
        .filter(|(_, template)| has_field_writing_arm(template))
        .count();
    let shapes = VT_SHAPES
        .iter()
        .filter(|(_, _, may_reject)| !may_reject)
        .count();
    assert!(
        positions >= 10 && shapes >= 5,
        "{positions} positions x {shapes} shapes: the corpus has thinned out"
    );
    assert_eq!(programs().len(), positions * shapes);
}

#[test]
fn every_unbroken_field_writing_arm_conditional_compiles_with_a_silent_verifier() {
    let failures: Vec<String> = programs()
        .into_iter()
        .filter_map(|(name, src)| {
            verifier_report(&src, None).map(|report| format!("{name}: {report}\n{src}"))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn a_dropped_family_merge_is_reported_for_every_field_writing_arm_conditional() {
    let mut unreported = Vec::new();
    for (name, src) in programs() {
        match verifier_report(&src, Some(LocalRebindFault::DropFamilyBind)) {
            Some(report) if report.contains("ConstructFamilyDropped") => {}
            Some(report) => unreported.push(format!("{name}: wrong report: {report}\n{src}")),
            None => unreported.push(format!("{name}: compiled without a report\n{src}")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n\n"));
}
