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

/// Programs whose conditional arms rebind an outer local AND write a field: the construct
/// declares the local in `threads` and the verifier sees its `LocalRebind`.
fn mixed_local_and_field_programs() -> Vec<(String, String)> {
    programs()
        .into_iter()
        // `conditional_condition` splices the multi-statement position into parentheses,
        // which is not a program (no local `t` is ever bound).
        .filter(|(name, _)| {
            name.ends_with("conditional_field_and_local_arms")
                && !name.starts_with("conditional_condition")
        })
        .collect()
}

#[test]
fn a_dropped_local_rebind_of_a_mixed_conditional_is_reported() {
    let cases = mixed_local_and_field_programs();
    assert!(cases.len() >= 4, "only {} mixed cases", cases.len());
    let mut unreported = Vec::new();
    for (name, src) in cases {
        match verifier_report(&src, Some(LocalRebindFault::DropRebind)) {
            Some(report) if report.contains("ThreadedLocalDropped") => {}
            other => unreported.push(format!("{name}: {other:?}\n{src}")),
        }
    }
    assert!(unreported.is_empty(), "{}", unreported.join("\n\n"));
}

/// The producer's gate admits the method-root frame only, and the probe mirrors read the same
/// gate: inside loop / exception bodies a field-writing-arm conditional is a structured
/// rejection, never an internal error from a producer asked to lower in another frame.
#[test]
fn field_writing_arm_conditionals_in_non_root_frames_are_rejected_not_internal_errors() {
    use super::field_assignment_value_position::{VtOutcome, vt_outcome};
    let mut checked = 0;
    for (shape_name, shape, may_reject) in VT_SHAPES {
        if !*may_reject {
            continue;
        }
        for (i, (position_name, position)) in VT_POSITIONS.iter().enumerate() {
            if !has_field_writing_arm(position) {
                continue;
            }
            let stmt = position.replace("EXPR", "(self.n := 5)");
            let src = format!(
                "{VT_HEADER}\n  probe{i} =>\n{}",
                shape.replace("STMT", &stmt)
            );
            assert!(
                !matches!(vt_outcome(&src), VtOutcome::Fails(_)),
                "{shape_name} / {position_name} must compile or be rejected cleanly:\n{src}"
            );
            checked += 1;
        }
    }
    assert!(checked >= 20, "only {checked} non-root cases checked");
}
