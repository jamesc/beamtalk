// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0130 §4: the `on:do:` catch boundary. `verify()`'s
//! `CatchWithoutClassVarRestore` and the order `render()` emits.

use super::*;

fn vars() -> OnDoCatchVars {
    let v = |n: &str| n.to_string();
    OnDoCatchVars {
        type_var: v("Type1"),
        error_var: v("Error1"),
        stack_var: v("Stack1"),
        nlr_tok_var: v("Tok4"),
        nlr_val_var: v("Val4"),
        nlr_state_var: v("State4"),
        nlr_tok_var2: v("Tok3"),
        nlr_val_var2: v("Val3"),
        other_pair_var: v("Other1"),
        built_stack_var: v("Built1"),
        ex_obj_var: v("ExObj1"),
        match_var: v("Match1"),
        ex_class_var: v("ExClass1"),
        snapshot_var: v("ExClass1Snap"),
    }
}

fn restore_steps(snapshot: &str) -> Vec<CatchStep> {
    vec![
        CatchStep::ClassVarRestore {
            snapshot: snapshot.to_string(),
        },
        CatchStep::WrapException,
        CatchStep::ClassFilter,
    ]
}

fn node(clauses: Vec<CatchClause>) -> ThreadedStmt {
    ThreadedStmt::OnDoCatch {
        vars: Box::new(vars()),
        clauses,
        span: span(),
    }
}

/// The compiled order: actor 4-tuple arm, 3-tuple arm, then the non-NLR arm
/// beginning with the restore.
fn well_formed() -> Vec<CatchClause> {
    vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: restore_steps("ExClass1Snap"),
        },
    ]
}

fn defect(ir: &[ThreadedStmt]) -> Option<CatchRestoreDefect> {
    match verify(ir).as_slice() {
        [] => None,
        [VerifyError::CatchWithoutClassVarRestore { defect: found, at }] => {
            assert_eq!(*at, span());
            Some(*found)
        }
        other => panic!("expected one CatchWithoutClassVarRestore, got {other:?}"),
    }
}

#[test]
fn well_formed_catch_boundary_verifies_clean() {
    assert_eq!(verify(&[node(well_formed())]), Vec::new());
}

#[test]
fn restore_ordered_before_the_nlr_arms_fails() {
    // The restore reached before a `^` is matched: a non-local return would
    // discard the writes made before it.
    let ir = [node(vec![
        CatchClause::NonNlr {
            steps: restore_steps("ExClass1Snap"),
        },
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchRestoreDefect::NlrArmNotBeforeRestore(
            NlrThrowShape::Tuple3
        ))
    );
}

#[test]
fn restore_between_the_nlr_arms_fails() {
    // The 3-tuple arm is ordered after the restore: a plain `^` is restored.
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NonNlr {
            steps: restore_steps("ExClass1Snap"),
        },
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchRestoreDefect::NlrArmNotBeforeRestore(
            NlrThrowShape::Tuple3
        ))
    );
}

#[test]
fn a_missing_nlr_arm_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: restore_steps("ExClass1Snap"),
        },
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchRestoreDefect::NlrArmNotBeforeRestore(
            NlrThrowShape::Tuple4
        ))
    );
}

#[test]
fn a_non_nlr_arm_that_does_not_begin_with_the_restore_fails() {
    // The wrap and the class filter run first: the handler's class filter
    // would see the unrestored map.
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: vec![
                CatchStep::WrapException,
                CatchStep::ClassVarRestore {
                    snapshot: "ExClass1Snap".to_string(),
                },
                CatchStep::ClassFilter,
            ],
        },
    ])];
    assert_eq!(defect(&ir), Some(CatchRestoreDefect::RestoreNotFirst));
}

#[test]
fn a_non_nlr_arm_without_any_restore_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: vec![CatchStep::WrapException, CatchStep::ClassFilter],
        },
    ])];
    assert_eq!(defect(&ir), Some(CatchRestoreDefect::RestoreNotFirst));
}

#[test]
fn a_catch_with_no_non_nlr_arm_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(defect(&ir), Some(CatchRestoreDefect::NoNonNlrClause));
}

#[test]
fn a_restore_of_some_other_snapshot_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: restore_steps("SomeOtherSnap"),
        },
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchRestoreDefect::RestoreReadsWrongSnapshot)
    );
}

#[test]
fn render_orders_both_nlr_arms_before_the_restore_before_the_filter() {
    let text = lower_and_render(&[node(well_formed())]).to_pretty_string();
    let at = |needle: &str| {
        text.find(needle)
            .unwrap_or_else(|| panic!("`{needle}` missing from: {text}"))
    };
    let four = at("{'$bt_nlr', Tok4, Val4, State4}");
    let three = at("{'$bt_nlr', Tok3, Val3}");
    let restore = at("do call 'beamtalk_class_vars':'restore'(ExClass1Snap)");
    let wrap = at("'beamtalk_exception_handler':'ensure_wrapped'");
    let filter = at("'beamtalk_exception_handler':'matches_class'(ExClass1, ExObj1)");
    assert!(four < three && three < restore && restore < wrap && wrap < filter);
    assert!(text.starts_with("catch <Type1, Error1, Stack1> -> case {Type1, Error1} of "));
    assert!(text.ends_with("<'true'> when 'true' -> "));
}
