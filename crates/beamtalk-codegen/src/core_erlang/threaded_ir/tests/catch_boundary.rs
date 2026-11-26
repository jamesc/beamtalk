// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `on:do:` catch boundary: `verify()`'s `CatchNlrNotPassedThrough` and
//! `CatchFilterNotClosed`, and the order `render()` emits.

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
        ex_class_term: v("ExClass1"),
        filter_fallback_var: v("ExClass1NoMatch"),
    }
}

fn handler() -> beamtalk_cerl_doc::Document<'static> {
    beamtalk_cerl_doc::Document::Str("apply Handler1 (ExObj1)")
}

fn handler_steps() -> Vec<CatchStep> {
    vec![
        CatchStep::WrapException,
        CatchStep::ClassFilter,
        CatchStep::FilterHandler(handler()),
        CatchStep::FilterMiss,
    ]
}

fn node(clauses: Vec<CatchClause>) -> ThreadedStmt {
    ThreadedStmt::OnDoCatch {
        vars: Box::new(vars()),
        cell: None,
        try_region: TryRegion {
            body: beamtalk_cerl_doc::Document::Str("apply Block1 () "),
            result_var: "Result1".to_string(),
        },
        clauses,
        span: span(),
    }
}

/// The compiled order: actor 4-tuple arm, 3-tuple arm, then the non-NLR arm.
fn well_formed() -> Vec<CatchClause> {
    vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: handler_steps(),
        },
    ]
}

fn defect(ir: &[ThreadedStmt]) -> Option<CatchNlrDefect> {
    match verify(ir).as_slice() {
        [] => None,
        [VerifyError::CatchNlrNotPassedThrough { defect: found, at }] => {
            assert_eq!(*at, span());
            Some(*found)
        }
        other => panic!("expected one CatchNlrNotPassedThrough, got {other:?}"),
    }
}

#[test]
fn well_formed_catch_boundary_verifies_clean() {
    assert_eq!(verify(&[node(well_formed())]), Vec::new());
}

#[test]
fn non_nlr_arm_ordered_before_the_nlr_arms_fails() {
    // The handler reached before a `^` is matched: a non-local return would
    // be handled as an error.
    let ir = [node(vec![
        CatchClause::NonNlr {
            steps: handler_steps(),
        },
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchNlrDefect::NlrArmNotBeforeNonNlr(NlrThrowShape::Tuple3))
    );
}

#[test]
fn non_nlr_arm_between_the_nlr_arms_fails() {
    // The 3-tuple arm is ordered after the non-NLR arm: a plain `^` is handled.
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NonNlr {
            steps: handler_steps(),
        },
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchNlrDefect::NlrArmNotBeforeNonNlr(NlrThrowShape::Tuple3))
    );
}

#[test]
fn a_missing_nlr_arm_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: handler_steps(),
        },
    ])];
    assert_eq!(
        defect(&ir),
        Some(CatchNlrDefect::NlrArmNotBeforeNonNlr(NlrThrowShape::Tuple4))
    );
}

#[test]
fn a_catch_with_no_non_nlr_arm_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
    ])];
    assert_eq!(defect(&ir), Some(CatchNlrDefect::NoNonNlrClause));
}

#[test]
fn render_orders_both_nlr_arms_before_the_filter() {
    let text = lower_and_render(&[node(well_formed())]).to_pretty_string();
    let at = |needle: &str| {
        text.find(needle)
            .unwrap_or_else(|| panic!("`{needle}` missing from: {text}"))
    };
    let four = at("{'$bt_nlr', Tok4, Val4, State4}");
    let three = at("{'$bt_nlr', Tok3, Val3}");
    let wrap = at("'beamtalk_exception_handler':'ensure_wrapped'");
    let filter = at("'beamtalk_exception_handler':'matches_class'(ExClass1, ExObj1)");
    assert!(four < three && three < wrap && wrap < filter);
    // ADR 0153: the node is a plain `try`/`catch`; nothing before the `try`.
    assert!(text.starts_with(
        "try apply Block1 () of Result1 -> Result1 \
         catch <Type1, Error1, Stack1> -> case {Type1, Error1} of "
    ));
    assert!(text.ends_with(" end end"));
}

/// BT-3718: the class filter's `case` must be closed by its `'false'` re-raise
/// and an exhaustive wildcard clause, or `erlc` rejects an `on:do:` nested in
/// another protected region with `ambiguous_catch_try_state`.
#[test]
fn render_closes_the_filter_with_a_reraise_and_an_exhaustive_fallback() {
    let text = lower_and_render(&[node(well_formed())]).to_pretty_string();
    let handler = text.find("apply Handler1 (ExObj1)").expect("handler arm");
    let miss = text
        .find("<'false'> when 'true' -> primop 'raw_raise'(Type1, Error1, Stack1)")
        .expect("filter-miss re-raise");
    let fallback = text
        .find(
            "<ExClass1NoMatch> when 'true' -> call 'erlang':'error'({'case_clause', ExClass1NoMatch})",
        )
        .expect("exhaustive fallback clause");
    assert!(handler < miss && miss < fallback);
}

fn filter_defect(ir: &[ThreadedStmt]) -> bool {
    verify(ir)
        .iter()
        .any(|e| matches!(e, VerifyError::CatchFilterNotClosed { at } if *at == span()))
}

#[test]
fn a_filter_with_no_miss_arm_or_fallback_fails() {
    // The shape every compiled `on:do:` had before BT-3718: the filter opened,
    // its handler and `'false'` arm left to the caller's text, no wildcard.
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: vec![CatchStep::WrapException, CatchStep::ClassFilter],
        },
    ])];
    assert!(filter_defect(&ir));
}

#[test]
fn a_filter_closed_without_its_handler_fails() {
    let ir = [node(vec![
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple4),
        CatchClause::NlrPassThrough(NlrThrowShape::Tuple3),
        CatchClause::NonNlr {
            steps: vec![
                CatchStep::WrapException,
                CatchStep::ClassFilter,
                CatchStep::FilterMiss,
            ],
        },
    ])];
    assert!(filter_defect(&ir));
}

#[test]
fn a_well_formed_catch_boundary_closes_its_filter() {
    assert!(!filter_defect(&[node(well_formed())]));
}
