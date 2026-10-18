// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3863: `verify_fold_locals_carried` / `VerifyError::FoldLocalsNotCarried` over hand-built
//! fold lambda bodies. The same invariant is shown to fire on a real compile, over a generated
//! corpus, by `tests/fold_locals_carried_corpus.rs`.

use super::*;

const TUPLE: ThreadingMode = ThreadingMode::TupleAcc(0);

fn map_mode() -> ThreadingMode {
    ThreadingMode::StateAcc(StateAccFallbackReason::ControlFlowMutations)
}

fn threads() -> Vec<String> {
    vec!["t".to_string(), "u".to_string()]
}

fn accumulator(carry: FoldCarry) -> ThreadedStmt {
    ThreadedStmt::FoldAccumulator {
        carry,
        value: docvec!["{T1, U1}"],
        span: span(),
    }
}

fn body_ending_in(tail: ThreadedStmt) -> Vec<ThreadedStmt> {
    vec![
        ThreadedStmt::Statement(docvec!["let _ = 1 in "], span()),
        tail,
    ]
}

fn not_carried(found: Option<FoldCarry>) -> Vec<VerifyError> {
    vec![VerifyError::FoldLocalsNotCarried {
        threads: threads(),
        found,
        at: span(),
    }]
}

#[test]
fn a_tuple_fold_that_repacks_its_locals_in_order_passes() {
    let body = body_ending_in(accumulator(FoldCarry::Locals(threads())));
    assert_eq!(
        verify_fold_locals_carried(&body, &TUPLE, &threads(), span()),
        Vec::new()
    );
}

#[test]
fn a_map_fold_that_returns_its_state_acc_passes() {
    let body = body_ending_in(accumulator(FoldCarry::StateMap));
    assert_eq!(
        verify_fold_locals_carried(&body, &map_mode(), &threads(), span()),
        Vec::new()
    );
}

#[test]
fn a_fold_that_ends_in_its_raw_value_is_not_carried_in_either_mode() {
    // BT-3839: the nested loop's raw value took the accumulator's place.
    let body = body_ending_in(accumulator(FoldCarry::Value));
    for mode in [TUPLE, map_mode()] {
        assert_eq!(
            verify_fold_locals_carried(&body, &mode, &threads(), span()),
            not_carried(Some(FoldCarry::Value)),
            "{mode:?}"
        );
    }
}

#[test]
fn a_tuple_fold_that_repacks_the_wrong_locals_is_not_carried() {
    for locals in [
        vec!["u".to_string(), "t".to_string()],
        vec!["t".to_string()],
    ] {
        let body = body_ending_in(accumulator(FoldCarry::Locals(locals.clone())));
        assert_eq!(
            verify_fold_locals_carried(&body, &TUPLE, &threads(), span()),
            not_carried(Some(FoldCarry::Locals(locals)))
        );
    }
}

#[test]
fn an_accumulator_of_the_other_mode_is_not_carried() {
    let body = body_ending_in(accumulator(FoldCarry::StateMap));
    assert_eq!(
        verify_fold_locals_carried(&body, &TUPLE, &threads(), span()),
        not_carried(Some(FoldCarry::StateMap))
    );
    let body = body_ending_in(accumulator(FoldCarry::Locals(threads())));
    assert_eq!(
        verify_fold_locals_carried(&body, &map_mode(), &threads(), span()),
        not_carried(Some(FoldCarry::Locals(threads())))
    );
}

#[test]
fn a_fold_whose_last_statement_is_not_an_accumulator_is_not_carried() {
    let body = body_ending_in(ThreadedStmt::Statement(docvec!["{T1, U1}"], span()));
    assert_eq!(
        verify_fold_locals_carried(&body, &TUPLE, &threads(), span()),
        not_carried(None)
    );
    assert_eq!(
        verify_fold_locals_carried(&[], &TUPLE, &threads(), span()),
        not_carried(None)
    );
}

#[test]
fn a_fold_that_threads_no_local_may_end_in_its_value() {
    let body = body_ending_in(accumulator(FoldCarry::Value));
    assert_eq!(
        verify_fold_locals_carried(&body, &TUPLE, &[], span()),
        Vec::new()
    );
}

#[test]
fn a_fold_accumulator_renders_its_value_verbatim() {
    let ir = vec![accumulator(FoldCarry::Locals(threads()))];
    assert_eq!(lower_and_render(&ir).to_pretty_string(), "{T1, U1}");
}
