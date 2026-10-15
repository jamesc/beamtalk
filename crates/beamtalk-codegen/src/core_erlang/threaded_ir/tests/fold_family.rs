// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3827: `verify_fold_family_carried` / `VerifyError::FoldFamilyNotCarried` over hand-built
//! fold lambda bodies. The same invariant is shown to fire on a real compile, over a generated
//! corpus, by `tests/value_type_fold_family_corpus.rs`.

use super::*;

const FRAME: FrameId = FrameId::ROOT;

fn self_bind(source: usize, target: usize) -> ThreadedStmt {
    ThreadedStmt::Bind {
        target: self_var(target, FRAME),
        source: self_var(source, FRAME),
        op: BindOp::Put {
            field: "m".to_string(),
            value: ValueRef::Var("_Val".to_string()),
        },
        span: span(),
    }
}

fn accumulator() -> ThreadedStmt {
    ThreadedStmt::Statement(docvec!["StateAcc"], span())
}

fn returns_self(version: usize) -> ThreadedStmt {
    ThreadedStmt::Return(
        ValueRef::Doc(docvec!["StateAcc"]),
        self_var(version, FRAME),
        span(),
    )
}

fn not_carried() -> VerifyError {
    VerifyError::FoldFamilyNotCarried {
        family: VersionPrefix::SelfVt,
        at: span(),
    }
}

#[test]
fn a_fold_body_that_writes_self_and_returns_it_passes() {
    let body = vec![self_bind(0, 1), self_bind(1, 2), returns_self(2)];
    assert_eq!(verify_fold_family_carried(&body, FRAME), Vec::new());
}

#[test]
fn a_fold_body_that_writes_self_but_ends_in_the_bare_accumulator_is_not_carried() {
    // The shape the fold lowering used to refuse: the lambda's `Self{N}` rebind dies with the
    // closure, so every later statement reads the pre-fold `Self`.
    let body = vec![self_bind(0, 1), accumulator()];
    assert_eq!(
        verify_fold_family_carried(&body, FRAME),
        vec![not_carried()]
    );
}

#[test]
fn a_fold_body_that_returns_a_stale_self_version_is_not_carried() {
    let body = vec![self_bind(0, 1), self_bind(1, 2), returns_self(1)];
    assert_eq!(
        verify_fold_family_carried(&body, FRAME),
        vec![not_carried()]
    );
}

#[test]
fn a_return_that_is_not_the_last_statement_does_not_carry() {
    let body = vec![self_bind(0, 1), returns_self(1), accumulator()];
    assert_eq!(
        verify_fold_family_carried(&body, FRAME),
        vec![not_carried()]
    );
}

#[test]
fn a_fold_body_that_writes_no_self_needs_no_return() {
    assert_eq!(
        verify_fold_family_carried(&[accumulator()], FRAME),
        Vec::new()
    );
    assert_eq!(verify_fold_family_carried(&[], FRAME), Vec::new());
}

#[test]
fn a_self_bind_in_another_frame_is_that_frames_business() {
    // A nested construct's own `Self` chain belongs to its own frame node and is checked there.
    let nested = FrameId::ROOT;
    let other = FrameId::new(7);
    let foreign = ThreadedStmt::Bind {
        target: self_var(1, other),
        source: self_var(0, other),
        op: BindOp::Put {
            field: "m".to_string(),
            value: ValueRef::Var("_Val".to_string()),
        },
        span: span(),
    };
    assert_eq!(
        verify_fold_family_carried(&[foreign, accumulator()], nested),
        Vec::new()
    );
}
