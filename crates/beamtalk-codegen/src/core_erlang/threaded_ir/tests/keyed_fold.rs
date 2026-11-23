// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5097: `verify_keyed_fold_call` and `verify_fold_result_list_declared` over hand-built
//! keyed fold calls and fold lambda bodies. The same invariants are shown to fire on a real
//! compile, over a generated corpus, by `tests/keyed_fold_corpus.rs`.

use super::*;
use crate::core_erlang::threaded_ir::ir::{ConsPerCall, FOLD_RESULT_LIST_SLOT};

fn filter_list() -> FoldResultList {
    FoldResultList {
        slot: FOLD_RESULT_LIST_SLOT,
        cons: ConsPerCall::AtMostOne,
    }
}

fn call(result_list: Option<FoldResultList>, families: bool, path: Vec<usize>) -> KeyedFoldCall {
    KeyedFoldCall {
        lambda: "Lambda".to_string(),
        init: docvec!["{[], N}"],
        recv: "Recv".to_string(),
        selector: "select:".to_string(),
        result: "Result".to_string(),
        result_list,
        families,
        list_path: path,
        span: span(),
    }
}

#[test]
fn a_call_to_the_kinds_slot_is_clean_with_and_without_the_family_wrapper() {
    assert_eq!(
        verify_keyed_fold_call(&call(Some(filter_list()), false, vec![1])),
        Vec::new()
    );
    assert_eq!(
        verify_keyed_fold_call(&call(Some(filter_list()), true, vec![1, 1])),
        Vec::new()
    );
}

#[test]
fn a_kind_with_no_result_list_cannot_be_keyed() {
    assert_eq!(
        verify_keyed_fold_call(&call(None, false, vec![1])),
        vec![VerifyError::KeyedFoldNoResultList {
            selector: "select:".to_string(),
            at: span(),
        }]
    );
}

#[test]
fn a_path_that_disagrees_with_the_family_wrapper_is_reported() {
    // A wrapped fold walked as if bare reads element 1 of the wrapper (the whole
    // accumulator), not its list; a bare fold walked as wrapped indexes into the list.
    assert_eq!(
        verify_keyed_fold_call(&call(Some(filter_list()), true, vec![1])),
        vec![VerifyError::KeyedFoldPathMismatch {
            expected: vec![1, 1],
            found: vec![1],
            at: span(),
        }]
    );
    assert_eq!(
        verify_keyed_fold_call(&call(Some(filter_list()), false, vec![1, 1])),
        vec![VerifyError::KeyedFoldPathMismatch {
            expected: vec![1],
            found: vec![1, 1],
            at: span(),
        }]
    );
}

#[test]
fn a_slot_other_than_the_kinds_is_reported() {
    let list = FoldResultList {
        slot: 2,
        cons: ConsPerCall::ExactlyOne,
    };
    assert_eq!(
        verify_keyed_fold_call(&call(Some(list), false, vec![1])),
        vec![VerifyError::KeyedFoldPathMismatch {
            expected: vec![2],
            found: vec![1],
            at: span(),
        }]
    );
}

fn tail() -> Document<'static> {
    docvec!["StateAcc"]
}

/// `{<head>, StateAcc}` with each `head` slot one element of the accumulator tuple.
fn tuple_acc(head: Vec<AccSlot>) -> FoldAccValue {
    FoldAccValue::tuple(head, tail())
}

fn body_of(value: FoldAccValue, result_list: Option<FoldResultList>) -> Vec<ThreadedStmt> {
    vec![ThreadedStmt::FoldAccumulator {
        carry: FoldCarry::StateMap,
        value,
        span: span(),
        result_list,
    }]
}

fn body(result_list: Option<FoldResultList>) -> Vec<ThreadedStmt> {
    body_of(tuple_acc(vec![AccSlot::cons("E", "AccList")]), result_list)
}

fn collect_list() -> FoldResultList {
    FoldResultList {
        slot: FOLD_RESULT_LIST_SLOT,
        cons: ConsPerCall::ExactlyOne,
    }
}

fn twice() -> AccSlot {
    AccSlot::Cons {
        item: "E".to_string(),
        list: Box::new(AccSlot::cons("E", "AccList")),
    }
}

fn mismatch(expected: ConsPerCall, slot_conses: usize, stray: Vec<usize>) -> Vec<VerifyError> {
    vec![VerifyError::FoldResultListConsMismatch {
        expected,
        slot_conses,
        stray_slots: stray,
        at: span(),
    }]
}

#[test]
fn a_lambda_that_declares_its_kinds_list_is_clean() {
    assert_eq!(
        verify_fold_result_list_declared(&body(Some(filter_list())), Some(filter_list()), span()),
        Vec::new()
    );
    // A kind with no result list has nothing to declare.
    assert_eq!(
        verify_fold_result_list_declared(&body(None), None, span()),
        Vec::new()
    );
}

#[test]
fn a_lambda_that_does_not_declare_its_kinds_list_is_reported() {
    let expected = filter_list();
    let undeclared = |found| {
        vec![VerifyError::FoldResultListUndeclared {
            expected,
            found,
            at: span(),
        }]
    };
    assert_eq!(
        verify_fold_result_list_declared(&body(None), Some(expected), span()),
        undeclared(None)
    );
    let other = FoldResultList {
        slot: 1,
        cons: ConsPerCall::ExactlyOne,
    };
    assert_eq!(
        verify_fold_result_list_declared(&body(Some(other)), Some(expected), span()),
        undeclared(Some(other))
    );
    // A body that ends in no accumulator statement at all.
    assert_eq!(
        verify_fold_result_list_declared(
            &[ThreadedStmt::Statement(docvec!["ok"], span())],
            Some(expected),
            span()
        ),
        undeclared(None)
    );
}

/// A filter's `case` over the predicate: the include arm and the skip arm.
fn filter_case(include: AccSlot, skip: AccSlot) -> FoldAccValue {
    let arm = |value, head| {
        Box::new(BoolArm {
            value,
            body: FoldAccBody::Tuple {
                head: vec![head],
                tail: tail(),
            },
        })
    };
    FoldAccValue {
        prelude: None,
        body: FoldAccBody::BoolCase {
            scrutinee: docvec!["Pred"],
            arms: [arm(true, include), arm(false, skip)],
            fallback: docvec!["<V> when 'true' -> error(V)"],
        },
    }
}

#[test]
fn a_filter_that_includes_or_skips_is_clean() {
    let value = filter_case(AccSlot::cons("E", "AccList"), AccSlot::Name("AccList"));
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(value, Some(filter_list())),
            Some(filter_list()),
            span()
        ),
        Vec::new()
    );
}

#[test]
fn a_filter_arm_that_conses_twice_is_rejected() {
    let value = filter_case(twice(), AccSlot::Name("AccList"));
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(value, Some(filter_list())),
            Some(filter_list()),
            span()
        ),
        mismatch(ConsPerCall::AtMostOne, 2, vec![])
    );
}

#[test]
fn an_arm_that_conses_into_slot_two_is_rejected() {
    let value = tuple_acc(vec![AccSlot::Name("AccList"), AccSlot::cons("E", "Other")]);
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(value, Some(filter_list())),
            Some(filter_list()),
            span()
        ),
        mismatch(ConsPerCall::AtMostOne, 0, vec![2])
    );
}

#[test]
fn a_collect_arm_with_no_cons_is_rejected() {
    let value = tuple_acc(vec![AccSlot::Name("AccList")]);
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(value, Some(collect_list())),
            Some(collect_list()),
            span()
        ),
        mismatch(ConsPerCall::ExactlyOne, 0, vec![])
    );
    // And one that conses exactly once is clean.
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(
                tuple_acc(vec![AccSlot::cons("E", "AccList")]),
                Some(collect_list())
            ),
            Some(collect_list()),
            span()
        ),
        Vec::new()
    );
}

#[test]
fn an_accumulator_the_verifier_cannot_read_is_rejected() {
    let opaque = FoldAccValue::opaque(docvec!["{[E | AccList], StateAcc}"]);
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(opaque, Some(filter_list())),
            Some(filter_list()),
            span()
        ),
        vec![VerifyError::FoldResultListOpaque { at: span() }]
    );
}

#[test]
fn a_cons_onto_another_list_than_acc_list_is_rejected() {
    let wrong = |value| {
        verify_fold_result_list_declared(
            &body_of(value, Some(filter_list())),
            Some(filter_list()),
            span(),
        )
    };
    let base = |found| {
        vec![VerifyError::FoldResultListWrongBase {
            slot: FOLD_RESULT_LIST_SLOT,
            found,
            at: span(),
        }]
    };
    assert_eq!(
        wrong(tuple_acc(vec![AccSlot::cons("E", "MatchList")])),
        base("MatchList")
    );
    // A skip arm that carries some other list drops the accumulated one too.
    assert_eq!(
        wrong(filter_case(
            AccSlot::cons("E", "AccList"),
            AccSlot::Name("Other")
        )),
        base("Other")
    );
}

#[test]
fn a_head_too_short_to_reach_the_slot_is_reported_as_missing_not_opaque() {
    assert_eq!(
        verify_fold_result_list_declared(
            &body_of(tuple_acc(Vec::new()), Some(filter_list())),
            Some(filter_list()),
            span()
        ),
        vec![VerifyError::FoldResultListSlotMissing {
            slot: FOLD_RESULT_LIST_SLOT,
            at: span(),
        }]
    );
}
