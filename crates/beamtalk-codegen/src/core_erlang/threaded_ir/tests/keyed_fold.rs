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

fn body(result_list: Option<FoldResultList>) -> Vec<ThreadedStmt> {
    vec![ThreadedStmt::FoldAccumulator {
        carry: FoldCarry::StateMap,
        value: docvec!["{[E | AccList], StateAcc}"],
        span: span(),
        result_list,
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
