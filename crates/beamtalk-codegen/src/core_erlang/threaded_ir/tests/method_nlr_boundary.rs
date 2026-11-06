// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4484: `VerifyError::MethodNlrAnswersPair` — the NLR boundary of a
//! value-type or class-method function answers the bare value its callers
//! take as the send's result.

use super::*;
use crate::core_erlang::nlr::NlrArmShape;

fn nlr_catch(boundary: NlrBoundary) -> Vec<ThreadedStmt> {
    vec![
        ThreadedStmt::NlrCatch {
            boundary,
            token: TokenId::new("NlrTok"),
            frame: FrameId::ROOT,
            span: span(),
        },
        ThreadedStmt::Statement(Document::Str("Body"), span()),
    ]
}

/// Every boundary whose catch arm answers something other than the bare
/// value: the pre-BT-4484 value-type arm answered `{Value, Self}`, which is
/// this pair shape.
fn non_value_boundaries() -> Vec<NlrBoundary> {
    vec![
        NlrBoundary::ActorReply,
        NlrBoundary::Repl { trace_source: None },
        NlrBoundary::Repl {
            trace_source: Some("x".to_string()),
        },
    ]
}

#[test]
fn a_method_boundary_answering_anything_but_the_value_is_rejected() {
    for scope in [ScopeKind::ValueType, ScopeKind::ClassMethod] {
        for boundary in non_value_boundaries() {
            assert_eq!(
                verify_in_scope(&nlr_catch(boundary.clone()), scope),
                vec![VerifyError::MethodNlrAnswersPair {
                    boundary: boundary.clone(),
                    at: span(),
                }],
                "{scope:?} / {boundary:?}"
            );
        }
    }
}

#[test]
fn the_value_type_and_class_method_boundaries_verify_clean() {
    assert_eq!(NlrBoundary::ValueType.arm_shape(), NlrArmShape::Value);
    for scope in [ScopeKind::ValueType, ScopeKind::ClassMethod] {
        for boundary in [NlrBoundary::ValueType, NlrBoundary::ClassMethod] {
            assert_eq!(
                verify_in_scope(&nlr_catch(boundary.clone()), scope),
                Vec::new(),
                "{scope:?} / {boundary:?}"
            );
        }
    }
}

#[test]
fn actor_and_repl_scopes_are_not_constrained() {
    // An actor method answers the gen_server reply and a REPL eval its
    // `{Value, Bindings}` pair: their own calling conventions.
    for scope in [ScopeKind::Instance, ScopeKind::Repl] {
        for boundary in non_value_boundaries() {
            assert_eq!(
                verify_in_scope(&nlr_catch(boundary.clone()), scope),
                Vec::new(),
                "{scope:?} / {boundary:?}"
            );
        }
    }
}

#[test]
fn the_value_type_boundary_renders_the_bare_value_on_both_paths() {
    let out = lower_and_render(&nlr_catch(NlrBoundary::ValueType)).to_pretty_string();
    assert!(out.contains("'$bt_nlr'"), "{out}");
    // The catch arm's guard is followed by the bare caught value.
    let arm = out
        .split("when call 'erlang':'=:='(")
        .nth(1)
        .unwrap_or_else(|| panic!("no token-matching arm: {out}"));
    let answer = arm.split(") -> ").nth(1).unwrap_or_default();
    assert!(
        !answer.trim_start().starts_with('{'),
        "the value-type catch arm must answer the bare value: {out}"
    );
}
