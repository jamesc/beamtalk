// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

#[test]
fn wrap_body_with_nlr_catch_class_method_no_vars_contains_nlr_scaffolding() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let doc = generator.wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ClassMethod);
    let out = doc.to_pretty_string();
    assert!(
        out.contains("'erlang':'make_ref'()"),
        "missing make_ref: {out}"
    );
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr tag: {out}");
    assert!(
        out.contains("primop 'raw_raise'"),
        "missing raw_raise: {out}"
    );
    // ClassMethod catch arm yields bare value, not a tuple
    assert!(
        !out.contains("'reply'"),
        "should not contain actor-reply: {out}"
    );
    assert!(
        !out.contains("'class_var_result'"),
        "should not contain class_var_result: {out}"
    );
}

#[test]
fn wrap_body_with_nlr_catch_actor_reply_catch_arm_contains_reply_atom() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let doc = generator.wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ActorReply);
    let out = doc.to_pretty_string();
    assert!(out.contains("'reply'"), "missing reply atom: {out}");
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr tag: {out}");
}

#[test]
fn wrap_body_with_nlr_catch_value_type_mints_token_and_answers_bare_value() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let out = generator
        .wrap_body_with_nlr_catch(body, &token, &NlrBoundary::ValueType)
        .to_pretty_string();
    assert!(
        out.contains("'erlang':'make_ref'()"),
        "missing make_ref: {out}"
    );
    assert!(out.contains("try"), "missing try: {out}");
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr: {out}");
    assert!(
        out.contains("primop 'raw_raise'"),
        "missing raw_raise: {out}"
    );
    // BT-4503: the ValueType catch arm answers the bare value — neither the
    // actor-reply tuple nor a `{Value, State}` pair.
    assert!(
        !out.contains("'reply'"),
        "should not contain actor-reply: {out}"
    );
    assert!(
        !out.contains("{_NlrVal") && !out.contains("{NlrVal"),
        "should not answer a pair: {out}"
    );
}
