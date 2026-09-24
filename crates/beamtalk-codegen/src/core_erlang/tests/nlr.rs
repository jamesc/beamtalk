// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;

#[test]
fn wrap_body_with_nlr_catch_class_method_no_vars_contains_nlr_scaffolding() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let body = leaf::var("Body".to_string());
    let doc = generator.wrap_body_with_nlr_catch(
        body,
        &token,
        NlrBoundary::ClassMethod {
            has_class_vars: false,
        },
    );
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
    // ClassMethod{has_class_vars:false} catch arm yields bare value, not a tuple
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
    let doc = generator.wrap_body_with_nlr_catch(body, &token, NlrBoundary::ActorReply);
    let out = doc.to_pretty_string();
    assert!(out.contains("'reply'"), "missing reply atom: {out}");
    assert!(out.contains("'$bt_nlr'"), "missing bt_nlr tag: {out}");
}

#[test]
fn wrap_value_type_body_with_nlr_catch_try_prefix_contains_make_ref_and_try() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let vars = generator.wrap_value_type_body_with_nlr_catch(&token);
    let prefix = vars.format_try_prefix().to_pretty_string();
    assert!(
        prefix.contains("'erlang':'make_ref'()"),
        "prefix missing make_ref: {prefix}"
    );
    assert!(prefix.contains("try"), "prefix missing try: {prefix}");
}

#[test]
fn wrap_value_type_body_with_nlr_catch_suffix_contains_nlr_tag_and_raw_raise() {
    let mut generator = CoreErlangGenerator::new("nlr_test");
    let token = generator.fresh_temp_var("NlrToken");
    let vars = generator.wrap_value_type_body_with_nlr_catch(&token);
    let suffix = vars
        .format_catch_suffix(NlrBoundary::ValueType)
        .to_pretty_string();
    assert!(
        suffix.contains("'$bt_nlr'"),
        "suffix missing bt_nlr: {suffix}"
    );
    assert!(
        suffix.contains("primop 'raw_raise'"),
        "suffix missing raw_raise: {suffix}"
    );
    // ValueType catch arm yields {Value, State} — never the actor-reply tuple
    assert!(
        !suffix.contains("'reply'"),
        "should not contain actor-reply: {suffix}"
    );
}
