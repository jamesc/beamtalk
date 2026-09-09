// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use super::*;
use crate::core_erlang::block_analysis::BlockMutationAnalysis;

// ─── Helper constructors ────────────────────────────────────────────────────

/// Creates a bare `CoreErlangGenerator` (not inside a class method,
/// `Actor` context) — used only to exercise `select_hybrid_params`'s
/// `generator.in_class_method()` guard in isolation.
fn plain_generator() -> CoreErlangGenerator {
    CoreErlangGenerator::new("test")
}

/// Creates a `BodyEffects` with all flags set to `false`.
fn clean_effects() -> BodyEffects {
    BodyEffects {
        cond_has_state_effects: false,
        has_tier2_threaded_assign: false,
        has_non_tuple_safe_list_op: false,
        has_nested_counted_loop_mutation: false,
        has_cf_mutations: false,
        has_conditional_threaded_writes: false,
        last_is_destructure: false,
    }
}

/// Creates a `BlockMutationAnalysis` with no mutations.
fn clean_body_analysis() -> BlockMutationAnalysis {
    BlockMutationAnalysis::new()
}

/// Creates a `BlockMutationAnalysis` with self-sends.
fn body_with_self_sends() -> BlockMutationAnalysis {
    let mut a = BlockMutationAnalysis::new();
    a.has_self_sends = true;
    a
}

/// Creates a `BlockMutationAnalysis` with field writes.
fn body_with_field_writes(fields: &[&str]) -> BlockMutationAnalysis {
    let mut a = BlockMutationAnalysis::new();
    a.field_writes = fields.iter().map(ToString::to_string).collect();
    a
}

/// Creates a `BlockMutationAnalysis` with both field writes and self-sends.
fn body_with_field_writes_and_self_sends(fields: &[&str]) -> BlockMutationAnalysis {
    let mut a = body_with_field_writes(fields);
    a.has_self_sends = true;
    a
}

/// Builds a minimal `ThreadingPlan` for testing computed properties.
fn make_plan(
    threaded_locals: Vec<String>,
    key_style: KeyStyle,
    use_direct_params: bool,
    use_tuple_acc: bool,
    use_hybrid_params: bool,
    readonly_fields: Vec<String>,
    mutated_fields: Vec<String>,
) -> ThreadingPlan {
    ThreadingPlan {
        threaded_locals,
        initial_state_var: "State".to_string(),
        key_style,
        context: CodeGenContext::Actor,
        use_direct_params,
        use_tuple_acc,
        tuple_acc_gate_slots: 0,
        use_hybrid_params,
        readonly_fields,
        fallback_reason: StateAccFallbackReason::None,
        mutated_fields,
        threads_class_vars: false,
        initial_class_var: "ClassVars".to_string(),
    }
}

// ─── StateAccFallbackReason Display tests ───────────────────────────────────

#[test]
fn fallback_reason_display_none() {
    assert_eq!(StateAccFallbackReason::None.to_string(), "none");
}

#[test]
fn fallback_reason_display_self_send() {
    assert_eq!(
        StateAccFallbackReason::SelfSendInBody.to_string(),
        "self-send in loop body"
    );
}

#[test]
fn fallback_reason_display_nested_list_op() {
    assert_eq!(
        StateAccFallbackReason::NestedListOpCrossScope.to_string(),
        "nested list op with cross-scope mutation"
    );
}

#[test]
fn fallback_reason_display_tier2_value_call() {
    assert_eq!(
        StateAccFallbackReason::Tier2ValueCallOnThreaded.to_string(),
        "tier-2 value call on threaded local"
    );
}

#[test]
fn fallback_reason_display_all_variants() {
    // Ensure every variant has a non-empty display string.
    let variants = [
        StateAccFallbackReason::None,
        StateAccFallbackReason::SelfSendInBody,
        StateAccFallbackReason::NestedListOpCrossScope,
        StateAccFallbackReason::Tier2ValueCallOnThreaded,
        StateAccFallbackReason::InlineConditionalThreadedWrite,
        StateAccFallbackReason::ConditionStateEffects,
        StateAccFallbackReason::ControlFlowMutations,
        StateAccFallbackReason::NoThreadedLocals,
        StateAccFallbackReason::ValueTypeContext,
        StateAccFallbackReason::NotLetrec,
        StateAccFallbackReason::DestructureAsLastExpr,
    ];
    for v in &variants {
        assert!(!v.to_string().is_empty(), "variant {v:?} has empty display");
    }
}

// ─── convention_label tests ─────────────────────────────────────────────────

#[test]
fn convention_label_direct_params() {
    let plan = make_plan(
        vec!["x".into()],
        KeyStyle::LocalPrefixed,
        true,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.convention_label(), "direct-params");
}

#[test]
fn convention_label_tuple_acc() {
    let plan = make_plan(
        vec!["x".into()],
        KeyStyle::LocalPrefixed,
        false,
        true,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.convention_label(), "tuple-acc");
}

#[test]
fn convention_label_hybrid() {
    let plan = make_plan(
        vec!["x".into()],
        KeyStyle::LocalPrefixed,
        false,
        false,
        true,
        vec!["f".into()],
        vec!["g".into()],
    );
    assert_eq!(plan.convention_label(), "hybrid");
}

#[test]
fn convention_label_stateacc_fallback() {
    let plan = make_plan(
        vec!["x".into()],
        KeyStyle::LocalPrefixed,
        false,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.convention_label(), "StateAcc");
}

// ─── total_extracted_params tests ───────────────────────────────────────────

#[test]
fn total_extracted_params_empty() {
    let plan = make_plan(
        vec![],
        KeyStyle::LocalPrefixed,
        false,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.total_extracted_params(), 0);
}

#[test]
fn total_extracted_params_locals_only() {
    let plan = make_plan(
        vec!["a".into(), "b".into()],
        KeyStyle::LocalPrefixed,
        true,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.total_extracted_params(), 2);
}

#[test]
fn total_extracted_params_locals_and_readonly_fields() {
    let plan = make_plan(
        vec!["a".into(), "b".into()],
        KeyStyle::LocalPrefixed,
        false,
        false,
        true,
        vec!["rf1".into(), "rf2".into(), "rf3".into()],
        vec!["mf1".into()],
    );
    assert_eq!(plan.total_extracted_params(), 5);
}

// ─── state_key tests ────────────────────────────────────────────────────────

#[test]
fn state_key_local_prefixed() {
    let plan = make_plan(
        vec![],
        KeyStyle::LocalPrefixed,
        false,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.state_key("count"), "__local__count");
}

#[test]
fn state_key_repl_plain() {
    let plan = make_plan(
        vec![],
        KeyStyle::ReplPlain,
        false,
        false,
        false,
        vec![],
        vec![],
    );
    assert_eq!(plan.state_key("count"), "count");
}

// ─── select_direct_params tests ─────────────────────────────────────────────

#[test]
fn select_direct_params_eligible() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(ThreadingPlan::select_direct_params(
        true, &threaded, &analysis, &effects
    ));
}

#[test]
fn select_direct_params_not_allowed() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_direct_params(
        false, &threaded, &analysis, &effects
    ));
}

#[test]
fn select_direct_params_empty_threaded() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_direct_params(
        true,
        &[],
        &analysis,
        &effects
    ));
}

#[test]
fn select_direct_params_blocked_by_state_effects() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_self_sends();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_direct_params(
        true, &threaded, &analysis, &effects
    ));
}

#[test]
fn select_direct_params_blocked_by_cond_state_effects() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.cond_has_state_effects = true;
    assert!(!ThreadingPlan::select_direct_params(
        true, &threaded, &analysis, &effects
    ));
}

#[test]
fn select_direct_params_blocked_by_tier2() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_tier2_threaded_assign = true;
    assert!(!ThreadingPlan::select_direct_params(
        true, &threaded, &analysis, &effects
    ));
}

#[test]
fn select_direct_params_blocked_by_nested_list_op() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_non_tuple_safe_list_op = true;
    assert!(!ThreadingPlan::select_direct_params(
        true, &threaded, &analysis, &effects
    ));
}

// ─── select_tuple_acc tests ─────────────────────────────────────────────────

#[test]
fn select_tuple_acc_eligible() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_not_allowed() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_tuple_acc(
        false,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_empty_threaded() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &[],
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_blocked_by_value_type_context() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::ValueType,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_blocked_by_state_effects() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_blocked_by_cf_mutations() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_cf_mutations = true;
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_blocked_by_conditional_threaded_writes() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_conditional_threaded_writes = true;
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

#[test]
fn select_tuple_acc_blocked_by_destructure() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.last_is_destructure = true;
    assert!(!ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects
    ));
}

// ─── select_hybrid_params tests ─────────────────────────────────────────────

#[test]
fn select_hybrid_params_eligible() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    assert!(ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_not_allowed() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_hybrid_params(
        false,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_empty_threaded() {
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &[],
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_skipped_when_direct_params() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    // use_direct_params = true means hybrid is skipped
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        true,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_blocked_by_value_type() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::ValueType,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_blocked_by_no_field_writes() {
    let threaded = vec!["x".to_string()];
    let analysis = clean_body_analysis(); // no field_writes
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_blocked_by_self_sends() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes_and_self_sends(&["n"]);
    let effects = clean_effects();
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_blocked_by_cond_state_effects() {
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let mut effects = clean_effects();
    effects.cond_has_state_effects = true;
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &plain_generator()
    ));
}

#[test]
fn select_hybrid_params_blocked_by_class_method() {
    // ADR 0111 Addendum 9 Question 4 Part B: an otherwise-eligible
    // body (identical to `select_hybrid_params_eligible`'s fixture) must be
    // excluded from Hybrid mode once the generator is inside a class method,
    // since Hybrid's per-field pre-extraction doesn't understand ADR 0110's
    // multi-field class-var map + shadow-write semantics.
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["n"]);
    let effects = clean_effects();
    let mut generator = plain_generator();
    generator.set_in_class_method(true);
    assert!(!ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        false,
        &analysis,
        &effects,
        &generator
    ));
}

// ─── determine_fallback_reason tests ────────────────────────────────────────

#[test]
fn fallback_reason_none_when_optimized_selected() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    let reason = ThreadingPlan::determine_fallback_reason(
        true,
        true,
        &["x".to_string()],
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::None));
}

#[test]
fn fallback_reason_no_threaded_locals() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        true,
        &[],
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::NoThreadedLocals));
}

#[test]
fn fallback_reason_not_letrec() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        false,
        &["x".to_string()],
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::NotLetrec));
}

#[test]
fn fallback_reason_self_send_in_body() {
    let analysis = body_with_self_sends();
    let effects = clean_effects();
    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        true,
        &["x".to_string()],
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
}

#[test]
fn fallback_reason_field_writes_delegates_to_diagnose() {
    // Field writes present but no self-sends — should delegate to diagnose_guard_failure.
    let analysis = body_with_field_writes(&["n"]);
    let mut effects = clean_effects();
    effects.has_tier2_threaded_assign = true;
    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        true,
        &["x".to_string()],
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(
        reason,
        StateAccFallbackReason::Tier2ValueCallOnThreaded
    ));
}

#[test]
fn fallback_reason_value_type_context() {
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        true,
        &["x".to_string()],
        CodeGenContext::ValueType,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::ValueTypeContext));
}

// ─── diagnose_guard_failure tests ───────────────────────────────────────────

#[test]
fn diagnose_nested_list_op() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_non_tuple_safe_list_op = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::NestedListOpCrossScope
    ));
}

#[test]
fn diagnose_tier2_threaded_assign() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_tier2_threaded_assign = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::Tier2ValueCallOnThreaded
    ));
}

#[test]
fn diagnose_cond_state_effects() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.cond_has_state_effects = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::ConditionStateEffects
    ));
}

#[test]
fn diagnose_cf_mutations() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_cf_mutations = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::ControlFlowMutations
    ));
}

#[test]
fn diagnose_conditional_threaded_writes() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.has_conditional_threaded_writes = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::InlineConditionalThreadedWrite
    ));
}

#[test]
fn diagnose_destructure_as_last_expr() {
    let analysis = clean_body_analysis();
    let mut effects = clean_effects();
    effects.last_is_destructure = true;
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::DestructureAsLastExpr
    ));
}

#[test]
fn diagnose_self_sends_fallback() {
    let analysis = body_with_self_sends();
    let effects = clean_effects();
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
}

#[test]
fn diagnose_guard_priority_order() {
    // When multiple guards are set, the first in priority order wins.
    let analysis = body_with_self_sends();
    let mut effects = clean_effects();
    effects.has_non_tuple_safe_list_op = true;
    effects.has_tier2_threaded_assign = true;
    effects.cond_has_state_effects = true;
    // nested_list_op has highest priority in diagnose_guard_failure
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::NestedListOpCrossScope
    ));
}

#[test]
fn diagnose_default_is_control_flow_mutations() {
    // No specific guard triggered, no self-sends — falls through to default.
    let analysis = clean_body_analysis();
    let effects = clean_effects();
    let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
    assert!(matches!(
        reason,
        StateAccFallbackReason::ControlFlowMutations
    ));
}

// ─── BodyEffects struct tests ───────────────────────────────────────────────

#[test]
fn body_effects_default_all_false() {
    let effects = clean_effects();
    assert!(!effects.cond_has_state_effects);
    assert!(!effects.has_tier2_threaded_assign);
    assert!(!effects.has_non_tuple_safe_list_op);
    assert!(!effects.has_cf_mutations);
    assert!(!effects.has_conditional_threaded_writes);
    assert!(!effects.last_is_destructure);
}

// ─── Integration: combined strategy selection tests ─────────────────────────

#[test]
fn letrec_pure_locals_selects_direct_params() {
    // Letrec with pure local mutations (no field writes, no self-sends)
    // should select direct-params.
    let threaded = vec!["count".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();

    let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
    let tuple = ThreadingPlan::select_tuple_acc(
        false,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    let hybrid = ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        direct,
        &analysis,
        &effects,
        &plain_generator(),
    );

    assert!(direct);
    assert!(!tuple);
    assert!(!hybrid);
}

#[test]
fn foldl_pure_locals_selects_tuple_acc() {
    // Foldl list-op with pure local mutations should select tuple-acc.
    let threaded = vec!["sum".to_string()];
    let analysis = clean_body_analysis();
    let effects = clean_effects();

    let direct = ThreadingPlan::select_direct_params(false, &threaded, &analysis, &effects);
    let tuple = ThreadingPlan::select_tuple_acc(
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    let hybrid = ThreadingPlan::select_hybrid_params(
        false,
        &threaded,
        CodeGenContext::Actor,
        direct,
        &analysis,
        &effects,
        &plain_generator(),
    );

    assert!(!direct);
    assert!(tuple);
    assert!(!hybrid);
}

#[test]
fn letrec_field_writes_selects_hybrid() {
    // Letrec with field writes (no self-sends) should select hybrid.
    let threaded = vec!["x".to_string()];
    let analysis = body_with_field_writes(&["counter"]);
    let effects = clean_effects();

    let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
    let hybrid = ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        direct,
        &analysis,
        &effects,
        &plain_generator(),
    );

    assert!(!direct); // field writes block direct-params
    assert!(hybrid);
}

#[test]
fn letrec_self_sends_falls_back_to_stateacc() {
    // Letrec with self-sends should fall back to StateAcc.
    let threaded = vec!["x".to_string()];
    let analysis = body_with_self_sends();
    let effects = clean_effects();

    let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
    let tuple = ThreadingPlan::select_tuple_acc(
        false,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    let hybrid = ThreadingPlan::select_hybrid_params(
        true,
        &threaded,
        CodeGenContext::Actor,
        direct,
        &analysis,
        &effects,
        &plain_generator(),
    );

    assert!(!direct);
    assert!(!tuple);
    assert!(!hybrid);

    let reason = ThreadingPlan::determine_fallback_reason(
        false,
        true,
        &threaded,
        CodeGenContext::Actor,
        &analysis,
        &effects,
    );
    assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
}
