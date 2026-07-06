// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sendability tier checks (ADR 0103, Phase 0 — BT-2753).
//!
//! Covers the builtin tier table applied to actor message arguments: a
//! process-bound handle (`Port`) passed in an actor message warns with zero
//! user annotations, while sendable values, references, and node-scoped
//! handles stay silent.

use super::common::*;
use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
use crate::source_analysis::Severity;
use std::collections::HashMap;

/// Build a hierarchy containing one class whose single method `consume:` takes
/// one untyped parameter. `superclass` decides the class kind — pass `"Actor"`
/// for an actor, `"Object"` for a plain object.
fn hierarchy_with(class: &str, superclass: &str) -> ClassHierarchy {
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: class.into(),
        superclass: Some(superclass.into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "consume:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: class.into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            // Untyped parameter: the sendability check must fire regardless.
            param_types: vec![None],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);
    hierarchy
}

/// Run `consume:` on `class` with a single argument of the given inferred type.
fn check_consume(class: &str, superclass: &str, arg: InferredType) -> Vec<Diagnostic> {
    let hierarchy = hierarchy_with(class, superclass);
    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &class.into(),
        "consume:",
        &[arg],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    checker.take_diagnostics()
}

fn sendability_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Sendability))
        .collect()
}

#[test]
fn port_in_actor_message_warns() {
    let diags = check_consume("Worker", "Actor", InferredType::known("Port"));
    let warns = sendability_warnings(&diags);
    assert_eq!(
        warns.len(),
        1,
        "expected one sendability warning, got: {diags:?}"
    );
    assert_eq!(warns[0].severity, Severity::Warning);
    assert!(
        warns[0].message.contains("process-bound handle")
            && warns[0].message.contains("actor message"),
        "unexpected message: {}",
        warns[0].message
    );
}

#[test]
fn value_arg_silent() {
    let diags = check_consume("Worker", "Actor", InferredType::known("Integer"));
    assert!(
        sendability_warnings(&diags).is_empty(),
        "primitive arg must not warn: {diags:?}"
    );
}

#[test]
fn pid_arg_silent() {
    // Pid is SendableRef, not HandleScoped — no diagnostic in v1.
    let diags = check_consume("Worker", "Actor", InferredType::known("Pid"));
    assert!(
        sendability_warnings(&diags).is_empty(),
        "Pid arg must not warn: {diags:?}"
    );
}

#[test]
fn node_scoped_handle_silent_in_v1() {
    // Subscription is HandleScoped(#node) — silent until cluster-registration.
    let diags = check_consume("Worker", "Actor", InferredType::known("Subscription"));
    assert!(
        sendability_warnings(&diags).is_empty(),
        "#node-scoped arg must be silent in v1: {diags:?}"
    );
}

#[test]
fn port_to_non_actor_silent() {
    // Boundary #1 is actor sends only; a plain Object receiver does not warn.
    let diags = check_consume("Widget", "Object", InferredType::known("Port"));
    assert!(
        sendability_warnings(&diags).is_empty(),
        "non-actor receiver must not trigger the actor-message check: {diags:?}"
    );
}

#[test]
fn dynamic_arg_silent() {
    let diags = check_consume(
        "Worker",
        "Actor",
        InferredType::Dynamic(DynamicReason::UntypedFfi),
    );
    assert!(
        sendability_warnings(&diags).is_empty(),
        "Dynamic arg must be silent (advisory, ADR 0100): {diags:?}"
    );
}
