// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Sendability tier checks (ADR 0103, Phase 0).
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
        surface_incomplete: false,
        in_caller_new1: false,
        is_module_less: false,
        name: class.into(),
        superclass: Some(superclass.into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        auto_ctors: None,
        handle_scope: None,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        state_kinds: HashMap::new(),
        initialize_assigns: std::collections::BTreeSet::new(),
        has_dynamic_field_writer: false,
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

            origin: None,
            primitive: None,
            constant_value: None,
            answers_receiver: false,
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
        &[],
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

// --- spawnWith: map value check (ADR 0103 Phase 1) ---

#[test]
fn spawn_with_port_value_warns() {
    let src = "Actor subclass: Worker\n  \
        state: p :: Port = nil\n\n\
        Object subclass: Main\n  \
        run: aPort :: Port =>\n    \
        Worker spawnWith: #{ p => aPort }\n";
    let diags = diags(src);
    let warns = sendability_warnings(&diags);
    assert_eq!(
        warns.len(),
        1,
        "expected one spawnWith: sendability warning, got: {diags:?}"
    );
    assert!(
        warns[0].message.contains("spawnWith:") && warns[0].message.contains("process-bound"),
        "unexpected message: {}",
        warns[0].message
    );
}

#[test]
fn spawn_with_sendable_value_silent() {
    let src = "Actor subclass: Worker\n  \
        state: n :: Integer = 0\n\n\
        Object subclass: Main\n  \
        run: count :: Integer =>\n    \
        Worker spawnWith: #{ n => count }\n";
    let diags = diags(src);
    assert!(
        sendability_warnings(&diags).is_empty(),
        "sendable spawnWith: value must not warn: {diags:?}"
    );
}

// --- @expect sendability suppression ---

/// Parse `src`, build its hierarchy, run the full diagnostic pipeline
/// (type check + `@expect` suppression via `apply_expect_directives`).
fn check_source_with_expect(src: &str) -> Vec<Diagnostic> {
    let module = parse_source(src);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
}

#[test]
fn expect_sendability_suppresses_actor_message_warning() {
    // A `Port` passed in an actor message is a sendability Warning (boundary #1,
    // ADR 0103). `@expect sendability` on the preceding line must suppress it
    // end-to-end — this is the only coverage of the
    // `(Sendability, Sendability)` arm in `category_matches`; a rename of the
    // `DiagnosticCategory` variant would otherwise silently break suppression.

    // Without the directive: the warning fires.
    let bare = "Actor subclass: Worker\n  \
        use: p => p asString\n\n\
        Object subclass: Main\n  \
        run: port :: Port with: worker :: Worker =>\n    \
        worker use: port\n";
    let diags_without = check_source_with_expect(bare);
    assert_eq!(
        sendability_warnings(&diags_without).len(),
        1,
        "Port in an actor message must warn without @expect, got: {diags_without:?}"
    );

    // With `@expect sendability`: the warning is suppressed, and the directive
    // is not reported stale (it matched a real diagnostic).
    let suppressed = "Actor subclass: Worker\n  \
        use: p => p asString\n\n\
        Object subclass: Main\n  \
        run: port :: Port with: worker :: Worker =>\n    \
        @expect sendability\n    \
        worker use: port\n";
    let diags_with = check_source_with_expect(suppressed);
    assert!(
        sendability_warnings(&diags_with).is_empty(),
        "@expect sendability must suppress the actor-message warning, got: {diags_with:?}"
    );
    assert!(
        diags_with
            .iter()
            .all(|d| !d.message.contains("stale @expect")),
        "@expect sendability should not be reported stale, got: {diags_with:?}"
    );
}

// --- self-sends are not a process boundary (ADR 0103, BT-4447) ---

#[test]
fn actor_self_send_of_port_is_silent() {
    // An actor's self-send runs in its own process: the port never leaves
    // its owner, so neither a plain nor a local-block self-send warns.
    let src = "Actor subclass: Porter\n  \
        plain: p :: Port -> Nil =>\n    \
        self take: p\n    \
        nil\n\n  \
        inBlock: p :: Port -> Nil =>\n    \
        true ifTrue: [self take: p]\n    \
        nil\n\n  \
        take: p :: Port -> Nil => nil\n";
    let diags = diags(src);
    assert!(
        sendability_warnings(&diags).is_empty(),
        "self-sends are not actor messages: {diags:?}"
    );
}

#[test]
fn actor_send_of_port_to_another_actor_still_warns() {
    let src = "Actor subclass: Porter\n  \
        hand: p :: Port to: other :: Porter -> Nil =>\n    \
        other take: p\n    \
        nil\n\n  \
        take: p :: Port -> Nil => nil\n";
    let diags = diags(src);
    assert_eq!(
        sendability_warnings(&diags).len(),
        1,
        "a send to another actor crosses a process boundary: {diags:?}"
    );
}

#[test]
fn actor_self_send_in_escaping_block_is_exempt() {
    // Pins the exemption for a block that may run away from home (ADR
    // 0137): its self-send messages the owning actor back, so the handle
    // returns to its owner. Revisit this test, deliberately, if away-from-home
    // self-send lowering changes.
    let src = "Actor subclass: Ticker\n  \
        arm: p :: Port -> Nil =>\n    \
        Timer every: 1000 do: [self tick: p]\n    \
        nil\n\n  \
        tick: p :: Port -> Nil => nil\n";
    let diags = diags(src);
    assert!(
        !sendability_warnings(&diags)
            .iter()
            .any(|d| d.message.contains("passed in an actor message")),
        "the self-send inside the escaping block is exempt: {diags:?}"
    );
}

// ── BT-4774: the BT-4717 opaque handle classes carry a scope ────────────

/// A `ZlibStream` or `IoDevice` is process-bound (a zlib resource or a raw
/// file descriptor owned by the opening process), so passing one in an actor
/// message warns exactly as a `Port` does. Before BT-4774 these classes were
/// unscoped Objects and the send was silent.
#[test]
fn process_scoped_opaque_handles_warn_in_actor_message() {
    for class in ["ZlibStream", "IoDevice"] {
        let diags = check_consume("Worker", "Actor", InferredType::known(class));
        let warns = sendability_warnings(&diags);
        assert_eq!(warns.len(), 1, "{class}: expected one warning: {diags:?}");
        assert_eq!(warns[0].severity, Severity::Warning);
        assert!(
            warns[0].message.contains("process-bound handle"),
            "{class}: unexpected message: {}",
            warns[0].message
        );
    }
}

/// An `ets:tid()` is node-local (any process on the node can use it), so
/// `EtsTid` is `#node`-scoped and stays silent in v1 — the same grade `Ets`
/// and `Subscription` get.
#[test]
fn ets_tid_is_node_scoped_and_silent_in_v1() {
    let diags = check_consume("Worker", "Actor", InferredType::known("EtsTid"));
    assert!(
        sendability_warnings(&diags).is_empty(),
        "#node-scoped EtsTid must be silent in v1: {diags:?}"
    );
}
