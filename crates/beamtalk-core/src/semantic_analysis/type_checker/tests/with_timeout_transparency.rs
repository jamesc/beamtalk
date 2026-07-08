// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2751: `withTimeout:` return-type transparency + cross-process DNU
//! wording (ADR 0104 Phase 3).
//!
//! `db withTimeout: 30000` returns a `TimeoutProxy` at runtime, but the proxy
//! is *transparent*: it forwards every message to the wrapped actor, and a
//! timed-out call raises rather than returning. So statically the result is
//! typed as the wrapped receiver's type `C`, not the opaque `TimeoutProxy`.
//! Forwarded calls therefore resolve the wrapped class's real return types
//! instead of collapsing to `Dynamic`.
//!
//! Three behaviours are pinned here:
//!
//! 1. `db withTimeout: 30000` (`db :: SlowDb`) infers `SlowDb` — not
//!    `TimeoutProxy`, not `Dynamic`.
//! 2. A forwarded call on the proxy (`(db withTimeout: 30000) query: sql`)
//!    infers the wrapped method's declared return type (`List`).
//! 3. An unknown selector on a statically-known actor class gets the *same*
//!    knowledge-graded DNU diagnostic as a local send — the process boundary
//!    adds no new severity rule (ADR 0100). Sync actor sends already type as
//!    method sends (BT-2749), so no code change was needed; this pins it.
//! 4. A value typed literally as `TimeoutProxy` (the proxy's own type)
//!    silences unresolved selectors, because `TimeoutProxy` overrides
//!    `doesNotUnderstand:args:` to forward (ADR 0100).

use super::common::*;

/// Register a `SlowDb` Actor subclass with a `query: :: String -> List`
/// method, modelling a wrapped actor whose forwarded calls carry a real,
/// non-`Dynamic` return type.
fn add_slow_db_actor(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let info = ClassInfo {
        name: eco_string("SlowDb"),
        superclass: Some(eco_string("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("query:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("SlowDb"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("List")),
            param_types: vec![Some(eco_string("String"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![info]);
}

/// Register a non-Actor `Object` subclass `FakeProxy` that declares its own
/// `withTimeout: :: Integer -> TimeoutProxy` method — used to pin that the
/// transparency rule is guarded on Actor-kind and does NOT fire here.
fn add_fake_proxy_object(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let info = ClassInfo {
        name: eco_string("FakeProxy"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("withTimeout:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("FakeProxy"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("TimeoutProxy")),
            param_types: vec![Some(eco_string("Integer"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![info]);
}

/// Build `receiver withTimeout: <ms>`.
fn with_timeout(receiver: Expression, ms: i64) -> Expression {
    msg_send(
        receiver,
        MessageSelector::Keyword(vec![KeywordPart::new("withTimeout:", span())]),
        vec![int_lit(ms)],
    )
}

/// AC #1: `db withTimeout: 30000` on a `SlowDb` receiver infers `SlowDb`
/// (transparency) — not `TimeoutProxy`, not `Dynamic`.
#[test]
fn with_timeout_infers_receiver_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_slow_db_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("db", InferredType::known("SlowDb"));

    let ty = checker.infer_expr(&with_timeout(var("db"), 30000), &hierarchy, &mut env, false);

    assert_eq!(
        ty,
        InferredType::known("SlowDb"),
        "`db withTimeout: 30000` must infer the wrapped receiver type SlowDb, \
         not the opaque TimeoutProxy"
    );
    assert!(
        checker.take_diagnostics().is_empty(),
        "a well-typed withTimeout: send must not emit diagnostics"
    );
}

/// AC #2: a forwarded call on the proxy infers the wrapped method's return
/// type. `(db withTimeout: 30000) query: sql` → `List`. Before transparency
/// this collapsed to `Dynamic` (unknown selector on the opaque proxy).
#[test]
fn forwarded_call_on_proxy_infers_wrapped_return_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_slow_db_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("db", InferredType::known("SlowDb"));
    env.set_local("sql", InferredType::known("String"));

    let query = msg_send(
        with_timeout(var("db"), 30000),
        MessageSelector::Keyword(vec![KeywordPart::new("query:", span())]),
        vec![var("sql")],
    );

    let ty = checker.infer_expr(&query, &hierarchy, &mut env, false);

    assert_eq!(
        ty,
        InferredType::known("List"),
        "`(db withTimeout: 30000) query: sql` must infer query:'s declared \
         return type List through the transparent proxy"
    );
    assert!(
        checker
            .take_diagnostics()
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Dnu)),
        "a real forwarded method must not warn as an unknown selector"
    );
}

/// Guard invariant: the transparency rule fires only for `Actor`-kind
/// receivers. A non-Actor class with its own `withTimeout: -> TimeoutProxy`
/// method must keep the declared `TimeoutProxy` return, NOT be retyped as the
/// receiver — otherwise a user type wrapping `withTimeout:` would be silently
/// mis-typed. Pins the `is_actor_subclass` guard at inference.rs.
#[test]
fn non_actor_with_timeout_is_not_retyped() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_fake_proxy_object(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("fp", InferredType::known("FakeProxy"));

    let ty = checker.infer_expr(&with_timeout(var("fp"), 5000), &hierarchy, &mut env, false);

    assert_eq!(
        ty,
        InferredType::known("TimeoutProxy"),
        "a non-Actor class's own `withTimeout: -> TimeoutProxy` must keep its \
         declared return, not be retyped as the receiver (guard is Actor-only)"
    );
}

// Note: `withTimeout:` returns `receiver_ty.clone()`, so type args are
// preserved for any receiver that reaches the guard — including *generic*
// actors. `(q withTimeout: t) m` on `q :: Queue(Integer)` resolves `m`'s
// return with `E = Integer`; this is verified end-to-end from real source in
// `adr_0104_integration.rs::generic_actor_through_proxy_*`. It is pinned there
// rather than here because the hand-built `ClassHierarchy` in this unit file
// does not reproduce a generic actor's builtin-method inheritance the way the
// full compile pipeline does.

/// AC #2 (two-step form via a binding): `slowDb := db withTimeout: 30000`
/// then `slowDb query: sql` infers `List` — the binding carries the
/// transparent type, so the forwarded call resolves the wrapped method.
#[test]
fn proxy_binding_forwards_wrapped_return_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_slow_db_actor(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("db", InferredType::known("SlowDb"));
    env.set_local("sql", InferredType::known("String"));

    // slowDb := db withTimeout: 30000
    let slow_db_ty =
        checker.infer_expr(&with_timeout(var("db"), 30000), &hierarchy, &mut env, false);
    assert_eq!(slow_db_ty, InferredType::known("SlowDb"));
    env.set_local("slowDb", slow_db_ty);

    // slowDb query: sql
    let query = msg_send(
        var("slowDb"),
        MessageSelector::Keyword(vec![KeywordPart::new("query:", span())]),
        vec![var("sql")],
    );
    let ty = checker.infer_expr(&query, &hierarchy, &mut env, false);

    assert_eq!(
        ty,
        InferredType::known("List"),
        "a binding assigned from withTimeout: must forward the wrapped return type"
    );
}

/// Build a `ClassInfo` named `name` under `superclass` with one `log:` method.
fn logger_class(
    name: &str,
    superclass: &str,
) -> crate::semantic_analysis::class_hierarchy::ClassInfo {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    ClassInfo {
        name: eco_string(name),
        superclass: Some(eco_string(superclass)),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("log:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string(name),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("Nil")),
            param_types: vec![Some(eco_string("String"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

/// Send the typo selector `logg:` (for `log:`) to local `recv`.
fn logg_send(recv: &str) -> Expression {
    msg_send(
        var(recv),
        MessageSelector::Keyword(vec![KeywordPart::new("logg:", span())]),
        vec![str_lit("hi")],
    )
}

/// Infer `logg_send(recv)` against `hierarchy` with `recv :: class_name` and
/// return the emitted diagnostics.
fn dnu_diags_for(hierarchy: &ClassHierarchy, recv: &str, class_name: &str) -> Vec<Diagnostic> {
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(recv, InferredType::known(class_name));
    checker.infer_expr(&logg_send(recv), hierarchy, &mut env, false);
    checker.take_diagnostics()
}

/// AC #3: an unknown selector on a statically-known actor class gets the same
/// knowledge-graded DNU diagnostic as a local (plain-object) send. The process
/// boundary adds no new severity rule (ADR 0100); sync actor sends already
/// route through the shared `check_instance_selector` path (BT-2749), so the
/// wording is unified with no code change — this test pins it.
#[test]
fn cross_process_dnu_wording_matches_local_send() {
    // Two classes with an identical `log:` method: one an Actor subclass
    // (cross-process sends), one a plain Object (local sends).
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        logger_class("ActorLogger", "Actor"),
        logger_class("LocalLogger", "Object"),
    ]);

    let actor_diags = dnu_diags_for(&hierarchy, "actorLog", "ActorLogger");
    let local_diags = dnu_diags_for(&hierarchy, "localLog", "LocalLogger");

    let actor_dnu = actor_diags
        .iter()
        .find(|d| d.category == Some(DiagnosticCategory::Dnu))
        .expect("actor send of unknown selector must emit a DNU diagnostic");
    let local_dnu = local_diags
        .iter()
        .find(|d| d.category == Some(DiagnosticCategory::Dnu))
        .expect("local send of unknown selector must emit a DNU diagnostic");

    // Same severity grade (ADR 0100), same category, same did-you-mean hint —
    // the only difference is the receiver class name in the message text.
    assert_eq!(
        actor_dnu.severity, local_dnu.severity,
        "cross-process DNU must have the same severity grade as a local send"
    );
    assert_eq!(actor_dnu.category, local_dnu.category);
    assert_eq!(
        actor_dnu.hint, local_dnu.hint,
        "cross-process DNU must carry the same did-you-mean hint"
    );
    assert_eq!(
        actor_dnu.message.as_str(),
        "ActorLogger does not understand 'logg:'",
        "cross-process DNU wording must match the unified local-send format"
    );
    assert_eq!(
        local_dnu.message.as_str(),
        "LocalLogger does not understand 'logg:'"
    );
    assert_eq!(
        actor_dnu.hint.as_deref(),
        Some("Did you mean 'log:'?"),
        "the did-you-mean hint must survive the process boundary"
    );

    // The wording is identical once the receiver class name is factored out.
    assert_eq!(
        actor_dnu.message.replace("ActorLogger", "<Recv>"),
        local_dnu.message.replace("LocalLogger", "<Recv>"),
        "cross-process and local DNU wording must be unified"
    );
}

/// AC #4: a value typed literally as `TimeoutProxy` (the proxy's own type)
/// must not spuriously warn on any selector — `TimeoutProxy` overrides
/// `doesNotUnderstand:args:` to forward, and ADR 0100 silences unresolved
/// selectors on DNU-overriding receivers. This guards against a regression
/// where transparency would leak a raw-proxy value into DNU checking.
#[test]
fn timeout_proxy_forwarded_selectors_do_not_warn() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        hierarchy.has_instance_dnu_override("TimeoutProxy"),
        "TimeoutProxy must override doesNotUnderstand:args: for forwarding"
    );

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("proxy", InferredType::known("TimeoutProxy"));

    let send = msg_send(
        var("proxy"),
        MessageSelector::Keyword(vec![KeywordPart::new("anyForwardedSelector:", span())]),
        vec![int_lit(1)],
    );
    checker.infer_expr(&send, &hierarchy, &mut env, false);

    assert!(
        checker
            .take_diagnostics()
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Dnu)),
        "an arbitrary selector on a TimeoutProxy-typed value must not warn — \
         the proxy forwards via doesNotUnderstand:args:"
    );
}
