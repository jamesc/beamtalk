// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `self delegate` expressions infer the enclosing method's declared return
//! type instead of `Dynamic` (BT-2862).
//!
//! `delegate` (ADR 0056 / ADR 0101) is a sentinel with no per-selector return
//! type of its own — `Actor>>delegate` has no return-type annotation at all,
//! and `Object>>delegate -> Never` only exists to type the non-native error
//! path. A native class's `self delegate` method body used to inherit
//! whichever of those two shapes its base class carried (`Dynamic` for
//! Actor-backed classes, `Never` for Object-backed ones) — in neither case
//! reflecting the author-declared return type sitting right next to `=>`.
//!
//! The fix (`TypeChecker::resolve_self_delegate_return_type`) trusts the
//! enclosing method's own annotation whenever the body is exactly
//! `self delegate` (`MethodDefinition::is_self_delegate`), matching the
//! gradual-typing "trust the annotation" model (ADR 0025). A `self delegate`
//! body with no return-type annotation is untouched — same value as before.

use super::common::*;

fn build(source: &str) -> (Module, ClassHierarchy) {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    (module, hierarchy)
}

/// Look up the inferred type recorded for a method's `self delegate` body
/// expression (the method's single statement, per `is_self_delegate`).
fn self_delegate_body_ty<'a>(
    checker: &'a TypeChecker,
    class: &crate::ast::ClassDefinition,
    selector: &str,
) -> &'a InferredType {
    let method = class
        .methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .unwrap_or_else(|| panic!("no instance method `{selector}` found"));
    checker
        .type_map()
        .get(method.body[0].expression.span())
        .unwrap_or_else(|| panic!("no type_map entry for `{selector}`'s self-delegate body"))
}

fn class_method_self_delegate_body_ty<'a>(
    checker: &'a TypeChecker,
    class: &crate::ast::ClassDefinition,
    selector: &str,
) -> &'a InferredType {
    let method = class
        .class_methods
        .iter()
        .find(|m| m.selector.name() == selector)
        .unwrap_or_else(|| panic!("no class method `{selector}` found"));
    checker
        .type_map()
        .get(method.body[0].expression.span())
        .unwrap_or_else(|| {
            panic!("no type_map entry for class-side `{selector}`'s self-delegate body")
        })
}

// ── Actor-backed native classes ─────────────────────────────────────────
//
// `Actor>>delegate` (stdlib) has no return-type annotation at all, so
// pre-fix every `self delegate` body on a native Actor subclass inferred
// `Dynamic(DynamicReceiver)` regardless of the enclosing method's own
// declared type — this is the exact `HTTPServer` repro from the issue.

#[test]
fn actor_backed_self_delegate_trusts_declared_return_type() {
    let (module, hierarchy) = build(
        "Actor subclass: HTTPServer native: beamtalk_http_server\n\
         \x20\x20port -> Integer => self delegate\n\
         \x20\x20printString -> String => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert_eq!(
        *self_delegate_body_ty(&checker, class, "port"),
        InferredType::known("Integer"),
        "`port -> Integer => self delegate` should infer Integer, not Dynamic"
    );
    assert_eq!(
        *self_delegate_body_ty(&checker, class, "printString"),
        InferredType::known("String"),
        "`printString -> String => self delegate` should infer String, not Dynamic"
    );
}

/// AC: a `self delegate` method with no return-type annotation still infers
/// `Dynamic` — no regression from the fix.
#[test]
fn actor_backed_self_delegate_without_annotation_still_dynamic() {
    let (module, hierarchy) = build(
        "Actor subclass: HTTPServer native: beamtalk_http_server\n\
         \x20\x20raw => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert!(
        matches!(
            self_delegate_body_ty(&checker, class, "raw"),
            InferredType::Dynamic(_)
        ),
        "unannotated `self delegate` body should stay Dynamic, got: {:?}",
        self_delegate_body_ty(&checker, class, "raw")
    );
}

// ── Object/Value-backed native classes ──────────────────────────────────
//
// `Object>>delegate -> Never` (stdlib sentinel) means a stateless native
// Object's unannotated `self delegate` body pre-fix inferred `Never` (not
// `Dynamic`) — still not the author's declared type. The fix applies
// identically regardless of which sentinel shape the body would otherwise
// have inherited.

#[test]
fn object_backed_self_delegate_trusts_declared_return_type() {
    let (module, hierarchy) = build(
        "Object subclass: Widget native: my_widget\n\
         \x20\x20asList -> List(Integer) => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert_eq!(
        *self_delegate_body_ty(&checker, class, "asList"),
        InferredType::known_with_args("List", vec![InferredType::known("Integer")]),
        "`asList -> List(Integer) => self delegate` should infer List(Integer), not Never/Dynamic"
    );
}

/// AC: no regression on the pre-existing `Object>>delegate -> Never`
/// sentinel fallback when the enclosing method has no return-type
/// annotation of its own.
#[test]
fn object_backed_self_delegate_without_annotation_unchanged() {
    let (module, hierarchy) = build(
        "Object subclass: Widget native: my_widget\n\
         \x20\x20raw => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert_eq!(
        *self_delegate_body_ty(&checker, class, "raw"),
        InferredType::Never,
        "unannotated `self delegate` on an Object-backed class should keep \
         inheriting Object>>delegate's `-> Never` sentinel unchanged"
    );
}

// ── Class-side self delegate ─────────────────────────────────────────────

#[test]
fn class_side_self_delegate_trusts_declared_return_type() {
    let (module, hierarchy) = build(
        "Object subclass: Widget native: my_widget\n\
         \x20\x20class make -> Widget => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert_eq!(
        *class_method_self_delegate_body_ty(&checker, class, "make"),
        InferredType::known("Widget"),
        "class-side `make -> Widget => self delegate` should infer Widget, not Never/Dynamic"
    );
}

// ── `-> Self` on a self-delegate body ────────────────────────────────────

#[test]
fn self_delegate_with_self_return_type_resolves_to_receiver_class() {
    let (module, hierarchy) = build(
        "Object subclass: Widget native: my_widget\n\
         \x20\x20copy -> Self => self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];

    assert_eq!(
        *self_delegate_body_ty(&checker, class, "copy"),
        InferredType::known("Widget"),
        "`copy -> Self => self delegate` should resolve Self to the receiver class Widget"
    );
}

// ── Non-`self delegate` bodies are unaffected ────────────────────────────

/// A method whose body merely *contains* `self delegate` (not exactly, per
/// `is_self_delegate`'s single-statement requirement) must not be touched by
/// this fix — only the true marker pattern is special-cased.
#[test]
fn method_with_extra_statement_after_self_delegate_not_special_cased() {
    let (module, hierarchy) = build(
        "Actor subclass: HTTPServer native: beamtalk_http_server\n\
         \x20\x20port -> Integer =>\n\
         \x20\x20\x20\x20self delegate\n\
         \x20\x20\x20\x20self delegate\n",
    );
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let class = &module.classes[0];
    let method = &class.methods[0];
    assert!(
        !method.is_self_delegate(),
        "a two-statement body is not the `self delegate` marker pattern"
    );
    // Both statements independently infer Dynamic — the fix does not apply
    // since `is_self_delegate()` is false for a multi-statement body.
    for stmt in &method.body {
        assert!(matches!(
            checker.type_map().get(stmt.expression.span()),
            Some(InferredType::Dynamic(_))
        ));
    }
}
