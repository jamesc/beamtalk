// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `type_args_compatible` no longer treats a nested `Union` type-arg as
//! automatically compatible (BT-2847).
//!
//! Before this fix, any non-`Known` shape (`Dynamic`, `Union`, `Never`)
//! nested inside a generic's type arguments short-circuited to `true` on the
//! assumption a "broader checker" handled it elsewhere. That premise didn't
//! hold for `Union`: `check_union_body_return_type` (BT-2829) only validates
//! a *top-level* Union body type, so a method declared `-> Array(String)`
//! whose body infers `Array(String | Nil)` passed silently.
//!
//! Per the spec decision (issue comment): only the `Union` half changes —
//! a nested `Union` type-arg now recurses with the same
//! `classify_union_members`-style check used at the top level (BT-1832).
//! Nested `Dynamic`/`Never` stay permissive, unchanged.

use super::common::*;

/// Build a hierarchy containing a single empty `typed` class named
/// `class_name`, on top of the builtins. Mirrors the pattern in
/// `bt2829_return_type_union_dynamic.rs`.
fn typed_class_hierarchy(class_name: &str) -> ClassHierarchy {
    let source = format!("typed Object subclass: {class_name}\n");
    let module = parse_source(&source);
    ClassHierarchy::build(&module).0.unwrap()
}

/// Build a minimal unary method with the given declared return type
/// annotation. `body` is irrelevant — `check_return_type` is called
/// directly with a hand-constructed `body_type`, bypassing inference.
fn method_with_return_type(return_type: TypeAnnotation) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary("probe".into()),
        parameters: vec![],
        body: vec![bare(int_lit(0))],
        return_type: Some(return_type),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    }
}

fn array_of(element: TypeAnnotation) -> TypeAnnotation {
    TypeAnnotation::Generic {
        base: ident("Array"),
        parameters: vec![element],
        span: span(),
    }
}

fn type_mismatch_diagnostics(checker: &TypeChecker) -> Vec<&Diagnostic> {
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect()
}

/// AC: `Array(String)` declared vs. a body inferring `Array(String | Nil)`
/// now produces a diagnostic — `Nil` isn't assignable to the expected
/// `String` element type. This is the exact repro from the issue (nested
/// `List(String)` vs. `List(String | Nil)`, using `Array` to match the other
/// BT-2022-family tests in this suite).
#[test]
fn bt2847_nested_union_incompatible_member_warns() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(array_of(TypeAnnotation::Simple(ident("String"))));
    let body_type = InferredType::known_with_args(
        "Array",
        vec![InferredType::union_of(&[
            InferredType::known("String"),
            InferredType::known("Nil"),
        ])],
    );

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    let warnings = type_mismatch_diagnostics(&checker);
    assert_eq!(
        warnings.len(),
        1,
        "Array(String | Nil) body should warn against declared Array(String): {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("Array(String)"));
}

/// AC: no regression — a nested `Union` type-arg whose every member is
/// compatible with the expected inner type produces no diagnostic.
/// `String | Symbol` are both compatible with the declared `Object` element
/// type via the class hierarchy.
#[test]
fn bt2847_nested_union_all_compatible_no_diagnostic() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(array_of(TypeAnnotation::Simple(ident("Object"))));
    let body_type = InferredType::known_with_args(
        "Array",
        vec![InferredType::union_of(&[
            InferredType::known("String"),
            InferredType::known("Symbol"),
        ])],
    );

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Array(String | Symbol) body should not warn against declared Array(Object): {:?}",
        checker.diagnostics()
    );
}

/// AC: nested `Dynamic` type args continue to be treated permissively,
/// unchanged — the spec decision keeps `Dynamic` as the checker's universal
/// escape hatch (generics are erased at runtime).
#[test]
fn bt2847_nested_dynamic_stays_permissive() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(array_of(TypeAnnotation::Simple(ident("String"))));
    let body_type =
        InferredType::known_with_args("Array", vec![InferredType::Dynamic(DynamicReason::Unknown)]);

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Array(Dynamic) body should stay permissive against declared Array(String): {:?}",
        checker.diagnostics()
    );
}

/// Conservative skip: a nested `Union` type-arg with a non-`Known` member
/// (nested `Dynamic`) can't be reliably compared, so the whole union is
/// skipped — mirrors `classify_union_members`'s (BT-1832) conservative
/// fallback at the top level.
#[test]
fn bt2847_nested_union_with_dynamic_member_skips() {
    let hierarchy = typed_class_hierarchy("Probe");
    let method = method_with_return_type(array_of(TypeAnnotation::Simple(ident("String"))));
    let body_type = InferredType::known_with_args(
        "Array",
        vec![InferredType::Union {
            members: vec![
                InferredType::known("Integer"),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
            provenance: TypeProvenance::Inferred(span()),
        }],
    );

    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("Probe"), &hierarchy);

    assert!(
        type_mismatch_diagnostics(&checker).is_empty(),
        "Array(Integer | <dynamic>) body should be skipped conservatively: {:?}",
        checker.diagnostics()
    );
}
