// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! A method declared `-> Never` whose body is a direct class-side send to
//! another `-> Never`-declared method must type-check without `@expect type`
//! (BT-3075).
//!
//! Root cause: the substitution string-resolver
//! (`substitute_return_type_with_self`) — through which
//! `check_class_side_send` routes *every* declared class-method return type —
//! duplicated the union/generic parsing of `resolve_type_name_string` but
//! none of its leaf handling, so a declared `-> Never` came back as a
//! `Known{class_name: "Never"}` pseudo-class instead of
//! `InferredType::Never`. `check_return_type`'s `-> Never` honesty arm then
//! fired with the self-contradictory "declares return type Never, but body
//! returns Never". The same gap covered `Dynamic` (BT-2865's fix never
//! reached this resolver), the `nil`/`true`/`false` keywords, and `Never`
//! nested in substituted generic/union positions. Fixed by merging the two
//! string resolvers into one (`resolve_type_string`).

use super::common::*;
use std::collections::HashMap;

fn never_mismatch_diags(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("declares return type Never"))
        .collect()
}

/// The exact BT-3074 shape: a `-> Never` class method whose sole body
/// expression is `Exception signalKind:class:selector:hint:` (declared
/// `-> Never` on the builtin `Exception`).
#[test]
fn never_class_method_body_is_class_side_send_to_builtin_never_method() {
    let source = "\
Object subclass: Widget\n\
  class boom -> Never =>\n\
    Exception\n\
      signalKind: #instantiation_error\n\
      class: #Widget\n\
      selector: #boom\n\
      hint: \"no\"\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = never_mismatch_diags(&diags);
    assert!(
        mismatches.is_empty(),
        "signalKind:class:selector:hint: is declared -> Never, so the body \
         diverges and the declaration is honest — got: {mismatches:?}"
    );
}

/// Same shape with a user-defined `-> Never` callee: proves the fix isn't
/// specific to builtin-table methods.
#[test]
fn never_class_method_body_is_class_side_send_to_user_never_method() {
    let source = "\
Object subclass: Boom\n\
  class raise: msg :: String -> Never => self error: msg\n\
Object subclass: User\n\
  class fail -> Never => Boom raise: \"nope\"\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = never_mismatch_diags(&diags);
    assert!(
        mismatches.is_empty(),
        "Boom raise: is declared -> Never, so User class>>fail's body \
         diverges — got: {mismatches:?}"
    );
}

/// The honesty check must still fire when a `-> Never` class method's body
/// genuinely returns a value.
#[test]
fn never_class_method_with_value_body_still_warns() {
    let source = "\
Object subclass: Liar\n\
  class nope -> Never => 42\n";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let mismatches = never_mismatch_diags(&diags);
    assert_eq!(
        mismatches.len(),
        1,
        "a -> Never class method returning 42 must still be flagged, got: {diags:?}"
    );
    assert!(
        mismatches[0].message.contains("Integer"),
        "the mismatch should name the actual body type, got: {:?}",
        mismatches[0].message
    );
}

// ---- unit coverage for the merged resolver's substitution entry point ----

/// A declared `-> Never` through the substitution path must resolve to the
/// canonical `InferredType::Never`, not a `Known("Never")` pseudo-class.
#[test]
fn substitute_never_resolves_to_never_variant() {
    let result = TypeChecker::resolve_type_string(
        "Never",
        &HashMap::new(),
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert!(
        matches!(result, InferredType::Never),
        "expected the canonical Never variant, got: {result:?}"
    );
}

/// A declared `-> Dynamic` through the substitution path must resolve to the
/// real `Dynamic` variant (BT-2865's fix, previously missing here).
#[test]
fn substitute_dynamic_resolves_to_dynamic_variant() {
    let result = TypeChecker::resolve_type_string(
        "Dynamic",
        &HashMap::new(),
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert!(
        matches!(result, InferredType::Dynamic(_)),
        "expected the real Dynamic variant, got: {result:?}"
    );
}

/// Keyword spellings must normalise through `resolve_type_keyword` on the
/// substitution path too: `nil` (and `Nil`) map to `UndefinedObject`, so
/// `isNil`/`ifNil:` narrowing keeps working on class-method return values.
#[test]
fn substitute_union_with_nil_keyword_normalises_to_undefined_object() {
    let result = TypeChecker::resolve_type_string(
        "String | nil",
        &HashMap::new(),
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    let InferredType::Union { members, .. } = &result else {
        panic!("expected a Union, got: {result:?}");
    };
    let names: Vec<_> = members
        .iter()
        .filter_map(InferredType::as_known)
        .map(ecow::EcoString::as_str)
        .collect();
    assert!(
        names.contains(&"String") && names.contains(&"UndefinedObject"),
        "nil must normalise to UndefinedObject, got: {names:?}"
    );
}

/// `Never` in a substituted union position is eliminated by `union_of`
/// (the divergent arm contributes nothing): `T | Never` with `T = Integer`
/// collapses to `Integer`.
#[test]
fn substitute_union_with_never_member_collapses() {
    let mut subst: HashMap<EcoString, InferredType> = HashMap::new();
    subst.insert("T".into(), InferredType::known("Integer"));
    let result = TypeChecker::resolve_type_string(
        "T | Never",
        &subst,
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(
        result.as_known().map(ecow::EcoString::as_str),
        Some("Integer"),
        "the Never member is the union identity and must drop out, got: {result:?}"
    );
}

/// `Never` nested as a generic type argument resolves to the canonical
/// variant, not `Known("Never")`.
#[test]
fn substitute_never_nested_in_generic_resolves_to_never_variant() {
    let mut subst: HashMap<EcoString, InferredType> = HashMap::new();
    subst.insert("T".into(), InferredType::known("Integer"));
    let result = TypeChecker::resolve_type_string(
        "GenResult(T, Never)",
        &subst,
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    let InferredType::Known {
        class_name,
        type_args,
        ..
    } = &result
    else {
        panic!("expected a Known generic, got: {result:?}");
    };
    assert_eq!(class_name.as_str(), "GenResult");
    assert_eq!(
        type_args
            .first()
            .and_then(InferredType::as_known)
            .map(ecow::EcoString::as_str),
        Some("Integer")
    );
    assert!(
        matches!(type_args.get(1), Some(InferredType::Never)),
        "nested Never must be the canonical variant, got: {type_args:?}"
    );
}

/// A bare `Nil` return through the substitution path normalises to
/// `UndefinedObject` like every other resolver.
#[test]
fn substitute_bare_nil_normalises_to_undefined_object() {
    let result = TypeChecker::resolve_type_string(
        "Nil",
        &HashMap::new(),
        &HashMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(
        result.as_known().map(ecow::EcoString::as_str),
        Some("UndefinedObject"),
        "Nil must normalise to UndefinedObject, got: {result:?}"
    );
}
