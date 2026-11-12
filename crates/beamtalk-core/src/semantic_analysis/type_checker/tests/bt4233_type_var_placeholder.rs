// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4233: one shared `is_type_var_placeholder` rule. A single-letter name
//! is a symbolic type variable only when it is not a real class; a user
//! class named `E` is judged nominally everywhere.

use super::common::*;
use crate::semantic_analysis::type_checker::is_type_var_placeholder;

fn mismatches(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            d.message.contains("Type mismatch")
                || d.message.contains("type mismatch")
                || d.message.contains("but body returns")
        })
        .collect()
}

#[test]
fn placeholder_predicate_excludes_real_classes() {
    let module = parse_source("Object subclass: E\n");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    assert!(!is_type_var_placeholder("E", &hierarchy));
    assert!(is_type_var_placeholder("T", &hierarchy));
    assert!(!is_type_var_placeholder("Integer", &hierarchy));
    assert!(!is_type_var_placeholder("Ex", &hierarchy));
}

/// A generic type arg naming a real single-letter class is compared
/// nominally: `List(Integer)` does not satisfy `List(E)` when `E` is a class.
#[test]
fn real_single_letter_class_type_arg_is_judged_nominally() {
    let diags = diags_with_expect(
        r"
typed Object subclass: E

typed Object subclass: M
  run -> List(E) =>
    xs :: List(Integer) := #(1, 2)
    xs
",
    );
    assert_eq!(mismatches(&diags).len(), 1, "got: {diags:#?}");
}

/// The same shape with an unbound type variable stays symbolic: no warning.
#[test]
fn type_variable_type_arg_stays_symbolic() {
    let diags = diags_with_expect(
        r"
typed Object subclass: M
  run: xs :: List(Integer) -> List(E) => xs
",
    );
    assert!(mismatches(&diags).is_empty(), "got: {diags:#?}");
}

/// The string-level type-args comparison (`is_type_compatible` on
/// `List(Integer)` vs `List(E)`) uses the same predicate as
/// `type_args_compatible`: a real class `E` is judged nominally, an unbound
/// `E` stays symbolic.
#[test]
fn string_level_type_args_use_the_shared_predicate() {
    use crate::semantic_analysis::type_checker::TypeChecker;
    let module = parse_source("Object subclass: E\n");
    let with_class = ClassHierarchy::build(&module).0.unwrap();
    let without_class = ClassHierarchy::with_builtins();
    let actual: EcoString = "List(Integer)".into();
    let expected: EcoString = "List(E)".into();
    assert!(!TypeChecker::is_type_compatible(
        &actual,
        &expected,
        &with_class
    ));
    assert!(TypeChecker::is_type_compatible(
        &actual,
        &expected,
        &without_class
    ));
}

/// BT-4245: the substituting resolver consults the hierarchy, so an unbound
/// real class `E` resolves to `Known(E)` while an unbound placeholder `T`
/// still collapses to `Dynamic`.
#[test]
fn substitution_resolver_keeps_real_single_letter_class() {
    use crate::semantic_analysis::class_hierarchy::DeclaredType;
    use crate::semantic_analysis::type_checker::TypeStringContext;
    use crate::semantic_analysis::type_checker::type_resolver::{
        SubstitutionMap, resolve_declared_type_in,
    };
    let module = parse_source("Object subclass: E\n");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let subst = SubstitutionMap::new();
    let resolve = |name: &str| {
        resolve_declared_type_in(
            &DeclaredType::Simple(name.into()),
            &subst,
            None,
            None,
            TypeStringContext::Substitution,
            Some(&hierarchy),
        )
    };
    assert_eq!(resolve("E"), InferredType::known("E"));
    assert!(matches!(resolve("T"), InferredType::Dynamic(_)));
}

/// BT-4245: a method declared `-> E` reached through a substituting send
/// keeps `E`, so a DNU on the result is reported against `E`.
#[test]
fn substituting_send_keeps_real_single_letter_class_for_dnu() {
    let diags = diags_with_expect(
        r"
typed Object subclass: E

typed Object subclass: Maker(T)
  make -> E => E new

typed Object subclass: Use
  run -> Nil =>
    m :: Maker(Integer) := Maker new
    m make bogusSelector
    nil
",
    );
    assert!(
        diags.iter().any(|d| d.message.contains("bogusSelector")
            && d.message
                .split(|c: char| !c.is_alphanumeric())
                .any(|w| w == "E")),
        "expected DNU against E, got: {diags:#?}"
    );
}
