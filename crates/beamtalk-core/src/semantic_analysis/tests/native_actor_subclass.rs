// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4961: a non-abstract actor that descends from a `native:` actor must
//! declare its own `native:` backing module.
//!
//! A `native:` actor compiles to a facade module with no `init/1`. A subclass
//! without `native:` compiles to a full `gen_server` whose `init/1` calls its
//! superclass's `init/1`, so `Sub spawn` died on an undefined function.
//! `docs/beamtalk-native-erlang.md` § Inheritance says the backing module is
//! not inherited; this is the compile-time check for it.

use super::*;
use crate::source_analysis::{Diagnostic, Severity};

const NATIVE_ACTOR: &str = "Actor subclass: Counter native: counter_backing\n  \
                            get -> Integer => self delegate\n";

fn same_package(src: &str) -> Vec<Diagnostic> {
    analyse_full(&parse_bt(src), AnalysisContext::default()).diagnostics
}

/// `parent_src` is a dependency's class, `src` the module under analysis.
fn from_dependency(parent_src: &str, src: &str) -> Vec<Diagnostic> {
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("dep", parent_src));
    analyse_full(&parse_bt(src), ctx).diagnostics
}

fn missing_native(d: &[Diagnostic]) -> Vec<&Diagnostic> {
    d.iter()
        .filter(|d| d.message.contains("must declare its own `native:`"))
        .collect()
}

fn assert_one_error(d: &[Diagnostic], parent: &str, sub: &str) {
    let hits = missing_native(d);
    assert_eq!(hits.len(), 1, "{d:?}");
    assert_eq!(hits[0].severity, Severity::Error);
    assert!(
        hits[0].message.contains(&format!("`{sub}`"))
            && hits[0].message.contains(&format!("`{parent}`")),
        "{}",
        hits[0].message
    );
    assert!(
        hits[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("native:")),
        "{:?}",
        hits[0].hint
    );
}

// ---- forms that cannot work: errors ----

#[test]
fn same_package_subclass_without_native_is_an_error() {
    let d = same_package(&format!(
        "{NATIVE_ACTOR}Counter subclass: Sub\n  extra => 1\n"
    ));
    assert_one_error(&d, "Counter", "Sub");
}

#[test]
fn subclass_of_a_dependency_native_actor_is_an_error() {
    let d = from_dependency(NATIVE_ACTOR, "Counter subclass: Sub\n  extra => 1\n");
    assert_one_error(&d, "Counter", "Sub");
}

#[test]
fn grandchild_of_a_native_actor_is_an_error() {
    // `Mid` redeclares `native:`, so it is fine; `Leaf` inherits Mid's facade.
    let src = format!(
        "{NATIVE_ACTOR}Counter subclass: Mid native: mid_backing\n  more -> Integer => self delegate\n\
         Mid subclass: Leaf\n  extra => 1\n"
    );
    assert_one_error(&same_package(&src), "Mid", "Leaf");
}

#[test]
fn concrete_child_of_an_abstract_non_native_middle_class_is_an_error() {
    let src = format!(
        "{NATIVE_ACTOR}abstract Counter subclass: Mid\n  extra => 1\nMid subclass: Leaf\n  more => 2\n"
    );
    assert_one_error(&same_package(&src), "Mid", "Leaf");
}

// ---- forms that work (or are not decided here): silent ----

#[test]
fn subclass_that_redeclares_native_is_silent() {
    let d = same_package(&format!(
        "{NATIVE_ACTOR}Counter subclass: Sub native: other_backing\n  more -> Integer => self delegate\n"
    ));
    assert!(missing_native(&d).is_empty(), "{d:?}");
    let d = from_dependency(
        NATIVE_ACTOR,
        "Counter subclass: Sub native: other_backing\n  more -> Integer => self delegate\n",
    );
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn native_actor_itself_and_plain_actor_subclasses_are_silent() {
    let d = same_package(&format!(
        "{NATIVE_ACTOR}Actor subclass: Plain\n  go => 1\nPlain subclass: PlainChild\n  more => 2\n"
    ));
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn abstract_subclass_of_a_native_actor_is_not_reported() {
    // It can never be spawned itself, and a concrete child may redeclare `native:`.
    let d = same_package(&format!(
        "{NATIVE_ACTOR}abstract Counter subclass: Sub\n  extra => 1\n"
    ));
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn abstract_non_native_middle_class_with_a_native_redeclaring_child_is_silent() {
    // The abstract class is never spawned and the concrete child has its own facade.
    let src = format!(
        "{NATIVE_ACTOR}abstract Counter subclass: Mid\n  extra => 1\n\
         Mid subclass: Leaf native: leaf_backing\n  more -> Integer => self delegate\n"
    );
    let d = same_package(&src);
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

// ---- BT-4964: the same rule for `Value` / `Object` native classes ----

const NATIVE_VALUE: &str =
    "Value subclass: NVal native: nval_backing\n  size -> Integer => self delegate\n";
const NATIVE_OBJECT: &str =
    "Object subclass: NObj native: nobj_backing\n  size -> Integer => self delegate\n";

#[test]
fn same_package_value_and_object_subclasses_of_native_classes_are_errors() {
    let d = same_package(&format!(
        "{NATIVE_VALUE}NVal subclass: SubVal\n  extra => 1\n"
    ));
    assert_one_error(&d, "NVal", "SubVal");
    let d = same_package(&format!(
        "{NATIVE_OBJECT}NObj subclass: SubObj\n  extra => 1\n"
    ));
    assert_one_error(&d, "NObj", "SubObj");
}

#[test]
fn value_subclass_error_suggests_wrapping_in_a_field() {
    let d = same_package(&format!(
        "{NATIVE_VALUE}NVal subclass: SubVal\n  extra => 1\n"
    ));
    let hint = missing_native(&d)[0].hint.clone().unwrap_or_default();
    assert!(hint.contains("field"), "{hint}");
}

#[test]
fn value_subclass_of_a_dependency_native_class_is_an_error() {
    let d = from_dependency(NATIVE_VALUE, "NVal subclass: SubVal\n  extra => 1\n");
    assert_one_error(&d, "NVal", "SubVal");
    let d = from_dependency(NATIVE_OBJECT, "NObj subclass: SubObj\n  extra => 1\n");
    assert_one_error(&d, "NObj", "SubObj");
}

#[test]
fn value_subclass_that_redeclares_native_is_silent() {
    let sub = "NVal subclass: SubVal native: other_backing\n  more -> Integer => self delegate\n";
    let d = same_package(&format!("{NATIVE_VALUE}{sub}"));
    assert!(missing_native(&d).is_empty(), "{d:?}");
    let d = from_dependency(NATIVE_VALUE, sub);
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn abstract_value_subclass_is_silent_but_its_concrete_child_is_an_error() {
    let abs = format!("{NATIVE_VALUE}abstract NVal subclass: Mid\n  extra => 1\n");
    let d = same_package(&abs);
    assert!(missing_native(&d).is_empty(), "{d:?}");
    let d = same_package(&format!("{abs}Mid subclass: Leaf\n  more => 2\n"));
    assert_one_error(&d, "Mid", "Leaf");
    let d = same_package(&format!(
        "{abs}Mid subclass: Leaf native: leaf_backing\n  more -> Integer => self delegate\n"
    ));
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn plain_value_and_object_subclasses_are_silent() {
    let d = same_package(
        "Value subclass: PV\n  a => 1\nPV subclass: PV2\n  b => 2\nObject subclass: PO\n  c => 3\n",
    );
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

// ---- BT-4964: the term-shape built-in exemption (`String` under `Binary`) ----

const STRING_UNDER_BINARY: &str = "sealed typed Binary subclass: String\n  size -> Integer => 1\n";

fn stdlib_mode(src: &str) -> Vec<Diagnostic> {
    let ctx = AnalysisContext {
        stdlib_mode: true,
        ..AnalysisContext::default()
    };
    analyse_full(&parse_bt(src), ctx).diagnostics
}

#[test]
fn builtin_string_under_native_binary_is_exempt_in_stdlib_mode() {
    let d = stdlib_mode(STRING_UNDER_BINARY);
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn user_class_named_string_outside_the_stdlib_is_not_exempt() {
    // Same name as the built-in, but not the stdlib build: still checked.
    let d = same_package(&format!(
        "{NATIVE_VALUE}NVal subclass: String\n  extra => 1\n"
    ));
    assert_one_error(&d, "NVal", "String");
    // Under the sealed `Binary` it is still an error: the seal error.
    let d = same_package(STRING_UNDER_BINARY);
    assert!(missing_native(&d).is_empty(), "{d:?}");
    assert_eq!(sealed_errors(&d).len(), 1, "{d:?}");
}

#[test]
fn exemption_does_not_extend_to_other_classes_in_stdlib_mode() {
    let src = format!("{NATIVE_VALUE}NVal subclass: SubVal\n  extra => 1\n");
    assert_one_error(&stdlib_mode(&src), "NVal", "SubVal");
}

fn sealed_errors(d: &[Diagnostic]) -> Vec<&Diagnostic> {
    d.iter()
        .filter(|d| d.message.contains("Cannot subclass sealed class"))
        .collect()
}

#[test]
fn subclass_of_a_sealed_native_class_gets_only_the_seal_error() {
    // The seal error is the one diagnostic; a second "declare your own
    // `native:`" error would suggest a form the seal forbids.
    let parent =
        "sealed Value subclass: SVal native: sval_backing\n  size -> Integer => self delegate\n";
    let d = same_package(&format!("{parent}SVal subclass: SubSVal\n  extra => 1\n"));
    assert_eq!(sealed_errors(&d).len(), 1, "{d:?}");
    assert!(missing_native(&d).is_empty(), "{d:?}");
}

#[test]
fn subclass_of_a_sealed_native_class_from_a_dependency_is_still_an_error() {
    // The hierarchy build emits no seal error for a dependency's class, so the
    // native-subclass error must still fire, and it is the only error.
    let parent =
        "sealed Value subclass: SVal native: sval_backing\n  size -> Integer => self delegate\n";
    let d = from_dependency(parent, "SVal subclass: SubSVal\n  extra => 1\n");
    assert!(sealed_errors(&d).is_empty(), "{d:?}");
    assert_eq!(missing_native(&d).len(), 1, "{d:?}");
}

#[test]
fn user_sealed_subclass_of_a_non_sealed_native_class_is_an_error() {
    // `sealed` on the subclass is a keyword a user can write; it grants no exemption.
    let d = same_package(&format!(
        "{NATIVE_VALUE}sealed NVal subclass: SubVal\n  extra => 1\n"
    ));
    assert_one_error(&d, "NVal", "SubVal");
    let d = same_package("sealed Queue subclass: MyQ\n  extra -> Integer => 42\n");
    assert_one_error(&d, "Queue", "MyQ");
}

#[test]
fn dependency_class_without_the_native_flag_is_not_checked() {
    // A `.beam` built before BT-4930 decodes as non-native: no error.
    let mut infos = declared_by("dep", NATIVE_ACTOR);
    for info in &mut infos {
        info.is_native = false;
    }
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(infos);
    let d = analyse_full(&parse_bt("Counter subclass: Sub\n  extra => 1\n"), ctx).diagnostics;
    assert!(missing_native(&d).is_empty(), "{d:?}");
}
