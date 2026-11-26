// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Checks on a same-file `Foo >> sel` extension (BT-4923, BT-4721).
//!
//! A same-file `Foo >> sel` is folded into `Foo`'s body before the validators
//! run, so it must be checked exactly once, like a body method. A cross-file
//! `Foo >> sel` is a compile error (ADR 0152, `reject_cross_file_extensions`)
//! and gets no further checks, so the cross-file cases that used to live here
//! were removed (BT-5184).

use super::*;
use crate::source_analysis::Diagnostic;

// ---- validators see a folded extension once ----

#[test]
fn same_file_extension_shadowing_internal_method_warns_once() {
    // A same-file extension is folded into the class body, so the inline path
    // reports it; the standalone path must not report it a second time.
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by(
            "app",
            "Object subclass: Base\n  internal helper => 1\n",
        ));
    let src = "Base subclass: Sub\n  go => 2\nSub >> helper => 2\n";
    let d = analyse_full(&parse_bt(src), ctx).diagnostics;
    let hits = d
        .iter()
        .filter(|d| d.message.contains("shadows internal method"))
        .count();
    assert_eq!(hits, 1, "{d:?}");
}

#[test]
fn same_file_child_specs_extension_warns_once() {
    let ctx = AnalysisContext::default()
        .with_pre_loaded_classes(declared_by("app", "Actor subclass: Worker\n  go => nil\n"));
    let src = "Supervisor subclass: MySup\n  go => nil\nMySup >> childSpecs => #(Worker)\n";
    let d = analyse_full(&parse_bt(src), ctx).diagnostics;
    let hits = d
        .iter()
        .filter(|d| d.message.contains("no explicit `class supervisionPolicy`"))
        .count();
    assert_eq!(hits, 1, "{d:?}");
}

// ---- the two `is_class_method` flags on a standalone definition ----

fn flags(src: &str) -> Vec<(bool, bool)> {
    let (module, _) = crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    module
        .method_definitions
        .iter()
        .map(|s| (s.is_class_method, s.method.is_class_method))
        .collect()
}

#[test]
fn only_the_wrapper_flag_is_set_on_a_parsed_standalone_definition() {
    // The parser sets `StandaloneMethodDefinition::is_class_method` from the
    // `Foo class >> sel` form and never sets the inner
    // `MethodDefinition::is_class_method`, and `Foo >> class sel` is not a
    // standalone definition at all. So the two flags cannot disagree, and the
    // wrapper flag alone (what `Module::methods_with_owner` reads) is
    // authoritative.
    assert_eq!(flags("Foo >> bar => 1\n"), [(false, false)]);
    assert_eq!(flags("Foo class >> bar => 1\n"), [(true, false)]);
    assert!(flags("Foo >> class bar => 1\n").is_empty());
}

// ---- native-delegate nested-String hint on a same-file extension ----

fn nested_string_hints(d: &[Diagnostic]) -> usize {
    d.iter()
        .filter(|d| d.category == Some(crate::source_analysis::DiagnosticCategory::FfiNestedString))
        .count()
}

#[test]
fn same_file_native_delegate_extension_is_hinted() {
    let src = "Object subclass: Names native: my_names\n  size => 0\n\
               Names >> has: names :: List(String) => self delegate\n";
    let d = analyse_full(&parse_bt(src), AnalysisContext::default()).diagnostics;
    assert_eq!(nested_string_hints(&d), 1, "{d:?}");
}

// ---- `self delegate` on a `native:` class ----

const NATIVE_VALUE: &str = "Value subclass: Host native: host_backing\n  \
                            ping -> Integer => self delegate\n";

fn has(d: &[Diagnostic], needle: &str) -> bool {
    d.iter().any(|d| d.message.contains(needle))
}

#[test]
fn same_file_self_delegate_keeps_the_native_checks() {
    // Same-file is folded into the class and codegen lowers it through the
    // backing module, so the native delegate checks run on it.
    let src = format!("{NATIVE_VALUE}Host >> cross => self delegate\n");
    let d = analyse_full(&parse_bt(&src), AnalysisContext::default()).diagnostics;
    assert!(
        has(
            &d,
            "Method `cross` uses `self delegate` without a return type"
        ),
        "{d:?}"
    );

    let src = format!("{NATIVE_VALUE}Host >> case: x :: Integer -> Integer => self delegate\n");
    let d = analyse_full(&parse_bt(&src), AnalysisContext::default()).diagnostics;
    assert!(has(&d, "lowers to the Erlang function `case`"), "{d:?}");
}

#[test]
fn binary_only_native_class_carries_the_native_flag() {
    // `decode.rs` reads the native flag from `__beamtalk_meta`, so a class
    // that arrives from a compiled dependency is native like one in source.
    assert!(declared_by("app", NATIVE_VALUE).iter().any(|i| i.is_native));
}
