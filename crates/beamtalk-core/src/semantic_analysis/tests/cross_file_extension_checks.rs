// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4923: checks that must also run on a cross-file `Foo >> sel` extension.
//!
//! A same-file `Foo >> sel` is folded into `Foo`'s body before the validators
//! run, so only an extension in a *different* file than its class reaches the
//! standalone-method path of a validator. These tests hand `analyse_full` the
//! class as a pre-loaded declaration and the extension as the module under
//! analysis.

use super::*;
use crate::source_analysis::Diagnostic;

fn cross_file(class_src: &str, ext_src: &str) -> Vec<Diagnostic> {
    let ctx = AnalysisContext::default()
        .with_current_package("app")
        .with_pre_loaded_classes(declared_by("app", class_src));
    analyse_full(&parse_bt(ext_src), ctx).diagnostics
}

// ---- supervision policy on `MySup >> childSpecs` ----

const SUPERVISED: &str = "Actor subclass: Worker\n  go => nil\n\
                          Supervisor subclass: MySup\n  childSpecs => #()\n";

#[test]
fn cross_file_child_specs_without_supervision_policy_warns() {
    let d = cross_file(SUPERVISED, "MySup >> childSpecs => #(Worker)\n");
    let hits: Vec<_> = d
        .iter()
        .filter(|d| d.message.contains("no explicit `class supervisionPolicy`"))
        .collect();
    assert_eq!(hits.len(), 1, "{d:?}");
    assert!(hits[0].message.contains("`Worker`"), "{}", hits[0].message);
}

#[test]
fn cross_file_child_specs_with_supervision_policy_is_silent() {
    let class_src = "Actor subclass: Worker\n  class supervisionPolicy -> Symbol => #permanent\n  go => nil\n\
                     Supervisor subclass: MySup\n  childSpecs => #()\n";
    let d = cross_file(class_src, "MySup >> childSpecs => #(Worker)\n");
    assert!(
        !d.iter()
            .any(|d| d.message.contains("no explicit `class supervisionPolicy`")),
        "{d:?}"
    );
}

#[test]
fn cross_file_class_side_child_specs_is_not_checked() {
    // `childSpecs` is an instance-side selector; a class-side definition is a
    // different method.
    let d = cross_file(SUPERVISED, "MySup class >> childSpecs => #(Worker)\n");
    assert!(
        !d.iter()
            .any(|d| d.message.contains("no explicit `class supervisionPolicy`")),
        "{d:?}"
    );
}

// ---- internal-method shadowing (W0401) on `Sub >> helper` ----

const INTERNAL_BASE: &str =
    "Object subclass: Base\n  internal helper => 1\nBase subclass: Sub\n  go => 2\n";

#[test]
fn cross_file_extension_shadowing_internal_method_warns() {
    let d = cross_file(INTERNAL_BASE, "Sub >> helper => 2\n");
    let hits: Vec<_> = d
        .iter()
        .filter(|d| d.message.contains("shadows internal method"))
        .collect();
    assert_eq!(hits.len(), 1, "{d:?}");
    assert!(
        hits[0].message.contains("'helper' on 'Sub'"),
        "{}",
        hits[0].message
    );
}

#[test]
fn cross_file_class_side_extension_shadowing_internal_class_method_warns() {
    let class_src =
        "Object subclass: Base\n  class internal build => 1\nBase subclass: Sub\n  go => 2\n";
    let d = cross_file(class_src, "Sub class >> build => 2\n");
    let hits: Vec<_> = d
        .iter()
        .filter(|d| d.message.contains("shadows internal class method"))
        .collect();
    assert_eq!(hits.len(), 1, "{d:?}");
}

#[test]
fn cross_file_extension_not_shadowing_internal_method_is_silent() {
    let d = cross_file(INTERNAL_BASE, "Sub >> other => 2\n");
    assert!(
        !d.iter().any(|d| d.message.contains("shadows internal")),
        "{d:?}"
    );
}

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

// ---- native-delegate nested-String hint on a `native:` Object extension ----
//
// The hint says "Strings nested in parameter `p` reach <backing>:<sel>/n as
// tagged Beamtalk Strings", which is only true where codegen lowers
// `self delegate` through the class's backing module. A same-file extension is
// folded into the class body, so it is lowered that way. A cross-file extension
// is compiled as a foreign extension fun (`gen_server/extensions.rs`), whose
// body is lowered like any value-type body: `self delegate` there is a plain
// send, never an FFI call, so the hint would be false.

const NATIVE_NAMES: &str = "Object subclass: Names native: my_names\n  size => 0\n";

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

#[test]
fn cross_file_native_delegate_extension_is_not_hinted() {
    let d = cross_file(
        NATIVE_NAMES,
        "Names >> has: names :: List(String) => self delegate\n",
    );
    assert_eq!(nested_string_hints(&d), 0, "{d:?}");
}
