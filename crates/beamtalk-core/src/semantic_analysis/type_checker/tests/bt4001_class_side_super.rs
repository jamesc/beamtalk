// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4001: a `super` send in a class method resolves against the
//! superclass's *class* methods (the metaclass chain), as codegen's
//! `class_self_dispatch/4` does — not against its instance methods.

use super::common::*;

fn diagnostics_mentioning(source: &str, selector: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    run_with_expect(&module, &hierarchy)
        .into_iter()
        .filter(|d| d.message.contains(selector))
        .collect()
}

#[test]
fn class_side_super_to_superclass_class_method_is_clean() {
    for kind in ["Actor", "Object"] {
        let source = format!(
            "{kind} subclass: Base
  class peek: x => x + 100
Base subclass: C
  class run => super peek: 1
  class runAll => #(1, 2) collect: [:x | super peek: x]
"
        );
        let diags = diagnostics_mentioning(&source, "peek:");
        assert!(diags.is_empty(), "{kind}: got: {diags:#?}");
    }
}

#[test]
fn class_side_super_to_instance_only_selector_is_not_understood() {
    // The issue's repro: `peek:` exists only instance-side on the superclass,
    // so a class-side `super peek:` would raise DNU at runtime.
    for kind in ["Actor", "Object"] {
        let source = format!(
            "{kind} subclass: Base
  peek: x => x + 100
Base subclass: C
  class run => super peek: 1
  class runAll => #(1, 2) collect: [:x | super peek: x]
"
        );
        let diags = diagnostics_mentioning(&source, "peek:");
        assert_eq!(diags.len(), 2, "{kind}: got: {diags:#?}");
    }
}

#[test]
fn class_side_super_skips_the_defining_class() {
    // `super` starts at the superclass: a class method only the defining
    // class has is not reachable through it.
    let diags = diagnostics_mentioning(
        "Object subclass: Base
Base subclass: C
  class peek: x => x
  class run => super peek: 1
",
        "peek:",
    );
    assert_eq!(diags.len(), 1, "got: {diags:#?}");
}

#[test]
fn class_side_extension_super_resolves_against_the_target_superclass() {
    // A class-side extension's `super` starts at the *target's* superclass,
    // as codegen keys it (`class_var_home`), whatever class hosts the file.
    for host in ["", "Object subclass: Host\n  foo => 1\n"] {
        let clean = format!(
            "Object subclass: Base
  class peek: x => x + 100
Base subclass: C
{host}C class >> run => super peek: 1
"
        );
        let diags = diagnostics_mentioning(&clean, "peek:");
        assert!(diags.is_empty(), "host {host:?}: got: {diags:#?}");

        let instance_only = format!(
            "Object subclass: Base
  peek: x => x + 100
Base subclass: C
{host}C class >> run => super peek: 1
"
        );
        let diags = diagnostics_mentioning(&instance_only, "peek:");
        assert_eq!(diags.len(), 1, "host {host:?}: got: {diags:#?}");
    }
}

#[test]
fn instance_side_super_still_resolves_instance_methods() {
    let diags = diagnostics_mentioning(
        "Object subclass: Base
  peek: x => x + 100
Base subclass: C
  run => super peek: 1
",
        "peek:",
    );
    assert!(diags.is_empty(), "got: {diags:#?}");
}
