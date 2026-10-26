// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4028: the `match-arm-block` warning end to end. A `match:` arm whose
//! body is a bare zero-parameter block literal answers the Block without
//! running it. Shape-level cases (parens, `matchExhaustive:`, dispatch
//! tables, param blocks, `[ ... ] value`) live in the validator's own unit
//! tests; these pin the `analyse_full` wiring, `@expect` suppression and
//! the `[diagnostics]` key.

use super::*;
use crate::compilation::diagnostics_policy::{
    apply_diagnostics_table, apply_expect_directives, parse_diagnostics_table,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

fn arm_block(src: &str) -> Vec<Diagnostic> {
    analyse(&parse_module(src))
        .diagnostics
        .into_iter()
        .filter(|d| d.category == Some(DiagnosticCategory::MatchArmBlock))
        .collect()
}

#[test]
fn fires_through_analyse_full_in_actor_class_value_and_script() {
    // Actor (`Server handleInfo:`, the issue's repro).
    let d = arm_block(
        "Server subclass: Probe\n  handleInfo: msg =>\n    msg match: [\n      #tick -> [Erlang erlang display: msg];\n      _ -> nil\n    ]",
    );
    assert_eq!(d.len(), 1, "{d:?}");
    assert_eq!(d[0].severity, Severity::Warning);

    // Class method.
    let d = arm_block(
        "Object subclass: Shapes\n  class describe: x => x match: [#circle -> [self draw]; _ -> nil]",
    );
    assert_eq!(d.len(), 1, "{d:?}");

    // Value method.
    let d = arm_block(
        "Value subclass: Point\n  field: x\n  describe => self.x match: [0 -> [self origin]; _ -> nil]",
    );
    assert_eq!(d.len(), 1, "{d:?}");

    // Standalone definition.
    let d = arm_block(
        "Object subclass: Shapes\n\nShapes >> describe: x => x match: [#circle -> [x]; _ -> nil]",
    );
    assert_eq!(d.len(), 1, "{d:?}");

    // Script top level.
    let d = arm_block("x := 1\nx match: [1 -> [2 + 3]; _ -> 0]");
    assert_eq!(d.len(), 1, "{d:?}");
}

#[test]
fn dispatch_table_stays_silent_through_analyse_full() {
    let d =
        arm_block("Object subclass: Shapes\n  describe: x => x match: [1 -> [2 + 3]; _ -> [0]]");
    assert!(d.is_empty(), "{d:?}");
}

#[test]
fn expect_match_arm_block_suppresses_the_warning() {
    let src = "Object subclass: Shapes\n  describe: x =>\n    @expect match_arm_block\n    x match: [1 -> [2 + 3]; _ -> 0]\n";
    let module = parse_module(src);
    let mut diags = analyse(&module).diagnostics;
    apply_expect_directives(&module, &mut diags);
    assert!(
        !diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::MatchArmBlock)),
        "{diags:?}"
    );
    assert!(
        !diags.iter().any(|d| d.message.contains("stale @expect")),
        "the directive matched: {diags:?}"
    );
}

#[test]
fn diagnostics_table_off_mutes_the_warning() {
    let src = "Object subclass: Shapes\n  describe: x => x match: [1 -> [2 + 3]; _ -> 0]";
    let diags = analyse(&parse_module(src)).diagnostics;
    assert!(
        diags
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::MatchArmBlock)),
        "setup: expected a finding, got {diags:?}"
    );
    let value: toml::Value = toml::from_str("[diagnostics]\nmatch-arm-block = \"off\"\n").unwrap();
    let table = parse_diagnostics_table(value.get("diagnostics")).unwrap();
    let muted = apply_diagnostics_table(diags, &table);
    assert!(
        !muted
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::MatchArmBlock)),
        "{muted:?}"
    );
}
