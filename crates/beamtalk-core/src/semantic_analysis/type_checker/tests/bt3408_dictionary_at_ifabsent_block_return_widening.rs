// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3408: `Dictionary(K, V)>>at:ifAbsent:` must widen its result type
//! through the absent-block's own return type (`V | T`) rather than
//! requiring the block to return exactly `V`.
//!
//! Before this fix, `at:ifAbsent:` declared its absent-block parameter as
//! `Block(V)` and its own return type as bare `V` — so `dict at: k ifAbsent:
//! [nil]` failed to type-check whenever `V` didn't already include `Nil`,
//! forcing the `includesKey:`-then-`at:` double-lookup workaround (BT-3408)
//! described in the issue. The fix widens the block parameter to a fresh
//! method-local type param `Block(T)` and the return type to `V | T`,
//! mirroring `Behaviour>>removeSelector:ifAbsent:`'s established shape.
//!
//! These are full-pipeline tests (`check_module_with_protocols`, not a
//! direct `check_argument_types` unit call) because the bug's fix lives
//! entirely in the stdlib source declaration (`stdlib/src/dictionary.bt`) —
//! the type-checker's generic-type-param machinery (`check_argument_types`
//! and the separate `check_generic_variance_in_module` pass) already
//! handled a `Block(T)`-shaped parameter correctly; only the declared
//! signature needed to change. Running the real pipeline (both passes) is
//! what actually proves the fix, the same way `bt2949`'s `run_with_protocols`
//! does for its own generic-argument-checking regressions.

use super::common::*;

fn run_with_protocols(source: &str) -> Vec<Diagnostic> {
    let module = parse_source(source);
    let mut hierarchy = ClassHierarchy::with_builtins();
    let user_hierarchy = ClassHierarchy::build(&module).0.unwrap();
    hierarchy.merge(&user_hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(
        &module,
        &hierarchy,
        &crate::semantic_analysis::protocol_registry::ProtocolRegistry::new(),
    );
    checker.take_diagnostics()
}

fn assert_no_expects_diagnostic(diags: &[Diagnostic]) {
    let hits: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type) && d.message.contains("expects"))
        .collect();
    assert!(
        hits.is_empty(),
        "expected no argument-type diagnostic, got: {hits:#?}"
    );
}

/// The exact idiom from the issue: a `nil`-returning absent-block on a
/// `Dictionary(String, V)` whose value type doesn't itself include `Nil`
/// must type-check, and the enclosing method's declared `V | Nil` return
/// type must accept it.
#[test]
fn dictionary_at_ifabsent_nil_block_widens_to_value_type_or_nil() {
    let source = r"
Object subclass: RunningEntry
  label => 'entry'

typed Actor subclass: Orchestrator
  state: running :: Dictionary(String, RunningEntry) = #{}

  runningEntryFor: issueId :: String -> RunningEntry | Nil =>
    self.running at: issueId ifAbsent: [nil]
";
    let diags = run_with_protocols(source);
    assert_no_expects_diagnostic(&diags);
    assert!(
        diags
            .iter()
            .all(|d| d.category != Some(DiagnosticCategory::Type)
                || !d.message.contains("declares return type")),
        "runningEntryFor: should type-check against its declared `RunningEntry | Nil` \
         return type, got: {diags:#?}"
    );
}

/// The absent-block's return type need not be `Nil` specifically — any type
/// widens through, same as `Behaviour>>removeSelector:ifAbsent:`.
#[test]
fn dictionary_at_ifabsent_non_nil_block_widens_to_union() {
    let source = r#"
Object subclass: RunningEntry
  label => "entry"

typed Actor subclass: Orchestrator
  state: running :: Dictionary(String, RunningEntry) = #{}

  runningEntryOrDefault: issueId :: String -> RunningEntry | String =>
    self.running at: issueId ifAbsent: ["missing"]
"#;
    let diags = run_with_protocols(source);
    assert_no_expects_diagnostic(&diags);
}
