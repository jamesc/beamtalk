// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 stopgap (BT-3864): the Phase 0 rejection of a value-position local
//! assignment agrees with codegen, both ways, in class, value-type and actor
//! methods. `beamtalk-core`'s `value_position_local_assignments` names an
//! assignment exactly when codegen compiles it to its value only and drops the
//! write (`dropped_assignment_probe`, the `Expression::Assignment` arm of
//! `generate_expression`), and the Phase 0 check reports every assignment it
//! names. The REPL half is
//! `beamtalk-repl/tests/value_position_assignment_agreement.rs`.
//!
//! The corpus ([`dropped_assignment_probe::corpus`]) places a local assignment
//! in every statement and value position, inside every kind of body that has
//! its own statement lowering. A program the predicate rejects is compiled
//! too (with the check bypassed), so both directions are compared; one that
//! another check rejects is skipped, because it is never compiled.

use crate::core_erlang::dropped_assignment_probe::{self, PRELUDE, corpus};
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Expression, Module};
use beamtalk_core::semantic_analysis::block_facts::{
    RootMethodKind, value_position_local_assignments,
};
use beamtalk_core::semantic_analysis::stored_block_inline::inline_stored_tier2_values;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::HashSet;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// `body` as the body of a class, a value-type and an actor method.
fn sources(body: &str) -> [String; 3] {
    let mut indented = String::new();
    for line in format!("{PRELUDE}\n{body}").lines() {
        indented.push_str("    ");
        indented.push_str(line);
        indented.push('\n');
    }
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{indented}"),
        format!("Value subclass: AgreeValue\n  go =>\n{indented}"),
        format!("Actor subclass: AgreeActor\n  state: n = 0\n\n  go =>\n{indented}"),
    ]
}

/// The predicate's spans over every method body of `module`.
fn predicate(module: &Module) -> HashSet<Span> {
    let mut out = HashSet::new();
    for class in &module.classes {
        let instance = if class.name.name == "AgreeActor" {
            RootMethodKind::Actor
        } else {
            RootMethodKind::ValueType
        };
        let methods = class.methods.iter().map(|m| (m, instance));
        let class_methods = class
            .class_methods
            .iter()
            .map(|m| (m, RootMethodKind::Class));
        for (method, kind) in methods.chain(class_methods) {
            // ADR 0132 Phase 0b (BT-3867): the check and the pre-codegen
            // lowering see a class or value-type body with each promotable
            // stored block inlined at its sends, so the predicate does too.
            let params: Vec<_> = method
                .parameters
                .iter()
                .map(|p| p.name.name.clone())
                .collect();
            let inlined = (kind != RootMethodKind::Actor)
                .then(|| inline_stored_tier2_values(&method.body, &params))
                .flatten();
            out.extend(
                value_position_local_assignments(
                    inlined.as_deref().unwrap_or(&method.body),
                    Some(kind),
                )
                .into_iter()
                .map(|found| found.span),
            );
        }
    }
    out
}

/// The assignments codegen drops compiling `module`, or `None` when the
/// compile fails or panics before it finishes (nothing is emitted, so nothing
/// is silently dropped).
///
/// A method's last statement is its value: when it is a bare local assignment,
/// the method returns before anything can read the local, so the class and
/// value-type lowerings compile only its value. That drops nothing, so it is
/// not counted.
fn dropped(module: &Module) -> Option<HashSet<Span>> {
    let (compiled, spans) = dropped_assignment_probe::recording(|| {
        catch_unwind(AssertUnwindSafe(|| {
            generate_module_with_warnings(module, CodegenOptions::new("agree")).is_ok()
        }))
        .unwrap_or(false)
    });
    let dead: HashSet<Span> = module
        .classes
        .iter()
        .flat_map(|c| c.methods.iter().chain(&c.class_methods))
        .filter_map(|m| match &m.body.last()?.expression {
            last @ Expression::Assignment { target, .. }
                if matches!(target.as_ref(), Expression::Identifier(_)) =>
            {
                Some(last.span())
            }
            _ => None,
        })
        .collect();
    compiled.then(|| spans.into_iter().filter(|s| !dead.contains(s)).collect())
}

/// The method contexts of [`sources`], in its order: one test each below
/// (`per_family_tests!`, BT-3888).
const CONTEXTS: &[&str] = &["class", "value type", "actor"];

/// The families of [`CONTEXTS`]: one context each.
const CONTEXT_FAMILIES: &[std::ops::Range<usize>] = &[0..1, 1..2, 2..3];

/// Both ways over the whole corpus in the method contexts named: the whole
/// proof is the per-context tests below.
fn the_rejection_names_exactly_the_local_assignments_codegen_drops(contexts: &[&str]) {
    assert_eq!(sources("").len(), CONTEXTS.len(), "one source per context");
    let mut named = 0;
    let mut accepted = 0;
    let mut failures = Vec::new();
    for body in corpus() {
        let all = sources(&body);
        for context in contexts {
            let src = &all[CONTEXTS
                .iter()
                .position(|c| c == context)
                .expect("a context of CONTEXTS")];
            let (module, parse_diags) = parse(lex_with_eof(src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let expected = predicate(&module);
            let analysis = analyse_full(&module, AnalysisContext::default());
            // Every assignment the predicate names is a compile error at its
            // span, whichever check owns it: a `match:` arm's write to an outer
            // local keeps its own error, and an assignment inside an escaping
            // block is the ADR 0133 escaping-block error (one diagnostic per
            // assignment, BT-3902).
            for span in &expected {
                let reported = analysis.diagnostics.iter().any(|d| {
                    d.span == *span
                        && matches!(
                            d.category,
                            Some(
                                DiagnosticCategory::ValuePositionLocalAssignment
                                    | DiagnosticCategory::UnmigratedLocalThreading
                                    | DiagnosticCategory::Tier2BlockNoReturnChannel
                            )
                        )
                });
                if !reported {
                    failures.push(format!("{span:?} is not reported\n{src}"));
                }
            }
            let rejected_otherwise = analysis.diagnostics.iter().any(|d| {
                d.severity == Severity::Error
                    && d.category != Some(DiagnosticCategory::ValuePositionLocalAssignment)
            });
            if rejected_otherwise {
                continue;
            }
            match dropped(&module) {
                Some(dropped) if dropped == expected => {
                    if expected.is_empty() {
                        accepted += 1;
                    } else {
                        named += 1;
                    }
                }
                Some(dropped) => failures.push(format!(
                    "the predicate names {expected:?} but codegen drops {dropped:?}\n{src}"
                )),
                // A compile that fails (BT-3799's shape trips the debug
                // verifier) drops nothing silently; an accepted program must
                // compile.
                None if !expected.is_empty() => {}
                None => failures.push(format!("an accepted program fails to compile\n{src}")),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} disagreements:\n{}",
        failures.len(),
        failures.join("\n---\n")
    );
    // Not vacuous: both directions compare many compiled programs in every
    // context (the whole corpus once asserted more than 300 and 100 over its
    // three contexts).
    assert!(
        named > 100,
        "only {named} rejected programs compared in {contexts:?}"
    );
    assert!(
        accepted > 30,
        "only {accepted} accepted programs compared in {contexts:?}"
    );
}

beamtalk_core::per_family_tests! {
    list = CONTEXTS,
    families = CONTEXT_FAMILIES,
    check = the_rejection_names_exactly_the_local_assignments_codegen_drops,
    partition = the_value_position_assignment_agreement_tests_cover_every_context,
    the_rejection_names_exactly_the_local_assignments_codegen_drops_in_class_methods => 0,
    the_rejection_names_exactly_the_local_assignments_codegen_drops_in_value_type_methods => 1,
    the_rejection_names_exactly_the_local_assignments_codegen_drops_in_actor_methods => 2,
}
