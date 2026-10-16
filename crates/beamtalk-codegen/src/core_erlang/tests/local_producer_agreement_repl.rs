// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2d (BT-3795): the producer half of §6 for the REPL root.
//! The sibling of `local_producer_agreement.rs` for the method bodies: every
//! loop or fold the Phase 0 check accepts at an operand site of a REPL input
//! because it is a local-threading producer (`beamtalk-core`'s
//! `repl_operand_sites` + `operand_producer_writes`) is lowered by codegen's
//! REPL producer, and codegen lowers no producer anywhere else. Otherwise the
//! diagnostic would accept an input whose `{Value, Bindings}` tuple is never
//! unpacked.

use super::local_producer_agreement::CONSTRUCTS;
use super::local_producer_agreement_conditional::CONDITIONALS;
use crate::core_erlang::threaded_expr::fault::recording_producers;
use crate::core_erlang::{CodeGenContext, CodeGenError, CoreErlangGenerator};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{
    RootMethodKind, conditional_arm_blocks, construct_outer_local_writes, literal_block_value_arm,
    local_threading_construct, producer_body_block, repl_operand_sites, root_producer_writes,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::{HashMap, HashSet};

/// REPL inputs placing a construct `{c}` that writes `t` around the sites of
/// the REPL root: operands the producer lowers, the statement and whole-value
/// positions the REPL's own lowering keeps (the sites rule leaves them out),
/// and positions that stay Phase 0 errors (skipped below).
const REPL_POSITIONS: &[&str] = &[
    "({c}) size",
    "r := ({c}) size",
    "r := t + ({c})",
    "t + ({c})",
    "#[{c}, t]",
    "r := #[{c}, t]",
    "r := 0 max: ({c})",
    "Transcript show: ({c}) printString",
    // A producer in the receiver of a construct that is the whole value (a
    // site of the assignment) and of one that is the statement (not a site).
    "r := ({c}) inject: 0 into: [:a :x | u := u + 1. a + x]",
    "({c}) inject: 0 into: [:a :x | u := u + 1. a + x]",
    "({c}) do: [:e | u := u + e]",
    // The REPL's own lowering of a statement and of an assignment's value.
    "{c}",
    "({c})",
    "r := {c}",
    "r := ({c})",
    // Several statements in one input.
    "t := 0\nr := ({c}) size\n#[r, t]",
    "r := ({c}) size\nr := r + ({c}) size\nr",
    "({c}) size\n({c}) size",
    // Not sites: inside a block, a conditional's receiver, a cascade, a
    // destructuring, a field write.
    "[({c}) size] value",
    "({c}) isEmpty ifTrue: [0]",
    "({c}) size; yourself",
    "#(a, b) := ({c}) asArray",
    "self.n := ({c}) size",
];

/// The REPL root's producer sites, both ways. A site the core rule names is
/// lowered by codegen's REPL producer; the producer fires at exactly the loop
/// and fold sites that write a binding (a superset of the ones the rule names: the REPL
/// treats every name as a binding, so a name a construct introduces is
/// threaded too) and at no other construct; and the Phase 0 check never
/// rejects a site the rule names.
///
/// Drives the generator's `repl_statement_with_producers` (what the REPL
/// assembler calls) directly: the assembler's use of it is covered by the
/// `beamtalk-repl` agreement test and by the REPL protocol cases.
#[test]
fn every_repl_operand_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    check_repl_agreement(CONSTRUCTS, 100);
}

/// ADR 0131 Phase 3 (BT-3738): the same, for conditionals. A conditional at an
/// operand site of a REPL input that writes a binding is a producer that adopts
/// the bindings map whole (`conditional_producer_writes` for the core rule,
/// `ifNil:` and its relatives included).
#[test]
fn every_repl_conditional_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    check_repl_agreement(CONDITIONALS, 100);
}

fn check_repl_agreement(constructs: &[&str], least_named: usize) {
    let known = ["t", "u", "s", "r"];
    let bound = |name: &str| known.contains(&name);
    let (mut rule_named_total, mut rejected_total) = (0, 0);
    for construct in constructs {
        for position in REPL_POSITIONS {
            let src = format!("{}\n", position.replace("{c}", construct));
            let (module, parse_diags) = parse(lex_with_eof(&src));
            assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
            let analysis =
                analyse_full(&module, AnalysisContext::default().with_known_vars(&known));
            let phase0: Vec<_> = analysis
                .diagnostics
                .iter()
                .filter(|d| {
                    d.severity == Severity::Error
                        && matches!(
                            d.category,
                            Some(
                                DiagnosticCategory::UnmigratedLocalThreading
                                    | DiagnosticCategory::Tier2BlockNoReturnChannel
                            )
                        )
                })
                .collect();
            let statements: Vec<&Expression> =
                module.expressions.iter().map(|s| &s.expression).collect();

            let mut rule_named: HashMap<Span, Vec<String>> = HashMap::new();
            let mut writes_a_binding: HashSet<Span> = HashSet::new();
            for stmt in &statements {
                for site in repl_operand_sites(stmt) {
                    if let Some(writes) = root_producer_writes(site, &bound, RootMethodKind::Actor)
                    {
                        let mut names: Vec<String> =
                            writes.into_iter().map(|w| w.name.to_string()).collect();
                        names.sort();
                        names.dedup();
                        rule_named.insert(Expression::span(site), names);
                    }
                    if (producer_body_block(site).is_some()
                        || conditional_arm_blocks(site).is_some())
                        && local_threading_construct(site).is_some_and(|c| {
                            !construct_outer_local_writes(&c, &|_| true).is_empty()
                        })
                    {
                        writes_a_binding.insert(Expression::span(site));
                    }
                }
            }
            // BT-3841: an evaluated literal block (`[...] value`) is an arm-like
            // producer in a method only. The REPL root lowers it itself, so the
            // REPL producer must not fire at it (asserted below) and the sites
            // the method rule names for it are not REPL producer sites.
            let mut block_values: HashSet<Span> = HashSet::new();
            for stmt in &statements {
                beamtalk_core::ast_walker::walk_expression(stmt, &mut |e| {
                    if literal_block_value_arm(e).is_some() {
                        block_values.insert(Expression::span(e.unwrap_parens()));
                    }
                });
            }
            rule_named.retain(|span, _| !block_values.contains(span));
            writes_a_binding.retain(|span| !block_values.contains(span));
            // The Phase 0 check never rejects a site the rule names.
            for d in &phase0 {
                assert!(
                    !rule_named.contains_key(&d.span),
                    "Phase 0 rejects the REPL site {:?} the core rule names: {}\n{src}",
                    d.span,
                    d.message
                );
            }
            if !phase0.is_empty() {
                rejected_total += 1;
                continue;
            }
            // The producers the REPL codegen lowers for the whole input.
            let (generated, records) = recording_producers(|| {
                let mut generator = CoreErlangGenerator::new("agree_repl");
                generator.context = CodeGenContext::Repl;
                generator.set_is_repl_mode(true);
                generator.push_scope();
                generator.bind_var("__bindings__", "Bindings");
                for stmt in &statements {
                    generator.repl_statement_with_producers(stmt, stmt)?;
                }
                Ok::<(), CodeGenError>(())
            });
            assert!(generated.is_ok(), "{src}: {:?}", generated.err());
            let lowered: HashSet<Span> = records.iter().map(|(span, _)| *span).collect();
            assert!(
                lowered.is_disjoint(&block_values),
                "the REPL producer lowered an evaluated literal block\n{src}"
            );
            for (span, names) in &rule_named {
                assert!(
                    lowered.contains(span),
                    "the core rule accepts the REPL construct at {span:?} threading {names:?}, \
                     but codegen did not lower it as a producer\n{src}"
                );
                rule_named_total += 1;
            }
            assert_eq!(
                lowered, writes_a_binding,
                "the REPL producer fires at exactly the loop and fold sites that write a \
                 binding\n{src}"
            );
        }
    }
    assert!(
        rule_named_total > least_named && rejected_total > 20,
        "{rule_named_total} REPL sites the rule names compared, {rejected_total} programs rejected"
    );
}
