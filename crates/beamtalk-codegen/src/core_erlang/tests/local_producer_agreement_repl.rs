// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2d (BT-3795): the producer half of §6 for the REPL root.
//! The sibling of `local_producer_agreement.rs` for the method bodies: every
//! construct the Phase 0 check accepts at an operand site of a REPL input
//! because it is a local-threading producer (`beamtalk-core`'s
//! `repl_operand_sites` + `repl_producer_writes`) is lowered by codegen's
//! REPL producer, and codegen lowers no producer anywhere else. Otherwise the
//! diagnostic would accept an input whose `{Value, Bindings}` tuple is never
//! unpacked.
//!
//! BT-3823: the same both ways for the REPL's arms (a conditional arm, an
//! `on:do:`/`ensure:` block, a `Result tryDo:` block), whose sites are
//! `repl_arm_producer_sites`: every construct writing a binding at one of them
//! in an accepted input is lowered by the producer.

use super::local_producer_agreement::CONSTRUCTS;
use super::local_producer_agreement_conditional::CONDITIONALS;
use crate::core_erlang::threaded_expr::fault::recording_producers;
use crate::core_erlang::{CodeGenContext, CodeGenError, CoreErlangGenerator};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{
    LocalThreadingFamily, conditional_arm_blocks, construct_outer_local_writes,
    exception_arm_blocks, literal_block_value_arm, local_threading_construct, lookup_arm_blocks,
    repl_arm_producer_sites, repl_operand_sites, repl_producer_writes,
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
/// lowered by codegen's REPL producer; in an accepted input every construct
/// site that writes a binding (the REPL treats every name as one, so a name a
/// construct introduces counts too), at the root or in a REPL arm (BT-3823), is
/// lowered by it, so no `{Value, Bindings}` tuple leaks and no write is
/// dropped; and the Phase 0 check never rejects a site the rule names. Outside
/// every block the producer fires at exactly the root sites that write a
/// binding. (Inside a block it may also fire in a frame these sites do not
/// describe, such as a conditional statement of a loop body in an arm or the
/// separator of a desugared `do:separatedBy:`: those constructs adopt the same
/// bindings map and answer right, `adr0131_local_rebind.btscript`.)
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

/// At the input root, outside every block (no arm or loop frame applies), the
/// REPL producer fires at exactly the root sites that write a binding
/// (`root_writes`): never at a root construct the rule does not name.
fn assert_root_lowering_is_exact(
    statements: &[&Expression],
    lowered: &HashSet<Span>,
    root_writes: &HashSet<Span>,
    src: &str,
) {
    let blocks = block_spans(statements);
    let root_lowered: HashSet<Span> = lowered
        .iter()
        .copied()
        .filter(|span| !blocks.iter().any(|block| block.contains(*span)))
        .collect();
    assert_eq!(
        &root_lowered, root_writes,
        "the REPL producer fires at exactly the root construct sites that write a binding\n{src}"
    );
}

/// The spans of every block literal in `statements`.
fn block_spans(statements: &[&Expression]) -> Vec<Span> {
    let mut spans = Vec::new();
    for stmt in statements {
        beamtalk_core::ast_walker::walk_expression(stmt, &mut |e| {
            if let Expression::Block(block) = e {
                spans.push(block.span);
            }
        });
    }
    spans
}

/// Whether `site` is a construct the REPL producer may lower: a
/// local-threading construct other than a lookup (the REPL keeps the lookup's
/// §6 rejection) whose blocks write a binding (the REPL treats every name as
/// one).
fn writes_a_binding(site: &Expression) -> bool {
    local_threading_construct(site).is_some_and(|c| {
        c.family != LocalThreadingFamily::Lookup
            && !construct_outer_local_writes(&c, &|_| true).is_empty()
    })
}

/// The names `repl_producer_writes` threads at `site`, if it is a producer.
fn rule_names(site: &Expression, bound: &dyn Fn(&str) -> bool) -> Option<Vec<String>> {
    let mut names: Vec<String> = repl_producer_writes(site, bound)?
        .into_iter()
        .map(|w| w.name.to_string())
        .collect();
    names.sort();
    names.dedup();
    Some(names)
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
            let mut writes_binding: HashSet<Span> = HashSet::new();
            let mut root_writes: HashSet<Span> = HashSet::new();
            for stmt in &statements {
                for site in repl_operand_sites(stmt) {
                    if let Some(names) = rule_names(site, &bound) {
                        rule_named.insert(Expression::span(site), names);
                    }
                    if writes_a_binding(site) {
                        writes_binding.insert(Expression::span(site));
                        root_writes.insert(Expression::span(site));
                    }
                }
                // BT-3823: and the REPL arm sites of the constructs whose
                // blocks the REPL inlines (the arm half below checks those).
                for site in arm_sites_of_statement(stmt) {
                    if writes_a_binding(site) {
                        writes_binding.insert(Expression::span(site));
                    }
                }
            }
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
                    // A statement with no root producer keeps the assembler's
                    // own lowering, whose REPL arms may hold producers.
                    if generator
                        .repl_statement_with_producers(stmt, stmt)?
                        .is_none()
                    {
                        generator.expression_doc_with_repl_mutation_tracking(stmt)?;
                    }
                }
                Ok::<(), CodeGenError>(())
            });
            assert!(generated.is_ok(), "{src}: {:?}", generated.err());
            let lowered: HashSet<Span> = records.iter().map(|(span, _)| *span).collect();
            for (span, names) in &rule_named {
                assert!(
                    lowered.contains(span),
                    "the core rule accepts the REPL construct at {span:?} threading {names:?}, \
                     but codegen did not lower it as a producer\n{src}"
                );
                rule_named_total += 1;
            }
            assert!(
                writes_binding.is_subset(&lowered),
                "an accepted REPL site writes a binding but is not lowered by the producer \
                 (its write would be dropped): {:?}\n{src}",
                writes_binding.difference(&lowered).collect::<Vec<_>>()
            );
            assert_root_lowering_is_exact(&statements, &lowered, &root_writes, &src);
        }
    }
    assert!(
        rule_named_total > least_named && rejected_total > 20,
        "{rule_named_total} REPL sites the rule names compared, {rejected_total} programs rejected"
    );
}

/// BT-3823: REPL inputs placing an arm statement `{p}` in a REPL arm: the arms
/// of a conditional, the protected body, handler and cleanup of an
/// `on:do:`/`ensure:`, the block of a `Result tryDo:`, and arms nested in arms.
/// The REPL inlines each over its bindings map when the construct writes a
/// binding.
const REPL_ARMS: &[&str] = &[
    "r := (s < 3) ifTrue: [{p}. 7] ifFalse: [0]",
    "(s < 3) ifTrue: [{p}]",
    "r := (s < 3) ifFalse: [{p}. 7]",
    "r := (s < 3) and: [{p}. true]",
    "r := (s < 3) or: [{p}. true]",
    "r := s ifNotNil: [:v | {p}. v]",
    "r := s ifNil: [0] ifNotNil: [:v | {p}. v]",
    "r := s ifNotNil: [:v | {p}. v] ifNil: [0]",
    "r := [{p}. 7] on: Error do: [:e | 3]",
    "r := [{p}] on: Error do: [:e | 3]",
    "r := [1 / 0] on: Error do: [:e | {p}. 7]",
    "r := [{p}. 7] ensure: [nil]",
    "r := [7] ensure: [{p}]",
    "r := (Result tryDo: [{p}. 7]) unwrap",
    "r := [(s < 3) ifTrue: [{p}. 7] ifFalse: [0]] on: Error do: [:e | 3]",
    "r := (s < 3) ifTrue: [[{p}. 7] on: Error do: [:e | 3]] ifFalse: [0]",
    "r := s ifNil: [[7] ensure: [(s < 3) ifTrue: [{p}]]]",
];

/// Where a construct `{c}` sits in an arm statement: the statement itself, an
/// operand, an assignment's value, a literal's element; and positions that are
/// not arm sites (a loop body, a closure, a cascade), which stay Phase 0
/// errors.
const ARM_STATEMENTS: &[&str] = &[
    "{c}",
    "({c}) printString size",
    "u := {c}",
    "#[{c}, t]",
    "0 max: ({c}) printString size",
    "#(1) do: [:z | {c}]",
    "[{c}] value: 1",
    "({c}) printString; yourself",
];

/// Constructs the REPL lowers in an arm that are not in [`CONSTRUCTS`] or
/// [`CONDITIONALS`]: exception constructs, evaluated blocks, and a lookup
/// (which keeps its §6 rejection).
const REPL_ARM_CONSTRUCTS: &[&str] = &[
    "[t := t + 1. 5] on: Error do: [:e | 0]",
    "[t := t + 1. 5] ensure: [u := u + 1]",
    "Result tryDo: [t := t + 1. 5]",
    "[t := t + 1. 5] value",
    "[:k | t := t + k. 5] value: 1",
    // A Tier 2 call whose argument is a writing fold (rejected: the fold is an
    // operand of a construct, not a site).
    "[:k | t := t + k. k] value: (#(1, 2) inject: 0 into: [:a :x | t := t + x. a + x])",
    "#{#a => 1} at: #b ifAbsent: [t := t + 1. 0]",
];

/// The REPL arm sites of `expr` and of the arms nested in them: for a
/// conditional (not a lookup, not an evaluated literal block) or an
/// `on:do:`/`ensure:`/`tryDo:` whose blocks write a binding, every
/// `repl_arm_producer_sites` site of every statement of its blocks, recursing
/// into a site that is itself such a construct.
fn repl_arm_sites<'a>(expr: &'a Expression, out: &mut Vec<&'a Expression>) {
    let inner = expr.unwrap_parens();
    if lookup_arm_blocks(inner).is_some()
        || literal_block_value_arm(inner).is_some()
        || !writes_a_binding(inner)
    {
        return;
    }
    let Some(blocks) = conditional_arm_blocks(inner).or_else(|| exception_arm_blocks(inner)) else {
        return;
    };
    for block in blocks {
        for stmt in &block.body {
            for site in repl_arm_producer_sites(&stmt.expression) {
                out.push(site);
                repl_arm_sites(site, out);
            }
        }
    }
}

/// The REPL arm sites of the input statement `stmt`: those of the construct
/// that is the statement or an assignment's whole value, and of the constructs
/// at its operand sites (`(Result tryDo: [...]) unwrap`).
fn arm_sites_of_statement(stmt: &Expression) -> Vec<&Expression> {
    let mut sites = Vec::new();
    let root = match stmt.unwrap_parens() {
        Expression::Assignment { value, .. } => value.as_ref(),
        other => other,
    };
    repl_arm_sites(root, &mut sites);
    for operand in repl_operand_sites(stmt) {
        repl_arm_sites(operand, &mut sites);
    }
    sites
}

/// The Phase 0 errors (both ADR 0131 categories) of an analysed REPL input.
fn phase0_errors(
    analysis: &beamtalk_core::semantic_analysis::AnalysisResult,
) -> Vec<&beamtalk_core::source_analysis::Diagnostic> {
    analysis
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
        .collect()
}

/// The REPL arm half of the agreement, both ways: in an input the Phase 0
/// check accepts, codegen's REPL producer lowers every construct writing a
/// binding at a REPL arm site (so no arm leaks a `{Value, Bindings}` tuple or
/// drops a write), and the Phase 0 check never rejects a site
/// `repl_producer_writes` names.
#[test]
fn every_repl_arm_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let known = ["t", "u", "s", "r"];
    let bound = |name: &str| known.contains(&name);
    let (mut rule_named_total, mut rejected_total) = (0, 0);
    let constructs = CONSTRUCTS
        .iter()
        .chain(CONDITIONALS)
        .chain(REPL_ARM_CONSTRUCTS);
    for construct in constructs {
        for arm in REPL_ARMS {
            for statement in ARM_STATEMENTS {
                let src = format!(
                    "{}\n",
                    arm.replace("{p}", &statement.replace("{c}", construct))
                );
                let (module, parse_diags) = parse(lex_with_eof(&src));
                assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
                let analysis =
                    analyse_full(&module, AnalysisContext::default().with_known_vars(&known));
                let phase0 = phase0_errors(&analysis);
                let statements: Vec<&Expression> =
                    module.expressions.iter().map(|s| &s.expression).collect();
                let sites: Vec<&Expression> = statements
                    .iter()
                    .flat_map(|stmt| arm_sites_of_statement(stmt))
                    .collect();
                let rule_named: HashMap<Span, Vec<String>> = sites
                    .iter()
                    .filter_map(|site| Some((Expression::span(site), rule_names(site, &bound)?)))
                    .collect();
                for d in &phase0 {
                    assert!(
                        !rule_named.contains_key(&d.span),
                        "Phase 0 rejects the REPL arm site {:?} the core rule names: {}\n{src}",
                        d.span,
                        d.message
                    );
                }
                if !phase0.is_empty() {
                    rejected_total += 1;
                    continue;
                }
                let writing: HashSet<Span> = sites
                    .iter()
                    .filter(|site| writes_a_binding(site))
                    .map(|site| Expression::span(site))
                    .collect();
                let (generated, records) = recording_producers(|| {
                    let mut generator = CoreErlangGenerator::new("agree_repl_arm");
                    generator.context = CodeGenContext::Repl;
                    generator.set_is_repl_mode(true);
                    generator.push_scope();
                    generator.bind_var("__bindings__", "Bindings");
                    for stmt in &statements {
                        if generator
                            .repl_statement_with_producers(stmt, stmt)?
                            .is_none()
                        {
                            generator.expression_doc_with_repl_mutation_tracking(stmt)?;
                        }
                    }
                    Ok::<(), CodeGenError>(())
                });
                assert!(generated.is_ok(), "{src}: {:?}", generated.err());
                let lowered: HashSet<Span> = records.iter().map(|(span, _)| *span).collect();
                for (span, names) in &rule_named {
                    assert!(
                        lowered.contains(span),
                        "the core rule accepts the REPL arm construct at {span:?} threading \
                         {names:?}, but codegen did not lower it as a producer\n{src}"
                    );
                    rule_named_total += 1;
                }
                assert!(
                    writing.is_subset(&lowered),
                    "an accepted REPL arm site writes a binding but is not lowered by the \
                     producer (its write would be dropped): {:?}\n{src}",
                    writing.difference(&lowered).collect::<Vec<_>>()
                );
            }
        }
    }
    assert!(
        rule_named_total > 1000 && rejected_total > 100,
        "{rule_named_total} REPL arm sites the rule names compared, {rejected_total} programs \
         rejected"
    );
}
