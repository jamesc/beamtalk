// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §6 "One predicate, in `beamtalk-core`" — the producer half
//! (BT-3749, from the BT-3746 review): every loop or fold the Phase 0 check
//! accepts at a sequenced operand site because it is a local-threading
//! producer (`beamtalk-core`'s `sequenced_operand_sites` +
//! `operand_producer_writes`) is lowered by codegen's producer, threading
//! exactly the locals the core rule names. Otherwise the diagnostic would
//! accept a program whose construct tuple is never unpacked.
//!
//! The corpus is every pairing of a construct that writes `t` with an
//! operand position, in each method context; a program the ADR 0131
//! checks reject is skipped (nothing is compiled for it).

use crate::core_erlang::threaded_expr::fault::recording_producers;
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{
    operand_producer_writes, sequenced_operand_sites,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::HashMap;

const CONSTRUCTS: &[&str] = &[
    "#(1, 2) collect: [:x | t := t + x]",
    "#(1, 2) reject: [:x | t := t + 1. false]",
    "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    "#(1, 2) count: [:x | t := t + 1. true]",
    "1 to: 2 do: [:i | t := t + i]",
    "2 timesRepeat: [t := t + 1]",
    "[s < 2] whileTrue: [s := s + 1. t := t + 1]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [0]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [t := t + 1. 0]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [u := 1. 0]",
    "#(1, 2) do: [:x | t := t + x]",
    "#(1, 2) anySatisfy: [:x | t := t + 1. false]",
];

const POSITIONS: &[&str] = &[
    "r := ({c}) size\n#[r, t]",
    "r := t + ({c})\n#[r, t]",
    "r := #[{c}, t]\nr",
    "r := {c}\n#[r, t]",
    "r := 0 max: ({c})\n#[r, t]",
    "Transcript show: ({c}) printString\nt",
    "^({c}) printString",
];

fn sources(construct: &str, position: &str) -> [String; 3] {
    let mut body = String::new();
    for line in format!(
        "t := 0\nu := 0\ns := 0\n{}",
        position.replace("{c}", construct)
    )
    .lines()
    {
        body.push_str("    ");
        body.push_str(line);
        body.push('\n');
    }
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{body}"),
        format!("Value subclass: AgreeValue\n  go =>\n{body}"),
        format!("Actor subclass: AgreeActor\n  state: n = 0\n\n  go =>\n{body}"),
    ]
}

/// The core rule's verdict for every sequenced operand site of the method
/// body that writes an outer local: its span, and the names it threads when
/// it is a producer.
fn core_producers(body: &[beamtalk_core::ast::ExpressionStatement]) -> HashMap<Span, Vec<String>> {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let mut out = HashMap::new();
    for stmt in body {
        for site in sequenced_operand_sites(&stmt.expression) {
            if let Some(writes) = operand_producer_writes(site, &bound) {
                let mut names: Vec<String> =
                    writes.into_iter().map(|w| w.name.to_string()).collect();
                names.sort();
                names.dedup();
                out.insert(Expression::span(site), names);
            }
        }
    }
    out
}

#[test]
fn every_operand_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for construct in CONSTRUCTS {
        for position in POSITIONS {
            for src in sources(construct, position) {
                let (module, parse_diags) = parse(lex_with_eof(&src));
                assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
                let analysis = analyse_full(&module, AnalysisContext::default());
                let rejected = analysis.diagnostics.iter().any(|d| {
                    d.severity == Severity::Error
                        && matches!(
                            d.category,
                            Some(
                                DiagnosticCategory::UnmigratedLocalThreading
                                    | DiagnosticCategory::Tier2BlockNoReturnChannel
                            )
                        )
                });
                if rejected {
                    continue;
                }
                let class = &module.classes[0];
                let method = class.methods.iter().chain(&class.class_methods).next();
                let expected = core_producers(&method.expect("one method").body);
                let (generated, records) = recording_producers(|| {
                    generate_module_with_warnings(&module, CodegenOptions::new("agree"))
                });
                assert!(generated.is_ok(), "{src}: {:?}", generated.err());
                let lowered: HashMap<Span, Vec<String>> = records.into_iter().collect();
                for (span, names) in &expected {
                    assert_eq!(
                        lowered.get(span),
                        Some(names),
                        "the core rule accepts the construct at {span:?} as a producer \
                         threading {names:?}, but codegen lowered {lowered:?}\n{src}"
                    );
                    checked += 1;
                }
            }
        }
    }
    // Not vacuous: most fold/loop × position × context cells are producers.
    assert!(checked > 100, "only {checked} producer sites compared");
}
