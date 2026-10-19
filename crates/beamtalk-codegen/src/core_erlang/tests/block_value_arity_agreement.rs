// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3884: the analyser's `BlockValue` classification and codegen's inlining
//! of a block literal's `value` send agree, over every `value`-family
//! selector and every block arity from 0 to 4, in class, value-type and actor
//! methods.
//!
//! `beamtalk-core`'s `local_threading_construct` names a send a
//! `LocalThreadingFamily::BlockValue` construct exactly when codegen compiles
//! the block's body in place (no `fun` for the block). Both read the one
//! arity rule, `block_literal_arity_matches` (BT-3881): a block literal sent a
//! selector of another arity is applied to a stand-in fun (`apply _Arity`),
//! raises `arity_mismatch` and never runs its body, so the analyser reports no
//! write for it, in any position, and the ADR 0133 escaping-block check does
//! not report it as escaping either.

use super::codegen;
use beamtalk_core::ast::{Expression, WellKnownSelector};
use beamtalk_core::semantic_analysis::block_facts::{
    LocalThreadingFamily, local_threading_construct,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

/// The `value`-family selectors, by the number of arguments they send, and
/// `valueWithArguments:` (`None`: its arguments are one runtime list).
const SELECTORS: &[(&str, Option<usize>)] = &[
    ("value", Some(0)),
    ("value:", Some(1)),
    ("value:value:", Some(2)),
    ("value:value:value:", Some(3)),
    ("value:value:value:value:", Some(4)),
    ("valueWithArguments:", None),
];

const PARAMS: &[&str] = &["a", "b", "c", "d"];

/// The method kinds: the class header and method header of a class whose
/// method `m` holds the statements.
const KINDS: &[(&str, &str)] = &[
    ("class", "Object subclass: P\n  class m =>\n"),
    ("value type", "Value subclass: P\n  m =>\n"),
    ("actor", "Actor subclass: P\n  m =>\n"),
];

/// The block body's marker: the literal is compiled wherever the body is.
const MARKER: &str = "7919";

/// `[:a … | t := t + 7919] <selector> …`, a block of `arity` parameters that
/// writes the outer local `t`, sent `selector`.
fn send(selector: &str, args: Option<usize>, arity: usize) -> String {
    let params: Vec<String> = PARAMS[..arity].iter().map(|p| format!(":{p}")).collect();
    let block = if arity == 0 {
        format!("[t := t + {MARKER}]")
    } else {
        format!("[{} | t := t + {MARKER}]", params.join(" "))
    };
    match args {
        Some(0) => format!("{block} {selector}"),
        Some(n) => {
            let parts: Vec<String> = (1..=n).map(|i| format!("value: {i}")).collect();
            format!("{block} {}", parts.join(" "))
        }
        None => {
            let elems: Vec<String> = (1..=arity).map(|i| i.to_string()).collect();
            format!("{block} {selector} #({})", elems.join(", "))
        }
    }
}

fn parse_send(src: &str) -> Expression {
    let (module, diagnostics) = parse(lex_with_eof(src));
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    module
        .expressions
        .into_iter()
        .next()
        .expect("one expression")
        .expression
}

/// Whether the analyser classifies `src` as a `BlockValue` construct.
fn analyser_block_value(src: &str) -> bool {
    local_threading_construct(&parse_send(src))
        .is_some_and(|construct| construct.family == LocalThreadingFamily::BlockValue)
}

/// Whether codegen compiled the block body of the send in `code` in place:
/// the body's marker is in the method and no `fun` opens between the
/// statement before the send (`t := 0`) and it.
fn codegen_inlines(code: &str) -> bool {
    let Some(marker) = code.find(MARKER) else {
        return false;
    };
    let start = code[..marker]
        .rfind("let T = 0 in")
        .unwrap_or_else(|| panic!("no `t := 0` before the block body:\n{code}"));
    !code[start..marker].contains("fun (")
}

fn is_block_value_selector(selector: &str) -> bool {
    WellKnownSelector::from_name(selector).is_some_and(WellKnownSelector::is_block_value)
}

#[test]
fn analyser_block_value_classification_agrees_with_codegen_inlining() {
    for &(selector, args) in SELECTORS {
        for arity in 0..=PARAMS.len() {
            let src = send(selector, args, arity);
            let classified = analyser_block_value(&src);
            assert_eq!(
                classified,
                is_block_value_selector(selector) && args == Some(arity),
                "{src}: the analyser's BlockValue classification"
            );
            for &(kind, header) in KINDS {
                let code = codegen(&format!("{header}    t := 0\n    {src}\n    t\n"));
                assert_eq!(
                    codegen_inlines(&code),
                    classified,
                    "{kind}: {src}: codegen inlines the block iff the analyser \
                     classifies the send as BlockValue\n{code}"
                );
                // A `value`-family send of another arity is applied to a
                // stand-in fun, so it raises `arity_mismatch`.
                if is_block_value_selector(selector) && !classified {
                    assert!(
                        code.contains("apply _Arity") && !code.contains(MARKER),
                        "{kind}: {src}: applied, body never compiled\n{code}"
                    );
                }
            }
        }
    }
}

/// The positions of a send in a method body: a statement, the value of an
/// assignment, a message receiver, a literal element and the last statement.
const POSITIONS: &[&str] = &[
    "t := 0\n    {s}\n    t\n",
    "t := 0\n    x := {s}\n    x printString\n    t\n",
    "t := 0\n    y := ({s}) printString\n    y size + t\n",
    "t := 0\n    z := #[{s}, t]\n    z size\n",
    "t := 0\n    {s}\n",
];

/// The messages of the analyser's errors about a local write in `src`: the
/// Phase 0 check, the ADR 0133 escaping-block check and the value-position
/// assignment check.
fn write_errors(src: &str) -> Vec<String> {
    let (module, parse_diags) = parse(lex_with_eof(src));
    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .filter(|d| {
            d.severity == Severity::Error
                && matches!(
                    d.category,
                    Some(
                        DiagnosticCategory::UnmigratedLocalThreading
                            | DiagnosticCategory::Tier2BlockNoReturnChannel
                            | DiagnosticCategory::ValuePositionLocalAssignment
                    )
                )
        })
        .map(|d| d.message.to_string())
        .collect()
}

#[test]
fn a_send_of_another_arity_reports_no_write_in_any_position() {
    for &(selector, args) in SELECTORS {
        if !is_block_value_selector(selector) {
            continue;
        }
        for arity in (0..=PARAMS.len()).filter(|&arity| args != Some(arity)) {
            let send = send(selector, args, arity);
            for &(kind, header) in KINDS {
                for position in POSITIONS {
                    let src = format!("{header}    {}", position.replace("{s}", &send));
                    let writes = write_errors(&src);
                    assert!(
                        writes.is_empty(),
                        "{kind}: {src}: a block of another arity never runs its body, \
                         so no write is reported: {writes:?}"
                    );
                }
            }
        }
    }
}

#[test]
fn a_parenthesized_block_of_another_arity_is_still_a_block_value() {
    // ADR 0131 §6: only a bare `[...]` literal is inlined or applied as one;
    // a parenthesized block is a block value whatever its arity, compiled as
    // a closure, so the ADR 0133 escaping-block check keeps reporting it.
    for &(kind, header) in KINDS {
        let src = format!("{header}    t := 0\n    ([t := t + 1]) value: 1\n    t\n");
        assert_eq!(write_errors(&src).len(), 1, "{kind}: {src}");
    }
}
